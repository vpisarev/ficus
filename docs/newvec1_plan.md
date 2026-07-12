# newvec-1 plan: promote the mutable vector to a first-class type

Spine of the plan is Vadim's 5 steps; **▸ Поправка (Claude)** blocks are
Claude-chat refinements for review. All prior **[OPEN]** decisions are now
resolved (see the decision log at the end).

## Intent / end state

Make the mutable, growable `fx_vec_t` container (today the temporary `Vec.t`
stdlib class, see `docs/newvec1_report.md`) a **first-class type**, and demote
the little-used immutable RRB `vector` to an explicit `rrbvec`. After this WP the
"default vector" a user writes is the mutable one; RRB is opt-in.

## Invariants that constrain every step

- **O(1) assignment** (report §0): `a = b`, parameter pass, return, store into a
  container/closure stay O(1). The first-class `Vector` keeps `ctp_copy =
  (FX_COPY_PTR, fx_copy_ptr)` — INCREF the shared heap header, never a deep copy.
  Its ctprops target is report §3.1.
- **Two-stage bootstrap + fixpoint.** Every step that touches `compiler/*.fx`
  and/or the 54 bootstrap stdlib modules needs `python3 tools/update_compiler.py`
  (asserts the self-hosting fixpoint). When the stdlib will *use* a new spelling,
  stage it: (1) teach the running compiler to parse/lower the new form with the
  stdlib unchanged + regen; (2) migrate the stdlib + drop the old + regen. This
  is the fold-1/types-1 playbook.
- **Test ladder each step:** `fxtest.py all` (unit + negative + ir + corpus
  O0/O3), `sanitize` (ASan+UBSan — load-bearing here, see Step 4), `determinism`.
  Renames legitimately churn IR goldens (`--update-golden`) and many bootstrap
  modules; generated C for *programs* must not change except where semantics do.

## Naming map (▸ Поправка (Claude) — resolve the `Vector.fx` collision first)

`lib/Vector.fx` **today holds the RRB operators** (`.+`/`.-`/… over `'t
vector`). So the two renames collide on the name `Vector`. Sequence to avoid
clobber:

| thing | now | after |
|---|---|---|
| immutable RRB type (surface) | `vector` / `'t vector` | `rrbvec` / `'t rrbvec` |
| RRB operators module | `lib/Vector.fx` | `lib/Rrbvec.fx` |
| RRB type constructors + mangling | `TypVector`/`KTypVector`/`CTypVector`, `"V"` | renamed `TypRRBVec`/`KTypRRBVec`/`CTypRRBVec`, mangling **`"W"`** |
| mutable type module | `lib/Vec.fx` | `lib/Vector.fx` |
| mutable type constructors + mangling | — (a stdlib class today) | reuse `TypVector`/`KTypVector`/`CTypVector`, mangling **`"V"`** |
| mutable type (surface) | `Vec.t` (class) | `Vector` / `vector` **[OPEN]** |

**Decision (Vadim): constructors swap, mangling swaps.** Rename RRB's internal
constructors to `TypRRBVec`/`KTypRRBVec`/`CTypRRBVec` and move its mangling to
**`"W"`**. The new mutable type — the "default" vector going forward — takes the
freed `TypVector`/`KTypVector`/`CTypVector` names and the **`"V"`** mangling
(`K_mangle.fx:255`). So the RRB rename touches every `Typ/KTyp/CTypVector` match
site (mechanical, Step 1), and the new type slots straight into the short names.

**RESOLVED (Vadim): the lowercase `vector` reassigns to the mutable type.** All
three Parser roles — type annotation (`Parser.fx:1939` `TypVector`),
comprehension (`573/578/589` `ForMakeVector`), named-reduction sugar
(`210/260`) — become the **mutable** type; `rrbvec` takes the old spellings.
This flips the meaning of every existing `vector` in tests/corpus/examples — the
RRB tests (`test_vec.fx` `vector.*`) move to `rrbvec`, and `vector` stays live
for the new type.

## Step 1 — rename immutable `vector` → `rrbvec` (mechanical, its own PR)

Rename the surface name everywhere; keep the RRB engine (`rrbvec.impl.h`, 1255
LoC) untouched.

- Parser: `"vector"` → `"rrbvec"` at the type-annotation, comprehension, and
  named-reduction-sugar sites (or split, per the [OPEN] decision).
- Type nodes: rename constructors `Typ/KTyp/CTypVector` → `*RRBVec` across
  `Ast`/`K_form`/`C_form` and every match site; pretty-printers
  (`Ast_pp`/`K_pp`/`C_pp`) and `Ast_typecheck` messages emit `rrbvec`; move the
  `K_mangle` letter `"V"`→`"W"`. (The freed `Typ/KTyp/CTypVector` names + `"V"`
  are then taken by the new type in Step 2.)
- Stdlib: `lib/Vector.fx` → `lib/Rrbvec.fx`; `lib/Builtins.fx` `string`/`repr`/
  `print` overloads for the type.
- Corpus/tests/examples: rewrite `vector(...)`, `'t vector`, `vector(for…)` →
  `rrbvec…` in the ~14 non-compiler files (6 test, 1 example, 7 lib) — but see
  [OPEN]: if `vector` reassigns to mutable, the RRB *tests* move to `rrbvec`
  while the name `vector` stays live for Step 2.
- **▸ Поправка (Claude):** land Step 1 fully green (ladder + fixpoint) before
  Step 2 — it is a large, self-contained mechanical sweep and must not entangle
  with the new-type work. `update_compiler.py` will regen many bootstrap modules
  (compile-time spelling only; program C must be unchanged).

## Step 2 — first-class mutable `Vector` + the 7 core ops

Add the type at all levels and wire 7 operations: **make-empty, `v[i]`,
`v[i]=a`, `push_back`, `pop_back`, `resize(n)`, `reserve(n)`**.

- **Compiler type node** (reusing `TypVector`/`KTypVector`/`CTypVector` per the
  Naming map): Ast/typecheck/K_form/K_normalize/K_mangle/C_form/C_gen_types +
  the `get_ctprops` arm = report §3.1 signature.
- **Codegen model — follow RRB, not the stdlib-@ccode stopgap.** The compiler
  emits `fx_vec_*` / `FX_VEC_*` directly with **statically known**
  `elemsize`/`copy_f`/`free_f` from `get_ctprops(elemtyp)`, exactly as it emits
  `fx_rrb_*`. This deletes the temporary hacks (`get_vec_data`, the `(val0,
  true)` tuple, the `array(1,·)` alloc) and the runtime `copy_f == 0` branches.
  Op → primitive: make-empty → `fx_make_vec(0,…)`; `v[i]` → `FX_VEC_PTR` (+
  `FX_VEC_CHKIDX`); `v[i]=a` → free-old-then-copy into the slot; `push_back` →
  `fx_vec_append`; `pop_back` → size-- + `free_elem`; `resize`/`reserve` →
  `fx_vec_resize`/`fx_vec_reserve` (all already in `vec.impl.h`).
- **Read surface (in-scope this PR, Vadim):** `size`/`empty` are trivial —
  implement `__intrin_size__()` for the new vector (O(1), `FX_VEC_SIZE`), `empty`
  = `size==0`. `==`/`<=>`/`string`/`print` are **plain stdlib loops** over the
  just-added `v[i]` and `v.size()` (no special codegen). All land in the same PR
  as the 7 core ops.
- **make-empty / `[]`:** RRB *rejects* the empty literal (`K_normalize.fx:350`);
  for the mutable type empty is the natural default. Add `[]`→empty-`Vector`
  (pinned by annotation) and `TypVarCollection` + `[]`-membership so generic
  collection code accepts it.
- **▸ Поправка (Claude):** `lib/Vector.fx` becomes a *thin* module — most ops
  are compiler-emitted; keep only what genuinely needs stdlib (`string`/`print`,
  maybe `==`/`<=>`). Drop the `init: 't` field (element type now static). If the
  stdlib uses `Vector`, this is two-stage (Step 2a compiler accepts the type;
  Step 2b stdlib uses it).
- Test: port `test/test_vec.fx`'s `vec.*` cases onto the first-class syntax;
  add negative goldens for misuse (e.g. `v[i]=a` on a non-lvalue, empty-`[]`
  without annotation) — new legality checks go in typecheck (caret + `-no-c`).

## Step 3 — slices (adapt from strings)

- **▸ Поправка (Claude):** copy the slice **syntax/typecheck** path from strings,
  but route the runtime to `fx_vec_slice` (already present) — a `Vector` slice is
  a **copy**, not a view like a string slice. `v[a:b]`, `v[a:b:s]`, `v[::-1]`,
  `v[:]`. Border modes `.clip/.wrap/.zero` are free: `FX_VEC_PTR_{CLIP,WRAP,ZERO}`
  already exist in `ficus.h`. Cost note in `test_vec.fx` header already promises
  slice/concat/reverse tests.
- `+`/concat via `fx_vec_concat` (present) — optional in this step.

## Step 4 — iteration + comprehensions

### Comprehensions (the easy half — Vadim's insight holds)

While a comprehension is building the result it has **no user-visible name**, so
nothing can resize/alias it mid-build → writes are pure appends into a growing
buffer, fast, like a string builder. Wire a `ForMakeVector`-analog + a writer:
grow-and-append (`fx_vec_append`, or a `FAST` variant that hoists the capacity
check off the hot path like RRB's `FX_RRB_WRITE_FAST`). No invalidation problem.

### Iteration (the novel, load-bearing half)

`for x <- v` where the body may `push_back`/`resize`/`clear` `v` (or an alias of
it) is **new for Ficus**: arrays are fixed-size, string/rrb are immutable — no
existing iterated container can realloc or shrink under the loop. A grow triggers
`realloc` (moves `data`, frees the old block); a shrink moves `size` below the
cursor. A naive "cache `data`+`size`" iterator → use-after-free / out-of-bounds.

**Design — variant D: read-lock (RESOLVED, Vadim).** `for x <- v` takes a
**read-lock** on the shared header for its duration; a *structural* mutation of a
locked vector throws.

- **Header field:** add `int_ nlocks` to `fx_vechdr_t` (`rc` stays first).
- **Loop brackets:** at loop setup `v->nlocks++` (`FX_VEC_START_READ`), in the
  loop's **cleanup section** `v->nlocks--` (`FX_VEC_END_READ`) — the decrement
  must run on *every* exit: normal, `break`, `continue`, `return`, exception.
  Model on RRB's `fx_rrb_start_read`/read-protocol + Ficus block cleanup (which
  already frees locals on all paths). Nested/aliased reads just stack the
  counter.
- **Hot path = array speed.** Because no realloc/shrink can happen while locked,
  the loop hoists `data`+`size` at entry and binds `x = copy(data[i])` (INCREF)
  with **no per-iteration check**. The check lives in the mutators, which the
  loop body usually doesn't call.
- **Which ops lock-check** (throw if `nlocks>0`): the buffer/size changers —
  `push_back`, `pop_back`, `resize`, `reserve`, `clear` (and in-place concat/
  append). New exn, e.g. `FX_EXN_VecModifiedError`.
- **Which ops do NOT check (Vadim):** `v[i] = a` (`set`) — it replaces an element
  *in place*, changing neither `size` nor the buffer, so it is legal during
  iteration. Likewise all reads (`v[i]`, `back`, `size`) and `slice` (builds a
  new vector).
- **Contract falls out correctly:** the lock is on the *shared header*, so
  `w = v; for x <- v { push_back(w, …) }` throws (aliased structural mutation),
  while the ubiquitous `for x <- v { push_back(result, f(x)) }` is fine
  (`result` is a different header). Self/alias structural mutation is forbidden;
  building a *different* vector is allowed.
- **Escape hatch:** the manual `for i <- 0:size(v) { … v[i] … }` index loop is
  **not** a locked read — the programmer opts out and may mutate at own risk.

**How to verify (the "надо понять как это проверить" part):**

- Structural mutation inside `for x <- v` **throws** `FX_EXN_VecModifiedError`:
  (a) `push_back` (would-realloc), (b) `pop_back`/`resize`/`clear`. Test that the
  exn fires and that `nlocks` is restored afterwards (loop cleanup ran) — a
  follow-up mutation outside the loop must succeed.
- **Allowed, must NOT throw and must be correct:** `v[i] = a` inside the loop;
  `for x <- v { push_back(result, …) }` into a different vector; nested
  `for x <- v { for y <- v { … } }`.
- Run all under `fxtest.py sanitize` — ASan/UBSan is the oracle that the locked
  loop never does the UAF/OOB a naive iterator would; pair with reference-checked
  values (over-reads may not trap). Add a `test/rand/` push/pop/set/iterate fuzz.

## Step 5 — bounds-check elimination for sequential vector access

After comprehensions (Step 4), extend the range-check hoisting pass
(`compiler/K_fast_idx.fx`, 796 LoC) to the new vector. Today it optimizes
**arrays**: for affine indices `alpha*i + beta` (loop-invariant `alpha`/`beta`,
loop index `i`) accessed unconditionally in a `for`/comprehension body, it drops
the per-iteration `CHECK_IDX` and instead checks the index range endpoints once
before the loop — e.g. `for i <- 1:n-1 { dst[i] = (src[i-1]+src[i]+src[i+1])/3.f }`
becomes one pre-loop check on `dst`/`src` bounds, then an unchecked hot loop.

**Soundness hinges on Step 4's read-lock (the mutable twist).** For arrays the
hoist is unconditionally safe — `size` and `data` never change. For a **mutable
vector** the pre-loop check and the cached `data` base are only valid if the
vector cannot be reallocated or shrunk during the loop. That is exactly the
read-lock invariant (variant D), so:

- **▸ Поправка (Claude):** when the pass hoists a vector's check out of a loop,
  it must **read-lock that vector for the loop's duration** (the same
  `nlocks`/`FX_VEC_START_READ`/`END_READ` brackets as `for x <- v`). Under the
  lock, structural mutation (`push_back`/`resize`/…) throws, so hoisted `size`
  stays valid and the cached `data` base cannot dangle — the optimized loop
  reads `v->data` once, like the array path. This generalizes D from "iterating
  a vector" to "any loop from which a vector's range check was hoisted."
- `dst[i] = …` writes are still fine under the read-lock (`set` doesn't
  lock-check — it changes neither `size` nor the buffer), so the stencil/scatter
  pattern above optimizes fully.
- The pass must **distinguish container kind**: `CTypArray` → hoist, no lock
  (fixed size); new `CTypVector` → hoist **+ lock**; `CTypRRBVec` is a tree, not
  affine-indexable in O(1) — leave as-is.
- **Fallback:** if locking a given loop is not expressible (e.g. the vector isn't
  a simple loop-invariant binding the pass can bracket), skip the hoist for that
  vector and keep per-iteration checks — correctness over speed.

Verify: the stencil example produces identical results checked vs. hoisted (T2
corpus O0-vs-O3 already exercises `K_fast_idx`); a structural mutation inside a
hoisted loop throws (as Step 4); ASan clean (no dangling `data` base).

## Decision log (all resolved)

1. ~~Lowercase `vector` reassignment~~ — **RESOLVED (Vadim):** yes, `vector`
   becomes the mutable type; `rrbvec` takes the old spellings.
2. ~~Iteration semantics~~ — **RESOLVED (Vadim):** variant **D**, read-lock
   (`nlocks` in the header; structural mutators throw when locked; `set` and
   reads don't check). See Step 4.
3. ~~Constructor/mangling assignment~~ — **RESOLVED (Vadim):** RRB →
   `*RRBVec` + mangling `"W"`; new mutable type → `Typ/KTyp/CTypVector` +
   mangling `"V"`.
4. ~~Scope of Step 2's op set~~ — **RESOLVED (Vadim):** `size`/`empty`
   (`__intrin_size__`) and `==`/`<=>`/`string`/`print` (stdlib loops over
   `v[i]`/`v.size()`) are in-scope for the same PR as the 7 core ops.
