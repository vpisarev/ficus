# newvec-1 — first-class mutable `vector` (final report)

**Outcome.** Ficus gains a first-class, mutable, growable `vector` (STL-vector /
Python-list). It replaces **both** stopgaps it was built to retire: the temporary
`Vec.t` class and the compiler's `Dynvec.t`. The mutable vector is now the default
container spelled `vector`; the old immutable RRB vector is `rrbvec`. The compiler
builds itself with its symbol tables backed by the new vector (determinism
byte-identical).

This report is the as-built companion to `docs/newvec1_plan.md` (the 5-step plan)
and `docs/newvec1_report.md` (the earlier stopgap analysis, now historical).

## 1. The design, in one screen

Governed by one invariant (`language_changes_brief.md` §1.8): **`a = b`** —
assignment, parameter passing, return, storing into a container/closure — **is
O(1)** for any container of statically-unknown element count. That forces copy =
refcount INCREF (never a deep clone), hence shared reference (aliasing) semantics.

What makes `vector` unique among the containers: it can **change its own size**, so
its header (`size`/`capacity`/`data`) cannot be carried inline — it lives on the
heap and the value is a **pointer** to it (`ctp_ptr=true, ctp_pass_by_ref=false`,
copy=`fx_copy_ptr` INCREF, free=`fx_free_vec`). This is exactly the **cptr model**:

| | array `fx_arr_t` | string | rrbvec | **vector `fx_vec_t`** |
|---|---|---|---|---|
| representation | inline struct (by-ref) | inline | inline | **pointer to heap header** |
| mutability | elements; size fixed | immutable | immutable/persistent | **elements + size, in place** |
| copy (O(1)) | shallow + INCREF | INCREF | INCREF root | **INCREF the header** |
| slice | view | view | new persistent | **copy** |

Consequences that recur throughout the implementation:
- **empty/default = NULL.** A zero-initialised vector variable is a NULL pointer
  (safe to free if a block's exception cleanup runs before it is constructed). All
  accessors are NULL-aware (`FX_VEC_SIZE(NULL)==0`). An explicit `[]` is different:
  it *allocates* a real empty vector (so `val v: int vector = []; v.push_back(5)`
  works). `[]` is built **in place** into the destination (no temp/copy) via
  `get_dstexp`; an atom-context `[]` goes into a registered temp.
- **slice is a copy** (Python semantics) — a view would alias under mutation.
- **element metadata is compile-time.** `fx_vecinfo_t{free_elem,copy_elem,
  elemsize}` is the runtime twin of `ctprops_t`/`get_ctprops(elemtyp)`. First-class
  construction reads it from the compiler, so the runtime hacks the stopgap needed
  (`get_vec_data`, the `(val0,true)` tuple, `array(1,·)`) are gone.

## 2. What was built, by step

Each step landed green through the full fxtest ladder + sanitize + the
self-hosting fixpoint; every stage that touches the stdlib used the two-stage
bootstrap playbook (teach the running compiler the new spelling, regen, then use
it).

- **Step 1 — rename immutable `vector`→`rrbvec`.** Frees the name for the mutable
  type. Constructors `Typ/KTyp/CTypVector`→`*RRBVec`, mangling `"V"`→`"W"`,
  `lib/Vector.fx`→`lib/Rrbvec.fx`. Two-stage (Builtins uses the surface type).
- **Step 2 — first-class type + operations.** New `Typ/KTyp/CTypVector` (mangling
  `"V"`, C type `fx_vec_t`) at every stage; `get_ctprops` §3.1 arm; `TypVarCollection`
  + `[]`. Element read `v[i]` (FX_VEC_ELEM/CHKIDX + border modes), write `v[i]=a`
  (fell out for free — `FX_VEC_ELEM` is a C lvalue), and the ops
  push/pop/back/resize/reserve/clear/assign/capacity/make/array/`==`/`<=>`/string/
  print. `==/<=>/string/print` live in Builtins (see FB-025).
- **Step 3 — slices.** `v[a:b]`, arbitrary stride `v[a:b:k]`, reverse `v[::-1]`,
  flatten `v[:]` → `fx_vec_slice` (a copy). Unlike rrbvec (±1 only), the vector
  supports any stride.
- **Retire the stopgap.** Deleted the temporary `Vec.t`; **auto-imported** the
  `Vector` module so the mutable vector is the default (no `import Vector`).
- **Step 4 — comprehensions + iteration.** Decomposed (Vadim) into: **(4a)**
  write-comprehension `vector(for…{…})` — mirrors the ARRAY writer (exact-size
  `fx_make_vec(n,n,…)`, contiguous `dstptr`), NO realloc; break/continue work for
  free because the write+increment sit in the body and the final size is
  `dstptr - data`. **(4b)** iteration `for x <- v` with the **variant-D atomic
  read-lock**: `nlocks` in the header; `for x <- v` brackets FX_VEC_START_READ /
  END_READ; structural mutators throw `VecModifiedError` while locked; `set`/reads
  don't. The lock is scoped by wrapping the for-statement in a dedicated block
  whose once-run cleanup releases it on every exit (the pitfall: putting END_READ
  in the per-iteration loop-body cleanup runs it per element and corrupts the
  count). Aliased mutation is caught because the lock is on the shared header.
- **stdlib rewrite.** With comprehensions/iteration available, the vector stdlib
  was rewritten off index loops: `[for x <- v {…}]`, `for x@i <- v`, `all(for
  xa<-a, xb<-b {…})`, `fold … for x <- v`.
- **Step 6 — migrate off Dynvec.t.** The compiler's symbol tables (`all_names`,
  `dm_table`, `all_idks`, `all_idcs`) and lib/NN + examples/knucleotide moved to
  `'t vector`. The compiler self-hosts with vector-backed tables. `lib/Dynvec.fx`
  deleted.

Note: **`K_fast_idx` was deliberately NOT extended to vectors.** Hoisting a
vector's bounds check would require the optimizer to insert a lock, which would
make a mutate-during-loop program throw at -O3 but not -O0 — an O0/O3 divergence
the T2 differential forbids. Locks are placed by semantics only; the fast path
already lives in the iterator.

## 3. Runtime

`runtime/ficus/impl/vec.impl.h` (fx_make_vec/reserve/resize/append/concat/slice/
free) + `ficus.h` (`fx_vechdr_t` with atomic `nlocks`, FX_VEC_SIZE/PTR/ELEM/CHKIDX
+ border variants, FX_VEC_START_READ/END_READ/CHECK_UNLOCKED, FX_FREE_VEC). New
builtin exception `VecModifiedError` (`FX_EXN_VecModifiedError=-30`). Several
`memset`/`memcpy` guards for the empty (data==NULL, size 0) case that UBSan flags.

## 4. Testing

`test/test_vec.fx` (first-class vector): access, write, ops, slice, comprehension,
break/continue, iteration, readlock. `test/test_rrbvec.fx` (the immutable RRB tests,
split out). Every feature verified for POD and complex (string) elements under
ASan+UBSan; the read-lock verified across break / caught-exception / out-of-range
/ deep-unwind / nested-iteration / aliased-mutation. No regression in
array/list/rrbvec comprehensions or any for-loop (T2 corpus differential).

## 5. Compiler bugs found (fenced, not fixed — `docs/found_bugs.md`)

- **FB-024** order-dependent generic-return inference pollution (`Vec.mapi` etc.);
  worked around by annotating the result.
- **FB-025** resolver internal error compiling a generic container operator's
  element compare in a large-overload scope; worked around by defining the vector
  `==/<=>/string/print` in Builtins (few overloads in scope).
- **FB-026** cannot bind a variable in an or-pattern (`A(c) | B(c)`); split the arms.

## 6. Status

Feature-complete; branch `newvec-1`. Fixpoint holds, rebuild byte-identical, full
ladder green, sanitize clean. Both stopgaps removed. The mutable vector is the
default `vector`.
