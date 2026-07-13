# newvec-1: the temporary mutable `Vec.t` (implementation report)

**Status:** preliminary, WIP (branch `newvec-1`, commit `2d462bb` + cleanup).
Tests: `test/test_vec.fx`, 6 cases green (4 legacy RRB `vector.*`, 2 new
`Vec.*`).

**Predecessor doc:** `docs/rrb_census.md` sized a *different* plan — promoting
`Dynvec.t` to a first-class immutable-symmetric `vector`. The design diverged:
what was actually built is a **temporary, runtime-general, mutable** container
implemented **entirely in the stdlib**, with **no new compiler type node**. This
report documents that as-built state, why it is temporary, and the compile-time
machinery a permanent version would lean on.

## 0. Governing invariant (Vadim): O(1) assignment

Ficus holds one load-bearing rule that dictates every container's representation:

> **`a = b` — assignment, and equally parameter passing, value return, and
> storing into a container or a closure — must be O(1) for any container whose
> element count is not known at compile time.**

Consequences, and why the whole family looks the way it does:

- O(1) copy ⟹ copy is a **refcount INCREF, never a deep clone**. Hence all four
  containers use an INCREF `ctp_copy` (`fx_copy_arr`/`fx_copy_str`/`fx_rrb_copy`/
  `fx_copy_ptr`) and therefore have **shared, reference (aliasing) semantics** —
  a mutation seen through one binding is seen through every other. There is no
  copy-on-write (that would push either the `=` or the first write to O(n)); an
  independent copy is an explicit operation (`.copy()` / a slice).
- For **`Vec`** specifically, "O(1) shared copy **and** it can grow" is what
  forces the heap-allocated, pointer-shared header (§1b, §3.1): the mutable
  `size`/`capacity`/`data` must sit in a single shared block, or cheap sharing
  of a growing buffer is impossible.
- A **slice** (or comprehension, or `+`) is O(n) and does *not* violate the
  rule: it constructs a *new* container, it is not an `a = b` on an existing one.

## 1. What was built

Three vector-like things now coexist; keep them distinct:

| # | Thing | Backing | Mutable? | Compiler type | Reached via |
|---|---|---|---|---|---|
| 1 | built-in `vector` | `fx_rrbvec_t` (RRB tree, `rrbvec.impl.h`, 1255 LoC) | no (persistent) | `TypVector`/`KTypVector`/`CTypVector`, mangling `"V"`, ctprops → `fx_rrb_{make,copy,free}` | first-class syntax (`v[i]`, `vector(for…)`, `+`, slices) |
| 2 | **new `Vec.t`** (this WP) | `fx_vec_t` (`fx_vechdr_t`, contiguous doubling buffer, `vec.impl.h`, 256 LoC) | **yes** (in-place `push_back`/`pop_back`/`set`/`resize`) | **none** — a plain `class 't t { v: cptr; init: 't }` (future first-class ctprops in §3.1) | `Vec.make`/`.at`/`.set`/… stdlib calls over the `cptr` |
| 3 | `Dynvec.t` (to be replaced) | `'t []` + count, pure Ficus (`Dynvec.fx`, 89 LoC) | yes | none (ordinary class) | `Dynvec.create`/`push`/`pop`/… |

**Key fact (Vadim):** #2 added **no compiler type**. `fx_vec_t`/`fx_make_vec`/
`fx_vec_slice`/… (`vec.impl.h`) are runtime-only; the compiler sees `Vec.t` as
an opaque `cptr`-holding class and never emits `fx_vec_*` calls itself — every
call is hand-written `@ccode` in `lib/Vec.fx`. (The `CTypVector` /
`std_fx_*_vec` machinery in the compiler still binds to the **RRB** engine —
`C_gen_std.fx:136-138` → `fx_rrb_{free,copy,make}` — and is unrelated to #2.)

## 1b. What distinguishes `Vec` from array / string / rrb `vector`

All four are `ctp_complex=true, ctp_scalar=false` refcounted containers, but
`Vec` sits on the other side of one structural axis:

| | array `fx_arr_t` | string `fx_str_t` | rrb `vector` `fx_rrbvec_t` | **new `Vec` `fx_vec_t`** |
|---|---|---|---|---|
| `ctp_ptr` | false | false | false | **true** |
| `ctp_pass_by_ref` | true | true | true | **false** |
| header lives | **inline in the variable** (by value) | inline | inline | **on the heap; the variable is a pointer** |
| `ctp_copy` | `fx_copy_arr` (shallow struct + INCREF data) | `fx_copy_str` | `fx_rrb_copy` | **`fx_copy_ptr` (INCREF the whole header)** |
| `ctp_free` | `FX_FREE_ARR` | `FX_FREE_STR` | `fx_rrb_free` | **`FX_FREE_VEC`** |
| mutability | elements yes, **size fixed** | **immutable** | **immutable / persistent** | **elements + size, in place** |
| layout | contiguous, N-D, strided | contiguous char32 | 32-way RRB tree + tail | contiguous 1-D, `size ≤ capacity` |
| `[i]` access | O(1) | O(1) | O(log n) | O(1) |
| grow | — | — | O(log n) persistent | amortized O(1) `push_back` |
| slice | **view** (strided) | **view** | new persistent (structural sharing) | **copy** |
| per-elem info | `fx_arrinfo_t` (inline) | none (char = POD) | `fx_rrbinfo_t` | `fx_vecinfo_t` (in the shared header) |

**The crux.** `Vec`'s value is a **pointer to a shared heap header**
(`ptr=true, pass_by_ref=false`, copy = INCREF), whereas array/string/rrb are
**inline structs passed by reference** (`ptr=false, pass_by_ref=true`, copy =
type-specific shallow). This is *forced* by `Vec`'s defining combination —
**mutable + growable + shared**: growth rewrites `size`/`capacity`/`data` in the
header, and for every alias to observe the growth the header must be single and
shared, i.e. behind a pointer.

- **Array** is mutable-in-elements but its header is by-value — which is exactly
  *why it cannot grow*: a realloc in one alias's inline header would be
  invisible to the others.
- **String and rrb** sidestep the whole problem by being **immutable**, which is
  also why their slices are cheap (a view / structural sharing). `Vec`'s slice
  must be a **copy** — a view would alias and an in-place mutation would corrupt
  the source (`vec.impl.h:202-207`). That copy is the price of mutability.
- Per-element lifecycle info (`free/copy/elemsize`) is carried at runtime by
  **array, rrb, and Vec** alike — so `Vec`'s "runtime-general element handling"
  is not unique; it mirrors arrays. What is unique to `Vec` is `ptr=true`, the
  whole-object INCREF copy, and the copying slice.

Versus the current stopgap: today `Vec.t` is a `{cptr, init}` *struct*
(`pass_by_ref=true` — still in the inline-struct camp, with an extra `cptr`
indirection and a sample-element `init` field). First-class `Vec` collapses it
to a bare `fx_vec_t` pointer and drops `init` (element type known statically).

## 2. The runtime-general design and its cost

Because the element type is *not* known to the compiler at the `@ccode`
boundary, `Vec.t` carries per-element lifecycle info **at runtime**, mirroring
`fx_arr_t`:

```c
typedef struct fx_vecinfo_t {
    fx_free_t free_elem;   // 0 for POD
    fx_copy_t copy_elem;   // 0 for POD
    size_t    elemsize;
} fx_vecinfo_t;
```

To populate it, `Vec.fx` uses three stopgaps, each flagged in-source:

- **`get_vec_data(val0)`** builds a throwaway `array(1, val0)` and reads
  `dim[0].step` (elemsize), `free_elem`, `copy_elem` back out of the array
  header — i.e. it borrows the array runtime's already-computed element props
  *at runtime* instead of the compiler supplying them.
- **`(val0, true)` tuple hack** — elements are passed to `@ccode` wrapped in a
  tuple so they are always passed *by reference*, sidestepping the unknown
  by-value/by-ref calling convention (`Vec.fx:48-53`: *"when vector/vec is
  first-class type in Ficus, this trick is not needed anymore, because we know
  element type at compile time"*).
- **runtime `if (!copy_f) memcpy(); else fx_copy_arr_elems()`** branches in
  `at`/`back`/`set` (`Vec.fx:155,193,217`: *"in real implementation that should
  be compile-time decision"*), plus the symmetric `free_elem` branch in
  `pop_back`/`resize`.

Other "an efficient version wouldn't do this" markers:

- `make`/`map`/`mapi` **initialize** the output vector (`make(n, v.init)`) though
  a comprehension-style build should fill an uninitialized buffer of exact size
  (`Vec.fx:130-132,301,312`).
- `push_back(v, elem)` is the generic append; the comment notes comprehensions
  must instead pre-size and `set` without size/capacity/index checks.
- `fx_vec_slice` always **copies** (Python-list semantics, never a view) — this
  is a deliberate semantic choice for a mutable container, not a temporary
  shortcut (`vec.impl.h:202-207`).

Cost of generality vs. a specialized backend: one indirect call per element via
`copy_elem`/`free_elem` (vs. inlined copy or a bulk `memcpy` for POD), a
`copy_f == 0` branch on every access, the `array(1,·)` allocation per `make`,
and lost devirtualization/vectorization the compiler could do if it emitted the
concrete `fx_copy_*`/`fx_free_*` (or nothing, for scalars) directly.

## 3. Compile-time analogue: `ctprops_t` / `get_ctprops`

`fx_vecinfo_t` is the **runtime twin of `ctprops_t`** (`compiler/C_form.fx:95`,
computed by `compiler/C_gen_types.fx:get_ctprops`). A first-class `Vec` would
read the element type's props at compile time instead of at runtime:

| runtime `fx_vecinfo_t` (per-element, dynamic) | compile-time `get_ctprops(elemtyp)` (static) |
|---|---|
| `elemsize` | element ctyp size |
| `copy_elem` (fn ptr, may be 0) | `ctp_copy.fn` (`noid` ⇒ trivial) |
| `free_elem` (fn ptr, may be 0) | `ctp_free.fn` (`noid` ⇒ trivial) |
| implicit "is POD?" test at runtime | `ctp_scalar` known at compile time |
| — (n/a) | `ctp_pass_by_ref` resolves the tuple-by-ref hack statically |

### 3.1 Container-level ctprops for first-class `Vec`

When `Vec` becomes a compiler type it gets its own `get_ctprops` case (a new
arm next to `CTypCSmartPtr`/`CTypVector`), with this signature (Vadim):

```
ctp_scalar      = false
ctp_complex     = true
ctp_make        = [ <constructor> ]
ctp_free        = (FX_FREE_VEC,  fx_free_vec)   // macro + destructor that also frees elements
ctp_copy        = (FX_COPY_PTR,  fx_copy_ptr)   // INCREF the whole header — NOT a deep clone
ctp_pass_by_ref = false                         // the value is itself a pointer → passed by value
ctp_ptr         = true
```

Read it against §1b: `ptr=true`/`pass_by_ref=false` because **the value is a
pointer to a heap-allocated `fx_vechdr_t`** — a `Vec` can change its own size,
so its header (holding `size`/`capacity`/`data`) cannot be carried inline by
value; it must live on the heap and be shared behind a pointer. `copy` is the
generic `fx_copy_ptr` (INCREF) precisely because the whole object is one
refcounted block with `rc` as its first field — so copy-on-assign shares the
buffer (mutations alias-visible), exactly like arrays, rather than cloning.
`free` is the type-specific `FX_FREE_VEC`/`fx_free_vec` (DECREF, and at `rc==0`
free every element via `free_elem` then the buffer). This is the same ctprops
*shape* as `CTypCSmartPtr` — unsurprising, since the current stopgap literally
wraps a `cptr`.

**Two structures per container.** The backend needs props for *both* levels:
`get_ctprops(CTypVector et)` for the container itself (its own
`fx_..._{free,copy}`, `ctp_pass_by_ref`, `ctp_complex`) **and**
`get_ctprops(et)` for the elements (to copy/free each on resize/slice/concat).
The RRB path already does exactly this; the temporary `Vec.t` pushes the
element half into `fx_vecinfo_t` at runtime. A permanent `Vec` would:

1. know `et` statically → drop `get_vec_data`, the `(val0,true)` tuple, and the
   `array(1,·)` allocation;
2. emit specialized copy/free inline (or `memcpy` when `ctp_scalar`), dropping
   the `copy_f == 0` runtime branch;
3. optionally use the already-present `FX_VEC_PTR{,_CLIP,_WRAP,_ZERO}` macros
   (`ficus.h:1053-1077`, the mutable analogue of `FX_RRB_ELEM*`) for `v[i]` and
   border access, i.e. real `[]` sugar instead of `.at(i)`.

That is the same shape the RRB `vector` already has (§2/§4 of
`rrb_census.md`): a compiler type node at every layer + `TypVarCollection`/`[]`
membership + access/iterate/build protocols. The open design question from the
census still gates it — the **mutation model** is what a mutable `Vec` has and
RRB never did: assignment shares the buffer (INCREF, §3.1), so `v[i] = x` and
`push_back` are visible through every alias. This is aliasing-by-reference under
the atomic-refcount runtime (same as arrays), *not* copy-on-write; a
value-semantics `Vec` would instead need an explicit `.copy()`.

## 4. Cleanup already applied on this branch

- Removed a leftover debug `printf` in `fx_vec_slice` (`vec.impl.h`) that printed
  `start=… end=… delta=…` on every slice (visible in `vec.str`).

## 5. Known rough edges (WIP, not yet addressed)

- `pop_back(v, size: int)` takes an unused `size` param that is immediately
  shadowed by a local `int_ size = vec->size;` — the parameter is dead and
  misleading; should be `pop_back(v)`.
- `map`/`mapi`/`make` initialize the result buffer (see §2).
- No `[]`/iterator/border sugar yet — access is `.at(i)`/`.set(i,x)`.
- `Vec.t` is not a collection for `TypVarCollection`/`[]` purposes.

## 6. Next steps (facts; sequencing is Vadim's)

- **Migration target:** `Dynvec.t` has 5 consumers — `compiler/{Ast,C_form,
  K_form}.fx`, `lib/NN/{BufferAllocator,ConstFold,FromOnnx,FuseBasic,OpDetect}
  .fx`, and `examples/knucleotide.fx`. Porting them to `Vec.t` (API map:
  `create`→`make`, `push`/`do_push`→`push_back`, `pop`/`do_pop`→`pop_back`,
  `top`→`back`, `size`/`empty`/`clear` parity, `v.data[i]`→`.at`/`.set`) then
  deleting `Dynvec.fx` is the concrete "replace Dynvec" work.
- **Promotion (later):** turn `Vec` first-class per §3 to erase the runtime
  generality cost; reuse `FX_VEC_PTR*` and the `get_ctprops` two-level lookup.
