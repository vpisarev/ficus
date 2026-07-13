# newvec-2 — `vector` performance & ergonomics

Follow-up work package to **newvec-1** (the first-class mutable `vector`, merged
as PR #41). This branch makes the vector faster (inline push/pop, `realloc`
growth) and more ergonomic (a `vector()` constructor family, in-place slice
assignment, a unified `append` family, from-end `back`), and consolidates the
API. Everything is on branch `newvec-2`; nine feature/doc commits, all green
against the full fxtest ladder + ASan/UBSan + the self-hosting fixpoint.

## What changed, by area

### 1. `vector()` constructor family (Builtins.fx)
`vector()` now mirrors `array()` instead of the old `Vector.make`:

| form | meaning |
|---|---|
| `vector()` / `vector(capacity=n)` | empty; optionally pre-reserved to `n` |
| `vector(n, x)` | `n` copies of `x` |
| `vector(a: 't [])` | from an array |
| `vector(l: 't list)` | from a list |
| `vector(v: 't rrbvec)` | from an rrbvec |
| `vector(s: string)` | `char vector` from a string |

`Vector.make` is gone everywhere (compiler symbol tables, NN, examples, tests).
`map`/`mapi`/`foldl` are removed too — comprehensions (`vector(for x <- v {..})`,
`vector(for x@i <- v {..})`) and `fold` express them directly. `capacity()` is
now `@nothrow`.

The `~capacity` overload needs `reserve`, which lives in Vector.fx (compiled
*after* Builtins). To keep the whole family in Builtins (a `vector()` split
across two modules confuses overload resolution), the reserve primitive was
factored into `Builtins.__vec_reserve__` (an `@ccode` wrapper over
`fx_vec_reserve`); `Vector.reserve` is now a thin wrapper over it. The
keywordless `vector()` was removed because it is ambiguous against the
all-defaulted-keyword `vector(~capacity=0)`, which subsumes it.

### 2. `fx_vec_reserve` grows via `fx_realloc`
The malloc + memcpy + swap + free dance became a single `fx_realloc`. Same pure
move of the live bytes (growth needs no copy-constructors), same NULL-first-grow
handling (realloc-of-NULL is malloc).

### 3. In-place slice assignment `vec[a:b:delta] = rhs`
Previously `vec[a:b] = rhs` silently built a discarded slice copy (a no-op). Now
it mutates in place:

- `vec[a:b] = vector(...)` — replace a contiguous range; sizes may differ (the
  tail is shifted, the removed elements freed).
- `vec[a:b:k] = []` — delete the selected elements (any step); survivors compacted.
- `vec[:] = []` — clear (a real allocated, still-pushable vector).
- `vec[i:i] = vector(...)` — insert at `i`.

A strided replace with a **non-empty** vector (`delta != 1` and `rhs != []`) is a
**compile error** with a caret (`test/negative/507`).

Pipeline: `K_normalize` intercepts `ExpAssign(ExpAt(vec, [range]), rhs)` for a
vector LHS and lowers it to `KExpIntrin(IntrinVecSplice, …)` (the legality check
lives here, so it carries a caret); `C_gen_code` emits `fx_vec_splice(vec, a, b,
delta, mask, rhs)`, the missing-bound mask computed exactly as for a slice read.
The runtime `fx_vec_splice` frees the removed elements, moves survivors with a
pure `memmove`, deep-copies `rhs` in (`copy_elem`), reserves up front (OOM leaves
the vector intact), and zeroes any shrink-vacated tail so no aliased pointer
lingers past the new size.

### 4/5. Inline `push_back` / `pop_back`
Both are now `@inline` wrappers over new intrinsics `__intrin_push__` /
`__intrin_pop__`, so the common case is emitted at the call site:

- POD element types → `FX_VEC_PUSH_BACK_FAST` / `FX_VEC_POP_BACK_FAST`: a raw slot
  write / bare size-decrement on the in-capacity, not-being-iterated path.
- Complex element types → `FX_VEC_PUSH_BACK` / `FX_VEC_POP_BACK`: always through
  the runtime (which runs the element copy/free).

POD-ness is decided at C-gen from `get_ktprops(elem).ktp_complex`; the catch
label is the call site's. Both intrinsics are marked **impure** in
`K_remove_unused` — otherwise the void-result push/pop calls (and the loops
around them) would be dead-code eliminated.

### 6/7. `append` family and `concat`
Every void appender is now spelled `append`, differing only by source kind:

    push(v, x: 't): int          // adds one element, returns its index
    append(v, elem: 't): void    // synonym for push_back
    append(v, arr: 't []): void
    append(v, src: 't vector): void

The three argument kinds (`'t` / `'t []` / `'t vector`) never overlap for a
resolved call, so the family is unambiguous — even for vectors-of-vectors, where
appending one `'x vector` element vs concatenating an `('x vector) vector` pick
different overloads. `Vector.concat` (`('t vector) []` and `('t vector) vector`)
builds the result via `vector(capacity=total)` and bulk `append`s.

The runtime `fx_vec_concat` was dropped: it derived element metadata from its
inputs, which is unavailable when every input is empty. Concatenation now lives
entirely in Ficus over `fx_vec_append`; the result is created via `[]` (which
carries `'t`'s metadata), so an all-empty input still yields a real allocated
vector. A future `fx_vec_compose` will cover heterogeneous compose.

### Polish
- `back()` is now `@inline fun back(v) = v[.-1]` — the from-end index `[.-1]`
  desugars to `v[__intrin_size__(v) - 1]` and already works for vectors
  (`IntrinGetSize` supports them). The hand-written `@ccode` is gone.
- Operating on an empty vector now consistently throws **`OutOfRangeError`**, not
  `SizeError`: `fx_vec_pop_back`'s empty case, `back()`, and `pop()` all agree
  with element access (`SizeError` stays only for genuine negative-size errors in
  make/resize/append).

## API summary (Vector + Builtins)

- Construct: `vector()`, `vector(capacity=n)`, `vector(n, x)`, `vector(arr)`,
  `vector(list)`, `vector(rrbvec)`, `vector(string)`, `vector(for …)`.
- Grow/shrink: `push_back`, `pop_back`, `push` (→ index), `append` (elem/array/
  vector), `resize`, `reserve`, `clear`, `assign`.
- Read: `v[i]`, `v[.-k]` (from end), `back`, `size`, `empty`, `capacity`,
  borders `v.clip/wrap/zero[i]`, slices `v[a:b:k]` (copy).
- Mutate slice: `v[a:b] = vector(...)`, `v[a:b:k] = []`, `v[:] = []`.
- Bulk: `concat(('t vector) [])`, `concat(('t vector) vector)`.
- `==`, `<=>`, `string`, `print`, comprehensions, `for`-iteration (variant-D
  read-lock), `array(v)`.

## Testing

- `test/test_vec.fx`: the `fcvector` suite grew to **13** tests, adding
  `pushpop_edge`, `splice`, `append` (and extending `concat`, `str`).
- `test/negative/507_vec_strided_replace`: golden caret diagnostic for the
  strided-replace error.
- Full ladder green (`fxtest.py all`: unit + negative + ir + cfold +
  corpus O0/O3), ASan+UBSan clean, self-hosting fixpoint holds (33 bootstrap
  modules regenerated across the intrinsic commits).

## Compiler bug found (fenced, not fixed)

**FB-027 — `ignore(coll[i])` swallows `OutOfRangeError`.** An element read whose
result does not escape is dead-code-eliminated together with its bounds check, so
`ignore(e[.-1])` on an empty vector/array does not throw. Root cause:
`pure_kexp_` in `K_remove_unused.fx` does not mark `KExpAt` impure. Affects
array/vector/string/rrbvec at both `-O0` and `-O3`. Fix direction noted in
`docs/found_bugs.md` (mark a bounds-checked `BorderNone` `KExpAt` impure; border
reads stay pure). Surfaced while simplifying `back` to `v[.-1]`.

## Notes for a future pass

- The 2-fold `concat` (`fold r = vector(capacity=total) for v { r.append(v) }`)
  was tried and dropped: an in-place `append` accumulator triggers the fold-1
  "accumulator never assigned" warning, and an unpinned `r` makes `r.append(v)`
  ambiguous between the element and vector overloads. Both could be candidates
  for future fold-1 / resolver refinement.
- `fx_vec_compose` for heterogeneous vector construction (`[\a, \b, 3.14, \c]`)
  remains to be implemented; `fx_vec_concat` was removed in anticipation of it.
