# newvec-2: vector performance & ergonomics

Follow-up to newvec-1 (#41). Makes the first-class mutable `vector` faster and
nicer to use, and consolidates the API. No change to program semantics beyond
the new features; all green on the fxtest ladder + ASan/UBSan + self-hosting
fixpoint.

## Highlights

- **`vector()` constructor family** (mirrors `array()`): `vector()`,
  `vector(capacity=n)`, `vector(n, x)`, `vector(arr)`, `vector(list)`,
  `vector(rrbvec)`, `vector(string)`. `Vector.make` removed everywhere;
  `map`/`mapi`/`foldl` dropped (comprehensions + `fold` replace them);
  `capacity()` is `@nothrow`.
- **Inline `push_back` / `pop_back`** via `__intrin_push__` / `__intrin_pop__`:
  a raw slot write / size-decrement inline for POD elements on the fast path,
  the runtime (with element copy/free) otherwise.
- **In-place slice assignment**: `vec[a:b] = vector(...)` (grow/shrink),
  `vec[a:b:k] = []` (delete), `vec[:] = []` (clear), `vec[i:i] = ...` (insert).
  Was silently a no-op before. Strided replace with a non-empty vector is a
  compile error (caret).
- **`fx_vec_reserve` uses `realloc`** instead of malloc+memcpy+free.
- **`append` family** unifies the void appenders: `append(elem)` /
  `append(arr)` / `append(vector)`; `push(elem): int` keeps its index return.
  `concat(('t vector)[])` and `concat(('t vector) vector)` added.
- **`back()` = `v[.-1]`** (from-end index); empty `back`/`pop_back`/`pop` now
  throw `OutOfRangeError` (was `SizeError`), consistent with element access.
- **fold-1 "accumulator never assigned" warning removed** — it false-positived on
  a reference-mutable accumulator updated in place (`fold r = vector(...) for v {
  r.append(v) }`); the void fold body already makes the old-style value-yielding
  body a type error, so the warning was redundant.

## Compiler changes

New intrinsics `IntrinVecPushBack` / `IntrinVecPopBack` / `IntrinVecSplice`
through Parser → typecheck → K-normalize → C-gen, all marked impure in
`K_remove_unused`. Runtime: `FX_VEC_PUSH_BACK*/POP_BACK*` macros,
`fx_vec_pop_back`, `fx_vec_splice`, `fx_realloc`-based grow; `fx_vec_concat`
removed. Bootstrap regenerated; fixpoint holds.

## Tests

`fcvector` suite → 13 tests (`pushpop_edge`, `splice`, `append`, extended
`concat`/`str`); negative golden `507_vec_strided_replace`. Full ladder green;
ASan+UBSan clean.

## Found (fenced, not fixed)

**FB-027** — `ignore(coll[i])` swallows `OutOfRangeError`: a bounds-checked
element read with an unused result is DCE'd along with its check (`KExpAt` not
marked impure in `pure_kexp_`). Affects array/vector/string/rrbvec. Fix
direction recorded in `docs/found_bugs.md`.

## Commits

- vector() constructors in Builtins; drop Vector.make/map/mapi/foldl
- fx_vec_reserve grows via fx_realloc
- Vector.append + Vector.concat (multi-vector concatenation)
- inline push_back/pop_back via __intrin_push__/__intrin_pop__
- in-place vector slice assignment (delete / replace / clear)
- unify the void appenders under append(); push() stays index-returning
- back() = v[.-1]; empty pop_back/back throw OutOfRangeError
- docs(found_bugs): FB-027 — ignore(coll[i]) swallows OutOfRangeError
- vector(~capacity) constructor; reserve over a Builtins primitive
- remove the fold "accumulator never assigned" warning
