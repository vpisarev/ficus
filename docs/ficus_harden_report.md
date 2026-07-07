# Brief #2 — Ficus Compiler Hardening: final report

Branch `harden-1` (off `test-infra`). All FB-001..006, 008 fixed; FB-007 out of
scope (missing spec); FB-009/010 discovered while hardening. Full ladder green,
bootstrap regenerated twice (its own commits). Nothing pushed.

## Per-WP summary

| WP | Bug | Fix (files) | Test / unfence |
|----|-----|-------------|----------------|
| A | FB-008 non-deterministic `.c` | `C_pp.fx` (proto param names), `Compiler.fx` `k_skip_some` (keep skipped bodies) | `fxtest determinism` (nightly) |
| B1 | FB-003 border on plain arrays | `ficus.h` 1D macros (`__idx0__`→`__idx__`, `))`→`})`), `C_gen_std.fx` (register `FX_STR_ELEM_WRAP`) | `array.border_matrix`, T5 border on arrays |
| B2 | FB-004 empty strided slice | `array.impl.h` `fx_subarr` (`total==0` guard) | `array.empty_slice`, T5 slice empty ranges |
| B3 | FB-005 `.wrap[-n]` over-read | Euclidean modulo in `rrbvec.impl.h`, `ficus.h` (array+string) | T5 border `[-3n,3n)` incl. `-n` |
| B4 | FB-001 `-c++` interface codegen | `C_gen_fdecls.fx` explicit `(void*)` casts | `-c++` smoke green, `cpp_xfail` removed |
| B5 | (sanitizer leg) → FB-009 | `array.impl.h` `fx_next_slice` `(ptrdiff_t)` casts | `fxtest sanitize` (nightly) |
| C | FB-002 uint64 folding | `K_cfold_dealias.fx` `ConstInt:(int64,bool)` unsigned-aware `>>`/`/`/`%`/cmp/conv | `fxtest cfold` (in `all`); `lsr()` removed from RandUtil |
| D | FB-006 nested comprehension | `C_gen_code.fx` map body reads back `dstexp_r` | `array.nested_comprehension`, T5, T4 IR snapshot |

## WP-A — the key reframing (with evidence)

The brief assumed FB-008 made *full* builds churn. It does not: two from-scratch
builds of the compiler (clean `Ast.fx` vs one extra builtin id) produce
**byte-identical `.c` for all 52 other modules** (`fxtest determinism --rebuilds`).
So bootstrap regeneration — a full build — was never the churn source.

The churn is an **incremental-rebuild** artifact (reuse the build dir, as during
development). Repro: perturb `Ast.fx`, rebuild in place → **25/53** unchanged
modules' `.c` change. Two causes:

1. extern-prototype parameter names (22 modules): `idc2str` falls back to
   `name_<id>` / bare builtin-arg names, which shift. → omit param names in
   prototypes (C ignores them). 25→3.
2. cross-module inlining suppressed (3 modules): `k_skip_some` replaced a
   skipped module's function bodies with empty `KExpCCode("")` **before** the
   `K_inline` pass, so a recompiled module could not inline them and an
   inlined-body temp `res_0` degraded to a call temp `v_7`. → keep the bodies
   (the skipped module's `.c`/`.o` are still reused via `cmod_skip`). 3→0.

Post-fix: incremental churn 0; full-build determinism preserved; **no new
unused-symbol warnings** (committed bootstrap 4611 → post-fix 4613). The 2nd
bootstrap regen changed exactly the 4 modules whose `.fx` was edited and **zero**
unrelated modules — the WP-A diff lens working as intended.

## cfold oracle — sensitivity

`fxtest cfold` (in `all`) emits 500 random integer expressions, each evaluated as
a fold-eligible literal AND through runtime-opaque module vars, asserted bit-equal
at -O0/-O3. It generates the FB-002-sensitive cases (uint64 `>>`/`/`/`%`/compare
with high-bit operands like `2^64-1`); pre-fix those mismatch (fold uses signed
semantics), post-fix 0 mismatches. Opacity verified via `-pr-k`: the op survives
as e.g. `bool o = Zu64 > Zu64` rather than a folded constant.

## Sanitizer findings beyond FB-005

- **FB-009** (fixed): `fx_next_slice` advanced a pointer by a `size_t` `step`
  holding a signed (negative-step) offset → UBSan unsigned-overflow. `(ptrdiff_t)`
  casts. Invisible to every other layer.
- **Note on FB-005 vs ASan:** ASan does *not* reliably catch FB-005. Reverting
  the wrap fix, `a.wrap[-n]`/`v.wrap[-n]` reads `buf[n]`, which `fx_make_arr`
  usually leaves as *valid adjacent heap* (garbage, no trap) — precisely why it
  was a silent UB. The reference-checked T5 suite, not the sanitizer, is the
  layer that catches it. Both earn their place: ASan found FB-009; T5 found
  FB-005.

## New bugs discovered while hardening

- **FB-009** — runtime pointer UB on reverse-step slices (fixed).
- **FB-010** — bare `int` literal `-2^63` emitted without `LL` → C reads it
  unsigned. Outside the typechecker's own int64 literal range
  `[-(2^63-1), 2^63-1]`; a gray area, recorded (not fixed), excluded from the
  oracle. Candidate for a future codegen pass (suffix 64-bit literals).
- Also fixed in passing: string `.wrap` emitted `<noid>` (`FX_STR_ELEM_WRAP`
  declared but never registered) — folded into the FB-003 fix.

## Acceptance

1. determinism / cfold / sanitizer / `-c++` smoke / border / slice / nested-comp
   suites — all green; no XPASS surprises.
2. Generated-C stability holds: the unrelated-change scenario produces zero diff
   outside the touched module (full build) and zero churn incrementally.
3. `Ast_typecheck.fx` / overload resolution untouched.
4. Every registry entry updated; every route-around removed (FB-001 cpp_xfail,
   FB-002 lsr, FB-003/004/005 T5 route-arounds).
5. Two bootstrap regen commits (`e6017ec`, `a87daa5`), each explained.

Only remaining fence: **FB-007** (generic `complex` operators) — a missing
overload-resolution spec, pending a design session.
