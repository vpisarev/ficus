# Found bugs registry

Append-only registry of compiler bugs discovered by the fxtest infrastructure.
The rule (Brief #1): **find and fence bugs, do not fix them.** Each entry is
fenced (manifest `quarantine=` / `cpp_xfail`, suite `xfail`, or routed around in
suite code with a comment) and stays fenced until a future fix lands, at which
point CI reports it loudly (XPASS / a reference-checked suite failing). This
file is the direct input to Brief #2.

Entry template:

```
## FB-NNN  <pass or area>: <one-line symptom>
- repro: <path to test file or inline snippet>
- config: <opt level, platform/compiler>; <expected> vs <got>
- status: fenced in <manifest entry / suite> as <xfail | crash | routed-around>
```

Digest: FB-001 is caught by the `-c++` smoke (fenced XFAIL); FB-002/004/005 are
miscompilations/UB invisible to the O0/O3 differential and were only caught by
the reference-checked T5 suites; FB-003 is a C-backend codegen failure caught
while writing T5. Each is invisible to some layers and caught by another — which
is the whole point of the ladder.

---

## FB-001  C++ backend: interface codegen does not compile under -c++
- symptom: any program defining an `interface` and a `class` implementing it
  compiles & runs correctly as C (default), but under `-c++` the generated C++
  fails to compile. Two errors per iface init site:
    * `cannot initialize an array element of type 'const void *' with an lvalue
       of type 'int (struct ..._data_t *, ..., void *)'`  (C allows fn-ptr ->
       void*, C++ does not), and
    * `no matching function for call to 'fx_init_ifaces'`.
- repro (14 lines, fails under `-c++`, ok under C):
  ```
  interface IShape { fun area(): double }
  class Circle : IShape { r: double }
  fun Circle.area(): double = 3.14159 * self.r * self.r
  val c = Circle {r=2.0}
  val s = (c :> IShape)
  println(s.area())
  ```
- config: `-c++`, macOS/clang (also expected on linux/g++ -- C++ language rule,
  not compiler-specific). Default C build: OK. `-c++`: 2 errors generated.
- discovered by: `fxtest.py corpus --cpp-smoke` (test_all fails: 12 errors, all
  from test_oop.fx which exercises interfaces).
- status: fenced -- `cpp_xfail` on the `test_all` corpus entry; the -c++ smoke
  reports XFAIL and will report XPASS when the codegen is fixed.
- fixed: branch `harden-1`. `C_gen_fdecls.fx` (the `KDefVariant` interface-init
  path) now emits explicit `(void*)` casts on (a) each method pointer in the
  `const void* vtbl[]` initializer and (b) the free-function pointer passed to
  `fx_init_ifaces`. C converts fn-ptr->void* implicitly; C++ does not, hence the
  two errors per iface init site. No compiler/stdlib module declares an
  interface, so the bootstrap is unchanged; only interface-using user programs
  gain the (harmless-in-C) casts. Unfenced: `cpp_xfail` removed from
  `manifest.toml`; the `-c++` smoke on `test_all` (and the whole corpus) is green.

## FB-002  uint64 `>>` is an arithmetic (sign-extending) shift, not logical
- symptom: right-shifting a `uint64` whose top bit is set fills with sign bits
  instead of zeros. `uint8`/`uint16`/`uint32` shift correctly (logical); only
  `uint64` is affected.
- repro:
  ```
  val a = 0x8000000000000000u64
  println(a >> 1)   // prints 13835058055282163712 (0xC000...) -- arithmetic
                    // correct logical result: 4611686018427387904 (0x4000...)
  ```
- config: reproduces at every -O level, C and C++, macOS/clang. Because the
  wrong result is identical at -O0 and -O3, the T2 differential CANNOT catch it
  -- a good illustration of why the ladder needs reference-checked suites (T5).
- consequence: this is why the stdlib `RNG` implements splitmix64 in `@ccode`.
- status: routed around in `test/rand/RandUtil.fx` via
  `lsr(x,n) = (x >> n) & ((1u64 << (64-n)) - 1u64)`; the T5 RandUtil splitmix64
  is validated against reference vectors, so if the workaround ever breaks the
  randomized suites fail loudly. Not fenced as xfail (no dedicated diagnostic).
- scope note: only the CONSTANT FOLDER is affected; the runtime path (C codegen
  on `uint64_t`) is correct -- an opaque `b >> 1` prints the right value, only a
  literal `a >> 1` folds wrong. So it is invisible to T2 (same wrong result at
  -O0/-O3) and needed a compile-time-vs-runtime oracle to pin down.
- fixed: branch `harden-1`. `K_cfold_dealias.fx`: the folder's `ConstInt` now
  carries an `is_u64` flag (`ConstInt: (int64, bool)`, true only for full-width
  uint64; narrower unsigned stay signed-flagged, being exactly representable in
  int64). `OpShiftRight`, `OpDiv`, `OpMod` and `OpCmp` fold with unsigned
  semantics when the flag is set, and uint64->float/string conversions in
  `finalize_cfold_result` use the unsigned value. Unfenced: `lsr()` removed from
  `RandUtil.fx` (plain `>>`, guarded by the splitmix64 reference vectors); new
  oracle `tools/fxtest/cfold_gen.py` + `fxtest.py cfold` proves compile-time ==
  runtime over 500 random expressions at -O0/-O3 (in `fxtest.py all`).
  Integer-semantics paragraph added to `doc/ficustut.md`.

## FB-010  [gray area] literal INT64_MIN (-2^63) emitted without an LL suffix
- discovered: 2026-07-07 while building the FB-002 cfold oracle -- it emitted a
  `-9223372036854775808` operand and the runtime path disagreed with the fold.
- detail: ficus's typechecker treats the 64-bit signed range as the symmetric
  `[-(2^63-1), 2^63-1]` (`Ast_typecheck.fx:504`), so `-9223372036854775808` is
  outside the supported *literal* range. When it slips through, `int` (no suffix)
  codegen emits bare `-9223372036854775808`, which C parses as unsigned (warns
  `-Wimplicitly-unsigned-literal`) -> wrong value. Explicit `...i64` gets an `LL`
  suffix and is fine.
- config: C backend; only the exact value -2^63 as a bare `int` literal.
- status: NOT a fix target here (outside FB-002; a rarely-hit gray area). Fenced
  in the oracle by generating 64-bit signed values in `[-(2^63-1), 2^63-1]`
  (`cfold_gen.py _range`); recorded here for a future codegen pass (emit 64-bit
  literals with an `LL`/`ULL` suffix, or special-case INT64_MIN).

## FB-003  border access `.clip/.wrap/.zero` on a plain 1D array emits broken C
- symptom: `arr.clip[i]` (also `.wrap` / `.zero`) on a plain array `'t []`
  typechecks but generates C that does not compile:
  `use of undeclared identifier '__idx0__'; did you mean '__idx__'?` inside the
  `FX_PTR_1D_CLIP` macro (runtime/ficus/ficus.h). The same access works fine on
  a `Vector` and on strings.
- repro:
  ```
  val a = [10, 20, 30, 40, 50]
  println(a.clip[7])        // C error in FX_PTR_1D_CLIP (__idx0__ vs __idx__)
  // works: import Vector; println(vector([10,20,30,40,50]).clip[7])  // -> 50
  ```
- config: reproduces at -O0/-O3, C backend, macOS/clang. A codegen bug (bad
  identifier in the emitted macro call), independent of the C compiler.
- discovered by: writing the T5 `rand.array.border` suite.
- status: routed around in `test/rand/test_rand_array.fx` (border test uses a
  `Vector`, the supported type). Not caught by T2 (never compiles) nor T3
  (passes -no-c typecheck) -- a pure C-backend codegen bug.
- fixed: branch `harden-1`. Two runtime-header bugs in the 1D border macros
  (`runtime/ficus/ficus.h`): (1) `FX_PTR_1D_CLIP`/`FX_PTR_1D_WRAP` used
  `__idx0__` (undeclared) instead of `__idx__`; (2) all three 1D macros closed
  the GCC statement-expression with `))` instead of `})`. 2D+ macros were fine.
  While here: string `.wrap` emitted `<noid>` because `std_FX_STR_ELEM_WRAP`
  was declared (`C_form.fx`) but never registered in `C_gen_std.fx` (`.clip`/
  `.zero` were, which is why the lexer's `.zero` usage worked). Unfenced: the T5
  `rand.array.border` suite now tests plain arrays too, and a new
  `array.border_matrix` unit test covers {1D array, 2D array, Vector, string} x
  {clip, zero, wrap}.

## FB-004  empty strided array slice produces a corrupt view -> SIGSEGV on use
- symptom: an *empty* strided slice `a[lo:lo:step]` with `step >= 2` yields a
  corrupt array view; consuming it (array `==`/`!=`, copy, iterate) segfaults.
  Non-empty strided slices, contiguous slices and `step == 1` are fine.
- repro (SIGSEGV, exit 139):
  ```
  val a = [1, 2, 3, 4, 5]
  val e = a[2:2:2]                 // empty strided slice
  val r = [for k <- 2:2:2 {a[k]}]  // empty reference
  println(e == r)                  // crashes
  ```
- config: -O0 and -O3, C backend, macOS/arm64. Runtime memory bug (bad view
  descriptor), identical at both opt levels -> invisible to T2, and passes T3
  (-no-c typecheck). Found while writing the T5 `rand.array.slice` suite.
- status: routed around in `test/rand/test_rand_array.fx` -- the slice test
  draws `hi` from `[lo+1, n]` so strided slices are never empty; strided slicing
  is therefore still exercised on non-degenerate ranges.
- fixed: branch `harden-1`. `fx_subarr` (`runtime/ficus/impl/array.impl.h`) took
  the copy path for strided slices (`need_copy` when `delta != 1`) and ran a
  `do { fx_copy_arr_elems(...); } while (fx_next_slice(...))` loop whose body
  executes once even when the result has 0 elements, copying from/deriving a
  null slice pointer. Added an early `if (total == 0) return FX_OK;` after
  `fx_make_arr` (the empty array is already correctly built). Unfenced: the T5
  slice suite now draws `hi` from `[lo, n]` (empty strided slices included) and a
  new `array.empty_slice` unit test covers empty at start/middle/end, step>len,
  `lo==hi==n`, and empty negative-step.

## FB-005  Vector `.wrap[-n]` reads one past the end (heap over-read)
- symptom: for a `Vector` of length n, `v.wrap[idx]` with `idx == -n` computes
  the wrapped index as `n` (off by one at the boundary) and reads `buf[n]` --
  one element past the end -- returning adjacent heap garbage instead of the
  wrapped element `v[0]`. The result is therefore non-deterministic: in a clean
  process it often reads 0, but under heap load it returns arbitrary values.
  Every other index in `[-n+1, 2n-1]` wraps correctly.
- repro: hard to see in isolation (clean adjacent memory reads as 0); reliably
  visible in `test/rand/test_rand_array.fx rand.array.border` when `idx == -n`
  (failures move as surrounding heap state changes -- the tell-tale of an
  over-read).
- config: -O0/-O3, C backend, macOS/arm64. An out-of-bounds read, so ASan would
  flag it; classic silent UB otherwise.
- status: routed around by drawing `idx` from `[-(n-1), 2n-1]` in the border
  test, excluding the exact `-n` boundary; every other wrap index is validated.
- fixed: branch `harden-1`. The wrap index was computed as
  `(idx % n) + (idx < 0 ? n : 0)`, which yields `n` (one past the end) when
  `idx % n == 0` and `idx < 0` (i.e. `idx == -n, -2n, ...`). Replaced with a
  true Euclidean modulo `((idx % n) + n) % n` in all copies of the formula:
  `fx_rrb_find_border` (Vector, `rrbvec.impl.h`), `FX_PTR_1D_WRAP` + `FX_WRAP_IDX`
  (arrays) and `fx_str_elem_wrap` (strings) in `ficus.h`. Unfenced: the T5
  `rand.array.border` suite now draws `idx` from `[-3n, 3n)` with the boundaries
  `-n, -2n, 2n` forced in, for both Vector and plain array.

## FB-006  nested array comprehension (array-of-arrays) -> broken C on indexing
- symptom: a nested comprehension whose body is itself a comprehension,
  `[for i {[for j {..}]}]`, has type `'t [] []` (array of arrays). Building it is
  fine, but **indexing the result** emits malformed C and fails to compile:
  `error: expected expression`. The generated element-copy reads from a literal
  `{0}` instead of the inner array, e.g.
  `fx_copy_arr(&{0}, dstptr_0);`  // should be `&arr_0` (the inner comp result)
  -- so even if it compiled, each outer element would be copied from an *empty*
  array (wrong result). The same `'t [] []` built from an **array literal**
  (`[r0, r1]`) indexes correctly -- the bug is specific to the comprehension
  producing the array-of-arrays.
- repro:
  ```
  val aa = [for i <- 0:2 {[for j <- 0:2 {i + j}]}]   // int [] []
  println(aa[0][0])          // C error: fx_copy_arr(&{0}, ...) -> expected expression
  // ok: val aa = [[0,1],[1,2]]; println(aa[0][0])   // literal array-of-arrays
  ```
- config: -O0 and -O3, C backend, macOS/clang. Codegen wires the inner
  comprehension result to `{0}` instead of its array variable. (An unused such
  `aa` compiles only because dead-code elimination drops it.)
- discovered: 2026-07-06, searching for a remembered nested-comprehension
  miscompilation (Vadim). NOT the same as the commented-out
  `NN.Quantized.globalAvgPool` test, whose `[for n for c {..scalar..}]` is a
  rectangular 2D comprehension (`uint8 [,]`) and compiles fine.
- status: not yet fenced by a test. Route-arounds: use a rectangular 2D
  comprehension `[for i for j {..}]` when the data is rectangular, or build
  array-of-arrays from literals / a list comprehension `[:: for i {[for j {..}]}]`.
- fixed: branch `harden-1`. Not a K-form bug (the K-form correctly indexes the
  inner array); it was C emission. In `C_gen_code.fx`'s array/vector/list
  comprehension, the loop body is evaluated with `kexp2cexp(body, ref None, ..)`
  and the body's value copied into the destination element. But a value-producing
  comprehension delivers its result by writing it back into `dstexp_r` (returning
  a dummy `{0}`), so when the body is itself a comprehension the outer map copied
  from `{0}`. Fix: pass a named `body_dst_r` ref and read it back
  (`match *body_dst_r { Some r => r | _ => result0 }`). Unfenced: new unit test
  `array.nested_comprehension` (2/3-level, tuple elements, comprehension vs
  literal vs list-comp agree), T5 `rand.array.nested_comp`, and a T4 IR snapshot
  `test/ir/nested_comprehension.fx`.

## FB-007  generic `complex` operators break overload resolution / type inference
- summary: the `+ - * /` operators for the `complex` class in `lib/Complex.fx`
  (lines 15-44) are commented out — enabling them breaks the typechecker, so
  complex arithmetic is unavailable and the complex demo in `examples/fst.fx`
  (lines 125-127) is commented too. Vadim's "probably a typechecker problem".
- three distinct symptoms, in order of depth:
  1. **Missing generic constructor.** The operators call `complex(a+b.re, b.im)`,
     but only `complex(float,float)` / `complex(double,double)` exist (no generic
     `complex('t,'t)`). So uncommenting them + `fst.fx` fails with:
     `the appropriate match for 'complex' of type '((int, float) -> ...)' is not
     found` (from `1 + 1.fi`; `int + 't` is typed `int`, giving `complex(int,..)`).
  2. **Operator mis-resolution.** Adding `fun complex(r:'t, i:'t) = complex{re=r,
     im=i}` fixes (1), but `c * 2` (a `float complex` times an int) then resolves
     `__mul__` to the **array** multiply (`Array.fx:341: unsupported iteration
     domain`) instead of `operator * (a:'t complex, b:int)`.
  3. **Inference pollution of unrelated code (the "interferes with NN" reason).**
     Typechecking an NN entry (`examples/vision_classify.fx`) then fails in
     `NN/FromOnnx.fx:1018`: a `nnop_t list` is inferred as
     `nnop_t list Complex.complex` — the overly-generic complex candidate
     spuriously wraps the list — so `.rev()` no longer matches.
- root: overload resolution / unification lets the very generic complex operator
  and constructor candidates (`'t`, `'t complex`, `'t2 complex`) match too
  eagerly, and `int + 't` inside a generic body doesn't resolve/coerce. Needs
  typechecker work, not a library tweak.
- config: -no-c typecheck already fails, all platforms.
- status: fenced by keeping both blocks commented (`lib/Complex.fx:15-44`,
  `examples/fst.fx:125-127`); recorded here per commit 786b446's note.

## FB-008  [UNCONFIRMED] non-deterministic-looking `.c`: unstable under unrelated changes
- symptom (Vadim, seen several times over development, not a bit-flip): the
  compiler emits a **different `.c`** for a `.fx` module whose own source and
  dependencies did not change — variables renamed, declarations/functions
  reordered. Persistent annoyance; not yet reproduced on demand.
- established: **pure recompilation is fully deterministic** (built
  `compiler/fx.fx` 4x -> byte-identical `.c`), so it is NOT ASLR/random. It only
  shows when *some* module's **K-form** changes (comment/formatting edits do not
  trigger it). An *unused* added function changes no `.c` (dead-code-eliminated),
  so a repro needs a change that survives DCE and shifts real symbol ids.
- likely mechanism: a global symbol-id counter (`name@NNN`, shared across all
  modules) feeding **order-dependent C-name generation** in
  `compiler/K_mangle.fx`:
    * `make_unique_(idx)` (~L100): duplicate mangled names get `_1`/`_2` suffixes
      by processing order -> a flipped order swaps them (= renamed variables);
    * `curr_km_idx` (~L265, `gen_idk`): type-name counter (`_fx_g<N>`) numbered in
      mangling order;
    * any decl-emission order taken from iterating a Hashmap/Hashset keyed on ids
      would reorder when ids shift (check `C_gen_fdecls.fx` / `C_gen_code.fx`).
- historical trace: commit `11034d4` "updated precompiled ficus compiler" changed
  `compiler/bootstrap/K_mangle.c` (+ Options/String/Sys) with NO `.fx` change.
- config: any; a codegen-stability issue, not a miscompilation (the emitted code
  is presumably still correct, just churns needlessly -> noisy bootstrap diffs).
- status: NOT fenced (no repro yet). Hunt during compiler fixes (esp. FB-006
  nested comprehensions and FB-007 typechecker work): watch regenerated
  `bootstrap/*.c` diffs, and try a used/DCE-surviving change in an early module
  then diff an unrelated module's `.c`. Details in the agent memory note
  `ficus-nondeterministic-codegen`.
- CONFIRMED (2026-07-07, Brief #2 WP-A1): repro built. Adding a single
  DCE-surviving builtin id to `compiler/Ast.fx`
  (`val (std__detprobe__, builtin_ids) = std_id("__detprobe__", builtin_ids)`)
  churned **25 unrelated compiler modules'** `.c` (Ast.c itself +21 lines),
  from 4 lines up to Lexer.c 1166, C_pp.c 170, Parser.c 50. An *unused* function
  in Ast.fx (`ignore(f(x))` at -O3) is fully DCE'd → zero churn, matching the
  earlier note. Two observed mechanisms:
    * M1 name-suffix disambiguation: params/locals gain or lose `_N`
      (`size_0`↔`size`, `f_0`↔`f`, `sep_0`↔`sep`) — a shifted global id makes a
      previously-ambiguous name unambiguous (or vice versa).
    * M2 declaration emission order: generated ref/closure types and their
      `_fx_make_*` fns reorder (e.g. `_fx_ri` = `int ref` block moves in Lexer.c)
      — emission order is keyed on numeric id, which shifts.
  Evidence in `docs/fb008_evidence/`.
- REFINED + fixed (2026-07-07, Brief #2 WP-A2): the earlier "M1/M2" note was
  measured through incrementally-confounded build dirs. The corrected picture:
  **a full (from-scratch) build is already byte-deterministic** under the
  unrelated change (two clean builds → identical `.c` for all 52 other modules;
  so bootstrap regeneration, a full build, never churned). FB-008 is an
  **incremental-rebuild** artifact (reuse the build dir, as during development):
  perturb `Ast.fx`, rebuild in place → 25/53 unchanged modules' `.c` change.
  Two causes, both fixed:
    * extern-prototype parameter names (22 modules): `idc2str` falls back to
      `name_<id>` / bare builtin-arg names, which are unstable. Fixed by omitting
      parameter names in prototypes (C ignores them) — `C_pp.fx` `pprint_fun_hdr`
      `fwd_mode` branch. 25→3 modules.
    * suppressed cross-module inlining (3 modules): `Compiler.fx` `k_skip_some`
      replaced skipped modules' bodies with empty `KExpCCode("")` *before* the
      `K_inline` pass, so recompiled modules couldn't inline them → an inlined-body
      temp (`res_0`) degraded to a call temp (`v_7`). Fixed by keeping the real
      bodies (skipped modules' `.c`/`.o` are still reused via `cmod_skip`). 3→0.
- fixed: branch `harden-1` (C_pp.fx + Compiler.fx), unfenced by new test `tools/fxtest/determinism.py`
  (`fxtest.py determinism`): 2+ from-scratch builds byte-identical AND an
  incremental rebuild after an unrelated `Ast.fx` change leaves every unchanged
  module's `.c` byte-identical. Post-fix both green; full-build determinism
  preserved; no new unused-symbol warnings (4611→4613).

## FB-009  runtime pointer-arithmetic UB on reverse/negative-step slices
- discovered: 2026-07-07 by the new `fxtest.py sanitize` leg (ASan+UBSan) while
  running `test_all` -- exactly the silent-UB class the leg was added for.
- symptom: `array.impl.h:408` `fx_next_slice` did `s->ptr += s->step` with `step`
  typed `size_t` but holding a *signed* byte offset (negative for reverse /
  negative-step slices). UBSan: "addition of unsigned offset ... overflowed".
  Benign on real targets (the wrap-around lands on the right address) but UB.
- config: any; only reached by negative-step array iteration.
- status: fixed on branch `harden-1` -- cast to `(ptrdiff_t)` at both pointer-
  advance sites (`s->ptr` and the carry `stack[i].ptr`). After the fix `test_all`
  (+ T5 suites) runs clean under `-fsanitize=address,undefined`.
- note on FB-005 vs ASan: the B5 acceptance asked to show ASan catches FB-005.
  It does NOT reliably: reverting the wrap fix, `a.wrap[-n]`/`v.wrap[-n]` reads
  `buf[n]`, which the `fx_make_arr` allocation usually leaves as *valid adjacent
  heap* (it prints garbage, no ASan trap) -- precisely why FB-005 was a silent
  UB and why the reference-checked T5 suite, not the sanitizer, is what caught
  it. The sanitizer leg still earns its place: it found FB-009, invisible to
  every other layer.
