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

## FB-010  folded INT64_MIN (-2^63) emitted as a bare (unsigned) C literal
- discovered: 2026-07-07 while building the FB-002 cfold oracle -- it emitted a
  `-9223372036854775808` operand and the runtime path disagreed with the fold.
- detail: ficus's typechecker treats the 64-bit signed range as the symmetric
  `[-(2^63-1), 2^63-1]` (`Ast_typecheck.fx:504`), so `-9223372036854775808` is
  outside the supported *literal* range. But the **constant folder** can still
  reach INT64_MIN by arithmetic (e.g. `-9223372036854775807 - 1`). Its C emission
  (`klit2str`, `KLitInt`/`KLitSInt(64)`) printed the value bare as
  `-9223372036854775808`, which C parses as unary minus on `9223372036854775808`
  -- a constant too large for `long long`, hence **unsigned** (a default
  `-Wimplicitly-unsigned-literal` warning, and unsigned semantics in any
  enclosing expression, which is how the folded operand disagreed with the signed
  runtime). A bare `LL` suffix does NOT help: `9223372036854775808LL` is still an
  out-of-range unsigned constant that warns.
- config: C backend; the exact value -2^63 reached by folding, at -O0 and -O3.
- fixed: branch `housekeeping-1` (WP-H3). `klit2str` now emits INT64_MIN as the
  canonical warning-free **signed split form** `(-9223372036854775807LL - 1)` for
  both `KLitInt` and `KLitSInt(64)` (via `i64_c_literal`); all other literals are
  unchanged (bare `int`, `LL`/`ULL` for explicit 64-bit) -- blanket-suffixing
  every `int` literal was rejected: it churns every module for zero correctness
  gain and still warns on INT64_MIN. `uint64` was already correct (`ULL`; 2^64-1
  fits). Unfenced in the oracle: `cfold_gen.py` now generates the full signed
  range `[-2^63, 2^63-1]`, reaching INT64_MIN via in-range arithmetic operands
  (`_lit`); `fxtest cfold` is 0/500 at -O0 and -O3 across seeds. Directed
  `basic.int64_min` unit test covers fold-vs-runtime agreement and signed
  expression semantics. The typechecker's symmetric *literal* range (rejecting a
  bare -2^63 in source) is left as-is on purpose -- a language-design question
  parked in `docs/language_changes_brief.md`, out of scope here.

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
- WP-E update (2026-07-08): all three symptoms **re-confirmed** live by
  uncommenting `lib/Complex.fx:15-44`:
  - S2 — `val c = ref (1 + 1.fi); *c *= 2` resolves `__mul__` to the ARRAY
    multiply (`lib/Array.fx:341: unsupported iteration domain`).
  - S3 — typechecking `examples/vision_classify.fx` infers
    `nnop_t list Complex.complex` for a plain `nnop_t list` (`lib/Complex.fx:17`),
    cascading to `NN/FromOnnx.fx:1018`.
  Finding: the symptoms are **not reproducible in a self-contained mini-`cplx`
  class** — S2 needs the `N.fi` imaginary literal (complex-specific) and S3 needs
  the NN library's generic `+` on `nnop_t list` colliding with the over-general
  `operator +(a:'t, b:'t complex)`. So the fenced reproductions live against the
  real `Complex.fx` (uncomment) + `fst.fx`/`vision_classify.fx`, documented as
  commented `FIXME(FB-007)` blocks in `test/test_resolve.fx` and analyzed in
  `docs/wpe_tests_report.md`. The minimal, self-contained form of the underlying
  greedy-first-match gap is split out as **FB-016**. The D1 generality comparator
  (`compare_fun_generality`) already ranks the concrete `complex` operators over
  the generic ones correctly where it can (`-pr-resolve` confirms); the S3
  over-general match is the `int + 't` deferral gap (proposal §5, tranche B).
- resolve-1 update (2026-07-08, branch `resolve-1`): **PARTIALLY FIXED** by the
  collect->rank->commit resolver (see FB-016). Per-symptom status:
  - **S2 — FIXED** (the mis-resolution mechanism): at a determined receiver,
    `'t cplx * int` now ranks the cplx operator over the array `__mul__`
    regardless of declaration/import order. Locked as a passing case in
    `test/test_resolve.fx` (`fb007.s2_cplx_times_int`, mini-cplx module
    `test/CplxHelper.fx` mirroring the fenced Complex.fx operator shapes).
    The trial-pollution half of S3 is fixed the same way: a non-winning
    candidate's unification no longer commits anything.
  - **S1 — still open (session 2, §5 deferral)**: re-tested with the operators
    uncommented + a generic `complex(r:'t, i:'t)` ctor added: `1 + 1.fi` still
    fails — `a + b.re` inside `operator +(a:int, b:'t complex)` is typed `int`
    at declaration check, so at 't=float the body calls `complex(int, float)`
    (no match, correctly). Needs instantiation-time re-resolution of operations
    involving template params.
  - **S3 commit-half — still open (session 2, deferral of under-constrained
    calls)**: `vision_classify.fx` still infers `nnop_t list Complex.complex`
    at `FromOnnx.fx:1012` (`rev_more_ops + prog`, `prog` free): list-concat `+`
    and `+('t, 't complex)` are genuinely incomparable, so the tie falls back
    to env-order = the complex one. NEW: this shape now has a SELF-CONTAINED
    fenced repro (free fold accumulator + `[:: x] + prog`) in
    `test/test_resolve.fx` — WP-E could not self-contain it; the key is that
    the accumulator's type must still be fully free at the `+`.
  `lib/Complex.fx:15-44` + `examples/fst.fx:125-127` therefore STAY FENCED
  until session 2 (when unfencing, also add the generic `complex(r:'t, i:'t)`
  constructor — sanctioned above).
- **UNFENCED (2026-07-08, resolve-1, follow-up commit)** — the operators are
  live again, in a simplified, homogeneous form (Vadim), which sidesteps both
  remaining symptoms without waiting for session-2 deferral:
  - All operator bodies are now `'t op 't`-homogeneous (the `int`/`'t2` mixed
    variants removed) => **no cross-type arithmetic inside generic bodies =>
    S1 never triggers**. The `fst.fx` demo is rewritten accordingly
    (`ref complex(1.f, 1.f)`, `*c *= 2.f`) and runs; `fst.fx` is in the fxtest
    corpus, so complex arithmetic is now permanently regression-tested.
  - The scalar-on-the-LEFT variants `op (a: 't, b: 't complex)` remain FENCED
    (commented in Complex.fx with an explanation): their unconstrained 't makes
    them viable at any `known + still-free` call site (the FromOnnx.fx:1012
    fold-accumulator shape) where they tie incomparably with list-concat `+`
    and the under-constrained fallback picks by import order. They return with
    session-2 deferral, or with the concatenation-spelling change below.
    Until then: `c + s` works, `s + c` does not.
  - The second collision found on the way (NN/Inference.fx:94
    `string(tensor)`: `string(nntensor_t, ~border=8, ~braces=true)` vs the
    ninja record-generic `string({...})`) was resolved properly by the **Q2
    keyword normalization** in `compare_fun_generality`: when exactly one
    candidate has keyword params, the keywordless one's tuple gets the same
    implicit empty keyword record the caller gets, and coverage then ranks an
    exact keywordless match ABOVE a candidate viable only via all-defaulted
    keywords (so `sqrt(81.0)` now resolves to `Math.sqrt(double)` over a local
    `sqrt('t, ~n=2)` instead of erroring; negative golden 014 was repurposed
    to the identical-signature cross-module case).
  - **Language-design direction (Vadim), recorded here for follow-up**: drop
    the functional record-update operator `.{...}` and reuse `.` as the
    concatenation operator, or spell concatenation `++`. Either way list/string
    concatenation stops overloading arithmetic `+`; the free-typed-`[]`
    accumulator collision class disappears, and the fenced scalar-left complex
    variants can return unconditionally. Tracked in
    `docs/language_changes_brief.md` §4.
- **S3 FIXED (2026-07-08, branch `resolve-2`): `TypVarCollection`** — instead of
  changing the concatenation spelling or waiting for session-2 deferral, the
  ROOT of the collision was removed: the empty-collection literal `[]`
  (`LitEmpty`) is no longer typed as a fully free `make_new_typ()`
  (`Ast.fx: get_lit_typ`) but as a new var-form
  `TypVar(ref Some(TypVarCollection))` — "some collection: list, vector or
  array, element type unknown". `maybe_unify` binds it only to
  `TypList`/`TypVector`/`TypArray`/`'t [+]`/a plain free var/another
  collection-var; everything else fails. Consequences:
  - a free-typed `[]` fold accumulator can no longer be captured by an
    unrelated generic parameter: at `rev_more_ops + prog` (FromOnnx.fx:1012)
    the scalar-left `+('t, 't complex)` **dies at the viability trial**
    (`'t := [...]`, then `'t complex` vs `nnop_t list` fails), so only
    list-concat remains and the under-constrained fallback picks correctly.
  - the scalar-left variants `op (a: 't, b: 't complex)` are **UNFENCED** in
    `lib/Complex.fx` (with the latent sign bug in `-` fixed on the way:
    `s - c` now negates `im`), `examples/fst.fx` exercises `1.f - *c` in the
    corpus, and the self-contained S3 test in `test/test_resolve.fx` is
    unfenced and passing (`fb007.s3_free_accumulator`).
  - `val n: int = []` is now a TYPE CHECKER error (used to pass `-no-c` and
    only die at K-normalization's "[] is misused"); new negative golden
    `test/negative/215_empty_collection_scalar.fx`. An unresolvable `[]`
    still errors at K-normalization, with a clearer message.
  - `typ2str` renders the new var-form as `[...]`; `typ_has_free_vars` counts
    it as free (so `[]`-sites remain "under-constrained" for the tie policy);
    `freeze_varform_typs` freezes it to an opaque constant.
  Remaining FB-007 tail: **S1 only** (mixed-type arithmetic like `int + 't`
  inside generic bodies pinned at declaration check — session-2 deferral).
  The `++`/`.`-concatenation idea (§4 of language_changes_brief.md) is now a
  pure language-design question, no longer needed for correctness (Vadim
  demoted it to "maybe not" in ficus_todo_2026.md).
- **Imaginary-literal follow-up (same day)**: `N.fi`/`N.i`/`N.hi` never
  actually worked — the lexer desugared them into `complex(0, N)` with an
  **int** zero (`LitInt(0)`), so the real demo `1 + 1.fi` typed as
  `complex(int, float)` and no constructor matched (part of the S1 symptom
  as originally recorded above). Fixed in `Lexer.fx`: the zero real part is
  a float literal of the same width as the imaginary part. The fst.fx demo
  is live as `val c = ref (1.f + 1.fi); *c *= 2.f`. Mixed-type variants:
  `*` and `/` are mixed (`'t1 op 't2`, return type INFERRED — both result
  components go through a mixed primitive op, so builtin numeric coercion
  resolves at instantiation; doubles as the widening-cast idiom
  `1.0 * fcomplex`, and `v *= 2` works with an int scalar). `+`/`-` must
  stay homogeneous until S1 deferral: one component passes through unchanged
  (`b.im`), and a mixed variant would build a record with differently-typed
  fields (`1.f + 1.fi`, not `1 + 1.fi`).

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

## FB-011  signed-overflow UB in generated C miscompiled by gcc 15 at -O2/-O3  [FIXED]
- discovered: 2026-07-07 by the `fxtest.py cfold` oracle (differential fold vs
  runtime) on Linux/gcc 15.2. This was mis-attributed by the PR #27 CI handoff:
  the truncated CI log (`[ FAILED ] 1 test(s):`) was assumed to be a nested-
  comprehension regression, but the actual unit failure was an unrelated DL bug
  (see the dequantize note below) and the real compiler-relevant failure was
  this cfold O3 mismatch, which does NOT reproduce on older gcc/clang.
- symptom: the oracle reported `1 mismatches / 500`, e.g.
  `MISMATCH t=int32 x=2147483646 + y=2147483646 fold=-4 run=-4` -- fold and run
  BOTH print -4, yet `f != o` is true. Generated C:
  `int32_t o = (int32_t)((int32_t)(2147483646+Z) + (int32_t)(2147483646+Z)); if (-4 != o) {...}`.
  `int32 + int32` promotes to `int` and overflows -> **signed-overflow UB**.
- mechanism (verified, gcc 15.2, pure C too): gcc does NOT compute an overflowed
  value; it attaches `nsw` (no-signed-wrap) flags and VRP folds the *predicate*
  `-4 != o` to the constant `1` under the no-overflow axiom, while the *value* `o`
  is materialized separately by the real add (== -4 at runtime). Same `printf`
  shows `o=-4` and `(-4 != o)=1` simultaneously -- the signature of UB
  exploitation. It is NOT infinite-precision/GMP folding and never yields INT_MAX;
  gcc constant-folds integers in the type's width (wrap + TREE_OVERFLOW), never
  saturates. The constant folder (K_cfold_dealias) is CORRECT here: it wraps in
  int64 and sign-extends to int32 (`(x<<32)>>32 = -4`, emitted as literal -4).
- config: -O2/-O3, gcc 15 (also -O2 clang-class exploiters). -O0 is fine.
- fix: add `-fwrapv` to the default unix/gcc/clang cflags
  (`compiler/Compiler.fx`, `common_cflags`). Defines signed overflow as
  2's-complement wrap, matching ficus/folder semantics and killing the whole UB
  class for all generated programs. `-fwrapv` is supported by every gcc 3.x+ /
  clang (safe on old distros, e.g. Ubuntu 16.04). MSVC path untouched (it wraps
  and does not do strict-overflow opts). After the fix cfold is 0/500 at O0 & O3;
  full ladder + sanitize + determinism green. NOTE: the fxtest build dir
  (`build/fxtest/cfold/<opt>`) is cached across runs and the oracle source is
  fixed, so a stale pre-fix `.o` will keep failing -- `rm -rf build/fxtest`
  before re-measuring (CLAUDE.md incremental-build trap; CI is a clean checkout).

## FB-012  fx_str_join: memcpy(dst, NULL, 0) on an empty element (nonnull UB)  [FIXED]
- discovered: 2026-07-07 by `fxtest.py sanitize` (UBSan) on Linux, running
  test_all's randomized string-join tests.
- symptom: `runtime/ficus/impl/string.impl.h:559` did an unguarded
  `memcpy(result->data+ofs, s[i].data, len_i*szch)`; for an empty joined element
  `s[i].data==NULL, len_i==0`, so `memcpy(dst, NULL, 0)` violates memcpy's
  nonnull attribute. Benign (0 bytes) but real UB -- same class as FB-011: an
  optimizer may assume the pointer non-null and drop a later null check.
- config: any; only reached when a joined element is the empty string.
- fix: guard `if (len_i > 0)` around the memcpy, matching the already-guarded
  begin/sep/end memcpy's in the same function. Sanitize clean after.

## FB-013  fxtest IR extractor: wrapped `.fx:` module header leaks into snapshot  [FIXED]
- discovered: 2026-07-07 -- this is the PR #27 CI handoff's "Failure 2"
  (`nested_comprehension:ast` differs on CI, k0/k match), finally root-caused. It
  does NOT reproduce on a short dev path and looked like a platform-dependent AST
  print; it is neither -- it is a harness extraction bug.
- symptom: `-pr-ast` prints each module as `<abs-path>/<mod>.fx: <dep, dep, ...>`
  then a `-----` rule then the body. That header is pretty-printed with a width
  limit, so a long dependency list WRAPS onto an indented continuation line
  (`   String`) before the rule. `normalize.ir_extract_module` skipped only a
  single rule line after the `.fx:` header, so on a long path the wrapped
  `   String` + rule leaked in as the first two snapshot lines. The wrap point
  depends on the absolute path length: local `/home/vpisarev/.../nested_
  comprehension.fx` (117 cols) stays on one line and passes; CI
  `/home/runner/work/ficus/ficus/.../nested_comprehension.fx` (and the mac
  `/Users/runner/...`) wraps and fails. Only nested_comprehension has a long
  enough dep list (Builtins..String) to sit right at the boundary, which is why
  the other 19 IR programs passed everywhere.
- config: any host whose repo path is long enough to wrap the header; the
  compiler and the golden are both correct.
- fix: `tools/fxtest/normalize.py` -- after the `.fx:` header, skip forward to
  (and past) the `-----` rule instead of assuming exactly one line, so the
  extracted body is identical regardless of where the repo lives. Verified by
  dumping the same program from a short and a CI-length path: both normalize to
  the committed golden. No golden regen and no compiler change needed.

## (non-compiler) NN.Quantized.dequantizeLinear -- DL-inference-engine bug  [FENCED]
- NOT a compiler bug. `lib/NN/OpQuantized.run_dequantize` (the scalar/non-NEON
  int8 path) yields 0 instead of the expected dequantized value on Linux/x86, so
  the `NN.Quantized.dequantizeLinear` unit test fails at runtime (Actual 0.0 vs
  Expected -262.0). Per Vadim this is a known DL-engine defect, unrelated to the
  compiler. Fenced by commenting the TEST block in `test/test_nn_quant.fx` (a
  `// FIXME` note points back here). This is the unit failure the PR #27 CI
  handoff saw (its log had truncated the test name); see FB-011 for the mixup.

## (FIXED in PR #29) FB-014  bare positive literal 2^63 silently wrapped to INT64_MIN
- discovered: 2026-07-07 while widening the int64 literal range (housekeeping
  follow-up to FB-010).
- detail: the lexer (`LexerUtils.getnumber_`) accumulates a decimal magnitude in
  `uint64` and, for a *bare* (unsuffixed) literal, accepts magnitudes up to and
  including 2^63 (`maxval = 1<<63`). This is deliberate so that
  `-9223372036854775808` (unary minus folded onto the literal in `Lexer.fx`) can
  denote INT64_MIN. The side effect: a bare **positive** `9223372036854775808`
  also parses -- `(int64_t)(1<<63)` wraps to INT64_MIN -- so `val x =
  9223372036854775808` silently yields `-9223372036854775808` instead of an
  overflow error. Only the exact value 2^63 slips through (2^63+1.. give a lex
  OutOfRangeError).
- config: any backend; the exact bare positive literal 2^63.
- fixed: branch `housekeeping-1` (Vadim). `Lexer.fx` threads an
  `expect_neg_number` flag: the unary-minus handler sets it (toggle + restore)
  around the `nexttokens()` call that lexes the following number, and the number
  branch rejects a wrapped-negative magnitude (`LitInt`/`LitSInt` with value < 0)
  unless a minus was expected -- so bare positive `9223372036854775808` now errors
  ("the numeric literal is out of range for the specified type") while
  `-9223372036854775808` still denotes INT64_MIN. The toggle handles nested minus
  by parity: `- -9223372036854775808` (= 2^63) and `-(9223372036854775808)`
  (parenthesized, not immediately negated) both correctly error. Bonus: a `-` in
  front of an unsigned literal (`-5u8`) is now a clear lexer error rather than a
  silent `MINUS :: LitUInt`. Regression goldens: `test/negative/007_pos_int64_
  overflow` and `008_neg_unsigned_literal`. Only `Lexer.c` changed in the
  bootstrap; fixpoint holds; full ladder green. (This complements the FB-010 /
  int64-range work: the typechecker now accepts the true INT64_MIN and the lexer
  now rejects the bare positive 2^63 that used to alias it.)

## FB-015  parser: `continue`/`break` and bare `return` rejected in expression position
- discovered: 2026-07-08 while writing the WP-E `-pr-resolve` trace (`trace_resolve`
  in `Ast_typecheck.fx`).
- detail: two related parser limitations, both surfacing as a misleading
  `error: unexpected token '}'. An identifier, literal or expression ... is
  expected here` rather than a targeted diagnostic:
  1. **`continue` (and presumably `break`) cannot be the *value* of a `match`
     arm / `if` branch.** `[:: for e <- xs { match e { | A(x) => x | _ => continue } }]`
     fails to parse; the working idiom is to run the `match` as a statement that
     may `continue` and yield the element separately (see the existing
     `add_id_to_env_check`, `Ast_typecheck.fx`), or accumulate into a `var` list.
  2. **A bare `return` (no value) in a `: void` function does not parse.**
     `if !cond { return }` errors at the `}`; `return <expr>` is fine (used widely),
     so only the value-less form is affected. Worked around by inverting the guard
     into a wrapping `if cond { ... }` instead of an early `return`.
- config: parser only; both are compile-time parse errors, no codegen involved.
- status: routed around in `trace_resolve` (accumulator loop + wrapping `if`, no
  bare `return`, no `continue`-as-expression). NOT a blocker for anything; logged
  for a later parser look (control-flow keywords as unit-typed expressions, and a
  clearer diagnostic than "unexpected token '}'"). Also noted in CLAUDE.md gotchas.

## FB-016  overload resolution: greedy first-match ignores specificity (last-declared overload wins)
- discovered: 2026-07-08 during WP-E, via the new `-pr-resolve` trace / the D1
  generality comparator. This is the **minimal, self-contained** form of FB-007's
  symptom S2 (no `complex`/NN machinery needed).
- repro (`bin/ficus -run`):
  ```
  fun f(x: int) = x + 1     // concrete
  fun f(x: 't) = x          // generic (identity)
  val a = f(5); println(a)  // prints 5  -- the GENERIC f('t) is chosen
  ```
  Reversing the two declarations makes `f(5)` print `6` (the concrete wins). So
  the chosen overload is purely the **last-declared** one that unifies
  (`find_first` commits to the first entry in env-list order, which is
  most-recently-added-first), NOT the most specific. The meaning of `f(5)` thus
  depends on the declaration order of otherwise-unrelated overloads.
- expected (per `docs/overload_resolution_proposal_v2.md` §2-§3): collect all
  viable candidates, then pick the least-generic (most specific); `f(int)` should
  win regardless of order. The D1 `compare_fun_generality` already returns the
  right answer here (`f(int)` is `LessGeneric`); `-pr-resolve` shows
  `env-order winner: f('t)` vs `generality winner: f(int)`, `winners agree: false`.
- config: any backend; `-no-c` typecheck already selects the wrong instance.
- status: NOT fenced (it is latent current behavior, not a crash); it is the very
  thing the resolver surgery (tranche A) fixes. Tracked by: the `-pr-resolve`
  instrument (D2), the E1 census (`docs/wpe_experiments/e1_census.md`), and a
  fenced `// FIXME(FB-016/FB-007)` case in `test/test_resolve.fx` that flips to
  pass when tranche A lands. Corpus note: the compiler's OWN code contains no site
  where specificity would flip a *concrete* winner (E1: 0 different-concrete-winner
  disagreements over 344 viable>1 sites; all 227 disagreements are `<none>` ties
  from under-constrained arguments) -- so this bites user code like the above, not
  the bootstrap.
- **FIXED** (2026-07-08, branch `resolve-1`, phase 2): `lookup_id_opt` is now
  collect -> rank -> commit: all candidates are tried side-effect-free
  (`update_refs=false`), the viable set is ranked by `compare_fun_generality`
  (unique least-generic wins), and only the winner commits. `f(5)` returns 6
  (the concrete `f(int)`) regardless of declaration order; the fenced
  `test/test_resolve.fx` case flipped and is locked as
  `FB016_fixed.concrete_beats_generic`; the `test/ir/overload_resolve` goldens
  now show the concrete instances selected. Tie policy (amends proposal §4,
  per `docs/resolve1_surgery_brief.md`): fully-determined tie -> ambiguity
  error (negative goldens 012-014); under-constrained tie -> env-order
  fallback until session-2 deferral. Corpus-invariance proof: bootstrap
  regeneration through the new resolver changed ONLY the edited
  `Ast_typecheck.c`; `-pr-resolve` over `compiler/fx.fx`: 350 ranking sites =
  343 ranked + 7 under-constrained fallbacks + 0 ambiguity errors.

## FB-017  generality comparator can't order `{...}` (TypVarRecord) auto-generics vs a concrete record
- discovered: 2026-07-08, WP-E D1/E1. Limitation of the NEW (unwired)
  `compare_typ_generality` in `Ast_typecheck.fx`; not a shipping-compiler defect,
  but it is the dominant reason the E1 census shows so many `<none>` verdicts and
  it must be closed before the resolver surgery relies on the comparator.
- detail: the auto-generated record/variant operators are typed with `{...}` =
  `TypVarRecord` (e.g. `template<__var_record__> operator ==(a: {...}, b: {...})`
  in `Builtins.fx:580`, and `string`/`<=>`/`hash`/`print` likewise). The
  comparator ranks generality with two one-way `maybe_unify(update_refs=false)`
  trials, but `maybe_unify` treats a record-var **symmetrically** -- a
  `TypVar(ref Some(TypVarRecord))` binds to a concrete record in *either*
  direction -- so both "covers" trials succeed and the pair is reported
  `EqGeneric`/`IncompGeneric` (`<none>`) instead of "concrete `≻` `{...}`".
  Repro: `bin/ficus -no-c -pr-resolve` on
  ```
  type pt = {x:int; y:int}
  fun myeq(a: {...}, b: {...}) = true
  fun myeq(a: pt, b: pt) = a.x==b.x && a.y==b.y
  val r = myeq(pt{x=1,y=2}, pt{x=1,y=2})
  ```
  -> `env-order winner: myeq(pt,pt)` (correct), `generality winner: <none>`.
- impact on E1: 224 of 227 census `<none>` sites are exactly this shape
  (`__eq__` 141, `string` 83) plus a few genuine identical-signature duplicates
  (e.g. `length(string)` defined in both `String.fx:10` and `Builtins.fx:133`).
  Crucially NONE is a real divergence: env-order already picks the concrete, and
  a *complete* comparator would too -- so tranche A stays corpus-invariant once
  this is fixed. It is comparator incompleteness, not a resolution flip.
- fix sketch (for the surgery session): skolemize `TypVarRecord` (and by symmetry
  `TypVarTuple`/`TypVarArray`) on the RIGID side into an opaque "some specific
  record" sentinel that a *different* concrete record does not unify with, while
  `unskolemize_typ` restores it to a free `TypVar(ref Some(TypVarRecord))` on the
  FREE side -- the same rigid-vs-free asymmetry already used for `'t` skolems,
  extended to the `TypVar*` family. Deliberately NOT done in this WP-E test/instr
  package (D1 scope is `df_templ_args` + keyword `TypRecord` + constructor return
  type); documented in `docs/wpe_tests_report.md` and noted in `test/test_gencmp.fx`.
- status: not fenced (unwired code); tracked here + in the E1 census + the report.
- **FIXED** (2026-07-08, branch `resolve-1`, phase 1): `compare_typ_generality`
  now freezes the var-form family on the RIGID side of each trial via the new
  `freeze_varform_typs` (non-destructive walk): `{...}` -> a record with a
  unique `__skolem_rec__` field, `(...)` -> a tuple of two distinct opaque
  constants (so `('t ...)` does not cover it), `('t ...)` -> a 1-tuple of the
  frozen payload, `'t [+]` -> `TypArray(-1, frozen 't)`. `maybe_unify` itself
  untouched. +14 verdicts in `test/test_gencmp.fx` (all pre-existing verdicts
  unchanged). E1 census re-run (`docs/wpe_experiments/e1_census_post_fb017.md`):
  344/117/227/0 -> 342/341/1/0 (sites/agree/none/concrete-flips); the one
  remaining `<none>` is the known under-constrained 21-viable `__cmp__` site.
