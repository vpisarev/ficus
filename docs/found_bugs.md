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
