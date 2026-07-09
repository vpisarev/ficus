# Working in the Ficus repo

Ficus is a self-hosted functional language (arrays first-class; also imperative
& OOP) that compiles to portable C/C++. The compiler is written in Ficus and
bootstraps itself.

## Build & run

```sh
make -j8                                  # builds bin/ficus (ficus0 from
                                          # pre-gen C, then ficus from sources)
bin/ficus -run file.fx -- arg1 arg2       # compile + run
bin/ficus -run -O0 -no-openmp -B DIR f.fx # opt level, no OpenMP, build root
bin/ficus -no-c file.fx                   # parse + typecheck only (fast)
```

- `bin/ficus` finds the stdlib on its own (no `FICUS_PATH` needed in-repo).
- **`-B <dir>` must already exist** — `mkdir -p` it first, or you get the
  misleading `error: failed to write some k-forms`.
- Compiler **diagnostics go to stdout** (not stderr), exit code 1.
- IR dumps: `-pr-ast` (typechecked AST), `-pr-k0` (K-form), `-pr-k` (optimized,
  use with `-O3`). All work with `-no-c`. The dump contains **every** imported
  module; the file-under-test is the **last** section. Gensyms look like
  `name@1234`.
- ficus compiles the generated C/C++ with `$CC` / `$CXX` (default `cc`) — set
  these to pick gcc vs clang. macOS OpenMP is bundled at
  `runtime/lib/macos_arm64/libomp.a` (no install needed).
- to build the same program, including compiler, several times
  (after modifying code or compiler) and compare generated code,
  pass `-o output_name`. ficus will put app `output_name` and all
  intermediate files (.c; (.k, .ast) if requested) to
  `<ficus_root>/__fxbuild__/output_name`.
- **Regenerating the bootstrap** — required after editing any `compiler/*.fx`
  **or any stdlib module the compiler pulls in** (`compiler/bootstrap/` has 54
  modules incl. `Builtins.c`, `Complex.c`, `String.c`, ... — `ls` it when in
  doubt): run `python3 tools/update_compiler.py`. It rebuilds the compiler,
  regenerates the pre-gen C in `compiler/bootstrap/*.c`, copies over only the
  changed modules, and asserts the self-hosting **fixpoint** — the
  freshly-built compiler must regenerate its own bootstrap byte-for-byte, else
  it's a determinism regression (`fxtest.py determinism`). `--check` is a
  CI-friendly dry run (exit 1 if the bootstrap is stale); `--no-make` skips
  the initial build. **CI runs `--check` on every push that touches
  compiler/lib/runtime** — a lib-only commit with a stale bootstrap is a red
  master (learned 2026-07-08: lib/Complex.fx edit, forgotten regen). The regen
  doubles as the ONLY routine self-build of the compiler, so it also catches
  stdlib changes that break compiling the compiler itself — `fxtest all` does
  NOT rebuild the compiler and cannot catch that class.

## Testing — the fxtest ladder (see tools/fxtest/README.md)

```sh
python3 tools/fxtest/fxtest.py all        # unit + negative + ir + corpus(O0/O3)
bin/ficus -run test/test_all.fx           # the UTest suite directly
```

Layers: **T2** corpus differential (every runnable `.fx` at `-O0` vs `-O3`),
**T3** golden diagnostics (`test/negative/`), **T4** IR snapshots (`test/ir/`),
**T5** randomized suites (`test/rand/`, wired into `test_all.fx`). Harness is
Python 3 stdlib-only (must not depend on the compiler it tests). Discovered
compiler bugs are recorded and fenced (not fixed) in `docs/found_bugs.md`.

## Writing Ficus (.fx) — hard-won gotchas

Ficus is underrepresented in training data; **do not improvise from OCaml/Rust
intuition**. Read `doc/ficustut.md` and existing files (`test/test_basic.fx`,
`test/test_array.fx`, `lib/UTest.fx`) for the constructs you need. Verified traps:

- **Reserved words can't be identifiers**: `ref`, `nan`, `nanf`, `null` (plus
  the obvious `fun`/`val`/`var`/`type`/`match`/...). Naming a variable `ref` or
  `nan` gives a confusing "pattern is expected" / "not an l-value" error.
- **Casts must be parenthesized *with* the operand**: write `(x :> uint8)`, not
  `(x) :> uint8`. Even a bare `x :> T` in some positions (e.g. a `match` arm
  body) fails — wrap it: `(x :> T)`. Chained: `((x :> uint32) :> int32)`.
  For simple types a functional notation, e.g. `uint32(x)`, works too.
- **`println` takes ONE argument.** For several values use an f-string or a
  tuple: `println(f"{a} {b}")` / `println((a, b))`.
- **f-string `{}` interpolation: write nested string literals UNESCAPED** —
  `f"{find("x")}"` works (inside `{}` the lexer is in normal token mode; even
  `"}"` and nested f-strings are fine). The C/Python-style escaped spelling
  `f"{find(\"x\")}"` is what fails, with a misleading "braces are not closed".
- **Record *type* fields are separated by `;`** (`{x: int; y: int}`), but record
  *construction* uses `,` (`pt_t {x=1, y=2}`).
- **Small unsigned types promote to `int` for arithmetic** — `200u8 + 100u8`
  is `300`, not `44`. To wrap, cast explicitly: `((a + b) :> uint8)`. `uint64`
  is full-width and wraps natively.
- Integer `/` truncates toward zero; `%` takes the **sign of the dividend**
  (`-17 % 5 == -2`). `int(x)` truncates toward zero. **`floor`/`ceil`/`trunc`/
  `round` currently return `int`, NOT float/double** (the scalar defs at
  `lib/Math.fx:45-50` are explicitly `: int`; array forms give `int [+]`) —
  verified 2026-07-09. This contradicted an earlier note here; whether it's
  intended (round-to-int) or a lossy bug is open — see FB-019.
- **Border access is `arr.MODE[i]`** (mode before the bracket): `a.clip[i]`,
  `a.wrap[i]`, `a.zero[i]`. Works on `Vector`, strings and arrays.
- Comprehensions: 1D `[for i <- 0:n {..}]`; **2D is nested** `[for i <- 0:m for
  j <- 0:n {..}]`; **zip is comma** `for x <- a, y <- b {..}`; index binding
  `for x@i <- a` (1D) / `for x@(i,j) <- m` (2D); fold `fold acc=init for x <- a
  {..}`. Lists: `[:: 1, 2, 3]`, cons `::`, list-comp `[:: for x <- l {..}]`.
- **Strings are UTF-32** (char sequences): `s.length()` counts characters,
  `s[i]` is a `char`, `s[i:j]` a substring; build from chars via
  `string([for c <- cs {c}])`. `split(s, sep, ~allow_empty=true)` returns a
  `string list`.
- `array(n, init)` / `array((m, n), init)` allocate mutable arrays. Ranges
  `a:b`, `a:b:step`; slices `a[i:j]`, `a[i:j:step]`, `a[:]`, `a[::-1]`.
- `Sys.getenv(name, defval)`; `s.to_int(): int?`; `s.to_int_or(defval)`.
- **A `match` arm body may be a block**: `| pat => val r = ...; expr` is fine
  (the arm extends to the next `|`); no extra `{ }` needed.
- **`continue`/`break` can't be a `match`-arm / `if`-branch *value*, and a bare
  `return` (no value) doesn't parse** — both give a misleading `unexpected token
  '}'` (FB-015). Run the `match` as a statement that `continue`s and yield
  separately (or accumulate into a `var`); invert a void early-`return` guard
  into a wrapping `if`. `return <expr>` is fine.
- **Shift count must be `int`**: `uint64 >> int64` does **not** typecheck
  (`__shr__ (uint64,int64)` not found); write `x >> int(n)`. Runtime uint64 `>>`
  is a correct logical shift — only the *constant folder* got it wrong (FB-002).
- **Overload resolution (resolve-1): the least-generic viable candidate wins**,
  independent of declaration/import order (concrete beats generic, `int complex`
  beats `'t complex`). If no candidate is strictly most specific at a call whose
  argument types are fully known, it's an **ambiguity error** — disambiguate
  with a module-qualified call; for operators use the mangled name:
  `Module.__mul__(a, b)` (`Module.(*)` does not parse). Two identical-signature
  overloads only error at a call that sees both. When argument types still
  contain free type vars, ties silently fall back to first-match (deferral is
  pending) — don't rely on it in new code. A candidate viable only via
  all-defaulted keyword args loses to an exact keywordless match
  (`sqrt(81.0)` → `Math.sqrt(double)`, not a local `sqrt('t, ~n=2)`).
- **`[]` is typed "some collection" (resolve-2, `TypVarCollection`)**: it
  unifies only with a list/vector/array (or a free type var), so
  `val n: int = []` is a typecheck error and a `[]`-initialized fold
  accumulator can't be captured by a non-collection overload. If the
  collection kind is never pinned, K-normalization asks for an annotation.
- **Return-type annotations on generic operators are viability filters, not
  decoration.** A generic `operator` whose return type is left to inference
  returns a free var that unifies with ANY expected type, so the candidate
  stays viable at under-constrained call sites (free fold/recursion
  accumulators) and can steal them via the env-order fallback — dropping
  `: 't complex` from Complex's mixed operators broke the compiler's own
  build (C_gen_code.fx:1328). Annotate with a FRESH var (`: 't3 complex`,
  `: 't3 [+]`, `: ('t3 ...)` — "returns SOME complex/array/tuple") to reject
  foreign contexts while keeping mixed-type widening. **Enforced by
  `python3 tools/lint_op_returns.py lib` (CI, gcc leg)**.
- **`-Wimplicit-rettype` / `-Wall` / `-Werror` exist (annotate-2).** The warning
  flags every **module-level** function (in the root modules named on the
  command line) whose return type is left to inference; nested functions,
  lambdas, `@ccode`, and auto-generated constructors are exempt. `-Werror`
  promotes ALL warnings (incl. the pre-existing unused-value ones) to a nonzero
  exit. The annotated stdlib — all of `lib/` **except** NN/, Onnx/, Protobuf/,
  `OpenCV.fx` — is gated by `tools/rettype_gate.sh` (CI, gcc leg); keep it clean
  when adding stdlib functions. Return annotations **erase in K-form**, so
  adding them yields byte-identical generated C (the bootstrap regen touches no
  stdlib module). The compiler sources are NOT gated yet (annotate-3; ~340 to go).
- **Return-type annotation spellings** (from the sweep): a nullary function type
  is `(void -> T)`, **not** `(() -> T)` (the latter errors "empty tuple"). A
  module's own type parses as a return both qualified (`Date.t`) and bare (`t`)
  inside that module. A uniform tuple return is `(int...)`; an any-dims array is
  `T [+]`. When the body widens via `*0.f` and the result type varies with the
  input (`Builtins.normL1(('t...))`: int→float, double→double), a **fresh scalar
  var `: 't3`** is the honest annotation — safe because the argument is already
  pinned (not a free-return hazard).
- **Comparing `-pr-resolve` censuses: normalize locations first.** Any unrelated
  compiler edit shifts call-site `line:col`, so a raw-text diff is pure noise.
  Gate on the (name | winner | outcome) multiset (strip `@NNN` gensyms and
  `@ path:line:col`); the *expected-arg-type* of a site legitimately sharpens
  from `<unknown>` to a concrete type as callers get annotated — that is not a
  resolution change.

### Build/run & measurement traps (Brief #2)

- **`-o name` drops the executable at the repo ROOT** (`./name`), with all
  intermediates under `__fxbuild__/name/`. `rm -f name` after, or it litters.
- **`-run` hides a runtime SIGSEGV as exit 1** (no output). To see a real crash,
  build a binary (`-o`) and run it directly — you'll get exit 139 and the trace.
- **Build dirs self-invalidate via a `.fxstamp`** (WP-H1): a `-o`/`-B` dir
  caches each module's `.c`/`.o` and reuses them across runs (it skips a module
  whose K-form dump is unchanged), but the compiler now stamps the dir with its
  own binary identity (size+mtime) + the codegen-affecting options (opt level,
  OpenMP, C/C++, `-debug`, inline threshold, cflags/defines). A changed compiler
  or build mode auto-triggers a full rebuild — no `rm -rf` needed between
  before/after diffs, and flipping `-no-openmp`↔OpenMP in one dir no longer
  causes `omp_outlined` link errors. Full clean builds are deterministic;
  incremental churn was FB-008.
- **ASan+UBSan** via `bin/ficus -run -cflags "-fsanitize=address,undefined
  -fno-omit-frame-pointer" -clibs "<same>"`. The runtime is clean post-Brief-2.
- **Signed integer overflow wraps (2's complement)**: ficus builds generated C
  (and itself) with `-fwrapv` — the constant folder wraps, and the runtime must
  match. Do NOT rely on signed overflow being UB, and do NOT drop `-fwrapv`.
- **UB signature at -O2/-O3**: if a value *prints* correctly but a branch/compare
  on it goes the wrong way (e.g. `printf` shows `-4` yet `if (x != -4)` is taken),
  that's the compiler exploiting UB (signed overflow, null-after-check, strict
  aliasing) — the *predicate* is folded to a constant while the *value* is
  materialized correctly. Repro in pure C; `-fwrapv`/`-fno-strict-*` confirm.
- **fxtest wipes `build/fxtest` when `bin/ficus` changes** (WP-H1): the harness
  stamps its cache with the md5 of the compiler and clears it on mismatch, so a
  compiler fix is always picked up (was the FB-011 stale-`.o` trap). Two runs
  with the same compiler reuse the cache — no `rm -rf build/fxtest` needed.
- **IR/`-pr-ast` snapshots are extracted by the harness**, which strips the
  per-module `<abs-path>.fx: <deps>` header. That header is width-wrapped, so a
  long repo path (CI's `/home/runner/work/ficus/ficus/...`) can wrap a dep onto
  its own line and confuse extraction — a golden that passes on a short dev path
  can fail on CI without any compiler change (FB-013). Suspect the harness, not
  the compiler, when only `:ast` differs while `:k0`/`:k` match.

**Iterate tiny**: write 10-20 lines → `bin/ficus -run f.fx` → fix → extend.
The compiler's error messages are ground truth. If it rejects something the
tutorial says is legal, it may be a bug — minimal repro into `docs/found_bugs.md`
and route around it (don't fix `compiler/` unless asked).

### Test-file naming caveat

A test `.fx` filename must not collide **case-insensitively** with a stdlib
module name (`Char`, `List`, `String`, `Array`, `Math`, `Map`, ...): the input
file's directory is searched first for the auto-imported preamble, so
`char.fx` shadows the real `Char`. Numeric prefixes (`001_name.fx`) are safe.
Subdir modules import as `Dir.Module` (`import NN.Ast as Ast`); siblings in the
same dir import by bare name.

## Environment notes (macos only)

- on macos python is **3.9** (no `tomllib` — the harness ships a fallback parser).
  On linux it's more fresh.
- macOS `sed` is BSD (no `\b`); use `perl -pe` for word-boundary edits.
