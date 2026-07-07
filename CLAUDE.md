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
- **Regenerating the bootstrap** (after editing any `compiler/*.fx`): the
  pre-gen C in `compiler/bootstrap/*.c` (one per module) must be refreshed so a
  fresh clone builds the updated compiler. Recipe: `bin/ficus -O3 -o boot
  compiler/fx.fx` (freshly-built compiler regenerates its own C into
  `__fxbuild__/boot/`), then copy the changed `.c` over `compiler/bootstrap/`.
  Sanity: only the module(s) whose `.fx` you edited should differ (`diff` each),
  and after `make` the self-hosted compiler must reproduce the bootstrap
  byte-for-byte (regenerate again → 0 further diffs = fixpoint). `rm -f ./boot`
  after (the `-o` binary litters the repo root).

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
  body) fails — wrap it: `(x :> T)`. Chained: `((x :> uint32) :> int32)`. For simple types a functional notation, e.g. `uint32(x)`, works too.
- **`println` takes ONE argument.** For several values use an f-string or a
  tuple: `println(f"{a} {b}")` / `println((a, b))`.
- **f-string `{}` interpolation can't contain a quoted string literal** —
  `f"{find(\"x\")}"` fails to parse. Hoist to a `val` first.
- **Record *type* fields are separated by `;`** (`{x: int; y: int}`), but record
  *construction* uses `,` (`pt_t {x=1, y=2}`).
- **Small unsigned types promote to `int` for arithmetic** — `200u8 + 100u8`
  is `300`, not `44`. To wrap, cast explicitly: `((a + b) :> uint8)`. `uint64`
  is full-width and wraps natively.
- Integer `/` truncates toward zero; `%` takes the **sign of the dividend**
  (`-17 % 5 == -2`). `int(x)` truncates toward zero; `floor`/`ceil` return the
  **float/double** type, not int (use `int(floor(x))`).
- **Border access is `arr.MODE[i]`** (mode before the bracket): `a.clip[i]`,
  `a.wrap[i]`, `a.zero[i]`. Works on `Vector` and strings; **broken on plain
  `'t []` arrays** (FB-003).
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
- **Shift count must be `int`**: `uint64 >> int64` does **not** typecheck
  (`__shr__ (uint64,int64)` not found); write `x >> int(n)`. Runtime uint64 `>>`
  is a correct logical shift — only the *constant folder* got it wrong (FB-002).

### Build/run & measurement traps (Brief #2)

- **`-o name` drops the executable at the repo ROOT** (`./name`), with all
  intermediates under `__fxbuild__/name/`. `rm -f name` after, or it litters.
- **`-run` hides a runtime SIGSEGV as exit 1** (no output). To see a real crash,
  build a binary (`-o`) and run it directly — you'll get exit 139 and the trace.
- **Incremental compilation reuses the `-o`/`-B` build dir** (it skips a module
  whose K-form *dump*, gensym numbers included, is unchanged). For a clean
  before/after `.c` comparison you MUST `rm -rf` the build dir between builds —
  otherwise stale `.c` from a prior run silently confounds the diff. A full
  clean build is deterministic; incremental churn was FB-008.
- **ASan+UBSan** via `bin/ficus -run -cflags "-fsanitize=address,undefined
  -fno-omit-frame-pointer" -clibs "<same>"`. The runtime is clean post-Brief-2.
- **Signed integer overflow wraps (2's complement)**: ficus builds generated C
  (and itself) with `-fwrapv` — the constant folder wraps, and the runtime must
  match. Do NOT rely on signed overflow being UB, and do NOT drop `-fwrapv`
  (`compiler/Compiler.fx` cflags + `GNUmakefile` CC). Without it, gcc 15 at
  `-O2/-O3` miscompiled overflowing arithmetic (FB-011).
- **UB signature at -O2/-O3**: if a value *prints* correctly but a branch/compare
  on it goes the wrong way (e.g. `printf` shows `-4` yet `if (x != -4)` is taken),
  that's the compiler exploiting UB (signed overflow, null-after-check, strict
  aliasing) — the *predicate* is folded to a constant while the *value* is
  materialized correctly. Repro in pure C; `-fwrapv`/`-fno-strict-*` confirm.
- **fxtest caches its own build dir** (`build/fxtest/<suite>/...`); since the
  cfold/oracle source is regenerated identically each run, a compiler fix won't
  be picked up there — `rm -rf build/fxtest` before re-measuring after any
  compiler change (same incremental trap as above; CI is a clean checkout so it's
  unaffected).
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
same dir import by bare name. Don't reuse one `-B` build dir across `-no-openmp`
and OpenMP builds — stale `.o` files cause `omp_outlined` link errors; use
isolated build dirs.

## Environment notes (this machine)

- Local Python is **3.9** (no `tomllib` — the harness ships a fallback parser).
- macOS `sed` is BSD (no `\b`); use `perl -pe` for word-boundary edits.
