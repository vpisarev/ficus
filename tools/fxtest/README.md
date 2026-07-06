# fxtest — Ficus compiler test harness

A layered "ladder of oracles" that makes compiler changes safe to make: each
layer is an independent way to catch a regression the others would miss.

**Two-language rule.** The harness (this directory) is **Python 3, stdlib only**
— it must not depend on the compiler it tests. The test *programs* are Ficus
(`.fx`). Do not add third-party Python packages.

```
tools/fxtest/
  fxtest.py       single entry point (discover / build / run / compare / report)
  manifest.toml   per-corpus-entry metadata (args, compare mode, quarantine)
  normalize.py    output / IR normalization helpers (pure string->string)
  README.md       this file
```

## The layers

| cmd        | layer | what it proves |
|------------|-------|----------------|
| `unit`     |       | the existing `test/test_all.fx` UTest suite passes |
| `corpus`   | T2    | every runnable `.fx` gives identical output at `-O0` and `-O3` (miscompilation detector) |
| `negative` | T3    | intentionally-broken programs produce the expected compiler diagnostics (golden `.err`) |
| `ir`       | T4    | small programs produce the expected typechecked-AST / K-form (golden snapshots) |
| `all`      |       | all of the above |

## Usage

```sh
make -j$(nproc)                          # build bin/ficus first
python3 tools/fxtest/fxtest.py all       # everything (PR-level)
python3 tools/fxtest/fxtest.py corpus --opt O0,O3 --jobs 8
python3 tools/fxtest/fxtest.py corpus --cpp-smoke --openmp-smoke   # nightly axes
python3 tools/fxtest/fxtest.py corpus --filter "mandelbrot"
python3 tools/fxtest/fxtest.py unit
```

**Exit code**: `0` = all green. Quarantined / `xfail` entries never fail the run;
an `xfail` that unexpectedly **passes** is reported as `XPASS` and *does* fail the
run ("remove from quarantine?"). Build artifacts go under `build/fxtest/`
(git-ignored).

Status tags: `PASS` `FAIL` `SKIP` (quarantined) `XFAIL` (known bug, fenced)
`XPASS` (fenced bug now passes — investigate).

## T2 — corpus differential

Every runnable `.fx` is compiled and run at `-O0` and `-O3` (with `-no-openmp`
for determinism); its output must agree between the two. This turns the whole
codebase into a miscompilation detector.

Each entry runs with `cwd` = its own build dir, so relative output files
(artifacts) are isolated per `(entry, opt)`.

### Determinism policy (why comparison is not naive `diff`)

- **Timings** (`(12 ms)`) and the `test_all.fx` banner (version/platform/
  compiler) are normalized away — see `normalize.py`.
- **Floating point is not byte-stable across `-O0`/`-O3`** (reassociation, FMA
  contraction, fusion). Entries whose output is numeric use `compare = "float"`.
- **`@parallel` breaks run-to-run determinism**, so the differential runs with
  `-no-openmp`. Parallel execution is covered by the OpenMP smoke axis and by
  `test_parallel.fx` in the unit suite — never diff parallel float output.
- `-Ofast` is excluded from the differential by design (it may relax semantics).

### Adding a corpus entry

Add a table to `manifest.toml`:

```toml
[corpus.myprog]
path = "examples/myprog.fx"   # required, repo-relative
args = ["256"]                # args after `--` (give benchmarks a SMALL N!)
compare = "exact"             # exact | float | artifact | none
# rtol = 1e-9                 # for compare="float"
# atol = 0.0
# artifact = "out.pgm"        # for compare="artifact" (compare a file, not stdout)
# strip_header = true         # drop test_all.fx banner lines
# timeout_sec = 60            # run timeout (default 60)
# compile_timeout_sec = 180   # build timeout (default 180)
# quarantine = "reason"       # listed but skipped (nondeterministic / needs data)
# cpp_xfail = "FB-NNN: ..."   # fenced known failure of the -c++ smoke
```

`compare` modes:
- **exact** — byte-exact stdout after normalization (integer/string output).
- **float** — numbers compared positionally within `rtol`/`atol`, other text
  exact (reassociation-sensitive numeric output).
- **artifact** — compare the bytes of the file named by `artifact` instead of
  stdout (e.g. `mandelbrot` writes a PGM).
- **none** — smoke only: builds, runs, exits 0.

Entries that cannot participate (need external data, nondeterministic, too slow)
get `quarantine = "reason"` and stay *listed* so coverage is auditable. If the
reason looks like an unintended compiler bug, also add it to
`docs/found_bugs.md`.

## Found bugs

Bugs discovered by the harness are recorded in `docs/found_bugs.md` and fenced
(quarantine / xfail) rather than fixed — see that file. Example: **FB-001**
(interface C++ codegen) is fenced via `cpp_xfail` on the `test_all` entry.

## T3 — negative (golden diagnostics)

Tiny intentionally-broken programs pin down the compiler's *error* behavior.
Each case is `test/negative/NNN_short_name.fx` (self-contained, < ~20 lines, one
intended error) plus a golden `NNN_short_name.err`. The runner invokes
`bin/ficus -no-c <file>` (parse + typecheck only), expects a nonzero exit, and
compares normalized stdout+stderr to the golden.

Every case's **first line** is a machine-checked intent marker:

```
// expect: <substring the diagnostic must contain>
val y = x + 1          // triggers "the appropriate match for 'x' ... is not found"
```

The runner fails a case that (a) compiles cleanly, (b) does not contain its
`expect:` substring (triggered the *wrong* error), or (c) differs from the
golden. Normalization: absolute paths → basename, ANSI stripped, repeated
whitespace collapsed; the **test file's own `line:col` is kept** (it's the
contract) but positions inside *other* files (stdlib candidate dumps) collapse
to `:L:C` so unrelated stdlib edits don't break these goldens.

### Adding / updating negative cases

```sh
# 1. write test/negative/NNN_name.fx with a `// expect:` first line
# 2. generate its golden:
python3 tools/fxtest/fxtest.py negative --update-golden
# 3. eyeball the new .err, then commit both files
python3 tools/fxtest/fxtest.py negative           # must be green
```

`--update-golden` warns if a case compiles cleanly or misses its `expect:`
substring. Cases under `test/negative/crashes/` document compiler bugs (see
`docs/found_bugs.md`): they are fenced — a golden match is `XFAIL`, any change is
`XPASS` (investigate).

## T4 — IR snapshots

Small focused programs in `test/ir/` (one language feature each) are dumped at
three compiler stages and compared to goldens:

| stage | invocation | golden |
|-------|------------|--------|
| typechecked AST | `-no-c -pr-ast` | `<name>.ast.golden` |
| initial K-form  | `-no-c -pr-k0`  | `<name>.k0.golden` |
| optimized K-form| `-no-c -O3 -pr-k` | `<name>.k.golden` |

(`-pr-k` works with `-no-c` — the optimizer runs before C generation; verified
empirically.) These are the proof, during a later syntax reform, that a
pure-syntax change is semantics-preserving.

**Normalization is the crux.** The compiler dumps *every* auto-imported module;
only the section for the file-under-test (always last) is kept. Generated ids
`name@1234` embed a global counter that shifts on any unrelated edit, so the
numeric parts are renumbered by first appearance (`@g0, @g1, ...`) in
`normalize.ir_extract_module` / `normalize_ir`. The optimized `-pr-k` mangled
names (`_fx_F12print_stringv1S`) are length-prefixed, not counter-based, so they
are already stable.

**Validated:** appending unused functions to an unrelated module (`lib/Math.fx`)
leaves all 57 snapshots byte-identical — the renumbering makes snapshots depend
only on the structure of the program under test. Re-run this check after
changing the normalizer:

```sh
# append a dummy fn to lib/Math.fx, then:
python3 tools/fxtest/fxtest.py ir --update-golden   # (to a scratch copy) must not diff
```

Keep the suite small (15–25 programs) — IR snapshots are high-maintenance and
earn their keep only for core constructs.

### Adding / updating IR snapshots

```sh
# 1. write a small test/ir/<feature>.fx (10-30 lines, one construct)
python3 tools/fxtest/fxtest.py ir --update-golden    # writes .ast/.k0/.k goldens
# 2. eyeball the goldens, then commit the .fx + its 3 .golden files
python3 tools/fxtest/fxtest.py ir                     # must be green
```

## T5 — deterministic randomized suites

`test/rand/` runs random *data* through fixed code paths and checks the result
against a straightforward reference implementation. These are ordinary UTest
modules wired into `test_all.fx`, so they also ride the T2 differential: the
reference and the optimized comprehension must agree at every optimization
level.

- `RandUtil.fx` — splitmix64 generator (pure Ficus; see FB-002 workaround),
  random array/string/list builders, and reproducible per-case seeding.
- `test_rand_arith.fx` — integer wraparound / div-mod / shifts / saturating
  casts / float↔int / NaN-Inf vs scalar references.
- `test_rand_array.fx` — comprehensions, zip, 2D, slicing, border access vs
  handwritten loops.
- `test_rand_str.fx` — string slice / find / split-join / concat / Unicode
  length vs naive references.

**Reproducibility.** The base seed is `FXTEST_SEED` (default a fixed constant),
and each case derives `case_seed = splitmix64(base ^ hash(test_name) ^ index)`.
Every failure prints its per-case seed and index; replay it directly with
`mk_rng(seed)`. Same `FXTEST_SEED` ⇒ identical run; change it to explore more of
the input space.

```sh
FXTEST_SEED=999 bin/ficus -run test/test_all.fx -- -f "rand.*"
```

Case counts are tuned so T5 adds only a couple of seconds to `test_all.fx`.
Several compiler bugs (FB-002..FB-005) were found while writing these suites and
are routed around in the suite code with a comment + a `docs/found_bugs.md`
entry.
