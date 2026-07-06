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

<!-- T3 (negative), T4 (ir) and T5 (rand) sections added in later phases. -->
