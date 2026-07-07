# Brief #1: Ficus Compiler Test Infrastructure

**Target repo:** https://github.com/vpisarev/ficus (work on a branch, e.g. `test-infra`)
**Executor:** Claude Code (agentic session, multiple phases with checkpoints)
**Author of record / reviewer:** Vadim Pisarevsky

## 0. Context and goal

Ficus is a self-hosted functional language for computing that compiles to C/C++.
The compiler (`compiler/*.fx`, ~50 passes across AST → K-form → C-form) currently has:

- a unit-test suite: `test/test_all.fx` importing ~25 modules built on `lib/UTest.fx`
  (`TEST(name, fn)`, `ASSERT_EQ/NE/LT/...`), run via `bin/ficus -run test/test_all.fx`;
- runnable examples in `examples/` (several take a size argument N, benchmarksgame-style);
- the strongest existing oracle: the compiler bootstraps itself (`make` builds `ficus0`
  from pre-generated C, then `ficus` from `.fx` sources).

Known problem this brief attacks: **coverage is thin and regressions are whack-a-mole** —
fixing the typechecker or an optimization pass in one place silently breaks another.
The goal is a layered test infrastructure ("ladder of oracles") that makes every
subsequent compiler change (bug-fixing brief #2, syntax reform brief #3) safe to delegate.

**The goal of this brief is to FIND and FENCE bugs, not to fix them.** Compiler bugs
discovered along the way are recorded and quarantined, never silently patched.

## 1. Non-goals

- No fixes inside `compiler/` except trivial blockers explicitly approved at a checkpoint.
- No random *program* generator yet (future brief; this brief lays its foundation:
  deterministic seeding + differential runners).
- No syntax or semantics changes. No stdlib extensions beyond test helpers.
- No Windows CI in v1 (Linux x86_64 + macOS arm64 only).

## 2. Architecture overview

Two-language rule:

- **Test harness / runners / CI glue: Python 3 (stdlib only), in `tools/fxtest/`.**
  Rationale: reliability and agent fluency; the harness must not depend on the very
  compiler it is testing.
- **Test programs themselves: Ficus (`.fx`).** Model every new `.fx` file on existing
  code (`test/test_basic.fx`, `test/test_array.fx`, `lib/UTest.fx`). See §8 survival kit.

Directory layout to create:

```
tools/fxtest/
  fxtest.py            # single entry point: discover, build, run, compare, report
  manifest.toml        # per-test metadata (args, timeout, compare mode, quarantine)
  normalize.py         # output/IR normalization helpers
  README.md
test/negative/         # golden diagnostics tests (T3): NNN_short_name.fx + .err
test/ir/               # IR snapshot tests (T4): small .fx + .ast.golden / .k.golden
test/rand/             # randomized data-driven suites (T5), regular UTest modules
docs/found_bugs.md     # registry of discovered compiler bugs (append-only)
.github/workflows/ci.yml
```

`fxtest.py` CLI contract:

```
fxtest.py corpus   [--opt O0,O3] [--jobs N] [--filter GLOB]   # T2
fxtest.py negative [--update-golden]                          # T3
fxtest.py ir       [--update-golden]                          # T4
fxtest.py unit                                                # wraps test_all.fx
fxtest.py all
```

Exit code 0 = all green (quarantined/xfail tests do not fail the run; an xfail that
unexpectedly PASSES is reported loudly as "XPASS — remove from quarantine?").

## 3. T2 — Corpus differential testing (the cheap, powerful layer)

**Idea:** every runnable `.fx` in the repo is compiled and executed at `-O0` and `-O3`;
stdout must match. This turns the whole codebase into a miscompilation detector.

Corpus = `test/test_all.fx` (as one entry) + each standalone-runnable file in
`examples/` + selected `test/*.fx` that have a `main`-like top level.

Mechanics:

- Build each corpus entry per optimization level into separate build dirs
  (`-B` flag controls build root; use `build/fxtest/<name>/<opt>/`).
- Run with per-entry args from `manifest.toml`. Benchmark-style examples MUST get
  small N (e.g. `mandelbrot 256`, `nbody 10000`, `spectralnorm 300`) — they default
  to huge N. Per-entry `timeout_sec` (default 60, compile timeout 180).
- Compare stdout between opt levels according to `compare` mode (see §3.1).
- Also include these single-config smoke axes (no cross-comparison, just "builds & runs
  & exits 0"): `-c++` mode for the corpus; default (OpenMP-enabled) build of
  `test_all.fx`.

### 3.1 Determinism policy — read carefully, this is where naive automation dies

1. **Default comparison mode: byte-exact stdout**, after normalization (strip the
   `Ficus version / Platform / Compiler` header lines that `test_all.fx` prints,
   normalize absolute paths, CR/LF).
2. **Floating point is NOT byte-stable across `-O0`/`-O3` in general** (reassociation,
   FMA contraction in the C compiler, loop fusion changing summation order). Policy:
   - `compare = "exact"` — default; use for integer/string-output entries.
   - `compare = "float"` with `rtol`/`atol` — parse numbers out of both outputs
     positionally, compare within tolerance, compare non-numeric text exactly.
     Use for nbody/spectralnorm-style entries. Start with `rtol=1e-10` for double
     pipelines, tighten/loosen per entry with a comment explaining why.
   - `compare = "none"` — smoke only (builds, runs, exit 0).
3. **`@parallel` breaks run-to-run determinism** (accumulation order, scheduling).
   The differential corpus is therefore run with **`-no-openmp`**. Parallel execution
   is covered separately by the OpenMP smoke axis and by `test_parallel.fx` in the
   unit suite. Do not attempt to diff parallel float output.
4. `-Ofast` is excluded from differential comparison by design (it is allowed to
   relax semantics). Optional smoke axis only.
5. Any entry with residual nondeterminism (time, RNG without fixed seed, dir listing
   order) gets `quarantine = "reason"` in the manifest instead of a hacky fix, and a
   note in `docs/found_bugs.md` if the nondeterminism looks unintended.

### 3.2 Manifest format (`tools/fxtest/manifest.toml`)

```toml
[corpus.mandelbrot]
path = "examples/mandelbrot.fx"
args = ["256"]
compare = "exact"        # prints a PGM; bytes must match

[corpus.nbody]
path = "examples/nbody.fx"
args = ["10000"]
compare = "float"
rtol = 1e-9

[corpus.btree]
path = "examples/btree.fx"
args = ["10"]
compare = "exact"

# entries not listed inherit defaults: args=[], compare="exact", timeout_sec=60
```

## 4. T3 — Golden diagnostics tests (negative programs)

Purpose: pin down typechecker/parser *error* behavior. This is simultaneously a
regression net for brief #2 (typechecker hardening) and a spec of diagnostic quality
for the future LSP.

- Each test: `test/negative/NNN_short_name.fx` (one file, self-contained, SMALL —
  under ~20 lines, exactly one intended error) + `NNN_short_name.err` golden.
- Runner invokes `bin/ficus -no-c <file>` (parse+typecheck only, no C generation),
  expects nonzero exit, captures stderr+stdout.
- Normalization before comparison: replace absolute paths with basename; keep
  file:line:col — positions are part of the contract; collapse repeated whitespace.
- `--update-golden` regenerates `.err` files; the diff is then reviewed by a human.
- **Initial content (target: 60–100 cases).** Mine three sources:
  1. every distinct `compile_err(...)` message family in `compiler/Ast_typecheck.fx`,
     `Parser.fx`, `Lexer.fx` — grep the format strings, write a minimal program
     triggering each reachable one (unreachable/internal ones: skip, list in report);
  2. classic traps: type mismatch in `if` branches, non-exhaustive match, wrong arity,
     unbound identifier, duplicate definition, recursive type without variant,
     generic instantiation failure, mutating a `val`, wrong array dimensionality;
  3. any crash found while doing (1)-(2): a program that makes the compiler abort or
     throw an internal error is GOLD — quarantine it as `expect = "crash"` in a
     `test/negative/crashes/` subfolder and log it in `docs/found_bugs.md`.
- Multiple-errors behavior: the compiler collects errors (`all_compile_errs`);
  goldens capture whatever is currently printed. Do not "improve" messages.

## 5. T4 — IR snapshot tests

Purpose: detect unintended changes in typechecked AST and K-form; later, during syntax
reform, these become the proof that a pure-syntax change is semantics-preserving.

- Small focused programs in `test/ir/` (10–30 lines: one feature each — comprehension,
  closure, pattern match lowering, exception handling, tail recursion, fold, generic
  instantiation, string interpolation, tuple ops).
- Runner: `bin/ficus -no-c -pr-ast <file>` → `<name>.ast.golden`;
  `bin/ficus -no-c -pr-k0 <file>` → `<name>.k0.golden`;
  `-pr-k` (post-optimization K-form) at `-O3` → `<name>.k.golden`.
  (Verify exact flag/stage interaction empirically first; adjust if `-pr-k` requires
  full pipeline. Record findings in tools/fxtest/README.md.)
- **Normalization is the crux:** generated ids/gensyms contain counters that shift with
  any unrelated change. In `normalize.py`, canonicalize identifiers matching the
  gensym pattern (inspect real dumps first; typical shape `name@1234` or `t_1234`) by
  renumbering them in order of first appearance (`g0, g1, ...`). Without this the
  snapshots are useless noise. Validate: inserting an unused top-level function in an
  unrelated module must not change a snapshot.
- Keep the suite small (15–25 files). IR snapshots are high-maintenance; they earn
  their keep only for core constructs.

## 6. T5 — Deterministic randomized suites (foundation layer)

Not a program generator — random *data* through fixed code paths, differentially
checked against straightforward reference implementations. Pattern proven in OpenCV
work: splitmix64 per-case seeding, every failure reproducible from a printed seed.

- Add `test/rand/RandUtil.fx`: splitmix64 state, `next_u64/next_int(range)/next_double`,
  plus helpers to build random arrays/strings/lists. Seed policy: base seed from env
  `FXTEST_SEED` (default fixed constant for CI reproducibility), per-case seed =
  `splitmix64(base ^ hash(test_name) ^ case_index)`. EVERY failure message must print
  the per-case seed and case index (extend UTest usage pattern, not UTest itself).
- Suites (each a normal UTest module wired into `test_all.fx`):
  1. `test_rand_arith.fx` — integer ops incl. wraparound/division/shift corner cases,
     saturating casts (`sat_uint8`-style patterns), float↔int conversions with NaN/Inf
     bounds — compare against carefully written scalar reference expressions;
  2. `test_rand_array.fx` — comprehensions vs handwritten loops: map, zip of 2 arrays,
     2D comprehension, slicing (contiguous and strided), border access `.clip/.zero/.wrap`
     vs explicit index clamping reference;
  3. `test_rand_str.fx` — string slicing/find/split vs naive references, Unicode
     content included.
- Case counts tuned so the whole T5 adds ≤ 20 seconds to `test_all.fx` (these run at
  both `-O0` and `-O3` via T2 — that's the differential payoff: the *reference* and
  the *optimized comprehension* must agree at every opt level).

## 7. T6 — CI (GitHub Actions)

`.github/workflows/ci.yml`, matrix:

- ubuntu-latest / gcc, ubuntu-latest / clang, macos-latest (arm64) / clang.
- Steps: checkout → `make -j$(nproc)` → `python3 tools/fxtest/fxtest.py all --jobs N`.
- Cache `__fxbuild__`/build dirs keyed on compiler version + source hash (best effort;
  correctness first, speed second).
- On failure upload the failing build dir + diff outputs as artifacts.
- Total budget target: ≤ 15 min per matrix leg. If corpus×opt-levels blows the budget,
  split: PR run = `unit + negative + ir + corpus(-O0,-O3)`; nightly = full axes
  (`-O1`, `-c++`, `-Ofast` smoke).

## 8. Ficus survival kit for the agent (READ BEFORE WRITING ANY .fx)

Ficus is underrepresented in training data. Do not improvise syntax from OCaml/Rust/
Python intuition. Rules:

1. Before writing any `.fx`, read `doc/ficustut.md` sections on the constructs you
   need, and open 2–3 existing files using them (`test/test_basic.fx`,
   `test/test_array.fx`, `lib/UTest.fx` are canonical). `examples/fst.fx` and maybe other examples may be useful too. 
2. Iterate in tiny steps: write 10–20 lines → `bin/ficus -run file.fx` → fix → extend.
   The compiler's error messages are the ground truth, not your prior.
3. Cheat-sheet of trap differences (verify each against the tutorial when used):
   - immutable `val` vs mutable `var`; everything is an expression; no `return`
     needed at tail position; blocks `{}` with `;`/newline separators;
   - generics: `'t` type vars, OCaml-style postfix in places (`'t list`);
   - arrays `[...]`/`[| ... |]` vs lists (`[:: a, b]` literals, `::` cons in patterns);
     array comprehension `[for i <- 0:n { ... }]`; ranges `a:b`;
   - string interpolation `f"..."` with `{}`;
   - `match ... { | pat => ... | _ => ... }`; exceptions via `throw` / `try/finally`;
   - `@parallel`, `@ifdef` attributes; C blocks `@ccode { ... }` exist — do NOT use
     them in new test code.
4. If the compiler rejects something the tutorial says is legal — that is potentially
   a bug: minimal repro into `docs/found_bugs.md`, then route around it.

## 9. Found-bugs protocol

`docs/found_bugs.md`, append-only entries:

```
## FB-007  K_fuse_loops: wrong result fusing comprehension over slice
- repro: test/negative/crashes/... or inline 5-line snippet
- config: -O3, linux/gcc; -O0 output differs: <expected> vs <got>
- status: quarantined in manifest / xfail in suite X
```

Quarantine ≠ delete: the test stays, marked xfail, and the CI reports XPASS when a
future fix lands. This file is the direct input to brief #2.

## 10. Phases and checkpoints (stop and report at each)

1. **P1:** `fxtest.py` skeleton + T2 corpus at `-O0`/`-O3` with manifest for all
   existing runnable files; quarantine what's flaky; report pass/fail table.
2. **P2:** T3 negative suite (target ≥ 60 cases) + normalization; report the list of
   error-message families covered vs skipped.
3. **P3:** T4 IR snapshots (15–25 files) + gensym normalization validated.
4. **P4:** T5 RandUtil + three randomized suites wired into test_all.fx.
5. **P5:** CI workflow green on the matrix; README for tools/fxtest; final report:
   coverage summary + found_bugs.md digest.

Before starting and after every phase run the full sanity loop:
`make -j8 && bin/ficus -run test/test_all.fx && python3 tools/fxtest/fxtest.py all`.
One commit per logical unit; commit messages `fxtest: <what>`.

## 11. Acceptance criteria

- `fxtest.py all` green locally (linux) and on CI matrix; runtime within budget.
- ≥ 60 negative golden tests; ≥ 15 IR snapshots; 3 randomized suites with printed-seed
  reproducibility (demonstrate: given a failure seed, rerun reproduces it).
- Corpus differential run covers 100% of runnable examples + test_all at O0/O3.
- Zero silent compiler modifications; docs/found_bugs.md exists (even if empty).
- A new contributor (or agent) can add a test of each kind by following
  tools/fxtest/README.md alone.
