# Ficus housekeeping (mini-WP H) — report

Branch: `housekeeping-1` off `master` (post `harden-1`). Three commits, one per
WP. **Not pushed** — diffs offered for review.

| commit    | WP    | subject |
|-----------|-------|---------|
| `987e898` | WP-H2 | `tools/update_compiler.py` — mechanize bootstrap regen |
| `a6794da` | WP-H1 | invalidate build caches by compiler identity + build mode |
| `9032734` | WP-H3 | emit folded INT64_MIN as a signed split literal (FB-010) |

Net change (excluding regenerated bootstrap C): **+420 / −42** across 9 files.
Regenerated bootstrap: `Compiler.c`, `Options.c` (WP-H1), `K_form.c` (WP-H3) —
each verified as the *only* module that changed, fixpoint holding after every WP.

Closing ladder (all green — see the tail of this file):
`test_all` 135/135 · `fxtest all` PASS · `determinism` PASS=3 ·
`sanitize` clean (ASan+UBSan) · bootstrap `--check` up to date.

---

## WP-H2 — `tools/update_compiler.py` (done first; H1/H3 dogfood it)

Replaces the hand-run "Regenerating the bootstrap" recipe (previously prose in
CLAUDE.md) with a Python-3, stdlib-only tool.

- **default**: `make` → regenerate the compiler's own C into `__fxbuild__/boot/`
  → copy over only the changed `compiler/bootstrap/*.c` → rebuild → regenerate
  again and **assert the fixpoint** (0 further diffs). Loud non-zero exit if the
  self-hosted compiler fails to reproduce its own bootstrap (a determinism
  regression → points at `fxtest.py determinism`).
- **`--check`**: dry run; reports the modules that *would* change and exits 1 if
  any (CI-friendly; not wired into CI in this WP).
- **`--no-make`**: skip the initial build. Wipes `__fxbuild__/boot` before each
  regen so its result never rides on a stale cache; removes the stray `./boot`.

Validated: clean tree → "already at fixpoint" (exit 0); the probe edit
(`compiler/Options.fx` help string) → exactly `Options.c` changes and the
fixpoint holds; a source-only revert leaving the bootstrap stale → `--check`
exit 1. Dogfooded for the H1 and H3 bootstrap regens (each: only the edited
module's `.c` changed).

CLAUDE.md: the multi-line recipe became a one-liner + a one-sentence fixpoint note.

## WP-H1 — build-cache invalidation by compiler identity + build mode

Root problem: `-o`/`-B` build dirs and the fxtest cache reuse `.c`/`.o` across
runs, with nothing invalidating them when the **compiler binary** or a
**codegen-affecting build mode** changes underneath. This bit Brief #2 three
times (FB-008 mis-measurement, FB-011 stale `.o`, omp/no-omp `omp_outlined` link
errors) and was papered over with "`rm -rf` the dir" folklore in three CLAUDE.md
traps. Replaced the folklore with a stamp.

- **Compiler side** (`compiler/Compiler.fx`, `Options.fx`): `k_skip_some` writes
  `<build_dir>/.fxstamp` = a fingerprint of the running compiler binary (its own
  file size+mtime, via a small `@ccode compiler_signature` — `/proc/self/exe` on
  Linux, `_NSGetExecutablePath` on macOS, `GetModuleFileName` on Windows) plus
  the codegen-affecting options (opt level, OpenMP, C/C++, `-debug`, inline
  threshold, `-cflags`/`FICUS_CFLAGS`/`-D`). On mismatch it forces a full rebuild
  (`Options.force_full_rebuild`, reusing the existing `-rebuild` machinery) and
  refreshes the stamp. size+mtime rather than an md5 of the ~6.5 MB binary: this
  is queried once per compile — hundreds of times across an fxtest run — and
  mtime is exactly ccache's default `compiler_check`.
- **fxtest side** (`tools/fxtest/fxtest.py`): `invalidate_stale_cache()` stamps
  `build/fxtest` with the md5 of `bin/ficus` (computed once per run, so md5 is
  fine here) and wipes it on mismatch. Same compiler across two runs → cache
  reused, no perf regression.

Acceptance, all reproduced green:
- edit a compiler module (banner in `C_gen_code.fx`) → `make` → rebuild a program
  into the **same** `-B` dir: the stamp forces a full regen; all `.c` are
  **byte-identical** to a clean-dir build (0 differences). Without the stamp the
  program's K-form is unchanged, so the stale (old-banner) `.c`/`.o` would have
  been reused — exactly the FB-011 trap.
- `-no-openmp`↔OpenMP in one `-B` dir: the mode flip invalidates the stamp and
  relinks with the right flags — no `omp_outlined` errors, in either direction.
- `fxtest` twice with the same compiler → cache reused (no wipe); a genuinely
  different `bin/ficus` (md5) → `build/fxtest` wiped, marker file gone.
- `determinism` green (`.fxstamp` is not a `.c`, so the byte-compares ignore it;
  the incremental unrelated-change check reuses the same compiler → stamp matches
  → normal incremental behaviour, FB-008 property intact).

CLAUDE.md: **three traps deleted**, replaced with two stamp lines (below).

## WP-H3 — FB-010: folded INT64_MIN emitted as a bare (unsigned) C literal

A constant-folded INT64_MIN (`-9223372036854775807 - 1` etc.) was printed to C
bare as `-9223372036854775808`. C parses that as unary minus on
`9223372036854775808`, a constant too large for `long long` and therefore
**unsigned** — a default `-Wimplicitly-unsigned-literal` warning *and* unsigned
semantics in any enclosing expression (which is how the folded operand disagreed
with the signed runtime). Empirically confirmed a bare `LL` suffix does **not**
help (`9223372036854775808LL` is still out-of-range/unsigned and still warns).

Fix (`compiler/K_form.fx`, `klit2str`): emit INT64_MIN as the canonical
warning-free **signed split form** `(-9223372036854775807LL - 1)` for both
`KLitInt` and `KLitSInt(64)`, via a small `i64_c_literal` helper; every other
literal is unchanged. **Blanket-suffixing every `int` literal (the brief's
suggested approach) was deliberately rejected**: measured, it churns every
generated module for zero correctness gain *and still warns on INT64_MIN* — so it
fails the "compile warning-free" acceptance while the split form meets it. `klit2str`
is the single C funnel for all literals (scalar, array-initializer, default-value),
so the one change covers every path.

Tests: `basic.int64_min` (fold-vs-runtime agreement + signed `/2`, `%`, `<0`,
`+1`, wrap); `cfold_gen.py` widened to the full signed range `[-2^63, 2^63-1]`,
reaching INT64_MIN through in-range arithmetic operands (`_lit`) so the literal
stays typecheckable — `fxtest cfold` 0/500 at -O0 and -O3 across five seeds, the
split form appearing 50× in a 500-expr oracle. FB-010 moved to *fixed*; the
typechecker's symmetric *literal* range (rejecting a bare `-2^63` in source) is
left as-is — a language-design item parked in `docs/language_changes_brief.md` §1.3.

---

## CLAUDE.md diff — sections removed

Three "hard-won gotchas" are now obsolete and were deleted, replaced by the two
stamp lines (net −1 line, but −3 traps):

1. **"Incremental compilation reuses the `-o`/`-B` build dir … you MUST `rm -rf`
   the build dir between builds"** → replaced by *"Build dirs self-invalidate via
   a `.fxstamp`"* (compiler stamps binary identity + codegen options; changed
   compiler/mode auto-triggers a full rebuild).
2. **"fxtest caches its own build dir … `rm -rf build/fxtest` before
   re-measuring"** → replaced by *"fxtest wipes `build/fxtest` when `bin/ficus`
   changes"* (md5 stamp; two same-compiler runs reuse the cache).
3. **"Don't reuse one `-B` build dir across `-no-openmp` and OpenMP builds — stale
   `.o` … `omp_outlined` link errors"** (in the test-file-naming caveat) →
   **removed entirely**; the mode is part of the stamp, so the flip is safe.

Also updated: the "Regenerating the bootstrap" bullet (prose recipe → one-liner
pointing at `tools/update_compiler.py`, keeping one sentence on the fixpoint).

## docs/found_bugs.md

- **FB-010** → *fixed* (WP-H3), with the corrected root cause (unsigned literal
  *and* unsigned expression semantics; bare `LL` insufficient), the split-form
  fix, the rejected blanket-suffix alternative, and the parked typechecker
  literal-range question. No new bugs were discovered during this WP.

## Lessons for the next CLAUDE.md revision

- **Folklore → mechanism.** Every "you MUST `rm -rf` …" trap was a missing cache
  key. When a doc note is a manual workaround for a stale cache, the durable fix
  is a stamp, not a better-worded warning. Two more caches worth auditing the
  same way if they ever bite: the `-o` app-up-to-date short-circuit and any
  future CI artifact cache.
- **"Simpler" blanket rules deserve a measurement.** The brief preferred blanket
  `LL` suffixing "for stability"; measuring showed it neither fixes INT64_MIN nor
  stays clean. Prefer the surgical fix + evidence when they diverge.
- **Prose recipes rot; scripts assert.** The bootstrap recipe drifted from the
  real invocation; `update_compiler.py` now *is* the source of truth and fails
  loudly on a broken fixpoint. Candidate follow-up: a CI leg running
  `update_compiler.py --check` so a stale bootstrap can never merge.

## Closing checklist (this run)

```
unit  test_all.fx ............ PASSED 135/135
fxtest all .................. PASSED  (unit 1 · negative 64 · ir 60 · cfold 2 · corpus 11, skip 7)
fxtest determinism .......... PASS=3  (2 rebuilds byte-identical + unrelated-change stable)
fxtest sanitize ............. PASS    (clean under ASan+UBSan)
update_compiler.py --check .. bootstrap up to date (fixpoint)
```
