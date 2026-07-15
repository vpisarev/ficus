# Brief: doctut-1 — the tutorial becomes executable (Opus)

**You are Claude Code (Opus) working in the Ficus repo.** Branch `doctut-1`
off master. Prerequisite (Vadim): the rewritten `doc/ficustut.md` is
committed. Read `docs/ficustut_rewrite_summary.md` first — it maps which
sections were rewritten (higher risk of code errors: the rewrite was done
WITHOUT compiling the snippets, deliberately leaving verification to you)
and lists the two known open items. Repo CLAUDE.md as always.

Goal: every code block in the tutorial is machine-verified, forever. A future
reform that breaks a documented example must break CI.

## WP-1 — the extractor + `fxtest.py doctut` leg

`tools/fxtest/doctut.py` (stdlib-only): extract fenced code blocks from
`doc/ficustut.md`, classify, compile, optionally run. Classification is via
**HTML-comment directives on the line right before the fence** — invisible in
the rendered PDF, so the tutorial text stays clean:

- (no directive) — a standalone runnable Ficus program: compiled with
  `bin/ficus -no-c` (NOT `-Wall`: tutorial examples legitimately skip return
  annotations); if it is a complete program with output, also `-run` (exit 0,
  no uncaught exceptions — no output goldens in v1).
- `<!-- doctut: fragment -->` — deliberately partial (grammar schemas like
  `fold acc1=initval1 ...`, `<type definition body>`, elided `...` bodies):
  skipped. YOU add these markers as part of this WP — that is a judgment
  pass over all ~150 blocks; every marker is a reviewable line in the diff.
- `<!-- doctut: continue -->` — the block extends the previous one within the
  section (the tutorial often builds definitions across consecutive blocks);
  the extractor concatenates the chain and checks the result once.
- `<!-- doctut: shell -->` / non-Ficus blocks (`$ ...`, C code in the
  interop chapter, the `-h` dump) — skipped automatically by content, marker
  optional.

Wire as `python3 tools/fxtest/fxtest.py doctut`; per-block pass/fail with the
tutorial line number in failures. It must run from a clean checkout (builds
into `build/fxtest/doctut/`, nothing stray).

## WP-2 — fix what fails (three different ways, don't mix them)

For every failing block decide and record which case it is:

1. **The snippet is wrong** (the rewrite introduced an error, or the text
   predates a reform in a spot the rewrite missed) → fix the snippet.
   Code-block fixes are yours to make; if the surrounding PROSE states
   something the compiler disproves, fix the claim minimally and list every
   prose edit in the report — the text is Vadim's voice, you correct facts,
   not style.
2. **The feature is genuinely broken or suspicious** (compiles but shouldn't,
   doesn't but should per the tutorial's claim, or behaves oddly) → minimal
   repro into `docs/found_bugs.md` per house rules, fence the block with a
   `<!-- doctut: fragment -->` + a `FIXME(FB-0NN)` comment, do NOT chase.
   Additionally (Vadim): for features that look shaky, write **mini unit
   tests into the regular suite** (`test/test_*.fx`) locking the actual
   current behavior — even when it is correct — so the doctut leg is not the
   only net.
3. **The block needs context** → `continue`-chain it or minimally extend it
   (a missing `val` from prose); prefer chaining over editing the example.

## WP-3 — Appendix A + known open items

- Regenerate the `ficus -h` dump in Appendix A from the REAL compiler
  (the current one is v0.1.0-era, pre-everything); then cross-check the
  appendix prose against the actual option set (`-Wall`, `-Werror`,
  `-Wimplicit-rettype`, `-diag-format`, `-pr-resolve`, `-no-c` behavior,
  build-dir/.fxstamp notes) — fix discrepancies, list them.
- Verify the `fp16` literal suffix claim in the Types chapter (`h` suffix)
  against the lexer; fix the entry if reality differs. Same for anything else
  the summary's "left for doctut" section names.
- Remove the `TODO(vadim)` marker once Appendix A is regenerated.

## Ground rules & acceptance

House standard: full ladder + the new `doctut` leg green; determinism;
bootstrap untouched (this WP should not edit the compiler — if a case-2
finding tempts you to, that is a found_bugs entry, not a fix); don't push.
Report `docs/doctut1_report.md`: block census (total / runnable / fragment /
continue-chained / shell), the failure triage table (case 1/2/3 per block
with resolutions), every prose edit verbatim (for Vadim's review), new
mini-tests added, found_bugs entries, the Appendix A diff summary. The
closing state: `fxtest.py doctut` is a permanent CI leg and the tutorial is
the first 100-page document in the project that cannot silently rot.
