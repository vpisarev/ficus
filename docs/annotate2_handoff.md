# Handoff: annotate-2 — return-type sweep + `-Wimplicit-rettype` (Opus)

**You are Claude Code (Opus) working in the Ficus repo.** Branch `annotate-2`
off master. Context: the resolve-2 near-miss (see
`docs/resolve2_report.md`, "Near-miss" section) established that return
annotations on overloaded generic functions are load-bearing **viability
filters** in the new overload resolution; annotate-1 already covered all 106
generic stdlib *operators* and gated them with `tools/lint_op_returns.py`.
This WP extends the discipline to functions and adds the opt-in strictness
flag. Design rationale: `docs/language_changes_brief.md` §1.7.

## Ground rules

- Standard ladder before/after each WP: `make -j$(nproc) && bin/ficus -run
  test/test_all.fx && python3 tools/fxtest/fxtest.py all`, plus
  `determinism` and `sanitize`; `python3 tools/update_compiler.py` fixpoint
  after compiler edits (stdlib edits regenerate bootstrap modules too —
  annotate-1's lesson).
- **The E1 census is a mandatory acceptance gate for the sweep.** Return
  annotations can change resolution at under-constrained sites (that is
  their whole point), so: capture `bin/ficus -no-c -pr-resolve
  compiler/fx.fx` before and after each sweep commit. Expected: **350 sites,
  343 ranked / 7 fallbacks / 0 errors — unchanged**, or strictly better
  (a fallback site becoming cleanly ranked). ANY other change — a different
  winner anywhere, a new ambiguity — stop, list the site verbatim, and leave
  it for review. Do not "fix" a changed site by tweaking the annotation
  until it disappears.
- Judgment cases are COLLECTED, not guessed (see WP-2 conventions).
- New bugs → `docs/found_bugs.md`, fence, don't chase. Do not push.

## WP-1 — `-Wimplicit-rettype` + the warning subsystem seed

Design (settled with Vadim): this is a **warning**, not an error — and it
deliberately bootstraps the compiler's warning subsystem (non-fatal
diagnostics), which session-2 (multiple errors per run) will build on.

Mechanics, three small layers:

1. **Warning infrastructure** (minimal, in `Ast.fx`/`Ast_typecheck.fx` next
   to the error machinery): a `warning(msg, loc)` emission that prints in
   the same format as errors but with `warning:` severity, does NOT stop
   compilation, and is counted; at the end of a run with warnings, a one-line
   summary (`N warning(s) generated`). Exit code stays 0 unless `-Werror`.
2. **Flags** in `compiler/Options.fx` (note the `options_t` struct-layout
   propagation lesson): `-Wimplicit-rettype` (this warning), `-Werror`
   (generic: promote ALL warnings to errors, nonzero exit), `-Wall`
   (umbrella: enables all recommended warnings — currently just this one;
   future warnings land together with their corpus cleanup, so `-Wall
   -Werror` on CI stays green by construction).
3. **The check** (at function-definition check, AFTER the body typechecks so
   the inferred type is available): under the flag, every **module-level**
   function lacking an explicit return annotation warns once:
   `warning: implicit return type of module-level function 'norm'
   (inferred: double)` — the inferred type makes the fix copy-paste. Exempt:
   nested functions, lambdas, `@ccode` functions with fully spelled types,
   auto-generated functions (constructors, record/variant
   `__eq__`/`string`/...), and anything not written by a user.

**Scope = root modules only** (the modules named on the command line), NOT
transitively imported ones: Ficus typechecks everything from source, so an
"all-modules" scope would bury any user of the flag in warnings about the
(not yet fully annotated) stdlib they didn't write and can't fix. For the CI
gate over stdlib itself, either loop over stdlib files as roots (`-no-c`,
cheap) or add an `=all` variant for a driver file — pick the simpler,
document the choice.

Tests: positive (annotated file is silent under `-Wall`), warning-format
golden (`test/negative/2xx_implicit_rettype*` — if the T3 harness assumes
nonzero exit, run this golden with `-Werror`, which also locks the promotion
path), flag off = zero behavior change (determinism leg).

## WP-2 — the 276-function sweep

Worklist: `python3 tools/lint_op_returns.py --funs` (per annotate-1: string
×17, print, size, norms, min/max; Builtins 101, UTest 52, Array 48, Math 23,
OpenCV 11, Map/Set/...). Work per family, one commit per family or sensible
group, ladder + census after each.

Conventions (starter set — extend in the report where a family doesn't fit):

- Mechanical, no judgment: `string(...): string`, `repr(...): string`,
  `print/println(...): void`, predicates (`is*`, `has*`, `contains`,
  `starts_with`, ...): `bool`, `hash(...): hash_t` (or the actual stdlib
  hash type), `size`/`length` families: `int` for 1D, the exact tuple form
  for ND (`(int, int)` etc. per dimensionality).
- Norms and numeric folds (`sum`, `mean`, `normL1/L2/Inf`, ...): annotate
  with the type the body actually computes today (many widen to `double` —
  keep the current behavior EXACTLY; the census guards you).
- Generic pass-throughs (`min/max('t,'t)`, clip, sort comparators):
  `: 't` — the input type var, NOT a fresh var (these return one of their
  arguments; a fresh var would be wrong).
- Fresh-var forms (`'t3 [+]`, `('t3 ...)`, `'t3 vector`) are for
  mixed-type/elementwise families that genuinely compute a combined type —
  same rule annotate-1 used for operators.
- **Type-level helpers (`elemtype`-style, shape/size-computing generics used
  in signatures): DO NOT annotate on your own judgment.** Collect them into
  a dedicated report section with your best-guess proposal each — Vadim
  rules on the convention (this is the one open taste question, flagged in
  advance).

When every family is either annotated or parked in the judgment section:
extend `tools/lint_op_returns.py` so `--funs` distinguishes "unannotated"
from "parked-by-decision" (a small allowlist file), and make the lint's
clean subset CI-enforced.

## WP-3 — CI gate

Once stdlib + `test/` are clean: add `-Wimplicit-rettype -Werror` compilation of the stdlib
and the test suite as a CI leg (cheap: it is a `-no-c` pass), alongside the
existing `lint_op_returns.py` check. The compiler sources themselves are NOT
gated yet (that is annotate-3, gradual, per Vadim) — but report how many
compiler functions currently violate, for planning.

## Final report

`docs/annotate2_report.md`: per-family counts, the judgment-section for
Vadim (type-level helpers + anything that didn't fit the conventions),
before/after census verdicts per commit, WP-1 design choices
(root-vs-all scoping), the compiler-violation count for annotate-3
planning, CLAUDE.md lessons (e.g. add "-Wall/-Werror/-Wimplicit-rettype exist; stdlib/tests are
gated" to the writing-Ficus section when WP-3 lands). Closing checklist:
full ladder + determinism + sanitize + bootstrap fixpoint + census
unchanged-or-reviewed.
