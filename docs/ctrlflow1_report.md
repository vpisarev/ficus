# ctrlflow-1 report — control-flow keywords in expression position (FB-015)

Branch `ctrlflow-1` off master. Brief: `docs/ctrlflow1_brief.md`. Status: **done,
full ladder green, not pushed.** Two additive frontend fixes let `break`,
`continue` and `return` (with a value or bare) appear in *expression* position —
as the value of a `match` arm or `if` branch — exactly the way `throw` already
does. Zero backend change.

## Phase A — the map (throw is the template)

`break`/`continue`/`return`/`throw` all already exist as **expression** AST
nodes (`Ast.fx:285-306`) and all lower to void/err K-nodes that route through
the block cleanup chain in the C backend (`C_gen_code.fx`). The gap was never in
lowering — it was two narrow frontend spots. Contrary to the brief's "parser
limitation" framing, the two symptoms had *different* root causes:

| symptom | real stage | root cause |
|---|---|---|
| `\| _ => continue` (jump as arm/branch value) | **typecheck** | `ExpBreak/Continue/Return` synthesised pseudo-type `TypVoid` (Ast.fx `get_exp_ctx`), which will not unify with a value-producing sibling arm. `throw` differs only in that it synthesises `TypErr`. |
| bare `return` before `}` / `\|` on one line | **lexer** | `RETURN(has_arg)` classifier (Lexer.fx) treated any same-line non-`;` char as "has an argument". `return\n}` and `return;` already worked. |

`throw`'s whole story is: AST node with no stored type → `get_exp_ctx` returns
`(TypErr, l)` → `TypErr` unifies with anything **without binding** the other
side (`Ast_typecheck.fx:341-342`, and `TypErr => {}` at 320/330 means a
`TypVar` is left free) → the sibling arms drive the match/if result type →
K-normalization lowers `KExpThrow` with no value written to the match temp.
Mirroring that for the three jump nodes is the entire fix.

**Backend confirmation (read-only, per brief's STOP rule):** a dedicated
exploration of `C_gen_code.fx` confirmed `KExpBreak/Continue/Return/Throw` are
lowered uniformly, each returning "no value produced"; tail dispatch
(`KExpSeq`/`KExpIf`/match-arm) is position-agnostic and shares the dst ref, so a
jump in tail position simply emits its jump and never writes the temp;
`get_dstexp` allocates no temp for a void branch. Arm/branch-tail placement is
therefore a pure frontend concern — **no backend change required, and none was
made.**

## The two fixes

1. **`compiler/Ast.fx` (`get_exp_ctx`)** — `ExpBreak`/`ExpContinue`/`ExpReturn`
   now return `(TypErr, l)` instead of `(TypVoid, l)`, exactly like `ExpThrow`.
   The `return`-value type check is unaffected: the *value*'s type is taken
   from `e_opt` separately (`Ast_typecheck.fx:2711`), so a bare `return` in a
   non-void function still reports a return-type mismatch.
2. **`compiler/Lexer.fx`** — a `return` whose next significant char is one of
   `} ) ] , |` (tokens that cannot begin an expression) is classified bare.

## Semantic decisions (documented corners)

- **All-jumping-arms** `match`/`if` (every arm `break`/`continue`/`return`/
  `throw`): the result type var stays unconstrained, exactly as an all-`throw`
  match does today. Value-position use in an under-constrained context (e.g. a
  comprehension whose every element jumps) may need an annotation — same as
  `throw`. Tested: `ctrlflow.both_branches_jump`, `ctrlflow.all_arms_jump`.
- **`@parallel` corner (no widening).** `continue`/`break` inside a `@parallel`
  loop stay rejected — with the existing targeted message
  `cgen: 'continue' may not be used inside parallel for` — in **both** statement
  and expression position (verified: a `continue`-as-arm-value inside
  `@parallel for` gives the identical error). `return` from a `@parallel` body
  propagates as a status today and continues to; unchanged. This is a
  C-generation-stage diagnostic (not reachable under `-no-c`), so it is *not*
  captured by a `test/negative/` golden — noted here instead.
- **`fold` corner.** `break`/`continue` inside a (current, tuple-style) `fold`
  body stay rejected (`cannot use 'break' inside 'fold' loop`) in both
  positions; `return` works (it exits the enclosing function, not the fold).
  The imperative-fold reform (`language_changes_brief.md` §2) will relax the
  `continue`/`break` case; that is out of scope here and this WP unblocks it.
- **`break` targets the innermost loop** — asserted via a side-effect counter,
  not "compiles" (`ctrlflow.nested_break_innermost`).

## Verification

- **Bootstrap fixpoint:** `update_compiler.py` regenerated exactly **2** modules
  (`Ast.c`, `Lexer.c` — the two edited); **no other module's `.c` changed**
  (Array.c included — `diag` is an uninstantiated template). Existing programs'
  K-form is byte-identical. Fixpoint holds.
- **`-pr-resolve` census: unchanged** — 351 sites, `(name|winner|outcome)`
  multiset byte-identical between master and patched (location-normalized diff
  empty). The change is orthogonal to overload resolution.
- **Ladder green:** `test_all` 147 (10 new `ctrlflow.*` + `matrix.diag_from_vector`);
  `fxtest all` (unit + negative + ir + cfold + corpus O0/O3) PASSED
  (`test_resolve`, `test_gencmp`, `test_all` all exact O0=O3);
  `determinism` 3/3; `sanitize` clean.
- **Leak oracle (the acceptance).** The whole suite runs clean under
  `ASAN_OPTIONS=detect_leaks=1` with sanitizer cflags at **both O0 and O3**
  (147/147, no leak reports). A dedicated stress (`scratchpad`, 1000×1000 double
  locals live at a deep `return`/`break`/`continue`, run 200×) is likewise
  leak-clean at O0 and O3. Confirmed at the C level: the jump routes
  `FX_RETURN/FX_BREAK/FX_CONTINUE` → block label → `FX_FREE_ARR(&big)` →
  `FX_CHECK_*` before leaving; K-form shows the 1000×1000 allocation survives
  (the matrix is read into the branch condition, so DCE keeps it). "Every
  cleanup section ran" is machine-checked, not eyeballed.

## Tests added

- `test/test_ctrlflow.fx` — 10 directed tests over
  {break, continue, bare return, return value} × {match arm, if branch, both
  branches jump, nested break, comprehension/loop body, ref-counted temps live
  at the jump}, wired into `test_all.fx` and the fxtest corpus (O0/O3).
- `test/negative/216-219` — targeted diagnostics: break outside loop
  (statement **and** expression position), bare return in a non-void function,
  continue-as-arm outside loop. All replace the old `unexpected token '}'`.
- `test/test_matrix.fx :: matrix.diag_from_vector` — FB-018 ride-along lock.

## FB-015 route-arounds (brief item) — already gone

The brief asked to rewrite the FB-015 route-arounds "in `trace_resolve`" in the
natural style. That standalone instrumentation function (WP-E, commit 36002f3)
— which did contain the `| _ => {}` (would-be `continue`) and inverted-guard
accumulator loop — was **deleted by the resolve-1/2 surgery**; tracing is now
integrated into `lookup_id_opt`'s `scan_`, which is the real
collect→rank→commit decision path written as idiomatic recursion, not a
continue-avoidance workaround. There is no in-tree route-around left to rewrite,
and converting the live resolver hot path to a `continue`-based loop would risk
census neutrality for no functional gain. The fix is instead proven by the
directed suite, the leak oracle, the byte-identical census, and the
2-module-only bootstrap regen. Ride-along FB-018 (`lib/Array.fx:346`) is a
separate commit as sanctioned.

## CLAUDE.md diff

The FB-015 "gotcha" bullet (jump keywords rejected in expression position; use a
statement match / inverted guard) is replaced by a bullet stating they are now
legal like `throw` (TypErr, unify-with-any, void lowering, cleanup runs) with
the unchanged legality/`@parallel`/`fold`/bare-return-in-non-void diagnostics.
