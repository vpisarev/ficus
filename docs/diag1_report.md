# diag-1 report — type-error recovery, multiple diagnostics, caret rendering

Branch `diag-1` off master. Brief: `docs/diag1_brief.md`. Status: **done, full
ladder green, not pushed.** One compiler run now reports MANY useful diagnostics
instead of dying on the first, with cascade noise suppressed, gcc/clang-style
caret excerpts, and "did you mean" suggestions. This is the LSP-groundwork
session.

Four commits: **B** recovery core, **E-ordering** sort/dedup/limits/tests,
**C** did-you-mean, **D** caret. (Phase E's determinism/sanitize/report and the
CLAUDE.md diff are folded into the close-out.)

## Phase A — map (the recovery machinery already existed)

The compiler already accumulated errors instead of throwing on the first: every
raise site funnels through one `compile_err` → `CompileError` → `push_compile_err`
→ `check_compile_errs`/`PropagateCompileError`, and `check_eseq` already wraps
each item of a statement/definition sequence in a `try/catch`. ~257 raise sites,
108 of them `unify` (which already stays silent on `TypErr`). `loc_t` carries a
full span `{line0,col0,line1,col1}`; source text is NOT held in memory (re-read
by `dm_filename`). So the work was to ADAPT this machinery, not build it — three
concrete gaps found empirically:
1. cascade not suppressed — poisoned symbols were bound to a fresh type var
   (not `TypErr`), so downstream uses re-errored;
2. an annotation-mismatch `unify` sat OUTSIDE the recovery `try`, leaving the
   symbol unbound → spurious "undefined";
3. tuple/record patterns were not bound on failure.

## Phase B — recovery core (structural cascade suppression)

Recovery = report + poison with `TypErr` + continue, strictly above unification.

- **`typ_has_typerr`** detects a type already contaminated upstream.
- **Choke points** (only fire when a type already has `TypErr`, so clean
  compiles are untouched and the `-pr-resolve` census is byte-identical):
  `lookup_id` suppresses a resolution whose expected type is poisoned and pins
  the result to `TypErr` (`poison_result_typ`); `scan_`'s `IdDVal` case and
  `commit_fun` propagate a value's/function's stored `TypErr` to the use site
  (`unify` leaves the use-site var free because `TypErr` never binds).
- **DefVal**: annotation `unify` moved inside the `try`; every name the pattern
  binds is poisoned via `poison_pattern_vars` (built on `walk_pat`, so
  tuple/record/as/... are covered) — with the declared type when annotated
  (the §1.7 firewall), `TypErr` otherwise.
- **DefFun**: a body failure keeps the declared signature (pre-registration is
  the firewall — a correct caller stays clean, only a genuinely mistyped caller
  errors); the return type is recovered from the branches that DID typecheck
  (an early `return 42` pins it), poisoned to `TypErr` only if nothing did.
- **check_cases / ExpIf**: a failed match arm / if branch is reported and its
  siblings are still checked, so errors in several arms/branches surface in one
  run; a poisoned arm/branch does not constrain the result type — the healthy
  siblings determine it (the jumping-throw/ctrlflow-1 logic).
- **Templates**: body errors surface at instantiation and poison correctly;
  duplicates across N distinct instantiations are collapsed by the Phase E dedup.

Tier-2 decision: per-statement recovery was already present (a function body is
a `check_eseq`), and per-arm/per-branch recovery is cheap and high-value, so
both landed. Per-subexpression recovery inside `check_exp` was NOT needed —
structural suppression already collapses intra-expression cascades.

## Phase E (ordering) — readable, deterministic multi-error output

`print_all_compile_errs` sorts diagnostics by `(module, line, col)` and drops
exact-duplicate messages keyed on the PRIMARY error line (a generic body error
reached from N call sites is one bug). `-fmax-errors=N` (default 100) caps the
output with a "further diagnostics suppressed" summary. Structural suppression
keeps real runs well under the cap.

## Phase C — "did you mean"

A not-found identifier/constructor/field close (Levenshtein, threshold scaled by
length) to a visible name gets a suggestion: `'lenght' ... did you mean
'length'?`. Only when there are no candidates (a genuinely unknown name); when
overloads exist but do not match, the resolve-1 candidate listing is shown as
before. Error-path only, so census-additive (the two new `min` sites in
`edit_distance`).

## Phase D — caret with stage-honest precision

A frontend diagnostic shows the source line with a caret under the column:

```
file.fx:2:9: error: the appropriate match for 'lenght' ... is not found; did you mean 'length'?
 2 | val x = lenght + 1
   |         ^
```

- `loc_excerpt` reads the line lazily (cached per module, reset in `init_all`),
  expands tabs 1:1 for caret alignment, draws `^` (or `^~~~` for a real span).
- Built at RAISE time inside `compile_err`/`compile_warning`, because precision
  depends on the current stage which advances as the driver runs (checking it
  at print time would tag every error as a backend error).
- **`Ast.compiler_stage`** (`Init|Frontend|Middle|Backend`) — an explicit stage,
  deliberately separate from the implicit `freeze_ids` (which flips at the start
  of K-form). The FRONTEND spans lexer + parser + typecheck **AND
  K-normalization** — all consume the AST with exact locations, so e.g.
  non-exhaustive-match errors (raised in K-normalization) get a caret. The
  MIDDLE end (K optimizations: inlining/fusion) and BACKEND (C generation) drop
  the caret: locations drift there and not all context reaches C-gen intact, so
  a confident caret would point at an innocent line. `process_all` sets
  Frontend before `parse_all`, Middle after `k_normalize_all`, Backend before
  `k2c_all`. One renderer serves errors AND warnings.
- fxtest T3 harness made caret-aware: excerpt lines (`  N | src` / `    | ^`)
  are compared verbatim (exact spacing is the contract); all other lines keep
  the whitespace-collapse. 60 negative goldens migrated.

## Before / after (one screenshot-style block)

```
// val a = undef_a + 1; val b = undef_b + 2; a + b   (a broken function body)
BEFORE: 3 errors -- undef_a, undef_b, AND a spurious "__add__((<unknown>,
        <unknown>)->int) is not found" with a 10-line candidate dump.
AFTER:  2 errors -- undef_a and undef_b, each with a caret; the a+b cascade is
        structurally suppressed.
```

## Verification

- test_all 147; `fxtest all` (unit + negative 84 + ir + cfold + corpus O0/O3);
  determinism 3/3; sanitize clean.
- `-pr-resolve` census: 356 sites = 349 ranked + **7 under-constrained
  fallbacks** — the 7 unchanged, no existing resolution altered; the extra
  ranked sites are the new diagnostic code (all recovery choke points only fire
  on already-poisoned types).
- Bootstrap fixpoint holds. Note: editing `Ast.fx` to newly use `length`/`nth`
  on `string list` reorders those shared generic instances in modules that also
  use them (C_gen_code.c, Ast_typecheck.c) — pure emission-order churn, same
  functions, fixpoint stable (the FB-008 churn class).

## Phase C (part 2) — lifted legality check

`break`/`continue` inside a `@parallel` loop was a C-gen-stage diagnostic (no
caret, invisible under `-no-c`). Whether a loop is `@parallel` is a pure nesting
fact, so the check moved to `check_inside_for` in typecheck: `ScLoop` now carries
a `parallel` flag (`(nested, parallel, block_idx)`), set from
`flags.for_flag_parallel` when the loop scope is created; the innermost-loop walk
rejects `break`/`continue` there with an exact caret. A `break` targeting a
nested non-parallel loop inside a `@parallel` loop stays legal (the inner loop is
found first). Locked by `test/negative/236_parallel_continue`. The old C-gen
check is left as an unreachable backstop. Cost: the `scope_t` layout change
reorders/regenerates many bootstrap modules (compile-time-only type, like
`options_t`; generated C for programs unchanged, fixpoint holds).

## Open follow-ups

- **`break`/`continue` in `@parallel` (future feature, Vadim)**: for now they
  are rejected (the lifted check above). A later work package may actually
  SUPPORT them in parallel loops; when that lands, relax the `check_inside_for`
  rejection (and the C-gen backstop) instead of treating it as illegal.
- **Column spans**: most locs are single-point (`^`); wiring `col1` at more
  parser sites would give `^~~~` spans. Estimated small, deferred.
- **LSP**: the recovery + `(loc, precision)` diagnostic model is what an LSP
  server consumes directly — one run yields a full diagnostic set with
  positions, and the stage tag tells the client how much to trust each caret.
