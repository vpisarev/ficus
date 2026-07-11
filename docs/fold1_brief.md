# Brief: fold-1 — the imperative fold reform (`__fold__` staging)

**You are Claude Code working in the Ficus repo.** Branch `fold-1` off master.
This is the FIRST construct of the syntax-reform epoch. Design (Vadim,
2026-07-10) + spec: `docs/language_changes_brief.md` §2; context: repo
CLAUDE.md, `docs/ctrlflow1_report.md` (jump keywords are expression-legal —
a prerequisite this reform consumes).

## The design (Vadim's, verbatim in spirit)

**Staging methodology**: introduce a NEW keyword `__fold__` (verified unused
in the tree) carrying the new semantics, alongside the untouched old `fold`.
Migrate the corpus module-by-module (a big one / a few small ones per batch),
ladder green after every batch. When the corpus is 100% `__fold__`: one
textual rename `__fold__` → `fold`, drop the old grammar, drop the alias.
No transitional warnings, no big-bang day; every step reversible and tested.

**New desugaring** — `__fold__` is a THINNER sugar over `for` than old fold:

```
__fold__ s = init for <...> { s = foo(s, ...) }
  ==>  { var s = init; for <...> { s = foo(s, ...) }; s }
```

The accumulator IS a `var`, visible and assignable in the body (the old form
manufactured a fresh immutable `val s = __fold_result__` per iteration). The
whole-fold value binds as usual; `val`-bound results stay immutable:
`val s = { var s = init; for ... ; s }` (the inner var is block-local).
Multiple accumulators = multiple vars: `__fold__ (a, b) = (0, 1) ...` →
`{ var a = 0, b = 1; ...; (a, b) }`.

Consequences that fall out BY CONSTRUCTION (assert with tests, don't
re-implement): `break`/`continue` are legal in `__fold__` bodies (it's a
plain `for`; ctrlflow-1 made jumps expression-legal; `continue` = "remaining
updates of this iteration don't apply"); nested `__fold__` may assign outer
accumulators (ordinary scoping); the old fold's break/continue ban stays for
old `fold` until removal.

## Phase 0 — prerequisites & census

1. **Simultaneous tuple assignment — CONFIRMED MISSING (Vadim), build it
   first, own commit.** Spec (Vadim): parse-time desugar, lexer/parser level
   only, no new AST node:
   `(x, y, z) = (a, b, c)` ==>
   `{ val __temp_tuple__ = (a, b, c); x = __temp_tuple__.0; y = __temp_tuple__.1; z = __temp_tuple__.2 }`
   — the RHS materializes fully before the first store (that IS the
   simultaneity), stores go left-to-right.
   - Parser route: the LHS parses as a tuple expression; in assignment
     parsing, a tuple-literal LHS whose elements are all lvalues (vars,
     array elements, record fields) triggers the desugar; a non-lvalue
     element gets a TARGETED error ("'x+1' is not assignable in a tuple
     assignment"), not the generic not-an-lvalue.
   - Nested tuples recurse (`((a, b), c) = t` → `(a, b) = __t.0` etc.) —
     cheaper than a rejection diagnostic. RHS may be any tuple-typed
     expression, not only a literal (`(a, b) = f()`).
   - **`_` is legal as an LHS component** (Vadim): `(q, _) = divmod(a, b)` —
     no store is emitted for a `_` component, but the RHS temp is still
     evaluated in full (side effects preserved). Degenerate all-`_`
     (`(_, _) = f()`) is legal = evaluate for effects. A BARE `_ = expr`
     outside a tuple LHS is NOT introduced (out of scope — that's a separate
     discard-assignment feature; expression-statements cover it).
   - Directed tests: Fibonacci `(a, b) = (b, a + b)` with exact values
     asserted; the classic swap `(arr[i], arr[j]) = (arr[j], arr[i])`;
     nested; tuple-returning call RHS; `(q, _) = ...` incl. an RHS whose
     side effect proves the temp was evaluated; non-lvalue negative golden.
   - Verify the temp ERASES: inspect `-pr-k`/generated C on the scalar case
     (tuple flattening should scalarize it); an O0 residual pair-copy is
     acceptable but documented.
2. **Fold census** (report `docs/fold_census.md`): every `fold` site in
   `lib/ compiler/ test/ examples/`, classified by body tail: single acc +
   binary-op tail (the `op=` beautification case) / single acc + other expr /
   tuple-construction tail / non-tuple tuple-valued tail / tail match-if /
   body mentions a closure capturing the accumulator (semantic-change
   watchlist — old form captured a per-iteration snapshot, new form captures
   the var; design log sanctions this, but every such site is listed for
   eyes) / anything else (the manual-rewrite bucket).

## Phase 1 — `__fold__` in the compiler

Parser: `__fold__` keyword, same head grammar as `fold` (single or tuple
accumulator, `val`-form), desugaring per the spec above at parse time
(mirroring where/how old fold desugars — read that code first). The
accumulator-unused warning per spec §2: warn only if the accumulator is never
assigned in the body; suppression `s = _`... verify the agreed spelling in
§2 and implement exactly that. Old `fold` untouched.

Directed tests (`test/test_fold2.fx`): single acc; tuple acc incl. the
Fibonacci simultaneity case; `op=` forms; `break`/`continue`/bare-`return`
in the body (values asserted); nested `__fold__` assigning outer acc;
`@parallel` still rejected on general `__fold__` (same rule as old fold —
named reduction sugars are a separate future wave); a closure capturing the
accumulator (locks the NEW semantics explicitly); `val`-form immutability
(assignment after the fold errors).

## Phase 2 — the automigrator

`tools/fold_migrate.py` (or .fx if more natural): span-based textual rewriter
— parse the file (`-pr-ast` or a lexer-level scan; token spans are real
post-reform-prep-1), compute edits, apply to the original text preserving
formatting/comments. Rules (Vadim's 4.1/4.2 + the simultaneity guard):

- keyword: `fold` → `__fold__` at the site;
- single accumulator, tail expression E:
  - E is a binary op with the accumulator as the LEFT operand and the op has
    an `op=` form → `s <op>= <rhs>`;
  - otherwise → `s = E` (covers tail `match`/`if` as a whole:
    `s = match ... {...}`);
  - E is the bare accumulator → drop the tail (or `{}`) — degenerate arms
    stay as-is inside a tail match (`| B => s` inside `s = match ...` is
    already correct untouched).
- tuple accumulators, tail is a tuple construction `(e1, ..., en)`:
  - if no `e_i` mentions any OTHER accumulator (cheap syntactic check) →
    per-component assignments with the `op=` beautification;
  - otherwise → the simultaneous tuple assignment `(a, ..., z) = (e1, ..., en)`
    — NEVER sequential component assignments (the Fibonacci trap);
- tuple accumulators, tail is NOT a tuple construction (a call etc.) →
  `val __t = E; (a, ..., z) = (__t.0, ..., __t.<n-1>)` — hmm, simpler and
  equally correct: `(a, ..., z) = E` if tuple assignment accepts a non-literal
  RHS (Phase 0 verifies); else the temp form;
- anything the rules don't confidently cover → NO edit; the site goes into
  the migrator's report for manual rewrite (append to `docs/fold_census.md`).

Self-check mode: `--check` re-parses the edited file and diffs its `-pr-k`
against the original's (see acceptance below) without writing.

## Phase 3 — migrate the corpus, in batches

Batches of one big / several small modules (suggested order: `test/` first
— richest fold variety, best safety net — then `lib/`, then `compiler/`,
`examples/` last). Per batch: run the migrator, eyeball its report, hand-fix
the manual bucket, full ladder + bootstrap fixpoint (compiler batches
regenerate their modules — expected; the K-form of migrated code CHANGES by
design, see acceptance).

**Acceptance (amended from §2's old "bitwise K-form" criterion — the desugar
itself changed, so K0 differs by construction):** on the FIRST batch, diff the
generated C (post-`K_cfold_dealias` the old form's per-iteration alias should
have been erased anyway, so near-identity is EXPECTED — measure it). Report
the verdict: if C is byte-identical modulo gensym numbering, enforce that for
all remaining batches; if not, characterize the delta (report examples) and
the criterion becomes: full ladder green + O0/O3 differential + `test_all`
values + no perf regression on `compiler/fx.fx` self-build time (±2%).

## Phase 4 — the flip (LAST, only when the corpus is 100% migrated)

Textual rename `__fold__` → `fold` corpus-wide; old `fold` grammar removed
(the keyword now means ONLY the new form); `__fold__` removed. The error for
old-style bodies must be excellent — a value-typed body now fails as
"fold body must be void" with a hint: `did you mean 's = <expr>' / 's += ...'?`
(golden-locked). T4 fold snapshots and any fold-mentioning negative goldens
regenerate — reviewed diffs. CLAUDE.md: the fold gotcha line rewritten for the
new form. `docs/language_changes_brief.md` §2 → DECIDED & implemented (Vadim
edits or leave a snippet in the report).

Out of scope (explicitly): writer accumulators (`l += x` on collections —
wave 2, needs its own design), named reduction sugars / the reduction
intrinsic, `@parallel` on fold, the scan question, old-`fold`-era tutorial
text (tutorial rewrite comes after the epoch).

## Ground rules

Standard ladder per phase/batch; `update_compiler.py` fixpoint;
`-pr-resolve` census unchanged through Phases 0–2 (nothing about resolution
changes; Phase 3+ re-baseline after the compiler's own modules migrate —
same (name|winner|outcome) multiset expected, fold desugar doesn't touch
resolution); new bugs → found_bugs, fence; don't push mid-way — this is a
long-running branch, PR at the flip or at agreed checkpoints (Vadim's call
per batch). STOP signals: simultaneous tuple assignment turning out to be a
deep change (report, don't improvise); any batch where the generated-C delta
is not explainable by the desugar change. Final report
`docs/fold1_report.md` per the house format.
