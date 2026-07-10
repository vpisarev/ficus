# reform-prep-1 report — parser spans + import/function diagnostics

Branch `reform-prep-1` off master. Handoff: `docs/reform_prep1_handoff.md`.
Status: **WP-1 (spans) done for the actionable subset + the diagnostic wins
Vadim prioritized; not pushed.** WP-2/3/4 (census, A/B renders, list[t] spike)
are measurement deliverables, not started — see "Open follow-ups". Nothing here
changes language behavior; all changes are locations + diagnostics.

Five compact commits:

- **WP-1a** — token spans (the foundation). `Lexer.make_lexer` now returns
  `((token, lloc) list, batch_begin, batch_end)`: the lexer works internally
  with point locs, but `nexttokens`' dispatch body is bound to `val tokens =
  ...` and after it runs `pos` sits at the batch end, so `getloc(pos)` is the
  end. The parser applies the batch span to every token. Result: `loc_t` is a
  real span, a caret shows the token width (`^~~~`), and `loclist2loc` (already
  used at ~18 parser sites) folds true node spans. Internal recursive
  `nexttokens()` calls (string interpolation etc.) take `.0`. No per-arm changes.
- **WP-1b** — `ExpCast`/`ExpTyped` fold to the whole `x :> T` / `x : T` (helper
  `consumed_loc`, since types carry no loc). (Cast is being removed by the
  reform — kept for completeness, harmless.)
- **WP-1c** — import errors underline the whole (dotted) module name instead of
  the first token / the `from` keyword; and the ParseError printer gained the
  same caret excerpt as CompileError, so ALL parse errors now carry carets
  (EOF errors degrade to no caret).
- **WP-1d** — a not-found import gets a Levenshtein `did you mean 'String'?`,
  candidates globbed from the include path (`Filename.glob`). `edit_distance`
  moved to `Ast.fx` so typecheck and parser share one copy.
- **WP-1e** — a function's own diagnostics (implicit-return warning,
  redeclaration, overload candidate listing) point at the NAME, not the `fun`
  keyword; operators span `operator X`.

## Span audit — what the reform constructs currently get

Full log with live compiler output: `docs/problematic_spans_log.txt`. Summary,
ranked by difficulty for a span-based migrator:

- **Token spans: solid.** Identifiers, operators, keywords all get their full
  width; sub-expression errors inside fold / `.op` / backtick point precisely.
- **Function name / import name: fixed** (WP-1c/e) — the two the diagnostics
  care about most.
- **`fun` header extent, fold whole-form, backtick delimiters:** the node loc is
  the opening token; folding these to the full construct is straightforward
  (like WP-1b) if a migrator needs them, but each construct is being reformed so
  the priority is low.
- **`.op` operator token:** the `ExpBinary` stores the operator KIND, not a loc
  for the `.*` token; a `.op → @`/plain-op migrator needs that token position.
  Also FB-021: `.op` type errors surface inside the library.
- **Types (`'t`, type names): the hard gap (FB-020).** `typ_t` carries NO source
  location, so type-name diagnostics land on the preceding `:` and a span-based
  migrator cannot locate `'t` / `half` / `vector` occurrences via the AST. The
  `'t → [t]` reform will need token-level scanning for types or locs added to
  `typ_t`.

## Bugs filed

FB-020 (types loc-less — reform blocker), FB-021 (`.op` diagnostic surfaces in
the library), FB-022 (a failed `fold`-valued `val` cascades because the
desugared shape sidesteps diag-1's DefVal recovery). All fenced, not chased.

## Verification

test_all 147; `fxtest all` (unit + negative 87 + ir + cfold + corpus O0/O3);
`-pr-resolve` census 356 unchanged; rettype gate clean; bootstrap fixpoint holds
(each WP regenerates only the modules it edits — Lexer/Parser/Compiler/Ast).
~90 negative goldens migrated to carets across diag-1 + this package; each diff
reviewed as a format change (wider carets, name-column citations).

## Open follow-ups

- **WP-2** migration census (`docs/reform_census.md`): counts of fold / cast /
  `.op` / `'t` / backtick / `half`/`vector`/`Dynvec` sites, type/value name
  shadowings, lowercase modules. Report only.
- **WP-3** A/B renders (`docs/reform_ab_renders.md`): hand-crafted old/new of a
  fold-heavy fn, a `.op` fragment, a generics signature set, a cast fragment.
- **WP-4** `list[t]` parser spike (throwaway branch): grammar-conflict
  measurement.
- **span_check oracle** (`tools/span_check.py`): the WP-1 acceptance criterion
  (cut `source[node.span]`, reparse, compare) — needs a fragment-reparse entry
  point; not built. The node-folding gaps above (types especially) are what it
  would flag.
- FB-020/021/022 fixes.
