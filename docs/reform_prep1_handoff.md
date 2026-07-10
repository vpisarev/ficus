# Handoff: reform-prep-1 — migrator spans + corpus census + A/B renders (Opus)

**You are Claude Code (Opus) working in the Ficus repo.** Branch
`reform-prep-1` off master. Context: `docs/language_changes_brief.md` (the
reform agenda; §8 methodology — "measure every reform on the corpus"),
`docs/diag1_report.md` (loc_t carries `{line0,col0,line1,col1}`; most locs
are currently single-point), repo CLAUDE.md. This package prepares the syntax
reform epoch: it makes the automigrator *possible* (WP-1), sizes the
migration (WP-2), and produces the eyes-on material for the design sessions
(WP-3/4). Nothing here changes language behavior.

## WP-1 — parser spans for the rewrite subset (the migrator prerequisite)

The automigrator will be a **span-based textual rewriter**: parse → compute
edits as `(source span → new text)` → apply to the original text (preserving
comments/formatting; NO pretty-print round-trip). It reads the AST as the
parser built it — stages after typecheck are irrelevant, and NO location
plumbing through K-form/optimizations is in scope (explicit non-goal,
per diag-1).

Required: for the **rewrite subset** below, every AST node's `loc_t` must be
a true span (first token's `{line0,col0}` .. last token's `{line1,col1}`),
not a single point. Audit the lexer first (tokens almost certainly carry
correct spans already), then the parser's loc-folding at these node kinds:

1. `fold` expressions — the whole form, the header (`fold acc=init for ...`),
   and the body separately;
2. `'t`-style type variables and the type expressions containing them
   (`'t list`, `('t, 't)`, `'t [+]`, ...);
3. cast expressions `(x :> T)` — the whole parenthesized form;
4. `.op` elementwise operator uses (`.*`, `.+`, `.<=`, ...);
5. backtick forms;
6. type name occurrences: `half`, `vector`, `Dynvec.t` (for the
   fp16/containers renames);
7. `import` clauses (module-name spans, for the module-naming reform);
8. function definition headers (for the generic-fun syntax rewrite).

**Acceptance oracle — reparse round-trip**: a new tool
`tools/span_check.py` (or an `.fx` tool if more natural): for every node of
the subset kinds in every corpus file, cut `source[span]` and verify it
lexes/parses back to the same node kind with the same structure (a
normalized-AST comparison at the subtree level; gensym-insensitive). Run over
the full corpus (`lib/`, `compiler/`, `test/`, `examples/`); the tool reports
per-kind counts and failures. 100% pass for the subset = WP-1 done. Wire it
into fxtest as an optional leg (`fxtest.py spans`) so the reform work keeps
it green.

Behavior neutrality: loc changes must not alter diagnostics' PRIMARY
positions in existing negative goldens except where a span legitimately
extends a caret to `^~~~` — each such golden diff is reviewed and expected.
Ladder + determinism + census + bootstrap fixpoint as always (Parser/Lexer
edits regenerate their bootstrap modules; program codegen byte-identical).

## WP-2 — migration census (report only, no changes)

One script pass (`tools/reform_census.py`, AST-driven where possible, else
`-pr-ast`-driven) over the full corpus, reporting per-file and total counts:

- **fold sites**, classified by body shape: single accumulator / tuple
  accumulator (the simultaneous-assignment migration case) / nested fold /
  fold-inside-comprehension / bodies already ending in an assignment-like
  form. This table IS the automigrator spec input and the migration size.
- `(x :> T)` cast sites (and how many are chained);
- `.op` elementwise uses, by operator;
- `'t`-notation sites (type-variable occurrences in type expressions) — the
  `[t]`-reform footprint;
- backtick uses (UTest and beyond);
- `half` / `vector` / `Dynvec` occurrences;
- type/value name shadowings (the long-planned `[t]` ambiguity census:
  identifiers used both as a type name and a value name in overlapping
  scopes — expected ~0, list every hit verbatim);
- modules whose names start lowercase (naming reform footprint).

Deliverable: `docs/reform_census.md` with the tables + verbatim lists for
anything unusual.

## WP-3 — A/B renders (for the design sessions, human-eyes material)

Hand-crafted (not tool-generated) rewrites of 2–3 REAL fragments each,
side-by-side old/new in `docs/reform_ab_renders.md`:

- a representative fold-heavy function (pick from `compiler/` or `lib/`) in
  today's tuple-fold form vs the imperative form (per language_changes §2);
- one `.op`-heavy numeric fragment in {.op kept} vs {plain ops + `@` matmul};
- one generics-heavy signature set in `'t`-notation vs `[t]`-notation
  (`fun add[u, v, r](a: u, b: v): r` style included);
- one cast-heavy fragment `:>` vs `T(x)`.

Faithfulness matters more than beauty: if the new form makes something
awkward, render it awkward and flag it — that is the point of the exercise.

## WP-4 — `list[t]` parser spike (measurement, THROWAWAY branch)

On a scratch branch off `reform-prep-1` (never to merge): teach the parser
to ALSO accept `name[targs]` in type position (keeping `'t name`), enough to
parse — no typecheck integration required. Then: (a) does the grammar
conflict anywhere (report each conflict with the production); (b) run the
parser over the corpus rendered... corpus is in old notation, so instead:
parse the WP-3 `[t]` renders plus a directed file of tricky cases
(`a[i]` indexing vs `list[t]` in expression position, `result[a, b]`,
nested `list[list[t]]`, `t[...]` array types adjacency). Deliverable: a
section in `docs/reform_census.md` — "list[t] parseability: conflicts found /
resolutions available", with the spike branch name for reference. Do NOT
polish the spike.

## Ground rules

Standard ladder per WP; `update_compiler.py` fixpoint; `-pr-resolve` census
unchanged; new bugs → found_bugs, fence, don't chase; don't push. Final
report `docs/reform_prep1_report.md`: WP-1 oracle results (per-kind
counts/failures fixed), the census headline numbers, A/B render pointers,
spike verdict, CLAUDE.md lesson lines if any. This package is the data floor
for design sessions D1 (fold) → D2 (generics/casts) → D3 (operators) → D4
(records/modules); implementation waves come after those.
