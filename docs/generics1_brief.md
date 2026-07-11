# Brief: generics-1 — the bracketed generics notation (`list[t]`)

**You are Claude Code working in the Ficus repo.** Branch `generics-1` off
master. The second construct of the syntax-reform epoch. Design decisions
(Vadim + design session, 2026-07-11) below are FINAL — implement, don't
re-litigate. Context: `docs/language_changes_brief.md` §3.1,
`docs/fold1_report.md` (the staging playbook this reuses, minus the temp
keyword), repo CLAUDE.md (esp. FB-020: `typ_t` is loc-less — the migrator
implication).

## The design (final)

New notation, coexisting with the old until the flip:

- **Type application is bracketed, prefix**: `list[t]`, `rrbvec[t]`,
  `Map.t[string, int]`, `result[a, b]` (multi-param in WRITTEN order —
  `('a, 'b) map` ≡ `map[a, b]`). Old postfix `'t list` keeps parsing until
  the flip.
- **The apostrophe dies at the flip.** Type parameters are DECLARED
  explicitly and referenced bare:
  - type definitions: `type list[t] = ...`, `type map[k, v] = ...`;
  - generic functions: `fun add[u, v, r](a: u, b: v): r {...}` — the bracket
    list after the name declares the params; today's implicit
    generic-by-using-`'t` requires the explicit list post-flip (the migrator
    synthesizes it: all `'t`-names of the signature in order of appearance).
- **Postfix builtins**: `ref[t]` joins the bracketed family; **`t?` stays**
  (own meaning; `option[t]` also works as the full form); **array types stay**
  — `t [,]` / `t [+]` etc., only the quote drops (the bracket-dims postfix is
  house style).
- **NO expression-position instantiation in this reform — DEFERRED
  indefinitely, not blocking.** `f[type](x)` may occasionally be useful and
  stays on the long-term shelf (Vadim), but it is not needed for using the
  language: a type in identifier position would need semantic
  disambiguation in the parser, and the official channels cover every
  use-case today, in decreasing frequency: inference from arguments; result
  annotation `(f(x): type)` / `(Empty : Map.t[string, int])` — `(expr : T)`
  is a HINT to inference, not a cast; function-type annotation
  `(f: argtype -> rettype)(x)` for the rare argument-polymorphic case.
  Record the rationale + the deferred status in language_changes §3.1 at
  close-out (the bracketed notation itself is what makes a future
  `f[typ](x)` even possible).
  - **Diagnostic requirement**: the "cannot infer the type/instance" error
    must SUGGEST the annotation channel, with the concrete expected type
    where partially known: `annotate the result, e.g. (Empty :
    Map.t[string, int])`. Golden-locked.
- **Staging**: no temp keyword needed — in TYPE positions the two notations
  coexist without grammar conflict (the parser knows it is parsing a type).
  Per-module migration, ladder green per batch, two-stage bootstrap at the
  flip; post-flip the `'`-form and postfix application die with an excellent
  error ("'t notation was removed; declare type parameters: fun f[t](x: t)" /
  "postfix type application was removed; write list[int]").

## Phases

### Phase 0 — recon + the notation census
- Grammar recon: where types are parsed (`parse_typespec` and friends), the
  full inventory of type-position contexts (val/fun annotations, type defs,
  record fields, variant payloads, casts, interface decls, exception
  payloads, ...). Confirm zero grammar conflicts for `Name[...]` in each
  (`Name` followed by `[` in a TYPE context; watch `t [,]` adjacency — a
  declared-param `t` followed by array dims must still parse as an array of
  `t`, disambiguated by content: dims are commas/`+`/empty, type args are
  type expressions; document the rule).
- Census (report `docs/generics_census.md`): all `'t`-sites by context
  (annotation / type def / fun signature / body-local annotation), postfix
  application sites by head (`list`, `vector`, `ref`, user types,
  multi-param), implicit-generic functions (no declaration site today), and
  **type/value shadowing** (an identifier used as both type and value in
  overlapping scopes — the long-promised count; expected ~0, list hits
  verbatim).

### Phase 1 — new notation in the parser
Type application `Name[targs]` in every type context; `type name[params] =`
definitions; `fun name[params](...)` declarations (params referenced bare in
the signature AND body-local annotations); `ref[t]`; `option[t]`. Both
notations produce identical `typ_t` (assert: T4 snapshots of a dual-notation
test file are identical modulo nothing). Directed tests: every context from
the Phase-0 inventory in new notation; nested `list[list[t]]`;
`Map.t[string, int]` qualified; multi-param; `t [,]` adjacency cases; the
inference-annotation channels (`(Empty : Map.t[string,int])` etc.) exercised.

### Phase 2 — the migrator
`tools/generics_migrate.py`, span-based textual like fold's, with the known
FB-020 twist: types are loc-less, so the migrator carries a MINI
TYPE-EXPRESSION PARSER over the token slice of each annotation/definition
(the type grammar is small; slice boundaries come from surrounding tokens —
`:` ... `=`/`)`/`;`/`,`). Rules: postfix→bracketed (inside-out for nesting:
`'t list list` → `list[list[t]]`), quote-drop, synthesize `[params]` lists
for implicit generics, `'t?`→`t?`, arrays quote-drop only. `--check` =
re-parse + typecheck + (strongest) the module's `-pr-ast` type structure
identical before/after (notation erases in typ_t — bitwise AST-types is the
oracle here, UNLIKE fold where the desugar changed).

### Phase 3 — migrate (batches: test → lib → compiler → examples)
Fold-1 lessons apply: watch shadowing-class silences (a bare param name `t`
colliding with a local value/type name post-quote-drop — the census's
shadowing list is the watchlist; rename on collision); bootstrap fixpoint is
the end-to-end oracle; per-batch ladder. Coordinate with container renames:
if `vector→rrbvec` has landed by migration time, write final names; if not,
migrate to current names (a later rename re-touches — acceptable).

### Phase 4 — the flip
`'`-notation and postfix application removed; the two excellent errors above
golden-locked; T4/negative goldens regenerate (reviewed); CLAUDE.md gotchas
rewritten (casts bullet updated — `(x :> uint8)` examples get new-notation
spellings; the cast REFORM itself is a separate later construct);
language_changes §3.1 → implemented snippet in the report.

Out of scope: the cast reform (`:>` → `T(x)`) — separate construct after
this lands; container renames (pending the types-1 census decision);
`Module.(op)` grammar; anything expression-position.

## Ground rules
Fold-1 house rules verbatim: ladder per phase/batch, fixpoint,
`-pr-resolve` census unchanged (notation erases before resolution — verify),
rettype gate clean, found_bugs, don't push mid-way, checkpoints per Vadim.
STOP signals: any grammar conflict Phase 0 can't resolve by the documented
adjacency rule; any site where the `--check` AST-types oracle fails
unexplained. Report `docs/generics1_report.md`.
