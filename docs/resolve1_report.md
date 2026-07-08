# resolve-1 — overload-resolver surgery, session 1: report

Branch `resolve-1` (4 commits off master `36002f3`), per
`docs/resolve1_surgery_brief.md` (which amends proposal §4's tie policy).
Not pushed; diffs left for review.

**Headline: greedy first-match is gone.** Overload resolution is now
collect → rank → commit: every candidate is tried side-effect-free, the
viable set is ranked by `compare_fun_generality`, the unique least-generic
candidate wins and only it commits. FB-016 fixed, FB-017 fixed, FB-007
S2 + the trial-pollution half of S3 fixed; S1 and S3's commit-half wait for
session-2 deferral, so `lib/Complex.fx` stays fenced. The whole ladder is
green and the bootstrap proves corpus-invariance: the compiler compiled
through the new resolver emits byte-identical C for every module except the
edited `Ast_typecheck.c`.

## Per-phase summary

### Phase 0 (`f63dbf2`) — stdlib dedupe, corrected en route

The plan said "remove the `length`/`join` re-declarations from String.fx".
That turned out WRONG: the re-declarations are **load-bearing** — `s.length()`
method syntax resolves through the object-type fallback
(`Ast_typecheck.fx`, `some_exp.foo(args)` → `<Module>.foo(some_exp, args)`),
so the names must exist in module `String`. (`List.fx:8` `length('t list)` is
the same idiom.) Removing them broke every dot-call on strings.

What was actually needed for the strict tie rule is *zero call sites that see
an equal-signature pair*, and the E1 census showed both such sites live inside
String.fx itself (its plain calls see its own wrapper + the star-imported
Builtins original). Fix: keep the wrappers, qualify the two internal call
sites (`rfind` → `__intrin_size__`, `format` → `Builtins.join`). Bootstrap
byte-identical ⇒ perfectly behavior-neutral. A user doing
`from String import *` and calling plain `length(s)` WILL now get an
"equally applicable" ambiguity error — correct per policy (star-import is
opted-in), and the error's hint resolves it.

### Phase 1 (`4a292b7`) — FB-017: the comparator ranks var-form generics

`compare_typ_generality` now freezes the anonymous var-form generics on the
RIGID side of each one-way trial (new `freeze_varform_typs`, non-destructive,
`walk_typ`-based like `unskolemize_typ`; `maybe_unify` untouched):

| var-form | rigid sentinel | why |
|---|---|---|
| `{...}` | record with one unique `__skolem_rec__` field | no user record unifies with it; a free `{...}`/`'t` still covers it |
| `(...)` | tuple of two DISTINCT opaque constants | `('t ...)` (uniform) does not cover it; a free `(...)` does |
| `('t ...)` | 1-tuple of the frozen payload | covered by `(...)`/`('u ...)`, not by `(int ...)` or concrete tuples |
| `'t [+]` | `TypArray(-1, frozen 't)` | dim −1 matches no concrete array; `'u [+]`/`'u` still cover |

`test/test_gencmp.fx`: +14 verdicts, all pre-existing ones unchanged.
**E1 census re-run** (`docs/wpe_experiments/e1_census_post_fb017.md`, raw
alongside): 344/117/227/0 → **342/341/1/0** (sites/agree/`<none>`/concrete
flips). The one remaining `<none>` is the known under-constrained 21-viable
`__cmp__` site — exactly the fallback case. Go/no-go for phase 2 confirmed.

### Phase 2 (`e54dec3`) — the restructure

`lookup_id_opt`: per env level (direct hit or dotted-path module search,
replicated from `find_first`, which survives for its other callers):

1. **Trial** — every `IdFun` via `fun_matches_trial` (`update_refs=false`;
   the constructor return-type check and the `df_templ_inst` cache scan are
   commit-only). Non-function entries keep exact first-match semantics: they
   win immediately iff no viable function precedes them.
2. **Rank** — size 1 = fast path; else unique least-generic per
   `compare_fun_generality`. Tie policy (brief): fully-determined call (new
   `typ_has_free_vars` over the expected arg types; var-forms count as free)
   → ambiguity error; under-constrained → env-order fallback (counted under
   `-pr-resolve`).
3. **Commit** — the old success path (unify `update_refs=true`, ctor return
   check, instance cache, `instantiate_fun` with `inst_merge_env(env, env1)`
   — `env` is the ORIGINAL env even for dotted-path hits, as before),
   executed once.

`-pr-resolve` is now the real decision path (same line format + a new
`outcome:` line), not a parallel reconstruction; `trace_resolve` deleted.

**Two corpus adjustments, both forced by the decided strict-tie policy and
worth review attention:**

- `test_basic.fx` keyword_args: `sqrt(81.0)` — a local generic with ALL
  keywords defaulted ties with the concrete `Math.sqrt(double)`. This is
  §10.Q2 in the flesh; per the decided no-fewer-defaults-tie-break policy the
  call is ambiguous. Changed to `sqrt(81.0, n=2)` (explicit intent, same
  behavior). *Q2 data point #1 for the session-2 review.*
- `lib/Builtins.fx` `.*`/`./` scalar-broadcast forms: `('t...) op 'ts` was
  semantically INCOMPARABLE with `(...) op (...)` (uniform first arg spoils
  the subset relation), so `tup .* tup` — core functionality — became an
  ambiguity error; only declaration order ever made it work. Changed the
  broadcast forms to `(a: (...), b: 'ts)` (element-wise independent): the
  tuple×tuple form is now strictly more specific and ranking picks it with no
  ordering dependence. Bonus: mixed tuples broadcast (`(1, 2.) .* 3`). Note:
  the comparison operators (`.>` etc.) never had this problem — their scalar
  forms constrain the scalar to the ELEMENT type `'t`, which is also the
  cleaner design the `.*`/`./` pair could adopt in the reform epoch.

Tests: FB-016 fence flipped (`bad(5) == 6`); `test/ir/overload_resolve`
goldens regenerated (the `pick(int)`/`pick(float)` calls now bind the concrete
overloads — the generic instances disappeared, previously ALL calls bound the
generic); new `test/CplxHelper.fx` + S2 lock; **new self-contained S3 fenced
repro** (see FB-007 below — WP-E couldn't self-contain it; the trick is a
still-free fold-accumulator type at the `+`).

Perf (typecheck `compiler/fx.fx` via `-no-c`, 3 runs each): master 6.48s →
resolve-1 6.60s = **+1.8%** — the trial+commit double unification on
single-candidate sites, as the brief predicted (negligible).

### Phase 3 (`cac02ae`) — diagnostics

- Ambiguity error distinguishes **equally applicable** (all pairs EqGeneric;
  hint: qualify) from **overlapping but unordered** (hint: qualify or add a
  more specific overload). Both suggest `Module.__op__(a, b)`-style qualified
  calls (the Q4-verified spelling).
- `report_not_found_typed`: candidates render via `fun2sigstr` + a one-line
  reason from the new `fun_mismatch_reason` — keyword-acceptance / positional
  arity / first mismatching argument (trial-based, display types from the raw
  signature so `'t` names survive) / keyword-record mismatch / return-type.
- New negative goldens `012_overload_ambiguous_incomp`,
  `013_overload_ambiguous_eq`, `014_overload_ambiguous_xmodule` (local
  defaulted-keywords generic vs `Math.sqrt` — cross-module attribution; also
  locks the Q2 policy). Regenerated: 009–011 (richer format), plus
  108/207/212/703 which also print candidate listings.
- Deviation from the brief: ambiguity "via two tiny local modules" is not
  possible under the negative runner (recursive discovery = every `.fx` must
  produce a diagnostic), so the cross-module case uses the stdlib instead.
- NOT here (session 2): TypErr recovery / multiple errors, edit-distance.

### Phase 4 (this commit) — bookkeeping

`found_bugs.md`: FB-016 FIXED, FB-017 FIXED, FB-007 PARTIALLY FIXED (S2 ✓,
S3-trial-half ✓; S1 + S3-commit-half → session 2, re-confirmed live against
unfenced Complex.fx: `1 + 1.fi` still S1-fails even WITH a generic ctor —
`a + b.re` is pinned `int` at declaration; `vision_classify` still infers
`nnop_t list Complex.complex`). `language_changes_brief.md` §6 rewritten
(session-1 landed / session-2 open). CLAUDE.md: the new resolution rule +
qualified-call hatch in the gotchas.

## The numbers

| check | result |
|---|---|
| `test_all.fx` | 135/135 |
| `fxtest all` (unit+negative 72+ir 63+cfold+corpus) | green |
| `fxtest determinism` | PASS=3 |
| `fxtest sanitize` (ASan+UBSan) | clean |
| bootstrap fixpoint | holds; ONLY `Ast_typecheck.c` differs |
| `-pr-resolve` over `compiler/fx.fx` | 350 sites: 343 ranked, **7 fallbacks**, 0 errors |
| E1 census post-FB-017 | 342 sites: 341 agree, 1 `<none>`, 0 concrete flips |
| typecheck perf | +1.8% |

The 7 under-constrained fallback sites (vs the census's single `<none>`)
include lookups whose expected type is not yet a `TypFun` — the new trace
fires on the real path wherever ranking actually happens, so it sees more
than the old TypFun-gated instrument. All 7 are ties among trivially-viable
candidates at free-var sites; none changes a winner (bootstrap proof).

## Open items for session 2

1. **Deferral** (proposal §5 + under-constrained calls): S1 (`int + 't` in
   generic bodies re-resolved at instantiation, then literal defaulting) and
   S3's commit-half (`FromOnnx.fx:1012`); then un-fence `lib/Complex.fx:15-44`
   (+ add the generic `complex(r:'t, i:'t)` ctor), `examples/fst.fx:125-127`,
   and the S3 fenced block in `test/test_resolve.fx`.
2. **Error recovery**: TypErr poisoning, multiple errors per run, LSP
   groundwork; edit-distance suggestions for name misses.
3. **Q2 watch**: the `sqrt(81.0)` test collision is the first real
   defaulted-keywords-vs-exact tie; if the pattern recurs in user code,
   revisit the fewer-defaults tie-break with corpus data.
4. **`.*`-family cleanup (reform epoch)**: consider constraining the
   scalar-broadcast scalar to the element type (as the comparison operators
   already do) instead of free `'ts`.
5. **E4 ninja lint** (Q5): with FB-017 fixed the equal-specificity lint is no
   longer swamped by false positives — a cheap permanent fxtest leg is now
   feasible.
6. **Curiosity noted, not chased**: in the S1 re-test the candidate listing
   for `complex(int, float)` did not include the added `complex('t,'t)` —
   possibly an `inst_merge_env` visibility artifact inside instantiated
   bodies; worth a look during session 2's deferral work.
