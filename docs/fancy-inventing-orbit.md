# WP-E tranche A — resolver surgery (draft; WAITING for Vadim's brief)

## Status

- 2026-07-08: reviewed `docs/wpe_tests_report.md`, `docs/overload_resolution_proposal_v2.md`,
  `docs/wpe_experiments/e1_census.md`, `docs/wpe_tests_handoff.md`, FB-007/016/017 in
  `docs/found_bugs.md`. WP-E test package is MERGED (PR #30, master 36002f3). Memory from the
  Opus session ("better_ficus") is shared — same project dir, `project-wpe-tests.md` current.
- Vadim: strict-error-on-tie is his inclination, but **paused** — a Fable chat (aware of the
  whole activity) prepares a brief; he passes it here, then we finalize the plan.
- Assignment widening (§7) deferred to later. New branch needed (wpe-tests-1 deleted after merge).

## Exploration findings (Ast_typecheck.fx, 4032 lines)

Surgery site `lookup_id_opt` **983–1070** (88 lines): `find_first` with COMMITTING
`maybe_unify(...,true)` inside the predicate — non-generic commit at **1012**, generic at
**1016** (after `preprocess_templ_typ` 1099–1107); `df_templ_inst` cache scan **1036–1042**
also commits; new-instance path **1049–1053** (`inst_merge_env` at 1049 → `instantiate_fun`);
`possible_matches` accumulated at 989. Keyword TypRecord append 999–1010; constructor branch
1025–1034 (return type participates). Caller `lookup_id` 1072–1083 (`report_not_found_typed`).

Ready-made pieces:
- `maybe_unify` 106–330; full rollback on `!ok || !update_refs` at **322–328** — trial mode works.
- FB-017 root: TypVarRecord arms **194–221** — symmetric flip at 221; record-var binds either
  direction → comparator sees EqGeneric. Fix: skolemize TypVar* family (Record/Tuple/Array) on
  the rigid side in `skolemize_fun_sig` (423–445; skolem = frozen `TypApp([],id)` at 432),
  extend `unskolemize_typ` (362–381). maybe_unify itself stays untouched.
- Comparator `compare_typ_generality` 395–408, `compare_fun_generality` 449–454 (unwired).
- `fun_matches_trial` **916–936** — already the update_refs=false viability test the collect
  phase needs; `trace_resolve` 946–981 computes env-order vs generality winner.
- `find_all` 533–537, `find_first` 852–909 (dotted-module fallback path 883–908 — keep).

## Key data constraints (from E1 census)

- 344 viable>1 sites in compiler; 0 different-concrete-winner disagreements; 227 `<none>`:
  ~224 = FB-017 (fixable), **2 genuine stdlib duplicate signatures** (`length(string)`,
  `join(string, string list)` in Builtins.fx AND String.fx) — strict error-on-tie requires
  de-duplicating String.fx first.
- Under-constrained args (free TypVar at call): many generics trivially viable (one 21-viable
  site; container size/string families). Today: env-order first + later narrowing. Strict
  error-on-tie risk here — must re-run census with fixed comparator to size the problem.
- S3 mechanism confirmed: `lib/NN/FromOnnx.fx:1012` `rev_more_ops + prog` — list-concat
  `+('t list,'t list)` (Builtins.fx:181) vs over-general `+('t,'t complex)` are INCOMPARABLE;
  prog's type still free → ranking alone doesn't fix S3; needs tranche-B deferral, a type
  annotation in FromOnnx, or keeping Complex ops fenced.
- S1 fix = add `fun complex(r:'t, i:'t)` to lib/Complex.fx (FB-007 entry sanctions it).
- FB-007 multi-module repro idea (Vadim: at my discretion): test-local mini `cplx` module with
  over-general `operator +(a:'t, b:'t cplx)` + consumer building a list with a free-typed
  accumulator (mimic `rev_more_ops + prog` shape), `.rev()` after — S3-shape without touching
  lib. S2-shape try: `val c = ref(cplx{...}); *c *= 2` with `*('t cplx, int)` defined.

## Open questions (answers to come from the brief)

1. Tie policy tranche A: strict ambiguity error (Vadim leans; needs String.fx dedup + a story
   for under-constrained sites) vs staged (rank+env-order-fallback first, census re-run, then
   flip to strict).
2. Complex.fx unlock scope in this PR (S3 not fixed by ranking).
3. PR scope: resolver core + basic ambiguity diagnostics vs full §8 (rich not-found reasons,
   edit-distance, TypErr recovery/multiple errors) in one PR.

## Next steps when brief arrives

Incorporate brief → finalize plan (steps, tests-to-flip list, acceptance = E1 corpus
invariance + fixpoint + determinism + sanitize) → ExitPlanMode.
