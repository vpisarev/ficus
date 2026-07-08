# Handoff: WP-E test package + experiments (pre-surgery)

**You are Claude Code (Opus) working in the Ficus repo.** Branch `wpe-tests-1`
off current main. This package builds the safety net and gathers the data
**before** the overload-resolver surgery (which happens in a separate, later
session — not yours). Read `docs/overload_resolution_proposal_v2.md` first;
this handoff implements its §9 and the instrumentation half of §3/§8.

## Ground rules

- **Do NOT change resolution behavior.** `lookup_id_opt` / `find_first`
  first-match semantics, `maybe_unify`'s commit behavior in the success path,
  instantiation logic — all stay as-is. Everything you add is either tests,
  behavior-neutral instrumentation behind a flag, or pure functions nothing in
  the decision path calls yet.
- Sanity loop: `make -j$(nproc) && bin/ficus -run test/test_all.fx && python3
  tools/fxtest/fxtest.py all`, plus `determinism` and `sanitize`, before and
  after each deliverable. `determinism` green is the proof your
  instrumentation is truly neutral.
- After compiler edits: `python3 tools/update_compiler.py` — only the modules
  you touched may differ; fixpoint must hold.
- New bugs: minimal repro into `docs/found_bugs.md`, fence, don't chase.
- Do not push; final report + diffs for review.

## D1 — the generality comparator (pure, unwired)

Implement §3's API in `Ast_typecheck.fx`, next to `maybe_unify`:

```
type gen_cmp_t = MoreGeneric | LessGeneric | EqGeneric | IncompGeneric
fun compare_typ_generality(t1, skolems1, t2, skolems2): gen_cmp_t
fun compare_fun_generality(df1: deffun_t ref, df2: deffun_t ref): gen_cmp_t
```

Mechanics: skolem mode for `maybe_unify` (opaque type parameters that unify
only with themselves — a new mode/flag whose default preserves today's
behavior exactly), then two one-way `maybe_unify(update_refs=false)` runs.
`compare_fun_generality` handles `df_templ_args` skolemization, the implicit
trailing keyword `TypRecord`, and the constructor return-type rule (§3).
**Nothing in resolution calls these functions** — they are consumed only by
E1/E4 and their unit tests.

Unit-test the comparator directly (a small `test/test_gencmp.fx` or a
dedicated section in the T-res suite): non-generic vs generic; `'t` vs
`'t complex`; interface direction (concrete class less generic than its
interface; child iface less generic than parent); keyword-record subsumption;
a genuinely incomparable pair (`f('t, int)` vs `f(int, 't)`); an EqGeneric
pair. These tests are the Fable session's contract.

## D2 — resolution trace (`-pr-resolve`)

New option (follow `-pr-ast`'s pattern in `Options.fx`): for every lookup
whose viable-set size is > 1, print the call location, each candidate via a
new `fun2sigstr(df)` (add to `Ast_pp.fx`: `Module.name(argtyps) -> rt
[generic: 't,...] @ file:line`), the env-order winner (today's behavior), and
the `compare_fun_generality`-based winner. Building the viable set for the
trace must itself be side-effect-free (`update_refs=false` trials) and must
not alter what the normal path then does. Off by default; with the flag off,
generated `.c` is byte-identical (determinism leg is the check).

This trace IS the E1 instrument — build once, use for the census, keep
forever as the debugging aid.

## D3 — test suites

1. **T-res positive suite** `test/test_resolve.fx` (mind the case-insensitive
   stdlib-shadowing rule for the filename — this name is safe). Contents:
   - a self-contained mini generic class (do NOT import `Complex`; define a
     local `cplx('t)`-style record + operators) reproducing all three FB-007
     symptoms. The three failing cases go in **fenced** (commented `TEST`
     blocks with `// FIXME(FB-007)` pointing at the registry, per the
     found_bugs convention) so they flip loudly when tranche A lands; update
     the FB-007 entry to reference them.
   - passing cases that lock CURRENT correct behavior: interface-upcast
     call selection; overloads differing only in keyword args; constructor
     resolution under an expected type; explicit module-qualified calls.
   - **Q4 check**: determine what qualified *operator* call syntax the
     grammar accepts today (`Complex.__mul__(a,b)`? an operator form?);
     document the answer in the report and encode it as a test.
2. **T3+ negative goldens** (`test/negative/`): today's diagnostics for
   not-found-with-candidates (`report_not_found_typed` output), arity
   mismatch, keyword mismatch, and the FB-007 symptom errors as they look
   NOW. Purpose: when tranche A changes the diagnostic format, every change
   is a deliberate golden update in review, not an accident.
3. **T4+ IR snapshots** (`test/ir/`): 2–3 small resolution-sensitive programs
   where the chosen instance names are visible in the dumps (e.g. mixed
   generic/non-generic overloads called with several types).

Wire all new suites into `fxtest.py all`.

## D4 — experiments (report findings; change nothing based on them)

- **E0**: notes-#8 repro — module A defines generic `mem(x, l)` using `==`;
  module B defines a new type + `==` overload; B calls `A.mem` on its type.
  Does it compile today (`inst_merge_env` hypothesis)? Either way, record the
  exact behavior; if it works, keep it as a positive test.
- **E1**: run the corpus (all runnable test/example programs + the compiler
  itself via `-no-c`) with `-pr-resolve`; collect every viable>1 site into
  `docs/wpe_experiments/e1_census.md`: location, candidates, env-order winner
  vs generality winner. Expected: winners identical everywhere; list every
  exception verbatim — that list is tranche A's go/no-go input.
- **E2**: census of intra-generic-body overloaded operations whose operand
  types involve the function's type parameters (grep-level static count is
  fine; it bounds §5's deferral cost).
- **E3**: confirm from the code (and a targeted experiment) that instance
  bodies are re-typechecked from AST at each instantiation
  (`instantiate_fun_body`) with no cached-template path that bypasses it.
- **E4**: for every `fname_always_import()` name, build the corpus-wide
  merged overload set and report equally-specific overlapping pairs via
  `compare_fun_generality` (expected ~0; each hit listed verbatim). Note in
  the report whether this is worth promoting to a permanent fxtest lint (Q5).

## Final report

`docs/wpe_tests_report.md`: per-deliverable summary, E0–E4 findings (E1/E4
exception lists verbatim), the Q4 answer, anything fenced, lessons for
CLAUDE.md. Closing checklist: full ladder + `determinism` + `sanitize` +
bootstrap `--check` — all green, with `-pr-resolve` both off (byte-identical
output) and on (trace sanity on one program).
