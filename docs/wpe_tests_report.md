# WP-E test package + experiments — report

Branch `wpe-tests-1`. This is the safety-net + data-gathering pass that precedes
the overload-resolver surgery (a later session). Nothing here changes resolution
behavior: every addition is a test, behavior-neutral instrumentation behind a
flag, or a pure function nothing in the decision path calls yet. Handoff:
`docs/wpe_tests_handoff.md`; design: `docs/overload_resolution_proposal_v2.md`.

## Deliverables

### D1 — generality comparator (pure, unwired) — `compiler/Ast_typecheck.fx`

`type gen_cmp_t = MoreGeneric | LessGeneric | EqGeneric | IncompGeneric` plus
`compare_typ_generality(t1, skolems1, t2, skolems2)` and
`compare_fun_generality(df1, df2)`, sitting next to `maybe_unify`. Implements §3:
C++ partial ordering transplanted into unification (A ≻ B iff B-matches-A and not
A-matches-B), via two one-way `maybe_unify(update_refs=false)` trials.

**Q1 answer (skolem mode): realized by REPRESENTATION, not a flag on the hot
unifier.** A template parameter frozen to an opaque constant `TypApp([], id)`
unifies only with an identical constant (`maybe_unify`'s existing TypApp
id-equality) or is captured by a free `TypVar` on the other side — exactly skolem
semantics with `maybe_unify` left byte-for-byte untouched. The rigid↔free
conversion for the opposite-direction trial is `unskolemize_typ`, written on top
of Ast.fx's `walk_typ`/`ast_callb_t` traversal (per review feedback — less code,
auto-generalizes to new `typ_t` constructors; the only hand-spelled arms are the
skolem interception and the two non-destructive cases `dup_typ_` uses,
`TypVar(ref Some …)` and `TypRecord`, since `walk_typ` mutates those in place).

`compare_fun_generality` compares the parameter tuple; keyword args ride along
for free (`df_typ` already carries the implicit trailing keyword `TypRecord`);
constructors additionally let the return type participate.

**Unit tests — `test/test_gencmp.fx`** (imports the compiler as a library via
`-I compiler`, calls the comparator on hand-built `typ_t`; wired into the fxtest
corpus). Verdicts locked as the surgery's contract: non-generic ≻ generic,
`'t complex` ≻ `'t`, `int complex` ≻ `'t complex`, EqGeneric pairs, the
incomparable `f('t,int)`/`f(int,'t)`, keyword-record subsumption (currently
IncompGeneric — the §10.Q2 default, no keyword tie-break), and a **fenced**
interface-direction case (see limitations).

### D2 — resolution trace `-pr-resolve` + `fun2sigstr`

`fun2sigstr(df)` in `Ast_pp.fx` renders one candidate as
`Module.name(argtyps) -> rt [generic: 't,…] @ file:line`. `-pr-resolve`
(`Options.fx`) makes `trace_resolve` (in `Ast_typecheck.fx`) print, for every
call whose expected type is a function type and whose viable **function** set has
>1 entry: the candidates, the env-order winner (what `find_first` commits to
today) and the generality winner. The viable set is built with
`update_refs=false` trials on throwaway copies of the expected type, so it never
touches inference state. **With the flag off it returns at the guard → the
generated `.c` is byte-identical** (verified directly and by the determinism
leg). This trace is the E1 instrument and a permanent debugging aid.

### D3 — test suites

- **T-res positive — `test/test_resolve.fx`** (+ helper `test/ResHelper.fx`).
  Self-asserting; locks current behavior: keyword-arg overload distinction,
  constructor-under-expected-type, concrete-beats-generic when order permits, the
  cross-module qualified-operator call (Q4), and the E0 instance-sees-caller's-`==`
  case. Fenced `FIXME(FB-016)` case locks the greedy-first-match bug (flips on
  fix); a commented `FIXME(FB-007)` block documents the real reproductions.
- **T3+ negative goldens** — `test/negative/009_overload_arity`,
  `010_overload_keyword`, `011_overload_not_found_candidates`: lock today's
  not-found/arity/keyword diagnostic format (incl. the `Candidates:` listing) so
  the surgery's richer messages are reviewed golden changes.
- **T4+ IR snapshot** — `test/ir/overload_resolve.fx`: mixed generic/non-generic
  overloads; the selected `@instance` names are visible in the dumps. It plainly
  shows FB-016 today (all calls, even `pick(5)`, bind the generic instance; the
  concrete overloads are dead) and will diff when the surgery lands.

All wired into `fxtest all` (corpus manifest gained an `extra_flags` key for the
`-I compiler` entry; negative/ir dirs auto-discover).

### D4 — experiments (findings only; nothing changed on their basis)

- **E0 (notes-#8): OBSOLETE concern.** A generic `rmem(x,l)` using `==` in module
  `ResHelper`, called from a module that defines a local `color_t` + `==`, sees
  that `==` when instantiated (`ResHelper.rmem(Green, […])` → `true`). So
  `inst_merge_env(env, env1)` already exposes call-site candidates to instances;
  no visibility mechanism is needed for this case. Kept as a positive test.
- **E1 (env-order vs generality census): the go/no-go proof.** Full writeup in
  `docs/wpe_experiments/e1_census.md`; raw trace `e1_compiler_raw.txt`. Over the
  compiler (`compiler/fx.fx`): 344 viable>1 sites, 117 agree, 227 disagree — and
  **0 of the disagreements pick a different concrete winner**; all 227 are
  `<none>` (generality can't rank). Of those, ~224 are the FB-017 record-generic
  limitation and 2 are genuine identical-signature duplicates (E4). Conclusion:
  a specificity resolver with first-match tie-fallback is corpus-invariant on the
  compiler. The self-contained divergence (FB-016) does not occur in the
  bootstrap; it is a user-code / tranche-B concern.
- **E2 (intra-generic-body op bound):** static grep over `lib` + `compiler`:
  **492** generic functions (≥1 type-param arg) and **165** generic operator
  definitions — the upper bound on §5's deferral sites.
- **E3 (instance re-typecheck): CONFIRMED from AST.** `instantiate_fun_`
  (`Ast_typecheck.fx`) does `inst_body = if instantiate { dup_exp(df_body) }` and
  re-checks it via `instantiate_fun_body`; args re-checked via `dup_pat`. The only
  cache is `df_templ_inst` (per-instance-**type** reuse scanned in
  `lookup_id_opt`), not a typecheck bypass. So tranche B's defer-then-resolve is
  nearly free.
- **E4 (ninja-name equal-specificity collisions): ~0, 2 benign.** Mined from the
  E1 census: exactly two genuine identical-signature pairs, both String.fx
  re-declaring a Builtins helper — `length(string) -> int` (String.fx:10 vs
  Builtins.fx:133) and `join(string, string list) -> string`. A permanent
  fxtest lint (Q5) is worth having but should wait until FB-017 is fixed,
  otherwise the record-generics swamp it with false positives.

## Q4 — qualified-call escape hatch (what parses today)

- `Module.__mul__(a, b)` — the module-qualified **mangled operator name** —
  **parses and typechecks** for user operators (encoded as `op.qualified_mangled`
  in `test/test_resolve.fx`). This is the working disambiguation hatch the
  proposal (§4) assumes.
- `__mul__(a, b)` (bare mangled name) also resolves for user types.
- `Module.(*)` / `Module.(+)` (operator-in-parens) — **does not parse**
  (`unexpected token '.'`). If the prettier form is wanted, it needs grammar work.
- (`Builtins.__add__(1,2)` fails only because integer `+` is an intrinsic, not an
  `__add__` function — not a syntax limit.)

So the surgery's ambiguity hint should suggest `Module.__op__(a, b)` today, and a
nicer `Module.(op)` spelling is a separate Brief-#3 grammar item.

## Bugs found (all in `docs/found_bugs.md`)

- **FB-015** — parser: `continue`/`break` can't be a `match`-arm/`if`-branch
  value; a bare `return` (no value) doesn't parse. Both misreport as
  `unexpected token '}'`. Routed around in `trace_resolve`; also in CLAUDE.md.
- **FB-016** — overload resolution: greedy first-match ignores specificity; the
  **last-declared** viable overload wins, so `f(5)` with `f(int)` then `f('t)`
  returns 5 (generic), reversing the declarations returns 6. The minimal,
  self-contained form of FB-007's S2. Tracked by `-pr-resolve`, the E1 census,
  and the fenced `test/test_resolve.fx` case + the `test/ir/overload_resolve`
  golden.
- **FB-017** — the D1 comparator can't order `{...}` (`TypVarRecord`)
  auto-generics (`__eq__`/`string`/… on records) vs a concrete record, because
  `maybe_unify` treats record-vars symmetrically → `EqGeneric`. Dominates the E1
  `<none>` count (~224 sites) but is comparator *incompleteness*, never a
  resolution flip. Fix sketch in the entry (skolemize the `TypVar*` family on the
  rigid side); left for the surgery session (out of D1's stated scope).
- **FB-007** — re-confirmed live and ground-truthed (S2 via `fst.fx`, S3 via
  `vision_classify.fx`); the symptoms need the real `Complex.fx` + those
  programs, not a mini-class. Entry updated.

## Lessons for CLAUDE.md / methodology

- The compiler is importable as a library (`-I compiler`) — enables direct
  unit-testing of internals (the comparator) from an ordinary `.fx` under fxtest.
- Adding a field to `Options.options_t` propagates the struct (decl/copy/make)
  into every module that embeds it: a one-line source change touched **10**
  bootstrap `.c` files (3 edited + 7 struct-layout). So the handoff's "only the
  modules you touched may differ" is an idealization — cross-module record-layout
  propagation is expected and benign (fixpoint still holds, determinism green).
  Added to CLAUDE.md via FB-015's note; worth a line in the housekeeping notes.
- FB-015 (`continue`/`return` in expression position) is now in CLAUDE.md gotchas.

## Closing checklist

- `make` + `bin/ficus -run test/test_all.fx` — 135/135.
- `python3 tools/fxtest/fxtest.py all` — unit + negative(72) + ir(63) + cfold +
  corpus (incl. `test_gencmp`, `test_resolve`) all green.
- `determinism` — PASS=3 (byte-identical rebuild + neutral instrumentation).
- `sanitize` — ASan+UBSan clean.
- `python3 tools/update_compiler.py` — fixpoint holds.
- `-pr-resolve` OFF → `.c` byte-identical; ON → trace sanity on real programs.
- Not pushed; diffs left for review.
