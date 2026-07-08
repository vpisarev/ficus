# Brief: overload-resolver surgery, session 1 (WP-E core)

**You are Claude (Fable) running in Claude Code, in the Ficus repo.** This brief
is self-contained: you were not present for the design discussions, so read the
context documents before touching anything. The test net and all measurements
were prepared in a previous session (branch `wpe-tests-1`, merged) specifically
so that this surgery can be verified step by step.

Read first, in this order:
1. `docs/overload_resolution_proposal_v2.md` — the design. This brief AMENDS
   its §4 (see "Tie policy" below) and re-scopes its tranche A.
2. `docs/wpe_tests_report.md` — what already exists: the generality comparator,
   the `-pr-resolve` trace, the test suites, experiment results E0–E4.
3. `docs/wpe_experiments/e1_census.md` — the corpus measurement that proves
   this change can be behavior-neutral, and the anatomy of the ties.
4. `docs/found_bugs.md` — entries FB-007 (three symptoms), FB-016 (greedy
   first-match), FB-017 (comparator vs record-generics).
5. Repo `CLAUDE.md` — build/test mechanics and Ficus-writing gotchas. The
   fxtest ladder, `update_compiler.py`, and `.fxstamp` cache invalidation are
   already in place; trust them.

## What this session does and does not do

DOES: makes `lookup_id_opt` (in `compiler/Ast_typecheck.fx`) collect all viable
candidates, rank them by the already-implemented-and-tested
`compare_fun_generality`, and commit the unique least-generic winner —
replacing today's greedy first-match (`find_first` commits the first candidate
that `maybe_unify(update_refs=true)`s). Fixes FB-016 and FB-007's symptom S2
(wrong candidate), and the *trial-pollution half* of S3 (no binding escapes a
non-winning trial).

**S3 is only half-fixed here — known and accepted.** Its second half is a
wrong COMMIT at an under-constrained site: at `lib/NN/FromOnnx.fx:1012`
(`rev_more_ops + prog`, `prog`'s type still free) the list-concat
`+('t list,'t list)` and an unfenced over-general `+('t,'t complex)` are
genuinely incomparable, so ranking cannot prefer the right one and the
fallback reproduces env-order. Consequently `lib/Complex.fx` operators STAY
FENCED in this session; the real fix is session-2 deferral (resolve
under-constrained calls once their variables are bound). Encode the S3-shape
as a fenced test via the sanctioned mini-`cplx` multi-module repro (your
discretion on the exact form, per the FB-007 entry): over-general
`operator +(a:'t, b:'t cplx)` + a consumer with a free-typed list accumulator
mimicking the FromOnnx shape, `.rev()` after; plus the S2-shape
(`*('t cplx, int)` with `*c *= 2`).

DOES NOT: implement deferral of overloaded ops inside generic bodies OR at
under-constrained call sites (proposal §5 + the FromOnnx case above) — that is
session 2; consequently FB-007's S1 (`int + 't` typed too early) and S3's
commit-half stay open, and `lib/Complex.fx` / `examples/fst.fx` stay fenced
(the FB-007 entry sanctions adding the generic constructor
`fun complex(r:'t, i:'t)` to Complex.fx whenever the operators unfence). Does
not touch the parser (FB-015 stays fenced), the constructor `is_constructor`
branch's *selection logic* (only its trials get sandboxed), keyword-default
tie-breaks (Q2: not added), edit-distance suggestions, `TypErr`
recovery/multiple errors (session 2), or `ExpAssign` widening (§7 — separate
work item, not yours).

## Ground rules

- Branch `resolve-1` off main. Do not push; final report + diffs for review.
- Sanity loop before/after every phase: `make -j$(nproc) && bin/ficus -run
  test/test_all.fx && python3 tools/fxtest/fxtest.py all`, plus
  `fxtest.py determinism` and `fxtest.py sanitize`.
- After compiler edits: `python3 tools/update_compiler.py`; the fixpoint must
  hold. **The bootstrap diff doubles as the corpus-invariance proof**: apart
  from the modules you edited (`Ast_typecheck.c`, possibly `Ast_pp.c` /
  `Options.c` and struct-layout propagation per the wpe_tests_report lesson),
  every other regenerated `bootstrap/*.c` must be byte-identical — the
  compiler compiling itself through the new resolver must emit the same code.
  Any unexpected module diff = a resolution flip; stop and investigate.
- New bugs: minimal repro into `docs/found_bugs.md`, fence, don't chase.
- Keep `-pr-resolve` truthful: after the restructure it must report the REAL
  decision path (the actual viable set, ranking, and outcome), not a parallel
  reconstruction. It is the primary debugging aid for this work.

## Tie policy (AMENDS proposal §4 — read carefully)

The E1 census killed the naive "tie → error" rule: at calls whose argument
types still contain free type variables, many mutually-incomparable concrete
overloads are trivially viable (the census's 21-viable `string` site), and
today's semantics — the first declared viable candidate wins and PINS the
free variable — is what the whole corpus, including the compiler, is built on.
The amended policy:

- **Fully-determined call** (no free type vars in the argument positions of
  the expected `TypFun` at lookup time): unique least-generic candidate wins;
  `EqGeneric`/`IncompGeneric` at the top → **ambiguity error** listing the
  tied candidates via `fun2sigstr` + their declaring modules and locations,
  with the hint to qualify the call (`Module.__mul__(a, b)` — the form the
  grammar accepts today, per wpe_tests_report Q4).
- **Under-constrained call** (free vars present): rank; if a unique
  least-generic exists, commit it; otherwise **fall back to env-order
  first-match** — today's behavior, now explicit and documented. Proper
  deferral is session 2; `FromOnnx.fx:1012` is the canonical example of why
  the fallback (not an error, not a guess by ranking) is the only
  corpus-safe choice at these sites today. Count fallback occurrences under
  `-pr-resolve`.

## Phases (each = one commit, ladder green after each)

### Phase 0 — stdlib dedupe (prep)

`String.fx` re-declares two `Builtins.fx` helpers with byte-identical
signatures: `length(string) -> int` and `join(string, string list) -> string`
(E4). Under the strict rule these become ambiguity errors on every call.
Remove the redundant declarations (keep the better-placed one; check which is
actually referenced/inlined today) so the corpus contains zero
equal-signature pairs. Re-run the ladder.

### Phase 1 — FB-017: complete the comparator

`compare_typ_generality` reads auto-generated record-generics
(`TypVarRecord`, i.e. `__eq__(a:{...}, b:{...})`-style) as `EqGeneric`
against a concrete record, because `maybe_unify` binds a record-var
symmetrically in both trial directions. Fix per the FB-017 entry's sketch:
on the rigid (skolemized) side, the `TypVar*` family (`TypVarRecord`,
`TypVarTuple`, `TypVarArray`, …) must be frozen too, so the concrete overload
ranks strictly less generic. Extend `test/test_gencmp.fx`: concrete-record
overload ≻ record-generic; same for the tuple/array var-forms; existing
verdicts unchanged. Acceptance: re-run the E1 census
(`bin/ficus -no-c -pr-resolve compiler/fx.fx`, compare against
`docs/wpe_experiments/e1_census.md`): the ~224 FB-017 `<none>` sites must now
AGREE with env-order; remaining `<none>` only at under-constrained sites; still
**zero** different concrete winners. Save the new census alongside the old.

### Phase 2 — the restructure: collect → rank → commit

In `lookup_id_opt`:

1. **Trial**: walk ALL candidates from `find_all` (no early exit), each tried
   with `update_refs=false` — including the generic path
   (`preprocess_templ_typ` + `maybe_unify`) and the `df_templ_inst`
   instance-cache scan (it currently unifies with `update_refs=true` during
   the scan — in the trial phase it must not bind).
2. **Rank**: viable set size 1 → fast path, done (the overwhelming majority;
   perf impact stays negligible). Size >1 → pairwise
   `compare_fun_generality`; apply the tie policy above.
3. **Commit**: re-unify the winner with `update_refs=true`, instantiate if
   generic — today's success path, executed once, for the right candidate.

Non-function/value/constructor lookups keep their current logic; only their
trials must not leak bindings. The FB-016 behavior flip (concrete beats
generic regardless of declaration order at determined sites) is a DECIDED
change (Vadim, this session): expected visible effects are exactly (a) the
fenced FB-016 case in `test/test_resolve.fx` un-fences and passes, (b) the
`test/ir/overload_resolve` golden diffs to select the concrete instances
(regenerate, include the diff in the report), (c) nothing else — per the
bootstrap-diff rule above.

### Phase 3 — diagnostics

- Ambiguity error (new): per the tie policy — candidates via `fun2sigstr`,
  declaring module + location each, qualified-call hint. New negative goldens
  (`test/negative/012_overload_ambiguous*`): a two-way concrete ambiguity and
  an equal-generics one, exercised via two tiny local modules.
- Not-found (existing `report_not_found_typed`): upgrade the `Candidates:`
  listing to `fun2sigstr` rendering with a one-line per-candidate reason
  (arity / argument-k mismatch / "less specific than <winner>" for rank
  losers). Goldens 009–011 will diff — update them; every golden change must
  be reviewable as a deliberate format improvement.
- Do NOT implement `TypErr` poisoning / multiple-errors-per-run here (that is
  session 2's error-recovery work); keep today's first-error-stops behavior.

### Phase 4 — bookkeeping

- `docs/found_bugs.md`: FB-016 → fixed (write-up: what flipped, the E1 proof);
  FB-017 → fixed; FB-007 → partially fixed: S2 fixed, S3's trial-pollution
  half fixed, S3's commit-half + S1 explicitly pending session 2 (deferral),
  with the FromOnnx.fx:1012 analysis recorded in the entry. Re-test
  `lib/Complex.fx` + `examples/fst.fx` and record exactly which symptoms
  still reproduce.
- `docs/language_changes_brief.md`: add a short §6 note that resolution is now
  "least-generic wins; ambiguity errors at determined sites; env-order
  fallback at under-constrained sites (deferral pending)". Vadim will edit.
- CLAUDE.md: replace the FB-016-flavored folklore (if any) with two lines on
  the new rule + the `Module.__op__` disambiguation hatch; add lessons.
- Final report `docs/resolve1_report.md`: per-phase summary, both census
  snapshots (before/after FB-017), the golden diffs, fallback-site count,
  Complex/fst status, open items for session 2 (deferral + literal
  defaulting, error recovery, under-constrained deferral, Complex unlock).

## Closing checklist

`test_all` + `fxtest all` + `determinism` + `sanitize` + bootstrap fixpoint
(`update_compiler.py`) all green; bootstrap diff limited to edited modules +
declared golden/test changes; `-pr-resolve` off → byte-identical output;
E1-post census attached. If at any point a phase cannot meet its acceptance
without widening scope — stop, write down why, and leave the decision to
review rather than improvising a design change.
