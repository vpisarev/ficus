# Brief: resolve-3 — cross-definition inference pollution + commit divergence (Fable)

**You are Claude (Fable) in Claude Code, in the Ficus repo.** Branch
`resolve-3` off master. Read first: `docs/found_bugs.md` FB-024 + FB-025
(the bugs, with workaround locations), FB-007's near-miss section (the
"free return steals under-constrained sites" class these extend),
`docs/overload_resolution_proposal_v2.md` §2 (collect→rank→commit — FB-025
is a hole in it), `docs/newvec2_report.md` §1 (API changes that affect the
repro and one extra check below), repo CLAUDE.md.

Both bugs are ORDER/CONTEXT-DEPENDENT resolver residue — the S3 family one
level up: resolve-1 sandboxed candidate *trials* within one lookup; these
leak state ACROSS definitions (FB-024) and between the trial and commit
phases of one lookup (FB-025). Both currently have workarounds IN THE TREE,
and removing them is the acceptance test.

## Phase A — exploration & repro (no fixes; the heaviest phase)

- **FB-024 repro rebuild**: the registry repro calls `Vec.mapi`, which
  newvec-2 REMOVED. Rebuild self-contained: a local generic whose return
  binds only through a closure argument
  (`fun mymapi(v: 't vector, f: ('t, int) -> 'r): 'r vector`), two callers,
  order-swapped — reproduce "compiles one order, fails the other". Then
  instrument: WHAT binds a type var that a later, unrelated inference
  consults? Hypotheses to test, not assume: (a) the `df_templ_inst` cache
  scan unifying with `update_refs=true` outside a committed win; (b) the
  template's own stored `df_typ` getting mutated (a commit unifying against
  the original rather than the `preprocess_templ_typ` copy); (c) instance
  typ reuse across incompatible contexts. `-pr-resolve` + targeted dumps of
  the suspect vars' identity across the two orders.
- **FB-025 anatomy**: reproduce the internal error ("the winning overload of
  '__cmp__' failed to re-unify at commit") with the registry's shape — a
  generic container `<=>` whose body compares free-`'t` elements, checked in
  a scope with many `__cmp__` overloads. Dump the viable set + bindings at
  trial AND at commit; diff. Working hypothesis: self-candidacy (the
  operator being defined is a candidate for its own body's call) combined
  with trial-undo leaving the commit environment different from the trial
  environment. Establish the true divergence before designing.
- **Bonus check (2 minutes, from newvec-2)**: the resolve-1 rule says an
  exact keywordless match beats an all-defaulted-keywords candidate — but
  newvec-2 observed `vector()` ambiguous against `vector(~capacity=0)` at
  ZERO arity. Directed test: does the tie-break fire for zero-arg calls? If
  not — either fix in scope (if it falls out of the FB-025 work) or a
  precise registry entry + CLAUDE.md rule correction.

## Phase B — fixes (design follows Phase A; constraints below)

- Whatever the mechanism, the fix must preserve the resolve-1 architecture:
  trials side-effect-free, exactly one commit, `compare_fun_generality`
  untouched unless Phase A proves it implicated. Commit must re-unify in an
  environment IDENTICAL to the winning trial's — if Phase A shows they can
  diverge, that invariant (not a workaround) is what gets engineered.
- The E1-census instrument is the safety net: `-pr-resolve` over
  `compiler/fx.fx` before/after — same (name | winner | outcome) multiset;
  any change listed verbatim and justified.
- Internal errors are forbidden as user-facing outcomes: whatever remains
  unresolvable must degrade to a proper diagnostic (candidates via
  `fun2sigstr`, the annotation hint per the diag-1 style), never
  "failed to re-unify".

## Phase C — FB-022 generalized: recovery inside block expressions

New evidence (Vadim): the gap is wider than the registry's fold case —
```
val fold sz = 0, have_neg = false for s <- shape {
    if s >= 0 { sz *= s1 /* typo: s1 for s */ } else { have_neg = true }
}
println(f"have_neg: {have_neg}")   // cascades: have_neg "not found /
                                   // unknown type" — yet its type is KNOWN
                                   // (bool, from `= false`) independent of
                                   // the body error
```
Root shape: diag-1's Tier-2 recovers a failed statement inside a FUNCTION
body, but not inside a BLOCK EXPRESSION: the fold desugars to
`{ var sz = 0; var have_neg = false; for ... ; (sz, have_neg) }`, the `for`'s
failure aborts checking the whole block-RHS, the DefVal wrapper doesn't fire
on the desugared shape (the original FB-022), and the names end up UNBOUND →
"not found" cascade.

Fix (principled, not fold-specific): extend Tier-2 statement recovery to
void statements inside `ExpSeq` in VALUE position — report the failed
statement, poison it, keep typing the block's tail. Declarations that
already typed (the accumulators' `var`s) keep their REAL types; TypErr
suppression handles any follow-on noise. Then the DefVal-shape workaround
becomes unnecessary — verify and remove/simplify the special case rather
than stacking on it.

Acceptance: Vadim's repro produces EXACTLY ONE diagnostic — `'s1' is not
found` **with the did-you-mean suggestion `'s'`** (distance 1, in scope —
verify it fires here; if not, that's in scope too) — and no cascade:
`have_neg`/`sz` are typed `bool`/`int` (locked by the golden's absence of
secondary errors). The registry FB-022 entry closes with the generalized
mechanism written up; a second directed case with a failed statement inside
a plain (non-fold) block expression locks the generalization itself.

## Acceptance — the workarounds come OUT

1. `==`/`<=>`/`string`/`print` for `vector` move from `Builtins.fx` back to
   `Vector.fx` (their natural home per the operators-live-with-their-type
   guideline) and the compiler builds — the FB-025 proof. (If Vadim prefers
   them staying in Builtins for other reasons, the acceptance is a COMPILING
   demonstration branch of the move, not necessarily merging it.)
2. The FB-024 repro (rebuilt, in `test/test_resolve.fx`) passes in BOTH
   orders WITHOUT the result annotation; the fenced xfail flips.
3. `fcvector.binomial`'s workaround annotation gets a comment or removal —
   whichever the fix makes true.
4. Full ladder + determinism + sanitize + census + bootstrap fixpoint; new
   directed tests for both mechanisms + the zero-arity tie-break.

## Ground rules
House standard; STOP signals: any fix requiring `maybe_unify` semantic
changes beyond bookkeeping (report first), any census delta you cannot
attribute. Don't push. Report `docs/resolve3_report.md` — Phase-A findings
are its centerpiece (these two mechanisms will inform the generics-1
migration, which re-touches every signature in the tree).
