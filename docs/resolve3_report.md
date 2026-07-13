# resolve-3 report

Branch `resolve-3` off master. Follow-up to `docs/resolve3_brief.md`. Work is
staged by phase; **Phase C (FB-022) is done first** at Vadim's request, since it
is the bug he hit directly. Phases A/B (FB-024/FB-025 resolver pollution) are the
heavier, separate work and are not started in this section yet.

---

## Phase C — FB-022 generalized: recovery inside block expressions  [DONE]

### The bug

A failed statement buried inside a block expression in **value position** lost
every name the block's `val` was supposed to bind, cascading into spurious
"not found" errors. Vadim's field repro (from `lib/NN/InferShapes.fx`, the
`Reshape` shape-inference fold): change `new_total *= sz` to `new_total *= sz1`
and the compiler correctly reports `sz1` not found — but then *also* claims
`havem1` is not found at a use two lines below, even though `havem1` was
explicitly initialised `havem1=false` at the fold head and its type (`bool`) is
known independent of the body error.

Minimal repro:

```
val fold sz = 0, have_neg = false for s <- shape {
    if s >= 0 { sz *= s1 /* typo for s */ } else { have_neg = true }
}
println(f"have_neg: {have_neg}")   // BEFORE: "have_neg not found" (cascade)
(sz, have_neg)                     // BEFORE: "sz not found" (cascade)
```

### Root cause — narrower than the brief assumed

A `val fold ...` desugars (parse time) to a plain annotated tuple `val`:

```
val (sz, have_neg): (int, bool) = {
    var sz = 0
    var have_neg = false
    for s <- shape { ... }         // <-- the failing statement
    (sz, have_neg)                 // tail
}
```

The brief hypothesised the block's tail was never typed. It **is**: the block's
own `check_eseq` already runs diag-1 Tier-2 per-statement recovery — the failing
`for` is caught, reported, and typing continues to the tail `(sz, have_neg)`,
which types cleanly to `(int, bool)` off the two `var`s' real types. The true gap
is one level up:

- `check_eseq` ends with `check_compile_errs()`, which — because the inner error
  was pushed to the global accumulator — throws **`PropagateCompileError`**, the
  "this subtree is tainted; don't re-report" signal.
- The outer `DefVal` recovery arm caught only `CompileError`. `PropagateCompileError`
  fell through to the generic per-statement catch, which keeps the statement but
  with the **original env** — so the pattern was never bound and `sz`/`have_neg`
  never entered scope → cascade.

A second, latent defect in the same arm: it poisoned *every* pattern var with one
whole-pattern type via `poison_pattern_vars`. For a tuple annotation that binds
both `sz` and `have_neg` to `(int, bool)` — a type mismatch that would itself
cascade at the later uses. Never exercised before because tuple-typed `val`s
whose RHS fails are rare outside the fold desugar.

### The fix (`compiler/Ast_typecheck.fx`, `check_eseq` DefVal arm)

1. **Catch `PropagateCompileError` too.** A direct RHS failure still arrives as
   `CompileError` and is pushed as before; a block-internal failure arrives as
   `PropagateCompileError` and is *not* re-pushed (already reported by the inner
   `check_eseq`). Either way we now reach the pattern-binding recovery.
2. **Bind through the annotation with `check_pat`, not a blanket poison.** With a
   usable annotation, `check_pat(p_inner, annotated_typ, ...)` destructures a
   tuple/record annotation per-field — `val (a,b):(int,bool)` → `a:int, b:bool`
   (the §1.7 annotation firewall: callers keep checking against the declared
   interface). Without a usable annotation, fall back to `TypErr` poison for
   structural cascade suppression, exactly as before.

The change is a **generalization of the existing diag-1 recovery, not a
fold-special-case** — so there is no fold-shaped workaround to remove; the fix is
the generalization the brief asked for. Any block expression in value position
now recovers, not just the fold desugar.

### Verification

- Vadim's repro and the real `InferShapes.fx` typo → **exactly one diagnostic**
  (`'s1'`/`'sz1'` not found, *with* the did-you-mean suggestion `'s'`/`'sz'`), no
  cascade.
- Directed cases confirmed: original FB-022 fold; annotated single-ident (`val
  x:int = undef`); tuple-annotated direct failure (`val (a,b):(int,bool) = undef`
  → both fields recover); plain non-fold block (`val r:int = {..undef..; r}`),
  annotated and unannotated.
- Golden `test/negative/709_fold_old_style_body` **loses its spurious `'s' not
  found` cascade** (2 diagnostics → 1) — the FB-022 pattern in an existing test.
- New goldens: `239_recovery_fold_block` (fold, did-you-mean) and
  `240_recovery_block_expr` (plain block).
- Full `fxtest.py all`, `negative`, `determinism`, `sanitize` (ASan+UBSan) green.
- Bootstrap fixpoint holds: only `Ast_typecheck.c` regenerates (recovery path is
  diagnostics-only; clean compiles never enter the catch, so the `-pr-resolve`
  census is untouched by construction).

### Registry

`docs/found_bugs.md` FB-022 → `FIXED — resolve-3 Phase C`, with the actual
`PropagateCompileError` root cause and the per-field destructuring detail.

---

## Phases A / B — FB-024 / FB-025 resolver pollution  [NOT STARTED]

The order/context-dependent generic-return pollution (FB-024) and the
commit-time re-unify internal error (FB-025) are untouched in this section. They
need the Phase-A exploration/repro work (rebuild FB-024's repro without the
removed `Vec.mapi`; reproduce FB-025's internal error) before any fix. Deferred
pending a check-in with Vadim.
