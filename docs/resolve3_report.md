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

## Phase A — FB-024: not reproducible; acceptance already met  [PARKED]

Faithful reconstruction of the documented repro (map/mapi/foldl restored into the
real `lib/Vector.fx`; the exact `import Vector` + `last.mapi(...)` two-function
program) compiles **clean in BOTH orders** on current master. To rule out an
intervening resolver fix, I built the compiler at the **newvec-1 merge commit
`8d423f7`** (where FB-024 was filed) in a throwaway worktree and ran the same
repro there — **also clean in both orders**. Since `Ast_typecheck.fx` (the
resolver) is unchanged between that merge and now except newvec-2's +27 lines of
intrinsic push/pop handling and this branch's Phase C, the order-dependent
pollution is not reproducible even at its filing point — the recipe in
`found_bugs.md` was likely captured against a mid-development WIP state, not the
merge. The trigger functions (`map`/`mapi`/`foldl`) were themselves retired by
newvec-2 in favour of comprehensions, so there is no live call site.

The variant hunt surfaced only **order-INDEPENDENT** phenomena, none matching
FB-024's signature: (a) UFCS `x.f(args)` resolves only for functions in the
type's owning module (per tutorial §UFCS, `str.foo(a)` ⇒ `String.foo(str,a)`) —
a same-file / non-owning-module generic or even a *non-generic* function is
"not found" via `.`, while the prefix form resolves; likely by-design, not a bug.
(b) a chained `curr.map(fun(y){y > 0.f})` with an element-type change fails
regardless of order.

**Decision (with Vadim): PARK FB-024.** Its acceptance criterion — "compiles in
both orders without the result annotation" — is already satisfied on master, and
the pollution is not reproducible. Pivoted to FB-025.

## Phase B — FB-025: resolver self-recursion on an under-constrained tie  [FIXED]

### Reproduction

Moved the vector `==`/`<=>` from `Builtins.fx` back to their natural home
`Vector.fx` (the FB-025 acceptance test) and rebuilt. On the current tree the
symptom is no longer the documented "failed to re-unify at commit" internal
error — it is a **`StackOverflowError`** during the self-build (compiling the
compiler with `Vector` auto-imported). A *small*-context program using vector
`<=>` still compiles clean, so it is context-dependent exactly as described.

### Root cause (from `-pr-resolve`, definitive)

Every generic comparison body — the container `<=>` bodies AND the generic
`operator </>/<=/>=` in Builtins (`(a <=> b) < 0`) — issues `a <=> b` on a
**free** element type `'t`. `__cmp__((<unknown>,<unknown>) -> ...)` then has **22
viable candidates** and is a **fully under-constrained tie**. resolve-1's
documented stopgap for such ties is **env-order first-match** (true deferral is
the unimplemented "session 2"). The env scan lists the most-recently-imported
scope first, so *which* candidate wins depends on module placement:

| placement of vector `<=>` | env-order winner at `a <=> b` (free) | result |
|---|---|---|
| `Builtins.fx` (workaround) | `__cmp__(string,string)` — **concrete, `@ccode`** | commits, no body to recurse into → **terminates** |
| `Vector.fx` (natural home) | `__cmp__('t vector,'t vector)` — **generic template** | commits → `instantiate_fun` type-checks its body → `xa <=> xb` (free) → picks the vector `<=>` **again** → instantiate → … → **StackOverflow** |

The cycle is **self-candidacy**: the generic container operator is itself a
viable candidate for its own body's free-element compare, and the under-constrained
fallback commits it, whose instantiation re-enters the identical resolution. The
instance cache can't cut it — each re-entry is a *fresh* free var, never a cache
hit. Committing a *concrete* candidate (string) terminates because there is no
template body to instantiate; committing the *template* self-recurses. Whether the
fallback lands on concrete vs template is essentially arbitrary (import order) —
that arbitrariness is the latent bug the Builtins placement was tuned around.

This confirms the brief's FB-025 hypothesis (self-candidacy + the under-constrained
fallback) and localizes it to `lookup_id_opt`'s `rank_and_commit` under-constrained
branch (`Ast_typecheck.fx:1378-1383`) feeding `commit_fun`'s template
instantiation (`:1307-1325`). The pre-resolve-1 codepath surfaced the same cycle
earlier, as the "failed to re-unify at commit" throw (`:1287`); resolve-1 turned it
into unbounded instantiation.

### Why the instance cache doesn't cut the recursion (instrumented)

Instrumenting `commit_fun`'s template-instance scan (`:1308`) over the overflow
build shows the request is always `((<unknown> vector, <unknown> vector) -> int)`
(a free element `'t`), **`hit=false` on every level**, and the instance count
grows **without bound** (…42, 43, … 81, …). So the cache is structurally unable
to dedup: committing the self-candidate `maybe_unify`s the request's free element
against the vector signature (`'t := 's vector`), so each recursion level asks
for a *fresh* free-var instance that never stably matches a prior one. The
`df_templ_inst` registration (`:4173`, before the body check) is real but useless
here.

### The precise trigger chain

1. Template-check of `operator < (a: 't, b: 't) = (a <=> b) < 0` (or any container
   `<=>` body). `a`,`b` are the template's OWN free param `'t`.
2. `a <=> b` → `__cmp__('t,'t)`, `'t` free → 22 viable → fully under-constrained tie.
3. resolve-1's stopgap commits the env-order-first candidate. Auto-imported
   `Vector`'s `<=>` is env-first → a **generic template container op**.
4. `commit_fun` instantiates it for `('t,'t)->int` → `maybe_unify` binds
   `'t := 's vector` → instantiates the vector `<=>` body → `xs <=> xs` (`'s` free)
   → back to step 2 → unbounded. Terminates only in the Builtins layout because
   there the env-first candidate is a *concrete* `__cmp__(string,string)` (`@ccode`,
   no body to instantiate).

### The fix (implemented)

Explored empirically first. A `commit_fun` guard that refused ANY under-constrained
template instantiation was too broad — it broke legitimate higher-order generics
(`map`/`foldl` instantiated with a still-free return `'r` that the closure body
then pins; a normal, convergent case). The distinguishing property is not "free
vars in the request" but **divergent self-recursion**: `map` with free `'r`
converges (the body binds `'r`); the container `<=>` with a free element re-issues
the identical free compare and never binds it.

The minimal, sound fix targets the exact fork the working (Builtins) layout hit by
luck: **in the under-constrained-tie fallback (`rank_and_commit`, the
`!fully_determined` branch), prefer a CONCRETE (non-template) candidate over a
template one.**

```
val concrete_opt = find_opt(for idf <- viable { idf.1->df_templ_args == [] })
val (i, df) = match concrete_opt { | Some(idf) => idf | _ => viable.hd() }
commit_fun(i, df)
```

A fully under-determined call (all arg types free) is a **speculative** resolution
— redone per real use with concrete args — so committing any candidate that
type-checks is fine. A concrete candidate fixes the arg types in the speculative
check and cannot self-instantiate; a template re-issues the free compare and
spawns instances without bound. This makes the working-vs-overflow behaviour
independent of where a generic container operator is declared. The real per-use
resolution (concrete args) never enters this branch, so runtime semantics are
untouched.

### Validation

- The FB-025 acceptance: with `==`/`<=>` for `vector` moved to `Vector.fx`, the
  compiler self-build type-checks cleanly (`exit 0`) where it previously
  StackOverflowed.
- `-pr-resolve` census over `compiler/fx.fx`, before vs after: **identical after
  line-number normalization** — the fix is dormant in the shipping layout (the
  under-constrained ties there already had a concrete env-order winner). It fires
  only on the pathological template-first tie.
- Bootstrap fixpoint holds; **only `Ast_typecheck.c` regenerates** (no codegen
  change to any other module — confirms no committed-candidate change reaches
  generated code in the shipping config).
- Full `fxtest.py all` + `determinism` + `sanitize` green.

Considered and rejected as the sole fix: aggressive `inst_merge_env` (the
self-candidate lives in the template's base `df_env`; merging more caller entries
can't remove it) and a `commit_fun` free-var refusal (breaks convergent
higher-order generics like `map`/`foldl`).

### Workaround removed (acceptance)

`==`/`<=>`/`string`/`print` for `vector` moved from `Builtins.fx` back to their
natural home `Vector.fx`. Churn is minimal: the compiler itself never
compares/prints vectors, so the bootstrap stays at a clean fixpoint (`nothing to
do`) — only the two lib files and the fix move. The compiler self-build
auto-imports `Vector`, so it now type-checks these generic container operators in
the large-overload context on every build and **doubles as the FB-025 regression
guard** (a reappearance of the resolver self-recursion re-breaks the build). Full
ladder + determinism + sanitize green with the operators in their new home;
`vector == / <=> / string / print` verified behaviourally.
