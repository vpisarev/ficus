# Ficus overload resolution — proposal (WP-E, v2)

Supersedes the cross-language review as the working document; the review stays
as background reading. This version is organized around the Ficus design
itself, cites other languages only where a specific decision leans on their
experience, and anchors every mechanism to the current code
(`compiler/Ast_typecheck.fx`, function names rather than line numbers — the
branch will drift).

Decisions already made (Vadim, this session): greedy first-match is replaced by
collect-then-rank ("wisdom over greed"); among equally-specific candidates we
**error**, no triage — and let real code tell us where explicit disambiguation
syntax is missing; keyword arguments participate in specificity; constructors
are ordinary-but-special candidates and are all **explicit**; the test harness
is extended *first*, before the resolver is touched.

---

## 1. Problem statement (recap)

FB-007's three symptoms, restated as design gaps:

- **S1** — `int + 't` inside a generic body is resolved (to `int`) at
  declaration time; the needed information (`'t`) exists only at instantiation.
- **S2** — no order among viable candidates: `find_first` commits to the first
  entry in env-list order that unifies, so `'t complex * int` loses to the
  array `__mul__`.
- **S3** — an over-general generic candidate "succeeds" against a caller's
  still-free type variable and its bindings are committed immediately
  (`maybe_unify(..., update_refs=true)` inside the `find_first` predicate),
  polluting unrelated inference (`nnop_t list` wrapped into `... complex`).

Code-level findings that shape the fix:

- `maybe_unify` already keeps `undo_stack`/`rec_undo_stack` and rolls back on
  *failure*. The sandbox half-exists; S3 is caused by committed *successes*
  under first-match, not by leaks from failed trials.
- The trial and the commit are the same call today. The restructure is
  localized: `lookup_id_opt` (and the `df_templ_inst` cache scan inside it,
  which also calls `maybe_unify(..., true)` and must become a trial too).
- `instantiate_fun` is called with `inst_merge_env(env, env1)` — the *call
  site's* environment is merged into the template's. The old notes-#8 concern
  ("instances can't see the caller's operators") may be partially obsolete;
  experiment E0 below settles it before we design any visibility change.

## 2. The mechanism: collect → rank → commit

Restructure `lookup_id_opt` into three phases:

1. **Trial.** Walk *all* candidates from `find_all` (not stopping at the first
   success). Each candidate is tried with `update_refs=false` — including the
   generic path (`preprocess_templ_typ` + `maybe_unify`) and the
   `df_templ_inst` instance-cache scan. Nothing binds. Collect the viable set:
   `(env_entry, candidate kind, prepared type)`.
2. **Rank.** If the viable set has 0 entries → not-found diagnostics (§8). If
   1 → done (the fast path; the overwhelming majority of calls, so the perf
   cost of the change is concentrated where a choice actually exists). If >1 →
   pairwise specificity (§3); a unique maximum wins, otherwise an ambiguity
   error listing the tied candidates (§4).
3. **Commit.** Re-unify the winner with `update_refs=true` (and instantiate if
   generic) — exactly today's success path, now executed once, for the right
   candidate.

Viability is *binary* — a candidate unifies or it does not. This is a direct
consequence of a deliberate Ficus property: **no implicit conversions** except
class→implemented-interface and child-interface→parent-interface; everything
that looks like numeric coercion is either an explicit overload or the
hand-written numeric/tuple rules local to arithmetic checking. There is
therefore no conversion-sequence ranking (the single largest source of C++
resolution complexity) and no user-defined conversions in resolution (Nim's
`converter` is the cautionary example).

## 3. Specificity: one rule

Candidate A is **more specific** than B iff B accepts everything A accepts and
not conversely, tested structurally on the written signatures:

> Skolemize A's template parameters (freeze `df_templ_args` as opaque types
> instead of the fresh `TypVar`s that `preprocess_templ_typ` makes today) and
> try a one-way `maybe_unify(update_refs=false)` of B's parameter tuple against
> A's skolemized tuple; A ≻ B iff B-matches-A succeeds and A-matches-B fails.

This is C++ partial ordering ([temp.func.order]) transplanted into
unification; the machinery (`preprocess_templ_typ`, `maybe_unify`) already
exists, the only new piece is the skolem mode. Properties that fall out for
free, with no extra tiers or rules:

- **Non-generic beats generic** — a non-generic candidate is the fully-skolem
  limit case of the same test.
- **Interfaces**: since the only implicit upcasts are class→iface and
  child-iface→parent-iface, the one-way test (with upcast allowed in the
  matching direction) yields "concrete class ≻ interface" and "child iface ≻
  parent iface" automatically. Two candidates taking two *unrelated*
  interfaces that the argument class implements are genuinely ambiguous →
  error.
- **Keyword arguments** participate automatically: keywords travel as the
  implicit trailing `TypRecord` argument (see the `fun_flag_have_keywords`
  handling in `lookup_id_opt`), and record unification is by field names — so
  overloads differing only in keyword sets are distinguished at viability, and
  the same structural test orders them when one keyword signature subsumes
  another. No special-case code. Open detail (§10.Q2): candidates viable only
  via *defaulted* keywords vs candidates matching exactly — Swift adds a
  "fewer defaults preferred" tie-breaker here; the default position is to
  *not* add it (ambiguity error) until the corpus shows a real need.
- **Constructors**: all explicit, and under the `list[t]` model
  (language_changes §3.1) the type and its constructors are one entity. They
  ride the same candidate machinery with one sanctioned extra: the expected
  *return* type participates — which is already the de-facto behavior (the
  `is_constructor` branch checks the return type first, instantiating the
  variant). This is the only place return type enters resolution; overload
  sets differing *only* in return type remain rejected at declaration
  (`check_for_duplicate_fun` already unifies ignoring nothing — verify it
  covers this).

**API (Vadim's suggestion, refined): centralize the comparison so the logic
never smears across the typechecker.** Two layers in `Ast_typecheck.fx`, next
to `maybe_unify`:

```
type gen_cmp_t = MoreGeneric | LessGeneric | EqGeneric | IncompGeneric
fun compare_typ_generality(t1: typ_t, skolems1: id_t list,
                           t2: typ_t, skolems2: id_t list): gen_cmp_t
fun compare_fun_generality(df1: deffun_t ref, df2: deffun_t ref): gen_cmp_t
```

(Naming per Vadim: `spec` reads as *specification*, so the API speaks the
generality axis end-to-end — the ranking winner is the **least generic**
viable candidate; constructor names drop the usual prefix notation for
readability.) The type-level function is two one-way skolem-mode
`maybe_unify(false)` calls
(§10.Q1); the function-level wrapper handles `df_templ_args` skolemization,
the implicit keyword `TypRecord`, and constructors' return-type participation.
Four outcomes rather than a 1/0/-1 int: `EqGeneric` and `IncompGeneric`
both mean "no winner" for ranking, but they need different diagnostics —
"identical applicability, qualify the call" vs "overlapping but unordered,
define/import a disambiguating overload or qualify" (the Julia-style hint). If
an int API is preferred for composability, keep 1/0/-1 but add a separate
equal-vs-incomparable predicate for the error path.

## 4. Equal candidates: error, not triage

Among candidates none of which is strictly more specific (e.g. `f('t, int)` vs
`f(int, 't)` on a `(int, int)` call), resolution fails with an ambiguity error
listing the tied signatures and their declaration sites. C++ and Julia both
land here; Swift is the counter-example — its ever-growing tower of
tie-breakers among "equal" candidates is precisely what makes its resolution
unpredictable (and slow) in practice, and we decline to start down that
ladder.

Deliberate side effect: ambiguity errors will show us where the language lacks
syntax for *explicitly naming the desired candidate*. The expected escape
hatch is a module-qualified call (`Complex.__mul__(c, 2)` / qualified operator
form); whether that spelling is adequate and pleasant is data this change will
generate — collected during tranche A, decided in Brief #3 if syntax is
needed.

## 5. Generic bodies: defer, then default (S1) — tranche B

At declaration check of a generic function, an overloaded operation whose
operand types involve the function's type parameters is recorded as a pending
obligation (arity/shape checked only) and resolved by §2–§3 at each
instantiation — C++'s "don't ask what `a + b` means until `T` is known",
adapted to Ficus's instantiation-by-duplication (and `instantiate_fun_body`
re-checks from AST, so the plumbing is close to free — E3 measures how close).
If, after instantiation-time resolution, a *literal* operand is still
unconstrained, default it through the existing numeric coercion table
((TInt, TReal n) → TReal n, …) — the same move as Rust's integer-literal
fallback, using rules Ficus already has.

This dissolves symptom 1 entirely once §3's constructor story lands: at
`'t = float`, `complex(a+b.re, b.im)` resolves `int + float` by the arithmetic
rules and finds the auto-derived generic constructor `complex('t,'t)`.

## 6. Merged overload sets, ninja names, and cross-module conflicts

Visibility comes in three regimes, and only one of them raises design
questions:

- Plain `import m`: named functions stay behind the prefix — no merging, no
  conflict (Vadim's observation; nothing to design).
- `from m import *`: the user explicitly merged the namespaces; surprises are
  opted into.
- **Ninja names** (`fname_always_import()` in `Ast.fx`): every operator
  (plain/dot/augmented), the `to_*` casts, `string`, `print`, `repr`, `hash` —
  imported into every scope unconditionally, because `a + b` must beat
  `Complex.__add__(a, b)` and the auto-generated container printers/hashers
  must find custom element implementations. For these, the merged multi-module
  overload set is the *normal* state, not a corner case.

Policy for a merged set (one rule, deliberately scope-blind):

1. **Specificity is the only order; locality does not rank.** A local generic
   candidate losing to an imported more-specific one is usually the *correct*
   semantics — it is exactly the mechanism that makes ninja machinery work
   (the auto-generated generic `string('t list)` must lose to a user's
   `string(mytype)` for elements; a user's generic `(+)('t,'t)` must lose to
   the builtin `(+)(int,int)` on an int call and serve the remaining types).
   Two stronger arguments against a scope-proximity tie-break: (a) it would
   make `a + b` mean different things in different files with an *identical*
   visible candidate set — the reader must reconstruct import history to know
   what code does; (b) instances are checked in `inst_merge_env(env, env1)`
   (call-site merged with definition-site), so scope-ranked resolution would
   give a generic function's body ambient-dependent meaning per instantiation
   point. C++'s behavior at the opposite extreme — an inner declaration
   *hides* all outer overloads of the name (the classic `using Base::foo`
   footgun) — is the standing warning against letting scope interact with
   overload sets at all.
2. **Equal specificity across modules → ambiguity error at the call site**,
   candidates listed with their defining modules and declaration locations,
   qualified call as the escape hatch. Not an error at import/declaration
   time: two modules may each define an identical-signature ninja overload and
   remain composable as long as no call sees both viable — the error belongs
   to the call that actually needs a choice.
3. **Deliberate global override** ("my local generic should beat the imported
   specific for *everything*") is replacement, not overloading — out of scope
   for the resolver; the answer is a qualified call or not importing.
4. **Tooling over rules**: E4 (§9) takes a census of merged ninja sets in the
   corpus and flags equally-specific overlapping pairs (expected ~0); the same
   check can later become a permanent fxtest lint, catching accidental
   collisions at CI time instead of at a user's call site.

Separately, **E0** still runs first: minimal repro of notes-#8 (module A:
generic `mem` using `==`; module B: new type + `==` overload; call `A.mem`
from B) — `inst_merge_env` suggests instances may already see call-site
candidates, in which case the remaining work here is documentation, not
mechanism. The review's ADL-lite idea stays on the shelf unless E0/E1 expose
a real gap.

## 7. Assignment widening (new, Vadim + Opus find)

`=` is not a function in Ficus — no overloads can fix an
`int32 := int16`-shaped rejection, and the old note in the `ExpAssign`
handling asks for exactly this. Proposal: at **assignment only** (ExpAssign:
plain `=` to var/array element/record field — not function-call argument
passing, not `val` bindings, which infer), allow an implicit numeric cast when
**every value of the source type is exactly representable in the destination
type**:

- widening within signed (`int8→int16→int32→int64`) and within unsigned;
- unsigned→signed when strictly wider (`uint8→int16`, `uint32→int64`, …);
- integer→float/double only when exact: `int8/int16 (u8/u16) → float`,
  `int8..int32 (u8..u32) → double`; `float → double`.
- Nothing else: no narrowing, no signed→unsigned, no `int64→double`,
  no float→integer.

Platform note: Ficus `int` is pointer-sized; to keep the *acceptance set*
portable, treat `int` as 64-bit for this rule on every platform (so
`double_var = int_var` is rejected even on 32-bit targets). Compound assigns
need no extra rule: in `x += y` the arithmetic result type is computed by the
existing numeric rules and the final store goes through the same widening
check. Pleasant synergy: the imperative-fold reform turns accumulator updates
into assignments, so `fold s = 0.0 ... {s += x}` with integer `x` works out of
the box.

Status: additive (accepts previously-rejected programs), so corpus-invariant
by construction — may land in tranche A as its own commit. Goes into
`language_changes_brief.md` as a new §1.5 (snippet in Appendix).

## 8. Diagnostics & error recovery

- Not-found: print the candidate set (`possible_matches` is already collected)
  with a one-line per-candidate rejection reason — arity; head-constructor
  mismatch at argument k; "less specific than <winner>" for rank losers.
  Name-miss → edit-distance suggestions.
- Ambiguity: list the tied candidates + declaration locations + the qualified-
  call hint (Julia's "define/import a disambiguating method" pattern, adapted).
- Recovery: a failed resolution poisons the expression with `TypErr` unifying
  with anything, so checking continues — the multiple-errors-per-run and LSP
  groundwork, riding this surgery as agreed (design log: error-recovery lands
  with WP-E).

Reuse audit (`Ast.fx` / `Ast_pp.fx` — Vadim's point 3): the raw material for
all of the above already exists — `typ2str` and `pprint_typ(~brief=true)` for
signatures, `pp(id)` + `id_info → df_loc/df_scope` for "declared at" and
module attribution, `scope2str`, and `possible_matches` already collected in
`lookup_id_opt` for the not-found path. Two small additions are worth making
rather than improvising at each error site:

- `fun2sigstr(df: deffun_t ref): string` — the canonical one-line candidate
  rendering (`Module.name(argtyps) -> rt` + `[generic: 't, 'u]` + `@ file:line`),
  used by every §8 message and by the traces below; lives in `Ast_pp.fx`.
- A resolution trace under the existing `-verbose` (or a dedicated
  `-pr-resolve`): for every call with viable-set > 1, dump the candidates via
  `fun2sigstr` and the ranking outcome. This is simultaneously the
  implementation of E1's instrumentation — build it once, use it for the
  corpus experiment, keep it as the debugging aid.

No `Ast.fx` *structure* changes are needed for tranche A; tranche B's pending
obligations may eventually want a slot on `deffun_t` or a dedicated exp
attribute — deliberately not pre-added.

## 9. Tests first (agreed order of work)

Before the resolver is touched, extend the harness with a typecheck-focused
package:

- **T3+**: negative goldens for resolution — ambiguity errors, arity/keyword
  mismatches, not-found with candidate listing (locks the diagnostic format).
- **T-res positive suite**: a self-contained `test/test_resolve.fx` with a
  custom mini-`complex`-like generic class + operators reproducing all three
  FB-007 symptoms (currently `xfail`/fenced → flips to pass in tranche A),
  interface-upcast specificity cases, keyword-only-differing overloads,
  constructor cases under expected-type, qualified-call escape hatch.
- **T4+**: IR snapshots for resolution-sensitive programs (the chosen instance
  names are visible in the dumps — a resolution change shows up as a diff).
- Instrumentation & experiments (all Opus-delegable):
  - **E0** — notes-#8 repro (does `inst_merge_env` already expose call-site
    candidates to instances?).
  - **E1** — log every call with viable-set > 1; report today's (env-order)
    winner vs the §3 winner. Expected: identical everywhere; every exception
    is examined by hand (latent bug or tranche-B item). This is the
    corpus-invariance proof for tranche A.
  - **E2** — census of intra-generic-body operations involving type params
    (bounds §5's cost).
  - **E3** — confirm instance bodies are re-checked from AST at instantiation
    (reading `instantiate_fun_body` says yes; verify no cached-template reuse
    path bypasses it).
  - **E4** — merged-set census for ninja names (§6): for every
    `fname_always_import()` name, collect the corpus-wide overload set and
    report equally-specific overlapping pairs (expected ~0; each hit is either
    a latent collision or a test case). Reusable later as a permanent fxtest
    lint.

## 10. Tranches, acceptance, open questions

**Tranche A** (pre-reform; acceptance = bitwise-identical K-form on everything
that compiles today, per E1, plus the new suites green): §2 collect-rank-
commit, §3 specificity, §4 ambiguity policy, §8 diagnostics/recovery, §7
assignment widening (separate commit), unlock `lib/Complex.fx` +
`examples/fst.fx`.

**Tranche B** (reform epoch): §5 pending obligations + literal defaulting; any
E1 exception that flips a real program's resolution; visibility changes if E0
finds a gap; disambiguation syntax if §4's data demands it (→ Brief #3).

Open questions for the session:

- **Q1**: skolem-mode plumbing — a flag in `maybe_unify` ("opaque `TypApp`
  that unifies only with itself"), or a dedicated lightweight matcher? The
  flag keeps one unifier; the matcher avoids touching the hot path.
- **Q2**: defaulted keywords in specificity (§3) — start with plain ambiguity
  error, add "exact keyword match beats defaulted" only on corpus evidence?
- **Q3**: §7 widening at plain `=` only, or also at `var x: T = init`
  declarations with an explicit type annotation (arguably the same intent)?
- **Q4**: does the ambiguity error suggest qualified-call syntax that
  currently parses for operators (`Complex.(*)`? `Complex.__mul__`)? Verify
  what the grammar accepts today.
- **Q5**: should E4's ninja-collision check become a permanent fxtest lint
  immediately (CI catches accidental equal-specificity overlaps at merge
  time), or stay a one-off experiment until the corpus shows real collisions?

## Appendix — language_changes_brief.md §1.5 (paste-in)

```markdown
### 1.5 Implicit numeric widening at assignment — CANDIDATE (WP-E §7)
`=` is not a function (no overloads possible), so assignment gets the one
implicit numeric conversion in the language: at ExpAssign (var / array element
/ record field; NOT call arguments, NOT val bindings), the RHS may widen to
the LHS type when every RHS-type value is exactly representable in the LHS
type (signed↑, unsigned↑, unsigned→wider signed, small ints→float/double
exactly, float→double; nothing else; `int` treated as 64-bit on all platforms
for portability of acceptance). Compound assigns inherit the rule via the
final store. Additive/non-breaking; synergizes with imperative fold
(accumulator updates are assignments).
```
