# Ficus language changes — accumulating brief

Living document: every planned syntax/semantics change lands here first, with a
status, and graduates into Brief #3 (the reform implementation brief) and the
tutorial rewrite. One breaking epoch, one automigrator; the T4 acceptance
criterion for behavior-preserving changes is bitwise-identical K-form.

Status legend: **DECIDED** (design fixed, spec below) · **CANDIDATE** (agreed
direction, details open) · **OPEN** (needs design session) · **DEFERRED**
(post-reform, additive) · **RESOLVED** (done / question dissolved).

---

## 1. Semantics

### 1.1 Signed integer overflow wraps — DECIDED (harden-1, FB-011)
Signed overflow is defined as 2's-complement wrap. Enforced by `-fwrapv` in
default cflags (`compiler/Compiler.fx`) and in `GNUmakefile` for the bootstrap;
the constant folder already wraps (int64, sign-extend). MSVC wraps natively.
Consequences: generated C must never rely on signed-overflow UB; saturating
arithmetic (see 1.4) becomes an explicit opt-in family on top of a defined
wrapping default. → tutorial: integer-semantics section (a first paragraph
already landed in `doc/ficustut.md` during harden-1; expand at rewrite time).

### 1.2 Shift semantics — DECIDED (harden-1, FB-002)
`>>` is arithmetic for signed, logical for unsigned; folder carries
`ConstInt: (int64, bool)` with the flag set only for full-width uint64. Shift
count must be `int`. → tutorial.

### 1.3 int64 literal range — RESOLVED (housekeeping)
Three coupled pieces, now all resolved:
- Typechecker accepts the full two's-complement range `[-2^63, 2^63-1]`
  (`typ_bounds_int`), so INT64_MIN can be assigned to an int64 lvalue / array
  element (was a symmetric `[-(2^63-1), 2^63-1]` placeholder — set that way in
  2021 only because INT64_MIN couldn't be spelled in the bounds table itself;
  the narrow types already used their true min).
- C emission of a folded INT64_MIN uses the signed split form (FB-010 / WP-H3).
- The lexer now rejects a bare **positive** `9223372036854775808` (which used
  to silently wrap to INT64_MIN) while still accepting `-9223372036854775808`
  (FB-014): an `expect_neg_number` flag, set by the unary-minus handler around
  the number-lexing call, blesses only an immediately-negated 2^63; nested/
  parenthesized forms (`- -2^63`, `-(2^63)`) correctly error. A `-` before an
  unsigned literal is now a clear error too.

Separate, still open: implicit literal coercion to another type is rare in
Ficus by design (args almost never coerce) — its own topic, not addressed here.

### 1.4 Saturating arithmetic — DEFERRED (design direction only)
With wrap as the defined default, add explicit saturating ops (integer, and
possibly clamping float→int) as a named family — syntax TBD (`+^`? `sat_add`?
`@saturate` block?). Keep the door open in operator-syntax decisions (sec. 4).

### 1.5 Implicit numeric widening at assignment — CANDIDATE (unassigned WP)
`=` is not a function (no overloads possible), so assignment gets the one
implicit numeric conversion in the language: at ExpAssign (var / array element
/ record field; NOT call arguments, NOT val bindings), the RHS may widen to
the LHS type when every RHS-type value is exactly representable in the LHS
type (signed↑, unsigned↑, unsigned→wider signed, small ints→float/double
exactly, float→double; nothing else; `int` treated as 64-bit on all platforms
for portability of acceptance). Compound assigns inherit the rule via the
final store. Additive/non-breaking; synergizes with imperative fold
(accumulator updates are assignments). Open: extend to `var x: T = init`
with an explicit annotation (arguably the same intent)?

### 1.6 `[]` is "some collection" — DECIDED & implemented (resolve-2)
`TypVarCollection`: the empty-collection literal types as "some
list/vector/array, elements unknown" instead of a fully free var; it unifies
only with collections / free vars. Kills the free-`[]`-accumulator capture
class (FB-007/S3 root), and `val n: int = []` is a typecheck-time error.
Reform-epoch option kept open but **DEFERRED as taste** (correctness
motivation is gone): `[]` = empty *list* only + a dedicated empty-array
spelling (`[.]`); TypVarCollection is forward-compatible (would just narrow).
No source syntax for the var-form yet; add `'t [..]`-style only if generic
signatures ever need it.

### 1.7 Closed signatures / `-Wimplicit-rettype` — DECIDED direction (annotate-2)
The resolve-2 near-miss established that a generic function's **return
annotation is a load-bearing viability filter**: a free return type unifies
with any expected type and lets the candidate invade foreign contexts at
under-constrained sites. Discipline, in decreasing order of necessity:
overloaded generic operators MUST annotate returns (done, annotate-1: all 106,
`tools/lint_op_returns.py` on CI; fresh-var forms `'t3 complex` / `'t3 [+]` /
`('t3 ...)` express "returns SOME such thing" without foreclosing widening);
overloaded generic functions SHOULD (annotate-2 sweep: 276); all module-level
functions — opt-in via a new **`-Wimplicit-rettype` warning** (root modules
only; nested functions and lambdas exempt; the message prints the inferred
type for a copy-paste fix), with generic `-Werror` promotion and a `-Wall`
umbrella; `-Wimplicit-rettype -Werror` is the permanent CI gate for
stdlib/tests and, gradually (annotate-3), the compiler. The warning
machinery itself (non-fatal diagnostics) is the deliberate seed of
session-2's multiple-errors-per-run work. Beyond resolution, declared signatures are the
firewall for type-error recovery (a broken body can be poisoned without
losing the function's interface — session-2/LSP groundwork) and a future
dividend for separate interface-only compilation. Not a language change for
*users* — inference stays; the flag is a discipline knob.

## 2. fold — imperative form — DECIDED (spec ready for Brief #3)

Body is a void expression; accumulators are scoped variables updated by named
assignment; tuple assignment `(a, b) = (b, a+b)` has simultaneous semantics
(automigrator relies on it); `break`/`continue` legal; warning only when an
accumulator is never mentioned, suppressed by `acc = _`; nested fold may
assign outer accumulators; fold is sugar, erased by K-form. Migration is loud
(old tuple bodies fail typecheck); acceptance = bitwise-identical K-form for
migrated code. (Vadim has a worked design for the reform mechanics — to be
written up for the Brief #3 session.)

- `@parallel` on general fold is forbidden by design. Named reduction sugars
  instead: `sum`, `minmax` (one pass), `argmin`/`argmax`, `count`, `mean`,
  `sumabs`/`sumsqr`/`maxabs`. **DECIDED direction (2026-07-09): the sugars
  are defined as expanding to a compiler-known reduction intrinsic**
  (identity + combine + declared associativity), NOT to a plain fold — so the
  syntax layer (future macros, §4) and the parallel/fusion semantics stay
  decoupled, and the syntax reform does not nail the sugars' meaning to
  "just fold". Histogram (scatter-add) not in v1 but the design must not
  preclude it; user-defined `(identity, combine)` reductions are v2.
- OPEN (pull into the fold design): **writer accumulators** — appending to a
  list/vector inside fold (`fold val l = [] for x <- 1:n {if isprime(x)
  {l += x}}`); runtime already has vector writers. Unifies with the view that
  list comprehensions are fold sugar. Related: fold inside an array
  comprehension as a free scan (prefix sums) — legal? what does it yield?
- OPEN: initial-value inference for reduction sugars — largely dissolved by
  the intrinsic direction (identity is part of the intrinsic contract, not
  the call syntax); confirm at the session.

## 3. Generics & types syntax

### 3.1 Bracketed generic types — DECIDED direction
`list[t]`-style application replaces `'t list` (spelling confirmed; `<t>`
retired). Disambiguation vs indexing is semantic (Go/Nim precedent), or v1
without explicit instantiation in expression position. Type and its
auto-generated constructor functions are one entity in resolution (the
resolve-1 machinery is ready for this). Multi-parameter types
(`result[a, b]`) become natural. Corpus script: count type/value shadowings
(expected ~0).
- Generic *function* declaration syntax — CANDIDATE (todo-2026):
  `fun add[u, v, r](a: u, b: v): r {...}` — return type in the parameter
  list rhymes with the closed-signature (§1.7) world.

### 3.2 Casts — CANDIDATE
Drop `(x :> T)` in favor of constructor-call notation `T(x)` (enabled by 3.1;
functional notation already works for simple types). Automigrator rewrite;
decide what remains for chained/exotic casts.

### 3.3 Records — OPEN (design session)
(a) Update syntax: keep/drop/revise `pt .= {y = ...}`; non-destructive update
`pt.{y = ...}` — is it worth it? (b) Variant cases with record payloads: no
way to name/pack a case's payload — proposal: auto-introduce `Case.t`
(`Engineer.t`) and allow `Engineer(data: Engineer.t)`. (2022-era wish,
resolve now.)

### 3.4 Type renames — CANDIDATE
`half` → `fp16` (`half` is too generic a name), add `bf16`. Mechanical,
automigrator-friendly.

### 3.5 Containers — DECIDED direction (2026-07-09)
Four sequence containers, renamed to match the target persona's intuition
(C++/Python: "vector" = growable mutable array):
- **`vector`** = today's `Dynvec.t`, promoted to first-class (literals?,
  comprehensions, slicing — settle slice semantics consistently with arrays);
  natural pymodule mapping to Python list. Lands in/adjacent to the epoch.
- **`rrbvec`** = today's `vector` (immutable RRB). Rename in the epoch
  (zero current users → free); the upgrade program (pattern matching over
  sequences, cursor = (container, start_index with cached block leaf) — a
  zipper/finger over RRB) is post-reform, gated on a real consumer; the
  natural first consumer is LSP incremental document snapshots.
- `list`, `array` unchanged in role.
- **TypVarCollection covers all four** (`[]` unifies with any of them).
- **Typed-empty syntax — DECIDED direction**: the converting-constructor
  family `list(...)/vector(...)/rrbvec(...)/array(...)` already exists
  stdlib-wide; `ctor([])` is its currently-missing empty case — the
  constructor's expected type pins the argument's TypVarCollection (a small
  LitEmpty case in the constructor branch, where return-type participation
  already lives). Post-reform also `list[t]([])`; `([] : list[t])` remains.
- **fold writer accumulators (§2) are designed container-generically**:
  `acc += x` appends uniformly for list/vector/rrbvec — this is what makes
  rrbvec "the efficient FP-friendly alternative to list" without a separate
  campaign.

## 4. Operators & expressions

- **`.op` elementwise family — OPEN**: keep Matlab-style `.op`, or drop and
  adopt `@` for matmul (needs disambiguation vs preprocessor `@`, unary `@`
  before macros, and `@{...}` interpolation)? Render corpus samples in both
  styles for A/B.
- **Tuple arithmetic — DECIDED & implemented (resolve-2)**: redefined on
  **uniform tuples only** (`('t1 ...) op ('t2 ...)` + scalar broadcast);
  structural generics keep the `(...)` meta-type. Open detail: is the
  uniform-shaped fresh-var return (`('t3 ...)`) the right long-term contract
  for the elementwise family?
- **`++` as concatenation — DEFERRED (taste)**: the correctness motivation
  (mis-typing of `+` at free-typed sites) is gone with 1.6 + 1.7; keep only
  if aesthetics demand it. (Vadim: "maybe not".)
- **Ternary `a ? b : c` — OPEN** (currently `if a {b} else {c}`).
- **Variadic functions — CANDIDATE, small delta** (todo-2026 sketch): the
  heavy machinery has existed for years — tuple comprehensions with full
  compile-time unroll, the `(...)` meta-type, uniform `('t ...)` packs. The
  actual delta: (1) pack call arguments into a tuple at the last non-keyword
  parameter (`fun max[t](args: t ...)`, `fun print(args: ...)`); (2) slice
  sugar `tup.n...` (skip first n); (3) one new axis in
  `compare_fun_generality`: fixed arity ≻ variadic (validated by the E1
  census, as usual); (4) recursion/pass-through falls out of existing tuple
  machinery. Additive — can land post-session-2 as its own WP, outside the
  breaking epoch. Interaction to settle at the session: `max(1,2,3)`
  (variadic) vs `max(for ...)` (reduction sugar/macro) — one name, two
  mechanisms; proposal: a visible macro wins, no macro/function overloading
  in v1.
- **Hygienic macros — OPEN (design session; big)** (todo-2026 sketch).
  Direction agreed in discussion 2026-07-09:
  - **Category-typed parameters**, not token trees: a macro parameter is an
    expression or a named syntactic category (`@for[t]` = the parsed clauses
    + body of a comprehension) — Racket-syntax-classes/Nim-typed-macros
    lineage, keeps macros readable (incl. for AI agents) and unable to grow
    arbitrary syntax. Macros only in call position; no statement-level forms.
  - **Hygiene by default**: binders introduced in a macro body are
    auto-renamed (the `name@NNN` gensym machinery exists); `@gensym` remains
    for deliberate anaphora only.
  - **Expansion is syntactic; typecheck runs after** (no
    expansion/inference interleaving à la Scala 3); macro type annotations
    are checked on the expanded form.
  - **Location chains from day one**: every expanded node carries
    (macro-def ↔ call-site), errors read "in expansion of sum(...) at
    file:line" — session-2's loc/recovery groundwork is the prerequisite.
  - Reduction sugars = macros expanding to the §2 reduction intrinsic;
    `@string`/`@file`/`@line` subsume the backtick notation — **sequencing
    decision needed**: backtick removal is breaking (UTest), so either the
    macro engine makes the epoch, or backticks survive one more version.
- **f-strings — RESOLVED (misdiagnosis, harden-1 era)**: unescaped
  `f"{find("x")}"` always worked; only the C/Python-style `\"` spelling
  fails, with a misleading error. Behavior locked by
  `basic.fstring_nested_literals`. Residual small item: a clearer parser
  error for the `\"` spelling.

## 5. Modules

- **Naming — CANDIDATE**: lift the capital-letter requirement (already
  half-lifted de facto); propagate the decision to stdlib naming.
- **Hierarchical modules — OPEN**: require `__init__.fx` in the root and
  every subdirectory of a complex module (Python-style), to distinguish a
  module tree from a mere directory of files. Interacts with the
  case-insensitive stdlib-shadowing trap (test-file naming caveat in
  CLAUDE.md).
- **Qualified operator spelling — CANDIDATE (Brief #3 grammar item)**:
  accept `(op)` wherever `__opname__` is accepted, e.g. `Complex.(*)(a, b)`
  (todo-2026). The mangled form `Module.__mul__(a, b)` already parses and is
  the current disambiguation hatch; `(op)` is the pleasant spelling.

## 6. Overload resolution — DECIDED & implemented (resolve-1/2)

Collect → rank → commit in `lookup_id_opt`: least-generic viable candidate
wins (`compare_fun_generality`, C++-partial-ordering-style one-way skolem
test; non-generic is the limit case; interface upcasts fold in); determined
ties are **ambiguity errors** (candidates listed with modules/locations,
qualified-call hint); under-constrained sites (free type vars) fall back to
env-order first-match — after 1.6, the compiler corpus has **7** such
harmless sites; the `-pr-resolve` fallback counter is the tripwire.
Keyword-argument overloads distinguished by normalization (no tie-break
ladder). FB-007/016/017 closed. **Reform-epoch endpoint**: promote the
remaining under-constrained fallback to a hard "ambiguous, annotate" error
(loud-migration philosophy); general call-site deferral was evaluated and
REJECTED on census data (7 sites don't justify worklist machinery).

## 7. Deferred / additive (post-reform; decide direction only)

- **Compiler-known intrinsic families (perf/GPU epoch design session)** —
  two families with a shared contract (index space + pure element function):
  **A: independent-outputs** (einsum, matmul, convolution — todo-2026's
  "class of special functions computing output elements independently"),
  giving the compiler parallelism-without-analysis, fusion with subsequent
  elementwise comprehensions, and a backend seam (C loops today; .cl/.cu
  emission + kernel disk cache; loops-JIT gemm/conv for CPU) — Fortran's
  MATMUL-as-intrinsic precedent; **B: reductions** (identity + combine +
  declared associativity) — the §2 sugars' target, the only home of
  `@parallel` reductions.
- `@broadcast` comprehensions (mismatch without the tag stays an error);
  `@parallel_if(cond)`; >5-dim arrays (cap 7?); auto serialization
  (json/yaml/pickle/flatbuffer hooks, extending auto-print);
  dynamically-typed `tensor` (CPU/GPU/NPU black box — core of the
  OpenCV-wrapping and pymodule story).

## 8. Methodology — DECIDED

Measure every reform on the 68K-line corpus (sites changed, parser
ambiguities); render one sample in N syntax variants for A/B by eye; all
breaking changes in one epoch with an AST-rewriting automigrator; tutorial
rewritten once after the epoch (fold in 1.1/1.2/1.6/§6 semantics text then).
The E1-census instrument (`-pr-resolve`) is the standing validator for any
change that can touch resolution — including "mere" annotation sweeps.
