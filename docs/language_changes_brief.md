# Ficus language changes — accumulating brief

Living document: every planned syntax/semantics change lands here first, with a
status, and graduates into Brief #3 (the reform implementation brief) and the
tutorial rewrite. One breaking epoch, one automigrator; the T4 acceptance
criterion for behavior-preserving changes is bitwise-identical K-form.

Status legend: **DECIDED** (design fixed, spec below) · **CANDIDATE** (agreed
direction, details open) · **OPEN** (needs design session) · **DEFERRED**
(post-reform, additive).

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

### 1.3 int64 literal range — DONE (housekeeping)
Three coupled pieces, now all resolved:
- Typechecker accepts the full two's-complement range `[-2^63, 2^63-1]`
  (`typ_bounds_int`), so INT64_MIN can be assigned to an int64 lvalue / array
  element (was a symmetric `[-(2^63-1), 2^63-1]` placeholder — set that way in
  2021 only because INT64_MIN couldn't be spelled in the bounds table itself; the
  narrow types already used their true min).
- C emission of a folded INT64_MIN uses the signed split form (FB-010 / WP-H3).
- The lexer now rejects a bare **positive** `9223372036854775808` (which used to
  silently wrap to INT64_MIN) while still accepting `-9223372036854775808`
  (FB-014): an `expect_neg_number` flag, set by the unary-minus handler around
  the number-lexing call, blesses only an immediately-negated 2^63; nested/
  parenthesized forms (`- -2^63`, `-(2^63)`) correctly error. A `-` before an
  unsigned literal is now a clear error too.

Separate, still open: implicit literal coercion to another type is rare in Ficus
by design (args almost never coerce) — its own topic, not addressed here.

### 1.4 Saturating arithmetic — DEFERRED (design direction only)
With wrap as the defined default, add explicit saturating ops (integer, and
possibly clamping float→int) as a named family — syntax TBD (`+^`? `sat_add`?
`@saturate` block?). Keep the door open in operator-syntax decisions (sec. 3).

## 2. fold — imperative form — DECIDED (spec ready for Brief #3)

Body is a void expression; accumulators are scoped variables updated by named
assignment; tuple assignment `(a, b) = (b, a+b)` has simultaneous semantics
(automigrator relies on it); `break`/`continue` legal; warning only when an
accumulator is never mentioned, suppressed by `acc = _`; nested fold may assign
outer accumulators; fold is sugar, erased by K-form. Migration is loud (old
tuple bodies fail typecheck); acceptance = bitwise-identical K-form for
migrated code.

- `@parallel` on general fold is forbidden by design. Named reduction sugars
  instead: `sum`, `minmax` (one pass), `argmin`/`argmax`, `count`, `mean`,
  `sumabs`/`sumsqr`/`maxabs`. Histogram (scatter-add) not in v1 but the design
  must not preclude it; user-defined `(identity, combine)` reductions are v2.
- OPEN (pull into the fold design): **writer accumulators** — appending to a
  list/vector inside fold (`fold val l = [] for x <- 1:n {if isprime(x)
  {l += x}}`); runtime already has vector writers. Unifies with the view that
  list comprehensions are fold sugar. Related: fold inside an array
  comprehension as a free scan (prefix sums) — legal? what does it yield?
- OPEN: initial-value inference for reduction sugars (`sum(for x <- a {x**2})`
  without an explicit `0.`)?

## 3. Generics & types syntax

### 3.1 Bracketed generic types — DECIDED direction, CONFIRM spelling
`list[t]`-style application replaces `'t list`. Design log (06–07.07.2026)
fixed **square brackets** `list[t]`. Disambiguation vs indexing is semantic
(Go/Nim precedent), or v1 without explicit instantiation in expression
position. Type and its auto-generated constructor functions are one entity in
resolution (feeds the FB-007 design). Multi-parameter types (`result[a, b]`)
become natural. Corpus script: count type/value shadowings (expected ~0).
- OPEN: generic *function* declaration syntax (`fun [u, v] add(a: u, b: v)`?).

### 3.2 Casts — CANDIDATE
Drop `(x :> T)` in favor of constructor-call notation `T(x)` (enabled by 3.1;
functional notation already works for simple types). Automigrator rewrite;
decide what remains for chained/exotic casts.

### 3.3 Records — OPEN (design session)
(a) Update syntax: keep/drop/revise `pt .= {y = ...}`; non-destructive update
`pt.{y = ...}` — is it worth it? (b) Variant cases with record payloads: no way
to name/pack a case's payload — proposal: auto-introduce `Case.t`
(`Engineer.t`) and allow `Engineer(data: Engineer.t)`. (2022-era wish,
resolve now.)

### 3.4 Type renames — CANDIDATE
`half` → `float16` (OpenCV-consistent; `hfloat` alternative), add `bfloat16`.
Mechanical, automigrator-friendly.

## 4. Operators & expressions

- **`.op` elementwise family — OPEN**: keep Matlab-style `.op`, or drop and
  adopt `@` for matmul (needs disambiguation vs preprocessor `@` and
  `@{...}`)? Render corpus samples in both styles for A/B.
- **Ternary `a ? b : c` — OPEN** (currently `if a {b} else {c}`).
- **Variadic functions — CANDIDATE**: `println(a, b, c)`, `max(a, b, c, d)`.
  Kills the "println takes ONE argument" trap.
- **Single-element tuple — CANDIDATE**: Python-style `(x,)` (construction and
  type positions; matters for function types).
- **f-strings — CANDIDATE (parser fix)**: allow string literals inside `{}`
  interpolations (`f"{find(\"x\")}"`).
- **Concatenation operator — CANDIDATE (Vadim, resolve-1 follow-up)**: stop
  spelling list/string concatenation as arithmetic `+`. Either drop the
  functional record-update operator `.{...}` and reuse `.` as concatenation,
  or introduce `++`. Motivation: `+('t list, 't list)` currently competes with
  every user-defined `+` at under-constrained call sites (a free-typed `[]`
  fold accumulator being concatenated — the FromOnnx.fx:1012 / FB-007 S3
  shape), forcing the scalar-left complex operators `op('t, 't complex)` to
  stay fenced. A dedicated concatenation spelling removes that entire
  collision class without deferral.

## 5. Modules

- **Naming — CANDIDATE**: lift the capital-letter requirement (already
  半-lifted de facto); propagate the decision to stdlib naming.
- **Hierarchical modules — OPEN**: require `__init__.fx` in the root and every
  subdirectory of a complex module (Python-style), to distinguish a module
  tree from a mere directory of files. Interacts with the case-insensitive
  stdlib-shadowing trap (test-file naming caveat in CLAUDE.md).

## 6. Overload resolution (FB-007) — SESSION 1 LANDED (`resolve-1`); deferral pending

Session 1 (branch `resolve-1`, 2026-07-08) DECIDED & implemented: resolution
is collect -> rank -> commit; the **least-generic viable candidate wins**
(C++-style partial ordering via two one-way skolemized unification trials,
`compare_fun_generality`); at a **fully-determined** call an unresolvable tie
is an **ambiguity error** (two flavors: equally-applicable -> qualify the
call; overlapping-but-unordered -> also possible to add a more specific
overload); at an **under-constrained** call (free type vars among the args)
a tie falls back to env-order first-match — today's semantics, kept until
deferral. Q2 resolved by **keyword normalization** (not a tie-break ladder):
when exactly one candidate has keyword params, the keywordless one is
compared with the same implicit empty keyword record the caller gets, so an
exact keywordless match ranks more specific than a candidate viable only via
all-defaulted keywords (`string(tensor)` picks `string(nntensor_t, ~kw..)`
over the record-generic; `sqrt(81.0)` picks `Math.sqrt(double)` over a local
`sqrt('t, ~n=2)`). No scope-proximity
ranking (§6 of the proposal). Escape hatch: module-qualified mangled-operator
call `Module.__op__(a, b)`; a prettier `Module.(op)` spelling is a Brief #3
grammar item. Not-found diagnostics list each candidate with a one-line
rejection reason.

`lib/Complex.fx` operators + the `examples/fst.fx` demo are UNLOCKED in a
homogeneous form (all bodies `'t op 't` — sidesteps S1; scalar-on-the-left
variants stay fenced, see FB-007 and the concatenation-operator candidate in
§4).

Session 2 (OPEN): deferral — `int + 't` inside generic bodies (S1, for
mixed-type operator variants) and under-constrained calls (S3 commit-half:
`FromOnnx.fx:1012` shape; un-fences the scalar-left complex variants and the
S3 test in `test/test_resolve.fx`); error recovery (TypErr poisoning,
multiple errors per run); edit-distance "did you mean".

## 7. Deferred / additive (post-reform; decide direction only)

`einsum` as a language intrinsic; explicit `@broadcast` comprehensions
(mismatch without the tag stays an error); `@parallel_if(cond)`; >5-dim arrays
(cap 7?); auto serialization (json/yaml/pickle/flatbuffer hooks, extending
auto-print); dynamically-typed `tensor` (CPU/GPU/NPU black box — core of the
OpenCV-wrapping and pymodule story); generic macro-like comprehension forms
(`all(for x <- a {p(x)})`).

## 8. Methodology — DECIDED

Measure every reform on the 68K-line corpus (sites changed, parser
ambiguities); render one sample in N syntax variants for A/B by eye; all
breaking changes in one epoch with an AST-rewriting automigrator; tutorial
rewritten once after the epoch (fold in 1.1/1.2 semantics text then).
