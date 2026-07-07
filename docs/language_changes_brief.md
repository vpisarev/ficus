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

### 1.3 int64 literal range — PARTLY DONE (housekeeping)
Typechecker now accepts the full two's-complement range `[-2^63, 2^63-1]`
(`typ_bounds_int`), so INT64_MIN can be assigned to an int64 lvalue / array
element (was a symmetric `[-(2^63-1), 2^63-1]` placeholder — set that way in 2021
only because INT64_MIN couldn't be spelled in the bounds table itself; the narrow
types already used their true min). Emission fixed separately (FB-010 / WP-H3).
REMAINING OPEN: the lexer accepts a bare **positive** `9223372036854775808` and
silently wraps it to INT64_MIN (FB-014). A clean fix must reject the bare
positive 2^63 while still allowing the negated form — the lexer can't currently
tell them apart (the unary minus is folded onto the literal only after
`getnumber` has already wrapped the magnitude). Related: implicit literal
coercion to another type is rare in Ficus by design (args almost never coerce) —
a separate, larger topic.

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
fixed **square brackets** `list[t]`; todo-2026 has an older `list<t>` spelling —
confirm `[t]` and retire `<t>`. Disambiguation vs indexing is semantic
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

## 5. Modules

- **Naming — CANDIDATE**: lift the capital-letter requirement (already
  半-lifted de facto); propagate the decision to stdlib naming.
- **Hierarchical modules — OPEN**: require `__init__.fx` in the root and every
  subdirectory of a complex module (Python-style), to distinguish a module
  tree from a mere directory of files. Interacts with the case-insensitive
  stdlib-shadowing trap (test-file naming caveat in CLAUDE.md).

## 6. Overload resolution (FB-007) — OPEN, gated on its design session

Specificity ordering (non-generic > generic; partial order à la C++ partial
ordering), `int + 't` inside generic bodies, cross-module candidate
visibility; diagnostic half (edit-distance "did you mean") separately.
Design against the 3.1 model (type+constructors one entity). Implementation
split: corpus-invariant hardening (bitwise-identical K-form on everything that
compiles today, Complex operators unlocked) may land pre-reform; anything that
changes candidate choice in currently-compiling code rides the reform epoch.

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
