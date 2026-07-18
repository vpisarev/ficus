# Ficus macros — design (v1)

Status: design session concluded 2026-07-20 (Vadim + Claude). This document
is the spec; `macro-1` is the implementation brief. Supersedes the sketch in
`language_changes_brief.md` §4 (fold this in at close-out).

## Goals & non-goals

Macros are **declarative templates over parsed syntax**: readable (for
humans and agents), unable to grow arbitrary grammar, hygienic by default.
v1 explicitly excludes: procedural (compile-time-executed) macros — that
would need an interpreter (the loops-JIT epoch, someday); statement-level
macro forms (grammar reserved, see §7); macro/function overloading;
variadic macro parameters (unneeded — a template may contain a compile-time
tuple-`for`, which unrolls AFTER expansion by the existing machinery, so
composition replaces macro recursion).

## 1. Where expansion lives: inside the type checker

The parser cannot do it: it sees one file, and a macro defined in the stdlib
is invisible to it — yet macros must be module-scoped citizens like
functions. Later than the type checker is impossible: macros are symbol
manipulation, and post-typecheck there are no abstract symbols left. So
expansion happens IN `check_exp`, interleaved — the established in-house
technique (tuple comprehensions and the object notation already build
subtrees during checking and feed them back), generalized. The lazy-lambda
trick (root-to-leaves checking in special cases) is the precedent for the
control-flow shape.

Mechanics in `check_exp(ExpCall(f, args))`: a cheap **pre-probe** — if `f`
is a plain identifier, look up a `DefMacro` binding by NAME and
arity/parameter-categories only (macro resolution is purely syntactic; no
types, no `compare_fun_generality`). Hit → expand the RAW (unchecked) AST
arguments into the template and `check_exp` the result; miss → the ordinary
path, arguments type-checked as usual. Expansion is recursive (the expansion
may contain macro calls); since templates are declarative (no conditional
expansion), direct/mutual macro recursion cannot terminate — a **depth
limit** (256) errors out printing the expansion chain
(`macro expansion too deep: count → helper → count at file:line`).

### 1.1. Macro parsing

Macros are parsed by Parser, `DefMacro` is added to `Ast.fx` to describe macros
in the same style as function definition, macro's body is exp_t.

### 1.2. Macro application

Macro's body is inserted where the macro is 'called',
formal macro parameters are replaced _everywhere_ in the body with actual parameter,
so some side-effects need to be avoided, it's macro author responsibility to
store actual parameters to intermediate values and then process those intermediate values.
See `EXPECT_EQ` example below.

After the insertion of the macro body to the place where it's called
and the parameter substitution, the result expression is then type-checked.

## 2. Namespace, shadowing, diagnostics

Macros share the value namespace. `DefMacro` entries live in module
environments and import like functions. The KIND of the nearest lexical
binding decides the path: a local `fun count` hides an imported macro
`count` (the user cannot be expected to know every stdlib macro name), and
vice versa. Two same-level bindings from different modules → the standard
ambiguity error + qualified call (`Builtins.count(...)`).

Mandatory diagnostic for the one nasty case: a macro shadowed by a function,
called macro-style — `count(for x <- a {p(x)})` hits "a `for` as a function
argument". The error must say: *"'count' here is a function (declared at
…); a macro 'count' exists in Builtins — write `Builtins.count(...)` if that
was intended."* The macro binding is visible in the env, so this is cheap.

No sigils: macros look exactly like function calls (`!`/`@` prefixes are
visual noise; the LSP answers "what is this" with a click).

## 3. Parameter categories (v1) and special types

- `@expr[T]` — an expression, optionally with a type ascription in the macro
  signature (checked AFTER expansion, on the expanded form — no
  expansion/inference interleaving à la Scala 3);
- `@for_expr[T]` — the parsed clauses + body of a comprehension/loop. Grammar:
  the parser generalizes today's special-cased `all/exists/...(for ...)`
  argument into a "loop-argument" node legal in ANY call; the type checker
  feeds it only to macros and gives a targeted error on a non-macro callee
  (see §2's diagnostic).

## 4. Hygiene & name resolution

- **Binders introduced by the template are hygienic automatically**: renamed
  via the existing `name@NNN` gensym machinery.
- **Free references resolve at the CALL SITE** (compile-time, in the
  caller's lexical env — this falls out naturally from typecheck-time
  expansion). In Lisp taxonomy this is the free-identifier-capture side of
  `defmacro`, NOT true runtime dynamic scope; the Common Lisp mitigation
  (packages/qualified symbols) is our convention: **exported macros qualify
  their helper names** (`Module.helper(...)`) — and the ninja names the
  reduction family relies on are visible everywhere by construction, so the
  practical exposure is small. Future refinement path, only if this bites:
  per-identifier `bind`/`mixin`-style knobs (Nim), not a wholesale flip to
  definition-site.
- **Refinement (macro-1, from the UTest migration).** Qualifying a helper name
  is right for a name-collision *shield*, but it must NOT be used for a **generic
  helper whose body depends on user-overloadable operations** (`string`, `==`,
  `+`, comparisons) applied to the macro's argument types. A macro expands into
  the caller's env and resolves there — that is the *whole point*, and the
  principled difference from a function, which resolves its body in its own
  module. Calling such a helper **qualified** (`UTest.cmp_eq_(...)`) re-pins its
  generic body to the helper's home module, so a user's LOCAL overload (e.g. a
  hand-written `string` for a recursive variant defined in the test function)
  becomes invisible and the expansion fails. Calling it **unqualified**
  (`cmp_eq_(...)`, brought in by the caller's `from Module import *`) keeps the
  whole chain resolving at the call site, so the local overloads are found. Rule
  of thumb: qualify a helper only when its body is self-contained; leave it
  unqualified when it must see the caller's overloads.

## 5. Primitives (built into the engine, not expressible as templates)

`@string(expr)` — the source text of the argument (spans are exact
post-reform-prep-1); `@file`, `@line` — the call-site location;
Exactly the set needed for `assert`/`EXPECT_*` and to
retire the backtick forms (UTest).

Placement semantics (Vadim's check-question, settled): inside a stored
template these are UNINTERPRETED primitive nodes — the lexer and parser
assign them no value (otherwise they would forever point at Builtins.fx);
the engine substitutes literals AT EXPANSION TIME from the `ExpCall` in
hand. With nested macros, `@file`/`@line` denote the OUTERMOST (user-code)
call site — the top of the expansion chain, like C's `__LINE__`; the full
chain is in the diagnostics anyway.

The proof-of-design example, verbatim implementable:

```
macro assert(e: @expr[bool]): void {
    if !e { throw AssertError(f"{@file}:{@line}: assertion '{@string(e)}' violation") }
}
```

(`AssertError` resolves at the call site; it lives in auto-imported
Builtins, so no qualification needed.) **Single-evaluation discipline** —
the canonical pattern when a hole is USED more than once (`EXPECT_EQ`
compares and prints): bind holes to hygienic temporaries first —
`val a_ = a; val b_ = b; if a_ != b_ {...}` — template binders cannot
capture caller names, so this is safe by construction and evaluates each
argument exactly once. (`@string(e)` does not evaluate, so it never counts
as a use.)

## 6. Locations & debugging

Every expanded node carries a (macro-def ↔ call-site) chain; diagnostics
render "in expansion of sum(...) at file:line" (the diag-1 printer already
supports notes). `-pr-expand` dumps the post-expansion AST of the root
module (the `-pr-ast`-family pattern).

## 7. Declaration syntax (proposal — Vadim confirms spelling)

```
macro sum[T](f: @for_expr[T]): T {
    fold s = (some_zero_of[T]...) for <f> { s += <f.body> }   // schematic
}
```

`macro name[typarams](param: category, ...) [: rettype] { template }` — the
template is Ficus syntax parsed at DEFINITION time with parameter holes;
the exact hole-reference notation inside templates (bare parameter name vs
`<param>`) is macro-1 Phase-A material: pick whichever the parser can
disambiguate cleanly, document, done. Statement-form macros
(`with f = File.open(...) { ... }` — `ident (pattern '=' exp)+ block`) are
v1.5: the juxtaposition `ident ident` became grammatically FREE after
generics-1 removed postfix type application, so the space is reserved —
design the production now, implement later.

### 7.1. simplification

For simplicity, we can eliminate type specification of `@expr[T]` and `@expr_for[T]`,
instead just use `@expr` and `@for_expr` and leave all the type checking
to the Type Checker.

Also, supporting `@for_expr` is the second priority.

## 8. Acceptance dogfood & staging

- **M6 (the v1 acceptance), reordered per Vadim — the assert family FIRST**:
  (E1) `assert`, `AssertError`, and a first gtest-style pair
  (`EXPECT_EQ`/`EXPECT_NEAR`) land as stdlib macros — they exercise the
  `expr` category, all three primitives, hygiene and the single-evaluation
  pattern, and they matter more day-to-day than another for-wrapper;
  (E2) the existing `all`/`exists`/`find`/`filter` special forms are
  rewritten as stdlib macros and DELETED from the compiler, with the current
  lowering as the oracle: K-form of the macro version must match the
  special-form version across the corpus. The compiler gets smaller; the
  engine is proven on real clients of both categories.
  (E2) the proper implementation of assert and the subsequent removal of back-tick
  notation and the corresponding cleanup of UTest.fx and the unit tests is
  the natural next step.
