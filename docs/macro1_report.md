# macro-1 report — the declarative macro engine (v1, E1: the assert family)

**Compiler LoC delta: +425 net (457 inserted / 32 deleted across 7 `compiler/*.fx`
files: Ast / Ast_pp / Ast_typecheck / Lexer / Parser / K_normalize / C_gen_code;
a good share is comments) + 58 net in the stdlib (Builtins / UTest).** Unlike the
E2 for-wrapper deletion, which will *shrink* the compiler by removing the special
forms, E1 only *adds* the engine — the shrink comes in the backtick-retirement
session.

This session implements the macro **engine** plus the `@expr` category, the three
primitives, hygiene, and the first clients (`assert_`, `EXPECT_*_`). The
for-wrapper family (`@for_expr`, rewriting `all`/`exists`/… as macros) is
deferred per the brief (E2), as is UTest's migration onto the new asserts.

## What macros are

A macro is a declarative template over parsed syntax, expanded **at type-check
time**, inside the `ExpCall` case of `check_exp`. It is a module-scoped, function-
like citizen (shares the value namespace, imports like a function), but produces
no runtime code of its own. Declaration:

```
macro name [ [typarams] ] ( param: @cat, ... ) [ : rettype ] { template }   // or  = expr
```

Every parameter carries a **category** (`@expr` in v1; `@for_expr` is reserved).
The proof-of-design client, verbatim from the spec, now compiles and runs:

```
macro assert_(e: @expr): void {
    if !e { throw AssertError(f"{@file}:{@line}: assertion '{@string(e)}' violation") }
}
```

## Phase A — the machinery we generalized (recon)

- **Parser-level pseudo-macros** — `all/exists/count/find/filter(for …)` are
  desugared *at parse time* in `transform_fold_exp` (`Parser.fx`), recognized by
  `extend_simple_exp_` when an `ExpIdent` callee is immediately followed by
  `(` + a for-start token. `fold` and `try/finally` are likewise parse-time
  desugars. These are **name-hardcoded** and invisible to module scope — the
  reason macros cannot live in the parser.
- **Typechecker-level subtree synthesis** — the reusable pattern is
  "build a fresh AST and feed it back to `check_exp`": the object-notation
  fallback (`a.foo(x)` → `Module.foo(a, x)` then re-check) and the tuple/record
  comprehension lowering (`gen_for_in_tuprec_it`: `dup_exp` the body, `check_pat`
  the binders to mint fresh hygienic ids, `check_exp` the result). The macro
  engine is this pattern, generalized and driven by a user-authored template.

## The hole-notation decision

**A hole is a bare parameter name.** Inside a template, a reference to a
parameter is an ordinary `ExpIdent(param)`; expansion substitutes it with the raw
argument AST. This is what the parser disambiguates with zero new grammar (no
`<param>` sigils), and it reads exactly like the code it stands for. The three
primitives reuse the existing `@`-lexing: an unknown `@name` already lexes as
`AT`+`IDENT`, so `@file`/`@line` parse to reserved placeholder idents and
`@string(e)` to an ordinary call `__macro_string__(e)` — again no new grammar.

## The engine (`Ast_typecheck.fx`)

1. **Pre-probe** (top of the `ExpCall` arm): if the callee is a plain identifier
   (or `Module.name`), `macro_lookup` scans the env by **name + arity only** —
   purely syntactic, no type ranking, so the `-pr-resolve` census for ordinary
   functions is untouched. The **nearest lexical binding's KIND decides**:
   reaching an `IdFun`/`IdDVal`/`IdExn` before any macro means "ordinary call"
   (a local `fun count` hides an imported macro `count`, and vice versa).
   **Macros overload by arity** (like functions): a same-named macro whose
   parameter count does not match the call is skipped so a sibling variant with
   the right arity can win (`macro EXPECT_EQ_(a,b)` + `macro EXPECT_EQ_(a,b,note)`
   coexist). Keyword/optional parameters are *not* supported in v1 — the call
   `m(a, b, msg=x)` folds the keyword into a trailing record, bumping the arity;
   restoring `msg=`-style calls is a later engine step (kept off v1 deliberately,
   since positional defaults would make macro calls read differently from
   function calls).
2. **Expansion** = one transform over a private `dup_exp` copy of the template:
   - **hole substitution** — each `ExpIdent(param)` → a fresh `dup_exp` of the
     actual argument (fresh copies so repeated holes don't alias);
   - **hygiene** — every binder the template introduces (collected from all
     pattern positions) is renamed to a fresh `name@NNN`, so it can neither
     capture nor be captured by caller names. *(This is done explicitly, not left
     to `check_pat`: `macro m(e){val t=5; e+t}` called as `m(t)` must yield
     `caller_t + 5`, which requires the template's `t` to be freshened before the
     hole is substituted.)*
   - **primitives** — `@file`/`@line` → the OUTERMOST (user) call site's basename
     / line, like C's `__LINE__`; `@string(e)` → the exact source text of `e`'s
     argument.
3. The expansion is checked with `check_exp` **in the caller's env**, so free
   names (`AssertError`, operators, qualified helpers) resolve at the call site —
   the design's free-identifier-capture semantics, for free.
4. A **depth limit** (256) with the printed expansion chain guards the
   non-termination that declarative templates otherwise cannot avoid.
5. Every expansion pushes an `all_compile_err_ctx` note, so a diagnostic inside a
   macro body renders `… in expansion of 'name' at file:line` beneath the caret.

## `@string` and the source-text plumbing (the fast path)

`@string` needs exact source text on the **normal build path** (many asserts per
compile), not just the error path. Two changes:

- **Source retained on the module.** `defmodule_t` gains `dm_source` +
  `dm_line_offsets` (a line-start offset array), built once by a single scan of
  the in-memory buffer in `Parser.parse`. `Ast.get_source_line` (the diagnostic
  caret) and the new `get_source_span` both read from these in O(1) + O(span)
  with **no file re-reads** — the old caret path re-`File.read_utf8`'d and did an
  O(line) `list.nth` per lookup, which would be quadratic under `@string`.
- **Bracket-balanced span extraction.** AST node locs never cover a trailing
  closing delimiter (a call's span is callee..last-arg; the `)` is a token owned
  by no node). `get_source_span` therefore counts brackets left open in the
  folded span and extends the end over exactly that many matching closers
  (whitespace allowed, literals ignored), so `@string(foo(a, b))` → `foo(a, b)`,
  `@string(m[i, j])` → `m[i, j]`, not the truncated forms.

`-pr-ast` already prints the fully expanded AST (macro gone, primitives lowered
to literals), so it serves as the post-expansion dump; no separate `-pr-expand`
flag was added.

## E1 clients

- **`Builtins.fx`**: `exception AssertError: string` (was payload-less; the
  in-code TODO wanted this) + `macro assert_(e: @expr)`. The old `fun assert`
  forms and backtick capture are untouched (they retire next session). One
  ripple: `test/test_basic.fx` passed the bare `AssertError` value to
  `EXPECT_THROWS`; now `AssertError("")`.
- **`UTest.fx`**: `EXPECT_EQ_`/`NE_`/`LT_`/`LE_`/`GT_`/`GE_`/`NEAR_` macros, each
  a thin wrapper that passes every argument **once** to a public backing helper
  (`UTest.expect_*_`, qualified so it resolves at any call site). This is the
  single-evaluation pattern: the helper's parameters are the one evaluation, and
  `@string`/`@file`/`@line` are captured at the macro. They coexist with the
  backtick `EXPECT`/`ASSERT` functions; the suite migrates onto them later.

## Verification

- Directed suite `test/test_macro.fx` (8 tests, wired into `test_all.fx`): basic
  expansion, **hygiene** (no capture), **single evaluation** (side-effecting arg
  runs once), `@string` exact source, `@line` = call site, `assert_` throw
  message carries the USER file:line + source text, nested macro reports the
  outermost site. All green (`test_all` = 212 tests).
- Negative goldens `test/negative/801-804`: unknown primitive, match-shorthand
  body rejected, bad parameter category, depth-limit chain (the chain is capped
  at the innermost 8 frames so a runaway recursion gives a compact, deterministic
  diagnostic instead of 256 lines).
- **Full ladder `fxtest.py all` PASSED** — unit, negative (99), ir (66), cfold
  (2), corpus O0/O3 differential (13, 7 skipped). Bootstrap fixpoint holds
  (`update_compiler.py`, 44 modules regenerated — `options_t` is unchanged, but
  the new `macro` keyword + the AST/typecheck additions touch most modules'
  generated C; generated C for *programs* is unaffected).
- `-pr-resolve` census unchanged: the pre-probe scans by name/arity and returns
  "not a macro" for every ordinary call, so resolution outcomes (and the corpus
  differential that would catch any change) are untouched.

## Note for review

- **`macro` is now a reserved keyword.** Three compiler locals named `macro`
  (`C_gen_code.fx`) were renamed (`macro_exp` / `macro_id`); no stdlib or corpus
  identifier used it.
- `@file` yields the **basename** (matching the retiring backtick capture, which
  stored `Filename.basename`), so assert messages stay path-stable.
- `AssertError` gained a `string` payload; the one value-use site
  (`test/test_basic.fx`, `EXPECT_THROWS(..., AssertError)`) became
  `AssertError("")`.

## Deferred (next sessions)

- **E2 / for-wrappers**: `@for_expr` category + the loop-argument grammar node;
  rewrite `all/exists/find/filter` as macros and delete the compiler special
  forms (the compiler LoC then *shrinks*); K-form equivalence vs the special-form
  lowering as the oracle.
- **Backtick retirement**: migrate UTest + the unit tests onto `assert_` /
  `EXPECT_*_`, then delete the errctx/backtick machinery and the `fun assert`
  forms.
- **Statement-form macros** (`with f = File.open(…) { … }`) — grammar reserved,
  production sketched, implementation later.
