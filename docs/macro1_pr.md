# macro-1: a declarative macro engine + the end of the backtick `…` notation

## Why

Ficus had a backtick context-capture hack — `` EXPECT_EQ(`icmp(5,3)`, 1) `` — a
pure lexer trick that desugared `` `expr` `` into a 4-tuple
`(expr, "expr", "file", line)` so test assertions could report *where* and *what*
failed. It worked, but it was a lexer-level wart: an ad-hoc notation, a matching
`errctx[T]` type, and 2–3 overloads of every `EXPECT_*`/`ASSERT_*` (plain / first
arg captured / both captured). `assert` had the same split (`fun assert(bool)` +
a backtick 4-tuple form).

This PR replaces all of it with a **real, declarative macro engine** and rebuilds
`assert` and the `EXPECT_*`/`ASSERT_*` family as macros that capture the call site
and the asserted expression's source text automatically — then deletes the
backtick notation entirely.

## The macro engine

Macros are declarative templates over parsed syntax, expanded **at type-check
time** (in the `ExpCall` pre-probe of `check_exp`), hygienic by default.

```
macro assert(e: @expr): void {
    if !e { throw AssertError(f"{@file}:{@line}: assertion '{@string(e)}' violation") }
}
```

- **Declaration**: `macro name[[typarams]](p: @cat, …) [: rt] { template }` (or
  `= expr`). Every parameter has a **category** — `@expr` in v1 (`@for_expr` is
  reserved for the deferred for-wrapper family). A hole is a bare parameter name
  (zero new grammar). The primitives reuse `@`-lexing: `@file`/`@line` (the
  **outermost** call site, like C's `__LINE__`) and `@string(e)` (the exact
  source text of `e`'s argument, multi-line spans included).
- **Pre-probe** (`macro_lookup`): a plain-identifier / `Module.name` callee that
  names a macro is expanded before the ordinary call path checks arguments —
  purely by name + arity, no type ranking, so the `-pr-resolve` census for
  functions is untouched. The nearest binding's KIND decides (a local `fun count`
  hides an imported macro `count`). **Macros overload by arity** like functions.
- **Expansion**: one pass over a private copy of the template — hole substitution
  (fresh per use), **explicit hygiene** (every template-introduced binder is
  renamed `name@NNN`, so it can neither capture nor be captured), and primitive
  lowering — then `check_exp` in the caller's env. A depth limit (256) with the
  printed chain guards runaway recursion; a diagnostic inside an expansion renders
  `… in expansion of 'name' at file:line`.
- **`@string` plumbing**: the module now retains its source (`dm_source` +
  `dm_line_offsets`, built once in `Parser.parse`), so both the diagnostic caret
  and `@string` extract spans in O(1)+O(span) with **no file re-reads** (the old
  caret path re-read the file and did an O(line) `list.nth`, which would be
  quadratic under `@string`). Extraction is bracket-balance-aware, since AST node
  locs never cover a trailing `)`/`]` — `@string(foo(a, b))` == `foo(a, b)`.

`-pr-ast` already prints the fully-expanded AST, so no separate `-pr-expand` flag
was needed.

## The clients

- **`assert` is a macro** (Builtins): `assert(cond)` prints the user file:line and
  the source of `cond`. The old `fun assert(bool)` and backtick-4-tuple forms are
  gone; call syntax is unchanged, so callers didn't move.
- **UTest** — one macro per name replacing the 2–3 backtick/errctx overloads:
  comparison `EXPECT_EQ/NE/LT/LE/GT/GE` + `ASSERT_*`, `EXPECT_NEAR`/`ASSERT_NEAR`
  (scalar / array / tuple / array-of-tuples / list), bool `EXPECT`/`ASSERT` with
  an optional message (arity overload), and `EXPECT_THROWS`/`EXPECT_NO_THROWS`.
  One helper set serves both families via a `fatal` flag. **UTest: 477 → 294
  lines.**

Sample failure — no backticks, exact location and source:

```
test_ds.fx:34: comparison 'u12.size == 99' failed.
  actual:   11
  expected: 99
```

## The key design point: macro name resolution

A macro **resolves in the caller's environment** — the mental model is *the body
is pasted inline where the call expands*. Two rules fall out (design §4, rewritten):

1. **Always-imported names resolve for free.** `==`, `string`, the Builtins/base
   operators are auto-imported everywhere, so a macro uses them unqualified and
   they bind at the call site — picking up a user's **local** overload (e.g. a
   hand-written `string` for a recursive variant defined inside a test function).
2. **A macro's own non-base-module names must be qualified.** A macro in `UTest`
   must write `UTest.test_report_cmp_` / `UTest.g_test_state`, or the expansion
   would compile only under `from UTest import *`.

So the macros do the overload-dependent work (`a == b`, `string(a)`) **inline**
(call-site overloads, lazy — only on failure) and hand the pre-stringified,
non-generic report to a **qualified** UTest helper. A qualified call to a
*generic* helper would re-pin its body to the helper's home module and lose the
local overloads — that was a real bug this uncovered (a variant with a local
`string`, and any `import UTest; UTest.EXPECT_EQ(…)` call site, both now work).

## Retiring the backtick notation

1. All 35 test files migrated `` `expr` ``→macro by a comment/string/char/
   f-string-aware Python scanner (handled the transpose operator `A'`, nested
   f-strings, and `~msg=`→positional). Old UTest functions removed; the
   transitional `EXPECT_*_` renamed to `EXPECT_*`.
2. `assert` migrated; the ~280 backtick `` assert(`cond`) `` sites in `lib/NN`
   rewritten to `assert(cond)`. Fixed a latent `| AssertError =>`→
   `| AssertError _ =>` catch (fallout of the earlier `exception AssertError:
   string`).
3. The **backtick lexer path is deleted** — a code-position backtick now errors
   with "the backtick `…` notation was removed" (negative golden 805). The
   `errctx` type, the `fun assert` forms, and the lexer's backquote state are all
   gone.

## Verification

- **`fxtest all`** (unit + negative + ir + cfold + corpus O0/O3) — **PASSED**
  (negative now 100 with the backtick golden); **determinism**, **sanitize**
  (ASan+UBSan), and the nightly **doctut** (87) legs green.
- **Bootstrap fixpoint holds** — `assert` and the compiler use each other, so the
  bootstrap was regenerated across the migration (44 modules); `update_compiler.py
  --check` is clean. `-pr-resolve` census unchanged (the pre-probe never fires for
  non-macro names).
- Every `lib` module and example compiles; `test/test_macro.fx` (9 directed
  tests) locks hygiene, single-evaluation, `@string`/`@line` (incl. multi-line and
  nested-macro = outermost site), arity overloading, and the `assert` throw
  message.

## Diffstat

- `compiler/*.fx`: +467 / −55 (7 files: Ast, Ast_pp, Ast_typecheck, Lexer, Parser,
  K_normalize, C_gen_code)
- stdlib (`lib/*.fx`, `lib/NN/*.fx`): +411 / −589 — a **net shrink**
- tests (`test/*.fx`): +575 / −451
- `compiler/bootstrap/`: regenerated (44 files)

## Deferred (next sessions)

- **E2 / for-wrappers**: the `@for_expr` category + a loop-argument grammar node;
  rewrite `all`/`exists`/`find`/`filter` as macros and **delete** the compiler
  special forms (the compiler then *shrinks*), with K-form equivalence vs the
  special-form lowering as the oracle.
- **Statement-form macros** (`with f = File.open(…) { … }`) — grammar space
  reserved, production sketched.

Design: `docs/macro_design.md`. Report: `docs/macro1_report.md`.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
