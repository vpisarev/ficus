# Brief: macro-1 — the macro engine v1 (Fable)

**You are Claude (Fable) in Claude Code, in the Ficus repo.** Branch
`macro-1` off master. THE spec is `docs/macro_design.md` — read it fully;
this brief only adds implementation order, oracles and stop-rules. Also
read: repo CLAUDE.md; `docs/fold1_report.md` (staging playbook; the note
that `all/exists/find/filter` exist as special forms with imperative
lowering — they are your acceptance oracle); diag-1 report §printer (notes/
chains you will attach expansion locations to).

## Phase A — recon (no code)

- Map today's pseudo-macro machinery you will generalize, on both floors:
  parser-level (fold, try-finally, the `all/exists/...(for ...)` special
  forms — where exactly is the loop-argument special-cased, what does the
  lowering produce) and typechecker-level (tuple comprehensions, object
  notation — HOW a synthesized subtree is fed back to `check_exp`; this is
  the pattern the engine reuses).
- Settle the template-hole notation (design §7 leaves it to you): pick what
  the parser disambiguates cleanly, document in the report.
- Inventory the exact lowering of each special form (IR dumps of directed
  examples) — these become the K-form equivalence goldens for Phase E.

## Phase B — `DefMacro` end to end

Parser: `macro name[typarams](param: category, ...) [: rettype] { template }`
— the template parses at definition time (Ficus grammar + holes). AST:
`DefMacro` (name, params with categories, template tree, def loc). Env: a
new binding kind, module-scoped, imported like functions; the §2 shadowing
rule (nearest binding's KIND decides) and the ambiguity error via the
standard machinery.

## Phase C — the engine

In `check_exp(ExpCall)`: the pre-probe (plain-identifier head, DefMacro
lookup by name + arity/categories — purely syntactic, NO type-based
ranking), expansion = template copy with:
- hole substitution (raw, unchecked argument ASTs);
- **automatic hygiene**: every binder introduced by the template body gets a
  fresh `name@NNN`. Probably, `gen_id()` can be used for that.
- **loc chains**: every produced node records (macro def loc ↔ call-site
  loc); wire into the diagnostic printer as a note line
  ("in expansion of 'sum' at file:line") — golden-locked;
- primitives: `@string(expr)` (source text via the argument's span —
  reform-prep-1 spans make this exact), `@file`, `@line` (call site);
  - implementation of `@file` and `@line` is more or less straightforward;
    inside parser we just represent them both as identifiers (ExpIdent).
    When the macro is expanded, we need to store the stack of expanded macros and
    @file and @line are replaced with a string and a number literal respectively that
    represent file and line of the bottom of the macro stack expansion
    (that is, the outermost macro expansion point).
  - @implementation of `@string(expr)` is also straightforward. When macro is expanded,
    we substitute all formal parameters with the actual parameters everywhere in the body.
    e.g. when `assert(foo(x + y*5) > 0.1)` is expanded, we get something like
    `"assertion '{@string(foo(x + y*5) > 0.1)}' is violated"` and we get nested expression
    as argument to `@string`. from this expression we get the minimum and maximum location
    in the file (possibly it's enough to get the start and end location of the outermost
    expression, which is `ExpBinary(OpCmp(),...)` in this case, and then extract the text
    from the specified file and the specified range of locations.
    Hopefully, it can be done much faster than O(N) where N is size of the corresponding source file.
- depth limit 256 with the chain-printing error.
Then feed the expansion to `check_exp`. Free names resolve in the caller's
env — that is not extra code, that is the default; do NOT build
definition-site capture.

## Phase D — grammar generalization + diagnostics

The loop-argument (`for`-construct as a call argument) becomes a general
node the parser allows in any call; the typechecker accepts it only for a
macro parameter of category `@for_expr` and otherwise emits the §2 targeted
error, including the shadowed-macro hint ("a macro 'count' exists in
Builtins — write Builtins.count(...)"). Negative goldens for: loop-arg to a
non-macro; macro shadowed by function called macro-style; ambiguity between
two imported macros; depth-limit error (its chain rendering).

## Phase E — M6 acceptance, in Vadim's priority order

**E1 — the assert family (FIRST; needs only the `@expr` category + the
primitives, no `@for_expr`):** `assert(e: @expr[bool])` per design §5 verbatim, an
`AssertError` exception in Builtins, and `EXPECT_EQ`/`EXPECT_NEAR` using the
single-evaluation pattern (hygienic temporaries — assert exactly-once
evaluation with a side-effecting-argument test). Directed tests lock: the
thrown message contains the USER file:line (not Builtins.fx!) and the
argument's source text; nested use (a macro whose template calls `assert`)
reports the outermost call site; a `@string` of a multi-token expression
reproduces its exact source span. UTest itself is NOT migrated in this
session (that is the backtick-retirement session) — but these macros are the
API it will migrate to.

**E2 — the for-wrappers:** Let's postpone it for now, just make it possible to
provide the new clean assert() implementation that does not require back ticks
to capture the context.
~~rewrite `all`, `exists`, `find`, `filter` as
macros in `Builtins.fx` (qualified helper names per design §4); DELETE the
compiler special forms.
Oracle: for the directed examples from Phase A AND every corpus use, the
K-form (`-pr-k` at -O0 and -O3) is equivalent to the special-form version —
byte-identical modulo gensym numbering; any structural delta is a stop-and-
report. The bootstrap diff doubles as the corpus proof (compiler sources use
these forms). fxtest/doctut stay green (the tutorial documents these ops —
their behavior must not move).~~

## Ground rules

House standard: full ladder + doctut + determinism + sanitize per phase;
`update_compiler.py` fixpoint; `-pr-resolve` census unchanged (macro
pre-probe must not perturb function resolution — the census is the proof);
found_bugs for discoveries; don't push. STOP signals: the pre-probe turning
out to need type information (that breaks the design — report); any Phase-E
K-form delta; template parsing demanding grammar changes beyond the
loop-argument node and the `macro` declaration itself. Out of scope:
UTest's migration to the new asserts (next session, with backtick removal);
statement-form macros (grammar reserved only — put the production sketch in
the report), backtick removal, the reduction family, `with`. Report
`docs/macro1_report.md` with the Phase-A maps, the hole-notation decision,
the K-equivalence evidence, and the compiler LoC delta (it should SHRINK —
that number goes in the report's first line).
