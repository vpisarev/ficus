# Brief: FB-015 — control-flow keywords in expression position (prep for the fold reform)

**You are Claude Code working in the Ficus repo.** Branch `ctrlflow-1` off
master. Context docs: `docs/found_bugs.md` FB-015 (priority note),
`docs/language_changes_brief.md` §2 (the imperative-fold reform this
unblocks), repo `CLAUDE.md`.

## The bug and why it matters now

Two parser limitations, both misreporting as `unexpected token '}'`:
1. `continue`/`break` cannot be the *value* of a `match` arm / `if` branch —
   `[:: for e <- xs { match e { | A(x) => x | _ => continue } }]` fails.
2. A bare `return` (no value) does not parse; `return <expr>` is fine.

The imperative-fold reform makes fold bodies void statement sequences where
`match e { | A(x) => acc += x | _ => continue }` is the PRIMARY idiom — the
automigrator would generate code the parser rejects. This fix is additive
(accepts strictly more programs) and must land before the reform.

## Architectural ground truth (Vadim — read before planning)

`C_gen_code.fx` (the hardest module in the compiler — treat it as read-only
reference) already implements general unwinding: code is split into blocks,
each `block_ctx_t` carries a cleanup section (destroying locally created
ref-counted values, some via decref) plus labels and `bctx_break_used` /
`bctx_continue_used` / `bctx_return_used` counters; break/continue/return AND
exceptions all route through these cleanup chains (exceptions additionally
set the thread-local error context and a negative return code). **The backend
already knows this semantics for statement-position control flow.** The fix
is therefore a FRONTEND change: open the expression-position road in parser /
typecheck / K-normalization, and let the existing lowering do its job.

- **Anti-pattern, explicitly forbidden**: do NOT implement any of this as
  "throw a special exception and catch it" — integrate at the existing
  low level or not at all.
- **`throw` is the template**: it is already legal as an arm/branch value and
  flows through all four passes. At every design decision, first answer "how
  does ExpThrow do it?" and mirror that.
- **@parallel context (Vadim)**: exceptions ARE already propagated out of
  @parallel loop bodies, with one restriction — the exception must carry no
  payload (a bare OCaml-style one-line exception; a payload is a ref-counted
  object allocated in a worker thread, a bare tag is just a constant, no
  cross-thread ownership). Implication for the corners: statement-position
  behavior in @parallel bodies is the spec — Phase A documents what
  break/continue/return may do there TODAY, and the expression forms inherit
  exactly that (no widening of @parallel semantics in this brief; if e.g.
  `break`-from-@parallel is currently rejected, it stays rejected with a
  targeted message).
- **C_gen_code.fx changes are a STOP signal**: the working hypothesis is
  zero backend changes. If Phase A exploration or any later phase concludes
  the backend must change, stop, write up why, and leave it for review —
  do not improvise in C_gen_code.

## Phases

### Phase A — exploration (no code)
Map the throw template end-to-end and the current control-flow handling:
- `Ast.fx`: the exp forms for break/continue/return (do they exist as
  expressions already, or statements only?), `ExpThrow`'s type story.
- `Parser.fx`: where throw is accepted vs where break/continue/return are;
  what exactly rejects `| _ => continue` and bare `return`.
- `Ast_typecheck.fx`: how ExpThrow gets its unify-with-anything type; where
  (if anywhere) "break outside a loop" is currently diagnosed.
- `K_normalize.fx`: how a throw-tailed arm/branch merges with valued arms
  (the match temp assignment must be skipped for the jumping arm).
- `C_gen_code.fx` (READ ONLY): confirm bctx routing for statement
  break/continue/return; note which `bctx_kind`s forbid what (e.g. `return`
  inside an outlined/@parallel body).
Deliverable: a short plan comment in the PR description; proceed only if the
zero-backend-change hypothesis survives.

### Phase B — parser
`break`/`continue` as primary expressions; `return` with an OPTIONAL value
(bare `return` = return-void). Where they remain illegal, the error must be
targeted (never the generic `unexpected token '}'`).

### Phase C — typecheck
- Type them like throw: a fresh type var (the expression never yields a
  value), so a jumping arm unifies with any sibling arm.
- Legality with real diagnostics: `break`/`continue` require an enclosing
  loop ("'break' outside of a loop"); bare `return` requires the enclosing
  function's return type to be void ("'return' without a value in a function
  returning 'double'"); `return e` unifies `e` with the declared/inferred
  return as today.
- Decide-and-document corner: `return`/`break` inside an `@parallel` loop
  body or comprehension that outlines — follow what statement-position does
  today (likely an error for @parallel; verify in Phase A), make the
  expression form behave identically.

### Phase D — K-normalization
Arm/branch tails: mirror the ExpThrow path exactly — the jumping arm
produces no value; no read of an unassigned match temp; no phantom decref of
a never-created value. An all-jumping-arms match in value position behaves
like today's all-throw match.

### Phase E — tests (the actual acceptance)
- Directed suite `test/test_ctrlflow.fx`: the matrix
  {break, continue, bare return, return expr} × {match arm, if branch, both
  branches, nested loops (break targets the INNERMOST — assert via
  side-effects), fold body, comprehension body} × {ref-counted temporaries
  (strings/arrays/closures) live at the jump}. Values asserted, not just
  "compiles".
- **Leak oracle**: run the suite (and `test_all`) under
  `ASAN_OPTIONS=detect_leaks=1` with the sanitize cflags, at **-O0 AND -O3**
  — "all cleanup sections ran" is machine-checked, not eyeballed. Wire the
  new file into the fxtest corpus (O0/O3 differential) as well.
- Negative goldens: break outside loop (expression AND statement position),
  bare return in a non-void function, @parallel corner — locking the NEW
  targeted messages (replacing `unexpected token '}'`).
- The FB-015 route-arounds in `trace_resolve` (accumulator loop, inverted
  guard) can now be rewritten in the natural style — do it; it is the
  in-tree proof the fix works, and `-pr-resolve` output must stay identical.

### Ride-along (one separate trivial commit, sanctioned)
FB-018: `lib/Array.fx:346` `size(a)` → `size(d)`; un-fence, add a
`diag([1,2,3])` unit test. Nothing else rides along.

## Ground rules

Standard ladder before/after each phase (`make`, `test_all`, `fxtest all`,
`determinism`, `sanitize`), `update_compiler.py` fixpoint after compiler
edits (parser/typecheck/K_normalize edits WILL regenerate those bootstrap
modules — expected; any OTHER module's `.c` changing is a red flag, since
this change is additive and existing programs' K-form must be bit-identical).
Census (`-pr-resolve` over `compiler/fx.fx`) unchanged. New bugs →
`found_bugs.md`, fence, don't chase. Do not push. Final report
`docs/ctrlflow1_report.md`: the Phase-A map, semantic decisions (the
@parallel corner, all-jumping-arms), leak-oracle results at both opt levels,
CLAUDE.md diff (the FB-015 gotcha replaced by two lines on the new
diagnostics).
