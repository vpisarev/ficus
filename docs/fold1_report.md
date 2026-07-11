# fold-1 report — the imperative fold reform (first construct of the epoch)

Branch `fold-1`. Brief: `docs/fold1_brief.md`; design: `docs/language_changes_brief.md` §2.

## What shipped

`fold` is now an **imperative** construct — a thin sugar over `for`:

```
fold <acc> = <init> for <clauses> { body }
  ==>  { var <acc> = init; for <clauses> { body }; <acc> }
```

The accumulator is a real mutable **var**, visible and assignable in the body;
the whole-fold value is the accumulator after the loop. Multiple accumulators
are multiple vars (`fold a=0, b=1 for ...`, or a tuple pattern). `break` /
`continue` / `return` are legal in the body (it is a plain `for`) — the old
form banned them. A closure in the body captures the accumulator **var** (sees
the final value), not a per-iteration snapshot.

The named reduction sugars (`all`/`exists`/`count`/`find`/`find_opt`/`filter`/
`vector`, spelled `name(for ...)`) are a **separate** construct and were NOT
touched — they already lower directly to an imperative accumulator loop.

## How it was staged (methodology)

Per the brief, the reform ran behind a temporary keyword `__fold__` so every
step was reversible and ladder-green:

1. **Phase 0.1 — simultaneous tuple assignment** (`(a,b) = (b,a+b)`): a
   parse-time desugar (`Parser.fx` `make_tuple_assign`) — the RHS materializes
   once into a temp before any store, so the Fibonacci and array-swap idioms
   are simultaneous. `_` components emit no store; bare `_ = e` evaluates for
   effects. This is the migration's target for tuple-accumulator folds.
   - Surfaced **FB-023** (fixed): the swap exposed a latent C-gen soundness
     hole — `find_single_use_vals` inlined a single-use temp that READS mutable
     memory past an intervening aliasing store. Fixed with a recursive
     `movement_unsafe_read` guard (keep such temps materialized). Perf within
     noise; 35/55 bootstrap modules regenerated, behavior-preserving.
2. **Phase 0.2 — census** (`docs/fold_census.md`): 261 `fold` sites classified
   by body-tail shape. Headline: **W (closure-captures-accumulator) = 0** across
   the whole corpus, so the new var-capture semantics can't change behavior at
   any migrated site — the reform's biggest semantic risk dissolved by data.
3. **Phase 1 — `__fold__` in the compiler**: the keyword + `transform_new_fold_exp`
   desugar; an accumulator-unused warning (suppressed by `acc = _`); an empty
   block `{}` (ExpNop) made a valid void no-op anywhere. Tests `test/test_fold2.fx`.
4. **Phase 2 — the automigrator + token-precise spans**:
   - Fixed reform-prep-1's batch spans to be **token-precise** (each token gets
     its own span from its start to the next token's start). A side win: 12
     negative goldens got tighter, correct carets (e.g. `break`, not `{ break`).
   - A compiler-assisted rewriter: `-pr-fold-sites` emitted a per-site migration
     descriptor; `tools/fold_migrate.py` applied span-based textual edits with a
     `--check` safety net (re-parse + typecheck). Both are single-use and were
     removed at the flip.
5. **Phase 3 — migrate the corpus, then the compiler**:
   - Corpus (test/lib/examples/tools) migrated idiomatically by Vadim.
   - `compiler/` (~156 sites) migrated in the same style — four parallel
     migrators for the bulk, `Parser.fx` (comprehension-desugar nested folds,
     the chained-cmp 3-accumulator fold) and the hardest `C_gen_code`/`K_fuse_loops`
     blocks (a 10-accumulator fold, doubly-nested folds) by hand.
   - **Compiler-specific silent-bug class**, handled throughout: an accumulator
     whose name also appears in its for-iteration collection (`fold X = .. for e
     <- X`) or is re-bound by a body `val X` would make the var-based form
     iterate/read the wrong binding — invisible to `--check`, caught only by the
     **bootstrap fixpoint / ladder**. Every such site renames the accumulator
     (or the shadowing local). Two the migrators missed were caught by the build.
6. **Phase 4 — the flip**: `__fold__` renamed to `fold` tree-wide; the old
   plain-`fold` desugar, the `FOLD: bool` staging, and the migrator tooling
   removed (`FOLD` is nullary again). An old-style value body now warns
   "accumulator never assigned … the body must UPDATE the accumulator (e.g.
   's = <expr>' / 's += ...')" (negative goldens 708/709). A 2-stage bootstrap
   avoided the chicken-and-egg: (1) make `fold` = new alongside the `__fold__`
   alias + regen bootstrap so `ficus0` understands it, (2) rename + drop the
   alias + regen.

## Acceptance / verification

- Full ladder green at every phase: unit 168, negative 90, T4 IR 63, cfold,
  corpus O0/O3. **Bootstrap fixpoint holds** — the compiler self-builds
  deterministically after migrating its own folds (the strongest check that no
  migrated fold silently miscompiles).
- The T4 `fold` IR snapshot did **not** need semantic regeneration — the new
  desugar lowers to a clean `{ var acc; for {..}; acc }` and is keyword-agnostic.
- `-pr-resolve` census unaffected (the fold desugar doesn't touch resolution).

## Follow-ups (not blockers)

- The old-style-body diagnostic is currently `warning: accumulator never
  assigned` + a downstream type error. A single fold-specific "fold body must be
  void" primary error would read better (golden 709 documents today's output).
- Writer accumulators (`l += x` appending to list/vector), named reduction
  sugars as macros, and `@parallel` reductions remain out of scope (wave 2 / §2).
