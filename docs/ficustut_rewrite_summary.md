# ficustut.md rewrite — reviewer summary (2026-07-15)

4474 → 4586 lines. Everything below is against your uploaded source; the
change-map (docs/ficustut_change_map.md) was the plan, all five Q-decisions
applied (Q1 new-form-only, Q2 "Vectors: mutable and not", Q3 parallel rules
in the Parallel chapter, Q4 Appendix B, Q5 → doctut follow-up).

## Rewritten / new
- **Folding** — teaches only the imperative form: accumulators are variables,
  body is void, `+=`/assignments, break/continue legal; `bounding_box` is the
  showcase (the unchanged-tuple `else`-arm is gone); `val fold` shortcut kept.
- **Numbers** — expanded: float→int functions return `int` by design
  (cvFloor/cvRound analogy); NEW semantic-guarantees block (signed overflow
  wraps; `>>` arithmetic/logical by signedness, count is `int`; small-unsigned
  promotion + explicit wrap-back; `/` and `%` rules); NEW fp16/bf16 paragraph.
- **Vectors: mutable and not** — new chapter after Arrays: `vector[T]`
  (construction family, push/pop/back, slice-assignment incl. insert/remove/
  replace, slices-are-copies + why, read-lock with a thrown-error example and
  the index-loop escape hatch), then `rrbvec` as the immutable sibling.
- **Memory Management** — vector moved to the mutable cluster, rrbvec added
  to the immutable one; the type table gained a `rrbvec` row and a mutable
  `vector` row; NEW closing passage: the O(1)-assignment axiom and how the
  whole table derives from it.
- **Generic Programming** — declaration story rewritten: explicit
  `fun f[T](...)` / `type name[K, D]`, uppercase-by-convention (declaration
  is what disambiguates); NEW generic-types passage (`point_t[T]`,
  `tree_t[K, D]`, application brackets, `T?`, arrays stay postfix);
  the `(dilate3x3 : float [,] -> float [,])` example promoted as THE
  instantiation channel ("an annotation is a hint to inference, not a cast");
  `T [+]` / `(T ...)` subsections converted, `dot` example fold imperative.
- **Choosing between list, vector, rrbvec and array** — the TBD stub replaced
  by a four-way decision list (typo "Choosting" fixed).
- **Appendix B. The Language Server** — new: what v1 does, build & configure.

## Sections with substantive additions
- **Usage** — new Note: multiple diagnostics/carets, `-Wall`/`-Werror`/
  `-Wimplicit-rettype`, `-diag-format=json`, Appendix B pointer ("great the
  world" typo fixed too).
- **Code Blocks** — jump keywords as arm/branch values, with an example.
- **Functions** — NEW "Overload resolution" subsection (least-generic wins,
  ambiguity error, qualified-call escape, why return annotations matter);
  bare-`return` note.
- **Tuples** — NEW "Tuple assignment" subsection (simultaneity, Fibonacci,
  swap, `_` components).
- **Types** — the notation NOTE rewritten for brackets; contour example
  converted (`vector[point]`, `vector[Complex[float]]`); generic-instance
  bullet rewritten; the `half` entry replaced by real `fp16` + `bf16`
  entries; keyword row updated (fp16 bf16, rrbvec added).
- **Parallel Programming** — new paragraph: payload-free exceptions propagate
  (and why), no jumps in parallel bodies, no `@parallel` on general fold
  (reductions planned).
- **Lists / Variants / OOP / everywhere** — notation migrated: 259 apostrophe
  type-vars, postfix applications (`int list` → `list[int]`, `'t ref` →
  `ref[T]`, old `'t vector` → `rrbvec[T]`), ~35 teaching-function headers
  gained explicit `[T]`/`[A, B]`/`[Key, Data]` parameter lists, type
  definitions converted to `type name[...]` form (rbtree, hashtable_t,
  Complex, option, Rect), chained `list[T] list` → `list[list[T]]`,
  graph_t examples restructured.
- **Stdlib overview** — `Json.string` compact mode noted.

## Method & safety
The migration ran as: hand-rewritten sections → fence/inline-aware mechanical
notation pass → full diff against your original reviewed line-by-line for
false positives (the pass initially mangled contractions like `it's` inside
code comments and quoted words like `'float [,]'` — all 30+ such hits were
found by the diff and reverted verbatim). Prose apostrophes are untouched.

## Left for you / the doctut session
- ONE `TODO(vadim)` marker: Appendix A's `ficus -h` dump is v0.1.0-era —
  regenerate from the real compiler (the doctut brief instructs the agent to
  cross-check the whole appendix).
- fp16 literal suffix: the Types entry keeps the historical `h` suffix claim —
  doctut verifies it against the actual lexer.
- The doctut session (per Q5): extract every code block, compile/run as
  `fxtest.py doctut` (`// fragment` marker for deliberately partial blocks),
  probe suspicious features with mini unit tests added to the suite, verify
  Appendix A options.
