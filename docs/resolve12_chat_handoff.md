# resolve-1/2 day outcome — handoff for the design chat (2026-07-08)

Everything below is MERGED to master (PRs #31, #32, #33; CI green;
working branches deleted). Full details: `docs/resolve1_report.md`,
`docs/resolve2_report.md`, `docs/found_bugs.md` (FB-007), and
`docs/language_changes_brief.md` §4/§6.

## Your brief was executed, then overtaken by better news

1. **resolve-1 (your surgery brief) landed as specified**: collect→rank→
   commit, least-generic viable wins, determined ties = ambiguity error
   (2 flavors), under-constrained ties = env-order fallback, Q2 solved by
   keyword normalization (not a tie-break ladder). FB-016/FB-017 fixed.
   Census over the compiler: 350 sites = 343 ranked + 7 harmless fallbacks
   + 0 errors; bootstrap fixpoint = corpus-invariance proof. Perf +1.8%.

2. **resolve-2, same day (not in your brief): `TypVarCollection`** — `[]`
   is now typed "some list/vector/array" instead of a fully free var; it
   unifies only with collections/free vars. This killed FB-007/S3 at the
   root (the free-`[]`-accumulator capture, FromOnnx.fx:1012), made
   `val n: int = []` a typecheck error, and unfenced the scalar-left
   Complex operators. Census unchanged. ~+2% typecheck.

3. **S1 turned out to be already dead** (killed by resolve-1 en passant):
   generic bodies re-resolve per instantiation — `fun h(a: int, b: 't) =
   a + b; h(2, 1.5) == 3.5` works. The recorded S1 evidence was a WP-E-era
   artifact plus a structural issue (see 4).

4. **Complex.fx is fully mixed-type now** (`'t1 op 't2 (complex)`, result
   type inferred; widening idiom `1.0 * fcomplex` → double complex;
   `v *= 2` works with an int scalar). For `+`/`-` the pass-through result
   component is nudged to the coerced type by **Vadim's `r - r` pattern**
   (`val r = a + b.re; complex(r, b.im + (r - r))`) — constant folding
   provably erases it at BOTH -O0 and -O3 (bare `:>` cast in the K-form).
   Imaginary literals `N.fi` were fixed in the lexer (they had NEVER
   worked: int zero real part), so `val c = ref (1 + 1.fi)` — the verbatim
   2021 fenced demo — runs in fst.fx. **FB-007 is closed.**

5. **§5 deferral is REJECTED as a near-term item** (decided with Vadim,
   data-driven): after TypVarCollection the census shows 7 harmless
   fallback sites in 68K lines, and generic bodies already re-resolve.
   Reform-epoch endpoint instead: make under-constrained ties a hard
   "annotate" error. Tripwire: the `-pr-resolve` fallback counter.
   Session-2 scope is now diagnostics only: TypErr recovery + multiple
   errors per run, edit-distance hints, `Module.(op)`.

## The near-miss you'll want in your risk models

Dropping the return annotations from Complex's mixed operators **broke the
compiler's own build** (red master; caught by the CI bootstrap check —
`fxtest all` never rebuilds the compiler). Mechanism: the homogeneous
`: 't complex` return annotation was a load-bearing **viability filter**;
without it the candidate's free return type unifies with any expected type,
so at an under-constrained site (a recursion-typed accumulator concatenated
into an annotated `cstmt_t list`, C_gen_code.fx:1328 — the S3 shape with a
plain free var, which TypVarCollection cannot guard) the env-order fallback
handed it the call. Fix: `: 't3 complex` — a FRESH var, "returns SOME
complex" — rejects foreign contexts, keeps widening.

Consequence, swept immediately (annotate-1): **all 106 generic stdlib
operators now annotate their return type** (fresh-var forms `'t3 [+]` /
`('t3 ...)` / `'t3 vector` for the mixed elementwise families), enforced on
CI by the new `tools/lint_op_returns.py`. Its `--funs` mode is the queued
worklist: **276 overloaded generic functions** (string ×17, print, size,
norms, min/max; Builtins 101, UTest 52, Array 48, Math 23, OpenCV 11,
Map/Set/...) still lack return annotations — next sweep, per-family
judgment, CI-gate once clean.

## Also closed on the way

- The "f-string `{}` can't contain a string literal" gotcha was a
  MISDIAGNOSIS (harden-1 era): unescaped `f"{find("x")}"` always worked;
  only the C/Python-style `\"` spelling fails. Docs fixed, behavior locked
  by `basic.fstring_nested_literals`.
- Tuple arithmetic redefined on uniform tuples only (`('t1...) op
  ('t2...)` + broadcast); structural generics keep `(...)`.
- `++`-as-concatenation is demoted to a taste question (correctness
  motivation gone with TypVarCollection).

## Open questions worth your thought

- The 276-function sweep: annotation conventions per family (`string(...)
  : string` is trivial; what should `elemtype`/`size`-style type-level
  helpers declare?).
- Reform-epoch: `[]` = empty list only + dedicated empty-array spelling
  (`[.]`)? And: promote the 7 remaining fallback sites to hard errors?
- `('t3 ...)` fresh-var returns are uniform-tuple-shaped; is that the
  right long-term contract for the elementwise tuple family?
