# Brief #2: Ficus Compiler Hardening — Fixing the fxtest Catch

**Target repo:** https://github.com/vpisarev/ficus (branch, e.g. `harden-1`)
**Prerequisite:** Brief #1 infrastructure merged (tools/fxtest, T2–T5 suites, CI),
`docs/found_bugs.md` registry present.
**Executor:** Claude Code. **Reviewer:** Vadim Pisarevsky.

## 0. Goal and ground rules

Fix the confirmed bugs from `docs/found_bugs.md` (FB-001..FB-006, FB-008),
un-fence every corresponding quarantine/xfail/route-around, and extend the test
ladder with the two layers this catch proved necessary (sanitizer leg,
cfold-vs-runtime oracle).

Ground rules:

1. **FB-007 is OUT OF SCOPE.** It is a missing *specification* (overload
   resolution rules), not a bug; a design session precedes any agent work.
   Do not modify `Ast_typecheck.fx` beyond what a WP explicitly requires
   (expected: nothing).
2. **One work package per phase, checkpoint after each.** Full sanity loop
   before and after every phase:
   `make -j8 && bin/ficus -run test/test_all.fx && python3 tools/fxtest/fxtest.py all`.
3. **Every fix must flip its fence.** The registry entry says where the fence
   is; a fix is complete only when the xfail turns XPASS and is then removed /
   the route-around in suite code is replaced by the direct test. Append a
   status line to the registry entry (registry is append-only):
   `- fixed: <commit>, unfenced in <place>`.
4. **Smallest possible diffs.** No drive-by refactoring, no renames beyond the
   fix. After WP-A lands, the regenerated-bootstrap diff becomes a verification
   lens: each later fix should change only the expected functions in generated C.
5. Bootstrap regeneration (`compiler/bootstrap/*.c`) is done ONLY at designated
   points (end of WP-A, end of P5), as its own commit. Locate the exact
   regeneration procedure in the Makefile first (there is a target for it;
   verify rather than assume).
6. Brief #1 §8 (Ficus survival kit) fully applies to any new `.fx` code.

## 1. WP-A — FB-008: deterministic, stable C output (DO THIS FIRST)

Everything else is verified through generated-C diffs; this WP makes those
diffs trustworthy.

**Chosen strategy (Vadim): per-module numbering.** Background facts from the
investigation + author:

- Pure recompilation is already byte-deterministic (verified 4x). The churn
  appears only when some module's K-form changes: the *global* symbol-id
  counter shifts, and downstream, order-dependent naming/mangling picks
  different names for *unrelated* modules.
- There is a post-mangling cleanup pass (`compiler/C_post_rename_locals.fx`)
  that strips disambiguation suffixes from local names where unambiguous —
  locals are therefore mostly clean; the residual instability lives at the
  global level (exported/global symbol names, `_fx_g<N>` type names via
  `curr_km_idx` in `K_mangle.fx`, `make_unique_` `_1/_2` suffixes assigned in
  processing order).
- The author recalls there were also declaration-ORDER issues somewhere in
  C emission — treat emission order as a first-class suspect, not only names.

### A1. Build the repro first (the bug is UNCONFIRMED — confirm it)

Scripted, in a scratch checkout:
1. baseline build; save all emitted `.c` for the compiler modules;
2. apply a canned, DCE-surviving change to an early module (e.g. add a small
   *used* helper in `Ast.fx` — it must survive dead-code elimination and shift
   real symbol ids; the registry notes an unused function changes nothing);
3. rebuild; diff `.c` of *unrelated later* modules (e.g. `K_mangle.c`,
   `Parser.c`).
Expected pre-fix: renames (`_1/_2` swaps, `_fx_g<N>` shifts) and/or reordered
declarations in modules whose sources did not change. Record the observed
mechanism(s) in the registry entry before fixing.

### A2. Fix, in increasing invasiveness — stop when A3's tests pass

1. **Stable emission order:** audit every place where declarations / forward
   decls / init calls are emitted by iterating a Hashmap/Hashset or sorting by
   numeric id (`C_gen_fdecls.fx`, `C_gen_code.fx`, `C_gen_types.fx`,
   `K_mangle.fx`). Replace id-order with a stable key: (module, source
   location, qualified source name). Iteration over hash containers must never
   directly determine output order.
2. **Per-module counters:** make `curr_km_idx` (`_fx_g<N>` type names) and the
   `make_unique_` disambiguation counters per-module (reset at module start,
   name them `_fx_M<mod>g<N>`-style or equivalent), so numbering depends only
   on the module's own content and processing order *within* the module.
3. **Per-module id namespaces** for symbol ids — ONLY if 1+2 do not suffice
   (this touches `id_t` plumbing in `Ast.fx` and is invasive; expected to be
   unnecessary).

Name-collision correctness must be preserved: two same-named symbols in
different modules must still mangle apart (module name is part of the mangled
name — verify).

### A3. Regression tests (new fxtest subcommand `determinism`)

1. `fxtest.py determinism --rebuilds 3`: N clean rebuilds → byte-identical
   `.c` set (automates what was done manually).
2. `fxtest.py determinism --unrelated-change`: the A1 scripted scenario →
   assert **zero diff** in `.c` of modules whose `.fx` (and imports' K-form
   signatures) did not change. This is the property, encoded forever.
Wire both into CI (nightly is fine if slow).

**Acceptance (WP-A):** A1 repro demonstrates churn pre-fix; both A3 tests
green post-fix; bootstrap regenerated once, as its own commit, with a diff
that is itself instructive (expect mass rename → then stability).

## 2. WP-B — Quick fixes: FB-001, FB-003, FB-004, FB-005 + sanitizer leg

Four localized fixes. Each: fix → unfence → extend the suite that caught it.

### B1. FB-003 — `.clip/.wrap/.zero` on plain 1D arrays: broken C
`__idx0__` vs `__idx__` mismatch between the emitted call and
`FX_PTR_1D_CLIP`-family macros (`runtime/ficus/ficus.h`) — determine whether
the bug is the emission site (`C_gen_code.fx`) or the macro definition; fix
the inconsistent side. Then: unfence the T5 border suite route-around (it
currently tests `Vector` only) and add a matrix unit test:
{plain array 1D, plain array 2D, Vector, string} × {clip, zero, wrap}
(skip combos the language legitimately does not support — verify against the
tutorial §array access attributes, and note skips in the test).

### B2. FB-004 — empty strided slice: corrupt view → SIGSEGV
Fix the slice descriptor construction for `a[lo:lo:step]`, `step >= 2`:
element count must floor at 0 (`max(0, (hi - lo + step - 1) / step)` for
positive step), and a zero-size view must be safe to compare/copy/iterate
(no pointer derivation from garbage). Check the same code path for
`step == 1` empty slices (currently fine — keep it so) and for whatever
negative-step support exists (verify whether the language allows it; if yes,
test the mirrored boundary). Unfence: T5 slice suite re-includes empty
strided ranges (`hi` drawn from `[lo, n]`); add explicit unit tests: empty at
start/middle/end, `step > len`, `lo == hi == n`.

### B3. FB-005 — `Vector.wrap[-n]` off-by-one heap over-read
Fix the wrap index computation to a true Euclidean modulo:
`((idx % n) + n) % n` (or branchless equivalent) wherever Vector's `.wrap`
path computes it (`lib/Vector.fx` or runtime — locate). Audit the *same
formula* for plain arrays and strings once B1 makes `.wrap` reachable there —
the bug pattern may be copy-pasted. Unfence: border suite draws
`idx ∈ [-3n, 3n)` (multiple wraps both directions), boundary values `-n`,
`-2n`, `2n` explicitly included.

### B4. FB-001 — `-c++` backend: interface codegen
Generated C++ rejects fn-ptr → `const void*` array init and the resulting
`fx_init_ifaces` call. Fix in codegen (emit explicit casts, e.g.
`(const void*)` / `(fx_iface_meth_t)` at vtable-init emission) or, if cleaner,
in the runtime table typing — choose the variant that keeps the C build
byte-identical (verify via WP-A diff lens). Flip `cpp_xfail` → expected-pass;
acceptance: `-c++` smoke green on `test_all` (all 12 errors gone), XPASS
reported then fence removed.

### B5. Sanitizer CI leg (FB-005 was silent UB — close the class)
New fxtest mode: build & run `test_all` + T5 suites with
`-cflags "-fsanitize=address,undefined -fno-omit-frame-pointer"` (plus the
matching link flags — check how `-clibs` interacts; verify the runtime builds
clean under both sanitizers and fix trivial findings or registry them).
CI: nightly job (PR job if runtime budget allows). Acceptance: sanitizer leg
green; demonstrate it would have caught FB-005 by temporarily reverting the
B3 fix locally (do not commit the revert).

## 3. WP-C — FB-002: constant folder vs unsigned 64-bit + the new oracle

### C1. Representation (decision made by Vadim)
Extend the folder's integer constant with an unsignedness flag:
`ConstInt: (int64, bool)` where the bool is true **only for uint64** (all
narrower unsigned values are exactly representable in int64; keep them
signed-flagged). Classification: `KLitUInt(64, x)` → `(x, true)`; everything
else → `(v, false)`. Propagation: result flag = per-op rule below.

### C2. Type-aware folds — make the folder match runtime semantics
In `cfold_bop`/`cfold_uop` (`compiler/K_cfold_dealias.fx`), for operands with
the uint64 flag:
- `OpShiftRight` → logical shift (mask after shift or reinterpret via uint64);
- `OpDiv`, `OpMod` → unsigned division/modulo;
- `OpCmp(...)` → unsigned comparison;
- unary negation, `OpSub` overflow wrap, conversions to float/string, `**` if
  foldable — AUDIT each `ConstInt` arm and ask "does signed int64 arithmetic
  give a different bit-result for uint64 ≥ 2^63?"; fix where yes.
- The truncation helper (`mk_int_lit`, ~L103–105) already masks for b<64 —
  verify it keeps doing the right thing for the b=64 unsigned case post-fix.
The reference for "correct" is the **runtime path** (C codegen on `uint64_t`),
which is believed correct — C3 encodes that belief as a test.

### C3. NEW ORACLE: compile-time evaluation == runtime evaluation
Add `fxtest.py cfold`: a seeded Python generator (splitmix64, same seed
policy as T5) emits a `.fx` program with N (default ≥ 500) random constant
expressions over {int, uint8/16/32/64, int8/16/32} × {+ - * / % << >> & | ^
comparisons, mixed casts}, avoiding UB-in-Ficus cases only if the language
defines them as errors (division by zero: generate nonzero divisors; shift
counts within [0, width) unless the language defines otherwise — check the
tutorial, note the decision in the generator).
Each expression is evaluated twice in the emitted program:
  (a) as a literal expression (fold-eligible),
  (b) with operands routed through opacity (module-level `var` array filled at
      startup) so the folder cannot see them,
and asserted bit-equal. Run at `-O0` and `-O3`.
Verify opacity once empirically: `-pr-k` dump of a sample must show the op
surviving un-folded on path (b); document the check in tools/fxtest/README.
Acceptance: pre-fix, the suite FAILS on uint64 `>>`/`/`/`%`/cmp cases
(demonstrates sensitivity); post-fix, green at both opt levels.

### C4. Spec + de-workaround
- Add a short "Integer semantics" note to `doc/ficustut.md`: `>>` is
  arithmetic for signed types, logical for unsigned; integer division
  truncates toward zero (verify!); unsigned compare/div/mod are unsigned.
  One paragraph, placed with the operators section.
- Replace the `lsr()` workaround in `test/rand/RandUtil.fx` with plain `>>`
  (the splitmix64 reference vectors then guard the fix forever).
- OPTIONAL (stretch, separate commit): reimplement stdlib RNG's splitmix64 in
  pure Ficus instead of `@ccode` — output must stay bit-identical (reference
  vectors). Skip if anything is unclear; note in report.

## 4. WP-D — FB-006: nested comprehension (array-of-arrays) codegen

Symptom recap: `[for i {[for j {..}]}]` builds, but *indexing* the result
emits `fx_copy_arr(&{0}, dstptr)` — the inner comprehension's result variable
is lost and a zero-initializer literal is wired in its place; `error:
expected expression` at C compile time, and semantically the element copy
would source an empty array.

1. Localize: is the wrong wiring introduced in K-form lowering of the
   comprehension body (`K_form.fx` / related passes) or at C emission
   (`C_gen_code.fx` array-copy path)? Use `-pr-k0`/`-pr-k` on the 4-line
   repro — if the K-form already references a bogus atom, it's the former.
2. Fix minimally; the WP-A diff lens must show changes confined to
   comprehension/copy codegen.
3. Tests: unit tests for nested comp indexing (2-level int, 2-level tuple
   elements, 3-level), `'t [] []` built by comprehension vs literal vs list
   comprehension `[:: ...]` (all three must agree element-wise); a T4 IR
   snapshot for the 2-level case; T5: extend the array suite with a small
   randomized nested-comp vs reference-loop check.
4. Registry: unfence FB-006 (currently "not yet fenced by a test" — the new
   tests ARE the fence; flip its status accordingly).

## 5. Final phase P5 — closure

- Full ladder green: `fxtest.py all` + `determinism` + `cfold` + sanitizer
  leg, on CI matrix.
- `docs/found_bugs.md`: every FB-001..006, 008 entry has a `fixed:` status
  line; FB-007 remains fenced with a pointer to the pending design session.
- Quarantine/xfail count in manifests: only FB-007-related fences remain.
- Bootstrap regenerated (second designated point), own commit.
- Final report: per-WP summary; the WP-A pre/post churn evidence; cfold
  oracle sensitivity demonstration; sanitizer findings if any beyond FB-005;
  a "lessons for CLAUDE.md" section — every Ficus-language stumble made
  during this brief, in the format agreed for the language cheat-sheet.

## 6. Acceptance criteria (whole brief)

1. All of: determinism tests, cfold oracle, sanitizer leg, `-c++` smoke,
   border/slice/nested-comp suites — green; zero XPASS surprises left.
2. Generated-C stability property holds: the canned unrelated-change scenario
   produces zero diff outside the touched module.
3. No modifications to `Ast_typecheck.fx` / overload resolution.
4. Every registry entry updated; every route-around removed or justified.
5. Bootstrap diffs reviewed: exactly two regeneration commits, each explained.
