# Found bugs registry

Append-only registry of compiler bugs discovered by the fxtest infrastructure.
The rule (Brief #1): **find and fence bugs, do not fix them** (in test/instr
sessions); fixes land in dedicated hardening sessions and are recorded here.
Entry template for new bugs:

```
## FB-NNN  <pass or area>: <one-line symptom>
- repro: <path to test file or inline snippet>
- config: <opt level, platform/compiler>; <expected> vs <got>
- status: fenced in <manifest entry / suite> as <xfail | crash | routed-around>
```

NOTE 2026-07-09: fixed entries below are condensed to symptom / root cause /
fix / tests. The full forensic write-ups live in this file's git history.

---

## FB-001  C++ backend: interface codegen fails under `-c++`  [FIXED harden-1]
Programs with `interface` + implementing `class` compiled as C but not as C++
(fn-ptr→`void*` needs explicit casts in C++). Fix: `C_gen_fdecls.fx` emits
`(void*)` casts in the vtbl initializer and the `fx_init_ifaces` argument.
Caught by the `-c++` corpus smoke; unfenced (cpp_xfail removed), smoke green.

## FB-002  uint64 `>>` folded as arithmetic shift  [FIXED harden-1]
Only the CONSTANT FOLDER was wrong (runtime `>>` was correct): `ConstInt`
carried plain int64, so uint64 shifts/div/mod/cmp folded signed. Fix:
`K_cfold_dealias.fx` `ConstInt: (int64, bool)` with the flag true only for
full-width uint64; unsigned semantics in the affected ops and in
uint64→float/string finalization. Invisible to the O0/O3 differential (same
wrong result both ways) — caught by T5 reference vectors; locked by the
`fxtest cfold` oracle (compile-time == runtime over random expressions) and
the splitmix64 suite (its `lsr()` workaround removed).

## FB-003  1D border access `.clip/.wrap/.zero` emitted broken C  [FIXED harden-1]
`runtime/ficus/ficus.h` 1D border macros used undeclared `__idx0__` and closed
statement-expressions with `))`; string `.wrap` was declared but never
registered (`C_gen_std.fx`). Fixed all three; locked by `array.border_matrix`
unit test ({1D,2D,Vector,string} × {clip,zero,wrap}) and the T5 border suite.

## FB-004  empty strided slice → corrupt view → SIGSEGV  [FIXED harden-1]
`fx_subarr` copy path ran its do/while at least once even for 0 elements,
deriving a null slice pointer. Fix: early `return FX_OK` when `total == 0`
(`runtime/ficus/impl/array.impl.h`). Locked by `array.empty_slice` + the T5
slice suite now including empty strided slices.

## FB-005  `.wrap[-n]` off-by-one heap over-read  [FIXED harden-1]
Wrap index `(idx % n) + (idx<0 ? n : 0)` yields `n` at `idx == -n`. Fix: true
Euclidean modulo `((idx % n) + n) % n` in `fx_rrb_find_border`,
`FX_PTR_1D_WRAP`/`FX_WRAP_IDX`, `fx_str_elem_wrap`. Silent UB (adjacent heap
usually reads 0; ASan does NOT reliably trap it) — caught only by the
reference-checked T5 border suite, which now forces the ±n boundaries.

## FB-006  nested comprehension (array-of-arrays) → broken C  [FIXED harden-1]
`C_gen_code.fx`: a value-producing inner comprehension returns its result via
`dstexp_r` (yielding a dummy `{0}`), so the outer map copied from `{0}`. Fix:
read back a named `body_dst_r`. Locked by `array.nested_comprehension`, the
T5 `rand.array.nested_comp`, and a T4 IR snapshot.

## FB-007  generic `complex` operators broke resolution/inference  [FIXED resolve-1/2]
The 2021-fenced umbrella bug (Complex.fx operators + fst.fx demo commented
out). Decomposed into three symptoms; all closed:
- **S2 (wrong candidate)**: `c * 2` resolved to the ARRAY `__mul__` — greedy
  first-match, minimal form split out as FB-016; fixed by the resolve-1
  collect→rank→commit resolver.
- **S3 (inference pollution)**: an over-general `+('t, 't complex)` captured
  free-typed accumulators (`nnop_t list` inferred as `... complex` at
  NN/FromOnnx.fx). Trial half fixed by resolve-1 sandboxed trials; commit
  half fixed by resolve-2 `TypVarCollection` (`[]` = "some collection", so
  the candidate dies at the viability trial). Self-contained test:
  `fb007.s3_free_accumulator` in `test/test_resolve.fx`.
- **S1 (`int + 't` pinned early)**: turned out already dead post-resolve-1
  (generic bodies re-resolve per instantiation); the residual failures were
  (a) imaginary literals `N.fi` desugaring with an **int** zero real part —
  never worked since 2021, fixed in `Lexer.fx` — and (b) missing return
  annotations, see the near-miss below.
Complex.fx is fully mixed-type now (`'t1 op 't2`, inferred result via builtin
coercion; `+`/`-` pass-through component nudged by the `r - r` pattern,
provably folded away at -O0/-O3). `val c = ref (1 + 1.fi)` — the verbatim
2021 demo — runs in fst.fx (in the fxtest corpus).
**NEAR-MISS (hotfix-1)**: dropping the return annotations from the mixed
`+`/`-` variants broke the compiler's own build — a generic operator's return
annotation is a load-bearing VIABILITY FILTER (a free return unifies with any
expected type → the candidate invades under-constrained sites; broke at
C_gen_code.fx:1328, cstmt_t-list accumulator). Fix: fresh-var returns
(`: 't3 complex`) on all mixed variants; institutionalized as annotate-1
(all 106 generic operators annotated, `tools/lint_op_returns.py` on CI) and
annotate-2 (§ see report). Caught by the CI bootstrap `--check` — the only
routine self-build of the compiler.

## FB-008  "non-deterministic" `.c` under unrelated changes  [FIXED harden-1]
Long-standing annoyance. Established: full builds were ALWAYS deterministic;
FB-008 was an **incremental-rebuild** artifact. Two causes, both fixed:
unstable parameter names in extern prototypes (fix: omit param names in
`C_pp.fx` fwd_mode) and `k_skip_some` blanking skipped modules' bodies before
K_inline, changing inlining opportunities (fix: keep real bodies;
`Compiler.fx`). Locked by `fxtest.py determinism` (clean-build byte-identity
+ incremental unrelated-change stability). The earlier "CONFIRMED M1/M2"
analysis was itself measured through confounded incremental dirs — the trap
that later became WP-H1's `.fxstamp`.

## FB-009  pointer-arithmetic UB on negative-step slices  [FIXED harden-1]
`fx_next_slice` advanced `s->ptr += s->step` with `size_t` step holding a
negative byte offset. Fix: `(ptrdiff_t)` casts at both advance sites. Found
by the ASan+UBSan `sanitize` leg (which FB-005 showed is NOT sufficient for
over-reads into valid heap — the two legs are complementary).

## FB-010  folded INT64_MIN emitted as bare (unsigned) C literal  [FIXED housekeeping-1]
`-9223372036854775808` parses in C as minus on an out-of-range (hence
unsigned) constant; a bare `LL` suffix does not help. Fix: `klit2str` emits
the signed split form `(-9223372036854775807LL - 1)`; blanket-suffixing was
measured and rejected (churns everything, still warns). Locked by
`basic.int64_min` and the cfold oracle generating the full signed range.

## FB-011  signed-overflow UB miscompiled by gcc 15 at -O2/-O3  [FIXED harden-1/PR-27]
Generated `int32 + int32` overflow is UB; gcc 15 folds the *predicate* under
the no-overflow axiom while the *value* materializes correctly (prints -4,
`if (x != -4)` taken). The folder was correct (wraps). Fix: `-fwrapv` in
default cflags (`Compiler.fx`) and `GNUmakefile` — signed overflow is now
DEFINED as 2's-complement wrap, language-wide (language_changes §1.1).

## FB-012  `fx_str_join`: memcpy(dst, NULL, 0) nonnull UB  [FIXED harden-1]
Unguarded memcpy for an empty joined element. Fix: `if (len_i > 0)` guard
(`string.impl.h`), matching the sibling memcpys. Found by the sanitize leg.

## FB-013  fxtest IR extractor: wrapped module header leaked into snapshot  [FIXED harden-1]
`-pr-ast` module headers width-wrap on long repo paths (CI), and the
extractor assumed one line — goldens passed locally, failed on CI with no
compiler change. Fix: `normalize.py` skips to the `-----` rule. Moral (in
CLAUDE.md): when only `:ast` differs and `:k0`/`:k` match, suspect the
harness.

## FB-014  bare positive literal 2^63 silently wrapped to INT64_MIN  [FIXED housekeeping-1]
Lexer accepted magnitude 2^63 unconditionally (so that `-9223372036854775808`
works). Fix (Vadim): `expect_neg_number` flag threaded around number lexing —
bare positive 2^63 errors, immediately-negated form still denotes INT64_MIN,
nested/parenthesized minus handled by parity, `-` before unsigned literal is
a clear error. Goldens 007/008.

## FB-015  `continue`/`break`/bare `return` in expression position  [FIXED — ctrlflow-1]
Two symptoms, both once misreporting as `unexpected token '}'`:
1. `continue`/`break` as the *value* of a `match` arm / `if` branch
   (`| _ => continue`) — this was a TYPECHECK issue, not a parse one: the arm
   parsed but `ExpBreak`/`ExpContinue`/`ExpReturn` had pseudo-type `TypVoid`
   and would not unify with a value-producing sibling.
2. a bare `return` (no value) before `}` or `|` on the same line — a LEXER
   issue: the `RETURN(has_arg)` classifier treated any same-line non-`;` char
   as "has an argument"; `return\n}` and `return;` already worked.
Fix (additive, accepts strictly more programs), mirroring `throw`:
- **Ast.fx `get_exp_ctx`**: `ExpBreak`/`ExpContinue`/`ExpReturn` now carry
  `TypErr` (like `ExpThrow`), which unifies with any type without binding it —
  so a jumping arm/branch sits beside valued siblings and the whole match/if
  takes the siblings' type.
- **Lexer.fx**: a `return` whose next significant char is `}` `)` `]` `,` `|`
  (a token that cannot begin an expression) is bare.
Zero backend change: K-normalization still lowers all three to void K-nodes,
so generated C is byte-identical for existing programs (bootstrap regen touched
only `Ast.c`+`Lexer.c`; `-pr-resolve` census 351/344/7/0 unchanged). Legality
(`check_inside_for`) and the `@parallel`/`fold` restrictions are unchanged and
fire identically in statement and expression position. This unblocks the
imperative-fold reform (fold bodies where `| _ => continue` is the natural
idiom). Directed suite `test/test_ctrlflow.fx` + negatives 216–219; leak oracle
(1000×1000 locals live at the jump) clean under ASan `detect_leaks` at O0/O3.
See `docs/ctrlflow1_report.md`.

## FB-016  greedy first-match resolution (last-declared overload wins)  [FIXED resolve-1]
Minimal form of FB-007/S2: `fun f(x:int)=x+1; fun f(x:'t)=x; f(5)` returned 5
or 6 depending on declaration order. Fixed by collect→rank→commit in
`lookup_id_opt` (least-generic viable wins; determined ties = ambiguity
error, goldens 012–014; under-constrained ties = env-order fallback, 7
harmless sites in the compiler, `-pr-resolve` counter is the tripwire).
Corpus-invariance proof: bootstrap regen through the new resolver changed
only the edited `Ast_typecheck.c`. Locked by
`FB016_fixed.concrete_beats_generic` + `test/ir/overload_resolve` goldens +
the E1 census pair (`docs/wpe_experiments/`).

## FB-017  generality comparator blind to `{...}` var-form generics  [FIXED resolve-1]
`maybe_unify` binds record-vars symmetrically, so auto-generated record
generics (`__eq__(a:{...},b:{...})`) read as EqGeneric vs a concrete record —
224 of the 227 pre-fix census `<none>`s. Fix: `freeze_varform_typs`
skolemizes the `TypVar*` family (Record/Tuple/uniform-Tuple/Array) on the
rigid side of each trial; `maybe_unify` untouched. +14 comparator verdicts;
census: 342/341/1/0 (the 1 = the known under-constrained 21-viable
`__cmp__` site).

## FB-018  `Array.diag(d: 't[])`: `size(a)` used before `a` is defined  [FIXED — ctrlflow-1 ride-along]
Was `size(a)` where `a` is the not-yet-defined result; corrected to `size(d)`
(`lib/Array.fx:346`). The array form was unusable but unnoticed because nothing
instantiates it (generic bodies typecheck at instantiation) — exposed by the
annotate-2 sweep. Locked by `matrix.diag_from_vector` (`test/test_matrix.fx`).

## FB-019  `floor`/`ceil`/`trunc`/`round` return `int`  [RESOLVED — by design]
Vadim (2026-07-09): intentional (OpenCV `cvFloor`/`cvRound` semantics —
"floor = index" is the dominant use in numeric code), a feature, not a bug.
CLAUDE.md and (at rewrite time) the tutorial state it plainly; the annotate-2
`int [+]` array annotations stand.

## (non-compiler) NN.Quantized.dequantizeLinear  [FENCED — DL-engine bug]
`lib/NN/OpQuantized.run_dequantize` int8 scalar path yields 0 on Linux/x86.
Known DL-engine defect, unrelated to the compiler; fenced by commenting the
TEST block in `test/test_nn_quant.fx`.
