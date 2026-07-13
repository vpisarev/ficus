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

## FB-020  types (`typ_t`) carry no source location  [OPEN — reform blocker]
- repro: `val x: undefined_type = 5` — the "type is not found" caret lands on
  the `:` / the val, NOT on `undefined_type`; `fun f(y: nosuchtype)` points at
  the `:` before `y`, not the type name. Confirmed on plain names, generic-sig
  names, and compound types (`nosuch list`).
- cause: `typ_t` is structural and stores no `loc_t`; the diagnostic falls back
  to the enclosing binding's loc. `parse_typespec` returns no loc either.
- impact: the `'t → [t]` reform and the type-name renames (`half → fp16`,
  `vector`, `Dynvec.t`) cannot locate type occurrences via AST spans, and type
  diagnostics point at the wrong column. Documented in
  `docs/problematic_spans_log.txt`.
- direction (Vadim): do NOT add a loc to `typ_t` — too fundamental (threads
  through typecheck/K-form/C-gen). Instead, later introduce a wrapper
  `typespec_t { typ: typ_t; loc: loc_t }` and switch `typ_t → typespec_t` ONLY
  where a source position is needed (parser output / type annotations), leaving
  the core structural type untouched. Deferred (reform-prep-1 fixed token spans +
  the function-name/import/cast node spans; types are the remaining hard case,
  resolved by this wrapper when the reform lands).

## FB-021  `.op` (elementwise) error surfaces inside the library  [OPEN — diag quality]
- repro: `[1.0,2.0] .* "str"` reports at `lib/Array.fx:90` (inside the generic
  `__dot_mul__` body, caret on the library's `.*`), with a
  "when instantiating ... at <user>:2:11" note pointing back — the primary
  location is library code, not the user's `.*`.
- cause: `.op` desugars to a generic operator call; the mismatch is caught at
  the instantiation site inside the operator body, whose loc wins.
- impact: confusing diagnostics for a common numeric-code mistake. Relevant to
  the `.op → plain op / @ matmul` reform (which removes `.op`) and to a future
  "report at the call site, not the instantiation site" diagnostic pass.

## FB-022  a failed `fold`-valued `val` cascades ("<name> not found")  [OPEN — recovery gap]
- repro: `val s = fold acc = 0 for x <- [1,2,3] {acc + undefined}` reports the
  body error AND a spurious `s is not found` at a later use of `s`.
- cause: `fold` desugars during parsing into a block (`__fold_result__` etc.),
  so the DefVal that binds `s` no longer has a simple RHS; the diag-1 recovery
  that poisons a failed `val`'s pattern to `TypErr` does not fire on this
  desugared shape, leaving `s` unbound → cascade.
- impact: minor extra diagnostic; the root error is still correct. A diag-1
  follow-up would extend the DefVal recovery to poison the pattern regardless of
  RHS shape.

## FB-023  single-use temp memory read inlined PAST an aliasing store  [FIXED — fold-1; refactored — purity-1]
- **purity-1 update**: the local `movement_unsafe_read` guard described below was
  removed; its `{KExpAt, OpDeref, mutable KExpMem, KExpMap}` set is now the
  `~mut_read_is_impure=true` grade of the shared `pure_kexp` predicate
  (`K_remove_unused.fx`). C-gen's `find_single_use_vals` calls
  `pure_kexp(e1, mut_read_is_impure=true)`; behaviour byte-identical. See FB-027
  and `docs/purity1_report.md`.
- repro (via fold-1 tuple assignment): `(arr[i], arr[j]) = (arr[j], arr[i])`
  desugars to `val __t = (arr[j], arr[i]); arr[i] = __t.0; arr[j] = __t.1`.
  The dealiaser (`K_cfold_dealias`, `mktup_map`) flattens `__t.k` to its
  component atoms `@temp val v = arr[k]`, and C-gen's single-use-temp inlining
  (`find_single_use_vals`) folded each such read directly into its store. That
  MOVED the read of `arr[i]` PAST the store `arr[i] = arr[j]`, so the second
  store saw the mutated element: `arr[i]=arr[j]; arr[j]=arr[i];` → both got
  `arr[j]`. Wrong at O0 and O3; `-pr-k0` was correct, so the hazard was purely
  in the temp-inlining.
- root cause: `find_single_use_vals` used `pure_kexp` (no side effects) as its
  inline criterion, but purity ≠ movement-invariance. A *read of mutable
  memory* (array element, ref-cell deref, mutable-record field) is pure yet
  can change value if moved across an intervening store to an aliasing
  location. `pure_kexp` is deliberately conservative for dead-code *removal*
  (K_remove_unused.fx:89-92 notes this), but inlining *moves* code, a stronger
  requirement. Normal code never exposes it (no read-write-read of one element
  in a straight-line block); a simultaneous swap does exactly that.
- fix (`C_gen_code.fx`): a `movement_unsafe_read` guard excludes such temps
  from `find_single_use_vals` — `KExpAt` (arrays are mutable), `KExpUnary
  OpDeref`, and `KExpMem` on an `is_mutable` base stay materialized (one named
  C temp each, which the C compiler coalesces where legal). The desugar keeps
  its natural temp-flagged tuple val, so the tuple still scalarizes where the
  dealiaser can prove it safe (immutable component atoms). Churn: 35/55
  bootstrap modules regenerate (memory-read temps are common) — all
  behavior-preserving. Perf measured: spectralnorm N=5500 within noise;
  compiler self-parse of fx.fx +0.3% (both < the ±2% budget), confirming the C
  compiler recovers the inlining where it is actually safe.

## (non-compiler) NN.Quantized.dequantizeLinear  [FENCED — DL-engine bug]
`lib/NN/OpQuantized.run_dequantize` int8 scalar path yields 0 on Linux/x86.
Known DL-engine defect, unrelated to the compiler; fenced by commenting the
TEST block in `test/test_nn_quant.fx`.

## FB-024  order-dependent generic-return inference pollution  [OPEN — fenced]
A generic stdlib function whose return type is bound only through a closure
argument (`Vec.map`/`mapi : ('t,int)->'r ... -> 'r vector`, `foldl`) can have its
return-type inference *polluted* by an earlier, unrelated instantiation of a
sibling generic in the same module, so a later call fails to unify the element
type. Minimal repro:
```
import Vector
fun t1(): void { val v: int vector = []; v.push_back(1); v[0] = 2 }
fun t2(): void {
    val last: float vector = []
    val curr = last.mapi(fun(x, j) { if j == 0 {1.f} else {last[j-1] + last[j]} })
    ignore(size(curr))
}
t1(); t2()   // ERROR at t2: "if() expression should have the same type as its
             // branches" (1.f : float vs last[j] : <polluted>)
```
- **Order-dependent**: swapping to `t2(); t1()` type-checks cleanly. So it is not
  a real type error — an earlier generic instantiation leaves a shared type var
  in a state the later inference wrongly reuses (the "inferred/free return steals
  under-constrained sites" class, CLAUDE.md — here *across* definitions).
- **Workaround (used in `test/test_vec.fx` fcvector.binomial)**: annotate the
  result — `val curr: float vector = last.mapi(...)` — which pins the return type
  before the polluted var is consulted. Not fixed (out of scope for newvec-1;
  `map`/`mapi`/`foldl` are themselves temporary until vector comprehensions).

## FB-025  resolver internal error: generic container operator in a large overload context  [OPEN — fenced]
Compiling a generic container operator whose body compares/formats elements
generically — `operator <=> (a: 't vector, b: 't vector): int { ... a[i] <=> b[i] ... }`
— throws an internal error **"the winning overload of '__cmp__' failed to
re-unify at commit"** when it is type-checked in a scope that already has many
`<=>`/`__cmp__` overloads (a second generic container `<=>` — rrbvec's — plus the
compiler's own per-type ones, as happens when `Vector` is auto-imported into the
whole compiler). The element access `a[i]` has a *free* element type `'t`, so the
resolver treats every container `<=>` (incl. the one being defined) as a
candidate and fails to commit a unique winner.
- **Order/context-dependent**, like FB-024: the same operator compiles fine when
  `Vector.fx` is imported by a small program (few `<=>` in scope).
- **Workaround (used for newvec-1 auto-import)**: define `==`/`<=>`/`string`/
  `print` for `vector` in `Builtins.fx` (next to the rrbvec ones) instead of in
  the auto-imported `Vector.fx`. Builtins is compiled first, with few overloads
  in scope, so the generic element compare resolves. Not fixed (resolver work is
  out of scope for newvec-1).

## FB-026  cannot bind a variable in an or-pattern  [OPEN — limitation]
An or-pattern that binds the same variable in each alternative is rejected:
`match dom { | DomainElem(AtomId c) | DomainFast(AtomId c) => c | _ => noid }`
gives "duplicate identifier 'c' in the pattern". Both arms bind `c` to the same
type, so this is a legal and common pattern in ML-family languages. Current
limitation (worked around by splitting into two arms, e.g. in the vector-iteration
read-lock codegen in C_gen_code.fx). Should be fixed — allow a variable bound
identically across all alternatives of an or-pattern.

## FB-027  `ignore(coll[i])` swallows the out-of-range exception  [FIXED — purity-1]
An element read whose result does not escape is dead-code-eliminated together
with its bounds check, so an out-of-bounds/empty access that *should* raise
`OutOfRangeError` silently does nothing:
```
val e: int vector = []
ignore(e[.-1])          // expected OutOfRangeError; actually NO throw
val ea: int [] = []
ignore(ea[.-1])         // same — arrays too
```
`ignore(x)` is the "evaluate for the side effects, discard the value" idiom, and
a bounds check is exactly such a side effect (it can throw), so dropping it is a
soundness surprise, not just a missed diagnostic.
- **Affects every checked container read** — `array`, `vector`, `string`,
  `rrbvec` — because they all lower to `KExpAt`. Independent of opt level (repro
  at both `-O0` and `-O3`).
- **Root cause**: `pure_kexp_` in `compiler/K_remove_unused.fx` does not list
  `KExpAt` among the impure forms (`| _ => {}` leaves `ispure = true`), so an
  element read with an unused result is removed before C-gen ever emits its
  `FX_VEC_CHKIDX`/`FX_CHKIDX`. Note the *separate* `IntrinCheckIdx` /
  `IntrinCheckIdxRange` intrinsics ARE marked impure there — but the plain
  `KExpAt` read path emits its check inline in C-gen, so eliminating the whole
  `KExpAt` at the K level drops the check.
- **Contrast**: an *escaping* store keeps the read live and the check fires —
  `outer_sink = coll[i]` (an outer `var`) throws; `{ var s=0; s = coll[i] }`
  (local, dead) does not. This is why `test/test_vec.fx` fcvector.pushpop_edge
  asserts `back()`-on-empty via a captured `sink`, not `ignore(v.back())`.
- **Fix (purity-1 Phase 1)**: `pure_kexp_` in `K_remove_unused.fx` now lists
  `KExpAt(_, BorderNone, _, _, _) => ispure = false` — a bounds-checked read
  (which emits a throwing `FX_CHKIDX`/`FX_VEC_CHKIDX` in C-gen) is impure and
  survives DCE. Border reads (`.clip`/`.wrap`/`.zero`) never throw and stay pure
  and eliminable (verified: `test/ir/checked_read.k.golden` shows a dead `.clip`
  read removed while the dead checked read is retained). Repro now throws
  `OutOfRangeError` for array/vector/string/rrbvec at both `-O0` and `-O3`
  (`test/test_basic.fx` `basic.checked_read_side_effect`; `test/test_vec.fx`
  `fcvector.pushpop_edge` restored to the natural `ignore(v.back())` spelling).
  Churn: only `K_remove_unused.c` regenerated among the 55 compiler modules;
  generated C for spectralnorm/mandelbrot/btree/nbody byte-identical to master
  (perf within noise); a single test scaffold (`test_matrix.c`) materializes a
  few extra named temps around checked reads it no longer inlines — behavior-
  preserving, coalesced by the C compiler. Phase 2 (purity-1) folds this into a
  parameterized `pure_kexp(~mut_read_is_impure)` so the throw-impurity and the
  FB-023 movement guard share one classification.

## FB-028  loop fusion moves a mutable-memory read past the consumer's stores  [FIXED — fuse-move-1]
Sibling of FB-023, in `K_fuse_loops`. Fusion takes a single-use comprehension
`val tmp = [for i <- A {body(i)}]` consumed by exactly one for-loop
`for x <- tmp {bar(x)}` and REPLAYS the body inside the consumer:
`for i <- A { val x = body(i); bar(x) }`. If `body` reads mutable memory and the
consumer `bar` writes it, the fused reads see values written earlier in the SAME
pass — the unfused meaning reads the original array (tmp is materialized first).
Minimal repro (an in-place 3-tap smoothing filter):
```
val arr = [30, 60, 90, 120, 150]
val n = size(arr)
val smoothed = [for i <- 0:n { (arr.clip[i-1] + arr.clip[i] + arr.clip[i+1]) / 3 }]
var k = 0
for s <- smoothed { arr[k] = s; k += 1 }   // write-back in place
// unfused (O0): [40, 60, 90, 120, 140]; fused (O3): [40, 63, 91, 120, 140]
```
- **Post-purity-1 reachability**: a plain `arr[i]` read is `BorderNone` → impure
  → already blocks fusion; the bug survives only through a NON-throwing mutable
  read — a border read (`.clip`/`.wrap`/`.zero`), `*ref`, or a mutable field.
- **Fix**: the fusion criterion asks `pure_kexp` for its MOVEMENT grade
  (`mut_read_is_impure=true`) — a body that reads mutable memory is not a fusion
  candidate (bodies reading only immutable data still fuse). Conservative (blocks
  even when the consumer provably doesn't alias), but measured zero corpus fusion
  loss for it.
- **Driver fix (bonus)**: while adding a regression test we found fusion **never
  fired inside function or lambda bodies** — `fuse_loops_all` only ran the pass
  on each module's top-level code, and the recursive descent (`fuse_kexp_`) only
  started once the *top-level* gate (`≥1 map`, `≥2 map+for`) passed; a module
  whose top level is only `KDefFun`s (every UTest file, all modular code) got no
  fusion at all. Killed the gate (`fuse_loops` → always `fuse_loops_`) and made
  `fuse_kexp_` process a `KExpSeq` with `fuse_loops_(elist)` directly WITHOUT a
  preceding `walk_kexp` (which also descends → every block would be traversed
  2^depth times; the gate had been masking that). Net: fusion now fires in
  function/lambda bodies too (25 test_all modules newly fuse; benchmark C
  unchanged), compiler front-end +4.5%, and the FB-028 regression is now
  observable from a UTest (`test/test_array.fx` `array.fuse_inplace_stencil`;
  positive companion `array.fuse_map_reduce`). So: trying to DISABLE fusion in
  unsafe places, we ENABLED it in safe ones.
