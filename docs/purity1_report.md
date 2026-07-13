# purity-1 report — FB-027 fix + the effect-facet predicate

Branch `purity-1` (off master `fbb985f`). Two commits:

- **Phase 1** `03775b8` — the precise FB-027 fix (checked reads survive DCE).
- **Phase 2** `c9baefe` — parameterize `pure_kexp` with `~mut_read_is_impure`,
  fold the three "is it pure?" questions into one classification, migrate every
  consumer to declare its grade.

Both green on the full ladder (`fxtest.py all`), `sanitize` (ASan+UBSan),
`determinism`, and the self-hosting fixpoint (`update_compiler.py`).

## The pattern being killed

`pure_kexp` answered one question — "no side effects, safe to REMOVE for DCE" —
but three consumers asked three different things, and the conflation produced two
soundness bugs plus a recurring authoring trap:

| # | question | consumer | what went wrong |
|---|----------|----------|-----------------|
| 1 | no effect to **remove**? | DCE (`K_remove_unused`) | correct baseline, but see FB-027 |
| 2 | safe to **move**? | C-gen temp inlining, loop-invariant hoisting | purity ≠ movement-invariance → **FB-023** (read moved past an aliasing store) |
| 3 | **effect-free** incl. throwing? | DCE of a discarded checked read | purity ≠ effect-freedom → **FB-027** (dead `coll[i]` dropped its bounds check, swallowing `OutOfRangeError`) |

FB-023 was patched with a *local* `movement_unsafe_read` guard in C-gen; FB-027
was open; and every new intrinsic author (newvec-2: push/pop/splice) had to
*remember* to hand-mark impurity or DCE ate their void-result calls.

## Phase 1 — the precise FB-027 fix

A bounds-checked element read lowers to `KExpAt(_, BorderNone, …)` and emits a
throwing `FX_CHKIDX`/`FX_VEC_CHKIDX` **inline** in C-gen. `pure_kexp_` did not
list `KExpAt`, so a read whose result was discarded (`ignore(coll[i])`, a dead
`val`) was eliminated at the K level — *before* C-gen ever emitted the check —
silently swallowing the exception. (The separate `IntrinCheckIdx*` intrinsics
were already marked impure, but the plain `KExpAt` path was not.)

Fix: `pure_kexp_` classifies `KExpAt(_, BorderNone, _, _, _)` as impure. Border
reads (`.clip`/`.wrap`/`.zero`) never throw and stay pure and eliminable — gating
on `BorderNone` keeps legitimate DCE intact.

**Tests**
- `test/test_basic.fx` `basic.checked_read_side_effect` — `ignore(e[0])` and
  `ignore(e[.-1])` on an empty **array / vector / string / rrbvec** each throw
  `OutOfRangeError` (T2 corpus runs the suite at both `-O0` and `-O3`); a dead
  `.clip` read does not throw.
- `test/test_vec.fx` `fcvector.pushpop_edge` — restored to the natural
  `ignore(v.back())` spelling; the captured-`sink` workaround the bug forced is
  gone.
- `test/ir/checked_read.*` — IR golden locking the DCE contrast: at `-O3 -pr-k`
  the dead `.clip[100]` read is **removed** while the dead checked `a[7]` read is
  **retained** (`v@14[7]`), the returned `a[0]` stays live.

**Churn / perf (Phase 1 vs master).** Only `K_remove_unused.c` regenerated among
the 55 bootstrap modules (fixpoint holds). Generated C for
`spectralnorm` / `mandelbrot` / `btree` / `nbody` is **byte-identical** to master
(nbody 50M iters 1.53 s → 1.53 s; spectralnorm N=5500 0.86 s → 0.85 s — within
noise, ≪ the ±2 % budget). The only corpus C change is `test_matrix.c`: it
materializes a few extra named temps (`fx_arr_t arr_0…arr_7`) around checked
reads C-gen no longer inlines — behaviour-preserving, coalesced by the C compiler
where legal, and confined to test scaffolding (not the `refmul` hot loop).

## Phase 2 — the parameterized predicate

```
fun pure_kexp(e: kexp_t, ~mut_read_is_impure: bool = true): bool
```

One short-circuiting fold, one `match` that is the single point of truth:

- **Throwing is impure UNCONDITIONALLY** (not the flag): `KExpAt(_, BorderNone,
  …)` and the throwing/mutating intrinsics (`IntrinCheckIdx`,
  `IntrinCheckIdxRange`, `IntrinPopExn`, `IntrinVecPushBack`, `IntrinVecPopBack`,
  `IntrinVecSplice`). Neither removable nor movable. This is Phase 1 baked in.
- **`~mut_read_is_impure`** governs non-throwing reads of mutable memory as a
  class — border-mode `KExpAt`, `KExpUnary(OpDeref, …)`, `KExpMem` on an
  `is_mutable` base, and `KExpMap` (a comprehension is never safe to move):
  `false` = removable (a dead read has no effect — DCE keeps today's power),
  `true` = not movable (FB-023). Default `true` = the safe, movement-grade
  reading for any caller that does not think.

The FB-023 `movement_unsafe_read` helper in `C_gen_code.fx` is **deleted**: its
`{KExpAt, OpDeref, mutable KExpMem, KExpMap}` set is exactly the `true`-grade of
the one predicate, so `pure_kexp(e1, mut_read_is_impure=true)` replaces
`pure_kexp(e1) && !movement_unsafe_read(e1)` verbatim.

### Consumer audit (every `pure_kexp` call site)

| call site | grade | intent | rationale |
|-----------|-------|--------|-----------|
| `K_remove_unused` `pure_fun` (body purity cache) | `false` | removal | a function whose unused call we might delete is "pure" if its body has no effect to remove; a mutable read has none. Throwing stays impure (unconditional). |
| `K_remove_unused` unused-`KDefVal` (`if !pure_kexp(e)`) | `false` | removal | the canonical DCE test — keep a dead binding's RHS only if impure. |
| `K_remove_unused` mid-sequence (`pure_kexp(e)` in `remove_unused_`) | `false` | removal | drop a pure statement in the middle of a block. |
| `C_gen_code` `find_single_use_vals` | `true` | movement | single-use temp inlining MOVES the initializer to its use site — FB-023. Replaces the deleted `movement_unsafe_read`. |
| `K_loop_inv` `is_loop_invariant` | `true` | movement | hoisting out of a loop is a MOVE; replaces the explicit `KExpAt => false` guard (now subsumed — every mutable read is non-movable). |
| `K_fuse_loops` fusion candidate | `false` | removal (preserved) | **see below** — audit observation, behaviour left unchanged. |

**Byte-identity.** Phase 2 is expression, not policy: generated C is
**byte-identical to Phase 1 across the whole corpus** — `test_all` (including
`test_matrix`), `spectralnorm`, `nbody`, `mandelbrot`, `btree` all diff clean.
The bootstrap regenerates only the four edited modules (`K_remove_unused`,
`C_gen_code`, `K_loop_inv`, `K_fuse_loops`); fixpoint holds. Notably K_loop_inv's
migration from `pure_kexp && (KExpAt => false)` to `pure_kexp(…, true)` — which
on paper *also* excludes `OpDeref`/mutable-`KExpMem`/`KExpMap` from hoisting —
changed no corpus output, i.e. no such non-KExpAt read was being hoisted in
practice.

## STOP-rule item — `K_fuse_loops` and movement grade

Per the brief: *if Phase 2's audit finds a consumer whose migration would CHANGE
optimization behaviour — report, don't decide.* `K_fuse_loops` is that consumer.

`K_fuse_loops` fuses a single-use comprehension `arr = [for … {body}]` into the
one `for`-loop that consumes it (guarded by `arr_nused == 1 && arr_nused_for ==
1`). It gates on `pure_kexp(body)`. Fusion *replays* the body inside the
consumer loop, so it is arguably a **move** of the body's reads — which would
argue for `mut_read_is_impure=true`. But:

1. It has always used the removal-grade reading (bare `pure_kexp`), so switching
   to `true` would make fewer comprehensions fusable — a real optimization
   change, not a bug fix.
2. Fusion's safety today rests on the single-use / single-consumer counters, not
   on `pure_kexp`'s movement grade; whether those counters fully cover the
   read-past-an-aliasing-store hazard is a separate question this brief did not
   scope.

So Phase 2 passes `mut_read_is_impure=false` to **preserve** existing behaviour
(byte-identity confirmed) and flags the movement question here rather than
silently changing fusion. Recommended follow-up: a focused audit of whether a
fused comprehension body can read memory the consumer loop writes earlier in the
same iteration; if so, that is a fusion-specific FB in the FB-023 family and
should be fixed in `K_fuse_loops` (grade flip + a repro), not folded in here.

## Registry

`docs/found_bugs.md` FB-027 updated `OPEN → FIXED — purity-1`, with the fix, the
tests, and the churn note. FB-023 remains `FIXED`; its C-gen local guard is now
expressed through the shared predicate (no behaviour change).
