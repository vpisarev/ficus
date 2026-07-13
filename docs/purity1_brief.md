# Brief: purity-1 — FB-027 + the effect-facet predicate (Opus)

**You are Claude Code (Opus) working in the Ficus repo.** Branch `purity-1`
off master. Context: `docs/found_bugs.md` FB-027 (the bug) and FB-023 (its
sibling — read the pair together), `docs/newvec2_report.md` (§4/5: the new
push/pop intrinsics had to be HAND-marked impure — the third reminder this
class needs an API), repo CLAUDE.md.

## The pattern being killed

`pure_kexp` answers one question ("no side effects for dead-code REMOVAL")
but has been consumed for three different ones, producing two soundness bugs:
- FB-023: purity ≠ movement-invariance (a pure read of mutable memory changes
  value when moved past an aliasing store) — fixed by a local
  `movement_unsafe_read` guard in C_gen;
- FB-027: purity ≠ effect-freedom (a bounds-checked read can THROW; removing
  it swallows `OutOfRangeError`) — open, this brief fixes it;
- newvec-2: every new intrinsic author must REMEMBER to mark impure or DCE
  eats their calls.

## Phase 1 — the precise FB-027 fix

Per the registry's fix direction: in `K_remove_unused.fx` `pure_kexp_`, a
`KExpAt` with `BorderNone` (the throwing-check read path) is IMPURE; border
reads (`.clip/.wrap/.zero` — never throw) stay pure and eliminable.

Tests (`test/test_vec.fx` + a small `basic.` case): `ignore(e[.-1])` /
`ignore(e[0])` on an EMPTY container throws `OutOfRangeError` for all four —
array, vector, string, rrbvec — at **-O0 and -O3**; a dead `.clip` read still
disappears from `-pr-k` (lock the non-regression of legitimate DCE with an IR
golden); `test_vec.fx` `pushpop_edge` switches back to the natural
`ignore(v.back())` spelling — the in-tree proof. Measure: bootstrap churn
(expected: some — dead checked reads stop being removed) and the perf budget
(±2% on spectralnorm + compiler self-parse, the FB-023 methodology).

## Phase 2 — the parameterized predicate (so there is no FB-0XX member three)

Design (Vadim): keep the single short-circuiting fold — do NOT build an
all-facets record collector (no consumer needs all facets; the lazy boolean
query preserves the first-impure-early-exit). Instead, parameterize what
counts:

```
fun pure_kexp(e: kexp_t, ~mut_read_is_impure: bool = true): bool
```

Semantics:
- **Throwing is impure UNCONDITIONALLY** (not a flag): checked `BorderNone`
  `KExpAt`, throwing intrinsics — cannot be removed (swallows the exception,
  FB-027) and cannot be moved (reorders throws). This is Phase 1 baked in.
- `~mut_read_is_impure` governs NON-throwing reads of mutable memory as a
  class — border-mode `KExpAt`, `OpDeref`, `KExpMem` on an `is_mutable`
  base: `false` = removable (a dead read has no effect — DCE keeps today's
  power), `true` = not movable (FB-023). Default `true` = the safe,
  movement-grade interpretation for any caller that doesn't think.

The classification stays in ONE match inside `pure_kexp` (the single point
of truth this phase exists for). Re-express the consumers:
- DCE (`K_remove_unused`): explicit `~mut_read_is_impure=false`;
- C_gen `find_single_use_vals`' FB-023 `movement_unsafe_read` guard: the
  default-true call replaces the local guard;
- the hand-marked intrinsics (`__intrin_push__`/`__intrin_pop__`,
  `IntrinCheckIdx*`, `IntrinVecSplice`, ...): classified in the one match,
  ad-hoc marks removed.
Audit OTHER `pure_kexp` call sites (grep) and give each an EXPLICIT argument
declaring its intent; anything ambiguous — leave as-is and LIST it in the
report (no speculative rewrites of optimization passes).

Acceptance: Phase-1 tests still green; bootstrap fixpoint; the full ladder +
sanitize; behavior-identical to Phase 1 (the refactor is expression, not
policy — generated C byte-identical between Phase-1 and Phase-2 commits
except modules whose source changed).

## Ground rules
House standard: ladder per phase, `update_compiler.py` fixpoint,
`-pr-resolve` census unchanged, found_bugs for discoveries, don't push. STOP:
if Phase 2's audit finds a consumer whose migration would CHANGE optimization
behavior — report, don't decide. Report `docs/purity1_report.md` with the
consumer-audit table and the FB-027 → fixed registry update.
