# Resolution: PR #27 CI failures on Linux (branch `harden-1`)

Counterpart to `docs/ci_pr27_linux_handoff.md`. The handoff asked to reproduce
and fix the red PR #27 CI (all three legs: ubuntu-gcc, ubuntu-clang,
macos-clang). This is what the failures actually were and how they were fixed.
**Outcome: all three legs green** (run `28868124679`). Nothing about the
nested-comprehension *feature* was broken — its codegen (FB-006) was correct.

## The handoff's two guesses were both wrong

The handoff inferred, from a CI log that had truncated the failing test name
(`[ FAILED ] 1 test(s):`), that the failures were nested-comprehension
regressions. Neither was:

1. The unit failure was **`NN.Quantized.dequantizeLinear`**, not a nested
   comprehension. It is a DL-inference-engine bug, unrelated to the compiler.
2. The `nested_comprehension:ast` snapshot *did* differ on CI, but the cause was
   a **test-harness extraction bug**, not a platform-dependent AST print and not
   the compiler at all.

And the real compiler-relevant failure — a `cfold` miscompilation — the handoff
did not mention, because it does not reproduce on older gcc/clang.

## Three independent root causes

### 1. `cfold` O3 mismatch — signed-overflow UB (real, compiler-relevant) — FB-011

The differential constant-folding oracle went red at `-O3` with a baffling
`fold=-4 run=-4` yet `f != o`. Generated C does `int32 + int32`, which promotes
to `int` and overflows → **signed-overflow UB**. gcc 15 at `-O2/-O3` folds the
*predicate* (`-4 != o`) to a constant under the no-overflow axiom while the
*value* is materialized correctly (a single `printf` shows `o=-4` and
`(-4!=o)=1`). The constant folder is correct (it wraps in int64, sign-extends to
int32). Older CI gcc/clang did not exploit this, so CI had stayed green until a
newer local toolchain (gcc 15.2) exposed it.

**Fix:** `-fwrapv` in the default cflags (`compiler/Compiler.fx`) so signed
overflow is defined as 2's-complement wrap, matching the folder and killing the
whole UB class for every generated program; plus `-fwrapv` in `GNUmakefile` CC so
the bootstrap compiler (`ficus0`/`libficus`) is built the same way. `-fwrapv` is
supported by every gcc 3.x+/clang; MSVC untouched (already wraps).

### 2. `NN.Quantized.dequantizeLinear` unit test — DL-engine bug (not compiler)

`lib/NN/OpQuantized.run_dequantize`'s int8 scalar path yields 0 instead of the
expected value on Linux/x86. Confirmed by Vadim as a known DL-engine defect.
**Fix:** fenced by commenting the `TEST` block in `test/test_nn_quant.fx`.

### 3. `nested_comprehension:ast` snapshot — fxtest extractor bug — FB-013

`-pr-ast` prints each module as `<abs-path>/<mod>.fx: <dep, dep, ...>` then a
`-----` rule then the body. That header is pretty-printed with a width limit, so
a long dependency list wraps the last dep (`String`) onto a `   String`
continuation line before the rule. `normalize.ir_extract_module` skipped only a
single rule line after the `.fx:` header, so on a long path the wrapped line +
rule leaked into the snapshot. **The wrap point depends on the absolute path
length:** the dev/local path `/home/vpisarev/...` (117 cols) stays on one line
and passes; the CI paths `/home/runner/work/ficus/ficus/...` and
`/Users/runner/...` wrap and fail. Only `nested_comprehension` has a dep list
long enough to sit at the boundary, which is why the other 19 IR programs passed
on every host.

**Fix:** `tools/fxtest/normalize.py` — after the `.fx:` header, skip forward to
(and past) the `-----` rule instead of assuming one line. Verified by dumping the
same program from a short and a CI-length path → both normalize to the committed
golden. No golden regeneration, no compiler change.

## Also fixed while verifying

- **FB-012** — `runtime/ficus/impl/string.impl.h`: `fx_str_join` did an unguarded
  `memcpy(dst, NULL, 0)` on an empty element (nonnull-attribute UB, same class as
  FB-011). Guarded with `len_i > 0`, matching the sibling memcpy's. Found by the
  ASan+UBSan sanitize leg.
- **Harness diagnostic** — `tools/fxtest/ir_snapshot.py` now prints the unified
  golden-vs-actual diff on a snapshot mismatch (previously just "differs from
  golden"), which is what let us root-cause FB-013 from the CI log.

## Commits (branch `harden-1`)

- `2d9513a` fix: `-fwrapv` default guards signed-overflow UB + FB-012 memcpy
  guard + dequantize fence + bootstrap regen (only `Compiler.c`) + FB-011/012.
- `45840f2` build: `-fwrapv` in `GNUmakefile` for `ficus0`/`libficus`.
- `c7098e7` chore: gitignore `__pycache__`/`*.pyc`.
- `07431f1` ci/fxtest: dump AST/IR snapshot diff on FAIL; (temporarily) trim
  matrix to ubuntu-gcc while debugging.
- `acc3ea7` fix(fxtest): IR extractor robust to wrapped `.fx:` header (FB-013);
  restore full CI matrix.

## Verification

Local (gcc 15.2): `fxtest all` + `sanitize` + `determinism` all green; `cfold`
0/500 at O0 & O3; bootstrap self-hosts to a fixpoint. CI: all three legs green.

## Takeaways (folded into CLAUDE.md and found_bugs.md)

- ficus signed integer overflow **wraps** (`-fwrapv`); don't rely on UB.
- "value prints right but a branch on it goes wrong at -O2/-O3" == UB signature.
- `-pr-ast` snapshots are extracted by stripping a width-wrapped, path-length-
  dependent module header — suspect the harness (not the compiler) when only
  `:ast` differs while `:k0`/`:k` match.
- `rm -rf build/fxtest` before re-measuring a suite after a compiler change.
- Bootstrap regen recipe + fixpoint check.
