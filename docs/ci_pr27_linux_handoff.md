# Handoff: PR #27 CI failures — reproduce & fix on Linux

**You are Claude Code running on Linux.** Branch `harden-1` (Brief #2 hardening).
PR #27 CI is red on all three matrix legs (ubuntu-gcc, ubuntu-clang,
macos-clang). The failures are **Linux/CI-specific** — they do NOT reproduce on
the author's macOS dev machine, even from a clean `git worktree` build. Your job
is to reproduce them on Linux, root-cause, fix, and (if needed) regenerate the
affected golden.

Two independent failures, both a regression from **WP-D (FB-006, nested
comprehensions / array-of-arrays)** — see commit `6a9ae58`:

```
=== unit (test_all.fx) ===
  FAIL   test_all   [  FAILED  ] 1 test(s):
=== T4 IR snapshots ===
  FAIL   nested_comprehension:ast   ast snapshot differs from golden
  PASS   nested_comprehension:k0    (matches)
  PASS   nested_comprehension:k     (matches)
```
Everything else (negative 64, cfold 2, corpus 11, the other 19 IR programs)
passes on all three legs.

## Ground rules (carry over from the brief)

- **Do NOT push** and do NOT touch `Ast_typecheck.fx` / overload resolution.
- Fixes go in `compiler/` / `runtime/`; this is sanctioned bug-fixing (not the
  usual find-and-fence). Record anything you fence in `docs/found_bugs.md`.
- Sanity loop before/after: `make -j$(nproc) && bin/ficus -run test/test_all.fx
  && python3 tools/fxtest/fxtest.py all`.
- Keep the generated-C stable: after a compiler fix, `python3
  tools/fxtest/fxtest.py determinism --unrelated-change` must stay green, and a
  bootstrap regen should touch only the module you edited.

## Failure 1 — a unit test fails at RUNTIME on Linux

`test_all.fx` COMPILES on Linux (the artifact has a built `test_all` binary), so
this is a **runtime assertion failure**, i.e. a wrong value — almost certainly a
**heap-layout-dependent memory bug** (the FB-005 class: correct on the dev
machine's allocations, wrong on Linux's).

The suspect is the FB-006 fix in **`compiler/C_gen_code.fx`** (search
`FB-006` — around the `KExpMap` body handling, ~line 2868). The fix made the
outer comprehension read the inner comprehension's result back from `dstexp_r`
instead of the dummy `{0}`:

```
val body_dst_r = ref None
val (result0, body_ccode) = kexp2cexp(body, body_dst_r, body_ccode)
val result = match *body_dst_r { | Some r => r | _ => result0 }
```

The two tests that exercise it (in `test/test_array.fx` and
`test/rand/test_rand_array.fx`):
- `array.nested_comprehension` — fixed `EXPECT_EQ`s (2-level, 3-level, tuple
  elements, comprehension-vs-literal-vs-list agreement).
- `rand.array.nested_comp` — random m×n, checks `aa[r][c] == flat[r*n+c]`.

### Already ruled out (don't re-chase these)

- The CI compiler is byte-identical to the dev one: `nested_comprehension.k`
  from the CI build artifact matched the dev machine's exactly.
- `fx_copy_arr` (runtime/ficus/impl/array.impl.h) is a *shallow* copy +
  `FX_INCREF` (`*dst = *src`), it does NOT free `dst`; `fx_make_arr` memsets the
  data to 0 (line ~346), so the "copy into uninitialised slot" theory is wrong.
  The refcounting itself looks platform-independent — so the bug is subtler.

### Plan

1. `make -j$(nproc)` then `bin/ficus -run test/test_all.fx` — note **which**
   test fails and the expected-vs-got values. If it's `rand.array.nested_comp`,
   the printed `[repro] ... seed=... m=... n=...` line reproduces it directly.
2. Minimise: e.g.
   ```
   val aa = [for i <- 0:3 {[for j <- 0:3 {i*10 + j}]}]
   println(f"{aa[0][0]} {aa[1][2]} {aa[2][1]}")   // want 0 12 21
   ```
   Run at -O0 AND -O3 (the bug may be opt-level-specific).
3. Under sanitizers (Linux ASan has leak detection):
   ```
   mkdir -p /tmp/nc
   ASAN_OPTIONS=detect_leaks=1 bin/ficus -run -O1 -no-openmp \
     -cflags "-fsanitize=address,undefined -fno-omit-frame-pointer" \
     -clibs  "-fsanitize=address,undefined" -B /tmp/nc /tmp/nc.fx
   ```
   or `python3 tools/fxtest/fxtest.py sanitize` (runs test_all under ASan+UBSan).
   The trap should point straight at the bad access.
4. Inspect the generated C for the minimal case: build with
   `bin/ficus -O0 -no-openmp -o nc /tmp/nc.fx` and read
   `__fxbuild__/nc/nc.c` around the nested-comp loop — look at the
   `fx_copy_arr(&<src>, dstptr_…)`, the `FX_FREE_ARR`/`FX_MOVE_ARR`, the incref,
   and the order of the inner-array free vs the copy. A likely real cause: the
   inner temp array is copied into the outer slot but then **freed on a path
   that also owns the slot** (double-free / use-after-free), or the copy should
   be a **move** (`FX_MOVE_ARR`) — the `[TODO]` comment right above the fix in
   `C_gen_code.fx` discusses exactly this move-vs-copy choice for temporaries.
5. Fix minimally in `C_gen_code.fx` (or the copy/move helper in
   `C_gen_types.fx` / `array.impl.h`). Re-run test_all + `fxtest sanitize` + the
   determinism check.

## Failure 2 — `nested_comprehension:ast` snapshot differs (k0/k match)

Only the **typechecked-AST** dump differs; the K-forms match, so semantics are
identical — this is an AST *representation* difference the normaliser doesn't
absorb, or a genuinely platform-dependent AST print.

- Golden: `test/ir/nested_comprehension.ast.golden` (generated on macOS/arm64).
  It is deterministic on macOS (same md5 across runs).
- Harness: `tools/fxtest/ir_snapshot.py` + `tools/fxtest/normalize.py`
  (`normalize_ir` / `ir_extract_module` — extracts the last module, renumbers
  `name@NNN` gensyms by first appearance).

### Plan

1. Get the actual Linux dump and diff it against the golden:
   ```
   bin/ficus -no-c -pr-ast test/ir/nested_comprehension.fx > /tmp/linux.ast
   # normalize it the same way the harness does, then diff vs the golden:
   python3 - <<'PY'
   import sys; sys.path.insert(0,'tools/fxtest'); import normalize
   raw=open('/tmp/linux.ast').read()
   print(normalize.normalize_ir(raw, 'ast'))   # check the exact function name/args
   PY
   ```
   (See `ir_snapshot.py` for exactly how it calls the normalizer, and mirror it.)
2. Classify the diff:
   - **If it's a spurious/normalisation gap** (a gensym-order or type-var
     numbering that differs by platform): extend `normalize.py` so both
     platforms normalise to the same text, then `python3
     tools/fxtest/fxtest.py ir --update-golden` and verify `ir` is green.
   - **If it's a genuine platform-dependent AST print** that also holds for the
     OTHER 19 programs once you look (it doesn't today — they pass), that's a
     deeper compiler issue; prefer fixing the print/normaliser over per-platform
     goldens. Do NOT commit macOS-only goldens.
   - The other 19 IR programs pass on Linux, so whatever differs is specific to
     the array-of-arrays typing/print — likely a small thing. Compare a passing
     program's `.ast` (e.g. `comprehension.ast.golden`) for what a stable dump
     looks like.
3. Whatever you change, re-run `fxtest ir` on this machine and confirm all 60
   snapshots pass. Note in the commit that the golden was regenerated on Linux
   and why the two platforms now agree.

## Context / where things are

- Registry: `docs/found_bugs.md` (FB-006 entry has the fix write-up).
- Final report: `docs/ficus_harden_report.md`.
- Build/test gotchas: repo `CLAUDE.md` (note: `-o name` drops a binary at repo
  root; `-run` hides SIGSEGV as exit 1; incremental builds reuse the `-B` dir so
  `rm -rf` it for clean diffs).
- The CI build artifacts (per-leg `build/fxtest/` trees) are in the author's
  `~/work/ficus_ci/pr27/*.zip` — not on this machine, but the failing dump is
  cheaper to regenerate directly here.

When done: run the full ladder (`fxtest all` + `determinism` + `sanitize`),
update the `found_bugs.md` FB-006 entry with the Linux root-cause + fix, and
report the diff for the author to review (do not push).
