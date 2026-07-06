"""
T4 -- IR snapshot suite.

Small focused programs in test/ir/ (one language feature each) are dumped at
three compiler stages and compared to goldens:

    -no-c -pr-ast          typechecked AST        -> <name>.ast.golden
    -no-c -pr-k0           initial K-form         -> <name>.k0.golden
    -no-c -O3 -pr-k        optimized K-form       -> <name>.k.golden

Only the section belonging to the file-under-test is kept (the compiler dumps
every auto-imported module), and generated gensym counters `name@1234` are
renumbered by first appearance (`@g0, @g1, ...`) -- see normalize.normalize_ir.
Without that renumbering an unrelated edit anywhere shifts every counter and the
snapshots become useless noise; the renumbering is validated in the README.

These snapshots are the proof, during later syntax reform, that a pure-syntax
change is semantics-preserving.  They are high-maintenance, so the suite is kept
small (core constructs only).
"""

import glob
import os
import subprocess

import fxtest as F
import normalize

STAGES = [
    ("ast", ["-no-c", "-pr-ast"]),
    ("k0", ["-no-c", "-pr-k0"]),
    ("k", ["-no-c", "-O3", "-pr-k"]),
]


def _dump(ficus, src, flags, builddir):
    os.makedirs(builddir, exist_ok=True)
    cmd = [ficus] + flags + ["-B", builddir, os.path.abspath(src)]
    try:
        p = subprocess.run(cmd, capture_output=True, timeout=180)
        return p.returncode, (p.stdout + p.stderr).decode("utf-8", "replace")
    except subprocess.TimeoutExpired:
        return None, "<TIMEOUT>"


def _run_stage(repo, ficus, src, stage, flags, update):
    stem = os.path.basename(src)[:-3]
    name = f"{stem}:{stage}"
    golden = src[:-3] + f".{stage}.golden"
    builddir = os.path.join(F.BUILD, "ir", f"{stem}_{stage}")
    rc, raw = _dump(ficus, src, flags, builddir)
    if rc not in (0, None) and "error:" in raw:
        first = next((l for l in raw.splitlines() if "error:" in l), "")[:80]
        return F.Result(name, F.FAIL, f"compile error: {first}")
    norm = normalize.normalize_ir(raw, stem)
    norm = normalize.normalize_paths(norm, repo)
    if not norm.strip():
        return F.Result(name, F.FAIL, "empty snapshot (extraction failed?)")

    if update:
        with open(golden, "w") as f:
            f.write(norm + "\n")
        return F.Result(name, F.PASS, "golden written")

    if not os.path.exists(golden):
        return F.Result(name, F.FAIL, "no golden; run --update-golden")
    with open(golden, "r") as f:
        want = f.read().rstrip("\n")
    return (F.Result(name, F.PASS, f"{stage} match") if norm == want
            else F.Result(name, F.FAIL, f"{stage} snapshot differs from golden"))


def run_ir(repo, ficus, update=False, jobs=None):
    ir_dir = os.path.join(repo, "test", "ir")
    if not os.path.isdir(ir_dir):
        print("[ir] test/ir/ not present -- nothing to do")
        return 0
    cases = sorted(glob.glob(os.path.join(ir_dir, "*.fx")))
    tasks = [(repo, ficus, src, stage, flags, update)
             for src in cases for (stage, flags) in STAGES]
    print(f"[ir] {len(cases)} programs x {len(STAGES)} stages = {len(tasks)} "
          f"snapshots{' (updating goldens)' if update else ''}")
    results = F._run_pool(lambda *t: _run_stage(*t), tasks,
                          jobs or (os.cpu_count() or 4))
    counts = F.print_table("T4 IR snapshots", results)
    return 1 if F.run_failed(counts) else 0
