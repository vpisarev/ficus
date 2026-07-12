#!/usr/bin/env python3
"""
determinism -- FB-008 regression checks (Python 3, stdlib only).

Encodes the property that the Ficus compiler's generated C is stable:

  --rebuilds N        N independent from-scratch builds of the compiler produce
                      a byte-identical set of .c files (full-build determinism).

  --unrelated-change  build the compiler, snapshot its .c; apply a small,
                      DCE-surviving change to an EARLY module (compiler/Ast.fx --
                      one extra builtin id); INCREMENTALLY rebuild into the same
                      build dir (as during development); assert every module
                      whose own source did not change still has byte-identical
                      .c.  This is the exact scenario FB-008 used to break:
                      recompiling a module reused a build dir where cross-module
                      inlining had been suppressed for skipped modules, so an
                      unchanged module's .c churned.

Both drive only `bin/ficus`; the harness never links against the compiler.

The compiler is compiled to C with `bin/ficus -O3 -o <name> compiler/fx.fx`,
which drops every module's .c into <repo>/__fxbuild__/<name>/.  A full build is
~1 min, so `all` runs a light default (rebuilds=2) and CI can run more nightly.
"""

import os
import shutil
import subprocess
import sys

# The anchor line in compiler/Ast.fx after which we insert a throw-away builtin
# id.  Registering a builtin id survives dead-code elimination (it is threaded
# into `builtin_ids`, which init_all() iterates) and shifts the global id space,
# which is what makes it a valid FB-008 perturbation.  A plain unused function
# would be DCE'd and change nothing.
_ANCHOR = 'val (std__Rrbvec__, builtin_ids) = std_id("Rrbvec", builtin_ids)'
_PROBE = 'val (std__detprobe__, builtin_ids) = std_id("__detprobe__", builtin_ids)'

_C = {"g": "\033[32;1m", "r": "\033[31;1m", "y": "\033[33;1m", "0": "\033[0m"}


def _col(tag):
    return _C.get(tag, "") if (sys.stdout.isatty() and
                               os.environ.get("NO_COLOR") is None) else ""


def _say(status, name, detail=""):
    c = {"PASS": "g", "FAIL": "r", "SKIP": "y"}.get(status, "0")
    on, off = _col(c), (_C["0"] if _col(c) else "")
    line = f"  {on}{status:<5}{off}  {name}"
    if detail:
        line += f"  {detail}"
    print(line)


def _build_compiler(ficus, repo, outname):
    """Compile compiler/fx.fx to C in __fxbuild__/<outname>.  Returns (ok, log)."""
    cmd = [ficus, "-O3", "-o", outname, os.path.join("compiler", "fx.fx")]
    p = subprocess.run(cmd, cwd=repo, capture_output=True)
    # -o also drops the linked executable at repo root; remove it, keep the .c
    stray = os.path.join(repo, outname)
    if os.path.exists(stray):
        try:
            os.remove(stray)
        except OSError:
            pass
    ok = p.returncode == 0
    log = (p.stdout + p.stderr).decode("utf-8", "replace")
    return ok, log


def _cdir(repo, outname):
    return os.path.join(repo, "__fxbuild__", outname)


def _read_cset(cdir):
    """basename -> bytes for every .c in cdir."""
    out = {}
    if not os.path.isdir(cdir):
        return out
    for fn in os.listdir(cdir):
        if fn.endswith(".c"):
            with open(os.path.join(cdir, fn), "rb") as f:
                out[fn] = f.read()
    return out


def _diff_lines(a, b):
    return sum(1 for x, y in zip(a.splitlines(), b.splitlines()) if x != y) \
        + abs(len(a.splitlines()) - len(b.splitlines()))


# --------------------------------------------------------------------------
# check 1: N from-scratch builds are byte-identical
# --------------------------------------------------------------------------

def _check_rebuilds(ficus, repo, n):
    results = []
    ref = None
    for i in range(n):
        name = f"det_rebuild{i}"
        shutil.rmtree(_cdir(repo, name), ignore_errors=True)
        ok, log = _build_compiler(ficus, repo, name)
        if not ok:
            tail = log.strip().splitlines()[-1:] or [""]
            results.append(("FAIL", f"rebuild#{i}", f"build failed: {tail[0][:80]}"))
            return results
        cset = _read_cset(_cdir(repo, name))
        if ref is None:
            ref = (name, cset)
            results.append(("PASS", f"rebuild#{i}", f"{len(cset)} modules (reference)"))
            continue
        churn = [fn for fn in ref[1] if ref[1].get(fn) != cset.get(fn)]
        if churn:
            worst = max(churn, key=lambda fn:
                        _diff_lines(ref[1][fn].decode("utf-8", "replace"),
                                    cset.get(fn, b"").decode("utf-8", "replace")))
            results.append(("FAIL", f"rebuild#{i}",
                            f"{len(churn)} modules differ from rebuild#0 "
                            f"(e.g. {worst})"))
        else:
            results.append(("PASS", f"rebuild#{i}", "byte-identical to rebuild#0"))
    return results


# --------------------------------------------------------------------------
# check 2: incremental rebuild after an unrelated change is byte-stable
# --------------------------------------------------------------------------

def _check_unrelated_change(ficus, repo):
    ast = os.path.join(repo, "compiler", "Ast.fx")
    with open(ast, "r", encoding="utf-8") as f:
        original = f.read()
    if _ANCHOR not in original:
        return [("SKIP", "unrelated-change",
                 "anchor line not found in compiler/Ast.fx; update determinism.py")]
    name = "det_incr_check"
    cdir = _cdir(repo, name)
    try:
        # 1. clean full build, snapshot
        shutil.rmtree(cdir, ignore_errors=True)
        ok, log = _build_compiler(ficus, repo, name)
        if not ok:
            tail = log.strip().splitlines()[-1:] or [""]
            return [("FAIL", "unrelated-change", f"baseline build failed: {tail[0][:80]}")]
        snap = _read_cset(cdir)

        # 2. perturb Ast.fx and INCREMENTALLY rebuild into the same dir
        with open(ast, "w", encoding="utf-8") as f:
            f.write(original.replace(_ANCHOR, _ANCHOR + "\n" + _PROBE, 1))
        ok, log = _build_compiler(ficus, repo, name)
    finally:
        # 3. always restore Ast.fx
        with open(ast, "w", encoding="utf-8") as f:
            f.write(original)

    if not ok:
        tail = log.strip().splitlines()[-1:] or [""]
        return [("FAIL", "unrelated-change", f"incremental build failed: {tail[0][:80]}")]

    now = _read_cset(cdir)
    churn = []
    for fn, before in snap.items():
        if fn == "Ast.c":       # the module we intentionally changed
            continue
        if now.get(fn) != before:
            churn.append((fn, _diff_lines(before.decode("utf-8", "replace"),
                                          now.get(fn, b"").decode("utf-8", "replace"))))
    if churn:
        churn.sort(key=lambda t: -t[1])
        detail = f"{len(churn)} unchanged modules churned: " + \
                 ", ".join(f"{fn}(+{n})" for fn, n in churn[:5])
        return [("FAIL", "unrelated-change", detail)]
    return [("PASS", "unrelated-change",
             f"{len(snap) - 1} unchanged modules byte-identical after incremental rebuild")]


def run_determinism(repo, ficus, rebuilds=2, unrelated_change=True):
    print("\n=== FB-008 determinism ===")
    results = []
    if rebuilds and rebuilds > 1:
        results += _check_rebuilds(ficus, repo, rebuilds)
    if unrelated_change:
        results += _check_unrelated_change(ficus, repo)
    for status, name, detail in results:
        _say(status, name, detail)
    nfail = sum(1 for s, _, _ in results if s == "FAIL")
    npass = sum(1 for s, _, _ in results if s == "PASS")
    nskip = sum(1 for s, _, _ in results if s == "SKIP")
    summary = f"PASS={npass}"
    if nfail:
        summary += f"  FAIL={nfail}"
    if nskip:
        summary += f"  SKIP={nskip}"
    print(f"  -> {summary}")
    return 1 if nfail else 0
