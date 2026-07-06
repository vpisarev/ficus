#!/usr/bin/env python3
"""
fxtest -- Ficus compiler test harness (Python 3, stdlib only).

Single entry point that discovers, builds, runs, compares and reports across a
"ladder of oracles":

  corpus    T2  differential: every runnable .fx compiled & run at -O0 and -O3,
                stdout compared (miscompilation detector)
  negative  T3  golden compiler diagnostics for intentionally-broken programs
  ir        T4  golden AST / K-form snapshots for small focused programs
  unit          wraps test/test_all.fx (the existing UTest suite)
  all           everything above

Design rule: this harness must not depend on the compiler it tests -- pure
stdlib Python only.  See tools/fxtest/README.md.
"""

import argparse
import concurrent.futures
import fnmatch
import os
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))          # tools/fxtest -> repo root
FICUS = os.path.join(REPO, "bin", "ficus")
BUILD = os.path.join(REPO, "build", "fxtest")
MANIFEST = os.path.join(HERE, "manifest.toml")

sys.path.insert(0, HERE)
import normalize  # noqa: E402


# ============================================================================
# tiny TOML loader (Python 3.9 has no tomllib; keep behavior identical on CI)
# ============================================================================

def _strip_toml_comment(line: str) -> str:
    out, in_str, esc = [], False, False
    for ch in line:
        if in_str:
            out.append(ch)
            if esc:
                esc = False
            elif ch == "\\":
                esc = True
            elif ch == '"':
                in_str = False
        else:
            if ch == '"':
                in_str = True
                out.append(ch)
            elif ch == "#":
                break
            else:
                out.append(ch)
    return "".join(out)


def _parse_toml_string(s: str) -> str:
    assert s[0] == '"'
    out, i, n = [], 1, len(s)
    while i < n:
        ch = s[i]
        if ch == "\\" and i + 1 < n:
            nxt = s[i + 1]
            out.append({"n": "\n", "t": "\t", '"': '"', "\\": "\\"}.get(nxt, nxt))
            i += 2
            continue
        if ch == '"':
            break
        out.append(ch)
        i += 1
    return "".join(out)


def _split_top_commas(s: str):
    parts, depth, in_str, esc, cur = [], 0, False, False, []
    for ch in s:
        if in_str:
            cur.append(ch)
            if esc:
                esc = False
            elif ch == "\\":
                esc = True
            elif ch == '"':
                in_str = False
            continue
        if ch == '"':
            in_str = True
            cur.append(ch)
        elif ch == "," and depth == 0:
            parts.append("".join(cur))
            cur = []
        else:
            if ch in "[":
                depth += 1
            elif ch in "]":
                depth -= 1
            cur.append(ch)
    if "".join(cur).strip():
        parts.append("".join(cur))
    return parts


def _parse_toml_value(v: str):
    v = v.strip()
    if v.startswith('"'):
        return _parse_toml_string(v)
    if v.startswith("["):
        inner = v[1:v.rindex("]")]
        return [_parse_toml_value(p.strip()) for p in _split_top_commas(inner) if p.strip()]
    if v in ("true", "false"):
        return v == "true"
    try:
        return int(v)
    except ValueError:
        pass
    return float(v)


def load_toml(path: str) -> dict:
    try:
        import tomllib
        with open(path, "rb") as f:
            return tomllib.load(f)
    except ModuleNotFoundError:
        pass
    root, cur = {}, None
    with open(path, "r") as f:
        for raw in f:
            line = _strip_toml_comment(raw).strip()
            if not line:
                continue
            if line.startswith("[") and line.endswith("]"):
                cur = root
                for part in line[1:-1].split("."):
                    cur = cur.setdefault(part.strip(), {})
            else:
                k, _, val = line.partition("=")
                if cur is None:
                    cur = root
                cur[k.strip()] = _parse_toml_value(val)
    return root


# ============================================================================
# result model + reporting
# ============================================================================

PASS, FAIL, SKIP, XFAIL, XPASS = "PASS", "FAIL", "SKIP", "XFAIL", "XPASS"

_COLORS = {
    PASS: "\033[32;1m", FAIL: "\033[31;1m", SKIP: "\033[33;1m",
    XFAIL: "\033[36m", XPASS: "\033[35;1m",
}


class Result:
    __slots__ = ("name", "status", "detail")

    def __init__(self, name, status, detail=""):
        self.name = name
        self.status = status
        self.detail = detail


def _use_color() -> bool:
    return sys.stdout.isatty() and os.environ.get("NO_COLOR") is None


def print_table(title, results):
    color = _use_color()
    width = max([len(r.name) for r in results], default=4)
    print(f"\n=== {title} ===")
    for r in results:
        tag = r.status
        if color and tag in _COLORS:
            shown = f"{_COLORS[tag]}{tag:<5}\033[0m"
        else:
            shown = f"{tag:<5}"
        line = f"  {shown}  {r.name:<{width}}"
        if r.detail:
            line += f"  {r.detail}"
        print(line)
    counts = {}
    for r in results:
        counts[r.status] = counts.get(r.status, 0) + 1
    summary = "  ".join(f"{k}={counts[k]}" for k in
                        (PASS, FAIL, XFAIL, XPASS, SKIP) if k in counts)
    print(f"  -> {summary}")
    return counts


def run_failed(counts) -> bool:
    """A run fails if anything FAILed or an xfail unexpectedly XPASSed."""
    return counts.get(FAIL, 0) > 0 or counts.get(XPASS, 0) > 0


# ============================================================================
# low-level compile+run
# ============================================================================

class RunOutcome:
    __slots__ = ("rc", "stdout", "stderr", "timed_out", "artifact")

    def __init__(self, rc, stdout, stderr, timed_out, artifact=None):
        self.rc = rc
        self.stdout = stdout
        self.stderr = stderr
        self.timed_out = timed_out
        self.artifact = artifact


def _build_root(name, opt, suffix=""):
    return os.path.join(BUILD, name, opt + suffix)


def compile_and_run(name, src, opt, args, timeout, *,
                    openmp=False, cpp=False, artifact=None, suffix=""):
    """Compile `src` at `opt` and run it, returning a RunOutcome.

    Runs with cwd=build_root so any relative output files (artifacts) are
    isolated per (entry, opt).  -no-openmp unless openmp=True.
    """
    broot = _build_root(name, opt, suffix)
    os.makedirs(broot, exist_ok=True)
    cmd = [FICUS, "-run", "-" + opt]
    if not openmp:
        cmd.append("-no-openmp")
    if cpp:
        cmd.append("-c++")
    cmd += ["-B", broot, os.path.abspath(src)]
    if args:
        cmd += ["--"] + list(args)
    try:
        p = subprocess.run(cmd, cwd=broot, capture_output=True, timeout=timeout)
        rc, out, err, to = p.returncode, p.stdout, p.stderr, False
    except subprocess.TimeoutExpired as e:
        rc, out, err, to = None, (e.stdout or b""), (e.stderr or b""), True
    art = None
    if artifact and not to and rc == 0:
        ap = os.path.join(broot, artifact)
        if os.path.exists(ap):
            with open(ap, "rb") as f:
                art = f.read()
    return RunOutcome(
        rc,
        out.decode("utf-8", "replace"),
        err.decode("utf-8", "replace"),
        to,
        art,
    )


# ============================================================================
# T2 -- corpus differential
# ============================================================================

def _entry_cfg(raw):
    return {
        "path": raw["path"],
        "args": raw.get("args", []),
        "compare": raw.get("compare", "exact"),
        "rtol": float(raw.get("rtol", 1e-10)),
        "atol": float(raw.get("atol", 0.0)),
        "artifact": raw.get("artifact"),
        "strip_header": bool(raw.get("strip_header", False)),
        "timeout_sec": int(raw.get("timeout_sec", 60)),
        "compile_timeout_sec": int(raw.get("compile_timeout_sec", 180)),
        "quarantine": raw.get("quarantine"),
    }


def _compare_outputs(cfg, ref_opt, ref, other_opt, other):
    """Compare two RunOutcomes per the entry's compare mode."""
    mode = cfg["compare"]
    for o, r in ((ref_opt, ref), (other_opt, other)):
        if r.timed_out:
            return FAIL, f"{o}: timed out"
        if r.rc != 0:
            tail = (r.stderr or r.stdout).strip().splitlines()[-1:] or [""]
            return FAIL, f"{o}: exited {r.rc} ({tail[0][:80]})"
    if mode == "none":
        return PASS, "smoke ok"
    if mode == "artifact":
        if ref.artifact is None or other.artifact is None:
            return FAIL, f"artifact '{cfg['artifact']}' not produced"
        if ref.artifact == other.artifact:
            return PASS, f"artifact {len(ref.artifact)} B match"
        return FAIL, f"artifact bytes differ ({len(ref.artifact)} vs {len(other.artifact)})"
    a = normalize.normalize_text(ref.stdout, strip_header=cfg["strip_header"])
    b = normalize.normalize_text(other.stdout, strip_header=cfg["strip_header"])
    if mode == "float":
        ok, why = normalize.float_compare(a, b, cfg["rtol"], cfg["atol"])
        return (PASS, "float match") if ok else (FAIL, f"{ref_opt}!={other_opt}: {why}")
    # exact
    if a == b:
        return PASS, "exact match"
    return FAIL, f"{ref_opt}!={other_opt}: stdout differs"


def run_corpus_entry(name, raw, opts):
    cfg = _entry_cfg(raw)
    if cfg["quarantine"]:
        return Result(name, SKIP, "quarantine: " + cfg["quarantine"][:90])
    src = os.path.join(REPO, cfg["path"])
    if not os.path.exists(src):
        return Result(name, FAIL, f"missing source {cfg['path']}")
    timeout = cfg["timeout_sec"] + cfg["compile_timeout_sec"]
    runs = {}
    for opt in opts:
        runs[opt] = compile_and_run(
            name, src, opt, cfg["args"], timeout,
            artifact=cfg["artifact"],
        )
    ref_opt = opts[0]
    if len(opts) == 1:
        r = runs[ref_opt]
        if r.timed_out:
            return Result(name, FAIL, f"{ref_opt}: timed out")
        if r.rc != 0:
            return Result(name, FAIL, f"{ref_opt}: exited {r.rc}")
        return Result(name, PASS, f"{ref_opt} builds & runs")
    for other in opts[1:]:
        status, detail = _compare_outputs(
            cfg, ref_opt, runs[ref_opt], other, runs[other])
        if status == FAIL:
            return Result(name, FAIL, detail)
    return Result(name, PASS, detail)


def _smoke_run(name, raw, *, cpp=False, openmp=False, suffix=""):
    """Single-config smoke: build+run once, require exit 0 (no diff).

    An entry may carry `cpp_xfail`/`openmp_xfail` = "reason" in the manifest
    (a fenced known bug): a failure is reported XFAIL, and an unexpected
    success is reported XPASS ("remove from quarantine?").
    """
    cfg = _entry_cfg(raw)
    if cfg["quarantine"]:
        return Result(name, SKIP, "quarantine")
    xfail = raw.get("cpp_xfail") if cpp else (raw.get("openmp_xfail") if openmp else None)
    src = os.path.join(REPO, cfg["path"])
    timeout = cfg["timeout_sec"] + cfg["compile_timeout_sec"]
    out = compile_and_run(name, src, "O1", cfg["args"], timeout,
                          cpp=cpp, openmp=openmp, suffix=suffix)
    failed, why = False, ""
    if out.timed_out:
        failed, why = True, "timed out"
    elif out.rc != 0:
        tail = (out.stderr or out.stdout).strip().splitlines()[-1:] or [""]
        failed, why = True, f"exited {out.rc} ({tail[0][:80]})"
    elif openmp and "[  FAILED  ]" in out.stdout:
        failed, why = True, "unit failure under OpenMP"
    if xfail:
        if failed:
            return Result(name, XFAIL, f"known: {xfail[:80]}")
        return Result(name, XPASS, f"unexpected pass -- remove xfail? ({xfail[:50]})")
    return Result(name, FAIL, why) if failed else Result(name, PASS, "smoke ok")


def _run_pool(fn, items, jobs):
    results = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
        futs = [ex.submit(fn, *it) for it in items]
        for fut in concurrent.futures.as_completed(futs):
            results.append(fut.result())
    results.sort(key=lambda r: r.name)
    return results


def cmd_corpus(args):
    manifest = load_toml(MANIFEST)
    corpus = manifest.get("corpus", {})
    opts = args.opt.split(",")
    names = sorted(corpus)
    if args.filter:
        names = [n for n in names if fnmatch.fnmatch(n, args.filter)]
    print(f"[corpus] {len(names)} entries, opts={opts}, jobs={args.jobs}")
    results = _run_pool(run_corpus_entry, [(n, corpus[n], opts) for n in names], args.jobs)
    counts = print_table("T2 corpus differential", results)
    rc = 1 if run_failed(counts) else 0

    # Optional single-config smoke axes (brief 3): no cross-comparison.
    if getattr(args, "cpp_smoke", False):
        res = _run_pool(lambda n, r: _smoke_run(n, r, cpp=True, suffix="_cpp"),
                        [(n, corpus[n]) for n in names], args.jobs)
        rc |= 1 if run_failed(print_table("smoke: -c++ (O1)", res)) else 0
    if getattr(args, "openmp_smoke", False) and "test_all" in corpus:
        res = [_smoke_run("test_all", corpus["test_all"], openmp=True, suffix="_omp")]
        rc |= 1 if run_failed(print_table("smoke: OpenMP test_all (O1)", res)) else 0
    return rc


# ============================================================================
# unit -- wrap test/test_all.fx
# ============================================================================

def cmd_unit(args):
    src = os.path.join(REPO, "test", "test_all.fx")
    print("[unit] running test/test_all.fx ...")
    out = compile_and_run("test_all_unit", src, "O1", [], 600)
    results = []
    if out.timed_out:
        results.append(Result("test_all", FAIL, "timed out"))
    elif out.rc != 0:
        results.append(Result("test_all", FAIL, f"exited {out.rc}"))
    elif "[  FAILED  ]" in out.stdout:
        # extract failed test names printed by UTest
        failed = [ln for ln in out.stdout.splitlines() if "[  FAILED  ]" in ln]
        results.append(Result("test_all", FAIL, failed[-1].strip()[:100]))
    else:
        passed = [ln for ln in out.stdout.splitlines() if "[  PASSED  ]" in ln]
        detail = passed[-1].strip() if passed else "ok"
        results.append(Result("test_all", PASS, normalize.strip_ansi(detail)))
    counts = print_table("unit (test_all.fx)", results)
    return 1 if run_failed(counts) else 0


# ============================================================================
# T3 / T4 dispatch (implemented in P2 / P3; discover-and-pass until populated)
# ============================================================================

def cmd_negative(args):
    from negative import run_negative  # noqa
    return run_negative(REPO, FICUS, update=args.update_golden)


def cmd_ir(args):
    from ir_snapshot import run_ir  # noqa
    return run_ir(REPO, FICUS, update=args.update_golden)


def _try(fn, args):
    """Run a subcommand module that may not exist yet (P2/P3)."""
    try:
        return fn(args)
    except ModuleNotFoundError as e:
        print(f"  (skipped: {e.name} not implemented yet)")
        return 0


def cmd_all(args):
    rc = 0
    rc |= _try(cmd_unit, args)
    rc |= _try(cmd_negative, args)
    rc |= _try(cmd_ir, args)
    rc |= _try(cmd_corpus, args)
    print(f"\n[fxtest all] {'FAILED' if rc else 'PASSED'}")
    return rc


# ============================================================================
# CLI
# ============================================================================

def main(argv=None):
    ap = argparse.ArgumentParser(prog="fxtest.py", description="Ficus test harness")
    sub = ap.add_subparsers(dest="cmd", required=True)

    def add_common(p):
        p.add_argument("--jobs", type=int, default=os.cpu_count() or 4)

    p = sub.add_parser("corpus", help="T2 differential O0/O3")
    p.add_argument("--opt", default="O0,O3")
    p.add_argument("--filter", default=None)
    p.add_argument("--cpp-smoke", action="store_true", help="also build+run each entry with -c++")
    p.add_argument("--openmp-smoke", action="store_true", help="also build+run test_all with OpenMP")
    add_common(p)
    p.set_defaults(func=cmd_corpus)

    p = sub.add_parser("negative", help="T3 golden diagnostics")
    p.add_argument("--update-golden", action="store_true")
    add_common(p)
    p.set_defaults(func=cmd_negative)

    p = sub.add_parser("ir", help="T4 IR snapshots")
    p.add_argument("--update-golden", action="store_true")
    add_common(p)
    p.set_defaults(func=cmd_ir)

    p = sub.add_parser("unit", help="wrap test_all.fx")
    add_common(p)
    p.set_defaults(func=cmd_unit)

    p = sub.add_parser("all", help="run everything")
    p.add_argument("--opt", default="O0,O3")
    p.add_argument("--filter", default=None)
    p.add_argument("--update-golden", action="store_true")
    p.add_argument("--cpp-smoke", action="store_true")
    p.add_argument("--openmp-smoke", action="store_true")
    add_common(p)
    p.set_defaults(func=cmd_all)

    args = ap.parse_args(argv)
    if not os.path.exists(FICUS):
        print(f"error: {FICUS} not found; run `make` first", file=sys.stderr)
        return 2
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
