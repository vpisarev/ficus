"""
T3 -- golden diagnostics ("negative") suite.

Each case is a tiny, self-contained, intentionally-broken program
`test/negative/NNN_short_name.fx` paired with a golden `NNN_short_name.err`
capturing the compiler's diagnostics.  The runner invokes

    bin/ficus -no-c <file>        (parse + typecheck only, no C generation)

expects a nonzero exit, captures stdout+stderr, normalizes it (absolute paths ->
basename, ANSI stripped, repeated spaces collapsed) and compares to the golden.

Convention: the FIRST line of every case is
    // expect: <substring the diagnostic must contain>
This documents the intended error family and lets the runner verify the case
triggers what it is supposed to (not some unrelated error) -- a program that
compiles cleanly, or reports a different error, fails loudly.

Cases under test/negative/crashes/ document compiler bugs (an abort / internal
error / wrong-diagnostic).  They are fenced: a match to the golden is XFAIL
(still broken as recorded); any change is XPASS (investigate -- see
docs/found_bugs.md).
"""

import os
import re
import subprocess

import fxtest as F
import normalize

_EXPECT_RE = re.compile(r"\s*//\s*expect:\s*(.*)")
# Optional per-case extra compiler flags, e.g. `// flags: -Wall -Werror` to
# exercise a warning-as-error case (a warning alone exits 0; this harness
# requires a nonzero exit). Scanned across the leading comment header.
_FLAGS_RE = re.compile(r"\s*//\s*flags:\s*(.*)")


def _discover(neg_dir):
    cases = []
    for dirpath, _dirs, files in os.walk(neg_dir):
        for fn in files:
            if fn.endswith(".fx"):
                cases.append(os.path.join(dirpath, fn))
    return sorted(cases)


def _expect_of(src):
    with open(src, "r") as f:
        m = _EXPECT_RE.match(f.readline())
    return m.group(1).strip() if m else None


def _flags_of(src):
    with open(src, "r") as f:
        for _ in range(6):                          # scan the comment header
            line = f.readline()
            if not line:
                break
            m = _FLAGS_RE.match(line)
            if m:
                return m.group(1).split()
    return []


_POS_RE = re.compile(r"([\w./-]+\.fx):(\d+):(\d+)")


def _normalize_err(text, repo, keep_basename=None):
    text = normalize.normalize_paths(text, repo)
    text = normalize.strip_ansi(text).replace("\r\n", "\n").replace("\r", "\n")

    # The test file's own line:col is part of the contract; positions inside
    # OTHER files (stdlib candidate dumps, e.g. Builtins.fx:157:1) are incidental
    # and would break these goldens on unrelated stdlib edits -> collapse to L:C.
    def _pos(m):
        fname = os.path.basename(m.group(1))
        if keep_basename and fname == keep_basename:
            return m.group(0)
        return f"{m.group(1)}:L:C"
    text = _POS_RE.sub(_pos, text)

    out = []
    for ln in text.split("\n"):
        ln = re.sub(r"[ \t]+", " ", ln).rstrip()   # collapse repeated whitespace
        out.append(ln)
    while out and out[-1] == "":
        out.pop()
    return "\n".join(out)


def _run_ficus_noc(ficus, src, builddir, flags=()):
    os.makedirs(builddir, exist_ok=True)
    cmd = [ficus, "-no-c", "-B", builddir] + list(flags) + [os.path.abspath(src)]
    try:
        p = subprocess.run(cmd, capture_output=True, timeout=120)
        combined = p.stdout + p.stderr
        return p.returncode, combined.decode("utf-8", "replace")
    except subprocess.TimeoutExpired:
        return None, "<TIMEOUT>"


def _case_name(neg_dir, src):
    rel = os.path.relpath(src, neg_dir)
    return rel[:-3]                                 # drop .fx


def _run_one(repo, ficus, neg_dir, src, update):
    name = _case_name(neg_dir, src)
    is_crash = os.path.basename(os.path.dirname(src)) == "crashes"
    expect = _expect_of(src)
    golden = src[:-3] + ".err"
    builddir = os.path.join(F.BUILD, "negative", name.replace("/", "_"))
    rc, raw = _run_ficus_noc(ficus, src, builddir, _flags_of(src))
    norm = _normalize_err(raw, repo, keep_basename=os.path.basename(src))

    if update:
        with open(golden, "w") as f:
            f.write(norm + "\n")
        warn = ""
        if rc == 0:
            warn = " [WARN compiled cleanly!]"
        elif expect and expect.lower() not in norm.lower():
            warn = f" [WARN expected '{expect}' not in output]"
        return F.Result(name, F.PASS, "golden written" + warn)

    if not os.path.exists(golden):
        return F.Result(name, F.FAIL, "no golden (.err); run --update-golden")

    with open(golden, "r") as f:
        want = f.read().rstrip("\n")

    if is_crash:
        return (F.Result(name, F.XFAIL, "still broken (see found_bugs)")
                if norm == want else
                F.Result(name, F.XPASS, "behavior changed -- investigate"))

    if rc == 0:
        return F.Result(name, F.FAIL, "expected a diagnostic, but compiled cleanly")
    if expect and expect.lower() not in norm.lower():
        first = (norm.split("\n", 1)[0])[:70]
        return F.Result(name, F.FAIL, f"intended error missing; got: {first}")
    if norm != want:
        return F.Result(name, F.FAIL, "diagnostics differ from golden")
    return F.Result(name, F.PASS, "golden match")


def run_negative(repo, ficus, update=False, jobs=None):
    neg_dir = os.path.join(repo, "test", "negative")
    if not os.path.isdir(neg_dir):
        print("[negative] test/negative/ not present -- nothing to do")
        return 0
    cases = _discover(neg_dir)
    print(f"[negative] {len(cases)} cases{' (updating goldens)' if update else ''}")
    results = F._run_pool(
        lambda s: _run_one(repo, ficus, neg_dir, s, update),
        [(s,) for s in cases], jobs or (os.cpu_count() or 4))
    counts = F.print_table("T3 negative diagnostics", results)
    return 1 if F.run_failed(counts) else 0
