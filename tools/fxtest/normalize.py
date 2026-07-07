"""
Output / IR normalization helpers for fxtest.

The differential corpus runner (T2) and the golden suites (T3/T4) all need to
compare program output that contains incidental, non-deterministic noise:

  * ANSI color escapes (emitted when stdout is a TTY),
  * the 3-line banner test_all.fx prints (version / platform / compiler),
  * per-test timing such as "(12 ms)" / "(<1 ms)",
  * absolute file paths in diagnostics (T3),
  * CR/LF differences.

Everything here is pure string->string so it is trivially testable and shared
by every subcommand.  See tools/fxtest/README.md for the rationale.
"""

import re

# --- regexes -----------------------------------------------------------------

_ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")

# UTest prints "(<1 ms)", "(12 ms)" and totals like "(1234 ms)".
_TIMING_RE = re.compile(r"\((?:<1 ms|\d+(?:\.\d+)? ms)\)")

# test_all.fx banner lines (after ANSI stripping).
_HEADER_PREFIXES = ("Ficus version:", "Platform:", "C/C++ Compiler:")

# A number, for positional float comparison.  Matches ints and floats with
# optional sign / exponent, plus the special tokens inf / nan.
_NUM_RE = re.compile(
    r"[-+]?(?:inf|nan|(?:\d+\.\d*|\.\d+|\d+)(?:[eE][-+]?\d+)?)",
    re.IGNORECASE,
)


def strip_ansi(s: str) -> str:
    return _ANSI_RE.sub("", s)


def normalize_text(s, *, strip_header=False, normalize_timing=True):
    """Canonicalize program stdout/stderr for comparison.

    - CR/LF -> LF
    - drop ANSI color escapes
    - optionally drop the test_all.fx banner lines
    - optionally collapse UTest timings to a stable token
    - strip trailing whitespace on each line and trailing blank lines
    """
    s = s.replace("\r\n", "\n").replace("\r", "\n")
    s = strip_ansi(s)
    out = []
    for ln in s.split("\n"):
        if strip_header and ln.startswith(_HEADER_PREFIXES):
            continue
        if normalize_timing:
            ln = _TIMING_RE.sub("(TIME)", ln)
        out.append(ln.rstrip())
    while out and out[-1] == "":
        out.pop()
    return "\n".join(out)


def normalize_paths(s: str, repo_root: str) -> str:
    """Replace absolute paths with basenames so diagnostics are host-agnostic.

    Used by the T3 negative suite where compiler messages embed the source
    path.  file:line:col is preserved -- only the directory part is dropped.
    """
    # 1) any absolute path to a .fx file -> basename
    s = re.sub(r"(/[^\s:]+/)([^/\s:]+\.fx)", r"\2", s)
    # 2) leftover references to the repo root itself
    s = s.replace(repo_root.rstrip("/") + "/", "")
    return s


# --- positional float comparison ---------------------------------------------

def _split_numbers(s: str):
    """Return (skeleton, [floats]) where every number is replaced by \\0.

    The skeleton is the surrounding text; comparing two skeletons for equality
    checks that all non-numeric content matches exactly, while the number lists
    are compared with a tolerance.
    """
    nums = []

    def repl(m):
        tok = m.group()
        try:
            nums.append(float(tok))
        except ValueError:
            # e.g. a bare "+"/"-" caught by an odd match; keep literal
            return tok
        return "\0"

    skeleton = _NUM_RE.sub(repl, s)
    return skeleton, nums


def _isclose(x: float, y: float, rtol: float, atol: float) -> bool:
    import math
    if math.isnan(x) or math.isnan(y):
        return math.isnan(x) and math.isnan(y)
    if math.isinf(x) or math.isinf(y):
        return x == y
    return abs(x - y) <= atol + rtol * abs(y)


# --- IR snapshot normalization (T4) ------------------------------------------

_IR_GENSYM_RE = re.compile(r"@(\d+)")


def ir_extract_module(text: str, stem: str):
    """Return only the dump section for module `stem` (the file under test).

    The compiler dumps every (auto-imported) module; we keep only the last
    section belonging to our test file so unrelated stdlib output is excluded.
    Handles both the -pr-ast header (`.../<stem>.fx: <deps>` + `---` rule) and
    the -pr-k0/-pr-k marker (`///////// module <stem>: N expressions /////////`).
    """
    lines = text.replace("\r\n", "\n").split("\n")

    marker = f"///////// module {stem}:"
    idxs = [i for i, l in enumerate(lines) if l.startswith(marker)]
    if idxs:
        start = idxs[-1] + 1
        end = len(lines)
        for j in range(start, len(lines)):
            if lines[j].startswith("///////// module "):
                end = j
                break
        return "\n".join(lines[start:end]).strip("\n")

    # -pr-ast: the module-under-test is the LAST `<path>/<stem>.fx: <deps>`
    # header; its body runs to EOF (a `---` rule line follows the header).
    hdr_re = re.compile(r"(^|/)" + re.escape(stem) + r"\.fx: ")
    hdrs = [i for i, l in enumerate(lines) if hdr_re.search(l)]
    if hdrs:
        start = hdrs[-1] + 1
        # The `<abs-path>.fx: <dep, dep, ...>` header is pretty-printed with a
        # width limit, so a long dependency list WRAPS onto indented
        # continuation lines before the `-----` rule.  The wrap point depends on
        # the absolute path length, which differs per machine (e.g. a short
        # /home/<user>/... dev path keeps it on one line, but a longer CI path
        # like /home/runner/work/ficus/ficus/... pushes the last dep onto its
        # own `   Foo` line).  Skip everything up to and including that rule so
        # the extracted body is identical regardless of where the repo lives.
        while start < len(lines) and set(lines[start].strip()) != {"-"}:
            start += 1
        if start < len(lines):   # skip the `-----` rule line itself
            start += 1
        return "\n".join(lines[start:]).strip("\n")

    return text.strip("\n")


def normalize_ir(text: str, stem: str) -> str:
    """Extract the module-under-test and canonicalize gensym counters.

    Generated ids `name@1234` embed a global counter that shifts with ANY
    unrelated change.  We renumber the numeric parts in order of first
    appearance (`@g0, @g1, ...`) so a snapshot depends only on the *structure*
    of the program under test, not on absolute counter values.
    """
    body = ir_extract_module(text, stem)
    body = strip_ansi(body)
    mapping = {}

    def repl(m):
        n = m.group(1)
        if n not in mapping:
            mapping[n] = f"g{len(mapping)}"
        return "@" + mapping[n]

    body = _IR_GENSYM_RE.sub(repl, body)
    return "\n".join(ln.rstrip() for ln in body.split("\n")).strip("\n")


def float_compare(a: str, b: str, rtol: float, atol: float):
    """Compare two normalized outputs numerically.

    Non-numeric text must match exactly; numbers must match within tolerance,
    positionally.  Returns (ok: bool, reason: str).
    """
    a_sk, a_nums = _split_numbers(a)
    b_sk, b_nums = _split_numbers(b)
    if a_sk != b_sk:
        return False, "non-numeric text differs"
    if len(a_nums) != len(b_nums):
        return False, f"different count of numbers ({len(a_nums)} vs {len(b_nums)})"
    for i, (x, y) in enumerate(zip(a_nums, b_nums)):
        if not _isclose(x, y, rtol, atol):
            return False, f"number #{i} differs: {x!r} vs {y!r} (rtol={rtol}, atol={atol})"
    return True, ""
