"""
doctut -- the tutorial-example oracle.

Every fenced code block in `doc/ficustut.md` is extracted, classified and
(unless it is deliberately non-checkable) fed to the real compiler, so a future
syntax reform that breaks a documented example breaks this leg.  Stdlib-only,
like the rest of the harness; wired as `python3 tools/fxtest/fxtest.py doctut`.

Classification is driven by an HTML-comment directive on the line right before
the opening fence -- invisible in the rendered PDF, so the tutorial text stays
clean, and every marker is a reviewable line in the diff:

  (no directive)          a standalone Ficus snippet: compiled with `-no-c`
                          (parse + typecheck only -- NOT -Wall: tutorial
                          examples legitimately skip return annotations).
  <!-- doctut: run -->    additionally build & run it (-run, exit 0, no
                          uncaught exception -- no output goldens in v1).
  <!-- doctut: fragment --> deliberately partial (grammar schema, elided `...`
                          body, undeclared names): skipped.
  <!-- doctut: continue --> the block extends the previous checkable block in
                          the same section; the chain is concatenated and
                          checked once.
  <!-- doctut: shell -->  shell transcript / non-Ficus: skipped (also
                          auto-detected by content and by a fence info-string).

A block introduced by a fence with an info-string (```text, ```ML, ...) is
non-Ficus by construction and skipped automatically.
"""

import os
import re
import subprocess

import fxtest as F

TUTORIAL = os.path.join(F.REPO, "doc", "ficustut.md")

_FENCE_RE = re.compile(r"^(\s*)```(.*)$")
_HEADING_RE = re.compile(r"^#{1,6}\s+(.*)$")
# The keyword may be followed by a free-text note (e.g. a FIXME) before `-->`.
_DIRECTIVE_RE = re.compile(r"<!--\s*doctut:\s*(\w+)\b")


class Block:
    __slots__ = ("line", "end_line", "section", "info", "directive", "body")

    def __init__(self, line, section, info, directive):
        self.line = line                 # 1-based line of the opening fence
        self.end_line = line
        self.section = section
        self.info = info                 # text after ``` on the opening fence
        self.directive = directive       # run|fragment|continue|shell|None
        self.body = []                   # dedented source lines


def extract_blocks(md_path):
    """Parse the markdown into a flat, ordered list of fenced Blocks."""
    with open(md_path, encoding="utf-8") as f:
        lines = f.readlines()
    blocks = []
    section = "(top)"
    prev_nonblank = ""                   # last non-blank line before a fence
    i, n = 0, len(lines)
    while i < n:
        raw = lines[i].rstrip("\n")
        m = _FENCE_RE.match(raw)
        if not m:
            hm = _HEADING_RE.match(raw)
            if hm:
                section = hm.group(1).strip()
            if raw.strip():
                prev_nonblank = raw.strip()
            i += 1
            continue
        # opening fence
        indent = len(m.group(1))
        info = m.group(2).strip()
        dm = _DIRECTIVE_RE.search(prev_nonblank)
        directive = dm.group(1).lower() if dm else None
        blk = Block(i + 1, section, info, directive)
        i += 1
        while i < n:
            body_raw = lines[i].rstrip("\n")
            # a closing fence is ``` (possibly indented), nothing after it
            if body_raw.strip() == "```":
                break
            # dedent by the opening fence's indentation when present
            if body_raw[:indent].strip() == "":
                blk.body.append(body_raw[indent:])
            else:
                blk.body.append(body_raw)
            i += 1
        blk.end_line = i + 1
        blocks.append(blk)
        prev_nonblank = ""               # a fence consumes the "preceding line"
        i += 1
    return blocks


_SHELL_HINT = re.compile(r"^\s*\$ ")
_C_HINT = re.compile(r"^\s*#(include|if|ifdef|ifndef|define|error|endif|else|elif)\b")


def looks_non_ficus(blk):
    """Content heuristic for shell transcripts / C code / plain output."""
    if blk.info:                         # ```text, ```ML, ```C ...
        return True
    body = [ln for ln in blk.body if ln.strip()]
    if not body:
        return True
    shellish = sum(bool(_SHELL_HINT.match(ln)) for ln in body)
    if shellish and shellish >= max(1, len(body) // 2):
        return True
    if any(_C_HINT.match(ln) for ln in body):
        return True
    return False


def classify(blk):
    """One of: run, compile, fragment, continue, skip."""
    if blk.directive in ("fragment", "shell"):
        return blk.directive
    if blk.directive == "continue":
        return "continue"
    if looks_non_ficus(blk):
        return "skip"
    if blk.directive == "run":
        return "run"
    return "compile"


def _run_ficus(ficus, src, builddir, run=False):
    os.makedirs(builddir, exist_ok=True)
    cmd = [ficus, "-run" if run else "-no-c", "-no-openmp", "-B", builddir,
           os.path.abspath(src)]
    try:
        p = subprocess.run(cmd, cwd=builddir, capture_output=True, timeout=120)
        return p.returncode, (p.stdout + p.stderr).decode("utf-8", "replace")
    except subprocess.TimeoutExpired:
        return None, "<TIMEOUT>"


_DIAG_LINE_RE = re.compile(r"\.fx:(\d+):(\d+):\s*(error|warning):\s*(.*)")


def _first_diag(out):
    """The first real diagnostic, stripped of the (long, absolute) build path:
    reported as `Ln:col error: message` so the tutorial-side location and the
    message both survive the column budget."""
    for ln in out.splitlines():
        m = _DIAG_LINE_RE.search(ln)
        if m:
            return f"snip {m.group(1)}:{m.group(2)} {m.group(3)}: {m.group(4)}"[:110]
    for ln in out.splitlines():
        s = ln.strip()
        if s and not s.startswith("```"):
            return s[:110]
    return ""


def _chain_name(head):
    sec = re.sub(r"[^\w]+", "-", head.section).strip("-").lower()[:28]
    return f"{sec}:{head.line}"


def _check_block(ficus, blk, srclines, run):
    name = _chain_name(blk)
    builddir = os.path.join(F.BUILD, "doctut", name.replace(":", "_"))
    src = os.path.join(builddir, "block.fx")
    os.makedirs(builddir, exist_ok=True)
    with open(src, "w", encoding="utf-8") as f:
        f.write("\n".join(srclines) + "\n")
    rc, out = _run_ficus(ficus, src, builddir, run=run)
    kind = "run" if run else "compile"
    if rc is None:
        return F.Result(name, F.FAIL, f"L{blk.line}: timed out")
    if rc != 0:
        return F.Result(name, F.FAIL, f"L{blk.line} ({kind}): {_first_diag(out)}")
    return F.Result(name, F.PASS, f"L{blk.line} {kind} ok")


def plan_blocks(blocks):
    """Resolve each block to a work item, section by section.

    A bare block (no directive) is checked STANDALONE. A `continue` block is
    checked with all preceding checkable blocks of the same section prepended
    (the tutorial builds definitions up across consecutive blocks). Fragments
    and shell/non-Ficus blocks are skipped and are NOT accumulated, so a
    deliberately-partial snippet never poisons a later `continue`.
    """
    items = []                           # (block, srclines_to_compile, run?)
    counts = {"compile": 0, "run": 0, "fragment": 0, "shell": 0,
              "skip": 0, "continue": 0, "orphan": 0}
    sec_acc, sec = [], None
    for blk in blocks:
        if blk.section != sec:
            sec, sec_acc = blk.section, []
        kind = classify(blk)
        counts[kind] = counts.get(kind, 0) + 1
        if kind in ("fragment", "shell", "skip"):
            continue
        if kind == "continue":
            if not sec_acc:
                counts["orphan"] += 1
                counts["continue"] -= 1
            srclines = sec_acc + [""] + blk.body if sec_acc else list(blk.body)
            items.append((blk, srclines, False))
        else:                            # compile | run: standalone
            items.append((blk, list(blk.body), kind == "run"))
        sec_acc = sec_acc + [""] + blk.body
    return items, counts


def run_doctut(repo, ficus, jobs=None):
    if not os.path.exists(TUTORIAL):
        print("[doctut] doc/ficustut.md not present -- nothing to do")
        return 0
    blocks = extract_blocks(TUTORIAL)
    items, counts = plan_blocks(blocks)

    print(f"[doctut] {len(blocks)} blocks: "
          f"compile={counts['compile']} run={counts['run']} "
          f"fragment={counts['fragment']} shell={counts['shell']} "
          f"continue={counts['continue']} skip={counts['skip']}"
          + (f" ORPHAN-continue={counts['orphan']}" if counts["orphan"] else ""))

    results = F._run_pool(
        lambda blk, srclines, run: _check_block(ficus, blk, srclines, run),
        items, jobs or (os.cpu_count() or 4))
    tbl = F.print_table("doctut (tutorial examples)", results)
    rc = 1 if F.run_failed(tbl) else 0
    if counts["orphan"]:
        print(f"[doctut] WARNING: {counts['orphan']} 'continue' directive(s) "
              f"with no preceding checkable block")
        rc = 1
    return rc
