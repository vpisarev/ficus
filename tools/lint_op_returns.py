#!/usr/bin/env python3
"""Lint: generic stdlib operators must annotate their return type.

Why (see docs/found_bugs.md FB-007, the hotfix-1 near-miss): a generic
`operator` whose return type is left to inference returns a free type var
that unifies with ANY expected type, so the candidate stays viable at
under-constrained call sites (free fold/recursion accumulators) and can
steal them from the intended candidate via the env-order fallback.
Dropping `: 't complex` from Complex.fx's operators broke the compiler's
own build (C_gen_code.fx:1328). The return annotation is a load-bearing
VIABILITY FILTER: annotate the exact type when the signature pins it, or a
fresh-var form (`: 't3 complex`, `: ('t3 ...)`, `: 't3 [+]`) meaning "some
complex / some tuple / some array".

Scans .fx sources (default: lib/) for `operator` declarations and reports
those whose signature has no `): <type>` annotation. Pure stdlib Python,
no dependency on the compiler (same rule as tools/fxtest).

Exit code: 0 if clean, 1 if violations found (CI-friendly).
"""
import argparse
import re
import sys
from pathlib import Path

# an operator declaration, possibly prefixed by attributes (@inline etc.)
DECL_RE = re.compile(r"^[ \t]*(?:@\w+[ \t]+)*operator\s", re.M)
# a function declaration (for --funs mode); name captured for overload grouping
FUN_RE = re.compile(r"^[ \t]*(?:@\w+[ \t]+)*fun[ \t]+([A-Za-z_]\w*)[ \t]*\(", re.M)


def strip_comments_keep_positions(text: str) -> str:
    """Blank out /* */ and // comments and string/char literals, preserving
    offsets and newlines, so paren matching cannot be confused."""
    out = list(text)
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if c == "/" and i + 1 < n and text[i + 1] == "*":
            depth, j = 1, i + 2
            while j < n and depth:
                if text[j] == "/" and j + 1 < n and text[j + 1] == "*":
                    depth += 1
                    j += 2
                elif text[j] == "*" and j + 1 < n and text[j + 1] == "/":
                    depth -= 1
                    j += 2
                else:
                    j += 1
            for k in range(i, min(j, n)):
                if out[k] != "\n":
                    out[k] = " "
            i = j
        elif c == "/" and i + 1 < n and text[i + 1] == "/":
            j = text.find("\n", i)
            j = n if j < 0 else j
            for k in range(i, j):
                out[k] = " "
            i = j
        elif c == '"' or c == "'":
            # 'x' char literals and type vars 't both start with '.
            # A type var has no closing quote on the same "token": treat '
            # followed by alnum and NOT closed within 2 chars as a tyvar.
            if c == "'" and i + 1 < n and (text[i + 1].isalpha() or text[i + 1] == "_"):
                if not (i + 2 < n and text[i + 2] == "'"):
                    i += 1
                    continue
            # the transpose operator: `operator ' (a: ...)` -- a lone ' not
            # followed by a closing ' two chars later is not a char literal
            if c == "'" and i + 2 < n and text[i + 2] != "'":
                i += 1
                continue
            j = i + 1
            while j < n and text[j] != c:
                j += 2 if text[j] == "\\" else 1
            for k in range(i + 1, min(j, n)):
                if out[k] != "\n":
                    out[k] = " "
            i = j + 1
        else:
            i += 1
    return "".join(out)


def scan_decls(text: str, regex):
    """Yield (start, name_or_None, sig_end, annotated, params) per declaration."""
    n = len(text)
    pos = 0
    while True:
        m = regex.search(text, pos)
        if not m:
            break
        name = m.group(1) if m.groups() else None
        lp = text.find("(", m.end() - (1 if m.groups() else 0))
        if lp < 0:
            break
        depth, j = 1, lp + 1
        while j < n and depth:
            if text[j] == "(":
                depth += 1
            elif text[j] == ")":
                depth -= 1
            j += 1
        k = j
        while k < n and text[k] in " \t\n":
            k += 1
        annotated = k < n and text[k] == ":"
        yield m.start(), name, j, annotated, text[lp:j]
        pos = j


def check_file(path: Path, funs=False):
    raw = path.read_text(encoding="utf-8")
    text = strip_comments_keep_positions(raw)
    violations = []   # operators: always flagged when unannotated
    fun_decls = []    # (name, lineno, sig, annotated, generic)
    for start, _, j, annotated, _ in scan_decls(text, DECL_RE):
        if not annotated:
            lineno = text.count("\n", 0, start) + 1
            violations.append((lineno, " ".join(raw[start:j].split())))
    if funs:
        for start, name, j, annotated, params in scan_decls(text, FUN_RE):
            lineno = text.count("\n", 0, start) + 1
            generic = "'" in params
            fun_decls.append((name, lineno, " ".join(raw[start:j].split()),
                              annotated, generic))
    return violations, fun_decls


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("paths", nargs="*", default=["lib"],
                    help="files or directories to scan (default: lib)")
    ap.add_argument("--funs", action="store_true",
                    help="also flag GENERIC `fun`s without a return annotation "
                         "whose name is overloaded (declared 2+ times in the "
                         "scanned set) -- the same capture risk as operators")
    args = ap.parse_args()
    root = Path(__file__).resolve().parent.parent
    files = []
    for p in args.paths:
        p = (root / p) if not Path(p).is_absolute() else Path(p)
        if p.is_dir():
            files += sorted(p.rglob("*.fx"))
        else:
            files.append(p)
    total = 0
    all_funs = []  # (file, name, lineno, sig, annotated, generic)
    for f in files:
        violations, fun_decls = check_file(f, funs=args.funs)
        for lineno, sig in violations:
            print(f"{f.relative_to(root)}:{lineno}: no return-type annotation: {sig}")
            total += 1
        for name, lineno, sig, annotated, generic in fun_decls:
            all_funs.append((f, name, lineno, sig, annotated, generic))
    if args.funs:
        counts = {}
        for _, name, *_ in all_funs:
            counts[name] = counts.get(name, 0) + 1
        for f, name, lineno, sig, annotated, generic in all_funs:
            if counts[name] >= 2 and generic and not annotated:
                print(f"{f.relative_to(root)}:{lineno}: overloaded generic fun "
                      f"without return annotation: {sig}")
                total += 1
    if total:
        print(f"\n{total} unannotated declaration(s). The return annotation is "
              f"a viability filter (FB-007 near-miss); annotate the exact type "
              f"or a fresh-var form like ': 't9 [+]' / ': ('t9 ...)'.")
        return 1
    print("lint_op_returns: clean")
    return 0


if __name__ == "__main__":
    sys.exit(main())
