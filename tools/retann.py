#!/usr/bin/env python3
"""annotate-2 helper: insert the return-type annotations that
-Wimplicit-rettype reports, using the compiler's own inferred type as the
oracle (Vadim's iterate-and-insert loop).

For every module-level function the compiler flags, the warning prints the
inferred return type. This tool re-spells that type into source syntax (the
only transform needed is stripping the `@NNN` gensym suffix -- both
`Date.t` and bare `t` parse inside their own module) and inserts `): <type>`
after the parameter list, reusing lint_op_returns' proven comment/string-safe
paren matcher.

CONCRETE inferred types only. Generic functions are reported as `<unknown>`
(their template body is not typechecked at the definition site) or contain a
type var `'t`; those need the WP-2 conventions and are SKIPPED + listed for
manual handling. Anonymous-record `{...}` returns are also skipped.

The compiler is the safety net: a wrong annotation fails typecheck, so this
tool cannot silently change behavior. Overload-resolution shifts are caught
by the -pr-resolve census (run separately after bootstrap-module edits).

Usage: python3 tools/retann.py <file.fx> [--dry]
"""
import re
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from lint_op_returns import strip_comments_keep_positions, scan_decls, FUN_RE

ROOT = Path(__file__).resolve().parent.parent
FICUS = ROOT / "bin" / "ficus"
WARN_RE = re.compile(
    r":(\d+):(\d+): warning: implicit return type of module-level function "
    r"'([^']+)' \(inferred: (.*)\)$"
)


def get_warnings(fx):
    out = subprocess.run(
        [str(FICUS), "-no-c", "-Wall", str(fx)],
        capture_output=True, text=True,
    ).stdout
    res = []
    for line in out.splitlines():
        m = WARN_RE.search(line)
        if m:
            res.append((int(m.group(1)), m.group(3), m.group(4)))
    return res


def clean_type(t):
    return re.sub(r"@\d+", "", t)


def is_concrete(t):
    # skip type vars (generic), <unknown> (uninferred template), anonymous
    # records `{...}`, and function types `->` (nullary `()` needs `void`, so
    # these are hand-spelled).
    return ("'" not in t and "<unknown>" not in t
            and "{" not in t and "->" not in t)


def main():
    fx = Path(sys.argv[1])
    if not fx.is_absolute():
        fx = ROOT / fx
    dry = "--dry" in sys.argv
    # --force TYPE: annotate EVERY still-flagged function with TYPE, overriding
    # the inferred-type oracle. For homogeneous families (e.g. all UTest
    # assertions return void) where the template return isn't inferred at the
    # definition site. Use only after verifying the family shares one type.
    force = None
    if "--force" in sys.argv:
        force = sys.argv[sys.argv.index("--force") + 1]
    # --spec FILE: FILE has one `<line> <type>` per line; insert <type> at the
    # decl on that source line. For heterogeneous files (Builtins) where the
    # type follows per-function rules the oracle can't infer (generic bodies).
    spec = None
    if "--spec" in sys.argv:
        spec = {}
        for ln in Path(sys.argv[sys.argv.index("--spec") + 1]).read_text().splitlines():
            ln = ln.strip()
            if not ln or ln.startswith("#"):
                continue
            num, typ = ln.split(None, 1)
            spec[int(num)] = typ
    raw = fx.read_text(encoding="utf-8")
    text = strip_comments_keep_positions(raw)
    by_line = {}
    for start, name, j, annotated, params in scan_decls(text, FUN_RE):
        lineno = text.count("\n", 0, start) + 1
        by_line.setdefault(lineno, []).append((name, j, annotated))

    inserts = []          # (offset, ": type")
    skipped = []          # (line, name, type, reason)
    for line, name, typ in get_warnings(fx):
        match = None
        for dl in (line, line - 1, line + 1, line - 2, line + 2):
            for (dname, j, annotated) in by_line.get(dl, []):
                if dname == name and not annotated:
                    match = j
                    break
            if match is not None:
                break
        if match is None:
            skipped.append((line, name, typ, "no-decl-match"))
            continue
        if spec is not None and line in spec:
            inserts.append((match, ": " + spec[line]))
            continue
        if spec is not None:
            skipped.append((line, name, typ, "not-in-spec"))
            continue
        if force is not None:
            inserts.append((match, ": " + force))
            continue
        if not is_concrete(typ):
            skipped.append((line, name, typ, "generic"))
            continue
        inserts.append((match, ": " + clean_type(typ)))

    inserts.sort(key=lambda x: -x[0])
    new = raw
    for off, ins in inserts:
        new = new[:off] + ins + new[off:]
    if not dry:
        fx.write_text(new, encoding="utf-8")
    print(f"{fx.relative_to(ROOT)}: inserted {len(inserts)}, "
          f"skipped {len(skipped)}")
    for line, name, typ, reason in skipped:
        print(f"  SKIP {reason:14s} L{line} {name} -> {typ}")


if __name__ == "__main__":
    main()
