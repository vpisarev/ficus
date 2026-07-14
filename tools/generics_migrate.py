#!/usr/bin/env python3
"""generics-1 migrator (SINGLE-USE; removed at the flip).

Rewrites a .fx file from the old ML-style 't generic notation into the new
bracketed uppercase notation (list[T], Map.t[K,V], ref[T], `fun f[T,R](...)`,
`type name[K,V] = ...`). The compiler itself supplies the edits: parsing under
`-pr-generics-sites` emits, for the file under test, one `GSITE` record per
source span to change (types carry no source location -- FB-020 -- so the parser
that consumed the tokens is the only thing that knows the spans). This script
just applies those textual edits.

Edit kinds (columns are 1-based, spans half-open [col0, col1)):
  ANNO       replace a type-annotation span with new-notation text
  FUNPARAMS  insert a `[T,...]` list at a zero-width point (function name end)
  TYPEHEAD   replace `('k,'d) name` (prefix params + name) with `name[K,D]`

Usage:
  generics_migrate.py FILE...                 # dry run: print a unified diff
  generics_migrate.py --apply FILE...         # rewrite in place
  generics_migrate.py --check FILE...         # apply to a temp, assert the
                                              # -pr-ast type structure is
                                              # unchanged (notation erases in
                                              # typ_t); combine with --apply to
                                              # only write when the check passes
"""
import os, sys, subprocess, tempfile, difflib, re

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FICUS = os.path.join(ROOT, "bin", "ficus")


def run_sites(path):
    """Return the GSITE edit records the compiler emits for `path`."""
    r = subprocess.run([FICUS, "-no-c", "-pr-generics-sites", path],
                       capture_output=True, text=True, cwd=ROOT)
    ap = os.path.abspath(path)
    edits = []
    for ln in r.stdout.splitlines():
        if not ln.startswith("GSITE\t"):
            continue
        _, fname, l0, c0, l1, c1, kind, *rest = ln.split("\t")
        if os.path.abspath(fname) != ap:
            continue           # sites from imported modules -- skip
        text = rest[0] if rest else ""
        edits.append((int(l0), int(c0), int(l1), int(c1), kind, text))
    return edits, r.stdout, r.stderr


def line_offsets(src):
    """Absolute char offset at the start of each 1-based line."""
    offs = [0, 0]  # offs[1] = start of line 1
    for ch_i, ch in enumerate(src):
        if ch == "\n":
            offs.append(ch_i + 1)
    return offs


def apply_edits(src, edits):
    offs = line_offsets(src)

    def pos(line, col):
        return offs[line] + (col - 1)

    spans = []
    for (l0, c0, l1, c1, kind, text) in edits:
        a, b = pos(l0, c0), pos(l1, c1)
        if a > b:
            a, b = b, a
        spans.append((a, b, kind, text))
    # A nested type records both an outer annotation and its inner parts: a
    # variant case with a RECORD payload records the whole record AND each field.
    # Drop the CONTAINER and keep the inner (leaf) edits -- rewriting only the
    # field TYPES leaves the record braces, field names, ';' and any field
    # DEFAULT VALUES (which typ2str_new does not reproduce) untouched.
    def contains(x, y):
        return x[0] <= y[0] and y[1] <= x[1] and (x[0], x[1]) != (y[0], y[1])
    keep = []
    for i, e in enumerate(spans):
        if any(i != j and contains(e, f) for j, f in enumerate(spans)):
            continue                       # e strictly contains another edit -> drop
        keep.append(e)
    # remaining edits are disjoint (leaves + non-overlapping); apply end-to-start
    keep.sort(key=lambda s: s[0], reverse=True)
    out = src
    prev = len(src) + 1
    for (a, b, kind, text) in keep:
        if b > prev:
            raise RuntimeError(f"unexpected overlapping leaf edit at [{a},{b}) kind={kind}")
        out = out[:a] + text + out[b:]
        prev = a
    return out


def norm_ast(path):
    """-pr-ast0 (post-PARSE, pre-typecheck: no use-dependent template instances)
    of `path`, normalized so the notation change is invisible. typ2str prints
    BOTH notations in the old spelling (`t list`), so after we strip gensyms,
    tyvar apostrophes, the per-function `template<...>` header (functions collect
    their params only during typecheck -- empty for the old form, [T] for the
    migrated form) and case-fold, identical typ_t compares equal."""
    r = subprocess.run([FICUS, "-no-c", "-pr-ast0", path],
                       capture_output=True, text=True, cwd=ROOT)
    s = r.stdout
    s = re.sub(r"@\d+", "", s)               # gensym ids
    s = s.replace("'", "")                    # tyvar apostrophes
    s = re.sub(r"template<[^>]*>\s*", "", s)  # per-decl template header
    s = re.sub(r"^.*\.fx:.*$", "", s, flags=re.M)   # module header line (path)
    s = s.lower()                             # T <-> t
    s = re.sub(r"\s+", "", s)                 # drop ALL whitespace: the pretty-
    # printer wraps by column and spaces punctuation by context, and the migrated
    # form is a few chars longer, so line breaks and `})` vs `} )` spacing drift
    # without any type change -- the token stream is what must match.
    return s, r.returncode, r.stderr


def check_preserves_ast(path, migrated_text):
    """Compare the type structure before/after using the SAME path (so the module
    identity, imports and gensym context are identical -- only the notation
    differs). Migrates in place transiently, then always restores the original;
    --apply writes the final version afterwards."""
    with open(path, encoding="utf-8") as f:
        orig = f.read()
    before, rc0, err0 = norm_ast(path)
    if rc0 != 0:
        return False, f"original does not compile:\n{err0}"
    try:
        with open(path, "w", encoding="utf-8") as f:
            f.write(migrated_text)
        after, rc1, err1 = norm_ast(path)
    finally:
        with open(path, "w", encoding="utf-8") as f:
            f.write(orig)
    if rc1 != 0:
        return False, f"migrated does not compile:\n{err1}"
    if before != after:
        diff = "\n".join(difflib.unified_diff(
            before.splitlines(), after.splitlines(),
            "before", "after", lineterm=""))
        return False, "AST type structure changed:\n" + diff
    return True, ""


def migrate_file(path, apply, check):
    edits, _, err = run_sites(path)
    if not edits:
        print(f"{path}: no generic sites")
        return True
    with open(path, encoding="utf-8") as f:
        src = f.read()
    migrated = apply_edits(src, edits)
    if migrated == src:
        print(f"{path}: no change")
        return True
    if check:
        ok, msg = check_preserves_ast(path, migrated)
        if not ok:
            print(f"{path}: CHECK FAILED -- {msg}")
            return False
        print(f"{path}: check OK ({len(edits)} edits)")
    if apply:
        with open(path, "w", encoding="utf-8") as f:
            f.write(migrated)
        print(f"{path}: migrated ({len(edits)} edits)")
    elif not check:
        diff = difflib.unified_diff(
            src.splitlines(), migrated.splitlines(),
            path + " (old)", path + " (new)", lineterm="")
        print("\n".join(diff))
    return True


def main(argv):
    apply = "--apply" in argv
    check = "--check" in argv
    files = [a for a in argv if not a.startswith("--")]
    if not files:
        print(__doc__)
        return 2
    ok = True
    for f in files:
        ok &= migrate_file(f, apply, check)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
