#!/usr/bin/env python3
"""
update_compiler.py -- mechanize the Ficus bootstrap-regeneration ritual.

The compiler is self-hosted: `bin/ficus` is built by `ficus0` (compiled from the
pre-generated C in `compiler/bootstrap/*.c`) compiling the actual `compiler/*.fx`
sources.  After editing any `compiler/*.fx` the checked-in bootstrap C must be
refreshed so a fresh clone builds the updated compiler.  This script replaces the
hand-run recipe that used to live in CLAUDE.md, and verifies the self-hosting
*fixpoint* (the freshly-built compiler must regenerate its own C byte-for-byte).

Recipe, per the CLAUDE.md prose it supersedes:
    bin/ficus -O3 -o boot compiler/fx.fx     # regen C into __fxbuild__/boot/
    cp changed __fxbuild__/boot/*.c  ->  compiler/bootstrap/
    make                                     # rebuild ficus0+ficus from new C
    (regen again; expect 0 further diffs = fixpoint)

Modes:
    (default)   regen, copy changed modules over compiler/bootstrap/, rebuild,
                regen once more and ASSERT the fixpoint (0 further diffs).
    --check     dry run: report which modules WOULD change; exit 1 if any,
                0 if the bootstrap is already up to date.  (CI-friendly.)

Options:
    --no-make   skip the initial `make` (assume bin/ficus is already current).
                The post-copy rebuild is intrinsic to the fixpoint check and
                always runs.
    --jobs N    parallelism for make (default: os.cpu_count()).

Python 3, standard library only -- like fxtest, this tooling must not depend on
the compiler it manages.
"""

import argparse
import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)                       # tools/ -> repo root
FICUS = os.path.join(REPO, "bin", "ficus")
BOOTSTRAP = os.path.join(REPO, "compiler", "bootstrap")
FX_MAIN = os.path.join(REPO, "compiler", "fx.fx")
BOOT_NAME = "boot"                                 # -o boot
BOOT_BUILD = os.path.join(REPO, "__fxbuild__", BOOT_NAME)
BOOT_BIN = os.path.join(REPO, BOOT_NAME)           # the stray ./boot at repo root

# ANSI (only when stdout is a TTY)
_TTY = sys.stdout.isatty()
def _c(code, s): return f"\033[{code}m{s}\033[0m" if _TTY else s
def ok(s):   return _c("32;1", s)
def info(s): return _c("34;1", s)
def warn(s): return _c("33;1", s)
def err(s):  return _c("31;1", s)


def run(cmd, **kw):
    """Run a command in REPO, streaming its output; raise on non-zero."""
    kw.setdefault("cwd", REPO)
    print(info("$ " + " ".join(cmd)))
    subprocess.run(cmd, check=True, **kw)


def make(jobs):
    run(["make", f"-j{jobs}"])


def _cleanup_boot_bin():
    # `-o boot` drops the executable at the repo ROOT; don't let it litter.
    try:
        os.remove(BOOT_BIN)
    except FileNotFoundError:
        pass


def regen():
    """Regenerate the compiler's own C into __fxbuild__/boot/ and return that dir.

    The build dir is wiped first so the result never depends on stale cached
    products from a previous, differently-built compiler (the FB-008 incremental
    trap): this tool must be correct regardless of the build-cache stamp.
    """
    if not os.path.exists(FICUS):
        sys.exit(err(f"error: {FICUS} not found; run `make` first (or drop --no-make)"))
    shutil.rmtree(BOOT_BUILD, ignore_errors=True)
    run([FICUS, "-O3", "-o", BOOT_NAME, FX_MAIN])
    _cleanup_boot_bin()
    return BOOT_BUILD


def _read(path):
    with open(path, "rb") as f:
        return f.read()


def changed_modules(gen_dir):
    """List of (basename, gen_path) for generated .c that differ from the
    committed bootstrap (or are new).  Also sanity-checks the module sets match.
    """
    gen = {f for f in os.listdir(gen_dir) if f.endswith(".c")}
    boot = {f for f in os.listdir(BOOTSTRAP) if f.endswith(".c")}
    only_gen = sorted(gen - boot)
    only_boot = sorted(boot - gen)
    if only_gen:
        print(warn(f"  note: generated but not in bootstrap: {', '.join(only_gen)}"))
    if only_boot:
        print(warn(f"  note: in bootstrap but not generated: {', '.join(only_boot)}"))
    changed = []
    for name in sorted(gen & boot):
        if _read(os.path.join(gen_dir, name)) != _read(os.path.join(BOOTSTRAP, name)):
            changed.append((name, os.path.join(gen_dir, name)))
    # A brand-new generated module is also a change to be copied in.
    for name in only_gen:
        changed.append((name, os.path.join(gen_dir, name)))
    return changed


def copy_over(changed):
    for name, gen_path in changed:
        shutil.copyfile(gen_path, os.path.join(BOOTSTRAP, name))


def report_list(changed):
    if not changed:
        print(ok("  (no modules changed)"))
    else:
        for name, _ in changed:
            print(f"    {warn('CHANGED')}  {name}")


def main(argv=None):
    ap = argparse.ArgumentParser(
        prog="update_compiler.py",
        description="Regenerate compiler/bootstrap/*.c and verify the self-hosting fixpoint.")
    ap.add_argument("--check", action="store_true",
                    help="dry run: report modules that would change; exit 1 if any")
    ap.add_argument("--no-make", action="store_true",
                    help="skip the initial make (assume bin/ficus is current)")
    ap.add_argument("--jobs", type=int, default=os.cpu_count() or 4,
                    help="parallelism for make")
    args = ap.parse_args(argv)

    if not args.no_make:
        print(info("[1/…] building the compiler (make)"))
        make(args.jobs)
    elif not os.path.exists(FICUS):
        sys.exit(err(f"error: --no-make given but {FICUS} does not exist"))

    print(info("[regen] generating compiler C into __fxbuild__/boot/"))
    gen_dir = regen()
    changed = changed_modules(gen_dir)

    if args.check:
        print(info(f"[check] {len(changed)} module(s) would change:"))
        report_list(changed)
        if changed:
            print(err("bootstrap is STALE -- run tools/update_compiler.py to refresh it"))
            return 1
        print(ok("bootstrap is up to date."))
        return 0

    if not changed:
        print(ok("[done] bootstrap already at a clean fixpoint -- nothing to do."))
        return 0

    print(info(f"[update] copying {len(changed)} changed module(s) over compiler/bootstrap/:"))
    report_list(changed)
    copy_over(changed)

    print(info("[rebuild] rebuilding compiler from the updated bootstrap"))
    make(args.jobs)

    print(info("[fixpoint] regenerating again; expecting 0 further diffs"))
    gen_dir = regen()
    residual = changed_modules(gen_dir)
    if residual:
        print(err("FIXPOINT FAILED -- the self-hosted compiler does not reproduce "
                  "its own bootstrap. This is a determinism regression."))
        report_list(residual)
        print(err("Investigate with:  python3 tools/fxtest/fxtest.py determinism"))
        return 2

    print(ok(f"[done] bootstrap updated ({len(changed)} module(s)); fixpoint holds."))
    return 0


if __name__ == "__main__":
    sys.exit(main())
