# ccache setup for the Ficus dev box — handoff

Short note so a future session (or a fresh machine) can reproduce and reason
about the ccache configuration used while developing Ficus (and OpenCV) on this
Linux box.

## TL;DR

ccache is enabled **globally via a PATH masquerade**, not via per-project
config. Every build that shells out to `cc`/`gcc`/`g++`/`c++` — Ficus, OpenCV
cmake/make, hand-run compiles — transparently goes through ccache.

## What was configured

1. **PATH masquerade** (the actual enabler). The distro package ships
   `/usr/lib/ccache/` full of symlinks (`cc`, `gcc`, `g++`, `c++`, versioned
   variants) that all point at the ccache binary. A guarded block at the end of
   `~/.bashrc` prepends that dir to `PATH`:

   ```sh
   # ccache: route cc/gcc/g++/c++ through ccache via the masquerade dir.
   if [ -d /usr/lib/ccache ] && [[ ":$PATH:" != *":/usr/lib/ccache:"* ]]; then
       export PATH="/usr/lib/ccache:$PATH"
   fi
   ```

   Idempotent (won't duplicate on re-source) and a no-op if ccache is removed.
   On Ubuntu `~/.profile` sources `~/.bashrc`, so this covers both login and
   interactive shells. **Not** installed system-wide in `/etc/profile.d/`
   (that needs sudo); the `.bashrc` route already covers this user for Ficus +
   OpenCV.

2. **`~/.config/ccache/ccache.conf`**:
   - `max_size = 20 GB`  (disk is a ~3 TB Linux partition, plenty of room)
   - `compiler_check = content`  (hash the compiler binary; robust even if a
     rebuilt compiler keeps the same mtime — safer than the default `mtime`)
   - `cache_dir` left at default `~/.cache/ccache`

## Why it works with Ficus specifically

Ficus compiles its generated C/C++ by invoking `cc`/`c++` looked up on `PATH`
(`Sys.getenv("CC", "cc")` / `getenv("CXX", "c++")` in `compiler/Compiler.fx`
~L399). With the masquerade dir first on `PATH`, those resolve to ccache — no
Ficus/Makefile change needed. (`GNUmakefile` `CC := cc -fwrapv ...` likewise
resolves `cc` through the masquerade.)

Generated C is **deterministic** (a clean build reproduces byte-for-byte, see
CLAUDE.md), so ccache's direct mode hits reliably on rebuilds where the `.fx`
source didn't change.

## Measured impact (this box, `-j8`)

| Scenario                                   | Cold cache | Warm cache | Speedup |
|--------------------------------------------|-----------:|-----------:|--------:|
| `make clean && make -j8` (build Ficus)     |    39.4 s  |     9.4 s  |  ×4.2   |
| `fxtest all` after `rm -rf build/fxtest`   |    22.1 s  |    12.3 s  |  ×1.8   |

The harness gains less because most of its wall time is Ficus **execution**
(parse/typecheck/codegen) plus running the compiled test programs — neither is
cacheable by ccache. Only the C-compile step is cached. The Ficus build itself
is dominated by C compilation, hence the big win.

**Workflow relevance:** after editing any `compiler/*.fx` you must
`rm -rf build/fxtest` (stale-`.o` trap; see CLAUDE.md "Build/run & measurement
traps"). Previously that forced a full C recompile of every test; now ccache
serves the unchanged modules' objects from cache.

## Operating notes

- Activate in an already-open shell: `source ~/.bashrc` (or
  `export PATH="/usr/lib/ccache:$PATH"`). New terminals get it automatically.
- Verify routing: `command -v cc` → `/usr/lib/ccache/cc`.
- Stats / maintenance: `ccache -s` (stats), `ccache -z` (zero stats before a
  before/after measurement), `ccache -C` (clear the cache), `ccache -p` (dump
  effective config).
- ccache is content-hash based and safe by construction; a wrong-object hazard
  would require a compiler-binary or flags change it can't see — `compiler_check
  = content` closes the compiler-binary side of that.
- OpenCV's cmake auto-detects ccache (`ENABLE_CCACHE=ON`) independently; the
  masquerade additionally covers any non-cmake / manual compiles.
