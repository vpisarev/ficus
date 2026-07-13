# fuse-move-1 report — FB-028 (fusion move hazard) + fusion in function bodies

Branch `fuse-move-1` off master (post-`purity-1`). One commit (`1045983`). This
closes the open item flagged in `docs/purity1_report.md` (the K_fuse_loops
STOP-rule finding) and, in the course of making it testable, fixes a second
long-standing limitation of the fusion pass.

Green on the full ladder (`fxtest.py all`), `sanitize` (ASan+UBSan),
`determinism`, and the self-hosting fixpoint (`update_compiler.py`).

## FB-028 — loop fusion moves a mutable-memory read past the consumer's stores

`K_fuse_loops` takes a single-use comprehension consumed by exactly one for-loop

```
val tmp = [for i <- A { body(i) }]
for x <- tmp { bar(x) }
```

and REPLAYS the body inside the consumer:

```
for i <- A { val x = body(i); bar(x) }
```

Unfused, `tmp` is fully materialized first, so every `body(i)` reads the source
in its original state. Fused, each `body(i)` runs inside the consumer loop, so if
`bar` writes memory that `body` reads, later reads see earlier writes. This is
FB-023's hazard (a read of mutable memory moved past an aliasing store), now in
the fusion pass instead of single-use-temp inlining.

**Repro — an in-place 3-tap smoothing filter** (`test/test_array.fx`
`array.fuse_inplace_stencil`):

```
val arr = [30, 60, 90, 120, 150]
val n = size(arr)
val smoothed = [for i <- 0:n { (arr.clip[i-1] + arr.clip[i] + arr.clip[i+1]) / 3 }]
var k = 0
for s <- smoothed { arr[k] = s; k += 1 }   // write-back in place
```

Unfused (O0): `[40, 60, 90, 120, 140]`. Fused (O3): `[40, 63, 91, 120, 140]` —
the write-back at index `i` corrupts the neighbour a later tap reads.

**Post-purity-1 reachability.** A plain `arr[i]` read is a `BorderNone` `KExpAt`,
which purity-1 already made impure (throwing) → it never was a fusion candidate.
The hazard survives ONLY through a NON-throwing mutable read: a border read
(`.clip`/`.wrap`/`.zero`), a `*ref` deref, or a mutable record field — exactly the
class the movement grade governs.

**Fix.** The fusion criterion now asks `pure_kexp` for its movement grade:

```
K_remove_unused.pure_kexp(body, mut_read_is_impure=true)
```

so a body that reads mutable memory is not a fusion candidate (bodies reading
only immutable data still fuse). Conservative — it blocks fusion even when the
consumer provably doesn't alias the read — but the alias analysis that would be
needed to be precise is not worth it: measured **zero corpus fusion loss** from
the flip alone.

## Driver fix — fusion now fires inside function and lambda bodies

Making the bug observable from a UTest test surfaced a second problem: **fusion
never fired inside a function or lambda body.**

Root cause: `fuse_loops_all` ran the pass only on each module's top-level code
(`km_top`). The recursive descent into nested blocks lived inside `fuse_kexp_`,
which only ran once the *top-level* gate `fuse_loops` passed (`≥1 map`, `≥2
map+for`), and `KDefFun` was not counted toward that gate. So a module whose top
level is only function definitions — **every UTest file, and essentially all
modular (non-script) code** — got no fusion at all. Fusion effectively only
helped top-level script code (or code inlined into it).

The naive fix (remove the gate, always recurse) is a trap: `fuse_kexp_` did
`walk_kexp(e)` **and then** `fuse_loops(elist)` for a `KExpSeq`, and `walk_kexp`
already descends into the same elements — so together they traverse every block
twice, i.e. **2^depth** times overall. The gate had been masking this by
short-circuiting almost always. (This is the ~100× blow-up to avoid.)

Two changes give single-pass fusion of every block:

1. **Killed `fuse_loops` (the gate).** `fuse_loops_all` and the `KExpSeq` arm call
   `fuse_loops_` directly — every block is analysed.
2. **`fuse_kexp_` handles `KExpSeq` via `fuse_loops_(elist)` directly, with NO
   preceding `walk_kexp`.** Non-`KExpSeq` nodes still `walk_kexp` (to recurse into
   their sub-blocks) and then match. Each block is now visited exactly once.

## Measurements

| aspect | result |
|--------|--------|
| corpus C (`test_all`) | **25 modules newly fuse** (test/NN/etc.) — fusion now reaches modular code |
| benchmark C (spectralnorm / nbody / mandelbrot / btree) | **byte-identical** to baseline → perf unchanged (spectralnorm N=5500 0.86 s both) |
| compiler front-end | `-no-c -O3` on `fx.fx`: 7.81 s → **8.15 s (+4.5%)** — the cost of analysing every block; no 2^depth blow-up |
| bootstrap fixpoint | holds; only `K_fuse_loops.c` regenerates |
| ladder / sanitize / determinism | all green |

So: in trying to DISABLE fusion in unsafe places we ENABLED it in safe ones — a
net codegen improvement (fusion finally helps library/modular code), with the
movement grade keeping the newly-reachable fusion sound.

## Tests (`test/test_array.fx`)

- **`array.fuse_inplace_stencil`** (negative) — the smoothing repro. VALIDATED as a
  real guard: temporarily reverting the criterion to the removal grade and
  rebuilding makes it **FAIL at O3** (`[40,63,91,120,140]`); with the fix it
  passes. Observable from a UTest only because of the driver fix (the test's
  closure is now itself a fusion site).
- **`array.fuse_map_reduce`** (positive) — a comprehension fused into a reduce
  (`sum(i*i)`), and a range comprehension consumed by a product loop. Confirmed a
  fusion site via `-pr-k` (the `squares`/`evens` arrays disappear between `-pr-k0`
  and `-O3 -pr-k`). Guards fusion's correctness where it fires.

## Registry

`docs/found_bugs.md`: FB-028 added, `FIXED — fuse-move-1`, with the repro, the
movement-grade fix, and the driver-fix write-up. Supersedes the open STOP-rule
item in `docs/purity1_report.md`.
