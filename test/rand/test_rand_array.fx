/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// T5 randomized suite: array comprehensions / slicing / border access checked
// against straightforward handwritten-loop reference implementations.  Since
// these run under the T2 differential too, the optimized comprehension and the
// naive reference must agree at every optimization level.

from UTest import *
from RandUtil import *
import Vector

TEST("rand.array.map", fun() {
    val name = "rand.array.map"
    for i <- 0:300 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 0, 40)
        val a = rand_iarray(rng, n, -100, 100)
        val got = [for x <- a {x*2 + 1}]
        val rf = array(n, 0)
        for j <- 0:n {rf[j] = a[j]*2 + 1}
        if got != rf {println(f"  [repro] {name} case={i} seed={seed} n={n}")}
        EXPECT_EQ(got, rf)
    }
})

TEST("rand.array.zip", fun() {
    val name = "rand.array.zip"
    for i <- 0:300 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 0, 40)
        val a = rand_iarray(rng, n, -50, 50), b = rand_iarray(rng, n, -50, 50)
        val got = [for x <- a, y <- b {x - y}]
        val rf = array(n, 0)
        for j <- 0:n {rf[j] = a[j] - b[j]}
        if got != rf {println(f"  [repro] {name} case={i} seed={seed} n={n}")}
        EXPECT_EQ(got, rf)
    }
})

TEST("rand.array.comp2d", fun() {
    val name = "rand.array.comp2d"
    for i <- 0:200 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val m = next_int(rng, 1, 8), n = next_int(rng, 1, 8)
        val got = [for i2 <- 0:m for j2 <- 0:n {i2*100 + j2}]
        val rf = array((m, n), 0)
        for i2 <- 0:m {for j2 <- 0:n {rf[i2, j2] = i2*100 + j2}}
        if got != rf {println(f"  [repro] {name} case={i} seed={seed} m={m} n={n}")}
        EXPECT_EQ(got, rf)
    }
})

TEST("rand.array.slice", fun() {
    val name = "rand.array.slice"
    for i <- 0:400 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 1, 40)
        val a = rand_iarray(rng, n, -100, 100)
        val lo = next_int(rng, 0, n - 1)
        // hi drawn from [lo, n], so EMPTY strided slices a[lo:lo:step] are
        // exercised too (FB-004, fixed: an empty strided view is now safe to
        // build/compare/copy instead of segfaulting).
        val hi = next_int(rng, lo, n)
        val step = next_int(rng, 1, 4)
        val gc = a[lo:hi],       rc = [for k <- lo:hi {a[k]}]
        val gs = a[lo:hi:step],  rs = [for k <- lo:hi:step {a[k]}]
        if gc != rc || gs != rs {
            println(f"  [repro] {name} case={i} seed={seed} n={n} lo={lo} hi={hi} step={step}")
        }
        EXPECT_EQ(gc, rc)
        EXPECT_EQ(gs, rs)
    }
})

// Border access (.clip/.wrap/.zero) is exercised on BOTH a plain 't [] array
// (FB-003, fixed) and a Vector, over idx in [-3n, 3n) so wrapping is tested in
// both directions across multiple periods; the exact boundaries -n, -2n, 2n are
// forced in (FB-005: Vector/array .wrap[-n] used to read one past the end).
TEST("rand.array.border", fun() {
    val name = "rand.array.border"
    for i <- 0:600 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 1, 30)
        val a = rand_iarray(rng, n, -100, 100)
        val v = vector(a)
        // most cases random in [-3n, 3n); a few forced boundary values.
        val idx = match i % 8 {
            | 0 => -n | 1 => -2*n | 2 => 2*n | 3 => n
            | _ => next_int(rng, -3*n, 3*n - 1)
            }
        val cref = a[max(0, min(n - 1, idx))]           // clip
        val zref = if 0 <= idx < n {a[idx]} else {0}    // zero
        val w = ((idx % n) + n) % n                     // true Euclidean modulo
        val wref = a[w]                                 // wrap (always an element)
        // plain array (FB-003)
        if a.clip[idx] != cref || a.zero[idx] != zref || a.wrap[idx] != wref {
            println(f"  [repro] {name}[arr] case={i} seed={seed} n={n} idx={idx}")
        }
        EXPECT_EQ(a.clip[idx], cref)
        EXPECT_EQ(a.zero[idx], zref)
        EXPECT_EQ(a.wrap[idx], wref)
        // Vector (FB-005)
        if v.clip[idx] != cref || v.zero[idx] != zref || v.wrap[idx] != wref {
            println(f"  [repro] {name}[vec] case={i} seed={seed} n={n} idx={idx}")
        }
        EXPECT_EQ(v.clip[idx], cref)
        EXPECT_EQ(v.zero[idx], zref)
        EXPECT_EQ(v.wrap[idx], wref)
    }
})

// FB-006: nested comprehension (array-of-arrays) built then indexed. Compare
// aa[r][c] against the flat reference the inner values came from.
TEST("rand.array.nested_comp", fun() {
    val name = "rand.array.nested_comp"
    for it <- 0:200 {
        val seed = case_seed(name, it)
        val rng = mk_rng(seed)
        val m = next_int(rng, 1, 8)
        val n = next_int(rng, 1, 8)
        val flat = rand_iarray(rng, m*n, -1000, 1000)
        val aa = [for r <- 0:m {[for c <- 0:n {flat[r*n + c]}]}]
        var ok = true
        for r <- 0:m {
            for c <- 0:n {
                if aa[r][c] != flat[r*n + c] { ok = false }
            }
        }
        if !ok { println(f"  [repro] {name} case={it} seed={seed} m={m} n={n}") }
        EXPECT_EQ(ok, true)
    }
})

TEST("rand.array.fold", fun() {
    val name = "rand.array.fold"
    for i <- 0:300 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 0, 50)
        val a = rand_iarray(rng, n, -1000, 1000)
        val got = fold acc = 0 for x <- a {acc + x}
        var rf = 0
        for x <- a {rf += x}
        if got != rf {println(f"  [repro] {name} case={i} seed={seed} n={n}")}
        EXPECT_EQ(got, rf)
    }
})
