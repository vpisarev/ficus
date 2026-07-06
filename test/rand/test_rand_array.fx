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
        // hi > lo: FB-004 makes an EMPTY strided slice a[lo:lo:step] crash on
        // use, so we keep strided ranges non-empty (still non-degenerate cover).
        val hi = next_int(rng, lo + 1, n)
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

// Border access (.clip/.wrap/.zero) is exercised on a Vector: FB-003 makes the
// same access on a plain 't [] array emit broken C, so we use the type on which
// the feature is supported and compare against explicit index-clamping on the
// underlying plain array.
TEST("rand.array.border", fun() {
    val name = "rand.array.border"
    for i <- 0:500 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val n = next_int(rng, 1, 30)
        val a = rand_iarray(rng, n, -100, 100)
        val v = vector(a)
        // Domain [-(n-1), 2n-1]: a single wrap, excluding idx == -n which
        // triggers FB-005 (Vector.wrap[-n] reads one past the buffer end).
        val idx = next_int(rng, -(n - 1), 2*n - 1)
        val gc = v.clip[idx], rc = a[max(0, min(n - 1, idx))]
        val gz = v.zero[idx], rz = if 0 <= idx < n {a[idx]} else {0}
        // wrap reference matches Vector's contract: w = idx%n + (n if idx<0),
        // with a zero fallback at the exact boundary idx == -n (w == n).
        val w = (idx % n) + (if idx < 0 {n} else {0})
        val gw = v.wrap[idx], rw = if 0 <= w < n {a[w]} else {0}
        if gc != rc || gz != rz || gw != rw {
            println(f"  [repro] {name} case={i} seed={seed} n={n} idx={idx}")
        }
        EXPECT_EQ(gc, rc)
        EXPECT_EQ(gz, rz)
        EXPECT_EQ(gw, rw)
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
