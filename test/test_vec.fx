/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// RRB rrbvec tests that check basic operaions:
// comprehensions, iteration, element and slice extraction, concatentation

from UTest import *
import Vec
import Vector

TEST("fcvector.access", fun()
{
    val v: int vector = []
    EXPECT_EQ(`size(v)`, 0)
    EXPECT_EQ(`empty(v)`, true)
    for i <- 0:5 { v.push_back(i*i) }
    EXPECT_EQ(`size(v)`, 5)
    EXPECT_EQ(`empty(v)`, false)
    for i <- 0:5 { EXPECT_EQ(`v[i]`, i*i) }
    EXPECT_EQ(`v.clip[100]`, 16)
    EXPECT_EQ(`v.clip[-100]`, 0)
    EXPECT_EQ(`v.zero[100]`, 0)
    EXPECT_EQ(`v.wrap[-1]`, 16)
    var sink = 0
    EXPECT_THROWS(fun() {sink = v[5]}, OutOfRangeError)
    ignore(sink)

    val s: string vector = []
    s.push_back("a"); s.push_back("bc"); s.push_back("def")
    EXPECT_EQ(`size(s)`, 3)
    EXPECT_EQ(`s[2]`, "def")
})

TEST("rrbvec.simple", fun()
{
    val N = 10
    val a = rrbvec([0, 1, 4, 9, 16, 25, 36, 49, 64, 81])
    var arr = [for i <- 0:N {i*i}]
    EXPECT_EQ(`size(a)`, 10)
    EXPECT_EQ(`a`, rrbvec(for i <- 0:N {i*i}))
    EXPECT_EQ(`a`, rrbvec(arr))
    EXPECT_EQ(`a[5]`, 25)
    EXPECT_EQ(`a[6:.-1][::-1] + a[:4]`, rrbvec([64, 49, 36, 0, 1, 4, 9]))
    //EXPECT_EQ(a.wrap[-2], 64)
})

TEST("rrbvec.comprehensions", fun()
{
    val N = 1000003
    val arr = [for i <- 0:N {i}]
    val vec = rrbvec([for i <- 0:N {float(i)}])
    EXPECT_EQ(`size(vec)`, size(arr))
    for x <- arr, y <- vec {
        EXPECT_EQ(`float(x)`, y)
        if float(x) != y {break}
    }
    val vec = rrbvec(for i <- 0:N {rrbvec([string(i)])})
    for x <- arr, y <- vec {
        EXPECT_EQ(`size(y)`, 1)
        val str_x = string(x), y_0 = y[0]
        EXPECT_EQ(`str_x`, y_0)
        if str_x != y_0 {break}
    }
})

TEST("rrbvec.concat_slice", fun()
{
    val rng = RNG(0xffffffffu64)
    val N = 1000003
    var i = 0
    var vec: float rrbvec = []
    while i < N {
        val j = rng.uniform(i, N+1)
        val added_vec = rrbvec(for k <- i:j {float(k)})
        vec += added_vec
        i = j
    }
    EXPECT_EQ(`size(vec)`, N)
    for x@i <- vec {
        val y = float(i)
        EXPECT_EQ(`x`, y)
        if x != y {break}
    }

    val ncuts = 1000
    val cuts = [for i <- 0:ncuts+1 {
        if i == 0 {0} else if i == ncuts {N} else {rng.uniform(0, N)}
    }]
    cuts.sort((<))
    val parts = rrbvec(for i <- 0:ncuts {vec[cuts[i]:cuts[i+1]]})
    var vec2: float rrbvec = []
    // test the reverse at once
    for part <- parts[::-1] { vec2 = part + vec2 }
    EXPECT_EQ(`size(vec2)`, N)
    for x@i <- vec2 {
        val y = float(i)
        EXPECT_EQ(`x`, `y`)
        if x != y {break}
    }
})

TEST("rrbvec.find", fun()
{
    val rng = RNG(0xffffffffu64)
    val N = 1000003
    val (simple_vec, complex_vec) = rrbvec(@unzip for i <- 0:N {(i, rrbvec([i]))})

    for i <- 0:10000 {
        val idx = rng.uniform(0, N)
        val big_idx = rng.uniform(-N, N*2)
        val clip_idx = max(min(big_idx, N-1), 0)
        val wrap_idx = (big_idx % N) + (if big_idx < 0 {N} else {0})
        val x = simple_vec[idx]
        val xv = complex_vec[idx]
        EXPECT_EQ(`x`, `idx`)
        EXPECT_EQ(`size(xv)`, 1)
        EXPECT_EQ(`xv[0]`, `idx`)
        EXPECT_EQ(`simple_vec.clip[big_idx]`, `simple_vec[clip_idx]`)
        EXPECT_EQ(`simple_vec.wrap[big_idx]`, `simple_vec[wrap_idx]`)
        EXPECT_EQ(`simple_vec.zero[big_idx]`, `if big_idx == clip_idx {big_idx} else {0}`)
    }
})

TEST("vec.binomial", fun()
{
    val emptyvec: float Vec.t = Vec.make(0)
    val myvecs: (float Vec.t) Vec.t = Vec.make(0)
    for i <- 1:10 {
        val last = if myvecs.empty() {emptyvec} else {myvecs.back()}
        val curr = last.mapi(fun(x, i) {
            if i == 0 {1.f} else {last.at(i-1) + last.at(i)}
        })
        curr.push_back(1.f)
        myvecs.push_back(curr)
    }

    fun sum(v: 't Vec.t, v0: 'r): 'r = v.foldl(fun (x, s) {x + s}, v0)
    for i <- 0:myvecs.size() {
        val expected_sum = double(1 << i)
        EXPECT_EQ(`sum(myvecs.at(i), 0.)`, expected_sum)
    }
})

TEST("vec.str", fun()
{
    val vecstr : string Vec.t = Vec.make(0)
    vecstr.push_back("a")
    vecstr.push_back("bc")
    vecstr.push_back("def")
    vecstr.push_back("ghij")
    vecstr.push_back("klmno")
    vecstr.push_back("pqrstu")
    vecstr.push_back("vwxyz")
    EXPECT_EQ(Vec.array(vecstr.slicefrom(0,2)), ["a", "def", "klmno", "vwxyz"])
    EXPECT_EQ(vecstr.slicefrom(0,2), Vec.make(["a", "def", "klmno", "vwxyz"]))
    EXPECT_EQ(Vec.array(vecstr.rev()), ["vwxyz", "pqrstu", "klmno", "ghij", "def", "bc", "a"])
    EXPECT_EQ(vecstr.rev(), Vec.make(["vwxyz", "pqrstu", "klmno", "ghij", "def", "bc", "a"]))
})
