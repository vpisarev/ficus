/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// RRB vector tests that check basic operaions:
// comprehensions, iteration, element and slice extraction, concatentation

from UTest import *

TEST("vector.simple", fun()
{
    val N = 10
    val a = vector([0, 1, 4, 9, 16, 25, 36, 49, 64, 81])
    var arr = [for i <- 0:N {i*i}]
    EXPECT_EQ(`size(a)`, 10)
    EXPECT_EQ(`a`, vector(for i <- 0:N {i*i}))
    EXPECT_EQ(`a`, vector(arr))
    EXPECT_EQ(`a[5]`, 25)
    EXPECT_EQ(`a[6:.-1][::-1] + a[:4]`, vector([64, 49, 36, 0, 1, 4, 9]))
    //EXPECT_EQ(a.wrap[-2], 64)
})

TEST("vector.comprehensions", fun()
{
    val N = 1000003
    val arr = [for i <- 0:N {i}]
    val vec = vector([for i <- 0:N {float(i)}])
    EXPECT_EQ(`size(vec)`, size(arr))
    for x <- arr, y <- vec {
        EXPECT_EQ(`float(x)`, y)
        if float(x) != y {break}
    }
    val vec = vector(for i <- 0:N {vector([string(i)])})
    for x <- arr, y <- vec {
        EXPECT_EQ(`size(y)`, 1)
        val str_x = string(x), y_0 = y[0]
        EXPECT_EQ(`str_x`, y_0)
        if str_x != y_0 {break}
    }
})

TEST("vector.concat_slice", fun()
{
    val rng = RNG(0xffffffffUL)
    val N = 1000003
    var i = 0
    var vec: float vector = []
    while i < N {
        val j = rng.uniform(i, N+1)
        val added_vec = vector(for k <- i:j {float(k)})
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
    val parts = vector(for i <- 0:ncuts {vec[cuts[i]:cuts[i+1]]})
    var vec2: float vector = []
    // test the reverse at once
    for part <- parts[::-1] { vec2 = part + vec2 }
    EXPECT_EQ(`size(vec2)`, N)
    for x@i <- vec2 {
        val y = float(i)
        EXPECT_EQ(`x`, `y`)
        if x != y {break}
    }
})

TEST("vector.find", fun()
{
    val rng = RNG(0xffffffffUL)
    val N = 1000003
    val (simple_vec, complex_vec) = vector(@unzip for i <- 0:N {(i, vector([i]))})

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
