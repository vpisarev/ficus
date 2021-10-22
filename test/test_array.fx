/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// btree.fx example converted into a test. Binary tree traversal

from UTest import *

TEST("array.stat", fun() {
    val arr = [| 1, 2, 3, 4, 5 |]
    EXPECT_EQ(sum(arr), double(1+2+3+4+5))
    EXPECT_NEAR(mean(arr), double(1+2+3+4+5)/5, 1e-5)
    EXPECT_NEAR(normL2(arr), 7.416198487095663, 1e-5)
    EXPECT_EQ(normInf(arr), 5.)
})

TEST("array.solve", fun() {
    val A =
     [| 2., 1., 0., 4.;
        0., 1., 1., 3.;
        1., 0., 1., 2.;
        2., 2., 0., 1. |]
    val I = diag(4, 1.)
    val b = [| 4.; 5.; 6.; 7. |]
    val x = A\b
    val Ainv = A\1
    EXPECT_NEAR(A*x, b, 1e-10)
    EXPECT_NEAR(A*Ainv, I, 1e-10)
})

TEST("array.tuple_index", fun() {
    val (h,w,d) = (10, 10, 10)
    val rng = RNG(0xffffffffUL)
    val A1 = random(rng, (h), -2., 2.)
    val B1 = random(rng, (h), -2., 2.)
    val A2 = random(rng, (h,w), -2., 2.)
    val B2 = random(rng, (h,w), -2., 2.)
    val A3 = random(rng, (h,w,d), -2., 2.)
    val B3 = random(rng, (h,w,d), -2., 2.)

    fun uniadd(A,B) { //TODO: Inefficient, must be implemented more precise. All of these functions.
        var res = A
        for x@idx <- A, y <- B { res[idx] = x + y}
        res
    }

    fun add1(A,B) {
        var res = A
        for x@idx <- A, y <- B { 
            val i:int = idx
            res[i] = x + y}
        res
    }

    fun add2(A,B) {
        var res = A
        for x@idx <- A, y <- B { 
            val (i, j) = idx
            res[i, j] = x + y}
        res
    }

    fun add3(A,B) {
        var res = A
        for x@idx <- A, y <- B { 
            val (i, j, k) = idx
            res[i, j, k] = x + y}
        res
    }

    EXPECT_EQ(uniadd(A1,B1), add1(A1,B1))
    EXPECT_EQ(uniadd(A2,B2), add2(A2,B2))
    val C3 = uniadd(A3,B3)
    val C3r = add3(A3,B3)
    for layer_num <- 0:h { //TODO: There is no Arr[,,] -> string conversion, so EXPECT_EQ cannot work with 3D arrays.
        EXPECT_EQ(C3[layer_num,:,:], C3r[layer_num,:,:])
    }
    
})
