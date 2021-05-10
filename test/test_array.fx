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

TEST("array.determinant", fun() {
    val A =
     [| 2., 1., 0., 4.;
        0., 1., 1., 3.;
        1., 0., 1., 2.;
        2., 2., 0., 1. |]
    val eps = 1e-10
    val I = diag(4, 1.)
    val As =
     [| \A[0,:];
        \A[1,:];
        \(A[0,:] + A[1,:]);
        \A[3,:] |]
    EXPECT_NEAR(det(A), -15., eps)
    EXPECT_NEAR(det(I), 1., eps)
    EXPECT_NEAR(det(As), 0., eps)
})
