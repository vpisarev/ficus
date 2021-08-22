/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Correctness tests for matrix operations

from UTest import *

fun refmul(a: 't [,], b: 't [,]) {
    val (ma, na) = size(a), (mb, nb) = size(b)
    assert(na == mb)
    val c = array((ma, nb), (0 :> 't))

    if ma*na*nb < (1<<20)
    {
        for i <- 0:ma for k <- 0:na {
            val alpha = a[i, k]
            for j <- 0:nb {
                c[i, j] += alpha*b[k, j]
            }
        }
    }
    else
    {
        @parallel for i <- 0:ma for k <- 0:na {
            val alpha = a[i, k]
            for j <- 0:nb {
                c[i, j] += alpha*b[k, j]
            }
        }
    }
    c
}

TEST("matrix.mul_squares", fun() {
    val A =
     [| 2., 1., 0., 4.;
        0., 1., 1., 3.;
        1., 0., 1., 2.;
        2., 2., 0., 1. |]
    val B =
     [| 1., 0., 4., 4.;
        3., 1., 1., 3.;
        1., 0., 3., 2.;
        1., 1., 0., 1. |]

    EXPECT_EQ(A*B, refmul(A,B))
    EXPECT_EQ(A'*B, refmul(A',B))
    EXPECT_EQ(A*B', refmul(A,B'))
    EXPECT_EQ(A'*B', refmul(A',B'))
})