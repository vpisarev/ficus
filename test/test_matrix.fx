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
     [ 2., 1., 0., 4.;
        0., 1., 1., 3.;
        1., 0., 1., 2.;
        2., 2., 0., 1. ]
    val B =
     [ 1., 0., 4., 4.;
        3., 1., 1., 3.;
        1., 0., 3., 2.;
        1., 1., 0., 1. ]

    EXPECT_NEAR(`A*B`, `refmul(A,B)`, 1e-5)
    EXPECT_NEAR(`A'*B`, `refmul(A',B)`, 1e-5)
    EXPECT_NEAR(`A*B'`, `refmul(A,B')`, 1e-5)
    EXPECT_NEAR(`A'*B'`, `refmul(A',B')`, 1e-5)

    val C =
     [ 2.f, 1.f, 0.f, 4.f;
        0.f, 1.f, 1.f, 3.f;
        1.f, 0.f, 1.f, 2.f;
        2.f, 2.f, 0.f, 1.f ]
    val D =
     [ 1.f, 0.f, 4.f, 4.f;
        3.f, 1.f, 1.f, 3.f;
        1.f, 0.f, 3.f, 2.f;
        1.f, 1.f, 0.f, 1.f ]

    EXPECT_NEAR(`C*D`, `refmul(C,D)`, 1e-5f)
    EXPECT_NEAR(`C'*D`, `refmul(C',D)`, 1e-5f)
    EXPECT_NEAR(`C*D'`, `refmul(C,D')`, 1e-5f)
    EXPECT_NEAR(`C'*D'`, `refmul(C',D')`, 1e-5f)
})

TEST("matrix.mul_big", fun() {
    val rng = RNG(0x12345u64)
    val M = 1007, N = 503, K = 185
    val A = rng.uniform((M, K), -1.f, 1.f)
    var At_ = A'
    val At = At_
    val B = rng.uniform((K, N), -1.f, 1.f)
    var Bt_ = B'
    val Bt = Bt_
    val ref_C = refmul(A,B)
    EXPECT_NEAR(`A*B`, `ref_C`, 1e-5f)
    EXPECT_NEAR(`At'*B`, `ref_C`, 1e-5f)
    EXPECT_NEAR(`A*Bt'`, `ref_C`, 1e-5f)
    EXPECT_NEAR(`At'*Bt'`, `ref_C`, 1e-5f)

    val Ad = rng.uniform((M, K), -1., 1.)
    var Adt_ = Ad'
    val Adt = Adt_
    val Bd = rng.uniform((K, N), -1., 1.)
    var Bdt_ = Bd'
    val Bdt = Bdt_
    val ref_Cd = refmul(Ad,Bd)
    EXPECT_NEAR(`Ad*Bd`, `ref_Cd`, 1e-5)
    EXPECT_NEAR(`Adt'*Bd`, `ref_Cd`, 1e-5)
    EXPECT_NEAR(`Ad*Bdt'`, `ref_Cd`, 1e-5)
    EXPECT_NEAR(`Adt'*Bdt'`, `ref_Cd`, 1e-5)
})

TEST("matrix.mul_ranges", fun() {
    val (h,w) = (10, 10)
    val rng = RNG(0xffffffffu64)
    val mothermat = random(rng, (h,w), -2., 2.)
    for y1 <- 1:h {
        for x1 <- 1:w {
            val A = mothermat[:y1,:x1]
            val B = (mothermat[h-y1:,w-x1:])'
            for y2 <- 0:(y1-1){
                for x2 <- 0:(x1-1){
                    val C = (A[y2:,x2:])'
                    val D = (B[:x1-x2,:y1-y2])'
                    EXPECT_NEAR(`C*D`, `refmul(C,D)`, 1e-5)
                    EXPECT_NEAR(`C'*D'`, `refmul(C',D')`, 1e-5)
                }
            }
        }
    }
})

TEST("matrix.mul_sparsed_ranges", fun() {
    val (h,w) = (10, 10)
    val rng = RNG(0xffffffffu64)
    val mothermat = random(rng, (h,w), -2., 2.)

    val A = mothermat[1:8:2,2:5]
    val B = mothermat[5:8,2:9:2]
    EXPECT_NEAR(`A*B`, `refmul(A,B)`, 1e-10)
    EXPECT_NEAR(`A'*B'`, `refmul(A',B')`, 1e-10)

    val C = mothermat[5:9,2:9:2]
    val D = mothermat[1:8:2,2:6]
    EXPECT_NEAR(`C*D'`, `refmul(C,D')`, 1e-10)

    for delta_outer <- 1:3 {
        val A = mothermat[1:9:delta_outer,:]
        val B = (mothermat[::delta_outer,:])'
        for delta_inner <- 1:2 {
            val C = A[1:3:delta_inner,:] //TODO: How to write subarraying with missed end? I tried A[1::delta_inner, but it doesn't work]
            val D = B[:,:3:delta_inner]
            EXPECT_NEAR(`C*D`, `refmul(C,D)`, 1e-10)
            }
        }

})
