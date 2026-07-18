/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// btree.fx example converted into a test. Binary tree traversal

from UTest import *
import Rrbvec

// FB-003 / FB-005: border access (.clip/.wrap/.zero) on the four supported
// containers x the three modes. Plain 1D arrays used to emit broken C
// (__idx0__/`))` in FX_PTR_1D_*), string .wrap was never wired
// (std_FX_STR_ELEM_WRAP unregistered), and .wrap[-n] read one past the end.
TEST("array.border_matrix", fun() {
    // plain 1D array
    val a = [10, 20, 30, 40, 50]
    EXPECT_EQ_(a.clip[7], 50); EXPECT_EQ_(a.clip[-3], 10)
    EXPECT_EQ_(a.zero[7], 0);  EXPECT_EQ_(a.zero[2], 30)
    EXPECT_EQ_(a.wrap[7], 30); EXPECT_EQ_(a.wrap[-1], 50)
    EXPECT_EQ_(a.wrap[-5], 10)          // idx == -n boundary (was FB-005)
    EXPECT_EQ_(a.wrap[-10], 10)         // multiple wraps
    // plain 2D array (2x3): 0 1 2 / 3 4 5
    val m = [for i <- 0:2 for j <- 0:3 {i*3 + j}]
    EXPECT_EQ_(m.clip[5, 5], 5); EXPECT_EQ_(m.clip[-1, -1], 0)
    EXPECT_EQ_(m.zero[9, 9], 0); EXPECT_EQ_(m.zero[1, 2], 5)
    EXPECT_EQ_(m.wrap[-1, -1], 5); EXPECT_EQ_(m.wrap[2, 3], 0)
    // Vector
    val v = rrbvec(a)
    EXPECT_EQ_(v.clip[7], 50); EXPECT_EQ_(v.zero[7], 0)
    EXPECT_EQ_(v.wrap[-5], 10); EXPECT_EQ_(v.wrap[-1], 50)
    // string (char sequence)
    val s = "abcde"
    EXPECT_EQ_(s.clip[7], 'e'); EXPECT_EQ_(s.zero[2], 'c')
    EXPECT_EQ_(s.wrap[-1], 'e'); EXPECT_EQ_(s.wrap[-5], 'a')
})

// FB-004: an empty strided slice a[lo:lo:step] (step>=2) built a corrupt view
// and segfaulted on any use. It must now be a well-formed size-0 array.
TEST("array.empty_slice", fun() {
    val a = [1, 2, 3, 4, 5]
    EXPECT_EQ_(size(a[0:0:2]), 0)          // empty at start
    EXPECT_EQ_(size(a[2:2:2]), 0)          // empty in the middle
    EXPECT_EQ_(size(a[5:5:2]), 0)          // lo == hi == n
    EXPECT_EQ_(size(a[1:1:9]), 0)          // step > len
    EXPECT_EQ_(size(a[4:4:-2]), 0)         // empty, negative step
    // a size-0 view is safe to compare / iterate
    EXPECT_EQ_(a[2:2:2] == [for k <- 2:2:2 {a[k]}], true)
    var cnt = 0; for x <- a[2:2:2] {cnt += 1}; EXPECT_EQ_(cnt, 0)
    // non-empty strided slices (both directions) still correct
    EXPECT_EQ_(a[0:5:2], [1, 3, 5])
    EXPECT_EQ_(a[4:0:-2], [5, 3])
})

// FB-006: indexing an array-of-arrays built by a NESTED comprehension used to
// emit `fx_copy_arr(&{0}, ...)` -- the inner comprehension's result array was
// dropped (delivered via dstexp_r, but the outer map read the dummy return).
TEST("array.nested_comprehension", fun() {
    // 2-level int [] []
    val aa = [for i <- 0:3 {[for j <- 0:3 {i*10 + j}]}]
    EXPECT_EQ_(aa[0][0], 0); EXPECT_EQ_(aa[1][2], 12); EXPECT_EQ_(aa[2][1], 21)
    // 3-level int [] [] []
    val aaa = [for i <- 0:2 {[for j <- 0:2 {[for k <- 0:2 {i*100 + j*10 + k}]}]}]
    EXPECT_EQ_(aaa[1][0][1], 101); EXPECT_EQ_(aaa[0][1][0], 10)
    // 2-level with tuple elements
    val tt = [for i <- 0:2 {[for j <- 0:2 {(i, j, i + j)}]}]
    EXPECT_EQ_(tt[1][0], (1, 0, 1)); EXPECT_EQ_(tt[0][1], (0, 1, 1))
    // comprehension vs literal vs list-comprehension must agree element-wise
    val byComp = [for i <- 0:2 {[for j <- 0:3 {i*10 + j}]}]
    val byLit  = [[0, 1, 2], [10, 11, 12]]
    val byList = [:: for i <- 0:2 {[for j <- 0:3 {i*10 + j}]}]
    EXPECT_EQ_(byComp[0], byLit[0]); EXPECT_EQ_(byComp[1], byLit[1])
    EXPECT_EQ_(byComp[0], byList.hd()); EXPECT_EQ_(byComp[1], byList.tl().hd())
})

TEST("array.stat", fun() {
    val arr = [ 1, 2, 3, 4, 5 ]
    EXPECT_EQ_(sum(arr), double(1+2+3+4+5))
    EXPECT_NEAR_(mean(arr), double(1+2+3+4+5)/5, 1e-5)
    EXPECT_NEAR_(normL2(arr), 7.416198487095663, 1e-5)
    EXPECT_EQ_(normInf(arr), 5)
})

TEST("array.solve", fun() {
    val A =
     [ 2., 1., 0., 4.;
        0., 1., 1., 3.;
        1., 0., 1., 2.;
        2., 2., 0., 1. ]
    val I = diag(4, 1.)
    val b = [ 4.; 5.; 6.; 7. ]
    val x = A\b
    val Ainv = A\1
    EXPECT_NEAR_(A*x, b, 1e-10)
    EXPECT_NEAR_(A*Ainv, I, 1e-10)
})

TEST("array.tuple_index", fun() {
    val (h,w,d) = (10, 10, 10)
    val rng = RNG(0xffffffffu64)
    var A1 = random(rng, (h), -2., 2.)
    val B1 = random(rng, (h), -2., 2.)
    var A2 = random(rng, (h,w), -2., 2.)
    val B2 = random(rng, (h,w), -2., 2.)
    var A3 = random(rng, (h,w,d), -2., 2.)
    val B3 = random(rng, (h,w,d), -2., 2.)

    fun add1(A,B) {
        for x@idx <- A, y <- B {
            val i:int = idx
            A[i] = x + y}
    }

    fun add2(A,B) {
        for x@idx <- A, y <- B {
            val (i, j) = idx
            A[i, j] = x + y}
    }

    fun add3(A,B) {
        for x@idx <- A, y <- B {
            val (i, j, k) = idx
            A[i, j, k] = x + y}
    }

    val A1ref = copy(A1)
    A1 += B1
    add1(A1ref, B1)
    EXPECT_EQ_(A1, A1ref)

    val A2ref = copy(A2)
    A2 += B2
    add2(A2ref, B2)
    EXPECT_EQ_(A2, A2ref)

    val A3ref = copy(A3)
    A3 += B3
    add3(A3ref, B3)
    EXPECT_EQ_(A3, A3ref)
})

TEST("array.bounding_box", fun() {
    fun bounding_box(image: uint8 [,])
    {
        val fold minx = 1000000, maxx = -1, miny = 1000000, maxy = -1
            for pix@(y, x) <- image {
                if pix != 0 {
                    (minx, maxx, miny, maxy) = (min(minx, x), max(maxx, x), min(miny, y), max(maxy, y))
                }
            }
        if maxx >= 0 { (minx, miny, maxx-minx+1, maxy-miny+1) }
        else { (0,0,0,0) }
    }

    val img = uint8([
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 1, 1, 1, 0, 0;
        0, 0, 1, 0, 0, 0, 1, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 1, 0, 0, 0, 0
        ])

    EXPECT_EQ_(bounding_box(img), (2, 1, 5, 4))
})

TEST("array.fuse_inplace_stencil", fun() {
    // FB-028: a single-use comprehension whose body reads mutable memory via a
    // non-throwing .clip read is a loop-fusion candidate. Fusing it into a
    // consumer loop that writes that array IN PLACE would replay the reads across
    // the consumer's stores (K_fuse_loops' sibling of the FB-023 move hazard):
    // a 3-tap smoothing filter must read the ORIGINAL array for every tap. The
    // fusion criterion uses pure_kexp's movement grade so this body (mutable read)
    // is NOT fused; without it the in-place write-back corrupts later taps at O3.
    // (Regression is observable here because fusion now fires inside lambda
    // bodies too, so this UTest closure is itself a fusion site.)
    val arr = [30, 60, 90, 120, 150]
    val n = size(arr)
    val smoothed = [for i <- 0:n { (arr.clip[i-1] + arr.clip[i] + arr.clip[i+1]) / 3 }]
    var k = 0
    for s <- smoothed { arr[k] = s; k += 1 }
    EXPECT_EQ_(arr, [40, 60, 90, 120, 140])
})

TEST("array.fuse_map_reduce", fun() {
    // positive: a single-use comprehension consumed by exactly one for-loop is
    // fused -- the intermediate `squares` array is never materialized, the map
    // body is replayed inside the reduce loop. The body reads no mutable memory,
    // so it fuses under the movement grade; the result must equal the unfused
    // meaning. Confirmed a fusion site via -pr-k. Guards fusion correctness on a
    // case where it fires (complement of fuse_inplace_stencil, where it must not).
    val n = 10
    val squares = [for i <- 0:n { i * i }]
    var s = 0
    for x <- squares { s += x }
    EXPECT_EQ_(s, 285)                        // 0+1+4+9+...+81 = 285
    // a second fusable shape: comprehension over a range, consumed once
    val evens = [for i <- 0:n { i * 2 }]
    var prod1 = 1
    for x <- evens { if x > 0 { prod1 *= x } }
    EXPECT_EQ_(prod1, 2*4*6*8*10*12*14*16*18) // product of the positive evens
})
