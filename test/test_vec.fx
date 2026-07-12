/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// first-class mutable vector ('t vector) tests: element access v[i], v[i]=a,
// size/empty, push_back/pop_back/back, resize/reserve/clear, slices, map/foldl.

from UTest import *

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

TEST("fcvector.write", fun()
{
    val v: int vector = []
    for i <- 0:5 { v.push_back(i) }
    v[0] = 100; v[2] = 300; v[4] = 500
    EXPECT_EQ(`array(v)`, [100, 1, 300, 3, 500])
    var sink = 0
    EXPECT_THROWS(fun() {v[5] = 9; sink = v[5]}, OutOfRangeError)
    ignore(sink)

    // complex element: overwriting must free the old string (ASan-checked)
    val s: string vector = []
    s.push_back("x"); s.push_back("y")
    s[0] = "hello"; s[1] = "world"
    EXPECT_EQ(`s[0]`, "hello")
    EXPECT_EQ(`s[1]`, "world")
})

TEST("fcvector.ops", fun()
{
    val v = Vector.make(3, 7)
    EXPECT_EQ(`size(v)`, 3)
    EXPECT_EQ(`Vector.capacity(v) >= 3`, true)
    EXPECT_EQ(`array(v)`, [7, 7, 7])
    v.push_back(9)
    v.resize(6, 0)
    EXPECT_EQ(`array(v)`, [7, 7, 7, 9, 0, 0])
    EXPECT_EQ(`v.back()`, 0)
    v.pop_back()
    EXPECT_EQ(`array(v)`, [7, 7, 7, 9, 0])
    v.clear()
    EXPECT_EQ(`size(v)`, 0)
    EXPECT_EQ(`empty(v)`, true)

    val a = Vector.make([1, 2, 3])
    val b = Vector.make([1, 2, 3])
    val c = Vector.make([1, 2, 4])
    EXPECT_EQ(`a == b`, true)
    EXPECT_EQ(`a == c`, false)
    EXPECT_EQ(`a <=> c`, -1)
    EXPECT_EQ(`string(a)`, "[1, 2, 3]")

    // complex-element resize (grow with fill, shrink freeing elements) — ASan
    val s = Vector.make(2, "ab")
    s.push_back("cd")
    s.resize(5, "zz")
    EXPECT_EQ(`size(s)`, 5)
    EXPECT_EQ(`s[4]`, "zz")
    s.resize(2, "")
    EXPECT_EQ(`array(s)`, ["ab", "ab"])
})

TEST("fcvector.slice", fun()
{
    val v: int vector = []
    for i <- 0:10 { v.push_back(i) }
    EXPECT_EQ(`array(v[2:5])`, [2, 3, 4])
    EXPECT_EQ(`array(v[:3])`, [0, 1, 2])
    EXPECT_EQ(`array(v[7:])`, [7, 8, 9])
    EXPECT_EQ(`array(v[:])`, array(v))      // flatten = full copy
    EXPECT_EQ(`array(v[::-1])`, [9, 8, 7, 6, 5, 4, 3, 2, 1, 0])
    EXPECT_EQ(`array(v[::2])`, [0, 2, 4, 6, 8])
    EXPECT_EQ(`array(v[1:8:3])`, [1, 4, 7])        // arbitrary stride

    // a slice / flatten is a COPY: mutating it must not touch the source
    val s = v[2:5]
    s[0] = 999
    EXPECT_EQ(`array(s)`, [999, 3, 4])
    EXPECT_EQ(`array(v[2:5])`, [2, 3, 4])

    // complex elements copied on slice
    val w: string vector = []
    for x <- ["a", "b", "c", "d"] { w.push_back(x) }
    EXPECT_EQ(`array(w[1:3])`, ["b", "c"])
    EXPECT_EQ(`array(w[::-1])`, ["d", "c", "b", "a"])
})

TEST("fcvector.binomial", fun()
{
    val emptyvec: float vector = []
    val myvecs: (float vector) vector = []
    for i <- 1:10 {
        val last = if empty(myvecs) {emptyvec} else {myvecs.back()}
        // curr annotated to sidestep FB-024 (order-dependent generic-return
        // inference pollution across tests); see docs/found_bugs.md
        val curr: float vector = last.mapi(fun(x, j) { if j == 0 {1.f} else {last[j-1] + last[j]} })
        curr.push_back(1.f)
        myvecs.push_back(curr)
    }

    fun sum(v: 't vector, v0: 'r): 'r = v.foldl(fun (x, s) {x + s}, v0)
    for i <- 0:size(myvecs) {
        val expected_sum = double(1 << i)
        EXPECT_EQ(`sum(myvecs[i], 0.)`, expected_sum)
    }
})

TEST("fcvector.str", fun()
{
    val vecstr: string vector = []
    for w <- ["a", "bc", "def", "ghij", "klmno", "pqrstu", "vwxyz"] {
        vecstr.push_back(w)
    }
    EXPECT_EQ(array(vecstr[::2]), ["a", "def", "klmno", "vwxyz"])
    EXPECT_EQ(vecstr[::2], Vector.make(["a", "def", "klmno", "vwxyz"]))
    EXPECT_EQ(array(vecstr[::-1]), ["vwxyz", "pqrstu", "klmno", "ghij", "def", "bc", "a"])
    EXPECT_EQ(vecstr[::-1], Vector.make(["vwxyz", "pqrstu", "klmno", "ghij", "def", "bc", "a"]))
})
