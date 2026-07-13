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
    val v = vector(3, 7)
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

    val a = vector([1, 2, 3])
    val b = vector([1, 2, 3])
    val c = vector([1, 2, 4])
    EXPECT_EQ(`a == b`, true)
    EXPECT_EQ(`a == c`, false)
    EXPECT_EQ(`a <=> c`, -1)
    EXPECT_EQ(`string(a)`, "[1, 2, 3]")

    // complex-element resize (grow with fill, shrink freeing elements) — ASan
    val s = vector(2, "ab")
    s.push_back("cd")
    s.resize(5, "zz")
    EXPECT_EQ(`size(s)`, 5)
    EXPECT_EQ(`s[4]`, "zz")
    s.resize(2, "")
    EXPECT_EQ(`array(s)`, ["ab", "ab"])
})

TEST("fcvector.pushpop_edge", fun()
{
    // __intrin_pop__ slow path: popping an empty vector throws SizeError
    val v: int vector = []
    EXPECT_THROWS(fun() { v.pop_back() }, SizeError)
    v.push_back(1); v.push_back(2)
    v.pop_back(); v.pop_back()
    EXPECT_THROWS(fun() { v.pop_back() }, SizeError)

    // complex-element push/pop frees correctly (ASan-checked), fast path bypassed
    val s: string vector = []
    for w <- ["aa", "bb", "cc"] { s.push_back(w) }
    s.pop_back()
    EXPECT_EQ(`array(s)`, ["aa", "bb"])

    // growth (capacity exceeded) goes through the slow path and keeps all elements
    val g: int vector = []
    for i <- 0:1000 { g.push_back(i) }
    EXPECT_EQ(`size(g)`, 1000)
    EXPECT_EQ(`g[999]`, 999)
    EXPECT_EQ(`g[0]`, 0)
})

TEST("fcvector.splice", fun()
{
    // contiguous replace, grow (2 elems -> 3)
    val v = vector([0,1,2,3,4,5,6,7,8,9])
    v[3:5] = vector([100,200,300])
    EXPECT_EQ(`array(v)`, [0,1,2,100,200,300,5,6,7,8,9])

    // contiguous replace, shrink (6 elems -> 1)
    val w = vector([0,1,2,3,4,5,6,7,8,9])
    w[2:8] = vector([99])
    EXPECT_EQ(`array(w)`, [0,1,99,8,9])

    // contiguous replace, same size (in place)
    val u = vector([0,1,2,3,4])
    u[1:4] = vector([10,20,30])
    EXPECT_EQ(`array(u)`, [0,10,20,30,4])

    // range delete (rhs [])
    val e = vector([0,1,2,3,4,5,6,7,8,9])
    e[3:6] = []
    EXPECT_EQ(`array(e)`, [0,1,2,6,7,8,9])

    // strided delete (even indices)
    val d = vector([0,1,2,3,4,5,6,7,8,9])
    d[::2] = []
    EXPECT_EQ(`array(d)`, [1,3,5,7,9])

    // strided delete with explicit bounds
    val d2 = vector([0,1,2,3,4,5,6,7,8,9])
    d2[1:8:3] = []                      // remove indices 1,4,7
    EXPECT_EQ(`array(d2)`, [0,2,3,5,6,8,9])

    // clear via the full slice
    val c = vector([1,2,3,4,5])
    c[:] = []
    EXPECT_EQ(`size(c)`, 0)
    c.push_back(42)                     // still an allocated (pushable) vector
    EXPECT_EQ(`array(c)`, [42])

    // insert into an empty vector via [0:0]
    val f: int vector = []
    f[0:0] = vector([7,8,9])
    EXPECT_EQ(`array(f)`, [7,8,9])

    // prepend / append via zero-width ranges
    val g = vector([3,4])
    g[0:0] = vector([1,2])
    g[4:4] = vector([5,6])
    EXPECT_EQ(`array(g)`, [1,2,3,4,5,6])

    // complex (string) elements: replace shrink frees the removed strings (ASan)
    val s = vector(["a","b","c","d","e"])
    s[1:4] = vector(["X"])
    EXPECT_EQ(`array(s)`, ["a","X","e"])
    // string strided delete
    val s2 = vector(["a","b","c","d","e","f"])
    s2[::2] = []
    EXPECT_EQ(`array(s2)`, ["b","d","f"])

    // out-of-range slice throws
    val r = vector([1,2,3])
    EXPECT_THROWS(fun() { r[2:9] = vector([0]) }, OutOfRangeError)
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
        val curr = vector(for x@j <- last { if j == 0 {1.f} else {last[j-1] + last[j]} })
        curr.push_back(1.f)
        myvecs.push_back(curr)
    }

    fun sum(v: 't vector, v0: 'r): 'r = fold s = v0 for x <- v {s = x + s}
    for i <- 0:size(myvecs) {
        val expected_sum = double(1 << i)
        EXPECT_EQ(`sum(myvecs[i], 0.)`, expected_sum)
    }
})

TEST("fcvector.comprehension", fun()
{
    // write-comprehension over a range and over an array
    val v = vector(for i <- 0:6 {i*i})
    EXPECT_EQ(`size(v)`, 6)
    EXPECT_EQ(`array(v)`, [0, 1, 4, 9, 16, 25])
    val a = [10, 20, 30]
    EXPECT_EQ(`array(vector(for x <- a {x + 1}))`, [11, 21, 31])
    // the result is a real mutable vector
    v.push_back(36)
    EXPECT_EQ(`v[6]`, 36)
    // empty comprehension
    EXPECT_EQ(`size(vector(for i <- 0:0 {i}))`, 0)
    // complex elements (strings) — ASan-checked
    val s = vector(for i <- 0:4 {string(i*i)})
    EXPECT_EQ(`array(s)`, ["0", "1", "4", "9"])
    // nested: a vector of vectors
    val vv = vector(for i <- 0:3 {vector(for j <- 0:i {j})})
    EXPECT_EQ(`size(vv)`, 3)
    EXPECT_EQ(`array(vv[2])`, [0, 1])

    // break / continue: the size is trimmed to the count actually written
    val odds_skipped = vector(for i <- 0:10 { if i % 2 == 1 {continue}; i })
    EXPECT_EQ(`array(odds_skipped)`, [0, 2, 4, 6, 8])
    val stopped = vector(for i <- 0:10 { if i == 3 {break}; i*10 })
    EXPECT_EQ(`array(stopped)`, [0, 10, 20])
})

TEST("fcvector.iteration", fun()
{
    val v = vector(for i <- 0:5 {i*i})
    var s = 0
    for x <- v { s += x }
    EXPECT_EQ(`s`, 0 + 1 + 4 + 9 + 16)
    // index binding
    var chk = true
    for x@i <- v { if x != i*i {chk = false} }
    EXPECT_EQ(`chk`, true)
    // comprehension with a vector source (read + write)
    EXPECT_EQ(`array(vector(for x <- v {x + 1}))`, [1, 2, 5, 10, 17])
    // iterating an empty vector does nothing
    val e: int vector = []
    var cnt = 0
    for x <- e { cnt += 1 }
    EXPECT_EQ(`cnt`, 0)
    // reads and set are fine during iteration; post-loop mutation works
    for x@i <- v { if i < 2 {v[i] = x + 100} }
    EXPECT_EQ(`array(v)`, [100, 101, 4, 9, 16])
    v.push_back(25)
    EXPECT_EQ(`size(v)`, 6)
})

TEST("fcvector.readlock", fun()
{
    // structural mutation while iterating throws VecModifiedError (variant D)
    val v = vector(for i <- 0:10 {i})
    EXPECT_THROWS(fun() { for x <- v { if x == 2 {v.push_back(99)} } }, VecModifiedError)
    // the lock was released on the exception exit -> mutation works again
    v.push_back(50)
    EXPECT_EQ(`size(v)`, 11)

    // reads and v[i]=a ARE allowed during iteration
    var acc = 0
    for x <- v { acc += v[0] }
    EXPECT_EQ(`acc`, 0)
    for x@i <- v { if i == 0 {v[0] = 7} }
    EXPECT_EQ(`v[0]`, 7)

    // break releases the lock
    for x <- v { if x == 3 {break} }
    v.push_back(51)
    EXPECT_EQ(`size(v)`, 12)

    // nested iteration of the same vector is fine (stacked read-locks)
    var pairs = 0
    for x <- v { for y <- v { pairs += 1 } }
    EXPECT_EQ(`pairs`, 12*12)

    // aliased structural mutation is caught (the lock is on the shared header)
    val w = v
    EXPECT_THROWS(fun() { for x <- v { w.push_back(1) } }, VecModifiedError)

    // an unrelated exception in the body still releases the lock
    EXPECT_THROWS(fun() { for x <- v { ignore(v[1000]) } }, OutOfRangeError)
    v.push_back(52)
    EXPECT_EQ(`size(v)`, 13)
})

TEST("fcvector.str", fun()
{
    val vecstr: string vector = []
    for w <- ["a", "bc", "def", "ghij", "klmno", "pqrstu", "vwxyz"] {
        vecstr.push_back(w)
    }
    EXPECT_EQ(array(vecstr[::2]), ["a", "def", "klmno", "vwxyz"])
    EXPECT_EQ(vecstr[::2], vector(["a", "def", "klmno", "vwxyz"]))
    EXPECT_EQ(array(vecstr[::-1]), ["vwxyz", "pqrstu", "klmno", "ghij", "def", "bc", "a"])
    EXPECT_EQ(vecstr[::-1], vector(["vwxyz", "pqrstu", "klmno", "ghij", "def", "bc", "a"]))
})

TEST("fcvector.concat", fun()
{
    val a = vector([1, 2, 3])
    val b = vector([4, 5])
    val c: int vector = []
    val d = vector([6, 7, 8, 9])
    // array of vectors
    EXPECT_EQ(`array(Vector.concat([a, b, d]))`, [1, 2, 3, 4, 5, 6, 7, 8, 9])
    // empty inputs contribute nothing
    EXPECT_EQ(`array(Vector.concat([a, c, b]))`, [1, 2, 3, 4, 5])
    // all-empty / empty list -> empty result that is still a real allocated
    // (pushable) vector, not the NULL/default vector
    EXPECT_EQ(`size(Vector.concat([c, c]))`, 0)
    val e = Vector.concat(([]: (int vector) []))
    EXPECT_EQ(`size(e)`, 0)
    e.push_back(42)
    EXPECT_EQ(`array(e)`, [42])
    // vector of vectors
    val vv = vector([a, b, d])
    EXPECT_EQ(`array(Vector.concat(vv))`, [1, 2, 3, 4, 5, 6, 7, 8, 9])
    // complex elements (strings) are copied — ASan-checked
    val s1 = vector(["a", "bc"])
    val s2 = vector(["def"])
    EXPECT_EQ(`array(Vector.concat([s1, s2]))`, ["a", "bc", "def"])
    // the result is independent of the inputs (a real fresh vector)
    val r = Vector.concat([a, b])
    r.push_back(99)
    EXPECT_EQ(`size(a)`, 3)
    EXPECT_EQ(`r[5]`, 99)
})
