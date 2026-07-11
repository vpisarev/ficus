/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// fold-1 Phase 0: simultaneous tuple assignment `(a, b) = (b, a + b)`.
// Parse-time desugar: the RHS materializes ONCE into a temp before any store
// (that is the simultaneity), stores run left-to-right, a '_' component emits
// no store but the RHS is still fully evaluated. See compiler/Parser.fx
// (make_tuple_assign); the swap cases (array_swap/scalar_swap/rotate3) lock the
// C-gen movement-unsafe-read fix, docs/found_bugs.md FB-023.

from UTest import *

// simultaneity: the classic Fibonacci step must use OLD a and OLD b.
TEST("tuple_assign.fibonacci", fun() {
    var a = 0, b = 1
    for _ <- 0:8 { (a, b) = (b, a + b) }
    EXPECT_EQ(a, 21)
    EXPECT_EQ(b, 34)
})

// scalar swap: both components are bare identifiers -- the temp must NOT be a
// dealiasable temp-val, else `a = b; b = a` collapses to both == old b.
TEST("tuple_assign.scalar_swap", fun() {
    var a = 10, b = 20
    (a, b) = (b, a)
    EXPECT_EQ(a, 20)
    EXPECT_EQ(b, 10)
})

// array-element swap: the aliasing case that broke a temp-val flattening
// (read of arr[i] must happen before the store to arr[i]).
TEST("tuple_assign.array_swap", fun() {
    var arr = [1, 2, 3, 4]
    val i = 0, j = 3
    (arr[i], arr[j]) = (arr[j], arr[i])
    EXPECT_EQ(arr, [4, 2, 3, 1])
})

// a longer rotation across array elements.
TEST("tuple_assign.rotate3", fun() {
    var a = [10, 20, 30]
    (a[0], a[1], a[2]) = (a[1], a[2], a[0])
    EXPECT_EQ(a, [20, 30, 10])
})

// a memory read hidden inside a COMPOUND rhs component (an if-expression):
// the movement-unsafe-read check must recurse into it, not just look at the top
// form, or the read is inlined past the store. Locks the recursive fold_kexp.
TEST("tuple_assign.compound_component", fun() {
    var arr = [5, 6]
    val cond = true
    (arr[0], arr[1]) = (if cond {arr[1]} else {0}, if cond {arr[0]} else {0})
    EXPECT_EQ(arr, [6, 5])
})

// nested tuple LHS recurses.
TEST("tuple_assign.nested", fun() {
    var x = 0, y = 0, z = 0
    ((x, y), z) = ((10, 20), 30)
    EXPECT_EQ(x, 10); EXPECT_EQ(y, 20); EXPECT_EQ(z, 30)
})

// RHS may be any tuple-typed expression, not only a literal.
TEST("tuple_assign.call_rhs", fun() {
    fun divmod(p: int, q: int) = (p / q, p % q)
    var d = 0, m = 0
    (d, m) = divmod(17, 5)
    EXPECT_EQ(d, 3); EXPECT_EQ(m, 2)
})

// '_' component: no store is emitted, but the RHS is evaluated in full.
TEST("tuple_assign.underscore_component", fun() {
    var log = 0
    fun eff(v: int) { log += 1; (v, v * v) }
    var q = 0
    (q, _) = eff(7)
    EXPECT_EQ(q, 7)
    EXPECT_EQ(log, 1)          // the effectful RHS ran exactly once
})

// degenerate all-'_' tuple: evaluate for effects only.
TEST("tuple_assign.all_underscore", fun() {
    var log = 0
    fun eff(v: int) { log += 1; (v, v) }
    (_, _) = eff(9)
    EXPECT_EQ(log, 1)
})

// bare `_ = expr`: evaluate for effects, discard the value.
TEST("tuple_assign.bare_underscore", fun() {
    var log = 0
    fun eff(v: int) { log += 1; (v, v) }
    _ = eff(11)
    EXPECT_EQ(log, 1)
})
