/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// fold-1: the new imperative fold. It is a thin sugar over `for`:
//   fold <acc> = <init> for <clauses> { body }
//     ==>  { var <acc> = init; for <clauses> { body }; <acc> }
// The accumulator is a real mutable var, visible and assignable in the body;
// break/continue/return fall out for free (it is an ordinary `for`). See
// compiler/Parser.fx (transform_new_fold_exp).

from UTest import *

// single accumulator, `op=` update.
TEST("fold2.single_sum", fun() {
    val s = fold s = 0 for x <- [1, 2, 3, 4] { s += x }
    EXPECT_EQ_(s, 10)
})

// single accumulator, plain `s = E` update.
TEST("fold2.single_assign", fun() {
    val p = fold p = 1 for x <- [1, 2, 3, 4] { p = p * x }
    EXPECT_EQ_(p, 24)
})

// tuple accumulator with the Fibonacci simultaneity case (the body uses a
// simultaneous tuple assignment from Phase 0.1).
TEST("fold2.tuple_fibonacci", fun() {
    val (a, b) = fold (a, b) = (0, 1) for _ <- 0:8 { (a, b) = (b, a + b) }
    EXPECT_EQ_(a, 21)
    EXPECT_EQ_(b, 34)
})

// tuple accumulator, independent per-component updates.
TEST("fold2.tuple_minmax", fun() {
    val (mn, mx) = fold (mn, mx) = (1000, -1000) for x <- [3, 1, 4, 1, 5, 9, 2, 6] {
        if x < mn { mn = x }
        if x > mx { mx = x }
    }
    EXPECT_EQ_(mn, 1)
    EXPECT_EQ_(mx, 9)
})

// break inside the body is legal (plain for) and stops the fold.
TEST("fold2.break_in_body", fun() {
    val first = fold r = -1 for x@i <- [5, 6, 7, 8] { if x == 7 { r = i; break } }
    EXPECT_EQ_(first, 2)
})

// continue inside the body skips the remaining updates of the iteration.
TEST("fold2.continue_in_body", fun() {
    val s = fold s = 0 for x <- [1, 2, 3, 4, 5, 6] {
        if x % 2 == 0 { continue }
        s += x
    }
    EXPECT_EQ_(s, 9)          // 1 + 3 + 5
})

// a valued return from within the body returns from the enclosing function,
// carrying the accumulated value.
TEST("fold2.return_in_body", fun() {
    fun sum_until_neg(a: int []): int {
        fold s = 0 for x <- a { if x < 0 { return s }; s += x }
    }
    EXPECT_EQ_(sum_until_neg([1, 2, 3, -1, 100]), 6)   // stops at the -1
    EXPECT_EQ_(sum_until_neg([1, 2, 3]), 6)            // no negative: full sum
})

// nested fold: the inner fold is an ordinary sub-expression whose value
// feeds the outer accumulator.
TEST("fold2.nested", fun() {
    val total = fold acc = 0 for i <- 0:3 {
        val row = fold t = 0 for j <- 0:3 { t += i * 10 + j }
        acc += row
    }
    EXPECT_EQ_(total, 99)     // sum over i,j of (i*10+j)
})

// the inner fold's body may assign the OUTER accumulator directly (the outer
// accumulator is just a var in scope) — ordinary lexical scoping.
TEST("fold2.nested_inner_assigns_outer", fun() {
    val count = fold c = 0 for i <- 0:3 {
        ignore(fold seen = 0 for j <- 0:3 { c += 1; seen += 1 })
    }
    EXPECT_EQ_(count, 9)      // c incremented 3*3 times
})

// NEW semantics lock: a closure in the body captures the accumulator VAR (it
// sees the final value), NOT a per-iteration immutable snapshot as old fold did.
TEST("fold2.closure_captures_var", fun() {
    val (s, fns) = fold (s, fns) = (0, ([]: list[(void -> int)])) for x <- [1, 2, 3] {
        s += x
        fns = (fun () { s }) :: fns
    }
    EXPECT_EQ_(s, 6)
    EXPECT_EQ_([:: for f <- fns { f() }], [:: 6, 6, 6])   // all see the final var
})

// the accumulator is an ordinary value after the fold; a `val`-bound result is
// immutable (this just documents that the whole-fold value binds as usual).
TEST("fold2.result_is_plain_value", fun() {
    val n = fold n = 0 for x <- [10, 20, 30] { n += x }
    EXPECT_EQ_(n, 60)
    EXPECT_EQ_(n * 2, 120)
})
