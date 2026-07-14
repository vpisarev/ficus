/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// FB-015 / ctrlflow-1: break, continue and return (with a value or bare) are
// legal in *expression* position -- as the value of a match arm or an if
// branch -- exactly like `throw`. A jumping arm/branch produces no value, so
// it unifies with any value-producing sibling. The backend's existing cleanup
// chain frees ref-counted locals that are live at the jump; several tests keep
// arrays/strings alive across the jump so that running the whole suite under
// `ASAN_OPTIONS=detect_leaks=1` machine-checks "every cleanup section ran".

from UTest import *

// continue as a match-arm value inside list[a] comprehension: a filter.
TEST("ctrlflow.continue_arm_filter", fun() {
    val xs = [:: 1, 2, 0, 3, 0, 0, 4, 5]
    val r = [:: for e <- xs { match e { | 0 => continue | x => x } }]
    EXPECT_EQ(r, [:: 1, 2, 3, 4, 5])
})

// continue as an if-branch value inside an array comprehension body is NOT a
// filter (arrays are dense); use a plain for-loop to accumulate instead.
TEST("ctrlflow.continue_if_branch_loop", fun() {
    var sum = 0
    for x <- 0:10 {
        val keep = if x % 3 == 0 { continue } else { x }
        sum += keep
    }
    // 1+2+4+5+7+8 = 27  (skip 0,3,6,9)
    EXPECT_EQ(sum, 27)
})

// break as an if-branch value; a ref-counted string temp is live at the jump.
TEST("ctrlflow.break_if_branch_refcount", fun() {
    val words = [:: "alpha", "beta", "STOP", "gamma"]
    var acc = ""
    for w <- words {
        val tagged = w + "!"                       // ref-counted temp, live at break
        val keep = if w == "STOP" { break } else { tagged }
        acc += keep + " "
    }
    EXPECT_EQ(acc, "alpha! beta! ")
})

// break as a match-arm value.
TEST("ctrlflow.break_match_arm", fun() {
    val xs = [:: 3, 1, 4, 2, 5, 9]
    var last = -1
    for x <- xs {
        // on the break arm the assignment `last = ...` never completes, so
        // `last` keeps the value from the previous (x=2) iteration.
        last = match x { | v when v >= 5 => break | v => v }
    }
    EXPECT_EQ(last, 2)             // 3,1,4,2 assigned; at x=5 break skips the store
})

// return WITH a value as a match-arm value (early exit out of a function).
TEST("ctrlflow.return_value_arm", fun() {
    fun first_pos(xs: list[int]): int {
        for x <- xs { match x { | v when v > 0 => return v | _ => {} } }
        -1
    }
    EXPECT_EQ(first_pos([:: -3, -1, 0, 7, 2]), 7)
    EXPECT_EQ(first_pos([:: -3, -1]), -1)
})

// bare return: same-line `{ return }` and `return` right before a `|` arm.
TEST("ctrlflow.bare_return", fun() {
    // same-line { return } in a void function
    fun stop_at(n: int, lim: int): int {
        var seen = 0
        for i <- 0:n { if i == lim { return seen } else {}; seen += 1 }
        seen
    }
    EXPECT_EQ(stop_at(10, 4), 4)
    EXPECT_EQ(stop_at(10, 100), 10)

    // bare return before a match arm separator '|'
    var log = ""
    fun classify(x: int): void {
        match x { | 0 => return | v => log += f"nz{v};" }
    }
    classify(0); classify(7); classify(0); classify(3)
    EXPECT_EQ(log, "nz7;nz3;")
})

// both branches of an `if` jump: the whole `if` is TypErr, the for-body stays
// void, and the function tail after the loop is what returns.
TEST("ctrlflow.both_branches_jump", fun() {
    fun find_neg(xs: list[int]): int {
        for x <- xs { if x < 0 { return x } else { continue } }
        999
    }
    EXPECT_EQ(find_neg([:: 3, 5, -2, 8]), -2)
    EXPECT_EQ(find_neg([:: 3, 5, 8]), 999)
})

// nested loops: break targets the INNERMOST loop (assert via a side-effect
// counter, not just "compiles"). Inner j runs 0..<i for each i.
TEST("ctrlflow.nested_break_innermost", fun() {
    var hits = 0
    for i <- 0:4 {
        for j <- 0:4 {
            val stop = if j == i { break } else { false }
            ignore(stop); hits += 1
        }
    }
    // i=0 ->0, i=1 ->1, i=2 ->2, i=3 ->3  => 0+1+2+3 = 6
    EXPECT_EQ(hits, 6)
})

// ref-counted 2D arrays live at a deep return jump: correctness proves the
// arrays were built; under ASAN detect_leaks it proves they were freed on the
// jump path. Kept small so the unit suite stays fast (the 1000x1000 leak
// oracle lives in the sanitize/manual run, see docs/ctrlflow1_report.md).
TEST("ctrlflow.refcounted_matrix_at_return", fun() {
    fun scan(n: int, thresh: int): int {
        for i <- 0:n {
            val m = [for r <- 0:64 for c <- 0:64 {(i + r + c) % 7}]   // ref-counted temp
            val tag = f"m{i}"                                          // ref-counted string
            var s = 0
            for r <- 0:64 { s += m[r, 0] }
            val out = match (s >= thresh) { | true => return i | _ => -1 }
            ignore(out); ignore(tag)
        }
        -1
    }
    val r = scan(20, 0)          // s>=0 always -> returns on i=0
    EXPECT_EQ(r, 0)
    val r2 = scan(3, 1000000)    // never crosses -> falls through to -1
    EXPECT_EQ(r2, -1)
})

// a match where EVERY arm jumps (all-jumping arms), used in statement position.
TEST("ctrlflow.all_arms_jump", fun() {
    fun categorize(xs: list[int]): (int, int) {
        var pos = 0, neg = 0
        for x <- xs {
            match x {
            | v when v > 0 => pos += 1; continue
            | 0 => continue
            | _ => neg += 1; continue
            }
        }
        (pos, neg)
    }
    EXPECT_EQ(categorize([:: -2, 0, 3, 5, -1, 0]), (2, 2))
})
