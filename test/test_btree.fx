/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// btree.fx example converted into a test. Binary tree traversal

from UTest import *

TEST("btree.depth_4_10", fun() {

type tree = Empty | Node: {left: tree; right: tree}

fun make (d: int) =
    if d == 0 { Node {left=Empty, right=Empty} }
    else { Node{right=make(d-1), left=make(d-1)} }

fun check (t: tree): int {
    | Node{left=l, right=r} => 1 + check(l) + check(r)
    | _ => 0
}

val min_depth = 4
val max_depth = 10
val max_depth = max(min_depth + 2, max_depth)
val stretch_depth = max_depth + 1

val c = check (make(stretch_depth))
EXPECT_EQ(c, 4095)

val long_lived_tree = make(max_depth)
val report = [@parallel for i <- 0 : (max_depth - min_depth) / 2 + 1
{
    val d = min_depth + i * 2
    val niter = 1 << (max_depth - d + min_depth)
    val fold c = 0 for i <- 0:niter {
        c + check(make(d))
    }
    (niter, d, c)
}]

EXPECT_EQ(report, [ (1024, 4, 31744), (256, 6, 32512), (64, 8, 32704), (16, 10, 32752) ])
EXPECT_EQ(check(long_lived_tree), 2047)
})
