/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// ycomb.fx example converted into a test

from UTest import *

TEST("ycomb.fac_fib", fun() {
// from https://rosettacode.org/wiki/Y_combinator; adoped from OCaml implementation.
// trick from Lambda calculus to convert non-recursive functions into recursive ones
type 'a mu = Roll: ('a mu -> 'a)
fun unroll(Roll(x): 'a mu) = x

fun ycomb (f: ('a -> 'b) -> ('a -> 'b)): 'a -> 'b {
    fun l(x: ('a -> 'b) mu): 'a -> 'b = fun (a: 'a) { f(unroll(x)(x))(a) }
    l(Roll(l))
}

fun fac (f: int->int) =
    fun (n: int) {
    | 0 => 1
    | _ => n*f(n-1)
    }

fun fib (f: int->int) =
    fun (n: int) {
    | 0 => 0
    | 1 => 1
    | _ => f(n-1) + f(n-2)
    }

val recfac = ycomb(fac)
EXPECT_EQ(recfac(5), 120)

val recfib = ycomb(fib)
EXPECT_EQ(recfib(8), 21)
})
