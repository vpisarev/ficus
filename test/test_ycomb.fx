/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// ycomb.fx example converted into a test

from UTest import *

TEST("ycomb.fac_fib", fun() {
// from https://rosettacode.org/wiki/Y_combinator; adoped from OCaml implementation.
// trick from Lambda calculus to convert non-recursive functions into recursive ones
type mu[A] = Roll: (mu[A] -> A)
fun unroll[A](Roll(x): mu[A]) = x

fun ycomb[A, B] (f: (A -> B) -> (A -> B)): A -> B {
    fun l(x: mu[(A -> B)]): A -> B = fun (a: A) { f(unroll(x)(x))(a) }
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
EXPECT_EQ_(recfac(5), 120)

val recfib = ycomb(fib)
EXPECT_EQ_(recfib(8), 21)
})
