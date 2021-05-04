/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// From https://rosettacode.org/wiki/Y_combinator; adoped from OCaml implementation.
// The trick from Lambda calculus to convert non-recursive functions into recursive ones
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
println(f"5! = {recfac(5)}")

val recfib = ycomb(fib)
println(f"fib(8) = {recfib(8)}")
