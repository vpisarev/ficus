// from https://rosettacode.org/wiki/Y_combinator; adoped from OCaml implementation.
// trick from Lambda calculus to convert non-recursive functions into recursive ones
type 'a mu = Roll: ('a mu -> 'a)
fun unroll(x: 'a mu) {
    | Roll (x) => x
}

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
println("5! = {recfac(5)}")

val recfib = ycomb(fib)
println("fib(8) = {recfib(8)}")
