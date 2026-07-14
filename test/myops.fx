/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// example of imported module with various tiny functions

fun sqr[T](x:T): T = x * x
@pure fun mad(a: int, b: int, c: int): int
@ccode {
    *fx_result = a*b + c;
    return FX_OK;
}
fun div3[T](x: T): T = x/(3.0 :> T)

fun add_scaled(a: float [+], b: float [+], scale: float): float [+] =
    [for x <- a, y <- b {x + y*scale}]

fun sum_arr[T](arr: T [+]): T = fold s=(0:>T) for x <- arr {s+=x}
