/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// example of imported module with various tiny functions

fun sqr(x:'t) = x * x
@pure fun mad(a: int, b: int, c: int): int = @ccode
{
    *fx_result = a*b + c;
    return FX_OK;
}
fun div3(x: 't) = x/(3.0 :> 't)

fun add_scaled(a: float [+], b: float [+], scale: float) =
    [| for x <- a, y <- b {x + y*scale} |]

fun sum_arr(arr: 't [+]) = fold s=(0:>'t) for x <- arr {s+x}
