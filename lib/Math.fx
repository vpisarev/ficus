/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* math functions on scalars and arrays */

@ccode {
#include <math.h>
#include <float.h>
}

val M_PI = 3.1415926535897932384626433832795
val M_E = 2.71828182845904523536028747135266
val M_LOG2 = 0.693147180559945309417232121458176568

val FLT_EPSILON: float = @ccode {FLT_EPSILON}
val DBL_EPSILON: double = @ccode {DBL_EPSILON}

@pure @nothrow fun floor(x: float): int = @ccode { int i = (int)x; return i - (i > x) }
@pure @nothrow fun floor(x: double): int = @ccode { int i = (int)x; return i - (i > x) }
@pure @nothrow fun ceil(x: float): int = @ccode { int i = (int)x; return i + (i < x) }
@pure @nothrow fun ceil(x: double): int = @ccode { int i = (int)x; return i + (i < x) }

fun pow(x: float, y: float): float = __intrin_pow__(x, y)
fun pow(x: double, y: double): double = __intrin_pow__(x, y)
fun sqrt(x: float): float = __intrin_sqrt__(x)
fun sqrt(x: double): double = __intrin_sqrt__(x)

fun atan(x: float): float = __intrin_atan__(x)
fun atan(x: double): double = __intrin_atan__(x)
fun atan2(y: float, x: float): float = __intrin_atan2__(y, x)
fun atan2(y: double, x: double): double = __intrin_atan2__(y, x)
fun cos(x: float): float = __intrin_cos__(x)
fun cos(x: double): double = __intrin_cos__(x)
fun sin(x: float): float = __intrin_sin__(x)
fun sin(x: double): double = __intrin_sin__(x)
fun tan(x: float): float = __intrin_tan__(x)
fun tan(x: double): double = __intrin_tan__(x)

fun log(x: float): float = __intrin_log__(x)
fun log(x: double): double = __intrin_log__(x)
fun exp(x: float): float = __intrin_exp__(x)
fun exp(x: double): double = __intrin_exp__(x)

fun atanh(x: float): float = __intrin_atanh__(x)
fun atanh(x: double): double = __intrin_atanh__(x)
fun cosh(x: float): float = __intrin_cosh__(x)
fun cosh(x: double): double = __intrin_cosh__(x)
fun sinh(x: float): float = __intrin_sinh__(x)
fun sinh(x: double): double = __intrin_sinh__(x)
fun tanh(x: float): float = __intrin_tanh__(x)
fun tanh(x: double): double = __intrin_tanh__(x)

fun hypot(a: 't, b: 't) {
    val aa = abs(a)
    val ab = abs(b)
    if aa > ab {
        val r = ab/aa
        aa*sqrt(1 + r*r)
    } else if ab > (0 :> 't) {
        val r = aa/ab
        ab*sqrt(1 + r*r)
    } else {
        ab
    }
}

fun GCD(n: int, d: int)
{
    fun GCD_(n: int, d: int) =
        if d == 0 {n} else {GCD_(d, n % d)}
    GCD_(abs(n), abs(d))
}
