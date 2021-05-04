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

@pure @nothrow fun floor(x: float): int = @ccode { int_ i = (int_)x; return i - (i > x) }
@pure @nothrow fun floor(x: double): int = @ccode { int_ i = (int_)x; return i - (i > x) }
@pure @nothrow fun ceil(x: float): int = @ccode { int_ i = (int_)x; return i + (i < x) }
@pure @nothrow fun ceil(x: double): int = @ccode { int_ i = (int_)x; return i + (i < x) }

@inline fun pow(x: float, y: float): float = __intrin_pow__(x, y)
@inline fun pow(x: double, y: double): double = __intrin_pow__(x, y)
@inline fun sqrt(x: float): float = __intrin_sqrt__(x)
@inline fun sqrt(x: double): double = __intrin_sqrt__(x)

@inline fun atan(x: float): float = __intrin_atan__(x)
@inline fun atan(x: double): double = __intrin_atan__(x)
@inline fun atan2(y: float, x: float): float = __intrin_atan2__(y, x)
@inline fun atan2(y: double, x: double): double = __intrin_atan2__(y, x)
@inline fun cos(x: float): float = __intrin_cos__(x)
@inline fun cos(x: double): double = __intrin_cos__(x)
@inline fun sin(x: float): float = __intrin_sin__(x)
@inline fun sin(x: double): double = __intrin_sin__(x)
@inline fun tan(x: float): float = __intrin_tan__(x)
@inline fun tan(x: double): double = __intrin_tan__(x)

@inline fun log(x: float): float = __intrin_log__(x)
@inline fun log(x: double): double = __intrin_log__(x)
@inline fun exp(x: float): float = __intrin_exp__(x)
@inline fun exp(x: double): double = __intrin_exp__(x)

@inline fun atanh(x: float): float = __intrin_atanh__(x)
@inline fun atanh(x: double): double = __intrin_atanh__(x)
@inline fun cosh(x: float): float = __intrin_cosh__(x)
@inline fun cosh(x: double): double = __intrin_cosh__(x)
@inline fun sinh(x: float): float = __intrin_sinh__(x)
@inline fun sinh(x: double): double = __intrin_sinh__(x)
@inline fun tanh(x: float): float = __intrin_tanh__(x)
@inline fun tanh(x: double): double = __intrin_tanh__(x)

@pure @nothrow fun isnan(x: float): bool = @ccode
{
    fx_bits32_t u;
    u.f = x;
    return (u.i & 0x7fffffff) > 0x7f800000;
}

@pure @nothrow fun isinf(x: float): bool = @ccode
{
    fx_bits32_t u;
    u.f = x;
    return (u.i & 0x7fffffff) == 0x7f800000;
}

@pure @nothrow fun isnan(x: double): bool = @ccode
{
    fx_bits64_t u;
    u.f = x;
    return (u.i & 0x7fffffffffffffffLL) > 0x7ff0000000000000LL;
}

@pure @nothrow fun isinf(x: double): bool = @ccode
{
    fx_bits64_t u;
    u.f = x;
    return (u.i & 0x7fffffffffffffffLL) == 0x7ff0000000000000LL;
}

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
