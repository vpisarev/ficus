/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* math functions on scalars and arrays */

@ccode {
#include <math.h>
#include <float.h>
}

val pi = 3.1415926535897932384626433832795
val FLT_EPSILON: float = @ccode {FLT_EPSILON}
val DBL_EPSILON: double = @ccode {DBL_EPSILON}

fun round(x: float): int = Builtins.round(x)
fun round(x: double): int = Builtins.round(x)
fun round(x: float, n: int): float = Builtins.round(x, n)
fun round(x: double, n: int): double = Builtins.round(x, n)

@pure @nothrow fun floor(x: float): int = @ccode { int i = (int)x; return i - (i > x) }
@pure @nothrow fun floor(x: double): int = @ccode { int i = (int)x; return i - (i > x) }
@pure @nothrow fun ceil(x: float): int = @ccode { int i = (int)x; return i + (i < x) }
@pure @nothrow fun ceil(x: double): int = @ccode { int i = (int)x; return i + (i < x) }

@pure @nothrow fun pow(x: float, y: float): float = @ccode { return powf(x, y) }
@pure @nothrow fun pow(x: double, y: double): double = @ccode { return pow(x, y) }
@pure @nothrow fun sqrt(x: float): float = @ccode { return sqrtf(x) }
@pure @nothrow fun sqrt(x: double): double = @ccode { return sqrt(x) }

@pure @nothrow fun atan(x: float): float = @ccode { return atanf(x) }
@pure @nothrow fun atan(x: double): double = @ccode { return atan(x) }
@pure @nothrow fun atan2(y: float, x: float): float = @ccode { return atan2f(y, x) }
@pure @nothrow fun atan2(y: double, x: double): double = @ccode { return atan2(y, x) }
@pure @nothrow fun cos(x: float): float = @ccode { return cosf(x) }
@pure @nothrow fun cos(x: double): double = @ccode { return cos(x) }
@pure @nothrow fun sin(x: float): float = @ccode { return sinf(x) }
@pure @nothrow fun sin(x: double): double = @ccode { return sin(x) }
@pure @nothrow fun tan(x: float): float = @ccode { return tanf(x) }
@pure @nothrow fun tan(x: double): double = @ccode { return tan(x) }

@pure @nothrow fun log(x: float): float = @ccode { return logf(x) }
@pure @nothrow fun log(x: double): double = @ccode { return log(x) }
@pure @nothrow fun exp(x: float): float = @ccode { return expf(x) }
@pure @nothrow fun exp(x: double): double = @ccode { return exp(x) }

@pure @nothrow fun atanh(x: float): float = @ccode { return atanhf(x) }
@pure @nothrow fun atanh(x: double): double = @ccode { return atanh(x) }
@pure @nothrow fun cosh(x: float): float = @ccode { return coshf(x) }
@pure @nothrow fun cosh(x: double): double = @ccode { return cosh(x) }
@pure @nothrow fun sinh(x: float): float = @ccode { return sinhf(x) }
@pure @nothrow fun sinh(x: double): double = @ccode { return sinh(x) }
@pure @nothrow fun tanh(x: float): float = @ccode { return tanhf(x) }
@pure @nothrow fun tanh(x: double): double = @ccode { return tanh(x) }

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

fun gcd(n: int, d: int)
{
    fun gcd_(n: int, d: int) =
        if d == 0 {n} else {gcd_(d, n % d)}
    gcd_(abs(n), abs(d))
}
