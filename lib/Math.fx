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
@pure @nothrow fun trunc(x: float): int = @ccode { return (int_)x }
@pure @nothrow fun trunc(x: double): int = @ccode { return (int_)x }

@inline fun pow(x: float, y: float): float = __intrin_pow__(x, y)
@inline fun pow(x: double, y: double): double = __intrin_pow__(x, y)
@inline fun sqrt(x: float): float = __intrin_sqrt__(x)
@inline fun sqrt(x: double): double = __intrin_sqrt__(x)

@inline fun atan(x: float): float = __intrin_atan__(x)
@inline fun atan(x: double): double = __intrin_atan__(x)
@inline fun atan2(y: float, x: float): float = __intrin_atan2__(y, x)
@inline fun atan2(y: double, x: double): double = __intrin_atan2__(y, x)
@inline fun asin(x: float): float = __intrin_asin__(x)
@inline fun asin(x: double): double = __intrin_asin__(x)
@inline fun acos(x: float): float = __intrin_acos__(x)
@inline fun acos(x: double): double = __intrin_acos__(x)
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
@inline fun asinh(x: float): float = __intrin_asinh__(x)
@inline fun asinh(x: double): double = __intrin_asinh__(x)
@inline fun acosh(x: float): float = __intrin_acosh__(x)
@inline fun acosh(x: double): double = __intrin_acosh__(x)
@inline fun cosh(x: float): float = __intrin_cosh__(x)
@inline fun cosh(x: double): double = __intrin_cosh__(x)
@inline fun sinh(x: float): float = __intrin_sinh__(x)
@inline fun sinh(x: double): double = __intrin_sinh__(x)
@inline fun tanh(x: float): float = __intrin_tanh__(x)
@inline fun tanh(x: double): double = __intrin_tanh__(x)

@pure @nothrow fun isnan(x: float): bool
@ccode {
    fx_bits32_t u;
    u.f = x;
    return (u.i & 0x7fffffff) > 0x7f800000;
}

@pure @nothrow fun isinf(x: float): bool
@ccode {
    fx_bits32_t u;
    u.f = x;
    return (u.i & 0x7fffffff) == 0x7f800000;
}

@pure @nothrow fun isnan(x: double): bool
@ccode {
    fx_bits64_t u;
    u.f = x;
    return (u.i & 0x7fffffffffffffffLL) > 0x7ff0000000000000LL;
}

@pure @nothrow fun isinf(x: double): bool
@ccode {
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

/*
    This is port of xoshiro256++ (and splitmix64 as initializer) from
    https://prng.di.unimi.it. Below is the original copyright and
    description:

    ---
    Written in 2019 by David Blackman and Sebastiano Vigna (vigna@acm.org)

    To the extent possible under law, the author has dedicated all copyright
    and related and neighboring rights to this software to the public domain
    worldwide. This software is distributed without any warranty.

    See <http://creativecommons.org/publicdomain/zero/1.0/>.

    This is xoshiro256++ 1.0, one of our all-purpose, rock-solid generators.
    It has excellent (sub-ns) speed, a state (256 bits) that is large
    enough for any parallel application, and it passes all tests we are
    aware of.

    For generating just floating-point numbers, xoshiro256+ is even faster.

    The state must be seeded so that it is not everywhere zero. If you have
    a 64-bit seed, we suggest to seed a splitmix64 generator and use its
    output to fill s.
    ---
*/
class RNG
{
    var state: (uint64*4)
}

@ccode {
    FX_INLINE uint64_t _fx_rng_rotl(uint64_t x, int k)
    {
        return (x << k) | (x >> (64 - k));
    }
    static uint64_t _fx_rng_next(uint64_t* s)
    {
        const uint64_t result = _fx_rng_rotl(s[0] + s[3], 23) + s[0];
        const uint64_t t = s[1] << 17;
        s[2] ^= s[0]; s[3] ^= s[1];
        s[1] ^= s[2]; s[0] ^= s[3];
        s[2] ^= t; s[3] = _fx_rng_rotl(s[3], 45);
        return result;
    }
}

fun RNG(seed: uint64)
{
    // initialize the state using splitmix64 generator
    @pure @nothrow fun nextmix64(x: uint64): uint64 =
    @ccode {
        uint64_t z = (x += 0x9e3779b97f4a7c15);
        z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
        z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
        return z ^ (z >> 31);
    }
    val x = nextmix64(seed)
    val y = nextmix64(x)
    val z = nextmix64(y)
    val w = nextmix64(z)
    RNG {state=(x, y, z, w)}
}

@pure @nothrow fun next(rng: RNG): uint64 = @ccode { return _fx_rng_next(&rng->u.RNG.t0); }

fun double(rng: RNG) = next(rng)*5.42101086242752217003726400434970855712890625e-20
fun float(rng: RNG) = float(next(rng)*5.42101086242752217003726400434970855712890625e-20)
fun bool(rng: RNG) = int64(next(rng)) < 0L

fun uniform(rng: RNG, a: uint8, b: uint8) =
    uint8((next(rng) % (b - a + 1 :> uint64) :> int) + a)

fun uniform(rng: RNG, a: uint16, b: uint16) =
    uint16((next(rng) % (b - a + 1 :> uint64) :> int) + a)

fun uniform(rng: RNG, a: uint32, b: uint32) =
    uint32((next(rng) % (b - a + 1u32 :> uint64) :> uint32) + a)

fun uniform(rng: RNG, a: int, b: int) =
    (next(rng) % (b - a + 1 :> uint64) :> int) + a

fun uniform(rng: RNG, a: float, b: float) =
    float(rng)*(b - a) + a

fun uniform(rng: RNG, a: double, b: double) =
    double(rng)*(b - a) + a

fun jump(rng: RNG): RNG
{
    val result = RNG {state=(0UL,0UL,0UL,0UL)}
    @nothrow fun jump_(rng: RNG, result: RNG): void
    @ccode {
        const uint64_t jump[] = {
            0x180ec6d33cfd0abaULL,
            0xd5a61266f0c9392cULL,
            0xa9582618e03fc9aaULL,
            0x39abdc4529b1661cULL,
        };
        uint64_t* s = &result->u.RNG.t0;
        result->u.RNG = rng->u.RNG;
        uint64_t s0 = 0, s1 = 0, s2 = 0, s3 = 0;
        for(int i = 0; i < 4; i++) {
            uint64_t c = jump[i];
            for(int b = 0; b < 64; b++) {
                if (c & (1ULL << b)) {
                    s0 ^= s[0]; s1 ^= s[1];
                    s2 ^= s[2]; s3 ^= s[3];
                }
                _fx_rng_next(s);
            }
        }
        s[0] = s0; s[1] = s1; s[2] = s2; s[3] = s3;
    }
    jump_(rng, result)
    result
}
