/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
class 't complex = {re: 't; im: 't}
type fcomplex = float complex
type dcomplex = double complex

fun complex(r: float, i: float) = complex {re=r, im=i}
fun complex(r: double, i: double) = complex {re=r, im=i}
fun conj(a: 't complex) = complex {re=a.re, im=-a.im}
fun abs(a: 't complex) = sqrt(a.re*a.re + a.im*a.im)
fun phase(a: 't complex) = atan2(a.im, a.re)

operator + (a: 't complex, b: 't): 't complex = complex(a.re + b, a.im)
operator + (a: 't complex, b: 't complex) = complex(a.re + b.re, a.im + b.im)
operator - (a: 't complex, b: 't): 't complex = complex(a.re - b, a.im)
operator - (a: 't complex, b: 't complex) = complex(a.re - b.re, a.im - b.im)
operator * (a: 't complex, b: 't): 't complex = complex(a.re * b, a.im * b)
operator * (a: 't complex, b: 't complex) =
    complex(a.re*b.re - a.im*b.im, a.re*b.im + a.im*b.re)
operator / (a: 't complex, b: 't): 't complex = complex(a.re/b, a.im/b)
operator / (a: 't complex, b: 't complex) {
    val denom = b.re*b.re + b.im*b.im
    complex((a.re*b.re + a.im*b.im)/denom, (a.im*b.re - a.re*b.im)/denom)
}

/* The scalar-on-the-LEFT variants stay FENCED for now (FB-007, S3):
   `op (a: 't, b: 't complex)` has an unconstrained 't in the first position,
   so at a call site whose SECOND operand's type is still a free inference
   variable (the canonical case: a fold accumulator initialized with `[]`,
   as in `rev_more_ops + prog` at NN/FromOnnx.fx:1012) it is viable alongside
   the list-concatenation `+('t list, 't list)`, the two are genuinely
   incomparable, and the under-constrained-tie fallback picks by import order
   -- wrongly. They can return when either (a) session-2 deferral lands
   (resolution of under-constrained calls is postponed until the variable is
   bound, at which point these variants stop being viable at list sites), or
   (b) list concatenation stops being spelled `+` (see the `++` /
   dot-concatenation idea in docs/language_changes_brief.md), which removes
   the colliding candidate altogether. Until then: write `c + s`, not `s + c`.

operator + (a: 't, b: 't complex): 't complex = complex(a + b.re, b.im)
operator - (a: 't, b: 't complex): 't complex = complex(a - b.re, b.im)
operator * (a: 't, b: 't complex): 't complex = complex(a * b.re, a * b.im)
operator / (a: 't, b: 't complex): 't complex {
    val denom = b.re*b.re + b.im*b.im
    complex(a*b.re/denom, -a*b.im/denom)
}
*/

fun exp(a: 't complex) {
    val er = exp(a.re)
    complex(er*cos(a.im), er*sin(a.im))
}

// log(a: 't complex) has multiple values:
//    log(abs(a)) + i*phase(a) + i*2*M_PI*k, where k is arbitrary integer
fun log(a: 't complex) = complex(log(abs(a)), phase(a))
fun cos(a: 't complex) = complex(cos(a.re)*cosh(a.im), -sin(a.re)*sinh(a.im))
fun sin(a: 't complex) = complex(sin(a.re)*cosh(a.im), cos(a.re)*sinh(a.im))
fun cosh(a: 't complex) = complex(cosh(a.re)*cos(a.im), sinh(a.re)*sin(a.im))
fun sinh(a: 't complex) = complex(sinh(a.re)*cos(a.im), cosh(a.re)*sin(a.im))

fun string(a: 't complex) {
    val s = if a.im >= 0 {"+"} else {""}
    f"{a.re}{s}{a.im}i"
}

fun print(a: 't complex) {
    print(a.re)
    if a.im >= 0 {print("+")}
    print(a.im)
    print("i")
}
