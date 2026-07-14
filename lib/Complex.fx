/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
class complex[T] = {re: T; im: T}
type fcomplex = complex[float]
type dcomplex = complex[double]

fun complex(r: float, i: float): complex[float] = complex {re=r, im=i}
fun complex(r: double, i: double): complex[double] = complex {re=r, im=i}
fun conj[T](a: complex[T]): complex[T] = complex {re=a.re, im=-a.im}
fun abs[T](a: complex[T]): T = sqrt(a.re*a.re + a.im*a.im)
fun phase[T](a: complex[T]): T = atan2(a.im, a.re)

/* All operators are mixed-type (T1 op T2): the result type is inferred
   from the body, where builtin numeric coercion combines the two types at
   instantiation -- so `1 + 1.fi` works and `1.0 * fcomplex` widens to
   complex[double] (the widening-cast idiom). For + and - one component of
   the result does not naturally pass through a mixed operation; it is
   nudged to the coerced type by adding `r - r` (Vadim's pattern), which
   constant folding provably erases at BOTH -O0 and -O3, leaving a bare
   cast in the K-form -- so no runtime cost and no inf/nan artifacts.

   The `: complex[T3]` return annotations (a FRESH type var: "the result is
   complex[SOME]") are LOAD-BEARING, not decoration: at a call site whose
   result type is already expected to be a non-complex (e.g. a still-free
   fold/recursion accumulator concatenated into an annotated list, the
   FB-007/S3 shape -- C_gen_code.fx:1328 in the compiler itself), the
   return-type unification fails and the candidate is rejected at the
   viability trial, so it cannot steal the call from list-concatenation
   via the under-constrained env-order fallback. Without them the mixed
   variants (whose inferred return is a free var that unifies with
   anything) broke the compiler's own build. */
operator + [T1, T2, T3](a: T1, b: complex[T2]): complex[T3] {
    val r = a + b.re
    complex(r, b.im + (r - r))
}
operator + [T1, T2, T3](a: complex[T1], b: T2): complex[T3] {
    val r = a.re + b
    complex(r, a.im + (r - r))
}
operator + [T1, T2, T3](a: complex[T1], b: complex[T2]): complex[T3] =
    complex(a.re + b.re, a.im + b.im)
operator - [T1, T2, T3](a: T1, b: complex[T2]): complex[T3] {
    val r = a - b.re
    complex(r, (r - r) - b.im)
}
operator - [T1, T2, T3](a: complex[T1], b: T2): complex[T3] {
    val r = a.re - b
    complex(r, a.im + (r - r))
}
operator - [T1, T2, T3](a: complex[T1], b: complex[T2]): complex[T3] =
    complex(a.re - b.re, a.im - b.im)
operator * [T1, T2, T3](a: T1, b: complex[T2]): complex[T3] = complex(a * b.re, a * b.im)
operator * [T1, T2, T3](a: complex[T1], b: T2): complex[T3] = complex(a.re * b, a.im * b)
operator * [T1, T2, T3](a: complex[T1], b: complex[T2]): complex[T3] =
    complex(a.re*b.re - a.im*b.im, a.re*b.im + a.im*b.re)
operator / [T1, T2, T3](a: complex[T1], b: T2): complex[T3] = complex(a.re/b, a.im/b)
operator / [T1, T2, T3](a: complex[T1], b: complex[T2]): complex[T3] {
    val denom = b.re*b.re + b.im*b.im
    complex((a.re*b.re + a.im*b.im)/denom, (a.im*b.re - a.re*b.im)/denom)
}
operator / [T1, T2, T3](a: T1, b: complex[T2]): complex[T3] {
    val denom = b.re*b.re + b.im*b.im
    complex(a*b.re/denom, -a*b.im/denom)
}

fun exp[T](a: complex[T]): complex[T] {
    val er = exp(a.re)
    complex(er*cos(a.im), er*sin(a.im))
}

// log(a: complex[T]) has multiple values:
//    log(abs(a)) + i*phase(a) + i*2*M_PI*k, where k is arbitrary integer
fun log[T](a: complex[T]): complex[T] = complex(log(abs(a)), phase(a))
fun cos[T](a: complex[T]): complex[T] = complex(cos(a.re)*cosh(a.im), -sin(a.re)*sinh(a.im))
fun sin[T](a: complex[T]): complex[T] = complex(sin(a.re)*cosh(a.im), cos(a.re)*sinh(a.im))
fun cosh[T](a: complex[T]): complex[T] = complex(cosh(a.re)*cos(a.im), sinh(a.re)*sin(a.im))
fun sinh[T](a: complex[T]): complex[T] = complex(sinh(a.re)*cos(a.im), cosh(a.re)*sin(a.im))

fun string[T](a: complex[T]): string {
    val s = if a.im >= 0 {"+"} else {""}
    f"{a.re}{s}{a.im}i"
}

fun print[T](a: complex[T]): void {
    print(a.re)
    if a.im >= 0 {print("+")}
    print(a.im)
    print("i")
}
