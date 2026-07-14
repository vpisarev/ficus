/*
    Helper module for test_resolve.fx (resolve-1): a mini generic complex-like
    class with the SAME over-general operator shapes as the fenced
    lib/Complex.fx:15-44 block (FB-007). The operators are ninja names, so a
    plain `import CplxHelper` makes them visible at every call site of the
    importing module -- exactly how the real Complex.fx operators leak into NN
    code and trigger FB-007's symptom S3.
*/

class cplx[T] = {re: T; im: T}

fun cplx[T](r: T, i: T): cplx[T] = cplx {re=r, im=i}

// the over-general shapes from lib/Complex.fx (S3's culprit is `+('t, 't cplx)`)
operator +[T] (a: T, b: cplx[T]): cplx[T] = cplx(a + b.re, b.im)
operator +[T] (a: cplx[T], b: T): cplx[T] = cplx(a.re + b, a.im)
operator +[T, T2, T3] (a: cplx[T], b: cplx[T2]): cplx[T3] = cplx(a.re + b.re, a.im + b.im)
operator -[T, T2, T3] (a: cplx[T], b: cplx[T2]): cplx[T3] = cplx(a.re - b.re, a.im - b.im)
// S2's shape: 't cplx * int (the real Complex.fx `c * 2` resolved to the
// ARRAY __mul__ under greedy first-match)
operator *[T] (a: int, b: cplx[T]): cplx[T] = cplx(a * b.re, a * b.im)
operator *[T] (a: cplx[T], b: int): cplx[T] = cplx(a.re * b, a.im * b)
operator *[T, T2, T3] (a: cplx[T], b: cplx[T2]): cplx[T3] =
    cplx(a.re*b.re - a.im*b.im, a.re*b.im + a.im*b.re)

fun string[T](a: cplx[T]): string {
    val s = if a.im >= (0 :> T) {"+"} else {""}
    f"{a.re}{s}{a.im}i"
}
