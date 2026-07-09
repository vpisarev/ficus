/*
    Helper module for test_resolve.fx (resolve-1): a mini generic complex-like
    class with the SAME over-general operator shapes as the fenced
    lib/Complex.fx:15-44 block (FB-007). The operators are ninja names, so a
    plain `import CplxHelper` makes them visible at every call site of the
    importing module -- exactly how the real Complex.fx operators leak into NN
    code and trigger FB-007's symptom S3.
*/

class 't cplx = {re: 't; im: 't}

fun cplx(r: 't, i: 't): 't cplx = cplx {re=r, im=i}

// the over-general shapes from lib/Complex.fx (S3's culprit is `+('t, 't cplx)`)
operator + (a: 't, b: 't cplx): 't cplx = cplx(a + b.re, b.im)
operator + (a: 't cplx, b: 't): 't cplx = cplx(a.re + b, a.im)
operator + (a: 't cplx, b: 't2 cplx): 't3 cplx = cplx(a.re + b.re, a.im + b.im)
operator - (a: 't cplx, b: 't2 cplx): 't3 cplx = cplx(a.re - b.re, a.im - b.im)
// S2's shape: 't cplx * int (the real Complex.fx `c * 2` resolved to the
// ARRAY __mul__ under greedy first-match)
operator * (a: int, b: 't cplx): 't cplx = cplx(a * b.re, a * b.im)
operator * (a: 't cplx, b: int): 't cplx = cplx(a.re * b, a.im * b)
operator * (a: 't cplx, b: 't2 cplx): 't3 cplx =
    cplx(a.re*b.re - a.im*b.im, a.re*b.im + a.im*b.re)

fun string(a: 't cplx): string {
    val s = if a.im >= (0 :> 't) {"+"} else {""}
    f"{a.re}{s}{a.im}i"
}
