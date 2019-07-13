fun fact(n: int)
{
    fun fact_(n: int, p: int) = if (n<=1) p else fact_(n-1, p*(n-1))
    fact_(n, 1)
}

/*fun rgb2gray(rgbimg: uint8 [,]) =
[
    for (r, g, b) in rgbimg =>
        sat_uint8(r*0.299 + g*0.587 + b*0.114)
]*/
