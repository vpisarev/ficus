/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

fun fact(n: long)
{
    fun fact_(n: long, p: long) = if n<=1L {p} else {fact_(n-1L, p*n)}
    fact_(n, 1L)
}

fun rgb2gray(rgbimg: (uint8, uint8, uint8) [,]) =
[
    for (r, g, b) <- rgbimg {
        sat_uint8(r*0.299f + g*0.587f + b*0.114f)
    }
]
