/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

fun fact(n: int)
{
    fun fact_(n: int, p: int) = if n<=1 {p} else {fact_(n-1, p*n)}
    fact_(n, 1)
}

fun rgb2gray(rgbimg: (uint8, uint8, uint8) [,]) =
[|
    for (r, g, b) <- rgbimg {
        sat_uint8(r*0.299f + g*0.587f + b*0.114f)
    }
|]
