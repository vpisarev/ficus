fun fact(n: int) is
    fun fact_(n: int, p: int) = if n<=1 then p else fact_(n-1, p*(n-1)) fi
    fact_(n, 1)
end

/*fun rgb2gray(rgbimg: uint8 [,]) is
[
    for (r, g, b) in rgbimg =>
        sat_uint8(r*0.299 + g*0.587 + b*0.114)
]*/
