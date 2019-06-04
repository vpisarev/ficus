fun fact(n: int) is
    fun fact_(n: int, p: int) = if n<=1 then p else fact_(n-1, p*(n-1)) fi
    fact_(n, 1)
end
