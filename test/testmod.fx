fun fact(n: int)
{
    fun fact_(n: int, p: int) = if (n<=1) p else fact_(n-1, p*(n-1))
    fact_(n, 1)
}
