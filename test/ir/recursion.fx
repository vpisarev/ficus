// IR snapshot: non-tail recursion (fib)
fun fib(n: int): int = if n < 2 { n } else { fib(n - 1) + fib(n - 2) }
println(fib(10))
