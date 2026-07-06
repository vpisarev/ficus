// IR snapshot: tail-recursive accumulator loop
fun fact(n: int, acc: int): int = if n <= 1 { acc } else { fact(n - 1, acc * n) }
println(fact(10, 1))
