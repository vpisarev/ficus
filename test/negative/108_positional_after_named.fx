// expect: positional
fun g(~a: int, ~b: int) = a + b
println(g(a=1, 2))
