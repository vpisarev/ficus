// expect: re-declared
fun f(a: int) = a
fun f(a: int) = a + 1
println(f(1))
