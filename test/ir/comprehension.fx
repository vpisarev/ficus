// IR snapshot: array comprehension with a helper function call
fun sqr(x: int) = x*x
val a = [for i <- 0:5 {sqr(i)}]
println(a)
