// IR snapshot: function passed as an argument
fun apply2(f: int -> int, x: int) = f(f(x))
println(apply2(fun (y: int) {y * 2}, 3))
