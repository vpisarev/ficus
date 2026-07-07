// expect: not covered
type rgb_t = Red | Green | Blue
fun f(c: rgb_t) = match c { | Red => 1 }
println(f(Green))
