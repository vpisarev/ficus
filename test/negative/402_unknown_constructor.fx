// expect: not found
type ab_t = A | B
val x = match A { | A => 0 | C => 1 }
println(x)
