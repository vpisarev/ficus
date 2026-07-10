// expect: undef_a
// diag-1: three INDEPENDENT undefined identifiers in three functions -> three
// diagnostics in one run (recovery per top-level definition), sorted by line.
fun a(): int { undef_a + 1 }
fun b(): int { undef_b + 2 }
fun c(): int { undef_c + 3 }
println(a() + b() + c())
