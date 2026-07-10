// expect: further diagnostic
// flags: -fmax-errors=2
// diag-1: with -fmax-errors=2, only the first two diagnostics print, then a
// "further diagnostics suppressed" summary line.
fun a(): int { undef_a }
fun b(): int { undef_b }
fun c(): int { undef_c }
fun d(): int { undef_d }
println(a() + b() + c() + d())
