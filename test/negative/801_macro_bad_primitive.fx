// expect: unknown macro primitive '@bogus'
// macro-1: only @file, @line and @string are recognized macro primitives.
macro m(e: @expr): int = @bogus
val x = m(1)
println(x)
