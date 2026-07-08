// expect: is not found
// WP-E: arity mismatch -- locks today's not-found diagnostic format so the
// resolver surgery's improved message is a reviewed golden change.
fun ff(a: int, b: int) = a + b
val r = ff(1)
println(r)
