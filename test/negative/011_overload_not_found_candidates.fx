// expect: is not found
// WP-E: no overload accepts a string; locks the candidate-listing format that
// report_not_found_typed prints today (the surgery will enrich it).
fun oo(a: int) = a
fun oo(a: float) = 1
val r = oo("x")
println(r)
