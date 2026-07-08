// expect: is not found
// WP-E: keyword mismatch (unknown keyword 'z').
fun kk(~a: int) = a
val r = kk(z=5)
println(r)
