// expect: implicit return type
// flags: -Wall -Werror
// annotate-2/WP-1: -Wimplicit-rettype warns on a module-level function whose
// return type is left to inference; -Werror promotes it to a nonzero exit.
// Locks BOTH the warning message format (with the inferred type) and the
// -Werror promotion path. 'scaled' is annotated -> must NOT warn.
fun norm(x: double, y: double) = sqrt(x*x + y*y)
fun scaled(x: double): double = x*2.0
val z = norm(3.0, 4.0) + scaled(1.0)
println(z)
