// expect: "severity": "warning"
// flags: -diag-format=json -Wall -Werror
// lsp-1: locks json-mode severity mapping for a warning. -Wimplicit-rettype
// emits a warning (as a json line with "severity": "warning"); -Werror forces
// the nonzero exit this harness requires. 'norm' warns; 'scaled' is annotated.
fun norm(x: double, y: double) = sqrt(x*x + y*y)
fun scaled(x: double): double = x*2.0
val z = norm(3.0, 4.0) + scaled(1.0)
println(z)
