// expect: cannot use 'continue' outside of loop
// ctrlflow-1: 'continue' as a match-arm value still requires an enclosing loop.
fun f(x: int): int = match x { | 0 => continue | v => v }
println(f(3))
