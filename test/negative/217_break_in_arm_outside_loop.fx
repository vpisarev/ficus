// expect: cannot use 'break' outside of loop
// ctrlflow-1: 'break' is now legal as a match-arm value, but legality still
// requires an enclosing loop. This locks the EXPRESSION-position diagnostic
// (previously the arm parsed then mis-typed; now it reports the real reason).
fun f(x: int): int = match x { | 0 => break | v => v }
println(f(3))
