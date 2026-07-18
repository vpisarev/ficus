// expect: a 'match'-shorthand body ('{| ... }') is not allowed
// macro-1: a macro body must be a full expression, not the match shorthand.
macro classify(e: @expr): string {| 0 => "zero" | _ => "nonzero" }
println(classify(0))
