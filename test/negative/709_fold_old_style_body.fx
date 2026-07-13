// expect: improper type of the arithmetic operation result
// fold-1: the fold body is now void, so an OLD-style body that yields a value
// instead of updating the accumulator (`s + x` here, meant to be `s += x`) is a
// type error — the value has nowhere to go. (There is no longer a separate
// 'never assigned' warning; the void-body type error is the diagnostic.)
val s = fold s = 0 for x <- [1, 2, 3] { s + x }
println(s)
