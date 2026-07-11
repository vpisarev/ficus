// expect: must UPDATE the accumulator
// fold-1 flip: after `fold` became the new imperative form, an OLD-style body
// that yields a value instead of updating the accumulator is a mistake; the
// 'never assigned' diagnostic points the way ('s += x' here).
val s = fold s = 0 for x <- [1, 2, 3] { s + x }
println(s)
