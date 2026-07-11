// expect: never assigned
// flags: -Werror
// fold-1 Phase 1: a fold whose accumulator is never assigned in the body
// is almost certainly a mistake; warn (promoted to an error here via -Werror).
// Writing 's = _' in the body would silence it.
val s = fold s = 0 for x <- [1, 2, 3] { println(x) }
println(s)
