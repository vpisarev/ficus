// expect: did you mean
// diag-1 Phase C: a not-found identifier close (Levenshtein) to a visible name
// gets a gcc/clang-style "did you mean" suggestion.
val length = 5
val doubled = length * 2
val r = lenght + doubled
println(r)
