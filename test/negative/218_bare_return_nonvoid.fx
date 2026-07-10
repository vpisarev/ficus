// expect: return statement type void is inconsistent
// ctrlflow-1: a bare 'return' (no value) now parses everywhere, but in a
// function with a non-void return type it must still be rejected -- with the
// return-type-mismatch message, not a parse error.
fun f(): int { for i <- 0:3 { if i == 1 { return } else {} }; 0 }
println(f())
