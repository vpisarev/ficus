// expect: undefined_root
// diag-1: one ROOT error; the dependent uses are structurally suppressed (the
// poisoned symbol carries TypErr, which flows silently). Exactly ONE diagnostic.
val x = undefined_root
val y = x + 1
val z = y * 2
println(z)
