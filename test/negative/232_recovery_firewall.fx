// expect: undefined_thing
// diag-1: a function with a broken body but a DECLARED return type keeps its
// signature (the pre-registration firewall). A caller that uses it correctly
// (r) stays clean; only the genuinely mistyped caller (bad) errors. So: the
// body error + the one real caller mismatch = two diagnostics, no cascade.
fun f(x: int): int { undefined_thing + x }
val r: int = f(5)
val bad: string = f(5)
println(r)
