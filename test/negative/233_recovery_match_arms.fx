// expect: undef_a
// diag-1: errors in two different match arms are BOTH reported (per-arm
// recovery); the healthy arm keeps the match well-typed.
fun classify(x: int): int {
    match x {
    | 0 => undef_a
    | 1 => undef_b
    | _ => 100
    }
}
println(classify(0))
