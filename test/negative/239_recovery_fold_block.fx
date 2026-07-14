// expect: did you mean
// diag-1 / FB-022 (resolve-3 Phase C): a failed statement inside the BLOCK a
// `val fold` desugars to must not lose the accumulators. The typo `s1` is the
// only real error; the accumulators `sz`/`have_neg` keep their real int/bool
// types, so the later uses do NOT cascade into spurious "not found". Exactly
// ONE diagnostic, carrying the did-you-mean suggestion.
fun test(shape: int []): (int, bool) {
    val fold sz = 0, have_neg = false for s <- shape {
        if s >= 0 { sz *= s1 } else { have_neg = true }
    }
    println(f"have_neg: {have_neg}")
    (sz, have_neg)
}
println(test([1, 2, 3]))
