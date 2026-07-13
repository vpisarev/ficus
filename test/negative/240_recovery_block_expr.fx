// expect: not found
// diag-1 / FB-022 (resolve-3 Phase C): the recovery generalizes beyond fold to
// ANY block expression in value position. A failed middle statement (`foo`
// undefined) is the only real error; the annotation `: int` is the firewall, so
// `r` stays typed int and its later use does NOT cascade. Exactly ONE diagnostic.
fun test(): int {
    val r: int = {
        val a = 10
        foo(a)
        a * 2
    }
    r + 1
}
println(test())
