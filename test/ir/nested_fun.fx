// IR snapshot: nested function definition
fun outer(n: int) {
    fun inner(x: int) = x + 1
    inner(n) * 2
}
println(outer(7))
