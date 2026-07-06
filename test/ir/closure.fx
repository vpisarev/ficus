// IR snapshot: closure capturing an enclosing binding
fun make_adder(n: int) {
    fun add(x: int) = x + n
    add
}
val add10 = make_adder(10)
println(add10(5))
