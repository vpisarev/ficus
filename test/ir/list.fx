// IR snapshot: list cons and recursive match
fun sum(l: int list): int = match l {
    | a :: rest => a + sum(rest)
    | _ => 0
}
println(sum([:: 1, 2, 3, 4]))
