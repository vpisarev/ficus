// IR snapshot: list cons and recursive match
fun sum(l: list[int]): int = match l {
    | a :: rest => a + sum(rest)
    | _ => 0
}
println(sum([:: 1, 2, 3, 4]))
