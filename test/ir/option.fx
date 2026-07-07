// IR snapshot: option match (Some / None)
fun unwrap(o: int?) = match o {
    | Some(v) => v
    | _ => 0
}
println(unwrap(Some(5)))
println(unwrap(None))
