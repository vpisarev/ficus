// expect: bool
val x = match 5 { | a when a => 1 | _ => 0 }
println(x)
