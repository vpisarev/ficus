// expect: variant
val x = match 5 { | Some y => 1 | _ => 0 }
println(x)
