// expect: reference
val x = match 5 { | ref y => 1 | _ => 0 }
println(x)
