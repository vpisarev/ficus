// expect: non-list
val x = match 5 { | a :: rest => 1 | _ => 0 }
println(x)
