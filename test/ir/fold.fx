// IR snapshot: fold loop
val s = fold acc = 0 for x <- [1, 2, 3, 4, 5] { acc + x }
println(s)
