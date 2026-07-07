// FB-006: array-of-arrays via nested comprehension, then indexed.
val aa = [for i <- 0:2 {[for j <- 0:2 {i + j}]}]
val x = aa[1][0]
println(x)
