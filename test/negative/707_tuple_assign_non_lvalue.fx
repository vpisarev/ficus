// expect: not assignable in a tuple assignment
var a = 1
(a, b + 1) = (2, 3)
println(a)
