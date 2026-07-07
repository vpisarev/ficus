// IR snapshot: while loop with mutation
var i = 0, s = 0
while i < 5 {
    s += i
    i += 1
}
println(s)
