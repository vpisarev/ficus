// expect: not found
type pt_t = {x: int; y: int}
val p = pt_t {x=1, y=2}
println(p.z)
