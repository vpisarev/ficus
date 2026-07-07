// IR snapshot: record construction and field access
type pt_t = {x: int; y: int}
val p = pt_t {x=3, y=4}
println(p.x + p.y)
