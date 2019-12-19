type 't point_ = {x: 't; y: 't}
type 't rect_ = {x: 't; y: 't; width: 't; height: 't}

fun contains(r: 'z rect_, p: 'z point_) =
    r.x <= p.x && p.x < r.x + r.width &&
    r.y <= p.y && p.y < r.y + r.height

fun string(p: 't point_) = "point {x=\(p.x), y=\(p.y)}"
fun string(r: 't rect_) = "rect {x=\(r.x), y=\(r.y), width=\(r.width), height=\(r.height)}"

val ip = point_ {x=1, y=2}
val fp = point_ {x=10.f, y=20.f}

val ir = rect_ {x=0, y=0, width=10, height=10}
val fr = rect_ {x=0.f, y=0.f, width=10.f, height=10.f}

fun inside_outside(r: 't rect_, p: 't point_) =
    if(contains(r, p)) "inside" else "outside of"

println("\(ip) is \(inside_outside(ir, ip)) \(ir)")
println("\(fp) is \(inside_outside(fr, fp)) \(fr)")
