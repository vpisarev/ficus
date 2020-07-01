/*type 't point_ = {x: 't; y: 't}
type 't rect_ = {x: 't; y: 't; width: 't; height: 't}

fun contains(r: 'z rect_, p: 'z point_) =
    r.x <= p.x < r.x + r.width &&
    r.y <= p.y < r.y + r.height

fun string(p: 't point_) = "point {x=\(p.x), y=\(p.y)}"
fun string(r: 't rect_) = "rect {x=\(r.x), y=\(r.y), width=\(r.width), height=\(r.height)}"

val ip = point_ {x=1, y=2}
val fp = point_ {x=10.f, y=20.f}

val ir = rect_ {x=0, y=0, width=10, height=10}
val fr = rect_ {x=0.f, y=0.f, width=10.f, height=10.f}

fun inside_outside(p: 't point_, r: 't rect_) =
    if contains(r, p) {"inside"} else {"outside of"}

println("\(ip) is \(inside_outside(ip, ir)) \(ir)")
println("\(fp) is \(inside_outside(fp, fr)) \(fr)")*/

//type pt = {x: int; y: int}
type tt = Empty | Node: {left: tt; v: int; right: tt}
val t0 = Node {left=Empty, v=5, right=Node{left=Empty, v=6, right=Empty}}

exception MySimpleException : tt
throw MySimpleException(t0)

//exception MyException: (int, string)

//throw MyException(5, "...4321, exception is thrown!")

//for c <- "hello" { print("'\(c)' ") }
//println()

//throw Fail("test")
