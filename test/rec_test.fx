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

//type pt = (int, int)
//type tt = Empty | Node: {left: tt, v: int, right: tt}
//val t0 = Node {right=Node{left=Empty, v=6, right=Empty}, left=Empty, v=5}
//type tt = Empty | Node: (tt, int, tt)
//val t0 = Node (Empty, 5, Node(Empty, 6, Empty))

/*fun print_tt(a: tt, indent: int)
{
    | Node {left, v, right} =>
        print("Node{left=")
        print_tt(left)
        print(", \(v), right=")
        print_tt(right)
        print("}")
    | _ => print("Empty")
}

fun print_ttln(a: tt)
{
    print_tt(a, 0); println()
}*/

//exception MySimpleException
//throw MySimpleException

//exception MyException: tt
//throw MyException(t0)

//for c <- "hello" { print("'\(c)' ") }
//println()

//throw Fail("test")

exception BreakWith: int

fun find_neg(a: 't [])
{
    val n = size(a)
    try
    {
        for i <- 0:n {if a[i] > 0 {throw Fail(">0")} else if a[i] < 0 {throw BreakWith(i)}}
        -1
    }
    catch
    {
    | BreakWith(i) => i
    | OutOfRangeError => println("out of range error happened"); -100
    | Fail(s) => println("Failure '\(s)'"); -100
    //| _ => println("unknown exception"); -100
    }
}

val a=[0, 1, 2, -10, 7]
println("excepion-based search: negative number in \(a): \(find_neg(a))")
