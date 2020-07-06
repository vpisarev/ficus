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

/*exception BreakWith: int

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
*/
//val arr=["false", "true", "maybe"]
//val idx = (5 <= 6)
//println("arr[\(idx)]=\(arr[idx])")
//val a = [1, 0; 0, 1;]
//println(a[: ])
//println(5 <=> 6)


/*type month = Jan | Feb:int | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

fun str(m: month)
{
    | Jan => "Jan"
    | Feb(n) => "Feb(\(n))"
    | Mar => "Mar"
    | Apr => "Apr"
    | May => "May"
    | Jun => "Jun"
    | Jul => "Jul"
    | Aug => "Aug"
    | Sep => "Sep"
    | Oct => "Oct"
    | Nov => "Nov"
    | Dec => "Dec"
}

for m <- [Jan, Feb(28), Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec] {
    println(str(m))
}
*/

type 't expand_t =
    | ExpandElem: 't
    | ExpandList: 't list
    | ExpandArray1D: 't []
    | ExpandArray2D: 't [,]
    | ExpandNextRow

fun str(a: 't expand_t) {
    | ExpandElem (v) => string(v)
    | ExpandList (l) => "\\" + string(l)
    | ExpandArray1D (a) => "\\" + string(a)
    | ExpandArray2D (a) => "\\" + string(a)
    | ExpandNextRow => ";"
}

exception BuildArrayError: string

fun build_array_2d(spec: 't expand_t []): 't [,]
{
    val fold (h, w0, i, j, curr_h, curr_w) = (0, 0, 0, 0, 0, 0) for s <- spec {
        match s {
        | ExpandElem v =>
            if curr_h > 1 {
                throw BuildArrayError("scalar at row \(i), column \(j) (0-based) occurs after multi-row array")
            }
            (h, w0, i, j+1, 1, curr_w+1)
        | ExpandList (l) =>
            val len = length(l)
            if curr_h > 1 {
                throw BuildArrayError("list at row \(i), column \(j) (0-based) occurs after multi-row array")
            }
            (h, w0, i, j+1, 1, curr_w+len)
        | ExpandArray1D (a) =>
            val len = size(a)
            if curr_h > 1 {
                throw BuildArrayError("1D array at row \(i), column \(j) (0-based) occurs after multi-row array")
            }
            (h, w0, i, j+1, 1, curr_w+len)
        | ExpandArray2D (a) =>
            val (ma, na) = size(a)
            if curr_h != 0 && curr_h != ma {
                throw BuildArrayError("2D array at row \(i), column \(j) (0-based) occurs after multi-row array")
            }
            (h, w0, i, j+1, ma, curr_w+na)
        | _ =>
            if w0 != 0 && w0 != curr_w {
                throw BuildArrayError("row \(i) (0-based) does not match the size of the previous rows")
            }
            (h + curr_h, curr_w, i+1, 0, 0, 0)
        }
    }

    val result = array((h, w0), (0 :> 't))
    ignore (fold (i, j, di) = (0, 0, 0) for s <- spec {
        match s {
        | ExpandElem v =>
            result[i, j] = v
            (i, j+1, 1)
        | ExpandList (l) =>
            val len = length(l)
            for v <- l, k <- 0: {
                result[i, j+k] = v
            }
            (i, j+len, 1)
        | ExpandArray1D (a) =>
            val len = size(a)
            for v <- a, k <- 0: {
                result[i, j+k] = v
            }
            (i, j+len, 1)
        | ExpandArray2D (a) =>
            val (ma, na) = size(a)
            for i1 <- 0:ma for j1 <- 0:na {
                result[i+i1, j+j1] = a[i1, j1]
            }
            (i, j+na, ma)
        | _ =>
            (i + di, 0, 0)
        }
    })
    result
}

/*val spec = [ExpandElem(5), ExpandElem(6), ExpandNextRow,
        ExpandElem(7), ExpandElem(8), ExpandNextRow]

//for s <- spec {println(str(s))}
//println(str(ExpandElem(5)))

val a = build_array_2d(spec)
println(size(a[:]))*/
val a = [1, 2, 3, 4]

val spec2 = [ExpandArray1D(a), ExpandNextRow,
            ExpandArray1D(a), ExpandNextRow,
            ExpandArray1D(a), ExpandNextRow,
            ExpandArray1D(a), ExpandNextRow,
            ExpandArray1D(a), ExpandNextRow,]
val b = build_array_2d(spec2)
println(b)
