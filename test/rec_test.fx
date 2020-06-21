/*import Args, Math

type 't point_ = {x: 't; y: 't}
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
println("\(fp) is \(inside_outside(fp, fr)) \(fr)")

fun is_prime(n: int)
{
    if n <= 1 {false} else if n % 2 == 0 {n == 2}
    else {
        fold r=true for p<-3:Math.floor(Math.sqrt(n :> double))+1:2 {
            if n % p == 0 {break with false};
            r
        }
    }
}

println("primes <100: \([: for i <- 0:100 {if !is_prime(i) {continue}; i} :])")
*/

//import List
//val sorted = List.mergeSort([: 2, -1, 100, 8, 7 :], fun (a: int, b: int) {a < b})
//println("sorted: \(sorted)")

/*val fpair = List.find_opt(("a", 0) :: ("b", 1) :: ("rest", 2) :: [], fun ((key, i): (string, int)) {key == "xyz"})

println(match fpair {
    | Some((x, y)) => y
    | _ => -1
    })
*/
/*import Math

type complex_t = {re: float; im: float}
val c = ref(complex_t {re=0.f, im=1.f})
val d = c->{re=c->re*2, im=c->im*2}
fun abs(c:complex_t) = Math.sqrt(c.re**2 + c.im**2)
println("abs(d)=\(abs(d))")*/
/*
import List
val lstr = [: "2", "-1", "-", "100", "8", "7" :]
val l = [: 2, -1, 5, 100, 8, 7 :]

val lstr = lstr + lstr + lstr + lstr + lstr
val l = l + l + l + l + l
val sorted = List.mergeSort(lstr, fun (a: string, b: string) {a > b})
println("sorted: \(sorted)")
*/

//println([: ref (1, 2), ref (2, 3), ref (3, 4) :])
//println(ref("a"))
//println("a" :: [])

fun fib_seq()
{
    val a=ref(1), b=ref(1)
    fun next_fib()
    {
        val t = *b
        (*b) = *a
        (*a) += t
        t
    }
    next_fib
}
val fib3 = fib_seq()

for i <- 1:31 {
    println("fib3(\(i))=\(fib3())")
}
