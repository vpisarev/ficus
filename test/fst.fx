// the first test script
import testmod, Math, List

val str = "
привет! 你好吗?
BTW,
    2*2 = \(2*2),
    π (the area of circle with radius 1) = \(Math.pi),
    e = \(Math.exp(1.)),
    φ (golden ratio) = \((1+5.**0.5)/2)
"
val c = '😄'
println(str + c)

println("\nfactorial(20)=\(testmod.fact(20))\n")

fun fib(n: int) = if n <= 2 {1} else {fib(n-1) + fib(n-2)}
fun fib2(n: int) {
    fun fib2_(a: int, b: int, n: int) = if n <= 2 {a} else {fib2_(a+b, a, n-1)}
    fib2_(1, 1, n)
}

operator * (((a11, a12), (a21, a22)): (('t, 't), ('t, 't)),
            ((b11, b12), (b21, b22)): (('t, 't), ('t, 't))) =
    ((a11*b11 + a12*b21, a11*b12 + a12*b22),
     (a21*b11 + a22*b21, a21*b12 + a22*b22))
operator ** (a: (('t, 't), ('t, 't)), n: int)
{
    val _0 = (0:>'t), _1 = (1:>'t)
    var p = ((_1, _0), (_0, _1))
    if n == 0 {p}
    else {
        var a = a
        var n = n
        while n > 0 {
            if n % 2 == 0 {
                n /= 2
                a *= a
            }
            else {
                p *= a
                n -= 1
            }
        }
        p
    }
}

fun fib3(n: int) {
    val a = ((1, 1), (1, 0))
    val a = a ** n
    a.1.0
}

fun fib_seq_gen()
{
    var a=1, b=1
    fun next_fib()
    {
        val t = b
        b = a
        a += t
        t
    }
    next_fib
}
val fib_seq = fib_seq_gen()

for i <- 1:31 {
    fun foo() {
        print(fib(i))
    }
    print("fib(\(i))=")
    foo()
    print(", fib2(\(i))=\(fib2(i))")
    print(", fib3(\(i))=\(fib3(i))")
    println(", fib_seq()=\(fib_seq())")
}
println()

var i = 0
val a=[0, 1, 2, -10, 7]
do
{
    val v = a[i]
    if v < 0 {break}
    i += 1
}
while i < 5
val msg = if i >= size(a) {"not found"} else {"a[\(i)]=\(a[i])"}
println("negative number in \(a): \(msg)")

//exception BreakWith: int

type complex_t = {re: float; im: float}
val c = ref (complex_t {re=1.f, im=1.f})
val d = c->{re=c->re*2, im=c->im*2}
fun abs(c:complex_t) = Math.sqrt(c.re**2 + c.im**2)
println("abs((1+1i)*2)=\(abs(d))")

/*fun find_idx(a: 't [], elem: 't)
{
    val n = size(a)
    try
    {
        for i <- 0:n {if a[i] == elem {throw BreakWith(i)}}
        -1
    }
    catch
    {
    | BreakWith(i) => i
    }
}*/

val fixed_choice = "five"

val result = match fixed_choice
{
    | "пять" => "нашли 5"
    | "five" => "found 5"
    | "五" => "找到五个"
    | _ => "some other number"
}

println("\(fixed_choice) ==> \(result)")
assert(result == "found 5")

println(if 0.1 <= Math.sin(1.) < 0.7 {
        "sin(1) is between 0.1 and 0.7"
    } else if Math.sin(1.) < 0.1 {
        "sin(1) is smaller than 0.1"
    } else {
        "sin(1) is greater or equal than 0.7"
    })

fun assoc(l: ('a, 'b) list, k: 'a) =
    List.find_opt(l, fun ((a, b): ('a, 'b)) {a == k})

val key1 = "xyz", key2 = "b"
val pairs = ("a", 0) :: ("b", 33) :: ("rest", 2) :: []
val r1 = assoc(pairs, key1)
val r2 = assoc(pairs, key2)

fun assoc_result(r: ('a, 'b)?)
{
    | Some((x, y)) => repr(y)
    | _ => "not found"
}

println("find_opt '\(key1)' @ \(pairs) result: \(assoc_result(r1))")
println("find_opt '\(key2)' @ \(pairs) result: \(assoc_result(r2))")

val n = 30
val a = [for i <- 0:n {i+1}]
for i <- 1:n {a[i] += a[i-1]}
println("triangular numbers: \(a)")

nothrow fun is_prime(n: int)
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

val sorted = List.mergeSort(
    [: 10, 355, 113, -1, 2, 26, 1, 1949, 0, 299792458,
    -460, 451, -11034, 8848 :],
    fun (a: int, b: int) {a < b})
print("sorted: ")
println(sorted)

fun plot(a: float, b: float, f: float->float, w: int, h: int) {
    val step = (b - a)/w
    val tab = [for x <- 0:w {f(a + step*x)}]
    val v0 = tab[0]
    val fold (minv, maxv)=(v0, v0) for y <- tab { (min(minv, y), max(maxv, y)) }
    val scale = (h-1)/(maxv - minv)
    val itab = [for y <- tab {clip(Math.round((y-minv)*scale), 0, h-1)}]

    val screen: char [,] = array((h, w+1), ' ')
    for x <- 0:w {
        screen[h-itab[x]-1, x] = '*'
    }
    for y <- 0:h {
        println(screen[y,:])
    }
}

val a = (Math.pi*(-0.5) :> float), b = (-a*5 :> float)
plot(a, b, (Math.sin: float->float), 80, 10)
