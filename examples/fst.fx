// the first test script
import testmod

val str = f"
привет! 你好吗?
BTW,
    2*2 = {2*2},
    π (the area of circle with radius 1) = {M_PI},
    e = {exp(1.)},
    φ (golden ratio) = {(1+5.**0.5)/2}
"
val c = '😄'
println(str + c)

println(f"\nfactorial(20)={testmod.fact(20)}\n")

fun fib(n: int) = if n <= 2 {1} else {fib(n-1) + fib(n-2)}
fun fib2(n: int) {
    fun fib2_(a, b, n) = if n <= 2 {a} else {fib2_(a+b, a, n-1)}
    fib2_(1, 1, n)
}

operator * (((a11, a12), (a21, a22)): (('t * 2) * 2),
            ((b11, b12), (b21, b22)): (('t * 2) * 2)) =
    ((a11*b11 + a12*b21, a11*b12 + a12*b22),
     (a21*b11 + a22*b21, a21*b12 + a22*b22))
operator ** (a: (('t * 2) * 2), n: int)
{
    val _0 = (0:>'t), _1 = (1:>'t)
    var p = ((_1, _0), (_0, _1))
    var a = a, n = n
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

fun fib3(n: int) {
    val a = ((1, 1), (1, 0))
    val a = a ** n
    a.1.0
}

fun start_fib_seq()
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
val fib_seq = start_fib_seq()

for i <- 1:31 {
    fun foo() {
        print(fib(i))
    }
    print(f"fib({i})=")
    foo()
    print(f", fib2({i})={fib2(i)}")
    print(f", fib3({i})={fib3(i)}")
    println(f", fib_seq()={fib_seq()}")
}
println()

val a=[|0, 1, 2, -10, 7, -3|]
var i1 = -1
for x@i <- a {
    if x < 0 {i1 = i; break}
}

fun gen_msg(i: int, a: 't []) = if i < 0 || i >= size(a) {"not found"} else {f"a[{i}]={a[i]}"}
println(f"imperative search: negative number in {a}: {gen_msg(i1, a)}")

val i2 = find_opt(for i<-0:size(a) {a[i] < 0}).value_or(-1)
println(f"fold-based search: negative number in {a}: {gen_msg(i2, a)}")

exception BreakWith: int

fun find_idx(a: 't [], f: 't -> bool): int
{
    val n = size(a)
    try
    {
        for i <- 0:n {if f(a[i]) {throw BreakWith(i)}}
        -1
    }
    catch
    {
    | BreakWith(i) => i
    }
}
val i3 = find_idx(a, fun (i) {i < 0})
println(f"excepion-based search: negative number in {a}: {gen_msg(i3, a)}")

type complex_t = {re: float; im: float}
val c = ref (complex_t {re=1.f, im=1.f})
val d = c->{re=c->re*2, im=c->im*2}
fun abs(c:complex_t) = sqrt(c.re**2 + c.im**2)
println(f"abs((1+1i)*2)={abs(d)}")

val fixed_choice = "five"

val result = match fixed_choice
{
    | "пять" => "нашли 5"
    | "five" => "found 5"
    | "五" => "找到五个"
    | _ => "some other number"
}

println(f"{fixed_choice} => {result}")
assert(result == "found 5")

println(if 0.1 <= sin(1.) < 0.7 {
        "sin(1) is between 0.1 and 0.7"
    } else if sin(1.) < 0.1 {
        "sin(1) is smaller than 0.1"
    } else {
        "sin(1) is no smaller than 0.7"
    })

val key1 = "xyz", key2 = "b"
val pairs = ("a", 0) :: ("b", 33) :: ("rest", 2) :: []
val r1 = pairs.assoc_opt(key1)
val r2 = pairs.assoc_opt(key2)

fun assoc_result(r: 'x?)
{
    | Some x => repr(x)
    | _ => "not found"
}

println(f"assoc '{key1}' @ {pairs}: {assoc_result(r1)}")
println(f"assoc '{key2}' @ {pairs}: {assoc_result(r2)}")

val n = 30
val a = [|for i <- 0:n {i+1}|]
for i <- 1:n {a[i] += a[i-1]}
println(f"triangular numbers: {a}")

@nothrow fun is_prime(n: int)
{
    if n <= 1 {false} else if n % 2 == 0 {n == 2}
    else {
        all(for p<-3:floor(sqrt(double(n)))+1:2 {n % p != 0})
    }
}

println(f"primes <100: {[for i <- 0:100 {if !is_prime(i) {continue}; i}]}")

val sorted = [: 10, 355, 113, -1, 2, 26, 1, 1949, 0,
                299792458, -460, 451, -11034, 8848 :].sort((<))
print("sorted: ")
println(sorted)

fun plot(a: float, b: float, f: float->float, w: int, h: int) {
    val step = (b - a)/w
    val tab = [|for x <- 0:w {f(a + step*x)}|]
    val v0 = tab[0]
    val fold (minv, maxv)=(v0, v0) for y <- tab { (min(minv, y), max(maxv, y)) }
    val scale = (h-1)/(maxv - minv)

    val screen: char [,] = array((h, w), ' ')
    for x <- 0:w, y <- tab {
        val iy = round((y-minv)*scale)
        screen[h-1-clip(iy, 0, h-1), x] = '*'
    }
    for y <- 0:h { println(screen[y,:]) }
}

val a = float(-0.5*M_PI), b = -a*5
plot(a, b, sin, 80, 10)
