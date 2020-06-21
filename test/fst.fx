// the first test script
import testmod, Math, List

val str = "привет! 你好吗?\nBTW,\n\t2*2=\(2*2),\n\tπ (the area of circle with radius 1) = \(Math.Pi),\n\te = \(Math.exp(1.)),\n\tφ (golden ratio) = \((1+5.**0.5)/2) "
val c = '😄'
println(str + c)

val _2 = 2
val a = (_2 + 2) | 8
val a = a + 1
fun fib(n: int) = if n <= 1 {1} else {fib(n-1) + fib(n-2)}
fun fib2(n: int) {
    fun fib2_(a: int, b: int, n: int) = if n <= 1 {a} else {fib2_(a+b, a, n-1)}
    fib2_(1, 1, n)
}
/*fun fib_seq()
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
val fib3 = fib_seq()*/

println("factorial(5)=\(testmod.fact(5))")

var i = 0
val a=[0, 1, 2, -1, 7]
do
{
    val v = a[i]
    if v < 0 {break}
    i += 1
}
while i < 5

for i <- 1:31 {
    fun foo() {
        print(fib(i))
    }
    print("fib(\(i))=")
    foo()
    print(", fib2(\(i))=\(fib2(i))\n")
    //println("fib3(\(i))=\(fib3())")
}

//exception BreakWith: int

type complex_t = {re: float; im: float}
/*val c = ref (complex_t {re=0.f, im=1.f})
val d = c->{re=c->re*2, im=c->im*2}
fun abs(c:complex_t) = Math.sqrt(c.re**2 + c.im**2)
println("abs(d)=\(abs(d))")*/

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

fun find_idx(a: 't [], elem: 't)
{
    val n = size(a)
    var idx = -1
    for i <- 0:n {if a[i] == elem {idx = i; break}}
    idx
}

for i <- 0:10 { for j <-0:10 { if i!=j {break}} }
fun is_prime(n: int) {
    if n == 1 {false} else if n == 2 {true} else if n % 2 == 0 {false}
    else {
        fold r=true for p<-3:Math.floor(Math.sqrt(n :> double))+1:2 {
            if n % p == 0 {break with false};
            r
        }
    }
}
val primes = [: for i <- 0:100 {if !is_prime(i) {continue}; i} :]

val fixed_choice = "five"

match fixed_choice
{
    | "пять" => println("нашли 5")
    | "five" => println("found 5")
    | "五" => println("找到五个")
    | _ => println("some other number")
}

assert(fixed_choice != "пять")

println(if 0.1 <= Math.sin(1.) < 0.7 {
        "sin(1) is between 0.1 and 0.7"
    } else if Math.sin(1.) < 0.1 {
        "sin(1) is smaller than 0.1"
    } else {
        "sin(1) is greater or equal than 0.7"
    })

val fpair = List.find_opt(("a", 0) :: ("b", 1) :: ("rest", 2) :: [], fun ((key, i): (string, int)) {key == "xyz"})

println(match fpair {
    | Some((x, y)) => y
    | _ => -1
    })

val n = 30
val a = [for i <- 0:n {i+1}]
for i <- 1:n {a[i] += a[i-1]}
println("triangular numbers: \(a)")

println(find_idx([1, 2, 5], 5))
val sorted = List.mergeSort([: 2, -1, 100, 8, 7 :], fun (a: int, b: int) {a < b})
print("sorted: ")
println(sorted)
