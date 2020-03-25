// the first test script
import testmod, Math, List

val str = "привет! 你好吗?\nBTW,\n\t2*2=\(2*2),\n\tπ (the area of circle with radius 1) = \(Math.Pi),\n\te = \(Math.exp(1.)),\n\tφ (golden ratio) = \((1+5.**0.5)/2) "
val c = '😄'
println(str + c)

val _2 = 2
val a = (_2 + 2) | 8
val a = a + 1
fun fib(n: int) = if (n <= 1) 1 else fib(n-1) + fib(n-2)
fun fib2(n: int) {
    fun fib2_(a: int, b: int, n: int) = if (n <= 1) a else fib2_(a+b, a, n-1)
    fib2_(1, 1, n)
}
fun fib_seq()
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
val fib3 = fib_seq()

println("factorial(5)=\(testmod.fact(5))")

for (i in 1:31) { print("fib(\(i))="); print(fib(i)); print(", fib2(\(i))=\(fib2(i)), "); println("fib3(\(i))=\(fib3())") }

exception Break: int

fun find_idx(a: 't [], elem: 't)
{
    val n = size(a)
    try
    {
        for (i in 0:n) if (a[i] == elem) throw Break(i)
        -1
    }
    catch
    {
    | Break(i) => i
    }
}

val fixed_choice = "five"

match (fixed_choice)
{
    | "пять" => println("нашли 5")
    | "five" => println("found 5")
    | "五" => println("找到五个")
    | _ => println("some other number")
}

val fpair = List.find_opt(("a", 0) :: ("b", 1) :: ("rest", 2) :: [], (fun ((key, i): (string, int)) => key == "xyz"))
println(match (fpair) {
    | Some((x, y)) => y
    | _ => -1
    })

val n = 30
val a = [for (i in 0:n) 1]
for (i in 1:n) a[i] += a[i-1]

println(find_idx([1, 2, 5], 5))
val sorted = List.mergeSort([:: 2, -1, 100, 8, 7], (fun (a: int, b: int) => a > b))
print("sorted: ")
println(sorted)
