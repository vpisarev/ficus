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

println("factorial(5)=\(testmod.fact(5))")

for (i <- 1:31) { print("fib(\(i))="); print(fib(i)); println("fib2(\(i))="); println(fib2(i)); }

exception Break: int

fun find_idx(a: 't [], elem: 't)
{
    val n = size(a)
    try
    {
        for (i <- 0:n) if (a[i] == elem) throw Break(i)
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

println(find_idx([1, 2, 5], 5))
val sorted = List.mergeSort([:: 2, -1, 100, 8, 7], (fun (a: int, b: int) => a > b))
print("sorted: ")
println(sorted)
