// the first test script
import testmod

val str = "привет! 你好吗?\nBTW,\n\t2*2=\(2*2),\n\tπ (the area of circle with radius 1) = \(Math.pi),\n\te = \(Math.exp(1.)),\n\tφ (golden ratio) = \((1+5.**0.5)/2)\n"
val c = '😄'
println(str + c)

val _2 = 2
val a = (_2 + 2) | 8
fun fib(n: int): int = if (n <= 1) 1 else fib(n-1) + fib(n-2)

for (i in 1:31) { print("fib(\(i))=\(fib(i))"); println() }
