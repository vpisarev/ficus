// the first test script
import testmod, Math

val str = "привет! 你好吗?\nBTW,\n\t2*2=\(2*2),\n\tπ (the area of circle with radius 1) = \(Math.Pi),\n\te = \(Math.exp(1.)),\n\tφ (golden ratio) = \((1+5.**0.5)/2)\n"
val c = '😄'
println(str + c)

val _2 = 2
val a = (_2 + 2) | 8
val a = a + 1
fun fib(n: int) = if (n <= 1) 1 else fib(n-1) + fib(n-2)

println("factorial(5)=\(testmod.fact(5))")

for (i <- 1:31) { print("fib(\(i))=\(fib(i))"); println() }
