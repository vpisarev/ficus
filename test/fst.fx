/*fun fib(n: int): int = if n <= 1 then 1 else fib(n-1) + fib(n-2) fi

val _2 = 2
val a = _2 + 2

//println("Hello, world!\n2x2=\(a)")

//for i in 1:31 do println("fib(\(i)) = \(fib(i))") end
*/
import testmod

val str = "привет! 你好吗? BTW, 2*2=\(2*2) and area of circle with radius 1 = \(Math.pi**2)"
val c = '😄'

val _2 = 2
val a = (_2 + 2 | 8)
fun fib(n: int): int = if (n <= 1) 1 else fib(n-1) + fib(n-2)

for (i in 1:31) { print("fib(\(i))=\(fib(i))"); println() }
