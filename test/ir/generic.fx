// IR snapshot: generic function instantiated at two types
fun ident[T](x: T) = x
println(ident(42))
println(ident("hello"))
