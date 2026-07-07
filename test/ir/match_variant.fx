// IR snapshot: pattern match lowering over a variant
type shape_t = Circle: double | Square: double
fun area(s: shape_t) = match s {
    | Circle(r) => 3.14159 * r * r
    | Square(a) => a * a
}
println(area(Circle(2.0)))
