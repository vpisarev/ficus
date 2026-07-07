// IR snapshot: throw / try-catch lowering
exception MyErr: int
fun safe(x: int) = try {
    if x < 0 { throw MyErr(x) } else { x }
} catch {
    | MyErr(e) => -e
}
println(safe(-7))
