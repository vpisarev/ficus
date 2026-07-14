// WP-E / D3 -- resolution-sensitive IR snapshot. Mixed generic/non-generic
// overloads called at several types; the selected instance names are visible in
// the AST/K-form dumps, so any change in overload resolution shows up as a diff.
// NOTE: today (FB-016) the generic pick('t), declared LAST, wins ALL calls --
// even pick(5)/pick(3.0f) route to @instance of the generic (returning 0), and
// the concrete pick(int)/pick(float) are dead. When the resolver surgery lands,
// pick(5)->concrete (6) and pick(3.0f)->concrete (2); this golden then changes.
fun pick(x: int): int = x + 1
fun pick(x: float): int = 2
fun pick[T](x: T): int = 0
val a = pick(5)        // concrete int
val b = pick(3.0f)     // concrete float
val c = pick("hi")     // generic 't
val d = pick([:: 1,2]) // generic 't (list)
println(f"{a} {b} {c} {d}")
