// expect: ambiguous call
// resolve-1: amb('t, int) and amb(int, 't) on an (int, int) call are both
// viable and genuinely incomparable (neither is more specific); the call is
// fully determined -> hard ambiguity error, "overlapping but unordered" flavor.
fun amb[T](x: T, y: int) = 1
fun amb[T](x: int, y: T) = 2
val r = amb(1, 2)
println(r)
