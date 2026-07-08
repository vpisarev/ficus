// expect: ambiguous call
// resolve-1: two byte-identical generic signatures are both viable and
// EqGeneric -> hard ambiguity error, "equally applicable" flavor (the hint is
// to qualify the call, since no more-specific overload can break an Eq tie).
fun g(x: 't) = 1
fun g(x: 'u) = 2
val r = g(5)
println(r)
