import Re2

println("Instant regexp matching")
println(Re2.full_match("hello", "h.*o"))
println(Re2.full_match("hello", "e"))
println(Re2.partial_match("hello", "h.*o"))
println(Re2.partial_match("hello", "e"))

println()
println("Matching with predefined regexps")

val reg_ho = Re2.compile("h.*o")
val reg_e  = Re2.compile("e")

println(Re2.full_match("hello", reg_ho))
println(Re2.full_match("hello", reg_e))
println(Re2.partial_match("hello", reg_ho))
println(Re2.partial_match("hello", reg_e))

println()
println("Submatch extraction:")

println(Re2.full_match_n("directions from mountain view to san jose", "directions from (?P<S>.*) to (?P<D>.*)"))
println(Re2.full_match_n_str("directions from mountain view to san jose", "directions from (?P<S>.*) to (?P<D>.*)"))

println()
println("Partial submatch extraction:")
println(Re2.partial_match_n("The moon 384467 kilometers distant.", "(\\d+)"))
println(Re2.partial_match_n_str("The moon 384467 kilometers distant.", "(\\d+)"))

println()
println("Exception:")
println(Re2.full_match("hello", "h.*o["))