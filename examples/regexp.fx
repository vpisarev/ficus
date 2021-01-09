import Re2

//Instant regexp matching
println(Re2.full_match("hello", "h.*o"))
println(Re2.full_match("hello", "e"))
println(Re2.partial_match("hello", "h.*o"))
println(Re2.partial_match("hello", "e"))

//Matching with predefined regexp

val reg_ho = Re2.compile("h.*o")
val reg_e  = Re2.compile("e")

println(Re2.full_match("hello", reg_ho))
println(Re2.full_match("hello", reg_e))
println(Re2.partial_match("hello", reg_ho))
println(Re2.partial_match("hello", reg_e))

println(Re2.full_match("hello", "h.*o["))