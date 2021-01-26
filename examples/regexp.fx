import Re2
//[TODO] Try use module types instead of Re2 calls for pre-compiled regexp.
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
println(Re2.partial_match_n("The moon is 384467 kilometers distant.", "(\\d+)"))
println(Re2.partial_match_n_str("The moon is 384467 kilometers distant.", "(\\d+)"))

println()
println("Consume:")
println(Re2.consume_n("Eagle, don't bait! I and Cheburashka, who haven't friends, both want to serve in the artillery.", 0, "(\\w+)"))
println(Re2.consume_n_str("Eagle, don't bait! I and Cheburashka, who haven't friends, both want to serve in the artillery.", 0, "(\\w+)"))

println()
println("Find And Consume :")
println(Re2.find_and_consume_n_str("The moon is 384467 kilometers distant.", 0, "(\\d+)"))
val fullstring_re = Re2.compile("^.*$",Re2.options_t {posix_syntax = true, one_line = false})
println(Re2.findall_str("The hooves clattered.\nAs if singing:\n- Crib.\nGrab.\nGrub.\nGruff.\n", fullstring_re))

println()
println("Replace:")
println(Re2.replace("If you'll find a Talker bird, please report me, Gromozeka@Chumaroza.org.", "(\\w+)@(\\w+)\\.(\\w+)", "\\1 from \\2"))
println(Re2.global_replace("2-12-85-06", "\\d+","(\\0)"))

println()
println("Exception:")
try 
{
    println(Re2.full_match("hello", "h.*o[")) 
}
catch 
{
    | Re2.BadRegexp(s) => println(f"exception BadRegexp('{s}') was caught")
}
