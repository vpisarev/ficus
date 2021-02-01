import Re2

fun example(funname: string, arg1: 'a, arg2: 'b, f: ('a, 'b) -> 't): void
{
	println(f"Re2.{funname}(\"{arg1}\", r\"{arg2}\") = {f(arg1, arg2)}")
}

fun example(funname: string, arg1: 'a, arg2: 'b, arg3: 'c, f: ('a, 'b, 'c) -> 't): void
{
	println(f"Re2.{funname}(\"{arg1}\", \"{arg2}\", r\"{arg3}\") = {f(arg1,arg2,arg3)}")
}

fun example(funname: string, arg1: 'a, arg2: 'b, arg3: 'c, arg4: 'd, arg5: 'e, f: ('a, 'b, 'c, 'd, 'e) -> 't): void
{
	println(f"Re2.{funname}(\"{arg1}\", \"{arg2}\", \"{arg3}\", \"{arg4}\", \"{arg5}\") = {f(arg1,arg2,arg3,arg4,arg5)}")
}

println("Instant regexp matching")
example("full_match", "hello", r"h.*o", Re2.full_match) // Bugreport1: Why in this case we CAN OMIT signature.
example("full_match", "hello", r"e", Re2.full_match)
example("partial_match", "hello", r"h.*o", Re2.partial_match)
example("partial_match", "hello", r"e", Re2.partial_match)

println()
println("Matching with predefined regexps")

val reg_ho = Re2.compile(r"h.*o")
val reg_e  = Re2.compile(r"e")

example("full_match", "hello", reg_ho, (Re2.full_match: (string, Re2.regex_t)->bool))   // Bugreport1: why in this case we MUST define signature directy 
example("full_match", "hello", reg_e, (Re2.full_match: (string, Re2.regex_t)->bool))
example("partial_match", "hello", reg_ho, (Re2.partial_match: (string, Re2.regex_t)->bool))
example("partial_match", "hello", reg_e, (Re2.partial_match: (string, Re2.regex_t)->bool))

val bards = "Mockles! Fent on silpen tree,\n Blockards three a-feening,\n Mockles, what silps came to thee\n In thy pantry dreaming?"

println()
println("Submatch extraction:")
example("partial_match_n", bards, r"[.,!? ]([^g-p^\s]+)[.,!? ]", Re2.partial_match_n)
val se_reg = Re2.compile(r"(\b[E-Te-t]+\b)", Re2.options_t {posix_syntax = true, word_boundary = true})
example("partial_match_n_str", bards, se_reg, (Re2.partial_match_n_str: (string, Re2.regex_t)->(bool,string [])))

println()
println("Consume:")
example("consume_n", "Eagle, don't bait! I and Cheburashka, who haven't friends, both want to serve in the artillery.", 0, r"([[:alpha:]]+)", Re2.consume_n)
example("consume_n_str", "Eagle, don't bait! I and Cheburashka, who haven't friends, both want to serve in the artillery.", 0, r"([[:alpha:]]+)", Re2.consume_n_str)

println()
println("Find And Consume :")
example("find_and_consume_n_str", "The moon is 384467 kilometers distant.", 0, r"(\d+)", Re2.find_and_consume_n_str)

println()
println("Find All:")
val fullstring_re = Re2.compile(r"^.*$",Re2.options_t {posix_syntax = true, one_line = false})
example("findall_str", "The hooves clattered.\nAs if singing:\n— Crib.\nGrab.\nGrub.\nGruff.\n", fullstring_re, (Re2.findall_str: (string, Re2.regex_t) -> (bool, string[,])))

println()
println("General match:")
example("general_match_str","Hello I'm Cornelius",r"(I.m)",6,9,Re2.anchor_t {anchor_both = true}, Re2.general_match_str)

println()
println("Replace:")
example("replace","If you'll find a Talker bird, please report me, Gromozeka@Chumaroza.org.", r"(\w+)@(\w+)\.(\w+)", r"\1 from \2", Re2.replace)
example("global_replace","2-12-85-06", r"\d+", r"(\0)", Re2.global_replace)

println()
val ngroup = Re2.compile(r"Variable (?P<varname>\w*) is equal to (?P<varval>0x[[:xdigit:]]*).") 
println(f"Named groups from regexp \"{ngroup}\" for string \"Variable len_in_pieces is equal to 0x509F934.\":")
val (_,named_groups) =  Re2.full_match_n_str("Variable len_in_pieces is equal to 0x509F934.", ngroup)
val group_names = Re2.named_capturing_groups(ngroup)
for (gname, gindex) <- group_names
{
	println(f"{gname} = {named_groups[gindex-1]}") //index in Re2 is 1-based, and in ficus arrays it's 0-based, so gindex-1
}

println()
println("Exception:")
try 
{
    println(Re2.full_match("hello", r"h.*o[")) 
}
catch 
{
    | Re2.BadRegexp(s) => println(f"exception BadRegexp('{s}') was caught")
}

//val bugreport2 = Re2.compile(r"h.*o")