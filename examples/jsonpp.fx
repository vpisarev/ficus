/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Json pretty printing example
import Json

val rng = RNG(123u64)
val s_list_list = [for i <- 0:10 {
    val n = rng.uniform(0, 99)
    Json.Commented(f"#{i}", Json.Seq([for j <- 0:n {
        Json.Int(rng.uniform(0, 99999) :> int64)}]))
    }]

val sample_js = Json.Commented(
    "a small Json pretty-printing example",
    Json.Map([
    ("ain't it cool?", Json.Bool(true)),
    ("pi", Json.Commented("the famous constant", Json.Real(3.1415926))),
    ("a little array of arrays", Json.Commented("demonstrates compact representation of scalar arrays",
        Json.Seq(s_list_list))),
    ("greeting", Json.Commented("'hello' in Chinese", Json.Str("你好!"))),
    ]))

println(sample_js)
