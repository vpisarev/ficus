/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Json pretty printing example

type json_scalar_t =
    | JsonScInt: int
    | JsonScReal: double
    | JsonScBool: bool
    | JsonScString: string

type json_t =
    | JsonScalar: json_scalar_t
    | JsonMap: (string, json_t) list
    | JsonSeq: json_t list
    | JsonCommented: (string, json_t)

fun string(jsc: json_scalar_t)
{
    | JsonScInt i => string(i)
    | JsonScReal f => string(f)
    | JsonScBool b => string(b)
    | JsonScString s => repr(s)
}

fun print_js(js: json_t, ofs: int, indent: string)
{
    val W0 = 80, W1 = 100
    fun all_scalars(l: json_t list) =
        all(for x <- l {
            | JsonScalar _ => true
            | _ => false
            })
    fun process_comments(j: json_t, indent: string) =
        match j {
        | JsonCommented(comm, nested_j) =>
            print(f"// {comm}\n{indent}")
            process_comments(nested_j, indent)
        | _ => j
        }
    val l_oldind = length(indent)
    val newind = indent + (if l_oldind > 40 {" "} else if l_oldind > 20 {"  "} else {"   "})
    val l_newind = length(newind)
    val js = process_comments(js, indent)
    match js {
    | JsonScalar(sc) =>
        val str = string(sc)
        print(str)
        ofs + length(str)
    | JsonCommented(comm, nested_js) =>
        throw Fail("comments are not expected here")
    | JsonMap(m) =>
        println("{")
        val n = length(m)
        for (k, v)@i <- m {
            print(newind)
            val v = process_comments(v, newind)
            val prefix = f"{repr(k)} : "
            print(prefix)
            ignore(print_js(v, l_newind + length(prefix), newind))
            if i < n-1 {print(",\n")}
        }
        print(f"\n{indent}}")
        l_oldind+1
    | JsonSeq(l) =>
        if all_scalars(l) {
            val n = length(l)
            print("[ ")
            val fold ofs = ofs + 2 for x@i <- l {
                match x {
                | JsonScalar(sc) =>
                    val str = string(sc)
                    val lstr = length(str)
                    val ofs = if ofs > l_newind && ofs + lstr > W1 {
                        print(f"\n{newind}"); l_newind
                    } else { ofs }
                    print(str)
                    val ofs = ofs + lstr
                    if i < n-1 {
                        print(",")
                        if ofs+1 > W0 {
                            print(f"\n{newind}"); l_newind
                        } else { print(" "); ofs + 2 }
                    } else { print(" "); ofs }
                | _ => throw Fail("scalar is expected here")
                }
            }
            print("]"); ofs + 1
        } else {
            println("[")
            val n = length(l)
            for v <- l, i <- 0: {
                print(newind)
                val v = process_comments(v, newind)
                ignore(print_js(v, l_newind, newind))
                if i < n-1 {print(",\n")}
            }
            print(f"\n{indent}]")
            l_oldind+1
        }
    }
}

val rng = RNG(123u64)
val s_list_list = [for i <- 0:10 {
    val n = rng.uniform(0, 99)
    JsonCommented(f"#{i}", JsonSeq([for j <- 0:n {
        JsonScalar(JsonScInt(rng.uniform(0, 99999)))}]))
    }]

val sample_js = JsonCommented(
    "a small Json pretty-printing example",
    JsonMap([
    ("ain't it cool?", JsonScalar(JsonScBool(true))),
    ("pi", JsonCommented("the famous constant", JsonScalar(JsonScReal(3.1415926)))),
    ("a little array of arrays", JsonCommented("demonstrates compact representation of scalar arrays",
        JsonSeq(s_list_list))),
    ("greeting", JsonCommented("'hello' in Chinese", JsonScalar(JsonScString("你好")))),
     ]))

ignore(print_js(sample_js, 0, ""))
