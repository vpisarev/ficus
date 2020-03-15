import Args

type 'a tree = Empty | Node: ('a tree, 'a, 'a tree)

fun make(i: 'a, d: int): 'a tree =
    if (d == 0)
        Node(Empty, i, Empty)
    else
        Node(make(i*2-1, d-1), i, make(i*2, d-1))

fun check(_: 'a tree)
{
    | Empty => 0
    | Node(l, i, r) => i + check(l) - check(r)
}

val min_depth = 4
val max_depth =
    match (Args.arguments())
    {
    | n_str :: [] => getOpt(atoi(n_str), 20)
    | _ => 20
    }
val stretch_depth = max_depth + 1

fun test_1()
{
    val c = check(make(0, stretch_depth))
    println("stretch tree of depth \(stretch_depth)\t check: \(c)")
}
test_1()

val long_lived_tree = make(0, max_depth)

parallel
for (depth in min_depth:(max_depth+1):2)
{
    val iterations = 1 << (max_depth - depth + min_depth)
    val fold (c = 0; i in 1:(iterations+1))
            c += check(make(i, depth)) + check(make(-i, depth))
    println("\(iterations * 2)\t trees of depth \(depth)\t check: \(c)")
}

println("long lived tree of depth \(max_depth)\t check: \(check(long_lived_tree))")
0
