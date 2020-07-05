import Args

type 'a tree = Empty | Node: {left:'a tree, v:'a, right:'a tree}

fun make(i: 'a, d: int): 'a tree =
    if d == 0 {
        Node {left=Empty, v=i, right=Empty}
    }
    else {
        Node {left=make(i*2-1, d-1), v=i, right=make(i*2, d-1)}
    }

fun check(_: 'a tree)
{
    | Empty => (0 :> 'a)
    | Node{left, v, right} => v + check(left) - check(right)
}

val min_depth = 4
val max_depth = match Args.arguments() {
    | n_str :: [] => getOpt(atoi(n_str), 20)
    | _ => 20
    }
val stretch_depth = max_depth + 1

val c = check(make(0, stretch_depth))
println("stretch tree of depth \(stretch_depth)\t check: \(c)")

val long_lived_tree = make(0, max_depth)
val long_lived_tree_dbl = make(0.0, max_depth)

for depth <- min_depth:(max_depth+1):2 {
    val iterations = 1 << (max_depth - depth + min_depth)
    val fold c = 0 for i <- 1:(iterations+1) {
        c + check(make(i, depth)) + check(make(-i, depth))
    }
    println("\(iterations * 2)\t trees of depth \(depth)\t check: \(c)")
}

println("long lived tree of depth \(max_depth)\t check: \(check(long_lived_tree))")
println("long lived tree of depth \(max_depth)\t check: \(check(long_lived_tree_dbl))")