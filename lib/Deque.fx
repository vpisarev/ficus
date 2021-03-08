/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// very simple list-based deque

exception NullQueueError

// the name is very short, but it's convenient to reference this type as Deque.t
@object type 't t = {head: 't list; tail: 't list}

fun empty(_: 't t): bool
{
    | {head=[], tail=[]} => true
    | _ => false
}

fun length(d: 't t) = d.head.length() + d.tail.length()
fun rev(d: 't t) = t {head=d.tail, tail=d.head}

// could improve performance in many cases but the worst one
operator == (d1: 't t, d2: 't t) = list(d1) == list(d2)

fun first(d: 't t): 't
{
    | {head=x :: _} => x
    | {tail=_ :: _} => d.tail.last()
    | _ => throw NullQueueError
}

fun last(d: 't t): 't
{
    | {tail=x :: _} => x
    | {head=_ :: _} => d.head.last()
    | _ => throw NullQueueError
}

fun pop_front(d: 't t): ('t, 't t)
{
    | {head=x :: rest, tail} => (x, t {head=rest, tail=tail})
    | {tail=_ :: _} => pop_front(t {head=d.tail.rev(), tail=[]})
    | _ => throw NullQueueError
}

fun push_front(d: 't t, x: 't): 't t = t {head=x::d.head, tail=d.tail}

fun pop_back(d: 't t): ('t, 't t)
{
    | {head, tail=x :: rest} => (x, t {head=head, tail=rest})
    | {head=_ :: _} => pop_back(t {head=[], tail=d.head.rev()})
    | _ => throw NullQueueError
}

fun push_back(d: 't t, x: 't): 't t = t {head=d.head, tail=x::d.tail}

fun from_list(l: 't list): 't t = t {head=l, tail=[]}
fun list(d: 't t): 't list = d.head + d.tail.rev()
fun array(d: 't t): 't list
{
    val h = [for x <- d.head {x}]
    val t = [for x <- d.tail.rev() {x}]
    [\h, \t]
}

fun map(d: 't t, f: 't -> 'rt): 'rt t
{
    val new_head = [: for x <- d.head {f(x)} :]
    val new_tail = [: for x <- d.tail {f(x)} :]
    t {head=new_head, tail=new_tail}
}

fun app(d: 't t, f: 't -> void, ~in_order:bool=true): void
{
    for x <- d.head {f(x)}
    for x <- (if in_order {d.tail.rev()} else {d.tail}) {f(x)}
}

fun foldl(d: 't t, f: ('t, 'acc) -> 'acc, res0: 'acc): 'acc
{
    val fold res=res0 for x <- d.head {f(x, res)}
    fold res=res for x <- d.tail.rev() {f(x, res)}
}

fun foldr(d: 't t, f: ('t, 'acc) -> 'acc, res0: 'acc): 'acc
{
    val fold res=res0 for x <- d.tail {f(x, res)}
    fold res=res for x <- d.head.rev() {f(x, res)}
}

fun string(d: 't t)
{
    val len = length(d)
    val elems = array(len, "")
    for x@i <- d.head {elems[i] = repr(x)}
    for x@i <- d.tail {elems[len-i-1] = repr(x)}
    join_embrace("[", "]", ", ", elems)
}

fun print(d: 't t)
{
    print("[")
    for x@i <- d.head {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    val nonempty = (!d.head.empty() :> int)
    for x@i <- d.tail.rev() {
        if i+nonempty > 0 {print(", ")}
        print_repr(x)
    }
    print("]")
}
