/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// very simple list-based deque

exception NullQueueError

object type 't t = {head: 't list; tail: 't list}

fun empty(_: 't Deque.t): bool
{
    | {head=[], tail=[]} => true
    | _ => false
}

fun empty(): 't Deque.t = t {head=[], tail=[]}
fun length(d: 't Deque.t) = d.head.length() + d.tail.length()
fun rev(d: 't Deque.t) = t {head=d.tail, tail=d.head}

// could improve performance in many cases but the worst one
operator == (d1: 't Deque.t, d2: 't Deque.t) = list(d1) == list(d2)

fun first(d: 't Deque.t): 't
{
    | {head=x :: _} => x
    | {tail=_ :: _} => d.tail.last()
    | _ => throw NullQueueError
}

fun last(d: 't Deque.t): 't
{
    | {tail=x :: _} => x
    | {head=_ :: _} => d.head.last()
    | _ => throw NullQueueError
}

fun pop_front(d: 't Deque.t): ('t, 't Deque.t)
{
    | {head=x :: rest, tail} => (x, t {head=rest, tail=tail})
    | {tail=_ :: _} => pop_front(t {head=d.tail.rev(), tail=[]})
    | _ => throw NullQueueError
}

fun push_front(d: 't Deque.t, x: 't): 't Deque.t = t {head=x::d.head, tail=d.tail}

fun pop_back(d: 't Deque.t): ('t, 't Deque.t)
{
    | {head, tail=x :: rest} => (x, t {head=head, tail=rest})
    | {head=_ :: _} => pop_back(t {head=[], tail=d.head.rev()})
    | _ => throw NullQueueError
}

fun push_back(d: 't Deque.t, x: 't): 't Deque.t = t {head=d.head, tail=x::d.tail}

fun from_list(l: 't list): 't Deque.t = t {head=l, tail=[]}
fun list(d: 't Deque.t): 't list = d.head + d.tail.rev()
fun array(d: 't Deque.t): 't list
{
    val h = [| for x <- d.head {x} |]
    val t = [| for x <- d.tail.rev() {x} |]
    [| \h, \t |]
}

fun map(d: 't Deque.t, f: 't -> 'rt): 'rt t
{
    val new_head = [: for x <- d.head {f(x)} :]
    val new_tail = [: for x <- d.tail {f(x)} :]
    t {head=new_head, tail=new_tail}
}

fun app(d: 't Deque.t, f: 't -> void, ~in_order:bool=true): void
{
    for x <- d.head {f(x)}
    for x <- (if in_order {d.tail.rev()} else {d.tail}) {f(x)}
}

fun foldl(d: 't Deque.t, f: ('t, 'acc) -> 'acc, res0: 'acc): 'acc
{
    val fold res=res0 for x <- d.head {f(x, res)}
    fold res=res for x <- d.tail.rev() {f(x, res)}
}

fun foldr(d: 't Deque.t, f: ('t, 'acc) -> 'acc, res0: 'acc): 'acc
{
    val fold res=res0 for x <- d.tail {f(x, res)}
    fold res=res for x <- d.head.rev() {f(x, res)}
}

fun all(d: 't Deque.t, f: 't -> bool) = d.head.all(f) && d.tail.all(f)
fun exists(d: 't Deque.t, f: 't -> bool) = d.head.exists(f) || d.tail.exists(f)
fun find(d: 't Deque.t, f: 't -> bool): 't =
    try {
        d.head.find(f)
    } catch {
        | NotFoundError => d.tail.rev().find(f)
    }
fun rfind(d: 't Deque.t, f: 't -> bool): 't =
    try {
        d.tail.find(f)
    } catch {
        | NotFoundError => d.head.rev().find(f)
    }
fun find_opt(d: 't Deque.t, f: 't -> bool): 't =
    try {
        Some(d.head.find(f))
    } catch {
        | NotFoundError => d.tail.rev().find_opt(f)
    }
fun rfind_opt(d: 't Deque.t, f: 't -> bool): 't =
    try {
        Some(d.tail.find(f))
    } catch {
        | NotFoundError => d.head.rev().find_opt(f)
    }

fun string(d: 't Deque.t)
{
    val len = length(d)
    val elems = array(len, "")
    for x@i <- d.head {elems[i] = repr(x)}
    for x@i <- d.tail {elems[len-i-1] = repr(x)}
    join_embrace("[", "]", ", ", elems)
}

fun print(d: 't Deque.t)
{
    print("[")
    for x@i <- d.head {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    val nonempty = int(d.head != [])
    for x@i <- d.tail.rev() {
        if i+nonempty > 0 {print(", ")}
        print_repr(x)
    }
    print("]")
}
