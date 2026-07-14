/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// very simple list-based deque

exception NullQueueError

class t[T] {head: list[T]; tail: list[T]}

fun empty[T](_: Deque.t[T]): bool
{
    | {head=[], tail=[]} => true
    | _ => false
}

fun empty[T](): Deque.t[T] = t {head=[], tail=[]}
fun length[T](d: Deque.t[T]): int = d.head.length() + d.tail.length()
fun rev[T](d: Deque.t[T]): Deque.t[T] = t {head=d.tail, tail=d.head}

// could improve performance in many cases but the worst one
operator == [T](d1: Deque.t[T], d2: Deque.t[T]): bool = list(d1) == list(d2)

fun first[T](d: Deque.t[T]): T
{
    | {head=x :: _} => x
    | {tail=_ :: _} => d.tail.last()
    | _ => throw NullQueueError
}

fun last[T](d: Deque.t[T]): T
{
    | {tail=x :: _} => x
    | {head=_ :: _} => d.head.last()
    | _ => throw NullQueueError
}

fun pop_front[T](d: Deque.t[T]): (T, Deque.t[T])
{
    | {head=x :: rest, tail} => (x, t {head=rest, tail=tail})
    | {tail=_ :: _} => pop_front(t {head=d.tail.rev(), tail=[]})
    | _ => throw NullQueueError
}

fun push_front[T](d: Deque.t[T], x: T): Deque.t[T] = t {head=x::d.head, tail=d.tail}

fun pop_back[T](d: Deque.t[T]): (T, Deque.t[T])
{
    | {head, tail=x :: rest} => (x, t {head=head, tail=rest})
    | {head=_ :: _} => pop_back(t {head=[], tail=d.head.rev()})
    | _ => throw NullQueueError
}

fun push_back[T](d: Deque.t[T], x: T): Deque.t[T] = t {head=d.head, tail=x::d.tail}

fun from_list[T](l: list[T]): Deque.t[T] = t {head=l, tail=[]}
fun list[T](d: Deque.t[T]): list[T] = d.head + d.tail.rev()
fun array[T](d: Deque.t[T]): list[T]
{
    val h = [for x <- d.head {x}]
    val t = [for x <- d.tail.rev() {x}]
    [ \h, \t ]
}

fun map[T, Tr](d: Deque.t[T], f: T -> Tr): Deque.t[Tr]
{
    val new_head = [:: for x <- d.head {f(x)}]
    val new_tail = [:: for x <- d.tail {f(x)}]
    t {head=new_head, tail=new_tail}
}

fun app[T](d: Deque.t[T], f: T -> void, ~in_order:bool=true): void
{
    for x <- d.head {f(x)}
    for x <- (if in_order {d.tail.rev()} else {d.tail}) {f(x)}
}

fun foldl[T, Tr](d: Deque.t[T], f: (T, Tr) -> Tr, res0: Tr): Tr
{
    val fold res=res0 for x <- d.head {res = f(x, res)}
    fold res=res for x <- d.tail.rev() {res = f(x, res)}
}

fun foldr[T, Tr](d: Deque.t[T], f: (T, Tr) -> Tr, res0: Tr): Tr
{
    val fold res=res0 for x <- d.tail {res = f(x, res)}
    fold res=res for x <- d.head.rev() {res = f(x, res)}
}

fun all[T](d: Deque.t[T], f: T -> bool): bool = d.head.all(f) && d.tail.all(f)
fun exists[T](d: Deque.t[T], f: T -> bool): bool = d.head.exists(f) || d.tail.exists(f)
fun find[T](d: Deque.t[T], f: T -> bool): T =
    try {
        d.head.find(f)
    } catch {
        | NotFoundError => d.tail.rev().find(f)
    }
fun rfind[T](d: Deque.t[T], f: T -> bool): T =
    try {
        d.tail.find(f)
    } catch {
        | NotFoundError => d.head.rev().find(f)
    }
fun find_opt[T](d: Deque.t[T], f: T -> bool): T =
    try {
        Some(d.head.find(f))
    } catch {
        | NotFoundError => d.tail.rev().find_opt(f)
    }
fun rfind_opt[T](d: Deque.t[T], f: T -> bool): T =
    try {
        Some(d.tail.find(f))
    } catch {
        | NotFoundError => d.head.rev().find_opt(f)
    }

fun string[T](d: Deque.t[T]): string
{
    val len = length(d)
    val elems = array(len, "")
    for x@i <- d.head {elems[i] = repr(x)}
    for x@i <- d.tail {elems[len-i-1] = repr(x)}
    join_embrace("[", "]", ", ", elems)
}

fun print[T](d: Deque.t[T]): void
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
