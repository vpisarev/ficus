/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Operations on LISP-like lists */

fun length(l: 't list) = Builtins.length(l)
fun hd(_: 't list) { | a :: _ => a | _ => throw NullListError }
fun tl(_: 't list) { | _ :: ll => ll | _ => throw NullListError }
fun null(_: 't list) { | [] => true | _ => false }
fun last(_: 't list)
{
    | a :: [] => a
    | a :: rest => last(rest)
    | _ => throw NullListError
}

fun nth(l: 't list, n: int) =
    match l
    {
        | a :: rest => if n == 0 {a} else {nth(rest, n-1)}
        | _ => throw OutOfRangeError
    }

fun rev(l: 't list): 't list =
    fold r=[] for a <- l {a :: r}

fun array(l: 't list): 't [] = [for x <- l {x}]

fun assoc_opt(l: ('a, 'b) list, x: 'a) =
    fold r=None for (a, b) <- l {if a == x {break with Some(b)}; r}

fun all(l: 't list, f: 't -> bool): bool =
    fold r=true for a <- l {if !f(a) { break with false}; r}

fun all2((la, lb): ('a list, 'b list), f: ('a, 'b) -> bool): bool =
    fold r=true for a <- l, b <- l {if !f(a, b) {break with false}; r}

fun exists(l: 't list, f: 't -> bool): bool =
    fold r=false for a <- l {if f(a) {break with true}; r}

fun mem(l: 't list, a: 't): bool =
    fold r=false for b <- l {if a == b {break with true}; r}

fun find_opt(l: 't list, f: 't -> bool): 't? =
    fold r=None for a <- l {if f(a) {break with Some(a)}; r}

fun concat(ll: 't list list): 't list =
    fold s = ([]: 't list) for l <- rev(ll) {l + s}

fun zip(la: 'a list, lb: 'b list): ('a, 'b) list =
    [: for x <- la, y <- lb {(x, y)} :]

fun unzip(lab: ('a, 'b) list): ('a list, 'b list) =
    unzip([: for x <- lab {x} :])

fun sort(l: 't list, lt: ('t, 't)->bool) =
    match l
    {
        | [] => l
        | a :: [] => l
        | a :: b :: [] => if lt(b, a) {b::a::[]} else {l}
        | _ =>
            val arr = [for x <- l {x}]
            sort(arr, lt)
            [: for x <- arr {x} :]
    }
