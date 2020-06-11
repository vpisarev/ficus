/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Operations on LISP-like lists */

fun hd(_: 't list) { | a :: _ => a | _ => throw NullListError }
fun tl(_: 't list) { | _ :: ll => ll | _ => throw NullListError }
fun null(_: 't list) { | [] => true | _ => false }
fun last(_: 't list)
{
    | a :: [] => a
    | a :: rest => last(rest)
    | _ => throw NullListError
}

fun nth(_: 't list, n: int)
{
    | a :: rest => if n == 0 {a} else {nth(rest, n-1)}
    | _ => throw IndexError
}

pure nothrow fun length(l: 't list): int = ccode
    "return fx_list_length(l);"

fun rev(l: 't list): 't list =
    fold r=([]: t list) for a <- l {r = a :: r}

fun array(l: 't list): 't [] = [for x <- l {x}]

fun all(l: 't list, f: 't -> bool): bool =
    fold r=true for a <- l {if !f(a) { r=false; break }}

fun all2((la, lb): ('a list, 'b list), f: ('a, 'b) -> bool): bool =
    fold r=true for a <- l, b <- l {if !f(a, b) { r=false; break }}

fun exists(l: 't list, f: 't -> bool): bool =
    fold r=false for a <- l {if f(a) {r=true; break}}

fun mem(l: 't list, a: 't): bool =
    fold r=false for b <- l {if a == b {r=true; break}}

fun find_opt(l: 't list, f: 't -> bool): 't? =
    fold r=None for a <- l {if f(a) {r=Some(a); break}}

fun concat(ll: 't list list): 't list =
    fold s = ([]: 't list) for l <- rev(ll) {s = l + s}

fun zip(la: 'a list, lb: 'b list): ('a, 'b) list =
    [: for x <- la, y <- lb {(x, y)} :]

fun unzip(lab: ('a, 'b) list): ('a list, 'b list) =
    unzip([: for x <- lab {x} :])

// O(n log n) merge sort
fun mergeSort(l: 't list, lt: ('t,'t)->bool): 't list =
    match l {
    | [] => []
    | l =>
        fun merge(_: 't list, _: 't list)
        {
            | ((a :: at) as l, (b :: bt) as r) =>
                if lt(b, a) {b :: merge(l, bt)} else {a :: merge(at, r)}
            | (l, []) => l
            | (_, r) => r
        }

        fun scan(_: 't list list)
        {
            | a :: b :: rest => merge(a, b) :: scan(rest)
            | l => l
        }

        fun loop(_: 't list list)
        {
            | a :: [] => a
            | l => loop(scan(l))
        }

        loop([: for a <- l {a :: []} :])
    }
