/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Operations on LISP-like lists */

fun hd(_: 't list) { | a :: _ -> a | _ -> throw NullListError }
fun tl(_: 't list) { | _ :: ll -> ll | _ -> throw NullListError }
fun null(_: 't list) { | [] -> true | _ -> false }
fun last(_: 't list)
{
    | a :: [] -> a
    | a :: rest -> last(rest)
    | _ -> throw NullListError
}

fun nth(_: 't list, n: int)
{
    | a :: rest -> if n == 0 {a} else {nth(rest, n-1)}
    | _ -> throw IndexError
}

fun length(l: 't list)
{
    fun length_(l: 't list, n: int) =
        match l {
        | _ :: rest -> length(rest, n+1)
        | _ -> n
        }
    length_(l, 0)
}

fun rev(l: 't list): 't list = rev(l, [])

fun rev(l: 't list, rl: 't list): 't list =
    match l {
    | a :: rest -> rev(rest, a :: rl)
    | _ -> rl
    }

fun array(l: 't list): 't [] = [for x in l {x}]

fun all(l: 't list, f: 't -> bool): bool =
    match l {
    | a :: rest -> if f(a) {all(rest, f)} else {false}
    | _ -> true
    }

fun all2((la, lb): ('a list, 'b list), f: ('a, 'b) -> bool): bool =
    match (la, lb) {
    | (a :: rest_a, b :: rest_b) -> if f(a, b) {all2((rest_a, rest_b), f)} else {false}
    | ([], []) -> true
    | _ -> throw ListSizeMismatchError
    }

fun exists(l: 't list, f: 't -> bool): bool =
    match l {
    | a :: rest -> if f(a) {true} else {exists(rest, f)}
    | _ -> false
    }

fun mem(l: 't list, a: 't): bool =
    match (l) {
    | b :: rest -> if a == b {true} else {mem(rest, a)}
    | _ -> false
    }

fun find_opt(l: 't list, f: 't -> bool): 't option =
    match (l) {
    | a :: rest -> if f(a) {Some(a)} else {find_opt(rest, f)}
    | _ -> None
    }

fun concat(ll: 't list list): 't list =
    fold s = [] for l in rev(ll) {
        s = l + s
    }

fun zip(la: 'a list, lb: 'b list): ('a, 'b) list =
    [:: for x in la, y in lb {(x, y)}]

fun unzip(lab: ('a, 'b) list): ('a list, 'b list) =
    unzip([:: for x in lab {x}])

// O(n log n) merge sort
fun mergeSort(l: 't list, gt: ('t,'t)->bool): 't list =
    match l {
    | [] -> []
    | l ->
        fun merge(_: 't list, _: 't list)
        {
            | ((a :: at) as l, (b :: bt) as r) -> if gt(a, b) {b :: merge(l, bt)} else {a :: merge(at, r)}
            | (l, []) -> l
            | ([], r) -> r
        }

        fun scan(_: 't list list)
        {
            | a :: b :: rest -> merge(a, b) :: scan(rest)
            | l -> l
        }

        fun loop(_: 't list list)
        {
            | a :: [] -> a
            | l -> loop(scan(l))
        }

        loop([:: for a in l {a :: []}])
    }
