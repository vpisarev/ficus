/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Operations on LISP-like lists */

fun length(l: 't list) = Builtins.length(l)
fun hd(_: 't list) { | a :: _ => a | _ => throw NullListError }
fun tl(_: 't list) { | _ :: ll => ll | _ => throw NullListError }
fun empty(_: 't list) { | [] => true | _ => false }
fun last(_: 't list)
{
    | [:: a] => a
    | _ :: rest => last(rest)
    | _ => throw NullListError
}

fun nth(l: 't list, n: int) =
match l
{
    | a :: rest => if n == 0 {a} else {nth(rest, n-1)}
    | _ => throw OutOfRangeError
}
fun skip(l: 't list, n: int): 't list =
    if n == 0 {l} else { match l { | a :: rest => skip(rest, n-1) | _ => throw OutOfRangeError } }
fun skip_nothrow(l: 't list, n: int): 't list =
    if n == 0 {l} else { match l { | a :: rest => skip_nothrow(rest, n-1) | _ => [] } }

fun rev(l: 't list): 't list =
    fold r=[] for a <- l {a :: r}

fun foldl(l: 't list, f: ('t, 'r) -> 'r, res0: 'r): 'r =
    fold res=res0 for a <- l {f(a, res)}

fun assoc(l: ('a, 'b) list, x: 'a): 'b =
    find(for (a, b) <- l {a == x}).1

fun assoc_opt(l: ('a, 'b) list, x: 'a): 'b? =
    match find_opt(for (a, _) <- l {a == x}) {
    | Some((_, b)) => Some(b)
    | _ => None
    }

fun app(l: 't list, f: 't -> void): void =
    for x <- l {f(x)}

fun map(l: 't list, f: 't -> 'rt): 'rt list =
    [:: for x <- l {f(x)}]

fun all(l: 't list, f: 't -> bool): bool =
    all(for a <- l {f(a)})

fun all2(la: 'a list, lb: 'b list, f: ('a, 'b) -> bool): bool =
    all(for a <- la, b <- lb {f(a, b)})

fun exists(l: 't list, f: 't -> bool): bool =
    exists(for a <- l {f(a)})

fun mem(l: 't list, a: 't): bool =
    exists(for b <- l {a == b})

fun find(l: 't list, f: 't -> bool): 't =
    find(for a <- l {f(a)})

fun find_opt(l: 't list, f: 't -> bool): 't? =
    find_opt(for a <- l {f(a)})

fun concat(ll: 't list list): 't list =
    fold s = ([]: 't list) for l <- rev(ll) {l + s}

fun filter(l: 't list, f: 't -> bool): 't list =
    [:: for x <- l { if !f(x) {continue}; x }]

fun zip(la: 'a list, lb: 'b list): ('a, 'b) list =
    [:: for x <- la, y <- lb {(x, y)}]

fun unzip(lab: ('a, 'b) list): ('a list, 'b list) =
    [:: @unzip for x <- lab {x}]

fun sort(l: 't list, lt: ('t, 't)->bool): 't list =
    match l
    {
        | [] => l
        | [:: _] => l
        | [:: a, b] => if lt(b, a) {b::a::[]} else {l}
        | _ =>
            val arr = array(l)
            arr.sort(lt)
            list(arr)
    }
