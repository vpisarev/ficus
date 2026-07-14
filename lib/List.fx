/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Operations on LISP-list[like]s */

fun length[T](l: list[T]): int = Builtins.length(l)
fun hd[T](_: list[T]): T { | a :: _ => a | _ => throw NullListError }
fun tl[T](_: list[T]): list[T] { | _ :: ll => ll | _ => throw NullListError }
fun empty[T](_: list[T]): bool { | [] => true | _ => false }
fun last[T](_: list[T]): T
{
    | [:: a] => a
    | _ :: rest => last(rest)
    | _ => throw NullListError
}

fun nth[T](l: list[T], n: int): T =
match l
{
    | a :: rest => if n == 0 {a} else {nth(rest, n-1)}
    | _ => throw OutOfRangeError
}
fun skip[T](l: list[T], n: int): list[T] =
    if n == 0 {l} else { match l { | a :: rest => skip(rest, n-1) | _ => throw OutOfRangeError } }
fun skip_nothrow[T](l: list[T], n: int): list[T] =
    if n == 0 {l} else { match l { | a :: rest => skip_nothrow(rest, n-1) | _ => [] } }

fun rev[T](l: list[T]): list[T] =
    fold res=[] for a <- l {res = a :: res}

fun foldl[T, Tr](l: list[T], f: (T, Tr) -> Tr, res0: Tr): Tr =
    fold res=res0 for a <- l {res = f(a, res)}

fun assoc[T1, T2](l: list[T1, T2], x: T1): T2 =
    find(for (a, b) <- l {a == x}).1

fun assoc_opt[T1, T2](l: list[T1, T2], x: T1): T2? =
    match find_opt(for (a, _) <- l {a == x}) {
    | Some((_, b)) => Some(b)
    | _ => None
    }

fun app[T](l: list[T], f: T -> void): void =
    for x <- l {f(x)}

fun map[T, Tr](l: list[T], f: T -> Tr): list[Tr] =
    [:: for x <- l {f(x)}]

fun all[T](l: list[T], f: T -> bool): bool =
    all(for a <- l {f(a)})

fun all2[T1, T2](la: list[T1], lb: list[T2], f: (T1, T2) -> bool): bool =
    all(for a <- la, b <- lb {f(a, b)})

fun exists[T](l: list[T], f: T -> bool): bool =
    exists(for a <- l {f(a)})

fun mem[T](l: list[T], a: T): bool =
    exists(for b <- l {a == b})

fun find[T](l: list[T], f: T -> bool): T =
    find(for a <- l {f(a)})

fun find_opt[T](l: list[T], f: T -> bool): T? =
    find_opt(for a <- l {f(a)})

fun concat[T](ll: list[T] list): list[T] =
    fold s = ([]: list[T]) for l <- rev(ll) {s = l + s}

fun filter[T](l: list[T], f: T -> bool): list[T] =
    [:: for x <- l { if !f(x) {continue}; x }]

fun zip[T1, T2](la: list[T1], lb: list[T2]): list[T1, T2] =
    [:: for x <- la, y <- lb {(x, y)}]

fun unzip[T1, T2](lab: list[T1, T2]): (list[T1], list[T2]) =
    [:: @unzip for x <- lab {x}]

fun sort[T](l: list[T], lt: (T, T)->bool): list[T] =
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
