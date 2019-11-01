/* Operations on LISP-like lists */

fun hd(_: 't list): 't { | a :: _ => a | _ => throw NullListError }
fun tl(_: 't list): 't { | _ :: ll => ll | _ => throw NullListError }
fun null(_: 't list): bool { | [] => true | _ => false }
fun last(_: 't list): 't
{
    | a :: [] => a
    | a :: rest => last(rest)
    | _ => throw NullListError
}

fun nth(l: 't list, n: int): 't
{
    | a :: rest => if (n == 0) a else nth(rest, n-1)
    | _ => throw OutOfRangeError
}

fun length(l: 't list): int
{
    fun length_(l: 't list, n: int) =
        match(l) {
        | _ :: rest => length(rest, n+1)
        | _ => n
        }
    length_(l, 0)
}

fun rev(l: 't list): 't list = rev(l, [])

fun rev(l: 't list, rl: 't list): 't list =
    match(l) {
    | a :: rest => rev(rest, a :: rl)
    | _ => rl
    }

fun array(l: 't list): 't [] = [for (x <- l) x]

fun all(l: 't list, f: 't -> bool): bool =
    match(l) {
    | a :: rest => if (f(a)) all(rest, f) else false
    | _ => true
    }

fun all2((la, lb): ('a list, 'b list), f: ('a, 'b) -> bool): bool =
    match (la, lb) {
    | (a :: rest_a, b :: rest_b) => if (f(a, b)) all2((rest_a, rest_b), f) else false
    | ([], []) => true
    | _ => throw ListSizeMismatchError
    }

fun exists(l: 't list, f: 't -> bool): bool =
    match(l) {
    | a :: rest => if (f(a)) true else exists(rest, f)
    | _ => false
    }

fun mem(l: 't list, a: 't): bool =
    match (l) {
    | b :: rest => if (a == b) true else mem(rest, a)
    | _ => false
    }

fun find_opt(l: 't list, f: 't -> bool): 't? =
    match (l) {
    | a :: rest => if (f(a)) Some(a) else find(rest, f)
    | _ => None
    }

fun concat(ll: 't list list): 't list =
    fold(s = []; l <- rev(ll))
        s = l + s

fun zip(la: 'a list, lb: 'b list): ('a, 'b) list =
    [:: for (x <- la, y <- lb) (x, y)]

fun unzip(lab: ('a, 'b) list): ('a list, 'b list) =
    unzip([:: for (x <- lab) x])

// O(n log n) merge sort
fun mergeSort(l: 't list, gt: ('t,'t)->bool): 't list =
    match(l) {
    | [] => []
    | l =>
        fun merge(_: 't list, _: 't list): 't list {
            | ((a :: at) as l, (b :: bt) as r) => if (gt(a, b)) b :: merge(l, bt) else a :: merge(at, r)
            | (l, []) => l
            | ([], r) => r
        }

        fun scan(_: 't list list): 't list list {
            | a :: b :: rest => merge(a, b) :: scan(rest)
            | l => l
        }

        fun loop(_: 't list list): 't list {
            | a :: [] => a
            | l => loop(scan(l))
        }

        loop([:: for (a <- l) a :: []])
    }
