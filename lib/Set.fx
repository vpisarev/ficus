/*
    This file is a part ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* The ordered set implemented using Red-Black trees.

   This is partially derived from Ocaml's Red-Black tree implementation
   by Benedikt Meurer taken from https://github.com/bmeurer/ocaml-rbtrees.

   Below is the original copyright and the license:

 * =====
 * Copyright (c) 2007, Benedikt Meurer <benedikt.meurer@googlemail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software {out restriction, including {out limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * This is my implementation of Red-Black Trees for OCaml. It is based upon
 * "Red-Black Trees in a Functional Setting", Chris Okasaki in "Functional
 * Pearls".
 * Red-Black Trees are exposed via a map and a set API, which is designed to
 * be compatible { the Map and Set modules in the OCaml standard library
 * (which are implemented using AVL trees). You can use the Rbmap and Rbset
 * modules as drop-in replacement for the Map and Set modules.
 * =====
*/

type cmp_t[T] = (T, T) -> int
type color_t = Red | Black
type tree_t[T] = Empty | Node: (color_t, tree_t[T], T, tree_t[T])
class t[T] { root: tree_t[T]; size: int; cmp: cmp_t[T] }

exception RBSetError

fun empty[T](cmp: cmp_t[T]): Set.t[T] =
    t { root=(Empty : tree_t[T]), size=0, cmp=cmp }

fun empty[T](s: Set.t[T]): bool
{
    | { root=(Empty : tree_t[T]) } => true
    | _ => false
}

@private fun mem_[T](t: tree_t[T], x: T, cmp: cmp_t[T]): bool =
match t
{
    | Node(_, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 { mem_(l, x, cmp) }
        else if c > 0 { mem_(r, x, cmp) }
        else { true }
    | _ => false
}

fun mem[T](s: Set.t[T], x: T): bool = mem_(s.root, x, s.cmp)

@private fun find_opt_[T](t: tree_t[T], x: T, cmp: cmp_t[T]): T? =
match t
{
    | Node(_, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 { mem_(l, x, cmp) }
        else if c > 0 { mem_(r, x, cmp) }
        else { Some(y) }
    | _ => None
}

fun find_opt[T](s: Set.t[T], x: T): T? = find_opt_(s.root, x, s.cmp)

@private fun balance_left[T](l: tree_t[T], x: T, r: tree_t[T]): tree_t[T]
{
    | (Node(Red, Node(Red, a, x, b), y, c), z, d) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | (Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | _ =>
        Node(Black, l, x, r)
}

@private fun balance_right[T](l: tree_t[T], x: T, r: tree_t[T]): tree_t[T]
{
    | (a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | (a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | _ =>
        Node(Black, l, x, r)
}

@private fun blackify[T](t: tree_t[T]): (tree_t[T], bool)
{
    | Node(Red, l, x, r) => (Node(Black, l, x, r), false)
    | _ => (t, true)
}

@private fun add_[T](t: tree_t[T], x: T, cmp: cmp_t[T]): (tree_t[T], int)
{
    fun add_to_tree_[T](t: tree_t[T], x: T, cmp: cmp_t[T]): (tree_t[T], int) =
    match t {
    | Node(Red, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 {
            val (l, dsz) = add_to_tree_(l, x, cmp)
            (Node(Red, l, y, r), dsz)
        }
        else if c > 0 {
            val (r, dsz) = add_to_tree_(r, x, cmp)
            (Node(Red, l, y, r), dsz)
        }
        else { (t, 0) }
    | Node(Black, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 {
            val (l, dsz) = add_to_tree_(l, x, cmp)
            (if dsz > 0 {balance_left(l, y, r)} else {Node(Black, l, y, r)}, dsz)
        }
        else if c > 0 {
            val (r, dsz) = add_to_tree_(r, x, cmp)
            (if dsz > 0 {balance_right(l, y, r)} else {Node(Black, l, y, r)}, dsz)
        }
        else { (t, 0) }
    | _ => (Node(Red, (Empty: tree_t[T]), x, (Empty: tree_t[T])), 1)
    }

    val (t, dsz) = add_to_tree_(t, x, cmp)
    val (t, _) = blackify(t)
    (t, dsz)
}

fun add[T](s: Set.t[T], x: T): Set.t[T]
{
    val (new_root, dsz) = add_(s.root, x, s.cmp)
    t { root=new_root, size=s.size+dsz, cmp=s.cmp }
}

fun singleton[T](x: T, cmp: cmp_t[T]): Set.t[T] =
    t { root=Node(Black, Empty, x, Empty), size=1, cmp=cmp }

@private fun unbalanced_left[T](t: tree_t[T], dsz: int): (tree_t[T], bool, int) =
match t
{
    | Node(Red, Node(Black, a, x, b), y, c) =>
        (balance_left(Node(Red, a, x, b), y, c), false, dsz)
    | Node(Black, Node(Black, a, x, b), y, c) =>
        (balance_left(Node(Red, a, x, b), y, c), true, dsz)
    | Node(Black, Node(Red, a, x, Node(Black, b, y, c)), z, d) =>
        (Node(Black, a, x, balance_left(Node(Red, b, y, c), z, d)), false, dsz)
    | _ => throw RBSetError
}

@private fun unbalanced_right[T](t: tree_t[T], dsz: int): (tree_t[T], bool, int) =
match t
{
    | Node(Red, a, x, Node(Black, b, y, c)) =>
        (balance_right(a, x, Node(Red, b, y, c)), false, dsz)
    | Node(Black, a, x, Node(Black, b, y, c)) =>
        (balance_right(a, x, Node(Red, b, y, c)), true, dsz)
    | Node(Black, a, x, Node(Red, Node(Black, b, y, c), z, d)) =>
        (Node(Black, balance_right(a, x, Node(Red, b, y, c)), z, d), false, dsz)
    | _ => throw RBSetError
}

@private fun remove_min[T](t: tree_t[T]): (tree_t[T], T, bool)
{
    | Node(Black, Empty, x, Empty) => (Empty, x, true)
    | Node(Black, Empty, x, Node(Red, l, y, r)) =>
        (Node(Black, l, y, r), x, false)
    | Node(Red, Empty, x, r) =>
        (r, x, false)
    | Node(Black, l, x, r) =>
        val (l, y, d) = remove_min(l)
        val s = Node(Black, l, x, r)
        if d {
            val (s, d, _) = unbalanced_right(s, 0)
            (s, y, d)
        } else {
            (s, y, false)
        }
    | Node(Red, l, x, r) =>
        val (l, y, d) = remove_min(l)
        val s = Node(Red, l, x, r)
        if d {
            val (s, d, _) = unbalanced_right(s, 0)
            (s, y, d)
        } else {
            (s, y, false)
        }
    | _ => throw RBSetError
}

@private fun remove_[T](t: tree_t[T], x: T, cmp: cmp_t[T]): (tree_t[T], bool, int) =
match t
{
    | Node(Black, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 {
            val (l, d, dsz) = remove_(l, x, cmp)
            val s = Node(Black, l, y, r)
            if d { unbalanced_right(s, dsz) } else { (s, false, dsz) }
        } else if c > 0 {
            val (r, d, dsz) = remove_(r, x, cmp)
            val s = Node(Black, l, y, r)
            if d { unbalanced_left(s, dsz) } else { (s, false, dsz) }
        } else {
            match r {
            | Empty =>
                val (l, d) = blackify(l)
                (l, d, -1)
            | _ =>
                val (r, y, d) = remove_min(r)
                val s = Node(Black, l, y, r)
                if d { unbalanced_left(s, -1) } else { (s, false, -1) }
            }
        }
    | Node(Red, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 {
            val (l, d, dsz) = remove_(l, x, cmp)
            val s = Node(Red, l, y, r)
            if d { unbalanced_right(s, dsz) } else { (s, false, dsz) }
        } else if c > 0 {
            val (r, d, dsz) = remove_(r, x, cmp)
            val s = Node(Red, l, y, r)
            if d { unbalanced_left(s, dsz) } else { (s, false, dsz) }
        } else {
            match r {
            | Empty => (l, false, -1)
            | _ =>
                val (r, y, d) = remove_min(r)
                val s = Node(Red, l, y, r)
                if d { unbalanced_left(s, -1) } else { (s, false, -1) }
            }
        }
    | _ =>
        (Empty, false, 0)
}

fun remove[T](s: Set.t[T], x: T): Set.t[T]
{
    val (new_root, _, dsz) = remove_(s.root, x, s.cmp)
    t { root=new_root, size=s.size+dsz, cmp=s.cmp }
}

fun foldl[T, Tr](s: Set.t[T], f: (T, Tr) -> Tr, res0: Tr): Tr
{
    fun update_[T](t: tree_t[T], f: (T, Tr) -> Tr, res: Tr): Tr =
    match t {
        | Node(_, l, x, r) => update_(r, f, f(x, update_(l, f, res)))
        | _ => res
    }
    update_(s.root, f, res0)
}

fun foldr[T, Tr](s: Set.t[T], f: (T, Tr) -> Tr, res0: Tr): Tr
{
    fun update_[T](t: tree_t[T], f: (T, Tr) -> Tr, res: Tr): Tr =
    match t {
        | Node(_, l, x, r) => update_(l, f, f(x, update_(r, f, res)))
        | _ => res
    }
    update_(s.root, f, res0)
}

fun all[T](s: Set.t[T], f: T -> bool): bool
{
    fun all_[T](t: tree_t[T], f: T -> bool): bool =
    match t {
        | Node(_, l, x, r) => f(x) && all_(l, f) && all_(r, f)
        | _ => true
    }
    all_(s.root, f)
}

fun exists[T](s: Set.t[T], f: T -> bool): bool
{
    fun exists_[T](t: tree_t[T], f: T -> bool): bool =
    match t {
        | Node(_, l, x, r) => f(x) || exists_(l, f) || exists_(r, f)
        | _ => false
    }
    exists_(s.root, f)
}

fun app[T](s: Set.t[T], f: T -> void): void
{
    fun app_[T](t: tree_t[T], f: T -> void): void =
    match t {
        | Node(_, l, x, r) => app_(l, f); f(x); app_(r, f)
        | _ => {}
    }
    app_(s.root, f)
}

// similar to foldr, but does a specific task - constructs the list of results
fun map[T, Tr](s: Set.t[T], f: T -> Tr): list[Tr]
{
    fun update_list_[T](t: tree_t[T], f: T -> Tr, res: list[Tr]): list[Tr] =
    match t {
        | Node(_, l, x, r) =>
            update_list_(l, f, f(x) :: update_list_(r, f, res))
        | _ => res
    }
    update_list_(s.root, f, [])
}

fun filter[T](s: Set.t[T], f: T -> bool): Set.t[T]
{
    fun filter_[T](t: tree_t[T], f: T -> bool, res: Set.t[T]): Set.t[T] =
    match t {
        | Node(_, l, x, r) =>
            val res = filter_(l, f, res)
            val res = if f(x) { add(res, x) } else { res }
            filter_(r, f, res)
        | _ => res
    }
    filter_(s.root, f, t {root=Empty, size=0, cmp=s.cmp})
}

fun add_list[T](s: Set.t[T], l: list[T]): Set.t[T]
{
    val cmp = s.cmp
    val fold new_root = s.root, size = s.size for x <- l {
        val (new_root_, dsz) = add_(new_root, x, cmp)
        new_root = new_root_;
        size += dsz
    }
    t {root=new_root, size=size, cmp=cmp}
}

fun from_list[T](cmp: cmp_t[T], l: list[T]): Set.t[T] = add_list(empty(cmp), l)

fun list[T](s: Set.t[T]): list[T]
{
    fun update_list_[T](t: tree_t[T], res: list[T]): list[T] =
    match t {
        | Node(_, l, x, r) =>
            update_list_(l, x :: update_list_(r, res))
        | _ => res
    }
    update_list_(s.root, [])
}

fun diff[T](xs: Set.t[T], ys: Set.t[T]): Set.t[T]
{
    fun update_[T](t: tree_t[T], cmp: cmp_t[T], res: tree_t[T], size: int): (tree_t[T], int) =
    match t
    {
        | Node(_, l, x, r) =>
            val (res, size) = update_(l, cmp, res, size)
            val (res, _, dsz) = remove_(res, x, cmp)
            update_(r, cmp, res, size+dsz)
        | _ => (res, size)
    }

    // Assuming that size(ys) is much smaller than size(xs),
    // the fastest way to compute the difference is to remove
    // ys' elements one-by-one from xs.
    // The complexity is O(log(size_xs)*size_ys).
    // If size(ys) ~ size(xs) ~ N then whatever algorithm we use
    // (using balanced trees), it will have O(log(N)*N) complexity.
    val (res, size) = update_(ys.root, xs.cmp, xs.root, xs.size)
    t {root=res, size=size, cmp=xs.cmp}
}

fun intersect[T](xs: Set.t[T], ys: Set.t[T]): Set.t[T]
{
    fun update_[T](t: tree_t[T], cmp: cmp_t[T], xs: tree_t[T], res: tree_t[T], size: int): (tree_t[T], int) =
    match t
    {
        | Node(_, l, x, r) =>
            val (res, size) = update_(l, cmp, xs, res, size)
            val (res, dsz) = if mem_(xs, x, cmp) {add_(res, x, cmp)} else {(res, 0)}
            update_(r, cmp, xs, res, size+dsz)
        | _ => (res, size)
    }

    val (xs, ys) = if xs.size >= ys.size {(xs, ys)} else {(ys, xs)}

    // Assuming that size(ys) is much smaller than size(xs),
    // the fastest way to compute the intersection is to
    // add those ys' elements that also belong to xs to the final set.
    // The complexity is O(log(size_xs)*size_ys).
    // If size(ys) ~ size(xs) ~ N then whatever algorithm we choose
    // (using balanced trees), it will have O(log(N)*N) complexity.
    val (res, size) = update_(ys.root, xs.cmp, xs.root, (Empty : tree_t[T]), 0)
    t {root=res, size=size, cmp=xs.cmp}
}

fun union[T](xs: Set.t[T], ys: Set.t[T]): Set.t[T]
{
    fun update_[T](t: tree_t[T], cmp: cmp_t[T], res: tree_t[T], size: int): (tree_t[T], int) =
    match t
    {
        | Node(_, l, x, r) =>
            val (res, size) = update_(l, cmp, res, size)
            val (res, dsz) = add_(res, x, cmp)
            update_(r, cmp, res, size+dsz)
        | _ => (res, size)
    }

    val (xs, ys) = if xs.size >= ys.size {(xs, ys)} else {(ys, xs)}

    // Assuming that size(ys) is much smaller than size(xs),
    // the fastest way to compute the union is to
    // add ys' elements xs. The complexity is O(log(size_xs)*size_ys).
    // If size(ys) ~ size(xs) ~ N then whatever algorithm we choose
    // (using balanced trees), it will have O(log(N)*N) complexity.
    val (res, size) = update_(ys.root, xs.cmp, xs.root, xs.size)
    t {root=res, size=size, cmp=xs.cmp}
}

fun minelem[T](s: Set.t[T]): T
{
    fun min_[T](t: tree_t[T]) {
        | Node(_, Empty, x, _) => x
        | Node(_, l, _, _) => min_(l)
        | _ => throw RBSetError
    }
    min_(s.root)
}

fun maxelem[T](s: Set.t[T]): T
{
    fun max_[T](t: tree_t[T]) {
        | Node(_, _, x, Empty) => x
        | Node(_, _, _, r) => max_(r)
        | _ => throw RBSetError
    }
    max_(s.root)
}
