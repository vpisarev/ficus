/*
    This file is a part ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* The ordered set implemented using Red-Black trees.

   This is partially derived from Ocaml's Red-Black tree implementation
   by Benedikt Meurer taken from https://github.com/bmeurer/ocaml-rbtrees.

   Below is the original copyright and the license:

 * ====
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
 * ====
*/

type 't cmp_t = ('t, 't) -> int
type color_t = Red | Black
type 't tree_t = Empty | Node: (color_t, 't tree_t, 't, 't tree_t)
module type 't set_t = { root: 't tree_t, size: int, cmp: 't cmp_t }

exception RBSetError

fun empty(cmp: 't cmp_t): 't set_t =
    set_t { root=(Empty : 't tree_t), size=0, cmp=cmp }

fun empty(s: 't set_t): bool
{
    | { root=(Empty : 't tree_t) } => true
    | _ => false
}

fun mem_(t: 't tree_t, x: 't, cmp: 't cmp_t) =
match t
{
    | Node(_, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 { mem_(l, x, cmp) }
        else if c > 0 { mem_(r, x, cmp) }
        else { true }
    | _ => false
}

fun mem(s: 't set_t, x: 't): bool = mem_(s.root, x, s.cmp)

fun balance_left(l: 't tree_t, x: 't, r: 't tree_t)
{
    | (Node(Red, Node(Red, a, x, b), y, c), z, d) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | (Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | _ =>
        Node(Black, l, x, r)
}

fun balance_right(l: 't tree_t, x: 't, r: 't tree_t)
{
    | (a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | (a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | _ =>
        Node(Black, l, x, r)
}

fun blackify(t: 't tree_t)
{
    | Node(Red, l, x, r) => (Node(Black, l, x, r), false)
    | _ => (t, true)
}

fun add_(t: 't tree_t, x: 't, cmp: 't cmp_t): ('t tree_t, int) =
match t
{
    | Node(Red, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 {
            val (l1, dsz) = add_(l, x, cmp)
            (Node(Red, l1, y, r), dsz)
        }
        else if c > 0 {
            val (r1, dsz) = add_(r, x, cmp)
            (Node(Red, l, y, r1), dsz)
        }
        else { (t, 0) }
    | Node(Black, l, y, r) =>
        val c = cmp(x, y)
        if c < 0 {
            val (l1, dsz) = add_(l, x, cmp)
            (balance_left(l1, y, r), dsz)
        }
        else if c > 0 {
            val (r1, dsz) = add_(r, x, cmp)
            (balance_right(l, y, r1), dsz)
        }
        else { (t, 0) }
    | _ => (Node(Red, (Empty: 't tree_t), x, (Empty: 't tree_t)), 1)
}

fun add(s: 't set_t, x: 't): 't set_t
{
    val (new_root, dsz) = add_(s.root, x, s.cmp)
    val new_root = blackify(new_root).0
    set_t { root=new_root, size=s.size+dsz, cmp=s.cmp }
}

fun singleton(x: 't, cmp: 't cmp_t): 't set_t =
    set_t { root=Node(Black, Empty, x, Empty), size=1, cmp=cmp }

fun unbalanced_left(t: 't tree_t, dsz: int): ('t tree_t, bool, int) =
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

fun unbalanced_right(t: 't tree_t, dsz: int): ('t tree_t, bool, int) =
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

fun remove_min(t: 't tree_t): ('t tree_t, 't, bool)
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

fun remove_(t: 't tree_t, x: 't, cmp: 't cmp_t): ('t tree_t, bool, int) =
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
                val (l1, d) = blackify(l)
                (l1, d, -1)
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

fun remove(s: 't set_t, x: 't)
{
    val (new_root, _, dsz) = remove_(s.root, x, s.cmp)
    set_t { root=new_root, size=s.size+dsz, cmp=s.cmp }
}

fun fold_left(s: 't set_t, f: ('t, 'r) -> 'r, res0: 'r)
{
    fun update_(t: 't tree_t, f: ('t, 'r) -> 'r, res: 'r): 'r =
    match t {
        | Node(_, l, x, r) => update_(r, f, f(x, update_(l, f, res)))
        | _ => res
    }
    update_(s.root, f, res0)
}

fun fold_right(s: 't set_t, f: ('t, 'r) -> 'r, res0: 'r)
{
    fun update_(t: 't tree_t, f: ('t, 'r) -> 'r, res: 'r): 'r =
    match t {
        | Node(_, l, x, r) => update_(l, f, f(x, update_(r, f, res)))
        | _ => res
    }
    update_(s.root, f, res0)
}

fun app(s: 't set_t, f: 't -> void)
{
    fun app_(t: 't rbtree, f: 't -> void): void =
    match t {
        | Node(_, l, x, r) => app_(l, f); f(x); app_(r, f)
        | _ => {}
    }
    app_(s.root, f)
}

// similar to fold_right, but does a specific task - constructs the list of results
fun map(s: 't set_t, f: 't -> 'r): 'res list {
    fun update_list_(t: 't tree_t, f: 't -> 'r, res: 'r list): 'r list =
    match t {
        | Node(_, l, x, r) =>
            update_list_(l, f, f(x) :: update_list_(r, f, res))
        | _ => res
    }
    update_list_(s.root, f, [])
}

fun add_list(s: 't set_t, l: 't list)
{
    val cmp = s.cmp
    val fold (new_root, size)=(s.root, s.size) for x <- l {
        val (new_root, dsz) = add_(new_root, x, cmp)
        (new_root, size+dsz)
    }
    set_t {root=new_root, size=size, cmp=cmp}
}

fun from_list(l: 't list, cmp: 't cmp_t): 't set_t = add_list(empty(cmp), l)

fun list(s: 't set_t): 't list
{
    fun update_list_(t: 't tree_t, res: 't list): 't list =
    match t {
        | Node(_, l, x, r) =>
            update_list_(l, x :: update_list_(r, res))
        | _ => res
    }
    update_list_(s.root, [])
}

fun diff(xs: 't set_t, ys: 't set_t)
{
    fun update_(t: 't tree_t, cmp: 't cmp_t, res: 't tree_t, size: int): ('t tree_t, int) =
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
    // ys elements one-by-one from xs.
    // The complexity is O(log(size_xs)*size_ys)
    val (res, size) = update_(ys.root, xs.cmp, xs.root, xs.size)
    set_t {root=res, size=size, cmp=xs.cmp}
}

fun intersect(xs: 't set_t, ys: 't set_t)
{
    fun update_(t: 't tree_t, cmp: 't cmp_t, xs: 't tree_t, res: 't tree_t, size: int): ('t tree_t, int) =
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
    // add those ys elements that also belong to xs to the final set.
    // The complexity is O(log(size_xs)*size_ys)
    val (res, size) = update_(ys.root, xs.cmp, xs.root, (Empty : 't tree_t), 0)
    set_t {root=res, size=size, cmp=xs.cmp}
}

fun union(xs: 't set_t, ys: 't set_t)
{
    fun update_(t: 't tree_t, cmp: 't cmp_t, res: 't tree_t, size: int): ('t tree_t, int) =
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
    // add ys elements xs. The complexity is O(log(size_xs)*size_ys)
    val (res, size) = update_(ys.root, xs.cmp, xs.root, xs.size)
    set_t {root=res, size=size, cmp=xs.cmp}
}

fun minelem(s: 't set_t): 't
{
    fun min_(t: 't tree_t) {
        | Node(_, Empty, x, _) => x
        | Node(_, l, _, _) => min_(l)
        | _ => throw RBSetError
    }
    min_(s.root)
}

fun maxelem(s: 't set_t): 't
{
    fun max_(t: 't tree_t) {
        | Node(_, _, x, Empty) => x
        | Node(_, _, _, r) => max_(r)
        | _ => throw RBSetError
    }
    max_(s.root)
}
