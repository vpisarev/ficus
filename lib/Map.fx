/*
    This file is a part ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* The ordered map implemented using Red-Black trees.

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

type cmp_t[K] = (K, K) -> int
type color_t = Red | Black
type tree_t[K, D] = Empty | Node: (color_t, tree_t[K, D], K, D, tree_t[K, D])
class t[K, D] { root: tree_t[K, D]; cmp: cmp_t[K] }

exception RBMapError

fun empty[K, D](cmp: cmp_t[K]): Map.t[K, D] =
    t { root=(Empty : tree_t[K, D]), cmp=cmp }

fun empty[K, D](m: Map.t[K, D]): bool
{
    | { root=(Empty : tree_t[K, D]) } => true
    | _ => false
}

@private fun find_opt_[K, D](t: tree_t[K, D], xk: K, cmp: cmp_t[K]) : D? =
match t
{
    | Node(_, l, yk, yd, r) =>
        val c = cmp(xk, yk)
        if c < 0 { find_opt_(l, xk, cmp) }
        else if c > 0 { find_opt_(r, xk, cmp) }
        else { Some(yd) }
    | _ => None
}

fun find_opt[K, D](m: Map.t[K, D], x: K): D? = find_opt_(m.root, x, m.cmp)

fun find[K, D](m: Map.t[K, D], xk: K, def_xd: D): D =
match find_opt(m, xk)
{
    | Some(xd) => xd
    | _ => def_xd
}

fun mem[K, D](m: Map.t[K, D], x: K): bool
{
    fun mem_[K, D](t: tree_t[K, D], xk: K, cmp: cmp_t[K]): bool =
    match t {
        | Node(_, l, yk, yd, r) =>
            val c = cmp(xk, yk)
            if c < 0 { mem_(l, xk, cmp) }
            else if c > 0 { mem_(r, xk, cmp) }
            else { true }
        | _ => false
    }
    mem_(m.root, x, m.cmp)
}

@private fun balance_left[K, D](l: tree_t[K, D], xk: K, xd: D, r: tree_t[K, D]): tree_t[K, D]
{
    | (Node(Red, Node(Red, a, xk, xd, b), yk, yd, c), zk, zd, d) =>
        Node(Red, Node(Black, a, xk, xd, b), yk, yd, Node(Black, c, zk, zd, d))
    | (Node(Red, a, xk, xd, Node(Red, b, yk, yd, c)), zk, zd, d) =>
        Node(Red, Node(Black, a, xk, xd, b), yk, yd, Node(Black, c, zk, zd, d))
    | _ =>
        Node(Black, l, xk, xd, r)
}

@private fun balance_right[K, D](l: tree_t[K, D], xk: K, xd: D, r: tree_t[K, D]): tree_t[K, D]
{
    | (a, xk, xd, Node(Red, Node(Red, b, yk, yd, c), zk, zd, d)) =>
        Node(Red, Node(Black, a, xk, xd, b), yk, yd, Node(Black, c, zk, zd, d))
    | (a, xk, xd, Node(Red, b, yk, yd, Node(Red, c, zk, zd, d))) =>
        Node(Red, Node(Black, a, xk, xd, b), yk, yd, Node(Black, c, zk, zd, d))
    | _ =>
        Node(Black, l, xk, xd, r)
}

@private fun blackify[K, D](t: tree_t[K, D]): (tree_t[K, D], bool)
{
    | Node(Red, l, xk, xd, r) => (Node(Black, l, xk, xd, r), false)
    | _ => (t, true)
}

@private fun add_[K, D](t: tree_t[K, D], xk: K, xd: D, cmp: cmp_t[K]): tree_t[K, D]
{
    fun add_to_tree_[K, D](t: tree_t[K, D], xk: K, xd: D, cmp: cmp_t[K]): tree_t[K, D] =
    match t
    {
        | Node(Red, l, yk, yd, r) =>
            val c = cmp(xk, yk)
            if c < 0 { Node(Red, add_to_tree_(l, xk, xd, cmp), yk, yd, r) }
            else if c > 0 { Node(Red, l, yk, yd, add_to_tree_(r, xk, xd, cmp)) }
            else { Node(Red, l, xk, xd, r) }
        | Node(Black, l, yk, yd, r) =>
            val c = cmp(xk, yk)
            if c < 0 { balance_left(add_to_tree_(l, xk, xd, cmp), yk, yd, r) }
            else if c > 0 { balance_right(l, yk, yd, add_to_tree_(r, xk, xd, cmp)) }
            else { Node(Black, l, xk, xd, r) }
        | _ => Node(Red, (Empty: tree_t[K, D]), xk, xd, (Empty: tree_t[K, D]))
    }
    blackify(add_to_tree_(t, xk, xd, cmp)).0
}

fun add[K, D](m: Map.t[K, D], xk: K, xd: D): Map.t[K, D]
{
    val new_root = add_(m.root, xk, xd, m.cmp)
    t { root=new_root, cmp=m.cmp }
}

@private fun update_[K, D](t: tree_t[K, D], xk: K, f: ((K, D?) -> D), cmp: cmp_t[K]): tree_t[K, D] =
match t
{
    | Node(Red, l, yk, yd, r) =>
        val c = cmp(xk, yk)
        if c < 0 { Node(Red, update_(l, xk, f, cmp), yk, yd, r) }
        else if c > 0 { Node(Red, l, yk, yd, update_(r, xk, f, cmp)) }
        else { Node(Red, l, xk, f(xk, Some(yd)), r) }
    | Node(Black, l, yk, yd, r) =>
        val c = cmp(xk, yk)
        if c < 0 { balance_left(update_(l, xk, f, cmp), yk, yd, r) }
        else if c > 0 { balance_right(l, yk, yd, update_(r, xk, f, cmp)) }
        else { Node(Black, l, xk, f(xk, Some(yd)), r) }
    | _ =>
        Node(Red, (Empty: tree_t[K, D]), xk,
            f(xk, (None : D?)), (Empty: tree_t[K, D]))
}

fun update[K, D](m: Map.t[K, D], xk: K, f: ((K, D?) -> D)): Map.t[K, D]
{
    val new_root = blackify(update_(m.root, xk, f, m.cmp)).0
    t { root=new_root, cmp=m.cmp }
}

@private fun unbalanced_left[K, D](t: tree_t[K, D]): (tree_t[K, D], bool) =
match t
{
    | Node(Red, Node(Black, a, xk, xd, b), yk, yd, c) =>
        (balance_left(Node(Red, a, xk, xd, b), yk, yd, c), false)
    | Node(Black, Node(Black, a, xk, xd, b), yk, yd, c) =>
        (balance_left(Node(Red, a, xk, xd, b), yk, yd, c), true)
    | Node(Black, Node(Red, a, xk, xd, Node(Black, b, yk, yd, c)), zk, zd, d) =>
        (Node(Black, a, xk, xd, balance_left(Node(Red, b, yk, yd, c), zk, zd, d)), false)
    | _ => throw RBMapError
}

@private fun unbalanced_right[K, D](t: tree_t[K, D]): (tree_t[K, D], bool) =
match t
{
    | Node(Red, a, xk, xd, Node(Black, b, yk, yd, c)) =>
        (balance_right(a, xk, xd, Node(Red, b, yk, yd, c)), false)
    | Node(Black, a, xk, xd, Node(Black, b, yk, yd, c)) =>
        (balance_right(a, xk, xd, Node(Red, b, yk, yd, c)), true)
    | Node(Black, a, xk, xd, Node(Red, Node(Black, b, yk, yd, c), zk, zd, d)) =>
        (Node(Black, balance_right(a, xk, xd, Node(Red, b, yk, yd, c)), zk, zd, d), false)
    | _ => throw RBMapError
}

@private fun remove_min[K, D](t: tree_t[K, D]): (tree_t[K, D], K, D, bool)
{
    | Node(Black, Empty, xk, xd, Empty) => (Empty, xk, xd, true)
    | Node(Black, Empty, xk, xd, Node(Red, l, yk, yd, r)) =>
        (Node(Black, l, yk, yd, r), xk, xd, false)
    | Node(Red, Empty, xk, xd, r) =>
        (r, xk, xd, false)
    | Node(Black, l, xk, xd, r) =>
        val (l, yk, yd, d) = remove_min(l)
        val s = Node(Black, l, xk, xd, r)
        if d {
            val (s, d) = unbalanced_right(s)
            (s, yk, yd, d)
        } else {
            (s, yk, yd, false)
        }
    | Node(Red, l, xk, xd, r) =>
        val (l, yk, yd, d) = remove_min(l)
        val s = Node(Red, l, xk, xd, r)
        if d {
            val (s, d) = unbalanced_right(s)
            (s, yk, yd, d)
        } else {
            (s, yk, yd, false)
        }
    | _ => throw RBMapError
}

@private fun remove_[K, D](t: tree_t[K, D], xk: K, cmp: cmp_t[K]): (tree_t[K, D], bool) =
match t
{
    | Node(Black, l, yk, yd, r) =>
        val c = cmp(xk, yk)
        if c < 0 {
            val (l, d) = remove_(l, xk, cmp)
            val s = Node(Black, l, yk, yd, r)
            if d { unbalanced_right(s) } else { (s, false) }
        } else if c > 0 {
            val (r, d) = remove_(r, xk, cmp)
            val s = Node(Black, l, yk, yd, r)
            if d { unbalanced_left(s) } else { (s, false) }
        } else {
            match r {
            | Empty =>
                val (l1, d) = blackify(l)
                (l1, d)
            | _ =>
                val (r, yk, yd, d) = remove_min(r)
                val s = Node(Black, l, yk, yd, r)
                if d { unbalanced_left(s) } else { (s, false) }
            }
        }
    | Node(Red, l, yk, yd, r) =>
        val c = cmp(xk, yk)
        if c < 0 {
            val (l, d) = remove_(l, xk, cmp)
            val s = Node(Red, l, yk, yd, r)
            if d { unbalanced_right(s) } else { (s, false) }
        } else if c > 0 {
            val (r, d) = remove_(r, xk, cmp)
            val s = Node(Red, l, yk, yd, r)
            if d { unbalanced_left(s) } else { (s, false) }
        } else {
            match r {
            | Empty => (l, false)
            | _ =>
                val (r, yk, yd, d) = remove_min(r)
                val s = Node(Red, l, yk, yd, r)
                if d { unbalanced_left(s) } else { (s, false) }
            }
        }
    | _ =>
        (Empty, false)
}

fun remove[K, D](m: Map.t[K, D], xk: K): Map.t[K, D]
{
    val (new_root, _) = remove_(m.root, xk, m.cmp)
    t { root=new_root, cmp=m.cmp }
}

fun foldl[K, D, Tr](m: Map.t[K, D], f: (K, D, Tr) -> Tr, res0: Tr): Tr
{
    fun update_[K, D, Tr](t: tree_t[K, D], f: (K, D, Tr) -> Tr, res: Tr): Tr =
    match t {
        | Node(_, l, xk, xd, r) => update_(r, f, f(xk, xd, update_(l, f, res)))
        | _ => res
    }
    update_(m.root, f, res0)
}

fun foldr[K, D, Tr](m: Map.t[K, D], f: (K, D, Tr) -> Tr, res0: Tr): Tr
{
    fun update_[K, D, Tr](t: tree_t[K, D], f: (K, D, Tr) -> Tr, res: Tr): Tr =
    match t {
        | Node(_, l, xk, xd, r) => update_(l, f, f(xk, xd, update_(r, f, res)))
        | _ => res
    }
    update_(m.root, f, res0)
}

fun app[K, D, Tr](m: Map.t[K, D], f: (K, D) -> void): void
{
    fun app_[K, D, Tr](t: tree_t[K, D], f: (K, D) -> void): void =
    match t {
        | Node(_, l, xk, xd, r) => app_(l, f); f(xk, xd); app_(r, f)
        | _ => {}
    }
    app_(m.root, f)
}

// similar to foldr, but does a specific task - constructs the list of results
fun map[K, D, Tr](m: Map.t[K, D], f: (K, D) -> Tr): list[Tr]
{
    fun update_list_[K, D, Tr](t: tree_t[K, D], f: (K, D) -> Tr, res: list[Tr]): list[Tr] =
    match t {
        | Node(_, l, xk, xd, r) =>
            update_list_(l, f, f(xk, xd) :: update_list_(r, f, res))
        | _ => res
    }
    update_list_(m.root, f, [])
}

fun filter[K, D](m: Map.t[K, D], f: (K, D) -> bool): Map.t[K, D]
{
    fun filter_[K, D, Tr](t: tree_t[K, D], f: (K, D) -> bool,
                          cmp: cmp_t[K], res: tree_t[K, D]): tree_t[K, D] =
    match t {
        | Node(_, l, xk, xd, r) =>
            val res = filter_(l, f, cmp, res)
            val res = if f(xk, xd) { add_(res, xk, xd, cmp) } else { res }
            filter_(r, f, cmp, res)
        | _ => res
    }
    val new_root = filter_(m.root, f, m.cmp, Empty)
    t {root=new_root, cmp=m.cmp}
}

fun add_list[K, D](m: Map.t[K, D], l: list[K, D]): Map.t[K, D]
{
    val cmp = m.cmp
    val fold new_root=m.root for (xk, xd) <- l {
        new_root = add_(new_root, xk, xd, cmp)
    }
    t {root=new_root, cmp=cmp}
}

fun from_list[K, D](cmp: cmp_t[K], l: list[K, D]): Map.t[K, D] =
    add_list((empty(cmp) : Map.t[K, D]), l)

fun list[K, D](m: Map.t[K, D]): list[K, D]
{
    fun update_list_[K, D](t: tree_t[K, D], res: list[K, D]): list[K, D] =
    match t {
        | Node(_, l, xk, xd, r) =>
            update_list_(l, (xk, xd) :: update_list_(r, res))
        | _ => res
    }
    update_list_(m.root, [])
}
