/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// generics-1: the new bracketed generic notation (list[T], Map.t[K,V], ref[T],
// option[T], T [,]) with UPPERCASE type parameters declared explicitly after the
// name (`fun f[T,R](...)`, `type name[T] = ...`). During the coexistence phase
// this file is written entirely in the NEW notation, so it doubles as a live
// parse+typecheck+run regression for it. Both notations erase to identical
// typ_t (verified out-of-band by an AST diff), so behavior must match the
// old-notation stdlib these call into.

from UTest import *
import Map

// --- generic functions: declared params, bracketed application, arrays ---
fun gmap[T, R](a: T [], f: T -> R): R [] = [for x <- a {f(x)}]
fun glen[T](l: list[T]): int = List.length(l)
fun gflatten[T](ll: list[list[T]]): list[T] = List.concat(ll)
fun gderef[T](r: ref[T]): T = *r
fun ghead[T](l: list[T]): option[T] { | x :: _ => Some(x) | _ => None }

TEST("generics1.fun_array_map", fun() {
    EXPECT_EQ(gmap([1, 2, 3], fun (x: int) {x * 2}), [2, 4, 6])
    EXPECT_EQ(gmap(["a", "bb"], fun (s: string) {s.length()}), [1, 2])
})

TEST("generics1.fun_list_apps", fun() {
    EXPECT_EQ(glen([:: 10, 20, 30]), 3)
    EXPECT_EQ(gflatten([:: [:: 1, 2], [:: 3], [:: 4, 5]]), [:: 1, 2, 3, 4, 5])
})

TEST("generics1.ref_and_option", fun() {
    val r = ref 42
    EXPECT_EQ(gderef(r), 42)
    EXPECT_EQ(ghead([:: 7, 8]), Some(7))
    EXPECT_EQ(ghead(([] : int list)), (None : int?))
})

// --- multi-param record type ---
type pair_t[K, V] = { key: K; value: V }
fun mkpair[K, V](k: K, v: V): pair_t[K, V] = pair_t { key=k, value=v }
fun swap_pair[K, V](p: pair_t[K, V]): pair_t[V, K] = pair_t { key=p.value, value=p.key }

TEST("generics1.record_multiparam", fun() {
    val p = mkpair("x", 10)
    EXPECT_EQ(p.key, "x")
    EXPECT_EQ(p.value, 10)
    val q = swap_pair(p)
    EXPECT_EQ(q.key, 10)
    EXPECT_EQ(q.value, "x")
})

// --- variant type with a declared param, self-referential ---
type tree_t[T] = Leaf | Node: (tree_t[T], T, tree_t[T])
fun tsize[T](t: tree_t[T]): int {
    | Leaf => 0
    | Node(l, _, r) => tsize(l) + 1 + tsize(r)
}
fun tsum(t: tree_t[int]): int {
    | Leaf => 0
    | Node(l, x, r) => tsum(l) + x + tsum(r)
}

TEST("generics1.variant_tree", fun() {
    val t = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))
    EXPECT_EQ(tsize(t), 3)
    EXPECT_EQ(tsum(t), 6)
})

// --- body-local annotations referencing a declared param ---
fun singleton[T](x: T): list[T] { val y: list[T] = [:: x]; y }

TEST("generics1.body_local_anno", fun() {
    EXPECT_EQ(singleton(99), [:: 99])
})

// --- 2-D array of a param + tuple / function type args ---
fun sum2d[T](a: T [,], zero: T, add: (T, T) -> T): T {
    val (m, n) = size(a)
    var s = zero
    for i <- 0:m for j <- 0:n { s = add(s, a[i, j]) }
    s
}

TEST("generics1.array2d_and_fntype", fun() {
    EXPECT_EQ(sum2d([1, 2; 3, 4], 0, fun (x: int, y: int) {x + y}), 10)
})

// --- qualified multi-param application + the inference-annotation channel ---
TEST("generics1.qualified_map", fun() {
    val m0: Map.t[string, int] = Map.empty(String.cmp)
    val m1 = m0.add("a", 1).add("b", 2)
    EXPECT_EQ(m1.list().length(), 2)
    val m2 = (Map.empty(String.cmp) : Map.t[string, int])
    EXPECT_EQ(m2.list().length(), 0)
})

// --- generic CLASS with methods (methods see the class params K/V via self,
// even though they are not re-declared on each method -- the reg_deffun fix) ---
class box_t[K, V] { k: K; v: V }
fun box_t.getk(): K = self.k
fun box_t.getv(): V = self.v
fun box_t.withv[W](w: W): box_t[K, W] = box_t { k=self.k, v=w }
fun mkbox[K, V](k: K, v: V): box_t[K, V] = box_t { k=k, v=v }

TEST("generics1.generic_class_methods", fun() {
    val b = mkbox("id", 7)
    EXPECT_EQ(b.getk(), "id")
    EXPECT_EQ(b.getv(), 7)
    val b2 = b.withv([:: 1, 2, 3])
    EXPECT_EQ(b2.getk(), "id")
    EXPECT_EQ(b2.getv(), [:: 1, 2, 3])
})

// --- generic operator with a declared param ---
operator + [T](a: tree_t[T], b: tree_t[T]): tree_t[T] =
    match a { | Leaf => b | Node(l, x, r) => Node(l + b, x, r) }

TEST("generics1.generic_operator", fun() {
    val a = Node(Leaf, 1, Leaf), b = Node(Leaf, 2, Leaf)
    EXPECT_EQ(tsize(a + b), 2)
})
