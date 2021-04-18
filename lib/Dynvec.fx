/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// very simple STL-like mutable vector (do not confuse it with embedded RRB tree-based immutable 'vector'),
// a simple wrapper on top of arrays. The functionality is just enough to implement symbol tables for the compiler

object type 't t =
{
    var count: int;
    var data: 't [];
    val val0: 't
}

fun create(n0: int, v0: 't): 't Dynvec.t = t { count = 0, data = array(n0, v0), val0 = v0 }
fun clear(v: 't Dynvec.t) { v.count = 0; v.data = [] }
fun empty(v: 't Dynvec.t): bool = v.count == 0
fun size(v: 't Dynvec.t): int = v.count

fun push(v: 't Dynvec.t): int
{
    val sz = size(v.data)
    val n0 = v.count
    if sz <= n0 {
        val n1 = max(n0, 128)*3/2
        val old_data = v.data
        val val0 = v.val0
        val new_data = [| for i <- 0:n1 { if i < n0 {old_data[i]} else {val0} } |]
        v.data = new_data
    }
    val i = n0
    v.count = n0 + 1
    i
}

fun push(v: 't Dynvec.t, x: 't)
{
    val idx = push(v)
    v.data[idx] = x
    idx
}
