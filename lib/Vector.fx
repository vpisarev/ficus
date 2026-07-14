/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on the first-class mutable vector ('t vector, runtime fx_vec_t).
// Unlike the immutable RRB 'rrbvec', this is a contiguous, growable, in-place
// mutable buffer (STL vector / Python list). Element access v[i], v[i]=a and
// size()/empty() are compiler intrinsics; the operations below are stdlib.
//
// Note: the element ops read the element size / free / copy from the vector's
// own header (fx_vecinfo_t, set when the vector is constructed), so they need no
// compile-time element metadata. Construction goes through `[]` (the compiler
// emits fx_make_vec with the correct metadata).

// construction is via the vector() family in Builtins.fx (mirrors array()):
// vector(~capacity=0), vector(n, x), vector(a: 't []), vector(l), vector(rrbvec),
// vector(s). `vector()` is the empty one; `vector(capacity=n)` pre-reserves n.

// ------------------------- capacity / size -------------------------

@nothrow fun capacity(v: 't vector): int
@ccode { return v ? v->capacity : 0; }

fun clear(v: 't vector): void
@ccode {
    if (v) return fx_vec_resize(v, 0, 0);
    return FX_OK;
}

// grow/shrink to `size`; new elements (if any) are filled with val0
fun resize(v: 't vector, size: int, val0: 't): void
{
    fun resize_(v: 't vector, size: int, val0_: ('t, bool)): void
    @ccode {
        if (!v)
            FX_FAST_THROW_RET(FX_EXN_NullPtrError);
        return fx_vec_resize(v, size, val0_);
    }
    resize_(v, size, (val0, true))
}

// clear, then resize to `size` copies of val0
fun assign(v: 't vector, size: int, val0: 't): void
{
    clear(v)
    resize(v, size, val0)
}

// reserve is a thin wrapper over the Builtins __vec_reserve__ primitive (which
// the vector(~capacity) constructor also uses).
fun reserve(v: 't vector, capacity: int): void = __vec_reserve__(v, capacity)

// ------------------------- element push/pop -------------------------

// append one element in place. The vector must already be allocated (e.g. via
// `[]`); a growth reallocates the internal data buffer inside the shared header,
// so the caller's binding keeps pointing at the same (updated) header.
// __intrin_push__ inlines the append: for POD elements the common in-capacity,
// not-being-iterated case is a raw slot write (no call); every other case (grow,
// empty, locked, NULL) falls through to fx_vec_append. See FX_VEC_PUSH_BACK* in
// ficus.h.
@inline fun push_back(v: 't vector, elem: 't): void = __intrin_push__(v, elem)

// append x and return its index (unlike push_back / append, which are void);
// used by the code migrated off the retired Dynvec.t where the index is needed
fun push(v: 't vector, x: 't): int { push_back(v, x); size(v) - 1 }
// remove the last element and return it
fun pop(v: 't vector): 't { val r = back(v); pop_back(v); r }

// the last element. `v[.-1]` desugars to `v[__intrin_size__(v) - 1]`, so the
// bounds check (FX_VEC_CHKIDX) yields OutOfRangeError on an empty/NULL vector.
@inline fun back(v: 't vector): 't = v[.-1]

// drop the last element (freeing it if it is a complex type). __intrin_pop__
// inlines the fast path: for POD elements a present, not-being-iterated last
// slot is dropped with a bare size decrement; every other case (empty, locked,
// NULL, or a complex element that must be freed) goes through fx_vec_pop_back.
@inline fun pop_back(v: 't vector): void = __intrin_pop__(v)

// map/mapi/foldl are gone: `vector(for x <- v {..})`, `vector(for x@i <- v {..})`
// and `fold acc=init for x <- v {..}` express them directly.

// ------------------------- append / concat -------------------------

// append one element (a void synonym for push_back; the overloads below append
// all elements of an array or of another vector in one shot, copying them). The
// three argument kinds ('t / 't [] / 't vector) never overlap for a resolved
// call, so the family is unambiguous.
@inline fun append(v: 't vector, elem: 't): void = push_back(v, elem)
fun append(v: 't vector, arr: 't []): void
@ccode {
    if (!v)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    return fx_vec_append(v, arr->data, arr->dim[0].size);
}
fun append(v: 't vector, src: 't vector): void
@ccode {
    if (!v)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    return fx_vec_append(v, src ? src->data : 0, src ? src->size : 0);
}

// concatenate several vectors into one fresh vector (elements are copied). The
// result vector is created here (via `[]`, which carries the correct element
// metadata), reserved to the total size up front, then filled by bulk appends;
// so no input vector is needed to supply metadata and an empty input yields a
// real allocated empty vector.
fun concat(vs: ('t vector) []): 't vector {
    val total = fold s = 0 for v <- vs { s += v.size() }
    fold r: 't vector = vector(capacity=total) for v <- vs { r.append(v) }
}
fun concat(vs: ('t vector) vector): 't vector {
    val total = fold s = 0 for v <- vs { s += v.size() }
    fold r: 't vector = vector(capacity=total) for v <- vs { r.append(v) }
}

// ------------------------- compare / string / print -------------------------

// ==, <=>, string and print for `vector` compare/format elements generically
// (xa <=> xb, repr(x)). These used to live in Builtins.fx next to their rrbvec
// counterparts to dodge FB-025 -- a resolver self-recursion that fired when a
// generic container operator was type-checked with many <=> / string overloads
// in scope (as when Vector is auto-imported into the whole compiler). FB-025 is
// fixed (resolve-3: the under-constrained-tie fallback prefers a concrete
// candidate over a template), so they now live with their type.
operator == (a: 't vector, b: 't vector): bool =
    size(a) == size(b) && all(for xa <- a, xb <- b {xa == xb})

operator <=> (a: 't vector, b: 't vector): int
{
    var d = 0
    for xa <- a, xb <- b {
        d = xa <=> xb
        if d != 0 {break}
    }
    if d != 0 {d} else {size(a) <=> size(b)}
}

fun string(v: 't vector): string = join_embrace("[", "]", ", ", [for x <- v {repr(x)}])

fun print(v: 't vector): void
{
    print("[")
    for x@i <- v {
        if i > 0 {print(", ")}
        print_repr(x)
    }
    print("]")
}
