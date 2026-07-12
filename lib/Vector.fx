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
// emits fx_make_vec with the correct metadata), and make(n,...) then resizes it.

// ------------------------- construction -------------------------

// a vector of `n` copies of val0
fun make(n: int, val0: 't): 't vector
{
    val v: 't vector = []
    resize(v, n, val0)
    v
}

// a vector with the elements of an array
fun make(arr: 't []): 't vector
{
    val n = size(arr)
    val v: 't vector = []
    reserve(v, n)
    for x <- arr { push_back(v, x) }
    v
}

// a vector of `n` default-initialized elements
fun make(n: int): 't vector
{
    val any: 't = __any_element__()
    make(n, any)
}

// ------------------------- capacity / size -------------------------

fun capacity(v: 't vector): int
@ccode {
    *fx_result = v ? v->capacity : 0;
    return FX_OK;
}

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

fun reserve(v: 't vector, capacity: int): void
@ccode {
    if (!v)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    return fx_vec_reserve(v, capacity);
}

// ------------------------- element push/pop -------------------------

// append one element in place. The vector must already be allocated (e.g. via
// `[]`); a growth reallocates the internal data buffer inside the shared header,
// so the caller's binding keeps pointing at the same (updated) header.
fun push_back(v: 't vector, elem: 't): void
{
    fun push_back_(v: 't vector, elem_: ('t, bool)): void
    @ccode {
        if (!v)
            FX_FAST_THROW_RET(FX_EXN_NullPtrError);
        return fx_vec_append(v, elem_, 1);
    }
    // the (elem, true) tuple is always passed by reference, so elem_ is a
    // pointer to the element (its first field) regardless of 't.
    push_back_(v, (elem, true))
}

// the last element
fun back(v: 't vector): 't
@ccode {
    if (!v)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    int_ size = v->size;
    if (size == 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);
    size_t elemsize = sizeof(*fx_result);
    const void* src = (const char*)v->data + (size - 1)*elemsize;
    fx_copy_t copy_f = v->info.copy_elem;
    if (!copy_f) {
        memcpy(fx_result, src, elemsize);
    } else {
        fx_copy_arr_elems(src, fx_result, 1, elemsize, copy_f);
    }
    return FX_OK;
}

// drop the last element (freeing it if it is a complex type)
fun pop_back(v: 't vector): void
@ccode {
    if (!v)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    int_ size = v->size;
    if (size == 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);
    v->size = --size;
    fx_free_t free_f = v->info.free_elem;
    if (free_f) {
        free_f((char*)v->data + size*v->info.elemsize);
    }
    return FX_OK;
}

// ------------------------- conversion / compare / print -------------------------

// TEMPORARY (until vector comprehensions, Step 4): map/mapi/foldl. Once
// `vector(for x <- v {...})` exists these become one-liners / disappear.

// map each element through f into a fresh vector
fun map(v: 't vector, f: 't -> 'r): 'r vector
{
    val n = size(v)
    val res: 'r vector = []
    reserve(res, n)
    for i <- 0:n { res.push_back(f(v[i])) }
    res
}

// map each (element, index) through f into a fresh vector
fun mapi(v: 't vector, f: ('t, int) -> 'r): 'r vector
{
    val n = size(v)
    val res: 'r vector = []
    reserve(res, n)
    for i <- 0:n { res.push_back(f(v[i], i)) }
    res
}

// left fold: res = f(v[n-1], ... f(v[1], f(v[0], init)))
fun foldl(v: 't vector, f: ('t, 'r) -> 'r, init: 'r): 'r
{
    val n = size(v)
    fold res = init for i <- 0:n { res = f(v[i], res) }
}

// NB: ==, <=>, string, print for `vector` live in Builtins.fx next to their
// rrbvec counterparts. They compare/format elements generically (a[i] <=> b[i],
// string(v[i])), and compiling that in a context with many <=> / string
// overloads (as when Vector is auto-imported into the whole compiler) trips a
// resolver internal error (FB-025); Builtins is compiled early, with few
// overloads in scope, so it resolves cleanly there.
