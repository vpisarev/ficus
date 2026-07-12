/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// fast STL-like/Python list-like vector

class 't t { v: cptr; init: 't };

fun get_vec_data(val0: 't): (int*3) {
    fun get_vec_data_(arr: 't []): (int*3)
    @ccode {
        fx_result->t0 = (int_)arr->dim[0].step;
        fx_result->t1 = (int_)(void*)arr->free_elem;
        fx_result->t2 = (int_)(void*)arr->copy_elem;
        return FX_OK;
    }
    get_vec_data_(array(1, val0))
}

fun make(size: int, val0: 't): 't Vec.t
{
    fun make_(size: int, info: (int*3), val0_: ('t, bool)): cptr
    @ccode {
        size_t elemsize = (size_t)info->t0;
        fx_free_t free_f = (fx_free_t)(void*)info->t1;
        fx_copy_t copy_f = (fx_copy_t)(void*)info->t2;
        fx_vec_t vec = 0;
        int fx_status = fx_make_vec(0, size, elemsize, free_f, copy_f, 0, &vec);
        if (fx_status < 0) {
            FX_UPDATE_BT();
            return fx_status;
        }
        fx_status = fx_vec_resize(vec, size, val0_);
        if (fx_status < 0) {
            fx_free_vec(&vec);
            FX_UPDATE_BT();
            return fx_status;
        }
        fx_status = fx_make_cptr(vec, fx_vec_destructor, fx_result);
        if (fx_status < 0) {
            fx_free_vec(&vec);
            FX_UPDATE_BT();
            return fx_status;
        }
        return fx_status;
    }
    // hack: we don't know whether val0 is passed by value or by reference.
    // we construct a tuple that is always passed in by reference.
    // possible alternative method is to use __is_scalar__;
    // but cptr is also passed by value.
    // when vector/vec is first-class type in Ficus, this trick
    // is not needed anymore, because we know element type at compile time
    val any: 't = __any_element__()
    t { v = make_(size, get_vec_data(val0), (val0, true)), init=any }
}

fun make(arr: 't []): 't Vec.t
{
    val n = arr.size()
    val v: 't Vec.t = make(n)
    for x@i <- arr {
        set(v, i, x)
    }
    v
}

fun make(size: int): 't Vec.t
{
    val any: 't = __any_element__()
    make(size, any)
}

fun empty(v: 't Vec.t): bool
@ccode {
    if(!v->v || !v->v->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    *fx_result = ((fx_vec_t)(v->v->ptr))->size == 0;
    return FX_OK;
}

fun size(v: 't Vec.t): int
@ccode {
    if(!v->v || !v->v->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    *fx_result = ((fx_vec_t)(v->v->ptr))->size;
    return FX_OK;
}

fun capacity(v: 't Vec.t): int
@ccode {
    if(!v->v || !v->v->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    *fx_result = ((fx_vec_t)(v->v->ptr))->capacity;
    return FX_OK;
}

fun clear(v: 't Vec.t): void
@ccode {
    if(!v->v || !v->v->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    fx_vec_t vec = (fx_vec_t)v->v->ptr;
    return fx_vec_resize(vec, 0, 0);
}

fun resize(v: 't Vec.t, size: int, val0: 't): void {
    fun resize_(v: 't Vec.t, size: int, val0: ('t, bool)): void
    @ccode {
        if(!v->v || !v->v->ptr)
            FX_FAST_THROW_RET(FX_EXN_NullPtrError);
        fx_vec_t vec = (fx_vec_t)v->v->ptr;
        return fx_vec_resize(vec, size, val0);
    }
    resize_(v, size, (val0, true))
}

fun assign(v: 't Vec.t, size: int, val0: 't) {
    clear(v);
    resize(v, size, val0);
}

fun reserve(v: 't Vec.t, capacity: int): void
@ccode {
    if(!v->v || !v->v->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    fx_vec_t vec = (fx_vec_t)v->v->ptr;
    return fx_vec_reserve(vec, capacity);
}

// in vec comprehensions we should not use it.
// we should construct vec of the proper size and set its elements
// without any size, capacity or index range check
fun push_back(v: 't Vec.t, elem: 't): void
{
    fun push_back_(v: 't Vec.t, elem_: ('t, bool)): void
    @ccode {
        if(!v->v || !v->v->ptr)
            FX_FAST_THROW_RET(FX_EXN_NullPtrError);
        fx_vec_t vec = (fx_vec_t)v->v->ptr;
        return fx_vec_append(vec, elem_, 1);
    }
    push_back_(v, (elem, true))
}

fun back(v: 't Vec.t): 't
@ccode {
    if(!v->v || !v->v->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    fx_vec_t vec = (fx_vec_t)v->v->ptr;
    int_ size = vec->size;
    if (size == 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);
    size_t elemsize = sizeof(*fx_result);
    const void* src = vec->data + (size - 1)*elemsize;
    // in real implementation that should be compile-time decision
    fx_copy_t copy_f = vec->info.copy_elem;
    if (!copy_f) {
        memcpy(fx_result, src, elemsize);
    } else {
        fx_copy_arr_elems(src, fx_result, 1, elemsize, copy_f);
    }
    return FX_OK;
}

fun pop_back(v: 't Vec.t, size: int): void
@ccode {
    if(!v->v || !v->v->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    fx_vec_t vec = (fx_vec_t)v->v->ptr;
    int_ size = vec->size;
    if (size == 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);
    vec->size = --size;
    fx_free_t free_f = vec->info.free_elem;
    if (free_f) {
        free_f(vec->data + size*vec->info.elemsize);
    }
    return FX_OK;
}

fun at(v: 't Vec.t, idx: int): 't
@ccode {
    if(!v->v || !v->v->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    fx_vec_t vec = (fx_vec_t)v->v->ptr;
    int_ size = vec->size;
    if (idx < 0)
        idx += size;
    if (idx < 0 || idx >= size)
        FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
    size_t elemsize = sizeof(*fx_result);
    const void* src = vec->data + idx*elemsize;
    // in real implementation that should be compile-time decision
    fx_copy_t copy_f = vec->info.copy_elem;
    if (!copy_f) {
        memcpy(fx_result, src, elemsize);
    } else {
        fx_copy_arr_elems(src, fx_result, 1, elemsize, copy_f);
    }
    return FX_OK;
}

fun set(v: 't Vec.t, idx: int, val0: 't): void
{
    fun set_(v: 't Vec.t, idx: int, val0_: ('t, bool)): void
    @ccode {
        if(!v->v || !v->v->ptr)
            FX_FAST_THROW_RET(FX_EXN_NullPtrError);
        fx_vec_t vec = (fx_vec_t)v->v->ptr;
        int_ size = vec->size;
        if (idx < 0)
            idx += size;
        if (idx < 0 || idx >= size)
            FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
        size_t elemsize = vec->info.elemsize;
        void* dst = vec->data + idx*elemsize;
        // in real implementation that should be compile-time decision
        fx_copy_t copy_f = vec->info.copy_elem;
        if (!copy_f) {
            memcpy(dst, val0_, elemsize);
        } else {
            vec->info.free_elem(dst);
            fx_copy_arr_elems(val0_, dst, 1, elemsize, copy_f);
        }
        return FX_OK;
    }
    set_(v, idx, (val0, true))
}

@private fun slice(v: 't Vec.t, start: int, end: int,
                   delta: int, mask: int): 't Vec.t
{
    fun slice_(v: 't Vec.t, start: int, end: int,
               delta: int, mask: int): cptr
    @ccode {
        if(!v->v || !v->v->ptr)
            FX_FAST_THROW_RET(FX_EXN_NullPtrError);
        fx_vec_t vec = (fx_vec_t)v->v->ptr;
        fx_vec_t dst = 0;
        int fx_status = fx_vec_slice(vec, start, end, delta, (int)mask, &dst);
        if (fx_status < 0) {
            FX_UPDATE_BT();
            return fx_status;
        }
        fx_status = fx_make_cptr(dst, fx_vec_destructor, fx_result);
        if (fx_status < 0) {
            fx_free_vec(&dst);
            FX_UPDATE_BT();
            return fx_status;
        }
        return fx_status;
    }
    t {v = slice_(v, start, end, delta, mask), init=v.init}
}

// should become v[:]
fun copy(v: 't Vec.t): 't Vec.t =
    slice(v, 0, 0, 1, 3)

// should become v[::-1]
fun rev(v: 't Vec.t): 't Vec.t =
    slice(v, 0, 0, -1, 3)

// should become v[start:end]
fun slice(v: 't Vec.t, start: int, end: int): 't Vec.t =
    slice(v, start, end, 1, 0)

// should become v[start:end:delta]
fun slice(v: 't Vec.t, start: int, end: int, delta: int): 't Vec.t =
    slice(v, start, end, delta, 0)

// should become v[start::delta]
fun slicefrom(v: 't Vec.t, start: int, delta: int): 't Vec.t =
    slice(v, start, 0, delta, 2)

fun print(v: 't Vec.t): void
{
    print("[");
    val n = size(v)
    for i <- 0:n {
        if (i > 0) { print(", ") }
        print(v.at(i))
    }
    print("]")
}

fun string(v: 't Vec.t): string
{
    val n = size(v)
    val strs = array(n, "")
    for i <- 0:n {
        strs[i] = string(v.at(i))
    }
    join_embrace("[", "]", ", ", strs)
}

// temporary substitution for real vector comprehensions
fun map(v: 't Vec.t, f: 't->'r): 'r Vec.t
{
    val n = size(v)
    // in real implementation we should not initialize the output vector
    val res = make(n, v.init)
    for i <- 0:n {
        res.set(i, f(v.at(i)))
    }
    res
}

fun mapi(v: 't Vec.t, f: ('t, int)->'r): 'r Vec.t
{
    val n = size(v)
    // in real implementation we should not initialize the output vector
    val res = make(n, v.init)
    for i <- 0:n {
        res.set(i, f(v.at(i), i))
    }
    res
}

// temporary substitution for a fold over vector
fun foldl(v: 't Vec.t, f: ('t,'r)->'r, init: 'r): 'r
{
    val n = size(v)
    fold res = init for i <- 0:n {
        res = f(v.at(i), res)
    }
}

operator == (a: 't Vec.t, b: 't Vec.t): bool
{
    val n = a.size()
    if b.size() != n { throw SizeMismatchError }
    for i <- 0:n {
        if a.at(i) != b.at(i) {return false}
    }
    return true
}

operator <=> (a: 't Vec.t, b: 't Vec.t): int
{
    val n = a.size()
    if b.size() != n { throw SizeMismatchError }
    for i <- 0:n {
        val d = a.at(i) <=> b.at(i)
        if d != 0 {return d}
    }
    return 0
}

fun array(v: 't Vec.t): 't [] {
    val n = v.size()
    [for i <- 0:n {v.at(i)}]
}
