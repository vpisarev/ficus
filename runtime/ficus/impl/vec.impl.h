/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    This is implementation of STL-like/Python list-like
    mutable contigous vector structure.
    Unlike Python, all vector elements must have the same type.
    Of course, the elements can be pointers to 'interface' and
    thus they can belong to different actual implementations of that interface.
    Or elements can be of an variant type with different options corresponding
    to different types.
*/

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef __FICUS_VEC_IMPL_H__
#define __FICUS_VEC_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif

void fx_free_vec(fx_vec_t* vec_)
{
    if (*vec_) {
        fx_vec_t vec = *vec_;
        if (FX_DECREF(vec->rc) == 1) {
            fx_free_t free_f = vec->info.free_elem;
            fx_free_arr_elems(vec->data, vec->size, (size_t)vec->info.elemsize, free_f);
            fx_free(vec->data);
            vec->size = 0;
            vec->data = 0;
            fx_free(vec);
        }
        *vec_ = 0;
    }
}

void fx_vec_destructor(void* vec_)
{
    fx_vec_t vec = vec_;
    fx_free_vec(&vec);
}

int fx_make_vec( int_ size, int_ capacity, size_t elemsize,
                 fx_free_t free_elem, fx_copy_t copy_elem,
                 const void* elems, fx_vec_t* vec_ )
{
    if (elemsize > FX_ZEROBUF_MAX_SIZE || size < 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);
    if ((copy_elem != 0) != (free_elem != 0))
        FX_FAST_THROW_RET(FX_EXN_TypeMismatchError);
    //printf("fx_make_vec: size=%zu, capacity=%zu, elemsize=%zu, free_elem=%p, copy_elem=%p\n",
    //            (size_t)size, (size_t)capacity, elemsize, free_elem, copy_elem);
    capacity = capacity < size ? size : capacity;
    size_t total = elemsize * (size_t)capacity;
    fx_vec_t vec;
    void* data = 0;
    if (total > 0u) {
        data = fx_malloc(total);
        if (!data)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }
    vec = (fx_vec_t)fx_malloc(sizeof(*vec));
    if( !vec ) {
        fx_free(data);
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }
    vec->rc = 1;
    vec->size = size;
    vec->capacity = capacity;
    vec->info.elemsize = elemsize;
    vec->info.copy_elem = copy_elem;
    vec->info.free_elem = free_elem;
    vec->data = data;
    vec->nlocks = 0;

    if(free_elem && size > 0)
        memset(vec->data, 0, (size_t)size * elemsize);
    if(elems && size > 0)
        fx_copy_arr_elems(elems, vec->data, size, elemsize, copy_elem);
    *vec_ = vec;
    return FX_OK;
}

int fx_compose_vec( size_t elemsize, fx_free_t free_elem, fx_copy_t copy_elem,
                    const int8_t* tags, const void** data, fx_vec_t* vec )
{
    FX_FAST_THROW_RET(FX_EXN_NotImplementedError);
}

int fx_vec_reserve(fx_vec_t vec, int_ new_capacity)
{
    if (vec->nlocks != 0)
        FX_FAST_THROW_RET(FX_EXN_VecModifiedError);
    if (new_capacity <= vec->capacity)
        return FX_OK;
    size_t esz = vec->info.elemsize;
    size_t total = new_capacity * esz;
    // realloc performs a pure move of the existing bytes (no copy-constructors
    // needed) and frees the old buffer; it treats vec->data==NULL (an empty
    // vector grown for the first time) as malloc. Only the first vec->size
    // elements are live; the freshly grown tail is set/filled by the caller
    // (fx_vec_resize / fx_vec_append) before use.
    void *data = fx_realloc(vec->data, total);
    if (!data)
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    vec->data = data;
    vec->capacity = new_capacity;
    return FX_OK;
}

int fx_vec_resize(fx_vec_t vec, int_ new_size, const void* fillelem)
{
    if (vec->nlocks != 0)
        FX_FAST_THROW_RET(FX_EXN_VecModifiedError);
    if (new_size < 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);
    if (new_size > vec->capacity) {
        int_ new_capacity = vec->capacity + vec->capacity/2;
        new_capacity = new_capacity >= new_size ? new_capacity : new_size;
        int fx_status = fx_vec_reserve(vec, new_capacity);
        if (fx_status < 0) {
            FX_UPDATE_BT();
            return fx_status;
        }
    }
    size_t elemsize = vec->info.elemsize;
    int_ size = vec->size;
    // make removed elements, if any, unavailable immediately
    vec->size = new_size < size ? new_size : size;
    if (new_size > size) {
        fx_set_arr_elems(fillelem, (char*)vec->data + elemsize*size,
                        new_size - size, elemsize, vec->info.copy_elem);
    } else if (new_size < size) {
        fx_free_arr_elems((char*)vec->data + elemsize*new_size,
                        size - new_size, elemsize, vec->info.free_elem);
    }
    vec->size = new_size;
    return FX_OK;
}

int fx_vec_append(fx_vec_t vec, const void* elems, int_ nelems)
{
    if (nelems == 0)
        return FX_OK;
    if (!vec)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    if (vec->nlocks != 0)
        FX_FAST_THROW_RET(FX_EXN_VecModifiedError);
    if (nelems < 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);
    size_t elemsize = vec->info.elemsize;
    int_ size = vec->size, new_size = size + nelems;
    if (vec->capacity < new_size) {
        int_ new_capacity = vec->capacity + vec->capacity/2;
        new_capacity = new_capacity >= new_size ? new_capacity : new_size;
        int fx_status = fx_vec_reserve(vec, new_capacity);
        if (fx_status < 0) {
            FX_UPDATE_BT();
            return fx_status;
        }
    }
    char* dst = (char*)vec->data + size*elemsize;
    //printf("APPEND: size=%zu, capacity=%zu, nelems=%zu, elemsize=%zu, copy_f=%p\n",
    //    (size_t)size, (size_t)vec->capacity, nelems, elemsize, vec->info.copy_elem);
    if (elems) {
        fx_copy_arr_elems(elems, dst, nelems, elemsize, vec->info.copy_elem);
    } else {
        memset(dst, 0, nelems*elemsize);
    }
    vec->size = new_size;
    return FX_OK;
}

// drop the last element (freeing it if it is a complex type). Used by the slow
// path of the __intrin_pop__ intrinsic (FX_VEC_POP_BACK / FX_VEC_POP_BACK_FAST).
int fx_vec_pop_back(fx_vec_t vec)
{
    if (!vec)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    if (vec->nlocks != 0)
        FX_FAST_THROW_RET(FX_EXN_VecModifiedError);
    int_ size = vec->size;
    if (size == 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);
    vec->size = --size;
    fx_free_t free_f = vec->info.free_elem;
    if (free_f)
        free_f((char*)vec->data + size*vec->info.elemsize);
    return FX_OK;
}

// just like in Python, we always create a copy of the slice, we don' create a 'view'
// to the existing vector. And this behavior is different from string or rrbvec,
// which are immutable structures and where creating a view is very safe.
// 0th bit of mask (mask & 1) == 1 for vec[:b:delta] slice specification where the left part is missing,
// 1st bit of mask (mask & 2) == 1 for vec[a::delta] slice specification
// mask == 3 corresponds to vec[:] (which means vector clone) or vec[::-1] (vector reverse)
int fx_vec_slice(fx_vec_t vec, int_ start, int_ end, int_ delta, int mask, fx_vec_t* subvec_)
{
    int_ size = vec->size;
    start = !(mask & 1) ? start : delta > 0 ? 0 : size-1;
    end = !(mask & 2) ? end : delta > 0 ? size : -1;
    if (delta == 0)
        FX_FAST_THROW_RET(FX_EXN_ZeroStepError);
    if ((delta > 0 && (start < 0 || start > end || end > size)) ||
        (delta < 0 && (end < -1 || start < end || start >= size)))
        FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
    size_t elemsize = vec->info.elemsize;
    fx_free_t free_f = vec->info.free_elem;
    fx_copy_t copy_f = vec->info.copy_elem;
    int_ newsize = end - start;
    if (delta != 1)
        newsize = FX_LOOP_COUNT(start, end, delta);
    int fx_status = fx_make_vec(newsize, newsize, elemsize, free_f, copy_f,
                                delta == 1 ? (char*)vec->data + start*elemsize : 0, subvec_);
    if (fx_status < 0) {
        FX_UPDATE_BT();
        return fx_status;
    }
    if (delta != 1) {
        fx_vec_t subvec = *subvec_;
        const char* src = (const char*)vec->data + start*elemsize;
        char* dst = (char*)subvec->data;
        if (!copy_f) {
            for(int_ i = 0; i < newsize; i++)
                memcpy(dst + i*elemsize, src + i*(delta*elemsize), elemsize);
        } else if (copy_f == fx_copy_ptr) {
            fx_ref_simple_t *srcp = (fx_ref_simple_t*)src, *dstp = (fx_ref_simple_t*)dst;
            for(int_ i = 0; i < newsize; i++) {
                FX_COPY_PTR(srcp[i*delta], dstp + i);
            }
        } else {
            for(int_ i = 0; i < newsize; i++)
                copy_f(src + i*(delta*elemsize), dst + i*elemsize);
        }
    }
    return FX_OK;
}

// In-place slice assignment `vec[start:end:delta] = rhs`.
//   - rhs == NULL or empty  => deletion of the selected elements (any delta);
//     the survivors are compacted and the freed/vacated slots zeroed.
//   - rhs non-empty         => contiguous replacement (delta == 1, guaranteed by
//     the compiler: a strided replace with a non-empty vector is a compile error).
//     The sizes may differ; the tail is shifted with memmove (a pure move — no
//     copy/free of the moved elements), the removed elements are freed first, and
//     any slot vacated by a shrink is zeroed so no aliased pointer lingers.
// Elements are never aliased into `vec`: rhs is deep-copied in (copy_elem).
int fx_vec_splice(fx_vec_t vec, int_ start, int_ end, int_ delta, int mask, fx_vec_t rhs)
{
    if (!vec)
        FX_FAST_THROW_RET(FX_EXN_NullPtrError);
    if (vec->nlocks != 0)
        FX_FAST_THROW_RET(FX_EXN_VecModifiedError);
    if (delta == 0)
        FX_FAST_THROW_RET(FX_EXN_ZeroStepError);
    int_ size = vec->size;
    start = !(mask & 1) ? start : delta > 0 ? 0 : size-1;
    end = !(mask & 2) ? end : delta > 0 ? size : -1;
    if ((delta > 0 && (start < 0 || start > end || end > size)) ||
        (delta < 0 && (end < -1 || start < end || start >= size)))
        FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
    size_t esz = vec->info.elemsize;
    fx_free_t free_f = vec->info.free_elem;
    fx_copy_t copy_f = vec->info.copy_elem;
    int_ nrhs = rhs ? rhs->size : 0;
    char* data;

    if (nrhs == 0) {
        // deletion (any delta): free selected elements, compact the survivors
        // down with a pure move, then zero the vacated tail.
        data = (char*)vec->data;
        int_ j = 0;
        for (int_ i = 0; i < size; i++) {
            int selected = delta > 0
                ? (i >= start && i < end && (i - start) % delta == 0)
                : (i <= start && i > end && (start - i) % (-delta) == 0);
            if (selected) {
                if (free_f) free_f(data + i*esz);
            } else {
                if (j != i) memmove(data + j*esz, data + i*esz, esz);
                j++;
            }
        }
        if (j < size)
            memset(data + j*esz, 0, (size - j)*esz);
        vec->size = j;
        return FX_OK;
    }

    // contiguous replacement (delta == 1 guaranteed by the compiler)
    int_ nremove = end - start;
    int_ newsize = size - nremove + nrhs;
    // reserve first so an OOM leaves the vector untouched
    if (newsize > vec->capacity) {
        int fx_status = fx_vec_reserve(vec, newsize);
        if (fx_status < 0) {
            FX_UPDATE_BT();
            return fx_status;
        }
    }
    data = (char*)vec->data;
    // free the elements being removed/overwritten
    if (free_f)
        fx_free_arr_elems(data + start*esz, nremove, esz, free_f);
    // shift the surviving tail to its new position (pure move, overlap-safe)
    int_ tail = size - end;
    if (tail > 0 && nrhs != nremove)
        memmove(data + (start+nrhs)*esz, data + end*esz, tail*esz);
    // deep-copy the replacement elements in (rhs keeps ownership)
    fx_copy_arr_elems(rhs->data, data + start*esz, nrhs, esz, copy_f);
    // on a shrink, zero the vacated tail so no aliased pointer lingers past size
    if (newsize < size)
        memset(data + newsize*esz, 0, (size - newsize)*esz);
    vec->size = newsize;
    return FX_OK;
}

#ifdef __cplusplus
}
#endif

#endif
