/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_H__
#define __FICUS_H__

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ficus/version.h"

//////////////////////// Error Codes //////////////////////
enum
{
    FX_OK = 0,
    FX_OUT_OF_MEM_ERR = -1,
    FX_OUT_OF_RANGE_ERR = -2,
    FX_DIV_BY_ZERO_ERR = -3,
    FX_UNMATCHED_SIZE_ERR = -4,
    FX_DIM_ERR = -5,
    FX_SIZE_ERR = -6
};

/////////////////// Various Basic Definitions ////////////////
#define FX_INLINE __inline

typedef intptr_t int_; // int size in ficus is equal to the pointer size
typedef int char_; // 4-byte unicode character

#ifndef FX_XADD
#ifdef _MSC_VER
#include <intrin.h>
#define FX_XADD(addr, delta) (int)_InterlockedExchangeAdd((long volatile*)addr, delta)
#elif defined __ATOMIC_ACQ_REL
#define FX_XADD(addr, delta) __c11_atomic_fetch_add((_Atomic(int)*)(addr), delta, __ATOMIC_ACQ_REL)
#else
#define FX_XADD(addr, delta) __atomic_fetch_add((_Atomic(int)*)(addr), delta, 4)
#endif
#endif

#define FX_INCREF(refcnt) FX_XADD(&(refcnt), 1)
#define FX_DECREF(refcnt) FX_XADD(&(refcnt), -1)
typedef int fx_refcount_t;

#ifdef _MSC_VER
#define FX_THREAD_LOCAL __declspec(thread)
#else
#define FX_THREAD_LOCAL __thread
#endif

typedef struct fx_rng_t
{
    uint64_t state;
} fx_rng_t;

struct fx_ctx_t;
struct fx_exndata_t;

typedef struct fx_exndata_t
{
    fx_refcount_t refcount;
    void (*free_f)(struct fx_exndata_t* data);

} fx_exndata_t;

typedef struct fx_exn_t
{
    int tag;
    struct fx_exndata_t* data;
} fx_exn_t;

extern FX_THREAD_LOCAL fx_exn_t fx_exn;
extern FX_THREAD_LOCAL fx_rng_t fx_rng;

void fx_init(int t_idx);

void* fx_alloc(size_t sz);
void fx_free(void* ptr);

#define FX_COPY_PTR(src, dst) FX_INCREF(src->refcount); *(dst) = (src)
#define FX_COPY_SIMPLE(src, dst) *(dst) = (src)
#define FX_NO_FREE(ctx, v)

////////////////////////// Strings //////////////////////
typedef struct fx_str_t
{
    int* refcount;
    size_t total;
    char_* data;
    int_ length;
} fx_str_t;

void fx_free_str(fx_str_t* str);
void fx_copy_str(const fx_str_t* src, fx_str_t* dst);
int fx_make_str(fx_str_t* str, char_* strdata, int_ length);

////////////////////////// Exceptions //////////////////////

#define FX_THROW_LIGHT(exn_name, catch_label) \
    fx_status = exn_name; goto catch_label

#define FX_FREE_EXN(exn) \
    if((exn).data != 0) fx_free_exn(&(exn)) else

void fx_free_exn(fx_exn_t* exn);
void fx_copy_exn(const fx_exn_t* src, fx_exn_t* dst);

#define FX_FREE_EXN(exn) if(!(exn)->data) ; else fx_free_exn(exn)
#define FX_COPY_EXN(src, dst) if(!(src)->data) *(dst)=*(src) else fx_copy_exn((src), (dst))

#define FX_EXN_MAKE_IMPL(exn_tag, exndata_typ, exndata_free, arg_copy) \
    exndata_typ* data = (exndata_typ*)fx_alloc(sizeof(*data)); \
    if(!data) return FX_OUT_OF_MEM_ERR; \
        \
    data->base.refcount = 1; \
    data->base.free_f = exndata_free; \
    arg_copy; \
        \
    fx_result->tag = exn_tag; \
    fx_result->data = (fx_exndata_t*)data; \
    return fx_result->tag

//////////////////////////// Lists /////////////////////////

#define FX_LIST_FREE_IMPL(typ, hd_free_f) \
    typ l = *pl; \
    while(l) { \
        if(FX_DECREF(l->refcount) > 1) \
            break; \
        typ tl = (typ)l->tl; \
        hd_free_f(&l->hd); \
        fx_free(l); \
        l = tl; \
    } \
    *pl = 0

#define FX_LIST_MAKE_IMPL(typ, hd_copy_f) \
    typ l = (typ)fx_alloc(sizeof(*l)); \
    if (!l) return FX_OUT_OF_MEM_ERR; \
    l->refcount = 1; \
    l->tl = tl; \
    if(tl) FX_INCREF(tl->refcount); \
    hd_copy_f(hd, &l->hd); \
    *fx_result = l; \
    return FX_OK

//////////////////////////// Arrays /////////////////////////

typedef void (*fx_free_elems_t)(const char* elems, int_ count);
typedef void (*fx_copy_elems_t)(const char* src, char* dst, int_ count);

#define FX_MAX_DIMS 5
#define FX_ARR_CONTINUOUS 1
#define FX_IS_ARR_CONTINUOUS(flags) ((flags) & FX_ARR_CONTINUOUS)

typedef struct fx_arrdim_t
{
    int_ size;
    size_t step;
} fx_arrdim_t;

typedef struct fx_arr_t
{
    int* refcount;
    int flags;
    int ndims;
    // put 'data' together with the interleaved '(size, step)' pairs
    // in order to improve the cache locality, e.g. in the case of
    // 2D array element access we just need to read 4
    // sequentially stored elements:
    // data, dim[0].size, dim[0].step, dim[1].size
    char*  data;
    fx_arrdim_t dim[FX_MAX_DIMS];
    fx_free_elems_t free_elems;
    fx_copy_elems_t copy_elems;
}
fx_arr_t;

typedef struct fx_arriter_t
{
    int ndims;
    int narrays;
    int_ nblocks;
    int_ blocksize;
    fx_arr_t** arrs;
    char** ptrs;
    int iterdepth;
    int_ idx;
} fx_arriter_t;

int fx_arr_startiter(int narrays, fx_arr_t** arrs, char** ptrs, fx_arriter_t* it);
void fx_arr_nextiter(fx_arriter_t* it);

#define FX_CHKIDX_1D(arr, idx, catch_label) \
    if((size_t)(idx) >= (size_t)(arr).dim[0].size) \
    { fx_status = FX_OUT_OF_RANGE_ERR; goto catch_label } \
    else
#define FX_CHKIDX_2D(arr, idx0, idx1, catch_label) \
    if((size_t)(idx0) >= (size_t)(arr).dim[0].size && \
       (size_t)(idx1) >= (size_t)(arr).dim[1].size) \
    { fx_status = FX_OUT_OF_RANGE_ERR; goto catch_label } \
    else
#define FX_CHKIDX_3D(arr, idx0, idx1, idx2, catch_label) \
    if((size_t)(idx0) >= (size_t)(arr).dim[0].size && \
       (size_t)(idx1) >= (size_t)(arr).dim[1].size && \
       (size_t)(idx2) >= (size_t)(arr).dim[2].size) \
    { fx_status = FX_OUT_OF_RANGE_ERR; goto catch_label } \
    else
#define FX_EPTR_1D_(typ, arr, idx) \
    (typ*)(arr).data + (idx)
#define FX_EPTR_2D_(typ, arr, idx0, idx1) \
    (typ*)((arr).data + (arr).dim[0].step*(idx0)) + (idx1)
#define FX_EPTR_3D_(typ, arr, idx0, idx1) \
    (typ*)((arr).data + (arr).dim[0].step*(idx0) + (arr).dim[1].step*(idx1)) + (idx2)
#define FX_EPTR_1D(typ, arr, idx) \
    FX_CHKIDX_1D((arr), (idx)); \
    FX_EPTR_1D_(typ, (arr), (idx))
#define FX_EPTR_2D(typ, arr, idx0, idx1) \
    FX_CHKIDX_2D((arr), (idx0), (idx1)); \
    FX_EPTR_2D_(typ, (arr), (idx0), (idx1))
#define FX_EPTR_3D(typ, arr, idx0, idx1, idx2) \
    FX_CHKIDX_3D((arr), (idx0), (idx1), (idx2)); \
    FX_EPTR_3D_(typ, (arr), (idx0), (idx1), (idx2))

void fx_free_arr(fx_arr_t* arr);
void fx_copy_arr(const fx_arr_t* src, fx_arr_t* dst);
int fx_make_arr( int ndims, const int_* size, size_t elemsize,
                 fx_free_elems_t free_elems, fx_copy_elems_t copy_elems,
                 fx_arr_t* arr );

////////////////////////// References //////////////////////////

#define FX_REF_FREE_IMPL(typ, arg_free_f) \
    typ r = *pr; \
    if(r && FX_DECREF(r->refcount) == 1) \
    { \
        arg_free_f(&r->data); \
        fx_free(r); \
    } \
    *pr = 0

#define FX_REF_MAKE_IMPL(typ, arg_copy_f) \
    typ r = (typ)fx_alloc(sizeof(*r)); \
    if (!r) return FX_OUT_OF_MEM_ERR; \
    r->refcount = 1; \
    arg_copy_f(arg, &r->data); \
    *fx_result = r; \
    return FX_OK

//////////////////////// Function pointers /////////////////////////

#define FX_FREE_FP(f) \
    if(f.fv) { f.fv->free_f(f.fv); f.fv=0; } else
#define FX_COPY_FP(src, dst) \
    if((src).fv) FX_INCREF((src).fv->base.refcount); *(dst) = (src)

///////////////////////////// C pointers ///////////////////////////

typedef void (*fx_cptr_destructor_t)(void*);

typedef struct fx_cptr_cell_t
{
    fx_refcount_t refcount;
    fx_cptr_destructor_t free_f;
    void* ptr;
} fx_cptr_cell_t, *fx_cptr_t;

void fx_free_cptr(fx_cptr_t** cptr);
int fx_make_cptr(void* ptr, fx_cptr_destructor_t free_f, fx_cptr_t* fx_result);

#define FX_FREE_CPTR(p) \
    if(p && FX_DECREF(p->refcount) == 1) fx_free_cptr(&p) else

#endif
