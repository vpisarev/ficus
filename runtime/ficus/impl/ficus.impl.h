/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_IMPL_H__
#define __FICUS_IMPL_H__

#include "ficus/ficus.h"

FX_THREAD_LOCAL fx_exn_t fx_exn;
FX_THREAD_LOCAL fx_rng_t fx_rng;

void fx_init(int t_idx)
{
    uint64_t state = (uint64_t)-1;
    for(int i = 0; i < t_idx*2 + 10; i++)
        state = (uint64_t)(unsigned)state*4187999619U + (unsigned)(state >> 32);
    fx_rng.state = state;
}

/* [TODO] replace it with something more efficient,
   e.g. mimalloc (https://github.com/microsoft/mimalloc) */
void* fx_alloc(size_t sz)
{
    return malloc(sz);
}

void fx_free(void* ptr)
{
    free(ptr);
}

/////////////// list ////////////////

typedef struct fx_list_simple_cell_t
{
    fx_rc_t rc;
    struct fx_list_simple_cell_t* tl;
} fx_list_simple_cell_t;

void fx_free_list_simple(void* pl)
{
    fx_list_simple_cell_t **pl_ = (fx_list_simple_cell_t**pl), l = *pl_;
    while(l) {
        if(FX_DECREF(l->rc) > 1)
            break;
        fx_list_simple_cell_t* tl = (fx_list_simple_cell_t*)l->tl;
        fx_free(l);
        l = tl;
    }
    *pl_ = 0;
}

////// reference-counted cells //////

void fx_free_ptr(void* pp)
{
    fx_rc_t** pp_ = (fx_rc_t**)pp;
    if(*pp_ && FX_DECREF(**pp_) == 1)
        fx_free(*pp_);
    *pp_ = 0;
}

void fx_copy_ptr(const void* src, void* dst)
{
    fx_rc_t* src_ = (fx_rc_t*)src;
    fx_rc_t** dst_ = (fx_rc_t**)dst;
    if(src_) FX_INCREF(*src_);
    *dst_ = src;
}

///////////// exceptions /////////////

void fx_free_exn(fx_exn_t* exn)
{
    if(exn->data)
    {
        if(FX_DECREF(exn->data->rc) == 1)
            exn->data->free_f(exn->data);
        exn->data = 0;
    }
}

void fx_copy_exn(const fx_exn_t* src, fx_exn_t* dst)
{
    if(src->data) FX_INCREF(src->data->rc);
    *dst = *src;
}

//////////////// function pointers ////////////////

void fx_free_fp(void* fp)
{
    fx_fp_t* fp_ = (fx_fp_t*)fp;
    FX_FREE_FP(fp_)
}

void fx_copy_fp(const void* src, void* pdst)
{
    fx_fp_t *src_ = (fx_fp_t*)src, **pdst_ = (fx_fp_t**)pdst;
    FX_COPY_FP(src_, *pdst_);
}

//////////////////// cpointers ////////////////////

void fx_cptr_no_free(void* ptr) {}

void fx_free_cptr(fx_cptr_t* cptr)
{
    if(*cptr && FX_DECREF((*cptr)->rc) == 1)
    {
        free_f(*(cptr)->ptr);
        fx_free(*cptr);
        *cptr = 0;
    }
}

void fx_copy_cptr(const fx_cptr_t* src, fx_cptr_t* dst)
{
    if(*src) FX_INCREF((*src)->rc);
    *dst = *src;
}

int fx_make_cptr(void* ptr, fx_free_t free_f, fx_cptr_t* fx_result)
{
    fx_cptr_t p = (fx_cptr_t)fx_alloc(sizeof(*p));
    if(!p) return FX_OUT_OF_MEM_ERR;
    p->rc = 1;
    p->free_f = free_f;
    p->ptr = ptr;
    *fx_result = p;
    return FX_OK;
}

#endif