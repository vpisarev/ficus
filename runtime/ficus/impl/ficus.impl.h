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
    fx_ctx->rng.state = state;

    return fx_ctx;
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

///////////// exceptions /////////////

void fx_free_exn(fx_ctx_t* fx_ctx, fx_exn_t* exn)
{
    if(exn->data)
    {
        if(FX_DECREF(exn->data->refcount) == 1)
            exn->data->free_f(fx_ctx, exn->data);
        exn->data = 0;
    }
}

void fx_copy_exn(const fx_exn_t* src, fx_exn_t* dst)
{
    if(src->data) FX_INCREF(src->data->refcount);
    *dst = *src;
}

//////////////////// cpointers ////////////////////

void fx_free_cptr(fx_cptr_t** cptr)
{
    if(cptr && *cptr)
    {
        if((*cptr)->free_f) free_f->(*(cptr)->ptr);
        fx_free(*cptr);
        *cptr = 0;
    }
}

int fx_make_cptr(void* ptr, fx_cptr_destructor_t free_f, fx_cptr_t* fx_result)
{
    fx_cptr_t p = (fx_cptr_t)fx_alloc(sizeof(*p));
    if(!p) return FX_OUT_OF_MEM_ERR;
    p->refcount = 1;
    p->free_f = free_f;
    p->ptr = ptr;
    *fx_result = p;
    return FX_OK;
}

#endif