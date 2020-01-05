/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_ARRAY_IMPL_H__
#define __FICUS_ARRAY_IMPL_H__

int fx_arr_startiter(int narrays, fx_arr_t** arrs, char** ptrs, fx_arriter_t* it)
{
    const fx_arr_t* arr0 = arrs[0];
    int i, j, d1=0, d=arr0->ndims;
    int iterdepth = 0;

    it->ndims = d;
    it->narrays = narrays;
    it->arrs = arrs;
    it->ptrs = ptrs;
    it->idx = 0;

    if( d == 1 )
    {
        int_ size = arr0->dim[0].size;
        for( i = 1; i < narrays; i++ )
        {
            const fx_arr_t* arri = arrs[i];
            if( arri->ndims != 1 || arri->dim[0].size != size )
                return FX_UNMATCHED_SIZE_ERR;
            ptrs[i] = arri->data;
        }
        it->iterdepth = 0;
        it->nblocks = 1;
        it->blocksize = size;
        return FX_OK;
    }
    else if( d == 2 )
    {
        int_ size0 = arr0->dim[0].size;
        int_ size1 = arr0->dim[1].size;
        int flags = arr0->flags;
        for( i = 1; i < narrays; i++ )
        {
            const fx_arr_t* arri = arrs[i];
            if( arri->ndims != 2 || arri->dim[0].size != size0 || arri->dim[1].size != size1 )
                return FX_UNMATCHED_SIZE_ERR;
            flags &= arri->flags;
            ptrs[i] = arri->data;
        }
        if( FX_IS_ARR_CONTINUOUS(flags) )
        {
            it->iterdepth = 0;
            it->nblocks = 1;
            it->blocksize = size0*size1;
        }
        else
        {
            it->iterdepth = 1;
            it->nblocks = size0;
            it->blocksize = size1;
        }
        return FX_OK;
    }

    for( d1 = 0; d1 < d; d1++ )
        if( arr0->dim[d1].size > 1 )
            break;

    for( i = 0; i < narrays; i++ )
    {
        const fx_arr_t* arri = arrs[i];
        if( i > 0 )
        {
            if( arri->ndims != d ) return FX_UNMATCHED_SIZE_ERR;
            for( j = 0; j < d; j++ )
                if(arri->dim[j].size != arr0->dim[j].size)
                    return FX_UNMATCHED_SIZE_ERR;
        }

        if( !FX_IS_ARR_CONTINUOUS(arri->flags) )
        {
            for( j = d-1; j > d1; j-- )
                if( arri->dim[j].step*arri->dim[j].size < arri->dim[j-1].step )
                    break;
            if(iterdepth < j) iterdepth = j;
        }

        ptrs[i] = arri->data;
    }

    size_t size = arr0->dim[d-1].size;
    for( j = d-1; j > iterdepth; j-- )
    {
        int64_t total1 = (int64_t)size*arr0->dim[j-1].size;
        if( total1 <= 0 || total1 != (size_t)total1 )
            break;
        size = (size_t)total1;
    }

    iterdepth = j;
    if( iterdepth == d1 )
        iterdepth = 0;

    int_ nblocks = 1;
    for( j = iterdepth-1; j >= 0; j-- )
        nblocks *= arr0->dim[j].size;

    it->iterdepth = iterdepth;
    it->nblocks = nblocks;
    it->blocksize = (int_)size;
    return FX_OK;
}

void fx_arr_nextiter(fx_arriter_t* it)
{
    // this check covers the continuous case
    // (all the arrays can be processed as a single block),
    // including 1D case
    if(it->idx >= it->nblocks-1)
        return;
    ++it->idx;

    int narrays = it->narrays;
    fx_arr_t** arrs = it->arrs;
    char** ptrs = it->ptrs;
    int iterdepth = it->iterdepth;

    // this check covers non-continuous 2D case
    if( iterdepth == 1 )
    {
        int idx = it->idx;
        for( int i = 0; i < narrays; i++ )
        {
            const fx_arr_t* arri = arrs[i];
            ptrs[i] = arri->data + arri->dim[0].step*idx;
        }
    }
    else
    {
        for( int i = 0; i < narrays; i++ )
        {
            const fx_arr_t* arri = arrs[i];
            int_ idx = (int_)it->idx;
            char* data = arri->data;
            for( int j = iterdepth-1; j >= 0 && idx > 0; j-- )
            {
                int szi = arri->dim[j].size, t = idx/szi;
                data += (idx - t * szi)*arri->dim[j].step;
                idx = t;
            }
            ptrs[i] = data;
        }
    }
}

void fx_free_arr(fx_ctx_t* fx_ctx, fx_arr_t* arr)
{
    if(arr->refcount) {
        if(FX_DECREF(arr->refcount) == 1)
        {
            fx_free_elems_t free_f = arr->free_elems;
            if(free_f)
            {
                if(arr->ndims == 1)
                    free_f(fx_ctx, arr->data, arr->dim[0].size);
                else if(arr->ndims == 2)
                {
                    int size0 = arr->dim[0].size, size1 = arr->dim[1].size;
                    if(FX_IS_ARR_CONTINUOUS(arr->flags))
                    {
                        size1 *= size0; size0 = 1;
                    }
                    for( int_ i = 0; i < size0; i++ )
                        free_f(fx_ctx, arr->data + i*arr->dim[0].step, size1);
                }
                else
                {
                    fx_arriter_t it;
                    char* ptr = 0;
                    fx_arr_startiter(1, &arr, &ptr, &it);
                    for(int_ i = 0; i < it.nblocks; i++)
                    {
                        free_f(fx_ctx, ptr, it.blocksize);
                        fx_arr_nextiter(&it);
                    }
                }
            }
            fx_free(fx_ctx, arr->refcount, arr->totalsize);
        }
        arr->refcount = 0;
        arr->data = 0;
    }
}

void fx_copy_arr(const fx_arr_t* src, fx_arr_t* dst)
{
    if(src->refcount) FX_INCREF(*src->refcount);
    *dst = *src;
}

int fx_make_arr( fx_ctx_t* fx_ctx, int ndims, const int_* size, size_t elemsize,
                 fx_free_elems_t free_elems, fx_copy_elems_t copy_elems,
                 fx_arr_t* arr )
{
    if(ndims <= 0 || ndims > FX_MAX_DIMS) return FX_DIM_ERR;
    size_t netw = elemsize;
    for(int i = ndims-1; i >= 0; i--)
    {
        int szi = size[i];
        // let's allow size[i] == 0 case for now
        if(szi < 0) return FX_SIZE_ERR;
        arr->dim[i].size = szi;
        arr->dim[i].step = netw;
        netw *= szi;
    }
    size_t grossw = netw + 8;
    arr->refcount = (int*)fx_alloc(fx_ctx, grossw);
    if(!arr->refcount) return FX_OUT_OF_MEM_ERR;
    arr->flags = FX_ARR_CONTINUOUS;
    arr->ndims = ndims;
    arr->data = (char*)arr->refcount + 8;
    arr->totalsize = grossw;
    arr->free_elems = free_elems;
    arr->copy_elems = copy_elems;
    // if there is destructor for elements specified, we must clear the array.
    // otherwise, if there is an exception during the array initialization,
    // we might not be able to tell, which elements are valid and needs to
    // be destructed.
    if(free_elems && netw > 0) memset(arr->data, 0, netw);
    return FX_OK;
}

#endif
