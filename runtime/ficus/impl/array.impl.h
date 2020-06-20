/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_ARRAY_IMPL_H__
#define __FICUS_ARRAY_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif

int fx_arr_startiter(int narrays, fx_arr_t** arrs, char** ptrs, fx_arriter_t* it)
{
    if(narrays <= 0) return FX_EXN_SizeError;
    if(!arrs || !arrs[0]) return FX_EXN_NullPtrError;

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
                return FX_EXN_SizeMismatchError;
            ptrs[i] = arri->data;
        }
        it->iterdepth = 0;
        it->nblocks = 1;
        it->blocksize = size;
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
                return FX_EXN_SizeMismatchError;
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
    }
    else
    {
        for( d1 = 0; d1 < d; d1++ )
            if( arr0->dim[d1].size > 1 )
                break;

        for( i = 0; i < narrays; i++ )
        {
            const fx_arr_t* arri = arrs[i];
            if( i > 0 )
            {
                if( arri->ndims != d ) return FX_EXN_SizeMismatchError;
                for( j = 0; j < d; j++ )
                    if(arri->dim[j].size != arr0->dim[j].size)
                        return FX_EXN_SizeMismatchError;
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
    }
    if( it->nblocks == 0 || it->blocksize == 0 )
        it->nblocks = 0;
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
        int_ idx = it->idx;
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
            int_ idx = it->idx;
            char* data = arri->data;
            for( int j = iterdepth-1; j >= 0 && idx > 0; j-- )
            {
                int_ szi = arri->dim[j].size, t = idx/szi;
                data += (idx - t * szi)*arri->dim[j].step;
                idx = t;
            }
            ptrs[i] = data;
        }
    }
}

static void fx_free_arr_elems(void* elems_, int_ nelems, size_t elemsize, fx_free_t free_f)
{
    char* elems = (char*)elems_;
    for(int_ i = 0; i < nelems; i++) free_f(elems + i*elemsize);
}

static void fx_copy_arr_elems(const void* src_, void* dst_, int_ nelems, size_t elemsize, fx_copy_t copy_f)
{
    if(!copy_f)
        memcpy(dst_, src_, nelems*elemsize);
    else if(copy_f == fx_copy_ptr) {
        fx_ref_simple_t *src = (fx_ref_simple_t*)src_, *dst = (fx_ref_simple_t*)dst_;
        for(int_ i = 0; i < nelems; i++) FX_COPY_PTR(src[i], dst+i);
    } else {
        char *src = (char*)src_, *dst = (char*)dst_;
        for(int_ i = 0; i < nelems; i++) copy_f(src + i*elemsize, dst + i*elemsize);
    }
}

void fx_free_arr(fx_arr_t* arr)
{
    if(arr->rc)
    {
        if(FX_DECREF(*arr->rc) == 1)
        {
            fx_free_t free_f = arr->free_elem;
            size_t elemsize = arr->dim[arr->ndims-1].step;
            if(free_f)
            {
                if(arr->ndims == 1)
                    fx_free_arr_elems(arr->data, arr->dim[0].size, elemsize, free_f);
                else if(arr->ndims == 2)
                {
                    int_ size0 = arr->dim[0].size, size1 = arr->dim[1].size;
                    if(FX_IS_ARR_CONTINUOUS(arr->flags))
                    {
                        size1 *= size0; size0 = 1;
                    }
                    for( int_ i = 0; i < size0; i++ )
                        fx_free_arr_elems(arr->data + i*arr->dim[0].step, size1, elemsize, free_f);
                }
                else
                {
                    fx_arriter_t it;
                    char* ptr = 0;
                    fx_arr_startiter(1, &arr, &ptr, &it);
                    for(int_ i = 0; i < it.nblocks; i++)
                    {
                        fx_free_arr_elems(ptr, it.blocksize, elemsize, free_f);
                        fx_arr_nextiter(&it);
                    }
                }
            }
            fx_free(arr->rc);
        }
        arr->rc = 0;
    }
    arr->data = 0;
}

int fx_copy_arr_data(const fx_arr_t* src, fx_arr_t* dst)
{
    fx_arr_t* arrs[] = {(fx_arr_t*)src, dst};
    char* ptrs[] = {0, 0};
    fx_arriter_t it = {};
    int fx_status = fx_arr_startiter(2, arrs, ptrs, &it);
    if( fx_status < 0 )
        return fx_status;
    int ndims = src->ndims;
    size_t elemsize = src->dim[ndims-1].step;
    if( src->copy_elem != dst->copy_elem || elemsize != dst->dim[ndims-1].step )
        return FX_EXN_TypeMismatchError;

    for( int_ i = 0; i < it.nblocks; i++ ) {
        fx_copy_arr_elems(ptrs[0], ptrs[1], it.blocksize, elemsize, src->copy_elem);
        if(i+1 < it.nblocks)
            fx_arr_nextiter(&it);
    }
    return FX_OK;
}

void fx_copy_arr(const fx_arr_t* src, fx_arr_t* dst)
{
    if(src->rc) FX_INCREF(*src->rc);
    *dst = *src;
}

int fx_make_arr( int ndims, const int_* size, size_t elemsize,
                 fx_free_t free_elem, fx_copy_t copy_elem, const void* elems,
                 fx_arr_t* arr )
{
    if (ndims <= 0 || ndims > FX_MAX_DIMS)
        return FX_EXN_DimError;
    size_t netw = elemsize;
    for(int i = ndims-1; i >= 0; i--)
    {
        int_ szi = size[i];
        if(szi < 0) return FX_EXN_SizeError;
        arr->dim[i].size = szi;
        arr->dim[i].step = netw;
        size_t netw_ = netw*szi;
        if (szi > 0 && netw_ < netw) return FX_EXN_SizeError;
        netw = netw_;
    }
    int_ total = (int_)(netw/elemsize);
    if (total < 0)
        return FX_EXN_SizeError;

    size_t dataoffset = elemsize % 8 == 0 ? (size_t)8 : sizeof(*arr->rc);
    size_t grossw = netw + dataoffset;
    if (netw > 0) {
        arr->rc = (int_*)fx_malloc(grossw);
        if( !arr->rc )
            return FX_EXN_OutOfMemError;
        *arr->rc = 1;
        arr->data = (char*)arr->rc + dataoffset;

        // if there is destructor for elements specified, we must clear the array.
        // otherwise, if there is an exception during further array initialization,
        // we might not be able to tell, which elements are valid and needs to
        // be destructed.
        if(free_elem)
            memset(arr->data, 0, netw);
        if(elems)
            fx_copy_arr_elems(elems, arr->data, total, elemsize, copy_elem);
    }
    arr->flags = FX_ARR_CONTINUOUS;
    arr->ndims = ndims;
    arr->free_elem = free_elem;
    arr->copy_elem = copy_elem;

    return FX_OK;
}

int fx_subarr(const fx_arr_t* arr, const int_* ranges, fx_arr_t* subarr)
{
    int i, ndims = arr->ndims;
    size_t total = 1, offset = 0;
    int state = 0;

    for( i = 0; i < ndims; i++ )
    {
        int_ size_i = arr->dim[i].size;
        size_t step_i = arr->dim[i].step;

        int_ tag = ranges[0];
        int_ a, b, delta;
        if( tag == 0 )
        {
            a = ranges[1];
            b = a+1;
            delta = 1;
            ranges += 2;
        }
        else if( tag == 1 )
        {
            a = ranges[1];
            b = ranges[2];
            delta = ranges[3];
            ranges += 4;
        }
        else if( tag == 2 )
        {
            a = ranges[1];
            b = size_i;
            delta = ranges[2];
            ranges += 3;
        }
        else
            return FX_EXN_SizeError;
        if( delta <= 0 || a > b )
            return FX_EXN_SizeError;
        if( i == ndims-1 && delta != 1)
            return FX_EXN_SizeError;
        if( a < 0 || b > size_i )
            return FX_EXN_OutOfRangeError;

        // a little state machine:
        //    the subarray is continuous
        //    iff zero or more leading dimensions
        //    are 1's and then there is at most one
        //    "non-full range" dimension immediately after them.
        // that is, if we denote a dimension with size 1 as I,
        // full range dimension as F and all other dimensions as D,
        // then the "regular expression" for continuous subarray of continuous array
        // will be I*D?F* (zero or more 1-dimensions, then at most one non-full range,
        // and then zero or more full ranges):
        // state 0: all 1/I's so far
        // state 1: D or F occured
        // state 3: D occured
        // state 7: I or D occured after D or F -> the subarray is non-continuous
        if( state != 0 && b - a < size_i )
        {
            if( state == 3 )
                state = 7;
            state |= 2;
        }

        offset += a*step_i;
        size_i = (b - a + delta - 1)/delta;
        total *= (size_t)size_i;
        subarr->dim[i].size = size_i;
        subarr->dim[i].step = delta*step_i;
    }

    subarr->free_elem = arr->free_elem;
    subarr->copy_elem = arr->copy_elem;
    subarr->flags = arr->flags & (state == 7 ? ~FX_ARR_CONTINUOUS : -1);
    subarr->ndims = arr->ndims;

    if (total > 0) {
        subarr->rc = arr->rc;
        if (subarr->rc)
            FX_INCREF(*subarr->rc);
        subarr->data = arr->data + offset;
    } else {
        subarr->rc = 0;
        subarr->data = 0;
    }

    return FX_OK;
}

#ifdef __cplusplus
}
#endif

#endif
