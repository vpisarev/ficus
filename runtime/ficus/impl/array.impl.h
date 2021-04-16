/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_ARRAY_IMPL_H__
#define __FICUS_ARRAY_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif

const char fx_zerobuf[FX_ZEROBUF_MAX_SIZE] = {'\0'};

int fx_arr_startiter(int narrays, fx_arr_t** arrs, char** ptrs, fx_arriter_t* it, bool reverse)
{
    if(narrays <= 0) FX_FAST_THROW_RET(FX_EXN_SizeError);
    if(!arrs || !arrs[0]) FX_FAST_THROW_RET(FX_EXN_NullPtrError);

    const fx_arr_t* arr0 = arrs[0];
    int i, j, d1=0, d=arr0->ndims;
    int iterdepth = 0;

    it->ndims = d;
    it->narrays = narrays;
    it->arrs = arrs;
    it->ptrs = ptrs;
    it->idx = 0;
    it->ptrs[0] = arr0->data;
    it->reverse = reverse;

    if( d == 1 ) {
        int_ size = arr0->dim[0].size;
        for( i = 1; i < narrays; i++ ) {
            const fx_arr_t* arri = arrs[i];
            if( arri->ndims != 1 || arri->dim[0].size != size )
                FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);
            ptrs[i] = arri->data;
        }
        it->iterdepth = 0;
        it->nblocks = 1;
        it->blocksize = size;
    } else if( d == 2 ) {
        int_ size0 = arr0->dim[0].size;
        int_ size1 = arr0->dim[1].size;
        int flags = arr0->flags;
        for( i = 1; i < narrays; i++ ) {
            const fx_arr_t* arri = arrs[i];
            if( arri->ndims != 2 || arri->dim[0].size != size0 || arri->dim[1].size != size1 )
                FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);
            flags &= arri->flags;
            ptrs[i] = arri->data;
        }
        if( FX_IS_ARR_CONTINUOUS(flags) ) {
            it->iterdepth = 0;
            it->nblocks = 1;
            it->blocksize = size0*size1;
        } else {
            it->iterdepth = 1;
            it->nblocks = size0;
            it->blocksize = size1;
        }
    } else {
        for( d1 = 0; d1 < d; d1++ )
            if( arr0->dim[d1].size > 1 )
                break;

        for( i = 0; i < narrays; i++ ) {
            const fx_arr_t* arri = arrs[i];
            if( i > 0 ) {
                if( arri->ndims != d ) FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);
                for( j = 0; j < d; j++ )
                    if(arri->dim[j].size != arr0->dim[j].size)
                        FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);
            }

            if( !FX_IS_ARR_CONTINUOUS(arri->flags) ) {
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
    if (reverse && it->nblocks > 1)
        fx_arr_nextiter(it);

    return FX_OK;
}

void fx_arr_nextiter(fx_arriter_t* it)
{
    if (it->nblocks <= 1)
        return;
    bool reverse = it->reverse;
    it->idx = (it->idx + (reverse ? -1 : 1) + it->nblocks) % it->nblocks;

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
                    fx_arr_startiter(1, &arr, &ptr, &it, false);
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

int fx_copy_arr_data(const fx_arr_t* src, fx_arr_t* dst, bool free_dst)
{
    int ndims = src->ndims;
    const char* srcdata = src->data;
    char* dstdata = dst->data;
    size_t elemsize = src->dim[ndims-1].step;
    size_t src_range = src->dim[0].size*src->dim[0].step;
    size_t dst_range = dst->dim[0].size*dst->dim[0].step;
    fx_copy_t copy_elem = src->copy_elem;
    fx_free_t free_elem = src->free_elem;
    if( ndims != dst->ndims || elemsize != dst->dim[ndims-1].step ||
        copy_elem != dst->copy_elem || free_elem != dst->free_elem )
        FX_FAST_THROW_RET(FX_EXN_TypeMismatchError);
    if( ndims == 0 )
        return FX_OK;

    // fast path: non-overlapping copy of simple elements, 1D or 2D case
    if (ndims <= 2 && !copy_elem && (srcdata + src_range <= dstdata || dstdata + dst_range <= srcdata) )
    {
        int_ nrows = ndims == 1 ? 1 : src->dim[0].size, ncols = src->dim[ndims-1].size;
        size_t srcstep = src->dim[0].step, dststep = dst->dim[0].step;

        if ((ndims > 1 && nrows != dst->dim[0].size) || ncols != dst->dim[ndims-1].size)
            FX_FAST_THROW_RET(FX_EXN_TypeMismatchError);
        if (FX_IS_ARR_CONTINUOUS(src->flags & dst->flags)) {
            ncols *= nrows;
            nrows = 1;
        }
        if (ncols > 0) {
            for (int_ i = 0; i < nrows; i++ )
                memcpy(dstdata + i*dststep, srcdata + i*srcstep, ncols*elemsize);
        }
        return FX_OK;
    }

    fx_arr_t* arrs[] = {(fx_arr_t*)src, dst};
    char* ptrs[] = {0, 0};
    fx_arriter_t it = {};
    bool reverse = srcdata < dstdata && srcdata + src_range > dstdata;
    int fx_status = fx_arr_startiter(2, arrs, ptrs, &it, reverse);
    size_t raw_blocksize = it.blocksize*elemsize;
    if( fx_status < 0 || raw_blocksize == 0 || it.nblocks == 0 )
        return fx_status;

    if (!copy_elem)
        free_dst = false;

    for( int_ i = 0; i < it.nblocks; i++ ) {
        char* p0 = ptrs[0], *p1 = ptrs[1];
        if(i+1 < it.nblocks)
            fx_arr_nextiter(&it);
        if (p0 == p1) continue;
        if (p0 + raw_blocksize <= p1 || p1 + raw_blocksize <= p0) {
            if (free_dst)
                fx_free_arr_elems(p1, it.blocksize, elemsize, free_elem);
            fx_copy_arr_elems(p0, p1, it.blocksize, elemsize, copy_elem);
        } else if (!copy_elem) {
            memmove(p1, p0, raw_blocksize);
        } else {
            int_ delta = (int_)elemsize;
            if(p0 < p1) {
                p0 += raw_blocksize - elemsize;
                p1 += raw_blocksize - elemsize;
                delta = -delta;
            }
            if (copy_elem == fx_copy_ptr) {
                for(int_ j = 0; j < it.blocksize; j++, p0 += delta, p1 += delta) {
                    fx_ref_simple_t *p0_ = (fx_ref_simple_t*)p0;
                    fx_ref_simple_t *p1_ = (fx_ref_simple_t*)p1;
                    if(free_dst)
                        free_elem(p1_);
                    FX_COPY_PTR(*p0_, p1_);
                }
            } else {
                for(int_ j = 0; j < it.blocksize; j++, p0 += delta, p1 += delta) {
                    if(free_dst)
                        free_elem(p1);
                    copy_elem(p0, p1);
                }
            }
        }
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
        FX_FAST_THROW_RET(FX_EXN_DimError);
    if ((copy_elem != 0) != (free_elem != 0))
        FX_FAST_THROW_RET(FX_EXN_TypeMismatchError);
    size_t netw = elemsize;
    for(int i = ndims-1; i >= 0; i--)
    {
        int_ szi = size ? size[i] : 0;
        if(szi < 0) FX_FAST_THROW_RET(FX_EXN_SizeError);
        arr->dim[i].size = szi;
        arr->dim[i].step = netw;
        size_t netw_ = netw*szi;
        if (szi > 0 && netw_ < netw) FX_FAST_THROW_RET(FX_EXN_SizeError);
        netw = netw_;
    }
    int_ total = (int_)(netw/elemsize);
    if (total < 0)
        FX_FAST_THROW_RET(FX_EXN_SizeError);

    size_t dataoffset = elemsize % 8 == 0 ? (size_t)8 : sizeof(*arr->rc);
    size_t grossw = netw + dataoffset;
    if (netw > 0) {
        arr->rc = (int_*)fx_malloc(grossw);
        if( !arr->rc )
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
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

int fx_flatten_arr(const fx_arr_t* arr, fx_arr_t* result)
{
    int fx_status = FX_OK;
    int_ arrsize[FX_MAX_DIMS];
    int i, ndims = arr->ndims;
    size_t elemsize = arr->dim[ndims-1].step;
    int_ total = 1;
    for(i=0; i < ndims; i++) {
        int_ szi = arr->dim[i].size;
        arrsize[i] = szi;
        total *= szi;
    }

    if(FX_IS_ARR_CONTINUOUS(arr->flags) || total == 0) {
        *result = *arr;
        if(*result->rc) FX_INCREF(*result->rc);
    }
    else {
        fx_status = fx_make_arr(ndims, arrsize, elemsize,
            arr->free_elem, arr->copy_elem, 0, result);
        if(fx_status >= 0) {
            assert(FX_IS_ARR_CONTINUOUS(result->flags));
            fx_status = fx_copy_arr_data(arr, result, false);
        }
    }
    if( fx_status >= 0 ) {
        result->ndims = 1;
        result->dim[0].size = total;
        result->dim[0].step = elemsize;
    }
    return fx_status;
}

/* alternative and probably easier-to-use method to iterate over nD arrays;
   it does not optimize 1D, 2D or continuous nD cases */
typedef struct fx_arriter_pos_t
{
    char* ptr;
    int_ pos, size;
    size_t step;
} fx_arriter_pos_t;

FX_INLINE bool fx_next_slice(fx_arriter_pos_t* stack, int ndims)
{
    fx_arriter_pos_t* s = &stack[ndims-1];
    int i;
    if (++s->pos < s->size) {
        s->ptr += s->step;
        return true;
    }
    for (i = ndims-2; i >= 0 && ++stack[i].pos >= stack[i].size; i--)
        ;
    if (i < 0)
        return false;
    stack[i].ptr += stack[i].step;
    for (++i; i < ndims; i++) {
        stack[i].ptr = stack[i-1].ptr;
        stack[i].pos = 0;
    }
    return true;
}

int fx_subarr(const fx_arr_t* arr, const int_* ranges, fx_arr_t* subarr)
{
    int fx_status = 0;
    int k = 0, i, ndims = arr->ndims;
    size_t total = 1, offset = 0;
    int state = 0;
    int nranges = 0;
    bool need_copy = false;
    fx_arr_t temp;
    int_ temp_size[FX_MAX_DIMS];
    size_t elemsize = arr->dim[ndims-1].step;
    fx_arriter_pos_t stack[FX_MAX_DIMS];
    int stacksize;
    int_ nelems;
    char* dstptr;

    for( i = 0; i < ndims; i++ )
    {
        int_ size_i = arr->dim[i].size;
        size_t step_i = arr->dim[i].step;
        bool scalar_range = false;

        int_ tag = ranges[0];
        int_ a, b, delta;
        if( tag == 0 )
        {
            a = ranges[1];
            b = a+1;
            delta = 1;
            ranges += 2;
            scalar_range = nranges == 0;
        }
        else if( tag == 1 )
        {
            a = ranges[1];
            b = ranges[2];
            delta = ranges[3];
            ranges += 4;
            nranges++;
        }
        else if( tag == 2 )
        {
            a = ranges[1];
            b = size_i;
            delta = ranges[2];
            if (delta < 0 && a == 0) {
                a = size_i-1;
                b = -1;
            }
            ranges += 3;
            nranges++;
        }
        else
            FX_FAST_THROW_RET(FX_EXN_RangeError);
        if( delta == 0 )
            FX_FAST_THROW_RET(FX_EXN_ZeroStepError);
        if( (delta > 0 && (a < 0 || b > size_i)) ||
            (delta < 0 && (b < -1 || a >= size_i)) )
            FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);

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
        // state 1: D or F occured once, followed by zero or more F's
        // state 2: I or D occured after D or F -> the subarray is non-continuous
        int_ subsize_i = b - a;
        if (state == 0 && subsize_i == 1)
            ; // all 1's/I's so far
        else if (subsize_i < size_i)
            state = state == 0 ? 1 : 2;
        else if (state == 0)
            state = 1;

        size_i = FX_LOOP_COUNT(a, b, delta);
        if( (i == ndims-1 && delta != 1) || delta < 0 )
            need_copy = true;

        total *= (size_t)size_i;
        offset += a*step_i;

        if( scalar_range )
            continue;
        subarr->dim[k].size = temp_size[k] = size_i;
        subarr->dim[k].step = delta*step_i;
        k++;
    }

    subarr->ndims = k;
    subarr->flags = arr->flags & (need_copy || state > 1 ? ~FX_ARR_CONTINUOUS : -1);
    subarr->free_elem = arr->free_elem;
    subarr->copy_elem = arr->copy_elem;

    if (total == 0 && subarr->ndims == 0) {
        subarr->rc = 0;
        subarr->data = 0;
        return FX_OK;
    }

    subarr->data = arr->data + offset;

    if (!need_copy) {
        subarr->rc = arr->rc;
        if (subarr->rc)
            FX_INCREF(*subarr->rc);
        return FX_OK;
    }

    // make a copy of the constructed "in-place subarray" header; then allocate fresh subarray
    temp = *subarr;
    fx_status = fx_make_arr(subarr->ndims, temp_size, elemsize, arr->free_elem, arr->copy_elem, 0, subarr);
    if (fx_status < 0)
        return fx_status;

    // initialize the stack
    for (k = 0; k < temp.ndims; k++) {
        fx_arriter_pos_t* s = &stack[k];
        s->ptr = temp.data;
        s->pos = 0;
        s->size = temp.dim[k].size;
        s->step = temp.dim[k].step;
    }

    stacksize = temp.ndims-1;
    nelems = temp.dim[stacksize].size;
    if (temp.dim[stacksize].step != elemsize)
    {
        stacksize++;
        nelems = 1;
    }
    dstptr = subarr->data;

    do {
        fx_arriter_pos_t* top = &stack[stacksize-1];
        fx_copy_arr_elems(top->ptr, dstptr, nelems, elemsize, arr->copy_elem);
        dstptr += nelems*elemsize;
    } while (fx_next_slice(stack, stacksize));

    return FX_OK;
}

int fx_compose_arr( int ndims, size_t elemsize, fx_free_t free_elem, fx_copy_t copy_elem,
                    const int8_t* tags, const void** data, fx_arr_t* arr )
{
    int_ nrows = 0, ncols = -1, nrows_i = -1, ncols_i = 0;
    //printf("ndims=%d, copy_elem=%p, free_elem=%p\n", ndims, copy_elem, free_elem);

    if (ndims <= 0 || ndims > 2)
        FX_FAST_THROW_RET(FX_EXN_DimError);
    if ((copy_elem != 0) != (free_elem != 0))
        FX_FAST_THROW_RET(FX_EXN_TypeMismatchError);

    const void** dataptr = data;
    for( int k = 0;; k++ )
    {
        int tag = tags[k];
        if (tag == 127 || tag < 0) {
            if (ncols >= 0 && ncols_i != ncols) {
                printf("throwing FX_EXN_SizeMismatchError\n");
                FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);
            }
            nrows += nrows_i;
            ncols = ncols_i;
            nrows_i = -1;
            ncols_i = 0;
            if (tag < 0)
                break;
            continue;
        }

        int_ nrows_k = 1, ncols_k = 1;
        switch (tag)
        {
        case 0:
            ++dataptr;
            break;
        case 1: case 2: {
            const fx_arr_t* arr_k = (const fx_arr_t*)*dataptr++;
            if (tag == 1)
                ncols_k = arr_k->dim[0].size;
            else {
                nrows_k = arr_k->dim[0].size;
                ncols_k = arr_k->dim[1].size;
            }}
            break;
        case 100:
            ncols_k = fx_list_length(*dataptr++);
            break;
        default:
            FX_FAST_THROW_RET(FX_EXN_NoMatchError);
        }

        if (nrows_i >= 0 && nrows_i != nrows_k)
            FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);
        nrows_i = nrows_k;
        ncols_i += ncols_k;
    }

    if( ndims == 1 && nrows != 1 )
        FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);

    int_ size[] = {(ndims == 1 ? ncols : nrows), ncols};
    int fx_status = fx_make_arr( ndims, size, elemsize, free_elem, copy_elem, 0, arr);
    if (fx_status < 0)
        return fx_status;

    nrows = 0;
    dataptr = data;
    char* dstdata = arr->data;
    size_t dststep = arr->dim[0].step;
    for( int k = 0;; k++ )
    {
        int tag = tags[k];
        if (tag == 127 || tag < 0) {
            nrows += nrows_i;
            nrows_i = 0;
            dstdata = arr->data + arr->dim[0].step*nrows;
            if (tag < 0)
                break;
            continue;
        }

        int_ nrows_k = 1;
        switch (tag)
        {
        case 0:
            if (!copy_elem)
                memcpy(dstdata, *dataptr, elemsize);
            else if(copy_elem != fx_copy_ptr)
                copy_elem(*dataptr, dstdata);
            else {
                fx_ref_simple_t *src = (fx_ref_simple_t*)*dataptr;
                fx_ref_simple_t *dst = (fx_ref_simple_t*)dstdata;
                FX_COPY_PTR(*src, dst);
            }
            ++dataptr;
            dstdata += elemsize;
            break;
        case 1: case 2: {
            const fx_arr_t* arr_k = (const fx_arr_t*)*dataptr++;
            int ncols_k;
            size_t srcstep = arr_k->dim[0].step;
            if (tag == 1)
                ncols_k = arr_k->dim[0].size;
            else {
                nrows_k = arr_k->dim[0].size;
                ncols_k = arr_k->dim[1].size;
            }
            for( int_ i = 0; i < nrows_k; i++ )
                fx_copy_arr_elems(arr_k->data + i*srcstep,
                                  dstdata + i*dststep,
                                  ncols_k, elemsize, copy_elem);
            dstdata += ncols_k*elemsize;
            }
            break;
        case 100: {
            fx_list_simple_t lst = (fx_list_simple_t)*dataptr++;
            if (!copy_elem) {
                for(; lst != 0; lst = lst->tl, dstdata += elemsize) {
                    memcpy(dstdata, &lst->hd, elemsize);
                }
            } else if (copy_elem != fx_copy_ptr) {
                for(; lst != 0; lst = lst->tl, dstdata += elemsize) {
                    copy_elem(&lst->hd, dstdata);
                }
            } else {
                for(; lst != 0; lst = lst->tl, dstdata += elemsize) {
                    fx_ref_simple_t* src = (fx_ref_simple_t*)&lst->hd;
                    fx_ref_simple_t *dst = (fx_ref_simple_t*)dstdata;
                    FX_COPY_PTR(*src, dst);
                }
            }}
            break;
        default:
            FX_FAST_THROW_RET(FX_EXN_NoMatchError);
        }
        nrows_i = nrows_k;
    }
    return FX_OK;
}

#ifdef __cplusplus
}
#endif

#endif
