/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_STRING_IMPL_H__
#define __FICUS_STRING_IMPL_H__

void fx_free_str(fx_str_t* str)
{
    if(str->refcount) {
        if(FX_DECREF(*str->refcount) == 1)
            fx_free(str->refcount);
        str->refcount = 0;
    }
}

void fx_copy_str(const fx_str_t* src, fx_str_t* dst)
{
    FX_INCREF(src->refcount);
    *dst = *src;
}

int fx_make_str(fx_str_t* str, char_* strdata, int_ length)
{
    if(!strdata) length = 0;
    size_t strsize = length*sizeof(strdata[0]);
    str->total = sizeof(*str->refcount) + strsize;
    str->refcount = (int*)fx_alloc(str->total);
    if(!str->refcount) return FX_OUT_OF_MEM_ERR;

    str->data = (char_*)(str->refcount + 1);
    str->length = length;
    memcpy(str->data, strdata, length*sizeof(strdata[0]));
    return FX_OK;
}

#endif
