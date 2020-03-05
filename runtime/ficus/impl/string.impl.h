/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_STRING_IMPL_H__
#define __FICUS_STRING_IMPL_H__

void fx_free_str(fx_str_t* str)
{
    if( str->rc )
    {
        if(FX_DECREF(*str->rc) == 1)
            fx_free(str->rc);
        str->rc = 0;
    }
}

void fx_copy_str(const fx_str_t* src, fx_str_t* dst)
{
    FX_INCREF(src->rc);
    *dst = *src;
}

int fx_make_str(fx_str_t* str, char_* strdata, int_ length)
{
    if( !strdata )
        length = 0;

    size_t total = sizeof(*str->rc) + length*sizeof(strdata[0]);
    str->rc = (fx_rc_t*)fx_alloc(total);
    if( !str->rc )
        return FX_OUT_OF_MEM_ERR;

    str->data = (char_*)(str->rc + 1);
    str->length = length;
    memcpy(str->data, strdata, length*sizeof(strdata[0]));
    return FX_OK;
}

#endif
