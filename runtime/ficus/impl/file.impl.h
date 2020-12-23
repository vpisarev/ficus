/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_FILE_IMPL_H__
#define __FICUS_FILE_IMPL_H__

#include "limits.h"

#ifdef __cplusplus
extern "C" {
#endif

enum { FX_FILE_ROW_BUFSIZE=128 };

int fx_fputs(FILE* f, const fx_str_t* str)
{
    const int BUFSZ = FX_FILE_ROW_BUFSIZE;
    char buf[FX_FILE_ROW_BUFSIZE*4 + 16];

    int_ i, len = str->length;
    for( i = 0; i < len; i += BUFSZ ) {
        // overflow is handled automatically inside fx_str2cstr_slice,
        // so no need to compute MIN(BUFSZ, len - i)
        fx_str2cstr_slice(str, i, BUFSZ, buf);
        if(fputs(buf, f) == EOF)
            FX_FAST_THROW_RET(FX_EXN_IOError);
    }
    return FX_OK;
}

int fx_fgets(FILE* f, fx_str_t* str)
{
    int_ bufsz = FX_FILE_ROW_BUFSIZE, bufofs = 0;
    char buf0[FX_FILE_ROW_BUFSIZE];
    char* buf = buf0;
    int fx_status = FX_OK;

    // read the whole line; (re)allocate buffer if necessary
    for(;;) {
        int count = bufsz - bufofs;
        if(count > INT_MAX) count = INT_MAX;
        char* ptr = fgets(buf + bufofs, count, f);
        if(!ptr)
        {
            if(!feof(f))
                fx_status = FX_SET_EXN_FAST(FX_EXN_IOError);
            break;
        }
        int blocksz = (int)strlen(ptr);
        bufofs += blocksz;
        if(blocksz < count-1)
            break;
        if(bufsz - bufofs < 16) {
            bufsz = bufsz*3/2;
            char* newbuf = (char*)fx_realloc((buf == buf0 ? 0 : buf), (size_t)bufsz);
            if(!newbuf) {
                fx_status = FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
                break;
            }
            if(buf == buf0 && bufofs > 0)
                memcpy(newbuf, buf, bufofs*sizeof(buf[0]));
            buf = newbuf;
        }
    }

    if( fx_status >= 0 ) {
        buf[bufofs] = '\0';
        fx_status = fx_cstr2str(buf, bufofs, str);
    }
    if(buf != buf0)
        fx_free(buf);
    return fx_status;
}

fx_cptr_t fx_get_stdstream(int stridx)
{
    static fx_cptr_data_t stdin_cptr = {1, 0, 0};
    static fx_cptr_data_t stdout_cptr = {1, 0, 0};
    static fx_cptr_data_t stderr_cptr = {1, 0, 0};

    stdin_cptr.ptr = stdin;
    stdout_cptr.ptr = stdout;
    stderr_cptr.ptr = stderr;
    return stridx == 0 ? &stdin_cptr : stridx == 1 ? &stdout_cptr : &stderr_cptr;
}

void fx_file_destructor(void* ptr) {
    FILE* f = (FILE*)ptr;
    if(f) {
        fclose(f);
    }
}

#ifdef __cplusplus
}
#endif

#endif
