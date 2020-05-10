/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_FILE_IMPL_H__
#define __FICUS_FILE_IMPL_H__

enum { FX_FILE_ROW_BUFSIZE=128 };

int fx_puts(FILE* f, const fx_str_t* str)
{
    const int BUFSZ = FX_FILE_ROW_BUFSIZE;
    char buf[FX_FILE_ROW_BUFSIZE*4 + 16];

    int_ i, len = str->length;
    for( i = 0; i < len; i += BUFSZ ) {
        _fx_str2cstr_slice(str, i, BUFSZ, buf);
        if(fputs(buf, f) == EOF)
            return FX_IO_ERR;
    }
    return FX_OK;
}

int fx_gets(FILE* f, fx_str_t* str)
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
                fx_status = FX_IO_ERR;
            break;
        }
        int blocksz = (int)strlen(ptr);
        bufofs += blocksz;
        if(blocksz < count-1)
            break;
        if(bufsz - bufofs < 16) {
            bufsz = bufsz*3/2;
            char* newbuf = fx_realloc((buf == buf0 ? 0 : buf), (size_t)bufsz);
            if(!newbuf) {
                fx_status = FX_OUT_OF_MEM_ERR;
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

fx_cptr_t fx_get_stdin(void)
{
    static fx_cptr_cell_t f = {1, 0, stdin};
    return &f;
}

fx_cptr_t fx_get_stdout(void)
{
    static fx_cptr_cell_t f = {1, 0, stdout};
    return &f;
}

fx_cptr_t fx_get_stderr(void)
{
    static fx_cptr_cell_t f = {1, 0, stderr};
    return &f;
}

#endif
