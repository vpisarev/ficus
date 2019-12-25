// C-style operations on files
import String

ccode "#include <stdio.h>"
exception NullFileError

val SEEK_SET: int = ccode "(int)SEEK_SET"
val SEEK_CURR: int = ccode "(int)SEEK_CUR"
val SEEK_END: int = ccode "(int)SEEK_END"

type file_t = { handle: cptr }

fun open(fname: string, mode: string)
{
    fun open_(fname: string, mode: string): cptr = ccode
    "
    fx_cstr_t fname_, mode_;
    int fx_status = fx_str2cstr(fx_ctx, &fname_, fname);
    if (!fx_status) {
        fx_status = fx_str2cstr(fx_ctx, &mode_, mode);
        if (!fx_status) {
            FILE* f = fopen(fname_.data, mode_.data);
            if (f)
                fx_status = fx_new_cptr(fx_ctx, fx_result, f, _fx_file_t_destructor);
            else
                fx_status = FX_THROW(FX_EXCEPTION_IO);
            fx_cstr_release(fx_ctx, &mode_);
        }
        fx_cstr_release(fx_ctx, &fname_);
    }
    return fx_status;
    "
    file_t { handle=open_(fname, mode) }
}

fun close(f: file_t)
{
    fun close_(f: cptr): void = ccode "if(f && f->p) { fclose((FILE*)(f->p)); f->p = NULL; } return FX_SUCCESS;"
    close_(f.handle)
}

fun isOpened(f: file_t)
{
    pure nothrow fun isOpened_(f: cptr): bool = ccode "return f && f->p;"
    isOpened_(f.handle)
}

fun isEOF(f: file_t)
{
    if (!isOpened(f)) throw NullFileError
    nothrow pure fun iseof_(f: cptr): bool =
        ccode "return feof((FILE*)(f->p));";
    iseof_(f.handle)
}

fun seek(f: file_t, pos: int64, origin: int)
{
    nothrow fun seek_(f: cptr, pos: int64, origin: int): bool =
        ccode "return fseek((FILE*)(f->p), (long)pos, origin) != 0;"
    if (!isOpened(f)) throw NullFileError
    seek_(f.handle, pos, origin)
}

fun tell(f: file_t)
{
    nothrow pure fun tell_(f: cptr): int64 = ccode "return (int64)ftell((FILE*)(f->p));"
    if (!isOpened(f)) throw NullFileError
    tell_(f.handle)
}

fun flush(f: file_t)
{
    nothrow fun flush_(f: cptr): void =
        ccode "fflush((FILE*)(f->p));"
    if (!isOpened(f)) throw NullFileError
    flush_(f.handle)
}

fun print(f: file_t, x: 't) = print(f, string(x))

fun print(f: file_t, x: string)
{
    nothrow fun print_(f: cptr, str: string): void = ccode "fx_fputs(fx_ctx, str, (FILE*)(f->p));"
    if (!isOpened(f)) throw NullFileError
    print_(f.handle, x)
}

fun println(f: file_t, x: 't): void
{
    print(f, x)
    print(f, "\n")
}

fun write(f: file_t, a: 't [])
{
    fun write_(f: cptr, a: 't []): void = ccode
    "
    size_t elem_size = a->dim[0].step;
    return fx_fwrite(fx_ctx, fx_result, a->data, elem_size, a->dim[0].size, (FILE*)(f->p));
    "
    if (!isOpened()) throw NullFileError
    write_(f.handle, a)
}

fun write(f: file_t, a: 't [,])
{
    fun write_(f: cptr, a: 't [,]): void = ccode
    "
    size_t elem_size = a->dim[1].step;
    size_t m = a->dim[0].size, n = a->dim[1].size;
    size_t idx0;
    int fx_status = FX_SUCCESS;
    for (idx0 = 0; idx0 < m && !fx_status; idx0++)
    {
        size_t offset = FX_ND_OFFSET_2D_UNWRAP(int8_t, a->dim, idx0, 0);
        int8_t* ptr = FX_ADDR_OFFSET(int8_t, a->data, offset);
        fx_int count = 0;
        fx_status = fx_fwrite(fx_ctx, &count, ptr, elem_size, n, (FILE*)(f->p));
    }
    return fx_status;
    "
    if (!isOpened(f)) throw NullFileError
    write_(f.handle, a)
}

fun read(f: file_t, buf: uint8 [])
{
    fun read_(f: cptr, buf: uint8 []): void =
        ccode "return fx_fread(fx_ctx, &fx_res, buf->data, 1, buf->dim[0].size, (FILE*)(f->p));"
    if (!isOpened(f)) throw NullFileError
    read_(f.handle, buf)
}

fun readln(f: file_t)
{
    fun readln_(f: cptr): string =
        ccode "return fx_fgets(fx_ctx, fx_result, (FILE*)(f->p));"
    if (!isOpened(f)) throw NullFileError
    readln_(f.handle)
}

fun remove(name: string): bool = ccode
    "
    fx_cstr_t name_;
    int fx_status = fx_str2cstr(fx_ctx, &name_, name);
    if (!fx_status) {
        *fx_result = remove(name_.data) == 0;
        fx_release_cstr(fx_ctx, &name_);
    }
    return fx_status;
    "

fun rename(name: string, new_name: string): bool = ccode
    "
    fx_cstr_t name_, new_name_;
    int fx_status = fx_str2cstr(fx_ctx, &name_, name);
    if (!fx_status) {
        fx_status = fx_str2cstr(fx_ctx, &new_name_, new_name);
        if (!fx_status) {
            *fx_result = rename(name_.data, new_name_.data) == 0;
            fx_cstr_release(fx_ctx, &new_name_);
        }
        fx_cstr_release(fx_ctx, &name_);
    }
    return fx_status;
    "
