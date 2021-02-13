/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// C-style operations on files
ccode { #include <stdio.h> }

val SEEK_SET: int = ccode { (int)SEEK_SET }
val SEEK_CURR: int = ccode { (int)SEEK_CUR }
val SEEK_END: int = ccode { (int)SEEK_END }

module type file_t = { handle: cptr }

fun get_stdstream(i: int): file_t = ccode
{
    if(i != 0 && i != 1 && i != 2)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    fx_result->handle = fx_get_stdstream(i);
    return FX_OK;
}

val stdin = get_stdstream(0)
val stdout = get_stdstream(1)
val stderr = get_stdstream(2)

fun open(fname: string, mode: string)
{
    fun open_(fname: string, mode: string): cptr = ccode
    {
        fx_cstr_t fname_, mode_;
        int fx_status = fx_str2cstr(fname, &fname_, 0, 0);
        if (fx_status >= 0) {
            fx_status = fx_str2cstr(mode, &mode_, 0, 0);
            if (fx_status >= 0) {
                FILE* f = fopen(fname_.data, mode_.data);
                if (f)
                    fx_status = fx_make_cptr(f, fx_file_destructor, fx_result);
                else
                    fx_status = FX_SET_EXN_FAST(FX_EXN_FileOpenError);
                fx_free_cstr(&mode_);
            }
            fx_free_cstr(&fname_);
        }
        return fx_status;
    }
    file_t { handle=open_(fname, mode) }
}

nothrow fun close(f: file_t): void = ccode
{
    if(f->handle && f->handle->ptr) {
        fclose((FILE*)(f->handle->ptr));
        f->handle->ptr = 0;
    }
}

nothrow fun is_open(f: file_t): bool = ccode
{ return f->handle && f->handle->ptr; }

fun eof(f: file_t): bool = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    *fx_result = feof((FILE*)(f->handle->ptr)) != 0;
    return FX_OK;
}

fun seek(f: file_t, pos: int64, origin: int): void = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    int code = fseek((FILE*)(f->handle->ptr), (long)pos, origin);
    return code == 0 ? FX_OK : FX_EXN_IOError;
}

fun tell(f: file_t): int64 = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    long code = ftell((FILE*)(f->handle->ptr));
    if(code == -1) FX_FAST_THROW_RET(FX_EXN_IOError);
    *fx_result = (int64_t)code;
    return FX_OK;
}

fun flush(f: file_t): void = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    fflush((FILE*)(f->handle->ptr));
    return FX_OK;
}

fun print(f: file_t, x: 't) = print(f, string(x))

fun print(f: file_t, x: string): void = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    return fx_fputs((FILE*)(f->handle->ptr), x);
}

fun print(f: file_t, x: int): void = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    fprintf((FILE*)(f->handle->ptr), "%zd", x);
    return FX_OK;
}

fun print(f: file_t, x: float): void = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    fprintf((FILE*)(f->handle->ptr), "%.8g", x);
    return FX_OK;
}

fun print(f: file_t, x: double): void = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    fprintf((FILE*)(f->handle->ptr), "%.16g", x);
    return FX_OK;
}

fun println(f: file_t, x: 't): void
{
    print(f, x)
    print(f, "\n")
}

fun println(f: file_t): void
{
    print(f, "\n")
}

fun write(f: file_t, a: 't []): void = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    size_t elem_size = a->dim[0].step, count0 = (size_t)a->dim[0].size;
    size_t count = fwrite(a->data, elem_size, count0, (FILE*)f->handle->ptr);
    return count == count0 ? FX_OK : FX_EXN_IOError;
}

fun write(f: file_t, a: 't [,]): void = ccode
{
    size_t step = a->dim[0].step, elem_size = a->dim[1].step;
    size_t count0 = (size_t)a->dim[1].size;
    int_ i, m = a->dim[0].size;
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);

    for(i = 0; i < m; i++) {
        size_t count = fwrite(a->data + i*step, elem_size, count0, (FILE*)f->handle->ptr);
        if(count != count0) FX_FAST_THROW_RET(FX_EXN_IOError);
    }
    return FX_OK;
}

fun read(f: file_t, buf: 't []): int = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    FILE* fh = (FILE*)f->handle->ptr;
    size_t elem_size = a->dim[0].step, count0 = (size_t)a->dim[0].size;
    size_t count = fread(a->data, elem_size, count0, fh);
    *fx_result = (int_)count;
    return count == count0 || feof(fh) ? FX_OK : FX_EXN_IOError;
}

fun readln(f: file_t): string = ccode
{
    if(!f->handle || !f->handle->ptr)
        FX_FAST_THROW_RET(FX_EXN_NullFileError);
    return fx_fgets((FILE*)(f->handle->ptr), fx_result);
}

fun remove(name: string): void = ccode
{
    fx_cstr_t name_;
    int fx_status = fx_str2cstr(name, &name_, 0, 0);
    if (fx_status >= 0) {
        if(remove(name_.data) != 0)
            fx_status = FX_SET_EXN_FAST(FX_EXN_IOError);
        fx_free_cstr(&name_);
    }
    return fx_status;
}

fun rename(name: string, new_name: string): bool = ccode
{
    fx_cstr_t name_, new_name_;
    int fx_status = fx_str2cstr(name, &name_, 0, 0);
    if (fx_status >= 0) {
        fx_status = fx_str2cstr(new_name, &new_name_, 0, 0);
        if (fx_status >= 0) {
            if(rename(name_.data, new_name_.data) != 0)
                fx_status = FX_SET_EXN_FAST(FX_EXN_IOError);
            fx_free_cstr(&new_name_);
        }
        fx_free_cstr(&name_);
    }
    return fx_status;
}
