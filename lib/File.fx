/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// C-style operations on files
import String

ccode "#include <stdio.h>"
exception NullFileError

val SEEK_SET: int = ccode "(int)SEEK_SET"
val SEEK_CURR: int = ccode "(int)SEEK_CUR"
val SEEK_END: int = ccode "(int)SEEK_END"

type file_t = { handle: cptr }

val stdin: file_t = "{ fx_get_stdin() }"
val stdout: file_t = "{ fx_get_stdout() }"
val stderr: file_t = "{ fx_get_stderr() }"

fun open(fname: string, mode: string)
{
    fun open_(fname: string, mode: string): cptr = ccode
    "
    fx_cstr_t fname_, mode_;
    int fx_status = fx_str2cstr(&fname_, fname);
    if (fx_status >= 0) {
        fx_status = fx_str2cstr(&mode_, mode);
        if (fx_status >= 0) {
            FILE* f = fopen(fname_.data, mode_.data);
            if (f)
                fx_status = fx_make_cptr(f, _fx_file_t_destructor, fx_result);
            else
                fx_status = FX_FILE_OPEN_ERR;
            fx_free_cstr(&mode_);
        }
        fx_free_cstr(&fname_);
    }
    return fx_status;
    "
    file_t { handle=open_(fname, mode) }
}

nothrow fun close(f: file_t): void = ccode
    "
    if(f->handle && f->handle->ptr) {
        fclose((FILE*)(f->handle->ptr));
        f->handle->ptr = 0;
    }
    "

nothrow fun isOpened(f: file_t): bool = ccode
    "return f->handle && f->handle->ptr;"

fun isEOF(f: file_t): bool = ccode
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    *fx_result = feof((FILE*)(f->ptr)) != 0;
    return FX_OK;
    "

fun seek(f: file_t, pos: int64, origin: int): void = ccode
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    int code = fseek((FILE*)(f->handle->ptr), (long)pos, origin);
    return code == 0 ? FX_OK : FX_FILE_IO_ERR;
    "

fun tell(f: file_t): int64 = ccode
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    long code = ftell((FILE*)(f->handle->ptr));
    if(code == -1) return FX_FILE_IO_ERR;
    *fx_result = (int64)code;
    return FX_OK;
    "

fun flush(f: file_t): void = ccode
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    fflush((FILE*)(f->handle->ptr));
    return FX_OK;
    "

fun print(f: file_t, x: 't) = print(f, string(x))

fun print(f: file_t, x: string): void = ccode
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    return fx_fputs((FILE*)(f->handle->ptr), str);
    "

fun print(f: file_t, x: int): void =
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    fprintf((FILE*)(f->handle->ptr), \"%zd\", x);
    return FX_OK;
    "

fun print(f: file_t, x: float): void =
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    fprintf((FILE*)(f->handle->ptr), \"%.8g\", x);
    return FX_OK;
    "

fun print(f: file_t, x: double): void =
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    fprintf((FILE*)(f->handle->ptr), \"%.16g\", x);
    return FX_OK;
    "

fun println(f: file_t, x: 't): void
{
    print(f, x)
    print(f, "\n")
}

fun write(f: file_t, a: 't []): void = ccode
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    size_t elem_size = a->dim[0].step, count0 = (size_t)a->dim[0].size;
    size_t count = fwrite(a->data, elem_size, count0, (FILE*)f->handle->ptr);
    return count == count0 ? FX_OK : FX_FILE_IO_ERR;
    "

fun write(f: file_t, a: 't [,]): void = ccode
    "
    size_t step = a->dim[0].step, elem_size = a->dim[1].step;
    size_t count0 = (size_t)a->dim[1].size;
    int_ i, m = a->dim[0].size;
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;

    for(i = 0; i < m; i++) {
        size_t count = fwrite(a->data + i*step, elem_size, count0, (FILE*)f->handle->ptr);
        if(count != count0) return FX_FILE_IO_ERR;
    }
    return FX_OK;
    "

fun read(f: file_t, buf: 't []): int = ccode
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    FILE* fh = (FILE*)f->handle->ptr;
    size_t elem_size = a->dim[0].step, count0 = (size_t)a->dim[0].size;
    size_t count = fread(a->data, elem_size, count0, fh);
    *fx_result = (int_)count;
    return count == count0 || feof(fh) ? FX_OK : FX_FILE_IO_ERR;
    "

fun readln(f: file_t): string = ccode
    "
    if(!f->handle || !f->handle->ptr)
        return FX_FILE_NULL_HANDLE_ERR;
    return fx_fgets((FILE*)(f->handle->ptr), fx_result);
    "

fun remove(name: string): bool = ccode
    "
    fx_cstr_t name_;
    int fx_status = fx_str2cstr(&name_, name, 0, 0);
    if (fx_status >= 0) {
        *fx_result = remove(name_.data) == 0;
        fx_free_cstr(&name_);
    }
    return fx_status;
    "

fun rename(name: string, new_name: string): bool = ccode
    "
    fx_cstr_t name_, new_name_;
    int fx_status = fx_str2cstr(&name_, name, 0, 0);
    if (fx_status >= 0) {
        fx_status = fx_str2cstr(&new_name_, new_name, 0, 0);
        if (fx_status >= 0) {
            *fx_result = rename(name_.data, new_name_.data) == 0;
            fx_free_cstr(&new_name_);
        }
        fx_free_cstr(&name_);
    }
    return fx_status;
    "
