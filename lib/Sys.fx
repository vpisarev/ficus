/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Various system services
import File, Filename, Math

@ccode {
    #include <limits.h>
    #include <stdlib.h>
    #include <stdio.h>
    #include <sys/stat.h>
    #include <unistd.h>

    #ifndef PATH_MAX
    #define PATH_MAX 8192
    #endif
}

val argv =
{
    @pure @nothrow fun argc(): int = @ccode { return fx_argc() }
    @pure fun argv(i: int): string = @ccode { return fx_cstr2str(fx_argv(i), -1, fx_result) }

    [: for i <- 0:argc() {argv(i)} :]
}

val win32 : bool = @ccode {
#if defined _WIN32 || defined WINCE
    true
#else
    false
#endif
}

val unix : bool = @ccode {
#if defined __linux__ || defined __unix__ || defined __MACH__ || \
    defined __APPLE__ || defined BSD || defined __hpux || \
    defined _AIX || defined __sun
    true
#else
    false
#endif
}

fun osname_(): bool -> string {
    var osname = ""
    var osinfo = ""
    fun(get_version: bool) {
        if !get_version && win32 { "Windows" }
        else {
            if osinfo == "" {
                var str = ""
                val p = File.popen(if win32 {"ver"} else {"uname -msr"},
                                   if win32 {"rt"} else {"r"})
                while true {
                    str = p.readln()
                    if str.empty() { break }
                    str = str.strip()
                    if !str.empty() { break }
                }
                p.close()
                osinfo = if !str.empty() { str } else if win32 { "Windows" } else { "Unix" }
                val sp = osinfo.find(" ")
                osname = if sp >= 0 {osinfo[:sp]} else {osinfo}
            }
            if get_version {osinfo} else {osname}
        }
    }
}

val osname = osname_()

@pure fun cc_version(): string = @ccode { return fx_cc_version(fx_result) }

fun appname() = List.hd(argv)
fun arguments() = List.tl(argv)

@pure @nothrow fun tick_count(): int64 = @ccode { return fx_tick_count() }
@pure @nothrow fun tick_frequency(): double = @ccode { return fx_tick_freq() }

fun timeit(f: void -> void, ~iterations: int=1, ~batch: int=1): double
{
    val fold gmean = 0. for i <- 0:iterations {
        val t = tick_count()
        for j <- 0:batch {f()}
        val t = tick_count() - t
        val t = t/tick_frequency()
        val t = if iterations > 1 { Math.log(max(t, 1e-16)) } else {t}
        gmean + t
    }
    if iterations > 1 { Math.exp(gmean/iterations)/batch } else {gmean/batch}
}

fun getcwd(): string = @ccode {
    char buf[PATH_MAX+16];
    char* p = getcwd(buf, PATH_MAX);
    return fx_cstr2str(p, p ? -1 : 0, fx_result);
}

fun remove(name: string): void = @ccode
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

fun rename(name: string, new_name: string): bool = @ccode
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

fun file_exists(name: string): bool = @ccode
{
    fx_cstr_t name_;
    int fx_status = fx_str2cstr(name, &name_, 0, 0);
    if (fx_status >= 0) {
        struct stat s;
        *fx_result = stat(name_.data, &s) == 0;
        fx_free_cstr(&name_);
    }
    return fx_status;
}

// throws NotFoundError if there is no such files in specified directories
fun locate_file(name: string, dirs: string list): string
{
    val dir = find(for d <- dirs {file_exists(Filename.concat(d, name))})
    Filename.normalize(getcwd(), Filename.concat(dir, name))
}

// [TODO] update to create all the parent directories if needed
fun mkdir(name: string, permissions: int): bool = @ccode
{
    fx_cstr_t name_;
    int fx_status = fx_str2cstr(name, &name_, 0, 0);
    struct stat s;
    *fx_result = false;
    if (fx_status >= 0) {
        if(stat(name_.data, &s) == -1) {
            *fx_result = mkdir(name_.data, (int)permissions) == 0;
        }
        fx_free_cstr(&name_);
    }
    return fx_status;
}

fun command(cmd: string): int = @ccode {
    fx_cstr_t cmd_;
    int fx_status = fx_str2cstr(cmd, &cmd_, 0, 0);
    if (fx_status >= 0) {
        *fx_result = system(cmd_.data);
        fx_free_cstr(&cmd_);
    }
    return fx_status;
}

fun getenv(name: string): string = @ccode {
    fx_cstr_t name_;
    int fx_status = fx_str2cstr(name, &name_, 0, 0);
    if (fx_status >= 0) {
        char* result = getenv(name_.data);
        fx_free_cstr(&name_);
        return fx_cstr2str(result, -1, fx_result);
    }
    return fx_status;
}
