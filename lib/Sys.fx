/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Various system services
import File, Filename

@ccode {
    #include <limits.h>
    #include <stdlib.h>
    #include <stdio.h>
    #include <sys/stat.h>
#if !defined WIN32 && !defined _WIN32
    #include <unistd.h>
#endif

    #ifndef PATH_MAX
    #define PATH_MAX 8192
    #endif
}

val argv =
{
    @pure @nothrow fun argc(): int = @ccode { return fx_argc() }
    @pure fun argv(i: int): string = @ccode { return fx_cstr2str(fx_argv(i), -1, fx_result) }

    [:: for i <- 0:argc() {argv(i)}]
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

fun osname_(): bool -> string
{
    var osname = ""
    var osinfo = ""
    fun(get_version: bool)
    {
        if !get_version && win32 { "Windows" }
        else {
            if osinfo == "" {
                var str = ""
                val p = File.popen(if win32 {"ver"} else {"uname -msr"},
                                   if win32 {"rt"} else {"r"})
                while true {
                    str = p.readln()
                    if str == "" { break }
                    str = str.strip()
                    if str != "" { break }
                }
                p.close()
                osinfo = if str != "" { str } else if win32 { "Windows" } else { "Unix" }
                val sp = osinfo.find(" ")
                osname = if sp >= 0 {osinfo[:sp]} else {osinfo}
            }
            if get_version {osinfo} else {osname}
        }
    }
}

val osname = osname_()

@pure fun cc_version(): string = @ccode { return fx_cc_version(fx_result) }

fun appname() = argv.hd()
fun arguments() = argv.tl()

@pure @nothrow fun tick_count(): int64 = @ccode { return fx_tick_count() }
@pure @nothrow fun tick_frequency(): double = @ccode { return fx_tick_frequency() }

fun timeit(f: void -> void, ~updated_min: (void->void)?,
           ~iterations: int=1, ~batch: int=1, ): (double, double)
{
    val nreal_iterations = max(iterations - 1, 1)
    val fold gmean = 0., mintime = 0. for i <- 0:iterations {
        val t = tick_count()
        for j <- 0:batch {f()}
        val t = tick_count() - t
        val t = t/tick_frequency()
        val log_t = if nreal_iterations > 1 { log(max(t, 1e-16)) } else {t}
        val new_mintime = if i == 0 {t} else {min(mintime, t)}
        match (i == 0 || new_mintime < mintime, updated_min) {
        | (true, Some(f)) => f()
        | _ => {}
        }
        if iterations > 1 && i == 0 {(gmean, new_mintime)}
        else { (gmean + log_t, new_mintime) }
    }
    val gmean = if nreal_iterations > 1 { exp(gmean/nreal_iterations)/batch } else {gmean/batch}
    (gmean, mintime/batch)
}

fun timeit(f: void -> void, ~iterations: int=1, ~batch: int=1, ): (double, double) =
    timeit(f, updated_min=None, iterations=iterations, batch=batch)

fun remove(name: string): void
@ccode {
    fx_cstr_t name_;
    int fx_status = fx_str2cstr(name, &name_, 0, 0);
    if (fx_status >= 0) {
        if(remove(name_.data) != 0)
            fx_status = FX_SET_EXN_FAST(FX_EXN_IOError);
        fx_free_cstr(&name_);
    }
    return fx_status;
}

fun rename(name: string, new_name: string): bool
@ccode {
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

// [TODO] update to create all the parent directories if needed
fun mkdir(name: string, permissions: int): bool
@ccode {
    fx_cstr_t name_;
    int fx_status = fx_str2cstr(name, &name_, 0, 0);
    struct stat s;
    *fx_result = false;
    if (fx_status >= 0) {
        int fstat_err = stat(name_.data, &s);
        if( fstat_err == -1) {
            *fx_result = mkdir(name_.data, (int)permissions) == 0;
        } else
            *fx_result = fstat_err == 0;
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

fun getenv(name: string, defval: string)
{
    val s = getenv(name)
    if s != "" {s} else {defval}
}

fun getpath(name: string): string list
{
    val pathsep = if win32 {';'} else {':'}
    getenv(name).split(pathsep, allow_empty=false)
}

fun colorterm(): bool = getenv("TERM").contains("xterm")
