/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Various system services

val argv =
{
    pure nothrow fun argc(): int = ccode { return fx_argc() }
    pure fun argv(i: int): string = ccode { return fx_cstr2str(fx_argv(i), -1, fx_result) }

    [: for i <- 0:argc() {argv(i)} :]
}

fun appname() = List.hd(argv)
fun arguments() = List.tl(argv)

pure nothrow fun getTickCount(): int64 = ccode { return fx_tickcount() }
pure nothrow fun getTickFrequency(): double = ccode { return fx_tickfreq() }
