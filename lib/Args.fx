/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import List

val argv =
{
    pure nothrow fun argc(): int = ccode "return fx_argc();"
    pure fun argv(i: int): string = ccode "return fx_cstr2str(fx_argv(i), -1, fx_result);"

    [: for i <- 0:argc() {argv(i)} :]
}

fun name() = List.hd(argv)
fun arguments() = List.tl(argv)
