import List

val argv =
{
    pure nothrow fun argc(): int = ccode "return __fx_argc();"
    pure fun argv(i: int): string = ccode "return __fx_cstr2str(fx_ctx, &fx_result, _fx_argv(i));"

    [:: for (i <- 0:argc()) argv(i)]
}

fun name() = List.hd(argv)
fun arguments() = List.tl(argv)
