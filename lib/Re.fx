/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// C-style operations on files
object type regex_t = { handle: cptr }

fun compile(rstr: string): regex_t = ccode
{
    int fx_status = fx_re_compile(rstr, &fx_result->handle);
    return fx_status;
}

fun match_(regex: regex_t, str: string): bool = ccode
{
    int fx_status = fx_re_match(regex->handle, str, fx_result);
    return fx_status;
}
