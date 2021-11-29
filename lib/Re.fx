/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// A simple regular expression engine.
// See ficus/runtime/impl/regex.impl.h for more information

class t { handle: cptr }

fun compile(rstr: string): t
@ccode {
    int fx_status = fx_re_compile(rstr, &fx_result->handle);
    return fx_status;
}

fun prefixmatch(regex: t, str: string,
    ~ignorecase:bool=false, ~multiline:bool=false): (int, int) []?
{
    fun prefixmatch_(regex: t, str: string,
        ignorecase:bool, multiline:bool): (int, int) []? =
    @ccode {
        int flags = (ignorecase ? FX_RE_IGNORECASE : 0) |
                    (multiline ? FX_RE_MULTILINE : 0) |
                    FX_RE_MATCH_MODE_;
        int fx_status = fx_re_match(regex->handle, str, flags, &fx_result->u.Some);
        fx_result->tag = fx_status >= 0 && fx_result->u.Some.dim[0].size > 0 ? 2 : 1;
        return fx_status;
    }
    prefixmatch_(regex: t, str: string, ignorecase, multiline)
}

fun prefixmatch_str(regex: t, str: string,
    ~ignorecase:bool=false, ~multiline:bool=false): string []? =
    match prefixmatch(regex, str, ignorecase=ignorecase, multiline=multiline) {
    | Some(matches) => Some([| for (i, j) <- matches {str[i:j]} |])
    | _ => None
    }

fun fullmatch(regex: t, str: string,
    ~ignorecase:bool=false, ~multiline:bool=false): bool
{
    fun fullmatch_(regex: t, str: string, ignorecase:bool, multiline:bool): bool =
    @ccode {
        int flags = (ignorecase ? FX_RE_IGNORECASE : 0) |
                    (multiline ? FX_RE_MULTILINE : 0);
        int fx_status = fx_re_fullmatch(regex->handle, str, flags);
        *fx_result = fx_status > 0;
        return fx_status > 0 ? 0 : fx_status;
    }
    fullmatch_(regex, str, ignorecase, multiline)
}

fun find(regex: t, str: string,
    ~ignorecase:bool=false, ~multiline:bool=false): (int, int) []?
{
    fun find_(regex: t, str: string,
        ignorecase:bool, multiline:bool): (int, int) []? =
    @ccode {
        int flags = (ignorecase ? FX_RE_IGNORECASE : 0) |
                    (multiline ? FX_RE_MULTILINE : 0) |
                    FX_RE_FIND_MODE_;
        int fx_status = fx_re_match(regex->handle, str, flags, &fx_result->u.Some);
        fx_result->tag = fx_status >= 0 && fx_result->u.Some.dim[0].size > 0 ? 2 : 1;
        return fx_status;
    }
    find_(regex, str, ignorecase, multiline)
}

fun find_str(regex: t, str: string,
    ~ignorecase:bool=false, ~multiline:bool=false): string []? =
    match find(regex, str, ignorecase=ignorecase, multiline=multiline) {
    | Some(matches) => Some([| for (i, j) <- matches {str[i:j]} |])
    | _ => None
    }

fun findall(regex: t, str: string,
    ~ignorecase:bool=false, ~multiline:bool=false): (int, int) [,]
{
    fun findall_(regex: t, str: string,
        ignorecase:bool, multiline:bool): (int, int) [,] =
    @ccode {
        int flags = (ignorecase ? FX_RE_IGNORECASE : 0) |
                    (multiline ? FX_RE_MULTILINE : 0) |
                    FX_RE_FINDALL_MODE_;
        return fx_re_match(regex->handle, str, flags, fx_result);
    }
    findall_(regex, str, ignorecase, multiline)
}

fun findall_str(regex: t, str: string,
    ~ignorecase:bool=false, ~multiline:bool=false): string [,]
{
    val matches = findall(regex, str, ignorecase=ignorecase, multiline=multiline)
    [| for (i, j) <- matches {str[i:j]} |]
}

fun replace(regex: t, str: string, subst: string,
    ~ignorecase:bool=false, ~multiline:bool=false): string
{
    fun replace_(regex: t, str: string, subst: string,
        ignorecase:bool, multiline:bool): string =
    @ccode {
        int flags = (ignorecase ? FX_RE_IGNORECASE : 0) |
                    (multiline ? FX_RE_MULTILINE : 0);
        return fx_re_replace(regex->handle, str, subst, flags, fx_result);
    }
    replace_(regex, str, subst, ignorecase, multiline)
}
