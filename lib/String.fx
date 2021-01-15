/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// String operations

ccode { #include <string.h> }

inline fun length(s: string) = Builtins.length(s)
inline fun join(sep: string, strs: string []) = Builtins.join(sep, strs)
inline fun join(sep: string, strs: string list) = Builtins.join(sep, strs)

nothrow pure fun empty(s: string): bool = ccode { return s->length == 0 }

pure nothrow fun startswith(s: string, prefix: string): bool = ccode
{
    int_ sz1 = s->length;
    int_ sz2 = prefix->length;
    return sz2 <= sz1 && memcmp(s->data, prefix->data,
        (size_t)(sz2*sizeof(s->data[0]))) == 0;
}

pure nothrow fun endswith(s: string, suffix: string): bool = ccode
{
    int_ sz1 = s->length;
    int_ sz2 = suffix->length;
    return sz2 <= sz1 && memcmp(s->data + (sz1 - sz2), suffix->data,
        (size_t)(sz2*sizeof(s->data[0]))) == 0;
}

pure nothrow fun find(s: string, part: string): int = ccode
{
    int_ i, sz1 = s->length, sz2 = part->length, l = sz1 - sz2 + 1;
    if (sz2 == 0)
        return 0;
    if (l <= 0)
        return -1;
    for( i = 0; i < l; i++ ) {
        if( s->data[i] == part->data[0] &&
            memcmp(s->data + i, part->data, sz2*sizeof(part->data[0])) == 0 )
            return i;
    }
    return -1;
}

pure nothrow fun rfind(s: string, part: string): int = ccode
{
    int_ sz1 = s->length, sz2 = part->length, pos = sz1 - sz2;
    for ( ; pos >= 0; pos--) {
        if( memcmp(s->data + pos, part->data, sz2*sizeof(part->data[0])) == 0)
            break;
    }
    return pos;
}

pure fun tolower(s: string): string = ccode
{
    int_ i, sz = s->length;
    const char_* src = s->data;
    for (i = 0; i < sz; i++ ) {
        char_ c = src[i];
        if (fx_tolower(c) != c) break;
    }
    if (i == 0) {
        fx_copy_str(s, fx_result);
        return FX_OK;
    }
    int fx_status = fx_make_str(0, sz, fx_result);
    if( fx_status >= 0 ) {
        char_* dst = fx_result->data;
        for (size_t i = 0; i < sz; i++)
            dst[i] = fx_tolower(src[i]);
    }
    return fx_status;
}

pure fun toupper(s: string): string = ccode
{
    int_ i, sz = s->length;
    const char_* src = s->data;
    for (i = 0; i < sz; i++ ) {
        char_ c = src[i];
        if (fx_toupper(c) != c) break;
    }
    if (i == 0) {
        fx_copy_str(s, fx_result);
        return FX_OK;
    }
    int fx_status = fx_make_str(0, sz, fx_result);
    if( fx_status >= 0 ) {
        char_* dst = fx_result->data;
        for (size_t i = 0; i < sz; i++)
            dst[i] = fx_toupper(src[i]);
    }
    return fx_status;
}

pure fun capitalize(s: string): string = ccode
{
    int_ sz = s->length;
    const char_* src = s->data;
    if (sz == 0 || fx_toupper(src[0]) == src[0]) {
        fx_copy_str(s, fx_result);
        return FX_OK;
    }
    int fx_status = fx_make_str(0, sz, fx_result);
    if( fx_status >= 0 ) {
        char_* dst = fx_result->data;
        dst[0] = fx_toupper(src[0]);
        for (size_t i = 1; i < sz; i++)
            dst[i] = src[i];
    }
    return fx_status;
}

pure fun lstrip(s: string): string = ccode
{
    const char_* ptr = s->data;
    size_t sz = s->length;
    size_t i = 0;
    for (; i < sz && fx_isspace(ptr[i]); i++)
        ;
    return fx_substr(s, i, sz, 1, 0, fx_result);
}

pure fun rstrip(s: string): string = ccode
{
    const char_* ptr = s->data;
    size_t sz = s->length;
    for (; sz > 0 && fx_isspace(ptr[sz - 1]); sz--)
        ;
    return fx_substr(s, 0, sz, 1, 0, fx_result);
}

pure fun strip(s: string): string = ccode
{
    const char_* ptr = s->data;
    int_ i = 0, sz = s->length;
    for (; i < sz && fx_isspace(ptr[i]); i++)
        ;
    for (; sz > i && fx_isspace(ptr[sz - 1]); sz--)
        ;
    return fx_substr(s, i, sz, 1, 0, fx_result);
}

pure fun substr(s: string, pos: int, len: int): string = ccode //TODO: make this function safe.
{
    return fx_substr(s, pos, pos + len, 1, 0, fx_result);
}

fun tokens(s: string, f: char->bool)
{
    val fold (sl, start, sep) = ([], 0, true) for c@i <- s {
        if f(c) {
            (if sep {sl} else {s[start:i] :: sl}, start, true)
        } else {
            (sl, if sep {i} else {start}, false)
        }
    }
    List.rev(if sep {sl} else {s[start:] :: sl})
}

pure nothrow fun isalpha(c: char): bool = ccode { return fx_isalpha(c) }
pure nothrow fun isalnum(c: char): bool = ccode { return fx_isalnum(c) }
pure nothrow fun isdigit(c: char): bool = ccode { return fx_isdigit(c) }
pure nothrow fun isdecimal(c: char): bool = ccode { return fx_isdecimal(c) }
pure nothrow fun isspace(c: char): bool = ccode { return fx_isspace(c) }
pure nothrow fun tolower(c: char): char = ccode { return fx_tolower(c) }
pure nothrow fun toupper(c: char): char = ccode { return fx_toupper(c) }
