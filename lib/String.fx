// String operations
import List

ccode "#include <string.h>"

inline pure nothrow fun length(s: string) = Builtin.length(s)

nothrow pure fun empty(s: string): bool = ccode "return s->length == 0;"

pure nothrow fun startsWith(s: string, prefix: string): bool = ccode "
    size_t sz1 = s->length;
    size_t sz2 = prefix->length;
    return sz2 <= sz1 && memcmp(s->data, prefix->data, sz2*sizeof(s->data[0])) == 0;
"

pure nothrow fun endsWith(s: string, suffix: string): bool = ccode "
    size_t sz1 = s->length;
    size_t sz2 = suffix->length;
    return sz2 <= sz1 && memcmp(s->data + (sz1 - sz2), suffix->data, sz2*sizeof(s->data[0])) == 0;
"

pure nothrow fun find(s: string, part: string): int = ccode "
    size_t i, sz1 = s->length, sz2 = part->length, l = sz1 - sz2 + 1;
    if (sz2 == 0)
        return 0;
    if (sz1 < sz2)
        return -1;
    for( i = 0; i < l; i++ )
    {
        if( s->data[i] == part->data[0] &&
            __fx_strncmp(s->data + i, part->data, sz2) == 0)
            return (int)i;
    }
    return -1;
"

pure nothrow fun rfind(s: string, part: string): int = ccode "
    size_t sz1 = s->length;
    size_t sz2 = part->length;
    ssize_t pos = sz1 - sz2;
    for ( ; pos >= 0; pos--) {
        if (__fx_strncmp(s->data + pos, part->data, sz2) == 0)
            break;
    }
    return (int)pos;
"

pure fun tolower(s: string): string = ccode "
    size_t sz = s->length;
    fx_STATUS fx_status = __fx_newstr_n(fx_ctx, fx_result, s->data, sz);
    if( fx_status == fx_SUCCESS )
    {
        __fx_char* const ptr = (__fx_char*)fx_result->data;
        for (size_t i = 0; i < sz; i++)
            ptr[i] = __fx_tolower(ptr[i]);
    }
    return fx_status;
"

pure fun toupper(s: string): string = ccode "
    size_t sz = s->length;
    fx_STATUS fx_status = __fx_String__create_n(fx_ctx, fx_result, s->data, sz);
    if( fx_status == fx_SUCCESS )
    {
        __fx_char* const ptr = (__fx_char*)fx_result->data;
        for (size_t i = 0; i < sz; i++)
            ptr[i] = __fx_toupper(ptr[i]);
    }
    return fx_status;
"

pure fun lstrip(s: string): string = ccode "
    const __fx_char* ptr = s->data;
    size_t sz = s->length;
    size_t i = 0;
    for (; i < sz && __fx_isspace(ptr[i]); i++)
        ;
    return __fx_substr(fx_ctx, s, fx_result, i, sz, 1);
"

pure fun rstrip(s: string): string = ccode "
    const __fx_char* ptr = s->data;
    size_t sz = s->length;
    for (; sz > 0 && __fx_isspace(ptr[sz - 1]); sz--)
        ;
    return __fx_substr(fx_ctx, s, fx_result, 0, sz, 1);
"

pure fun strip(s: string): string = ccode "
    const __fx_char* ptr = s->data;
    size_t sz = s->length;
    size_t i = 0;
    for (; i < sz && __fx_isspace(ptr[i]); i++)
        ;
    for (; sz > i && __fx_isspace(ptr[sz - 1]); sz--)
        ;
    return __fx_String__substring(fx_ctx, s, fx_result, i, sz, 1);
"

pure nothrow fun isalpha(c: char): bool = ccode "return __fx_isalpha(c);"
pure nothrow fun isalnum(c: char): bool = ccode "return __fx_isalnum(c);"
pure nothrow fun isdigit(c: char): bool = ccode "return __fx_isdigit(c);"
pure nothrow fun isspace(c: char): bool = ccode "return __fx_isspace(c);"
