// String operations
import List

ccode "#include <string.h>"

inline pure nothrow fun length(s: string) = Builtins.length(s)

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
            fx_strncmp(s->data + i, part->data, sz2) == 0)
            return (int)i;
    }
    return -1;
"

pure nothrow fun rfind(s: string, part: string): int = ccode "
    size_t sz1 = s->length;
    size_t sz2 = part->length;
    ssize_t pos = sz1 - sz2;
    for ( ; pos >= 0; pos--) {
        if (fx_strncmp(s->data + pos, part->data, sz2) == 0)
            break;
    }
    return (int)pos;
"

pure fun tolower(s: string): string = ccode "
    size_t sz = s->length;
    int fx_status = fx_string_create_sz(fx_ctx, fx_result, s->data, sz);
    if( fx_status == fx_SUCCESS )
    {
        char_* ptr = (char_*)fx_result->data;
        for (size_t i = 0; i < sz; i++)
            ptr[i] = fx_tolower(ptr[i]);
    }
    return fx_status;
"

pure fun toupper(s: string): string = ccode "
    size_t sz = s->length;
    int fx_status = fx_string_create_sz(fx_ctx, fx_result, s->data, sz);
    if( fx_status == fx_SUCCESS )
    {
        char_* ptr = (char_*)fx_result->data;
        for (size_t i = 0; i < sz; i++)
            ptr[i] = fx_toupper(ptr[i]);
    }
    return fx_status;
"

pure fun lstrip(s: string): string = ccode "
    const char_* ptr = s->data;
    size_t sz = s->length;
    size_t i = 0;
    for (; i < sz && fx_isspace(ptr[i]); i++)
        ;
    return fx_substr(fx_ctx, s, fx_result, i, sz, 1);
"

pure fun rstrip(s: string): string = ccode "
    const char_* ptr = s->data;
    size_t sz = s->length;
    for (; sz > 0 && fx_isspace(ptr[sz - 1]); sz--)
        ;
    return fx_substr(fx_ctx, s, fx_result, 0, sz, 1);
"

pure fun strip(s: string): string = ccode "
    const char_* ptr = s->data;
    size_t sz = s->length;
    size_t i = 0;
    for (; i < sz && fx_isspace(ptr[i]); i++)
        ;
    for (; sz > i && fx_isspace(ptr[sz - 1]); sz--)
        ;
    return fx_substr(fx_ctx, s, fx_result, i, sz, 1);
"

pure nothrow fun isalpha(c: char): bool = ccode "return fx_isalpha(c);"
pure nothrow fun isalnum(c: char): bool = ccode "return fx_isalnum(c);"
pure nothrow fun isdigit(c: char): bool = ccode "return fx_isdigit(c);"
pure nothrow fun isspace(c: char): bool = ccode "return fx_isspace(c);"
