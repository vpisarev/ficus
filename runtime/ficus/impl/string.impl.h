/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_STRING_IMPL_H__
#define __FICUS_STRING_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif

int_ fx_strlen(const char_* rawstr)
{
    int_ len = 0;
    while(rawstr[len] != 0)
        len++;
    return len;
}

void fx_free_str(fx_str_t* str)
{
    if( str->rc )
    {
        if(FX_DECREF(*str->rc) == 1)
            fx_free(str->rc);
        str->data = 0;
        str->length = 0;
        str->rc = 0;
    }
}

void fx_copy_str(const fx_str_t* src, fx_str_t* dst)
{
    FX_COPY_STR(src, dst);
}

int fx_make_str(const char_* strdata, int_ length, fx_str_t* str)
{
    size_t total = sizeof(*str->rc) + length*sizeof(strdata[0]);
    str->rc = (int_*)fx_malloc(total);
    if(!str->rc) FX_FAST_THROW_RET(FX_EXN_OutOfMemError);

    *str->rc = 1;
    str->data = (char_*)(str->rc + 1);
    str->length = length;
    if(strdata)
        memcpy(str->data, strdata, length*sizeof(strdata[0]));
    return FX_OK;
}

void fx_free_cstr(fx_cstr_t* str)
{
    if( str->rc )
    {
        if(FX_DECREF(*str->rc) == 1)
            fx_free(str->rc);
        str->rc = 0;
    }
}

static size_t fx_str2cstr_size(const fx_str_t* str)
{
    size_t sz = 1;
    int_ i, len = str->length;
    const char_* src = str->data;
    for( i = 0; i < len; i++ )
    {
        char_ ch = src[i];
        sz++;
        sz += ch > 127;
        sz += ch > 2047;
        sz += (ch > 65535) & (ch <= 1114111);
    }
    return sz;
}

size_t fx_str2cstr_slice(const fx_str_t* str, int_ start, int_ maxcount, char* buf)
{
    const char_* src = str->data + start;
    int_ i, count = str->length - start;
    char* dst = buf;
    if( count > maxcount ) count = maxcount;

    for( i = 0; i < count; i++ )
    {
        char_ ch = src[i];
        if( ch <= 127 )
            *dst++ = (char)ch;
        else if( ch <= 2047 ) {
            *dst++ = (char)(192 | (ch >> 6));
            *dst++ = (char)(128 | (ch & 63));
        }
        else if( ch <= 65535 ) {
            *dst++ = (char)(224 | (ch >> 12));
            *dst++ = (char)(128 | ((ch >> 6) & 63));
            *dst++ = (char)(128 | (ch & 63));
        }
        else if( ch <= 1114111 ) {
            *dst++ = (char)(240 | (ch >> 18));
            *dst++ = (char)(128 | ((ch >> 12) & 63));
            *dst++ = (char)(128 | ((ch >> 6) & 63));
            *dst++ = (char)(128 | (ch & 63));
        }
        else { // <?>
            *dst++ = (char)239;
            *dst++ = (char)191;
            *dst++ = (char)189;
        }
    }
    *dst++ = '\0';
    return (size_t)(dst - buf);
}

int fx_str2cstr(const fx_str_t* str, fx_cstr_t* cstr, char* buf, size_t bufsz)
{
    size_t sz = fx_str2cstr_size(str);
    if( buf && sz <= bufsz )
    {
        cstr->rc = 0;
        cstr->data = buf;
    }
    else
    {
        size_t total = sizeof(*cstr->rc) + sz*sizeof(cstr->data[0]);
        cstr->rc = (int_*)fx_malloc(total);
        if( !cstr->rc )
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
        cstr->data = (char*)(cstr->rc + 1);
        *cstr->rc = 1;
    }
    cstr->length = sz-1;
    fx_str2cstr_slice(str, 0, str->length, cstr->data);
    return FX_OK;
}

static int_ fx_cstr2str_len(const char* src, int_ srclen)
{
    int_ i, dstlen = 0;
    for( i = 0; i < srclen; i++ )
    {
        unsigned char ch = (unsigned char)src[i];
        dstlen++;
        if( ch <= 127 )
            ;
        else
        {
            while(i+1 < srclen && (src[i+1] & 0xc0) == 0x80)
                i++;
        }
    }
    return dstlen;
}

int fx_cstr2str(const char* src, int_ srclen, fx_str_t* str)
{
    if(srclen < 0)
        srclen = (int_)strlen(src);
    int_ dstlen = fx_cstr2str_len(src, srclen);
    size_t total = sizeof(*str->rc) + dstlen*sizeof(str->data[0]);
    str->rc = (int_*)fx_malloc(total);
    if( !str->rc )
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);

    *str->rc = 1;
    str->data = (char_*)(str->rc + 1);
    str->length = dstlen;
    char_* dst = str->data;

    for( int_ i = 0; i < srclen; i++ )
    {
        unsigned char ch = (unsigned char)src[i];
        if( ch <= 127 )
            *dst++ = ch;
        else if( ch <= 223 && i+1 < srclen && (src[i+1] & 0xc0) == 0x80) {
            *dst++ = ((ch & 31) << 6) | (src[i+1] & 63);
            i++;
        }
        else if( ch <= 239 && i+2 < srclen && (src[i+1] & 0xc0) == 0x80 && (src[i+2] & 0xc0) == 0x80) {
            *dst++ = ((ch & 15) << 12) | ((src[i+1] & 63) << 6) | (src[i+2] & 63);
            i += 2;
        }
        else if( ch <= 247 && i+3 < srclen && (src[i+1] & 0xc0) == 0x80 && (src[i+2] & 0xc0) == 0x80 && (src[i+3] & 0xc0) == 0x80) {
            char_ val = (char_)(((ch & 15) << 18) | ((src[i+1] & 63) << 12) | ((src[i+2] & 63) << 6) | (src[i+3] & 63));
            if( val > 1114111 ) val = (char_)65533;
            *dst++ = val;
            i += 3;
        }
        else {
            *dst++ = (char_)65533;
            while(i+1 < srclen && (src[i+1] & 0xc0) == 0x80)
                i++;
        }
    }
    assert(dst - str->data == dstlen);
    return FX_OK;
}

int fx_ascii2str(const char* src, int_ srclen, fx_str_t* str)
{
    if(srclen < 0)
        srclen = (int_)strlen(src);

    size_t total = sizeof(*str->rc) + srclen*sizeof(str->data[0]);
    int_* rcptr = (int_*)fx_malloc(total);
    if( !rcptr )
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);

    char_* dst = (char_*)(rcptr + 1);
    for( int_ i = 0; i < srclen; i++ )
    {
        unsigned char ch = (unsigned char)src[i];
        if( ch > 127 ) {
            fx_free(rcptr);
            FX_FAST_THROW_RET(FX_EXN_ASCIIError);
        }
        dst[i] = ch;
    }

    *rcptr = 1;
    str->rc = rcptr;
    str->data = dst;
    str->length = srclen;
    return FX_OK;
}

#include "_fx_unicode_data.gen.h"

static int _fx_char_category(char_ ch)
{
    return _fx_uni_getdata(ch) & FX_UNICODE_CAT_Mask;
}

bool fx_isalpha(char_ ch)
{
    return _fx_char_category(ch) <= FX_UNICODE_CAT_Lo;
}

bool fx_isdigit(char_ ch)
{
    return (((1 << FX_UNICODE_CAT_Nd) |
            (1 << FX_UNICODE_CAT_No)) & (1 << _fx_char_category(ch))) != 0;
}

bool fx_isalnum(char_ ch)
{
    return (((1 << FX_UNICODE_CAT_Lu) |
            (1 << FX_UNICODE_CAT_Ll) |
            (1 << FX_UNICODE_CAT_Lt) |
            (1 << FX_UNICODE_CAT_Lm) |
            (1 << FX_UNICODE_CAT_Lo) |
            (1 << FX_UNICODE_CAT_Nd) |
            (1 << FX_UNICODE_CAT_Nl) |
            (1 << FX_UNICODE_CAT_No)) & (1 << _fx_char_category(ch))) != 0;
}

bool fx_ispunct(char_ ch)
{
    return (((1 << FX_UNICODE_CAT_Pd) |
            (1 << FX_UNICODE_CAT_Ps) |
            (1 << FX_UNICODE_CAT_Pe) |
            (1 << FX_UNICODE_CAT_Pc) |
            (1 << FX_UNICODE_CAT_Po) |
            (1 << FX_UNICODE_CAT_Pi) |
            (1 << FX_UNICODE_CAT_Pf)) & (1 << _fx_char_category(ch))) != 0;
}

bool fx_isdecimal(char_ ch)
{
    return _fx_char_category(ch) == FX_UNICODE_CAT_Nd;
}

bool fx_isspace(char_ ch)
{
    return (((1 << FX_UNICODE_CAT_Zs) |
            (1 << FX_UNICODE_CAT_Zl) |
            (1 << FX_UNICODE_CAT_Zp) |
            (1 << FX_UNICODE_CAT_Zextra)) & (1 << _fx_char_category(ch))) != 0;
}

char_ fx_tolower(char_ ch)
{
    int cdata = _fx_uni_getdata(ch);
    int cat = cdata & FX_UNICODE_CAT_Mask;
    int ofs = cat == FX_UNICODE_CAT_Lu ? (cdata >> (FX_UNICODE_CAT_Shift + FX_UNICODE_BIDIR_Shift)) : 0;
    return (char_)(ch + ofs);
}

char_ fx_toupper(char_ ch)
{
    int cdata = _fx_uni_getdata(ch);
    int cat = cdata & FX_UNICODE_CAT_Mask;
    int ofs = cat == FX_UNICODE_CAT_Ll ? (cdata >> (FX_UNICODE_CAT_Shift + FX_UNICODE_BIDIR_Shift)) : 0;
    return (char_)(ch + ofs);
}

int fx_todigit(char_ ch)
{
    int cdata = _fx_uni_getdata(ch);
    int cat = cdata & FX_UNICODE_CAT_Mask;
    return cat == FX_UNICODE_CAT_Nd ? (cdata >> (FX_UNICODE_CAT_Shift + FX_UNICODE_BIDIR_Shift)) : -1;
}

int fx_bidirectional(char_ ch)
{
    int cdata = _fx_uni_getdata(ch);
    return (cdata >> FX_UNICODE_CAT_Shift) & FX_UNICODE_BIDIR_Mask;
}

bool fx_atoi(const fx_str_t* str, int_* result, int base)
{
    int_ i, len = str->length;
    int_ s = 1, r = 0;
    const char_ *ptr = str->data;
    bool ok = false;
    *result = 0;
    if(len == 0)
        return false;
    if(*ptr == '-') {
        s = -1;
        ptr++;
        if(--len == 0)
            return false;
    }
    if( base == 0 ) {
        if( ptr[0] == '0' ) {
            if(len > 2 && (ptr[1] == 'x' || ptr[1] == 'X')) {
                base = 16;
                ptr += 2;
                len -= 2;
            }
            else if(len > 2 && (ptr[1] == 'b' || ptr[1] == 'B')) {
                base = 2;
                ptr += 2;
                len -= 2;
            }
            else base = 8;
        }
        else if( fx_isdecimal(ptr[0]))
            base = 10;
        else
            return false;
    }
    else if( base < 2 || base > 36 )
        return false;

    if( base == 10 ) {
        for( i = 0; i < len; i++ ) {
            int digit = fx_todigit(ptr[i]);
            if(digit < 0)
                break;
            r = r*10 + digit;
        }
    }
    else {
        for( i = 0; i < len; i++ ) {
            char_ c = ptr[i];
            int digit = 0;
            if('0' <= c && c <= '9')
                digit = (int)(c - '0');
            else if('a' <= c && c <= 'z')
                digit = (int)(c - 'a' + 10);
            else if('A' <= c && c <= 'Z')
                digit = (int)(c - 'A' + 10);
            else
                break;
            if(digit >= base)
                break;
            r = r*base + digit;
        }
    }
    *result = r;
    return i == len;
}

int fx_itoa(int_ n, fx_str_t* str)
{
    static const char* tab =
        "0001020304050607080910111213141516171819202122232425262728293031323334353637383940414243444546474849"
        "5051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899";
    int_ len = 1;
    int_ neg = -(n < 0);

    n = (n ^ neg) - neg;
    size_t a = (size_t)n;
    if(a >= 100000000) {
        if(a >= 10000000000000000ULL) {
            a = (size_t)(a/10000000000000000ULL);
            len += 16;
            // at most 3-4 digits left, so we skip the second check for >=10**8
        }
        else {
            a /= 100000000;
            len += 8;
        }
    }
    if(a >= 10000) {
        a /= 10000;
        len += 4;
    }
    if(a >= 100) {
        a /= 100;
        len += 2;
    }
    len += a >= 10;

    int status = fx_make_str(0, len-neg, str);
    if(status < 0) return status;

    char_* buf = str->data;
    *buf = '-';
    buf -= neg;
    char_* ptr = buf + len;

    a = (size_t)n;
    while((ptr -= 2) >= buf) {
        size_t q = a / 100;
        size_t r = a - q*100;
        ptr[0] = (char_)tab[r*2];
        ptr[1] = (char_)tab[r*2+1];
        a = q;
    }
    if(ptr+1 == buf) buf[0] = (char_)(a + '0');
    return FX_OK;
}

int fx_substr(const fx_str_t* str, int_ start, int_ end, int_ delta, int mask, fx_str_t* substr)
{
    int_ len = FX_STR_LENGTH(*str);
    start = delta < 0 && (mask & 1) ? len - 1 : start;
    end = delta > 0 && (mask & 2) ? len : end;
    printf("fx_substr: start=%zd, end=%zd, delta=%zd, mask=%d, len=%zd\n", start, end, delta, mask, len);
    if (start < 0 || start > end || end > len)
        FX_FAST_THROW_RET(FX_EXN_OutOfRangeError);
    if (delta == 0)
        FX_FAST_THROW_RET(FX_EXN_ZeroStepError);
    if (delta != 1)
    {
        int_ dstlen = FX_LOOP_COUNT(start, end, delta);
        if(dstlen == 0)
            return FX_OK;

        int status = fx_make_str(0, dstlen, substr);
        if(status < 0) return status;
        const char_* src = str->data;
        char_* dst = substr->data;
        for(int_ i = 0; i < dstlen; i++)
            dst[i] = src[start + i*delta];
    }
    else
    {
        if (start == end)
            return FX_OK;
        substr->rc = str->rc;
        if(str->rc) FX_INCREF(*str->rc);
        substr->data = str->data + start;
        substr->length = end - start;
    }
    return FX_OK;
}

int fx_strjoin(const fx_str_t* begin, const fx_str_t* end, const fx_str_t* sep,
               const fx_str_t* s, int_ count, fx_str_t* result)
{
    int_ seplen = sep ? sep->length : 0;
    int_ beginlen = begin ? begin->length : 0;
    int_ endlen = end ? end->length : 0;

    if(beginlen == 0 && endlen == 0) {
        if(count == 0) {
            // result should already be initialized with empty string
            return FX_OK;
        }
        if(count == 1) {
            fx_copy_str(&s[0], result);
            return FX_OK;
        }
    }
    int_ i, total = seplen*(count > 1 ? count-1 : 0);
    for( i = 0; i < count; i++ )
        total += s[i].length;
    if(begin)
        total += begin->length;
    if(end)
        total += end->length;
    int status = fx_make_str(0, total, result);
    if(status < 0) return status;

    size_t szch = sizeof(result->data[0]);
    int_ ofs = 0;
    if (beginlen) {
        memcpy(result->data, begin->data, beginlen*szch);
        ofs = beginlen;
    }
    for( i = 0; i < count; i++ ) {
        if (i > 0 && seplen > 0) {
            memcpy(result->data + ofs, sep->data, seplen*szch);
            ofs += seplen;
        }
        int_ len_i = s[i].length;
        memcpy(result->data + ofs, s[i].data, len_i*szch);
        ofs += len_i;
    }
    if (endlen) {
        memcpy(result->data + ofs, end->data, endlen*szch);
    }
    return FX_OK;
}

#ifdef __cplusplus
}
#endif

#endif
