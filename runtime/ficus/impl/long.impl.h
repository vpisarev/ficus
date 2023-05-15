/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

#ifndef __FICUS_LONG_IMPL_H__
#define __FICUS_LONG_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned __int128 fx_wdigit_t;
#define FX_LONG_DSHIFT 64
#define FX_LONG_ONES 0xFFFFFFFFFFFFFFFFULL

#define FX_LONG_SIGNBITS(digits, len) \
    (fx_digit_t)((len) > 0 ? (fx_sdigit_t)(digits)[(len)-1] >> (FX_LONG_DSHIFT-1) : 0LL)
#define FX_LONG_BUFSIZE (1 << 9)

void fx_free_long(fx_long_t* num)
{
    if( ((num->length > FX_LONG_LDIGITS) & (num->u.vl.rc != 0)) != 0 )
    {
        if(FX_DECREF(*num->u.vl.rc) == 1)
            fx_free(num->u.vl.rc);
    }
    num->length = 0;
}

void fx_copy_long(const fx_long_t* src, fx_long_t* dst)
{
    FX_COPY_LONG(src, dst);
}

static int_ _fx_long_shrink(const fx_digit_t* digits, int_ len)
{
    if (len > 0) {
        fx_digit_t d0 = digits[len-1];
        if (d0+1 <= 1) {
            while(len > 0 && digits[len-1] == d0)
                --len;
            len += (fx_sdigit_t)((len > 0 ? digits[len-1] : 0) ^ d0) < 0;
        }
    }
    return len;
}

static int_ _fx_ulong_shrink(const fx_digit_t* digits, int_ len)
{
    while (len > 0 && digits[len-1] == 0)
        len--;
    return len;
}

static int _fx_make_long(const fx_digit_t* digits, int_ len, fx_long_t* num)
{
    len = _fx_long_shrink(digits, len);
    size_t numsize = len*sizeof(digits[0]);
    if (len <= FX_LONG_LDIGITS) {
        memcpy(num->u.digits, digits, numsize);
    } else {
        num->u.vl.rc = (int_*)fx_malloc(sizeof(*num->u.vl.rc) + numsize);
        if (!num->u.vl.rc)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
        *num->u.vl.rc = 1;
        num->u.vl.digits = (fx_digit_t*)(num->u.vl.rc + 1);
        memcpy(num->u.vl.digits, digits, numsize);
    }
    num->length = len;
    return FX_OK;
}

static int _fx_temp2long(fx_digit_t* digits, int_ len,
                         int sign, fx_long_t* num)
{
    if (sign < 0 && len > 0) {
        fx_digit_t carry = 1;
        for (int_ i = 0; i < len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)~digits[i] + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        digits[len++] = (fx_digit_t)(FX_LONG_ONES + carry);
    } else {
        digits[len] = 0;
        len += (fx_sdigit_t)digits[len-1] < 0;
    }
    return _fx_make_long(digits, len, num);
}

// supports in-place (out == inp) mode
static int_ _fx_ulong_muladd1(const fx_digit_t* a_digits, int_ a_len,
                              fx_digit_t scale, fx_digit_t delta,
                              fx_digit_t* digits)
{
    fx_digit_t carry = delta;
    for (int_ i = 0; i < a_len; i++) {
        fx_wdigit_t temp = (fx_wdigit_t)a_digits[i]*scale + carry;
        digits[i] = (fx_digit_t)temp;
        carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
    }
    digits[a_len] = carry;
    return a_len + (carry > 0);
}

// supports in-place (out == inp) mode
static int_ _fx_ulong_divrem1(const fx_digit_t* a_digits, int_ a_len,
                              fx_digit_t divisor,
                              fx_digit_t* digits, fx_digit_t* rem)
{
    fx_digit_t carry = 0;
    for (int_ i = a_len-1; i >= 0; i--) {
        fx_wdigit_t temp = ((fx_wdigit_t)carry << FX_LONG_DSHIFT) | a_digits[i];
        fx_digit_t res = (fx_digit_t)(temp / divisor);
        carry = (fx_digit_t)(temp - (fx_wdigit_t)res*divisor);
        digits[i] = res;
    }
    if (rem)
        *rem = carry;
    return a_len - (digits[a_len-1] == 0);
}

static int _fx_long2temp_abs(const fx_long_t* a, bool always_copy, fx_digit_t* buf, int_ bufsize,
                             fx_digit_t** digits_, int_* len_, int* sign_)
{
    int_ a_len = a->length;
    const fx_digit_t* a_digits = FX_LONG_DIGITS(a);
    fx_digit_t* digits = buf;
    int a_sign = 0;

    if (a_len > 0 && (a_len != 1 || a_digits[0] != 0)) {
        fx_sdigit_t d = (fx_sdigit_t)a_digits[a_len-1];
        a_sign = (d >= 0) - (d < 0);
    }
    *sign_ = a_sign;
    if (a_sign < 0 || always_copy) {
        if (a_len > bufsize) {
            digits = (fx_digit_t*)fx_malloc(a_len*sizeof(digits[0]));
            if (!digits)
                FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
        } else
            digits = buf;
        *digits_ = digits;
    }

    if (a_sign >= 0) {
        a_len -= (a_len > 0 && a_digits[a_len-1] == 0);
        if (always_copy)
            memcpy(digits, a_digits, a_len*sizeof(digits[0]));
        else
            *digits_ = (fx_digit_t*)a_digits;
    } else {
        fx_digit_t carry = 1;
        for (int_ i = 0; i < a_len; i++) {
            fx_digit_t d = a_digits[i];
            fx_wdigit_t temp = (fx_wdigit_t)~d + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
    }
    *len_ = a_len;
    return FX_OK;
}

int fx_atol(const fx_str_t* str, int base, fx_long_t* num)
{
    const int_ group_bits = (int_)(sizeof(fx_digit_t)*8);
    fx_digit_t buf[FX_LONG_BUFSIZE];
    fx_digit_t group=0, *digits = buf;
    int_ i = 0, strlen = str->length, maxlen, len = 0;
    int_ groupsize = 0, maxgroupsize;
    const char_ *strdata = str->data;
    int sign = 1;
    double log2_base = 3.321928094888; // rounded to an upper value
    int_ nchars_per_group = 9 + (group_bits > 32)*10;
    fx_digit_t group_scale = 10000000000000000000ULL;
    bool fastmode = false;
    int fx_status = FX_OK;

    if (i < strlen && strdata[i] == '-') {
        sign = -1;
        i++;
    }

    if (base == 0) {
        base = 10;
        if (i+1 < strlen && strdata[i] == '0') {
            if (strdata[i+1] == 'x') {
                fastmode = true;
                nchars_per_group = group_bits/4;
                log2_base = 4;
                base = 16;
                i += 2;
            } else if (strdata[i+1] == 'b') {
                fastmode = true;
                nchars_per_group = group_bits;
                log2_base = 1;
                base = 2;
                i += 2;
            } else if (strdata[i+1] == 'o' || fx_isdigit(strdata[i+1])) {
                nchars_per_group = group_bits/3;
                group_scale = 8LL << nchars_per_group;
                log2_base = 3;
                base = 8;
                i += 1 + strdata[i+1] == 'o';
            }
        }
    }

    if (strlen - i <= nchars_per_group)
        maxlen = 2;
    else {
        maxlen = (int_)ceil((strlen - i)*log2_base/group_bits) + 1;
        if (maxlen > FX_LONG_BUFSIZE) {
            digits = fx_malloc(maxlen*sizeof(digits[0]));
            if (!digits)
                FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
        }
    }
    maxgroupsize = (strlen - i) % nchars_per_group;
    maxgroupsize += (maxgroupsize == 0)*nchars_per_group;

    for (; i < strlen; i++) {
        char_ c = strdata[i];
        int digit = base;
        if('0' <= c && c <= '9')
            digit = (int)(c - '0');
        else if(c >= 'a')
            digit = (int)(c - 'a' + 10);
        else if(c >= 'A')
            digit = (int)(c - 'A' + 10);
        if(digit >= base)
            break;
        if(digit >= base)
            break;
        group = group*base + digit;
        if (++groupsize >= maxgroupsize || i+1 == strlen) {
            if (fastmode)
                digits[len++] = group;
            else
                len = _fx_ulong_muladd1(digits, len,
                                        group_scale, group, digits);
            group = 0;
            groupsize = 0;
            maxgroupsize = nchars_per_group;
        }
    }

    if (i < strlen || i == 0)
        fx_status = FX_SET_EXN_FAST(FX_EXN_BadArgError);

    if (fx_status >= 0)
    {
        if (fastmode)
            for (int_ i = 0; i < len/2; i++) {
                fx_digit_t t;
                FX_SWAP(digits[i], digits[len-i-1], t);
            }
        fx_status = _fx_temp2long(digits, len, sign, num);
    }
    if (digits != buf)
        fx_free(digits);
    return fx_status;
}

static int _fx_ltoa(const fx_long_t* a, char basec, bool add_prefix, bool ascii, void* str_, int_* strlen_)
{
    fx_digit_t buf[FX_LONG_BUFSIZE];
    fx_digit_t* a_digits = 0;
    char* str_ascii = (char*)str_;
    char_* str_uni = (char_*)str_;
    int_ i, a_len = 0, strlen = 0;
    int a_sign = 0;
    int status = _fx_long2temp_abs(a, true, buf, FX_LONG_BUFSIZE, &a_digits, &a_len, &a_sign);
    int_ prefixlen = 0;
    if (status < 0)
        return status;
    if (a_len == 0) {
        if (ascii)
            str_ascii[strlen++] = '0';
        else
            str_uni[strlen++] = '0';
    } else {
        const int_ group_bits = (int_)(sizeof(fx_digit_t)*8);
        fx_digit_t group_scale = 10000000000000000000ULL;
        int_ group_size = 9 + (group_bits > 32)*10;
        assert(basec == 'd');
        if (a_sign < 0) {
            if (ascii)
                str_ascii[strlen++] = '-';
            else
                str_ascii[strlen++] = '-';
        }
        while (a_len > 0) {
            fx_digit_t group = 0;
            a_len = _fx_ulong_divrem1(a_digits, a_len, group_scale, a_digits, &group);
            for (i = 0; i < group_size; i++) {
                fx_digit_t q = group / 10;
                char c = (char)(group - q*10 + '0');
                if (ascii)
                    str_ascii[strlen++] = c;
                else
                    str_uni[strlen++] = c;
                if (a_len == 0 && q == 0)
                    break;
                group = q;
            }
        }
        prefixlen += a_sign < 0;
        int_ ndigits = strlen - prefixlen;
        if (ascii) {
            char* str_digits = str_ascii + prefixlen;
            for (i = 0; i < ndigits/2; i++) {
                char t;
                FX_SWAP(str_digits[i], str_digits[ndigits-i-1], t);
            }
        } else {
            char_* str_digits = str_uni + prefixlen;
            for (i = 0; i < ndigits/2; i++) {
                char_ t;
                FX_SWAP(str_digits[i], str_digits[ndigits-i-1], t);
            }
        }
    }
    *strlen_ = strlen;
    return status;
}

static int _fx_ltoa_maxlen(const fx_long_t* a, char basec, bool add_prefix, int_* maxlen_)
{
    int_ a_len = a->length;
    int_ maxlen = (a_len > 0) & ((fx_sdigit_t)FX_LONG_DIGITS(a)[a_len-1] < 0);
    if (basec == 'd')
        maxlen += ceil(19.265919722494797*a_len);
    else if (basec == 'x' || basec == 'X')
        maxlen += a_len*16 + (add_prefix & (a_len > 0))*2;
    else if (basec == 'o')
        maxlen += (a_len*64 + 2)/3 + (add_prefix & (a_len > 0));
    else if (basec == 'b')
        maxlen += a_len*64 + (add_prefix & (a_len > 0))*2;
    else
        FX_FAST_THROW_RET(FX_EXN_BadArgError);
    *maxlen_ = maxlen;
    return FX_OK;
}

int fx_ltoa(const fx_long_t* a, char basec, bool add_prefix, fx_str_t* str)
{
    assert(sizeof(fx_digit_t) == 8);

    enum {STRBUF_SIZE = 1 << 10};
    char_ strbuf[STRBUF_SIZE];
    char_* digits = strbuf;
    int_ strlen = 0, maxlen = 0;
    int status = _fx_ltoa_maxlen(a, basec, add_prefix, &maxlen);
    if (maxlen < 0)
        return (int)maxlen;

    str->data = 0;
    if (maxlen > STRBUF_SIZE) {
        status = fx_make_str(0, maxlen, str);
        if (status < 0)
            return status;
        digits = str->data;
    }
    status = _fx_ltoa(a, basec, add_prefix, false, digits, &strlen);
    if (digits == strbuf)
        status = fx_make_str(digits, strlen, str);
    else
        str->length = strlen;
    return status;
}

int fx_ltoa_ascii(const fx_long_t* a, char basec, bool add_prefix, fx_cstr_t* str)
{
    assert(sizeof(fx_digit_t) == 8);

    enum {STRBUF_SIZE = 1 << 10};
    char strbuf[STRBUF_SIZE];
    char* digits = strbuf;
    int_ strlen = 0, maxlen = 0;
    int status = _fx_ltoa_maxlen(a, basec, add_prefix, &maxlen);
    if (maxlen < 0)
        return (int)maxlen;

    str->data = 0;
    if (maxlen > STRBUF_SIZE) {
        status = fx_make_cstr(0, maxlen, str);
        if (status < 0)
            return status;
        digits = str->data;
    }
    status = _fx_ltoa(a, basec, add_prefix, true, digits, &strlen);
    if (digits == strbuf)
        status = fx_make_cstr(digits, strlen, str);
    else
        str->length = strlen;
    return status;
}

int fx_format_long(const fx_long_t* x, const fx_format_t* fmt, fx_str_t* result)
{
    FX_FAST_THROW_RET(FX_EXN_NotImplementedError);
}

int_ fx_ltoi(const fx_long_t* a)
{
    return (int_)FX_LONG_DIGITS(a)[0];
}

int fx_long_abs(const fx_long_t* a, fx_long_t* res)
{
    if (a->length == 0 || (fx_sdigit_t)FX_LONG_DIGITS(a)[a->length-1] >= 0) {
        fx_copy_long(a, res);
        return FX_OK;
    }
    return fx_long_neg(a, res);
}

int fx_long_neg(const fx_long_t* a, fx_long_t* res)
{
    fx_digit_t buf[FX_LONG_BUFSIZE];
    fx_digit_t* digits = buf;
    int_ i, a_len = a->length;
    fx_digit_t carry = 1, a_sbits;
    const fx_digit_t* a_digits;

    if (a_len == 0) {
        fx_copy_long(a, res);
        return FX_OK;
    }

    if (a_len + 1 > FX_LONG_BUFSIZE) {
        digits = (fx_digit_t*)fx_malloc((a_len + 1)*sizeof(digits[0]));
        if (!digits)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }

    a_digits = FX_LONG_DIGITS(a);
    a_sbits = FX_LONG_SIGNBITS(a_digits, a_len);

    for (i = 0; i < a_len; i++) {
        fx_digit_t d = a_digits[i];
        fx_wdigit_t temp = (fx_wdigit_t)~d + carry;
        digits[i] = (fx_digit_t)temp;
        carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
    }
    digits[a_len] = (fx_digit_t)(~a_sbits + carry);
    int status = _fx_make_long(digits, a_len+1, res);
    if (digits != buf)
        fx_free(digits);
    return status;
}

int fx_long_add(const fx_long_t* a, const fx_long_t* b, fx_long_t* res)
{
    fx_digit_t buf[FX_LONG_BUFSIZE];
    fx_digit_t* digits = buf;
    int_ i, a_len, b_len;
    const fx_digit_t* a_digits;
    const fx_digit_t* b_digits;
    fx_digit_t carry = 0;
    fx_digit_t a_sbits, b_sbits;

    if (a->length < b->length) {
        const fx_long_t* t;
        FX_SWAP(a, b, t);
    }

    a_len = a->length;
    b_len = b->length;

    if (b_len == 0) {
        fx_copy_long(a, res);
        return FX_OK;
    }

    if (a_len + 1 > FX_LONG_BUFSIZE) {
        digits = (fx_digit_t*)fx_malloc((a_len + 1)*sizeof(digits[0]));
        if (!digits)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }

    a_digits = FX_LONG_DIGITS(a);
    b_digits = FX_LONG_DIGITS(b);
    a_sbits = FX_LONG_SIGNBITS(a_digits, a_len);
    b_sbits = FX_LONG_SIGNBITS(b_digits, b_len);

    for (i = 0; i < b_len; i++) {
        fx_wdigit_t temp = (fx_wdigit_t)a_digits[i] + b_digits[i] + carry;
        digits[i] = (fx_digit_t)temp;
        carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
    }

    for (; i < a_len; i++) {
        fx_wdigit_t temp = (fx_wdigit_t)a_digits[i] + b_sbits + carry;
        digits[i] = (fx_digit_t)temp;
        carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
    }

    digits[a_len] = a_sbits + b_sbits + carry;
    int status = _fx_make_long(digits, a_len+1, res);
    if (digits != buf)
        fx_free(digits);
    return status;
}

int fx_long_sub(const fx_long_t* a, const fx_long_t* b, fx_long_t* res)
{
    fx_digit_t buf[FX_LONG_BUFSIZE];
    fx_digit_t* digits = buf;
    int_ i, a_len, b_len, len;
    const fx_digit_t* a_digits;
    const fx_digit_t* b_digits;
    fx_digit_t carry = 0;
    fx_digit_t a_sbits, b_sbits;

    a_len = a->length;
    b_len = b->length;

    if (b_len == 0) {
        fx_copy_long(a, res);
        return FX_OK;
    } else if (a_len == 0) {
        return fx_long_neg(b, res);
    }

    len = FX_MAX(a_len, b_len) + 1;

    if (len + 1 > FX_LONG_BUFSIZE) {
        digits = (fx_digit_t*)fx_malloc((len + 1)*sizeof(digits[0]));
        if (!digits)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }

    a_digits = FX_LONG_DIGITS(a);
    b_digits = FX_LONG_DIGITS(b);
    a_sbits = FX_LONG_SIGNBITS(a_digits, a_len);
    b_sbits = FX_LONG_SIGNBITS(b_digits, b_len);

    if (a_len >= b_len) {
        for (i = 0; i < b_len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)a_digits[i] - b_digits[i] + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }

        for (; i < a_len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)a_digits[i] - b_sbits + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
    } else {
        for (i = 0; i < a_len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)a_digits[i] - b_digits[i] + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }

        for (; i < b_len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)a_sbits - b_digits[i] + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
    }

    digits[len] = a_sbits - b_sbits + carry;
    int status = _fx_make_long(digits, len+1, res);
    if (digits != buf)
        fx_free(digits);
    return status;
}

// compute temporary buffer size to multiply a*b
// (provided that the output buffer is pre-allocated)
static int_ _fx_long_karatsuba_bufsize(int_ a_len, int_ b_len)
{
    assert(a_len >= b_len);
    if (b_len <= 3)
        return 0;
    int_ a_len0 = (a_len+1)/2, a_len1 = a_len - a_len0;
    int_ b_len0 = FX_MIN(b_len, a_len0), b_len1 = b_len - b_len0;

    // we need to compute a0*b0, a1*b1, (a0+a1)*(b0+b1) - a0*b0 - a1*b1
    int_ a0b0 = a_len0 + b_len0;
    int_ bufsize_a0b0 = _fx_long_karatsuba_bufsize(a_len0, b_len0);
    int_ a1b1 = a_len1 + b_len1;
    int_ bufsize_a1b1 =
        a_len0 == a_len1 && b_len0 == b_len1 ? bufsize_a0b0 :
        _fx_long_karatsuba_bufsize(a_len1, b_len1);
    int_ a01b01 = a_len0 + b_len0 + 2;
    int_ bufsize_a01b01 = _fx_long_karatsuba_bufsize(a_len0 + 1, b_len0 + 1);

    int_ bufsize = a0b0 + FX_MAX(bufsize_a0b0, a1b1 + bufsize_a1b1);
    bufsize = a01b01 + FX_MAX(bufsize, a_len0 + 1 + b_len0 + 1 + bufsize_a01b01);
    return bufsize;
}

static int _fx_long_karatsuba(const fx_digit_t* a_digits, int_ a_len,
                              const fx_digit_t* b_digits, int_ b_len,
                              fx_digit_t* digits, fx_digit_t* buf, int_ spaceleft)
{
    int_ i, j, len = a_len + b_len;
    int status = FX_OK;
    if (b_len <= 3) {
        // base of the recursion - "school" algorithm
        if (b_len == 1) {
            _fx_ulong_muladd1(a_digits, a_len, b_digits[0], 0, digits);
        } else {
            memset(digits, 0, len*sizeof(digits[0]));
            for (i = 0; i < b_len; i++) {
                fx_digit_t bi = b_digits[i], carry = digits[i];
                for (j = 0; j < a_len; j++) {
                    fx_wdigit_t temp = (fx_wdigit_t)a_digits[j]*bi + digits[i+j] + carry;
                    digits[i+j] = (fx_digit_t)temp;
                    carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
                }
                for (; i+j < len; j++) {
                    fx_wdigit_t temp = (fx_wdigit_t)digits[i+j] + carry;
                    digits[i+j] = (fx_digit_t)temp;
                    carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
                }
            }
        }
    } else {
        int_ a_len0 = (a_len+1)/2, a_len1 = a_len - a_len0;
        int_ b_len0 = FX_MIN(b_len, a_len0), b_len1 = b_len - b_len0;
        int_ z1_len = a_len0 + b_len0 + 2;
        int_ z0_len = a_len0 + b_len0;
        int_ z2_len = a_len1 + b_len1;
        fx_digit_t* z1 = buf;
        fx_digit_t* a01 = z1 + z1_len;
        fx_digit_t* b01 = a01 + a_len0 + 1;
        fx_digit_t* temp_z1 = b01 + b_len0 + 1;
        fx_digit_t* z0 = a01;
        fx_digit_t* z2 = z0 + z0_len;
        fx_digit_t* temp_z0 = z2;
        fx_digit_t* temp_z2 = z2 + z2_len;
        fx_digit_t carry = 0;

        if (temp_z1 > buf + spaceleft || temp_z2 > buf + spaceleft)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);

        // a0 + a1
        for (i = 0; i < a_len1; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)a_digits[i] + a_digits[i + a_len0] + carry;
            a01[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        for (; i < a_len0; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)a_digits[i] + carry;
            a01[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        a01[i] = carry;
        // b0 + b1
        carry = 0;
        for (i = 0; i < b_len1; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)b_digits[i] + b_digits[i + b_len0] + carry;
            b01[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        for (; i < b_len0; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)b_digits[i] + carry;
            b01[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        b01[i] = carry;
        // z1 = (a0 + a1)*(b0 + b1)
        status = _fx_long_karatsuba(a01, a_len0 + 1, b01, b_len0 + 1,
                                    z1, temp_z1, spaceleft - (temp_z1 - buf));
        if (status >= 0)
            // z0 = a0*b0
            status = _fx_long_karatsuba(a_digits, a_len0, b_digits, b_len0,
                                        z0, temp_z0, spaceleft - (temp_z0 - buf));
        if (status >= 0)
            // z2 = a1*b1
            status = _fx_long_karatsuba(a_digits + a_len0, a_len1, b_digits + b_len0, b_len1,
                                        z2, temp_z2, spaceleft - (temp_z2 - buf));
        if (status < 0)
            return status;
        // z1 -= z0 + z2 == a0*b1 + a1*b0
        carry = 0;
        for (i = 0; i < z2_len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)z1[i] - z0[i] - z2[i] + carry;
            z1[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        for (; i < z0_len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)z1[i] - z0[i] + carry;
            z1[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        for (; i < z1_len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)z1[i] + carry;
            z1[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        // z0 + z1*(base**a_len0) + z2*(base**(a_len0*2))
        memcpy(digits, z0, a_len0*sizeof(digits[0]));
        carry = 0;
        for (i = a_len0; i < z0_len; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)z1[i-a_len0] + z0[i] + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        for (; i < a_len0*2; i++) {
            fx_wdigit_t temp = (fx_wdigit_t)z1[i-a_len0] + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
        for (; i < len; i++) {
            int_ j1 = i - a_len0, j2 = i - a_len0*2;
            fx_digit_t f1 = -(j1 < z1_len), f2 = -(j2 < z2_len);
            fx_digit_t d1 = z1[j1&f1]&f1, d2 = z2[j2&f2]&f2;
            fx_wdigit_t temp = (fx_wdigit_t)d1 + d2 + carry;
            digits[i] = (fx_digit_t)temp;
            carry = (fx_digit_t)(temp >> FX_LONG_DSHIFT);
        }
    }
    return status;
}

int fx_long_mul(const fx_long_t* a, const fx_long_t* b, fx_long_t* res)
{
    fx_digit_t localbuf[FX_LONG_BUFSIZE];
    fx_digit_t* buf = localbuf;
    int_ i, a_len, b_len, len, res_offset0 = 0, bufsize = 0;
    fx_digit_t* a_digits;
    fx_digit_t* b_digits;
    fx_digit_t* digits;
    fx_digit_t d, carry = 0;
    int a_sign, b_sign;

    if (a->length < b->length) {
        const fx_long_t* t;
        FX_SWAP(a, b, t);
    }

    a_len = a->length;
    b_len = b->length;

    if (b_len == 0) {
        fx_copy_long(b, res);
        return FX_OK;
    }

    a_digits = (fx_digit_t*)FX_LONG_DIGITS(a);
    b_digits = (fx_digit_t*)FX_LONG_DIGITS(b);
    d = a_digits[a_len-1];
    a_sign = (d >= 0) - (d < 0);
    d = b_digits[b_len-1];
    b_sign = (d >= 0) - (d < 0);

    if (a_sign < 0)
        bufsize += a_len;
    if (b_sign < 0)
        bufsize += b_len;
    res_offset0 = bufsize;

    len = a_len + b_len;
    bufsize += len;
    if (a_len >= 3) {
        bufsize += _fx_long_karatsuba_bufsize(a_len, b_len);
        if (bufsize > FX_LONG_BUFSIZE) {
            buf = (fx_digit_t*)fx_malloc(bufsize*sizeof(buf[0]));
            if (!buf)
                FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
        }
    }
    if (a_sign < 0)
        _fx_long2temp_abs(a, false, buf, a_len, &a_digits, &a_len, &a_sign);
    if (b_sign < 0)
        _fx_long2temp_abs(b, false, buf + (a_sign < 0)*a_len, b_len, &b_digits, &b_len, &b_sign);

    digits = &buf[res_offset0];
    int_ temp_offset = res_offset0 + len;
    int status = _fx_long_karatsuba(a_digits, a_len, b_digits, b_len,
                                    digits, buf + temp_offset,
                                    bufsize - temp_offset);
    if (status >= 0)
        status = _fx_temp2long(digits, len, a_sign*b_sign, res);
    if (buf != localbuf)
        fx_free(buf);
    return status;
}

int fx_long_div(const fx_long_t* a, const fx_long_t* b, fx_long_t* res);
int fx_long_mod(const fx_long_t* a, const fx_long_t* b, fx_long_t* res);

static int _fx_long_bitwise(const fx_long_t* a, const fx_long_t* b,
                            char op, fx_long_t* res)
{
    fx_digit_t buf[FX_LONG_BUFSIZE];
    fx_digit_t* digits = buf;
    int_ i, a_len, b_len;
    const fx_digit_t* a_digits;
    const fx_digit_t* b_digits;
    fx_digit_t a_sbits, b_sbits;

    if (a->length < b->length) {
        const fx_long_t* t;
        FX_SWAP(a, b, t);
    }

    a_len = a->length;
    b_len = b->length;

    if (b_len == 0) {
        fx_copy_long((op == '&' ? b : a), res);
        return FX_OK;
    }

    if (a_len > FX_LONG_BUFSIZE) {
        digits = (fx_digit_t*)fx_malloc(a_len*sizeof(digits[0]));
        if (!digits)
            FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }

    a_digits = FX_LONG_DIGITS(a);
    b_digits = FX_LONG_DIGITS(b);
    b_sbits = FX_LONG_SIGNBITS(b_digits, b_len);

    if (op == '&') {
        for (i = 0; i < b_len; i++)
            digits[i] = a_digits[i] & b_digits[i];
        for (; i < a_len; i++)
            digits[i] = a_digits[i] & b_sbits;
    } else if (op == '|') {
        for (i = 0; i < b_len; i++)
            digits[i] = a_digits[i] | b_digits[i];
        for (; i < a_len; i++)
            digits[i] = a_digits[i] | b_sbits;
    } else {
        assert(op == '^');
        for (i = 0; i < b_len; i++)
            digits[i] = a_digits[i] ^ b_digits[i];
        for (; i < a_len; i++)
            digits[i] = a_digits[i] ^ b_sbits;
    }

    int fx_status = _fx_make_long(digits, a_len, res);
    if (digits != buf)
        fx_free(digits);
    return fx_status;
}

int fx_long_and(const fx_long_t* a, const fx_long_t* b, fx_long_t* res)
{
    return _fx_long_bitwise(a, b, '&', res);
}

int fx_long_or(const fx_long_t* a, const fx_long_t* b, fx_long_t* res)
{
    return _fx_long_bitwise(a, b, '|', res);
}

int fx_long_xor(const fx_long_t* a, const fx_long_t* b, fx_long_t* res)
{
    return _fx_long_bitwise(a, b, '^', res);
}

bool fx_long_eq(const fx_long_t* a, const fx_long_t* b)
{
    int_ i, a_len = a->length, b_len = b->length;
    const fx_digit_t *a_digits, *b_digits;

    if (a_len != b_len)
        return false;

    a_digits = FX_LONG_DIGITS(a);
    b_digits = FX_LONG_DIGITS(b);
    for (i = 0; i < a_len; i++) {
        if (a_digits[i] != b_digits[i])
            return false;
    }
    return true;
}

int fx_long_cmp(const fx_long_t* a, const fx_long_t* b)
{
    int a_sign = fx_long_sign(a), b_sign = fx_long_sign(b);
    if (a_sign != b_sign)
        return (a_sign > b_sign)*2 - 1;
    if (a_sign == 0)
        return 0;

    int_ i, a_len = a->length, b_len = b->length;
    const fx_digit_t *a_digits, *b_digits;
    if (a_len != b_len)
        return ((a_len > b_len)*2 - 1)*a_sign;
    a_digits = FX_LONG_DIGITS(a);
    b_digits = FX_LONG_DIGITS(b);

    for (i = a_len-1; i >= 0; i--) {
        fx_digit_t ai = a_digits[i], bi = b_digits[i];
        if (ai != bi)
            return ((ai > bi)*2 - 1)*a_sign;
    }
    return 0;
}

int fx_long_sign(const fx_long_t* a)
{
    fx_sdigit_t d;
    int_ len = a->length;
    if (((len == 0) | ((len == 1) & (a->u.digits[0] == 0))) != 0)
        return 0;
    d = (fx_sdigit_t)FX_LONG_DIGITS(a)[len-1];
    return (d >= 0) - (d < 0);
}

#ifdef __cplusplus
}
#endif

#endif
