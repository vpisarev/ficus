/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some basic functions for implementing lexers

import File

type lloc_t = (int, int)
exception LexerError : (lloc_t, string)

type stream_t =
{
    fname: string;  // filename used for diagnostic messages
    var lineno: int;// the current line number (counted from 1)
    var bol: int;   // beginning-of-line. The index of the first character
                    // of the current line (counted from 0)
    buf: string;    // the stream content; currently we do not support reading streams
                    // iteratively. The whole file is loaded into memory and converted
                    // from UTF-8 or whatever encoding at once.
                    // In principle, it should not be difficult to replace it with a
                    // callback that reads the stream line by line.
}

fun make_stream(fname: string)
{
    val buf = File.read_utf8(fname)
    stream_t { fname=fname, lineno=1, bol=0, buf=buf }
}

fun make_stream_from_string(fname: string, buf: string)
{
    stream_t { fname=fname, lineno=1, bol=0, buf=buf }
}

fun skip_spaces(s: stream_t, pos: int, allow_nested: bool)
{
    val lineno0 = s.lineno
    var lineno = lineno0
    var bol = s.bol
    val buf = s.buf
    val len = buf.length()
    var inside_comment = 0
    var inside_eol_comment = false
    var c_res = '\0'
    var pos = pos

    while pos < len {
        val c = buf[pos]
        pos += 1
        val c1 = buf.zero[pos]
        if c == '\n' || (c == '\r' && c1 != '\n') {
            lineno += 1
            bol = pos
            inside_eol_comment = false
        } else if c == ' ' || c == '\t' || c == '\r' || inside_eol_comment {
        } else if inside_comment > 0 {
            if c == '*' && c1 == '/' {
                inside_comment -= 1
                pos += 1
            } else if c == '/' && c1 == '*' && allow_nested {
                inside_comment += 1
                pos += 1
            }
        } else if c == '/' && c1 == '*' {
            inside_comment = 1
        } else if c == '/' && c1 == '/' {
            inside_eol_comment = true
        } else {
            c_res = c
            pos -= 1
            break
        }
    }

    s.lineno = lineno
    s.bol = bol
    if inside_comment > 0 {
        throw LexerError((lineno, pos-bol), "unterminated comment")
    }
    (c_res, pos, lineno > lineno0)
}

/* Extracts an integer or floating-point number literal.
   The number should not have a sign in front and must start with a digit.
   0x, 0b, 0o/0 prefixes are supported.
   h, f, uNN, iNN, L and UL suffixes are also supported.
*/
@pure fun getnumber(s: string, pos: int, just_int: bool, get_suffix: bool):
    (int, int64, double, int, char)
@ccode {
    const int MAX_ATOF = 128;
    char buf[128 + 16];
    int_ i = 0, len = s->length - pos;
    const char_ *ptr = s->data + pos;
    bool ok = true;
    uint64_t r = 0, r1;
    int base = 10;
    bool flt = false, have_dot = false, have_e = false;
    char_ c = '\0';

    if(len <= 0)
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);

    if( ptr[0] == '0' ) {
        if(len > 2 && (ptr[1] == 'x' || ptr[1] == 'X')) {
            base = 16;
            ptr += 2;
            len -= 2;
        } else if(len > 2 && (ptr[1] == 'b' || ptr[1] == 'B')) {
            base = 2;
            ptr += 2;
            len -= 2;
        } else if(len > 2 && (ptr[1] == 'o' || ptr[1] == 'O')) {
            base = 8;
            ptr += 2;
            len -= 2;
        } else if(!just_int && len >= 2 && (ptr[1] == '.' || ptr[1] == 'e' || ptr[1] == 'E')) {
            flt = true;
        }
        else base = 8;
    }

    if( !flt ) {
        if( base == 10 ) {
            for( i = 0; i < len; i++ ) {
                c = ptr[i];
                if(c < '0' || c > '9')
                    break;
                r1 = r*10 + (c - '0');
                if (r1 < r)
                    ok = false; // do not exit the loop, because it can be
                                // a floating-point literal
                buf[i] = (char)c;
                if( i >= MAX_ATOF )
                    return FX_SET_EXN_FAST(FX_EXN_OverflowError);
                r = r1;
            }
            if( !just_int && (c == '.' || c == 'e' || c == 'E') )
                flt = true;
        } else {
            for( i = 0; i < len; i++ ) {
                c = ptr[i];
                int digit = 0;
                if('0' <= c && c <= '9')
                    digit = (int)(c - '0');
                else if('a' <= c && c <= 'f')
                    digit = (int)(c - 'a' + 10);
                else if('A' <= c && c <= 'F')
                    digit = (int)(c - 'A' + 10);
                else
                    break;
                if(digit >= base)
                    break;
                r1 = r*base + digit;
                if (r1 < r) {
                    ok = false;
                    break;
                }
                r = r1;
            }
        }
    }

    if(!flt) {
        int bits=0;
        bool unsigned_ = false;
        if (get_suffix) {
            uint64_t maxval = (uint64_t)1 << (sizeof(size_t)*8-1);
            if( c == 'L' ) {
                bits = 128;
                i++;
            } else if( c == 'i' || c == 'I' || c == 'u' || c == 'U' ) {
                unsigned_ = c == 'u' || c == 'U';
                char_ c1 = i+1 < len ? ptr[i+1] : (char_)'\0';
                char_ c2 = i+2 < len ? ptr[i+2] : (char_)'\0';
                char_ c3 = i+3 < len ? ptr[i+3] : (char_)'\0';

                if(c1 == '8' && (c2 < '0' || '9' < c2)) {
                    bits = 8;
                    maxval = unsigned_ ? 255 : 128;
                    i += 2;
                } else if(c1 == '1' && c2 == '6' && (c3 < '0' || '9' < c3)) {
                    bits = 16;
                    maxval = unsigned_ ? 65535 : 32768;
                    i += 3;
                } else if(c1 == '3' && c2 == '2' && (c3 < '0' || '9' < c3)) {
                    bits = 32;
                    maxval = unsigned_ ? 0xFFFFFFFFULL : 0x80000000ULL;
                    i += 3;
                } else if(c1 == '6' && c2 == '4' && (c3 < '0' || '9' < c3)) {
                    bits = 64;
                    maxval = unsigned_ ? 0xFFFFFFFFFFFFFFFFULL : 0x8000000000000000ULL;
                    i += 3;
                } else
                    return FX_SET_EXN_FAST(FX_EXN_BadArgError);
            }
            if(bits <= 64 && (!ok || r > maxval))
                return FX_SET_EXN_FAST(FX_EXN_OutOfRangeError);
        }
        fx_result->t0 = (ptr - s->data) + i;
        fx_result->t1 = (int64_t)r;
        fx_result->t2 = 0.;
        fx_result->t3 = bits;
        fx_result->t4 = unsigned_ ? 'u' : 'i';
        return FX_OK;
    }

    for(; i < len; i++) {
        c = ptr[i];
        if('0' <= c && c <= '9') {
        } else if(c == '.') {
            if(have_e || have_dot)
                break;
            have_dot = true;
        } else if(c == 'e' || c == 'E') {
            if(have_e)
                break;
            have_e = true;
        } else if(c == '+' || c == '-') {
            if(i == 0 || (ptr[i-1] != 'e' && ptr[i-1] != 'E'))
                break;
        } else
            break;
        buf[i] = (char)c;
        if(i >= MAX_ATOF) {
            ok = false;
            break;
        }
    }

    if (ok) {
        int bits = 64;
        char* endptr = 0;
        buf[i] = '\0';
        fx_result->t2 = strtod(buf, &endptr);
        ok = endptr == buf + i;
        if (c == 'f' || c == 'F') {
            bits = 32;
            endptr++;
        } else if (c == 'h' || c == 'H') {
            bits = 16;
            endptr++;
        }
        fx_result->t0 = (ptr - s->data) + (endptr - buf);
        fx_result->t1 = 0;
        fx_result->t3 = bits;
        fx_result->t4 = 'f';
    }
    return ok ? FX_OK : FX_SET_EXN_FAST(FX_EXN_OverflowError);
}

@ccode
{
    static int decodeoct(char_ c)
    {
        return ('0' <= c && c <= '7') ? c - '0' : -1;
    }
    static int decodehex(char_ c)
    {
        return ('0' <= c && c <= '9') ? c - '0' :
                ('a' <= c && c <= 'f') ? c - 'a' + 10 :
                ('A' <= c && c <= 'F') ? c - 'A' + 10 : -1;
    }
}

/*
    Parses the string (term is double quote)
    or character (term is single quote) literal.

    Python's f"" and r"" types of string literals are supported.
    In the case of f"" string the decoding stops when '{' occurs and
    so-far decoded part of the string is returned.

    Special characters, such as \n, \t etc. are automatically decoded
    (unless r"" is used).
    Numerically specified ASCII characters (\xNN, \0[N...]) and
    Unicode characters (\uxxxx and \Uxxxxxxxx) are also decoded.
*/
fun getstring_(s: string, pos: int, term: char, raw: bool, fmt: bool):
    (int, string, int, bool)
@ccode {
    int delta_lines = 0;
    int_ sz = 256, n = 0;
    char_ buf0[256 + 32];
    char_* buf = buf0;
    int_ i = 0, j, len = s->length - pos;
    const char_ *ptr = s->data + pos;
    char_ c;
    bool inline_exp = false;

    if(len <= 0)
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);

    for( i = 0; i < len; i++ ) {
        c = ptr[i];
        if (c == term) {
            i++;
            break;
        }
        if (c == 92) { // backslash
            if ((i+1 < len && ptr[i+1] == 10) || (i+2 < len && ptr[i+1] == 13 && ptr[i+2] == 10)) {
                for (++i; i < len; i++) {
                    c = ptr[i];
                    if (c == 10) delta_lines++;
                    if (c != 32 && c != 9 && c != 10 && c != 13)
                        break;
                }
                i--;
                continue;
            }
            if(raw) {
                if (i+1 < len && (ptr[i+1] == 34 || ptr[i+1] == 39)) { // quote
                    buf[n++] = 92;
                    buf[n++] = ptr[i+1];
                    i++;
                } else
                    buf[n++] = (char_)92; // backslash
            }
            else {
                if(++i >= len)
                    return FX_SET_EXN_FAST(FX_EXN_OverflowError);
                c = ptr[i];
                if(c == 'n')
                    c = '\n';
                else if(c == 'r')
                    c = '\r';
                else if(c == 'e')
                    c = 27;
                else if(c == 't')
                    c = '\t';
                else if(c == 'b') // backspace
                    c = 8;
                else if(c == ' ')
                    c = ' ';
                else if(c == 39) // single quote
                    c = (char_)39;
                else if(c == 34) // double quote
                    c = (char_)34;
                else if(c == 92) // backslash
                    c = (char_)92;
                else if(c == 'x') {
                    int x0=0, x1=0;
                    if( i+2 >= len || (x0 = decodehex(ptr[i+1])) < 0 ||
                        (x1 = decodehex(ptr[i+2])) < 0 )
                        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                    c = x0*16+x1;
                    i++;
                } else if('0' <= c && c <= '7') {
                    c = 0;
                    for(j = 0; j < 4 && i < len; j++, i++) {
                        int d = decodeoct(ptr[i]);
                        if(d < 0)
                            break;
                        c = (char_)(c*8 + d);
                    }
                    if(i >= len)
                        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                    i--;
                } else if(c == 'u' || c == 'U') {
                    int K = c == 'u' ? 4 : 8;
                    c = 0;
                    i++;
                    for(j = 0; j < K && i < len; j++, i++) {
                        int x = decodehex(ptr[i]);
                        if(x < 0)
                            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                        c = (char_)(c*16 + x);
                    }
                    if(i >= len || c > 1114111)
                        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                    i--;
                } else
                    return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                buf[n++] = c;
            }
        } else if(fmt && c == 123 && i+1 < len && ptr[i+1] != 34) { // { or {{
            if (ptr[i+1] == 123) { i++; buf[n++] = 123; }
            else {
                inline_exp = true;
                i++;
                break;
            }
        } else if(fmt && c == 125 && i+1 < len && ptr[i+1] == 125) { // }}
            i++; buf[n++] = 125;
        } else if(c == 10 || (c == 13 && i+1 < len && ptr[i+1] == 10)) {
            delta_lines += 1;
            i += c == 13;
            buf[n++] = 10;
        } else {
            buf[n++] = c;
        }
        if( n >= sz ) {
            sz = sz*3/2;
            char_* buf1 = (char_*)fx_malloc(sz*sizeof(buf[0]));
            if(buf1) memcpy(buf1, buf, n*sizeof(buf[0]));
            if(buf != buf0) fx_free(buf);
            buf = buf1;
            if(!buf1)
                return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
        }
    }
    if (i == len)
        return FX_SET_EXN_FAST(FX_EXN_OverflowError);
    int fx_status = fx_make_str(buf, n, &fx_result->t1);
    if(buf != buf0) fx_free(buf);
    fx_result->t0 = (ptr - s->data) + i;
    fx_result->t2 = delta_lines;
    fx_result->t3 = inline_exp;
    return fx_status;
}

fun getstring(s: string, pos: int, loc: lloc_t, term: char, raw: bool, fmt: bool) =
    try {
        getstring_(s, pos, term, raw, fmt)
    } catch {
        | OverflowError => throw LexerError(loc, "unterminated string")
        | BadArgError => throw LexerError(loc, "unvalid escape sequence")
        | e => throw LexerError(loc, f"exception {e} occured when parsing string literal")
    }
