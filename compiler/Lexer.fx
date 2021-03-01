/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Ficus lexer/tokenizer

import File, Map, Sys
import Ast

type lloc_t = (int, int)
exception LexerError : (lloc_t, string)

// Ficus tokens
type token_t =
    | LITERAL: Ast.lit_t | IDENT: (bool, string) | TYVAR: string
    | APOS | AS | AT | BREAK | CATCH | CCODE | CLASS | CONTINUE | DO
    | ELLIPSIS | ELSE | EXCEPTION | EXTENDS | FINALLY | FOLD
    | FOR: bool | FROM | FUN | IF | IMPLEMENTS | IMPORT: bool
    | INLINE | INTERFACE | MATCH | NOTHROW | OBJECT | OPERATOR
    | PARALLEL | PRAGMA | PURE | REF: bool | STATIC | THROW
    | TRY | TYPE | VAL | VAR | WHEN | WITH | WHILE: bool | UNZIP
    | LPAREN: bool | STR_INTERP_LPAREN | RPAREN
    | LSQUARE: bool | RSQUARE | LBRACE | RBRACE
    | LLIST | RLIST | COMMA | DOT | SEMICOLON | COLON
    | BAR | CONS | CAST | BACKSLASH | BACK_ARROW | DOUBLE_ARROW
    | ARROW | QUESTION | EOF | MINUS: bool | PLUS: bool
    | STAR: bool | SLASH | PERCENT | POWER: bool | DOT_STAR
    | DOT_MINUS: bool | DOT_SLASH | DOT_PERCENT | DOT_POWER
    | SHIFT_RIGHT | SHIFT_LEFT | BITWISE_AND | BITWISE_XOR | BITWISE_OR
    | TILDE | LOGICAL_AND | LOGICAL_OR | LOGICAL_NOT | EQUAL
    | DOT_EQUAL | AUG_BINOP: Ast.binop_t | SPACESHIP
    | CMP_EQ | CMP_NE | CMP_LE | CMP_GE | CMP_LT | CMP_GT
    | DOT_SPACESHIP | DOT_CMP_EQ | DOT_CMP_NE | DOT_CMP_LE
    | DOT_CMP_GE | DOT_CMP_LT | DOT_CMP_GT | FOLD_RESULT

fun ne2u(ne: bool, s: string) = if ne {s} else {s.decapitalize()}

fun tok2str(t: token_t)
{
    | LITERAL(l) => (f"LITERAL({Ast.lit2str(l)}", Ast.lit2str(l))
    | IDENT(ne, s) => (ne2u(ne, f"IDENT({s})"), s)
    | TYVAR(s) => (f"TYVAR({s})", s)
    | APOS => ("APOS", "'")
    | AS => ("AS", "as")
    | AT => ("AT", "@")
    | BREAK => ("BREAK", "break")
    | CATCH => ("CATCH", "catch")
    | CCODE => ("CCODE", "ccode")
    | CLASS => ("CLASS", "class")
    | CONTINUE => ("CONTINUE", "continue")
    | DO => ("DO", "do")
    | ELLIPSIS => ("ELLIPSIS", "...")
    | ELSE => ("ELSE", "else")
    | EXCEPTION => ("EXCEPTION", "exception")
    | EXTENDS => ("EXTENDS", "extends")
    | FINALLY => ("FINALLY", "finally")
    | FOLD => ("FOLD", "fold")
    | FOR(ne) => (ne2u(ne, "FOR"), "for")
    | FROM => ("FROM", "from")
    | FUN => ("FUN", "fun")
    | IF => ("IF", "if")
    | IMPLEMENTS => ("IMPLEMENTS", "implements")
    | IMPORT(ne) => (ne2u(ne, "IMPORT"), "import")
    | INLINE => ("INLINE", "inline")
    | INTERFACE => ("INTERFACE", "interface")
    | MATCH => ("MATCH", "match")
    | NOTHROW => ("NOTHROW", "nothrow")
    | OBJECT => ("OBJECT", "object")
    | OPERATOR => ("OPERATOR", "operator")
    | PARALLEL => ("PARALLEL", "parallel")
    | PRAGMA => ("PRAGMA", "pragma")
    | PURE => ("PURE", "pure")
    | REF(ne) => (ne2u(ne, "REF"), "ref")
    | STATIC => ("STATIC", "static")
    | THROW => ("THROW", "throw")
    | TRY => ("TRY", "try")
    | TYPE => ("TYPE", "type")
    | VAL => ("VAL", "val")
    | VAR => ("VAR", "var")
    | WHEN => ("WHEN", "when")
    | WITH => ("WITH", "with")
    | WHILE(ne) => (ne2u(ne, "WHILE"), "while")
    | UNZIP => ("UNZIP", "unzip")
    | LPAREN(ne) => (ne2u(ne, "LPAREN"), "(")
    | STR_INTERP_LPAREN => ("STR_INTERP_LPAREN", "<str_interp>{")
    | RPAREN => ("RPAREN", ")")
    | LSQUARE(ne) => (ne2u(ne, "LSQUARE"), "[")
    | RSQUARE => ("RSQUARE", "]")
    | LBRACE => ("LBRACE", "{")
    | RBRACE => ("RBRACE", "}")
    | LLIST => ("LLIST", "[:")
    | RLIST => ("RLIST", ":]")
    | COMMA => ("COMMA", ",")
    | DOT => ("DOT", ".")
    | SEMICOLON => ("SEMICOLON", ";")
    | COLON => ("COLON", ":")
    | BAR => ("BAR", "|")
    | CONS => ("CONS", "::")
    | CAST => ("CAST", ":>")
    | BACKSLASH => ("BACKSLASH", "\\")
    | ARROW => ("ARROW", "->")
    | BACK_ARROW => ("BACK_ARROW", "<-")
    | DOUBLE_ARROW => ("DOUBLE_ARROW", "=>")
    | QUESTION => ("QUESTION", "?")
    | EOF => ("EOF", "<EOF>")
    | MINUS(ne) => (ne2u(ne, "MINUS"), "-")
    | PLUS(ne) => (ne2u(ne, "PLUS"), "+")
    | STAR(ne) => (ne2u(ne, "STAR"), "*")
    | SLASH => ("SLASH", "/")
    | PERCENT => ("PERCENT", "%")
    | POWER(ne) => (ne2u(ne, "POWER"), "**")
    | DOT_MINUS(ne) => (ne2u(ne, "DOT_MINUS"), ".-")
    | DOT_STAR => ("DOT_STAR", ".*")
    | DOT_SLASH => ("DOT_SLASH", "./")
    | DOT_PERCENT => ("DOT_PERCENT", ".%")
    | DOT_POWER => ("DOT_POWER", ".**")
    | SHIFT_LEFT => ("SHIFT_LEFT", "<<")
    | SHIFT_RIGHT => ("SHIFT_RIGHT", ">>")
    | BITWISE_AND => ("BITWISE_AND", "&")
    | BITWISE_XOR => ("BITWISE_XOR", "^")
    | BITWISE_OR => ("BITWISE_OR", "|")
    | TILDE => ("TILDE", "~")
    | LOGICAL_AND => ("LOGICAL_AND", "&&")
    | LOGICAL_OR => ("LOGICAL_OR", "||")
    | LOGICAL_NOT => ("LOGICAL_NOT", "!")
    | EQUAL => ("EQUAL", "=")
    | DOT_EQUAL => ("DOT_EQUAL", ".=")
    | AUG_BINOP(o) => (f"AUG_BINOP(binop2_str() PLUS_EQUAL", "+=")
    | MINUS_EQUAL => ("MINUS_EQUAL", "-=")
    | STAR_EQUAL => ("STAR_EQUAL", "*=")
    | SLASH_EQUAL => ("SLASH_EQUAL", "/=")
    | PERCENT_EQUAL => ("PERCENT_EQUAL", "%=")
    | AND_EQUAL => ("AND_EQUAL", "&=")
    | OR_EQUAL => ("OR_EQUAL", "|=")
    | XOR_EQUAL => ("XOR_EQUAL", "^=")
    | SHIFT_LEFT_EQUAL => ("SHIFT_LEFT_EQUAL", "<<=")
    | SHIFT_RIGHT_EQUAL => ("SHIFT_RIGHT_EQUAL", ">>=")

    | DOT_STAR_EQUAL => ("DOT_STAR_EQUAL", ".*=")
    | DOT_SLASH_EQUAL => ("DOT_SLASH_EQUAL", "./=")
    | DOT_PERCENT_EQUAL => ("DOT_PERCENT_EQUAL", ".%=")
    | SPACESHIP => ("SPACESHIP", "<=>")
    | CMP_EQ => ("CMP_EQ", "==")
    | CMP_NE => ("CMP_NE", "!=")
    | CMP_LT => ("CMP_LT", "<")
    | CMP_LE => ("CMP_LE", "<=")
    | CMP_GE => ("CMP_GE", ">=")
    | CMP_GT => ("CMP_GT", ">")
    | DOT_SPACESHIP => ("DOT_SPACESHIP", ".<=>")
    | DOT_CMP_EQ => ("DOT_CMP_EQ", ".==")
    | DOT_CMP_NE => ("DOT_CMP_NE", ".!=")
    | DOT_CMP_LT => ("DOT_CMP_LT", ".<")
    | DOT_CMP_LE => ("DOT_CMP_LE", ".<=")
    | DOT_CMP_GE => ("DOT_CMP_GE", ".>=")
    | DOT_CMP_GT => ("DOT_CMP_GT", ".>")
    | FOLD_RESULT => ("FOLD_RESULT", "__fold_result__")
}

type stream_t =
{
    fname: string;  // filename used for diagnostic messages
    lineno: int ref;// the current line number (counted from 1)
    bol: int ref;   // beginning-of-line. The index of the first character
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
    stream_t { fname=fname, lineno=ref(1), bol=ref(0), buf=buf }
}

/*
   The key utility function to safely access stream within
   it's boundaries and beyond. In the latter case we return '\0'
   character, which means <EOF> ('end of file' or 'end of stream').

   That is, the stream is represented as
   c c c ... c EOF EOF EOF EOF ...
   ^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^
   the actual    the extrapolated
    content           content

   Such a function helps to eliminate multiple tedious checks
   and/or try-catch expressions when parsing a stream
*/
pure nothrow fun peekch(s: string, pos: int): char = ccode
{
    return (size_t)pos < (size_t)s->length ? s->data[pos] : (char_)0;
}

/* Skips whitespaces and comments (single-line and block).
   Nested block comments are supported by demand.
   Automatically updates the line number and
   the 'beginning-of-line' index.
   Supports '\n', '\r' and '\r\n' line endings
*/
fun skip_spaces(s: stream_t, pos: int, allow_nested: bool)
{
    val lineno0 = *s.lineno
    var lineno = lineno0
    var bol = *s.bol
    val buf = s.buf
    val len = buf.length()
    var inside_comment = 0
    var inside_eol_comment = false
    var c_res = '\0'
    var pos = pos

    while pos < len {
        val c = buf[pos]
        pos += 1
        val c1 = peekch(buf, pos)
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

    *s.lineno = lineno
    *s.bol = bol
    (c_res, pos, lineno > lineno0, inside_comment > 0)
}

/* Extracts an integer or floating-point number literal.
   The number should not have a sign in front and must start with a digit.
   0x, 0b, 0o/0 prefixes are supported.
   h, f, uNN, iNN, L and UL suffixes are also supported.

   For floating-point numbers without h/f suffix and without
   exponent (eE) specification, e.g. 1234.5678, we
   return FLOAT_LIKE(num, str) token, where num is the decoded number
   and str is the original representation of that number.

   This is because when accessing a nested tuple element we may
   have such "false" floating-point literals appear in the source, e.g.

   val m = ((a, b), (c, d))
   val det = m.0.0*m.1.1 - m.0.1*m.1.0

   where m.0.0 should be parsed as IDENT(m) DOT INT(0) DOT INT(0)
   instead of IDENT(m) DOT FLOAT(64, 0.0)
   and we leave it to the parser to figure that out.

   At the parser stage if FLOAT_LIKE(n,s) token follows
   right after DOT token, it's correctly re-interpreted
   as the nested tuple access.
*/
pure fun getnumber_(s: string, pos: int, just_int: bool): (int, int64, double, int, char) = ccode
{
    const int MAX_ATOF = 128;
    char buf[128 + 16];
    int_ i = 0, len = s->length - pos;
    const char_ *ptr = s->data + pos;
    bool ok = true;
    uint64_t r = 0, r1;
    int base = 10;
    bool flt = false, have_dot = false, have_e = false;
    char c = '\0';

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
        uint64_t maxval = (uint64_t)1 << (sizeof(size_t)*8-1);
        if( c == 'L' ) {
            bits = 64;
            maxval = 0x8000000000000000ULL;
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
            } else if(c == 'U' && c1 == 'L') {
                bits = 64;
                maxval = 0xFFFFFFFFFFFFFFFFULL;
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
        if(!ok || r > maxval)
            return FX_SET_EXN_FAST(FX_EXN_OutOfRangeError);
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

fun getnumber(s: string, pos: int, loc: lloc_t, just_int: bool): (int, token_t) =
    try {
        val (pos1, i, f, bits, c) = getnumber_(s, pos, just_int)
        (pos1, match (c, bits) {
        | ('i', 0) => LITERAL(Ast.LitInt(i))
        | ('i', _) => LITERAL(Ast.LitSInt(bits, i))
        | ('u', _) => LITERAL(Ast.LitUInt(bits, uint64(i)))
        | ('f', _) => LITERAL(Ast.LitFloat(bits, f))
        | (c, _) => throw LexerError(loc, f"unknown type '{c}' of numeric literal")
        })
    } catch {
        | OverflowError => throw LexerError(loc, "too long numeric literal")
        | OutOfRangeError =>
            throw LexerError(loc, "the numeric literal is out of range for the specified type")
        | BadArgError =>
            throw LexerError(loc, "invalid i/u suffix (should be 8, 16, 32 or 64)")
        | e =>
            throw LexerError(loc, f"exception {e} occured when parsing numeric literal")
    }

ccode
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
    (int, string, bool) = ccode
{
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
                    c = '\e';
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
                else if(fmt && c == 123) // open brace
                    c = (char_)123;
                else if(c == 'x') {
                    int x0=0, x1=0;
                    if( i+2 >= len || (x0 = decodehex(ptr[i+1])) < 0 ||
                        (x1 = decodehex(ptr[i+2])) < 0 )
                        return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                    c = x0*16+x1;
                    i++;
                } else if(c == '0') {
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
        } else if(fmt && c == 123 && i+1 < len && ptr[i+1] != 34) {
            inline_exp = true;
            i++;
            break;
        } else
            buf[n++] = c;
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
    fx_result->t2 = inline_exp;
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

/*
    Ficus keywords, represented as string -> (token, kwtyp) dictionary, where kwtyp is:
    0 - keyword that represents a single-keyword expression
        (well, 'operator' is not a single-keyword expression,
        but from lexer/parser perspective it behaves as if it was).
    1 - a keyword that cannot start a new expression, but
        it links the previous part of expression with the subsequent one;
        so it can immediately follow expression (be placed on the same line),
        e.g. "else" in if-then expression
    2 - a keyword that starts a new expression; it cannot follow another expression
        without some explicit operator or a separator (';' or a newline) in between.
        Note that sometimes several such keywords are used together,
        e.g. pure nothrow fun foo(x: int) = ...
    3 - a keyword that can play a role of a connector (type 1)
        or an expression beginning (type 2), depending on context
   -1 - reserved/internal-use keyword.
*/
var ficus_keywords = Map.from_list(String.cmp,
    [: ("as", (AS, 1)), ("break", (BREAK, 0)), ("catch", (CATCH, 1)), ("ccode", (CCODE, 2)),
    ("class", (CLASS, 2)), ("continue", (CONTINUE, 0)), ("do", (DO, 2)),
    ("else", (ELSE, 1)), ("exception", (EXCEPTION, 2)), ("extends", (EXTENDS, 1)),
    ("false", (LITERAL(Ast.LitBool(false)), 0)), ("finally", (FINALLY, 1)), ("fold", (FOLD, 2)),
    ("for", (FOR(true), 2)), ("from", (FROM, 2)), ("fun", (FUN, 2)), ("if", (IF, 2)),
    ("implements", (IMPLEMENTS, 1)), ("import", (IMPORT(true), 3)), ("inline", (INLINE, 2)),
    ("interface", (INTERFACE, 2)), ("match", (MATCH, 2)), ("nothrow", (NOTHROW, 2)),
    ("object", (OBJECT, 2)), ("operator", (OPERATOR, 0)), ("parallel", (PARALLEL, 2)),
    ("pragma", (PRAGMA, 2)), ("pure", (PURE, 2)), ("ref", (REF(true), 3)), ("static", (STATIC, 2)),
    ("throw", (THROW, 2)), ("true", (LITERAL(Ast.LitBool(true)), 0)), ("try", (TRY, 2)), ("type", (TYPE, 2)),
    ("val", (VAL, 2)), ("var", (VAR, 2)), ("when", (WHEN, 1)), ("while", (WHILE(true), 2)),
    ("with", (WITH, 1)), ("unzip", (UNZIP, 2)), ("__fold_result__", (FOLD_RESULT, -1)) :])

/*  The function that returns the actual tokenizer/lexer function,
    a closure with all the necessary parameters inside.

    Probably, later on this functional construction should be
    replaced with a class and methods.

    The nested/returned tokenizer function returns the "next" token on each
    call. In fact, on each call it may return several tokens, e.g.

    println(f"x={x}")
    is parsed as

    IDENT("println") LPAREN { LPAREN STRING("x=") PLUS }
        { IDENT("string") LPAREN IDENT(x) RPAREN PLUS }
        { STRING("") RPAREN } RPAREN
    where {} denotes groups that are returned together by nexttokens().
*/
fun make_lexer(strm: stream_t)
{
    var new_exp = true  // Ficus is the language with optional ';' separators between
                        // expressions in a block, tha's why we need to distinguish
                        // between unary and binary operators that look the same (+, -, *),
                        // between function calls and tuple expressions etc.
                        // The 'new expression' flag helps with that.
                        // It's set to true by ';', a newline characters and
                        // many of the operators/keywords.
                        // It's set to false after an identifier, a literal,
                        // a closing paren or one of a few operators/keywords.

    var paren_stack = ([] : (token_t, lloc_t) list)
                        // Another helper data structure
                        // that adds some context information to
                        // a primitive finite state machine that
                        // the lexer should have been in theory.
                        // It does not only contain a stack of so-far opened
                        // and not closd yet parens, but also
                        // some other selected tokens, such as CCODE, MATCH, CATCH etc.

    var pos = *strm.bol // The current position that is automatically updated after each call.
                        // Want to retrieve the position? Each returned token is supplied with
                        // with a tuple (begin_lineno, begin_col, end_lineno, end_col)
                        // describing where it was captured.
                        // Want to reset position or scan the particular fragment of code?
                        // Just create a new lexer with the same string
                        // or its substring of interest - it's a cheap operation.
    var prev_dot = false

    fun getloc(pos: int) = (*strm.lineno, max(pos - *strm.bol, 0) + 1)
    fun addloc(loc: lloc_t, tokens: token_t list) = [: for t <- tokens {(t, loc)} :]
    fun removeloc(tokens_l: (token_t, lloc_t) list) = [: for (t, _) <- tokens_l {t} :]
    fun check_ne(ne: bool, loc: lloc_t, name: string): void =
        if !ne {
            throw LexerError(loc, f"unexpected '{name}'. Insert ';' or newline")
        }
    fun lloc2str(l: lloc_t) {
        val colspec = if l.1 == 1 {""} else {f":{l.1}"}
        f"{strm.fname}:{l.0}{colspec}"
    }

    fun get_ccode(p: int): (int, string)
    {
        val buf = strm.buf
        val len = buf.length()
        var lbraces = 1
        // This implementation assumes that the whole buffer is available.
        // We just find the position q of terminating '}' of the inline C code
        // and then capture the whole thing between '{' (at p), i.e. buf[p:q].
        // If we ever switch to per-line buffer, the code needs to be updated
        // to accumuate C code into some text string
        val fold q = p for k <- p:len {
            var c = peekch(buf, k)
            var c1 = peekch(buf, k+1)
            if (c == '/' && (c1 == '/' || c1 == '*')) || c == '\n' || c == '\r' {
                val (_, q_, _, inside_comment) = skip_spaces(strm, k, false)
                if inside_comment {
                    throw LexerError(getloc(p), "unterminated comment")
                }
                q_
            } else if c == '"' || c == '\'' { // "
                val (q_, _, _) = getstring(buf, p+1, getloc(p+1), c, true, false)
                q_
            } else {
                match c {
                | '{' => lbraces += 1
                | '}' => lbraces -= 1; if lbraces == 0 {break with k+1}
                | _ => {}
                }
                k+1
            }
        }
        if lbraces > 0 {throw LexerError(getloc(p), "unterminated ccode block (check braces)")}
        (q, buf[p:q].copy())
    }

    fun nexttokens(): (token_t, lloc_t) list
    {
        val buf = strm.buf
        val len = buf.length()
        var c = peekch(buf, pos)
        var c1 = peekch(buf, pos+1)
        if c.isspace() ||
            (c == '/' && (c1 == '/' || c1 == '*')) {
            val (c_, p, nl, inside_comment) = skip_spaces(strm, pos, true)
            c = c_
            if nl {
                /* If we met a newline character during the whitespace
                   and comments scan, we set the 'new expression' flag,
                   which helps to distinguish between unary '-' and
                   the binary one, array comprehension from array
                   access operator etc.

                   We do it unless we are inside square or round parentheses.

                   This is because inside those parens it's all one expression
                   anyway. Whereas at the top level or inside curly braces
                   there can be multiple expressions, separated by a new line.
                */
                match paren_stack {
                | (LPAREN _, _) :: _ | (LSQUARE _, _) :: _ => {}
                | _ => new_exp = true
                }
            }
            if inside_comment {
                throw LexerError(getloc(pos), "unterminated comment")
            }
            pos = p
            c1 = peekch(buf, pos+1)
        }

        val loc = getloc(pos)

        if c.isalpha() || c == '_' {
            val fold p1 = pos for p <- pos:len {
                val cp = buf[p]
                if !cp.isalnum() && cp != '_' {break with p}
                p+1
            }
            val ident = buf[pos:p1].copy()
            pos = p1
            val t =
            match ficus_keywords.find_opt(ident) {
            | Some((t, n)) =>
                match (t, n) {
                | (CCODE, _) =>
                    check_ne(new_exp, loc, "ccode")
                    paren_stack = (CCODE, loc) :: paren_stack
                    new_exp = true; CCODE
                | (FOR _, _) =>
                    val t = FOR(new_exp); new_exp = true; t
                | (IMPORT _, _) =>
                    val t = IMPORT(new_exp); new_exp = true; t
                | (WHILE _, _) =>
                    val t = WHILE(new_exp); new_exp = true; t
                | (REF _, _) =>
                    val t = REF(new_exp); /* retain new_exp as-is */ t
                | (t, -1) =>
                    throw LexerError(loc, f"Identifier '{ident}' is reserved and cannot be used")
                | (t, 0) => check_ne(new_exp, loc, ident); new_exp = false; t
                | (t, 1) => new_exp = true; t
                | (t, 2) => check_ne(new_exp, loc, ident); new_exp = true; t
                | _ => throw LexerError(loc, f"Unexpected keyword '{ident}'")
                }
            | _ =>
                val t = IDENT(new_exp, ident); new_exp = false; t
            }
            prev_dot = false
            (t, loc) :: []
        } else if '0' <= c <= '9' {
            val (p, t) = getnumber(buf, pos, getloc(pos), prev_dot)
            new_exp = false
            pos = p
            prev_dot = false
            (t, loc) :: []
        }
        /*
            single-quote (apostrophe) symbol is used in multiple cases:
            - as matrix transposition operator, e.g. val C = A'*B
            - as a type varible prefix in generic type/function definition,
              e.g. fun foo(a: 'elem_type [+]) { ... }
            - to enclose character literals, e.g. '好'

            therefore, we need to carefully order the checks
            to correctly classify each use case.
        */
        else if c == '\'' && !new_exp {
            prev_dot = false
            (APOS, loc) :: []
        } else if c == '\'' && c1.isalpha() && peekch(buf, pos+2) != '\'' {
            val fold p1 = pos for p <- pos+1:len {
                val cp = buf[p]
                if !cp.isalnum() && cp != '_' {break with p}
                p+1
            }
            pos = p1
            new_exp = false
            prev_dot = false
            (TYVAR(buf[pos:p1].copy()), loc) :: []
        }
        else if c == '"' || c == '\'' || ((c == 'f' || c == 'r') && c1 == '"') {
            val termpos = if c == 'f' || c == 'r' {pos+1} else {pos}
            val term = peekch(buf, termpos)
            val (p, res, inline_exp) = getstring(buf, termpos+1, getloc(termpos+1),
                                                term, c == 'r', c == 'f')
            val prev_pos = pos
            pos = p
            new_exp = false
            prev_dot = false
            if term == '\'' {
                if res.length() != 1 {
                    throw LexerError(getloc(pos),
                        "character literal should contain exactly one character")
                }
                (LITERAL(Ast.LitChar(res[0])), loc) :: []
            } else if inline_exp {
                paren_stack = (STR_INTERP_LPAREN, getloc(prev_pos)) :: paren_stack
                addloc(loc, (if res.empty() {LPAREN(true) :: []}
                        else {LPAREN(true) :: LITERAL(Ast.LitString(res)) :: PLUS(false) :: []}) +
                        (IDENT(true, "string") :: LPAREN(false) :: []))
            } else {
                (LITERAL(Ast.LitString(res)), loc) :: []
            }
        } else {
            val prev_ne = new_exp
            val prev_prev_dot = prev_dot
            prev_dot = false
            new_exp = true
            pos = min(pos+1, len)
            val c2 = peekch(buf, pos+2)
            val c3 = peekch(buf, pos+3)
            match c {
            | '(' =>
                paren_stack = (LPAREN(prev_ne), getloc(pos-1)) :: paren_stack
                (LPAREN(prev_ne), loc) :: []
            | ')' =>
                new_exp = false
                match paren_stack {
                | (LPAREN _, _) :: rest =>
                    paren_stack = rest
                    (RPAREN, loc) :: []
                | _ =>
                    throw LexerError(loc, "Unexpected ')', check parens")
                }
            | '[' =>
                if c1 == ':' {
                    pos += 1
                    val tokens = if prev_ne {(LLIST, loc) :: []}
                        else {(LSQUARE(false), loc) :: (COLON, loc) :: []}
                    paren_stack = tokens.hd() :: paren_stack
                    tokens
                } else {
                    paren_stack = (LSQUARE(prev_ne), getloc(pos-1)) :: paren_stack
                    (LSQUARE(prev_ne), loc) :: []
                }
            | ']' =>
                new_exp = false
                match paren_stack {
                | (LSQUARE _, _) :: rest =>
                    paren_stack = rest
                    (RSQUARE, loc) :: []
                | _ =>
                    throw LexerError(loc, "Unexpected ']', check parens")
                }
            | '{' =>
                paren_stack = (LBRACE, getloc(pos-1)) :: paren_stack
                val p = getloc(pos-1)
                match paren_stack {
                | (LBRACE, _) :: (CCODE, _) :: rest =>
                    new_exp = false
                    paren_stack = rest
                    val (p, s) = get_ccode(pos)
                    pos = p
                    (LITERAL(Ast.LitString(s)), loc) :: []
                | _ =>
                    /*
                       call nexttokens recursively; if the next token is '|',
                       i.e. '|' goes immediately after '{', it's represented
                       as 'BAR', not 'BITWISE_OR'
                    */
                    (LBRACE, loc) :: (match nexttokens() {
                    | (BITWISE_OR, p) :: rest =>
                        paren_stack = (BAR, p) :: paren_stack
                        (BAR, p) :: rest
                    | tokens => tokens
                    })
                }
            | '}' =>
                new_exp = false
                match paren_stack {
                // handle string interpolation e.g. f"f({x})={f(x)}"
                | (STR_INTERP_LPAREN, _) :: rest =>
                    paren_stack = rest
                    val (p, s, inline_exp) = getstring(buf, pos, getloc(pos), chr(34), false, true)
                    pos = p
                    (if s.empty() {(RPAREN, loc) :: []}
                    else {(RPAREN, loc) :: (PLUS(false), loc) :: (LITERAL(Ast.LitString(s)), loc) :: []}) +
                    (if inline_exp {
                        paren_stack = (STR_INTERP_LPAREN, getloc(pos)) :: paren_stack
                        (PLUS(false), loc) :: (IDENT(true, "string"), loc) :: (LPAREN(false), loc) :: []
                    } else {
                        (RPAREN, loc) :: []
                    })
                | (BAR, _) :: (LBRACE, _) :: rest =>
                    paren_stack = rest
                    (RBRACE, loc) :: []
                | (LBRACE, _) :: rest =>
                    paren_stack = rest
                    (RBRACE, loc) :: []
                | _ =>
                    throw LexerError(loc, "Unexpected '}', check parens")
                }
            | '|' =>
                if c1 == '=' {(OR_EQUAL, loc) :: []}
                else {
                    match paren_stack {
                    | (BAR, _) :: (LBRACE, _) :: _ => (BAR, loc) :: []
                    | _ => (BITWISE_OR, loc) :: []
                    }
                }
            | '+' =>
                if c1 == '=' {pos += 1; (PLUS_EQUAL, loc) :: []}
                else {(PLUS(prev_ne), loc) :: []}
            | '-' =>
                if c1 == '=' {pos += 1; (MINUS_EQUAL, loc) :: []}
                else if c1 == '>' {pos += 1; (ARROW, loc) :: []}
                else if !prev_ne {(MINUS(false), loc) :: []} else {
                    match nexttokens() {
                    | (LITERAL(Ast.LitInt(x)), _) :: rest => (LITERAL(Ast.LitInt(-x)), loc) :: rest
                    | (LITERAL(Ast.LitSInt(b, x)), _) :: rest => (LITERAL(Ast.LitSInt(b, -x)), loc) :: rest
                    | (LITERAL(Ast.LitFloat(b, x)), _) :: rest => (LITERAL(Ast.LitFloat(b, -x)), loc) :: rest
                    | ts => (MINUS(true), loc) :: ts
                    }
                }
            | '*' =>
                if c1 == '=' {pos += 1; (STAR_EQUAL, loc) :: []}
                else if c1 == '*' {pos += 1; (POWER(prev_ne), loc) :: []}
                else {(STAR(prev_ne), loc) :: []}
            | '/' =>
                if c1 == '=' {pos += 1; (SLASH_EQUAL, loc) :: []}
                else {(SLASH, loc) :: []}
            | '%' =>
                if c1 == '=' {pos += 1; (PERCENT_EQUAL, loc) :: []}
                else {(PERCENT, loc) :: []}
            | '=' =>
                if c1 == '=' {pos += 1; (CMP_EQ, loc) :: []}
                else if c1 == '>' {pos += 1; (DOUBLE_ARROW, loc) :: []}
                else {(EQUAL, loc) :: []}
            | '^' =>
                if c1 == '=' {pos += 1; (XOR_EQUAL, loc) :: []}
                else { (BITWISE_XOR, loc) :: [] }
            | '&' =>
                if c1 == '=' {pos += 1; (AND_EQUAL, loc) :: []}
                else if c1 == '&' {pos += 1; (LOGICAL_AND, loc) :: []}
                else {(BITWISE_AND, loc) :: []}
            | '~' => check_ne(prev_ne, getloc(pos-1), "~"); (TILDE, loc) :: []
            | '\\' => check_ne(prev_ne, getloc(pos-1), "\\"); (BACKSLASH, loc) :: []
            | '@' => (AT, loc) :: []
            | '.' =>
                if c1 == '=' {
                    if c2 == '=' {pos += 2; (DOT_CMP_EQ, loc) :: []}
                    else {pos += 1; (DOT_EQUAL, loc) :: []}
                } else if c1 == '!' && c2 == '=' {
                    pos += 2; (DOT_CMP_NE, loc) :: []
                } else if c1 == '<' {
                    if c2 == '=' && c3 == '>' {pos += 3; (DOT_SPACESHIP, loc) :: []}
                    else if c2 == '=' {pos += 2; (DOT_CMP_LE, loc) :: []}
                    else {pos += 1; (DOT_CMP_LT, loc) :: []}
                } else if c1 == '>' {
                    if c2 == '=' {pos += 2; (DOT_CMP_GE, loc) :: []}
                    else {pos += 1; (DOT_CMP_GT, loc) :: []}
                } else if c1 == '-' {
                    check_ne(prev_ne, getloc(pos-1), ".-");
                    pos += 1
                    (DOT_MINUS(true), loc) :: []
                } else if c1 == '*' {
                    if c2 == '*' {pos += 2; (DOT_POWER, loc) :: []}
                    else if c2 == '=' {pos += 2; (DOT_STAR_EQUAL, loc) :: []}
                    else {pos += 1; (DOT_STAR, loc) :: []}
                } else if c1 == '/' {
                    if c2 == '=' {pos += 2; (DOT_SLASH_EQUAL, loc) :: []}
                    else {pos += 1; (DOT_SLASH, loc) :: []}
                } else if c1 == '%' {
                    if c2 == '=' {pos += 2; (DOT_PERCENT_EQUAL, loc) :: []}
                    else {pos += 1; (DOT_PERCENT, loc) :: []}
                } else if c1 == '.' && c2 == '.' {
                    pos += 2; (ELLIPSIS, loc) :: []
                } else {
                    prev_dot = true; (DOT, loc) :: []
                }
            | ',' => (COMMA, loc) :: []
            | ';' => (SEMICOLON, loc) :: []
            | ':' =>
                if c1 == ':' {pos += 1; (CONS, loc) :: []}
                else if c1 == '>' {pos += 1; (CAST, loc) :: []}
                else if c1 == ']' {
                    pos += 1
                    new_exp = false
                    match paren_stack {
                        | (LLIST, _) :: rest =>
                            paren_stack = rest; (RLIST, loc) :: []
                        | (LSQUARE _, _) :: rest =>
                            paren_stack = rest; (COLON, loc) :: (RSQUARE, loc) :: []
                        | _ => throw LexerError(getloc(pos-2),
                            "Unexpected ':]', check parens")
                    }
                }
                else {(COLON, loc) :: []}
            | '!' =>
                if c1 == '=' {
                    pos += 1
                    (CMP_NE, loc) :: []
                }
                else {
                    check_ne(prev_ne, getloc(pos-1), "!")
                    (LOGICAL_NOT, loc) :: []
                }
            | '?' => new_exp = false; (QUESTION, loc) :: []
            | '<' =>
                if c1 == '=' {
                    if c2 == '>' {pos += 2; (SPACESHIP, loc) :: []}
                    else {pos += 1; (CMP_LE, loc) :: []}
                } else if c1 == '<' {
                    if c2 == '=' {pos += 2; (SHIFT_LEFT_EQUAL, loc) :: []}
                    else {pos += 1; (SHIFT_LEFT, loc) :: []}
                } else if c1 == '-' {
                    pos += 1; (BACK_ARROW, loc) :: []
                } else {
                    (CMP_LT, loc) :: []
                }
            | '>' =>
                if c1 == '=' {
                    pos += 1; (CMP_GE, loc) :: []
                } else if c1 == '>' {
                    if c2 == '=' {pos += 2; (SHIFT_RIGHT_EQUAL, loc) :: []}
                    else {pos += 1; (SHIFT_RIGHT, loc) :: []}
                } else {
                    (CMP_GT, loc) :: []
                }
            | '@' => (AT, loc) :: []
            | '\0' =>
                match paren_stack {
                | (_, l) :: _ => throw LexerError(loc, f"some braces (around {lloc2str(l)}) are not closed")
                | _ => {}
                }
                (EOF, loc) :: []
            | _ =>
                println(f"unrecognized character '{c}' at lineno={*strm.lineno}")
                throw LexerError(loc, f"unrecognized character '{c}'")
            }
        }
    }
    nexttokens
}