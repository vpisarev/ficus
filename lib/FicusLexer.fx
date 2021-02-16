import File, Map

exception LexerEOF
exception LexerError : ((int, int), string)

// Ficus tokens
type token_t =
    | TRUE | FALSE
    | INT: int64 | SINT: (int, int64) | UINT: (int, uint64)
    | FLOAT: (int, double) | FLOAT_LIKE: (double, string)
    | IDENT: string | B_IDENT: string
    | STRING: string | CHAR: char | TYVAR: string
    | APOS | AS | AT | BREAK | CATCH | CCODE | CLASS | CONTINUE | DO
    | ELLIPSIS | ELSE | EXCEPTION | EXPAND | EXTENDS | FINALLY | FOLD
    | B_FOR | FOR | FROM | FUN | IF | IMPLEMENTS | B_IMPORT | IMPORT
    | INLINE | INTERFACE | MATCH | MODULE | NOTHROW | OPERATOR
    | PARALLEL | PRAGMA | PURE | REF | REF_TYPE | STATIC
    | THROW | TRY | TYPE | VAL | VAR | WHEN | WITH | B_WHILE | WHILE
    | B_LPAREN | LPAREN | STR_INTERP_LPAREN | RPAREN
    | B_LSQUARE | LSQUARE | RSQUARE | LBRACE | RBRACE
    | LLIST | RLIST | COMMA | DOT | SEMICOLON | COLON
    | BAR | CONS | CAST | BACKSLASH | BACK_ARROW | DOUBLE_ARROW
    | ARROW | QUESTION | EOF | B_MINUS | MINUS | B_PLUS | PLUS
    | B_STAR | STAR | SLASH | MOD | B_POWER | POWER | DOT_STAR
    | B_DOT_MINUS | DOT_SLASH | DOT_MOD | DOT_POWER
    | SHIFT_RIGHT | SHIFT_LEFT | BITWISE_AND | BITWISE_XOR | BITWISE_OR
    | TILDE | LOGICAL_AND | LOGICAL_OR | LOGICAL_NOT | EQUAL
    | PLUS_EQUAL | MINUS_EQUAL | STAR_EQUAL | SLASH_EQUAL
    | DOT_EQUAL | MOD_EQUAL | AND_EQUAL | OR_EQUAL | XOR_EQUAL
    | SHIFT_LEFT_EQUAL | SHIFT_RIGHT_EQUAL | DOT_STAR_EQUAL
    | DOT_SLASH_EQUAL | DOT_MOD_EQUAL | SPACESHIP
    | CMP_EQ | CMP_NE | CMP_LE | CMP_GE | CMP_LT | CMP_GT
    | DOT_SPACESHIP | DOT_CMP_EQ | DOT_CMP_NE | DOT_CMP_LE
    | DOT_CMP_GE | DOT_CMP_LT | DOT_CMP_GT | FOLD_RESULT

fun string(t: token_t)
{
    | TRUE => "TRUE"
    | FALSE => "FALSE"
    | INT(i) => f"INT({i})"
    | SINT(b, i) => f"SINT({b}, {i})"
    | UINT(b, i) => f"UINT({b}, {i})"
    | FLOAT(b, f) => f"FLOAT({b}, {f})"
    | FLOAT_LIKE(f, s) => f"FLOAT_LIKE({f}, {s})"
    | IDENT(s) => f"IDENT(s)"
    | B_IDENT(s) => f"B_IDENT({s})"
    | STRING(s) => f"STRING({s})"
    | CHAR(s) => f"CHAR({s})"
    | TYVAR(s) => f"TYVAR({s})"
    | APOS => "APOS"
    | AS => "AS"
    | AT => "AT"
    | BREAK => "BREAK"
    | CATCH => "CATCH"
    | CCODE => "CCODE"
    | CLASS => "CLASS"
    | CONTINUE => "CONTINUE"
    | DO => "DO"
    | ELLIPSIS => "ELLIPSIS"
    | ELSE => "ELSE"
    | EXCEPTION => "EXCEPTION"
    | EXPAND => "EXPAND"
    | EXTENDS => "EXTENDS"
    | FINALLY => "FINALLY"
    | FOLD => "FOLD"
    | B_FOR => "B_FOR"
    | FOR => "FOR"
    | FROM => "FROM"
    | FUN => "FUN"
    | IF => "IF"
    | IMPLEMENTS => "IMPLEMENTS"
    | B_IMPORT => "B_IMPORT"
    | IMPORT => "IMPORT"
    | INLINE => "INLINE"
    | INTERFACE => "INTERFACE"
    | MATCH => "MATCH"
    | MODULE => "MODULE"
    | NOTHROW => "NOTHROW"
    | OPERATOR => "OPERATOR"
    | PARALLEL => "PARALLEL"
    | PRAGMA => "PRAGMA"
    | PURE => "PURE"
    | REF => "MAKE_REF"
    | REF_TYPE => "REF_TYPE"
    | STATIC => "STATIC"
    | THROW => "THROW"
    | TRY => "TRY"
    | TYPE => "TYPE"
    | VAL => "VAL"
    | VAR => "VAR"
    | WHEN => "WHEN"
    | WITH => "WITH"
    | B_WHILE => "B_WHILE"
    | WHILE => "WHILE"
    | B_LPAREN => "B_LPAREN"
    | LPAREN => "LPAREN"
    | STR_INTERP_LPAREN => "STR_INTERP_LPAREN"
    | RPAREN => "RPAREN"
    | B_LSQUARE => "B_LSQUARE"
    | LSQUARE => "LSQUARE"
    | RSQUARE => "RSQUARE"
    | LBRACE => "LBRACE"
    | RBRACE => "RBRACE"
    | LLIST => "LLIST"
    | RLIST => "RLIST"
    | COMMA => "COMMA"
    | DOT => "DOT"
    | SEMICOLON => "SEMICOLON"
    | COLON => "COLON"
    | BAR => "BAR"
    | CONS => "CONS"
    | CAST => "CAST"
    | BACKSLASH => "BACKSLASH"
    | BACK_ARROW => "BACK_ARROW"
    | DOUBLE_ARROW => "DOUBLE_ARROW"
    | ARROW => "ARROW"
    | QUESTION => "QUESTION"
    | EOF => "EOF"
    | B_MINUS => "B_MINUS"
    | MINUS => "MINUS"
    | B_PLUS => "B_PLUS"
    | PLUS => "PLUS"
    | B_STAR => "B_STAR"
    | STAR => "STAR"
    | SLASH => "SLASH"
    | MOD => "MOD"
    | B_POWER => "B_POWER"
    | POWER => "POWER"
    | DOT_STAR => "DOT_STAR"
    | B_DOT_MINUS => "B_DOT_MINUS"
    | DOT_SLASH => "DOT_SLASH"
    | DOT_MOD => "DOT_MOD"
    | DOT_POWER => "DOT_POWER"
    | SHIFT_RIGHT => "SHIFT_RIGHT"
    | SHIFT_LEFT => "SHIFT_LEFT"
    | BITWISE_AND => "BITWISE_AND"
    | BITWISE_XOR => "BITWISE_XOR"
    | BITWISE_OR => "BITWISE_OR"
    | TILDE => "TILDE"
    | LOGICAL_AND => "LOGICAL_AND"
    | LOGICAL_OR => "LOGICAL_OR"
    | LOGICAL_NOT => "LOGICAL_NOT"
    | EQUAL => "EQUAL"
    | PLUS_EQUAL => "PLUS_EQUAL"
    | MINUS_EQUAL => "MINUS_EQUAL"
    | STAR_EQUAL => "STAR_EQUAL"
    | SLASH_EQUAL => "SLASH_EQUAL"
    | DOT_EQUAL => "DOT_EQUAL"
    | MOD_EQUAL => "MOD_EQUAL"
    | AND_EQUAL => "AND_EQUAL"
    | OR_EQUAL => "OR_EQUAL"
    | XOR_EQUAL => "XOR_EQUAL"
    | SHIFT_LEFT_EQUAL => "SHIFT_LEFT_EQUAL"
    | SHIFT_RIGHT_EQUAL => "SHIFT_RIGHT_EQUAL"
    | DOT_STAR_EQUAL => "DOT_STAR_EQUAL"
    | DOT_SLASH_EQUAL => "DOT_SLASH_EQUAL"
    | DOT_MOD_EQUAL => "DOT_MOD_EQUAL"
    | SPACESHIP => "SPACESHIP"
    | CMP_EQ => "CMP_EQ"
    | CMP_NE => "CMP_NE"
    | CMP_LE => "CMP_LE"
    | CMP_GE => "CMP_GE"
    | CMP_LT => "CMP_LT"
    | CMP_GT => "CMP_GT"
    | DOT_SPACESHIP => "DOT_SPACESHIP"
    | DOT_CMP_EQ => "DOT_CMP_EQ"
    | DOT_CMP_NE => "DOT_CMP_NE"
    | DOT_CMP_LE => "DOT_CMP_LE"
    | DOT_CMP_GE => "DOT_CMP_GE"
    | DOT_CMP_LT => "DOT_CMP_LT"
    | DOT_CMP_GT => "DOT_CMP_GT"
    | FOLD_RESULT => "FOLD_RESULT"
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
        val c1 = peekch(buf, pos+1)
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
pure fun getnumber_(s: string, pos: int): (int, int64, double, int, char) = ccode
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
        } else if(len >= 2 && (ptr[1] == '.' || ptr[1] == 'e' || ptr[1] == 'E')) {
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
                    return FX_SET_EXN_FAST(FX_EXN_BadArgError);
                r = r1;
            }
            if( c == '.' || c == 'e' || c == 'E' )
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
            return FX_SET_EXN_FAST(FX_EXN_OverflowError);
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
        fx_result->t4 = bits == 64 && have_dot && !have_e ? '~' : 'f';
    }
    return ok ? FX_OK : FX_SET_EXN_FAST(FX_EXN_OverflowError);
}

fun getnumber(s: string, pos: int): (int, token_t)
{
    val (pos1, i, f, bits, c) = getnumber_(s, pos)
    (pos1, match (c, bits) {
    | ('i', 0) => INT(i)
    | ('i', _) => SINT(bits, i)
    | ('u', _) => UINT(bits, (i :> uint64))
    | ('f', _) => FLOAT(bits, f)
    | ('~', _) => FLOAT_LIKE(f, s[pos:pos1])
    })
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
fun getstring(s: string, pos: int, term: char, raw: bool, fmt: bool):
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
        if (c == term)
            break;
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
                    return FX_SET_EXN_FAST(FX_EXN_BadArgError);
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
                    if(i+2 >= len || (x0 = decodehex(ptr[i+1])) < 0 || (x1 = decodehex(ptr[i+2])) < 0)
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
var ficus_keywords = Map.from_list(
    [: ("as", (AS, 1)), ("break", (BREAK, 0)), ("catch", (CATCH, 1)), ("ccode", (CCODE, 2)),
        ("class", (CLASS, 2)), ("continue", (CONTINUE, 0)), ("do", (DO, 2)),
        ("else", (ELSE, 1)), ("exception", (EXCEPTION, 2)), ("extends", (EXTENDS, 1)),
        ("false", (FALSE, 0)), ("finally", (FINALLY, 1)), ("fold", (FOLD, 2)),
        ("for", (FOR, 2)), ("from", (FROM, 2)), ("fun", (FUN, 2)), ("if", (IF, 2)),
        ("implements", (IMPLEMENTS, 1)), ("import", (IMPORT, 3)), ("inline", (INLINE, 2)),
        ("interface", (INTERFACE, 2)), ("match", (MATCH, 2)), ("module", (MODULE, 2)),
        ("nothrow", (NOTHROW, 2)), ("operator", (OPERATOR, 0)), ("parallel", (PARALLEL, 2)),
        ("pragma", (PRAGMA, 2)), ("pure", (PURE, 2)), ("ref", (REF, 3)), ("static", (STATIC, 2)),
        ("throw", (THROW, 2)), ("true", (TRUE, 0)), ("try", (TRY, 2)), ("type", (TYPE, 2)),
        ("val", (VAL, 2)), ("var", (VAR, 2)), ("when", (WHEN, 1)), ("while", (WHILE, 2)),
        ("with", (WITH, 1)), ("__fold_result__", (FOLD_RESULT, -1)) :],
    String.cmp)

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

    var paren_stack =   // Another helper data structure
      ([] : (token_t,   // that adds some context information to
       (int, int))      // a primitive finite state machine that
        list)           // the lexer should have been in theory.
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

    fun getpos(pos: int) = (*strm.lineno, max(pos - *strm.bol, 0) + 1)
    fun check_ne(ne: bool, pos: int, opname: string): void =
        if !ne {
            throw LexerError(getpos(pos),
                f"unexpected operator {opname}. Insert ';' or newline")
        }

    fun get_ccode(p: int): (int, string)
    {
        val buf = strm.buf
        val len =
        var ccode = ""
        var p = p
        while true {
            var c = peekch(buf, p)
            var c1 = peekch(buf, p+1)
            if c.isspace() || (c == '/' && (c1 == '/' || c1 == '*')) {
                val (_, p_, nl, inside_comment) = skip_spaces(strm, pos, false)
                if inside_comment {
                    throw LexerError(getpos(p), "non-terminaed comment")
                }
                p = p_
                ccode += "\n"
                continue
            }

            if c == '"' || c == '\'' { // "
                val (p_, res, _) = getstring(buf, p+1, term, true, false)
                ccode += c + res + c
                p = p_
                continue
            }

            // [TODO] count opening and closing (), [], {}
            val fold q = p for ...
        }
        (p, ccode)
    }

    fun nexttokens(): token_t list
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
                | (LPAREN, _) :: _ | (LSQUARE, _) :: _ => {}
                | _ => new_exp = true
                }
            }
            if inside_comment {
                throw LexerError(getpos(pos), "unterminated comment")
            }
            pos = p
            c1 = peekch(buf, pos+1)
        }

        /*
            quote (apostrophe) symbol is used in multiple cases:
            - as matrix transposition operator, e.g. val C = A'*B
            - as a type varible prefix in generic type/function definition,
              e.g. fun foo(a: 'elem_type [+]) { ... }
            - to enclose character literals, e.g. '好'

            therefore, we need to carefully order the checks
            to correctly classify each use case.
        */
        if c == '\'' && !new_exp {
            APOS :: []
        } else if c == '\'' && c1.isalpha() && peekch(buf, pos+2) != '\'' {
            val fold p1 = pos for p <- pos+1:len {
                val cp = buf[p]
                if !cp.isalnum() && cp != '_' {break with p}
                p+1
            }
            new_exp = false
            TYVAR(buf[pos:p1]) :: []
        } else if c == '"' || c == '\'' || ((c == 'f' || c == 'r') && c1 == '"') {
            val termpos = if c == 'f' || c == 'r' {pos+1} else {pos}
            val term = peekch(buf, termpos)
            val (p, res, inline_exp) = getstring(buf, termpos+1, term, c == 'r', c == 'f')
            val prev_pos = pos
            pos = p
            new_exp = false
            if term == '\'' {
                if res.length() != 1 {
                    throw LexerError(getpos(pos),
                        "character literal should contain exactly one character")
                }
                CHAR(res[0]) :: []
            } else if inline_exp {
                paren_stack = (STR_INTERP_LPAREN, getpos(prev_pos)) :: paren_stack
                (if res.empty() {LPAREN :: []} else {LPAREN :: STRING(res) :: PLUS}) +
                (IDENT("string") :: LPAREN :: [])
            } else {
                STRING(res) :: []
            }
        } else if c.isalpha() || c == '_' {
            val fold p1 = pos for p <- pos:len {
                val cp = buf[p]
                if !cp.isalnum() && cp != '_' {break with p}
                p+1
            }
            val ident = buf[pos:p1]
            pos = p1
            if ident.length() > 1 {
                match ficus_keywords.find_opt(ident) {
                | Some((t, _)) => t :: []
                | _ =>
                    new_exp = false
                    IDENT(ident) :: []
                }
            } else {
                new_exp = false
                IDENT(ident) :: []
            }
        } else if '0' <= c <= '9' {
            val (p, t) = getnumber(buf, pos)
            new_exp = false
            pos = p
            t :: []
        } else {
            bool prev_ne = new_exp
            new_exp = true
            pos = min(pos+1, len)
            val c2 = peekch(buf, pos+2)
            val c3 = peekch(buf, pos+3)
            match c {
            | '(' =>
                paren_stack = (LPAREN, getpos(pos-1)) :: paren_stack
                if prev_ne {B_LPAREN :: []} else {LPAREN :: []}
            | ')' =>
                new_exp = false
                match paren_stack {
                | (LPAREN, _) :: rest =>
                    paren_stack = rest
                    RPAREN :: []
                | _ =>
                    throw LexerError(get_pos(pos-1), "Unexpected ')', check parens")
                }
            | '[' =>
                if c1 == ':' {
                    pos += 1
                    val tokens = if prev_ne {LLIST :: []} else {LSQUARE :: COLON :: []}
                    paren_stack = (tokens.hd(), getpos(pos-2)) :: paren_stack
                    tokens
                } else {
                    paren_stack = (LSQUARE, getpos(pos-1)) :: paren_stack
                    if prev_ne {B_LSQUARE :: []} else {LSQUARE :: []}
                }
            | ']' =>
                new_exp = false
                match paren_stack {
                | (LSQUARE, _) :: rest =>
                    paren_stack = rest
                    RSQUARE :: []
                | _ =>
                    throw LexerError(get_pos(pos-1), "Unexpected ']', check parens")
                }
            | '{' =>
                paren_stack = (LBRACE, getpos(pos-1)) :: paren_stack
                val p = getpos(pos-1)
                match paren_stack {
                | (LBRACE, _) :: (CCODE, _) :: rest =>
                    new_exp = false
                    val (p, s) = get_ccode(pos)
                    pos = p
                    CCODE(s) :: []
                | _ =>
                    // call nexttokens recursively; if the next token is '|',
                    // i.e. '|' goes immediately after '{', it's represented
                    // as 'BAR', not 'BITWISE_OR'
                    LBRACE :: (match nexttokens() {
                    | BITWISE_OR :: rest =>
                        paren_stack = (BAR, p) :: paren_stack
                        BAR :: rest
                    | tokens => tokens
                    })
                }
            | '}' =>
                new_exp = false
                match paren_stack {
                // handle string interpolation e.g. f"f({x})={f(x)}"
                | (STR_INTERP_LPAREN, _) :: rest =>
                    paren_stack = rest
                    val (p, s, inline_exp) = getstring(buf, pos, chr(34), false, true)
                    pos = p
                    (if s.empty() { RPAREN :: [] } else { RPAREN :: PLUS :: STRING(s) :: [] }) +
                    (if inline_exp {
                        paren_stack = (STR_INTERP_LPAREN, getpos(pos)) :: paren_stack
                        PLUS :: IDENT("string") :: LPAREN :: []
                    } else {
                        RPAREN :: []
                    }
                | (BAR, _) :: (LBRACE, _) :: rest =>
                    paren_stack = rest
                    [RBRACE]
                | (LBRACE, _) :: rest =>
                    paren_stack = rest
                    [RBRACE]
                | _ =>
                    throw LexerError(get_pos(pos-1), "Unexpected '}', check parens")
                }
            | '|' =>
                if c1 == '=' {OR_EQUAL :: []}
                else {
                    match !paren_stack with {
                    | (BAR, _) :: (LBRACE, _) :: _ => BAR :: []
                    | _ => BITWISE_OR :: []
                    }
                }
            | '+' =>
                if c1 == '=' {pos += 1; PLUS_EQUAL :: []}
                else if prev_ne {B_PLUS :: []} else {PLUS :: []}
            | '-' =>
                if c1 == '=' {pos += 1; MINUS_EQUAL :: []}
                else if c1 == '>' {pos += 1; ARROW :: []}
                else if prev_ne {B_MINUS :: []} else {MINUS :: []}
            | '*' =>
                if c1 == '=' {
                    pos += 1; STAR_EQUAL :: []
                } else if c1 == '*' {
                    pos += 1
                    if prev_ne {B_POWER :: []}
                    else {POWER :: []}
                } else if prev_ne {
                    B_STAR :: []
                } else {
                    STAR :: []
                }
            | '/' =>
                if c1 == '=' {pos += 1; SLASH_EQUAL :: []}
                else {SLASH :: []}
            | '%' =>
                if c1 == '=' {pos += 1; MOD_EQUAL :: []}
                else {MOD :: []}
            | '=' =>
                if c1 == '=' {pos += 1; CMP_EQ :: []}
                else if c1 == '>' {pos += 1; DOUBLE_ARROW :: []}
                else {EQUAL :: []}
            | '^' =>
                if c1 == '=' {pos += 1; XOR_EQUAL :: []}
                else { BITWISE_XOR :: [] }
            | '&' =>
                if c1 == '=' {pos += 1; AND_EQUAL :: []}
                else if c1 == '&' {pos += 1; LOGICAL_AND :: []}
                else {BITWISE_AND :: []}
            | '~' => check_ne(prev_ne, pos-1, "~"); TILDE :: []
            | '\\' => check_ne(prev_ne, pos-1, "\\"); EXPAND :: []
            | '@' => AT :: []
            | '.' =>
                if c1 == '=' {
                    if c2 == '=' {pos += 2; DOT_CMP_EQ :: []}
                    else {pos += 1; DOT_EQUAL :: []}
                } else if c1 == '!' && c2 == '=' {
                    pos += 2; DOT_CMP_NE :: []
                } else if c1 == '<' {
                    if c2 == '=' && c3 == '>' {pos += 3; DOT_SPACESHIP :: []}
                    else if c2 == '=' {pos += 2; DOT_CMP_LE :: []}
                    else {pos += 1; DOT_CMP_LT :: []}
                } else if c1 == '>' {
                    if c2 == '=' {pos += 2; DOT_CMP_GE :: []}
                    else {pos += 1; DOT_CMP_GT :: []}
                } else if c1 == '-' {
                    check_ne(prev_ne, pos-1, ".-");
                    pos += 1
                    B_DOT_MINUS :: []
                } else if c1 == '*' {
                    if c2 == '*' {pos += 2; DOT_POWER :: []}
                    else if c2 == '=' {pos += 2; DOT_STAR_EQUAL :: []}
                    else {pos += 1; DOT_STAR :: []}
                } else if c1 == '/' {
                    if c2 == '=' {pos += 2; DOT_SLASH_EQUAL :: []}
                    else {pos += 1; DOT_SLASH :: []}
                } else if c1 == '%' {
                    if c2 == '=' {pos += 2; DOT_MOD_EQUAL :: []}
                    else {pos += 1; DOT_MOD :: []}
                } else if c1 == '.' && c2 == '.' {pos += 2; ELLIPSIS :: []}
                else {DOT :: []}
            | ',' => COMMA :: []
            | ';' => SEMICOLON :: []
            | ':' =>
                if c1 == ':' {pos += 1; CONS :: []}
                else if c1 == '>' {pos += 1; CAST :: []}
                else if c1 == ']' {
                    pos += 1
                    new_exp = false
                    match paren_stack {
                        | (LLIST, _) :: rest => paren_stack = rest; RLIST :: []
                        | (LSQUARE, _) :: rest => paren_stack = rest; COLON :: RSQUARE :: []
                        | _ => throw LexerError(getpos(pos-2),
                            "Unexpected ':]', check parens")
                    }
                }
                else {COLON :: []}
            | '!' =>
                if c1 == '=' {pos += 1; CMP_NE :: []}
                else {check_ne(prev_ne, pos-1, "!"); LOGICAL_NOT :: []}
            | '?' => new_exp = false; QUESTION :: []
            | '<' =>
                if c1 == '=' {
                    if c2 == '>' {pos += 2; SPACESHIP :: []}
                    else {pos += 1; CMP_LE :: []}
                } else if c1 == '<' {
                    if c2 == '=' {pos += 2; SHIFT_LEFT_EQUAL :: []}
                    else {pos += 1; SHIFT_LEFT :: []}
                } else if c1 == '-' {
                    pos += 1; BACK_ARROW :: []
                } else {
                    CMP_LT :: []
                }
            | '>' =>
                if c1 == '=' {
                    pos += 1; CMP_GE :: []
                } else if c1 == '>' {
                    if c2 == '=' {pos += 2; SHIFT_RIGHT_EQUAL :: []}
                    else {pos += 1; SHIFT_RIGHT :: []}
                } else {
                    CMP_GT :: []
                }
            | "@" => AT :: []
            | "|="  => pos += 1; new_exp = true; OR_EQUAL :: [] }
            | '\0' =>
                EOF :: []
            | _ =>
                println(f"unrecognized character '{c}' at lineno={*strm.lineno}")
                throw LexerError(pos, f"unrecognized character {c}")
            }
        }
    }
    nexttokens
}

val s = "-234, 0.555,0x12u8, 255u8, 5e-3, -32i16, 5.0+1e+10, 314.15e-2f, 34509.380583049530459834059304958e3"
/*println(f"\ngrabbing floating-point numbers from {s}")
val (p, x) = getnumber(s, 1)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+2)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+2)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+2)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+2)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+3)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+2)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+1)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+2)
println(f"captured={x}: rest={s[p:]}")
val (p, x) = getnumber(s, p+2)
println(f"captured={x}: rest={s[p:]}")

val slist = [: "f\"abc{s}\"", r"\"привет,\n你好吗?\"", "r\"\\n\\r\\\\\"", r"'\U0001F600'" :]
for s <- slist {
    val c = s[0]
    val (fmt, raw, p0) =
        if c == 'f' {(true, false, 2)}
        else if c == 'r' {(false, true, 2)}
        else {(false, false, 1)}
    val term = s[p0-1]
    val (p, slit, inline_e) = getstring(s, p0, term, raw, fmt)
    println(f"orig.length()={s.length()}, slit={term}{slit}{term}, slit.length()={slit.length()}, rest={s[p:]}, raw={raw}, fmt={fmt}, inline_e={inline_e}")
}
*/
val strm = stream_t { fname="noname", lineno=ref(1), bol=ref(0), buf=s }
val lexer = make_lexer(strm)

while true {
    val t = lexer()
    println(t)
    match t.last() {
        | EOF => break
        | _ => {}
    }
}
