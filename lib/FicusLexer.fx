import File, Map

exception Lexer_EOS
exception LexerError : (int, string)

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
    fname: string;
    lineno: int ref;
    bol: int ref;
    buf: string;
}

fun make_stream(fname: string)
{
    val buf = File.read_utf8(fname)
    stream_t { fname=fname, lineno=ref(1), bol=ref(0), buf=buf }
}

pure nothrow fun peekch(s: string, pos: int): char = ccode
{
    return (size_t)pos < (size_t)s->length ? s->data[pos] : (char_)0;
}

fun skip_spaces(s: stream_t, pos: int)
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
        if c == '\n' || (c == '\r' && peekch(buf, pos) != '\n') {
            lineno += 1
            bol = pos
            inside_eol_comment = false
        } else if c == ' ' || c == '\t' || c == '\r' || inside_eol_comment {
        } else if inside_comment > 0 {
            if c == '*' && peekch(buf, pos) == '/' {
                inside_comment -= 1
                pos += 1
            } else if c == '/' && peekch(buf, pos) == '*' {
                inside_comment += 1
                pos += 1
            }
        } else if c == '/' {
            val c1 = peekch(buf, pos)
            if c1 == '*' {
                inside_comment += 1
            } else if c1 == '/' {
                inside_eol_comment = true
            } else { break }
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

fun getstring(s: string, pos: int, term: char, raw: bool, fmt: bool): (int, string, bool) = ccode
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
                if (i+1 < len && ptr[i+1] == 34) { // double quote
                    buf[n++] = 34;
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
        } else if(fmt && c == 123) {
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
    int fx_status = fx_make_str(buf, n, &fx_result->t1);
    if(buf != buf0) fx_free(buf);
    fx_result->t0 = (ptr - s->data) + i;
    fx_result->t2 = inline_exp;
    return fx_status;
}

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

fun make_lexer(strm: stream_t)
{
    var new_exp = true
    var paren_stack = ([] : (token_t, int) list)
    var pos = *strm.bol

    fun nexttokens(): token_t list
    {
        val buf = strm.buf
        val len = buf.length()
        var c = peekch(buf, pos)
        if c.isspace() ||
            (c == '/' && ({val c1 = peekch(buf, pos+1); c1 == '/' || c1 == '*'})) {
            val (c1, p, nl, inside_comment) = skip_spaces(strm, pos)
            c = c1
            if nl {new_exp = true}
            if inside_comment {throw LexerError(pos, "unterminated comment")}
            pos = p
        }

        if c == '"' || c == '\'' || ((c == 'f' || c == 'r') && peekch(buf, pos+1) == '"') {
            val termpos = if c == 'f' || c == 'r' {pos+1} else {pos}
            val term = peekch(buf, termpos)
            val (p, res, inline_exp) = getstring(buf, termpos+1, term, c == 'r', c == 'f')
            pos = p
            if term == '\'' && res.length() != 1 {
                throw LexerError(pos, "character literal should contain exactly one character")
            }
            new_exp = false
            STRING(res) :: []
        } else if c.isalpha() {
            val fold p1 = pos for p <- pos:len {
                if !buf[p].isalnum() {break with p}
                p+1
            }
            val ident = buf[pos:p1]
            pos = p1
            match ficus_keywords.find_opt(ident) {
            | Some((t, _)) => t :: []
            | _ => new_exp = false; IDENT(ident) :: []
            }
        } else if '0' <= c <= '9' {
            val (p, t) = getnumber(buf, pos)
            new_exp = false
            pos = p
            t :: []
        } else {
            match c {
            | ':' => pos += 1; new_exp = true; if peekch(buf, pos) == ':' {pos += 1; CONS :: []} else {COLON :: []}
            | '+' => pos += 1; new_exp = true; if peekch(buf, pos) == '=' {pos += 1; PLUS_EQUAL :: []} else {PLUS :: []}
            | '-' => pos += 1; new_exp = true; if peekch(buf, pos) == '=' {pos += 1; MINUS_EQUAL :: []} else {MINUS :: []}
            | ',' => pos += 1; new_exp = true; COMMA :: []
            | ';' => pos += 1; new_exp = true; SEMICOLON :: []
            | '.' => pos += 1; new_exp = true; if peekch(buf, pos) == '=' {pos += 1; MINUS_EQUAL :: []}
                    else if peekch(buf, pos+1) == '.' && peekch(buf, pos+2) == '.' {pos += 2; ELLIPSIS :: []}
                    else {DOT :: []}
            | '\0' => EOF :: []
            | _ => println(f"unrecognized character '{c}' at pos={pos}"); throw LexerError(pos, f"unrecognized character {c}")
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
