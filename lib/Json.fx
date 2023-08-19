/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Json parser and pretty-printer

import File, LexerUtils as Lxu

type t =
    | Str: string
    | Int: int64
    | Real: double
    | Bool: bool
    | Null
    | Seq: t []
    | Map: (string, t) []
    | Commented: (string, t)

type lloc_t = Lxu.lloc_t
type stream_t = Lxu.stream_t

fun parse_file(fname: string) = parse_string(fname, File.read_utf8(fname))
fun parse_string(fname: string, s: string): t
{
    val strm = Lxu.make_stream_from_string(fname, s)
    fun getloc(strm: stream_t, pos: int) = (strm.lineno, pos - strm.bol)

    fun parse_value(strm: stream_t, pos: int, skipspaces: bool): (int, t)
    {
        val pos = if skipspaces {Lxu.skip_spaces(strm, pos, false).1} else {pos}
        val s = strm.buf
        val c = s.zero[pos]
        if c == '\"' { //"
            val (pos, result, dl, _) =
                Lxu.getstring(s, pos+1, getloc(strm, pos+1), '\"' /* " */, false, false)
            strm.lineno += dl
            (pos, Str(result))
        } else if '0' <= c <= '9' || c == '+' || c == '-' {
            val (sign, pos) =
                if c == '+' || c == '-' {
                    val sign = if c == '-' {-1} else {1}
                    val pos = if '0' <= s.zero[pos+1] <= '9' {pos+1}
                            else {Lxu.skip_spaces(strm, pos+1, false).1}
                    (sign, pos)
                } else {(1, pos)}
            val (pos, i, f, _, kind) = Lxu.getnumber(s, pos, just_int=false, get_suffix=false)
            (pos, if kind == 'f' {Real(sign*f)} else {Int(sign*i)})
        } else if c == '[' {
            parse_seq(strm, pos+1)
        } else if c == '{' {
            parse_map(strm, pos+1)
        } else {
            val c1 = s.zero[pos+1], c2 = s.zero[pos+2],
                c3 = s.zero[pos+3], c4 = s.zero[pos+4]
            if c == 't' && c1 == 'r' && c2 == 'u' && c3 == 'e' {
                (pos+4, Bool(true))
            } else if c == 'f' && c1 == 'a' && c2 == 'l' && c3 == 's' && c4 == 'e' {
                (pos+5, Bool(false))
            } else if c == 'n' && c1 == 'u' && c2 == 'l' && c3 == 'l' {
                (pos+4, Null)
            } else {
                throw Lxu.LexerError(getloc(strm, pos),
                "Unexpected character. Json value (number, string, boolean, null, sequence [...] or map {...}) is expected")
            }
        }
    }

    fun parse_seq(strm: stream_t, pos: int): (int, t) {
        var elems: t list = []
        var vpos = pos
        while true {
            val (c, pos, _) = Lxu.skip_spaces(strm, vpos, false)
            if c == ']' {vpos = pos+1; break}
            val (pos, v) = parse_value(strm, pos, false)
            elems = v :: elems
            val c = strm.buf.zero[pos]
            val pos = if c != ',' && c != ']' {
                Lxu.skip_spaces(strm, pos, false).1
            } else { pos }
            if strm.buf.zero[pos] == ',' {
                vpos = pos+1
            } else if strm.buf.zero[pos] == ']' {
                vpos = pos+1
                break
            } else {
                throw Lxu.LexerError(getloc(strm, pos), "',' or ']' is expected")
            }
        }
        (vpos, Seq(array(elems.rev())))
    }

    fun parse_map(strm: stream_t, pos: int) {
        var elems: (string, t) list = []
        var vpos = pos
        while true {
            val (c, pos, _) = Lxu.skip_spaces(strm, vpos, false)
            if c == '}' {vpos = pos+1; break}
            val (pos, k) =
                if c == '\"' { // "
                    val (pos, result, dl, _) =
                        Lxu.getstring(s, pos+1, getloc(strm, pos+1), '\"' /* " */, false, false)
                    strm.lineno += dl
                    (pos, result)
                } else if c.isalpha() || c == '_' {
                    vpos = pos+1
                    while true {
                        val c = strm.buf.zero[vpos]
                        if !c.isalnum() && c != '_' {break}
                        vpos += 1
                    }
                    val substr = strm.buf[pos:vpos]
                    (vpos, substr.copy())
                } else if c.isdigit() || c == '-' {
                    vpos = pos+1
                    while true {
                        val c = strm.buf.zero[vpos]
                        if !c.isdigit() {break}
                        vpos += 1
                    }
                    val substr = strm.buf[pos:vpos]
                    (vpos, substr.copy())
                } else {
                    throw Lxu.LexerError(getloc(strm, pos),
                        "unexpected character; Json key, \"...\" or <identifier> is expected")
                }
            val pos =
                if strm.buf.zero[pos] == ':' {pos+1}
                else {
                    val (c, pos, _) = Lxu.skip_spaces(strm, vpos, false)
                    if c != ':' {
                        throw Lxu.LexerError(getloc(strm, pos), "':' is expected after a key")
                    }
                    pos+1
                }
            val (pos, v) = parse_value(strm, pos, true)
            elems = (k, v) :: elems
            val c = strm.buf.zero[pos]
            val pos = if c != ',' && c != '}' {
                Lxu.skip_spaces(strm, pos, false).1
            } else { pos }
            if strm.buf.zero[pos] == ',' {
                vpos = pos+1
            } else if strm.buf.zero[pos] == '}' {
                vpos = pos+1
                break
            } else {
                throw Lxu.LexerError(getloc(strm, pos), "',' or '}' is expected")
            }
        }
        (vpos, Map(array(elems.rev())))
    }

    parse_value(strm, 0, true).1
}

fun scalar2string(js: t): (string, bool)
{
    | Int i => (string(i), true)
    | Real f => (string(f), true)
    | Bool b => (string(b), true)
    | Str s => (repr(s), true)
    | Null => ("null", true)
    | _ => ("", false)
}

@private fun print_(js: t, ofs: int, indent: string, printf: string -> void)
{
    val W0 = 80, W1 = 100
    fun all_scalars(l: t []) =
        all(for x <- l {
            | Int _ | Real _ | Bool _ | Str _ | Null => true
            | _ => false
            })
    fun process_comments(j: t, indent: string) =
        match j {
        | Commented(comm, nested_j) =>
            printf(f"// {comm}\n{indent}")
            process_comments(nested_j, indent)
        | _ => j
        }
    val l_oldind = length(indent)
    val newind = indent + (if l_oldind > 40 {" "} else if l_oldind > 20 {"  "} else {"   "})
    val l_newind = length(newind)
    val js = process_comments(js, indent)
    val (str, printed) = scalar2string(js)
    match (printed, js) {
    | (true, _) =>
        printf(str)
        ofs + length(str)
    | (_, Commented(comm, nested_js)) =>
        throw Fail("comments are not expected here")
    | (_, Map(m)) =>
        printf("{\n")
        val n = m.size()
        for (k, v)@i <- m {
            printf(newind)
            val v = process_comments(v, newind)
            val prefix = f"{repr(k)}: "
            printf(prefix)
            ignore(print_(v, l_newind + length(prefix), newind, printf))
            if i < n-1 {printf(",\n")}
        }
        printf(f"\n{indent}}")
        l_oldind+1
    | (_, Seq(l)) =>
        if all_scalars(l) {
            val n = l.size()
            printf("[ ")
            val fold ofs = ofs + 2 for x@i <- l {
                val (str, printed) = scalar2string(x)
                if !printed { throw Fail("scalar is expected here") }
                val lstr = length(str)
                val ofs = if ofs > l_newind && ofs + lstr > W1 {
                    printf(f"\n{newind}"); l_newind
                } else { ofs }
                printf(str)
                val ofs = ofs + lstr
                if i < n-1 {
                    printf(",")
                    if ofs+1 > W0 {
                        printf(f"\n{newind}"); l_newind
                    } else { printf(" "); ofs + 2 }
                } else { printf(" "); ofs }
            }
            printf("]"); ofs + 1
        } else {
            printf("[\n")
            val n = l.size()
            for v@i <- l {
                printf(newind)
                val v = process_comments(v, newind)
                ignore(print_(v, l_newind, newind, printf))
                if i < n-1 {printf(",\n")}
            }
            printf(f"\n{indent}]")
            l_oldind+1
        }
    }
}

fun print_to_file(js: t, filename: string)
{
    val f = File.open(filename, "wt")
    ignore(print_(js, 0, "", fun(s: string) {f.print(s)}))
    f.close()
}
fun print(js: t) = ignore(print_(js, 0, "", fun(s: string) {print(s)}))
fun string(js: t) {
    var sl: string list = []
    val _ = print_(js, 0, "", fun(s: string) {sl = s :: sl})
    "".join(sl.rev())
}
