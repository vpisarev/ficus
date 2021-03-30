/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// O(N)-time, O(1)-space pretty-printing engine using the following algorithm:
// Oppen, Dereck C. "Pretty-printing". ACM trans. program. lang. syst., 2(4), 465–483, 1980.

import File

exception PPStackOverflow
exception PPEmptyStack
exception PPQueueOverflow

type ppstyle_t = Auto | Fits | Consistent
type pptok_t =
    | PPString: string
    | PPBreak: (int, int, char)
    | PPBegin: (int, ppstyle_t)
    | PPEnd
    | PPEof
type ppelem_t = (pptok_t, int)

fun string(s: ppstyle_t) { | Auto => "Auto" | Fits => "Fits" | _ => "Consistent" }
fun string(t: pptok_t) {
    | PPString (s) => f"String({s})"
    | PPBreak(s, o, c) => f"Break({s}, {o}, '{c}')"
    | PPBegin(o, s) => f"Begin({o}, {s})"
    | PPEnd => "End"
    | PPEof => "Eof"
}
fun string((t, l): ppelem_t) = f"({t}, {l})"

type state_t =
{
    space: int=0
    left: int=0
    right: int=0
    top: int=0
    bottom: int=0
    lefttotal: int=0
    righttotal: int=0
    q: ppelem_t []
    stack: int []
    pp_stack: (int, ppstyle_t) []
    pp_top: int=0
    emptystack: bool=true
}

object type t =
{
    margin: int
    default_indent: int
    print_f: string -> void
    get_f: void -> string list
    r: state_t ref
}

fun no_get(): string list = []

fun make_pprinter(margin: int, print_f: string->void,
                  get_f: void->string list, ~default_indent: int=4): t
{
    val n=max(margin, 16)*3
    val pp = t {
        margin = margin,
        default_indent = default_indent,
        print_f = print_f,
        get_f = get_f,
        r = ref (state_t {
            q = array(n, (PPEof, 0)),
            stack = array(n, 0),
            pp_stack = array(n, (0, Auto))
        })
    }
    reset(pp)
    pp
}

fun pprint_to_string_list(margin: int, ~default_indent: int=4): t
{
    var lines : string list = []
    var capacity = 100, bufsize = 0
    var curr = array(capacity, ' ')
    fun print_f(s: string)
    {
        val strsize = s.length(), bufsz = bufsize
        while bufsz + strsize > capacity {
            capacity *= 2
            curr = [| \curr, \curr |]
        }
        for c@i <- s { curr[bufsz+i] = c }
        bufsize = bufsz + strsize
        if s.endswith('\n') {
            lines = string(curr[:bufsize]).rstrip() :: lines
            bufsize = 0
        }
    }
    fun get_f()
    {
        if bufsize > 0 {lines = string(curr[:bufsize]).strip() :: lines}
        lines.rev()
    }
    make_pprinter(margin, print_f, get_f, default_indent=default_indent)
}

fun pprint_to_file(margin: int, f: File.t, ~default_indent: int=4): t
{
    fun print_f(s: string) { f.print(s) }
    make_pprinter(margin, print_f, no_get, default_indent=default_indent)
}

fun pprint_to_stdout(margin: int, ~default_indent: int=4): t =
    pprint_to_file(margin, File.stdout, default_indent=default_indent)

fun reset(pp: PP.t): void
{
    pp.r->space = pp.margin
    pp.r->left = 0
    pp.r->right = 0
    pp.r->top = 0
    pp.r->bottom = 0
    pp.r->emptystack = true
    pp.r->pp_top = 0
}

fun flush(pp: PP.t): void
{
    if !pp.r->emptystack {
        check_stack(pp, 0)
        ignore(advance_left(pp))
    }
}

fun begin(pp: PP.t) = begin(pp, pp.default_indent, Auto)
fun begin(pp: PP.t, indent: int) = begin(pp, indent, Auto)
fun beginv(pp: PP.t) = begin(pp, pp.default_indent, Consistent)
fun beginv(pp: PP.t, indent: int) = begin(pp, indent, Consistent)

fun begin(pp: PP.t, indent: int, style: ppstyle_t): void
{
    val right = if pp.r->emptystack {
        pp.r->lefttotal = 1
        pp.r->righttotal = 1
        pp.r->left = 0
        pp.r->right = 0
        0
    } else {
        advance_right(pp)
    }
    val tk = PPBegin(indent, style)
    pp.r->q[right] = (tk, -pp.r->righttotal)
    scan_push(pp, right)
}

fun end(pp: PP.t): void
{
    if pp.r->emptystack {
        pprint(pp, PPEnd, 0)
    } else {
        val right = advance_right(pp)
        pp.r->q[right] = (PPEnd, -1)
        scan_push(pp, right)
    }
}

fun br(pp: PP.t, spaces: int, offset: int, ~sep: char='\0'): void
{
    val right = if pp.r->emptystack {
        pp.r->lefttotal = 1
        pp.r->righttotal = 1
        pp.r->left = 0
        pp.r->right = 0
        0
    } else {
        advance_right(pp)
    }
    check_stack(pp, 0)
    scan_push(pp, right)
    val tk = PPBreak(spaces, offset, sep)
    pp.r->q[right] = (tk, -pp.r->righttotal)
    pp.r->righttotal += spaces
}

fun cut(pp: PP.t) = br(pp, 0, 0)
fun space(pp: PP.t) = br(pp, 1, 0)
fun sep_space(pp: PP.t, c: char) = br(pp, 2, 0, sep=c)
fun opt_semi(pp: PP.t) = br(pp, 2, 0, sep=';')
fun break0(pp: PP.t) = br(pp, 1, 0)
fun breaki(pp: PP.t) = br(pp, 1, pp.default_indent)
fun breaku(pp: PP.t) = br(pp, 1, -pp.default_indent)
fun newline(pp: PP.t) = br(pp, pp.margin, 0)

fun str(pp: PP.t, s: string): void
{
    val tk = PPString(s), l = s.length()
    if pp.r->emptystack {
        pprint(pp, tk, l)
    } else {
        pp.r->q[advance_right(pp)] = (tk, l)
        pp.r->righttotal += l
        check_stream(pp)
    }
}

@private fun check_stream(pp: PP.t): void
{
    if pp.r->righttotal - pp.r->lefttotal > pp.r->space {
        if !pp.r->emptystack &&
            pp.r->left == pp.r->stack[pp.r->bottom] {
                pp.r->q[scan_pop_bottom(pp)].1 = 1000000
            }
        val left = advance_left(pp)
        if left != pp.r->right { check_stream(pp) }
    }
}

@private fun scan_push(pp: PP.t, i: int): void
{
    if !pp.r->emptystack {
        val top = (pp.r->top + 1) % size(pp.r->stack)
        pp.r->top = top
        if top == pp.r->bottom {throw PPStackOverflow}
    }
    pp.r->stack[pp.r->top] = i
    pp.r->emptystack = false
}

@private fun scan_pop(pp: PP.t): int
{
    if pp.r->emptystack {throw PPEmptyStack}
    val top = pp.r->top
    val x = pp.r->stack[top]
    if top == pp.r->bottom {
        pp.r->emptystack = true
    } else {
        val stacksize = size(pp.r->stack)
        pp.r->top = (top + stacksize - 1) % stacksize
    }
    x
}

@private fun scan_pop_bottom(pp: PP.t): int
{
    if pp.r->emptystack {throw PPEmptyStack}
    val bottom = pp.r->bottom
    val x = pp.r->stack[bottom]
    if bottom == pp.r->top {
        pp.r->emptystack = true
    } else {
        pp.r->bottom = (bottom + 1) % size(pp.r->stack)
    }
    x
}

@private fun advance_right(pp: PP.t): int
{
    val right = (pp.r->right + 1) % size(pp.r->q)
    pp.r->right = right
    if right == pp.r->left {throw PPQueueOverflow}
    right
}

@private fun advance_left(pp: PP.t): int
{
    val left = pp.r->left
    val (tk, len) = pp.r->q[left]
    if len >= 0 {
        pprint(pp, tk, len)
        val spaces = match tk {
            | PPBreak(spaces, _, _) => spaces
            | PPString(s) => s.length()
            | _ => 0
        }
        pp.r->lefttotal += spaces
        if left != pp.r->right {
            pp.r->left = (left + 1) % size(pp.r->q)
            advance_left(pp)
        } else { left }
    } else { left }
}

@private fun check_stack(pp: PP.t, k: int): void =
    if !pp.r->emptystack {
        val x = pp.r->stack[pp.r->top]
        val (tk, len) = pp.r->q[x]
        match tk {
        | PPBegin(_, _) =>
            if k > 0 {
                val _ = scan_pop(pp)
                pp.r->q[x].1 = len + pp.r->righttotal
                check_stack(pp, k - 1)
            }
        | PPEnd =>
            val _ = scan_pop(pp)
            pp.r->q[x].1 = 1
            check_stack(pp, k + 1)
        | _ =>
            val _ = scan_pop(pp)
            pp.r->q[x].1 = len + pp.r->righttotal
            if k > 0 {check_stack(pp, k)}
        }
    }

@private fun pp_newline(pp: PP.t, n: int)
{
    pp.print_f("\n"); pp.print_f(' '*n)
}

@private fun pp_indent(pp: PP.t, n: int, c: char) =
    if c == '\0' {
        pp.print_f(' '*n)
    } else {
        pp.print_f(c + ' '*(n-1))
    }

@private fun pprint(pp: PP.t, x: pptok_t, len: int) {
    val top = pp.r->pp_top
    //println(f"pprinting {(x,len)}; space={pp.r->space}")
    match x {
    | PPBegin(offset, style) =>
        val x =
            if len > pp.r->space {
                val sp = max(min(pp.r->space - offset, pp.margin), 10)
                (sp, match style { | Consistent => Consistent | _ => Auto })
            } else {
                (pp.margin, Fits)
            }
        pp.r->pp_stack[top] = x
        pp.r->pp_top = top + 1
    | PPEnd =>
        if top > 0 {
            pp.r->pp_top = top - 1
        }
    | PPBreak(spaces, offset, c) =>
        val (block_offset, style) =
            if top > 0 { pp.r->pp_stack[top-1] }
            else { (0, Auto) }
        match style {
        | Fits =>
            pp.r->space -= spaces
            pp_indent(pp, spaces, c)
        | Consistent =>
            pp.r->space = min(block_offset - offset, pp.margin)
            pp_newline(pp, pp.margin - pp.r->space)
            pp.r->space = max(pp.r->space, 10)
        | _ =>
            if len > pp.r->space {
                pp.r->space = block_offset - offset
                pp_newline(pp, pp.margin - pp.r->space)
            } else {
                pp.r->space -= spaces
                pp_indent(pp, spaces, c)
            }
        }
    | PPString(s) =>
        pp.r->space = max(pp.r->space - len, 0)
        pp.print_f(s)
    | PPEof =>
        flush(pp)
    }
}
