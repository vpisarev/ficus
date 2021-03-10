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
    print_f: string -> void
    get_f: void -> string
    _state: state_t ref
}

fun no_get(): string = ""

fun make_pprinter(margin: int, print_f: string->void, get_f: void->string): t
{
    val n=max(margin, 16)*3
    val pp = t {
        margin = margin,
        print_f = print_f,
        get_f = get_f,
        _state = ref (state_t {
            q = array(n, (PPEof, 0)),
            stack = array(n, 0),
            pp_stack = array(n, (0, Auto))
        })
    }
    reset(pp)
    pp
}

fun pprint_to_string(margin: int): t
{
    var strbuf : string list = []
    var curr = ""
    fun print_f(s: string)
    {
        if s =="\n" {
            if curr != "" {strbuf = curr :: strbuf}
            curr = ""
        } else {
            curr += s
        }
    }
    fun get_f()
    {
        if curr != "" {strbuf = curr :: strbuf}
        join("\n", strbuf.rev())
    }
    make_pprinter(margin, print_f, get_f)
}

fun pprint_to_file(margin: int, f: File.t): t
{
    fun print_f(s: string) { f.print(s) }
    make_pprinter(margin, print_f, no_get)
}

fun pprint_to_stdout(margin: int): t =
    pprint_to_file(margin, File.stdout)

fun reset(pp: PP.t): void
{
    pp._state->space = pp.margin
    pp._state->left = 0
    pp._state->right = 0
    pp._state->top = 0
    pp._state->bottom = 0
    pp._state->emptystack = true
    pp._state->pp_top = 0
}

fun flush(pp: PP.t): void
{
    if !pp._state->emptystack {
        check_stack(pp, 0)
        ignore(advance_left(pp))
    }
}

fun begin(pp: PP.t, indent: int) = begin(pp, indent, Auto)
fun beginv(pp: PP.t, indent: int) = begin(pp, indent, Consistent)

fun begin(pp: PP.t, indent: int, style: ppstyle_t): void
{
    val right = if pp._state->emptystack {
        pp._state->lefttotal = 1
        pp._state->righttotal = 1
        pp._state->left = 0
        pp._state->right = 0
        0
    } else {
        advance_right(pp)
    }
    val tk = PPBegin(indent, style)
    pp._state->q[right] = (tk, -pp._state->righttotal)
    scan_push(pp, right)
}

fun end(pp: PP.t): void
{
    if pp._state->emptystack {
        pprint(pp, PPEnd, 0)
    } else {
        val right = advance_right(pp)
        pp._state->q[right] = (PPEnd, -1)
        scan_push(pp, right)
    }
}

fun br(pp: PP.t, spaces: int, offset: int, sep: char): void
{
    val right = if pp._state->emptystack {
        pp._state->lefttotal = 1
        pp._state->righttotal = 1
        pp._state->left = 0
        pp._state->right = 0
        0
    } else {
        advance_right(pp)
    }
    check_stack(pp, 0)
    scan_push(pp, right)
    val tk = PPBreak(spaces, offset, sep)
    pp._state->q[right] = (tk, -pp._state->righttotal)
    pp._state->righttotal += spaces
}

fun cut(pp: PP.t) = br(pp, 0, 0, '\0')
fun space(pp: PP.t) = br(pp, 1, 0, '\0')
fun sep_space(pp: PP.t, c: char) = br(pp, 2, 0, c)

fun str(pp: PP.t, s: string): void
{
    val tk = PPString(s), l = s.length()
    if pp._state->emptystack {
        pprint(pp, tk, l)
    } else {
        pp._state->q[advance_right(pp)] = (tk, l)
        pp._state->righttotal += l
        check_stream(pp)
    }
}

@private fun check_stream(pp: PP.t): void
{
    if pp._state->righttotal - pp._state->lefttotal > pp._state->space {
        if !pp._state->emptystack &&
            pp._state->left == pp._state->stack[pp._state->bottom] {
                pp._state->q[scan_pop_bottom(pp)].1 = 1000000
            }
        val left = advance_left(pp)
        if left != pp._state->right { check_stream(pp) }
    }
}

@private fun scan_push(pp: PP.t, i: int): void
{
    if !pp._state->emptystack {
        val top = (pp._state->top + 1) % size(pp._state->stack)
        pp._state->top = top
        if top == pp._state->bottom {throw PPStackOverflow}
    }
    pp._state->stack[pp._state->top] = i
    pp._state->emptystack = false
}

@private fun scan_pop(pp: PP.t): int
{
    if pp._state->emptystack {throw PPEmptyStack}
    val top = pp._state->top
    val x = pp._state->stack[top]
    if top == pp._state->bottom {
        pp._state->emptystack = true
    } else {
        val stacksize = size(pp._state->stack)
        pp._state->top = (top + stacksize - 1) % stacksize
    }
    x
}

@private fun scan_pop_bottom(pp: PP.t): int
{
    if pp._state->emptystack {throw PPEmptyStack}
    val bottom = pp._state->bottom
    val x = pp._state->stack[bottom]
    if bottom == pp._state->top {
        pp._state->emptystack = true
    } else {
        pp._state->bottom = (bottom + 1) % size(pp._state->stack)
    }
    x
}

@private fun advance_right(pp: PP.t): int
{
    val right = (pp._state->right + 1) % size(pp._state->q)
    pp._state->right = right
    if right == pp._state->left {throw PPQueueOverflow}
    right
}

@private fun advance_left(pp: PP.t): int
{
    val left = pp._state->left
    val (tk, len) = pp._state->q[left]
    if len >= 0 {
        pprint(pp, tk, len)
        val spaces = match tk {
            | PPBreak(spaces, _, _) => spaces
            | PPString(s) => s.length()
            | _ => 0
        }
        pp._state->lefttotal += spaces
        if left != pp._state->right {
            pp._state->left = (left + 1) % size(pp._state->q)
            advance_left(pp)
        } else { left }
    } else { left }
}

@private fun check_stack(pp: PP.t, k: int): void =
    if !pp._state->emptystack {
        val x = pp._state->stack[pp._state->top]
        val (tk, len) = pp._state->q[x]
        match tk {
        | PPBegin(_, _) =>
            if k > 0 {
                val _ = scan_pop(pp)
                pp._state->q[x].1 = len + pp._state->righttotal
                check_stack(pp, k - 1)
            }
        | PPEnd =>
            val _ = scan_pop(pp)
            pp._state->q[x].1 = 1
            check_stack(pp, k + 1)
        | _ =>
            val _ = scan_pop(pp)
            pp._state->q[x].1 = len + pp._state->righttotal
            if k > 0 {check_stack(pp, k)}
        }
    }

@private fun pp_newline(pp: PP.t, n: int) = pp.print_f("\n" + (' '*n))
@private fun pp_indent(pp: PP.t, n: int, c: char) =
    if c == '\0' {
        pp.print_f(' '*n)
    } else {
        pp.print_f(c + ' '*(n-1))
    }

@private fun pprint(pp: PP.t, x: pptok_t, len: int) {
    val top = pp._state->pp_top
    //println(f"pprinting {(x,len)}; space={pp._state->space}")
    match x {
    | PPBegin(offset, style) =>
        val x = if len > pp._state->space {
                (pp._state->space - offset,
                match style { | Consistent => Consistent | _ => Auto })
            } else {
                (0, Fits)
            }
        pp._state->pp_stack[top] = x
        pp._state->pp_top = top + 1
    | PPEnd =>
        if top > 0 {
            pp._state->pp_top = top - 1
        }
    | PPBreak(spaces, offset, c) =>
        val (block_offset, style) =
            if top > 0 { pp._state->pp_stack[top-1] }
            else { (0, Auto) }
        match style {
        | Fits =>
            pp._state->space -= spaces
            pp_indent(pp, spaces, c)
        | Consistent =>
            pp._state->space = block_offset - offset
            pp_newline(pp, pp.margin - pp._state->space)
        | _ =>
            if len > pp._state->space {
                pp._state->space = block_offset - offset
                pp_newline(pp, pp.margin - pp._state->space)
            } else {
                pp._state->space -= spaces
                pp_indent(pp, spaces, c)
            }
        }
    | PPString(s) =>
        pp._state->space = max(pp._state->space - len, 0)
        pp.print_f(s)
    }
}
