/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Ficus recursive descent parser

import File, Map, Sys
from Ast import *
from Lexer import *

exception ParseError: (loc_t, string)
var last_loc = noloc

type kw_mode_t = KwNone | KwMay | KwMust
type tklist_t = (token_t, loc_t) list
type elist_t = exp_t list
type id_exp_t = (id_t, exp_t)
type id_elist_t = id_exp_t list

fun make_unary(uop: unary_t, e: exp_t, loc: loc_t) = ExpUnary(uop, e, make_new_ctx(loc))
fun make_binary(bop: binary_t, e1: exp_t, e2: exp_t, loc: loc_t) = ExpBinary(bop, e1, e2, make_new_ctx(loc))
fun make_literal(lit: lit_t, loc: loc_t) = ExpLit(lit, (get_lit_typ(lit), loc))
fun make_ident(i: id_t, loc: loc_t) = ExpIdent(i, make_new_ctx(loc))
fun make_ident(s: string, loc: loc_t) = ExpIdent(get_id(s), make_new_ctx(loc))
fun make_tuple(el: exp_t list, loc: loc_t) = ExpMkTuple(el, make_new_ctx(loc))

fun parse_err(ts: tklist_t, msg: string): exn
{
    val loc = match ts {
    | (_, l) :: _ => l
    | _ => last_loc
    }
    ParseError(loc, msg)
}

fun match_paren((ts: tklist_t, e: exp_t), ct: token_t, ol: loc_t): (tklist_t, exp_t)
{
    match ts {
    | (ct_, l) :: rest when ct_ == ct => (rest, e)
    | _ => throw parse_err(ts, f"'{tok2str(ct).1}' is expected; the opening paren is here {ol}")
    }
}

fun is_for_start((t: token_t, l: loc_t), nested: bool) =
match t {
    | FOR(ne) => (ne | nested)
    | PARALLEL => !nested
    | UNZIP => !nested
    | _ => false
}

fun check_ne(ne: bool, ts: tklist_t) = if !ne {
    throw parse_err(ts, "new line or ';' is expected before new expression")
}

fun parse_exp_list(ts0: tklist_t, parse_exp_f: tklist_t->(tklist_t, exp_t),
                   ct: token_t, ~kw_mode: kw_mode_t, ~allow_empty: bool,
                   ~stop_at_semicolon: bool=false)
    : (tklist_t, elist_t, id_elist_t)
{
    fun parse_exp_list_(idx: int, ts: tklist_t, el: elist_t, kw_el: id_elist_t) =
        match ts {
        | (t, _) :: rest when t == ct =>
            if idx == 0 && !allow_empty {throw parse_err(ts, "empty expression list is not allowed here")}
            (rest, el.rev(), kw_el.rev())
        | (SEMICOLON, _) :: rest =>
            if idx == 0 || !stop_at_semicolon {throw parse_err(ts, "unxpected ';'")}
            (rest, el.rev(), kw_el.rev())
        | (t, _) :: rest =>
            // 'eat' comma in the middle of list
            val ts = match t {
                | COMMA =>
                    if idx == 0 {throw parse_err(ts, "unxpected ',' in the beginning of the list")}
                    rest
                | _ =>
                    if idx > 0 {throw parse_err(ts, f"',' or '{tok2str(ct).1}' is expected")}
                    ts
                }
            // capture the next expression or the next pair ident=exp and then parse the remaining items
            // using tail recursion, should be pretty efficient.
            match ts {
            | (IDENT(_, i), _) :: (EQUAL, _) :: rest =>
                if kw_mode == KwNone { throw parse_err(ts, f"unexpected keyword element '{i}=...'") }
                val (ts, e) = parse_complex_exp(rest)
                parse_exp_list_(idx+1, ts, el, (get_id(i), e) :: kw_el)
            | _ =>
                if kw_mode == KwMust { throw parse_err(ts, f"expect a keyword element here '<ident> = ...'") }
                val (ts, e) = parse_exp_f(ts)
                parse_exp_list_(idx+1, ts, e :: el, kw_el)
            }
        | _ => throw parse_err(ts0, "the expression list is not complete by the end of file, check parentheses")
    }
    parse_exp_list_(0, ts0, [], [])
}

fun parse_typed_exp(ts: tklist_t): (tklist_t, exp_t) {
    val (ts, e) = parse_complex_exp(ts)
    match ts {
    | (COLON, _) :: rest =>
        val (ts, t) = parse_typespec(rest)
        (ts, ExpTyped(e, t, (t, get_exp_loc(e))))
    | (CAST, _) :: rest =>
        val (ts, t) = parse_typespec(rest)
        (ts, ExpCast(e, t, (make_new_typ(), get_exp_loc(e))))
    | _ => (ts, e)
    }
}

fun parse_array_literal(ts: tklist_t, start: loc_t): (tklist_t, exp_t) = throw parse_err(ts, "array literals are not supported")
fun parse_complex_exp(ts: tklist_t): (tklist_t, exp_t) = throw parse_err(ts, "complex expressions are not supported")
fun parse_typespec(ts: tklist_t): (tklist_t, typ_t) = throw parse_err(ts, "type specifications are not supported")
fun parse_block(ts: tklist_t): (tklist_t, exp_t) = throw parse_err(ts, "blocks are not supported")
fun parse_for(ts: tklist_t, for_make: for_make_t): (tklist_t, exp_t) = throw parse_err(ts, "blocks are not supported")
fun parse_array_idx(ts: tklist_t): (tklist_t, exp_t) = parse_binary_exp(ts) // [TODO]: add range support

fun parse_atomic_exp(ts: tklist_t): (tklist_t, exp_t)
{
    match ts {
    | (IDENT(ne, i), l1) :: rest =>
        check_ne(ne, ts)
        (rest, make_ident(i, l1))
    | (LITERAL(lit), l1) :: rest => (rest, make_literal(lit, l1))
    | (LPAREN(ne), l1) :: (LBRACE, _) :: _ =>
        check_ne(ne, ts)
        match_paren(parse_block(ts.tl()), RPAREN, l1)
    | (LPAREN(ne), l1) :: (FOR(_), _) :: _ =>
        check_ne(ne, ts)
        match_paren(parse_for(ts.tl(), ForMakeTuple), RPAREN, l1)
    | (LPAREN(ne), l1) :: rest =>
        check_ne(ne, ts)
        val (ts, el, _) = parse_exp_list(rest, parse_typed_exp, RPAREN, kw_mode=KwNone, allow_empty=false)
        (ts, match el {
        | e :: [] => e
        | _ => make_tuple(el, l1)
        })
    | (LSQUARE(ne), l1) :: f :: rest when is_for_start(f, false) =>
        check_ne(ne, ts)
        match_paren(parse_for(ts.tl(), ForMakeArray), RSQUARE, l1)
    | (LSQUARE(ne), l1) :: rest =>
        check_ne(ne, ts)
        parse_array_literal(rest, l1)
    | (LLIST, l1) :: f :: _ when is_for_start(f, false) =>
        match_paren(parse_for(ts.tl(), ForMakeList), RLIST, l1)
    | (LLIST, l1) :: rest =>
        val (ts, el, _) = parse_exp_list(rest, parse_typed_exp, RLIST, kw_mode=KwNone, allow_empty=true)
        (ts, fold mklist_e = make_literal(LitNil, l1) for e <- el.rev() {
            ExpBinary(OpCons, e, mklist_e, make_new_ctx(get_exp_loc(e)))
        })
    | (t, _) :: _ =>
        throw parse_err(ts, f"unxpected token '{tok2str(t).1}'. An identifier, literal or expression enclosed in '( )', '[ ]' or '[: :]' brackets is expected here")
    | _ =>
        throw parse_err(ts, f"premature end of the stream; check the parens")
    }
}

fun parse_simple_exp(ts: tklist_t): (tklist_t, exp_t)
{
    fun extend_simple_exp_(ts: tklist_t, e: exp_t) {
        val eloc = get_exp_loc(e)
        // 1. element access (.|->) (int|ident)
        // 2. array access [...]
        // 3. function call (...)
        match ts {
        | (LPAREN(false), _) :: rest =>
            // function call ([TODO] support keyword args)
            val (ts, args, kw_args) = parse_exp_list(rest, parse_typed_exp, RPAREN, kw_mode=KwNone, allow_empty=true)
            extend_simple_exp_(ts, ExpCall(e, args, make_new_ctx(eloc)))
        | (LSQUARE(false), _) :: rest =>
            // array access ([TODO] support ranges)
            val (ts, idxs, _) = parse_exp_list(rest, parse_array_idx, RPAREN, kw_mode=KwNone, allow_empty=false)
            extend_simple_exp_(ts, ExpAt(e, BorderNone, InterpNone, idxs, make_new_ctx(eloc)))
        | (t1, _) :: (t2, l2) :: rest when
            // [TODO] add support for alternating patterns right into the pattern matching syntax
            (match t1 {| DOT | ARROW => true | _ => false}) &&
            (match t2 {| IDENT(false, _) | LITERAL(LitInt _) => true | _ => false}) =>
            val e = match t1 { | ARROW => make_unary(OpDeref, e, eloc) | _ => e }
            val i = match t2 {
                | IDENT(true, i) => make_ident(i, l2)
                | LITERAL(LitInt(i)) => make_literal(LitInt(i), l2)
                | _ => ExpNop(l2)
                }
            extend_simple_exp_(rest, ExpMem(e, i, make_new_ctx(eloc)))
        | (t1, _) :: (LBRACE, l2) :: rest when (match t1 {| DOT | ARROW => true | _ => false}) =>
            val e = match t1 { | ARROW => make_unary(OpDeref, e, eloc) | _ => e }
            val (ts, _, rec_init_elems) = parse_exp_list(rest, parse_typed_exp, RBRACE, kw_mode=KwMust, allow_empty=true)
            extend_simple_exp_(ts, ExpUpdateRecord(e, rec_init_elems, make_new_ctx(eloc)))
        | _ => (ts, e)
        }
    }

    val (ts, e) = parse_atomic_exp(ts)
    extend_simple_exp_(ts, e)
}

fun parse_deref_exp(ts: tklist_t): (tklist_t, exp_t)
{
    | (STAR(true), l1) :: rest =>
        val (ts, e) = parse_deref_exp(rest)
        (ts, make_unary(OpDeref, e, l1))
    | (POWER(true), l1) :: rest =>
        val (ts, e) = parse_deref_exp(rest)
        (ts, make_unary(OpDeref, make_unary(OpDeref, e, l1), l1))
    | _ =>
        parse_simple_exp(ts)
}

fun parse_apos_exp(ts: tklist_t): (tklist_t, exp_t)
{
    val (ts, e) = parse_deref_exp(ts)
    match ts {
    | (APOS, l1) :: rest => (rest, ExpUnary(OpApos, e, make_new_ctx(l1)))
    | _ => (ts, e)
    }
}

fun parse_unary_exp(ts: tklist_t): (tklist_t, exp_t)
{
    | (REF(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpMkRef, e, l1))
    | (MINUS(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpNegate, e, l1))
    | (PLUS(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpPlus, e, l1))
    | (TILDE, l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpBitwiseNot, e, l1))
    | (BACKSLASH, l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpExpand, e, l1))
    | _ =>
        parse_unary_exp(ts)
}

fun parse_binary_exp(ts: tklist_t) : (tklist_t, exp_t)
{
    fun parse_binary_exp_(ts: tklist_t, result: exp_t, min_prec: int) =
    match ts {
    | (t, l) :: rest =>
        // roughly sort binary ops by how often they are met in the code,
        // so that we have less checks in general
        val (bop, prec, assoc) = match t {
            | PLUS(false) => (OpAdd, 210, AssocLeft)
            | MINUS(false) => (OpSub, 210, AssocLeft)
            | STAR(false) => (OpMul, 220, AssocLeft)
            | SLASH => (OpDiv, 220, AssocLeft)
            | PERCENT => (OpMod, 220, AssocLeft)
            | CONS => (OpCons, 100, AssocRight)
            | POWER(false) => (OpPow, 230, AssocRight)
            | SHIFT_LEFT => (OpShiftLeft, 200, AssocLeft)
            | SHIFT_RIGHT => (OpShiftRight, 200, AssocLeft)
            | BITWISE_OR => (OpBitwiseOr, 130, AssocLeft)
            | BITWISE_AND => (OpBitwiseAnd, 150, AssocLeft)
            | BITWISE_XOR => (OpBitwiseXor, 140, AssocLeft)
            | SPACESHIP => (OpSpaceship, 170, AssocLeft)
            | DOT_MINUS(false) => (OpSub, 210, AssocLeft)
            | DOT_STAR => (OpDotMul, 220, AssocLeft)
            | DOT_SLASH => (OpDotDiv, 220, AssocLeft)
            | DOT_PERCENT => (OpDotMod, 220, AssocLeft)
            | DOT_STAR => (OpDotMul, 220, AssocLeft)
            | DOT_CMP(cmpop) => (OpDotCmp(cmpop), 180, AssocLeft)
            | DOT_SPACESHIP => (OpDotSpaceship, 190, AssocLeft)
            | _ => (OpAdd, -1, AssocLeft)
        }
        if prec < min_prec { (ts, result) }
        else {
            val next_min_prec = match assoc { | AssocLeft => prec+1 | _ => prec }
            val (ts, e) = parse_unary_exp(rest)
            // non-tail call, parse rhs
            val (ts, rhs) = parse_binary_exp_(ts, e, next_min_prec)
            val result = make_binary(bop, result, rhs, l)
            parse_binary_exp_(ts, result, min_prec)
        }
    | _ => (ts, result)
    }
    val (ts, e) = parse_unary_exp(ts)
    parse_binary_exp_(ts, e, 0)
}
