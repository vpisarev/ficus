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

fun tok2str(ts: (token_t, loc_t) list) =
    " ".join([: for (t, _) <- ts {tok2str(t).0} :])

fun make_unary(uop: unary_t, e: exp_t, loc: loc_t) = ExpUnary(uop, e, make_new_ctx(loc))
fun make_binary(bop: binary_t, e1: exp_t, e2: exp_t, loc: loc_t) = ExpBinary(bop, e1, e2, make_new_ctx(loc))
fun make_literal(lit: lit_t, loc: loc_t) = ExpLit(lit, (get_lit_typ(lit), loc))
fun make_ident(i: id_t, loc: loc_t) = ExpIdent(i, make_new_ctx(loc))
fun make_ident(s: string, loc: loc_t) = ExpIdent(get_id(s), make_new_ctx(loc))
fun make_tuple(el: exp_t list, loc: loc_t) = ExpMkTuple(el, make_new_ctx(loc))

fun transform_fold_exp(special: string, fold_pat: pat_t, fold_init_exp: exp_t,
                       nested_fold: ((pat_t, exp_t) list, pat_t) list, fold_body: exp_t)
{
    val acc_loc = get_pat_loc(fold_pat)
    val fr_id = __fold_result_id__
    val fr_exp = make_ident(fr_id, acc_loc)
    val (for_iter_e, nested_fold_cl, fold_body) =
        process_nested_for(nested_fold, fold_body)
    val body_loc = get_exp_loc(fold_body)
    val body_end_loc = get_end_loc(body_loc)
    val void_ctx = (TypVoid, body_loc)
    val bool_ctx = (TypBool, body_loc)
    val (fr_decl, new_body, fr_exp) =
    match special {
    | "" =>
        val fr_decl = DefVal(PatIdent(fr_id, acc_loc), fold_init_exp, default_var_flags(), acc_loc)
        val acc_decl = DefVal(fold_pat, fr_exp, default_tempval_flags(), acc_loc)
        val update_fr = ExpAssign(fr_exp, fold_body, body_loc)
        val new_body = ExpSeq([: acc_decl, update_fr :], void_ctx)
        (fr_decl, new_body, fr_exp)
    | "all" | "exists" =>
        val is_any = special == "exists"
        val fr_decl = DefVal(PatIdent(fr_id, acc_loc), ExpLit(LitBool(!is_any),
                            (TypBool, acc_loc)), default_var_flags(), acc_loc)
        val break_exp = ExpSeq([: ExpAssign(fr_exp, ExpLit(LitBool(is_any), bool_ctx), body_loc),
                                ExpBreak(true, body_loc) :], void_ctx)
        val predicate_exp = if is_any { fold_body }
                            else { ExpUnary(OpLogicNot, fold_body, bool_ctx) }
        val new_body = ExpIf(predicate_exp, break_exp, ExpNop(body_loc), void_ctx)
        (fr_decl, new_body, fr_exp)
    | "find" | "find_opt" =>
        val none = get_id("None")
        val some = get_id("Some")
        val fr_decl = DefVal(PatIdent(fr_id, acc_loc), make_ident(none, acc_loc),
                            default_var_flags(), acc_loc)
        val mksome_exp = ExpCall(make_ident(some, body_loc), for_iter_e :: [],
                                 make_new_ctx(body_loc))
        val break_exp = ExpSeq([: ExpAssign(fr_exp, mksome_exp, body_loc),
                                ExpBreak(true, body_loc) :], void_ctx)
        val new_body = ExpIf(fold_body, break_exp, ExpNop(body_loc), void_ctx)
        val new_fr_exp =
        match special {
        | "find" =>
            val x = get_id("x")
            val some_pattern_clause = ([: PatVariant(some, [: PatIdent(x, body_end_loc) :], body_end_loc) :], make_ident(x, body_end_loc))
            val else_pattern_clause =
            ([: PatAny(body_end_loc) :], ExpThrow(make_typed_ident(get_id("NotFoundError"), TypExn, body_end_loc), body_end_loc))
            ExpMatch(fr_exp, [: some_pattern_clause, else_pattern_clause :], (make_new_typ(), body_end_loc))
        | _ => fr_exp
        }
        (fr_decl, new_body, new_fr_exp)
    | f => raise_syntax_err_loc(acc_loc, sprintf("unknown fold variation '%s'", f))
    }
    val for_exp = make_for(nested_fold_cl, new_body, default_for_flags().{for_flag_fold=true})
    ExpSeq([: fr_decl, for_exp, fr_exp :], (make_new_typ(), curr_loc()))
}

fun parse_err(ts: tklist_t, msg: string): exn
{
    val loc = match ts {
    | (_, l) :: _ => l
    | _ => last_loc
    }
    ParseError(loc, msg)
}

fun expseq2exp(eseq: exp_t list, loc: loc_t) =
    match eseq {
    | [] => ExpNop(loc)
    | e :: [] => e
    | _ =>
        val llist = [: for e <- eseq {get_exp_loc(e)} :]
        val loc = loclist2loc(llist, loc)
        ExpSeq(eseq, (make_new_typ(), loc))
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
fun parse_typespec(ts: tklist_t): (tklist_t, typ_t) = throw parse_err(ts, "type specifications are not supported")
fun parse_for(ts: tklist_t, for_make: for_make_t): (tklist_t, exp_t) = throw parse_err(ts, "blocks are not supported")
fun parse_array_idx(ts: tklist_t): (tklist_t, exp_t) = parse_complex_exp(ts) // [TODO]: add range support

fun parse_atomic_exp(ts: tklist_t): (tklist_t, exp_t)
{
    println(f"atomic_exp({tok2str(ts)})\n")
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
    println(f"simple_exp({tok2str(ts)})\n")
    fun extend_simple_exp_(ts: tklist_t, e: exp_t) {
        println(f"extend_simple_exp({tok2str(ts)})\n")
        val eloc = get_exp_loc(e)
        // 1. element access (.|->) (int|ident)
        // 2. array access [...]
        // 3. function call (...)
        match ts {
        | (LPAREN(false), _) :: rest =>
            // function call ([TODO] support keyword args)
            val (ts, args, kw_args) = parse_exp_list(rest, parse_typed_exp, RPAREN,
                                            kw_mode=KwNone, allow_empty=true)
            extend_simple_exp_(ts, ExpCall(e, args, make_new_ctx(eloc)))
        | (LSQUARE(false), _) :: rest =>
            // array access ([TODO] support ranges)
            val (ts, idxs, _) = parse_exp_list(rest, parse_array_idx, RPAREN,
                                            kw_mode=KwNone, allow_empty=false)
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
            val (ts, _, rec_init_elems) = parse_exp_list(rest, parse_typed_exp, RBRACE,
                                                kw_mode=KwMust, allow_empty=true)
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
    println(f"unary_exp({tok2str(ts)})\n")
    match ts {
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
    | (LOGICAL_NOT, l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpLogicNot, e, l1))
    | (BACKSLASH, l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpExpand, e, l1))
    | _ =>
        parse_apos_exp(ts)
    }
}

fun parse_binary_exp(ts: tklist_t) : (tklist_t, exp_t)
{
    println(f"binary_exp({tok2str(ts)})\n")
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

fun parse_exp(ts: tklist_t): (tklist_t, exp_t)
{
    fun extend_chained_cmp_(ts: tklist_t,
        chain: ((cmpop_t, loc_t), exp_t) list): (tklist_t, exp_t)
    {
    println(f"binary_exp({tok2str(ts)})\n")
    match ts {
    | (CMP(cmpop), l1) :: rest =>
        val (ts, e) = parse_binary_exp(rest)
        extend_chained_cmp_(ts, ((cmpop, l1), e) :: chain)
    | _ =>
        val result = match chain {
        | (_, e1) :: [] => e1
        | ((cmpop, l2), e2) :: (_, e1) :: [] =>
            ExpBinary(OpCmp(cmpop), e1, e2, (TypBool, l2))
        | _ =>
            val chain = chain.rev()
            val nexp = chain.length()
            val fold (result, prev_e, code) = (ExpNop(noloc), chain.hd().1, [])
                for ((cmpop, loc), e)@i <- chain.tl() {
                    val (next_e, code) = if i == nexp-2 {(e, code)} else {
                        match e {
                        | ExpLit(_, _) | ExpIdent(_, _) => (e, code)
                        | _ =>
                            val e_loc = get_exp_loc(e)
                            val tmp_id = gen_temp_id("t")
                            val tmp_decl = DefVal(PatIdent(tmp_id, e_loc), e,
                                                  default_tempval_flags(), e_loc)
                            (make_ident(tmp_id, e_loc), tmp_decl :: code)
                        }
                    }
                    val cmp_exp = ExpBinary(OpCmp(cmpop), prev_e, next_e, (TypBool, loc))
                    val result = if i == 0 {cmp_exp}
                        else { ExpBinary(OpBitwiseAnd, result, cmp_exp, (TypBool, loc)) }
                    (result, next_e, code)
                }
            expseq2exp(code.rev() + (result :: []), get_exp_loc(result))
        }
        (ts, result)
    }}

    fun parse_chained_cmp_(ts: tklist_t): (tklist_t, exp_t)
    {
        val (ts, e) = parse_binary_exp(ts)
        extend_chained_cmp_(ts, ((CmpEQ, noloc), e) :: [])
    }

    fun parse_logic_exp_(ts: tklist_t, result: exp_t, min_prec: int) =
    match ts {
    | (t, l) :: rest =>
        // roughly sort binary ops by how often they are met in the code,
        // so that we have less checks in general
        val (bop, prec, assoc) = match t {
            | LOGICAL_OR => (OpLogicOr, 10, AssocLeft)
            | LOGICAL_AND => (OpLogicAnd, 20, AssocLeft)
            | _ => (OpAdd, -1, AssocLeft)
        }
        if prec < min_prec { (ts, result) }
        else {
            val next_min_prec = match assoc { | AssocLeft => prec+1 | _ => prec }
            val (ts, e) = parse_chained_cmp_(rest)
            // non-tail call, parse rhs
            val (ts, rhs) = parse_logic_exp_(ts, e, next_min_prec)
            val result = ExpBinary(bop, result, rhs, (TypBool, l))
            parse_logic_exp_(ts, result, min_prec)
        }
    }
    val (ts, e) = parse_chained_cmp_(ts)
    parse_logic_exp_(ts, e, 0)
}

fun parse_exp_or_block(ts: tklist_t): (tklist_t, exp_t)
{
    | (LBRACE, _) :: _ => parse_block(ts)
    | _ => parse_exp(ts)
}

fun parse_complex_exp_or_block(ts: tklist_t): (tklist_t, exp_t)
{
    | (LBRACE, _) :: _ => parse_block(ts)
    | _ => parse_complex_exp(ts)
}

fun parse_block(ts: tklist_t): (tklist_t, exp_t)
{
    | (LBRACE, l1) :: rest =>
        val (ts, eseq) = parse_exp_seq(rest, false)
        val e = expseq2exp(eseq, l1)
        match_paren((ts, e), RBRACE, l1)
    | _ =>
        throw parse_err(ts, f"'{' is expected")
}

fun parse_expseq(ts: tklist_t, toplevel: bool): (tklist_t, exp_t list)
{
    fun extend_expseq_(ts: tklist_t, result: exp_t list): (tklist_t, exp_t)
    {
        // if there are more attributes, shared between val, type and fun in the future,
        // then we can have a loop here and extract them all.
        // have_attr will then will be their bitwise union (with '|').
        val (is_private, ts) = match ts {
            | (PRIVATE, _) :: rest => (true, rest)
            | _ => (false, ts)
        }

        fun check_no_attr(ts: tklist_t, f: bool) =
            if f {throw parse_err(ts,
            "unexpected token after declaration-related attribute")}

        val have_attr = is_private

        match ts {
        | (SEMICOLON, _) :: rest =>
            check_no_attr(have_attr); extend_exp_seq_(rest, result)
        | (RBRACE, _) :: _ | (BAR, _) :: _ | (EOF, _) :: _ =>
            check_no_attr(have_attr); (ts, result.rev())
        | (VAL, _) :: _ | (VAR, _) :: _ =>
            val (ts, defvals) = parse_defval(ts, is_private)
            extend_exp_seq_(ts, valdecls + result)
        | (OBJECT, _) :: (TYPE, _) :: _ | (TYPE, _) :: _ =>
            val (ts, deftyp) = parse_deftype(ts)
            extend_exp_seq_(ts, deftyp :: result)
        | (FUN, _) :: (LPAREN(_), _) :: _ =>
            if is_private { throw parse_err("lambda functions cannot be private") }
            val (ts, e) = parse_lambda(ts)
            extend_exp_seq_(ts, e :: result)
        | (PRIVATE, _) :: _ | (PURE, _) :: _ | (NOTHROW, _) :: _
        | (INLINE, _) :: _ | (FUN, _) :: _ =>
            val (ts, defun) = parse_defun(ts, is_private)
            extend_exp_seq_(ts, defun :: result)
        | (EXCEPTION, _) :: _ =>
            if is_private { throw parse_err("exceptions cannot be private") }
            if !toplevel { throw parse_err("exceptions can only be defined at module level") }
            val (ts, defexn) = parse_defexn(ts, is_private)
            extend_exp_seq(ts, defexn :: result)
        | (IMPORT, _) :: _ | (FROM, _) :: _ =>
            if is_private { throw parse_err("import directives cannot be private") }
            if !toplevel {throw parse_err("import directives can only be used at the module level")}
            val (ts, import_dirs) = parse_import_dirs(ts, is)
            extend_exp_seq(ts, import_dirs + result)
        | _ =>
            check_no_attr(have_attr)
            val (ts, exps) = parse_stmt(ts)
            extend_exp_seq(ts, exps + result)
        }
    }
    extend_exp_seq_(ts, [])
}

fun parse_defvals(ts: tklist_t): (tklist_t, exp_t list)
{
    // parse value declaration; [TODO] add support for @private attribute
    val loc = ts.hd().1
    val (is_private, rest) = match ts { | (PRIVATE, _) :: rest => (true, rest) | _ => (false, ts) }
    val is_mutable = match rest { | (VAR, _) :: _ => true | _ => false }

    val (ts, valdecls) = parse_val_decl(rest.tl(), is_private, is_mutable, loc)
}

fun parse_complex_exp(ts: tklist_t): (tklist_t, exp_t)
{
    match ts {
    | (IF, l1) :: rest =>
        /*let rec make_if elif_seq else_exp = match elif_seq with
            | (c, a) :: rest ->
                let if_loc = loclist2loc [get_exp_loc c] (get_exp_loc else_exp) in
                let new_else = ExpIf(c, a, else_exp, (make_new_typ(), if_loc)) in
                make_if rest new_else
            | _ -> else_exp
        in
        let (elif_seq, else_exp) = $2 in make_if elif_seq else_exp }*/
        parse_if_(ts: )
    | (TRY, l1) :: rest =>
        val (ts, e) = parse_exp_or_block(rest)
        val (ts, e) = match ts {
            | (CATCH, _) :: rest =>
                val (ts, clauses) = parse_matching_clauses(rest)
                ExpTryCatch(e, clauses, make_new_ctx(l1))
            | _ => (ts, e)
            }
        match ts {
        | (FINALLY, fe_loc) :: rest =>
            val (ts, final_e) = parse_block(rest)
            val eloc = l1
            val ll = loclist2loc([: eloc, fe_loc :], eloc)
            val tmp = gen_temp_id("v")
            val def_tmp = DefVal(PatIdent(tmp, eloc), e, default_tempval_flags(), l1)
            val try_block = (def_tmp :: exp2expseq(final_e)) + (make_ident(tmp, l1) :: [])
            val try_block = expseq2exp(try_block, l1)
            val some_exn = gen_temp_id("e")
            val some_exn_pat = PatIdent(some_exn, fe_loc)
            val rethrow_exn = ExpThrow(make_typed_ident(some_exn, TypExn, fe_loc), fe_loc)
            val catch_block = exp2expseq(dup_exp(final_e)) + (rethrow_exn :: [])
            val catch_block = expseq2exp(catch_block, fe_loc)
            val try_finally =
                ExpTryCatch(try_block, ((some_exn_pat::[]), catch_block) :: [], make_new_ctx(ll))
            (ts, try_finally)
        | _ => (ts, e)
        }
    | (MATCH, l1) :: rest =>
        val (ts, e) = parse_exp_or_block(rest)
        val (ts, clauses) = parse_matching_clauses(ts)
        (ts, ExpMatch(e, clauses, make_new_ctx(l1)))
    | (FOLD, l1) :: rest =>
        val (ts, (p, e), for_clauses, body) = parse_fold(rest)
        val fold_exp = transform_fold_exp("", p, e, for_clauses, body, l1)
        (ts, fold_exp)
    | (FUN, l1) :: rest =>
        parse_lambda(ts, l1)
    | _ =>
        val (ts, e) = parse_exp(ts)
        match ts {
        | (LBRACE, _) :: rest =>
            val (ts, _, rec_init_elems) = parse_exp_list(ts, parse_exp, RBRACE,
                kw_mode=KwMust, allow_empty=true, stop_at_semicolon=false)
            (ts, ExpMkRecord(e, rec_init_elems, make_new_ctx(get_exp_loc(e))))
        | _ => (ts, e)
        }
    }
}

fun parse_lambda(ts: tklist_t)
{

}

| FUN fun_args block
    {
        let (args, rt) = $2 in
        let body = expseq2exp (exp2expseq $3) 3 in
        let fname = gen_temp_id "lambda" in
        let df = make_deffun fname args rt body (default_fun_flags()) (curr_loc()) in
        ExpSeq(df @ [make_ident fname (curr_loc_n 1)], make_new_ctx())
    }
| FUN fun_args LBRACE BAR pattern_matching_clauses_ RBRACE
    {
        let fname = gen_temp_id "lambda" in
        let df = make_pmatch_deffun (default_fun_flags(), fname) $2 $5 2 5 in
        ExpSeq(df @ [make_ident fname (curr_loc_n 1)], make_new_ctx())
    }

fun parse(ts: tklist_t): (tklist_t, exp_t list) = parse_expseq(ts, true)
