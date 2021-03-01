/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Ficus lexer/tokenizer

import File, Map, Sys
from Ast import *
from Lexer import *

exception ParseError: (loc_t, string)
var last_loc = noloc

fun parse_err(ts: token_t list, msg: string)
{
    val loc = match ts {
    | (_, l) :: _ => l
    | _ => last_loc
    }
    ParseError(loc, msg)
}

fun match_paren((e: exp_t, ts: token_t list), ct: token_t, ol: loc_t)
{
    match ts {
    | (ct_, l) :: rest when ct_ = ct => (e, rest)
    | _ => parse_err(ts, f"'{tok2str(ct)}' is expected; the opening paren is here {ol}")
    }
}

fun is_for_start((f: token_t, l: loc_t), nested: bool) {
    | FOR => nested
    | B_FOR => true
    | PARALLEL => !nested
    | UNZIP => !nested
    | _ => false
}

fun check_ne(ne: bool) = if !ne {
    throw parse_err(ts, "new line or ';' is expected before new expression")
}

type kw_mode_t = KwNone | KwMay | KwMust

fun parse_exp_list(ts: token_t list, parse_exp_f: token_t list->(exp_t, token_t list),
        kw: kw_mode_t, ct: token_t, ~allow_empty: bool, ~stop_at_semicolon: bool=false) =
    fold (el, kw_el, ts) = ([]: exp_t list, ts) for i <- 0: {
        match ts {
        | t :: rest when t == ct =>
            if i == 0 && !allow_empty {throw parse_err(ts, "empty expression list is not allowed here")}
            break with (el.rev(), kw_el.rev(), rest)
        | (COMMA, _) :: _ => if i == 0 {throw parse_err(ts, "unxpected ','")}
        | (SEMICOLON, _) :: rest =>
            if i == 0 || !stop_at_semicolon {throw parse_err(ts, "unxpected ';'")}
            break with (el.rev(), kw_el.rev(), rest)
        | _ => if i > 0 {throw parse_err(ts, f"',' or '{tok2str(ct).1}' is expected")}
        }
        val ts = if i == 0 {ts} else {
            ts.tl() // skip ','
        }
        match ts {
        | (IDENT(_, i), il) :: (EQUAL, _) :: rest =>
            if kw == KwNone { throw parse_err(ts, f"unexpected keyword element '{i}=...'") }
            val (e, ts) = parse_complex_exp(rest)
            (el, (get_id(i), e) :: kw_el, ts)
        | _ =>
            if kw == KwMust {
                if kw == KwNone { throw parse_err(ts, f"expect a keyword element here '<ident> = ...'") }
            }
            val (e, ts) = parse_exp_f(ts)
            (e :: el, kw_el, ts)
        }
    }

fun parse_typed_exp(ts: token_t list) {
    val (e, ts) = parse_complex_exp(ts)
    match ts {
    | (COLON, _) :: rest =>
        val (t, ts) = parse_typespec(rest)
        ExpTyped(e, t, (t, get_exp_loc(e)))
    | (CAST, _) :: rest =>
        val (t, ts) = parse_typespec(rest)
        ExpCast(e, t, (make_new_typ(), get_exp_loc(e)))
    | _ => (e, ts)
    }
}

fun parse_array(ts: token_t list): (exp_t, token_t list) = throw "not implemented"

fun parse_atomic_exp(ts: token_t list)
{
    match ts {
    | (IDENT(ne, i), l) :: rest =>
        check_ne(ne, ts)
        (ExpIdent(get_id(i), make_new_ctx(l)), rest)
    | (LITERAL(lit), l) :: rest => (ExpLit(l, (get_lit_typ(lit), l)), rest)
    | (LPAREN(ne), l1) :: (LBRACE, _) :: _ =>
        check_ne(ne, ts)
        match_paren(parse_block(ts.tl()), RPAREN, l1)
    | (LPAREN(ne), l1) :: (B_FOR, _) :: _ =>
        check_ne(ne, ts)
        match_paren(parse_for(ts.tl(), ForMakeTuple), RPAREN, l1)
    | (LPAREN(ne), l1) :: rest =>
        check_ne(ne, ts)
        val (el, _, ts) = parse_exp_list(rest, parse_typed_exp, KwNone, RPAREN, allow_empty=false)
        (match el {
        | e :: [] => e
        | _ => ExpTuple(el, make_new_ctx(l1))
        }, ts)
    | (LSQUARE(ne), l1) :: f :: rest when is_for_start(f, false) =>
        check_ne(ne, ts)
        match_paren(parse_for(ts.tl(), ForMakeArray), RSQUARE, l1)
    | (LSQUARE(ne), l1) :: rest =>
        check_ne(ne, ts)
        parse_array(rest, l1)
    | (LLIST, l1) :: f :: _ when is_for_start(f, false) =>
        match_paren(parse_for(ts.tl(), ForMakeList), RLIST, l1)
    | (LLIST, l1) :: rest =>
        val (el, _, ts) = parse_exp_list(rest, parse_typed_exp, KwNone, RLIST, false, allow_empty=true,)
        (fold mklist_e = ExpLit(LitNil, make_new_ctx(ol)) for e <- el.rev() {
            ExpBinOp(OpCons, e, mklist_e, make_new_ctx(get_exp_loc(e)))
        }, ts)
    | t :: _ -> throw parse_err(ts, f"unxpected token '{tok2str(t).1}. An identifier, literal or expression enclosed in '( )', '[ ]' or '[: :]' brackets is expected here")
    | _ -> throw parse_err(ts, f"premature end of the stream; check the parens")
    }
}

fun parse_simple_exp(ts: token_t list)
{
    fold (e, ts) = parse_atomic_exp(ts) for ... {
        val eloc = get_exp_loc(e)
        // 1. element access (.|->) (int|ident)
        // 2. array access [...]
        // 3. function call (...)
        val (proceed, e, ts) = match ts {
        | (LPAREN(false), _) :: rest =>
            // function call ([TODO] support keyword args)
            val (args, kw_args, ts) = parse_exp_list(rest, parse_typed_exp, KwMust, RPAREN, allow_empty=true)
            (true, ExpCall(e, args, make_new_ctx(eloc)), ts)
        | (LSQUARE(false), _) :: rest =>
            // array access
            val (idxs, _, ts) = parse_exp_list(rest, parse_array_idx, KwNone, RPAREN, allow_empty=false)
            (true, ExpAt(e, BorderNone, InterpNone, idxs, make_new_ctx(eloc)), ts)
        | (t1, _) :: (t2, l2) :: rest when
            // [TODO] add support for alternating patterns right into the pattern matching syntax
            (match t1 {| DOT | ARROW => true | _ => false}) &&
            (match t2 {| IDENT(false, _) | LITERAL(LitInt _) => true | _ => false}) =>
            val e = match t1 { | ARROW => ExpUnOp(OpDeref, e, make_new_ctx(eloc)) | _ => e }
            val i = match t2 {
                | IDENT(false, i) => ExpIdent(get_id(i), make_new_ctx(l2))
                | LITERAL(LitInt(i)) => ExpLit(LitInt(i), (TypInt, l2))
                | _ => ExpNop(l2)
                }
            (true, ExpMem(e, i, make_new_ctx(eloc)), rest)
        | (t1, _) :: (LBRACE, l2) :: rest when (match t1 {| DOT | ARROW => true | _ => false}) =>
            val e = match t1 { | ARROW => ExpUnOp(OpDeref, e, make_new_ctx(eloc)) | _ => e }
            val (_, rec_init_elems, ts) = parse_exp_list(rest, parse_typed_exp, KwMust, RBRACE, allow_empty=true)
            (true, ExpUpdateRecord(e, rec_init_elems, make_new_ctx(eloc)), rest)
        | _ => (false, e, ts)
        }
        if !proceed { break with (e, ts) }
        (e, ts)
    }
}

fun parse_deref_apos_exp(ts: token_t list)
{
    | (STAR(true), l1) :: rest =>
        val (e, ts) = parse_deref_exp(rest)
        (ExpUnOp(OpDeref, e, make_new_ctx(l1)), ts)
    | (POWER(true), l1) :: rest =>
        val (e, ts) = parse_deref_exp(rest)
        (ExnUnOp(OpDerf, ExpUnOp(OpDeref, e, make_new_ctx(l1)), make_new_ctx(l1)), ts)
    | _ =>
        parse_simple_exp(ts)
        match ts {
        | (APOS, l1) :: rest =>

        }
}

apos_exp:
| apos_exp APOS { make_un_op(OpApos, $1) }
| deref_exp { $1 }

for_flags:
| PARALLEL { {(default_for_flags()) with for_flag_parallel=true} }
| /* empty */ { default_for_flags() }

fun parse_complex_exp(ts: token_t list)
{
    match ts {
    | IF
    }
}
complex_exp:
| IF elif_seq {
    let rec make_if elif_seq else_exp = match elif_seq with
        | (c, a) :: rest ->
            let if_loc = loclist2loc [get_exp_loc c] (get_exp_loc else_exp) in
            let new_else = ExpIf(c, a, else_exp, (make_new_typ(), if_loc)) in
            make_if rest new_else
        | _ -> else_exp
    in
    let (elif_seq, else_exp) = $2 in make_if elif_seq else_exp }
| TRY block CATCH LBRACE pattern_matching_clauses_with_opt_bar RBRACE
{
    ExpTryCatch ($2, $5, make_new_ctx())
}
| TRY block CATCH LBRACE pattern_matching_clauses_with_opt_bar RBRACE FINALLY block
{
    let loc = curr_loc() in
    make_finally (ExpTryCatch ($2, $5, make_new_ctx())) $8 loc
}
| TRY block FINALLY block
{
    make_finally $2 $4 (curr_loc())
}
| MATCH exp_or_block LBRACE pattern_matching_clauses_with_opt_bar RBRACE
    {
        ExpMatch ($2, $4, make_new_ctx())
    }
| FOLD fold_clause block
    {
        let ((fold_pat, fold_init_exp), fold_cl) = $2 in
        transform_fold_exp fold_pat 2 fold_init_exp fold_cl $3
    }
| FUN fun_args block
    {
        let ctx = make_new_ctx() in
        let (args, rt) = $2 in
        let body = expseq2exp (exp2expseq $3) 3 in
        let fname = gen_temp_id "lambda" in
        let df = make_deffun fname args rt body (default_fun_flags()) (curr_loc()) in
        ExpSeq(df @ [ExpIdent (fname, ctx)], ctx)
    }
| FUN fun_args LBRACE BAR pattern_matching_clauses_ RBRACE
    {
        let ctx = make_new_ctx() in
        let fname = gen_temp_id "lambda" in
        let df = make_pmatch_deffun (default_fun_flags(), fname) $2 $5 2 5 in
        ExpSeq(df @ [ExpIdent (fname, ctx)], ctx)
    }
| simple_exp LBRACE id_exp_list_ RBRACE
    %prec fcall_prec
    { ExpMkRecord($1, (List.rev $3), make_new_ctx()) }
| simple_exp LBRACE RBRACE
    %prec fcall_prec
    { ExpMkRecord($1, [], make_new_ctx()) }
| exp { $1 }

typed_exp:
| complex_exp COLON typespec { ExpTyped($1, $3, make_new_ctx()) }
| complex_exp CAST typespec { ExpCast($1, $3, make_new_ctx()) }

unary_exp:
| apos_exp { $1 }
| REF unary_exp { make_un_op(OpMkRef, $2) }
| B_MINUS unary_exp { make_un_op(OpNegate, $2) }
| B_PLUS unary_exp { make_un_op(OpPlus, $2) }
| TILDE unary_exp { make_un_op(OpBitwiseNot, $2) }
| EXPAND unary_exp { make_un_op(OpExpand, $2) }

binary_exp:
| binary_exp PLUS binary_exp { make_bin_op(OpAdd, $1, $3) }
| binary_exp MINUS binary_exp { make_bin_op(OpSub, $1, $3) }
| binary_exp STAR binary_exp { make_bin_op(OpMul, $1, $3) }
| binary_exp SLASH binary_exp { make_bin_op(OpDiv, $1, $3) }
| binary_exp MOD binary_exp { make_bin_op(OpMod, $1, $3) }
| binary_exp POWER binary_exp { make_bin_op(OpPow, $1, $3) }
| binary_exp SHIFT_LEFT binary_exp { make_bin_op(OpShiftLeft, $1, $3) }
| binary_exp SHIFT_RIGHT binary_exp { make_bin_op(OpShiftRight, $1, $3) }
| binary_exp BITWISE_AND binary_exp { make_bin_op(OpBitwiseAnd, $1, $3) }
| binary_exp BITWISE_OR binary_exp { make_bin_op(OpBitwiseOr, $1, $3) }
| binary_exp BITWISE_XOR binary_exp { make_bin_op(OpBitwiseXor, $1, $3) }
| binary_exp CONS binary_exp { make_bin_op(OpCons, $1, $3) }
| binary_exp DOT_STAR binary_exp { make_bin_op(OpDotMul, $1, $3) }
| binary_exp DOT_SLASH binary_exp { make_bin_op(OpDotDiv, $1, $3) }
| binary_exp DOT_MOD binary_exp { make_bin_op(OpDotMod, $1, $3) }
| binary_exp DOT_POWER binary_exp { make_bin_op(OpDotPow, $1, $3) }
| binary_exp SPACESHIP binary_exp { make_bin_op(OpSpaceship, $1, $3) }
| binary_exp DOT_SPACESHIP binary_exp { make_bin_op(OpDotSpaceship, $1, $3) }
| binary_exp DOT_CMP_EQ binary_exp { make_bin_op(OpDotCompareEQ, $1, $3) }
| binary_exp DOT_CMP_NE binary_exp { make_bin_op(OpDotCompareNE, $1, $3) }
| binary_exp DOT_CMP_LE binary_exp { make_bin_op(OpDotCompareLE, $1, $3) }
| binary_exp DOT_CMP_GE binary_exp { make_bin_op(OpDotCompareGE, $1, $3) }
| binary_exp DOT_CMP_LT binary_exp { make_bin_op(OpDotCompareLT, $1, $3) }
| binary_exp DOT_CMP_GT binary_exp { make_bin_op(OpDotCompareGT, $1, $3) }
| unary_exp { $1 }

chained_cmp_exp:
| chained_cmp_exp CMP_EQ binary_exp { (OpCompareEQ, $3) :: $1 }
| chained_cmp_exp CMP_NE binary_exp { (OpCompareNE, $3) :: $1 }
| chained_cmp_exp CMP_LE binary_exp { (OpCompareLE, $3) :: $1 }
| chained_cmp_exp CMP_GE binary_exp { (OpCompareGE, $3) :: $1 }
| chained_cmp_exp CMP_LT binary_exp { (OpCompareLT, $3) :: $1 }
| chained_cmp_exp CMP_GT binary_exp { (OpCompareGT, $3) :: $1 }
| binary_exp
    {
        (* the actual operation is not used here; just put some weird one *)
        (OpCons, $1) :: []
    }

exp:
| LOGICAL_NOT exp { make_un_op(OpLogicNot, $2) }
| exp LOGICAL_OR exp { make_bin_op(OpLogicOr, $1, $3) }
| exp LOGICAL_AND exp { make_bin_op(OpLogicAnd, $1, $3) }
| chained_cmp_exp { make_chained_cmp($1) }



fun parse(t: token_t list)
{


}
