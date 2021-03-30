/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Ficus recursive descent parser

import File, Filename, Sys
from Ast import *
from Lexer import *

exception ParseError: (loc_t, string)

type parser_ctx_t =
{
    module_id: id_t;
    filename: string;
    deps: id_t list;
    inc_dirs: string list;
    default_loc: loc_t;
}

var parser_ctx = parser_ctx_t { module_id=noid, filename="", deps=[], inc_dirs=[], default_loc=noloc }

fun add_to_imported_modules(mname_id: id_t, loc: loc_t): id_t {
    val mname = pp(mname_id)
    val mfname = mname.replace(".", Filename.dir_sep()) + ".fx"
    val mfname =
        try Sys.locate_file(mfname, parser_ctx.inc_dirs)
        catch {
        | NotFoundError => throw ParseError(loc, f"module {mname} is not found")
        }
    val dep_minfo = find_module(mname_id, mfname)
    val mname_unique_id = dep_minfo->dm_name
    if !parser_ctx.deps.mem(mname_unique_id) {
        parser_ctx.deps = mname_unique_id :: parser_ctx.deps
    }
    mname_unique_id
}

type kw_mode_t = KwNone | KwMaybe | KwMust
type tklist_t = (token_t, loc_t) list
type elist_t = exp_t list
type id_exp_t = (id_t, exp_t)
type id_elist_t = id_exp_t list

fun good_variant_name(s: string) {
    val c = s[0]
    ('A' <= c <= 'Z') || s.contains('.')
}

fun tok2str(ts: (token_t, loc_t) list)
{
    var ts_part = []
    for (t, _)@i <- ts {
        ts_part = t :: ts_part
        if i >= 9 {break}
    }
    " ".join([: for t <- ts_part.rev() {tok2str(t).0} :])
}

fun make_unary(uop: unary_t, e: exp_t, loc: loc_t) = ExpUnary(uop, e, make_new_ctx(loc))
fun make_binary(bop: binary_t, e1: exp_t, e2: exp_t, loc: loc_t) = ExpBinary(bop, e1, e2, make_new_ctx(loc))
fun make_literal(lit: lit_t, loc: loc_t) = ExpLit(lit, (get_lit_typ(lit), loc))
fun make_ident(i: id_t, loc: loc_t) = ExpIdent(i, make_new_ctx(loc))
fun make_ident(s: string, loc: loc_t) = ExpIdent(get_id(s), make_new_ctx(loc))
fun make_tuple(el: exp_t list, loc: loc_t) = ExpMkTuple(el, make_new_ctx(loc))

fun get_string(lit: lit_t, loc: loc_t): string =
    match lit {
    | LitString(s) => s
    | _ => throw ParseError(loc, "string literal is expected")
    }

fun plist2exp(pl: pat_t list, prefix: string, loc: loc_t): (pat_t list, exp_t)
{
    fun pat2exp_(p: pat_t): (pat_t, exp_t)
    {
        | PatAny(loc) =>
            val param_id = gen_temp_id(prefix)
            (PatIdent(param_id, loc), make_ident(param_id, loc))
        | PatIdent(i, loc) => (p, make_ident(i, loc))
        | PatAs(p, i, loc) => (p, make_ident(i, loc))
        | PatTyped(p, t, loc) =>
            val (p, e) = pat2exp_(p)
            (PatTyped(p, t, loc), e)
        | _ =>
            val loc = get_pat_loc(p)
            val param_id = gen_temp_id(prefix)
            (PatAs(p, param_id, loc), make_ident(param_id, loc))
    }
    val fold (plist, elist) = ([], []) for p <- pl {
        val (p_, e_) = pat2exp_(p)
        (p_ :: plist, e_ :: elist)
    }
    (plist.rev(), match elist { | e :: [] => e | _ => ExpMkTuple(elist.rev(), make_new_ctx(loc)) })
}

type for_data_t = ((pat_t, exp_t) list, pat_t, for_flags_t, loc_t)

fun transform_fold_exp(special: string, fold_pat: pat_t, fold_init_exp: exp_t,
                       for_exp: exp_t, for_iter_exp: exp_t, fold_loc: loc_t)
{
    fun unpack_for_(e: exp_t, result: for_data_t list): (for_data_t list, exp_t) =
        match e {
        | ExpFor(pe_l, idxp, body, flags, loc)
            when result == [] || flags.for_flag_nested =>
            unpack_for_(body, (pe_l, idxp, flags, loc) :: result)
        | _ => (result, e)
        }

    val acc_loc = get_pat_loc(fold_pat)
    val fr_id = __fold_result_id__
    val fr_exp = make_ident(fr_id, acc_loc)
    val (nested_fors, fold_body) = unpack_for_(for_exp, [])
    val global_flags = nested_fors.last().2
    val body_loc = get_exp_loc(fold_body)
    val body_end_loc = get_end_loc(body_loc)
    val void_ctx = (TypVoid, body_loc)
    val bool_ctx = (TypBool, body_loc)

    val (fr_decl, new_body, fr_exp, global_flags) =
    match special {
    | "" =>
        val fr_decl = DefVal(PatIdent(fr_id, acc_loc), fold_init_exp, default_var_flags(), acc_loc)
        val acc_decl = DefVal(fold_pat, fr_exp, default_tempval_flags(), acc_loc)
        val update_fr = ExpAssign(fr_exp, fold_body, body_loc)
        val new_body = ExpSeq([: acc_decl, update_fr :], void_ctx)
        (fr_decl, new_body, fr_exp, global_flags)
    | "all" | "exists" =>
        val is_all = special == "all"
        val fr_decl = DefVal(PatIdent(fr_id, acc_loc), ExpLit(LitBool(is_all),
                            (TypBool, acc_loc)), default_var_flags(), acc_loc)
        val break_exp = ExpSeq([: ExpAssign(fr_exp, ExpLit(LitBool(!is_all), bool_ctx), body_loc),
                                ExpBreak(true, body_loc) :], void_ctx)
        val predicate_exp = if !is_all { fold_body }
                            else { ExpUnary(OpLogicNot, fold_body, bool_ctx) }
        val new_body = ExpIf(predicate_exp, break_exp, ExpNop(body_loc), void_ctx)
        (fr_decl, new_body, fr_exp, global_flags)
    | "find" | "find_opt" =>
        val none = get_id("None")
        val some = get_id("Some")
        val fr_decl = DefVal(PatIdent(fr_id, acc_loc), make_ident(none, acc_loc),
                            default_var_flags(), acc_loc)
        val mksome_exp = ExpCall(make_ident(some, body_loc), for_iter_exp :: [],
                                 make_new_ctx(body_loc))
        val break_exp = ExpSeq([: ExpAssign(fr_exp, mksome_exp, body_loc),
                                ExpBreak(true, body_loc) :], void_ctx)
        val new_body = ExpIf(fold_body, break_exp, ExpNop(body_loc), void_ctx)
        val new_fr_exp =
        match special {
        | "find" =>
            val x = get_id("x")
            val some_case = (PatVariant(some, [: PatIdent(x, body_end_loc) :],
                             body_end_loc), make_ident(x, body_end_loc))
            val none_case = (PatAny(body_end_loc), ExpThrow(ExpIdent(
                            get_id("NotFoundError"), (TypExn, body_end_loc)), body_end_loc))
            ExpMatch(fr_exp, [: some_case, none_case :], (make_new_typ(), body_end_loc))
        | _ => fr_exp
        }
        (fr_decl, new_body, new_fr_exp, global_flags)
    | "filter" =>
        val check_exp = ExpIf(ExpUnary(OpLogicNot, fold_body, bool_ctx),
                              ExpContinue(body_loc), ExpNop(body_loc), void_ctx)
        val new_body = ExpSeq([: check_exp, for_iter_exp :], make_new_ctx(body_loc))
        (ExpNop(body_loc), new_body, ExpNop(body_loc),
         global_flags.{for_flag_make=ForMakeList})
    | _ => throw ParseError(acc_loc, f"unknown fold variation '{special}'")
    }

    // pack for back
    match global_flags.for_flag_make {
    | ForMakeNone =>
        val fold for_exp = new_body for (pe_l, idxp, flags, loc) <- nested_fors {
                ExpFor(pe_l, idxp, for_exp, flags.{for_flag_fold=true}, loc)
            }
        ExpSeq(fr_decl :: for_exp :: fr_exp :: [], make_new_ctx(fold_loc))
    | _ =>
        val fold nd_map = [] for (pe_l, idxp, flags, loc) <- nested_fors {
                (pe_l, idxp) :: nd_map
            }
        ExpMap(nd_map, new_body, global_flags, make_new_ctx(fold_loc))
    }
}

fun parse_err(ts: tklist_t, msg: string): exn
{
    val loc = match ts {
    | (_, l) :: _ => l
    | _ => parser_ctx.default_loc
    }
    ParseError(loc, msg)
}

fun exp2expseq(e: exp_t): exp_t list
{
    | ExpNop _ => []
    | ExpSeq(eseq, _) => eseq
    | _ => e :: []
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

fun is_for_start(t: token_t)
{
    | FOR(_) | PARALLEL | UNZIP => true
    | _ => false
}

fun check_ne(ne: bool, ts: tklist_t) =
    if !ne {
        throw parse_err(ts, "new line or ';' is expected before new expression")
    }

fun parse_exp_list_f(ts0: tklist_t, parse_exp_f: tklist_t -> (tklist_t, exp_t),
                    ct: token_t, ~kw_mode: kw_mode_t, ~allow_empty: bool,
                  ~stop_at_semicolon: bool=false): (tklist_t, bool, elist_t, id_elist_t)
{
    fun parse_exp_list_(ts: tklist_t, expect_comma: bool, el: elist_t,
                        kw_el: id_elist_t): (tklist_t, bool, elist_t, id_elist_t) =
    match ts {
    | (COMMA, _) :: rest =>
        if expect_comma { parse_exp_list_(rest, false, el, kw_el) }
        else { throw parse_err(ts, "extra ','?") }
    | (t, _) :: rest when t == ct =>
        (rest, true, el.rev(), kw_el.rev())
    | (SEMICOLON, _) :: rest =>
        if !stop_at_semicolon {throw parse_err(ts, "unxpected ';'")}
        (rest, false, el.rev(), kw_el.rev())
    | (t, _) :: rest =>
        if expect_comma {
            val semi_msg = if stop_at_semicolon {"';' or "} else {""}
            throw parse_err(ts, f"',' or {semi_msg}'{tok2str(ct).1}' is expected")
        }
        match ts {
        | (IDENT(_, i), _) :: (EQUAL, _) :: rest =>
            if kw_mode == KwNone { throw parse_err(ts, f"unexpected keyword element '{i}=...'") }
            val (ts, e) = parse_complex_exp(rest)
            parse_exp_list_(ts, true, el, (get_id(i), e) :: kw_el)
        | _ =>
            if kw_mode == KwMust { throw parse_err(ts, f"expect a keyword element here '<ident> = ...'") }
            val (ts, e) = parse_exp_f(ts)
            parse_exp_list_(ts, true, e :: el, kw_el)
        }
    | _ => throw parse_err(ts0, "the expression list is not complete by the end of file, check parentheses")
    }
    val (ts, f, el, kw_el) = parse_exp_list_(ts0, false, [], [])
    if !allow_empty && el == [] && kw_el == [] {
        throw parse_err(ts, "empty expression list is not allowed here")
    }
    (ts, f, el, kw_el)
}

fun parse_exp_list(ts0: tklist_t, ct: token_t, ~kw_mode: kw_mode_t, ~allow_empty: bool,
                  ~stop_at_semicolon: bool=false): (tklist_t, bool, elist_t, id_elist_t)
{
    parse_exp_list_f(ts0, parse_typed_exp, ct, kw_mode=kw_mode, allow_empty=allow_empty,
                     stop_at_semicolon=stop_at_semicolon)
}

fun parse_dot_ident(ts: tklist_t, expect_dot: bool, result: string): (tklist_t, string)
{
    match (ts, expect_dot) {
    | ((DOT, _) :: rest, _) =>
        if expect_dot { parse_dot_ident(rest, false, result) }
        else { throw parse_err(ts, "extra '.'?") }
    | (_, true) => (ts, result)
    | ((IDENT(_, i), _) :: rest, _) =>
        parse_dot_ident(rest, true, if result == "" {i} else {result + "." + i})
    | _ =>
        throw parse_err(ts, "identifier is expected")
    }
}

fun parse_ident_list(ts: tklist_t, expect_comma: bool, result: id_t list): (tklist_t, id_t list) =
    match ts {
    | (COMMA, _) :: rest =>
        if expect_comma {parse_ident_list(rest, false, result)}
        else {throw parse_err(ts, "extra ','?")}
    | (IDENT(_, i), _) :: rest =>
        if expect_comma { (ts, result.rev()) }
        else { parse_ident_list(rest, true, get_id(i) :: result) }
    | _ =>
        if result == [] { throw parse_err(ts, "empty id list") }
        (ts, result.rev())
    }

fun parse_atomic_exp(ts: tklist_t): (tklist_t, exp_t)
{
    //println(f"parse_atomic_exp @ {ts.hd().1}\n")
    match ts {
    | (IDENT(ne, i), l1) :: rest =>
        check_ne(ne, ts)
        (rest, make_ident(i, l1))
    | (LITERAL(lit), l1) :: rest => (rest, make_literal(lit, l1))
    | (LPAREN(true), l1) :: (t, l2) :: (RPAREN, _) :: rest
        when get_opname(t) != noid =>
        val opname = get_opname(t)
        (rest, ExpIdent(opname, make_new_ctx(l2)))
    | (LPAREN(ne), l1) :: (LBRACE, _) :: _ =>
        check_ne(ne, ts)
        match_paren(parse_block(ts.tl()), RPAREN, l1)
    | (LPAREN(ne), l1) :: (FOR _, _) :: _ =>
        check_ne(ne, ts)
        val (ts, for_exp, _) = parse_for(ts.tl(), ForMakeTuple)
        match_paren((ts, for_exp), RPAREN, l1)
    | (LPAREN(ne), l1) :: rest =>
        check_ne(ne, ts)
        val (ts, _, el, _) = parse_exp_list(rest, RPAREN, kw_mode=KwNone, allow_empty=false)
        (ts, match el {
        | e :: [] => e
        | _ => make_tuple(el, l1)
        })
    | (LARRAY, l1) :: (f, _) :: rest when is_for_start(f) =>
        val (ts, for_exp, _) = parse_for(ts.tl(), ForMakeArray)
        match_paren((ts, for_exp), RARRAY, l1)
    | (LARRAY, l1) :: rest =>
        var vts = rest, result = []
        while true {
            val (ts, done, el, _) = parse_exp_list(vts, RARRAY, kw_mode=KwNone,
                                        allow_empty=false, stop_at_semicolon=true)
            vts = ts
            result = el :: result
            if done { break }
        }
        (vts, ExpMkArray(result.rev(), make_new_ctx(l1)))
    | (LLIST, l1) :: (f, _) :: _ when is_for_start(f) =>
        val (ts, for_exp, _) = parse_for(ts.tl(), ForMakeList)
        match_paren((ts, for_exp), RLIST, l1)
    | (LLIST, l1) :: rest =>
        val (ts, _, el, _) = parse_exp_list(rest, RLIST, kw_mode=KwNone, allow_empty=true)
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
    //println(f"simple_exp({tok2str(ts)})\n")
    fun extend_simple_exp_(ts: tklist_t, e: exp_t) {
        //println(f"extend_simple_exp({tok2str(ts)})\n")
        val eloc = get_exp_loc(e)
        // 1. element access (.|->) (int|ident)
        // 2. array access [...]
        // 3. function call (...)
        match ts {
        | (LPAREN(false), l1) :: (FOR _, l2) :: rest =>
            val eloc = get_exp_loc(e)
            val istr = match e {
                | ExpIdent(i, _) => pp(i)
                | _ => throw ParseError(l2, "incorrect use of for (';' or newline or maybe 'fold' is missing?)")
            }
            val (ts, for_exp, for_iter_exp) = parse_for(ts.tl(), ForMakeNone)
            val fold_exp = transform_fold_exp(istr, PatAny(eloc), ExpNop(eloc), for_exp, for_iter_exp, eloc)
            val (ts, fold_exp) = match_paren((ts, fold_exp), RPAREN, l1)
            extend_simple_exp_(ts, fold_exp)
        | (LPAREN(false), l1) :: rest =>
            // function call ([TODO] support keyword args)
            val (ts_, _, args, kw_args) = parse_exp_list(rest, RPAREN,
                                            kw_mode=KwMaybe, allow_empty=true)
            val args =
                if kw_args == [] {args}
                else {
                    args + (ExpMkRecord(ExpNop(l1), kw_args, make_new_ctx(l1)) :: [])
                }
            val call_exp = match e {
                | ExpIdent(i, _) when pp(i).startswith("__intrin_") =>
                    val istr = pp(i)
                    val iop = match istr {
                              | "__intrin_sqrt__" => IntrinMath(get_id("sqrt"))
                              | "__intrin_pow__" => IntrinMath(get_id("pow"))
                              | "__intrin_sin__" => IntrinMath(get_id("sin"))
                              | "__intrin_cos__" => IntrinMath(get_id("cos"))
                              | "__intrin_tan__" => IntrinMath(get_id("tan"))
                              | "__intrin_atan__" => IntrinMath(get_id("atan"))
                              | "__intrin_atan2__" => IntrinMath(get_id("atan2"))
                              | "__intrin_log__" => IntrinMath(get_id("log"))
                              | "__intrin_exp__" => IntrinMath(get_id("exp"))
                              | "__intrin_atanh__" => IntrinMath(get_id("atanh"))
                              | "__intrin_sinh__" => IntrinMath(get_id("sinh"))
                              | "__intrin_cosh__" => IntrinMath(get_id("cosh"))
                              | "__intrin_tanh__" => IntrinMath(get_id("tanh"))
                              | _ => throw compile_err(l1, f"unknown/unsupported intrinsic {istr}")
                              }
                    ExpIntrin(iop, args, make_new_ctx(eloc))
                | _ =>
                    ExpCall(e, args, make_new_ctx(eloc))
                }
            extend_simple_exp_(ts_, call_exp)
        | (LSQUARE(false), _) :: rest =>
            val (ts, _, idxs, _) = parse_exp_list_f(rest, parse_range_exp,
                            RSQUARE, kw_mode=KwNone, allow_empty=false)
            extend_simple_exp_(ts, ExpAt(e, BorderNone, InterpNone, idxs, make_new_ctx(eloc)))
        | (t1, _) :: (t2, l2) :: rest when
            // [TODO] add support for alternating patterns right into the pattern matching syntax
            (match t1 {| DOT | ARROW => true | _ => false}) &&
            (match t2 {| IDENT(true, _) | LITERAL(LitInt _) => true | _ => false}) =>
            val e = match t1 { | ARROW => make_unary(OpDeref, e, eloc) | _ => e }
            val i = match t2 {
                | IDENT(true, i) => make_ident(i, l2)
                | LITERAL(LitInt(i)) => make_literal(LitInt(i), l2)
                | _ => ExpNop(l2)
                }
            extend_simple_exp_(rest, ExpMem(e, i, make_new_ctx(eloc)))
        | (t1, _) :: (LBRACE, l2) :: rest when (match t1 {| DOT | ARROW => true | _ => false}) =>
            val e = match t1 { | ARROW => make_unary(OpDeref, e, eloc) | _ => e }
            val (ts, _, _, rec_init_elems) = parse_exp_list(rest, RBRACE, kw_mode=KwMust, allow_empty=true)
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
    //println(f"unary_exp({tok2str(ts)})\n")
    match ts {
    | (REF(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpMkRef, e, l1))
    | (MINUS(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpNegate, e, l1))
    | (DOT_MINUS(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest)
        (ts, make_unary(OpDotMinus, e, l1))
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
    //println(f"binary_exp({tok2str(ts)})\n")
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
            | POWER => (OpPow, 230, AssocRight)
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
            | DOT_POWER => (OpDotPow, 230, AssocRight)
            | DOT_CMP(cmpop) => (OpDotCmp(cmpop), 180, AssocLeft)
            | DOT_SPACESHIP => (OpDotSpaceship, 190, AssocLeft)
            //| SAME => (OpSame, 190, AssocLeft)
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
    //println(f"binary_exp({tok2str(ts)})\n")
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
            val (result, _, code) =
                fold (result, prev_e, code) = (ExpNop(noloc), chain.hd().1, [])
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
    | _ => (ts, result)
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
        val (ts, eseq) = parse_expseq(rest, false)
        val e = expseq2exp(eseq, l1)
        match_paren((ts, e), RBRACE, l1)
    | _ =>
        throw parse_err(ts, "'{' is expected")
}

fun parse_ccode_exp(ts: tklist_t, t: typ_t): (tklist_t, exp_t) =
    match ts {
    | (CCODE, l1) :: (LITERAL(lit), l2) :: rest =>
        val s = get_string(lit, l2)
        (rest, ExpCCode(s, (t, l1)))
    | _ => throw parse_err(ts, "'ccode {...}' is expected")
    }

fun parse_range_exp(ts0: tklist_t): (tklist_t, exp_t)
{
    val loc = match ts0 { | _ :: _ => ts0.hd().1 | _ => noloc }
    fun parse_range_(ts: tklist_t, expect_sep: bool, result: exp_t? list): (tklist_t, exp_t? list) =
        match ts {
        | (CONS, _) :: rest =>
            val result = if expect_sep {None :: result} else {None :: None :: result}
            parse_range_(rest, false, result)
        | (COLON, _) :: rest =>
            val result = if expect_sep {result} else {None :: result}
            parse_range_(rest, false, result)
        | (LBRACE, _) :: _ | (FOR _, _) :: _ | (RSQUARE, _) :: _ | (COMMA, _) :: _ =>
            if !expect_sep && result.length() != 1 {
                throw parse_err(ts, "range expression may not end with ':', unless it's ':' or '<start>:' ")
            }
            val result = if expect_sep {result} else {None :: result}
            (ts, result.rev())
        | _ =>
            if expect_sep { throw parse_err(ts, "':' is expected") }
            val (ts, e) = parse_exp(ts)
            parse_range_(ts, true, Some(e) :: result)
        }
    val (ts, elist) = parse_range_(ts0, false, [])
    val ctx = make_new_ctx(loc)

    val e = match elist {
    | Some(e) :: [] => e // scalar index
    | None :: None :: [] => ExpRange(None, None, None, ctx) // :
    | [] => throw parse_err(ts0, "empty range expression")
    | None :: [] | None :: None :: None :: [] => throw parse_err(ts0, "invalid range expression")
    | e1 :: [] => ExpRange(e1, None, None, ctx) // a:
    | e1 :: e2 :: [] => ExpRange(e1, e2, None, ctx) // a:b or :b
    | e1 :: e2 :: e3 :: [] => ExpRange(e1, e2, e3, ctx) // a:b:c or a::c or :b:c or ::c
    | _ => throw parse_err(ts0, f"invalid range expression of {elist.length()} components")
    }

    (ts, e)
}

fun parse_expseq(ts: tklist_t, toplevel: bool): (tklist_t, exp_t list)
{
    fun extend_expseq_(ts: tklist_t, result: exp_t list): (tklist_t, exp_t list)
    {
        val ts = match ts {
        | (SEMICOLON, _) :: rest =>
            if result == [] {
                throw parse_err(ts, "unexpected ';' in the beginning of expression sequence")
            }
            rest
        | _ => ts
        }

        match ts {
        | (RBRACE, _) :: _ | (BAR, _) :: _ | (EOF, _) :: _ =>
            if toplevel && (match ts {| (EOF, _) :: _ => false | _ => true}) {
                throw parse_err(ts, "unexpected '}'")
            }
            (ts, result.rev())
        | (VAL, _) :: _ | (PRIVATE, _) :: (VAL, _) :: _
        | (VAR, _) :: _ | (PRIVATE, _) :: (VAR, _) :: _ =>
            val (ts, defvals) = parse_defvals(ts)
            extend_expseq_(ts, defvals + result)
        | (OBJECT, _) :: (TYPE, _) :: _ | (TYPE, _) :: _ =>
            val (ts, deftyp) = parse_deftype(ts)
            extend_expseq_(ts, deftyp :: result)
        | (FUN, _) :: (LPAREN _, _) :: _ =>
            val (ts, e) = parse_lambda(ts)
            extend_expseq_(ts, e :: result)
        | (PRIVATE, _) :: _ | (PURE, _) :: _ | (NOTHROW, _) :: _
        | (INLINE, _) :: _ | (FUN, _) :: _ | (OPERATOR, _) :: _ =>
            val (ts, defun) = parse_defun(ts)
            extend_expseq_(ts, defun :: result)
        | (EXCEPTION, l1) :: rest =>
            if !toplevel { throw parse_err(ts, "exceptions can only be defined at module level") }
            val (ts, i) = match rest {
                | (IDENT(_, i), _) :: rest =>
                    if !good_variant_name(i) {
                        throw parse_err(ts, "exception name should start with a capital letter (A..Z)")
                    }
                    (rest, get_id(i))
                | _ =>
                    throw parse_err(rest, "identifier is expected")
                }
            val (ts, t) = match ts {
                | (COLON, _) :: rest => parse_typespec(rest)
                | _ => (ts, TypVoid)
                }
            val de = ref (defexn_t { dexn_name=i, dexn_typ=t, dexn_scope=[], dexn_loc=l1 })
            extend_expseq_(ts, DefExn(de) :: result)
        | (IMPORT(f), l1) :: rest =>
            if !toplevel {throw parse_err(ts, "import directives can only be used at the module level")}
            if !f { throw parse_err(ts, "';' or newline is expected before the import directive") }
            fun parse_imported_(ts: tklist_t, expect_comma: bool, result: (id_t, id_t) list):
                (tklist_t, (id_t, id_t) list) =
                match ts {
                | (COMMA, _) :: rest =>
                    if expect_comma {parse_imported_(rest, false, result)}
                    else {throw parse_err(ts, "extra ','?")}
                | (IDENT(_, _), loc_i) :: _ =>
                    if expect_comma { (ts, result.rev()) }
                    else {
                        val (ts, i) = parse_dot_ident(ts, false, "")
                        val i = get_id(i)
                        val (ts, j) = match ts {
                            | (AS, _) :: (IDENT(_, j), _) :: rest => (rest, get_id(j))
                            | _ => (ts, i)
                            }
                        val i_ = add_to_imported_modules(i, loc_i)
                        parse_imported_(ts, true, (i_, j) :: result)
                    }
                | _ =>
                    if result == [] { throw parse_err(ts, "empty module list") }
                    (ts, result.rev())
                }
            val (ts, imported) = parse_imported_(rest, false, [])
            extend_expseq_(ts, DirImport(imported, l1) :: result)
        | (FROM, l1) :: rest =>
            if !toplevel {throw parse_err(ts, "import directives can only be used at the module level")}
            val (ts, m_) = parse_dot_ident(rest, false, "")
            val m = get_id(m_)
            val m_ = add_to_imported_modules(m, l1)
            match ts {
            | (IMPORT _, _) :: (STAR _, _) :: rest =>
                extend_expseq_(rest, DirImportFrom(m_, [], l1) :: result)
            | (IMPORT _, _) :: rest =>
                val (ts, il) = parse_ident_list(rest, false, [])
                extend_expseq_(ts, DirImportFrom(m_, il, l1) :: result)
            | _ =>
                throw parse_err(ts, "'import' is expected")
            }
        | (PRAGMA, l1) :: (LITERAL(lit), l2) :: rest =>
            if !toplevel {throw parse_err(ts, "pragma directives can only be used at the module level")}
            val pr = get_string(lit, l2)
            fun more_pragmas_(ts: tklist_t, prl: string list) =
                match ts {
                | (COMMA, _) :: (LITERAL(lit), l2) :: rest =>
                    val pr = get_string(lit, l2)
                    more_pragmas_(rest, pr :: prl)
                | _ => (ts, prl.rev())
                }
            val (ts, prl) = more_pragmas_(rest, pr :: [])
            extend_expseq_(ts, DirPragma(prl, l2) :: result)
        | (CCODE, _) :: _ =>
            if !toplevel {throw parse_err(ts, "C code is only allowed at the top level or as rhs of function or value definition")}
            val (ts, e) = parse_ccode_exp(ts, TypVoid)
            extend_expseq_(ts, e :: result)
        | _ =>
            val (ts, e) = parse_stmt(ts)
            extend_expseq_(ts, e :: result)
        }
    }
    extend_expseq_(ts, [])
}

fun parse_for(ts: tklist_t, for_make: for_make_t): (tklist_t, exp_t, exp_t)
{
    var is_parallel = false, need_unzip = false
    var vts = ts, nested_fors = ([] : ((pat_t, pat_t, exp_t) list, loc_t) list)

    while true {
        match vts {
        | (PARALLEL, _) :: rest =>
            if is_parallel {throw parse_err(ts, "duplicate @parallel attribute")}
            is_parallel = true; vts = rest
        | (UNZIP, _) :: rest =>
            if need_unzip {throw parse_err(ts, "duplicate @unzip attribute")}
            match for_make {
            | ForMakeNone | ForMakeTuple =>
                throw parse_err(ts, "@unzip can only be used with array and list comprehensions")
            | _ => {}}
            need_unzip = true; vts = rest
        | (FOR _, _) :: rest =>
            break
        | _ =>
            throw parse_err(vts, "'for' is expected (after optional attributes)")
        }
    }

    fun parse_for_clause_(ts: tklist_t, expect_comma: bool,
        result: (pat_t, pat_t, exp_t) list, loc: loc_t): (tklist_t, (pat_t, pat_t, exp_t) list) =
        match ts {
        | (COMMA, _) :: rest =>
            if expect_comma { parse_for_clause_(rest, false, result, loc) }
            else { throw parse_err(ts, "extra ','?") }
        | (FOR _, _) :: _ | (LBRACE, _) :: _ when expect_comma =>
            if result == [] {
                throw parse_err(ts, "empty for? (need at least one <iter_pat> <- <iter_range or collection>)")
            }
            (ts, result.rev())
        | _ =>
            if expect_comma { throw parse_err(ts, "',' is expected") }
            val (ts, p) = parse_pat(ts, true)
            val (ts, idx_pat) = match ts {
                | (AT, _) :: rest =>
                    parse_pat(rest, true)
                | _ => (ts, PatAny(loc))
                }
            val ts = match ts {
                | (BACK_ARROW, _) :: rest => rest
                | _ => throw parse_err(ts, "'<-' is expected")
                }
            val (ts, e) = parse_range_exp(ts)
            parse_for_clause_(ts, true, (p, idx_pat, e) :: result, loc)
        }

    while true {
        var loc_i = noloc
        match vts {
        | (FOR _, l1) :: rest =>
            vts = rest
            loc_i = l1
        | _ =>
            nested_fors = nested_fors.rev()
            break
        }
        val (ts, for_cl)  = parse_for_clause_(vts, false, [], loc_i)
        nested_fors = (for_cl, loc_i) :: nested_fors
        vts = ts
    }

    val glob_loc = match nested_fors {| (_, loc) :: _ => loc | _ => noloc}

    // process the nested for.
    val fold (glob_el, nested_fors) = ([], []) for (ppe_list, loc) <- nested_fors {
        val fold (glob_el, for_cl_, idx_pat) = (glob_el, [], PatAny(loc)) for (p, idxp, e) <- ppe_list {
            val (p_, p_e) = plist2exp(p :: [], "x", get_pat_loc(p))
            val p = p_.hd()
            match (idxp, idx_pat) {
            | (PatAny _, idx_pat) => (p_e :: glob_el, (p, e) :: for_cl_, idx_pat)
            | (_, PatAny _) =>
                val (idxp, idxp_e) = plist2exp(idxp :: [], "i", get_pat_loc(idxp))
                (idxp_e :: p_e :: glob_el, (p, e) :: for_cl_, idxp.hd())
            | _ => throw ParseError(get_pat_loc(idxp), "@ is used more than once, which does not make sence and is not supported")
            }
        }
        (glob_el, (for_cl_.rev(), idx_pat, loc) :: nested_fors)
    }

    val for_iter_exp = match glob_el.rev() { | e :: [] => e | el => make_tuple(el, glob_loc) }
    val (ts, body) = match vts {
        | (LBRACE, l1) :: (BAR, _) :: _ =>
            val (ts, cases) = parse_match_cases(vts)
            val match_e = ExpMatch(for_iter_exp, cases, make_new_ctx(l1))
            (ts, match_e)
        | (LBRACE, l1) :: _ =>
            parse_block(vts)
        | _ => throw parse_err(ts, "'{' is expected (beginning of for-loop body)")
    }

    val for_exp = match for_make
    {
    | ForMakeArray | ForMakeList | ForMakeTuple =>
        val fold (pel_i_l, loc) = ([], noloc) for (pe_l, idxp, loc) <- nested_fors {
            ((pe_l, idxp) :: pel_i_l, loc)
            }
        ExpMap(pel_i_l, body, default_for_flags().{
            for_flag_make=for_make,
            for_flag_parallel=is_parallel,
            for_flag_unzip=need_unzip},
            make_new_ctx(loc))
    | _ =>
        val nfors = nested_fors.length()
        fold e = body for (pe_l, idxp, loc)@i <- nested_fors {
            val nested = i < nfors-1
            val flags = default_for_flags()
            val flags = if nested {flags.{for_flag_nested=true}}
                else {flags.{for_flag_parallel=is_parallel}}
            ExpFor(pe_l, idxp, e, flags, loc)
            }
    }
    (ts, for_exp, for_iter_exp)
}

fun parse_fold_init_(ts: tklist_t, expect_comma: bool, pl: pat_t list,
                    el: exp_t list): (tklist_t, pat_t, exp_t) =
    match ts {
    | (COMMA, _) :: rest =>
        if expect_comma { parse_fold_init_(rest, false, pl, el) }
        else { throw parse_err(ts, "extra ','?") }
    | (FOR _, _) :: _ =>
        val pl = pl.rev(), el = el.rev()
        match (pl, el) {
        | (p :: [], e :: []) => (ts, p, e)
        | _ => (ts, PatTuple(pl, get_pat_loc(pl.hd())),
            ExpMkTuple(el, make_new_ctx(get_exp_loc(el.hd()))))
        }
    | _ =>
        if expect_comma { throw parse_err(ts, "missing ','?") }
        val (ts, p) = parse_pat(ts, true)
        match ts {
        | (EQUAL, l1) :: rest =>
            val (ts, e) = parse_complex_exp(rest)
            parse_fold_init_(ts, true, p :: pl, e :: el)
        | _ => throw parse_err(ts, "'=' is expected")
        }
    }

fun parse_defvals(ts: tklist_t): (tklist_t, exp_t list)
{
    val (ts, is_private) = match ts {
        | (PRIVATE, _) :: rest => (rest, true)
        | _ => (ts, false)
    }
    val (ts, is_mutable) = match ts {
        | (VAL, _) :: rest => (rest, false)
        | (VAR, _) :: rest => (rest, true)
        | _ => throw parse_err(ts, "'val' or 'var' is expected")
    }
    val flags = default_val_flags().{
        val_flag_private=is_private,
        val_flag_mutable=is_mutable
        }

    fun extend_defvals_(ts: tklist_t, expect_comma: bool, result: exp_t list): (tklist_t, exp_t list) =
        match ts {
        | (COMMA, _) :: rest =>
            if expect_comma { extend_defvals_(rest, false, result) }
            else { throw parse_err(ts, "extra ','?") }
        | (IDENT(true, _), _) :: _ | (LPAREN(true), _) :: _
        | (LBRACE, _) :: _ | (REF(true), _) :: _ =>
            // do not reverse the result
            if expect_comma { (ts, result) }
            else {
                val (ts, p) = parse_pat(ts, true)
                match ts {
                | (EQUAL, l1) :: (CCODE, l2) :: _ =>
                    val (ts, e) = parse_ccode_exp(ts.tl(), make_new_typ())
                    val dv = DefVal(p, e, flags, l1)
                    extend_defvals_(ts, true, dv :: result)
                | (EQUAL, l1) :: rest =>
                    val (ts, e) = parse_complex_exp_or_block(rest)
                    val dv = DefVal(p, e, flags, l1)
                    extend_defvals_(ts, true, dv :: result)
                | _ => throw parse_err(ts, "'=' is expected")
                }
            }
        | _ =>
            // do not reverse the result
            (ts, result)
        }

    match ts {
    | (FOLD, l1) :: rest =>
        val (ts, p, e) = parse_fold_init_(rest, false, [], [])
        val (ts, for_exp, for_iter_exp) = parse_for(ts, ForMakeNone)
        val fold_exp = transform_fold_exp("", p, e, for_exp, for_iter_exp, l1)
        (ts, DefVal(p, fold_exp, flags, get_pat_loc(p)) :: [])
    | _ => extend_defvals_(ts, false, [])
    }
}

fun get_opname(t: token_t): id_t =
    match t {
    | APOS => fname_op_apos()
    | PLUS _ => fname_op_add()
    | MINUS _  => fname_op_sub()
    | STAR _  => fname_op_mul()
    | SLASH  => fname_op_div()
    | PERCENT  => fname_op_mod()
    | POWER  => fname_op_pow()
    | DOT_STAR  => fname_op_dot_mul()
    | DOT_SLASH  => fname_op_dot_div()
    | DOT_PERCENT  => fname_op_dot_mod()
    | DOT_POWER  => fname_op_dot_pow()
    | SHIFT_LEFT  => fname_op_shl()
    | SHIFT_RIGHT  => fname_op_shr()
    | BITWISE_AND  => fname_op_bit_and()
    | BITWISE_OR   => fname_op_bit_or()
    | BITWISE_XOR  => fname_op_bit_xor()
    | TILDE  => fname_op_bit_not()
    | SPACESHIP  => fname_op_cmp()
    //| SAME    => fname_op_same()
    | CMP(CmpEQ)  => fname_op_eq()
    | CMP(CmpNE)  => fname_op_ne()
    | CMP(CmpLE)  => fname_op_le()
    | CMP(CmpGE)  => fname_op_ge()
    | CMP(CmpLT)  => fname_op_lt()
    | CMP(CmpGT)  => fname_op_gt()
    | DOT_SPACESHIP  => fname_op_dot_cmp()
    | DOT_CMP(CmpEQ)  => fname_op_dot_eq()
    | DOT_CMP(CmpNE)  => fname_op_dot_ne()
    | DOT_CMP(CmpLE)  => fname_op_dot_le()
    | DOT_CMP(CmpGE)  => fname_op_dot_ge()
    | DOT_CMP(CmpLT)  => fname_op_dot_lt()
    | DOT_CMP(CmpGT)  => fname_op_dot_gt()
    | _ => noid
    }

fun parse_defun(ts: tklist_t): (tklist_t, exp_t)
{
    var is_private = false, is_pure = false, is_nothrow = false, is_inline = false
    var vts = ts, fname = noid, loc = noloc

    while true {
        match vts {
        | (PRIVATE, _) :: rest =>
            if is_private {throw parse_err(ts, "duplicate @private attribute")}
            is_private = true; vts = rest
        | (PURE, _) :: rest =>
            if is_pure {throw parse_err(ts, "duplicate @pure attribute")}
            is_pure = true; vts = rest
        | (NOTHROW, _) :: rest =>
            if is_nothrow {throw parse_err(ts, "duplicate @nothrow attribute")}
            is_nothrow = true; vts = rest
        | (INLINE, _) :: rest =>
            if is_inline {throw parse_err(ts, "duplicate @inline attribute")}
            is_inline = true; vts = rest
        | (FUN, l1) :: (IDENT(_, i), _) :: (LPAREN _, _) :: rest =>
            vts = rest; fname = get_id(i); loc = l1; break
        | (OPERATOR, l1) :: (t, l2) :: (LPAREN _, _) :: rest =>
            vts = rest; fname = get_opname(t); loc = l1
            if fname == noid { throw ParseError(l2, "invalid operator name") }
            break
        | _ =>
            throw parse_err(vts, "'fun <funcname> (' is expected (after optional attributes)")
        }
    }

    val (ts, params, rt, prologue, have_keywords) = parse_fun_params(vts)
    parse_body_and_make_fun(ts, fname, params, rt, prologue,
        default_fun_flags().{
            fun_flag_private=is_private,
            fun_flag_nothrow=is_nothrow,
            fun_flag_pure=if is_pure {1} else {-1},
            fun_flag_inline=is_inline,
            fun_flag_has_keywords=have_keywords}, loc)
}

fun parse_complex_exp(ts: tklist_t): (tklist_t, exp_t)
{
    match ts {
    | (IF, l1) :: rest =>
        fun parse_if_(ts: tklist_t, loc: loc_t): (tklist_t, exp_t)
        {
            val (ts, c) = parse_exp_or_block(ts)
            val (ts, then_e) = parse_block(ts)
            val (ts, else_e) = match ts {
                | (ELSE, _) :: (IF, l2) :: rest =>
                    // non-tail recursive call: continue else-if chain
                    parse_if_(rest, l2)
                | (ELSE, _) :: rest =>
                    parse_block(rest)
                | _ => (ts, ExpNop(l1))
            }
            (ts, ExpIf(c, then_e, else_e, make_new_ctx(l1)))
        }
        parse_if_(rest, l1)
    | (TRY, l1) :: rest =>
        val (ts, e) = parse_exp_or_block(rest)
        val (ts, e) = match ts {
            | (CATCH, _) :: rest =>
                val (ts, cases) = parse_match_cases(rest)
                (ts, ExpTryCatch(e, cases, make_new_ctx(l1)))
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
            val rethrow_exn = ExpThrow(ExpIdent(some_exn, (TypExn, fe_loc)), fe_loc)
            val catch_block = exp2expseq(dup_exp(final_e)) + (rethrow_exn :: [])
            val catch_block = expseq2exp(catch_block, fe_loc)
            val try_finally =
                ExpTryCatch(try_block, (some_exn_pat, catch_block) :: [], make_new_ctx(ll))
            (ts, try_finally)
        | _ => (ts, e)
        }
    | (MATCH, l1) :: rest =>
        val (ts, e) = parse_exp_or_block(rest)
        val (ts, cases) = parse_match_cases(ts)
        (ts, ExpMatch(e, cases, make_new_ctx(l1)))
    | (FOLD, l1) :: rest =>
        val (ts, p, e) = parse_fold_init_(rest, false, [], [])
        val (ts, for_exp, for_iter_exp) = parse_for(ts, ForMakeNone)
        val fold_exp = transform_fold_exp("", p, e, for_exp, for_iter_exp, l1)
        (ts, fold_exp)
    | (FUN, _) :: _ => parse_lambda(ts)
    | _ =>
        val (ts, e) = parse_exp(ts)
        match ts {
        | (LBRACE, _) :: rest =>
            val (ts, _, _, rec_init_elems) = parse_exp_list(rest, RBRACE,
                kw_mode=KwMust, allow_empty=true, stop_at_semicolon=false)
            (ts, ExpMkRecord(e, rec_init_elems, make_new_ctx(get_exp_loc(e))))
        | _ => (ts, e)
        }
    }
}

fun parse_lambda(ts: tklist_t): (tklist_t, exp_t)
{
    val (ts, loc) = match ts {
        | (FUN, l1) :: (LPAREN _, _) :: rest => (rest, l1)
        | _ => throw parse_err(ts, "lambda function (starting with 'fun (') is expected")
    }
    val (ts, params, rt, prologue, have_keywords) = parse_fun_params(ts)
    val fname = gen_temp_id("lambda")
    val fname_exp = make_ident(fname, loc)
    val (ts, df) = parse_body_and_make_fun(ts, fname, params, rt, prologue,
        default_fun_flags().{fun_flag_private=true, fun_flag_has_keywords=have_keywords}, loc)
    (ts, ExpSeq(df :: fname_exp :: [], make_new_ctx(loc)))
}

fun parse_typed_exp(ts: tklist_t): (tklist_t, exp_t) {
    val (ts, e) = parse_complex_exp(ts)
    match ts {
    | (COLON, _) :: rest =>
        val (ts, t) = parse_typespec(rest)
        (ts, ExpTyped(e, t, make_new_ctx(get_exp_loc(e))))
    | (CAST, _) :: rest =>
        val (ts, t) = parse_typespec(rest)
        (ts, ExpCast(e, t, (make_new_typ(), get_exp_loc(e))))
    | _ => (ts, e)
    }
}

type kw_param_t = (id_t, typ_t, defparam_t?, loc_t)

fun parse_fun_params(ts: tklist_t): (tklist_t, pat_t list, typ_t, exp_t list, bool)
{
    fun add_fun_param(ts: tklist_t, expect_comma: bool, params: pat_t list,
        kw_params: kw_param_t list): (tklist_t, pat_t list, kw_param_t list) =
        match ts {
        | (COMMA, _) :: rest =>
            if expect_comma { add_fun_param(rest, false, params, kw_params) }
            else { throw parse_err(ts, "extra ','?") }
        | (RPAREN, _) :: rest =>
            (rest, params.rev(), kw_params.rev())
        | (TILDE, ploc) :: (IDENT(_, i), _) :: (COLON, _) :: rest =>
            if expect_comma { throw parse_err(ts, "',' is expected") }
            val (ts, t) = parse_typespec(rest)
            val (ts, defparam) = match ts {
                | (EQUAL, _) :: (LITERAL(lit), _) :: rest => (rest, Some(lit))
                | _ => (ts, None)
                }
            add_fun_param(ts, true, params, (get_id(i), t, defparam, ploc) :: kw_params)
        | _ =>
            if expect_comma { throw parse_err(ts, "',' is expected") }
            if kw_params != [] {
                throw parse_err(ts, "positional parameters cannot occur after or between named parameters")
            }
            val (ts, p) = parse_pat(ts, true)
            add_fun_param(ts, true, p :: params, kw_params)
        }
    val (ts, params, kw_params) = add_fun_param(ts, false, [], [])
    val (ts, rt) = match ts {
        | (COLON, _) :: rest => parse_typespec(rest)
        | _ => (ts, make_new_typ())
        }
    if kw_params == [] {
        (ts, params, rt, [], false)
    } else {
        val recarg = gen_temp_id("__kwargs__")
        val relems = [: for (i, t, v0, _) <- kw_params { (i, t, v0) } :]
        val rectyp = TypRecord(ref (relems, true))
        val (_, _, _, loc) = kw_params.hd()
        val recpat = PatRecord(None, [: for (i, _, _, loci) <- kw_params {(i, PatIdent(i, loci))} :], loc)
        val unpack_rec = DefVal(recpat, make_ident(recarg, loc), default_tempval_flags(), loc)
        val recparam = PatTyped(PatIdent(recarg, loc), rectyp, loc)
        (ts, params + (recparam :: []), rt, unpack_rec :: [], true)
    }
}

fun parse_body_and_make_fun(ts: tklist_t, fname: id_t, params: pat_t list, rt: typ_t,
                            prologue: exp_t list, fflags: fun_flags_t, loc: loc_t): (tklist_t, exp_t)
{
    val (ts, params, body, fflags) = match ts {
        | (EQUAL, _) :: (CCODE, _) :: _ =>
            val (ts, body) = parse_ccode_exp(ts.tl(), make_new_typ())
            (ts, params, body, fflags.{fun_flag_ccode = true})
        | (EQUAL, _) :: rest =>
            val (ts, body) = parse_stmt(ts.tl())
            (ts, params, body, fflags)
        | (LBRACE, l1) :: (BAR, _) :: _ =>
            val (ts, cases) = parse_match_cases(ts)
            val (params, match_arg) = plist2exp(params, "param", l1)
            val match_e = ExpMatch(match_arg, cases, make_new_ctx(l1))
            (ts, params, match_e, fflags)
        | (LBRACE, _) :: _ =>
            val (ts, body) = parse_block(ts)
            (ts, params, body, fflags)
        | _ =>
            throw parse_err(ts, "'=' or '{' is expected")
        }

    val body =
        if prologue == [] { body }
        else {
            val loc = get_exp_loc(body)
            expseq2exp(prologue + exp2expseq(body), loc)
        }
    val paramtyps = [: for p <- params {
            match p {
            | PatTyped(_, t, _) => t
            | _ => make_new_typ()
            }
        } :]
    val df = ref (deffun_t { df_name=fname, df_templ_args=[],
        df_args=params, df_typ=TypFun(paramtyps, rt), df_body=body,
        df_flags=fflags, df_scope=[], df_loc=loc,
        df_templ_inst=ref [], df_env=empty_env})

    (ts, DefFun(df))
}


fun parse_stmt(ts: tklist_t): (tklist_t, exp_t)
{
    | (BREAK, l1) :: rest => (rest, ExpBreak(false, l1))
    | (CONTINUE, l1) :: rest => (rest, ExpContinue(l1))
    | (THROW, l1) :: rest => val (ts, e) = parse_exp(rest); (ts, ExpThrow(e, l1))
    | (WHILE(true), l1) :: rest =>
        val (ts, c) = parse_exp_or_block(rest)
        val (ts, body) = parse_block(ts)
        (ts, ExpWhile(c, body, l1))
    | (DO, l1) :: rest =>
        val (ts, body) = parse_exp_or_block(rest)
        val (ts, c) = match ts {
            | (WHILE _, _) :: rest => parse_exp_or_block(rest)
            | _ => throw parse_err(ts, "'while' is expected in do-while loop")
            }
        (ts, ExpDoWhile(body, c, l1))
    | (PARALLEL, _) :: _ | (FOR _, _) :: _ =>
        val (ts, for_exp, _) = parse_for(ts, ForMakeNone)
        (ts, for_exp)
    | (t, l1) :: _  =>
        val (ts, e1) = parse_complex_exp(ts)
        val lvalue_e1 = // very light check; more detailed one is done by the type checker
            match e1 {
            | ExpUnary(OpDeref, _, _) | ExpIdent _ | ExpAt _ | ExpMem _ => true
            | _ => false }
        match ts {
        | (EQUAL, l2) :: rest =>
            if !lvalue_e1 { throw parse_err(ts, "left-hand-side of the assignment is not an l-value") }
            val (ts, e2) = parse_complex_exp(rest)
            (ts, ExpAssign(e1, e2, l2))
        | (AUG_BINOP(binop), l2) :: rest =>
            if !lvalue_e1 { throw parse_err(ts, "left-hand-side of the assignment is not an l-value") }
            val (ts, e2) = parse_complex_exp(rest)
            val e2 = make_binary(binop, e1, e2, l2)
            (ts, ExpAssign(e1, e2, l1))
        | (DOT_EQUAL, l2) :: (LBRACE, _) :: rest =>
            if !lvalue_e1 { throw parse_err(ts, "left-hand-side of the assignment is not an l-value") }
            val (ts, _, _, rec_init_elems) = parse_exp_list(ts, RBRACE,
                kw_mode=KwMust, allow_empty=true, stop_at_semicolon=false)
            (ts, ExpUpdateRecord(e1, rec_init_elems, make_new_ctx(l2)))
        | _ => (ts, e1)
        }
}

fun parse_pat_list(ts: tklist_t, expect_comma: bool,
                   result: pat_t list, simple: bool): (tklist_t, pat_t list)
{
    //println(f"parse_pat_list (expect_comma={expect_comma}): {tok2str(ts)}\n")
    match (ts, expect_comma) {
    | ((COMMA, _) :: rest, _) =>
        if expect_comma { parse_pat_list(rest, false, result, simple) }
        else { throw parse_err(ts, "extra ','?")}
    | ((RPAREN, _) :: rest, _) =>
        if result == [] { throw parse_err(ts, "empty tuple pattern are not allowed") }
        (rest, result.rev())
    | (_, true) =>
        throw parse_err(ts, "',' is expected")
    | _ =>
        val (ts, p) = parse_pat(ts, simple)
        parse_pat_list(ts, true, p :: result, simple)
    }
}

fun parse_idpat_list(ts: tklist_t, expect_comma: bool,
        result: (id_t, pat_t) list, simple: bool): (tklist_t, (id_t, pat_t) list) =
    match (ts, expect_comma) {
    | ((COMMA, _) :: rest, _) =>
        if expect_comma { parse_idpat_list(rest, false, result, simple) }
        else { throw parse_err(ts, "extra ','?") }
    | ((RBRACE, _) :: rest, _) =>
        (rest, result.rev())
    | (_, true) =>
        throw parse_err(ts, "',' is expected")
    | ((IDENT(_, i_), l1) :: rest, _) =>
        if i_ == "_" {throw ParseError(l1, "'_' cannot be used as a record elem name")}
        val i = get_id(i_)
        val (ts, p) = match rest {
            | (EQUAL, _) :: rest => parse_pat(rest, simple)
            | _ => (rest, PatIdent(i, l1))
            }
        parse_idpat_list(ts, true, (i, p) :: result, simple)
    | _ => throw parse_err(ts, "identifier is expected")
    }

fun parse_pat(ts: tklist_t, simple: bool): (tklist_t, pat_t)
{
    fun parse_base_pat_(ts: tklist_t, simple: bool)
    {
        //println(f"parse_base_pat_: {tok2str(ts)}\n")
        match ts {
        | (LITERAL(lit), l1) :: rest =>
            if simple {throw parse_err(ts, "literals cannot be used in this pattern")}
            (rest, PatLit(lit, l1))
        | (IDENT(_, _), l1) :: _ =>
            val (ts, i1_) = parse_dot_ident(ts, false, "")
            val i1 = get_id(i1_)
            val is_any = i1_ == "_"
            match ts {
            | (IDENT(_, i2_), l2) :: rest =>
                if is_any { throw ParseError(l1, "'_' cannot be used as a variant label") }
                val p2 = if i2_ == "_" {PatAny(l2)} else {PatIdent(get_id(i2_), l2)}
                (rest, PatVariant(i1, p2 :: [], l1))
            | (LITERAL(lit), l2) :: rest =>
                if is_any { throw ParseError(l1, "'_' cannot be used as a variant label") }
                if simple {throw parse_err(ts, "literals cannot be used in this pattern")}
                (rest, PatVariant(i1, PatLit(lit, l2) :: [], l1))
            | (LPAREN(false), l2) :: rest =>
                if is_any { throw ParseError(l1, "'_' cannot be used as a variant label") }
                val (ts, pl) = parse_pat_list(rest, false, [], simple)
                (ts, PatVariant(i1, pl, l1))
            | (LBRACE, l2) :: rest =>
                if is_any { throw ParseError(l1, "'_' cannot be used as a variant label") }
                val (ts, ipl) = parse_idpat_list(rest, false, [], simple)
                (ts, PatRecord(Some(i1), ipl, l1))
            | _ =>
                if is_any {
                    (ts, PatAny(l1))
                }
                else if good_variant_name(i1_) && (!simple || i1_.contains('.')) {
                    (ts, PatVariant(i1, [], l1))
                }
                else {
                    (ts, PatIdent(i1, l1))
                }
            }
        | (LPAREN _, l1) :: rest =>
            val (ts, pl) = parse_pat_list(rest, false, [], simple)
            (ts, match pl { | p :: [] => p | _ => PatTuple(pl, l1) })
        | (LBRACE, l1) :: rest =>
            val (ts, ipl) = parse_idpat_list(rest, false, [], simple)
            (ts, PatRecord(None, ipl, l1))
        | (REF(true), l1) :: rest =>
            val (ts, p) = parse_base_pat_(rest, simple)
            (ts, PatRef(p, l1))
        | _ =>
            throw parse_err(ts, "pattern is expected")
        }
    }

    fun extend_pat_(ts: tklist_t, result: pat_t, min_prec: int, simple: bool)
    {
        //println(f"extend_pat_: {tok2str(ts)}\n")
        match ts {
        | (t, l) :: rest =>
            // roughly sort binary ops by how often they are met in the code,
            // so that we have less checks in general
            val (prec, assoc) = match t {
                | WHEN => (40, AssocLeft)
                | CONS => (50, AssocRight)
                | AS => (60, AssocLeft)
                | COLON => (70, AssocLeft)
                | _ => (-1, AssocLeft)
            }
            if prec < min_prec { (ts, result) }
            else {
                val next_min_prec = match assoc { | AssocLeft => prec+1 | _ => prec }
                match t {
                | WHEN =>
                    if simple {throw parse_err(ts, "when-patterns are not allowed here")}
                    val (ts, e) = parse_complex_exp(rest)
                    (ts, PatWhen(result, e, l))
                | AS =>
                    match rest {
                    | (IDENT(_, i), _) :: rest =>
                        extend_pat_(rest, PatAs(result, get_id(i), l), min_prec, simple)
                    | _ => throw parse_err(ts, "identifier is expected")
                    }
                | COLON =>
                    val (ts, t) = parse_typespec(rest)
                    extend_pat_(ts, PatTyped(result, t, l), min_prec, simple)
                | CONS =>
                    if simple {throw parse_err(ts, "'::'-patterns are not allowed here")}
                    val (ts, p2) = parse_base_pat_(rest, simple)
                    val (ts, p2) = extend_pat_(ts, p2, next_min_prec, simple)
                    extend_pat_(ts, PatCons(result, p2, l), min_prec, simple)
                | _ => throw parse_err(ts, "unxpected token")
                }
            }
        | _ => (ts, result)
        }
    }

    fun parse_alt_(ts: tklist_t, expect_bar: bool, simple: bool, result: pat_t list): (tklist_t, pat_t)
    {
        //println(f"parse_alt_(expect_bar={expect_bar}): {tok2str(ts)}\n")
        match (ts, expect_bar) {
        | ((BAR, _) :: _, _) | ((BITWISE_OR, _) :: _, _) =>
            if expect_bar { parse_alt_(ts.tl(), false, simple, result) }
            else { throw parse_err(ts, "extra '|'?") }
        | ((ARROW, _) :: _, _) => throw parse_err(ts, "unexpected '->', did you mean '=>'?")
        | ((DOUBLE_ARROW, _) :: _, true) | ((COMMA, _) :: _, true) | ((RPAREN, _) :: _, true) | ((RBRACE, _) :: _, true) =>
            val p = match result {
                | p :: [] => p
                | _ =>
                    val result = result.rev()
                    PatAlt(result, get_pat_loc(result.hd()))
                }
            (ts, p)
        | ((DOUBLE_ARROW, _) :: _, false) =>
            throw parse_err(ts, "pattern is expected")
        | _ =>
            val (ts, p) = parse_base_pat_(ts, simple)
            val (ts, p) = extend_pat_(ts, p, 0, simple)
            if simple { (ts, p) } else { parse_alt_(ts, true, simple, p :: result) }
        }
    }
    parse_alt_(ts, false, simple, [])
}

type mcase_t = (pat_t, exp_t)

fun parse_match_cases(ts: tklist_t): (tklist_t, mcase_t list)
{
    val ts = match ts {
    | (LBRACE, _) :: (BAR, _) :: rest => rest
    | (LBRACE, _) :: rest => rest
    | _ => throw parse_err(ts, "'{' is expected")
    }

    fun extend_match_cases_(ts: tklist_t, result: mcase_t list): (tklist_t, mcase_t list) =
        match ts {
        | (RBRACE, _) :: rest =>
            if result == [] {
                throw parse_err(ts, "at least one pattern-matching case is expected")
            }
            (rest, result.rev())
        | _ =>
            val (ts, p) = parse_pat(ts, false)
            val ts = match ts {
                | (DOUBLE_ARROW, _) :: rest => rest
                | _ => throw parse_err(ts, "'=>' is expected")
                }
            val (ts, e) = match ts {
                | (LBRACE, l1) :: (RBRACE, _) :: rest =>
                    (rest, ExpNop(l1))
                | (t, l1) :: _ =>
                    val (ts, el) = parse_expseq(ts, false)
                    match el {
                    | _ :: _ => (ts, expseq2exp(el, noloc))
                    | _ => throw parse_err(ts,
                        "some expressions are expected; use '{}' if there is no action for the particular match case")
                    }
                }
            val ts = match ts {
                | (BAR, _) :: rest => rest
                | _ => ts
                }
            extend_match_cases_(ts, (p, e) :: result)
        }
    extend_match_cases_(ts, [])
}

fun typ2typlist(t: typ_t): typ_t list
{
    | TypTuple(tl) => tl
    | TypVoid => []
    | _ => t :: []
}

fun parse_atomic_typ_(ts: tklist_t): (tklist_t, typ_t)
{
    | (IDENT(_, i), _) :: _ =>
        val (ts, i) = parse_dot_ident(ts, false, "")
        val t = match i {
            | "int" => TypInt
            | "float" => TypFloat(32)
            | "double" => TypFloat(64)
            | "string" => TypString
            | "char" => TypChar
            | "bool" => TypBool
            | "int8" => TypSInt(8)
            | "uint8" => TypUInt(8)
            | "int16" => TypSInt(16)
            | "uint16" => TypUInt(16)
            | "int32" => TypSInt(32)
            | "uint32" => TypUInt(32)
            | "int64" => TypSInt(64)
            | "uint64" => TypUInt(64)
            | "half" => TypFloat(16)
            | "void" => TypVoid
            | "exn" => TypExn
            | "cptr" => TypCPointer
            | _ => TypApp([], get_id(i))
        }
        (ts, t)
    | (TYVAR(i), _) :: rest =>
        (rest, TypApp([], get_id(i)))
    | (LPAREN _, l1) :: rest =>
        parse_typtuple_(rest, false, [], l1)
    | (LBRACE, _) :: (ELLIPSIS, _) :: (RBRACE, _) :: rest =>
        (rest, TypVar (ref (Some (TypVarRecord))))
    | _ =>
        throw parse_err(ts, "type is expected here (starting from indent, type var, '(' or '{'")
}

fun extend_typespec_nf_(ts: tklist_t, result: typ_t): (tklist_t, typ_t) =
    match ts {
    | (IDENT(false, _), _) :: _ =>
        val (ts, i) = parse_dot_ident(ts, false, "")
        val t = match i {
            | "list" => TypList(result)
            | _ => TypApp(typ2typlist(result), get_id(i))
            }
        extend_typespec_nf_(ts, t)
    | (QUESTION, _) :: rest =>
        extend_typespec_nf_(rest, TypApp(result :: [], get_id("option")))
    | (REF(false), _) :: rest =>
        extend_typespec_nf_(rest, TypRef(result))
    | (LSQUARE(false), _) :: (PLUS _, _) :: (RSQUARE, _) :: rest =>
        extend_typespec_nf_(rest, TypVar(ref (Some(TypVarArray(result)))))
    | (LSQUARE(false), _) :: rest =>
        var vts = rest, ndims = 1
        while true {
            match vts {
            | (COMMA, _) :: rest => vts = rest; ndims += 1
            | (RSQUARE, _) :: rest => vts = rest; break
            | _ => throw parse_err(ts, "unfinished array shape specification (missing ']'?)")
            }
        }
        extend_typespec_nf_(vts, TypArray(ndims, result))
    | _ => (ts, result)
    }

fun parse_typespec_nf(ts: tklist_t): (tklist_t, typ_t)
{
    val (ts, t) = parse_atomic_typ_(ts)
    extend_typespec_nf_(ts, t)
}

fun parse_typtuple_(ts: tklist_t, expect_comma: bool,
    result: typ_t list, loc: loc_t): (tklist_t, typ_t) =
    match ts {
    | (ELLIPSIS, _) :: (RPAREN, _) :: rest =>
        val t = match result {
            | [] => TypVar (ref (Some (TypVarTuple(None))))
            | t :: [] =>
                if !expect_comma { throw parse_err(ts, "extra ',' before '...'?") }
                TypVar (ref (Some (TypVarTuple(Some(t)))))
            | _ => throw parse_err(ts, "'...' can only be used with '{...}', '(...)' or '(t1 ...)' type specifications")
            }
        (rest, t)
    | (COMMA, _) :: rest =>
        if expect_comma { parse_typtuple_(rest, false, result, loc) }
        else { throw parse_err(ts, "extra ','?") }
    | (LITERAL(LitInt(n)), _) :: (STAR _, _) :: rest =>
        if expect_comma { throw parse_err(ts, "',' is expected") }
        if n <= 0L { throw parse_err(ts, "tuple multiplicator should be positive") }
        val (ts, t) = parse_typespec_nf(rest)
        val tt = [: for i <- 0:(n :> int) {t} :]
        parse_typtuple_(ts, true, tt + result, loc)
    | (RPAREN, _) :: rest =>
        val t = match result {
            | t :: [] => t
            | [] => throw parse_err(ts, "empty tuple")
            | _ => TypTuple(result.rev())
        }
        (rest, t)
    | _ =>
        if expect_comma { throw parse_err(ts, "',' is expected") }
        val (ts, t) = parse_typespec_nf(ts)
        match ts {
        | (STAR _, _) :: (LITERAL(LitInt(n)), _) :: rest =>
            val tt = [: for i <- 0:(n :> int) {t} :]
            parse_typtuple_(rest, true, tt + result, loc)
        | (ARROW, _) :: rest =>
            val (ts, rt) = parse_typespec(rest)
            parse_typtuple_(ts, true, TypFun(typ2typlist(t), rt) :: result, loc)
        | _ =>
            val (ts, t) = extend_typespec_nf_(ts, t)
            parse_typtuple_(ts, true, t :: result, loc)
        }
    }

fun parse_typespec(ts: tklist_t): (tklist_t, typ_t)
{
    val (ts, t) = parse_typespec_nf(ts)
    match ts {
    | (ARROW, _) :: rest =>
        val (ts, rt) = parse_typespec(rest)
        (ts, TypFun(typ2typlist(t), rt))
    | _ => (ts, t)
    }
}

type relem_t = (id_t, typ_t, defparam_t?)

fun parse_typespec_or_record(ts: tklist_t): (tklist_t, typ_t)
{
    | (LBRACE, _) :: (ELLIPSIS, _) :: (RBRACE, _) :: _ =>
        throw parse_err(ts, "'{...}' cannot be used inside type definitions, only for function parameters")
    | (LBRACE, _) :: rest =>
        fun parse_relems_(ts: tklist_t, expect_semicolon: bool, result: relem_t list): (tklist_t, relem_t list) =
            match ts {
            | (SEMICOLON, _) :: rest =>
                if expect_semicolon { parse_relems_(rest, false, result) }
                else { throw parse_err(ts, "extra ';'?") }
            | (RBRACE, _) :: rest =>
                if result == [] { throw parse_err(ts, "empty list of record elements") }
                (rest, result.rev())
            | (IDENT(f, i), l1) :: (COLON, _) :: rest =>
                if expect_semicolon && !f {
                    throw parse_err(ts, "';' or newline should be inserted between record elements")
                }
                val (ts, t) = parse_typespec(rest)
                val (ts, default_) = match ts {
                    | (EQUAL, _) :: (LITERAL(lit), _) :: rest => (rest, Some(lit))
                    | _ => (ts, None)
                }
                parse_relems_(ts, true, (get_id(i), t, default_) :: result)
            | _ =>
                throw parse_err(ts, (if expect_semicolon {"';' or newline is expected"} else
                    {"identifier followed by ':' is expected"}))
            }
        val (ts, relems) = parse_relems_(rest, false, [])
        (ts, TypRecord(ref (relems, true)))
    | _ => parse_typespec(ts)
}

fun parse_deftype(ts: tklist_t)
{
    val (ts, object_type_module) = match ts {
        | (OBJECT, _) :: rest => (rest, parser_ctx.module_id)
        | _ => (ts, noid)
        }

    val ts = match ts {
        | (TYPE, _) :: rest => rest
        | _ => throw parse_err(ts, "'type' is expected")
        }

    fun parse_tyvars_(ts: tklist_t, expect_comma: bool, tyvars: id_t list, loc: loc_t): (tklist_t, id_t list) =
        match ts {
        | (COMMA, _) :: rest =>
            if expect_comma { parse_tyvars_(rest, false, tyvars, loc) }
            else { throw parse_err(ts, "extra ','?") }
        | (TYVAR(i), _) :: rest =>
            if expect_comma { throw parse_err(ts, "',' is expected") }
            parse_tyvars_(rest, true, get_id(i) :: tyvars, loc)
        | (RPAREN, _) :: rest => (rest, tyvars.rev())
        | _ => throw parse_err(ts, f"incomplete type var list started at {loc}, ')' is missing?")
        }

    val (ts, type_params) = match ts {
        | (TYVAR(i), _) :: rest => (rest, get_id(i) :: [])
        | (LPAREN _, l1) :: rest =>
            val (ts, type_params) = parse_tyvars_(rest, false, [], l1)
            if type_params == [] { throw parse_err(ts,
                "empty list of type parameters inside (); if you don't want type parameters, just remove ()")
            }
            (ts, type_params)
        | _ => (ts, [])
        }

    val (ts, tname, loc) = match ts {
        | (IDENT(_, i), loc) :: (EQUAL, _) :: rest => (rest, get_id(i), loc)
        | _ => throw parse_err(ts, f"the type name, followed by '=' is expected")
        }

    //println(f"type definition body({tok2str(ts)})\n")
    match ts {
    | (LBRACE, _) :: _ =>
        val (ts, t) = parse_typespec_or_record(ts)
        val dvar = ref (defvariant_t {
            dvar_name = tname, dvar_templ_args=type_params,
            dvar_alias = make_new_typ(),
            dvar_flags = default_variant_flags().{
                var_flag_record=true,
                var_flag_object=object_type_module },
            dvar_cases = (tname, t) :: [],
            dvar_ctors = [],
            dvar_templ_inst = ref [],
            dvar_scope = [],
            dvar_loc = loc
        })
        (ts, DefVariant(dvar))
    | (BITWISE_OR, _) :: (IDENT(_, _), _) :: _
    | (IDENT(_, _), _) :: (BITWISE_OR, _) :: _
    | (IDENT(_, _), _) :: (COLON, _) :: _ =>
        val ts = match ts { | (BITWISE_OR, _) :: rest => rest | _ => ts }
        fun parse_cases_(ts: tklist_t, expect_bar: bool,
            result: (id_t, typ_t) list): (tklist_t, (id_t, typ_t) list) =
            match ts {
            | (BITWISE_OR, _) :: rest =>
                if expect_bar { parse_cases_(rest, false, result) }
                else { throw parse_err(ts, "extra '|'?") }
            | (IDENT(_, i), _) :: (COLON, _) :: rest =>
                if expect_bar { (ts, result.rev()) }
                else {
                    if !good_variant_name(i) {
                        throw parse_err(ts, "variant label should start with a capital letter A..Z")
                    }
                    val (ts, t) = parse_typespec_or_record(rest)
                    parse_cases_(ts, true, (get_id(i), t) :: result)
                }
            | (IDENT(_, i), _) :: rest =>
                if expect_bar { (ts, result.rev()) }
                else {
                    if !good_variant_name(i) {
                        throw parse_err(ts, "variant label should start with a capital letter A..Z")
                    }
                    parse_cases_(rest, true, (get_id(i), TypVoid) :: result)
                }
            | _ => (ts, result.rev())
            }
        val (ts, cases) = parse_cases_(ts, false, [])
        val dvar = ref (defvariant_t {
            dvar_name = tname, dvar_templ_args=type_params,
            dvar_alias = make_new_typ(),
            dvar_flags = default_variant_flags().{
                var_flag_record=false,
                var_flag_object=object_type_module},
            dvar_cases = cases,
            dvar_ctors = [],
            dvar_templ_inst = ref [],
            dvar_scope = [],
            dvar_loc = loc
        })
        (ts, DefVariant(dvar))
    | _ =>
        if object_type_module != noid { throw parse_err(ts, "type aliase (i.e. not a record nor variant) cannot be 'object type'") }
        val (ts, t) = parse_typespec(ts)
        val dt = ref (deftyp_t {
            dt_name=tname, dt_templ_args=type_params, dt_typ=t, dt_finalized=false,
            dt_scope=[], dt_loc=loc })
        (ts, DefTyp(dt))
    }
}

fun parse(dm: Ast.defmodule_t ref, preamble: token_t list, inc_dirs: string list): bool
{
    val fname_id = get_id(dm->dm_filename)
    parser_ctx = parser_ctx_t {
        module_id = dm->dm_name,
        filename = dm->dm_filename,
        deps = [],
        inc_dirs = inc_dirs,
        default_loc = loc_t { fname=fname_id, line0=1, col0=1, line1=1, col1=1 }
    }
    val strm = try Lexer.make_stream(dm->dm_filename)
        catch {
        | FileOpenError => throw ParseError(parser_ctx.default_loc, "cannot open file")
        | IOError => throw ParseError(parser_ctx.default_loc, "cannot read file")
        }
    val lexer = Lexer.make_lexer(strm)

    // [TODO] perhaps, need to avoid fetching all the tokens at once
    var all_tokens: (Lexer.token_t, Ast.loc_t) list = []
    while true {
        val more_tokens = lexer()
        for (t, (lineno, col)) <- more_tokens {
            val loc = Ast.loc_t {fname=fname_id, line0=lineno, col0=col, line1=lineno, col1=col}
            //print_tokens((t, loc) :: [], endl=false)
            all_tokens = (t, loc) :: all_tokens
        }
        match all_tokens {
        | (Lexer.EOF, _) :: _ => break
        | _ => {}
        }
    }
    all_tokens = all_tokens.rev()
    for t <- preamble.rev() { all_tokens = (t, parser_ctx.default_loc) :: all_tokens }
    dm->dm_parsed = true
    dm->dm_defs = parse_expseq(all_tokens, true).1
    dm->dm_deps = parser_ctx.deps.rev()
    true
}
