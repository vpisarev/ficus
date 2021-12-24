/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Ficus recursive descent parser

import File, Filename, Hashmap, Hashset, Sys
from Ast import *
import LexerUtils as Lxu
from Lexer import *
import Options

exception ParseError: (loc_t, string)

type parser_ctx_t =
{
    m_idx: int;
    filename: string;
    deps: int list;
    inc_dirs: string list;
    default_loc: loc_t;
}

var parser_ctx = parser_ctx_t { m_idx=-1, filename="", deps=[], inc_dirs=[], default_loc=noloc }

fun add_to_imported_modules(mname: id_t, loc: loc_t): int
{
    val mfname = pp(mname)
    var ncomps = mfname.split('.', allow_empty=false).length()
    val mfname = mfname.replace(".", Filename.dir_sep())
    val mfname =
        try Filename.locate(mfname + ".fx", parser_ctx.inc_dirs)
        catch {
        | NotFoundError =>
            try {
                ncomps += 1
                Filename.locate(Filename.concat(mfname, "init.fx"), parser_ctx.inc_dirs)
            }
            catch {
            | NotFoundError => throw ParseError(loc, f"module {mname} is not found")
            }
        }
    var dirname = mfname
    for i <- 0:ncomps {
        dirname = Filename.dirname(dirname)
        if all_c_inc_dirs.mem(dirname) {break}
        all_c_inc_dirs.add(dirname)
    }
    val m_idx = find_module(mname, mfname)
    if !parser_ctx.deps.mem(m_idx) {
        parser_ctx.deps = m_idx :: parser_ctx.deps
    }
    m_idx
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
    " ".join([for t <- ts_part.rev() {tok2str(t).0}])
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

fun plist2exp(pl: pat_t list, loc: loc_t): (pat_t list, exp_t)
{
    val prefix = "__pat__"
    fun pat2exp_(p: pat_t): (pat_t, exp_t)
    {
        | PatAny(loc) =>
            val param_id = gen_id(parser_ctx.m_idx, prefix)
            (PatIdent(param_id, loc), make_ident(param_id, loc))
        | PatIdent(i, loc) => (p, make_ident(i, loc))
        | PatAs(p, i, loc) => (p, make_ident(i, loc))
        | PatTyped(p, t, loc) =>
            val (p, e) = pat2exp_(p)
            (PatTyped(p, t, loc), e)
        | _ =>
            val loc = get_pat_loc(p)
            val param_id = gen_id(parser_ctx.m_idx, prefix)
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
    val fr_id = std__fold_result__
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
        val new_body = ExpSeq([acc_decl, update_fr], void_ctx)
        (fr_decl, new_body, fr_exp, global_flags)
    | "all" | "exists" =>
        val is_all = special == "all"
        val fr_decl = DefVal(PatIdent(fr_id, acc_loc), ExpLit(LitBool(is_all),
                            (TypBool, acc_loc)), default_var_flags(), acc_loc)
        val break_exp = ExpSeq([ExpAssign(fr_exp, ExpLit(LitBool(!is_all), bool_ctx), body_loc),
                                ExpBreak(true, body_loc)], void_ctx)
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
        val break_exp = ExpSeq([ExpAssign(fr_exp, mksome_exp, body_loc),
                                ExpBreak(true, body_loc)], void_ctx)
        val new_body = ExpIf(fold_body, break_exp, ExpNop(body_loc), void_ctx)
        val new_fr_exp =
        match special {
        | "find" =>
            val x = get_id("x")
            val some_case = (PatVariant(some, [PatIdent(x, body_end_loc) ],
                             body_end_loc), make_ident(x, body_end_loc))
            val none_case = (PatAny(body_end_loc), ExpThrow(ExpIdent(
                            get_id("NotFoundError"), (TypExn, body_end_loc)), body_end_loc))
            ExpMatch(fr_exp, [some_case, none_case ], (make_new_typ(), body_end_loc))
        | _ => fr_exp
        }
        (fr_decl, new_body, new_fr_exp, global_flags)
    | "filter" =>
        val check_exp = ExpIf(ExpUnary(OpLogicNot, fold_body, bool_ctx),
                              ExpContinue(body_loc), ExpNop(body_loc), void_ctx)
        val new_body = ExpSeq([check_exp, for_iter_exp ], make_new_ctx(body_loc))
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
        val llist = [for e <- eseq {get_exp_loc(e)} ]
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

fun parse_ident_list(ts: tklist_t, expect_comma: bool, dot_idents: bool, result: id_t list): (tklist_t, id_t list) =
    match ts {
    | (COMMA, _) :: rest =>
        if expect_comma {parse_ident_list(rest, false, dot_idents, result)}
        else {throw parse_err(ts, "extra ','?")}
    | (IDENT(_, i), _) :: rest =>
        if expect_comma { (ts, result.rev()) }
        else {
            val (ts, i) = if dot_idents { parse_dot_ident(ts, false, "") } else { (rest, i) }
            parse_ident_list(ts, true, dot_idents, get_id(i) :: result)
        }
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
    | (LPAREN(ne), l1) :: (SYNC, _) :: _ =>
        check_ne(ne, ts)
        match_paren(parse_sync(ts.tl().tl()), RPAREN, l1)
    | (SYNC, _) :: _ =>
        parse_sync(ts.tl())
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
    | (LVECTOR, l1) :: (f, _) :: _ when is_for_start(f) =>
        val (ts, for_exp, _) = parse_for(ts.tl(), ForMakeVector)
        match_paren((ts, for_exp), RVECTOR, l1)
    | (LVECTOR, l1) :: rest =>
        val (ts, _, el, _) = parse_exp_list(rest, RVECTOR, kw_mode=KwNone, allow_empty=false)
        (ts, ExpMkVector(el, make_new_ctx(l1)))
    | (LSQUARE(true), l1) :: (f, _) :: rest when is_for_start(f) =>
        val (ts, for_exp, _) = parse_for(ts.tl(), ForMakeList)
        match_paren((ts, for_exp), RSQUARE, l1)
    | (LSQUARE(true), l1) :: rest =>
        val (ts, _, el, _) = parse_exp_list(rest, RSQUARE, kw_mode=KwNone, allow_empty=true)
        (ts, fold mklist_e = make_literal(LitEmpty, l1) for e <- el.rev() {
            ExpBinary(OpCons, e, mklist_e, make_new_ctx(get_exp_loc(e)))
        })
    | (t, _) :: _ =>
        throw parse_err(ts, f"unxpected token '{tok2str(t).1}'. An identifier, literal or expression enclosed in '( )', '[ ]' or '[: :]' brackets is expected here")
    | _ =>
        throw parse_err(ts, f"premature end of the stream; check the parens")
    }
}

fun parse_simple_exp(ts: tklist_t, allow_mkrecord: bool): (tklist_t, exp_t)
{
    //println(f"simple_exp({tok2str(ts)}), allow_mkrecord={allow_mkrecord}")
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
                    val iop =
                        match istr {
                        | "__intrin_sqrt__" => IntrinMath(get_id("sqrt"))
                        | "__intrin_pow__" => IntrinMath(get_id("pow"))
                        | "__intrin_sin__" => IntrinMath(get_id("sin"))
                        | "__intrin_cos__" => IntrinMath(get_id("cos"))
                        | "__intrin_tan__" => IntrinMath(get_id("tan"))
                        | "__intrin_asin__" => IntrinMath(get_id("asin"))
                        | "__intrin_acos__" => IntrinMath(get_id("acos"))
                        | "__intrin_atan__" => IntrinMath(get_id("atan"))
                        | "__intrin_atan2__" => IntrinMath(get_id("atan2"))
                        | "__intrin_log__" => IntrinMath(get_id("log"))
                        | "__intrin_exp__" => IntrinMath(get_id("exp"))
                        | "__intrin_atanh__" => IntrinMath(get_id("atanh"))
                        | "__intrin_asinh__" => IntrinMath(get_id("asinh"))
                        | "__intrin_acosh__" => IntrinMath(get_id("acosh"))
                        | "__intrin_sinh__" => IntrinMath(get_id("sinh"))
                        | "__intrin_cosh__" => IntrinMath(get_id("cosh"))
                        | "__intrin_tanh__" => IntrinMath(get_id("tanh"))
                        | "__intrin_min__" => IntrinMath(get_id("min"))
                        | "__intrin_max__" => IntrinMath(get_id("max"))
                        | "__intrin_abs__" => IntrinMath(get_id("abs"))
                        | "__intrin_floor__" => IntrinMath(get_id("floor"))
                        | "__intrin_ceil__" => IntrinMath(get_id("ceil"))
                        | "__intrin_round__" => IntrinMath(get_id("round"))
                        | "__intrin_sat_uint8__" => IntrinSaturate(ScUInt8)
                        | "__intrin_sat_int8__" => IntrinSaturate(ScInt8)
                        | "__intrin_sat_uint16__" => IntrinSaturate(ScUInt16)
                        | "__intrin_sat_int16__" => IntrinSaturate(ScInt16)
                        | "__intrin_size__" => IntrinGetSize
                        | _ => throw parse_err(ts, f"unknown/unsupported intrinsic {istr}")
                        }
                    ExpIntrin(iop, args, make_new_ctx(eloc))
                | _ =>
                    ExpCall(e, args, make_new_ctx(eloc))
                }
            extend_simple_exp_(ts_, call_exp)
        | (LSQUARE(false), _) :: rest =>
            val (ts, _, idxs, _) =
                parse_exp_list_f(rest,
                    fun(ts) {parse_range_exp(ts, allow_mkrecord=false)},
                    RSQUARE, kw_mode=KwNone, allow_empty=false)
            val at_exp = ExpAt(e, BorderNone, InterpNone, idxs, make_new_ctx(eloc))
            extend_simple_exp_(ts, at_exp)
        | (LBRACE, _) :: rest when allow_mkrecord =>
            val (ts, _, _, rec_init_elems) = parse_exp_list(rest, RBRACE,
                kw_mode=KwMust, allow_empty=true, stop_at_semicolon=false)
            val mkrecord_exp = ExpMkRecord(e, rec_init_elems, make_new_ctx(get_exp_loc(e)))
            extend_simple_exp_(ts, mkrecord_exp)
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

fun parse_deref_exp(ts: tklist_t, allow_mkrecord: bool): (tklist_t, exp_t) =
    match ts {
    | (STAR(true), l1) :: rest =>
        val (ts, e) = parse_deref_exp(rest, allow_mkrecord)
        (ts, make_unary(OpDeref, e, l1))
    | _ =>
        parse_simple_exp(ts, allow_mkrecord)
    }

fun parse_apos_exp(ts: tklist_t, allow_mkrecord: bool): (tklist_t, exp_t)
{
    val (ts, e) = parse_deref_exp(ts, allow_mkrecord)
    match ts {
    | (APOS, l1) :: rest => (rest, ExpUnary(OpApos, e, make_new_ctx(l1)))
    | _ => (ts, e)
    }
}

fun parse_unary_exp(ts: tklist_t, allow_mkrecord: bool): (tklist_t, exp_t)
{
    //println(f"unary_exp({tok2str(ts)})\n")
    match ts {
    | (REF(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest, allow_mkrecord)
        (ts, make_unary(OpMkRef, e, l1))
    | (MINUS(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest, allow_mkrecord)
        (ts, make_unary(OpNegate, e, l1))
    | (DOT_MINUS(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest, allow_mkrecord)
        (ts, make_unary(OpDotMinus, e, l1))
    | (PLUS(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest, allow_mkrecord)
        (ts, make_unary(OpPlus, e, l1))
    | (TILDE, l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest, allow_mkrecord)
        (ts, make_unary(OpBitwiseNot, e, l1))
    | (LOGICAL_NOT, l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest, allow_mkrecord)
        (ts, make_unary(OpLogicNot, e, l1))
    | (BACKSLASH(true), l1) :: rest =>
        val (ts, e) = parse_unary_exp(rest, allow_mkrecord)
        (ts, make_unary(OpExpand, e, l1))
    | _ =>
        parse_apos_exp(ts, allow_mkrecord)
    }
}

fun parse_binary_exp(ts: tklist_t, allow_mkrecord:bool) : (tklist_t, exp_t)
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
            | BACKSLASH(false) => (OpRDiv, 220, AssocLeft)
            | PERCENT => (OpMod, 220, AssocLeft)
            | CONS => (OpCons, 100, AssocRight)
            | POWER => (OpPow, 230, AssocRight)
            | SHIFT_LEFT => (OpShiftLeft, 200, AssocLeft)
            | SHIFT_RIGHT => (OpShiftRight, 200, AssocLeft)
            | BITWISE_OR => (OpBitwiseOr, 130, AssocLeft)
            | BITWISE_AND => (OpBitwiseAnd, 150, AssocLeft)
            | BITWISE_XOR => (OpBitwiseXor, 140, AssocLeft)
            | SPACESHIP => (OpSpaceship, 170, AssocLeft)
            | DOT_PLUS(false) => (OpDotAdd, 210, AssocLeft)
            | DOT_MINUS(false) => (OpDotSub, 210, AssocLeft)
            | DOT_STAR => (OpDotMul, 220, AssocLeft)
            | DOT_SLASH => (OpDotDiv, 220, AssocLeft)
            | DOT_PERCENT => (OpDotMod, 220, AssocLeft)
            | DOT_STAR => (OpDotMul, 220, AssocLeft)
            | DOT_POWER => (OpDotPow, 230, AssocRight)
            | DOT_CMP(cmpop) => (OpDotCmp(cmpop), 180, AssocLeft)
            | DOT_SPACESHIP => (OpDotSpaceship, 190, AssocLeft)
            | SAME => (OpSame, 190, AssocLeft)
            | _ => (OpAdd, -1, AssocLeft)
        }
        if prec < min_prec { (ts, result) }
        else {
            val next_min_prec = match assoc { | AssocLeft => prec+1 | _ => prec }
            val (ts, e) = parse_unary_exp(rest, allow_mkrecord)
            // non-tail call, parse rhs
            val (ts, rhs) = parse_binary_exp_(ts, e, next_min_prec)
            val result = make_binary(bop, result, rhs, l)
            parse_binary_exp_(ts, result, min_prec)
        }
    | _ => (ts, result)
    }
    val (ts, e) = parse_unary_exp(ts, allow_mkrecord)
    parse_binary_exp_(ts, e, 0)
}

fun parse_exp(ts: tklist_t, ~allow_mkrecord: bool): (tklist_t, exp_t)
{
    //println(f"parse_exp({tok2str(ts)}), allow_mkrecord={allow_mkrecord}\n")
    fun extend_chained_cmp_(ts: tklist_t,
        chain: ((cmpop_t, loc_t), exp_t) list): (tklist_t, exp_t)
    {
    //println(f"binary_exp({tok2str(ts)})\n")
    match ts {
    | (CMP(cmpop), l1) :: rest =>
        val (ts, e) = parse_binary_exp(rest, allow_mkrecord)
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
                            val tmp_id = gen_id(parser_ctx.m_idx, "t")
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
        val (ts, e) = parse_binary_exp(ts, allow_mkrecord)
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
    | _ => parse_exp(ts, allow_mkrecord=true)
}

fun parse_complex_exp_or_block(ts: tklist_t): (tklist_t, exp_t)
{
    | (LBRACE, _) :: _ => parse_block(ts)
    | _ => parse_complex_exp(ts)
}

fun parse_block(ts: tklist_t): (tklist_t, exp_t)
{
    | (SYNC, l1) :: rest =>
        parse_sync(rest)
    | (LBRACE, l1) :: rest =>
        val (ts, eseq) = parse_expseq(rest, false)
        val e = expseq2exp(eseq, l1)
        match_paren((ts, e), RBRACE, l1)
    | _ =>
        throw parse_err(ts, "'{' is expected")
}

fun parse_sync(ts: tklist_t): (tklist_t, exp_t)
{
    val (ts, n) = match ts {
        | (IDENT(_, _), _) :: _ =>
            val (ts, n) = parse_dot_ident(ts, false, "")
            (ts, get_id(n))
        | _ => (ts, noid)
        }
    match ts {
    | (LBRACE, l1) :: rest =>
        val (ts, eseq) = parse_expseq(rest, false)
        val e = ExpSync(n, expseq2exp(eseq, l1))
        match_paren((ts, e), RBRACE, l1)
    | _ => throw parse_err(ts, "'{' is expected")
    }
}

fun parse_ccode_exp(ts: tklist_t, t: typ_t): (tklist_t, exp_t) =
    match ts {
    | (CCODE, l1) :: (LITERAL(lit), l2) :: rest =>
        val s = get_string(lit, l2)
        (rest, ExpCCode(s, (t, l1)))
    | _ => throw parse_err(ts, "'ccode {...}' is expected")
    }

fun parse_range_exp(ts0: tklist_t, ~allow_mkrecord: bool): (tklist_t, exp_t)
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
        | (LBRACE, _) :: _ | (FOR _, _) :: _ | (WHEN, _) :: _
        | (RSQUARE, _) :: _ | (COMMA, _) :: _ =>
            if !expect_sep && result.length() != 1 {
                throw parse_err(ts,
                    "range expression may not end with ':', unless it's ':' or '<start>:' ")
            }
            val result = if expect_sep {result} else {None :: result}
            (ts, result.rev())
        | _ =>
            if expect_sep { throw parse_err(ts, "':' is expected") }
            val (ts, e) = parse_exp(ts, allow_mkrecord=allow_mkrecord)
            parse_range_(ts, true, Some(e) :: result)
        }
    val (ts, elist) = parse_range_(ts0, false, [])
    val ctx = make_new_ctx(loc)

    val e = match elist {
    | Some(e) :: [] => e // scalar index
    | None :: None :: [] => ExpRange(None, None, None, ctx) // :
    | [] => throw parse_err(ts0, "empty range expression")
    | None :: [] | None :: None :: None :: [] =>
        throw parse_err(ts0, "invalid range expression")
    | e1 :: [] => ExpRange(e1, None, None, ctx) // a:
    | e1 :: e2 :: [] => ExpRange(e1, e2, None, ctx) // a:b or :b
    | e1 :: e2 :: e3 :: [] => ExpRange(e1, e2, e3, ctx) // a:b:c or a::c or :b:c or ::c
    | _ =>
        throw parse_err(ts0, f"invalid range expression of {elist.length()} components")
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
        | (TYPE, _) :: _ | (CLASS, _) :: _ =>
            val (ts, deftyp) = parse_deftype(ts)
            extend_expseq_(ts, deftyp :: result)
        | (INTERFACE, l1) :: rest =>
            val (ts, defiface) = parse_iface(rest, l1)
            extend_expseq_(ts, defiface :: result)
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
            fun parse_imported_(ts: tklist_t, expect_comma: bool, result: (int, id_t) list):
                (tklist_t, (int, id_t) list) =
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
                val (ts, il) = parse_ident_list(rest, false, false, [])
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
    var nested_fors: ((pat_t, pat_t, exp_t) list, exp_t?, loc_t) list = []
    var vts = ts

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
        result: (pat_t, pat_t, exp_t) list, loc: loc_t):
        (tklist_t, (pat_t, pat_t, exp_t) list) =
        match ts {
        | (COMMA, _) :: rest =>
            if expect_comma { parse_for_clause_(rest, false, result, loc) }
            else { throw parse_err(ts, "extra ','?") }
        | (FOR _, _) :: _  | (WHEN, _) :: _ | (LBRACE, _) :: _ when expect_comma =>
            if result == [] {
                throw parse_err(ts,
                    "empty for? (need at least one <iter_pat> <- <iter_range or collection>)")
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
            val (ts, e) = parse_range_exp(ts, allow_mkrecord=false)
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
        val (ts, when_e) = match ts {
            | (WHEN, _) :: rest =>
                val (ts, e) = parse_exp(rest, allow_mkrecord=false)
                (ts, Some(e))
            | _ => (ts, None)
            }
        nested_fors = (for_cl, when_e, loc_i) :: nested_fors
        vts = ts
    }

    val glob_loc = match nested_fors {| (_, _, loc) :: _ => loc | _ => noloc}

    // process the nested for.
    val fold (glob_el, nested_fors) = ([], []) for (ppe_list, when_e, loc) <- nested_fors {
        val fold (glob_el, for_cl_, idx_pat) = (glob_el, [], PatAny(loc)) for (p, idxp, e) <- ppe_list {
            val (p_, p_e) = plist2exp(p :: [], get_pat_loc(p))
            val p = p_.hd()
            match (idxp, idx_pat) {
            | (PatAny _, idx_pat) => (p_e :: glob_el, (p, e) :: for_cl_, idx_pat)
            | (_, PatAny _) =>
                val (idxp, idxp_e) = plist2exp(idxp :: [], get_pat_loc(idxp))
                (idxp_e :: p_e :: glob_el, (p, e) :: for_cl_, idxp.hd())
            | _ => throw ParseError(get_pat_loc(idxp), "@ is used more than once, which does not make sence and is not supported")
            }
        }
        (glob_el, (for_cl_.rev(), idx_pat, when_e, loc) :: nested_fors)
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
    | ForMakeArray | ForMakeList | ForMakeTuple | ForMakeVector =>
        val fold pel_i_l=[], glob_when_e=ExpNop(noloc), loc = noloc
            for (pe_l, idxp, when_e, loc) <- nested_fors {
                val glob_when_e =
                    match (glob_when_e, when_e) {
                    | (_, None) => glob_when_e
                    | (ExpNop(_), Some(e)) => e
                    | (_, Some(e)) =>
                        ExpBinary(OpLogicAnd, glob_when_e, e,
                            (TypBool, get_exp_loc(glob_when_e)))
                    }
                ((pe_l, idxp) :: pel_i_l, glob_when_e, loc)
            }
        val body = match glob_when_e {
            | ExpNop _ => body
            | _ =>
                match for_make {
                | ForMakeArray | ForMakeTuple =>
                    throw parse_err(ts, "'when' cannot be used inside array or tuple comprehensions")
                | _ => {}
                }
                val loc = get_exp_loc(glob_when_e)
                val check_e = ExpIf(glob_when_e, ExpNop(loc), ExpContinue(loc), (TypVoid, loc))
                expseq2exp([check_e, body ], loc)
            }
        ExpMap(pel_i_l, body, default_for_flags().{
            for_flag_make=for_make,
            for_flag_parallel=is_parallel,
            for_flag_unzip=need_unzip},
            make_new_ctx(loc))
    | _ =>
        val nfors = nested_fors.length()
        fold e = body for (pe_l, idxp, when_e, loc)@i <- nested_fors {
            val nested = i < nfors-1
            val flags = default_for_flags()
            val flags = if nested {flags.{for_flag_nested=true}}
                else {flags.{for_flag_parallel=is_parallel}}
            val body = match when_e {
                | Some(when_e) =>
                    val loc = get_exp_loc(when_e)
                    val check_e = ExpIf(when_e, ExpNop(loc), ExpContinue(loc), (TypVoid, loc))
                    expseq2exp([check_e, e ], loc)
                | _ => e
                }
            ExpFor(pe_l, idxp, body, flags, loc)
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
                | (EQUAL, l1) :: (DATA(kind), l2) :: (LITERAL(LitString fname), _) :: rest =>
                    val fname =
                        try Filename.locate(fname, parser_ctx.inc_dirs)
                        catch {
                        | NotFoundError => throw ParseError(l2, f"file {fname} is not found")
                        }
                    val e = ExpData(kind, fname, make_new_ctx(l1))
                    val dv = DefVal(p, e, flags, l1)
                    extend_defvals_(rest, true, dv :: result)
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
    | BACKSLASH _  => fname_op_rdiv()
    | SLASH  => fname_op_div()
    | PERCENT  => fname_op_mod()
    | POWER  => fname_op_pow()
    | DOT_PLUS _ => fname_op_dot_add()
    | DOT_MINUS _ => fname_op_dot_sub()
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
    | AUG_BINOP(OpAdd)  => fname_op_aug_add()
    | AUG_BINOP(OpSub)  => fname_op_aug_sub()
    | AUG_BINOP(OpMul)  => fname_op_aug_mul()
    | AUG_BINOP(OpDiv)  => fname_op_aug_div()
    | AUG_BINOP(OpMod)  => fname_op_aug_mod()
    | AUG_BINOP(OpBitwiseAnd)  => fname_op_aug_bit_and()
    | AUG_BINOP(OpBitwiseOr)  => fname_op_aug_bit_or()
    | AUG_BINOP(OpBitwiseXor)  => fname_op_aug_bit_xor()
    | AUG_BINOP(OpDotMul)  => fname_op_aug_dot_mul()
    | AUG_BINOP(OpDotDiv)  => fname_op_aug_dot_div()
    | AUG_BINOP(OpDotMod)  => fname_op_aug_dot_mod()
    | AUG_BINOP(OpShiftLeft)  => fname_op_aug_shl()
    | AUG_BINOP(OpShiftRight)  => fname_op_aug_shr()
    | SPACESHIP  => fname_op_cmp()
    | SAME    => fname_op_same()
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
    var vts = ts, class_id = noid, fname = noid, loc = noloc

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
        | (FUN, l1) :: (IDENT(_, i), _) :: _ =>
            val (ts, f) = parse_dot_ident(vts.tl(), false, "")
            val dot_pos = f.rfind('.')
            val (class_id_, fname_) =
                if dot_pos >= 0 {(get_id(f[:dot_pos]), get_id(f[dot_pos+1:]))}
                else {(noid, get_id(f))}
            vts = match ts {
                | (LPAREN(_), _) :: rest => rest
                | _ => throw parse_err(ts, "'(' is expected after function name")
                }
            class_id = class_id_; fname = fname_; loc = l1; break
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
            fun_flag_method_of=class_id,
            fun_flag_have_keywords=have_keywords}, loc)
}

fun parse_complex_exp(ts: tklist_t): (tklist_t, exp_t)
{
    match ts {
    | (IF, l1) :: rest =>
        fun parse_if_(ts: tklist_t, loc: loc_t): (tklist_t, exp_t)
        {
            val (ts, c) = parse_exp(ts, allow_mkrecord=false)
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
            val ll = loclist2loc([eloc, fe_loc ], eloc)
            val tmp = gen_id(parser_ctx.m_idx, "v")
            val def_tmp = DefVal(PatIdent(tmp, eloc), e, default_tempval_flags(), l1)
            val try_block = (def_tmp :: exp2expseq(final_e)) + (make_ident(tmp, l1) :: [])
            val try_block = expseq2exp(try_block, l1)
            val some_exn = gen_id(parser_ctx.m_idx, "e")
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
        val (ts, e) = parse_exp(rest, allow_mkrecord=false)
        val (ts, cases) = parse_match_cases(ts)
        (ts, ExpMatch(e, cases, make_new_ctx(l1)))
    | (FOLD, l1) :: rest =>
        val (ts, p, e) = parse_fold_init_(rest, false, [], [])
        val (ts, for_exp, for_iter_exp) = parse_for(ts, ForMakeNone)
        val fold_exp = transform_fold_exp("", p, e, for_exp, for_iter_exp, l1)
        (ts, fold_exp)
    | (FUN, _) :: _ => parse_lambda(ts)
    | _ =>
        parse_exp(ts, allow_mkrecord=true)
    }
}

fun parse_lambda(ts: tklist_t): (tklist_t, exp_t)
{
    val (ts, loc) = match ts {
        | (FUN, l1) :: (LPAREN _, _) :: rest => (rest, l1)
        | _ => throw parse_err(ts, "lambda function (starting with 'fun (') is expected")
    }
    val (ts, params, rt, prologue, have_keywords) = parse_fun_params(ts)
    val fname = dup_id(parser_ctx.m_idx, std__lambda__)
    val fname_exp = make_ident(fname, loc)
    val (ts, df) = parse_body_and_make_fun(ts, fname, params, rt, prologue,
        default_fun_flags().{fun_flag_private=true, fun_flag_have_keywords=have_keywords}, loc)
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

type kw_param_t = (id_t, typ_t, exp_t, loc_t)

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
                | (EQUAL, _) :: rest =>
                    val (ts, v0) = parse_exp(rest, allow_mkrecord=true)
                    (ts, v0)
                | (_, l1) :: _ => (ts, ExpNop(l1))
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
        val recarg = std__kwargs__
        val relems = [for (i, t, v0, _) <- kw_params { (default_arg_flags(), i, t, v0) } ]
        val rectyp = TypRecord(ref (relems, true))
        val (_, _, _, loc) = kw_params.hd()
        val recpat = PatRecord(None, [for (i, _, _, loci) <- kw_params {(i, PatIdent(i, loci))} ], loc)
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
        | (CCODE, _) :: _ =>
            val (ts, body) = parse_ccode_exp(ts, make_new_typ())
            (ts, params, body, fflags.{fun_flag_ccode = true})
        | (EQUAL, _) :: rest =>
            val (ts, body) = parse_stmt(ts.tl())
            (ts, params, body, fflags)
        | (LBRACE, l1) :: (BAR, _) :: _ =>
            val (ts, cases) = parse_match_cases(ts)
            val (params, match_arg) = plist2exp(params, l1)
            val match_e = ExpMatch(match_arg, cases, make_new_ctx(l1))
            (ts, params, match_e, fflags)
        | (LBRACE, _) :: _ =>
            val (ts, body) = parse_block(ts)
            (ts, params, body, fflags)
        | _ =>
            throw parse_err(ts, "'=' or '{' is expected before the function body")
        }

    val body =
        if prologue == [] { body }
        else {
            val loc = get_exp_loc(body)
            expseq2exp(prologue + exp2expseq(body), loc)
        }
    val paramtyps = [for p <- params {
            match p {
            | PatTyped(_, t, _) => t
            | _ => make_new_typ()
            }
        }]
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
    | (RETURN(true), l1) :: rest =>
        val (ts, e) = parse_exp(rest, allow_mkrecord=true)
        (ts, ExpReturn(Some(e), l1))
    | (RETURN(false), l1) :: rest =>
        (rest, ExpReturn(None, l1))
    | (THROW, l1) :: rest =>
        val (ts, e) = parse_exp(rest, allow_mkrecord=true)
        (ts, ExpThrow(e, l1))
    | (WHILE(true), l1) :: rest =>
        val (ts, c) = parse_exp(rest, allow_mkrecord=false)
        val (ts, body) = parse_block(ts)
        (ts, ExpWhile(c, body, l1))
    | (DO, l1) :: rest =>
        val (ts, body) = parse_exp_or_block(rest)
        val (ts, c) = match ts {
            | (WHILE _, _) :: rest => parse_exp(rest, allow_mkrecord=false)
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
            (ts, make_binary(OpAugBinary(binop), e1, e2, l2))
        | (DOT_EQUAL, l2) :: (LBRACE, _) :: rest =>
            if !lvalue_e1 { throw parse_err(ts, "left-hand-side of the assignment is not an l-value") }
            val (ts, _, _, rec_init_elems) = parse_exp_list(ts, RBRACE,
                kw_mode=KwMust, allow_empty=true, stop_at_semicolon=false)
            (ts, ExpUpdateRecord(e1, rec_init_elems, make_new_ctx(l2)))
        | _ => (ts, e1)
        }
}

fun parse_pat_list(ts: tklist_t, expect_comma: bool,
                   result: pat_t list, simple: bool, rbrace: char): (tklist_t, pat_t list)
{
    //println(f"parse_pat_list (expect_comma={expect_comma}): {tok2str(ts)}\n")
    match (ts, expect_comma) {
    | ((COMMA, _) :: rest, _) =>
        if expect_comma { parse_pat_list(rest, false, result, simple, rbrace) }
        else { throw parse_err(ts, "extra ','?")}
    | ((RPAREN, _) :: rest, _) =>
        if rbrace != ')' { throw parse_err(ts, "mismatched closing brace; must be ')'") }
        if result == [] { throw parse_err(ts, "empty tuple pattern are not allowed") }
        (rest, result.rev())
    | ((RSQUARE, _) :: rest, _) =>
        if rbrace != ']' { throw parse_err(ts, "mismatched closing brace; must be ')'") }
        (rest, result) // return the reversed list, because we want to convert it to a sequence '::'
    | (_, true) =>
        throw parse_err(ts, "',' is expected")
    | _ =>
        val (ts, p) = parse_pat(ts, simple)
        parse_pat_list(ts, true, p :: result, simple, rbrace)
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
                val (ts, pl) = parse_pat_list(rest, false, [], simple, ')')
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
            val (ts, pl) = parse_pat_list(rest, false, [], simple, ')')
            (ts, match pl { | p :: [] => p | _ => PatTuple(pl, l1) })
        | (LSQUARE _, l1) :: rest =>
            if simple {throw parse_err(ts, "list pattern cannot be used here")}
            val (ts, pl) = parse_pat_list(rest, false, [], simple, ']')
            (ts, fold tail = PatLit(LitEmpty, get_pat_loc(pl.hd()))
                for p <- pl { PatCons(p, tail, loclist2loc([get_pat_loc(p), get_pat_loc(tail)], noloc)) })
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
        | ((DOUBLE_ARROW, _) :: _, true) | ((COMMA, _) :: _, true)
        | ((RPAREN, _) :: _, true) | ((RSQUARE, _) :: _, true) | ((RBRACE, _) :: _, true) =>
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
    | _ => throw parse_err(ts, "'{', followed by optional '|', is expected")
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
            | "vector" => TypVector(result)
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
        val tt = [for i <- 0:(n :> int) {t} ]
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
            val tt = [for i <- 0:(n :> int) {t} ]
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

type relem_t = (val_flags_t, id_t, typ_t, exp_t)

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
            | (IDENT(_, _), _) :: (COLON, _) :: _
            | (VAR, _) :: (IDENT(_, _), _) :: (COLON, _) :: _
            | (VAL, _) :: (IDENT(_, _), _) :: (COLON, _) :: _ =>
                val (ts, f, i, flags) = match ts {
                    | (IDENT(f, i), _) :: (COLON, _) :: rest =>
                        (rest, f, i, default_val_flags())
                    | (VAR, _) :: (IDENT(f, i), _) :: (COLON, _) :: rest =>
                        (rest, f, i, default_var_flags())
                    | (VAL, _) :: (IDENT(f, i), _) :: (COLON, _) :: rest =>
                        (rest, f, i, default_val_flags())
                    | _ => throw parse_err(ts, "unexpected token")
                    }
                if expect_semicolon && !f {
                    throw parse_err(ts, "';' or newline should be inserted between record elements")
                }
                val (ts, t) = parse_typespec(ts)
                val (ts, default_) = match ts {
                    | (EQUAL, _) :: rest =>
                        val (ts, v0) = parse_exp(rest, allow_mkrecord=true)
                        (ts, v0)
                    | (_, l1) :: _ => (ts, ExpNop(l1))
                }
                parse_relems_(ts, true, (flags, get_id(i), t, default_) :: result)
            | _ =>
                throw parse_err(ts, (if expect_semicolon {"';' or newline is expected"} else
                    {"identifier followed by ':' is expected"}))
            }
        val (ts, relems) = parse_relems_(rest, false, [])
        (ts, TypRecord(ref (relems, true)))
    | _ => parse_typespec(ts)
}

fun have_mutable(cases: (id_t, typ_t) list) =
    exists(for (_, t) <- cases {
        | (_, TypRecord(ref (relems, _))) =>
            exists(for (flags, _, _, _) <- relems {flags.val_flag_mutable})
        | _ => false
        })

fun parse_deftype(ts: tklist_t)
{
    val (ts, class_module) = match ts {
        | (CLASS, _) :: rest => (rest, parser_ctx.m_idx)
        | (TYPE, _) :: rest => (rest, 0)
        | _ => throw parse_err(ts, "'type' or 'class' is expected")
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
        | (IDENT(_, n), loc) :: rest => (rest, get_id(n), loc)
        | _ => throw parse_err(ts, "the type name is expected")
        }

    val (ts, ifaces) = match ts {
        | (COLON, _) :: rest =>
            val (ts, ifaces) = parse_ident_list(rest, false, true, [])
            (ts, [for i <- ifaces { (i, ([] : (id_t, id_t) list)) } ])
        | _ => (ts, [])
        }
    val ts = match ts {
        | (EQUAL, _) :: rest => rest
        | (LBRACE, _) :: _ => ts
        | _ => throw parse_err(ts, "'=' or '{' is expected")
        }

    //println(f"type definition body({tok2str(ts)})\n")
    match ts {
    | (LBRACE, _) :: _ =>
        val (ts, t) = parse_typespec_or_record(ts)
        val cases = (tname, t) :: []
        var hm = have_mutable(cases)
        val dvar = ref (defvariant_t {
            dvar_name = tname, dvar_templ_args=type_params,
            dvar_alias = make_new_typ(),
            dvar_flags = default_variant_flags().{
                var_flag_record=ifaces == [] && !hm,
                var_flag_class_from=class_module,
                var_flag_have_mutable=hm
                },
            dvar_cases = cases,
            dvar_ctors = [],
            dvar_ifaces = ifaces,
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
                var_flag_class_from=class_module,
                var_flag_have_mutable=have_mutable(cases)
                },
            dvar_cases = cases,
            dvar_ctors = [],
            dvar_ifaces = ifaces,
            dvar_templ_inst = ref [],
            dvar_scope = [],
            dvar_loc = loc
        })
        (ts, DefVariant(dvar))
    | _ =>
        if class_module > 0 { throw parse_err(ts, "type alias (i.e. not a record nor variant) cannot be class") }
        if ifaces != [] { throw parse_err(ts, "type alias (i.e. not a record nor variant) cannot implement any interfaces") }
        val (ts, t) = parse_typespec(ts)
        val dt = ref (deftyp_t {
            dt_name=tname, dt_templ_args=type_params, dt_typ=t, dt_finalized=false,
            dt_scope=[], dt_loc=loc })
        (ts, DefTyp(dt))
    }
}

type iface_method_t = (id_t, typ_t, fun_flags_t)

fun parse_iface(ts: tklist_t, loc: loc_t)
{
    val (ts, iname) = match ts {
        | (IDENT(_, n), _) :: rest => (rest, get_id(n))
        | _ => throw parse_err(ts, "interface name (identifier) is expected")
        }
    val (ts, base_iface) = match ts {
        | (COLON, _) :: rest =>
            val (ts, base_iface) = parse_dot_ident(rest, false, "")
            (ts, get_id(base_iface))
        | _ => (ts, noid)
        }
    val ts = match ts {
        | (LBRACE, _) :: rest => rest
        | _ => throw parse_err(ts, "'{' is expected")
        }
    fun parse_iface_members_(ts: tklist_t, members: iface_method_t list): (tklist_t, iface_method_t list) =
        match ts {
        | (FUN, _) :: (IDENT(_, f), _) :: (LPAREN _, _) :: rest =>
            val (ts, params, rt, _, have_keywords) = parse_fun_params(rest)
            val paramtyps = [for p <- params {
                match p {
                | PatTyped(_, t, loc) => t
                | _ => throw parse_err(rest,
                    "all parameters of interface methods should be identifiers with types ('param: type'); more complex patterns are not allowed")
                }
            }]
            val fflags = default_fun_flags().{fun_flag_have_keywords = have_keywords}
            parse_iface_members_(ts, (get_id(f), TypFun(paramtyps, rt), fflags) :: members)
        | (RBRACE, _) :: rest => (rest, members.rev())
        | _ => throw parse_err(ts, "expected 'fun <ident> (...' or '}'")
        }
    val (ts, members) = parse_iface_members_(ts, [])
    val iface = ref (definterface_t {di_name=iname, di_base=base_iface, di_new_methods=members,
                                     di_all_methods=[], di_scope=[], di_loc=loc})
    (ts, DefInterface(iface))
}

type ppifstate_t =
    | PP_BR_TRUE // inside 'true' if/elif branch of conditional compilation
    | PP_BR_FALSE: bool // inside 'false' if/elif branch of conditional compilation:
                        // the argument tells if the true branch was already taken or not
    | PP_BR_ELSE: bool  // inside 'else' branch.
                        // the argument tells whether the 'else' branch is 'true' or not
type ppstack_t = (ppifstate_t, loc_t) list
type ppval_t = PP_INT: int64 | PP_BOOL: bool | PP_STRING: string
type ppenv_t = (string, ppval_t) Hashmap.t

fun preprocess(ts: tklist_t): tklist_t
{
    var env = Hashmap.empty(256, "", PP_INT(0L))

    fun pp_err(ts: tklist_t, msg: string) = parse_err(ts, msg)

    fun pp_match_paren((ts: tklist_t, x: ppval_t), ct: token_t, ol: loc_t): (tklist_t, ppval_t)
    {
        match ts {
        | (ct_, l) :: rest when ct_ == ct => (rest, x)
        | _ => throw pp_err(ts, f"'{tok2str(ct).1}' is expected; the opening paren is here {ol}")
        }
    }

    fun pp_atomic(ts: tklist_t, calc: bool): (tklist_t, ppval_t)
    {
        //println(f"pp_atomic @ {ts.hd().1}, calc={calc}\n")
        val defval = PP_BOOL(false)
        match ts {
        | (IDENT(_, "defined"), _) :: (LPAREN(false), _) :: (IDENT(_, n), _) :: (RPAREN, _) :: rest =>
            (rest, if calc {PP_BOOL(env.mem(n))} else {defval})
        | (IDENT(_, fname), _) :: (LPAREN(false), l1) :: rest =>
            // [TODO] currently only single-argument preprocessor intrinsic functions are supported
            val (ts, x) = pp_match_paren(pp_exp(rest, calc), RPAREN, l1)
            val x = if calc {
                match (fname, x) {
                | ("int", PP_BOOL(b)) => PP_INT(if b {1L} else {0L})
                | ("abs", PP_INT(i)) => PP_INT(if i >= 0L {i} else {-i})
                | ("string", PP_BOOL(b)) => PP_STRING(string(b))
                | ("string", PP_INT(i)) => PP_STRING(string(i))
                | _ => throw pp_err(ts, f"unknown/unsupported function {fname}")
                }
            } else {defval}
            (ts, x)
        | (IDENT(ne, i), _) :: rest =>
            val x = if calc {
                match env.find_opt(i) {
                | Some(x) => x
                | _ => throw pp_err(ts, f"identifier '{i}' is undefined")
                }
            } else {
                defval
            }
            (rest, x)
        | (LITERAL(lit), l1) :: rest =>
            (rest, (match lit {
            | LitInt(i) => PP_INT(i)
            | LitBool(b) => PP_BOOL(b)
            | LitString(s) => PP_STRING(s)
            | _ => throw pp_err(ts, f"preprocessor: unsupported literal (only integers, boolean values and strings are supported)")
            }))
        | (LPAREN(ne), l1) :: rest =>
            check_ne(ne, ts)
            pp_match_paren(pp_exp(rest, calc), RPAREN, l1)
        | (t, _) :: _ =>
            throw pp_err(ts, f"unxpected token '{tok2str(t).1}'. An identifier, literal or '(' is expected")
        | _ =>
            throw pp_err(ts, f"premature end of the stream; check the parens")
        }
    }

    fun pp_unary(ts: tklist_t, calc: bool): (tklist_t, ppval_t)
    {
        val defval = PP_BOOL(false)
        //println(f"pp_unary({tok2str(ts)})\n")
        match ts {
        | (MINUS(true), l1) :: rest =>
            val (ts, x) = pp_unary(rest, calc)
            val x = if calc {
                match x {
                | PP_INT(x) => PP_INT(-x)
                | _ => throw pp_err(ts, f"argument of unary '-' must be an integer")
                }
            } else {defval}
            (ts, x)
        | (PLUS(true), l1) :: rest =>
            val (ts, x) = pp_unary(rest, calc)
            val x = if calc {
                match x {
                | PP_INT(_) => x
                | _ => throw pp_err(ts, f"argument of unary '+' must be an integer")
                }
            } else {defval}
            (ts, x)
        | (TILDE, l1) :: rest =>
            val (ts, x) = pp_unary(rest, calc)
            val x = if calc {
                match x {
                | PP_INT(x) => PP_INT(~x)
                | _ => throw pp_err(ts, f"argument of unary '~' must be an integer")
                }
            } else {defval}
            (ts, x)
        | (LOGICAL_NOT, l1) :: rest =>
            val (ts, x) = pp_unary(rest, calc)
            val x = if calc {
                match x {
                | PP_BOOL(b) => PP_BOOL(!b)
                | _ => throw pp_err(ts, f"argument of unary '!' must be a boolean")
                }
            } else {defval}
            (ts, x)
        | _ =>
            pp_atomic(ts, calc)
        }
    }

    fun pp_binary(ts: tklist_t, calc: bool) : (tklist_t, ppval_t)
    {
        //println(f"pp_exp({tok2str(ts)})\n")
        fun pp_binary_(ts: tklist_t, calc: bool, result: ppval_t, min_prec: int) =
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
                | POWER => (OpPow, 230, AssocRight)
                | SHIFT_LEFT => (OpShiftLeft, 200, AssocLeft)
                | SHIFT_RIGHT => (OpShiftRight, 200, AssocLeft)
                | BITWISE_OR => (OpBitwiseOr, 130, AssocLeft)
                | BITWISE_AND => (OpBitwiseAnd, 150, AssocLeft)
                | BITWISE_XOR => (OpBitwiseXor, 140, AssocLeft)
                | SPACESHIP => (OpSpaceship, 170, AssocLeft)
                | _ => (OpAdd, -1, AssocLeft)
            }
            if prec < min_prec { (ts, result) }
            else {
                val next_min_prec = match assoc { | AssocLeft => prec+1 | _ => prec }
                val (ts, x) = pp_unary(rest, calc)
                // non-tail call, parse rhs
                val (ts, rhs) = pp_binary_(ts, calc, x, next_min_prec)
                val result = if calc {
                    match (bop, result, rhs) {
                    | (OpAdd, PP_INT(a), PP_INT(b)) => PP_INT(a + b)
                    | (OpAdd, PP_STRING(a), PP_STRING(b)) => PP_STRING(a + b)
                    | (OpSub, PP_INT(a), PP_INT(b)) => PP_INT(a - b)
                    | (OpMul, PP_INT(a), PP_INT(b)) => PP_INT(a * b)
                    | (OpDiv, PP_INT(a), PP_INT(b)) =>
                        if b == 0L {throw pp_err(ts, f"division by zero")}
                        PP_INT(a / b)
                    | (OpMod, PP_INT(a), PP_INT(b)) =>
                        if b == 0L {throw pp_err(ts, f"division by zero")}
                        PP_INT(a % b)
                    | (OpPow, PP_INT(a), PP_INT(b)) =>
                        if b < 0L {throw pp_err(ts, f"negative power")}
                        PP_INT(a ** b)
                    | (OpShiftLeft, PP_INT(a), PP_INT(b)) =>
                        PP_INT(a << b)
                    | (OpShiftRight, PP_INT(a), PP_INT(b)) =>
                        PP_INT(a >> b)
                    | (OpBitwiseOr, PP_INT(a), PP_INT(b)) =>
                        PP_INT(a | b)
                    | (OpBitwiseOr, PP_BOOL(a), PP_BOOL(b)) =>
                        PP_BOOL(a | b)
                    | (OpBitwiseXor, PP_INT(a), PP_INT(b)) =>
                        PP_INT(a ^ b)
                    | (OpBitwiseXor, PP_BOOL(a), PP_BOOL(b)) =>
                        PP_BOOL(a ^ b)
                    | (OpBitwiseAnd, PP_INT(a), PP_INT(b)) =>
                        PP_INT(a & b)
                    | (OpBitwiseAnd, PP_BOOL(a), PP_BOOL(b)) =>
                        PP_BOOL(a & b)
                    | (OpSpaceship, PP_INT(a), PP_INT(b)) =>
                        PP_INT(int64(a <=> b))
                    | (OpSpaceship, PP_BOOL(a), PP_BOOL(b)) =>
                        PP_INT(int64(a <=> b))
                    | (OpSpaceship, PP_STRING(a), PP_STRING(b)) =>
                        PP_INT(int64(a <=> b))
                    | _ => throw pp_err(ts, f"unsupported binary operation")
                    }
                } else {PP_BOOL(false)}
                pp_binary_(ts, calc, result, min_prec)
            }
        | _ => (ts, result)
        }
        val (ts, x) = pp_unary(ts, calc)
        pp_binary_(ts, calc, x, 0)
    }

    fun pp_extend_cmp(ts: tklist_t, calc: bool, result: bool, left: ppval_t): (tklist_t, ppval_t)
    {
        match ts {
        | (CMP(cmpop), l1) :: rest =>
            val (ts, right) = pp_binary(rest, calc)
            val result = if !calc {false} else {
                result & (match (cmpop, left, right) {
                    | (CmpEQ, PP_INT(a), PP_INT(b)) => a == b
                    | (CmpEQ, PP_BOOL(a), PP_BOOL(b)) => a == b
                    | (CmpEQ, PP_STRING(a), PP_STRING(b)) => a == b
                    | (CmpNE, PP_INT(a), PP_INT(b)) => a != b
                    | (CmpNE, PP_BOOL(a), PP_BOOL(b)) => a != b
                    | (CmpNE, PP_STRING(a), PP_STRING(b)) => a != b
                    | (CmpLT, PP_INT(a), PP_INT(b)) => a < b
                    | (CmpLT, PP_BOOL(a), PP_BOOL(b)) => a < b
                    | (CmpLT, PP_STRING(a), PP_STRING(b)) => a < b
                    | (CmpLE, PP_INT(a), PP_INT(b)) => a <= b
                    | (CmpLE, PP_BOOL(a), PP_BOOL(b)) => a <= b
                    | (CmpLE, PP_STRING(a), PP_STRING(b)) => a <= b
                    | (CmpGE, PP_INT(a), PP_INT(b)) => a >= b
                    | (CmpGE, PP_BOOL(a), PP_BOOL(b)) => a >= b
                    | (CmpGE, PP_STRING(a), PP_STRING(b)) => a >= b
                    | (CmpGT, PP_INT(a), PP_INT(b)) => a > b
                    | (CmpGT, PP_BOOL(a), PP_BOOL(b)) => a > b
                    | (CmpGT, PP_STRING(a), PP_STRING(b)) => a > b
                    | _ => throw pp_err(ts, f"unsupported comparison operation")
                })
            }
            pp_extend_cmp(ts, calc, result, right)
        | _ =>
            (ts, PP_BOOL(result))
        }
    }

    fun pp_chained_cmp(ts: tklist_t, calc: bool): (tklist_t, ppval_t)
    {
        val (ts, x) = pp_binary(ts, calc)
        match ts {
        | (CMP(cmpop), _) :: _ =>
            pp_extend_cmp(ts, calc, true, x)
        | _ => (ts, x)
        }
    }

    fun pp_logic(ts: tklist_t, calc: bool, result: ppval_t, min_prec: int): (tklist_t, ppval_t)
    {
        match ts {
        | (t, l) :: rest =>
            val (bop, prec, assoc) = match t {
                | LOGICAL_OR => (OpLogicOr, 10, AssocLeft)
                | LOGICAL_AND => (OpLogicAnd, 20, AssocLeft)
                | _ => (OpAdd, -1, AssocLeft)
            }
            if prec < min_prec { (ts, result) }
            else {
                match result {
                | PP_BOOL(_) => {}
                | _ => throw pp_err(ts, "arguments of || and && operations must be booleans")
                }
                val next_min_prec = match assoc { | AssocLeft => prec+1 | _ => prec }
                val (ts, x) = pp_chained_cmp(rest, calc)
                val calc_rhs =
                    match (bop, result) {
                    | (OpLogicOr, PP_BOOL(true)) => false
                    | (OpLogicAnd, PP_BOOL(false)) => false
                    | _ => calc
                    }
                // non-tail call, parse rhs
                val (ts, rhs) = pp_logic(ts, calc_rhs, x, next_min_prec)
                val result = if calc_rhs {
                    match (bop, result, rhs) {
                    | (OpLogicOr, PP_BOOL(a), PP_BOOL(b)) => PP_BOOL(a | b)
                    | (OpLogicAnd, PP_BOOL(a), PP_BOOL(b)) => PP_BOOL(a & b)
                    | _ => throw pp_err(ts, "arguments of || and && operations must be booleans")
                    }
                } else {result}
                pp_logic(ts, calc, result, min_prec)
            }
        | _ => (ts, result)
        }
    }

    fun pp_exp(ts: tklist_t, calc: bool): (tklist_t, ppval_t)
    {
        //println(f"pp_exp @ {ts.hd().1}, calc={calc}\n")
        val (ts, x) = pp_chained_cmp(ts, calc)
        pp_logic(ts, calc, x, 0)
    }

    fun pp_get_bool(x: ppval_t, ts: tklist_t): bool =
        match x {
        | PP_BOOL(x) => x
        | _ => throw pp_err(ts, "boolean value is expected here")
        }

    fun ppnext(ts: tklist_t, ppstack: ppstack_t, result: tklist_t): tklist_t
    {
        val process = match ppstack {
            | (PP_BR_FALSE(_), _) :: _ | (PP_BR_ELSE(false), _) :: _ => false
            | _ => true
        }
        val parent_process = match ppstack {
            | _ :: (PP_BR_FALSE(_), _) :: _ | _ :: (PP_BR_ELSE(false), _) :: _ => false
            | _ => true
        }
        match ts {
        | (PP_DEFINE, _) :: rest =>
            match rest {
            | (IDENT(_, n), _) :: rest =>
                val (ts, x) = pp_exp(rest, process)
                if process {
                    if env.mem(n) {throw pp_err(ts, f"symbol '{n}' is already defined")}
                    env.add(n, x)
                }
                ppnext(ts, ppstack, result)
            | _ => throw pp_err(ts, f"invalid syntax of the new preprocessor symbol definition. It should be '@DEFINE name expr'")
            }
        | (PP_UNDEF, _) :: rest =>
            match rest {
            | (IDENT(_, n), _) :: rest =>
                if process { env.remove(n) }
                ppnext(rest, ppstack, result)
            }
        | (PP_IFDEF, _) :: _
        | (PP_IFNDEF, _) :: _ =>
            val negate = match ts {(PP_IFNDEF, _) :: _ => true | _ => false}
            match ts.tl() {
            | (IDENT(_, n), _) :: rest =>
                val state =
                    if !process {PP_BR_FALSE(true)}
                    else if (env.mem(n) ^ negate) {PP_BR_TRUE}
                    else {PP_BR_FALSE(false)}
                ppnext(rest, (state, ts.hd().1) :: ppstack, result)
            | _ => throw pp_err(ts, f"invalid @IFDEF/@IFNDEF syntax: It should be '@IF[N]DEF name'")
            }
        | (PP_IF, _) :: _ =>
            val (ts, x) = pp_exp(ts.tl(), process)
            val state =
                    if !process {PP_BR_FALSE(true)}
                    else if pp_get_bool(x, ts.tl()) {PP_BR_TRUE}
                    else {PP_BR_FALSE(false)}
            ppnext(ts, (state, ts.hd().1) :: ppstack, result)
        | (PP_ELIF, _) :: _ =>
            val process_elif = parent_process &&
                (match ppstack {
                | (PP_BR_FALSE(false), _) :: _ => true
                | (PP_BR_ELSE(_), _) :: _ =>
                    throw pp_err(ts, f"@ELIF may not follow @ELSE (missing @ENDIF?)")
                | [] =>
                    throw pp_err(ts, f"@ELIF occurs without preceeding @IF")
                | _ => false
                })
            val (ts, x) = pp_exp(ts.tl(), process_elif)
            val state =
                    if !process_elif {PP_BR_FALSE(true)}
                    else if pp_get_bool(x, ts.tl()) {PP_BR_TRUE}
                    else {PP_BR_FALSE(false)}
            ppnext(ts, (state, ts.hd().1) :: ppstack.tl(), result)
        | (PP_ELSE, _) :: _ =>
            val process_else = parent_process &&
                (match ppstack {
                | (PP_BR_FALSE(false), _) :: _ => true
                | (PP_BR_ELSE(_), _) :: _ =>
                    throw pp_err(ts, f"@ELSE may not follow @ELSE (missing @ENDIF?)")
                | [] =>
                    throw pp_err(ts, f"@ELSE occurs without preceeding @IF")
                | _ => false
                })
            val state = PP_BR_ELSE(process_else)
            ppnext(ts.tl(), (state, ts.hd().1) :: ppstack.tl(), result)
        | (PP_ENDIF, _) :: _ =>
            match ppstack {
            | [] => throw pp_err(ts, f"@ENDIF occurs without the matching @IF")
            | _ => {}
            }
            ppnext(ts.tl(), ppstack.tl(), result)
        | (PP_ERROR, _) :: _ =>
            val (next_ts, x) = pp_exp(ts.tl(), process)
            if process {
                val msg = match x {
                    | PP_STRING(x) => x
                    | _ => "@ERROR argument must be a string"
                    }
                throw pp_err(ts, msg)
            }
            ppnext(next_ts, ppstack, result)
        | (PP_WARNING, _) :: _ =>
            val (next_ts, x) = pp_exp(ts.tl(), process)
            if process {
                val msg = match x {
                    | PP_STRING(x) => x
                    | _ => throw pp_err(ts, "@WARNING argument must be a string")
                    }
                println(f"{ts.hd().1}: warning: {msg}")
            }
            ppnext(next_ts, ppstack, result)
        | (AT, _) :: (LBRACE, l1) :: rest =>
            val (ts, x) = pp_match_paren(pp_exp(rest, process), RBRACE, l1)
            val result = if process {
                    val t = match x {
                        | PP_INT(x) => LITERAL(LitInt(x))
                        | PP_BOOL(x) => LITERAL(LitBool(x))
                        | PP_STRING(x) => LITERAL(LitString(x))
                        }
                    (t, l1) :: result
                } else { result }
            ppnext(ts, ppstack, result)
        | t :: rest =>
            val result = if process {t :: result} else {result}
            ppnext(rest, ppstack, result)
        | _ =>
            match ppstack {
            | (_, l) :: _ => throw pp_err(ts, f"@IF/ELIF/ELSE-block starting at {l} is not terminated by @ENDIF")
            | _ => {}
            }
            result.rev()
        }
    }
    ppnext(ts, [], [])
}

fun parse(m_idx: int, preamble: token_t list, inc_dirs: string list): bool
{
    var dm = all_modules[m_idx]
    val fname_id = get_id(dm.dm_filename)
    parser_ctx = parser_ctx_t {
        m_idx = dm.dm_idx,
        filename = dm.dm_filename,
        deps = [],
        inc_dirs = inc_dirs,
        default_loc = loc_t { m_idx=dm.dm_idx, line0=1, col0=1, line1=1, col1=1 }
    }

    // protect the module from repeated processing in the case of Lexer/Parser error
    dm.dm_parsed = true
    all_modules[m_idx].dm_parsed = true

    val strm = try Lxu.make_stream(dm.dm_filename)
        catch {
        | FileOpenError => throw ParseError(parser_ctx.default_loc, "cannot open file")
        | IOError => throw ParseError(parser_ctx.default_loc, "cannot read file")
        }
    val lexer = make_lexer(strm)

    // [TODO] perhaps, need to avoid fetching all the tokens at once
    var all_tokens: (Lexer.token_t, Ast.loc_t) list = []
    var prev_lineno = -1
    while true {
        val more_tokens = lexer()
        for (t, (lineno, col)) <- more_tokens {
            val loc = Ast.loc_t {m_idx=dm.dm_idx, line0=lineno, col0=col, line1=lineno, col1=col}
            if Options.opt.print_tokens {
                if lineno != prev_lineno {
                    print(f"\n{pp(fname_id)}:{lineno}: ")
                    prev_lineno = lineno
                }
                print(f"{Lexer.tok2str(t).0} ")
            }
            all_tokens = (t, loc) :: all_tokens
        }
        match all_tokens {
        | (Lexer.EOF, _) :: _ => break
        | _ => {}
        }
    }
    all_tokens = all_tokens.rev()
    for t <- preamble.rev() { all_tokens = (t, parser_ctx.default_loc) :: all_tokens }
    all_tokens = preprocess(all_tokens)
    dm.dm_defs = parse_expseq(all_tokens, true).1
    dm.dm_deps = parser_ctx.deps.rev()
    all_modules[m_idx] = dm
    true
}
