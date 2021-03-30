/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

//////// ficus abstract syntax definition + helper structures and functions ////////

from Ast import *
import PP, File

val margin = 120
val default_indent = 3

fun pplit(pp: PP.t, x: lit_t) = pp.str(lit2str(x))
fun ppid(pp: PP.t, x: id_t) = pp.str(
    match x {
    | IdName(0) => "<noid>"
    | _ => string(x)
    })

fun pprint_val_flags(pp: PP.t, flags: val_flags_t): void
{
    if flags.val_flag_tempref { pp.str("@tempref"); pp.space() }
    if flags.val_flag_temp { pp.str("@temp"); pp.space() }
    if flags.val_flag_private { pp.str("@private"); pp.space() }
    if flags.val_flag_subarray { pp.str("@subarray"); pp.space() }
    if flags.val_flag_global != [] { pp.str("@global"); pp.space() }
    if flags.val_flag_arg { pp.str("@arg"); pp.space() }
}

fun pprint_fun_flags(pp: PP.t, flags: fun_flags_t): void
{
    if flags.fun_flag_pure >= 0 {
        pp.str(if flags.fun_flag_pure > 0 {"@pure"} else {"@impure"})
        pp.space()
    }
    if flags.fun_flag_inline { pp.str("@inline"); pp.space() }
    if flags.fun_flag_nothrow { pp.str("@nothrow"); pp.space() }
    if flags.fun_flag_really_nothrow { pp.str("@really_nothrow"); pp.space() }
    if flags.fun_flag_private { pp.str("@private"); pp.space() }
    if flags.fun_flag_uses_fv { pp.str("@uses_fv"); pp.space() }
    if flags.fun_flag_recursive { pp.str("@recursive"); pp.space() }
    if flags.fun_flag_has_keywords { pp.str("@with_keywords"); pp.space() }
    if flags.fun_flag_ccode { pp.str("@cfunc"); pp.space() }
}

type typ_pr_t = TypPr0 | TypPrFun | TypPrComplex | TypPrBase

fun get_typ_pr(t: typ_t): typ_pr_t
{
    | TypVar (ref None) => TypPrBase
    | TypVar (ref Some(t1)) => get_typ_pr(t1)
    | TypInt | TypSInt _ | TypUInt _ | TypFloat _ | TypString | TypChar
    | TypBool | TypVoid | TypExn | TypErr | TypCPointer | TypDecl | TypModule =>
        TypPrBase
    | TypApp([], _) => TypPrBase
    | TypTuple _ | TypVarTuple _ => TypPrBase
    | TypRecord _ | TypVarRecord => TypPrBase
    | TypList _ | TypRef _ | TypArray(_, _) | TypVarArray _ | TypApp(_, _) => TypPrComplex
    | TypFun(_, _) => TypPrFun
}

fun opt_parens(p: typ_pr_t, p1: typ_pr_t) =
    if p1.__tag__ > p.__tag__ { ("(", ")") } else { ("", "") }

fun pprint_typ(pp: PP.t, t: typ_t, loc: loc_t)
{
    fun pptype_(t: typ_t, p1: typ_pr_t)
    {
        val prec = get_typ_pr(t)
        fun pptypsuf(t1: typ_t, suf: string)
        {
            val (lp, rp) = opt_parens(prec, p1)
            pp.begin(); pp.str(lp); pptype_(t1, prec)
            pp.str(rp); pp.str(" "); pp.str(suf); pp.end()
        }

        match t {
        | TypVar ((ref None) as r) => pp.str("<auto>")
        | TypVar (ref Some(t1)) => pptype_(t1, p1)
        | TypInt => pp.str("int")
        | TypSInt(b) => pp.str(f"int{b}")
        | TypUInt(b) => pp.str(f"uint{b}")
        | TypFloat(16) => pp.str("half")
        | TypFloat(32) => pp.str("float")
        | TypFloat(64) => pp.str("double")
        | TypFloat(b) => throw compile_err(loc, f"invalid type TypFloat({b})")
        | TypString => pp.str("string")
        | TypChar => pp.str("char")
        | TypBool => pp.str("bool")
        | TypVoid => pp.str("void")
        | TypFun(tl, t2) =>
            pp.begin(); pp.str("(")
            match tl {
            | [] => pp.str("void")
            | t1 :: [] => pptype_(t1, prec)
            | _ => pptype_(TypTuple(tl), prec)
            }
            pp.space(); pp.str("->"); pp.space(); pptype_(t2, prec); pp.str(")")
            pp.end()
        | TypList(t1) => pptypsuf(t1, "list")
        | TypRef(t1) => pptypsuf(t1, "ref")
        | TypArray(d, t1) =>
            val shape = if d == 0 {"+"} else {','*(d-1)}
            pptypsuf(t1, f"[{shape}]")
        | TypVarArray(t1) => pptypsuf(t1, "[+]")
        | TypVarRecord => pp.str("{...}")
        | TypApp([], n) => ppid(pp, n)
        | TypApp(t1 :: [], n) => pptypsuf(t1, string(n))
        | TypApp(tl, n) => pptypsuf(TypTuple(tl), string(n))
        | TypTuple(tl) =>
            pp.str("("); pp.cut(); pp.begin()
            for t@i <- tl {
                if i > 0 {pp.str(","); pp.space()}
                pptype_(t, TypPr0)
            }
            pp.cut(); pp.end(); pp.str(")")
        | TypVarTuple(t_opt) =>
            pp.str("(")
            match t_opt {
            | Some(t) => pptype_(t, TypPr0); pp.space()
            | _ => {}
            }
            pp.str("...)")
        | TypRecord (ref (rec_elems, ordered)) =>
            pp.begin(); pp.str(if ordered {"{"} else {"@ordered {"}); pp.breaki();
            for (n, t, v0_opt)@i <- rec_elems {
                if i > 0 { pp.str(";"); pp.space() }
                ppid(pp, n); pp.str(":"); pp.space(); pptype_(t, TypPr0)
                match v0_opt {
                | Some(v0) => pp.str("="); pplit(pp, v0)
                | _ => {}
                }
            }
            pp.breaku(); pp.str("}"); pp.end()
        | TypExn => pp.str("exn")
        | TypErr => pp.str("err")
        | TypCPointer => pp.str("cptr")
        | TypDecl => pp.str("<decl>")
        | TypModule => pp.str("<module>")
        }
    }
    pptype_(t, TypPr0)
}


fun pprint_templ_args(pp: PP.t, tt: id_t list) =
match tt {
    | [] => {}
    | t :: [] => ppid(pp, t)
    | _ =>
        pp.str("(")
        for t@i <- tt {
            if i > 0 { pp.str(","); pp.space() }
            ppid(pp, t)
        }
        pp.str(")")
}

fun pprint_for_flags(pp: PP.t, flags: for_flags_t)
{
    if flags.for_flag_parallel {pp.str("@parallel"); pp.space() }
    if flags.for_flag_unzip { pp.str("@unzip"); pp.space() }
}

fun pprint_exp(pp: PP.t, e: exp_t): void
{
    fun ppcases(pe_l: (pat_t, exp_t) list) {
        pp.str("{"); pp.cut(); pp.begin()
        for (p, e) <- pe_l {
            pp.space(); pp.str("| "); pprint_pat(pp, p);
            pp.space(); pp.str("=>"); pp.space(); pprint_exp_as_seq(pp, e)
        }
        pp.cut(); pp.end(); pp.str("}")
    }

    fun ppexp(e: exp_t): void {
    | DefVal(p, e0, vflags, loc) =>
        pp.begin(); pprint_val_flags(pp, vflags)
        val ctor_id = vflags.val_flag_ctor
        pp.str(if vflags.val_flag_mutable {"var"} else {"val"})
        pp.space(); pprint_pat(pp, p);
        match p {
        | PatTyped(_, _, _) => {}
        | _ => pp.str(":"); pp.space(); pprint_typ(pp, get_exp_typ(e0), loc)
        }
        pp.str(" ="); pp.space()
        if ctor_id != noid { pp.str(f"@constructor({ctor_id})") }
        else { ppexp(e0) }
        pp.end()
    | DefFun df =>
        val {df_name, df_templ_args, df_args, df_typ, df_body, df_flags, df_loc} = *df
        val ctor_id = df_flags.fun_flag_ctor
        pp.begin(0); pp.begin(); pprint_fun_flags(pp, df_flags)
        match df_templ_args {
        | [] => {}
        | _ =>
            pp.begin(); pp.str("template<"); pprint_templ_args(pp, df_templ_args)
            pp.str(">"); pp.end(); pp.space()
        }
        pp.str("fun "); ppid(pp, df_name); pp.str("("); pp.cut();
        for p@i <- df_args {
            if i > 0 { pp.str(","); pp.space() }
            pprint_pat(pp, p)
        }
        pp.cut(); pp.str(")"); pp.str(":"); pp.space()
        pprint_typ(pp, df_typ, df_loc); pp.space(); pp.str("=");
        pp.end(); pp.space()
        if ctor_id != CtorNone { pp.str(ctor2str(ctor_id)) }
        else { pprint_exp_as_block(pp, df_body) }
        pp.end(); pp.newline()
    | DefExn (ref {dexn_name, dexn_typ, dexn_loc}) =>
        pp.begin(); pp.str("exception "); ppid(pp, dexn_name)
        match dexn_typ {
        | TypVoid => {}
        | _ => pp.str(":"); pp.space(); pprint_typ(pp, dexn_typ, dexn_loc)
        }
        pp.end()
    | DefTyp (ref {dt_name, dt_templ_args, dt_typ, dt_loc}) =>
        pp.begin();
        match dt_templ_args {
        | [] => {}
        | _ =>
            pp.str("@template <"); pprint_templ_args(pp, dt_templ_args);
            pp.str(">"); pp.space()
        }
        pp.str("type"); pp.space(); ppid(pp, dt_name); pp.str(" =")
        pp.space(); pprint_typ(pp, dt_typ, dt_loc); pp.end()
    | DefVariant (ref
        { dvar_name, dvar_templ_args, dvar_alias, dvar_cases,
          dvar_ctors, dvar_flags, dvar_templ_inst, dvar_loc }) =>
        pp.begin();
        pp.begin(); pp.str("/*")
        pprint_typ(pp, dvar_alias, dvar_loc)
        pp.str("*/"); pp.end()
        match dvar_templ_args {
        | [] => {}
        | _ =>
            pp.str("@template <"); pprint_templ_args(pp, dvar_templ_args);
            pp.str(">"); pp.space()
        }
        if dvar_flags.var_flag_record {
            pp.str("@record type")
        } else {
            pp.str("type")
        }
        pp.space(); ppid(pp, dvar_name);
        pp.str(" ="); pp.space()
        val ctors = if dvar_ctors != [] { dvar_ctors } else { [: for (n, t) <- dvar_cases {n} :] }
        for (_, t)@i <- dvar_cases, c <- ctors {
            pp.begin(); pp.str("| ");
            ppid(pp, c); pp.str(": "); pp.space();
            pprint_typ(pp, t, dvar_loc); pp.end(); pp.space()
        }
        pp.end(); pp.space()
        match *dvar_templ_inst {
        | [] => {}
        | _ =>
            pp.newline(); pp.str(f"/* {dvar_name} instances */"); pp.space();
            for inst_id@i <- *dvar_templ_inst {
                pp.opt_semi()
                match id_info(inst_id, dvar_loc) {
                | IdVariant(inst_kvar) => ppexp(DefVariant(inst_kvar))
                | _ => {}
                }
            }
        }
    | DirImport(ml, _) =>
        pp.begin(); pp.str("import"); pp.space()
        for (n1, n2)@i <- ml {
            if i > 0 { pp.str(","); pp.space() }
            ppid(pp, n1)
            if n1 != n2 { pp.str(" as "); ppid(pp, n2) }
        }
        pp.end()
    | DirImportFrom(m, nl, _) =>
        pp.begin(); pp.str("from "); ppid(pp, m); pp.space(); pp.str("import"); pp.space()
        match nl {
        | [] => pp.str("*")
        | _ => for n@i <- nl { if i > 0 { pp.str(","); pp.space() }; ppid(pp, n) }
        }
        pp.end()
    | DirPragma(prl, _) =>
        pp.begin(); pp.str("pragma"); pp.space()
        for p@i <- prl {
            if i > 0 { pp.str(","); pp.space() }
            pplit(pp, LitString(p))
        }
        pp.end()
    | ExpSeq(eseq, _) => pprint_expseq(pp, eseq, true)
    | _ =>
        pp.begin()
        match e {
        | ExpNop _ => pp.str("{}")
        | ExpBreak(f, _) =>
            pp.str(if f {"@fold break"} else {"break"})
        | ExpContinue _ => pp.str("continue")
        | ExpRange(e1_opt, e2_opt, e3_opt, _) =>
            pp.str("(")
            match e1_opt {
            | Some(e1) => ppexp(e1)
            | None => {}
            }
            pp.str(":"); pp.cut()
            match e2_opt {
            | Some(e2) => ppexp(e2)
            | None => {}
            }
            match e3_opt {
            | Some(e3) => pp.str(":"); pp.cut(); ppexp(e3)
            | None => {}
            }
            pp.str(")")
        | ExpLit(x, (_, loc)) => pplit(pp, x)
        | ExpIdent(n, (t, loc)) => pp.str("<"); pprint_typ(pp, t, loc); pp.str(">"); ppid(pp, n)
        | ExpBinary(o, e1, e2, _) =>
            pp.str("("); ppexp(e1);
            pp.str(f" {o}");
            val e2 = match o {
            | OpCons =>
                fun print_list_(e: exp_t): exp_t {
                    | ExpBinary(OpCons, e2, rest, _) =>
                        pp.space(); ppexp(e2); pp.str(" ::")
                        print_list_(rest)
                    | _ => e
                }
                print_list_(e2)
            | _ =>
                e2
            }
            pp.space(); ppexp(e2)
            pp.str(")")
        | ExpAssign(e1, e2, _) =>
            ppexp(e1); pp.space()
            pp.str("="); pp.space(); ppexp(e2);
        | ExpMem(e1, e2, _) =>
            ppexp(e1); pp.str("."); pp.cut(); ppexp(e2)
        | ExpUnary(o, e1, _) =>
            pp.str("("); pp.str(f"{o}");
            pp.space(); ppexp(e1); pp.str(")")
        | ExpIntrin(i, args, _) =>
            pp.str(string(i)); pp.str("(")
            for e@i <- args {
                if i > 0 { pp.str(","); pp.space() }
                ppexp(e)
            }
            pp.str(")")
        | ExpThrow(e1, _) =>
            pp.str("throw"); pp.space(); ppexp(e1)
        | ExpMkTuple(el, _) =>
            pp.str("(")
            for e@i <- el {
                if i > 0 { pp.str(","); pp.space() }
                ppexp(e)
            }
            match el {
            | e :: [] => pp.str(",")
            | _ => {}
            }
            pp.str(")")
        | ExpMkRecord(rn, relems, _) =>
            ppexp(rn)
            pp.str("{")
            pp.begin()
            for (n, v)@i <- relems {
                if i > 0 { pp.str(","); pp.space() }
                ppid(pp, n); pp.str("="); ppexp(v)
            }
            pp.end()
            pp.str("}")
        | ExpUpdateRecord(e, relems, _) =>
            ppexp(e); pp.str(".{")
            pp.begin()
            for (n, v)@i <- relems {
                if i > 0 { pp.str(","); pp.space() }
                ppid(pp, n); pp.str("="); ppexp(v)
            }
            pp.end(); pp.str("}")
        | ExpMkArray(arows, _) =>
            pp.str("[")
            for acols@i <- arows {
                if i > 0 { pp.str(";"); pp.space() }
                pp.begin()
                for a@i <- acols {
                    if i > 0 { pp.str(","); pp.space() }
                    ppexp(a)
                }
                pp.end()
            }
            pp.str("]")
        | ExpCall(f, args, _) =>
            ppexp(f); pp.str("(")
            for e@i <- args {
                if i > 0 { pp.str(","); pp.space() }
                ppexp(e)
            }
            pp.str(")")
        | ExpAt(a, border, interp, args, _) =>
            ppexp(a)
            pp.str(border2str(border, true))
            pp.str(interp2str(interp, true))
            pp.str("["); pp.begin()
            for e@i <- args {
                if i > 0 { pp.str(","); pp.space() }
                ppexp(e)
            }
            pp.end(); pp.str("]")
        | ExpIf(if_seq, if_then, if_else, _) =>
            pp.begin(); pp.begin(); pp.str("if "); ppexp(if_seq); pp.end(); pp.space();
            pprint_exp_as_block(pp, if_then); pp.space(); pp.str("else")
            pp.space(); pprint_exp_as_block(pp, if_else); pp.end()
        | ExpWhile(c, body, _) =>
            pp.begin(); pp.str("while ")
            ppexp(c); pp.space()
            pprint_exp_as_block(pp, body); pp.end()
        | ExpDoWhile(body, c, _) =>
            pp.begin(); pp.str("do")
            pp.space(); ppexp(body)
            pp.str("while (")
            pp.cut(); ppexp(c); pp.cut(); pp.str(")"); pp.end()
        | ExpFor(for_cl, idx_pat, for_body, flags, _) =>
            pp.begin(); pprint_for_flags(pp, flags); pp.str("for"); pp.space()
            for (p, e)@i <- for_cl {
                if i > 0 { pp.str(","); pp.space() }
                pp.begin()
                pprint_pat(pp, p)
                if i == 0 {
                    match idx_pat {
                    | PatAny _ => {}
                    | _ => pp.str("@")
                        pprint_pat(pp, idx_pat)
                    }
                }
                pp.str(" <- ")
                ppexp(e)
                pp.end()
            }
            pp.end(); pp.space(); pprint_exp_as_block(pp, for_body)
        | ExpMap(map_cl, map_body, flags, _) =>
            var (oparen, cparen) = match flags.for_flag_make {
            | ForMakeList => ("[:", ":]")
            | ForMakeTuple => ("(", ")")
            | _ => ("[", "]")
            }
            pp.begin(); pp.str(oparen); pp.str(" ");
            for (pe_l, idx_pat)@j <- map_cl {
                if j == 0 {pprint_for_flags(pp, flags)} else {pp.space()}
                pp.begin(); pp.str("for ");
                for (p, e)@i <- pe_l {
                    if i > 0 { pp.str(","); pp.space() }
                    pp.begin()
                    pprint_pat(pp, p)
                    if i == 0 {
                        match idx_pat {
                        | PatAny _ => {}
                        | _ => pp.str("@")
                            pprint_pat(pp, idx_pat)
                        }
                    }
                    pp.str(" <- ")
                    ppexp(e)
                    pp.end()
                }
                pp.end()
            }
            pp.end(); pp.space(); pprint_exp_as_block(pp, map_body)
            pp.str(cparen);
        | ExpMatch(e, pe_l, _) =>
            pp.begin(); pp.str("match"); pp.space()
            ppexp(e); pp.space();
            ppcases(pe_l); pp.end()
        | ExpTryCatch(e, pe_l, _) =>
            pp.str("try"); pp.space(); ppexp(e); pp.space()
            pp.str("catch"); ppcases(pe_l);
        | ExpCast(e, t, (_, loc)) =>
            pp.str("("); ppexp(e); pp.str(":>");
            pp.space(); pprint_typ(pp, t, loc); pp.str(")")
        | ExpTyped(e, t, (_, loc)) =>
            pp.str("("); ppexp(e); pp.str(":");
            pp.space(); pprint_typ(pp, t, loc); pp.str(")")
        | ExpCCode(s, _) =>
            pp.str("@ccode "); pp.space(); pp.str("\""); pp.str(s); pp.str("\"")
        | DefVal(_, _, _, _) | DefFun _ | DefExn _ | DefTyp _
        | DefVariant _ | DefInterface _
        | DirImport(_, _) | DirImportFrom(_, _, _) | DirPragma(_, _) | ExpSeq(_, _) => {}
        }
        pp.end()
    }
    ppexp(e)
}

fun pprint_exp_as_block(pp: PP.t, e: exp_t) =
match e {
    | ExpSeq(eseq, _) => pprint_expseq(pp, eseq, true)
    | _ => pprint_expseq(pp, e :: [], true)
}

fun pprint_exp_as_seq(pp: PP.t, e: exp_t) =
match e {
    | ExpSeq(eseq, _) => pprint_expseq(pp, eseq, false)
    | _ => pprint_exp(pp, e)
}

fun pprint_expseq(pp: PP.t, eseq: exp_t list, braces: bool): void
{
    pp.beginv()
    if braces { pp.str("{"); pp.space() }
    for e@i <- eseq {
        if i > 0 { pp.opt_semi() }
        pprint_exp(pp, e)
    }
    pp.breaku()
    if braces { pp.str("}") }
    pp.end()
}

fun pprint_pat(pp: PP.t, p: pat_t)
{
    fun pppat(p: pat_t) {
    | PatAny _ => pp.str("_")
    | PatAs(p, n, _) =>
        pp.begin(); pp.str("("); pppat(p);
        pp.str(" as"); pp.space(); ppid(pp, n); pp.str(")"); pp.end();
    | PatLit(c, loc) => pplit(pp, c)
    | PatCons(p1, p2, _) =>
        pp.begin(); pp.str("("); pppat(p1); pp.str(" ::");
        pp.space(); pppat(p2); pp.str(")"); pp.end()
    | PatIdent(n, _) => ppid(pp, n)
    | PatTuple(pl, _) =>
        pp.begin(); pp.str("(");
        for p@i <- pl {
            if i > 0 { pp.str(","); pp.space() }
            pppat(p)
        }
        pp.str(")")
        pp.end()
    | PatVariant(n, elems, loc) =>
        pp.begin(); ppid(pp, n); pppat(PatTuple(elems, loc)); pp.end();
    | PatRecord(n_opt, elems, loc) =>
        pp.begin(); match n_opt {
        | Some(n) => ppid(pp, n); pp.str(" ")
        | _ => {}
        }
        pp.str("{"); pp.breaki();
        for (n, p)@i <- elems {
            if i > 0 { pp.str(","); pp.space() }
            ppid(pp, n); pp.str("="); pppat(p)
        }
        pp.breaku(); pp.str("}")
        pp.end()
    | PatTyped(p, t, loc) =>
        pp.begin(); pppat(p); pp.str(":"); pp.space(); pprint_typ(pp, t, loc); pp.end()
    | PatRef(p, _) =>
        pp.begin(); pp.str("ref ("); pp.cut(); pppat(p); pp.cut(); pp.str(")"); pp.end()
    | PatWhen(p, e, _) =>
        pp.begin(); pppat(p); pp.space(); pp.str("when")
        pp.space(); pprint_exp(pp, e); pp.end()
    | PatAlt(pl, _) =>
        match pl {
        | p :: [] => pppat(p)
        | _ =>
            pp.beginv(0); pp.str("(");
            for p@i <- pl {
                if i > 0 {pp.space()}
                pp.str("| "); pppat(p)
            }
            pp.str(")"); pp.end()
        }
    }
    pppat(p)
}

fun pprint_mod(dm: defmodule_t ref)
{
    File.stdout.flush()
    val {dm_filename, dm_defs, dm_deps} = *dm
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.beginv()
    pp.cut()
    pp.begin(); pp.str(dm_filename)
    match dm_defs {
    | [] => {}
    | _ =>
        pp.str(": ")
        match dm_deps {
        | [] => pp.str("<no deps>")
        | _ =>
            for n@i <- dm_deps {
                if i > 0 { pp.str(","); pp.space() }
                ppid(pp, n)
            }
        }
    }
    pp.end()
    pp.cut()
    pp.str("---------------------------------------------------------")
    pp.cut()
    for e <- dm_defs {
        pprint_exp(pp, e)
        pp.opt_semi()
    }
    pp.end()
    pp.flush()
    File.stdout.flush()
}

fun pprint_top_x(top: exp_t list)
{
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.beginv()
    for e <- top {
        pprint_exp(pp, e)
        pp.newline()
    }
    pp.end()
    pp.flush()
    println()
    File.stdout.flush()
}

fun pprint_typ_x(t: typ_t, loc: loc_t): void {
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.begin(); pprint_typ(pp, t, loc); pp.end(); pp.flush()
    File.stdout.flush()
}

fun pprint_exp_x(e: exp_t): void {
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.begin(); pprint_exp(pp, e); pp.end(); pp.flush()
    File.stdout.flush()
}

fun pprint_pat_x(p: pat_t): void {
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.begin(); pprint_pat(pp, p); pp.end(); pp.flush()
    File.stdout.flush()
}
