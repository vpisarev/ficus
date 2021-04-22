/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    K-form pretty printer.
    Can be used to debug the K-normalization step,
    as well as all the further K-form transformations
*/

from Ast import *
from K_form import *
import PP, Ast_pp, File

val margin = 120
val default_indent = 3
val pp_ = Ast.pp

@private fun pp_id_(pp: PP.t, n: id_t, loc: loc_t) = pp.str(idk2str(n, loc))

@private fun get_ktyp_pr(t: ktyp_t): int {
    | KTypInt | KTypCInt | KTypSInt _ | KTypUInt _ | KTypFloat _
    | KTypString | KTypChar | KTypBool | KTypVoid | KTypExn
    | KTypErr | KTypCPointer | KTypModule | KTypName _
    | KTypTuple _ | KTypRecord _ => 3
    | KTypList _ | KTypRef _ | KTypArray _ | KTypVector _ => 2
    | KTypFun _ => 1
    }

@private fun opt_parens(p: int, p1: int): (string, string) =
    if p < p1 { ("(", ")") } else { ("", "") }

@private fun pp_ktyp_(pp: PP.t, t: ktyp_t, p1: int, loc: loc_t, ~detailed: bool=false)
{
    val prec = get_ktyp_pr(t)
    fun ppktyp_(t: ktyp_t, prec: int) = pp_ktyp_(pp, t, prec, loc, detailed=false)
    fun ppktypsuf(t1: ktyp_t, suf: string) {
        val (lp, rp) = opt_parens(prec, p1)
        pp.begin(); pp.str(lp); ppktyp_(t1, prec)
        pp.str(rp); pp.str(" "); pp.str(suf); pp.end()
    }

    fun ppktyplist_(prefix: string, args: ktyp_t list) {
        pp.str(prefix)
        pp.cut();
        pp.begin()
        for t@i <- args {
            if i != 0 { pp.str(","); pp.space() }
            ppktyp_(t, 0)
        }
        pp.end(); pp.cut(); pp.str(")")
    }

    match t {
    | KTypInt  => pp.str("int")
    | KTypCInt  => pp.str("int32")
    | KTypSInt b => pp.str(f"int{b}")
    | KTypUInt b => pp.str(f"uint{b}")
    | KTypFloat(16) => pp.str("half")
    | KTypFloat(32) => pp.str("float")
    | KTypFloat(64) => pp.str("double")
    | KTypFloat b => throw compile_err(loc, f"K_pp: invalid type TypFloat({b})")
    | KTypString  => pp.str("string")
    | KTypChar  => pp.str("char")
    | KTypBool  => pp.str("bool")
    | KTypVoid  => pp.str("void")
    | KTypFun (tl, t2) =>
        pp.begin(); pp.str("(")
        match tl {
        | [] => pp.str("void")
        | t1 :: [] => ppktyp_(t1, prec)
        | _ => ppktyp_(KTypTuple(tl), prec)
        }
        pp.space(); pp.str("->"); pp.space(); ppktyp_(t2, prec)
        pp.str(")"); pp.end()
    | KTypList t1 => ppktypsuf(t1, "list")
    | KTypVector t1 => ppktypsuf(t1, "vector")
    | KTypRef t1 => ppktypsuf(t1, "ref")
    | KTypArray (d, t1) => ppktypsuf(t1, "[" + ','*(d-1) + "]")
    | KTypName n => pp_id_(pp, n, loc)
    | KTypTuple tl => ppktyplist_("(", tl)
    | KTypRecord (rn, relems) =>
        pp.begin()
        pp_id_(pp, rn, loc)
        if detailed {
            pp.str(" {")
            for (ni, ti)@i <- relems {
                if i != 0 { pp.str(","); pp.space() }
                pp_id_(pp, ni, loc)
                pp.str(": "); ppktyp_(ti, 0)
            }
            pp.str("}")
        }
        pp.end()
    | KTypExn  => pp.str("exn")
    | KTypErr  => pp.str("<err>")
    | KTypCPointer  => pp.str("cptr")
    | KTypModule  => pp.str("module")
    }
}

@private fun pp_atom_(pp: PP.t, a: atom_t, loc: loc_t): void =
    match a {
    | AtomId n => pp_id_(pp, n, loc)
    | AtomLit lit => pp.str(klit2str(lit, false, loc))
    }

@private fun pp_dom_(pp: PP.t, r: dom_t, loc: loc_t): void =
    match r {
    | DomainElem a => pp_atom_(pp, a, loc)
    | DomainFast a => pp.str("<"); pp_atom_(pp, a, loc); pp.str(">")
    | DomainRange (i, j, k) =>
        match i {
        | AtomLit(KLitNil _) => {}
        | _ => pp_atom_(pp, i, loc)
        }
        pp.str(":")
        match j {
        | AtomLit(KLitNil _) => {}
        | _ => pp_atom_(pp, j, loc)
        }
        match k {
        | AtomLit(KLitNil _) => {}
        | AtomLit(KLitInt(1L)) => {}
        | _ => pp.str(":")
               pp_atom_(pp, k, loc)
        }
    }

@private fun pp_exp_(pp: PP.t, e: kexp_t): void
{
    val eloc = get_kexp_loc(e)
    fun ppktp(ktp: ktprops_t?) {
    | Some({ktp_complex}) =>
        pp.str(if ktp_complex { "@@complex" } else { "@@simple" }); pp.space()
    | _ => {}
    }

    fun pp_atom_(a: atom_t) = pp_atom_(pp, a, eloc)
    fun pp_ktyp_(t: ktyp_t) = pp_ktyp_(pp, t, 0, eloc)
    fun pp_ktyp_detailed_(t: ktyp_t) = pp_ktyp_(pp, t, 0, eloc, detailed=true)
    fun pp_dom_(r: dom_t) = pp_dom_(pp, r, eloc)
    fun pp_id_(i: id_t) = pp_id_(pp, i, eloc)
    fun pp_idtyp_(i: id_t, t: ktyp_t, ~detailed:bool) {
        pp_id_(pp, i, eloc)
        match t {
        | KTypVoid => {}
        | _ => pp.str(": "); pp_ktyp_(pp, t, 0, eloc, detailed=detailed)
        }
    }
    fun pp_exp_(e: kexp_t) = pp_exp_(pp, e)
    /*fun pp_exp_as_seq_(e: kexp_t) {
    | KExpSeq (eseq, _) =>
        for e@i <- eseq { if i > 0 { pp.opt_semi() }; pp_exp(pp, e) }
    | _ => pp_exp(pp, e)
    }*/
    fun pp_exp_as_block_(e: kexp_t, ~flow: bool = false, ~closing: bool=true)
    {
        val e = match e {
            | KExpCCode(s, _) => pp.str("@ccode "); KExpAtom(AtomLit(KLitString(s)), get_kexp_ctx(e))
            | _ => e
        }
        val eseq = match e { | KExpSeq(eseq, _) => eseq | _ => e :: [] }
        if flow { pp.str(" {"); pp.end(); pp.breaki(); pp.beginv(0) }
        else { pp.str("{"); pp.beginv(); pp.space() };
        for e@i <- eseq { if i > 0 { pp.opt_semi() }; pp_exp_(pp, e) }
        pp.end(); pp.space()
        if closing { pp.begin(); pp.str("}"); pp.end() }
    }
    fun ppatoms_(al: atom_t list) =
        for a@i <- al { if i > 0 {pp.str(","); pp.space() }; pp_atom_(a) }
    fun pp_for_hdr_(pre_exp: kexp_t, for_cl: (id_t, dom_t) list, at_ids: id_t list): void
    {
        pp.begin(); pp.str("for "); pp.cut()
        match pre_exp {
        | KExpNop _ => {}
        | _ => pp_exp_(pre_exp); pp.str(";"); pp.space()
        }
        for (n, dom)@i <- for_cl {
            if i != 0 { pp.str(","); pp.space() }
            pp_id_(n);
            if i == 0 {
                match at_ids {
                | [] => {}
                | a :: [] => pp.str("@"); pp_id_(a)
                | _ =>
                    pp.str("@"); pp.str("(");
                    for idx@i <- at_ids {
                        if i > 0 { pp.str(","); pp.space() }
                        pp_id_(idx)
                    }
                    pp.str(")")
                }
            }
            pp.str(" <-")
            pp.space(); pp_dom_(dom)
        }
        pp.end()
    }

    match e {
    | KDefVal (n, e0, loc) =>
        pp.beginv()
        pp.begin()
        val {kv_typ, kv_flags} = get_kval(n, loc)
        Ast_pp.pprint_val_flags(pp, kv_flags)
        if kv_flags.val_flag_mutable {pp.str("var ")} else {pp.str("val ")}
        pp_idtyp_(n, kv_typ, detailed=false)
        pp.str(" ="); pp.end(); pp.space(); pp_exp_(e0)
        pp.end()
    | KDefFun (ref {kf_name, kf_params, kf_rt, kf_body, kf_closure, kf_flags, kf_loc}) =>
        val {kci_arg, kci_fcv_t} = kf_closure
        val nargs = kf_params.length()
        val ctor_id = kf_flags.fun_flag_ctor
        pp.beginv(0)
        pp.begin()
        Ast_pp.pprint_fun_flags(pp, kf_flags)
        pp.str("fun "); pp_id_(kf_name); pp.str("(")
        pp.cut(); pp.begin()
        for n@i <- kf_params {
            if i > 0 { pp.str(","); pp.space() }
            val {kv_typ=t} = get_kval(n, kf_loc)
            pp_idtyp_(n, t, detailed=false)
        }
        if kci_arg != noid {
            if nargs > 0 { pp.str(";"); pp.space() }
            pp_idtyp_(kci_arg, KTypName(kci_fcv_t), detailed=false)
        }
        pp.end(); pp.cut(); pp.str("):")
        pp.space(); pp_ktyp_(kf_rt)
        match ctor_id {
        | CtorNone => pp.end(); pp.space(); pp_exp_as_block_(kf_body)
        | _ => pp.str(" ="); pp.space(); pp.str(ctor2str(ctor_id)); pp.end()
        }
        pp.end(); pp.newline()
    | KDefExn (ref {ke_name, ke_typ, ke_loc}) =>
        pp.str("exception "); pp_idtyp_(ke_name, ke_typ, detailed=true)
    | KDefTyp (ref {kt_name, kt_typ, kt_props, kt_loc}) =>
        pp.begin(); ppktp(kt_props); pp.str("type ")
        pp_id_(kt_name); pp.str(" ="); pp.space(); pp_ktyp_detailed_(kt_typ)
        pp.end(); pp.newline()
    | KDefVariant (ref {kvar_name, kvar_cases, kvar_props, kvar_ctors, kvar_ifaces, kvar_flags, kvar_loc}) =>
        val is_opt0 = kvar_flags.var_flag_opt
        val is_recursive = kvar_flags.var_flag_recursive
        pp.begin(); ppktp(kvar_props)
        if is_recursive { pp.str("@@recursive ") }
        if is_opt0 { pp.str("@@option_like ") }
        if !kvar_flags.var_flag_have_tag { pp.str("@@no_tag ") }
        if kvar_flags.var_flag_instance { pp.str("@@instance ") }
        pp.str("type"); pp.space()
        pp_id_(kvar_name);
        if kvar_ifaces != [] {
            pp.space(); pp.str("implements"); pp.space()
            pp.beginv(0)
            for (iface, impl)@i <- kvar_ifaces {
                if i > 0 { pp.str(","); pp.space() }
                pp.begin(); pp_id_(iface); pp.str(":{ ");
                for f@j <- impl {
                    if j > 0 { pp.str(","); pp.space() }
                    pp_id_(f)
                }
                pp.str(" }"); pp.end()
            }
            pp.end()
        }
        pp.str(" ="); pp.space()
        val ncases = kvar_cases.length()
        for (v, t)@i <- kvar_cases {
            pp.str("| "); pp_idtyp_(v, t, detailed=true)
            if i+1 < ncases { pp.space() }
        }
        pp.end(); pp.newline()
        /*match kvar_ctors {
        | [] => {}
        | _ =>
            pp.begin(); pp.str("Constructors: ")
            for c@i <- kvar_ctors {
                if i > 0 { pp.str(","); pp.space() }
                pp.str("<"); pp_id_(c); pp.str(": ");
                pp_idtyp_(c, get_idk_ktyp(c, kvar_loc), detailed=false)
                pp.str(">: ")
            }
            pp.end(); pp.newline()
        }*/
    | KDefInterface (ref {ki_name, ki_base, ki_all_methods, ki_scope, ki_loc}) =>
        pp.beginv()
        pp.str("interface "); pp_id_(ki_name)
        if ki_base != noid { pp.str(": "); pp_id_(ki_base) }
        val nmembers = ki_all_methods.length()
        pp.str(" {"); pp.newline();
        for (f, t)@i <- ki_all_methods {
            pp_idtyp_(f, t, detailed=false); pp.str(";");
            if i < nmembers - 1 {pp.space()}
        }
        pp.end(); pp.space(); pp.str("}")
        pp.newline()
    | KDefClosureVars (ref {kcv_name, kcv_freevars, kcv_loc}) =>
        pp.beginv(); pp.begin(); pp.str("closure_data "); pp_id_(kcv_name)
        pp.str(" = {"); pp.end(); pp.newline();
        val nfv = kcv_freevars.length()
        for (n, t)@i <- kcv_freevars {
            pp_idtyp_(n, t, detailed=false); pp.str(";");
            if i+1 < nfv { pp.space() }
        }
        pp.space(); pp.str("}")
        pp.end(); pp.newline()
    | KExpCCode (s, (_, loc)) =>
        pp.begin(); pp.str("@ccode ")
        pp_exp_(KExpAtom(AtomLit(KLitString(s)), (KTypString, loc)))
        pp.end(); pp.newline()
    | KExpData (kind, fname, _) =>
        pp.begin(); pp.str(f"@data({kind}) '{fname}'"); pp.end()
    | KExpSeq(_, _) =>
        pp.beginv(0); pp_exp_as_block_(e); pp.end()
    | KExpSync(n, e) => pp.begin(); pp.str("@sync "); pp_exp_(pp, e); pp.end()
    | KExpNop _ => pp.str("{}")
    | KExpBreak _ => pp.str("break")
    | KExpContinue _ => pp.str("continue")
    | KExpAtom (a, _) => pp_atom_(a)
    | KExpBinary (o, a, b, _) =>
        pp.begin(); val ostr = f" {o}"
        pp_atom_(a); pp.str(ostr); pp.space(); pp_atom_(b); pp.end()
    | KExpAssign (n, a, _) =>
        pp.begin(); pp_id_(n); pp.str(" ="); pp.space(); pp_atom_(a); pp.end()
    | KExpMem (n, i, (_, loc)) =>
        pp.begin();
        pp_id_(n); pp.str(".")
        match get_idk_ktyp(n, loc) {
        | KTypRecord (rn, relems) =>
            val (ni, _) = relems.nth(i)
            pp.str(Ast.pp(ni))
        | _ => pp.str(string(i))
        }; pp.end()
    | KExpUnary(OpDeref, AtomId n, (_, loc)) => pp.begin(); pp.str("*"); pp_id_(n); pp.end()
    | KExpUnary(OpMkRef , a, _) =>
        pp.begin(); pp.str("ref "); pp_atom_(a); pp.end()
    | KExpUnary(o, a, _) =>
        pp.begin(); val ostr = string(o); pp.str(ostr); pp_atom_(a); pp.end()
    | KExpIntrin(iop, args, _) =>
        pp.begin(); pp.str(string(iop)); pp.str("(")
        for a@i <- args { if i > 0 { pp.str(","); pp.space() }; pp_atom_(a) }
        pp.str(")"); pp.end()
    | KExpThrow(n, f, _) =>
        pp.begin(); pp.str(if f { "rethrow " } else { "throw " }); pp_id_(n); pp.end()
    | KExpMkTuple (al, _) =>
        pp.begin(); pp.str("("); pp.cut();
        ppatoms_(al); pp.cut(); pp.str(")"); pp.end()
    | KExpMkRecord (al, (t, loc)) =>
        pp.beginv();
        val (rn, relems) =
            match t {
            | KTypRecord (rn, relems) => (rn, relems)
            | KTypName n =>
                match kinfo_(n, loc) {
                | KTyp (ref {kt_name, kt_typ=KTypRecord (_, rec_elems)}) => (kt_name, rec_elems)
                | KVariant (ref {kvar_cases=(rn, KTypRecord(_, rec_elems)) :: []}) => (rn, rec_elems)
                | _ => throw compile_err(loc, f"KPP: invalid record type '{KTypName(n)}' in KExpMkRecord(...)")
                }
            | _ => throw compile_err(loc, f"KPP: invalid record type '{t}' in KExpMkRecord(...)")
            }
        pp.str("__record__ "); if rn != noid { pp_id_(rn); pp.str(" ") }
        pp.str("{"); pp.newline()
        for a@i <- al, (n, t) <- relems {
            if i > 0 { pp.str(","); pp.space() }
            pp_id_(n); pp.str(" = "); pp_atom_(a)
        }
        pp.breaku(); pp.str("}"); pp.end()
    | KExpMkClosure(make_fp, f, args, (_, loc)) =>
        pp.begin();
        pp.str("__make_closure__ "); pp_id_(make_fp);
        pp.str("{"); pp.breaki()
        pp.str("__fp__ = "); pp_id_(f);
        if make_fp != noid {
            val (fvars, _) = get_closure_freevars(f, loc)
            for (n, _) <- fvars, a <- args {
                pp.str(","); pp.space()
                pp_id_(n); pp.str(" = "); pp_atom_(a)
            }
        }
        pp.breaku(); pp.str("}"); pp.end()
    | KExpMkArray (elems, (_, l)) =>
        pp.beginv();
        pp.str("[|"); pp.space();
        val nrows = elems.length()
        for arow@i <- elems {
            pp.begin()
            for (f, a)@j <- arow {
                if j > 0 { pp.str(","); pp.space() }
                if f { pp.str("\\") }; pp_atom_(a)
            }
            pp.end();
            if i+1 < nrows { pp.str(";"); pp.space() }
        }
        pp.end(); pp.space(); pp.str("|]")
    | KExpMkVector (elems, (_, l)) =>
        pp.begin();
        pp.str("["); pp.space();
        for (f, a)@j <- elems {
            if j > 0 { pp.str(","); pp.space() }
            if f { pp.str("\\") }; pp_atom_(a)
        }
        pp.end(); pp.space(); pp.str("]")
    | KExpCall (f, args, (_, loc)) =>
        pp.begin(); pp_id_(f); pp.str("("); pp.cut();
        ppatoms_(args); pp.cut(); pp.str(")"); pp.end()
    | KExpICall (obj, idx, args, (_, loc)) =>
        pp.begin(); pp_id_(obj); pp.str(".");
        val obj_typ = get_idk_ktyp(obj, loc)
        val mname = match get_kinterface_opt(obj_typ, loc) {
            | Some(iface) => iface->ki_all_methods.nth(idx).0
            | _ => throw compile_err(loc, "object used in method call is not a valid interface")
            }
        pp.str(pp_(mname)); pp.str("("); pp.cut();
        ppatoms_(args); pp.cut(); pp.str(")"); pp.end()
    | KExpAt (a, border, interp, args, _) =>
        pp.begin();
        pp_atom_(a)
        pp.str(border2str(border, true))
        pp.str(interp2str(interp, true))
        pp.str("[")
        for dom@i <- args { if i > 0 { pp.str(","); pp.space() }; pp_dom_(dom) }
        pp.str("]"); pp.end()
    | KExpIf _ =>
        pp.beginv(0);
        fun pp_if_chain(e: kexp_t, start: bool): void =
            match e {
            | KExpIf(if_c, then_e, else_e, _) =>
                if start { pp.begin(); pp.str("if "); } else { pp.str("else if ") }
                pp_exp_(if_c);
                val have_else = match else_e { | KExpNop _ => false | _ => true }
                pp_exp_as_block_(then_e, flow=true, closing=!have_else)
                if have_else {
                    pp.begin(); pp.str("} ")
                    pp_if_chain(else_e, false)
                }
            | _ => pp.str("else"); pp_exp_as_block_(e, flow=true, closing=true)
            }
        pp_if_chain(e, true); pp.end()
    | KExpWhile(c, body, _) =>
        pp.beginv(0); pp.begin(); pp.str("while "); pp_exp_(c);
        pp_exp_as_block_(body, flow=true); pp.end()
    | KExpDoWhile (body, c, _) =>
        pp.beginv(0);
        pp.begin(); pp.str("do"); pp_exp_as_block_(body, flow=true, closing=false);
        pp.begin(); pp.begin(); pp.str("} while "); pp_exp_(c); pp.end();
        pp.end()
    | KExpFor (for_cl, at_ids, for_body, flags, loc) =>
        pp.beginv(0);
        pp.begin(); Ast_pp.pprint_for_flags(pp, flags);
        pp_for_hdr_(KExpNop(loc), for_cl, at_ids);
        pp_exp_as_block_(for_body, flow=true)
        pp.end()
    | KExpMap (map_cl, map_body, flags, (_, loc)) =>
        pp.beginv(0);
        Ast_pp.pprint_for_flags(pp, flags)
        val (begin, end) =
            match flags.for_flag_make {
            | ForMakeList => ("[:", ":]")
            | ForMakeArray => ("[|", "|]")
            | ForMakeVector => ("[", "]")
            | ForMakeTuple => ("(", ")")
            | _ => throw compile_err(loc, f"unsupported type of comprehension '{flags.for_flag_make}'")
            }
        pp.begin();
        pp.str(begin)
        for (pre_e, pe_l, at_ids) <- map_cl {
            pp.space(); pp_for_hdr_(pre_e, pe_l, at_ids)
        }
        pp_exp_as_block_(map_body, flow=true)
        pp.str(end); pp.end()
    | KExpMatch (cases, _) =>
        pp.beginv(0);
        for (checks_i, e_i)@i <- cases {
            pp.begin()
            pp.str(if i == 0 { "if " }
                    else if checks_i == [] { "else" }
                    else { "else if "})
            for cj@j <- checks_i {
                if j > 0 { pp.str(" &&"); pp.space() }
                pp_exp_(cj)
            }
            pp_exp_as_block_(e_i, flow=true)
            pp.space()
        }
        pp.end()
    | KExpTryCatch (e1, e2, _) =>
        pp.beginv(0)
        pp.begin(); pp.str("try");
        pp_exp_as_block_(e1, flow=true, closing=false);
        pp.begin(); pp.str("} catch");
        pp_exp_as_block_(e2, flow=true, closing=true);
        pp.end();
    | KExpCast(a, t, loc) =>
        pp.begin(); pp.str("("); pp_atom_(a); pp.str(" :> ");
        pp_ktyp_(t); pp.str(")"); pp.end()
    }
}

@private fun pp_exp_seq(pp: PP.t, eseq: kexp_t list, braces: bool)
{
    if braces { pp.str("{") }
    pp.beginv()
    for e@i <- eseq {
        if i > 0 { pp.str(";"); pp.space() }; pp_exp_(pp, e)
    }
    pp.end()
    if braces { pp.str("}") }
}

fun pp_atom(a: atom_t, loc: loc_t)
{
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.begin(); pp_atom_(pp, a, loc); pp.end(); pp.flush()
    File.stdout.flush()
}

fun pp_ktyp(t: ktyp_t, loc: loc_t)
{
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.begin(); pp_ktyp_(pp, t, 0, loc); pp.end(); pp.flush()
    File.stdout.flush()
}

fun pp_kexp(e: kexp_t) {
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.begin(); pp_exp_(pp, e); pp.end(); pp.flush()
    File.stdout.flush()
}

fun pp_top(title: string, code: kexp_t list) {
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(margin, default_indent=default_indent)
    pp.beginv(0)
    match title {
    | "" => {}
    | _ => pp.str(title); pp.newline()
    }
    for e@i <- code {
        if i > 0 { pp.opt_semi() }
        pp_exp_(pp, e)
    }
    pp.end(); pp.flush();
    println(); File.stdout.flush()
}

fun pp_kmods(kmods: kmodule_t list) =
    for {km_cname, km_top}@i <- kmods {
        pp_top(f"\n///////// module {km_cname}: {km_top.length()} expressions //////////", km_top)
    }

fun pp_top_to_string(code: kexp_t list) {
    val pp = PP.pprint_to_string_list(margin, default_indent=default_indent)
    pp.beginv(0)
    for e@i <- code {
        if i != 0 { pp.break0() }
        pp_exp_(pp, e)
    }
    pp.newline(); pp.end(); pp.flush()
    val all_lines = pp.get_f()
    join_embrace("", "\n", "\n", all_lines)
}
