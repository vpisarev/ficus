/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    C-form pretty printer.

    Unlike Ast_pp or K_pp, the output of this module
    is not just free-form.

    It should output valid, and yet preferably
    well-formatted beautifully-looking C code.
*/

from Ast import *
from K_form import *
from C_form import *
import Options
import File, PP, Sys

val pp_ = Ast.pp

val default_indent = 3
val ccode_margin = 128

type assoc_t = AssocLeft | AssocRight

@private fun binop2str_(bop: cbinary_t)
{
    | COpArrayElem => ("", 1400, AssocLeft)
    | COpMul => ("*", 1200, AssocLeft)
    | COpDiv => ("/", 1200, AssocLeft)
    | COpMod => ("%", 1200, AssocLeft)
    | COpAdd => ("+", 1100, AssocLeft)
    | COpSub => ("-", 1100, AssocLeft)
    | COpShiftLeft => ("<<", 1000, AssocLeft)
    | COpShiftRight => (">>", 1000, AssocLeft)
    | COpCmp(CmpLT) => ("<", 900, AssocLeft)
    | COpCmp(CmpLE) => ("<=", 900, AssocLeft)
    | COpCmp(CmpGT) => (">", 900, AssocLeft)
    | COpCmp(CmpGE) => (">=", 900, AssocLeft)
    | COpCmp(CmpEQ) => ("==", 800, AssocLeft)
    | COpCmp(CmpNE) => ("!=", 800, AssocLeft)
    | COpBitwiseAnd => ("&", 700, AssocLeft)
    | COpBitwiseXor => ("^", 600, AssocLeft)
    | COpBitwiseOr => ("|", 500, AssocLeft)
    | COpLogicAnd => ("&&", 400, AssocLeft)
    | COpLogicOr => ("||", 300, AssocLeft)
    | COpAssign => ("=", 100, AssocRight)
    | COpAugAdd => ("+=", 100, AssocRight)
    | COpAugSub => ("-=", 100, AssocRight)
    | COpAugMul => ("*=", 100, AssocRight)
    | COpAugDiv => ("/=", 100, AssocRight)
    | COpAugMod => ("%=", 100, AssocRight)
    | COpAugSHL => ("<<=", 100, AssocRight)
    | COpAugSHR => (">>=", 100, AssocRight)
    | COpAugBitwiseAnd => ("&=", 100, AssocRight)
    | COpAugBitwiseOr => ("|=", 100, AssocRight)
    | COpAugBitwiseXor => ("^=", 100, AssocRight)
}

@private fun unop2str_(uop: cunary_t)
{
    | COpPlus => ("+", 1300, AssocRight)
    | COpNegate => ("-", 1300, AssocRight)
    | COpBitwiseNot => ("~", 1300, AssocRight)
    | COpLogicNot => ("!", 1300, AssocRight)
    | COpDeref => ("*", 1300, AssocRight)
    | COpGetAddr => ("&", 1300, AssocRight)
    | COpPrefixInc => ("++", 1300, AssocRight)
    | COpPrefixDec => ("--", 1300, AssocRight)
    | COpSuffixInc => ("++", 1400, AssocLeft)
    | COpSuffixDec => ("--", 1400, AssocLeft)
}

@private fun pp_id(pp: PP.t, n: id_t, loc: loc_t) = pp.str(idc2str(n, loc))

@private fun pp_ctyp__(pp: PP.t, prefix0: string, suffix0: string, t: ctyp_t, id_opt: id_t?, fwd_mode: bool, loc: loc_t)
{
    fun pr_id_opt_(add_space: bool) =
        match id_opt {
        | Some i =>
            if add_space { pp.space() }
            pp_id(pp, i, loc)
        | _ => {}
        }
    fun pr_id_opt() = pr_id_opt_(true)

    fun pr_struct(prefix: string, n_opt: id_t?,
                 elems: (id_t, ctyp_t) list, suffix: string)
    {
        pp.str(prefix + " ")
        match n_opt {
        | Some n => pp_id(pp, n, loc); pp.str(" ")
        | _ => {}
        }
        pp.str("{")
        for (ni, ti) <- elems {
            pp.newline()
            val need_nested_box =
                match ti {
                | CTypStruct _ | CTypUnion _ | CTypRawPtr (_, CTypStruct _) => false
                | _ => true }
            if need_nested_box { pp.begin() }
            pp_ctyp__(pp, "", ";", ti, Some(ni), true, loc)
            if need_nested_box { pp.end() }
        }
        pp.newlineu()
        pp.str("} " + suffix); pr_id_opt_(false)
    }

    pp.begin()
    match t {
    | CTypInt | CTypCInt | CTypSize_t | CTypSInt _ | CTypUInt _ | CTypFloat _
    | CTypString | CTypUniChar | CTypBool | CTypExn | CTypCSmartPtr
    | CTypArray _ | CTypVector _ =>
        pp.str(ctyp2str_(t, loc))
        pr_id_opt()
    | CTypVoid =>
        pp.str("void")
        match id_opt {
        | Some i => throw compile_err(loc, f"c_pp.ml: void cannot be used with id '{idc2str(i, loc)}'")
        | _ => {}
        }
    | CTypFunRawPtr (args, rt) =>
        pp.begin()
        pp_ctyp__(pp, "", "", rt, None, true, loc)
        pp.space(); pp.str("(*"); pr_id_opt_(false)
        pp.str(")("); pp.cut()
        pp.begin()
        match args {
        | [] => pp.str("void")
        | t :: [] =>
            pp_ctyp__(pp, "", "", t, None, true, loc)
        | _ =>
            val nargs = args.length()
            for ti@i <- args {
                val last = i == nargs-1
                pp_ctyp__(pp, "", if last {""} else {","}, ti, None, true, loc)
                if !last { pp.space() }
            }
        }
        pp.end()
        pp.str(")"); pp.end()
    | CTypStruct (n_opt, selems) => pr_struct(prefix0 + "struct", n_opt, selems, "")
    | CTypRawPtr ([], CTypStruct (n_opt, selems)) =>
        val suffix =
            match n_opt {
            | Some n => idc2str(n, loc) + ", *"
            | _ => "*"
            }
        pr_struct(prefix0 + "struct", n_opt, selems, suffix)
    | CTypUnion (n_opt, uelems) => pr_struct(prefix0 + "union", n_opt, uelems, "")
    | CTypRawPtr (attrs, t) =>
        pp.begin()
        if attrs.mem(CTypStatic) { pp.str("static ") }
        if attrs.mem(CTypVolatile) { pp.str("volatile ") }
        pp_ctyp__(pp, "", "", t, None, fwd_mode, loc)
        pp.str("*")
        pr_id_opt()
        pp.end()
    | CTypRawArray (attrs, et) =>
        pp.begin()
        if attrs.mem(CTypStatic) { pp.str("static ") }
        if attrs.mem(CTypVolatile) { pp.str("volatile ") }
        if attrs.mem(CTypConst) { pp.str("const ") }
        pp_ctyp__(pp, "", "", et, None, fwd_mode, loc)
        pp.space()
        pr_id_opt_(false)
        pp.str("[]")
        pp.end()
    | CTypName n =>
        match (fwd_mode, n) {
        | (false, _) => pp_id(pp, n, loc)
        | (true, IdName _) => pp_id(pp, n, loc)
        | _ =>
            match cinfo_(n, loc) {
            | CTyp (ref {ct_typ=CTypRawPtr (_, CTypStruct (Some struct_id, _))}) =>
                pp.str("struct "); pp_id(pp, struct_id, loc); pp.str("*")
            | CTyp (ref {ct_typ=CTypStruct (Some struct_id, _)}) =>
                pp.str("struct "); pp_id(pp, struct_id, loc)
            | CInterface (ref {ci_cname}) =>
                pp.str(f"struct {ci_cname}")
            | _ =>
                if prefix0 != "" { pp.str(prefix0); pp.space() }
                pp_id(pp, n, loc)
            }
        }
        pr_id_opt()
    | CTypLabel => pp.str("/*<label>*/"); pr_id_opt()
    | CTypAny => pp.str("void"); pr_id_opt()
    }
    pp.str(suffix0)
    pp.end()
}

@private fun pp_ctyp_(pp: PP.t, t: ctyp_t, id_opt: id_t?, loc: loc_t) = pp_ctyp__(pp, "", "", t, id_opt, false, loc)

@private fun embed_text(pp: PP.t, fname: string, loc: loc_t)
{
    try {
        val text = File.read_utf8(fname)
        val lines = text.split('\n', allow_empty=true)
        pp.beginv(0)
        pp.str("FX_MAKE_STR("); pp.space()
        val nlines = lines.length()
        for l@j <- lines {
            pp.str((if j > 0 {"U"} else {""}) +
                    (if j < nlines-1 {l+'\n'} else {l}).escaped(quotes=true))
            pp.newline()
        }
        pp.str(")"); pp.end()
    } catch {
    | _ => throw compile_err(loc, f"@text: {fname} cannot be read")
    }
}

@private fun embed_data(pp: PP.t, kind: string, fname: string, elemtyp: ctyp_t, loc: loc_t)
{
    pp.str("{"); pp.begin(0);
    for v@i <- [| 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 |] {
        if i > 0 { pp.str(","); pp.space() }
        pp.str(f"{v}")
    }
    pp.end(); pp.space(); pp.str("}")
}

@private fun pp_cexp_(pp: PP.t, e: cexp_t, pr: int) =
    match e {
    | CExpIdent(i, (_, loc)) => pp_id(pp, i, loc)
    | CExpLit(l, (_, loc)) =>
        val s = match l {
                | KLitNil _ => "0"
                | KLitChar c => f"(char_){ord(c)}"
                | _ => K_form.klit2str(l, true, loc)
                }
        pp.str(s)
    | CExpBinary(COpArrayElem as bop, a, b, _) =>
        val (_, pr0, _) = binop2str_(bop)
        pp.begin()
        pp_cexp_(pp, a, pr0)
        pp.str("["); pp.cut()
        pp_cexp_(pp, b, 0)
        pp.cut(); pp.str("]")
        pp.end()
    | CExpBinary (bop, a, b, _) =>
        val (bop_str, pr0, assoc) = binop2str_(bop)
        pp.begin()
        if pr0 < pr { pp.str("("); pp.cut() }
        val is_shift = bop == COpShiftLeft || bop == COpShiftRight
        val a_pr = if is_shift { 1350 } else if assoc == AssocLeft { pr0 } else { pr0 + 1 }
        val b_pr = if is_shift { 1350 } else if assoc == AssocRight { pr0 } else { pr0 + 1 }
        pp_cexp_(pp, a, a_pr); pp.space()
        pp.str(bop_str); pp.space()
        pp_cexp_(pp, b, b_pr)
        if pr0 < pr { pp.cut(); pp.str(")") }
        pp.end()
    | CExpUnary (uop, e, _) =>
        val (uop_str, pr0, _) = unop2str_(uop)
        pp.begin()
        if pr0 < pr { pp.str("("); pp.cut() }
        match uop {
        | COpSuffixInc | COpSuffixDec => pp_cexp_(pp, e, pr0)
                                         pp.str(uop_str)
        | _ => pp.str(uop_str)
               pp_cexp_(pp, e, pr0)
        }
        if pr0 < pr { pp.cut(); pp.str(")") }
        pp.end()
    | CExpMem (e, m, (_, loc)) =>
        pp_cexp_(pp, e, 1400); pp.str("."); pp_id(pp, m, loc)
    | CExpArrow (e, m, (_, loc)) =>
        pp_cexp_(pp, e, 1400); pp.str("->"); pp_id(pp, m, loc)
    | CExpCast (e, t, loc) =>
        pp.begin(); pp.str("("); pp_ctyp_(pp, t, None, loc)
        pp.str(")"); pp.cut(); pp_cexp_(pp, e, 1301); pp.end()
    | CExpTernary (e1, e2, e3, _) =>
        val pr0 = 200
        pp.begin()
        if pr0 < pr { pp.str("("); pp.cut() }
        pp_cexp_(pp, e1, 0); pp.space(); pp.str("?"); pp.space()
        pp_cexp_(pp, e2, 0); pp.space()
        pp.str(":"); pp.space()
        pp_cexp_(pp, e3, 0)
        if pr0 < pr { pp.cut(); pp.str(")") }
        pp.end()
    | CExpCall (f, args, _) =>
        pp.begin()
        pp_cexp_(pp, f, 1400); pp.str("("); pp.cut()
        pp_elist(pp, args); pp.str(")"); pp.end()
    | CExpInit (eseq, _) =>
        pp.begin(); pp.str("{");
        pp.space();
        if eseq != [] {
            for e@i <- eseq {
                if i > 0 { pp.str(","); pp.space() }
                pp_cexp_(pp, e, 0)
            }
        }
        pp.end(); pp.space(); pp.str("}")
    | CExpData (kind, fname, (t, loc)) =>
        match kind {
        | "text" => embed_text(pp, fname, loc)
        | "binary" | "binary_le" | "binary_be" =>
            val elemtyp = match t {
            | CTypArray(1, elemtyp) => elemtyp
            | _ => throw compile_err(get_cexp_loc(e),
                f"c_pp: invalid type '{ctyp2str(t, loc).0}' of embedded data array")
            }
            embed_data(pp, kind, fname, elemtyp, loc)
        | _ =>
            throw compile_err(get_cexp_loc(e),
                f"c_pp: unsupported kind {kind} of embedded data; must be 'text' or 'binary*'")
        }
    | CExpTyp (t, loc) =>
        pp.begin(); pp_ctyp_(pp, t, None, loc); pp.end()
    | CExpCCode (ccode, l) =>
        pp.begin(); pp.str("\n"+ccode.strip()+"\n"); pp.end()
    | CExpData (kind, fname, l) =>
        pp.begin();
    }

@private fun pp_elist(pp: PP.t, el: cexp_t list)
{
    for e@i <- el {
        if i > 0 { pp.str(","); pp.space() }
        pp_cexp_(pp, e, 0)
    }
}

@private fun pprint_fun_hdr(pp: PP.t, fname: id_t, semicolon: bool, loc: loc_t, fwd_mode: bool)
{
    val {cf_args, cf_rt, cf_cname, cf_flags, cf_loc} =
    match cinfo_(fname, loc) {
    | CFun cf => *cf
    | _ => throw compile_err(loc, f"the forward declaration of {idc2str(fname, loc)} does not reference a function")
    }
    pp.beginv()
    pp.begin()
    if cf_flags.fun_flag_private { pp.str("static ") }
    else { pp.str("FX_EXTERN_C ") }
    pp_ctyp_(pp, cf_rt, None, cf_loc)
    pp.space(); pp.str(cf_cname); pp.str("("); pp.end(); pp.cut()
    match cf_args {
    | [] => pp.str("void")
            pp.cut()
    | _ =>
        val nargs = cf_args.length()
        for (n, t, _)@i <- cf_args {
            val last = i == nargs - 1
            pp_ctyp__(pp, "", if last {""} else {","}, t, Some(n), true, cf_loc)
            if !last { pp.space() }
        }
    }
    pp.str(")" + (if semicolon { ";" } else { "" }))
    pp.end()
    pp.break0()
}

@private fun pprint_cstmt_or_block_cbox(pp: PP.t, s: cstmt_t)
{
    val sl = match s { | CStmtBlock (sl, _) => sl | CStmtNop _ => [] | _ => s :: [] }

    pp.str("{"); pp.newline(); pp.beginv(0)
    for s@i <- sl {
        if i > 0 { pp.break0() }
        pp_cstmt_(pp, s)
    }
    pp.end(); pp.end(); pp.break0(); pp.str("}")
}

@private fun pprint_cstmt_as_block(pp: PP.t, s: cstmt_t)
{
    val sl = match s { | CStmtBlock(sl, _) => sl | CStmtNop _ => [] | _ => s :: [] }
    match sl {
    | [] => pp.str("{}")
    | _ =>
        pp.beginv(); pp.str("{");
        for s@i <- sl {
            pp.newline(); pp_cstmt_(pp, s)
        }
        pp.end(); pp.break0(); pp.str("}")
    }
}

@private fun pp_cstmt_(pp: PP.t, s: cstmt_t) =
    match s {
    | CStmtNop _ => pp.str("{}")
    | CComment (s, _) => pp.str(s)
    | CExp e =>
        pp_cexp_(pp, e, 0)
        match e { | CExpCCode _ => {} | _ => pp.str(";") }
    | CStmtBreak _ => pp.str("break;")
    | CStmtContinue _ => pp.str("continue;")
    | CStmtReturn (e_opt, l) =>
        pp.begin(); pp.str("return")
        match e_opt {
        | Some e => pp.space(); pp_cexp_(pp, e, 0)
        | _ => {}
        }
        pp.str(";"); pp.end()
    | CStmtBlock _ =>
        pprint_cstmt_as_block(pp, s)
    | CStmtSync (n, s) =>
        if Options.opt.enable_openmp {
            pp.newline()
            pp.str("#pragma omp critical")
            if n != noid {
                val nstr = pp_(n).replace(".", "__")
                pp.str(f" ({nstr})")
            }
            pp.newline()
        }
        pprint_cstmt_as_block(pp, s)
    | CStmtIf (e, s1, s2, _) =>
        fun print_cascade_if(prefix: string, e: cexp_t, s1: cstmt_t, s2: cstmt_t)
        {
            pp.begin(); pp.str(prefix + " (")
            pp_cexp_(pp, e, 0); pp.str(")")
            pp.space(); pprint_cstmt_or_block_cbox(pp, s1)
            match s2 {
            | CStmtNop _ | CStmtBlock ([], _) => {}
            | CStmtIf (e_, s1_, s2_, _) =>
                pp.space(); print_cascade_if("else if", e_, s1_, s2_)
            | _ =>
                pp.space(); pp.begin(); pp.str("else")
                pp.space(); pprint_cstmt_or_block_cbox(pp, s2)
            }
        }
        print_cascade_if("if", e, s1, s2)
    | CStmtGoto (n, loc) =>
        pp.begin(); pp.str("goto")
        pp.space(); pp_id(pp, n, loc)
        pp.str(";"); pp.end()
    | CStmtLabel (n, loc) =>
        pp.breaku()
        pp_id(pp, n, loc)
        pp.str(": ;")
    | CStmtFor (t_opt, e1, e2_opt, e3, body, loc) =>
        pp.begin(); pp.str("for ("); pp.cut()
        match e1 {
        | [] => {}
        | _ =>  match t_opt {
                | Some t => pp_ctyp_(pp, t, None, loc); pp.space()
                | _ => {}
                }
                pp.begin(); pp_elist(pp, e1); pp.end()
        }
        pp.str(";")
        match e2_opt {
        | Some e2 => pp.space(); pp_cexp_(pp, e2, 0)
        | _ => {}
        }
        pp.str(";")
        match e3 {
        | [] => {}
        | _ => pp.space(); pp.begin(); pp_elist(pp, e3); pp.end()
        }
        pp.cut(); pp.str(")"); pp.space()
        pprint_cstmt_or_block_cbox(pp, body)
    | CStmtWhile (e, body, _) =>
        pp.begin(); pp.str("while (")
        pp.cut(); pp_cexp_(pp, e, 0)
        pp.cut(); pp.str(")"); pp.space()
        pprint_cstmt_or_block_cbox(pp, body)
    | CStmtDoWhile (body, e, _) =>
        pp.begin(); pp.str("do")
        pprint_cstmt_or_block_cbox(pp, body)
        pp.begin(); pp.str("while (")
        pp.cut(); pp_cexp_(pp, e, 0)
        pp.cut(); pp.str(");"); pp.end()
    | CStmtSwitch (e, cases, _) =>
        pp.begin(); pp.str("switch ("); pp.begin()
        pp_cexp_(pp, e, 0); pp.end(); pp.str(") {")
        pp.end(); pp.break0()
        for (labels, code) <- cases {
            pp.begin()
            val isdefault =
                match labels {
                | [] => pp.str("default:"); true
                | _ =>
                    for l <- labels {
                        pp.str("case "); pp_cexp_(pp, l, 0)
                        pp.str(":"); pp.space()
                    }
                    false
                }
            pp.end(); pp.break0(); pp.beginv()
            val codelen = code.length() + (if isdefault { 0 } else { 1 })
            for s@i <- code {
                if i == 0 { pp.str("   ") }
                pp_cstmt_(pp, s)
                if i < codelen - 1 { pp.break0() }
            }
            if isdefault { if code == [] { pp.str((' '*default_indent)+";") } }
            else { pp.str("break;") }
            pp.end()
            pp.break0()
        }
        pp.str("}")
    | CDefVal (t, n, e_opt, loc) =>
        val is_private = match cinfo_(n, loc) {
                         | CVal ({cv_flags}) => cv_flags.val_flag_private
                         | _ => false
                         }
        pp.begin()
        if is_private { pp.str("static"); pp.space() }
        pp_ctyp_(pp, t, Some(n), loc)
        match e_opt {
        | Some e => pp.str(" ="); pp.space(); pp_cexp_(pp, e, 0)
        | _ => {}
        }
        pp.str(";"); pp.end()
    | CDefFun cf =>
        val {cf_name, cf_body, cf_loc} = *cf
        pprint_fun_hdr(pp, cf_name, false, cf_loc, false)
        pp.beginv()
        pp.str("{"); pp.newline()
        for s@i <- cf_body { if i > 0 {pp.break0()}; pp_cstmt_(pp, s) }
        pp.end()
        pp.break0()
        pp.str("}")
        pp.newline()
    | CDefForwardSym (cf_name, cf_loc) =>
        match cinfo_(cf_name, cf_loc) {
        | CFun _ => pprint_fun_hdr(pp, cf_name, true, cf_loc, true)
        | CVal ({cv_typ}) =>
            pp.begin(); pp.str("FX_EXTERN_C_VAL(")
            pp.cut(); pp_ctyp__(pp, "", ")", cv_typ, Some(cf_name), true, cf_loc)
            pp.end()
        | _ =>
            throw compile_err(cf_loc,
                f"the forward declaration of {idc2str(cf_name, cf_loc)} does not reference a function or a value")
        }
    | CDefTyp ct =>
        val {ct_name, ct_typ, ct_loc} = *ct
        pp_ctyp__(pp, "typedef ", ";", ct_typ, Some(ct_name), true, ct_loc)
        pp.newline()
    | CDefForwardTyp (n, loc) =>
        pp.begin(); pp.str("struct "); pp_id(pp, n, loc); pp.str(";"); pp.end()
        pp.newline()
    | CDefEnum ce =>
        val {cenum_cname, cenum_members, cenum_loc} = *ce
        pp.str("typedef enum {")
        pp.break0(); pp.beginv()
        for (n, e_opt)@i <- cenum_members {
            if i == 0 { pp.str("   ") } else { pp.str(","); pp.space() }
            pp_id(pp, n, cenum_loc)
            match e_opt {
            | Some e => pp.str("="); pp_cexp_(pp, e, 0)
            | _ => {}
            }
        }
        pp.end(); pp.break0(); pp.str("} ")
        pp.str(cenum_cname); pp.str(";")
        pp.newline()
    | CDefInterface ci =>
        val {ci_cname, ci_vtbl, ci_loc} = *ci
        pp.beginv();
        pp.str(f"typedef struct {ci_cname} {"); pp.newline();
        val vtbl_cname = get_idc_cname(ci_vtbl, ci_loc)
        pp.str(vtbl_cname); pp.str("* vtbl;"); pp.newline();
        pp.str("fx_object_t* obj;");
        pp.end(); pp.newline()
        pp.str(f"}} {ci_cname};")
        pp.newline()
    | CMacroDef cm =>
        val {cm_cname, cm_args, cm_body, cm_loc} = *cm
        pp.str("#define "); pp.str(cm_cname)
        match cm_args {
        | [] => {}
        | _ =>  pp.str("(")
                for a@i <- cm_args {
                    if i > 0 { pp.str(", ") }
                    pp_id(pp, a, cm_loc)
                }
                pp.str(")")
        }
        match cm_body {
        | [] => {}
        | _ =>  for s <- cm_body {
                    pp.space(); pp.str("\\")
                    pp.break0(); pp.str("    ")
                    pp_cstmt_(pp, s)
                }
        }
    | CMacroUndef (n, loc) =>
        pp.begin(); pp.str("#undef "); pp_id(pp, n, loc); pp.end()
    | CMacroIf (cs_l, else_l, _) =>
        for (c, sl)@i <- cs_l {
            pp.break0(); pp.begin()
            pp.str(if i == 0 { "#if " } else { "#elif " })
            pp_cexp_(pp, c, 0); pp.end(); pp.break0()
            for s <- sl { pp_cstmt_(pp, s) }
        }
        match else_l {
        | [] => {}
        | _ => pp.break0(); pp.begin(); pp.str("#else")
               pp.end(); pp.break0()
               for s <- else_l { pp_cstmt_(pp, s) }
        }
        pp.break0(); pp.begin(); pp.str("#endif")
    | CMacroInclude (s, _) =>
        pp.break0(); pp.begin(); pp.str("#include ")
        pp.str(s); pp.end()
    | CMacroPragma (s, _) =>
        pp.break0(); pp.begin()
        pp.str("#pragma "); pp.str(s); pp.end()
    }

fun pp_ctyp(t: ctyp_t, loc: loc_t)
{
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(ccode_margin, default_indent=default_indent)
    pp_ctyp_(pp, t, None, loc)
    pp.flush()
    File.stdout.flush()
}

fun pp_cexp(e: cexp_t)
{
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(ccode_margin, default_indent=default_indent)
    pp_cexp_(pp, e, 0)
    pp.flush()
    File.stdout.flush()
}

fun pp_cstmt(s: cstmt_t)
{
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(ccode_margin, default_indent=default_indent)
    pp_cstmt_(pp, s)
    pp.flush()
    File.stdout.flush()
}

fun pprint_top(code: ccode_t)
{
    File.stdout.flush()
    val pp = PP.pprint_to_stdout(ccode_margin, default_indent=default_indent)
    pp.beginv(0)
    for s@i <- code {
        if i != 0 { pp.break0() }
        pp_cstmt_(pp, s)
    }
    pp.newline();
    pp.end(); pp.flush()
    File.stdout.flush()
}

fun pprint_top_to_string(code: ccode_t): string
{
    val pp = PP.pprint_to_string_list(ccode_margin, default_indent=default_indent)
    pp.beginv(0)
    for s@i <- code {
        if i != 0 { pp.break0() }
        pp_cstmt_(pp, s)
    }
    pp.newline(); pp.end(); pp.flush()
    val all_lines = pp.get_f()
    join_embrace("", "\n", "\n", all_lines)
}
