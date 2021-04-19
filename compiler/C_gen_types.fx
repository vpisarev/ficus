/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Generates C code for the data structures and the supporting
    functions (_fx_free_..., _fx_copy_..., (some of) _fx_make_... etc.).

    We traverse through the code; represent each complex type of K-form is a C structure.
    Earlier on (in k_mangle.ml), we replaced all the complex types with KTypName(...)
    and gave them unique names. Now need to convert those types to C,
    put all the definitions in the beginning of the generated C code.
    If a type definition depends on other types that are delcared later
    (e.g. in the case of mutually-recursive variants), insert forward declarations
    of those types ('struct _fx_V<dep_variant_name>;') and the forward declarations of
    their destructors.
*/

from Ast import *
from K_form import *
from C_form import *

import K_annotate, K_mangle
import Map, Set, Hashset

/* converts function arguments and the return type to ctyp_t.
   where needed, adds pointers. The return value is added to the argument list.
   For example,
   (int, float [,], string list) -> int
   is converted to
   (int, fx_arr_t*, _fx_LS, int*)
*/
fun ktyp2ctyp_fargs(args: ktyp_t list, rt: ktyp_t, loc: loc_t)
{
    val args_ =
    [: for kt <- args {
        val ctyp = ktyp2ctyp(kt, loc)
        val {ktp_pass_by_ref} = K_annotate.get_ktprops(kt, loc)
        val ptr_attr =
            match deref_ktyp(kt, loc) {
            | KTypArray (_, _) => []
            | _ => [: CTypConst :]
            }
        if ktp_pass_by_ref { CTypRawPtr(ptr_attr, ctyp) }
        else { ctyp }
    } :]
    val rct = if rt == KTypVoid { [] }
              else { [: CTypRawPtr([], ktyp2ctyp(rt, loc)) :] }
    args_ + (rct + [: std_CTypVoidPtr :])
}

fun ktyp2ctyp(t: ktyp_t, loc: loc_t) {
    fun type_err(tname: string) =
        compile_err(loc,
            f"ktyp2ctyp: {tname} is not supported here. " +
            f"It should have been converted to KTypName() at K_mangle stage")

    fun ktyp2ctyp_(t: ktyp_t) =
        match t {
        | KTypInt => CTypInt
        | KTypCInt => CTypCInt
        | KTypSInt n => CTypSInt(n)
        | KTypUInt n => CTypUInt(n)
        | KTypFloat n => CTypFloat(n)
        | KTypVoid => CTypVoid
        | KTypBool => CTypBool
        | KTypChar => CTypUniChar
        | KTypString => CTypString
        | KTypCPointer => CTypCSmartPtr
        | KTypFun (args, rt) =>
            val args_ = ktyp2ctyp_fargs(args, rt, loc)
            CTypFunRawPtr(args_, CTypCInt)
        | KTypTuple _ => throw type_err("KTypTuple")
        | KTypRecord _ => throw type_err("KTypRecord")
        | KTypName i => CTypName(i)
        | KTypArray (d, et) => CTypArray(d, ktyp2ctyp_(et))
        | KTypVector et => CTypVector(ktyp2ctyp_(et))
        | KTypList _ => throw type_err("KTypList")
        | KTypRef _ => throw type_err("KTypRef")
        | KTypExn => CTypExn
        | KTypErr => CTypVoid
        | KTypModule => CTypVoid
        }
    ktyp2ctyp_(t)
}

/* returns some basic information about particular ctyp_t */
fun get_ctprops(ctyp: ctyp_t, loc: loc_t): ctprops_t
{
    val ctp =
    match ctyp {
    | CTypInt | CTypCInt | CTypSize_t | CTypSInt _ | CTypUInt _
    | CTypFloat _ | CTypVoid | CTypBool | CTypUniChar =>
        ctprops_t { ctp_scalar=true, ctp_complex=false, ctp_make=[], ctp_free=(noid, noid),
                    ctp_copy=(noid, noid), ctp_pass_by_ref=false, ctp_ptr=false }
    | CTypCSmartPtr =>
        ctprops_t { ctp_scalar=false, ctp_complex=true, ctp_make=[],
            ctp_free=(noid, std_fx_free_cptr), ctp_copy=(noid, std_fx_copy_cptr),
            ctp_pass_by_ref=false, ctp_ptr=true }
    | CTypString =>
        ctprops_t { ctp_scalar=false, ctp_complex=true, ctp_make=[],
            ctp_free=(std_FX_FREE_STR, std_fx_free_str),
            ctp_copy=(noid, std_fx_copy_str),
            ctp_pass_by_ref=true, ctp_ptr=false }
    | CTypExn =>
        ctprops_t { ctp_scalar=false, ctp_complex=true, ctp_make=[],
            ctp_free=(noid, std_fx_free_exn), ctp_copy=(noid, std_fx_copy_exn),
            ctp_pass_by_ref=true, ctp_ptr=false
        }
    | CTypStruct (Some i, _) => get_ctprops(CTypName(i), loc)
    | CTypStruct _ => throw compile_err(loc, "there is no type properties for the anonymoous struct")
    | CTypUnion (Some i, _) => throw compile_err(loc, f"there is no type properties for union '{idk2str(i, loc)}'")
    | CTypUnion _ => throw compile_err(loc, "there is no type properties for the anonymoous union")
    | CTypFunRawPtr (_, _) =>
        ctprops_t { ctp_scalar=true, ctp_complex=false, ctp_make=[], ctp_free=(noid, noid),
                    ctp_copy=(noid, noid), ctp_pass_by_ref=false, ctp_ptr=true}
    | CTypRawPtr (_, t) =>
        ctprops_t { ctp_scalar=true, ctp_complex=false, ctp_make=[], ctp_free=(noid, noid),
                    ctp_copy=(noid, noid), ctp_pass_by_ref=false, ctp_ptr=true}
    | CTypRawArray (_, t) =>
        ctprops_t { ctp_scalar=false, ctp_complex=false, ctp_make=[], ctp_free=(noid, noid),
                    ctp_copy=(noid, noid), ctp_pass_by_ref=false, ctp_ptr=false}
    | CTypArray (_, _) =>
        ctprops_t { ctp_scalar=false, ctp_complex=true, ctp_make=[],
            ctp_free=(std_FX_FREE_ARR, std_fx_free_arr),
            ctp_copy=(noid, std_fx_copy_arr),
            ctp_pass_by_ref=true, ctp_ptr=false }
    | CTypVector (_) =>
        ctprops_t { ctp_scalar=false, ctp_complex=true, ctp_make=[],
            ctp_free=(noid, std_fx_free_vec),
            ctp_copy=(noid, std_fx_copy_vec),
            ctp_pass_by_ref=true, ctp_ptr=false }
    | CTypName i =>
        match cinfo_(i, loc) {
        | CTyp (ref {ct_props}) => ct_props
        | CInterface _ =>
            ctprops_t { ctp_scalar=false, ctp_complex=true, ctp_make=[],
            ctp_free=(std_FX_FREE_IFACE, std_fx_free_iface),
            ctp_copy=(std_FX_COPY_IFACE, std_fx_copy_iface),
            ctp_pass_by_ref=true, ctp_ptr=false }
        | _ => throw compile_err(loc, f"properties of non-type '{idk2str(i, loc)}' cannot be requested")
        }
    | CTypLabel => throw compile_err(loc, "properties of label cannot be requested")
    | CTypAny => throw compile_err(loc, "properties of 'any type' cannot be requested")
    }
    val {ctp_free=(free_m, free_f), ctp_copy=(copy_m, copy_f)} = ctp
    if free_m != noid && free_f == noid {
        throw compile_err(loc, "cgen: if free_m is non-empty then free_f must also be non-empty")
    }
    if copy_m != noid && copy_f == noid {
        throw compile_err(loc, "cgen: if copy_m is non-empty then copy_f must also be non-empty")
    }
    ctp
}

fun get_constructor(ctyp: ctyp_t, required: bool, loc: loc_t): id_t
{
    val {ctp_make} = get_ctprops(ctyp, loc)
    match ctp_make {
    | i :: [] => i
    | _ =>
        if !required { noid }
        else {
            throw compile_err(loc, f"cgen: missing type constructor for '{ctyp2str_(ctyp, loc)}'")
        }
    }
}

fun get_free_f(ctyp: ctyp_t, let_none: bool, let_macro: bool, loc: loc_t): (bool, cexp_t?)
{
    val {ctp_ptr, ctp_free=(free_m, free_f)} = get_ctprops(ctyp, loc)
    if free_m == noid && free_f == noid && let_none {
        (false, None)
    } else {
        val (use_if, i) =
            if let_macro && free_m != noid { (false, free_m) }
            else if free_f != noid { (ctp_ptr, free_f) }
            else { (false, std_FX_NOP) }
        (use_if, Some(make_id_exp(i, loc)))
    }
}

/* generates the copy/assignment expression.
   It's assumed that the destination is empty, i.e. not initialized.
   If it's initialized then the compiler should call the destructor for it first */
fun get_copy_f(ctyp: ctyp_t, let_none: bool, let_macro: bool, loc: loc_t)
{
    val {ctp_pass_by_ref, ctp_copy=(copy_m, copy_f)} = get_ctprops(ctyp, loc)
    (ctp_pass_by_ref,
    if copy_m == noid && copy_f == noid && let_none {
        None
    } else {
        val i =
            if copy_m != noid && let_macro { copy_m }
            else if copy_f != noid { copy_f }
            else if ctp_pass_by_ref { std_FX_COPY_SIMPLE_BY_PTR }
            else { std_FX_COPY_SIMPLE }
        Some(make_id_exp(i, loc))
    })
}

fun gen_copy_code(src_exp: cexp_t, dst_exp: cexp_t, ctyp: ctyp_t, code: ccode_t, loc: loc_t)
{
    val (pass_by_ref, copy_f_opt) = get_copy_f(ctyp, true, true, loc)
    val ctx = (CTypVoid, loc)
    val e =
    match (src_exp, copy_f_opt) {
    | (CExpLit (KLitNil _, _), Some(CExpIdent (f, _)))
        when f == std_FX_COPY_PTR => CExpBinary(COpAssign, dst_exp, src_exp, ctx)
    | (_, Some f) =>
        val src_exp = if pass_by_ref { cexp_get_addr(src_exp) }
                      else { src_exp }
        val dst_exp = cexp_get_addr(dst_exp)
        CExpCall(f, src_exp :: dst_exp :: [], ctx)
    /* in C the assignment operator returns the assigned value,
        but since in this particular case we are not going to chain
        assignment operators, we assume that it returns 'void' */
    | (_, _) => CExpBinary(COpAssign, dst_exp, src_exp, ctx)
    }
    CExp(e) :: code
}

/* generates the destructor call if needed */
fun gen_free_code(elem_exp: cexp_t, ctyp: ctyp_t, let_macro: bool,
                  use_if: bool, code: ccode_t, loc: loc_t)
{
    val (can_use_if, free_f_opt) = get_free_f(ctyp, true, let_macro, loc)
    val ctx = (CTypVoid, loc)
    val elem_exp_ptr = cexp_get_addr(elem_exp)
    match free_f_opt {
    | Some f =>
        val call_stmt = CExp(CExpCall(f, elem_exp_ptr :: [], ctx))
        val stmt =
            if can_use_if && use_if {
                CStmtIf(elem_exp, call_stmt, CStmtNop(loc), loc)
            } else { call_stmt }
        stmt :: code
    | _ => code
    }
}

type ctyp_temp_info_t =
{
    ctti_struct_decl: cdeftyp_t ref;
    ctti_freef_decl: cdeffun_t ref;
    ctti_copyf_decl: cdeffun_t ref;
    ctti_src_exp: cexp_t;
    ctti_dst_exp: cexp_t;
    ctti_cname_wo_prefix: string
}

type enum_map_t = (id_t, id_t) Map.t
type ctyp_map_t = (id_t, ctyp_temp_info_t) Map.t

fun convert_all_typs(kmods: kmodule_t list)
{
    var top_fwd_decl: cstmt_t list = []
    var top_typ_decl: cstmt_t list = []
    var top_typfun_decl: cstmt_t list = []
    var all_decls = empty_idset
    var all_fwd_decls = empty_idset
    var all_visited = empty_idset
    var all_var_enums: enum_map_t = Map.empty(cmp_id)
    var all_saved_rec_vars: ctyp_map_t = Map.empty(cmp_id)
    var curr_cm_idx = -1

    fun add_fwd_decl(i: id_t, fwd_decl: bool, decl: cstmt_t) =
        if i != noid {
            val have_fwd_decl = all_fwd_decls.mem(i)
            if !have_fwd_decl && fwd_decl {
                all_fwd_decls = all_fwd_decls.add(i)
                top_fwd_decl = decl :: top_fwd_decl
            }
        }

    fun add_decl(i: id_t, decl: cstmt_t) =
        if i != noid {
            all_decls = all_decls.add(i)
            match decl {
            | CDefFun _ => top_typfun_decl = decl :: top_typfun_decl
            | _ => top_typ_decl = decl :: top_typ_decl
            }
        }

    /* creates declaration of the data structure with optional forward declaration;
       if needed, creates declarations of the destructor and
       copy operator (with empty bodies; the bodies are updated later on),
       together with thier optional forward declarations */
    fun create_ctyp_decl(tn: id_t, fwd_decl: bool, loc: loc_t)
    {
        val ktp = K_annotate.get_ktprops(KTypName(tn), loc)
        val {ktp_ptr, ktp_pass_by_ref, ktp_custom_free, ktp_custom_copy} = ktp
        val (free_m, free_f) =
            if ktp_custom_free { (noid, gen_idc(curr_cm_idx, "free")) }
            else { (noid, noid) }
        val (copy_m, copy_f) =
        if ktp_custom_copy {
            (noid, gen_idc(curr_cm_idx, "copy"))
        } else if ktp_ptr {
            (std_FX_COPY_PTR, std_fx_copy_ptr)
        } else {
            (noid, noid)
        }
        val cname = get_idk_cname(tn, loc)
        val struct_id = if ktp_ptr { get_id(cname + "_data_t") }
                        else { tn }
        val struct_typ = CTypStruct(Some(struct_id), [])
        val struct_or_ptr_typ = if ktp_ptr { make_ptr(struct_typ) }
                                else { struct_typ }
        val struct_decl = ref (cdeftyp_t {
            ct_name=tn, ct_typ=struct_or_ptr_typ,
            ct_cname=cname, ct_data_start=0,
            ct_ifaces=[], ct_ifaces_id=noid,
            ct_enum=noid, ct_scope=[], ct_loc=loc,
            ct_props=ctprops_t { ctp_scalar=false,
                ctp_complex=ktp_custom_free || ktp_custom_copy,
                    ctp_ptr=ktp_ptr, ctp_pass_by_ref=ktp_pass_by_ref,
                    ctp_make=[], ctp_free=(free_m, free_f),
                    ctp_copy=(copy_m, copy_f)
                }
            })
        val ctyp = CTypName(tn)
        val src_typ0 = ctyp, dst_typ = make_ptr(ctyp)
        val (src_typ, src_flags) =
            if ktp_pass_by_ref {
                (make_const_ptr(src_typ0), [: CArgPassByPtr :])
            } else {
                (src_typ0, [])
            }
        val src_id = get_id("src")
        val dst_id = get_id("dst")
        val src_exp = make_id_t_exp(src_id, src_typ, loc)
        val dst_exp = make_id_t_exp(dst_id, dst_typ, loc)
        val cname_wo_prefix = K_mangle.remove_fx(cname)
        val freef_decl = ref (cdeffun_t {
            cf_name=free_f, cf_args=[: (dst_id, dst_typ, [: CArgPassByPtr :]) :],
            cf_rt=CTypVoid, cf_cname="_fx_free_" + cname_wo_prefix, cf_body=[],
            cf_flags=default_fun_flags().{fun_flag_nothrow=true, fun_flag_private=true},
            cf_scope=[], cf_loc=loc })
        val copyf_decl = ref freef_decl->{
            cf_name=copy_f, cf_args=[: (src_id, src_typ, src_flags),
                (dst_id, dst_typ, CArgPassByPtr :: []) :],
            cf_rt=CTypVoid, cf_cname="_fx_copy_" + cname_wo_prefix
            }
        set_idc_entry(tn, CTyp(struct_decl))
        if ktp_ptr {
            add_fwd_decl(tn, fwd_decl, CDefForwardTyp(struct_id, loc))
        }
        if ktp_custom_free {
            set_idc_entry(free_f, CFun(freef_decl))
            add_fwd_decl(free_f, fwd_decl, CDefForwardSym(free_f, loc))
        }
        if ktp_custom_copy {
            set_idc_entry(copy_f, CFun(copyf_decl))
            add_fwd_decl(copy_f, fwd_decl, CDefForwardSym(copy_f, loc))
        }
        ctyp_temp_info_t {
            ctti_struct_decl=struct_decl,
            ctti_freef_decl=freef_decl,
            ctti_copyf_decl=copyf_decl,
            ctti_src_exp=src_exp,
            ctti_dst_exp=dst_exp,
            ctti_cname_wo_prefix=cname_wo_prefix
        }
    }

    fun get_var_enum(kvar: kdefvariant_t ref)
    {
        val {kvar_base_name, kvar_cases, kvar_flags, kvar_loc} = *kvar
        match all_var_enums.find_opt(kvar_base_name) {
        | Some e_id =>
            match cinfo_(e_id, kvar_loc) {
            | CEnum (ref {cenum_members}) => (e_id, cenum_members)
            | _ => throw compile_err(kvar_loc,
                f"invalid variant enumeration '{idk2str(e_id, kvar_loc)}'")
            }
        | _ =>
            val e_base_cname = pp(kvar_base_name) + "_"
            val e_cname = e_base_cname + "tag_t"
            val e_id = gen_idc(curr_cm_idx, e_cname)
            val start_idx = if kvar_flags.var_flag_opt { 0 }
                            else { 1 }
            val ctx = (CTypCInt, kvar_loc)
            val fold members = [] for (ni, ti)@idx <- kvar_cases {
                val {kv_cname=cname_i} = get_kval(ni, kvar_loc)
                val dv = cdefval_t { cv_name=ni, cv_typ=CTypCInt, cv_cname=cname_i,
                                     cv_flags=default_val_flags(), cv_loc=kvar_loc }
                set_idc_entry(ni, CVal(dv))
                val vali = Some(CExpLit(KLitInt(int64(idx + start_idx)), ctx))
                (ni, vali) :: members
            }
            val members = members.rev()
            val ce = ref (cdefenum_t {cenum_name=e_id, cenum_members=members,
                                    cenum_cname=K_mangle.add_fx(e_cname),
                                    cenum_scope=[], cenum_loc=kvar_loc})
            set_idc_entry(e_id, CEnum(ce))
            all_var_enums = all_var_enums.add(kvar_base_name, e_id)
            add_decl(e_id, CDefEnum(ce))
            (e_id, members)
        }
    }

    fun cvt2ctyp(tn: id_t, loc: loc_t) =
        if !all_decls.mem(tn) { cvt2ctyp_(tn, loc) }

    fun cvt2ctyp_(tn: id_t, loc: loc_t)
    {
        val visited = all_visited.mem(tn)
        val deps = K_annotate.get_typ_deps(tn, loc)
        val kt_info = kinfo_(tn, loc)
        val (ce_id, ce_members, recursive_variant, fwd_decl, deps) =
        match kt_info {
        | KVariant kvar =>
            val (ce_id, ce_members) = get_var_enum(kvar)
            val {kvar_flags} = *kvar
            if kvar_flags.var_flag_recursive {
                val fwd_decl = !all_fwd_decls.mem(tn)
                (ce_id, ce_members, true, fwd_decl,
                deps.filter(fun (d) {d != tn}))
            } else {
                (ce_id, ce_members, false, false, deps)
            }
        | _ => (noid, [], false, false, deps)
        }
        match (recursive_variant, visited) {
        | (false, true) =>
            throw compile_err(loc,
                f"non-recursive variant type '{idk2str(tn, loc)}' references itself directly or indirectly")
        | _ => {}
        }
        all_visited = all_visited.add(tn)
        val ktp = K_annotate.get_ktprops(KTypName(tn), loc)
        val fx_result_id = get_id("fx_result")
        val fx_result_exp = make_id_exp(fx_result_id, loc)
        val fx_result_ct = CTypName(tn)
        val {ctti_struct_decl=struct_decl, ctti_freef_decl=freef_decl,
            ctti_copyf_decl=copyf_decl, ctti_src_exp=src_exp,
            ctti_dst_exp=dst_exp, ctti_cname_wo_prefix=tp_cname_wo_prefix} =
            if !recursive_variant {
                create_ctyp_decl(tn, fwd_decl, loc)
            } else {
                match all_saved_rec_vars.find_opt(tn) {
                | Some i => i
                | _ => throw compile_err(loc, f"cgen: missing information about recursive variant '{get_idk_cname(tn, loc)}'")
                }
            }
        val {cf_name=free_f} = *freef_decl
        val {cf_name=copy_f} = *copyf_decl
        val {ct_props} = *struct_decl
        val (struct_typ, struct_id_opt) =
            match *struct_decl {
            | {ct_typ=CTypRawPtr (_, CTypStruct (struct_id_opt, _) as a_struct_typ)} =>
                val struct_typ = match struct_id_opt {
                                | Some tn => CTypName(tn)
                                | _ => a_struct_typ
                                }
                (struct_typ, struct_id_opt)
            | _ => (CTypName(tn), Some(tn))
            }

        fun decref_and_free(dst_exp, free_code, loc) {
            val rc_exp = CExpArrow(dst_exp, get_id("rc"), (CTypInt, loc))
            val decref_exp = make_call(std_FX_DECREF, [: rc_exp :], CTypInt, loc)
            val cmp_1_exp = CExpBinary(COpCmp(CmpEQ), decref_exp, make_int_exp(1, loc), (CTypBool, loc))
            val cc_exp = CExpBinary(COpLogicAnd, dst_exp, cmp_1_exp, (CTypBool, loc))
            val call_free = make_call(std_fx_free, [: dst_exp :], CTypVoid, loc)
            val then_exp = rccode2stmt(CExp(call_free) :: free_code, loc)
            val clear_ptr = CExpBinary(COpAssign, dst_exp, make_nullptr(loc), (CTypVoid, loc))
            [: CExp(clear_ptr), CStmtIf(cc_exp, then_exp, CStmtNop(loc), loc) :]
        }

        all_decls = all_decls.add(tn)
        deps.app(fun (dep) {
                if !all_saved_rec_vars.mem(dep) {
                    val dep_loc = get_idk_loc(dep, loc)
                    cvt2ctyp(dep, dep_loc)
                }
            })
        match kt_info {
        | KInterface (ref {ki_name}) =>
            all_decls = all_decls.add(ki_name)
        | _ =>
            add_decl(tn, CDefTyp(struct_decl))
            if ktp.ktp_custom_free {
                add_decl(free_f, CDefFun(freef_decl))
            }
            if ktp.ktp_custom_copy {
                add_decl(copy_f, CDefFun(copyf_decl))
            }
        }
        match kt_info {
        | KTyp kt =>
            val {kt_typ, kt_loc} = *kt
            match kt_typ {
            | KTypTuple telems =>
                val (free_code, copy_code, make_code, relems, make_args) =
                fold free_code = [], copy_code = [], make_code = [],
                        relems = [], make_args = [] for kti@i <- telems {
                    val ni = get_id(f"t{i}")
                    val cti = ktyp2ctyp(kti, kt_loc)
                    val selem = CExpArrow(src_exp, ni, (cti, loc))
                    val delem = CExpArrow(dst_exp, ni, (cti, loc))
                    val free_code = gen_free_code(delem, cti, false, false, free_code, kt_loc)
                    val copy_code = gen_copy_code(selem, delem, cti, copy_code, kt_loc)
                    val {ktp_pass_by_ref} = K_annotate.get_ktprops(kti, kt_loc)
                    val selem2 = make_id_exp(ni, loc)
                    val (cti_arg, cti_arg_flags, selem2) =
                    if ktp_pass_by_ref {
                        (make_const_ptr(cti), [: CArgPassByPtr :], cexp_deref(selem2))
                    } else {
                        (cti, [], selem2)
                    }
                    val delem2 = CExpArrow(fx_result_exp, ni, (cti, loc))
                    val make_code = gen_copy_code(selem2, delem2, cti, make_code, kt_loc)
                    (free_code, copy_code, make_code, (ni, cti) :: relems,
                    (ni, cti_arg, cti_arg_flags) :: make_args)
                }
                val mktupl =
                if ktp.ktp_complex {
                    val make_args = (fx_result_id, make_ptr(fx_result_ct),
                                    [: CArgPassByPtr, CArgRetVal :]) :: make_args
                    val mktup_id = gen_idc(curr_cm_idx, "mktup")
                    val mktup_decl = ref (cdeffun_t {
                        cf_name=mktup_id, cf_args=make_args.rev(),
                        cf_rt=CTypVoid, cf_cname="_fx_make_" + tp_cname_wo_prefix,
                        cf_body=make_code.rev(),
                        cf_flags=default_fun_flags().{
                            fun_flag_nothrow=true,
                            fun_flag_private=true},
                        cf_scope=[],
                        cf_loc=loc
                        })
                    set_idc_entry(mktup_id, CFun(mktup_decl))
                    add_decl(mktup_id, CDefFun(mktup_decl))
                    *freef_decl = freef_decl->{cf_body=free_code.rev()}
                    *copyf_decl = copyf_decl->{cf_body=copy_code.rev()}
                    mktup_id :: []
                } else {
                    []
                }
                *struct_decl = struct_decl->{
                    ct_typ=CTypStruct(Some(tn), relems.rev()),
                    ct_props=ct_props.{ctp_make=mktupl}
                    }
            | KTypRecord (_, relems) =>
                val fold free_code = [], copy_code = [], make_code = [],
                    relems = [], make_args = [] for (ni, kti) <- relems {
                    val cti = ktyp2ctyp(kti, loc)
                    val selem = CExpArrow(src_exp, ni, (cti, kt_loc))
                    val delem = CExpArrow(dst_exp, ni, (cti, kt_loc))
                    val free_code = gen_free_code(delem, cti, false, false, free_code, kt_loc)
                    val copy_code = gen_copy_code(selem, delem, cti, copy_code, kt_loc)
                    val {ktp_pass_by_ref} = K_annotate.get_ktprops(kti, kt_loc)
                    val arg_ni = get_id("r_" + pp(ni))
                    val selem2 = make_id_exp(arg_ni, loc)
                    val (cti_arg, cti_arg_flags, selem2) =
                    if ktp_pass_by_ref {
                        (make_const_ptr(cti), [: CArgPassByPtr :], cexp_deref(selem2))
                    } else {
                        (cti, [], selem2)
                    }
                    val delem2 = CExpArrow(fx_result_exp, ni, (cti, loc))
                    val make_code = gen_copy_code(selem2, delem2, cti, make_code, kt_loc)
                    (free_code, copy_code, make_code, (ni, cti) :: relems,
                    (arg_ni, cti_arg, cti_arg_flags) :: make_args)
                }

                val mkrecl = if ktp.ktp_complex {
                    val make_args = (fx_result_id, make_ptr(fx_result_ct),
                                    [: CArgPassByPtr, CArgRetVal :]) :: make_args
                    val mkrec_id = gen_idc(curr_cm_idx, "mktup")
                    val mkrec_decl = ref (cdeffun_t {
                        cf_name=mkrec_id,
                        cf_args=make_args.rev(),
                        cf_rt=CTypVoid,
                        cf_cname="_fx_make_" + tp_cname_wo_prefix,
                        cf_body=make_code.rev(),
                        cf_flags=default_fun_flags().{fun_flag_nothrow=true, fun_flag_private=true},
                        cf_scope=[],
                        cf_loc=loc
                        })
                    set_idc_entry(mkrec_id, CFun(mkrec_decl))
                    add_decl(mkrec_id, CDefFun(mkrec_decl))
                    *freef_decl = freef_decl->{cf_body=free_code.rev()}
                    *copyf_decl = copyf_decl->{cf_body=copy_code.rev()}
                    mkrec_id :: []
                } else {
                    []
                }
                *struct_decl = struct_decl->{
                    ct_typ=CTypStruct(Some(tn), relems.rev()),
                    ct_props=ct_props.{ctp_make=mkrecl}
                    }
            | KTypList et =>
                val ct_hd = ktyp2ctyp(et, kt_loc)
                val ct_tl = fx_result_ct
                val {ctp_free=(_, free_f)} = ct_props
                val relems = (get_id("rc"), CTypInt) :: (get_id("tl"), ct_tl) ::
                            (get_id("hd"), ct_hd) :: []
                val (free_m, free_f) =
                    if free_f == noid {
                        (std_FX_FREE_LIST_SIMPLE, std_fx_free_list_simple)
                    } else {
                        val f = make_id_exp(std_FX_FREE_LIST_IMPL, loc)
                        val tn_arg = make_id_exp(tn, loc)
                        val free_hd_f =
                            match get_free_f(ct_hd, true, false, loc) {
                            | (_, Some free_hd_f) => free_hd_f
                            | _ => throw compile_err(loc,
                                f"unexpected element destructor when converting {get_idk_cname(tn, loc)}")
                            }
                        val free_code = [: CExp(CExpCall(f, tn_arg ::
                                free_hd_f :: [], (CTypVoid, loc))) :]
                        *freef_decl = freef_decl->{cf_body=free_code}
                        (noid, free_f)
                    }
                val cons_id = gen_idc(curr_cm_idx, "cons")
                val make_list_m = make_id_exp(std_FX_MAKE_LIST_IMPL, kt_loc)
                val (pass_by_ref, copy_hd_f) =
                    match get_copy_f(ct_hd, false, true, loc) {
                    | (pbr, Some f) => (pbr, f)
                    | _ => throw compile_err(loc,
                        f"missing element copy operator when converting {get_idk_cname(tn, loc)}")
                    }
                val (ct_hd_arg, hd_arg_flags) =
                    if pass_by_ref { (make_const_ptr(ct_hd), [: CArgPassByPtr :]) }
                    else { (ct_hd, []) }
                val make_list_body = CExp(CExpCall(make_list_m, CExpTyp(CTypName(tn), loc) ::
                                    copy_hd_f :: [], (CTypInt, loc))) :: []
                val cons_decl =
                    ref (cdeffun_t {
                        cf_name=cons_id,
                        cf_rt=CTypCInt,
                        cf_args = [:(get_id("hd"), ct_hd_arg, hd_arg_flags),
                                    (get_id("tl"), ct_tl, []),
                                    (get_id("addref_tl"), CTypBool, []),
                                    (fx_result_id, make_ptr(ct_tl),
                                        [: CArgPassByPtr, CArgRetVal :])
                                    :],
                        cf_cname="_fx_cons_" + tp_cname_wo_prefix,
                        cf_body=make_list_body,
                        cf_flags=default_fun_flags().{fun_flag_private=true},
                        cf_scope=[],
                        cf_loc=loc
                        })
                set_idc_entry(cons_id, CFun(cons_decl))
                add_decl(cons_id, CDefFun(cons_decl))
                *struct_decl = struct_decl->{
                    ct_typ=make_ptr(CTypStruct(struct_id_opt, relems)),
                    ct_props=ct_props.{ctp_free=(free_m, free_f), ctp_make=cons_id :: []}
                    }
            | KTypRef et =>
                val ctyp = ktyp2ctyp(et, kt_loc)
                val {ctp_free=(_, free_f)} = ct_props
                val relems = (get_id("rc"), CTypInt) :: (get_id("data"), ctyp) :: []
                val (free_m, free_f) =
                if free_f == noid {
                    (std_FX_FREE_REF_SIMPLE, std_fx_free_ref_simple)
                } else {
                    val f = make_id_exp(std_FX_FREE_REF_IMPL, loc)
                    val tn_arg = make_id_exp(tn, loc)
                    val free_hd_f =
                    match get_free_f(ctyp, true, false, loc) {
                    | (_, Some free_hd_f) => free_hd_f
                    | _ => throw compile_err(loc,
                        f"unexpected element destructor when converting {get_idk_cname(tn, loc)}")
                    }
                    val free_code = [: CExp(CExpCall(f, tn_arg :: free_hd_f :: [], (CTypVoid, loc))) :]
                    *freef_decl = freef_decl->{cf_body=free_code}
                    (noid, free_f)
                }
                val mkref_id = gen_idc(curr_cm_idx, "mkref")
                val mkref_m = make_id_exp(std_FX_MAKE_REF_IMPL, kt_loc)
                val (pass_by_ref, copy_data_f) =
                match get_copy_f(ctyp, false, true, loc) {
                | (pbr, Some f) => (pbr, f)
                | _ => throw compile_err(loc,
                    f"missing element copy operator when converting {get_idk_cname(tn, loc)}")
                }
                val (ct_arg, ct_arg_flags) =
                    if pass_by_ref { (make_const_ptr(ctyp), [: CArgPassByPtr :]) }
                    else { (ctyp, []) }
                val mkref_body = CExp(CExpCall(mkref_m, CExpTyp(CTypName(tn), loc) ::
                                        copy_data_f :: [], (CTypInt, loc))) :: []
                val mkref_decl = ref (cdeffun_t {
                    cf_name=mkref_id, cf_rt=CTypCInt,
                    cf_args = [: (get_id("arg"), ct_arg, ct_arg_flags),
                                (fx_result_id, make_ptr(fx_result_ct),
                                [: CArgPassByPtr, CArgRetVal :])
                                :],
                    cf_cname="_fx_make_" + tp_cname_wo_prefix, cf_body=mkref_body,
                    cf_flags=default_fun_flags().{fun_flag_private=true},
                    cf_scope=[], cf_loc=loc
                    })
                set_idc_entry(mkref_id, CFun(mkref_decl))
                add_decl(mkref_id, CDefFun(mkref_decl))
                *struct_decl = struct_decl->{
                    ct_typ=make_ptr(CTypStruct(struct_id_opt, relems)),
                    ct_props=ct_props.{ctp_free=(free_m, free_f), ctp_make=mkref_id :: []}
                    }
            | KTypFun (argtyps, rt) =>
                val cargs = ktyp2ctyp_fargs(argtyps, rt, loc)
                val fp_ctyp = CTypFunRawPtr(cargs, CTypCInt)
                val fv_ctyp = make_ptr(CTypName(get_id("fx_fcv_t")))
                val relems = (get_id("fp"), fp_ctyp) :: (get_id("fcv"), fv_ctyp) :: []
                *struct_decl = struct_decl->{
                    ct_typ=CTypStruct(struct_id_opt, relems),
                    ct_props=ct_props.{
                        ctp_ptr=false, ctp_pass_by_ref=true,
                        ctp_free=(std_FX_FREE_FP, std_fx_free_fp),
                        ctp_copy=(std_FX_COPY_FP, std_fx_copy_fp)
                        }
                    }
            | _ => {}
            }
        | KVariant kvar =>
            val {kvar_cname, kvar_cases, kvar_ifaces, kvar_flags, kvar_loc} = *kvar
            val have_tag = kvar_flags.var_flag_have_tag
            val int_ctx = (CTypCInt, kvar_loc)
            val void_ctx = (CTypVoid, kvar_loc)
            val tag_id = get_id("tag")
            val u_id = get_id("u")
            val dst_exp =
                if recursive_variant {
                    CExpUnary(COpDeref, dst_exp, (struct_typ, kvar_loc))
                } else {
                    dst_exp
                }
            val src_tag_exp = CExpArrow(src_exp, tag_id, int_ctx)
            val dst_tag_exp = CExpArrow(dst_exp, tag_id, int_ctx)
            val src_u_exp = CExpArrow(src_exp, u_id, (CTypAny, kvar_loc))
            val dst_u_exp = CExpArrow(dst_exp, u_id, (CTypAny, kvar_loc))
            val fold free_cases=[], copy_cases=[], uelems=[]
                for (ni, kt) <- kvar_cases, (label_i, _) <- ce_members {
                    match kt {
                    | KTypVoid => (free_cases, copy_cases, uelems)
                    | _ =>
                        val ti = ktyp2ctyp(kt, loc)
                        val ni_clean = get_orig_id(ni)
                        val selem_i = CExpMem(src_u_exp, ni_clean, (ti, kvar_loc))
                        val delem_i = CExpMem(dst_u_exp, ni_clean, (ti, kvar_loc))
                        val switch_label_i_exps = [: make_id_t_exp(label_i, CTypCInt, kvar_loc) :]
                        val free_code_i = gen_free_code(delem_i, ti, false, false, [], kvar_loc)
                        val free_cases= match free_code_i {
                                        | [] => free_cases
                                        | _ => (switch_label_i_exps, free_code_i) :: free_cases
                                        }
                        val copy_code_i = gen_copy_code(selem_i, delem_i, ti, [], kvar_loc)
                        val copy_cases =
                            match copy_code_i {
                            | CExp(CExpBinary (COpAssign, _, _, _)) :: [] => copy_cases
                            | _ => (switch_label_i_exps, copy_code_i) :: copy_cases
                            }
                        (free_cases, copy_cases, (ni_clean, ti) :: uelems)
                    }
                }
            val free_code =
                match (have_tag, free_cases) {
                | (_, []) => []
                | (false, (_, free_code) :: rest) =>
                    if rest != [] {
                        throw compile_err(kvar_loc,
                            f"cgen: variant '{kvar_cname}' with no tag somehow has multiple cases in the destructor")
                    }
                    free_code
                | _ => val free_cases = ([], []) :: free_cases
                    [: CStmtSwitch(dst_tag_exp, free_cases.rev(), kvar_loc) :]
                }
            val free_code =
                if recursive_variant {
                    decref_and_free(dst_exp, free_code, kvar_loc)
                } else if have_tag {
                    val clear_tag = CExpBinary(COpAssign, dst_tag_exp,
                            make_int_exp(0, kvar_loc), void_ctx)
                    if free_code == [] { [] }
                    else { CExp(clear_tag) :: free_code }
                } else {
                    free_code
                }
            val copy_code =
                if have_tag {
                    val copy_code = gen_copy_code(src_tag_exp, dst_tag_exp, CTypCInt, [], kvar_loc)
                    val default_copy_code = CExp(CExpBinary(COpAssign, dst_u_exp, src_u_exp, void_ctx))
                    match copy_cases {
                    | [] => default_copy_code :: copy_code
                    | _ =>
                        val copy_cases = ([], [: default_copy_code :]) :: copy_cases
                        CStmtSwitch(src_tag_exp, copy_cases.rev(), kvar_loc) :: copy_code
                    }
                } else {
                    val default_copy_code = [: CExp(CExpBinary(COpAssign,
                                    dst_u_exp, src_u_exp, void_ctx)) :]
                    match copy_cases {
                    | (_, copy_code1) :: [] => copy_code1
                    | _ => default_copy_code
                    }
                }
            val relems= if uelems == [] { [] }
                        else { (u_id, CTypUnion(None, uelems.rev())) :: [] }
            val relems= if have_tag { (tag_id, CTypCInt) :: relems }
                        else { relems }
            if recursive_variant {
                val relems =
                    if kvar_ifaces == [] { relems }
                    else { (get_id("ifaces"), std_fx_ifaces_t_cptr) :: relems }
                val relems = (get_id("rc"), CTypInt) :: relems
                *struct_decl = struct_decl->{ct_typ=make_ptr(CTypStruct(struct_id_opt, relems))}
            } else {
                *struct_decl = struct_decl->{ct_typ=CTypStruct(struct_id_opt, relems)}
            }
            val ct_ifaces = [: for (iname, _) <- kvar_ifaces { iname } :]
            *struct_decl = struct_decl->{ct_enum=ce_id, ct_ifaces=ct_ifaces}
            *freef_decl = freef_decl->{cf_body=free_code.rev()}
            *copyf_decl = copyf_decl->{cf_body=copy_code.rev()}
        | KInterface ki =>
            val {ki_name, ki_cname, ki_base, ki_id, ki_all_methods, ki_scope, ki_loc} = *ki
            val vtbl_id = gen_idc(curr_cm_idx, pp(ki_name)+"_vtbl_t")
            val vtbl_cname = K_mangle.remove_fx(ki_cname) + "_vtbl_t"
            val iface_decl = ref (cdefinterface_t {
                ci_name=ki_name, ci_cname=ki_cname,
                ci_base=ki_base,
                ci_id=ki_id, ci_vtbl=vtbl_id,
                ci_all_methods=[],
                ci_scope=ki_scope, ci_loc=ki_loc
                })
            set_idc_entry(ki_name, CInterface(iface_decl))
            val vtbl_elems = [: for (f, t) <- ki_all_methods {
                    val f = get_orig_id(f)
                    val (args, rt) = match t {
                        | KTypFun(args, rt) => (args, rt)
                        | _ => throw compile_err(ki_loc,
                            f"cgen: method {pp(ki_name)}.{pp(f)} has non-function type {t}")
                        }
                    val cargs = std_CTypVoidPtr :: ktyp2ctyp_fargs(args, rt, loc)
                    val ctyp = CTypFunRawPtr(cargs, CTypCInt)
                    (f, ctyp) } :]
            val ctp = ctprops_t { ctp_scalar=false, ctp_complex=true, ctp_make=[],
                                  ctp_free=(std_FX_FREE_IFACE, std_fx_free_iface),
                                  ctp_copy=(std_FX_COPY_IFACE, std_fx_copy_iface),
                                  ctp_pass_by_ref=true, ctp_ptr=false }
            val relems = (get_id("vtbl"), make_const_ptr(CTypName(vtbl_id))) :: (get_id("obj"), std_CTypVoidPtr) :: []
            add_fwd_decl(tn, true, CDefForwardTyp(ki_name, loc))
            val vtbl_decl = ref (cdeftyp_t {
                ct_name=vtbl_id, ct_typ=CTypStruct(Some(vtbl_id), vtbl_elems),
                ct_cname=vtbl_cname, ct_data_start=0,
                ct_ifaces=[], ct_ifaces_id=noid,
                ct_enum=noid, ct_scope=ki_scope, ct_loc=ki_loc,
                ct_props=ctprops_t { ctp_scalar=false,
                        ctp_complex=false,
                        ctp_ptr=true, ctp_pass_by_ref=true,
                        ctp_make=[], ctp_free=(noid, noid),
                        ctp_copy=(noid, noid)
                    }
                })
            set_idc_entry(vtbl_id, CTyp(vtbl_decl))
            add_decl(vtbl_id, CDefTyp(vtbl_decl))
            top_typ_decl = CDefInterface(iface_decl) :: top_typ_decl
            *struct_decl = struct_decl->{ct_typ=CTypStruct(Some(ki_name), relems), ct_props=ctp}
            *iface_decl = iface_decl->{ci_all_methods=vtbl_elems}
        | _ => throw compile_err(loc, f"type '{idk2str(tn, loc)}' cannot be converted to C")
        }
    }

    fun fold_n_cvt_ktyp(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun fold_n_cvt_kexp(e: kexp_t, callb: k_fold_callb_t) =
        match e {
        | KDefTyp (ref {kt_name, kt_cname, kt_loc}) => cvt2ctyp(kt_name, kt_loc)
        | KDefVariant (ref {kvar_name, kvar_loc}) => cvt2ctyp(kvar_name, kvar_loc)
        | KDefClosureVars _ => {}
        | KDefExn _ => {}
        | _ => fold_kexp(e, callb)
        }

    val fold_n_cvt_callb = k_fold_callb_t {
        kcb_fold_ktyp=Some(fold_n_cvt_ktyp),
        kcb_fold_kexp=Some(fold_n_cvt_kexp),
        kcb_fold_atom=None
        }
    for {km_idx, km_top} <- kmods {
        curr_cm_idx = km_idx
        for e <- km_top {
            match e {
            | KDefVariant (ref {kvar_name, kvar_flags, kvar_loc}) =>
                if kvar_flags.var_flag_recursive {
                    val i = create_ctyp_decl(kvar_name, true, kvar_loc)
                    all_saved_rec_vars = all_saved_rec_vars.add(kvar_name, i)
                }
            | _ => {}
            }
        }
    }
    for {km_idx, km_top} <- kmods {
        curr_cm_idx = km_idx
        for e <- km_top {
            fold_n_cvt_kexp(e, fold_n_cvt_callb)
        }
    }
    (top_fwd_decl.rev(), top_typ_decl.rev(), top_typfun_decl.rev())
}

/* Excludes types (together with their utility functions like
   _fx_copy_..., _fx_make_..., _fx_free_... etc.)
   that are not used by the module (passed as ccode) */
fun elim_unused_ctypes(mname: id_t, all_ctypes_fwd_decl: cstmt_t list,
                       all_ctypes_decl: cstmt_t list,
                       all_ctypes_fun_decl: cstmt_t list,
                       ccode: ccode_t)
{
    fun get_ctyp_id(t: ctyp_t) =
        match t {
        | CTypName tn => tn
        | CTypStruct (Some n, _) => n
        | CTypUnion (Some n, _) => n
        | CTypRawPtr (_, t) => get_ctyp_id(t)
        | _ => noid
        }

    fun is_used_decl(s: cstmt_t, used_ids: id_hashset_t): bool =
        match s {
        | CDefTyp (ref {ct_name, ct_typ}) =>
            used_ids.mem(ct_name) || used_ids.mem(get_ctyp_id(ct_typ))
        | CDefForwardTyp (tn, loc) => used_ids.mem(tn)
        | CDefForwardSym (f, loc) => used_ids.mem(f)
        | CDefFun (ref {cf_name}) => used_ids.mem(cf_name)
        | CDefEnum (ref {cenum_name, cenum_members}) =>
            used_ids.mem(cenum_name) ||
            cenum_members.exists(fun ((m, _)) { used_ids.mem(m) })
        | CDefInterface (ref {ci_name}) =>
            used_ids.mem(ci_name)
        | _ => false
        }

    fun used_ids_by_ccode(ccode: ccode_t, size0: int): id_hashset_t
    {
        var used_ids = empty_id_hashset(size0)
        fun add_used_id(n: id_t, callb: c_fold_callb_t) =
            if n.m > 0 || !pp(n).startswith("fx_") { used_ids.add(n) }

        fun used_ctyp(t: ctyp_t, callb: c_fold_callb_t)
        {
            fold_ctyp(t, callb)
            match t {
            | CTypStruct(Some n, _) => add_used_id(n, callb)
            | CTypUnion(Some n, _) => add_used_id(n, callb)
            | CTypName n => add_used_id(n, callb)
            | _ => {}
            }
        }

        fun used_ids_by_cstmt(s: cstmt_t, callb: c_fold_callb_t)
        {
            fold_cstmt(s, callb)
            match s {
            | CDefTyp (ref {ct_typ, ct_enum, ct_ifaces, ct_ifaces_id,
                ct_props={ctp_make, ctp_free=(_, free_f),
                          ctp_copy=(_, copy_f)}, ct_loc}) =>
                for i <- ct_enum :: free_f :: copy_f :: ctp_make {
                    add_used_id(i, callb)
                }
                for i <- ct_ifaces { add_used_id(i, callb) }
                add_used_id(ct_ifaces_id, callb)
            | CDefInterface (ref {ci_base, ci_vtbl}) =>
                add_used_id(ci_base, callb); add_used_id(ci_vtbl, callb)
            | CDefForwardTyp(n, _) => add_used_id(n, callb)
            | CDefEnum(ref {cenum_members}) => {}
            | _ => {}
            }
        }

        val callb = c_fold_callb_t
        {
            ccb_fold_ident=None,
            ccb_fold_typ=Some(used_ctyp),
            ccb_fold_exp=None,
            ccb_fold_stmt=Some(used_ids_by_cstmt)
        }

        for s <- ccode { used_ids_by_cstmt(s, callb) }
        used_ids
    }

    val used_ids = used_ids_by_ccode(ccode, 1024)
    fun update_used_ids(used_ids: id_hashset_t)
    {
        val decls_plus =
            [: for s <- all_ctypes_decl {
                val uv = used_ids_by_ccode(s :: [], 8)
                (s, uv) } :]
        var done = false
        for i <- 0:100 {
            val size0 = used_ids.size()
            for (s, uv) <- decls_plus {
                if  is_used_decl(s, used_ids) {
                    used_ids.union(uv)
                }
            }
            if used_ids.size() == size0 {
                done = true; break
            }
        }
        if !done {throw compile_err(noloc, f"{pp(mname)}: too many iterations in update_used_ctypes_")}
    }

    update_used_ids(used_ids)
    val fold ctypes_ccode = []
        for s <- all_ctypes_fwd_decl + (all_ctypes_decl + all_ctypes_fun_decl) {
            if is_used_decl(s, used_ids) { s :: ctypes_ccode }
            else { ctypes_ccode }
        }
    ctypes_ccode.rev()
}
