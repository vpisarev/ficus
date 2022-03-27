/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Generates declarations of C functions
*/

from Ast import *
from K_form import *
from C_form import *

import K_annotate, K_mangle, C_gen_types
import Set

fun convert_all_fdecls(cm_idx: int, top_code: kcode_t)
{
    var top_fcv_decls: ccode_t = []
    var top_func_decls: ccode_t = []
    var mod_init_calls: ccode_t = []
    var mod_exn_data_decls: ccode_t = []

    for e <- top_code {
        match e {
        | KDefFun kf =>
            val {kf_name, kf_cname, kf_params, kf_rt=rt, kf_closure,
                kf_flags, kf_body, kf_scope, kf_loc} = *kf
            val {kci_arg} = kf_closure
            val is_ccode_func = match kf_body { | KExpCCode _ => true | _ => false }
            val ctor = kf_flags.fun_flag_ctor
            val fold args = [] for arg@idx <- kf_params {
                val {kv_typ=t} = get_kval(arg, kf_loc)
                val arg = if arg.m == 0 {dup_idc(cm_idx, arg)} else {arg}
                val cname = if is_ccode_func { pp(arg) }
                            else if ctor != CtorNone { f"arg{idx}" }
                            else { "" }
                val ctyp = C_gen_types.ktyp2ctyp(t, kf_loc)
                val {ktp_pass_by_ref} = K_annotate.get_ktprops(t, kf_loc)
                val (ctyp, arg_flags) =
                if ktp_pass_by_ref {
                    match ctyp {
                    | CTypArray _ => (make_ptr(ctyp), [CArgPassByPtr])
                    | _ => (make_const_ptr(ctyp), [CArgPassByPtr])
                    }
                } else {
                    (ctyp, [])
                }
                add_cf_arg(arg, ctyp, cname, kf_loc)
                (arg, ctyp, arg_flags) :: args
            }
            val {ktp_scalar=rt_scalar} = K_annotate.get_ktprops(rt, kf_loc)
            val crt = C_gen_types.ktyp2ctyp(rt, kf_loc)
            val is_nothrow = kf_flags.fun_flag_nothrow
            val (new_crt, args) =
            if is_nothrow && rt_scalar {
                (crt, args)
            } else if crt == CTypVoid {
                (CTypCInt, args)
            } else {
                val v = gen_idc(cm_idx, "fx_result")
                val crt = make_ptr(crt)
                add_cf_arg(v, crt, "fx_result", kf_loc)
                (if is_nothrow { CTypVoid } else { CTypCInt },
                (v, crt, [CArgPassByPtr, CArgRetVal ]) :: args)
            }
            val args =
                if ctor != CtorNone {
                    args
                } else {
                    val fv_cname = "fx_fv"
                    val fv_arg = gen_idc(cm_idx, fv_cname)
                    add_cf_arg(fv_arg, std_CTypVoidPtr, fv_cname, kf_loc)
                    (fv_arg, std_CTypVoidPtr, [CArgFV]) :: args
                }
            val cf = ref (cdeffun_t {
                cf_name=kf_name, cf_rt=new_crt, cf_args=args.rev(),
                cf_cname=kf_cname, cf_body=[],
                cf_flags=if kci_arg == noid { kf_flags }
                        else { kf_flags.{fun_flag_uses_fv=true} },
                cf_scope=kf_scope,
                cf_loc=kf_loc
                })
            set_idc_entry(kf_name, CFun(cf))
            top_func_decls = CDefFun(cf) :: top_func_decls
        | KDefClosureVars kcv =>
            val {kcv_name, kcv_cname, kcv_freevars, kcv_loc} = *kcv
            val fcv_typ = make_ptr(CTypName(kcv_name))
            val dst_id = get_id("dst")
            val dst_exp = make_id_t_exp(dst_id, fcv_typ, kcv_loc)
            val relems = (get_id("free_f"), std_fx_free_t) :: (get_id("rc"), CTypInt) :.
            val fold relems=relems, free_ccode=[] for (n, kt)@idx <- kcv_freevars {
                val ctyp = C_gen_types.ktyp2ctyp(kt, kcv_loc)
                val c_id = get_id(f"t{idx}")
                val elem_exp = cexp_arrow(dst_exp, c_id, ctyp)
                val free_ccode = C_gen_types.gen_free_code(elem_exp, ctyp, true, false, free_ccode, kcv_loc)
                ((c_id, ctyp) :: relems, free_ccode)
            }
            val (free_f, decl_free_f) =
            if free_ccode == [] {
                (std_fx_free, [])
            } else {
                val call_free = make_call(std_fx_free, [dst_exp], CTypVoid, kcv_loc)
                val freecode = CExp(call_free) :: free_ccode
                val free_f = gen_idc(cm_idx, "free_cv")
                val free_f_cname = "_fx_free_" + K_mangle.remove_fx(kcv_cname[0:.-"_cldata_t".length()])
                val cf = ref (cdeffun_t {
                    cf_name=free_f, cf_rt=CTypVoid,
                    cf_args=[(dst_id, fcv_typ, [CArgPassByPtr]) ],
                    cf_cname=free_f_cname, cf_body=freecode.rev(),
                    cf_flags=default_fun_flags().{fun_flag_nothrow=true},
                    cf_scope=[], cf_loc=kcv_loc
                    })
                set_idc_entry(free_f, CFun(cf))
                (free_f, [CDefFun(cf)])
            }
            val ct = ref (cdeftyp_t {
                ct_name=kcv_name, ct_typ=CTypStruct(None, relems.rev()),
                ct_cname=kcv_cname, ct_data_start=2,
                ct_ifaces=[], ct_ifaces_id=noid,
                ct_enum=noid, ct_scope=[], ct_loc=kcv_loc,
                ct_props = ctprops_t {
                    ctp_scalar=false, ctp_complex=true, ctp_ptr=false,
                    ctp_pass_by_ref=true, ctp_make=[],
                    ctp_free=(noid, free_f), ctp_copy=(noid, noid) }
                })
            set_idc_entry(kcv_name, CTyp(ct))
            top_fcv_decls = decl_free_f + (CDefTyp(ct) :: top_fcv_decls)
        | KDefExn (ref {ke_name, ke_std=true, ke_tag, ke_typ=KTypVoid, ke_cname, ke_scope, ke_loc}) =>
            val {kv_cname} = get_kval(ke_tag, ke_loc)
            val cv_flags = default_val_flags().{val_flag_global=ke_scope, val_flag_mutable=true}
            val _ = create_cdefval(ke_tag, CTypCInt, cv_flags, kv_cname, None, [], ke_loc)
            val cv_flags = default_val_flags().{val_flag_global=ke_scope, val_flag_mutable=true}
            val (exn_exp, decls) = create_cdefval(ke_name, CTypExn, cv_flags, ke_cname,
                                                  Some(make_dummy_exp(ke_loc)), [], ke_loc)
            val call_reg_exn = CExp(make_call(std_FX_REG_SIMPLE_STD_EXN,
                [make_id_t_exp(get_id(kv_cname), CTypCInt, ke_loc), exn_exp ], CTypVoid, ke_loc))
            top_fcv_decls = decls + top_fcv_decls
            mod_init_calls = call_reg_exn :: mod_init_calls
        | KDefExn ke =>
            val {ke_name, ke_typ, ke_std, ke_tag, ke_cname, ke_base_cname, ke_make, ke_scope, ke_loc} = *ke
            val exn_strname = get_qualified_name(pp(ke_name), ke_scope)
            val exn_strname = CExpLit(KLitString(exn_strname), (CTypString, ke_loc))
            val exn_info = gen_idc(cm_idx, pp(ke_name) + "_info")
            val info_cname = K_mangle.add_fx(ke_base_cname + "_info")
            val cv_flags = default_val_flags().{val_flag_global=ke_scope, val_flag_mutable=true}
            val (info_exp, decls) = create_cdefval(exn_info, std_fx_exn_info_t, cv_flags,
                                        info_cname, Some(make_dummy_exp(ke_loc)), [], ke_loc)
            val ke_tag_exp = make_id_t_exp(ke_tag, CTypCInt, ke_loc)
            val (reg_calls, decls, exn_data_decls) =
            match ke_typ {
            /*
                case 1. exceptions without parameters:

                `exception MyException`
                will be translated to

                // those three will be initialized by fx_register_simple_exn()
                static int _FX_EXN_E11MyException = 0;
                static fx_exn_info_t _fx_E11MyException_info = {}
                static fx_exn_t _fx_E11MyExceptionv = {}
                ...
                int fx_toplevel() {
                    ...
                    fx_register_simple_exn("CurrModule.MyException", &_FX_EXN_E11MyException,
                        &_fx_E11MyException_info, &_fx_E11MyExceptionv);
                    // 1. sets _FX_EXN_E11MyException to global exn counter and decrements global exn counter (e.g. -1100 => -1101)
                    // 2. sets _fx_E11MyException_info to {name="CurrModule.MyException", free_f=0, to_string=0, print_repr=0}
                    // 3. sets _fx_E11MyException = {_FX_E11MyException, &_FX_E11MyException_info, 0}.
                }

                `throw MyException` will be converted to
                `FX_THROW(_fx_E11MyExceptionv, false, catch_label)`
            */
            | KTypVoid =>
                val cv_flags = default_val_flags().{val_flag_global=ke_scope, val_flag_mutable=true}
                val (exn_exp, decls) = create_cdefval(ke_name, CTypExn, cv_flags,
                                            ke_cname, Some(make_dummy_exp(ke_loc)), decls, ke_loc)
                val call_reg_exn = make_call(std_FX_REG_SIMPLE_EXN, [exn_strname,
                                            ke_tag_exp, info_exp, exn_exp], CTypVoid, ke_loc)
                (CExp(call_reg_exn) :., decls, [])
            /*
                case 2. Exceptions with parameters

                `exception MyException: (int, string)`
                will be translated to

                static int _FX_EXN_E11MyException = 0;
                static fx_exn_info_t _fx_E11MyException_info;

                typedef struct _fx_E11MyException_data_t
                {
                    int_ rc;
                    _fx_T2iS data;
                } _fx_E11MyException_data_t;
                static void _fx_free_E11MyException(_fx_E11MyException_data_t* dst)
                {
                    if(dst) {
                        fx_free_str(&dst->data.t1);
                        fx_free(dst);
                    }
                }
                static int _fx_make_E11MyException(int arg0, fx_str_t* arg1, fx_exn_t* fx_result)
                {
                    FX_MAKE_EXN_IMPL_START(_FX_E11MyException, _fx_E11MyException);
                    exn_data->data.t0 = arg0;
                    fx_copy_str(arg1, &exn_data->data.t1);
                    return FX_OK;
                }

                That is, we do not declare a single global exception in this case,
                because there is no one, it needs to be constructed dynamically using
                the specified parameters.
                There is also free function. Note that it's called when the reference
                counter reached 0. If none of the exception parameters needs
                dedicated destructor, we can just use fx_free.

                `throw MyException(3, "abc")` will be converted at K-normalization to
                `TEMP val e = MyException(3, "abc")
                throw e`,

                which will be converted to
                fx_exn_t e = {};
                fx_str_t abc_str = FX_MAKE_STR("abc");
                FX_CALL(_fx_make_E11MyException(3, &abc_str, &e), catch_label);
                FX_THROW(e, true, catch_label);

                inside fx_toplevel() we need to register it with
                fx_register_exn("CurrModule.MyException", &_FX_EXN_E11MyException,
                    &_fx_E11MyException_info, _fx_free_E11MyException, 0, 0);
                (in principle, we could also specify "to_string" and "print" functions,
                but we postpone it till we have auto-generated string() and print_repr()
                functions for arbitrary types)
            */
            | _ =>
                val exn_data_id = gen_idc(cm_idx, pp(ke_name) + "_data_t")
                val exn_data_cname = K_mangle.add_fx(ke_base_cname + "_data_t")
                val exn_data_ptr_t = make_ptr(CTypName(exn_data_id))
                val dst_id = get_id("dst")
                val dst_exp = make_id_t_exp(dst_id, exn_data_ptr_t, ke_loc)
                val ke_ctyp = C_gen_types.ktyp2ctyp(ke_typ, ke_loc)
                val dst_data_exp = cexp_arrow(dst_exp, get_id("data"), ke_ctyp)
                val free_ccode = C_gen_types.gen_free_code(dst_data_exp, ke_ctyp, true, false, [], ke_loc)
                val relems = [(get_id("rc"), CTypInt), (get_id("data"), ke_ctyp) ]
                val (free_f, free_f_decl) =
                if free_ccode == [] {
                    (std_fx_free, [])
                } else {
                    val call_free = make_call(std_fx_free, [dst_exp], CTypVoid, ke_loc)
                    val freecode = CExp(call_free) :: free_ccode
                    val free_f = gen_idc(cm_idx, "free_exn")
                    val free_f_cname = "_fx_free_" + ke_base_cname
                    val cf = ref (cdeffun_t {
                        cf_name=free_f, cf_rt=CTypVoid,
                        cf_args=[(dst_id, exn_data_ptr_t, [CArgPassByPtr]) ],
                        cf_cname=free_f_cname, cf_body=freecode.rev(),
                        cf_flags=default_fun_flags().{fun_flag_nothrow=true},
                        cf_scope=[], cf_loc=ke_loc
                        })
                    set_idc_entry(free_f, CFun(cf))
                    (free_f, CDefFun(cf) :.)
                }
                val exn_data_ct = ref (cdeftyp_t {
                    ct_name=exn_data_id, ct_typ=CTypStruct(None, relems),
                    ct_cname=exn_data_cname, ct_data_start=1, ct_enum=noid,
                    ct_ifaces=[], ct_ifaces_id=noid, ct_scope=[], ct_loc=ke_loc,
                    ct_props = ctprops_t {
                        ctp_scalar=false, ctp_complex=true, ctp_ptr=false,
                        ctp_pass_by_ref=true, ctp_make=[],
                        ctp_free=(noid, free_f), ctp_copy=(noid, noid)}
                    })
                val free_f_exp = make_id_t_exp(free_f, std_fx_free_t, ke_loc)
                val cexn = ref (cdefexn_t {
                    cexn_name=ke_name, cexn_cname=ke_cname,
                    cexn_base_cname=ke_base_cname,
                    cexn_typ=C_gen_types.ktyp2ctyp(ke_typ, ke_loc),
                    cexn_std=ke_std, cexn_tag=ke_tag,
                    cexn_data=exn_data_id, cexn_info=exn_info,
                    cexn_make=ke_make, cexn_scope=ke_scope,
                    cexn_loc=ke_loc })
                set_idc_entry(ke_name, CExn(cexn))
                set_idc_entry(exn_data_id, CTyp(exn_data_ct))
                val call_reg_exn = make_call(std_FX_REG_EXN, [exn_strname, ke_tag_exp,
                                             info_exp, free_f_exp], CTypVoid, ke_loc)
                (CExp(call_reg_exn) :.,
                free_f_decl + (CDefTyp(exn_data_ct) :: decls),
                CDefTyp(exn_data_ct) :.)
            }
            top_fcv_decls = decls + top_fcv_decls
            mod_init_calls = reg_calls + mod_init_calls
            mod_exn_data_decls = exn_data_decls + mod_exn_data_decls
        | KDefInterface ki =>
            val {ki_id, ki_loc} = *ki
            val id_exp = make_id_t_exp(ki_id, CTypInt, ki_loc)
            val reg_iface_call = make_call(get_id("fx_register_iface"),
                    [cexp_get_addr(id_exp)], CTypVoid, ki_loc)
            mod_init_calls = CExp(reg_iface_call) :: mod_init_calls
        | KDefVariant (ref {kvar_name, kvar_cname, kvar_ifaces, kvar_scope, kvar_loc})
            when kvar_ifaces != [] =>
            val ct = match cinfo_(kvar_name, kvar_loc) {
                | CTyp ct => ct
                | _ => throw compile_err(kvar_loc,
                    "variant type '{idk2str(kvar_name, kvar_loc)}' was not converted to C yet")
                }
            val {ct_props={ctp_free=(_, free_f)}} = *ct
            val entry_ctyp = CTypName(get_id("fx_iface_entry_t"))
            val entries_ctyp = CTypRawArray([CTypStatic], entry_ctyp)
            var fold init_ccode = [], ids = [], pairs = [], all_ids = empty_idset
                for (iname, methods) <- kvar_ifaces {
                val mptrs = [for m <- methods
                    { make_id_t_exp(m, std_CTypVoidPtr, kvar_loc) }]
                val vtbl_ctyp = CTypRawArray([CTypStatic, CTypConst], std_CTypVoidPtr)
                val vtbl_init_exp = CExpInit(mptrs, (vtbl_ctyp, kvar_loc))
                val vtbl = gen_idc(cm_idx, "vtbl")
                val (_, init_ccode) = create_cdefval(vtbl, vtbl_ctyp, default_tempval_flags(),
                                             "", Some(vtbl_init_exp), init_ccode, kvar_loc)
                val iface = match get_kinterface_opt(KTypName(iname), kvar_loc) {
                    | Some(iface) => iface
                    | _ => throw compile_err(kvar_loc,
                        "cgen: cannot find information about interface '{idk2str(iname, kvar_loc)}'")
                    }
                val pair = CExpInit([make_int_exp(0, kvar_loc),
                            make_id_exp(vtbl, kvar_loc)], (entry_ctyp, kvar_loc))
                (init_ccode, iface->ki_id :: ids, pair :: pairs, all_ids.add(iface->ki_id))
            }
            // extend the set of pairs (interface_id, vtbl) to the parent interfaces
            for (iname, _) <- kvar_ifaces, pair <- pairs.rev() {
                var iname = iname
                while iname != noid {
                    val (iface_id, parent) = match get_kinterface_opt(KTypName(iname), kvar_loc) {
                        | Some(iface) => (iface->ki_id, iface->ki_base)
                        | _ => (noid, noid)
                        }
                    if !all_ids.mem(iface_id) {
                        ids = iface_id :: ids
                        pairs = pair :: pairs
                        all_ids = all_ids.add(iface_id)
                    }
                    iname = parent
                }
            }
            val ifaces_ctyp = CTypName(get_id("fx_ifaces_t"))
            val ifaces_id = gen_idc(cm_idx, pp(kvar_name) + "_ifaces")
            val ifaces_cname = kvar_cname + "_ifaces"
            val cv_flags = default_val_flags().{val_flag_global=kvar_scope, val_flag_mutable=true}
            val (ifaces_id_exp, decl_ifaces_id) = create_cdefval(ifaces_id, ifaces_ctyp,
                                cv_flags, ifaces_cname, None, [], kvar_loc)
            val entries_init_exp = CExpInit(pairs.rev(), (entries_ctyp, kvar_loc))
            val iface_entries = gen_idc(cm_idx, "ifaces_entries")
            val (iface_entries_exp, init_ccode) =
                create_cdefval(iface_entries, entries_ctyp, default_tempval_flags(),
                               "", Some(entries_init_exp), init_ccode, kvar_loc)
            val ids_ctyp = CTypRawArray([CTypConst], CTypCInt)
            val ids_init_exp = CExpInit([for i <- ids.rev() {
                    make_id_t_exp(i, CTypCInt, kvar_loc)
                }], (ids_ctyp, kvar_loc))
            val ifaces_ids = gen_idc(cm_idx, "ifaces_ids")
            val (ids_exp, init_ccode) = create_cdefval(ifaces_ids, ids_ctyp,
                default_tempval_flags(), "", Some(ids_init_exp), init_ccode, kvar_loc)
            val init_ifaces_args =
                make_id_t_exp(free_f, std_fx_free_t, kvar_loc) ::
                make_int_exp(ids.length(), kvar_loc) :: ids_exp :: iface_entries_exp ::
                cexp_get_addr(ifaces_id_exp) :.
            val call_init_ifaces = make_call(get_id("fx_init_ifaces"), init_ifaces_args, CTypVoid, kvar_loc)
            val init_ccode = CExp(call_init_ifaces) :: init_ccode
            *ct = ct->{ct_ifaces_id = ifaces_id}
            top_fcv_decls = decl_ifaces_id + top_fcv_decls
            mod_init_calls = CStmtBlock(init_ccode.rev(), kvar_loc) :: mod_init_calls
        | _ => {}
        }
    }
    (top_fcv_decls.rev() + top_func_decls.rev(), mod_init_calls.rev(), mod_exn_data_decls.rev())
}
