(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    Generates declarations of C functions
*)

open Ast
open K_form
open C_form

let convert_all_fdecls top_code =
    let top_fcv_decls = ref ([]: cstmt_t list) in
    let top_func_decls = ref ([]: cstmt_t list) in
    List.iter (fun e -> match e with
        | KDefFun kf ->
            let {kf_name; kf_cname; kf_args; kf_rt=rt; kf_closure;
                kf_flags; kf_body; kf_scope; kf_loc} = !kf in
            let {kci_arg} = kf_closure in
            let is_ccode_func = match kf_body with KExpCCode _ -> true | _ -> false in
            let ctor = get_fun_ctor kf_flags in
            let (_, args) = List.fold_left (fun (arg_idx, args) (arg, t) ->
                let arg = match arg with
                    | Id.Name _ -> dup_idc arg
                    | _ -> arg
                    in
                let cname = if is_ccode_func then pp_id2str arg else
                    if ctor <> CtorNone then (sprintf "arg%d" arg_idx) else "" in
                let ctyp = C_gen_types.ktyp2ctyp t kf_loc in
                let {ktp_pass_by_ref} = K_annotate_types.get_ktprops t kf_loc in
                let (ctyp, arg_flags) = if ktp_pass_by_ref then
                    (match ctyp with
                    | CTypArray _ -> ((make_ptr ctyp), [CArgPassByPtr])
                    | _ -> ((make_const_ptr ctyp), [CArgPassByPtr])) else (ctyp, []) in
                add_cf_arg arg ctyp cname kf_scope kf_loc;
                (arg_idx+1, (arg, ctyp, arg_flags) :: args)) (0, []) kf_args
                in
            let {ktp_scalar=rt_scalar} = K_annotate_types.get_ktprops rt kf_loc in
            let crt = C_gen_types.ktyp2ctyp rt kf_loc in
            let is_nothrow = List.mem FunNoThrow kf_flags in
            let (new_crt, args) =
                if is_nothrow && rt_scalar then
                    (crt, args)
                else if crt = CTypVoid then
                    (CTypCInt, args)
                else
                    let v = gen_temp_idc "fx_result" in
                    let crt = make_ptr crt in
                    (add_cf_arg v crt "fx_result" kf_scope kf_loc;
                    ((if is_nothrow then CTypVoid else CTypCInt), (v, crt,
                        [CArgPassByPtr; CArgRetVal]) :: args))
                in
            let args =
                if ctor <> CtorNone then
                    args
                else
                    let fv_cname = "fx_fv" in
                    let fv_arg = gen_temp_idc fv_cname in
                    let _ = add_cf_arg fv_arg std_CTypVoidPtr fv_cname kf_scope kf_loc in
                    (fv_arg, std_CTypVoidPtr, [CArgFV]) :: args
                in
            let flags = kf_flags in
            let flags = if kci_arg = noid then flags else FunUseFV :: flags in
            let cf = ref {cf_name=kf_name; cf_rt=new_crt;
                cf_args=(List.rev args); cf_cname=kf_cname; cf_body=[];
                cf_flags=flags; cf_scope=kf_scope; cf_loc=kf_loc } in
            set_idc_entry kf_name (CFun cf);
            top_func_decls := (CDefFun cf) :: !top_func_decls
        | KDefClosureVars kcv ->
            let {kcv_name; kcv_cname; kcv_freevars; kcv_loc} = !kcv in
            let fcv_typ = make_ptr (CTypName kcv_name) in
            let dst_id = get_id "dst" in
            let dst_exp = make_id_t_exp dst_id fcv_typ kcv_loc in
            let relems = ((get_id "free_f"), (CTypName (get_id "fx_free_t"))) :: ((get_id "rc"), CTypInt) :: [] in
            let (_, relems, free_ccode) =
                List.fold_left(fun (idx, relems, free_ccode) (n, kt) ->
                    let ctyp = C_gen_types.ktyp2ctyp kt kcv_loc in
                    let c_id = get_id ("t" ^ (string_of_int idx)) in
                    let elem_exp = cexp_arrow dst_exp c_id ctyp in
                    let free_ccode = C_gen_types.gen_free_code elem_exp ctyp true false free_ccode kcv_loc in
                    (idx+1, ((c_id, ctyp) :: relems), free_ccode))
                        (0, relems, []) kcv_freevars
                in
            let (free_f, decl_free_f) = if free_ccode = [] then (!std_fx_free, []) else
                let call_free = make_call !std_fx_free [dst_exp] CTypVoid kcv_loc in
                let freecode = (CExp call_free) :: free_ccode in
                let free_f = gen_temp_idc "free_cv" in
                let free_f_cname = "_fx_free_" ^ (K_mangle.remove_fx
                    (Utils.trim_right kcv_cname (String.length "_cldata_t"))) in
                let cf = ref {
                    cf_name=free_f; cf_rt=CTypVoid;
                    cf_args=[(dst_id, fcv_typ, [CArgPassByPtr])];
                    cf_cname=free_f_cname;
                    cf_body=(List.rev freecode);
                    cf_flags=[FunNoThrow]; cf_scope=[ScGlobal]; cf_loc=kcv_loc } in
                let _ = set_idc_entry free_f (CFun cf) in
                (free_f, [CDefFun cf])
                in
            let ct = ref { ct_name=kcv_name; ct_typ = CTypStruct(None, (List.rev relems));
                ct_ktyp=KTypName kcv_name; ct_cname=kcv_cname; ct_tagenum=noid; ct_data_start=2;
                ct_scope=ScGlobal::[]; ct_loc=kcv_loc;
                ct_props={
                    ctp_scalar=false; ctp_complex=true;
                    ctp_ptr=false; ctp_pass_by_ref=true;
                    ctp_make=[]; ctp_free=(noid, free_f); ctp_copy=(noid, noid)
                    }
                } in
            let _ = set_idc_entry kcv_name (CTyp ct) in
            let decl_fcvt = [CDefTyp ct] in
            top_fcv_decls := decl_free_f @ decl_fcvt @ !top_fcv_decls
        | KDefExn ke -> ()
            (*
                exception MyException[: (int, string)]

                // 1) simple exceptions
                static int _FX_E11MyException = 0;
                static fx_exn_info_t _FX_E11MyException_info;
                static fx_exn_t _fx_E11MyException;
                ...
                int fx_toplevel() {
                    ...
                    fx_register_simple_exn("CurrModule.MyException", &_FX_E11MyException, &_FX_E11MyException_info, &_fx_E11MyException);
                    // 1. sets _FX_E11MyException to global exn counter and decrements global exn counter (e.g. -1100 => -1101)
                    // 2. sets _FX_E11MyException_info to {name="CurrModule.MyException", free_f=0, to_string=0, print_repr=0}
                    // 3. sets _fx_E11MyException = {_FX_E11MyException, &_FX_E11MyException_info, 0}.
                }

                `throw MyException` will do the following
                `FX_THROW(_FX_E11MyException, false, label)`

                // 2) complex exceptions
                static int _FX_E11MyException = 0;
                static fx_exn_info_t _FX_E11MyException_info;

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

                `throw MyException(3, "abc")` will be converted at K-normalization to

                `TEMP val e = MyException(3, "abc")
                throw e`,

                which will be converted to
                fx_exn_t e = {};
                fx_str_t abc_str = FX_MAKE_STR("abc");
                FX_CALL(_fx_make_E11MyException(3, &abc_str, &e), catch_label);
                FX_THROW(e, true, catch_label);

                fx_register_exn(name, tag, info_ptr, free_f, to_string, print_f);
            *)
        | _ -> ()) top_code;
    (List.rev !top_fcv_decls) @ (List.rev !top_func_decls)
