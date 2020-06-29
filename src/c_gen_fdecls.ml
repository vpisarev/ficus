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

let add_farg v ctyp cname sc loc =
    let cv = { cv_name=v; cv_typ=ctyp; cv_cname=cname;
        cv_flags=ValArg::[]; cv_scope=sc; cv_loc=loc }
    in set_idc_entry v (CVal cv)

let convert_all_fdecls top_code =
    let top_fcv_decls = ref ([]: cstmt_t list) in
    let top_func_decls = ref ([]: cstmt_t list) in
    List.iter (fun e -> match e with
        | KDefFun kf ->
            let {kf_name; kf_cname; kf_args; kf_rt=rt; kf_flags; kf_body; kf_scope; kf_loc} = !kf in
            let cbody = match kf_body with KExpCCode _ -> true | _ -> false in
            let ctor = get_fun_ctor kf_flags in
            let (_, args, argctyps) = List.fold_left (fun (arg_idx, args, argctyps) (arg, t) ->
                let arg = match arg with
                    | Id.Name _ -> dup_idc arg
                    | _ -> arg
                    in
                let cname = if cbody then pp_id2str arg else
                    if ctor <> CtorNone then (sprintf "arg%d" arg_idx) else "" in
                let ctyp = C_gen_types.ktyp2ctyp t kf_loc in
                let {ktp_pass_by_ref} = K_annotate_types.get_ktprops t kf_loc in
                let ctyp = if ktp_pass_by_ref then
                    (match ctyp with
                    | CTypArray _ -> (make_ptr ctyp)
                    | _ -> (make_const_ptr ctyp)) else ctyp in
                add_farg arg ctyp cname kf_scope kf_loc;
                (arg_idx+1, (arg :: args), (ctyp :: argctyps))) (0, [], []) kf_args
                in
            let {ktp_scalar=rt_scalar} = K_annotate_types.get_ktprops rt kf_loc in
            let crt = C_gen_types.ktyp2ctyp rt kf_loc in
            let is_nothrow = List.mem FunNoThrow kf_flags in
            let (new_crt, args, argctyps) =
                if is_nothrow && rt_scalar then
                    (crt, args, argctyps)
                else if crt = CTypVoid then
                    (CTypCInt, args, argctyps)
                else
                    let v = gen_temp_idc "fx_result" in
                    let crt = make_ptr crt in
                    (add_farg v crt "fx_result" kf_scope kf_loc;
                    ((if is_nothrow then CTypVoid else CTypCInt), v :: args, crt :: argctyps))
                in
            let (fv_arg, fv_cname) = ((gen_temp_idc "fx_fv"), "fx_fv") in
            let (args, argctyps) =
                if ctor <> CtorNone then
                    (args, argctyps)
                else
                    (add_farg fv_arg std_CTypVoidPtr fv_cname kf_scope kf_loc;
                    ((fv_arg :: args), (std_CTypVoidPtr :: argctyps)))
                in
            let cf = ref {cf_name=kf_name; cf_typ=CTypFun((List.rev argctyps), new_crt);
                cf_args=(List.rev args); cf_cname=kf_cname; cf_body=[];
                cf_flags=kf_flags; cf_scope=kf_scope; cf_loc=kf_loc } in
            set_idc_entry kf_name (CFun cf);
            top_func_decls := (CDefFun cf) :: !top_func_decls
        | KDefClosureVars kcv ->
            let {kcv_name; kcv_cname; kcv_freevars; kcv_loc} = !kcv in
            let fcv_typ = make_ptr (CTypName kcv_name) in
            let dst_id = get_id "dst" in
            let dst_exp = make_id_t_exp dst_id fcv_typ kcv_loc in
            let relems = ((get_id "free_f"), (CTypName (get_id "fx_free_t"))) :: ((get_id "rc"), CTypInt) :: [] in
            let fcv_id = get_id "fcv" in
            let fcv_exp = make_id_t_exp fcv_id fcv_typ kcv_loc in
            let (_, relems, make_args, make_ccode, free_ccode) =
                List.fold_left(fun (idx, relems, make_args, make_ccode, free_ccode) (n, kt) ->
                    let ctyp = C_gen_types.ktyp2ctyp kt kcv_loc in
                    let c_id = get_id ("t" ^ (string_of_int idx)) in
                    let elem_exp = cexp_arrow dst_exp c_id ctyp in
                    let free_ccode = C_gen_types.gen_free_code elem_exp ctyp true false free_ccode kcv_loc in
                    let arg_id = get_id ("arg" ^ (string_of_int idx)) in
                    let arg_exp = make_id_t_exp arg_id ctyp kcv_loc in
                    let {ctp_pass_by_ref} = C_gen_types.get_ctprops ctyp kcv_loc in
                    let (arg_typ, arg_exp) = if not ctp_pass_by_ref then (ctyp, arg_exp)
                        else ((make_const_ptr ctyp), (cexp_deref arg_exp)) in
                    let dst_exp = cexp_arrow fcv_exp c_id ctyp in
                    let make_ccode = C_gen_types.gen_copy_code arg_exp dst_exp ctyp make_ccode kcv_loc in
                    (idx+1, ((c_id, ctyp) :: relems), ((arg_id, arg_typ) :: make_args), make_ccode, free_ccode))
                        (0, relems, [], [], []) kcv_freevars
                in
            let (free_f, decl_free_f) = if free_ccode = [] then (!std_fx_free, []) else
                let call_free = make_call !std_fx_free [dst_exp] CTypVoid kcv_loc in
                let freecode = (CExp call_free) :: free_ccode in
                let free_f = gen_temp_idc "free_cv" in
                let free_f_cname = "_fx_free_" ^ (K_mangle.remove_fx
                    (Utils.trim_right kcv_cname (String.length "_cldata_t"))) in
                let cf = ref {
                    cf_name=free_f; cf_typ=CTypFun(fcv_typ :: [], CTypVoid); cf_cname=free_f_cname;
                    cf_args=dst_id :: []; cf_body=(List.rev freecode);
                    cf_flags=FunNoThrow :: []; cf_scope=ScGlobal :: []; cf_loc=kcv_loc } in
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
        | _ -> ()) top_code;
    (List.rev !top_fcv_decls) @ (List.rev !top_func_decls)
