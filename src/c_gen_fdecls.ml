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
    let top_func_decl = ref ([]: cstmt_t list) in
    List.iter (fun e -> match e with
        | KDefFun kf ->
            let {kf_name; kf_cname; kf_typ; kf_args; kf_flags; kf_closure=(kf_fv_arg, _); kf_body; kf_scope; kf_loc} = !kf in
            let (argtyps, rt) = match kf_typ with
                | KTypFun(argtyps, rt) -> (argtyps, rt)
                | _ -> ([], kf_typ)
                in
            let cbody = match kf_body with KExpCCode _ -> true | _ -> false in
            let (args, argctyps) = List.fold_left2 (fun (args, argctyps) arg t ->
                let arg = if arg = (get_orig_id arg) then (dup_idc arg) else arg in
                let cname = if cbody then pp_id2str arg else "" in
                let ctyp = C_gen_types.ktyp2ctyp t kf_loc in
                let {ktp_pass_by_ref} = K_annotate_types.get_ktprops t kf_loc in
                let ctyp = if ktp_pass_by_ref then
                    (match ctyp with
                    | CTypArray _ -> (make_ptr ctyp)
                    | _ -> (make_const_ptr ctyp)) else ctyp in
                add_farg arg ctyp cname kf_scope kf_loc;
                ((arg :: args), (ctyp :: argctyps))) ([], []) kf_args argtyps
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
            let (fv_arg, fv_cname) = if kf_fv_arg = noid then
                    ((gen_temp_idc "fx_fv"), "fx_fv")
                else
                    (kf_fv_arg, "fx_fv")
                in
            let (args, argctyps) =
                if List.mem FunConstr kf_flags then
                    (args, argctyps)
                else
                    (add_farg fv_arg std_CTypVoidPtr fv_cname kf_scope kf_loc;
                    ((fv_arg :: args), (std_CTypVoidPtr :: argctyps)))
                in
            let cf = ref {cf_name=kf_name; cf_typ=CTypFun((List.rev argctyps), new_crt);
                cf_args=(List.rev args); cf_cname=kf_cname; cf_body=[];
                cf_flags=kf_flags; cf_scope=kf_scope; cf_loc=kf_loc } in
            set_idc_entry kf_name (CFun cf);
            top_func_decl := (CDefFun cf) :: !top_func_decl
        | _ -> ()) top_code;
    List.rev !top_func_decl
