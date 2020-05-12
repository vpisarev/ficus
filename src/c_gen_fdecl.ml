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
                | _ -> ([], rt)
                in
            let argctyps = List.map (fun t -> ktyp2ctyp t kf_loc) argtyps in
            let crt = ktyp2ctyp rt kf_loc in
            let cbody = match kf_body with KExpCCode _ -> true | _ -> false in
            let _ = List.iter2 (fun arg t ->
                let cname = if cbody then pp_id2str arg else "" in
                add_farg arg t cname) kf_args argctyps
                in
            let (fv_arg, fv_cname) = if kf_fv_arg = noid then
                    ((gen_temp_idc "fx_fv_unused"), "fv_fv_unused")
                else
                    (kf_fv_arg, "fx_fv")
                in
            let _ = add_farg fv_arg std_CTypVoidPtr fv_cname kf_scope kf_loc in
            let (extra_args, extra_argctyps) = ([fv_arg], [std_CTypVoidPtr]) in
            let (new_crt, extra_args, extra_argctyps) = if (List.mem FunNoThrow kf_flags) then
                    (crt, extra_args, extra_argctyps)
                else
                    let v = gen_temp_idc "fx_result" in
                    (add_farg v crt "fx_result" kf_scope kf_loc;
                    (CTypCInt, v :: extra_args, crt :: extra_argctyps))
                in
            let (args, argctyps) = ((kf_args @ extra_args), (argctyps @ extra_argctyps)) in
            let cf = ref {cf_name=kf_name; cf_typ=CTypFunRawPtr(argctyps, new_crt);
                cf_args=args; cf_cname=kf_cname; cf_body=CStmtNop(kf_loc);
                cf_flags=kf_flags; cf_scope=kf_scope; cf_loc=kf_loc } in
            set_idc_entry cf_name (CFun cf);
            top_func_decl := (CDefFun cf) :: !top_func_decl
        | _ -> ()) top_code;
    List.rev !top_func_decl
