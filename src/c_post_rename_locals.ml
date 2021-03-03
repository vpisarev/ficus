(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*  rename the local variables in each generated C function so that
    the function body does not change if neither its .fx source was changed
    nor the source of inline functions it calls. It's a crucial feature
    for separate compilation *)

open Ast
open K_form
open C_form

module Intmap = Map.Make(struct type t = int let compare = compare end)

let rename_locals_ cmod =
    let {cmod_ccode} = cmod in
    let prefix_hash = ref (Intmap.empty : int Intmap.t) in
    let gen_cname n =
        let prefix = match n with
            | Id.Name(i) -> i
            | Id.Val(i, j) -> i
            | Id.Temp(i, j) -> i
            in
        let j1 = match Intmap.find_opt prefix !prefix_hash with
            | Some(j) -> j+1
            | _ -> 0
            in
        let _ = prefix_hash := Intmap.add prefix j1 !prefix_hash in
        let prefix = dynvec_get all_strings prefix in
        sprintf "%s_%d" prefix j1
        in
    let gen_cval_cname n loc =
        match n with
        | Id.Name _ -> ()
        | _ ->
            (match cinfo_ n loc with
            | CVal cv ->
                let { cv_cname } = cv in
                if cv_cname <> "" then () else
                    let new_cname = gen_cname n in
                    set_idc_entry n (CVal {cv with cv_cname=new_cname})
            | _ -> raise_compile_err loc (sprintf "invalid id info for '%s'; it must be CVal" (id2str n)))
        in
    let rename_cstmt s callb =
        match s with
        | CDefVal(t, n, e_opt, loc) when
            (match n with Id.Name _ -> false | _ -> true) ->
            (match e_opt with
            | Some e -> fold_cexp e callb
            | _ -> ());
            gen_cval_cname n loc
        | CStmtLabel(n, loc) when (match n with Id.Name _ -> false | _ -> true) ->
            (match cinfo_ n loc with
            | CLabel cl ->
                let { cl_cname } = cl in
                if cl_cname <> "" then () else
                    let new_cname = gen_cname n in
                    set_idc_entry n (CLabel {cl with cl_cname=new_cname})
            | _ -> raise_compile_err loc (sprintf "invalid id info for '%s'; it must be CLabel" (id2str n)))
        | CStmtFor((Some t), decls, _, _, _, loc) ->
            List.iter (fun e ->
                match e with
                | CExpBinary(COpAssign, CExpIdent(n, _), _, _) -> gen_cval_cname n loc
                | _ -> ()) decls;
            fold_cstmt s callb
        | CDefFun cf ->
            let { cf_args; cf_loc } = !cf in
            let saved_hash = !prefix_hash in
            prefix_hash := Intmap.empty;
            List.iter (fun (argname, _, _) -> gen_cval_cname argname cf_loc) cf_args;
            fold_cstmt s callb;
            prefix_hash := saved_hash
        | _ -> fold_cstmt s callb
        in
    let rename_callb =
    {
        ccb_fold_ident = None;
        ccb_fold_typ = None;
        ccb_fold_exp = None;
        ccb_fold_stmt = Some(rename_cstmt);
        ccb_fold_result = 0;
    } in
    List.iter (fun s -> rename_cstmt s rename_callb) cmod_ccode

let rename_locals cmods =
    List.iter rename_locals_ cmods;
    cmods
