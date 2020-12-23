(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*  rename the local variables in each generated C function so that
    the function body does not change if neither its source was changed
    nor the source of inline functions it calls. It's crucial feature
    for separate compilation *)

open Ast
open K_form
open C_form

let adjust_decls top =
    let local_decls = ref ([]: cstmt_t list) in
    let local_have_ops = ref false in
    let rec adjust_sseq sseq callb =
        let saved_decls = !local_decls in
        let _ = local_decls := [] in
        let sseq = List.map (fun s -> adjust_cstmt s callb) sseq in
        let sseq = (List.rev !local_decls) @ sseq in
        local_decls := saved_decls;
        sseq
    and adjust_cstmt s callb =
        match s with
        | CDefVal (t, n, Some(e), loc) when
            !local_have_ops &&
            (match t with
            | CTypRawArray _ -> false
            | _ -> true) ->
            local_decls := CDefVal(t, n, None, loc) :: !local_decls;
            CExp (CExpBinOp (COpAssign, (make_id_t_exp n t loc), e, (CTypVoid, loc)))
        | CDefVal _ | CDefForwardFun _ | CDefForwardTyp _ | CDefTyp _
        | CDefEnum _ | CMacroDef _ | CMacroUndef _ | CMacroIf _
        | CMacroInclude _ | CMacroPragma _ | CExp (CExpCCode _) -> s
        | CDefFun cf ->
            let {cf_body} = !cf in
            let new_cf_body = adjust_sseq cf_body callb in
            cf := {!cf with cf_body=new_cf_body};
            local_have_ops := false; s
        | _ ->
            local_have_ops := true;
            (match s with
            | CStmtBlock (sseq, loc) ->
                let sseq = adjust_sseq sseq callb in
                CStmtBlock(sseq, loc)
            | _ -> walk_cstmt s callb)
        in
    let adjust_callb =
    {
        ccb_ident = None;
        ccb_typ = None;
        ccb_exp = None;
        ccb_stmt = Some(adjust_cstmt);
    } in
    List.map (fun s -> adjust_cstmt s adjust_callb) top
