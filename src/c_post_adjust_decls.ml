(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    separates declarations from initialization in some cases, e.g.

    {
    if (foo(x, y) < 0) goto catch;
    int a = 5;
    ...
  catch:
    ...
    }

    =>

    {
    int a;
    if (foo(x, y) < 0) goto catch;
    a = 5;
    ...
  catch:
    ...
    }

    this is necessary to shutup C++ compiler that reports error in such case,
    even though C compiler processes the same code just fine.
    the extra trick is applied not to retain the array initializations as-is (we
    take care in c_gen_code.ml that such array initializations are enclosed into dedicated
    code blocks without jumps in the middle) and to retain declarations+initializations
    in the beginning of each code block.
*)

open Ast
open K_form
open C_form

let adjust_decls_ cmod =
    let {cmod_ccode} = cmod in
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
        | CDefVal _ | CDefForwardSym _ | CDefForwardTyp _ | CDefTyp _
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
    let ccode = List.map (fun s -> adjust_cstmt s adjust_callb) cmod_ccode in
    {cmod with cmod_ccode=ccode}

let adjust_decls cmods =
    List.map adjust_decls_ cmods
