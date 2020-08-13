(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    try to move loop invariants outside of the loops
*)
open Ast
open K_form

let rec move_loop_invs code =
    let curr_inloop = ref IdSet.empty in
    let curr_moved = ref ([]: kexp_t list) in

    let rec is_loop_invariant e =
        let isinv = ref true in
        let isinv_atom_ a loc callb =
            match a with
            | Atom.Id i ->
                if  (is_mutable i loc) ||
                    not (is_ktyp_scalar (get_idk_ktyp i loc)) ||
                    (IdSet.mem i !curr_inloop) then isinv := false else ()
            | _ -> ()
            in
        let isinv_ktyp_ t loc callb = () in
        let rec isinv_kexp_ e callb =
            if !isinv then fold_kexp e callb else () in
        let isinv_callb =
        {
            kcb_fold_atom = Some(isinv_atom_);
            kcb_fold_ktyp = Some(isinv_ktyp_);
            kcb_fold_kexp = Some(isinv_kexp_);
            kcb_fold_result = 0
        } in
        (K_deadcode_elim.pure_kexp e) && (isinv_kexp_ e isinv_callb; !isinv)
    in
    let mli_ktyp t loc callb = t in
    let rec mli_process_loop e_idl_l body loc callb =
        let (_, inloop) = used_decl_by_kexp body in
        let saved_inloop = !curr_inloop in
        let saved_moved = !curr_moved in
        let (outer_moved, new_e_idl_l, new_body, _) =
            List.fold_left (fun (nested_elist, e_idl_l, body, inloop) (pre_e, idl, idxl) ->
                let (_, in_pre) = used_decl_by_kexp pre_e in
                let inloop = List.fold_left (fun inloop (i, _) -> IdSet.add i inloop) inloop idl in
                let inloop = List.fold_left (fun inloop i -> IdSet.add i inloop) inloop idxl in
                let _ = curr_inloop := inloop in
                let _ = curr_moved := [] in
                let nested_e = mli_kexp (code2kexp nested_elist loc) callb in
                let new_elist = (kexp2code pre_e) @ (List.rev !curr_moved) in
                let new_inloop = IdSet.union inloop in_pre in
                let (e_idl_l, new_body) =
                    match e_idl_l with
                    | (_, prev_idl, prev_idxl) :: rest ->
                        (((nested_e, prev_idl, prev_idxl) :: rest), body)
                    | _ ->
                        ([], nested_e)
                    in
                let new_e_idl_l = ((KExpNop loc), idl, idxl) :: e_idl_l in
                (new_elist, new_e_idl_l, new_body, new_inloop))
            ((kexp2code body), [], (KExpNop loc), inloop) (List.rev e_idl_l)
            in
        let _ = curr_inloop := saved_inloop in
        let _ = curr_moved := saved_moved in
        (outer_moved, new_e_idl_l, new_body)
    and mli_kexp e callb =
        let e = walk_kexp e callb in
        match e with
        | KExpFor(idl, idxl, body, flags, loc) ->
            let (outer_moved, _, body) = mli_process_loop [((KExpNop loc), idl, idxl)] body loc callb in
            code2kexp (outer_moved @ [KExpFor(idl, idxl, body, flags, loc)]) loc
        | KExpMap (e_idl_l, body, flags, (t, loc)) ->
            let (outer_moved, e_idl_l, body) = mli_process_loop e_idl_l body loc callb in
            code2kexp (outer_moved @ [KExpMap(e_idl_l, body, flags, (t, loc))]) loc
        | KExpWhile(c, body, loc) ->
            let (outer_moved_c, _, c) = mli_process_loop [((KExpNop loc), [], [])] c loc callb in
            let (outer_moved, _, body) = mli_process_loop [((KExpNop loc), [], [])] body loc callb in
            code2kexp (outer_moved_c @ outer_moved @ [KExpWhile(c, body, loc)]) loc
        | KExpDoWhile(body, c, loc) ->
            let (outer_moved, _, body) = mli_process_loop [((KExpNop loc), [], [])] body loc callb in
            let (outer_moved_c, _, c) = mli_process_loop [((KExpNop loc), [], [])] c loc callb in
            code2kexp (outer_moved @ outer_moved_c @ [KExpDoWhile(body, c, loc)]) loc
        | KDefVal(n, rhs, loc) ->
            if not (IdSet.mem n !curr_inloop) ||
               (is_mutable n loc) ||
               not (is_loop_invariant rhs) then e
            else
            (
                curr_moved := e :: !curr_moved;
                KExpNop loc
            )
        | _ -> walk_kexp e callb
    in let mli_callb =
    {
        kcb_typ=Some(mli_ktyp);
        kcb_exp=Some(mli_kexp);
        kcb_atom=None
    }
    in List.map (fun e -> mli_kexp e mli_callb) code
