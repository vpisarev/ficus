(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    try to fuse sequential loops/comprehensions inside basic blocks
*)
open Ast
open K_form

type array_stat_t = { mutable arr_nused: int; mutable arr_nused_for: int; arr_idl: (id_t*dom_t) list; arr_body: kexp_t }

let rec fuse_loops code =
    let counters = ref (Env.empty : array_stat_t Env.t) in
    let arrs = ref IdSet.empty in

    let process_atom a loc callb =
        match a with
        | Atom.Id i ->
            if not (IdSet.mem i !arrs) then ()
            else
                (match (Env.find_opt i !counters) with
                | Some(arr_stat) ->
                    arr_stat.arr_nused <- arr_stat.arr_nused+1
                | _ -> ())
        | _->()
    in
    let process_ktyp_ t loc callb = () in
    let process_callb = {
        kcb_fold_atom = Some(process_atom);
        kcb_fold_ktyp = Some(process_ktyp_);
        kcb_fold_kexp = None;
        kcb_fold_result = 0
    } in
    let process_idl idl = List.iter (fun (_, dom) ->
        match dom with
        | Domain.Elem(Atom.Id col) ->
            (match (Env.find_opt i !counters) with
            | Some(arr_stat) ->
                arr_stat.arr_nused_for <- arr_stat.arr_nused_for+1
            | _ -> ())
        | _ -> ()) idl
        in
    let _ = List.iter (fun e ->
        fold_kexp e process_callb;
        match e with
        | KDefVal(i, KExpMap ((_, idl, []) :: [], body, _, ((KTypArray _), _)), loc) ->
            if (K_deadcode_elim.pure_kexp body) then
                (arrs := IdSet.add i !arrs;
                counters := Env.add i {arr_nused = 0; arr_nused_for = 0; arr_idl=idl; arr_body=body} !counters)
            else ()
        | KExpFor(idl, _, _, _, _) -> process_idl idl
        | KExpMap(e_idl_i_l, _, _, _) -> List.iter (fun (_, idl, _) -> process_idl idl) e_idl_i_l
        | _ -> ()) code
    in
    let fuse_ktyp_ t loc callb = t in
    let fuse_kexp_ e callb =
        let e = walk_kexp e callb in
        match e with
        | KExpSeq(...) ->
        | KExpFor(...)

        | KDefVal(i, KExpMap (_, body, _, ((KTypArray _), _)), loc) ->
            if (K_deadcode_elim.pure_kexp body) then
                arrs := IdSet.add i !arrs
            else ()
        | KExpFor(idl, _, _, _, _) -> process_idl idl
        | KExpMap(e_idl_i_l, _, _, _) -> List.iter (fun (_, idl, _) -> process_idl idl) e_idl_i_l
        | _ -> ()

        (*| KExpMap of (kexp_t * (id_t * dom_t) list * id_t list) list * kexp_t * for_flag_t list * kctx_t
        | KExpFor of (id_t * dom_t) list * id_t list * kexp_t * for_flag_t list * loc_t*)
        | _ -> ()
    | ExpSeq(elist, (_, loc)) ->
        let new_rcode = flatten_ elist callb in
        rcode2kexp new_rcode loc
    | KExpIf(c, then_e, else_e, ((_, loc) as kctx)) ->
        let (c, code) = try_flatten c [] callb in
        let then_e = flatten_kexp_ then_e callb in
        let else_e = flatten_kexp_ else_e callb in
        let new_if = KExpIf(c, then_e, else_e, kctx) in
        rcode2kexp (new_if :: code) loc
    | KExpMatch _ ->
        (* just call walk_kexp to flatten all
            the nested expressions with 1 line of code *)
        let e = walk_kexp e callb in
        (* now let's move up all but the last one
           expressions in the very first check of the
           very first match case.
           That's all we can do here *)
        (match e with
        | KExpMatch(((c0 :: crest0), e0) :: other_cases, ((_, loc) as kctx)) ->
            let (c0, code) = try_flatten c0 [] callb in
            let new_match = KExpMatch(((c0 :: crest0), e0) :: other_cases, kctx) in
            rcode2kexp (new_match :: code) loc
        | _ -> e)
    | KExpMap _ ->
        (* just call walk_kexp to flatten all
        the nested expressions with 1 line of code *)
        let e = walk_kexp e callb in
        (* now let's move up all but the last one
           expressions in the very first check of the
           very first match case.
           That's all we can do here *)
        (match e with
        | KExpMap((e0, for_iters0, at_ids0) :: other_map_clauses, body, flags, ((_, loc) as kctx)) ->
            let code = List.rev (kexp2code e0) in
            let new_map = KExpMap(((KExpNop (get_kexp_loc e0)), for_iters0, at_ids0) ::
                other_map_clauses, body, flags, kctx) in
            rcode2kexp (new_map :: code) loc
        | _ -> e)
    | KExpDoWhile(body, c, loc) ->
        let body = flatten_kexp_ body callb in
        let body_code = List.rev (kexp2code body) in
        let (c, body_code) = try_flatten c body_code callb in
        let body = rcode2kexp body_code loc in
        KExpDoWhile(body, c, loc)
    | KDefVal(n, e, loc) ->
        let (e, code) = try_flatten e [] callb in
        let new_defval = KDefVal(n, e, loc) in
        rcode2kexp (new_defval :: code) loc
    | _ -> walk_kexp e callb
and try_flatten e code callb =
    let new_e = flatten_kexp_ e callb in
    match new_e with
    | KExpSeq([], (_, loc)) -> ((KExpNop loc), code)
    | KExpSeq((e :: []), _) -> (e, code)
    | KExpSeq(((e :: rest) as nested_list), _) ->
        let rnested = List.rev nested_list in
        ((List.hd rnested), ((List.tl rnested) @ code))
    | _ -> (new_e, code)
and flatten_ code callb =
    List.fold_left (fun code e ->
        let new_e = flatten_kexp_ e callb in
        match new_e with
        | KExpSeq(nested_elist, _) ->
            (List.rev nested_elist) @ code
        | _ -> new_e :: code) [] code
    let callb =
    {
        kcb_typ=Some(flatten_ktyp_);
        kcb_exp=Some(flatten_kexp_);
        kcb_atom=None
    }
    in List.rev (flatten_ top_code callb)
