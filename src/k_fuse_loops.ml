(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    try to fuse sequential loops/comprehensions inside basic blocks.
    [TODO] handle nested-for array comprehensions.
       Currently only single for's are supported (even though they can be over multi-dimensional arrays)
*)
open Ast
open K_form

type array_stat_t = { mutable arr_nused: int; mutable arr_nused_for: int; mutable arr_idl: (id_t*dom_t) list;
                      mutable arr_body: kexp_t; arr_map_flags: for_flag_t list }

let rec fuse_loops code =
    let counters = ref (Env.empty : array_stat_t Env.t) in

    let process_atom a loc callb =
        match a with
        | Atom.Id i ->
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
    let process_idl inside_for idl = List.iter (fun (_, dom) ->
        match dom with
        | Domain.Elem(Atom.Id col) ->
            (match (Env.find_opt col !counters) with
            | Some(arr_stat) ->
                let is_parallel = List.mem ForParallel arr_stat.arr_map_flags in
                if inside_for && is_parallel then () else
                    arr_stat.arr_nused_for <- arr_stat.arr_nused_for + 1
            | _ -> ())
        | _ -> ()) idl
        in
    let _ = List.iter (fun e ->
        fold_kexp e process_callb;
        match e with
        | KDefVal(i, KExpMap ((_, idl, []) :: [], body, flags, ((KTypArray _), _)), loc) ->
            if (K_deadcode_elim.pure_kexp body) then
                (counters := Env.add i {arr_nused = 0; arr_nused_for = 0;
                    arr_idl=idl; arr_body=body; arr_map_flags=flags} !counters;
                process_idl false idl)
            else ()
        | KExpFor(idl, [], _, _, _) -> process_idl true idl
        | KExpMap((_, idl, []) :: [], _, _, _) -> process_idl false idl
        | _ -> ()) code
    in
    let arrs_to_fuse = Env.filter (fun i ainfo ->
        match ainfo with
        | {arr_nused = 1; arr_nused_for = 1} -> true
        | _ -> false
        ) !counters
    in
    let fuse_for idl body loc =
        let (arr_env, a2f) = List.fold_left (fun (arr_env, a2f) (i, dom) ->
                match dom with
                | Domain.Elem (Atom.Id arr) ->
                    let arr_env = Env.add arr i arr_env in
                    (match (Env.find_opt arr arrs_to_fuse) with
                    | Some(ainfo) -> arr_env, ((arr, ainfo) :: a2f)
                    | _ -> arr_env, a2f)
                | _ -> (arr_env, a2f)) ((Env.empty : id_t Env.t), []) idl in
        let (new_idl, pbody, _) = List.fold_left (fun (new_idl, pbody, arr_env) (i, dom) ->
            match dom with
            | Domain.Elem (Atom.Id arr) ->
                (match (List.find_opt (fun (arr2, _) -> arr = arr2) a2f) with
                | Some((_, {arr_idl; arr_body})) ->
                    let (new_idl2, pbody2, arr_env2) =
                        List.fold_left (fun (new_idl2, pbody2, arr_env2) (nested_i, nested_dom) ->
                        match nested_dom with
                        | Domain.Elem (Atom.Id nested_arr) ->
                            (match (Env.find_opt nested_arr arr_env) with
                            | Some(outer_i) ->
                                let t = get_idk_ktyp outer_i loc in
                                let pbody2 = create_kdefval nested_i t [ValTemp]
                                    (Some (KExpAtom((Atom.Id outer_i), (t, loc)))) pbody2 loc in
                                (new_idl2, pbody2, arr_env2)
                            | _ ->
                                let arr_env2 = Env.add nested_arr nested_i arr_env2 in
                                (((nested_i, nested_dom):: new_idl2), pbody2, arr_env2))
                        | _ -> (((nested_i, nested_dom):: new_idl2), pbody2, arr_env2))
                        (new_idl, pbody, arr_env) arr_idl
                        in
                    let t = get_kexp_typ arr_body in
                    let pbody2 = create_kdefval i t [ValTemp] (Some arr_body) pbody2 loc in
                    (new_idl2, pbody2, arr_env2)
                | _ ->
                    let arr_env = Env.add arr i arr_env in
                    (((i, dom):: new_idl), pbody, arr_env))
            | _ -> (((i, dom):: new_idl), pbody, arr_env))
            ([], [], arr_env) idl in
        let new_body = rcode2kexp (body :: pbody) loc in
        (new_idl, new_body)
    in
    let fuse_ktyp_ t loc callb = t in
    let fuse_kexp_ e callb =
        let e = walk_kexp e callb in
        match e with
        | KExpSeq(elist, (t, loc)) -> code2kexp (fuse_loops elist) loc
        | KDefVal(i, KExpMap ((e0, idl, idxs) :: [], body, _, _), loc) ->
            (
                match (Env.find_opt i arrs_to_fuse) with
                | Some (ainfo) ->
                    ainfo.arr_idl <- idl;
                    ainfo.arr_body <- body;
                    KExpNop(loc)
                | _ -> e
            )
        | KExpFor(idl, [], body, flags, loc) ->
            let (new_idl, new_body) = fuse_for idl body loc in
            KExpFor ((List.rev new_idl), [], new_body, flags, loc)
        | KExpMap((e0, idl, []) :: [], body, flags, (map_result_type, loc)) ->
            let (new_idl, new_body) = fuse_for idl body loc in
            KExpMap ((e0, (List.rev new_idl), []) :: [], new_body, flags, (map_result_type, loc))
        | _ -> e
        in
    let fuse_callb =
    {
        kcb_ktyp=Some(fuse_ktyp_);
        kcb_kexp=Some(fuse_kexp_);
        kcb_atom=None
    }
    in List.map (fun e -> fuse_kexp_ e fuse_callb) code
