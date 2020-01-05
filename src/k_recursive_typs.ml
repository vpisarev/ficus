(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    Finds out all the recursive types defined in the code.
    Also, for each type collects the list of all other types that it references,
    directly or indirectly.

    The algorithm is similar to one used in k_lift.ml:
        * we start with the initial sets of dependencies for each type
        * and then we recursively find closures of those sets
*)

open Ast
open K_form

let get_typ_dependencies n loc =
    let rec get_ktyp_deps_ t deps =
        match t with
        | KTypInt | KTypSInt _ | KTypUInt _  | KTypFloat _
        | KTypVoid | KTypNil | KTypBool | KTypChar | KTypString
        | KTypCPointer | KTypExn | KTypErr | KTypModule -> deps
        | KTypFun (args, rt) ->
            List.fold_left (fun deps t -> get_ktyp_deps_ t deps) deps (rt :: args)
        | KTypTuple tl ->
            List.fold_left (fun deps t -> get_ktyp_deps_ t deps) deps tl
        | KTypClosure (ft, cl_id) ->
            let deps = get_ktyp_deps_ ft deps in
            if cl_id = noid then deps else IdSet.add cl_id deps
        | KTypRecord (rn, relems) ->
            (* skip rn, because in this particular case it's not a real dependency *)
            List.fold_left (fun deps (_, ti) -> get_ktyp_deps_ ti deps) deps relems
        | KTypName i -> IdSet.add i deps
        | KTypArray (_, et) -> get_ktyp_deps_ et deps
        | KTypList et -> get_ktyp_deps_ et deps
        | KTypRef et -> get_ktyp_deps_ et deps
    in
    match (kinfo_ n loc) with
    | KRecord {contents={krec_elems}} -> List.fold_left (fun deps (_, ti) -> get_ktyp_deps_ ti deps) IdSet.empty krec_elems
    | KVariant {contents={kvar_cases}} -> List.fold_left (fun deps (_, ti) -> get_ktyp_deps_ ti deps) IdSet.empty kvar_cases
    | KGenTyp {contents={kgen_typ}} -> get_ktyp_deps_ kgen_typ IdSet.empty
    | _ -> raise_compile_err loc (sprintf "the symbol '%s' is not a type" (id2str n))

let find_recursive top_code =
    let dep_env = ref (Env.empty : IdSet.t ref Env.t) in
    let fold_deps0_ktyp_ t loc callb = () in
    let fold_deps0_kexp_ e callb =
        match e with
        | KDefRecord {contents={krec_name; krec_loc}} ->
            let deps = get_typ_dependencies krec_name krec_loc in
            dep_env := Env.add krec_name (ref deps) !dep_env
        | KDefVariant {contents={kvar_name; kvar_loc}} ->
            let deps = get_typ_dependencies kvar_name kvar_loc in
            dep_env := Env.add kvar_name (ref deps) !dep_env
        | KDefGenTyp {contents={kgen_name; kgen_loc}} ->
            let deps = get_typ_dependencies kgen_name kgen_loc in
            dep_env := Env.add kgen_name (ref deps) !dep_env
        | _ -> fold_kexp e callb
    in let deps0_callb =
    {
        kcb_fold_atom = None;
        kcb_fold_ktyp = Some(fold_deps0_ktyp_);
        kcb_fold_kexp = Some(fold_deps0_kexp_);
        kcb_fold_result = 0
    } in
    (* for each type find the initial set of its dependencies *)
    let _ = List.iter (fun e -> fold_deps0_kexp_ e deps0_callb) top_code in

    (* now expand those sets. recursively add to the list of free variables
       all the free variables from the called functions
       (but not defined locally) *)
    let rec finalize_deps iters all_typs =
        let visited_typs = ref IdSet.empty in
        let changed = ref false in
        let _ = if iters > 0 then () else raise_compile_err noloc
            "finalization of the defined types' dependency sets takes too much iterations" in
        let rec update_deps n =
            match (Env.find_opt n !dep_env) with
            | Some rdeps ->
                if IdSet.mem n !visited_typs then !rdeps
                else
                    let _ = visited_typs := IdSet.add n !visited_typs in
                    let size0 = IdSet.cardinal !rdeps in
                    let upd_deps = IdSet.fold (fun d deps ->
                        if d = n then deps else
                            let ddeps = update_deps d in
                            IdSet.union deps ddeps) (!rdeps) (!rdeps) in
                    let size1 = IdSet.cardinal upd_deps in
                    let _ = if size1 = size0 then () else
                        (rdeps := upd_deps; changed := true) in
                    upd_deps
            | _ -> IdSet.empty
        in List.iter (fun n -> ignore (update_deps n)) all_typs;
        if not !changed then (iters-1) else finalize_deps (iters - 1) (List.rev all_typs)
        in
    let iters0 = 10 in
    let all_typs = List.rev (Env.fold (fun n _ all_typs -> n :: all_typs) !dep_env []) in
    let _ = finalize_deps iters0 all_typs in
    let is_recursive n = match (Env.find_opt n !dep_env) with
        | Some rdeps -> IdSet.mem n !rdeps
        | _ -> false in
    List.iter (fun n ->
        let isrec = is_recursive n in
        let info = kinfo n in
        match info with
        | KVariant kvar ->
            let {kvar_flags} = !kvar in
            if isrec then kvar := {!kvar with kvar_flags=VariantRecursive :: kvar_flags} else ()
        | _ -> ()) all_typs;
    top_code
