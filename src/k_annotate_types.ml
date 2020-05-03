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

let get_typ_deps n loc =
    let rec get_ktyp_deps_ t deps =
        match t with
        | KTypInt | KTypSInt _ | KTypUInt _  | KTypFloat _
        | KTypVoid | KTypNil | KTypBool | KTypChar | KTypString
        | KTypCPointer | KTypExn | KTypErr | KTypModule -> deps
        | KTypFun (args, rt) ->
            List.fold_left (fun deps t -> get_ktyp_deps_ t deps) deps (rt :: args)
        | KTypTuple tl ->
            List.fold_left (fun deps t -> get_ktyp_deps_ t deps) deps tl
        (*| KTypClosure (ft, cl_id) ->
            let deps = get_ktyp_deps_ ft deps in
            if cl_id = noid then deps else IdSet.add cl_id deps*)
        | KTypRecord (rn, relems) ->
            (* skip rn, because in this particular case it's not a real dependency *)
            List.fold_left (fun deps (_, ti) -> get_ktyp_deps_ ti deps) deps relems
        | KTypName i -> IdSet.add i deps
        | KTypArray (_, et) -> get_ktyp_deps_ et deps
        | KTypList et -> get_ktyp_deps_ et deps
        | KTypRef et -> get_ktyp_deps_ et deps
    in
    match (kinfo_ n loc) with
    | KVariant {contents={kvar_cases}} -> List.fold_left (fun deps (_, ti) -> get_ktyp_deps_ ti deps) IdSet.empty kvar_cases
    | KTyp {contents={kt_typ}} -> get_ktyp_deps_ kt_typ IdSet.empty
    | _ -> raise_compile_err loc (sprintf "the symbol '%s' is not a type" (id2str n))

let find_recursive top_code =
    let dep_env = ref (Env.empty : IdSet.t ref Env.t) in
    let fold_deps0_ktyp_ t loc callb = () in
    let fold_deps0_kexp_ e callb =
        match e with
        | KDefVariant {contents={kvar_name; kvar_loc}} ->
            let deps = get_typ_deps kvar_name kvar_loc in
            dep_env := Env.add kvar_name (ref deps) !dep_env
        | KDefTyp {contents={kt_name; kt_loc}} ->
            let deps = get_typ_deps kt_name kt_loc in
            dep_env := Env.add kt_name (ref deps) !dep_env
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
        if (is_recursive n) then
            match (kinfo_ n noloc) with
            | KVariant kvar ->
                let {kvar_flags} = !kvar in
                kvar := {!kvar with kvar_flags=VariantRecursive :: kvar_flags}
            | _ -> ()
        else
            ()) all_typs;
    top_code

(* returns the ktprops_t structure for each value of ktyp_t:
    1. whether the type is complex, i.e. needs any special handling (non-trivial destructor and non-trivial copy operator)
    2. whether the type is pointer type (i.e. which content is allocated on heap rather than stack).
       note that string and array are non-pointer types,
       because their headers are allocated on stack,
       even though their elements are allocated on the heap.
    3. whether the type needs custom destructor
    4. whether the type needs custom copy operator

    note that KTypString and KTypArray, for example, are complex types, but
    they do not need custom destructor or custom copy operator,
    because there are standard ones in the runtime.
*)
let get_ktprops t loc =
    let visited = ref (IdSet.empty) in
    let rec get_ktprops_ t loc =
    match t with
    | KTypInt | KTypSInt _ | KTypUInt _ | KTypFloat _
    | KTypVoid | KTypNil | KTypBool | KTypChar | KTypErr ->
        { ktp_complex=false; ktp_ptr=false; ktp_pass_by_ref=false;
          ktp_custom_free=false; ktp_custom_copy=false }
    | KTypString | KTypArray _ | KTypExn | KTypFun _ | KTypModule ->
        { ktp_complex=true; ktp_ptr=false; ktp_pass_by_ref=true;
          ktp_custom_free=false; ktp_custom_copy=false }
    | KTypCPointer ->
        { ktp_complex=true; ktp_ptr=true; ktp_pass_by_ref=false;
          ktp_custom_free=false; ktp_custom_copy=false }
    | KTypTuple(elems) ->
        let have_complex = List.exists (fun ti ->
            (get_ktprops_ ti loc).ktp_complex) elems in
        { ktp_complex=have_complex; ktp_ptr=false; ktp_pass_by_ref=true;
          ktp_custom_free=have_complex; ktp_custom_copy=have_complex }
    | KTypRecord(_, relems) ->
        let have_complex = List.exists (fun (_, ti) ->
            (get_ktprops_ ti loc).ktp_complex) relems in
        { ktp_complex=have_complex; ktp_ptr=false; ktp_pass_by_ref=true;
          ktp_custom_free=have_complex; ktp_custom_copy=have_complex }
    | KTypList et ->
        let have_complex = (get_ktprops_ et loc).ktp_complex in
        { ktp_complex=true; ktp_ptr=true; ktp_pass_by_ref=false;
          ktp_custom_free=have_complex; ktp_custom_copy=false }
    | KTypRef et ->
        let have_complex = (get_ktprops_ et loc).ktp_complex in
        { ktp_complex=true; ktp_ptr=true; ktp_pass_by_ref=false;
          ktp_custom_free=have_complex; ktp_custom_copy=false }
    | KTypName n ->
        (match (kinfo_ n loc) with
        | KVariant kvar ->
            let {kvar_name; kvar_cname; kvar_cases; kvar_props; kvar_flags; kvar_loc} = !kvar in
            (match kvar_props with
            | Some(kvp) -> kvp
            | _ ->
                let kvp = (if List.mem VariantRecursive kvar_flags then
                    { ktp_complex=true; ktp_ptr=true; ktp_pass_by_ref=false;
                      ktp_custom_free=true; ktp_custom_copy=false }
                else
                    let _ = if IdSet.mem n !visited then raise_compile_err loc
                        (sprintf "unexpected recursive variant %s" (if kvar_cname<>"" then kvar_cname else pp_id2str kvar_name)) in
                    let _ = visited := IdSet.add n !visited in
                    let have_complex = List.exists (fun (_, ti) -> (get_ktprops_ ti kvar_loc).ktp_complex) kvar_cases
                    in
                    { ktp_complex=have_complex; ktp_ptr=false; ktp_pass_by_ref=true;
                     ktp_custom_free=have_complex; ktp_custom_copy=have_complex })
                in
                    kvar := {!kvar with kvar_props=Some(kvp)};
                    kvp)
        | KTyp kt ->
            let {kt_name; kt_cname; kt_typ; kt_props; kt_loc} = !kt in
            (match kt_props with
            | Some(ktp) -> ktp
            | _ ->
                let _ = if IdSet.mem n !visited then raise_compile_err loc
                    (sprintf "unexpected recursive type %s" (if kt_cname<>"" then kt_cname else pp_id2str kt_name)) in
                let _ = visited := IdSet.add n !visited in
                let ktp = get_ktprops_ kt_typ kt_loc
                in
                    kt := {!kt with kt_props=Some(ktp)};
                    ktp)
        | _ -> raise_compile_err loc (sprintf "unsupported named type '%s'" (id2str n)))

    in get_ktprops_ t loc

let annotate_types top_code =
    let top_code = find_recursive top_code in
    List.iter (fun e -> match e with
        | KDefVariant kvar ->
            let {kvar_name; kvar_cases; kvar_loc} = !kvar in
            let _ = get_ktprops (KTypName kvar_name) kvar_loc in
            let {kvar_flags} = !kvar in
            let is_recursive = List.mem VariantRecursive kvar_flags in
            let ncases = List.length kvar_cases in
            let have_null = match kvar_cases with
                    | (_, KTypVoid) :: _ -> true
                    | _ -> false in
            let no_tag = ncases == 1 || (is_recursive && ncases == 2 && have_null) in
            kvar := {!kvar with kvar_flags =
                (if have_null then [VariantHaveNull] else []) @
                (if no_tag then [VariantNoTag] else []) @ kvar_flags}
        | KDefTyp {contents={kt_name; kt_loc}} ->
            ignore(get_ktprops (KTypName kt_name) kt_loc)
        | _ -> ()) top_code;
    top_code
