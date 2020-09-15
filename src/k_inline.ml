(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* Do inline expansion of some function calls:
    1. find which functions can be inlined;
       * Inline functions cannot be inlined (including indirectly/mutually recursive functions).
       * Complex functions, i.e. functions that contain other functions, cannot be inlined.
       * Functions with C code bodies or type constructors cannot be inlined.
    2. compute approximate size of each function body. We cannot accurately estimate,
       how much bytes/machine instructions each function will take, since other optimizations
       are also applied after inline expansion, since there are different hardware architectures etc.
       So the calculated size of an expression is very approximate and is a weigted sum
       of K-form constructions.
    3. we compute how many times each function is mentioned (not just called, but mentioned).
       If it's mentioned once and this is the direct call of the function,
       we should always expand this call. If it's mentioned more than once,
       in each direct call we take into account the function size
       and the total size of caller function and the called function. If they are below certain
       thresholds, we do the inline expansion. If a function is declared as inline, we
       try it expand calls to this function before trying to expand calls from this function.
       Otherwise we can do the opposite thing.
*)

open Ast
open K_form
open Options

type func_info_t =
{
    fi_can_inline: bool;
    fi_size: int;
    fi_nrefs: int;
    fi_flags: fun_flag_t list
}

let find_recursive_funcs top_code =
    let all_called = ref (Env.empty : IdSet.t ref Env.t) in
    let curr_called = ref IdSet.empty in
    let fold_recfun_ktyp_ t loc callb = () in
    let rec fold_recfun_kexp_ e callb =
        match e with
        | KDefFun {contents={kf_name; kf_body}} ->
            let saved_called = !curr_called in
            curr_called := IdSet.empty;
            fold_recfun_kexp_ kf_body callb;
            all_called := Env.add kf_name (ref !curr_called) !all_called;
            curr_called := saved_called
        | KExpCall(f, _, (_, loc)) ->
            (match (kinfo_ f loc) with
            | KFun _ ->
                curr_called := IdSet.add f !curr_called
            | _ -> ())
        | _ -> fold_kexp e callb
    in let recfun_callb =
    {
        kcb_fold_atom = None;
        kcb_fold_ktyp = Some(fold_recfun_ktyp_);
        kcb_fold_kexp = Some(fold_recfun_kexp_);
        kcb_fold_result = 0
    } in
    (* for each function find the set of functions it calls directly *)
    let _ = List.iter (fun e -> fold_recfun_kexp_ e recfun_callb) top_code in
    let rec finalize_called iters all_funcs =
        let visited_funcs = ref IdSet.empty in
        let changed = ref false in
        let _ = if iters > 0 then () else raise_compile_err noloc
            "finalization of called functions' sets takes too much iterations" in
        let rec update_called f =
            match (Env.find_opt f !all_called) with
            | Some fcalled ->
                if IdSet.mem f !visited_funcs then !fcalled
                else
                    let _ = visited_funcs := IdSet.add f !visited_funcs in
                    let size0 = IdSet.cardinal !fcalled in
                    let upd_called = IdSet.fold (fun fi fcalled ->
                        if fi = f then fcalled else
                            let fcalled_fi = update_called fi in
                            IdSet.union fcalled fcalled_fi) (!fcalled) (!fcalled)
                        in
                    let size1 = IdSet.cardinal upd_called in
                    let _ = if size1 = size0 then () else
                        (fcalled := upd_called; changed := true) in
                    upd_called
            | _ -> IdSet.empty
        in List.iter (fun f -> ignore (update_called f)) all_funcs;
        (* before the next iteration we reverse the list of functions *)
        if not !changed then (iters-1) else finalize_called (iters - 1) (List.rev all_funcs)
    in
    (* do no more than certain number of iterations (10) through all the functions to update
       the dependencies. In principle, 2 iterations should be enough for all cases,
       and there is early-stop criteria, but without formal proof we do 10 iterations *)
    let iters0 = 10 in
    let all_funcs = List.rev (Env.fold (fun f _ all_funcs -> f :: all_funcs) !all_called []) in
    let _ = finalize_called iters0 all_funcs in
    (* the final pass; mark the recursive functions *)
    List.iter (fun f ->
        match ((kinfo_ f noloc), (Env.find_opt f !all_called)) with
        | ((KFun kf), (Some fcalled)) ->
            let {kf_flags} = !kf in
            (* because of the dead code and dead code branches elimination,
              a recursive function at some point may become non-recursive;
              so we always reset the flag and then set it back if needed *)
            let flags = remove_flag FunRecursive kf_flags in
            let flags = if (IdSet.mem f !fcalled) then FunRecursive :: flags else flags in
            kf := {!kf with kf_flags = flags}
        | _ -> ()) all_funcs;
    top_code

let calc_exp_size e =
    let sz = ref 0 in
    let fold_size_ktyp_ t loc callb = () in
    let fold_size_kexp_ e callb =
        fold_kexp e callb;
        let dsz = match e with
        | KExpNop _ | KExpAtom _ | KDefFun _ | KDefExn _
        | KDefVariant _ | KDefTyp _ | KDefClosureVars _ -> 0
        | KExpIntrin(IntrinStrConcat, args, _) -> (List.length args)
        | KExpBinOp _ | KExpUnOp _ | KExpCast _
        | KExpIntrin _ | KExpBreak _ | KExpContinue _
        | KExpMem _ | KExpAssign _ | KDefVal _ -> 1
        | KExpSeq(elist, _) -> List.length elist
        | KExpMkRecord(args, _) -> List.length args
        | KExpMkTuple(args, _) -> List.length args
        | KExpMkArray(args, _) ->
            List.fold_left (fun s al ->
                List.fold_left (fun s (f, a) -> s + (if f then 5 else 1)) s al)
            0 args
        | KExpMkClosure(_, _, args, _) -> List.length args
        | KExpCall(_, args, _) -> 1 + (List.length args)
        | KExpAt(_, _, _, idxs, _) -> List.length idxs
        | KExpCCode _ -> 100
        | KExpThrow _ -> 10
        | KExpWhile _ | KExpDoWhile _ | KExpIf _ -> 2
        | KExpMatch (cases, _) ->
            List.fold_left (fun total (checks, _) -> total + (List.length checks)) 0 cases
        | KExpTryCatch _ -> 10
        | KExpMap (e_idl_l, _, _, _) ->
            List.fold_left (fun total (_, idl, _) -> 10 + total + (List.length idl)) 0 e_idl_l
        | KExpFor (idl, _, _, _, _) -> 10 + (List.length idl)
        in
        sz := !sz + dsz
    in let size_callb =
    {
        kcb_fold_atom = None;
        kcb_fold_ktyp = Some(fold_size_ktyp_);
        kcb_fold_kexp = Some(fold_size_kexp_);
        kcb_fold_result = 0
    } in
    fold_size_kexp_ e size_callb;
    !sz

let subst_names e subst_env0 rename_locals =
    let subst_env0 = if not rename_locals then subst_env0 else
        let (_, decl_set) = used_decl_by_kexp e in
        let loc = get_kexp_loc e in
        IdSet.fold (fun i subst_env ->
            match (kinfo_ i loc) with
            | KVal kv ->
                let {kv_name; kv_typ; kv_flags; kv_loc} = kv in
                let new_name = dup_idk kv_name in
                let _ = create_kdefval new_name kv_typ kv_flags None [] kv_loc in
                Env.add kv_name (Atom.Id new_name) subst_env
            | _ -> subst_env) decl_set subst_env0
        in
    let subst_env = ref subst_env0 in
    let subst_atom_ a loc callb =
        match a with
        | Atom.Id i ->
            if i = noid then a else
            (match (Env.find_opt i !subst_env) with
            | Some(new_a) -> new_a
            | _ -> a)
        | _ -> a
        in
    let subst_ktyp_ t loc callb = t in
    let subst_kexp_ e callb = walk_kexp e callb in
    let subst_callb =
    {
        kcb_atom=Some(subst_atom_);
        kcb_ktyp=Some(subst_ktyp_);
        kcb_kexp=Some(subst_kexp_)
    } in
    subst_kexp_ e subst_callb

let expand_call e =
    match e with
    | KExpCall(f, real_args, (_, loc)) ->
        (match (kinfo_ f loc) with
        | KFun kf ->
            let {kf_args; kf_body} = !kf in
            let subst_env = List.fold_left2 (fun subst_env (formal_arg, _) real_arg ->
                Env.add formal_arg real_arg subst_env) (Env.empty : atom_t Env.t) kf_args real_args
                in
            let (_, decl_set) = used_decl_by_kexp kf_body in
            let have_bad_defs = IdSet.fold (fun i have_bad_defs ->
                have_bad_defs ||
                (match (kinfo_ i loc) with
                | KVal kv -> false
                | _ -> true)) decl_set false in
            if have_bad_defs then (e, false) else
            ((subst_names kf_body subst_env true), true)
        | _ -> (e, false))
    | _ -> (e, false)

let inline_some top_code =
    let all_funcs_info = ref (Env.empty : (func_info_t ref) Env.t) in
    let gen_default_func_info size nrefs =
        ref {fi_can_inline=false; fi_size=0; fi_nrefs=nrefs; fi_flags=[]}
    in
    let global_size = calc_exp_size (code2kexp top_code noloc) in
    let curr_fi = ref (gen_default_func_info global_size 0) in

    (* step 1 of the actual function call expansion algorithm:
       collect information about each function *)
    (* we calculate not only direct calls, but any references to each function *)
    let fold_finfo_atom_ a loc callb =
        match a with
        | Atom.Id f -> if f = noid then () else
            (match (kinfo_ f loc) with
            | KFun kf ->
                (match (Env.find_opt f !all_funcs_info) with
                | Some (r_fi) ->
                    let {fi_nrefs} = !r_fi in
                    r_fi := {!r_fi with fi_nrefs=fi_nrefs + 1}
                | _ ->
                    let r_fi = gen_default_func_info 1000000 1 in
                    all_funcs_info := Env.add f r_fi !all_funcs_info)
            | _ -> ())
        | _ -> () in
    let fold_finfo_ktyp_ t loc callb = () in
    let fold_finfo_kexp_ e callb =
        match e with
        | KDefFun {contents={kf_name; kf_body; kf_flags; kf_loc}} ->
            let saved_fi = !curr_fi in
            let r_fi = match (Env.find_opt kf_name !all_funcs_info) with
                | Some (r_fi) -> r_fi
                | _ -> gen_default_func_info 0 0
                in
            let can_inline = List.for_all (function
                | FunRecursive | FunInC | FunCtor _ -> false
                | _ -> true) kf_flags in
            let fsize = calc_exp_size kf_body in
            r_fi := {!r_fi with fi_can_inline=can_inline; fi_size=fsize; fi_flags=kf_flags};
            all_funcs_info := Env.add kf_name r_fi !all_funcs_info;
            curr_fi := r_fi;
            fold_kexp e callb;
            curr_fi := saved_fi;
            (* previous curr_fi is the outer function; functions that contain other function cannot be inlined *)
            saved_fi := {!saved_fi with fi_can_inline=false}
        | _ -> fold_kexp e callb
    in let finfo_callb =
    {
        kcb_fold_atom = Some(fold_finfo_atom_);
        kcb_fold_ktyp = Some(fold_finfo_ktyp_);
        kcb_fold_kexp = Some(fold_finfo_kexp_);
        kcb_fold_result = 0
    } in
    let _ = find_recursive_funcs top_code in
    let _ = List.iter (fun e -> fold_finfo_kexp_ e finfo_callb) top_code in

    (* step 2. try to actually expand some calls *)
    let inline_ktyp_ t loc callb = t in
    let rec inline_kexp_ e callb =
        match e with
        | KDefFun kf ->
            let {kf_name; kf_body; kf_loc} = !kf in
            let saved_fi = !curr_fi in
            let r_fi = match (Env.find_opt kf_name !all_funcs_info) with
                | Some (r_fi) -> r_fi
                | _ -> raise_compile_err kf_loc "inline: function is not found the collected function database"
                in
            let _ = curr_fi := r_fi in
            let new_body = inline_kexp_ kf_body callb in
            kf := {!kf with kf_body=new_body};
            curr_fi := saved_fi;
            e
        | KExpCall(f, real_args, _) ->
            (match (Env.find_opt f !all_funcs_info) with
            | Some(r_fi) ->
                let {fi_can_inline=caller_can_inline; fi_size=caller_size; fi_flags=caller_flags} = !(!curr_fi) in
                let caller_is_inline = caller_can_inline && (List.mem FunInline caller_flags) in
                let max_caller_size = if caller_is_inline then options.inline_thresh*3/2 else options.inline_thresh*10 in
                let {fi_can_inline; fi_size; fi_nrefs; fi_flags} = !r_fi in
                let f_is_inline = fi_can_inline && (List.mem FunInline fi_flags) in
                let f_max_size = if f_is_inline then options.inline_thresh*3/2 else options.inline_thresh in
                let new_size = caller_size + fi_size - (List.length real_args) - 1 in
                let new_size = if new_size >= 0 then new_size else 0 in
                if fi_can_inline && (fi_nrefs = 1 ||
                  (fi_size <= f_max_size && new_size <= max_caller_size)) then
                    let (new_e, inlined) = expand_call e in
                    let _ = if not inlined then () else !curr_fi := {!(!curr_fi) with fi_size=new_size} in
                    new_e
                else e
            | _ -> e)
        | _ -> walk_kexp e callb
    in let inline_callb =
    {
        kcb_atom = None;
        kcb_ktyp=Some(inline_ktyp_);
        kcb_kexp=Some(inline_kexp_)
    } in
    List.map (fun e -> inline_kexp_ e inline_callb) top_code
