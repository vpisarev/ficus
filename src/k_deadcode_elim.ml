(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    remove unused code from the K-form:
    * any type or exception that is declared but not referenced.
      that includes the types that reference itself (recursive variants),
      but are not referenced anywhere else.
    * any function that is declared but not called (including recursive functions).
      This rule should actually become somewhat more complex in the future, because:
      - for now mutually recursive functions that are not used anywhere
        are not removed because they call each other
      - in a per-module compilation mode (that is not implemented yet)
        only "static", i.e. module-local functions should be removed.
        other unused functions should be excluded at the link stage.
    * if there is a value that is defined but not used, we remove it as well.
      however, if the right-hand-side expression that initializes the value
      is impure, we retain this call, e.g. we replace
      'val usused_flag = func_with_side_effects(args)' with
      'func_with_side_effects(args)'.
      if it's not a temporary value, we issue a warning about unused value.
    * `if (true) a else b` is replaced with `a`
    * `if (false) a else b` is replaced with `b`
       if there is value/variable of type `void` that is assigned,
      `val a: void = f(...)`, it's replaced with `f(...)`
    * if there is a pure expression in the middle of expression sequence
      (i.e. it's not the last expression), we remove it as well.
*)

open Ast
open K_form

let rec pure_kexp e =
    let ispure = ref true in
    let local_vars = ref IdSet.empty in
    let pure_ktyp_ t loc callb = () in
    let rec pure_kexp_ e callb =
        match e with
        | KExpBreak _ -> ispure := false
        | KExpContinue _ -> ispure := false
        | KExpIntrin(intr, _, _) when
            intr = IntrinPopExn || intr = IntrinCheckIdx || intr = IntrinCheckIdxRange
            -> ispure := false
        | KExpCall(f, _, (_, loc)) -> if (pure_fun f loc) then () else ispure := false
        | KExpAssign (i, (AtomId j), _) when i = j -> ()
        | KExpAssign (i, _, _) ->
            if (IdSet.mem i !local_vars) then () else ispure := false
            (*ispure := false*)
        | KExpTryCatch _ -> ispure := false
        | KExpThrow _ -> ispure := false
        | KExpCCode _ -> ispure := false
        | KExpSeq(elist, (_, loc)) ->
            let saved_local_vars = !local_vars in
            let (_, dv) = used_decl_by_kexp e in
            let _ = IdSet.iter (fun i ->
                match (kinfo_ i loc) with
                | KVal {kv_flags} ->
                    if kv_flags.val_flag_mutable && not kv_flags.val_flag_tempref then
                        local_vars := IdSet.add i !local_vars
                    else ()
                | _ -> ()) dv
                in
            ignore(List.for_all (fun e ->
                pure_kexp_ e callb; !ispure) elist);
            local_vars := saved_local_vars
        | _ -> if !ispure then fold_kexp e callb else ()
    in let pure_callb =
    {
        kcb_fold_atom = None;
        kcb_fold_ktyp = Some(pure_ktyp_);
        kcb_fold_kexp = Some(pure_kexp_);
        kcb_fold_result = 0
    } in
    pure_kexp_ e pure_callb;
    !ispure
and pure_fun f loc =
    match (kinfo_ f loc) with
    | KFun df ->
        let {kf_body; kf_flags} = !df in
        if kf_flags.fun_flag_pure > 0 then true
        else if kf_flags.fun_flag_pure = 0 || kf_flags.fun_flag_ccode then false
        else
        ((* temporarily mark the function as 'pure' (optimistic approach) to avoid an infinite loop *)
        let _ = df := {!df with kf_flags = {kf_flags with fun_flag_pure=1}} in
        let pf = pure_kexp kf_body in
        df := { !df with kf_flags = {kf_flags with fun_flag_pure=(if pf then 1 else 0)} };
        pf)
    | KExn _ -> true
    (* all indirect calls via values etc. are considered impure, because of uncertainty *)
    | _ -> false

let rec reset_purity_flags_ktyp_ t loc callb = ()
and reset_purity_flags_kexp_ e callb =
    match e with
    | KDefFun df ->
        let {kf_flags} = !df in
        df := {!df with kf_flags={kf_flags with fun_flag_pure = -1}}
    | _ -> fold_kexp e callb

let reset_purity_flags code =
    let callb =
    {
        kcb_fold_atom = None;
        kcb_fold_ktyp = Some(reset_purity_flags_ktyp_);
        kcb_fold_kexp = Some(reset_purity_flags_kexp_);
        kcb_fold_result = 0
    }
    in List.iter (fun e -> reset_purity_flags_kexp_ e callb) code

let elim_unused kmods =
    let _ = List.iter (fun {km_top} -> reset_purity_flags km_top) kmods in
    let uv = List.fold_left (fun uv {km_top} ->
        let uv_i = used_by km_top in
        IdSet.union uv_i uv) IdSet.empty kmods in
    let used i = IdSet.mem i uv in
    let fold_values = ref (IdSet.empty) in
    let is_main = ref false in

    let rec elim_unused_ktyp_ t loc callb = t
    and elim_unused_kexp_ e callb =
        match e with
        | KDefVal(i, e, loc) ->
            let e = elim_unused_kexp_ e callb in
            let is_ccode = match e with KExpCCode _ -> true | _ -> false in
            (match e with
            | KExpAtom((AtomId fr), _) when (get_orig_id fr) = __fold_result_id__ ->
                fold_values := IdSet.add i !fold_values
            | _ -> ());
            let is_really_used = (used i) || is_ccode (*|| ((not !is_main) &&
                (let {kv_flags} = get_kval i loc in is_val_global kv_flags))*) in
            if is_really_used then
                KDefVal(i, e, loc)
            (* [TODO] issue a warning about unused identifier *)
            else if not (pure_kexp e) then
                e
            else
                KExpNop(loc)
        | KDefFun kf ->
            let {kf_name; kf_body; kf_flags; kf_loc} = !kf in
            if (used kf_name) (*|| ((not !is_main) && not (List.mem FunPrivate kf_flags))*) then
                (let new_body = elim_unused_kexp_ kf_body callb in
                kf := {!kf with kf_body=new_body};
                e)
            else KExpNop(kf_loc)
        | KDefExn ke ->
            let {ke_name; ke_std; ke_loc} = !ke in
            if (used ke_name) || (not !is_main) then e
            else KExpNop(ke_loc)
        | KDefVariant kvar ->
            let {kvar_name; kvar_loc} = !kvar in
            (* if at least one of the variant constructors is used then
               the constructor call returns the variant type,
               i.e. the variant name gets used. So, we may be sure that
               we will never eliminate variant definition and yet retain
               some of its used constructors. *)
            if (used kvar_name) (*|| (not !is_main)*) then e
            else KExpNop(kvar_loc)
        | KDefTyp kt ->
            let {kt_name; kt_loc} = !kt in
            (* if at least one of the variant constructors is used then
               the constructor call returns the variant type,
               i.e. the variant name gets used. So, we may be sure that
               we will never eliminate variant definition and yet retain
               some of its used constructors. *)
            if (used kt_name) (*|| (not !is_main)*) then e
            else KExpNop(kt_loc)
        | KDefClosureVars kcv ->
            let {kcv_name; kcv_loc} = !kcv in
            if (used kcv_name) (*|| (not !is_main)*) then e
            else KExpNop(kcv_loc)
        | KExpSeq(code, (ktyp, loc)) ->
            let code = elim_unused_ code [] callb in
            (match (List.rev code) with
            | KExpAtom((AtomId j), _) :: KDefVal(i, rhs, _) :: rest when i = j ->
                rcode2kexp (rhs :: rest) loc
            | _ -> code2kexp code loc)
        | _ -> walk_kexp e callb
    and elim_unused_ code result callb =
        match code with
        | e :: rest ->
            let e = elim_unused_kexp_ e callb in
            let result = (match e with
            | KExpNop _ ->
                if rest = [] then e :: result else result
            | KExpAssign(fr, (AtomId r), loc)
                when (get_orig_id fr) = __fold_result_id__ && (IdSet.mem r !fold_values) ->
                result
            | KDefVal _ | KDefFun _ | KDefExn _ | KDefVariant _ | KDefTyp _ | KDefClosureVars _ ->
                e :: result
            | _ -> if (pure_kexp e) && rest != [] then result
                    else e :: result) in
            elim_unused_ rest result callb
        | _ -> List.rev result
    in let elim_callb =
    {
        kcb_ktyp=Some(elim_unused_ktyp_);
        kcb_kexp=Some(elim_unused_kexp_);
        kcb_atom=None
    }
    in List.map (fun km ->
        let {km_top; km_main} = km in
        let _ = is_main := km_main in
        let new_top = elim_unused_ km_top [] elim_callb in
        {km with km_top=new_top}) kmods
