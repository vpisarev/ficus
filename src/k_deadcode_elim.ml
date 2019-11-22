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
    * if there is a value that is assigned but not used, we remove it as well.
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

let pure_ktyp_ t callb = ()
let rec pure_kexp_ e callb =
    let set_impure () = callb.kcb_fold_result <- false in
    match e with
    | KExpBreak _ -> set_impure()
    | KExpContinue _ -> set_impure()
    | KExpIntrin(IntrinPopExn, _, _) -> set_impure()
    | KExpCall(f, _, _) -> if (pure_fun f) then () else set_impure()

    (* [TODO] make it more sophisticated. A block of expressions may change value
       of a locally-defined variable and yet, as a whole, stay a pure expression *)
    | KExpAssign _ -> set_impure()
    | KExpTryCatch _ -> set_impure()
    | KExpThrow _ -> set_impure()
    | KExpCCode _ -> set_impure()
    | _ -> fold_kexp e callb
and new_pure_callb () =
{
    kcb_fold_atom=None;
    kcb_fold_ktyp=Some(pure_ktyp_);
    kcb_fold_kexp=Some(pure_kexp_);
    kcb_fold_result=true
}
and pure_kexp e =
    let callb = new_pure_callb() in
    pure_kexp_ e callb;
    callb.kcb_fold_result
and pure_fun f =
    match (kinfo f) with
    | KFun df ->
        let {kf_body; kf_flags} = !df in
        if (List.mem FunPure kf_flags) then true
        else if (List.mem FunImpure kf_flags) ||
                (List.mem FunInC kf_flags) then false
        else
        ((* temporarily mark the function as 'pure' (optimistic approach) to avoid an infinite loop *)
        let _ = df := { !df with kf_flags = FunPure :: kf_flags } in
        let pf = pure_kexp kf_body in
        df := { !df with kf_flags = (if pf then FunPure else FunImpure) :: kf_flags };
        pf)
    | KExn _ -> true
    (* all indirect calls via values etc. are considered impure, because of uncertainty *)
    | _ -> false

let rec reset_purity_flags_ktyp_ t callb = ()
and reset_purity_flags_kexp_ e callb =
    match e with
    | KDefFun df ->
        let {kf_flags} = !df in
        if (List.mem FunPure kf_flags) then ()
        else if (List.mem FunInC kf_flags) then ()
        else
        let new_flags = List.filter (fun f -> f != FunImpure) kf_flags in
        df := {!df with kf_flags=new_flags}
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

let add_used1 i callb =
    callb.kcb_fold_result <- IdSet.add i callb.kcb_fold_result

let add_used uv_set callb =
    callb.kcb_fold_result <- IdSet.union uv_set callb.kcb_fold_result

let rec used_by_atom_ a callb =
    match a with
    | Atom.Id i -> add_used1 i callb
    | _ -> ()
and used_by_ktyp_ t callb = fold_ktyp t callb
and used_by_kexp_ e callb =
    match e with
    | KDefVal(i, e, _) ->
        let uv = used_by_kexp e in
        add_used uv callb
    | KDefFun {contents={kf_name; kf_typ; kf_body}} ->
        (* the function arguments are not included into the "used id's" set by default,
           they should be referenced by the function body to be included *)
        let uv_typ = used_by_ktyp kf_typ in
        let uv_body = used_by_kexp kf_body in
        let uv = IdSet.union uv_typ (IdSet.remove kf_name uv_body) in
        add_used uv callb
    | KDefExn {contents={ke_typ}} ->
        let uv = used_by_ktyp ke_typ in
        add_used uv callb
    | KDefVariant {contents={kvar_name; kvar_cases}} ->
        (* variant constructors are not included into the "used id's" set by default;
           they should be referenced somewhere to be included *)
        let uv = List.fold_left (fun uv (ni, ti) ->
            let uv = IdSet.add ni uv in
            let uv_ti = IdSet.remove kvar_name (used_by_ktyp ti) in
            IdSet.union uv_ti uv) IdSet.empty kvar_cases in
        add_used uv callb
    | _ -> fold_kexp e callb

and new_used_ids_callb () =
    {
        kcb_fold_atom = Some(used_by_atom_);
        kcb_fold_ktyp = Some(used_by_ktyp_);
        kcb_fold_kexp = Some(used_by_kexp_);
        kcb_fold_result = IdSet.empty
    }
and used_by_ktyp t =
    let callb = new_used_ids_callb() in
    used_by_ktyp_ t callb;
    callb.kcb_fold_result
and used_by_kexp e =
    let callb = new_used_ids_callb() in
    used_by_kexp_ e callb;
    callb.kcb_fold_result

let used_by code =
    List.fold_left (fun uv e -> IdSet.union (used_by_kexp e) uv) IdSet.empty code

let rec elim_unused code =
    let _ = reset_purity_flags code in
    let uv = used_by code in
    let used i = IdSet.mem i uv in

    let rec elim_unused_ktyp_ t callb = t
    and elim_unused_kexp_ e callb =
        match e with
        | KDefVal(i, e, loc) ->
            let e = elim_unused_kexp_ e callb in
            if (used i) then KDefVal(i, e, loc)
            (* [TODO] issue a warning about unused identifier *)
            else if not (pure_kexp e) then
                e
            else
                KExpNop(loc)
        | KDefFun kf ->
            let {kf_name; kf_body; kf_loc} = !kf in
            if (used kf_name) then
                (let new_body = elim_unused_kexp_ kf_body callb in
                kf := {!kf with kf_body=new_body};
                e)
            else KExpNop(kf_loc)
        | KDefExn ke ->
            let {ke_name; ke_loc} = !ke in
            if (used ke_name) then e
            else KExpNop(ke_loc)
        | KDefVariant kvar ->
            let {kvar_name; kvar_loc} = !kvar in
            (* if at least one of the variant constructors is used then
               the constructor call returns the variant type,
               i.e. the variant name gets used. So, we may be sure that
               we will never eliminate variant definition and yet retain
               some of its used constructors. *)
            if (used kvar_name) then e
            else KExpNop(kvar_loc)
        | KExpIf(c, then_e, else_e, kctx) ->
            let c = elim_unused_kexp_ c callb in
            let then_e = elim_unused_kexp_ then_e callb in
            let else_e = elim_unused_kexp_ else_e callb in
            (* eliminate dead branches *)
            (match c with
            | KExpAtom(Atom.Lit (LitBool true), _) -> then_e
            | KExpAtom(Atom.Lit (LitBool false), _) -> else_e
            | _ -> KExpIf(c, then_e, else_e, kctx))
        | KExpSeq(code, kctx) ->
            let code = elim_unused_ code [] callb in
            KExpSeq(code, kctx)
        | _ -> walk_kexp e callb
    and elim_unused_ code result callb =
        match code with
        | e :: rest ->
            let e = elim_unused_kexp_ e callb in
            let result = (match e with
            | KExpNop _ ->
                if rest = [] then e :: result else result
            | KDefVal _ | KDefFun _ | KDefExn _ | KDefVariant _ ->
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
    in elim_unused_ code [] elim_callb
