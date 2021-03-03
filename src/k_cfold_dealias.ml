(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    perform "constant folding" + "dealiasing" optimization step of K-form:
    * `1+2` is replaced with `3` etc. (const1 op const2 => result_const)
    * `a+0, a-0, a*1, a/1` is replaced with `a`
    * `a-a, a*0, 0*a` is replaced with `0`
    * `a & true, true & a, a | false, false | a` is replaced with a
    * `a & false, false & a` is replaced with false
    * trivial definitions are eliminated.
      That is, if the algorithm finds the definitions that look like
        * `val a@34535=b@795`, where `b@795` is immutable value,
        * or `val a@34535=3.1415`,
      it removes them and the replaces `a@34535` with `b@795` or `3.1415`,
      respectively, everywhere in the code
*)

open Ast
open K_form

type aclass_t =
    | ConstZero
    | ConstInt of int64
    | ConstFloat of float
    | ConstBool of bool
    | ConstString of string
    | NonConst

let aclass2str a =
    match a with
    | ConstZero -> "Zero"
    | ConstInt i -> sprintf "ConstInt(%Li)" i
    | ConstFloat f -> sprintf "ConstInt(%g)" f
    | ConstBool b -> sprintf "ConstBool(%B)" b
    | ConstString s -> sprintf "ConstString(%s)" s
    | NonConst -> "NonConst"

let classify_atom a z =
    match a with
    | AtomLit la ->
        (match la with
        | KLitInt x -> if z && x = 0L then ConstZero else ConstInt(x)
        | KLitSInt(_, x) -> if z && x = 0L then ConstZero else ConstInt(x)
        | KLitUInt(_, x) -> if z && x = 0L then ConstZero else ConstInt(x)
        | KLitFloat(_, x) -> if z && x = 0.0 then ConstZero else ConstFloat(x)
        | KLitBool x -> if z && x = false then ConstZero else ConstBool(x)
        | KLitChar x -> ConstString(x)
        | KLitString x -> if z && x = "" then ConstZero else ConstString(x)
        | KLitNil _ -> NonConst)
    | AtomId _ -> NonConst

let retain_atom ac a at =
    match ac with
    | NonConst -> (None, (Some (a, at)))
    | _ -> ((Some ac), None)

(* cfold_bop, cfold_uop, cfold_cast produce a pair of options:
   (const_option, atom_n_type_option) and pass it to finalize_cfold_result
   that returns a single option:
        * Some(const_fold_result_exp) - successfully folded.
          The caller function replaces the original expression with the
          provided result.
        * None - no folding has been done; retain the original expression as-is
   if const_option is Some(...:aclass_t) then it takes the preference,
   then we output (if possible) the computed atom of the specified type (res_t).
   otherwise if the second option is Some((atom, atom_type)) then
   we output either the original atom, or the casting expression to res_t. *)
let finalize_cfold_result c_opt at_opt res_t loc =
    let mk_some_lit_atom l = Some (KExpAtom((AtomLit l), (res_t, loc))) in
    let f2i x = Int64.of_float x in
    let b2i x = if x then 1L else 0L in
    match (c_opt, at_opt) with
    | ((Some c), _) ->
        (match c with
        | ConstZero ->
            (match res_t with
            | KTypInt -> mk_some_lit_atom (KLitInt 0L)
            | KTypSInt b -> mk_some_lit_atom (KLitSInt(b, 0L))
            | KTypUInt b -> mk_some_lit_atom (KLitUInt(b, 0L))
            | KTypFloat b -> mk_some_lit_atom (KLitFloat(b, 0.0))
            | KTypBool -> mk_some_lit_atom (KLitBool false)
            | KTypString -> mk_some_lit_atom (KLitString "")
            | _ -> None)
        | ConstInt x ->
            (match res_t with
            | KTypInt -> mk_some_lit_atom (KLitInt x)
            | KTypSInt b -> mk_some_lit_atom (KLitSInt(b, x))
            | KTypUInt b -> mk_some_lit_atom (KLitUInt(b, x))
            | KTypFloat b -> mk_some_lit_atom (KLitFloat(b, (Int64.to_float x)))
            | KTypBool -> mk_some_lit_atom (KLitBool (x != 0L))
            | KTypString -> mk_some_lit_atom (KLitString (Int64.to_string x))
            | _ -> None)
        | ConstFloat x ->
            (* we do not round floating-point numbers, because when
            C/C++/Java etc. programmers write (int)5.9, they expect 5, not 6 *)
            (match res_t with
            | KTypInt -> mk_some_lit_atom (KLitInt (f2i x))
            | KTypSInt b -> mk_some_lit_atom (KLitSInt(b, (f2i x)))
            | KTypUInt b -> mk_some_lit_atom (KLitUInt(b, (f2i x)))
            | KTypFloat b -> mk_some_lit_atom (KLitFloat(b, x))
            | KTypBool -> mk_some_lit_atom (KLitBool (x != 0.0))
            | KTypString -> mk_some_lit_atom (KLitString (Float.to_string x))
            | _ -> None)
        | ConstBool x ->
            (match res_t with
            | KTypInt -> mk_some_lit_atom (KLitInt (b2i x))
            | KTypSInt b -> mk_some_lit_atom (KLitSInt(b, (b2i x)))
            | KTypUInt b -> mk_some_lit_atom (KLitUInt(b, (b2i x)))
            | KTypFloat b -> mk_some_lit_atom (KLitFloat(b, (if x then 1.0 else 0.0)))
            | KTypBool -> mk_some_lit_atom (KLitBool x)
            | KTypString -> mk_some_lit_atom (KLitString (if x then "true" else "false"))
            | _ -> None)
        | ConstString x ->
            (match res_t with
            | KTypString -> mk_some_lit_atom (KLitString x)
            | _ -> None)
        | _ -> None)
    | (_, (Some (a, at))) ->
        if at = res_t then
            Some (KExpAtom(a, (at, loc)))
        else
            Some (KExpCast(a, res_t, loc))
    | _ -> None

let cfold_bop bop a b res_t loc =
    let at = get_atom_ktyp a loc in
    let bt = get_atom_ktyp b loc in
    let z = match bop with | OpCmp _  | OpPow -> false
        | _ -> true in
    let ac = classify_atom a z in
    let bc = classify_atom b z in
    let retain_a () = retain_atom ac a at in
    let retain_b () = retain_atom bc b bt in
    let (c_opt, a_opt) =
    (* the type checker has checked that the combination of 'a' and 'b' types is valid
        and it also computed the output type (passed here as 'res_t'), so
        we just rely on the type checker and avoid extra checks here *)
    match (bop, ac, bc) with
    | (_, NonConst, NonConst) -> (* fast track to skip the remaining checks *)
        if a = b then
            match bop with
            | OpSub | OpBitwiseXor -> ((Some ConstZero), None)
            (* OpDiv and OpMod are skipped because there can be a=b=0 *)
            | OpBitwiseAnd | OpBitwiseOr -> retain_a()
            | OpCmp(CmpEQ) | OpCmp(CmpLE) | OpCmp(CmpGE) -> ((Some (ConstBool true)), None)
            | OpCmp(CmpNE) | OpCmp(CmpLT) | OpCmp(CmpGT) -> ((Some (ConstBool false)), None)
            | _ -> (None, None)
        else
            (None, None)
    | (OpAdd, ConstZero, _) -> retain_b()
    | (OpAdd, _, ConstZero) -> retain_a()
    | (OpAdd, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt(Int64.add ia ib))), None)
    | (OpAdd, (ConstInt ia), (ConstFloat fb)) -> ((Some (ConstFloat((Int64.to_float ia) +. fb))), None)
    | (OpAdd, (ConstFloat fa), (ConstInt ib)) -> ((Some (ConstFloat(fa +. (Int64.to_float ib)))), None)
    | (OpAdd, (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstFloat(fa +. fb))), None)
    | (OpAdd, (ConstString sa), (ConstString sb)) -> ((Some (ConstString(sa ^ sb))), None)
    | (OpSub, ConstZero, ConstZero) -> ((Some ac), None)
    | (OpSub, ConstZero, ConstInt(x)) -> ((Some (ConstInt (Int64.neg x))), None)
    | (OpSub, ConstZero, ConstFloat(x)) -> ((Some (ConstFloat (-. x))), None)
    | (OpSub, _, ConstZero) -> retain_a()
    | (OpSub, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt(Int64.sub ia ib))), None)
    | (OpSub, (ConstInt ia), (ConstFloat fb)) -> ((Some (ConstFloat((Int64.to_float ia) -. fb))), None)
    | (OpSub, (ConstFloat fa), (ConstInt ib)) -> ((Some (ConstFloat(fa -. (Int64.to_float ib)))), None)
    | (OpSub, (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstFloat(fa -. fb))), None)
    | (OpMul, ConstZero, _) -> ((Some ConstZero), None)
    | (OpMul, _, ConstZero) -> ((Some ConstZero), None)
    | (OpMul, (ConstInt 1L), _) -> retain_b()
    | (OpMul, (ConstFloat 1.0), _) -> retain_b()
    | (OpMul, _, (ConstInt 1L)) -> retain_a()
    | (OpMul, _, (ConstFloat 1.0)) -> retain_a()
    | (OpMul, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt(Int64.mul ia ib))), None)
    | (OpMul, (ConstInt ia), (ConstFloat fb)) -> ((Some (ConstFloat((Int64.to_float ia) *. fb))), None)
    | (OpMul, (ConstFloat fa), (ConstInt ib)) -> ((Some (ConstFloat(fa *. (Int64.to_float ib)))), None)
    | (OpMul, (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstFloat(fa *. fb))), None)
    | (OpDiv, _, ConstZero)
    | (OpMod, _, ConstZero) ->
        if not (is_ktyp_integer at true) then (None, None) else
            raise_compile_err loc "division by constant 0"
    | (OpDiv, _, (ConstInt 1L)) -> retain_a()
    | (OpDiv, _, (ConstFloat 1.0)) -> retain_a()
    | (OpDiv, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt(Int64.div ia ib))), None)
    | (OpDiv, (ConstInt ia), (ConstFloat fb)) -> ((Some (ConstFloat((Int64.to_float ia) /. fb))), None)
    | (OpDiv, (ConstFloat fa), (ConstInt ib)) -> ((Some (ConstFloat(fa /. (Int64.to_float ib)))), None)
    | (OpDiv, (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstFloat(fa /. fb))), None)
    | (OpMod, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt(Int64.rem ia ib))), None)
    | (OpMod, (ConstInt ia), (ConstFloat fb)) -> ((Some (ConstFloat(Float.rem (Int64.to_float ia) fb))), None)
    | (OpMod, (ConstFloat fa), (ConstInt ib)) -> ((Some (ConstFloat(Float.rem fa (Int64.to_float ib)))), None)
    | (OpMod, (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstFloat(Float.rem fa fb))), None)
    | (OpBitwiseAnd, ConstZero, _) -> ((Some ConstZero), None)
    | (OpBitwiseAnd, _, ConstZero) -> ((Some ConstZero), None)
    | (OpBitwiseAnd, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt (Int64.logand ia ib))), None)
    | (OpBitwiseAnd, (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba && bb))), None)
    | (OpBitwiseOr, ConstZero, _) -> retain_b()
    | (OpBitwiseOr, _, ConstZero) -> retain_a()
    | (OpBitwiseOr, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt (Int64.logor ia ib))), None)
    | (OpBitwiseOr, (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba || bb))), None)
    | (OpBitwiseXor, ConstZero, _) -> retain_b()
    | (OpBitwiseXor, _, ConstZero) -> retain_a()
    | (OpBitwiseXor, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt (Int64.logxor ia ib))), None)
    | (OpBitwiseXor, (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba != bb))), None)

    | (OpPow, (ConstInt ia), (ConstInt ib)) ->
        if ib >= 0L then
            ((Some (ConstInt (Utils.ipower ia ib))), None)
        else
            raise_compile_err loc "integer is raised to negative power; just use literal '0' instead"
    | (OpPow, (ConstInt ia), (ConstFloat fb)) ->
        ((Some (ConstFloat (Float.pow (Int64.to_float ia) fb))), None)
    | (OpPow, (ConstFloat fa), (ConstInt ib)) ->
        ((Some (ConstFloat (Float.pow fa (Int64.to_float ib)))), None)
    | (OpPow, (ConstFloat fa), (ConstFloat fb)) ->
        ((Some (ConstFloat (Float.pow fa fb))), None)

    | (OpCmp(CmpEQ), (ConstInt ia), (ConstInt ib)) -> ((Some (ConstBool (ia = ib))), None)
    | (OpCmp(CmpNE), (ConstInt ia), (ConstInt ib)) -> ((Some (ConstBool (ia != ib))), None)
    | (OpCmp(CmpLT), (ConstInt ia), (ConstInt ib)) -> ((Some (ConstBool (ia < ib))), None)
    | (OpCmp(CmpGT), (ConstInt ia), (ConstInt ib)) -> ((Some (ConstBool (ia > ib))), None)
    | (OpCmp(CmpLE), (ConstInt ia), (ConstInt ib)) -> ((Some (ConstBool (ia <= ib))), None)
    | (OpCmp(CmpGE), (ConstInt ia), (ConstInt ib)) -> ((Some (ConstBool (ia >= ib))), None)

    (* [TODO] we do not have >, >=, < and <= comparison operations on strings yet,
       because it requires a better Unicode support *)
    | (OpCmp(CmpEQ), (ConstString sa), (ConstString sb)) -> ((Some (ConstBool (sa = sb))), None)
    | (OpCmp(CmpNE), (ConstString sa), (ConstString sb)) -> ((Some (ConstBool (sa != sb))), None)

    | (OpCmp(CmpEQ), (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstBool (fa = fb))), None)
    | (OpCmp(CmpNE), (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstBool (fa != fb))), None)
    | (OpCmp(CmpLT), (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstBool (fa < fb))), None)
    | (OpCmp(CmpGT), (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstBool (fa > fb))), None)
    | (OpCmp(CmpLE), (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstBool (fa <= fb))), None)
    | (OpCmp(CmpGE), (ConstFloat fa), (ConstFloat fb)) -> ((Some (ConstBool (fa >= fb))), None)

    | (OpCmp(CmpEQ), (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba = bb))), None)
    | (OpCmp(CmpNE), (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba != bb))), None)
    | (OpCmp(CmpLT), (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba < bb))), None)
    | (OpCmp(CmpGT), (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba > bb))), None)
    | (OpCmp(CmpLE), (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba <= bb))), None)
    | (OpCmp(CmpGE), (ConstBool ba), (ConstBool bb)) -> ((Some (ConstBool (ba >= bb))), None)

    | (OpShiftLeft, ConstZero, _) -> ((Some ConstZero), None)
    | (OpShiftLeft, _, ConstZero) -> retain_a()
    | (OpShiftLeft, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt (Int64.shift_left ia (Int64.to_int ib)))), None)
    | (OpShiftRight, ConstZero, _) -> ((Some ConstZero), None)
    | (OpShiftRight, _, ConstZero) -> retain_a()
    | (OpShiftRight, (ConstInt ia), (ConstInt ib)) -> ((Some (ConstInt (Int64.shift_right ia (Int64.to_int ib)))), None)

    | _ -> (None, None) in
    finalize_cfold_result c_opt a_opt res_t loc

let cfold_uop uop a res_t loc =
    let at = get_atom_ktyp a loc in
    let ac = classify_atom a false in
    let retain_a () = retain_atom ac a at in
    let (c_opt, a_opt) =
    match (uop, ac) with
    | (OpPlus, NonConst) -> retain_a()
    | (OpPlus, ConstInt _) -> retain_a()
    | (OpPlus, ConstFloat _) -> retain_a()
    | (OpNegate, ConstInt x) -> ((Some (ConstInt (Int64.neg x))), None)
    | (OpNegate, ConstFloat x) -> ((Some (ConstFloat (-. x))), None)
    | (OpBitwiseNot, ConstInt x) -> ((Some (ConstInt (Int64.lognot x))), None)
    | (OpBitwiseNot, ConstBool x) -> ((Some (ConstBool (x = false))), None)
    | (OpLogicNot, ConstBool x) -> ((Some (ConstBool (x = false))), None)
    | _ -> (None, None) in
    finalize_cfold_result c_opt a_opt res_t loc

let cfold_cast a res_t loc =
    let at = get_atom_ktyp a loc in
    let ac = classify_atom a false in
    let (c_opt, a_opt) = retain_atom ac a at in
    finalize_cfold_result c_opt a_opt res_t loc

let print_subst_map m loc =
    printf "subsitution map {\n";
    Env.iter (fun n a ->
        printf "\t%s: " (id2str n);
        K_pp.pprint_atom_x a loc;
        printf ";\n") m;
    printf "}\n"

let cfold_dealias kmods =
    let id_map = ref (Env.empty : atom_t Env.t) in
    let concat_map = ref (Env.empty : atom_t list Env.t) in
    let add_to_map n a = id_map := Env.add n a !id_map in
    let add_to_concat_map n al = concat_map := Env.add n al !concat_map in
    let rec cfd_atom_ a loc callb =
        match a with
        | AtomId n ->
            (match (Env.find_opt n !id_map) with
            | Some a2 ->
                a2
            | _ -> a)
        | _ -> a
    and cfd_ktyp_ t loc callb = t
    and cfd_kexp_ e callb =
        (* [TODO] add handling of tuple/record construction with
           subsequent field access, e.g.
           val a = ...
           val b = ...
           val mytup = (a, b, 1)
           ...
           val (x, y, z) = mytup // equivalent to x = a, y = b, z = 1
           *)

        (* first, process all the sub-expressions; the only exception is KDefVal,
           which we handle separately *)
        let e = match e with
            | KDefVal _ -> e
            | _ -> walk_kexp e callb in
        match e with
        | KDefVal(n, rhs_e, loc) ->
            (*printf "defval: "; K_pp.pprint_kexp_x e; printf "\n";*)
            let rhs_e = cfd_kexp_ rhs_e callb in
            let n = match cfd_atom_ (AtomId n) loc callb with
                        | AtomId n2 -> n2
                        | _ -> n in
            let e = KDefVal(n, rhs_e, loc) in
            if not (is_mutable n loc) then
                (match rhs_e with
                | KExpAtom(a, (_, loc2)) ->
                    (match a with
                    | AtomId n2 ->
                        (* in order to do a safe substitution, both values
                            must be immutable, otherwise the change may affect the semantics
                        *)
                        if not (is_mutable n2 loc2) then
                            match (n, n2) with
                            | ((Id.Val _), (Id.Temp _)) ->
                                (* if a temporary value is assigned to the user-defined value,
                                    we'd better keep the user-specified name so that the output code is cleaner.
                                    We will do the inverse substitution n2<-n rather than n<-n2 *)
                                add_to_map n2 (AtomId n);
                                set_idk_entry n (kinfo_ n2 loc);
                                KExpNop(loc)
                            | _ ->
                                add_to_map n (AtomId n2);
                                KExpNop(loc)
                        else
                            e
                    | AtomLit c ->
                        (*printf "will replace '%s' with literal '%s'\n" (id2str n) (Ast_pp.lit2str c);*)
                        add_to_map n a;
                        KExpNop(loc))
                | KExpIntrin(IntrinStrConcat, al, (_, loc)) when
                    List.for_all (fun a -> not (is_mutable_atom a loc)) al ->
                    add_to_concat_map n al; e
                | _ -> e)
            else e
        | KExpIntrin(IntrinStrConcat, al, (res_t, loc)) ->
            let try_cfold_str_concat a res_al =
                match (a, res_al) with
                | (AtomLit (KLitChar s1), (AtomLit (KLitChar s2) :: rest))
                | (AtomLit (KLitString s1), (AtomLit (KLitChar s2) :: rest))
                | (AtomLit (KLitChar s1), (AtomLit (KLitString s2) :: rest))
                | (AtomLit (KLitString s1), (AtomLit (KLitString s2) :: rest)) ->
                    (* the order s2 + s1 is correct here, since we operate on reversed lists *)
                    AtomLit (KLitString (s2 ^ s1)) :: rest
                | (AtomLit (KLitString ""), res_al) -> res_al
                | (a, (AtomLit (KLitString "") :: rest)) -> a :: rest
                | _ -> a :: res_al
                in
            let res_al = List.fold_left (fun res_al a ->
                match a with
                | AtomId n ->
                    (match (Env.find_opt n !concat_map) with
                    | Some(a :: rest) -> (List.rev rest) @ (try_cfold_str_concat a res_al)
                    | _ -> a :: res_al)
                | _ -> try_cfold_str_concat a res_al) [] al
                in
            (match res_al with
            | a :: [] when (get_atom_ktyp a loc) = KTypString -> KExpAtom(a, (res_t, loc))
            | _ -> KExpIntrin(IntrinStrConcat, (List.rev res_al), (res_t, loc)))
        | KExpBinary(bop, a, b, (res_t, loc)) ->
            (match (cfold_bop bop a b res_t loc) with
            | Some(new_e) -> new_e
            | _ -> e)
        | KExpUnary(uop, a, (res_t, loc)) ->
            (match (cfold_uop uop a res_t loc) with
            | Some(new_e) -> new_e
            | _ -> e)
        | KExpCast(a, t, loc) ->
            (match (cfold_cast a t loc) with
            | Some(new_e) -> new_e
            | _ -> e)
        | KExpIf(c, then_e, else_e, kctx) ->
            (* eliminate dead branches *)
            (match c with
            | KExpAtom(AtomLit (KLitBool true), _) -> then_e
            | KExpAtom(AtomLit (KLitBool false), _) -> else_e
            | _ -> KExpIf(c, then_e, else_e, kctx))
        | KExpWhile(c, body, loc) ->
            (* eliminate dead branches *)
            (match c with
            | KExpAtom(AtomLit (KLitBool false), _) -> KExpNop(loc)
            | _ -> e)
        (* we do not convert KExpDoWhile(body, false, loc)
           into a nested KExpSeq(), because then we will have to
           correctly transform the nested break and continue operators, if any.
           For now, leave it as-is *)
        | KExpMatch _ ->
            (match e with
            | KExpMatch(cases, ((match_ktyp, match_loc) as kctx)) ->
                (* in general, the KExpMatch() operator has the following form
                    if( {expressions11 ... actual_check11} &&
                        {expressions12 ... actual_check12} && ... )
                        action1
                    else if( {expressions21 ... actual_check21} && ... )
                        action2
                    ...
                    else
                        // k-normalization step makes sure that there is always the 'else' branch.
                        actionN.

                    If some of the actual_check(i,j) are constant "true" or "false",
                    we can optimize the conditions a bit:

                    * if some actual_check(i,j) === false,
                      then the remaining checks {expressions(i,k) ... actual_check(i,k)} for k > j can be removed;
                      the corresponding action(i) can probably be converted into a 'nop' (or 'nil').
                    * if some actual_check(i,j) === true then
                      expressions(i,j) ... should be added to the next expressions(i,j+1).
                      if it was the last check in case, then the expressions should be added
                      to the action(i).
                      if there was just one check, i.e. we have
                      "...
                       else if( {expressions(i,1) ... true }) action(i)
                       ...", then we replace it with
                       "... else { expressions(i,1) ... action(i) }". The remaining cases are removed.
                       if i=1 then the whole KExpMatch is replaced with KExpSeq({expressions(1,1) ... action(1)}).
                    Sounds rather complex, but the code is not much longer than this description.
                *)
                let rec process_case_checks checks next_check_code result_checks =
                    match checks with
                    | c :: other_checks ->
                        let code = next_check_code @ (kexp2code c) in
                        (match code with
                        | [] -> process_case_checks other_checks [] result_checks
                        | _ ->
                            let actual_check = Utils.last_elem code in
                            (match actual_check with
                            | KExpAtom((AtomLit (KLitBool false)), _) ->
                                (* skip the remaining checks,
                                   report that the action will not be executed *)
                                let loc = get_kexp_loc actual_check in
                                let new_c = code2kexp code loc in
                                (false, [], List.rev (new_c :: result_checks))
                            | KExpAtom((AtomLit (KLitBool true)), _) ->
                                let code_wo_check = List.rev (List.tl (List.rev code)) in
                                process_case_checks other_checks code_wo_check result_checks
                            | _ ->
                                let loc = get_kexp_loc actual_check in
                                let new_c = code2kexp code loc in
                                process_case_checks other_checks [] (new_c :: result_checks)))
                    | _ -> (true, next_check_code, (List.rev result_checks))
                and process_cases cases result =
                    match cases with
                    | (checks, e) :: other_cases ->
                        if other_cases = [] && checks != [] then
                            raise_compile_err match_loc "the match does not have the else branch"
                        else ();
                        let (keep_action, action_extra_code, checks) = process_case_checks checks [] [] in
                        let eloc = get_kexp_loc e in
                        let new_action = if keep_action then
                                code2kexp (action_extra_code @ (e :: [])) eloc
                            else (match match_ktyp with
                                | KTypVoid -> KExpNop(eloc)
                                | _ -> KExpAtom((AtomLit (KLitNil match_ktyp)), (match_ktyp, eloc))) in
                        (match (keep_action, checks) with
                        | (false, (KExpAtom((AtomLit (KLitBool false)), _)) :: []) ->
                            (* drop the case completely, because the check is trivial (==FALSE) *)
                            process_cases other_cases result
                        | (true, []) ->
                            (* no checks; it means we have 'else' or the new 'else' case;
                               we can skip the rest *)
                            List.rev (([], new_action) :: result)
                        | _ ->
                            process_cases other_cases ((checks, new_action) :: result))
                    | _ -> List.rev result in
                let cases = process_cases cases [] in
                (match cases with
                | ([], else_action) :: [] ->
                    else_action
                | _ -> KExpMatch(cases, kctx))
            | _ -> e)
        | _ -> walk_kexp e callb in
    let cfd_callb =
    {
        kcb_atom=Some(cfd_atom_);
        kcb_ktyp=Some(cfd_ktyp_);
        kcb_kexp=Some(cfd_kexp_)
    } in
    List.map (fun km ->
        let {km_top=top_code} = km in
        (* do 2 passes to implement the backward subsitution properly
        (see the comments in KDefVal case above) *)
        let top_code = List.map (fun e -> cfd_kexp_ e cfd_callb) top_code in
        let top_code = List.map (fun e -> cfd_kexp_ e cfd_callb) top_code in
        (*print_subst_map !id_map;*)
        {km with km_top=top_code}) kmods
