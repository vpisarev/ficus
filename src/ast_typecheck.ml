(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* The type checker *)

open Ast
open Options

(*
The type checker component performs semantical analysis and various
sanity checks of just parsed Ficus module(s).

In theory, this stage, together with the previous lexical and syntactic analysis stages,
ensures that the code is valid, typesafe and can be compiled to a valid C/machine code.
In practice, however, some checks are better to be postponed to further stages.
Nevertheless, the first 3 stages (lexer, parser and the typechecker) are responsible
for reporting majority of the problems in the code.

These are the tasks performed by type checker:
  * infere the missing type specifications. In Ficus one must explicitly define function argument types
    and may also optionally specify function return types, types of the declared values/variables and
    even types of some expressions using (exp: type) syntax (e.g. (None: int option)). Unop completion of
    the type checker stage, each symbol is given the proper (and the only possible) type.
    When the type cannot be inferenced, an error is reported by the type checker or at the subsequent
    K-normalization step.
  * find the proper match for each abstract symbol referenced in the program. This part deals with
    overloaded functions, generic functions, overriden symbols, etc. It also recognizes '.' notation,
    such as "Modulename.symbolname" or "record_instance_name.record_field" and looks inside imported modules
    or records to find the proper match.
  * as a part of the previous step, instantiate generic functions and types.
    Unlike some functional languages, such as OCaml, and similar to some other languages with generics, like C++,
    Ficus compiler deals with generic definitions only at the type checker stage. Later on, only
    concrete instances, used by the rest of the code, survive.
    They are processed further and put into the final C code/binary.
  * make sure that explicitly specified and inferenced types do not contradict each other
    and follow the language semantics.
    For example, condition in "if" statement must have type "bool",
    "then-" and "else-" branches must have the same type, etc.
  * do various sanity checks, e.g.
    ** a type with the same name cannot be redefined in the same scope,
    ** a function with the same name and same types of input parameters cannot be redefined in the same scope,
    ** there cannot be duplicated names in function arguments and in any other pattern.
    ** etc.
*)

let pprint_typ_x = Ast_pp.pprint_typ_x
let pprint_exp_x = Ast_pp.pprint_exp_x
let pprint_pat_x = Ast_pp.pprint_pat_x

let print_env msg env loc =
    printf "%s. env at %s [\n" msg (loc2str loc);
    Env.iter (fun k entries -> printf "\t%s:" (id2str k); (List.iter (fun e ->
        match e with
        | EnvId n -> printf " %s" (id2str n);
            let templ_inst =
                match id_info n with
                | IdFun {contents={df_templ_inst}} -> n :: df_templ_inst
                | IdVariant {contents={dvar_templ_inst}} -> n :: dvar_templ_inst
                | _ -> []
            in if templ_inst = [] then () else
                (List.iteri (fun i ni ->
                    let t = match id_info ni with
                        | IdFun {contents={df_typ}} -> df_typ
                        | IdVariant {contents={dvar_alias}} -> dvar_alias
                        | IdTyp {contents={dt_typ}} -> dt_typ
                        | _ -> TypVar(ref None)
                    in printf "%s%s<" (if i = 0 then " => [" else ", ") (id2str ni);
                    pprint_typ_x t loc; printf ">") templ_inst;
                printf "]")
        | EnvTyp t -> printf "<some type>") entries);
        printf ";\n") env;
    printf "]\n"

(*
  Try to match (i.e. unify) two types,
  possibly indirectly represented or unknown/undefined.
  We use slightly extended type unification algorithm from min-caml.
  In fact, it's [almost] canonical type unification algorithm
  using so-called destructive unification. The only difference from
  the canonical algorithm is that we memorize all the destructive
  changes in unify_undo_stack, so that we can restore all the
  unsuccessfully matched types. Why do we need to recover from
  the unsuccessful unification?
  This is because Ficus allows overloaded functions, so that we
  should iterate through a list of candidates and
  try to unify them one by one with the searched type.
  We only throw a type unification error
  (or rather "overloaded function not found" error)
  in the very end, when we are out of candidates.
*)
let rec maybe_unify t1 t2 update_refs =
    (*let _ = (printf "\n<<<trying to unify types: "; pprint_typ_x t1 noloc; printf " and "; pprint_typ_x t2 noloc; printf "\n"; flush stdout) in*)
    let undo_stack = ref [] in
    let rec_undo_stack = ref [] in
    (* checks if a reference to undefined type (an argument of TypVar (ref None))
       occurs in the other type (t2). If yes, then we have a cyclic dependency
       between types and so they cannot in principle be unified (i.e. no type
       cannot be unified with a part of it).
       In all other cases the undefined type can be unified with t2. *)
    let rec occurs r1 t2 =
        (match t2 with
        | TypFun(args2, rt2) -> List.exists (occurs r1) args2 || occurs r1 rt2
        | TypList(t2_) -> occurs r1 t2_
        | TypTuple tl2 -> List.exists (occurs r1) tl2
        | TypVarTuple (Some (t2_)) -> occurs r1 t2_
        | TypVarTuple _ -> false
        | TypRef(t2_) -> occurs r1 t2_
        | TypArray(_, et2) -> occurs r1 et2
        | TypVarArray et2 -> occurs r1 et2
        | TypRecord {contents=(relems2, _)} ->
            List.exists (fun (_, t, _) -> occurs r1 t) relems2
        | TypVar(r2) when r1 == r2 -> true
        | TypVar({ contents = None }) -> false
        | TypVar({ contents = Some(t2_) }) -> occurs r1 t2_
        | TypApp(tl2, _) -> List.exists (occurs r1) tl2
        | TypInt | TypSInt _ | TypUInt _ | TypFloat _ | TypString | TypChar
        | TypBool | TypVoid | TypExn | TypErr | TypCPointer | TypDecl | TypModule | TypVarRecord -> false) in
    let rec maybe_unify_ t1 t2 =
        (*let _ = (printf "\tmaybe_unify_: "; pprint_typ_x t1 noloc; printf " and "; pprint_typ_x t2 noloc; printf "\n"; flush stdout) in*)
        (match (t1, t2) with
        (* a type should be unified successfully with itself *)
        | (TypInt, TypInt) | (TypString, TypString) | (TypChar, TypChar)
        | (TypBool, TypBool) | (TypVoid, TypVoid) | (TypExn, TypExn)
        | (TypCPointer, TypCPointer) | (TypDecl, TypDecl) | (TypModule, TypModule) -> true
        | (TypSInt (bits1), TypSInt (bits2)) -> bits1 = bits2
        | (TypUInt (bits1), TypUInt (bits2)) -> bits1 = bits2
        | (TypFloat (bits1), TypFloat (bits2)) -> bits1 = bits2
        | (TypFun (args1, rt1), TypFun (args2, rt2)) ->
            (List.length args1) = (List.length args2) &&
            (List.for_all2 maybe_unify_ args1 args2) &&
            (maybe_unify_ rt1 rt2)
        | (TypList (et1), TypList (et2)) -> maybe_unify_ et1 et2
        | (TypTuple (tl1), TypTuple (tl2)) ->
            (List.length tl1) = (List.length tl2) &&
            List.for_all2 maybe_unify_ tl1 tl2
        | (TypVar ({contents=Some(TypVarTuple t1_opt)} as r1), TypVar ({contents=Some(TypVarTuple t2_opt)} as r2)) ->
            if r1 == r2 then true
            else if (occurs r2 t1) || (occurs r1 t2) then false
            else if (match (t1_opt, t2_opt) with
                | (Some t1_), (Some t2_) -> maybe_unify_ t1_ t2_
                | _ -> true) then
                (undo_stack := (r2, !r2) :: !undo_stack;
                r2 := Some(t1); true)
            else false
        | (TypVar {contents=Some(TypVarTuple _)}, TypVar {contents=Some(t2_)}) ->
            maybe_unify_ t2_ t1
        | (TypVar {contents=Some(TypVarTuple _)}, TypTuple _) ->
            maybe_unify_ t2 t1
        | (TypTuple tl1, TypVar ({contents=Some(TypVarTuple t2_opt)} as r2)) ->
            if List.exists (occurs r2) tl1 then false
            else if (match t2_opt with
                | Some t2_ -> List.for_all (maybe_unify_ t2_) tl1
                | _ -> true) then
                (undo_stack := (r2, !r2) :: !undo_stack;
                r2 := Some(t1); true)
            else false
        | (TypVar ({contents=Some(TypVarRecord)} as r1), t2) ->
            let t2 = deref_typ t2 in
            (match t2 with
            | TypVar ({contents=Some(TypVarRecord)} as r2) ->
                if r1 == r2 then ()
                else (undo_stack := (r2, !r2) :: !undo_stack; r2 := Some(t1));
                true
            | TypVar ({contents=None} as r2) ->
                undo_stack := (r2, !r2) :: !undo_stack; r2 := Some(t1);
                true
            | TypRecord _ ->
                undo_stack := (r1, !r1) :: !undo_stack; r1 := Some(t2);
                true
            | TypApp(t_args, tn) ->
                (match (id_info tn) with
                | IdVariant {contents={dvar_cases=(_, TypRecord _) :: []}}
                    ->
                    undo_stack := (r1, !r1) :: !undo_stack; r1 := Some(t2);
                    true
                | _ -> false)
            | _ -> false)
        | (_, TypVar {contents=Some(TypVarRecord)}) ->
            maybe_unify_ t2 t1
        | ((TypRef drt1), (TypRef drt2)) -> maybe_unify_ drt1 drt2
        | (TypRecord r1, TypRecord r2) when r1 == r2 -> true
        | (TypRecord {contents=(_, false)}, TypRecord {contents=(_, true)}) -> maybe_unify_ t2 t1
        | (TypRecord r1, TypRecord r2) ->
            let ok = match (!r1, !r2) with
                | ((relems1, true), (relems2, true)) ->
                    (List.length relems1) = (List.length relems2) &&
                    (List.for_all2 (fun (n1, t1, _) (n2, t2, _) ->
                        n1 = n2 && maybe_unify_ t1 t2) relems1 relems2)
                | ((relems1, _), (relems2, _)) ->
                    let have_all_matches = List.for_all (fun (n1, t1, v1opt) ->
                        (List.exists (fun (n2, t2, _) -> n1 = n2 && maybe_unify_ t1 t2) relems2)
                        || (Utils.is_some v1opt)) relems1 in
                    (* if both the record types are unknown then all the v1opt's in relems1
                       are None's. Since we do not have duplicates, which is checked by the parser,
                       then if for each record field in relems1 we have a match in relems2
                       then len(relems2) >= len(relems1).
                       So, below we put the opposite check,
                       which ensures that len(relems1) = len(relems2). *)
                    have_all_matches && (List.length relems1) >= (List.length relems2)
            in if ok then
                (rec_undo_stack := (r2, !r2) :: !rec_undo_stack; r2 := !r1)
            else (); ok
        | (TypArray (d1, et1), TypArray (d2, et2)) ->
            d1 = d2 && maybe_unify_ et1 et2
        | (TypVar ({contents=Some(TypVarArray t1_)} as r1), TypVar ({contents=Some(TypVarArray t2_)} as r2)) ->
            if r1 == r2 then true
            else if (occurs r2 t1) || (occurs r1 t2) then false
            else if maybe_unify_ t1_ t2_ then
                (undo_stack := (r2, !r2) :: !undo_stack;
                r2 := Some(t1); true)
            else false
        | (TypVar {contents=Some(TypVarArray _)}, TypVar {contents=Some(t2_)}) ->
            maybe_unify_ t2_ t1
        | (TypVar {contents=Some(TypVarArray _)}, TypArray _) ->
            maybe_unify_ t2 t1
        | (TypArray (d1, et1), TypVar ({contents=Some(TypVarArray (et2))} as r2)) ->
            if occurs r2 et1 then false
            else if maybe_unify_ et1 et2 then
                (undo_stack := (r2, !r2) :: !undo_stack;
                r2 := Some(t1); true)
            else false
        | (TypApp (args1, id1), TypApp (args2, id2)) ->
            if id1 <> id2 then false
            else
                let t1 = match args1 with [] -> TypVoid | t :: [] -> t | _ -> TypTuple args1 in
                let t2 = match args2 with [] -> TypVoid | t :: [] -> t | _ -> TypTuple args2 in
                maybe_unify_ t1 t2
        (* unify TypVar(_) with another TypVar(_): consider several cases *)
        | (TypVar (r1), TypVar (r2)) when r1 == r2 -> true
        | (TypVar {contents = Some(t1_)}, _) -> maybe_unify_ t1_ t2
        | (_, TypVar {contents = Some(t2_)}) -> maybe_unify_ t1 t2_
        | (TypVar ({contents = None} as r1), _) ->
            if occurs r1 t2 then false else
            (* This is the destructive unification step:
               if there is unknown type t1 = TypVar(ref None), and it's not
               equivalent to t2, neither is a part of t2, then we unify it with t2.
               Before that, we update the undo stack. There is no need
               to memorize the previous value of r1, because we know that it's None. *)
            ((match t2 with
            | TypErr -> ()
            | _ -> undo_stack := (r1, !r1) :: !undo_stack; r1 := Some(t2)); true)
          (* symmetrical case *)
        | (_, (TypVar ({contents = None} as r2))) ->
            if occurs r2 t1 then false else
            ((match t1 with
            | TypErr -> ()
            | _ -> undo_stack := (r2, !r2) :: !undo_stack; r2 := Some(t1)); true)
        (* a declaration cannot be unified with any non-decl type *)
        | (TypErr, _ ) -> true
        | (_, TypErr) -> true
        | (TypDecl, _) | (_, TypDecl) -> false
        (* in all other cases the types cannot be unified *)
        | (_, _) -> false) in

    let ok = maybe_unify_ t1 t2 in
    (*((printf "result (%B): " ok); pprint_typ_x t1 noloc; printf " and "; pprint_typ_x t2 noloc; printf ">>>\n");*)
    if ok && update_refs then () else
    (* restore the original types in the case of type unification failure
       or when update_refs=false *)
        ((List.iter (fun (r, prev) -> r := prev) !rec_undo_stack);
        (List.iter (fun (r, old_val) -> r := old_val) !undo_stack));
    ok

(* this is another flavor of type unification function;
   it throws an exception in the case of failure *)
let unify t1 t2 loc msg =
    if maybe_unify t1 t2 true then () else
    raise_compile_err loc msg

let coerce_types t1 t2 allow_tuples allow_fp is_shift loc =
    let safe_max_ubits = if options.arch64 then 32 else 16 in
    let rec coerce_types_ t1 t2 =
    match (t1, t2) with
    | (TypInt, TypInt) -> TypInt
    | (TypSInt b1), (TypSInt b2) ->
        let b = max b1 b2 in if b <= 32 then TypInt else TypSInt(64)
    | (TypUInt b1), (TypUInt b2) ->
        let b = max b1 b2 in
        if b <= safe_max_ubits then TypInt else TypUInt(b)
    | TypInt, (TypSInt b) -> if b <= 32 then TypInt else TypSInt(b)
    | TypInt, (TypUInt b) -> if b <= safe_max_ubits then TypInt else
        raise_compile_err loc
            "implicit type coercion for (int, uint32/uint64) pair is not allowed; use explicit type cast"
    | (TypSInt b), TypInt -> if is_shift then TypSInt(b) else if b <= 32 then TypInt else TypSInt(b)
    | (TypUInt b), TypInt -> if is_shift then TypUInt(b) else if b <= safe_max_ubits then TypInt else
        raise_compile_err loc
            "implicit type coercion for (int, uint32/uint64) pair is not allowed; use explicit type cast"
    | (TypSInt b1), (TypUInt b2) ->
        if b1 <= 32 && b2 <= safe_max_ubits then TypInt else
        raise_compile_err loc
            "implicit type coercion for this (signed, unsigned) pair of integer is not allowed; use explicit type cast"
    | (TypUInt b1), (TypSInt b2) ->
        if b1 <= safe_max_ubits && b2 <= 32 then TypInt else
        raise_compile_err loc
            "implicit type coercion for this (unsigned, signed) pair of integer is not allowed; use explicit type cast"
    | ((TypFloat b1), (TypFloat b2)) when allow_fp ->
        let max_b = max (max b1 b2) 32 in TypFloat(max_b)
    | ((TypFloat b), TypInt) when allow_fp -> TypFloat(max b 32)
    | ((TypFloat b), (TypSInt _)) when allow_fp -> TypFloat(max b 32)
    | ((TypFloat b), (TypUInt _)) when allow_fp -> TypFloat(max b 32)
    | (TypInt, (TypFloat b)) when allow_fp -> TypFloat(max b 32)
    | ((TypSInt _), (TypFloat b)) when allow_fp -> TypFloat(max b 32)
    | ((TypUInt _), (TypFloat b)) when allow_fp -> TypFloat(max b 32)
    | (TypBool, TypBool) -> TypInt
    | (TypBool, t2) -> coerce_types_ TypInt t2
    | (t1, TypBool) -> coerce_types_ t1 TypInt
    | (TypTuple tl1, TypTuple tl2) ->
        if not allow_tuples then
            raise_compile_err loc "tuples are not allowed in this operation"
        else if (List.length tl1) = (List.length tl2) then
            TypTuple (List.map2 (fun et1 et2 -> coerce_types_ et1 et2) tl1 tl2)
        else
            raise_compile_err loc "tuples have different number of elements"
    | _ -> raise_compile_err loc "the types cannot be implicitly coerced; use explicit type cast"
    in try Some(coerce_types_ (deref_typ t1) (deref_typ t2)) with CompileError(_, _) -> None

let find_all n env =
    match Env.find_opt n env with
    | Some(l) -> l
    | _ -> []

let typ2constr t rt loc =
    match t with
    | TypVoid -> rt
    | _ -> TypFun((match t with
                  | TypTuple(tl) -> tl
                  | _ -> t :: []), rt)

let get_eseq_typ eseq =
    match eseq with
    | [] -> TypVoid
    | _ -> get_exp_typ(Utils.last_elem eseq)

let add_typ_to_env key t env =
    let entries = find_all key env in
    Env.add key ((EnvTyp t) :: entries) env

let add_id_to_env key i env =
    let entries = find_all key env in
    let entries = List.filter (fun e ->
    match e with
    | EnvId j -> i != j
    | _ -> true) entries in
    Env.add key ((EnvId i)::entries) env

let add_id_to_env_check key i env check =
    let entries = find_all key env in
    let entries = List.filter (fun e ->
    match e with
    | EnvId j ->
        if i != j then (check (id_info j); true) else false
    | _ -> true) entries in
    Env.add key ((EnvId i)::entries) env

let inst_merge_env env_from env_to =
    Env.fold (fun i entries new_env ->
        match Env.find_opt i env_to with
        | Some(prev_entries) ->
            (* add only those symbols from env_from which also exist in env_to;
               the goal is not to implement dynamic scope; the goal is to make
               function instantiation work correctly in the presense of
               locally overloaded functions *)
            let extra_entries = List.fold_left (fun extra_entries e ->
                match e with
                | EnvId i ->
                    let add = match (id_info i) with
                        | IdFun _ -> true
                        | _ -> false
                        in
                    if add && not (List.mem e prev_entries) then e :: extra_entries else extra_entries
                | _ -> extra_entries) [] entries
                in
            if extra_entries = [] then new_env
            else
                let new_entries = (List.rev extra_entries) @ prev_entries in
                Env.add i new_entries new_env
        | _ ->
            new_env) env_from env_to

let check_for_duplicate_typ key sc loc =
    (fun i ->
    match i with
    | IdTyp _ | IdVariant _ | IdClass _ | IdInterface _ ->
        if (List.hd (get_scope i)) = (List.hd sc) then
            raise_compile_err loc
                (sprintf "the type %s is re-declared in the same scope; the previous declaration is here %s"
                (pp_id2str key) (loc2str (get_idinfo_loc i)))
        else ()
    | _ -> ())

let check_for_rec_field_duplicates rfnames loc =
    List.iteri (fun i ni ->
        List.iteri (fun j nj -> if i > j && ni = nj then
                raise_compile_err loc (sprintf "duplicate record field '%s'" (id2str ni))
            else ())
        rfnames) rfnames

let finalize_record_typ t loc =
    let t = deref_typ (dup_typ t) in
    match t with
    | TypRecord ({contents=(relems, _)} as r) ->
        List.iter (fun (n, t, v_opt) ->
            match v_opt with
            | Some(v) -> unify t (get_lit_typ v) loc
                (sprintf "type of the field '%s' and its initializer do not match" (pp_id2str n))
            | _ -> ()) relems;
        TypRecord(r)
    | _ -> t

let find_typ_instance t loc =
    let t = deref_typ t in
    match t with
    | TypApp(targs, n) ->
        if targs = [] then Some t
        else
            let inst_list = (match (id_info n) with
                | IdVariant {contents={dvar_templ_inst}} -> dvar_templ_inst
                | _ -> [])
            in (try
                let n = List.find (fun inst ->
                    match (id_info inst) with
                    | IdVariant {contents={dvar_alias=inst_alias}} ->
                        maybe_unify t inst_alias false
                    | _ -> false)
                inst_list in Some (TypApp([], n))
            with Not_found -> None)
    | _ -> Some t

let get_record_elems vn_opt t loc =
    let t = deref_typ t in
    let input_vn = match vn_opt with
        | Some(vn) ->
            let orig_input_vn = get_orig_id vn in
            get_id (Utils.last_elem (String.split_on_char '.' (pp_id2str orig_input_vn)))
        | _ -> noid
        in
    match t with
    | TypRecord {contents=(relems, true)} -> (noid, relems)
    | TypRecord _ -> raise_compile_err loc "the records, which elements we request, is not finalized"
    | TypApp _ ->
        (match (find_typ_instance t loc) with
        | Some (TypApp([], n)) ->
            (match (id_info n) with
            | IdVariant {contents={dvar_flags;
                dvar_cases=(vn0, TypRecord {contents=(relems, true)}) :: []}}
                when dvar_flags.var_flag_record ->
                if input_vn = noid || input_vn = (get_orig_id vn0) then ()
                else raise_compile_err loc (sprintf "mismatch in the record name: given '%s', expected '%s'"
                    (pp_id2str input_vn) (pp_id2str vn0));
                (noid, relems)
            | IdVariant {contents={dvar_name; dvar_cases; dvar_ctors}} ->
                let dvar_cases_ctors = Utils.zip dvar_cases dvar_ctors in
                let single_case = (List.length dvar_cases) = 1 in
                (match (List.find_opt (fun ((vn, t), c_id) ->
                    (get_orig_id vn) = input_vn ||
                    (single_case && input_vn=noid)) dvar_cases_ctors) with
                | Some(((_, TypRecord {contents=(relems, true)}), ctor)) -> (ctor, relems)
                | _ -> raise_compile_err loc
                    (if input_vn = noid then
                    (sprintf "variant '%s' is not a record" (pp_id2str dvar_name))
                    else
                    (sprintf "tag '%s' is not found or '%s' is not a record" (pp_id2str input_vn) (pp_id2str dvar_name))))
            | _ -> raise_compile_err loc (sprintf "cannot find a proper record constructor in type '%s'" (id2str n)))
        | _ -> raise_compile_err loc "proper instance of the template [record?] type is not found")
    | _ -> raise_compile_err loc "attempt to treat non-record and non-variant as a record"

let is_real_typ t =
    let have_typ_vars = ref false in
    let rec is_real_typ_ t callb =
        match t with
        | TypApp([], (Id.Name _)) -> have_typ_vars := true; t
        | TypApp([], _) -> t
        | TypVar {contents=None} -> have_typ_vars := true; t
        | _ -> walk_typ t callb
    in
    let callb = { acb_typ = Some(is_real_typ_); acb_exp = None; acb_pat = None; } in
    let _ = is_real_typ_ t callb in
    not !have_typ_vars

let report_not_found n loc =
    raise_compile_err loc (sprintf "the appropriate match for '%s' is not found" (pp_id2str n))

let report_not_found_typed n t loc =
    raise_compile_err loc (sprintf "the appropriate match for '%s' of type '%s' is not found" (pp_id2str n) (typ2str t))

let rec find_first n env loc pred =
    let rec find_next_ elist =
        match elist with
        | e :: rest ->
            (match (pred e) with
            | Some x -> Some x
            | _ -> find_next_ rest)
        | _ -> None in
    let rec search_path n_path dot_pos env =
        try
            let dot_pos = String.rindex_from n_path dot_pos '.' in
            let prefix = String.sub n_path 0 dot_pos in
            let len = String.length n_path in
            (match find_all (get_id prefix) env with
            | EnvId m :: _ ->
                (match (id_info m) with
                | IdModule {contents={dm_env}} ->
                    let suffix = String.sub n_path (dot_pos+1) (len-dot_pos-1) in
                    find_first (get_id suffix) dm_env loc pred
                | _ -> None)
            | _ ->
                search_path n_path (dot_pos-1) env)
        with Not_found -> None
        in
    let e_all = find_all n env in
    let e_all = if e_all = [] && (is_unique_id n) then
        (EnvId n) :: [] else e_all in
    if e_all != [] then find_next_ e_all
    else
        let n_path = pp_id2str n in
        search_path n_path ((String.length n_path) - 1) env

let rec lookup_id n t env sc loc =
    match (find_first n env loc (fun e ->
        match e with
        | EnvId(Id.Name _) -> None
        | EnvId i ->
            (match id_info i with
            | IdVal {dv_typ} ->
                unify dv_typ t loc (sprintf "incorrect type of value '%s': expected='%s', actual='%s')"
                    (pp_id2str n) (typ2str t) (typ2str dv_typ)); Some((i, t))
            | IdFun df ->
                let { df_name; df_templ_args; df_typ; df_flags; df_env; df_scope } = !df in
                let t = match (df_flags.fun_flag_has_keywords, (deref_typ t)) with
                    | (true, TypFun(argtyps, rt)) ->
                        let argtyps = match (List.map deref_typ (List.rev argtyps)) with
                            | TypRecord {contents=(_, false)} :: _ -> argtyps
                            | _ -> argtyps @ [TypRecord {contents=([], false)}]
                            in
                        TypFun(argtyps, rt)
                    | _ -> t
                    in
                if df_templ_args = [] then
                    if maybe_unify df_typ t true then
                        Some((i, t))
                    else
                        None
                else
                    let (ftyp, env1) = preprocess_templ_typ df_templ_args df_typ df_env sc loc in
                    if not (maybe_unify ftyp t true) then None
                    else
                    (try
                        (* a necessary extra step to do before function instantiation;
                            if it's a constructor, we first need to check the return type.
                            this check may implicitly instantiate generic variant type, and so
                            the df_templ_inst list of this constructor will be expanded with new instance.
                            In theory, with such an extra step we should never enter
                            'instantiate_fun' for constructors, because they all will be
                            instantiated from check_typ => instantiate_variant => register_typ_constructor. *)
                        let return_orig =
                            if not (is_fun_ctor df_flags) then
                                false
                            else
                                match ftyp with
                                | TypFun(_, rt) ->
                                    ignore(check_typ rt env1 sc loc);
                                    false
                                | rt ->
                                    (*printf "got here for n='%s', t='%s'\n" (id2str n) (typ2str t);*)
                                    not (is_fixed_typ t)
                            in
                        if return_orig then
                            Some((df_name, t))
                        else
                            Some((List.find (fun inst ->
                            match id_info inst with
                            | IdFun {contents={df_typ=inst_typ}} ->
                                maybe_unify inst_typ t true
                            | _ -> raise_compile_err loc
                                (sprintf "invalid (non-function) instance %s of template function %s"
                                (id2str inst) (pp_id2str n))
                            ) (!df).df_templ_inst, t))
                    with Not_found ->
                        (* the generic function matches the requested type,
                            but there is no appropriate instance;
                            let's create a new one *)
                        let inst_env = inst_merge_env env env1 in
                        let { df_name=inst_name; df_typ=inst_typ } = !(instantiate_fun df ftyp inst_env sc loc true) in
                        unify inst_typ t loc "inconsistent type of the instantiated function";
                        Some(inst_name, t))
            | IdModule _ -> unify TypModule t loc "unexpected module name"; Some((i, t))
            | IdExn {contents={ dexn_typ; dexn_loc }} ->
                let ctyp = typ2constr dexn_typ TypExn dexn_loc in
                unify ctyp t loc "uncorrect type of exception constructor and/or its arguments";
                Some((i, t))
            | IdNone | IdTyp _ | IdVariant _
            | IdClass _ | IdInterface _ -> None)
        | EnvTyp _ -> None)) with
    | Some(x) -> x
    | None ->
        (*print_env (sprintf "env @ report_not_found for %s: " (id2str n)) env loc;*)
        (match (try_autogen_symbols n t env sc loc) with
        | Some(n1) -> n1
        | _ -> report_not_found_typed n t loc)

and try_autogen_symbols n t env sc loc =
    let nstr = id2str n in
    (*printf "try_autogen_symbol: nstr = %s; t = %s\n" nstr (typ2str (deref_typ t));*)
    match (nstr, (deref_typ_rec t)) with
    | ("__eq__", TypFun(TypApp([], n1) :: TypApp([], n2) :: [], TypBool))
        when n1 = n2 && (match (id_info n1) with IdVariant _ -> true | _ -> false) ->
        Some(lookup_id (get_id "__eq_variants__") t env sc loc)
    | _ -> None

and preprocess_templ_typ templ_args typ env sc loc =
    let t = dup_typ typ in
    let env1 = List.fold_left (fun env1 nt ->
        let t = make_new_typ () in
        add_typ_to_env nt t env1) env templ_args in
    ((check_typ t env1 sc loc), env1)

and check_for_duplicate_fun ftyp env sc loc =
    (fun i ->
    match i with
    | IdFun {contents={df_name; df_templ_args; df_typ; df_scope; df_loc}} ->
        if (List.hd df_scope) = (List.hd sc) then
            let (t, _) = preprocess_templ_typ df_templ_args df_typ env sc loc in
            if (maybe_unify t ftyp false) && (df_templ_args = []) then
                raise_compile_err loc
                    (sprintf "the symbol %s is re-declared in the same scope; the previous declaration is here %s"
                    (pp_id2str df_name) (loc2str df_loc))
            else ()
        else ()
    | IdExn {contents={dexn_name; dexn_typ; dexn_scope; dexn_loc}} ->
        if (List.hd dexn_scope) = (List.hd sc) then
            let t = typ2constr dexn_typ TypExn dexn_loc in
            if maybe_unify t ftyp false then
                raise_compile_err loc
                    (sprintf "the symbol %s is re-declared in the same scope; the previous declaration is here %s"
                    (pp_id2str dexn_name) (loc2str dexn_loc))
                else ()
        else ()
    | _ -> ())

and match_ty_templ_args actual_ty_args templ_args env def_loc inst_loc =
    let n_aty_args = List.length actual_ty_args in
    let n_templ_args = List.length templ_args in
    let norm_ty_args = (match (actual_ty_args, n_aty_args, n_templ_args) with
        | (_, m, n) when m = n -> actual_ty_args
        | ((TypTuple tl):: [], 1, n) when (List.length tl) = n -> tl
        | (_, _, 1) -> TypTuple(actual_ty_args) :: []
        | _ -> raise_compile_err inst_loc
        (sprintf "the number of actual type parameters and formal type parameters, as declared at\n\t%s,\n\tdo not match"
        (loc2str def_loc))) in
    List.fold_left2 (fun env n t -> add_typ_to_env n t env) env templ_args norm_ty_args

(*
    One of the main functions in type checker:
    infers the proper type of expression and recursively processes sub-expressions.

    Whenever possible, we try to do type unification before processing sub-expressions.
    This is because sometimes we know type of the outer expression (type of the expression result)
    and using this information we can possibly deduce the types of arguments.
    For example, in `5 :: []` operation we can figure out that the second argument has `int list` type,
    we do not have to write more explicit `5 :: ([]: int list)` expression.
    Or, in the case of some hypothetical "fun integrate(a: 't, b: 't, f: 't->'t)" we can just call it
    with `integrate(0.f, 10.f, Math.sin)` instead of more explicit `integrate(0.f, 10.f, (Math.sin: float->float))`.

    Also, note that the function does not modify the environment. When a complex expression
    is analyzed (such as for-loop), a temporary environment can be created with extra content
    (like iteration variables in the case of for loop), but that temporary environment
    is discared as soon the analysis is over. But the function may alter the global symbol table.

    The declarations should modify the environment, e.g. `val (a, b) = (cos(0.5), sin(0.5))` should introduce
    some fresh ids like `a12345`, `b12346`, add the pairs (a, a12345) and (b, b12346) into the environment
    and then analyze the subsequent expressions in the same scope using this augmented environment,
    but this is done in check_eseq() function.
*)
and check_exp e env sc =
    (*let _ = (match e with ExpSeq _ -> () | _ -> (printf "checking exp at (%s): "
        (loc2str (get_exp_loc e))); (pprint_exp_x e); printf "\n========\n") in*)
    let (etyp, eloc) as ctx = get_exp_ctx e in
    let check_for_clause p e env idset sc =
        let e = check_exp e env sc in
        let is_range = match e with ExpRange _ -> true | _ -> false in
        let (etyp, eloc) = get_exp_ctx e in
        let etyp = deref_typ etyp in
        let (_, relems) = match etyp with
            | TypApp _ ->
                (try
                    get_record_elems None etyp eloc
                with CompileError _ -> (noid, []))
            | _ -> (noid, [])
            in
        match (is_range, etyp, relems) with
        | (false, (TypTuple tl), _) ->
            let tup_pat = PatIdent((gen_temp_id "tup"), eloc) in
            let (tup_pat, env, idset, _, _) = check_pat tup_pat etyp env idset IdSet.empty sc false true false in
            let def_e = DefVal(tup_pat, e, default_tempval_flags(), eloc) in
            let tup_e = match tup_pat with
                | PatIdent(i, _) -> ExpIdent(i, (etyp, eloc))
                | PatTyped(PatIdent(i, _), _, _) -> ExpIdent(i, (etyp, eloc))
                | _ -> raise_compile_err eloc "for over tuple: invalid result of check_pat result (id is expected)"
                in
            ((List.length tl), (p, tup_e), [def_e], 1, env, idset)
        | (false, _, relems) when relems != [] ->
            let rec_pat = PatIdent((gen_temp_id "rec"), eloc) in
            let (rec_pat, env, idset, _, _) = check_pat rec_pat etyp env idset IdSet.empty sc false true false in
            let def_e = DefVal(rec_pat, e, default_tempval_flags(), eloc) in
            let rec_e = match rec_pat with
                | PatIdent(i, _) -> ExpIdent(i, (etyp, eloc))
                | PatTyped(PatIdent(i, _), _, _) -> ExpIdent(i, (etyp, eloc))
                | _ -> raise_compile_err eloc "for over record: invalid result of check_pat result (id is expected)"
                in
            ((List.length relems), (p, rec_e), [def_e], 1, env, idset)
        | _ ->
            let (ptyp, dims) = (match (e, etyp) with
                | (ExpRange(_, _, _, _), _) -> (TypInt, 1)
                | (_, TypArray(d, et)) -> (et, d)
                | (_, TypList(et)) -> (et, 1)
                | (_, TypString) -> (TypChar, 1)
                | _ -> raise_compile_err eloc "unsupported iteration domain; it should be a range, array, list, string, tuple or a record")
                in
            let (p, env, idset, _, _) = check_pat p ptyp env idset IdSet.empty sc false true false in
            (0, (p, e), [], dims, env, idset)
    in
    let check_for_clauses for_clauses idx_pat env idset for_sc =
        let (_, trsz, for_clauses, code, dims, env, idset) = List.fold_left
            (fun (idx, trsz, for_clauses, code, dims, env, idset) (pi, ei) ->
                let (trszj, (pi, ei), code_i, dims_i, env, idset) = check_for_clause pi ei env idset for_sc in
                if idx > 0 && dims_i != dims then
                    raise_compile_err (get_exp_loc ei)
                        "the dimensionalities of simultaneously iterated containers/ranges do not match"
                else ();
                if idx > 0 && trszj != trsz then
                    raise_compile_err (get_exp_loc ei)
                        (if (trsz = 0 || trszj = 0) then
                            "simultaneous iteration over tuples/records and some other containers or ranges is not supported yet"
                        else
                            "cannot iterate over tuples/records of different size")
                else ();
            (idx+1, trszj, ((pi, ei) :: for_clauses), (code_i @ code), dims_i, env, idset))
                (0, 0, [], [], 0, env, idset) for_clauses in
        let idx_typ = if dims = 1 then TypInt else TypTuple(List.init dims (fun _ -> TypInt)) in
        let (idx_pat, env, idset) = if trsz != 0 then (idx_pat, env, idset) else
            (match idx_pat with
            | PatAny _ -> (idx_pat, env, idset)
            | _ ->
                let (idx_pat, env, idset, _, _) = check_pat idx_pat idx_typ env idset
                    IdSet.empty for_sc false true false in
                (PatTyped(idx_pat, idx_typ, (get_pat_loc idx_pat)), env, idset))
            in
        (trsz, (List.rev code), (List.rev for_clauses), idx_pat, dims, env, idset)
    in
    let gen_for_in_tuprec_it idx for_clauses idx_pat body env for_sc =
        let sc = new_block_scope () :: for_sc in
        let (_, code, env, idset) = List.fold_left (fun (j, code, env, idset) (pj, trj) ->
            let (ttrj, locj) = get_exp_ctx trj in
            match ttrj with
            | TypTuple tl ->
                let tj = List.nth tl idx in
                let ej = ExpMem(trj, ExpLit(LitInt (Int64.of_int idx), (TypInt, locj)), (tj, locj)) in
                let pj = dup_pat pj in
                let (pj, env, idset, _, _) = check_pat pj tj env idset IdSet.empty sc false true false in
                let def_pj = DefVal(pj, ej, default_tempval_flags(), locj) in
                (j+1, (def_pj :: code), env, idset)
            | _ ->
                let (_, relems) = get_record_elems None ttrj locj in
                let (nj, tj, _) = List.nth relems idx in
                let ej = ExpMem(trj, ExpIdent(nj, (TypString, locj)), (tj, locj)) in
                let pj = dup_pat pj in
                let (pnj, pvj) = match (pat_skip_typed pj) with
                    | PatTuple(pnj :: pvj :: [], _) -> (pnj, pvj)
                    | PatAs(PatTuple(pnj :: pvj :: [], _), as_id, _)
                        when (match as_id with Id.Temp _ -> true | _ -> false) -> (pnj, pvj)
                    | _ -> raise_compile_err eloc "when iterating through record, a 2-element tuple should be used as pattern"
                    in
                let (pnj, env, idset, _, _) = check_pat pnj TypString env idset IdSet.empty sc false true false in
                let (pvj, env, idset, _, _) = check_pat pvj tj env idset IdSet.empty sc false true false in
                let def_pvj = DefVal(pvj, ej, default_tempval_flags(), locj) in
                let def_pnj = DefVal(pnj, ExpLit(LitString (pp_id2str nj),
                    (TypString, locj)), default_tempval_flags(), locj) in
                (j+1, (def_pvj :: def_pnj :: code), env, idset)) (0, [], env, IdSet.empty) for_clauses
            in
        let (code, env, idset) = match idx_pat with
            | PatAny _ -> (code, env, idset)
            | _ ->
                let idx_pat = dup_pat idx_pat in
                let (idx_pat, env, idset, _, _) = check_pat idx_pat TypInt env idset
                    IdSet.empty sc false true false in
                let idx_loc = get_pat_loc idx_pat in
                let def_idx = DefVal(idx_pat, ExpLit(LitInt (Int64.of_int idx),
                    (TypInt, idx_loc)), default_tempval_flags(), idx_loc) in
                ((def_idx :: code), env, idset)
            in
        let body = check_exp (dup_exp body) env sc in
        let (body_typ, body_loc) = get_exp_ctx body in
        ExpSeq(List.rev (body :: code), (body_typ, body_loc))
    in

    let check_inside_for expect_fold_loop isbr =
        let kw = if not isbr then "continue" else if expect_fold_loop then "fold break" else "break" in
        let rec check_inside_ sc =
            match sc with
            | ScTry _ :: _ ->
                raise_compile_err eloc (sprintf "cannot use '%s' inside 'try-catch' block" kw)
            | ScFun _ :: _ | ScModule _ :: _ | ScGlobal :: _ | ScClass _ :: _ | ScInterface _ :: _ | [] ->
                raise_compile_err eloc (sprintf "cannot use '%s' outside of loop" kw)
            | ScLoop (nested, _) :: _ ->
                if expect_fold_loop then
                    raise_compile_err eloc (sprintf "'%s' can only be used inside 'fold' loop" kw)
                else if isbr && nested then
                    raise_compile_err eloc
                    ("break cannot be used inside nested for-loop because of ambiguity.\n" ^
                    "\tUse explicit curly braces, e.g. 'for ... { for ... { for ... { break }}} to'\n" ^
                    "\texit a single for-loop, or use exceptions, e.g. standard Break exception to exit nested loops.")
                else ()
            | ScFold _ :: _ ->
                if expect_fold_loop then ()
                else raise_compile_err eloc (sprintf "cannot use '%s' inside 'fold' loop" kw)
            | ScArrMap _ :: _ ->
                raise_compile_err eloc (sprintf "cannot use '%s' inside array comprehension" kw)
            | ScMap _ :: _ ->
                if expect_fold_loop then
                    raise_compile_err eloc (sprintf "'%s' can only be used inside 'fold' loop" kw)
                else ()
            | ScBlock _ :: outer_sc ->
                check_inside_ outer_sc
        in
            check_inside_ sc
    in
    (* find the proper function and make "call" expression
       given that all the parameters are already type-checked *)
    let rec check_and_make_call f_id args =
        let arg_typs = List.map (fun a -> get_exp_typ a) args in
        let f_expected_typ = TypFun(arg_typs, etyp) in
        let f_exp = ExpIdent(f_id, (make_new_typ(), eloc)) in
        let (f_real_typ, floc) = get_exp_ctx f_exp in
        let _ = unify f_real_typ f_expected_typ floc "the real and expected function type do not match" in
        let new_f = check_exp f_exp env sc in
        ExpCall(new_f, args, ctx)
    in
    let new_e =
    (match e with
    | ExpNop(_) -> e
    | ExpRange(e1_opt, e2_opt, e3_opt, _) ->
        let check_range_e e_opt =
            (match e_opt with
            | None -> None
            | Some(e) ->
                let (etyp1, eloc1) = get_exp_ctx e in
                let _ = unify etyp1 TypInt eloc1 "explicitly specified component of a range must be an integer" in
                let new_e = check_exp e env sc in
                Some new_e) in
        let new_e1_opt = check_range_e e1_opt in
        let new_e2_opt = check_range_e e2_opt in
        let new_e3_opt = match e3_opt with Some(_) -> check_range_e e3_opt | _ -> Some(ExpLit((LitInt 1L), (TypInt, eloc))) in
        unify etyp (TypTuple [TypInt;TypInt;TypInt]) eloc "the range type should have (int, int, int) type";
        ExpRange(new_e1_opt, new_e2_opt, new_e3_opt, ctx)
    | ExpLit(lit, _) -> unify etyp (get_lit_typ lit) eloc "the literal has improper type"; e
    | ExpIdent(n, _) ->
        (*let _ = if (pp_id2str n) = "println_tup" then
            printf "break here\n"
            else ()
            in*)
        let (n, t) = lookup_id n etyp env sc eloc in
        ExpIdent(n, (t, eloc))
    | ExpMem(e1, e2, _) ->
        (* in the case of '.' operation we do not check e2 immediately after e1,
           because e2 is a 'member' of a structure/module e1,
           so it cannot be independently analyzed *)
        let new_e1 = check_exp e1 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        let etyp1 = deref_typ etyp1 in
        (match (etyp1, new_e1, e2) with
        | (TypModule, ExpIdent(n1, _), ExpIdent(n2, (etyp2, eloc2))) ->
            let {dm_env; dm_real} = !(get_module n1) in
            (*let _ = printf "looking for %s in module %s ...\n" (id2str n2) (id2str n1) in*)
            let (new_n2, t) =
                if dm_real then
                    lookup_id n2 etyp dm_env sc eloc
                else
                    let n1n2 = (pp_id2str n1) ^ "." ^ (id2str n2) in
                    lookup_id (get_id n1n2) etyp env sc eloc
                in
            ExpIdent(new_n2, (t, eloc))
        | ((TypTuple tl), _, ExpLit((LitInt idx), (etyp2, eloc2))) ->
            unify etyp2 TypInt eloc2 "index must be int!";
            (* we do not handle negative indices, because the parser would not allow that;
               ok, if it's wrong assumption, an exception will be thrown
               (and be catched at the higher level) anyway *)
            let et = (try List.nth tl (Int64.to_int idx) with Failure _ ->
                raise_compile_err eloc2 "the tuple index is out of range") in
            unify etyp et eloc "incorrect type of the tuple element";
            ExpMem(new_e1, e2, ctx)
        | (TypApp(_, vn), _, ExpIdent(n2, (etyp2, eloc2))) when n2 = __tag_id__ ->
            (match (id_info vn) with
            | IdVariant _ ->
                unify etyp TypInt eloc "variant tag is integer, but the other type is expected";
                ExpMem(new_e1, e2, ctx)
            | _ ->
                raise_compile_err eloc "__tag__ can only be requestd for variants and exceptions")
        | (TypExn, _, ExpIdent(n2, (etyp2, eloc2))) when n2 = __tag_id__ ->
            unify etyp TypInt eloc "variant tag is integer, but the other type is expected";
            ExpMem(new_e1, e2, ctx)
        | (_, _, ExpIdent(n2, (etyp2, eloc2))) ->
            let (_, relems) = get_record_elems None etyp1 eloc1 in
            (try
                let (_, t1, _) = List.find (fun (n1, t1, _) -> n1 = n2) relems in
                unify etyp t1 eloc "incorrect type of the record element";
                ExpMem(new_e1, e2, ctx)
            with Not_found -> raise_compile_err eloc (sprintf "the record element %s is not found" (pp_id2str n2)))
        | _ -> raise_compile_err eloc "unsupported element access operation"
        )

    | ExpAssign(e1, e2, _) ->
        let (etyp1, eloc1) = get_exp_ctx e1 in
        let (etyp2, eloc2) = get_exp_ctx e2 in
        let _ = unify etyp1 etyp2 eloc "the left and the right sides of the assignment must have the same type" in
        let new_e1 = check_exp e1 env sc in
        let new_e2 = check_exp e2 env sc in
        (* check that new_e1 is lvalue and that new_e1 and new_e2 have equal types;
           in future we can let etyp1_ and etyp2_ be different as long as the assignment
           is safe and does not loose precision, e.g. int8 to int, float to double etc. *)
        let rec is_lvalue need_mutable_id e = (match e with
            | ExpAt (arr, BorderNone, InterpNone, _, _) -> is_lvalue false arr (* an_arr[idx] = e2 *)
            | ExpUnary(OpDeref, r, _) -> is_lvalue false r (* *a_ref = e2 *)
            | ExpIdent(n1, _) -> (* a_var = e2 *)
                (not need_mutable_id) ||
                (match (id_info n1) with
                | IdVal { dv_flags } -> dv_flags.val_flag_mutable
                | _ -> false)
            | ExpMem(rcrd, ExpIdent(_, _), _) -> is_lvalue need_mutable_id rcrd
            | ExpMem(tup, ExpLit(_, _), _) -> is_lvalue need_mutable_id tup
            | _ -> false) in
        if not (is_lvalue true new_e1) then raise_compile_err eloc "the left side of assignment is not an l-value"
        else ();
        ExpAssign(new_e1, new_e2, eloc)

    | ExpBinary(OpCons, e1, e2, _) ->
        let new_e1 = check_exp e1 env sc in
        let new_e2 = check_exp e2 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        let (etyp2, eloc2) = get_exp_ctx new_e2 in

        if (maybe_unify etyp (TypList (make_new_typ())) false) ||
           (maybe_unify etyp2 (TypList (make_new_typ())) false) then
            let _ = unify etyp (TypList etyp1) eloc1 "'::' operation should produce a list" in
            let _ = unify etyp2 (TypList etyp1) eloc2 "incorrect type of the second argument of '::' operation" in
            ExpBinary(OpCons, new_e1, new_e2, ctx)
        else
            let _ = unify etyp1 TypInt eloc1 "explicitly specified component of a range must be an integer" in
            let _ = unify etyp2 TypInt eloc2 "explicitly specified component of a range must be an integer" in
            let _ = unify etyp (TypTuple [TypInt;TypInt;TypInt]) eloc "the range type should have (int, int, int) type" in
            ExpRange((Some new_e1), None, (Some new_e2), ctx)
    | ExpBinary(bop, e1, e2, _) ->
        let new_e1 = check_exp e1 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        let new_e2 = check_exp e2 env sc in
        let (etyp2, eloc2) = get_exp_ctx new_e2 in
        let bop_wo_dot = binop_try_remove_dot bop in

        (* depending on the operation, figure out the type of result
           (or set it to None if there is no embedded implementation)
           and also do some other op-specific checks *)
        let (bop, typ_opt) =
        (match bop with
        | OpAdd | OpSub | OpMul | OpDiv | OpMod | OpPow | OpShiftLeft | OpShiftRight ->
            let is_shift = bop = OpShiftLeft || bop == OpShiftRight in
            let allow_fp = not is_shift in
            let typ_opt = coerce_types etyp1 etyp2 false allow_fp is_shift eloc in
            (bop, typ_opt)
        | OpDotMul | OpDotDiv | OpDotMod | OpDotPow ->
            let typ_opt = coerce_types etyp1 etyp2 false true false eloc in
            (match typ_opt with
            | Some _ -> (bop_wo_dot, typ_opt)
            | _ -> (bop, typ_opt))
        | OpBitwiseAnd | OpBitwiseOr | OpBitwiseXor ->
            let check_bitwise t1 t2 =
                (match ((deref_typ t1), (deref_typ t2)) with
                | (TypInt, TypInt) -> TypInt
                | (TypSInt(b1), TypSInt(b2)) when b1 = b2 -> TypSInt(b1)
                | (TypUInt(b1), TypUInt(b2)) when b1 = b2 -> TypUInt(b1)
                | (TypBool, TypBool) -> TypBool
                | _ -> invalid_arg "")
            in
            let typ_opt = (try Some(check_bitwise etyp1 etyp2) with Invalid_argument _ -> None) in
            (bop, typ_opt)
        | OpLogicAnd | OpLogicOr ->
            unify etyp1 TypBool eloc1 "arguments of logical operation must be boolean";
            unify etyp2 TypBool eloc2 "arguments of logical operation must be boolean";
            (bop, Some(TypBool))
        (* [TODO] comparison operations are now implemented for anything but variants *)
        | OpCmp _ | OpDotCmp _ ->
            if bop <> bop_wo_dot then () else
                (unify etyp1 etyp2 eloc "the compared elements must have the same type";
                unify etyp TypBool eloc (sprintf "result of comparison operation '%s' must be bool"
                    (binop_to_string bop)));
            if (is_typ_scalar etyp1) && (is_typ_scalar etyp2) then
                (unify etyp1 etyp2 eloc "only equal types can be compared";
                (bop_wo_dot, Some(TypBool)))
            else if etyp1 = TypString then
                (bop_wo_dot, None)
            else
                (bop, None)
        | OpSpaceship | OpDotSpaceship ->
            if ((is_typ_scalar etyp1) && (is_typ_scalar etyp2)) ||
                ((deref_typ etyp1) = TypString && (deref_typ etyp2) = TypString) then
                (bop_wo_dot, None)
            else
                (bop, None)
        | OpCons -> raise_compile_err eloc (sprintf "unsupported binary operation %s" (binop_to_string bop))) in

        (match (typ_opt, bop, (deref_typ etyp1), (deref_typ etyp2), e1, e2) with
        | ((Some typ), _, _, _, _, _) ->
            unify typ etyp eloc "improper type of the arithmetic operation result";
            ExpBinary(bop, new_e1, new_e2, ctx)
        | (_, OpAdd, TypString, TypString, _, _)
        | (_, OpAdd, TypString, TypChar, _, _)
        | (_, OpAdd, TypChar, TypString, _, _)
        | (_, OpAdd, TypChar, TypChar, _, _) ->
            unify TypString etyp eloc "improper type of the string concatenation operation (string is expected)";
            ExpBinary(bop, new_e1, new_e2, ctx)
        | (_, OpAdd, TypList _, TypList _, ExpBinary(OpAdd, sub_e1, sub_e2, _), _) ->
            (* make list concatenation right-associative instead of left-associative *)
            let sub_e2_loc = get_exp_loc sub_e2 in
            let e2_loc = loclist2loc [sub_e2_loc; eloc2] eloc2 in
            (* [TODO: not that relevant anymore]
            fix bug with repetitive check of the same expression (since the check has some side effects) *)
            let e2_ = ExpBinary(OpAdd, sub_e2, e2, (make_new_typ(), e2_loc)) in
            check_exp (ExpBinary (OpAdd, sub_e1, e2_, (etyp, eloc))) env sc
        | _ ->
            (* try to find an overloaded function that will handle such operation with combination of types, e.g.
               operator + (p: point, q: point) = point { p.x + q.x, p.y + q.y } *)
            let f_id = get_binop_fname bop eloc in
            check_and_make_call f_id [new_e1; new_e2])

    | ExpThrow(e1, _) ->
        let (etyp1, eloc1) = get_exp_ctx e1 in
        let _ = unify TypExn etyp1 eloc "the argument of 'throw' operator must be an exception" in
        let new_e1 = check_exp e1 env sc in
        ExpThrow(new_e1, eloc)

    | ExpUnary(OpMkRef, e1, _) ->
        let (etyp1, eloc1) = get_exp_ctx e1 in
        let _ = unify etyp (TypRef etyp1) eloc "the types of ref() operation argument and result are inconsistent" in
        let new_e1 = check_exp e1 env sc in
        ExpUnary(OpMkRef, new_e1, ctx)

    | ExpUnary(OpDeref, e1, _) ->
        let (etyp1, eloc1) = get_exp_ctx e1 in
        let _ = unify (TypRef etyp) etyp1 eloc "the types of unary '*' operation argument and result are inconsistent" in
        let new_e1 = check_exp e1 env sc in
        ExpUnary(OpDeref, new_e1, ctx)

    | ExpUnary(uop, e1, _) ->
        let new_e1 = check_exp e1 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        (match uop with
        | OpNegate | OpPlus | OpDotMinus ->
            let allow_fp = uop = OpNegate || uop = OpPlus in
            let t_opt = coerce_types etyp1 etyp1 false allow_fp false eloc in
            (match t_opt with
            | Some(t) ->
                unify etyp t eloc "improper type of the unary '-' operator result";
                ExpUnary(uop, new_e1, ctx)
            | None ->
                let f_id = get_unop_fname uop eloc in
                check_and_make_call f_id [new_e1])
        | OpBitwiseNot ->
            (let rec check_bitwise t =
                match (deref_typ t) with
                | TypInt -> TypInt
                | TypSInt b -> TypSInt b
                | TypUInt b -> TypUInt b
                | TypBool -> TypBool
                | TypTuple tl ->
                    TypTuple(List.map check_bitwise tl)
                | _ -> invalid_arg "" in
            try
                unify etyp (check_bitwise etyp1) eloc "invalid type of bitwise-not result";
                ExpUnary(uop, new_e1, ctx)
            with Invalid_argument _ ->
                let f_id = get_unop_fname uop eloc in
                check_and_make_call f_id [new_e1])
        | OpLogicNot ->
            unify etyp1 TypBool eloc1 "the argument of ! operator must be a boolean";
            unify etyp TypBool eloc "the result of ! operator must be a boolean";
            ExpUnary(uop, new_e1, ctx)
        | OpApos ->
            let f_id = get_unop_fname uop eloc in
            check_and_make_call f_id [new_e1]
        | _ ->
            raise_compile_err eloc (sprintf "unsupported unary operation '%s'" (unop_to_string uop)))
    | ExpSeq(eseq, _) ->
        let eseq_typ = get_eseq_typ eseq in
        let _ = unify etyp eseq_typ eloc "the expected type of block expression does not match its actual type" in
        let (eseq, _) = check_eseq eseq env sc true in
        ExpSeq(eseq, ctx)
    | ExpMkTuple(el, _) ->
        let tl = List.map (fun e -> get_exp_typ e) el in
        let _ = unify etyp (TypTuple tl) eloc "improper type of tuple elements or the number of elements" in
        ExpMkTuple((List.map (fun e -> check_exp e env sc) el), ctx)
    | ExpCall(f0, args0, _) ->
        let f = dup_exp f0 in
        let args = List.map dup_exp args0 in
        let arg_typs = List.map (fun a -> get_exp_typ a) args in
        let f_expected_typ = TypFun(arg_typs, etyp) in
        let (f_real_typ, floc) = get_exp_ctx f in
        let _ = unify f_real_typ f_expected_typ floc "the real and expected function type do not match" in
        (* we type-check some of the arguments immediately
           and postpone checking some other arguments till after the proper function is found:
           1. if argument is lambda function, which type is not completely specified, we postpone checking it.
           2. if the argument is identifier (possibly specified using 'dot' notation, e.g. Math.sin) and
              if it's a function and if there are template functions or other overloaded functions defined
              within the same scope, we also postpone checking it
           *)
        let new_args = List.fold_left (fun new_args a ->
            let need_to_check =
            match a with
            | ExpSeq([(DefFun {contents={df_name}}) as exp_df; ExpIdent(f, _)], _) when
                (match f with Id.Temp _ -> true | _ -> false) &&
                Utils.starts_with (pp_id2str f) "lambda" &&
                f = df_name ->
                let df = match (dup_exp exp_df) with DefFun df -> df
                    | _ -> raise_compile_err (get_exp_loc a) "internal error: deffun expected"
                    in
                let _ = reg_deffun df env sc in
                (match !df with
                | {df_templ_args=_::_} -> false
                | _ -> true)
            | ExpIdent _ | ExpMem(_, (ExpIdent _), _) ->
                (match (check_exp (dup_exp a) env sc) with
                | ExpIdent(f, (t, loc)) when
                    (match (deref_typ t) with TypFun _ -> true | _ -> false) ->
                    (match (id_info f) with
                    | IdFun {contents={df_env}} ->
                        let all_entries = find_all (get_orig_id f) df_env in
                        let possible_matches = List.fold_left (fun possible_matches entry ->
                            possible_matches + (match entry with
                            | EnvId i ->
                                (match (id_info i) with
                                | IdFun {contents={df_templ_args}} ->
                                    if df_templ_args != [] then 100 else 1
                                | _ -> 0)
                            | _ -> 0)) 0 all_entries
                            in
                        possible_matches <= 1
                    | _ -> true)
                | _ -> true)
            | _ -> true
            in if need_to_check then
                ((check_exp a env sc), true) :: new_args
            else (a, false) :: new_args) [] args in
        let new_args = List.rev new_args in
        (try
            let new_f = check_exp f env sc in
            let new_args = match deref_typ (get_exp_typ new_f) with
                | TypFun(argtyps, rt) ->
                    if (List.length argtyps) = (List.length new_args) then new_args
                    else
                        let last_typ = Utils.last_elem argtyps in
                        let mkrec = ExpMkRecord((ExpNop eloc), [], (last_typ, eloc)) in
                        new_args @ [(mkrec, true)]
                | _ -> new_args
                in
            let new_args = List.map (fun (e, checked) ->
                match (e, checked) with
                | (_, true) -> e
                | _ -> check_exp e env sc) new_args in
            ExpCall(new_f, new_args, ctx)
        with (CompileError _) as ex ->
            (* fallback for so called "object types": if we have expression like
                some_exp.foo(args) where some_exp's type is "object type"
                (i.e. "list", "string" or some user type declared in a module <Module>
                as "object type = ...") then the call is transformed to
                <Module>.foo(some_exp, args)
            *)
            (match f0 with
            | ExpMem(r0, (ExpIdent (mem_f, _) as mem_f_exp), mem_ctx) ->
                let r = check_exp (dup_exp r0) env sc in
                let r_t = get_exp_typ r in
                let mstr = match (deref_typ r_t) with
                    | TypList _ -> "List"
                    | TypString -> "String"
                    | TypChar -> "Char"
                    | TypApp(_, tn) ->
                        (match (id_info tn) with
                        | IdVariant {contents={dvar_flags={var_flag_object=m}}} when m <> noid -> pp_id2str m
                        | _ -> "")
                    | _ -> ""
                    in
                let new_f = if mstr = "Builtins" then mem_f_exp else
                    let m_id = if mstr <> "" then (get_id mstr) else raise ex in
                    let (ftyp, floc) = mem_ctx in
                    ExpMem(ExpIdent(m_id, (make_new_typ(), floc)), mem_f_exp, mem_ctx)
                    in
                let new_exp = ExpCall(new_f, r0 :: args0, ctx) in
                check_exp new_exp env sc
            | _ -> raise ex))
    | ExpAt(arr, border, interp, idxs, _) ->
        let new_arr = check_exp arr env sc in
        let (new_atyp, new_aloc) = get_exp_ctx new_arr in
        (match idxs with
        (* flatten case "arr[:]" *)
        | ExpRange(None, None, None, _) :: [] ->
            let new_idx = check_exp (List.hd idxs) env sc in
            let _ = if border = BorderNone then () else
                raise_compile_err eloc "border extrapolation with ranges is not supported yet" in
            let _ = if interp = InterpNone then () else
                raise_compile_err eloc "inter-element interpolation with ranges is not supported yet" in
            (match (deref_typ new_atyp) with
            | TypArray(d, et) ->
                unify etyp (TypArray(1, et)) eloc
                "the result of flatten operation ([:]) applied to N-D array must be 1D array with elements of the same type as input array";
                ExpAt(new_arr, BorderNone, InterpNone, new_idx :: [], ctx)
            | TypString ->
                unify etyp TypString eloc
                "the result of flatten operation ([:]) applied to string must be string";
                new_arr
            | _ -> raise_compile_err eloc "the argument of the flatten operation must be an array")
        (* other cases: check each index, it should be either a scalar or a range;
           in the first case it should have integer type.
           If all the indices are scalars, then the result should have et type,
           otherwise it's an array of as high dimensionality as the number of range indices *)
        | _ ->
            let (new_idxs, ndims, nfirst_scalars, nranges) =
                List.fold_left (fun (new_idxs, ndims, nfirst_scalars, nranges) idx ->
                let new_idx = check_exp idx env sc in
                match new_idx with
                | ExpRange(_, _, _, _) ->
                    let _ = if border = BorderNone then () else
                        raise_compile_err eloc "border extrapolation with ranges is not supported yet" in
                    let _ = if interp = InterpNone then () else
                        raise_compile_err eloc "inter-element interpolation with ranges is not supported yet" in
                    (new_idx :: new_idxs, ndims+1, nfirst_scalars, nranges+1)
                | _ ->
                    let (new_ityp, new_iloc) = get_exp_ctx new_idx in
                    let new_idx = if (maybe_unify new_ityp TypInt true) then new_idx else
                        let possible_idx_typs=[TypBool; TypUInt(8); TypSInt(8); TypUInt(16);
                            TypSInt(16); TypUInt(32); TypSInt(32); TypUInt(64); TypSInt(64)] in
                        if List.exists(fun t -> maybe_unify new_ityp t true) possible_idx_typs then
                            ExpCast(new_idx, TypInt, (TypInt, new_iloc))
                        else if interp = InterpLinear &&
                            ((maybe_unify new_ityp (TypFloat 32) true) ||
                             (maybe_unify new_ityp (TypFloat 64) true)) then new_idx else
                            raise_compile_err new_iloc
                            ("each scalar index in array access op must have some integer type or bool; " ^
                            "in the case of interpolation it can also be float or double") in
                    let nfirst_scalars = if nranges = 0 then nfirst_scalars + 1 else nfirst_scalars in
                    (new_idx :: new_idxs, ndims+1, nfirst_scalars, nranges)) ([], 0, 0, 0) idxs in
            (match (ndims, nranges, (deref_typ new_atyp)) with
            | (1, 0, TypString) ->
                unify etyp TypChar new_aloc "indexing string should give a char"
            | (1, 1, TypString) ->
                unify etyp TypString new_aloc "indexing string with a range should give a string"
            | _ ->
                let et = make_new_typ() in
                unify new_atyp (TypArray(ndims, et)) new_aloc "the array dimensionality does not match the number of indices";
                (if nranges = 0 then
                    unify etyp et eloc "the type of array access expression does not match the array element type"
                else
                    unify etyp (TypArray(ndims - nfirst_scalars, et)) eloc
                    "the number of ranges does not match dimensionality of the result, or the element type is incorrect"));
            if interp <> InterpLinear then () else
            if (is_typ_numeric etyp true) then () else
                raise_compile_err eloc "in the case of interpolation the array type should be numeric";
            ExpAt(new_arr, border, interp, (List.rev new_idxs), ctx))
    | ExpIf(c, e1, e2, _) ->
        let (ctyp, cloc) = get_exp_ctx c in
        let (typ1, loc1) = get_exp_ctx e1 in
        let (typ2, loc2) = get_exp_ctx e2 in
        let _ = unify ctyp TypBool cloc "if() condition should have 'bool' type" in
        let _ = unify typ1 etyp loc1 "if() expression should have the same type as its branches" in
        let _ = unify typ2 etyp loc2 "if() expression should have the same type as its branches" in
        let new_c = check_exp c env sc in
        let new_e1 = check_exp e1 env sc in
        let new_e2 = check_exp e2 env sc in
        ExpIf(new_c, new_e1, new_e2, ctx)
    | ExpWhile(c, body, _) ->
        let (ctyp, cloc) = get_exp_ctx c in
        let (btyp, bloc) = get_exp_ctx body in
        let _ = unify ctyp TypBool cloc "while() loop condition should have 'bool' type" in
        let _ = unify btyp TypVoid bloc "while() loop body should have 'void' type" in
        let new_c = check_exp c env sc in
        let loop_sc = new_loop_scope(false) :: sc in
        let new_body = check_exp body env loop_sc in
        ExpWhile (new_c, new_body, eloc)
    | ExpDoWhile(body, c, _) ->
        let (ctyp, cloc) = get_exp_ctx c in
        let (btyp, bloc) = get_exp_ctx body in
        let _ = unify ctyp TypBool cloc "do-while() loop condition should have 'bool' type" in
        let _ = unify btyp TypVoid bloc "do-while() loop body should have 'void' type" in
        let new_c = check_exp c env sc in
        let loop_sc = new_loop_scope(false) :: sc in
        let new_body = check_exp body env loop_sc in
        ExpDoWhile (new_body, new_c, eloc)
    | ExpFor(for_clauses, idx_pat, body, flags, _) ->
        let is_fold = flags.for_flag_fold in
        let is_nested = flags.for_flag_nested in
        let _ = if not flags.for_flag_unzip then () else
            raise_compile_err eloc "@unzip for does not make sense outside of comprehensions" in
        let for_sc = (if is_fold then new_fold_scope() else new_loop_scope(is_nested)) :: sc in
        let (trsz, pre_code, for_clauses, idx_pat, dims, env, _) = check_for_clauses for_clauses idx_pat env IdSet.empty for_sc in
        if trsz > 0 then
            (* iteration over tuple(s) is replaced with unrolled code.
               * this is possible, since we know the tuple size at compile time
               * this is necessary, because tuple elements may have different type,
                 so for each "iteration" potentially a completely different code could be generated *)
            let code = List.init trsz (fun idx ->
                let it_j = gen_for_in_tuprec_it idx for_clauses idx_pat body env for_sc in
                let (tj, locj) = get_exp_ctx it_j in
                let _ = unify tj TypVoid locj "'for()' body should have 'void' type" in
                it_j) in
            ExpSeq((pre_code @ code), (TypVoid, eloc))
        else
            let (btyp, bloc) = get_exp_ctx body in
            let _ = unify btyp TypVoid bloc "'for()' body should have 'void' type" in
            let new_body = check_exp body env for_sc in
            ExpFor(for_clauses, idx_pat, new_body, flags, eloc)
    | ExpMap (map_clauses, body, flags, ctx) ->
        let make_list = flags.for_flag_make = ForMakeList in
        let make_tuple = flags.for_flag_make = ForMakeTuple in
        let unzip_mode = flags.for_flag_unzip in
        let for_sc = (if make_tuple then new_block_scope() else if make_list then new_map_scope() else new_arr_map_scope()) :: sc in
        let (trsz, pre_code, map_clauses, total_dims, env, _) = List.fold_left
            (fun (trsz, pre_code, map_clauses, total_dims, env, idset) (for_clauses, idx_pat) ->
                let (trsz_k, pre_code_k, for_clauses, idx_pat, dims, env, idset) = check_for_clauses for_clauses idx_pat env idset for_sc in
                ((trsz + trsz_k), (pre_code_k @ pre_code), (for_clauses, idx_pat) :: map_clauses,
                total_dims + dims, env, idset)) (0, [], [], 0, env, IdSet.empty) map_clauses in
        let _ = if trsz > 0 || not make_tuple then () else
            raise_compile_err eloc "tuple comprehension with iteration over non-tuples and non-records is not supported"
            in
        let coll_name = if make_list then "list" else if make_tuple then "tuple" else "array" in
        let check_map_typ elem_typ coll_typ idx : unit =
            let idx_str = if idx < 0 then "of comprehension" else (sprintf "#%d (0-based) of @unzip comprehension" idx) in
            if make_list then
                (if total_dims = 1 then () else raise_compile_err eloc "the list comprehension should use 1-dimensional loop";
                unify coll_typ (TypList elem_typ) eloc
                (sprintf "the result %s should have type '%s', but it has type '%s'"
                idx_str (typ2str (TypList elem_typ)) (typ2str coll_typ)))
            else
                unify coll_typ (TypArray(total_dims, elem_typ)) eloc
                (sprintf "the result %s should have type '%s', but it has type '%s'"
                idx_str (typ2str (TypArray (total_dims, elem_typ))) (typ2str coll_typ))
            in
        if trsz > 0 then
            let _ = if not flags.for_flag_unzip then () else
                raise_compile_err eloc "@unzip flag is not supported in tuple/record comprehensions" in
            let (for_clauses, idx_pat) = match map_clauses with
                | (for_clauses, idx_pat) :: [] -> (for_clauses, idx_pat)
                | _ -> raise_compile_err eloc "tuple comprehension with nested for is not supported yet"
            in
            let elem_typ = make_new_typ() in
            let elems = List.init trsz (fun idx ->
                let it_j = gen_for_in_tuprec_it idx for_clauses idx_pat body env for_sc in
                let (tj, locj) = get_exp_ctx it_j in
                let _ = if make_tuple then () else
                    unify tj elem_typ locj (sprintf "%s comprehension should produce elements of the same type" coll_name) in
                it_j) in
            let mk_struct_exp = if make_tuple then
                    let tl = List.map get_exp_typ elems in
                    ExpMkTuple(elems, (TypTuple(tl), eloc))
                else if make_list then
                    let ltyp = TypList(elem_typ) in
                    List.fold_left (fun l_exp ej ->
                        ExpBinary(OpCons, ej, l_exp, (ltyp, eloc))) (ExpLit(LitNil, (ltyp, eloc))) (List.rev elems)
                else
                    ExpMkArray(elems :: [], (TypArray (1, elem_typ), eloc))
                in
            let coll_typ = get_exp_typ mk_struct_exp in
            let _ = unify etyp coll_typ eloc (sprintf "inconsistent type of the constructed %s" coll_name) in
            ExpSeq((pre_code @ [mk_struct_exp]), (coll_typ, eloc))
        else
            let (btyp, bloc) = get_exp_ctx body in
            let _ = if unzip_mode then ()
                else check_map_typ btyp etyp (-1) in
            let new_body = check_exp body env for_sc in
            let new_unzip_mode =
                if unzip_mode then
                    (match (deref_typ btyp, deref_typ etyp) with
                    | (TypTuple(b_elems), TypVar {contents=None}) ->
                        let nb_elems = List.length b_elems in
                        let colls = List.init nb_elems (fun _ -> make_new_typ()) in
                        let _ = List.fold_left2 (fun idx bt ct -> check_map_typ bt ct idx; idx+1) 0 b_elems colls in
                        unify etyp (TypTuple(colls)) eloc "incorrect type of @unzip'ped comprehension";
                        true
                    | (TypTuple(b_elems), TypTuple(colls)) ->
                        let nb_elems = List.length b_elems in
                        let ncolls = List.length colls in
                        if nb_elems = ncolls then ()
                        else
                            raise_compile_err eloc (sprintf
                            "the number of elements in a tuple produced by the @unzip'pped comprehension (=%d) and in the output tuple (=%d) do not match"
                            nb_elems ncolls);
                        ignore(List.fold_left2 (fun idx bt ct ->
                            check_map_typ bt ct idx; idx+1) 0 b_elems colls);
                        true
                    | (TypTuple _, _) | (_, TypTuple _) ->
                        raise_compile_err eloc (sprintf
                        "in the case of @unzip comprehension either both the body type ('%s') and the result type ('%s') should be tuples or none of them"
                        (typ2str btyp) (typ2str etyp))
                    | _ -> check_map_typ btyp etyp (-1); false)
                else if deref_typ (get_exp_typ new_body) = TypVoid then
                    raise_compile_err eloc "array/list comprehension body cannot have 'void' type"
                else false in
            ExpMap((List.rev map_clauses), new_body, {flags with for_flag_unzip=new_unzip_mode}, ctx)
    | ExpBreak (f, _) -> check_inside_for f true; e
    | ExpContinue _ -> check_inside_for false false; e
    | ExpMkArray (arows, _) ->
        let elemtyp = make_new_typ() in
        let (_, _, arows, have_expanded, dims) = List.fold_left (fun (k, ncols, arows, have_expanded, dims) arow ->
            let (have_expanded_i, row_dims, arow) = List.fold_left (fun (have_expanded_i, row_dims, arow) elem ->
                let (is_expanded, elem_dims, elem1, elem_loc) = match elem with
                    | ExpUnary(OpExpand, e1, (t, loc)) ->
                        let e1 = check_exp e1 env sc in
                        let (arrtyp1, eloc1) = get_exp_ctx e1 in
                        let _ = unify t arrtyp1 loc "incorrect type of expanded collection" in
                        let (colname, d, elemtyp1) = match (deref_typ arrtyp1) with
                            | TypArray(d, elemtyp1) -> ("array", d, elemtyp1)
                            | TypList(elemtyp1) -> ("list", 1, elemtyp1)
                            | TypString -> ("string", 1, TypChar)
                            | _ -> raise_compile_err loc "incorrect type of expanded collection (it should be an array, list or string)"
                            in
                        let _ = if d <= 2 then () else raise_compile_err loc
                            "currently expansion of more than 2-dimensional arrays is not supported" in
                        let _ = unify elemtyp elemtyp1 eloc1 (sprintf "the expanded %s elem type does not match the previous elements" colname) in
                        (true, d, ExpUnary(OpExpand, e1, (arrtyp1, loc)), loc)
                    | _ ->
                        let (elemtyp1, eloc1) = get_exp_ctx elem in
                        let _ = unify elemtyp elemtyp1 eloc1 "all the array literal elements should have the same type" in
                        (false, 1, (check_exp elem env sc), eloc1)
                    in
                let row_dims = if row_dims >= 0 then row_dims else elem_dims in
                let _ = if row_dims = elem_dims then () else raise_compile_err elem_loc
                    (sprintf "dimensionality of array element (=%d) does not match the previous elements dimensionality (=%d) in the same row"
                    elem_dims row_dims)
                in
                ((have_expanded_i || is_expanded), row_dims, (elem1 :: arow))) (false, -1, []) arow in
            let ncols_i = List.length arow in
            let elem_loc = match arow with
                | e :: _ -> get_exp_loc e
                | _ ->
                    (match arows with
                    | r :: _ -> get_exp_loc (Utils.last_elem r)
                    | _ -> eloc)
            in
            let _ = if ncols_i <> 0 then () else raise_compile_err elem_loc
                (sprintf "the %d-%s matrix row is empty" (k+1) (Utils.num_suffix (k+1))) in
            let have_expanded = have_expanded || have_expanded_i in
            let _ = if have_expanded || ncols = 0 || ncols = ncols_i then () else
                raise_compile_err elem_loc
                    (sprintf "the %d-%s matrix row contains a different number of elements"
                    (k+1) (Utils.num_suffix (k+1))) in
            let dims = if dims < 0 then row_dims else 2 in
            (k+1, (if have_expanded_i then ncols_i else ncols),
            ((List.rev arow) :: arows), have_expanded, dims)) (0, 0, [], false, -1) arows in
        let atyp = TypArray(dims, elemtyp) in
        let _ = unify atyp etyp eloc "the array literal should produce an array" in
        ExpMkArray((List.rev arows), ctx)
    | ExpMkRecord (r_e, r_initializers, _) ->
        let _ = check_for_rec_field_duplicates (List.map (fun (n, _) -> n) r_initializers) eloc in
        let (r_new_initializers, relems) = List.fold_left (fun (r_new_initializers, relems) (n, e) ->
            let e = check_exp e env sc in
            let etyp = get_exp_typ e in
            ((n, e) :: r_new_initializers, (n, etyp, None) :: relems)) ([], []) r_initializers in
        let rtyp = TypRecord(ref ((List.rev relems), false)) in
        (match r_e with
        | ExpNop (nop_loc) ->
            unify etyp rtyp eloc "unexpected record type";
            ExpMkRecord(r_e, (List.rev r_new_initializers), ctx)
        | _ ->
            let (r_etyp, r_eloc) = get_exp_ctx r_e in
            let r_expected_typ = TypFun(rtyp :: [], etyp) in
            let _ = unify r_etyp r_expected_typ r_eloc "there is no proper record constructor/function with record argument" in
            let new_r_e = check_exp r_e env sc in
            ExpMkRecord(new_r_e, (List.rev r_new_initializers), ctx))
    | ExpUpdateRecord (r_e, r_initializers, _) ->
        let _ = check_for_rec_field_duplicates (List.map (fun (n, _) -> n) r_initializers) eloc in
        let (rtyp, rloc) = get_exp_ctx r_e in
        let _ = unify rtyp etyp eloc "the types of the update-record argument and the result do not match" in
        let new_r_e = check_exp r_e env sc in
        let (_, relems) = get_record_elems None rtyp rloc in
        let new_r_initializers = List.map (fun (ni, ei) ->
            let (ei_typ, ei_loc) = get_exp_ctx ei in
            try
                let (_, ti, _) = List.find (fun (nj, _, _) -> ni = nj) relems in
                let _ = unify ti ei_typ ei_loc (sprintf
                    "invalid type of the initializer of record field '%s'" (id2str ni)) in
                let new_ei = check_exp ei env sc in
                (ni, new_ei)
            with Not_found -> raise_compile_err ei_loc (sprintf
                "there is no record field '%s' in the updated record" (id2str ni))) r_initializers in
        ExpUpdateRecord(new_r_e, new_r_initializers, ctx)
    | ExpTryCatch(e1, cases, _) ->
        let sc = new_try_scope() :: sc in
        let (e1typ, e1loc) = get_exp_ctx e1 in
        let _ = unify etyp e1typ e1loc "try body type does match the whole try-catch type" in
        let new_e1 = check_exp e1 env sc in
        let new_cases = check_cases cases TypExn etyp env sc eloc in
        ExpTryCatch(new_e1, new_cases, ctx)
    | ExpMatch(e1, cases, _) ->
        let new_e1 = check_exp e1 env sc in
        let new_e1typ = get_exp_typ new_e1 in
        let new_cases = check_cases cases new_e1typ etyp env sc eloc in
        ExpMatch(new_e1, new_cases, ctx)
    | ExpCast(e1, t1, _) ->
        (* [TODO] check that e1 can be cast to t1 *)
        let new_t1 = check_typ t1 env sc eloc in
        let _ = unify etyp new_t1 eloc "unexpected type of cast operation" in
        let new_e1 = check_exp e1 env sc in
        ExpCast(new_e1, new_t1, ctx)
    | ExpTyped(e1, t1, _) ->
        let new_t1 = check_typ t1 env sc eloc in
        let (e1typ, e1loc) = get_exp_ctx e1 in
        let _ = unify etyp new_t1 eloc "improper explicit type of the expression" in
        let _ = unify e1typ new_t1 e1loc "improper explicit type of the expression" in
        let new_e1 = check_exp e1 env sc in
        ExpTyped(new_e1, new_t1, ctx)
    | ExpCCode(str, _) ->
        (match sc with
        | ScModule(_) :: _ -> e (*unify etyp TypVoid eloc "module-level ccode should have 'void' type"*)
        | ScFun(_) :: _ ->
            (* do some favor for those who start to forget to put the final ';' before } *)
            let str = String.trim str in
            let str = if (Utils.ends_with str "}") || (Utils.ends_with str ";") then str else str ^ ";" in
            ExpCCode(str, ctx)
        | _ -> raise_compile_err eloc
            "ccode may be used only at the top (module level) or as a single expression in function definition")
    (* all the declarations are checked in check_eseq *)
    | DefVal(_, _, _, _)
    | DefFun(_)
    | DefVariant(_)
    | DefClass(_)
    | DefInterface(_)
    | DefExn(_)
    | DefTyp(_)
    | DirImport(_, _)
    | DirImportFrom(_, _, _)
    | DirPragma(_, _) ->
        raise_compile_err eloc
        ("internal err: should not get here; all the declarations and " ^
        "directives must be handled in check_eseq")) in
    (*let _ = match new_e with ExpSeq _ -> () | _ -> (printf "\ninferenced type: \n";
        (pprint_typ_x etyp noloc); printf "\n========\n") in*)
    new_e

and check_eseq eseq env sc create_sc =
    (*
      * scan eseq, look for type declarations. Give all the types fresh names,
        update the env, add the type declaration "headers" to the global symbol table.
        check that there are no other types with the same name in the same scope.
      * scan eseq again, now process all the type declaration bodies, resolve the types.
      * scan eseq, look for function definitions and exception definitions. Give all the
        exceptions and all the functions' fresh names, analyze their types. Since there can be
        mutually recursive functions with unspecified return types, we are not always able to
        figure out the return type at this point. So, some return types may be set to TypVar(ref None).
        For each exception we introduce a function (or a value for an exception without parameters)
        that constucts the exception. For each function definition we check that we do not have duplicates
        in the argument names. We also make sure that each argument has a type and it's not ambiguous.
        [probably, we should check whether there are other overloaded functions similar to the current one;
        if there are other functions in the same scope with the same types of input arguments, we should
        issue an error]
      * [TODO] when we add classes, if eseq is the class body, we'd need to scan it again
        to get all the members declarations and add them to env. This is needed because methods
        can access members that are declared after methods.
      * scan eseq one more time, process value definitions an functions bodies.
        ** For each value we introduce a fresh name, add it to env and to the global sym table.
        [if a value with the same name is defined in the same scope, we should issue an optional
        "value redefinition" warning (if it's a class scope, probably we should issue an error)]
        Before that, we need to analyze the pattern to handle cases like "val (x, y, _) = some_3d_point".
        Underscores in such cases are replaced with temporary id's. They will be excluded later on at the
        unused code elimination stage.
        ** For each function we add all the arguments to the env, form a new scope and check the body
        within this scope and using this special "inside-function" env.
      * return the final env. It's especially useful when processing modules (top-level definitions) and classes,
        because this env is then stored inside the module/class structure.
    *)
    (* create the nested block scope if requested *)
    let sc = if create_sc then new_block_scope() :: sc else sc in
    (* process directives (now we have only import directives) *)
    let env = check_directives eseq env sc in
    (* process type declarations: use 2-pass procedure
       in order to handle mutually-recursive types properly *)
    let env = reg_types eseq env sc in
    let env = check_types eseq env sc in

    (* register exceptions and function declarations *)
    let env = List.fold_left (fun env e ->
        match e with
        | DefFun (df) -> reg_deffun df env sc
        | DefExn (de) -> check_defexn de env sc
        | _ -> env) env eseq in

    (* finally, process everything else:
       function bodies, values declarations as well as normal expressions/statements *)
    let nexps = List.length eseq in
    let (eseq, env, _) = List.fold_left (fun (eseq, env, idx) e ->
        let (eseq1, env1) =
        try
            match e with
            | DirImport(_, _) | DirImportFrom(_, _, _) | DirPragma(_, _)
            | DefTyp _ | DefVariant _ | DefClass _ | DefInterface _ | DefExn _ -> (e :: eseq, env)
            | DefVal (p, e, flags, loc) ->
                let is_mutable = flags.val_flag_mutable in
                let t = get_exp_typ e in
                let p = match p with
                    | PatTyped(p1, t1, loc) ->
                        let t1 = check_typ t1 env sc loc in
                        unify t t1 loc "explicit type specification of the defined value does not match the assigned expression type";
                        p1
                    | _ -> p
                    in
                let e1 =
                    (try
                        check_exp e env sc
                    with
                    | CompileError(_, _) as err ->
                        push_compile_err err; e) in
                let (p1, env1, _, _, _) = check_pat p t env IdSet.empty IdSet.empty sc false true is_mutable in
                (DefVal(p1, e1, flags, loc) :: eseq, env1)
            | DefFun (df) ->
                let df = if !df.df_templ_args = [] then
                    check_deffun df env
                else
                    (* update environment of the template function
                        to give it access to the above defined
                        non-template values *)
                    (df := {!df with df_env = env}; df)
                in
                ((DefFun df) :: eseq, env)
            | _ ->
                let e = check_exp e env sc in
                let (etyp, eloc) = get_exp_ctx e in
                (match e with
                | ExpNop _ ->
                    if nexps = 1 then () else
                        raise_compile_err eloc "there cannot be {} operators inside code blocks"
                | ExpBreak _ | ExpContinue _ ->
                    if idx = nexps - 1 then () else
                        raise_compile_err eloc
                        "break/continue operator should not be followed by any other operators in the same linear code sequence"
                | ExpCCode _ -> ()
                | _ ->
                    (match (deref_typ etyp) with
                    | TypVoid | TypDecl -> ()
                    | TypVar {contents=None} -> ()
                    | _ -> if idx = nexps - 1 then () else
                        ((*(printf "exp type: "; pprint_typ_x (deref_typ etyp) eloc; printf "\n");*)
                        raise_compile_err eloc
                        "non-void expression occurs before the end of code block. Check the line breaks; if it's valid, use ignore() function to dismiss the error")));
                (e :: eseq, env)
        with
        | CompileError(_, _) as err ->
            push_compile_err err; (e :: eseq, env)
        | PropagateCompileError -> (e :: eseq, env)
        in
        (eseq1, env1, idx+1)) ([], env, 0) eseq in

    check_compile_errs();
    (List.rev eseq, env)

and check_directives eseq env sc =
    let is_imported alias n env allow_duplicate_import loc =
        (List.exists(fun entry ->
            match entry with
            | EnvId m ->
                (try
                    let minfo = get_module m in
                    let mname = !minfo.dm_name in
                    if mname = n then
                        let astr = pp_id2str alias in
                        let mstr = pp_id2str mname in
                        if allow_duplicate_import then true
                        else if astr = mstr then
                            raise_compile_err loc (sprintf "duplicate import of %s" mstr)
                        else
                            raise_compile_err loc (sprintf "duplicate import of %s as %s" mstr astr)
                    else
                        raise_compile_err loc
                            (sprintf "another module %s has been already imported as %s"
                            (pp_id2str mname) (pp_id2str alias))
                with Failure _ -> false)
            | EnvTyp _ -> false) (find_all alias env)) in


    let import_entries env parent_mod key entries loc =
        (List.fold_left (fun env i ->
            match i with
            | EnvId(i) ->
                (let info = id_info i in
                let sc = get_scope info in
                match (info, sc) with
                | (_, ScModule(m) :: _) when parent_mod = noid || parent_mod = m ->
                    add_id_to_env key i env
                | (IdModule _, _) ->
                    add_id_to_env key i env
                | _ -> env)
            | EnvTyp _ -> env) env (List.rev entries)) in

    let import_mod env alias m allow_duplicate_import loc =
        (if is_imported alias m env allow_duplicate_import loc then env
        else
            (* add the imported module id to the env *)
            let env = add_id_to_env alias m env in
            let menv = !(get_module m).dm_env in
            let alias_path = pp_id2str alias in
            let rec add_parents alias_path env =
                (try
                    let idx = String.rindex alias_path '.' in
                    let prefix = String.sub alias_path 0 idx in
                    let prefix_alias = get_id prefix in
                    let prefix_id = dup_id prefix_alias in
                    let dm = ref {
                        dm_name=prefix_id; dm_filename=""; dm_idx= -1; dm_defs=[];
                        dm_deps=[]; dm_env=Env.empty; dm_parsed=false; dm_real=false } in
                    let _ = set_id_entry prefix_id (IdModule dm) in
                    (*let _ = printf "adding parent module '%s' to env\n" (id2str prefix_alias) in*)
                    let env = add_id_to_env prefix_alias prefix_id env in
                    add_parents prefix env
                with Not_found -> env)
                in
            let env = add_parents alias_path env in
            (*printf "Importing %s into %s\n" (id2str m) (id2str current_mod);*)
            (* and also import all the overloaded operators from the module
               to make them usable in the corresponding arithmetic expressions *)
            List.fold_left (fun env op_name ->
                let entries = find_all op_name menv in
                import_entries env m op_name entries loc)
            env (fname_always_import())) in

    let (env, mlist) = (List.fold_left (fun (env, mlist) e ->
        match e with
        | DirImport(impdirs, eloc) ->
            ((List.fold_left (fun env (m, alias) ->
                try
                    import_mod env alias m true eloc
                with CompileError(_, _) as err -> push_compile_err err; env) env impdirs), mlist)
        | DirImportFrom(m, implist, eloc) ->
            let env =
            (try
                let menv = !(get_module m).dm_env in
                let keys = if implist != [] then implist else
                    (Env.fold (fun k ids l -> k :: l) menv []) in
                (*let _ = ((printf "imported from %s: " (id2str m)); (List.iter (fun k -> printf "%s, " (id2str k)) keys); (printf "\n")) in*)
                let env = List.fold_left (fun env k ->
                    try
                        let entries = find_all k menv in
                        let _ = (if entries != [] then () else
                            raise_compile_err eloc
                                (sprintf "no symbol %s found in %s" (pp_id2str k) (pp_id2str m))) in
                        import_entries env m k entries eloc
                    with CompileError(_, _) as err -> push_compile_err err; env) env keys
                    in
                (* after processing "from m import ..." we also implicitly insert "import m" *)
                let alias = get_orig_id m in
                import_mod env alias m true eloc
            with CompileError(_, _) as err -> push_compile_err err; env) in
            (env, (m, eloc) :: mlist)
        | DirPragma(prl, eloc) ->
            (env, mlist)
        | _ -> (env, mlist)) (env, []) eseq) in

    check_compile_errs();
    env

(*
    create fresh unique name for each type and register it (put it to env)
*)
and reg_types eseq env sc =
    let make_default_alias templ_args tn =
        TypApp((List.map (fun targ -> TypApp([], targ)) templ_args), tn) in
    List.fold_left (fun env e ->
        match e with
        | DefTyp dt ->
            let {dt_name; dt_templ_args; dt_typ; dt_loc} = !dt in
            let dt_name1 = dup_id dt_name in
            dt := { dt_name=dt_name1; dt_templ_args; dt_typ; dt_finalized=false; dt_scope=sc; dt_loc };
            set_id_entry dt_name1 (IdTyp dt);
            add_id_to_env_check dt_name dt_name1 env (check_for_duplicate_typ dt_name sc dt_loc)
        | DefVariant dvar ->
            let { dvar_name; dvar_templ_args; dvar_flags; dvar_cases; dvar_loc } = !dvar in
            let dvar_name1 = dup_id dvar_name in
            let dvar_alias1 = make_default_alias dvar_templ_args dvar_name1 in
            let dummy_ctors = List.map (fun (n, _) -> n) dvar_cases in
            dvar := {dvar_name=dvar_name1; dvar_templ_args; dvar_alias=dvar_alias1; dvar_flags; dvar_cases;
                    dvar_ctors=dummy_ctors; dvar_templ_inst=[]; dvar_scope=sc; dvar_loc};
            set_id_entry dvar_name1 (IdVariant dvar);
            add_id_to_env_check dvar_name dvar_name1 env (check_for_duplicate_typ dvar_name sc dvar_loc)
        | DefClass {contents={dcl_loc=loc}} -> raise_compile_err loc "classes are not supported yet"
        | DefInterface {contents={di_loc=loc}} -> raise_compile_err loc "interfaces are not supported yet"
        | _ -> env) env eseq

and register_typ_constructor n ctor templ_args argtyps rt env sc decl_loc =
    let ctyp = match argtyps with [] -> rt | _ -> TypFun(argtyps, rt) in
    let args = List.mapi (fun i _ -> PatIdent((get_id (sprintf "arg%d" i)), decl_loc)) argtyps in
    let cname = dup_id n in
    (*let _ = (printf "registering constructor '%s' of type " (id2str cname); pprint_typ_x (TypFun(argtyps, rt)) decl_loc; printf "\n") in*)
    let df = ref { df_name=cname; df_templ_args=templ_args;
            df_args=args; df_typ=ctyp; df_body=(ExpNop decl_loc);
            df_flags={(default_fun_flags()) with fun_flag_ctor=ctor};
            df_templ_inst=[]; df_scope=sc; df_env=env; df_loc=decl_loc } in
    set_id_entry cname (IdFun df);
    (cname, ctyp)

(* check the type definition body (for simple types, including records, and also variant types) *)
and check_types eseq env sc =
    List.fold_left (fun env e ->
        match e with
        | DefTyp dt ->
            let {dt_name; dt_templ_args; dt_typ; dt_scope; dt_loc} = !dt in
            let env1 = List.fold_left (fun env1 t_arg ->
                add_typ_to_env t_arg (TypApp([], t_arg)) env1) env dt_templ_args in
            let dt_typ = deref_typ (check_typ dt_typ env1 dt_scope dt_loc) in
            let dt_typ = finalize_record_typ dt_typ dt_loc in
            dt := {dt_name; dt_templ_args; dt_typ=dt_typ; dt_finalized=true; dt_scope; dt_loc};
            (* in the case of record we add the record constructor function
               to support the constructions 'record_name { f1=e1, f2=e2, ..., fn=en }' gracefully *)
            (match dt_typ with
            | TypRecord _ ->
                let (cname, ctyp) = register_typ_constructor dt_name CtorStruct dt_templ_args (dt_typ::[]) dt_typ env1 dt_scope dt_loc in
                add_id_to_env_check (get_orig_id dt_name) cname env (check_for_duplicate_fun ctyp env dt_scope dt_loc)
            | _ -> env)
        | DefVariant dvar ->
            let _ = instantiate_variant [] dvar env sc (!dvar.dvar_loc) in
            let { dvar_name; dvar_templ_args; dvar_cases; dvar_ctors; dvar_scope; dvar_loc } = !dvar in
            List.fold_left2 (fun env (n, t) cname ->
                let {df_name; df_templ_args; df_typ} = (match (id_info cname) with
                    | IdFun df -> !df
                    | _ -> raise_compile_err dvar_loc (sprintf "internal error: constructor %s is not a function" (id2str cname))) in
                let (t, _) = preprocess_templ_typ df_templ_args df_typ env sc dvar_loc in
                add_id_to_env_check n cname env (check_for_duplicate_fun t env sc dvar_loc)) env dvar_cases dvar_ctors
        | _ -> env
    ) env eseq

(*
    * create fresh unique name for the function
    * check its arguments (very light sanity check for template functions;
        full-scale check for non-template function)
    * update the function definition (put in the new name, verified args, verified function type, the correct scope)
    * put the entry into the global symbol table
    * add it into the environment
*)
and reg_deffun df env sc =
    let { df_name; df_args; df_typ; df_body; df_flags; df_loc } = !df in
    let df_name1 = dup_id df_name in
    let rt = (match df_typ with
              | TypFun(_, rt) -> rt
              | _ -> raise_compile_err df_loc "incorrect function type") in
    let df_sc = (ScFun df_name1) :: sc in
    let (args1, argtyps1, env1, idset1, templ_args1, all_typed) = List.fold_left
            (fun (args1, argtyps1, env1, idset1, templ_args1, all_typed) arg ->
            let t = make_new_typ() in
            let (arg1, env1, idset1, templ_args1, typed) =
                check_pat arg t env1 idset1 templ_args1 df_sc true true false in
            let (arg1, templ_args1) = if typed then (arg1, templ_args1) else
                let targ = gen_temp_id "'targ" in
                let arg1 = PatTyped(arg1, TypApp([], targ), get_pat_loc arg1) in
                let templ_args1 = IdSet.add targ templ_args1 in
                (arg1, templ_args1)
                in
            ((arg1 :: args1), (t :: argtyps1), env1, idset1, templ_args1, all_typed && typed))
            ([], [], env, IdSet.empty, IdSet.empty, true) df_args in
    let _ = match (options.relax, all_typed, df_name1, sc) with
        | (false, false, Id.Val(_, _), ScModule _ :: _) ->
            raise_compile_err df_loc "types of all the parameters of global functions must be explicitly specified"
        | _ -> () in
    let dummy_rt_pat = PatTyped(PatAny(df_loc), rt, df_loc) in
    let (dummy_rt_pat1, env1, idset1, templ_args1, _) =
            check_pat dummy_rt_pat (make_new_typ()) env1
            idset1 templ_args1 df_sc true true false in
    let rt = match dummy_rt_pat1 with
             | PatTyped(_, rt, _) -> rt
             | _ -> raise_compile_err df_loc "invalid return pattern after check" in
    let df_typ1 = TypFun((List.rev argtyps1), rt) in
    let env1 = add_id_to_env_check df_name df_name1 env1
            (check_for_duplicate_fun df_typ1 env1 sc df_loc) in
    df := { df_name=df_name1; df_templ_args=(IdSet.elements templ_args1);
            df_args=(List.rev args1); df_typ=df_typ1;
            df_body; df_flags; df_scope=sc; df_loc; df_templ_inst=[]; df_env=env1 };
    (*let _ = (printf "\tfun after registration: "; pprint_exp_x (DefFun df); printf "\n~~~~~~~~~~~~~~~~\n") in*)
    set_id_entry df_name1 (IdFun df);
    env1

and check_defexn de env sc =
    let { dexn_name=n0; dexn_typ=t; dexn_loc=loc } = !de in
    let _ = match sc with
        | ScModule _ :: _ -> ()
        | _ -> raise_compile_err loc "exceptions can only be defined at a module level"
        in
    let t = check_typ t env sc loc in
    let n = dup_id n0 in
    let ftyp = typ2constr t TypExn loc in
    de := { dexn_name=n; dexn_typ=t; dexn_scope=sc; dexn_loc=loc };
    set_id_entry n (IdExn de);
    (match sc with
    | ScModule m :: _ when (pp_id2str m) = "Builtins" ->
        (* add both forward and inverse mapping *)
        Hashtbl.add builtin_exceptions n0 n;
        Hashtbl.add builtin_exceptions n n0
    | _ -> ());
    add_id_to_env_check n0 n env (check_for_duplicate_fun ftyp env sc loc)

and check_deffun df env =
    let { df_typ; df_scope; df_loc } = !df in
    instantiate_fun df df_typ env df_scope df_loc false

(*
    * leave simple types as-is
    * in the case of complex type (ref, array, tuple, list, fun, ...) process the nested types.
    * leave TypVar(ref None) as is
    * process t inside TypVar(ref Some(t))
    * the most sophisticated case: TypApp(args, id)
      ** process all the args
      ** look for id in the env; if there is type definition with the same id,
         1. check that the number of args is the same OR that there is 1-to-n or n-to-1 relationship.
            (that is, a type with a single arg can be instantiated using a tuple parameter,
            or a type with multiple args is instantiated with a single tuple argument
            (of as many elements as the number of parameters))
         2. if number of args is wrong, report an error
         3. otherwise, create a temporary environment where type arguments are replaced with the actual types.
            process the type definition body using this temporary environment.
         3.1. maybe in some cases before doing 3 need to iterate through a list of type instances and try to find
            the proper match. this step is optional for now, but it will become a mandatory when handling complex types,
            such as variants, classes and maybe records. It can also help to save some time and some space
*)
and check_typ_and_collect_typ_vars t env r_opt_typ_vars sc loc =
    let r_env = ref env in
    let rec check_typ_ t callb =
        (match t with
        | TypApp(ty_args, n) ->
            let ty_args  = List.map (fun t -> check_typ_ t callb) ty_args in
            let ty_args_are_real = List.for_all is_real_typ ty_args in
            let found_typ_opt = find_first n !r_env loc (fun entry ->
                match entry with
                | EnvTyp(t) ->
                    if ty_args = [] then Some(t) else
                    raise_compile_err loc (sprintf "a concrete type (%s) cannot be further instantiated" (pp_id2str n))
                | EnvId(Id.Name _) -> None
                | EnvId(i) ->
                    match (id_info i) with
                    | IdNone | IdVal _ | IdFun _ | IdExn _ | IdModule _ -> None
                    | IdClass _ | IdInterface _ ->
                        (* [TODO] *)
                        raise_compile_err loc "classes & interfaces are not supported yet"
                    | IdTyp dt ->
                        let { dt_name; dt_templ_args; dt_typ; dt_scope; dt_finalized; dt_loc } = !dt in
                        let _ = if dt_finalized then () else
                            raise_compile_err loc
                            (sprintf "later declared non-variant type %s is referenced; try to reorder the type declarations"
                            (pp_id2str dt_name)) in
                        if dt_name = n && ty_args = [] then
                            Some(t)
                        else
                            let env1 = match_ty_templ_args ty_args dt_templ_args !r_env dt_loc loc in
                            Some(check_typ (dup_typ dt_typ) env1 sc loc)
                    | IdVariant dvar ->
                        let { dvar_name; dvar_templ_args; dvar_alias; dvar_templ_inst } = !dvar in
                        Some(if dvar_templ_args = [] then
                            dvar_alias
                        else if ty_args_are_real then
                            (try
                                let t1 = TypApp(ty_args, dvar_name) in
                                let _ = List.find (fun inst ->
                                    match (id_info inst) with
                                    | IdVariant dvar_inst ->
                                        let {dvar_alias=dvar_inst_alias} = !dvar_inst in
                                        maybe_unify t1 dvar_inst_alias true
                                    | _ -> raise_compile_err loc
                                        (sprintf "invalid type of variant instance %s (must be also a variant)" (id2str i)))
                                    dvar_templ_inst in
                                t1
                            with Not_found ->
                                let (_, inst_app_typ) = instantiate_variant ty_args dvar !r_env sc loc in inst_app_typ)
                        else TypApp(ty_args, dvar_name))) in
            (match (r_opt_typ_vars, ty_args, found_typ_opt) with
            | (_, _, Some(new_t)) -> new_t
            | (Some(r_typ_vars), [], _) when (Utils.starts_with (id2str n) "'") ->
                r_typ_vars := IdSet.add n !r_typ_vars;
                r_env := add_typ_to_env n t !r_env;
                t
            | _ -> report_not_found n loc)
        | TypVarTuple t_opt ->
            (match r_opt_typ_vars with
            | Some(r_typ_vars) -> r_typ_vars := IdSet.add (get_id "__var_tuple__") !r_typ_vars
            | _ -> ());
            walk_typ t callb
        | TypVarArray t_opt ->
            (match r_opt_typ_vars with
            | Some(r_typ_vars) -> r_typ_vars := IdSet.add (get_id "__var_array__") !r_typ_vars
            | _ -> ());
            walk_typ t callb
        | TypVarRecord ->
            (match r_opt_typ_vars with
            | Some(r_typ_vars) -> r_typ_vars := IdSet.add (get_id "__var_record__") !r_typ_vars
            | _ -> ());
            walk_typ t callb
        | TypRecord {contents=(relems, _)} ->
            check_for_rec_field_duplicates (List.map (fun (n, _, _) -> n) relems) loc;
            walk_typ t callb
        | _ -> walk_typ t callb) in
    let callb = { acb_typ=Some(check_typ_); acb_exp=None; acb_pat=None } in
    (check_typ_ t callb, !r_env)

and check_typ t env sc loc =
    let (t, _) = check_typ_and_collect_typ_vars t env None sc loc in t

and instantiate_fun templ_df inst_ftyp inst_env0 inst_sc inst_loc instantiate =
    let { df_name } = !templ_df in
    if instantiate then
        compile_err_ctx :=
            (sprintf "when instantiating '%s' at %s" (pp_id2str df_name) (loc2str inst_loc)) ::
            !compile_err_ctx
    else ();
    (try
        let inst_df = instantiate_fun_ templ_df inst_ftyp inst_env0 inst_sc inst_loc instantiate in
        let _ = if instantiate then compile_err_ctx := List.tl !compile_err_ctx else () in
        inst_df
    with e ->
        if instantiate then compile_err_ctx := List.tl !compile_err_ctx else ();
        raise e)

and instantiate_fun_ templ_df inst_ftyp inst_env0 inst_sc inst_loc instantiate =
    let { df_name; df_templ_args; df_args; df_body; df_flags; df_scope; df_loc; df_templ_inst } = !templ_df in
    let is_constr = is_fun_ctor df_flags in
    let _ = if not is_constr then () else raise_compile_err inst_loc
        (sprintf "internal error: attempt to instantiate constructor '%s'. it should be instantiated in a different way. try to use explicit type specification somewhere" (id2str df_name)) in
    let nargs = List.length df_args in
    let inst_env = inst_env0 in
    let inst_ftyp = deref_typ inst_ftyp in
    let (arg_typs, rt) = (match inst_ftyp with
                    | TypFun((TypVoid :: []), rt) -> ([], rt)
                    | TypFun(arg_typs, rt) -> (arg_typs, rt)
                    | rt -> if is_constr (*&& df_args = []*) then ([], rt)
                        else raise_compile_err inst_loc
                        "internal error: the type of instantiated function is not a function") in
    let ninst_args = List.length arg_typs in
    let arg_typs = if nargs = ninst_args then arg_typs else
                    if nargs = 1 then TypTuple(arg_typs) :: [] else
                    (match arg_typs with
                    | TypTuple(elems) :: [] when (List.length elems)=nargs -> elems
                    | _ -> raise_compile_err df_loc
                        (sprintf "during instantion at %s: incorrect number of actual parameters =%d (vs expected %d)"
                        (loc2str inst_loc) ninst_args nargs)) in
    let inst_name = if instantiate then (dup_id df_name) else df_name in
    (*let _ = (printf "instantiation of %s %s with type<" (if is_constr then
        "constructor" else "function") (id2str inst_name); pprint_typ_x inst_ftyp inst_loc; printf ">:\n") in*)
    (*let _ = print_env "" inst_env inst_loc in*)
    let fun_sc = (ScFun inst_name) :: inst_sc in
    let (df_inst_args, inst_env, _) = List.fold_left2
        (fun (df_inst_args, inst_env, idset) df_arg arg_typ ->
            let (df_inst_arg, inst_env, idset, _, _) =
                check_pat (dup_pat df_arg) arg_typ inst_env idset IdSet.empty fun_sc false true false in
            ((df_inst_arg :: df_inst_args), inst_env, idset)) ([], inst_env, IdSet.empty) df_args arg_typs in
    let df_inst_args = List.rev df_inst_args in
    let rt = check_typ rt inst_env df_scope inst_loc in
    let inst_body = if instantiate then dup_exp df_body else df_body in
    (*let _ = ((printf "processing function %s " (id2str inst_name));
        pprint_pat_x (PatTuple(df_inst_args, inst_loc)); printf "<";
        List.iteri (fun i n -> printf "%s%s" (if i=0 then "" else ", ") (id2str n)) df_templ_args;
        printf ">\n") in
    let _ = (printf "\tof typ: "; pprint_typ_x inst_ftyp; printf "\n") in
    let _ = (printf "\tfun before type checking: ";
        pprint_exp_x (DefFun templ_df); printf "\n~~~~~~~~~~~~~~~~\n") in
    let _ = print_env (sprintf "before processing function body of %s defined at %s"
        (id2str inst_name) (loc2str df_loc)) inst_env inst_loc in*)
    let (body_typ, body_loc) = get_exp_ctx inst_body in
    let _ = if is_constr then () else unify body_typ rt body_loc "the function body type does not match the function type" in
    let inst_ftyp = match (arg_typs, is_constr) with ([], true) -> rt | _ -> TypFun(arg_typs, rt) in
    (*let _ = (printf "instantiated '%s' with nargs=%d, typ=" (id2str inst_name) nargs;
        pprint_typ_x inst_ftyp; printf "\n") in*)
    let inst_df = ref { df_name=inst_name; df_templ_args=[]; df_args=df_inst_args;
                        df_typ=inst_ftyp; df_body=inst_body;
                        df_flags; df_scope=inst_sc; df_loc=inst_loc; df_templ_inst=[]; df_env=inst_env0 } in
    let _ = set_id_entry inst_name (IdFun inst_df) in
    let _ = if instantiate then
        templ_df := {!templ_df with df_templ_inst = inst_name :: !templ_df.df_templ_inst}
    else
        () in
    let inst_body = instantiate_fun_body inst_name inst_ftyp
        df_inst_args inst_body inst_env fun_sc inst_loc in
    (* update the function return type *)
    let _ = unify (get_exp_typ inst_body) rt inst_loc "the function body has inconsistent type" in
    let inst_ftyp = match (arg_typs, is_constr) with ([], true) -> rt | _ -> TypFun(arg_typs, rt) in
    inst_df := {!inst_df with df_body=inst_body; df_typ=inst_ftyp};
    (*!inst_or_templ_df.df_body <- inst_body;*)
    (*(printf "<<<processed function:\n"; (pprint_exp_x (DefFun inst_df)); printf "\n>>>\n");*)
    inst_df

and instantiate_fun_body inst_name inst_ftyp inst_args inst_body inst_env fun_sc inst_loc =
    let ftyp = deref_typ_rec inst_ftyp in
    let body_loc = get_exp_loc inst_body in
    match (pp_id2str inst_name) with
    | "__eq_variants__" ->
        (match (ftyp, inst_args) with
        | (TypFun(TypApp([], n1) :: TypApp([], n2) :: [], TypBool),
            PatTyped(PatIdent(a, _), _, _) :: PatTyped(PatIdent(b, _), _, _) :: [])
            when n1 = n2 && (match (id_info n1) with IdVariant _ -> true | _ -> false) ->
            (*
               if the variant is an instance of template variant,
               we take the variant cases from the original prototype, not from the instance.
               This is because we need to correctly calculate the number of parameters for each
               variant constructor.

               e.g. type 't option = None | Some: 't
               type_t i2_opt = (int, int) option
               Some() for i2_opt will still have 1 parameter.
            *)
            let argtyp = TypApp([], n1) in
            let astr = pp_id2str a in
            let bstr = pp_id2str b in
            let (var_ctors, proto_cases) = match (id_info n1) with
                | IdVariant {contents={dvar_ctors; dvar_alias=TypApp(_, proto_n)}} ->
                    (match (id_info proto_n) with
                    | IdVariant {contents={dvar_cases=proto_cases}} -> (dvar_ctors, proto_cases)
                    | _ -> raise_compile_err inst_loc "the prototype of variant instance should be a variant")
                | _ -> raise_compile_err inst_loc "variant is expected here"
                in
            let complex_cases = List.fold_left2 (fun complex_cases n (n_orig, t_orig) ->
                let t = deref_typ_rec t_orig in
                match t with
                | TypVoid -> complex_cases
                | TypRecord {contents=(relems, _)} ->
                    let (_, al, bl, cmp_code) = List.fold_left (fun (idx, al, bl, cmp_code) (rn, _, _) ->
                        let ai = get_id (astr ^ (string_of_int idx)) in
                        let bi = get_id (bstr ^ (string_of_int idx)) in
                        let cmp_ab = ExpBinary(OpCmp(CmpEQ),
                            ExpIdent(ai, (make_new_typ(), body_loc)),
                            ExpIdent(bi, (make_new_typ(), body_loc)),
                            (TypBool, body_loc)) in
                        let cmp_code = if idx = 1 then cmp_ab else
                            ExpBinary(OpBitwiseAnd, cmp_code, cmp_ab, (TypBool, body_loc))
                            in
                        (idx+1, (rn, PatIdent(ai, body_loc)) :: al, (rn, PatIdent(bi, body_loc)) :: bl, cmp_code))
                        (1, [], [], ExpNop(body_loc)) relems
                        in
                    let a_case_pat = PatRecord((Some n), (List.rev al), body_loc) in
                    let b_case_pat = PatRecord((Some n), (List.rev bl), body_loc) in
                    let ab_case_pat = PatTuple([a_case_pat; b_case_pat], body_loc) in
                    (ab_case_pat :: [], cmp_code) :: complex_cases
                | _ ->
                    let args = match t with TypTuple(tl) -> tl | _ -> t :: [] in
                    let (_, al, bl, cmp_code) = List.fold_left (fun (idx, al, bl, cmp_code) _ ->
                        let ai = get_id (astr ^ (string_of_int idx)) in
                        let bi = get_id (bstr ^ (string_of_int idx)) in
                        let cmp_ab = ExpBinary(OpCmp(CmpEQ),
                            ExpIdent(ai, (make_new_typ(), body_loc)),
                            ExpIdent(bi, (make_new_typ(), body_loc)),
                            (TypBool, body_loc)) in
                        let cmp_code = if idx = 1 then cmp_ab else
                            ExpBinary(OpBitwiseAnd, cmp_code, cmp_ab, (TypBool, body_loc))
                            in
                        (idx+1, PatIdent(ai, body_loc) :: al, PatIdent(bi, body_loc) :: bl, cmp_code))
                        (1, [], [], ExpNop(body_loc)) args
                        in
                    let a_case_pat = PatVariant(n, (List.rev al), body_loc) in
                    let b_case_pat = PatVariant(n, (List.rev bl), body_loc) in
                    let ab_case_pat = PatTuple([a_case_pat; b_case_pat], body_loc) in
                    (ab_case_pat :: [], cmp_code) :: complex_cases) [] var_ctors proto_cases
                in
            let a = ExpIdent((get_id astr), (argtyp, body_loc)) in
            let b = ExpIdent((get_id bstr), (argtyp, body_loc)) in
            let tag = ExpIdent(__tag_id__, (TypString, body_loc)) in
            let a_tag = ExpMem(a, tag, (TypInt, body_loc)) in
            let b_tag = ExpMem(b, tag, (TypInt, body_loc)) in
            let cmp_tags = ExpBinary(OpCmp(CmpEQ), a_tag, b_tag, (TypBool, body_loc)) in
            let inst_body = match complex_cases with
                | [] -> cmp_tags
                | _ ->
                    let default_case = (PatAny(body_loc) :: [], cmp_tags) in
                    let ab = ExpMkTuple([a; b], (TypTuple([argtyp; argtyp]), body_loc)) in
                    ExpMatch(ab, List.rev (default_case :: complex_cases), (TypBool, body_loc))
                in
            let body = check_exp inst_body inst_env fun_sc in
            (*(printf "generated and typechecked body:\n"; (pprint_exp_x body); printf "\n");*)
            body
        | _ -> raise_compile_err inst_loc
            "__eq_variants__ can only be applied to 2 variants of the same type")
    | _ -> check_exp inst_body inst_env fun_sc

and instantiate_variant ty_args dvar env sc loc =
    let { dvar_name; dvar_templ_args; dvar_alias; dvar_flags; dvar_cases;
          dvar_ctors; dvar_scope; dvar_loc } = !dvar in
    (*
       env - the input environment updated with the mappings from the formal variant type parameters to the actual values;
       inst_typ - the return type in the variant constructors; it is used to inference the return type of constructors.
       inst_app_typ - typ used to reference the original template type from the instance
            (or reference the template type from itself). it's used to search for the proper instance of the variant type.
    *)
    (*let _ = (printf "variant before instantiation: {{{ "; pprint_exp_x (DefVariant dvar); printf "\n\twith ty_args=[";
        List.iteri (fun i t -> if i = 0 then () else printf ", "; pprint_typ_x t) ty_args; printf "] }}}\n") in*)
    let (instantiate, env, inst_name, inst_app_typ, inst_dvar) = match ty_args with
        | [] -> (* not actually an instantiation, but finalization of the original (template or not) variant declaration *)
            let env = List.fold_left (fun env tn -> add_typ_to_env tn (TypApp([], tn)) env) env dvar_templ_args in
                (false, env, dvar_name, dvar_alias, dvar)
        | _ ->
            let inst_name = dup_id dvar_name in
            let inst_app_typ = TypApp(ty_args, dvar_name) in
            (true, (match_ty_templ_args ty_args dvar_templ_args env dvar_loc loc),
            inst_name, inst_app_typ,
            ref { dvar_name=inst_name; dvar_templ_args=[];
                dvar_alias=inst_app_typ; dvar_flags; dvar_cases; dvar_ctors;
                dvar_templ_inst=[]; dvar_scope; dvar_loc=loc }) in
    (* register the incomplete-yet variant in order to avoid inifinite loop *)
    let _ = if instantiate then
        (set_id_entry inst_name (IdVariant inst_dvar);
        !dvar.dvar_templ_inst <- inst_name :: !dvar.dvar_templ_inst) else () in
    let (_, inst_cases, inst_constr) = List.fold_left2 (fun (idx, inst_cases, inst_constr) (n, t) cname ->
        let nargs = match t with TypTuple(telems) -> List.length telems | TypVoid -> 0 | _ -> 1 in
        let t = check_typ (dup_typ t) env sc loc in
        let t = finalize_record_typ t loc in
        let nrealargs = match t with TypTuple(telems) -> List.length telems | TypVoid -> 0 | _ -> 1 in
        let argtyps = match (t, nargs) with
            | (TypVoid, 0) -> []
            | (_, 1) -> t :: []
            | (TypTuple(telems), _) when nrealargs = nargs -> telems
            | _ ->
                raise_compile_err loc
                (sprintf "cannot instantiate case '%s' of variant '%s' defined at '%s': wrong number of actual parameters %d (vs %d expected)"
                (id2str cname) (id2str dvar_name) (loc2str dvar_loc) nrealargs nargs) in
        let (inst_cname, _) = register_typ_constructor cname (CtorVariant n) (!inst_dvar.dvar_templ_args)
            argtyps inst_app_typ env dvar_scope dvar_loc in
        if instantiate then
            match id_info cname with
            | IdFun c_def -> c_def := {!c_def with df_templ_inst = inst_cname :: !c_def.df_templ_inst }
            | _ -> raise_compile_err loc (sprintf "invalid constructor %s of variant %s" (id2str cname) (id2str dvar_name))
        else ();
        (idx+1, (n, t) :: inst_cases, inst_cname :: inst_constr)) (0, [], []) dvar_cases dvar_ctors in
    !inst_dvar.dvar_cases <- List.rev inst_cases;
    !inst_dvar.dvar_ctors <- List.rev inst_constr;
    (*printf "variant after instantiation: {{{ "; pprint_exp_x (DefVariant inst_dvar); printf " }}}\n";*)
    (inst_name, inst_app_typ)

and check_pat pat typ env idset typ_vars sc proto_mode simple_pat_mode is_mutable =
    let r_idset = ref idset in
    let r_env = ref env in
    let r_typ_vars = ref typ_vars in
    let captured_val_flags = {(default_val_flags()) with val_flag_mutable=is_mutable} in
    let rec process_id i t loc =
        let i0 = get_orig_id i in
        (if (IdSet.mem i0 !r_idset) then
            raise_compile_err loc (sprintf "duplicate identifier '%s' in the pattern" (id2str i0))
        else
            r_idset := IdSet.add i0 !r_idset;
        if proto_mode then i0 else
            (let j = dup_id i0 in
            (*let _ = printf "check_pat: pattern %s is replaced with %s at %s\n" (id2str i0) (id2str j) (loc2str loc) in*)
            let dv = { dv_name=j; dv_typ=t; dv_flags=captured_val_flags; dv_scope=sc; dv_loc=loc } in
            set_id_entry j (IdVal dv);
            r_env := add_id_to_env i0 j !r_env;
            j))
    and check_pat_ p t = match p with
        | PatAny _ -> (p, false)
        | PatLit(l, loc) ->
            if simple_pat_mode then raise_compile_err loc "literals are not allowed here" else ();
            unify t (get_lit_typ l) loc "the literal of unexpected type";
            (p, (match l with LitNil -> false | _ -> true))
        | PatIdent(i, loc) ->
            if (pp_id2str i) = "_" then raise_compile_err loc "'_' occured in PatIdent()" else ();
            (PatIdent((process_id i t loc), loc), false)
        | PatTuple(pl, loc) ->
            let tl = List.map (fun p -> make_new_typ ()) pl in
            let _ = unify t (TypTuple tl) loc "improper type of the tuple pattern" in
            let (pl_new, typed) = List.fold_left2 (fun (pl_new, typed) p t ->
                let (pj, typed_j) = check_pat_ p t in
                (pj:: pl_new, typed && typed_j)) ([], true) pl tl in
            (PatTuple(List.rev pl_new, loc), typed)
        | PatVariant(v, pl, loc) ->
            if not proto_mode then
                (* [TODO] in the ideal case this branch should work fine in the prototype mode as well,
                   just need to make lookup_id smart enough (maybe add some extra parameters to
                   avoid preliminary type instantiation) *)
                let tl = List.map (fun p -> make_new_typ()) pl in
                let ctyp = match tl with [] -> t | _ -> TypFun(tl, t) in
                (*let _ = print_env "env @ report_not_found: " env loc in*)
                let (v_new, _) = lookup_id v ctyp !r_env sc loc in
                (*let _ = printf "checking '%s'~'%s' with %d params at %s\n" (id2str v) (id2str v_new) (List.length pl) (loc2str loc) in*)

                (* in principle, non-template variant with a single case can be considered
                   as explicitly typed, but we set it typed=false for now for simplicity *)
                (PatVariant(v_new, (List.map2 (fun p t -> let (p, _) = check_pat_ p t in p) pl tl), loc), false)
            else
            (match (deref_typ t) with
            | TypApp(ty_args, n) ->
                (match (id_info n) with
                | IdVariant dv ->
                    let { dvar_cases } = !dv in
                    if (List.length dvar_cases) != 1 then
                        raise_compile_err loc "a label of multi-case variant may not be used in a formal function parameter"
                    else
                    (try
                        let (vi, ti) = List.find (fun (vi, ti) -> vi = v) dvar_cases in
                        let ni = (match ti with
                        | TypTuple(tl) -> List.length tl
                        | TypVoid -> raise_compile_err loc
                            (sprintf "a variant label '%s' with no arguments may not be used in a formal function parameter"
                                (pp_id2str vi))
                        | _ -> 1) in
                        if ni != (List.length pl) then
                            raise_compile_err loc
                            (sprintf "the number of variant pattern arguments does not match to the description of variant case '%s'"
                                (pp_id2str vi))
                            else ();
                        (PatVariant(v, pl, loc), false)
                    with Not_found ->
                        raise_compile_err loc
                        (sprintf "the variant constructor '%s' is not found" (pp_id2str v)))
                | _ -> raise_compile_err loc "variant pattern is used with non-variant type")
            | _ -> raise_compile_err loc "variant pattern is used with non-variant type")
        | PatRecord(rn_opt, relems, loc) ->
            if not proto_mode then
                (* [TODO] in the ideal case this branch should work fine in the prototype mode as well,
                   just need to make lookup_id smart enough (maybe add some extra parameters to
                   avoid preliminary type instantiation) *)
                let (ctor, relems_found) = get_record_elems rn_opt t loc in
                let new_relems = List.fold_left (fun new_relems (n, p) ->
                    let n_orig = get_orig_id n in
                    match (List.find_opt (fun (nj, tj, _) -> (get_orig_id nj) = n_orig) relems_found) with
                    | Some((nj, tj, _)) ->
                        let (p, _) = check_pat_ p tj in (n, p) :: new_relems
                    | _ -> raise_compile_err loc
                        (sprintf "element '%s' is not found in the record '%s'" (pp_id2str n) (pp_id2str (Utils.opt_get rn_opt noid))))
                    [] relems
                    in
                (PatRecord((if ctor <> noid then Some ctor else None), (List.rev new_relems), loc), false)
            else
                raise_compile_err loc "record patterns are not supported in function prototypes yet"
        | PatCons(p1, p2, loc) ->
            let t1 = make_new_typ() in
            let t2 = TypList t1 in
            let _ = if simple_pat_mode then raise_compile_err loc "'::' pattern is not allowed here" else () in
            let _ = unify t t2 loc "'::' pattern is used with non-list type" in
            let (p1, _) = check_pat_ p1 t1 in
            let (p2, _) = check_pat_ p2 t2 in
            (PatCons(p1, p2, loc), false)
        | PatAs(p1, i, loc) ->
            let (p1_new, typed) = check_pat_ p1 t in
            let i_new = process_id i t loc in
            (PatAs(p1_new, i_new, loc), typed)
        | PatTyped(p1, t1, loc) ->
            let (t1_new, env1) = check_typ_and_collect_typ_vars t1 !r_env
                (if proto_mode then Some(r_typ_vars) else None) sc loc in
            let _ = r_env := env1 in
            let _ = unify t1_new t loc "inconsistent explicit type specification" in
            let (p1, _) = check_pat_ p1 t in
            (PatTyped(p1, t1_new, loc), true)
        | PatRef(p1, loc) ->
            let t1 = make_new_typ() in
            let _ = unify t (TypRef t1) loc "'ref' pattern is used with non-reference type" in
            let (p_new, typed) = check_pat_ p1 t1 in
            (PatRef(p_new, loc), typed)
        | PatWhen(p1, e1, loc) ->
            let (p1, _) = check_pat_ p1 t in
            let (etyp, eloc) = get_exp_ctx e1 in
            let _ = unify etyp TypBool loc "'when' clause should have boolean type" in
            let e1 = check_exp e1 !r_env sc in
            if simple_pat_mode then raise_compile_err loc "'when' pattern is not allowed here" else ();
            (PatWhen(p1, e1, loc), false)
    in let (pat_new, typed) = check_pat_ pat typ in
    (pat_new, !r_env, !r_idset, !r_typ_vars, typed)

and check_cases cases inptyp outtyp env sc loc =
    List.map (fun (plist, e) ->
        let case_sc = new_block_scope() :: sc in
        let (plist1, env1, capt1) = List.fold_left (fun (plist1, env1, capt1) p ->
            let (p2, env2, capt2, _, _) = check_pat p inptyp env1 capt1 IdSet.empty case_sc false false false in
            (p2 :: plist1, env2, capt2)) ([], env, IdSet.empty) plist in
        let (plist1, env1, capt1) = if (List.length plist1) = 1 then (plist1, env1, capt1) else
            let _ = if (IdSet.is_empty capt1) then () else
                raise_compile_err loc "captured variables may not be used in the case of multiple alternatives ('|' pattern)" in
            let temp_id = gen_temp_id "p" in
            let temp_dv = { dv_name=temp_id; dv_typ=inptyp;
                dv_flags=default_tempval_flags(); dv_scope=sc; dv_loc=loc } in
            let _ = set_id_entry temp_id (IdVal temp_dv) in
            let capt1 = IdSet.add temp_id capt1 in
            let env1 = add_id_to_env temp_id temp_id env1 in
            let temp_pat = PatIdent(temp_id, loc) in
            let temp_id_exp = ExpIdent(temp_id, (inptyp, loc)) in
            let bool_ctx = (TypBool, loc) in
            let when_cases = List.map (fun p ->
                match p with
                | PatAny _ | PatIdent _ ->
                    raise_compile_err loc "in the case of multiple alternatives ('|' pattern) '_' or indent cannot be used"
                | _ -> ([p], ExpLit((LitBool true), bool_ctx))) plist1 in
            let when_cases = when_cases @ [([PatAny(loc)], ExpLit((LitBool false), bool_ctx))] in
            let when_pat = PatWhen(temp_pat, ExpMatch(temp_id_exp, when_cases, bool_ctx), loc) in
            ([when_pat], env1, capt1)
            in
        let (e1_typ, e1_loc) = get_exp_ctx e in
        unify e1_typ outtyp e1_loc
            "the case expression type does not match the whole expression type (or the type of previous case(s))";
        ((List.rev plist1), (check_exp e env1 case_sc))) cases

and check_mod m =
    let minfo = !(get_module m) in
    try
        let modsc = (ScModule m) :: [] in
        let (seq, env) = check_eseq minfo.dm_defs Env.empty modsc false in
        minfo.dm_defs <- seq;
        minfo.dm_env <- env
    with
    | CompileError(_, _) as err -> push_compile_err err
    | PropagateCompileError -> ()
