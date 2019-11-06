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
                | IdFun {contents={df_templ_inst}} -> df_templ_inst
                | IdVariant {contents={dvar_templ_inst}} -> dvar_templ_inst
                | _ -> []
            in if templ_inst = [] then () else
                (List.iteri (fun i ni ->
                    let t = match id_info ni with
                        | IdFun {contents={df_typ}} -> df_typ
                        | IdVariant {contents={dvar_alias}} -> dvar_alias
                        | _ -> TypVar(ref None)
                    in printf "%s%s<" (if i = 0 then " -> [" else ", ") (id2str ni);
                    pprint_typ_x t; printf ">") templ_inst;
                printf "]")
        | EnvTyp t -> printf "<some type>") entries);
        printf ";\n") env;
    printf "]\n"

type ast_callb_t =
{
    acb_typ: (typ_t -> ast_callb_t -> typ_t) option;
    acb_exp: (exp_t -> ast_callb_t -> exp_t) option;
    acb_pat: (pat_t -> ast_callb_t -> pat_t) option;
}

let rec check_n_walk_typ t callb =
    match callb.acb_typ with
    | Some(f) -> f t callb
    | _ -> walk_typ t callb
and check_n_walk_tlist tlist callb =
    List.map (fun t -> check_n_walk_typ t callb) tlist

and check_n_walk_exp e callb =
    match callb.acb_exp with
    | Some(f) -> f e callb
    | _ -> walk_exp e callb
and check_n_walk_elist elist callb =
    List.map (fun e -> check_n_walk_exp e callb) elist

and check_n_walk_pat p callb =
    match callb.acb_pat with
    | Some(f) -> f p callb
    | _ -> walk_pat p callb
and check_n_walk_plist plist callb =
    List.map (fun p -> check_n_walk_pat p callb) plist

and walk_typ t callb =
    let walk_typ_ t = check_n_walk_typ t callb in
    let walk_tl_ tl = check_n_walk_tlist tl callb in
    (match t with
    | TypVar r ->
        (match !r with
        | Some t ->
            let t1 = walk_typ_ t in
            r := Some t1
        | _ -> ());
        t
    | TypInt -> t
    | TypSInt _ -> t
    | TypUInt _ -> t
    | TypFloat _ -> t
    | TypString -> t
    | TypChar -> t
    | TypBool -> t
    | TypVoid -> t
    | TypFun(args, rt) -> TypFun((walk_tl_ args), (walk_typ_ rt))
    | TypList t -> TypList(walk_typ_ t)
    | TypTuple args -> TypTuple(walk_tl_ args)
    | TypRef t -> TypRef(walk_typ_ t)
    | TypArray(d, et) -> TypArray(d, walk_typ_ et)
    | TypRecord ({contents=(relems, rn)} as r) ->
        let new_relems = List.map (fun (n, t, v) -> (n, (walk_typ_ t), v)) relems in
        r := (new_relems, rn); t
    | TypExn -> t
    | TypErr -> t
    | TypCPointer -> t
    | TypApp(ty_args, n) -> TypApp((walk_tl_ ty_args), n)
    | TypDecl -> t
    | TypModule -> t)

and walk_exp e callb =
    let walk_typ_ t = check_n_walk_typ t callb in
    let walk_exp_ e = check_n_walk_exp e callb in
    let walk_elist_ el = check_n_walk_elist el callb in
    let walk_pat_ p = check_n_walk_pat p callb in
    let walk_plist_ pl = check_n_walk_plist pl callb in
    let walk_pe_l_ pe_l = List.map (fun (p, e) -> ((walk_pat_ p), (walk_exp_ e))) pe_l in
    let walk_ne_l_ ne_l = List.map (fun (n, e) -> (n, (walk_exp_ e))) ne_l in
    let walk_handlers_ ple_l =
        List.map (fun (pl, e) -> ((walk_plist_ pl), (walk_exp_ e))) ple_l in
    let walk_exp_opt_ e_opt =
        (match e_opt with
        | Some e -> Some(walk_exp_ e)
        | None -> None) in
    let walk_ctx_ (t, loc) = ((walk_typ_ t), loc) in

    (match e with
    | ExpNop (_) -> e
    | ExpBreak _ -> e
    | ExpContinue _ -> e
    | ExpRange(e1_opt, e2_opt, e3_opt, ctx) ->
        ExpRange((walk_exp_opt_ e1_opt), (walk_exp_opt_ e2_opt),
                 (walk_exp_opt_ e3_opt), (walk_ctx_ ctx))
    | ExpLit(l, ctx) -> ExpLit(l, (walk_ctx_ ctx))
    | ExpIdent(n, ctx) -> ExpIdent(n, (walk_ctx_ ctx))
    | ExpBinOp(bop, e1, e2, ctx) ->
        ExpBinOp(bop, (walk_exp_ e1), (walk_exp_ e2), (walk_ctx_ ctx))
    | ExpUnOp(uop, e, ctx) -> ExpUnOp(uop, (walk_exp_ e), (walk_ctx_ ctx))
    | ExpSeq(elist, ctx) -> ExpSeq((walk_elist_ elist), (walk_ctx_ ctx))
    | ExpMkTuple(elist, ctx) -> ExpMkTuple((walk_elist_ elist), (walk_ctx_ ctx))
    | ExpMkArray(ell, ctx) -> ExpMkArray((List.map walk_elist_ ell), (walk_ctx_ ctx))
    | ExpMkRecord(e, ne_l, ctx) -> ExpMkRecord((walk_exp_ e), (walk_ne_l_ ne_l), (walk_ctx_ ctx))
    | ExpUpdateRecord(e, ne_l, ctx) -> ExpUpdateRecord((walk_exp_ e), (walk_ne_l_ ne_l), (walk_ctx_ ctx))
    | ExpCall(f, args, ctx) -> ExpCall((walk_exp_ f), (walk_elist_ args), (walk_ctx_ ctx))
    | ExpAt(arr, idx, ctx) -> ExpAt((walk_exp_ arr), (walk_elist_ idx), (walk_ctx_ ctx))
    | ExpAssign(lv, rv, loc) -> ExpAssign((walk_exp_ lv), (walk_exp_ rv), loc)
    | ExpMem(a, member, ctx) -> ExpMem((walk_exp_ a), (walk_exp_ member), (walk_ctx_ ctx))
    | ExpDeref(a, ctx) -> ExpDeref((walk_exp_ a), (walk_ctx_ ctx))
    | ExpMakeRef(a, ctx) -> ExpMakeRef((walk_exp_ a), (walk_ctx_ ctx))
    | ExpThrow(a, loc) -> ExpThrow((walk_exp_ a), loc)
    | ExpIf(c, then_e, else_e, ctx) ->
        ExpIf((walk_exp_ c), (walk_exp_ then_e), (walk_exp_ else_e), (walk_ctx_ ctx))
    | ExpWhile(c, e, loc) -> ExpWhile((walk_exp_ c), (walk_exp_ e), loc)
    | ExpDoWhile(c, e, loc) -> ExpDoWhile((walk_exp_ c), (walk_exp_ e), loc)
    | ExpFor(pe_l, body, flags, loc) ->
        ExpFor((walk_pe_l_ pe_l), (walk_exp_ body), flags, loc)
    | ExpMap(pew_ll, body, flags, ctx) ->
        ExpMap((List.map (fun (pe_l, when_opt) ->
            (walk_pe_l_ pe_l), (walk_exp_opt_ when_opt)) pew_ll),
            (walk_exp_ body), flags, (walk_ctx_ ctx))
    | ExpTryCatch(e, handlers, ctx) ->
        ExpTryCatch((walk_exp_ e), (walk_handlers_ handlers), (walk_ctx_ ctx))
    | ExpMatch(e, handlers, ctx) ->
        ExpMatch((walk_exp_ e), (walk_handlers_ handlers), (walk_ctx_ ctx))
    | ExpCast(e, t, ctx) -> ExpCast((walk_exp_ e), (walk_typ_ t), (walk_ctx_ ctx))
    | ExpTyped(e, t, ctx) -> ExpTyped((walk_exp_ e), (walk_typ_ t), (walk_ctx_ ctx))
    | ExpCCode(str, ctx) -> ExpCCode(str, (walk_ctx_ ctx))
    | DefVal(p, v, flags, loc) ->
        DefVal((walk_pat_ p), (walk_exp_ v), flags, loc)
    | DefFun(df) ->
        if !df.df_templ_args != [] then e else
        (let { df_name; df_templ_args; df_args; df_typ; df_body;
               df_flags; df_scope; df_loc; df_templ_inst; df_env } = !df in
        df := { df_name; df_templ_args; df_args=(walk_plist_ df_args); df_typ=(walk_typ_ df_typ);
                df_body=(walk_exp_ df_body); df_flags; df_scope; df_loc; df_templ_inst; df_env };
        e)
    | DefExn(de) ->
        let { dexn_name; dexn_typ; dexn_scope; dexn_loc } = !de in
        de := { dexn_name; dexn_typ=(walk_typ_ dexn_typ); dexn_scope; dexn_loc };
        e
    | DefTyp(dt) ->
        let { dt_name; dt_templ_args; dt_typ; dt_finalized; dt_scope; dt_loc } = !dt in
        dt := { dt_name; dt_templ_args; dt_typ=(walk_typ_ dt_typ); dt_finalized; dt_scope; dt_loc };
        e
    | DefVariant(dvar) ->
        let { dvar_name; dvar_templ_args; dvar_alias; dvar_flags; dvar_cases;
               dvar_constr; dvar_templ_inst; dvar_scope; dvar_loc } = !dvar in
        dvar := { dvar_name; dvar_templ_args; dvar_alias=(walk_typ_ dvar_alias); dvar_flags;
                  dvar_cases=(List.map (fun (n, t) -> (n, walk_typ_ t)) dvar_cases);
                  dvar_constr; dvar_templ_inst; dvar_scope; dvar_loc };
        e
    | DefClass(dc) -> (* [TODO] *) e
    | DefInterface(di) -> (* [TODO] *) e
    | DirImport (_, _) -> e
    | DirImportFrom (_, _, _) -> e)

and walk_pat p callb =
    let walk_typ_ t = check_n_walk_typ t callb in
    let walk_pat_ p = check_n_walk_pat p callb in
    let walk_pl_ p = check_n_walk_plist p callb in

    (match p with
    | PatAny _ -> p
    | PatLit _ -> p
    | PatIdent _ -> p
    | PatTuple(pl, loc) -> PatTuple((walk_pl_ pl), loc)
    | PatVariant(n, args, loc) -> PatVariant(n, (walk_pl_ args), loc)
    | PatRec(n_opt, np_l, loc) ->
        PatRec(n_opt, (List.map (fun (n, p) -> (n, (walk_pat_ p))) np_l), loc)
    | PatCons(p1, p2, loc) -> PatCons((walk_pat_ p1), (walk_pat_ p2), loc)
    | PatAs(p, n, loc) -> PatAs((walk_pat_ p), n, loc)
    | PatTyped(p, t, loc) -> PatTyped((walk_pat_ p), (walk_typ_ t), loc))

let rec dup_typ_ t callb =
    match t with
    | TypVar {contents=Some(t1)} ->
        TypVar (ref (Some(dup_typ_ t1 callb)))
    | TypVar _ -> TypVar (ref None)
    | TypRecord r ->
        let (relems, rn) = !r in
        let new_relems = List.map (fun (n, t, v) -> (n, (dup_typ_ t callb), v)) relems in
        TypRecord(ref (new_relems, rn))
    | _ -> walk_typ t callb

let rec dup_exp_ e callb =
    match e with
    | DefFun (r) -> walk_exp (DefFun(ref (!r))) callb
    | DefExn (r) -> walk_exp (DefExn(ref (!r))) callb
    | DefTyp (r) -> walk_exp (DefTyp(ref (!r))) callb
    | DefVariant (r) -> walk_exp (DefVariant(ref (!r))) callb
    | DefClass (r) -> walk_exp (DefClass(ref (!r))) callb
    | DefInterface (r) -> walk_exp (DefInterface(ref (!r))) callb
    | _ -> walk_exp e callb

let dup_callb =
{
    acb_typ = Some(dup_typ_);
    acb_exp = Some(dup_exp_);
    acb_pat = None;
}

let dup_typ t = dup_typ_ t dup_callb
let dup_exp e = dup_exp_ e dup_callb
let dup_pat p = walk_pat p dup_callb

(* shorten type specification by redirecting the references in TypVar
   to the "root" of each connected component tree - a cluster of equivalent/unified types.
   In other words, if we had
   t -> t2 -> t3 ... -> root
   before the call, after the call we will have
   t -> root, t2 -> root, t3 -> root, ...
   Returns the root. *)
let rec deref_typ t =
    match t with
    | TypVar _ ->
        let rec find_root t = match t with
            | TypVar {contents=Some(t2)} -> find_root t2
            | _ -> t in
        let root = find_root t in
        let rec update_refs t =
            match t with
            | TypVar ({contents=Some((TypVar {contents=(Some _)}) as t1)} as r) ->
                r := Some(root); update_refs t1
            | _ -> root
        in update_refs t
    | _ -> t

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
let maybe_unify t1 t2 update_refs =
    (*let _ = (printf "\n<<<trying to unify types: "; pprint_typ_x t1; printf " and "; pprint_typ_x t2; printf "\n"; flush stdout) in*)
    let rec_undo_stack = ref [] in
    let undo_stack = ref [] in
    (* checks if a reference to undefined type (an argument of TypVar (ref None))
       occurs in the other type (t2). If yes, then we have a cyclic dependency
       between types and so they cannot in principle be unified (i.e. no type
       cannot be unified with a part of it).
       In all other cases the undefined type can be unified with t2. *)
    let rec occurs r1 t2 =
        (match t2 with
        | TypFun(args2, rt2) -> List.exists (occurs r1) args2 || occurs r1 rt2
        | TypList(t2_) -> occurs r1 t2_
        | TypTuple(tl2) -> List.exists (occurs r1) tl2
        | TypRef(t2_) -> occurs r1 t2_
        | TypArray(_, et2) -> occurs r1 et2
        | TypRecord {contents=(relems2, _)} ->
            List.exists (fun (_, t, _) -> occurs r1 t) relems2
        | TypVar(r2) when r1 == r2 -> true
        | TypVar({ contents = None }) -> false
        | TypVar({ contents = Some(t2_) }) -> occurs r1 t2_
        | TypApp(tl2, _) -> List.exists (occurs r1) tl2
        | TypInt | TypSInt _ | TypUInt _ | TypFloat _ | TypString | TypChar
        | TypBool | TypVoid | TypExn | TypErr | TypCPointer | TypDecl | TypModule -> false) in
    let rec maybe_unify_ t1 t2 =
        (match (t1, t2) with
        (* a type should be unified successfully with itself *)
        | (TypInt, TypInt) | (TypString, TypString) | (TypChar, TypChar)
        | (TypBool, TypBool) | (TypVoid, TypVoid) | (TypExn, TypExn)
        | (TypCPointer, TypCPointer) | (TypDecl, TypDecl) | (TypModule, TypModule) -> true
        | ((TypSInt bits1), (TypSInt bits2)) -> bits1 = bits2
        | ((TypUInt bits1), (TypUInt bits2)) -> bits1 = bits2
        | ((TypFloat bits1), (TypFloat bits2)) -> bits1 = bits2
        | ((TypFun (args1, rt1)), (TypFun (args2, rt2))) ->
            (List.length args1) = (List.length args2) &&
            (List.for_all2 maybe_unify_ args1 args2) &&
            (maybe_unify_ rt1 rt2)
        | ((TypList et1), (TypList et2)) -> maybe_unify_ et1 et2
        | ((TypTuple tl1), (TypTuple tl2)) ->
            (List.length tl1) = (List.length tl2) &&
            List.for_all2 maybe_unify_ tl1 tl2
        | ((TypRef drt1), (TypRef drt2)) -> maybe_unify_ drt1 drt2
        | (TypRecord r1, TypRecord r2) when r1 == r2 -> true
        | (TypRecord {contents=(_, None)}, TypRecord {contents=(_, Some _)}) -> maybe_unify_ t2 t1
        | (TypRecord r1, TypRecord r2) ->
            let ok = match (!r1, !r2) with
                | ((relems1, Some rn1), (relems2, Some rn2)) ->
                    rn1 = rn2 && (List.for_all2 (fun (n1, t1, _) (n2, t2, _) ->
                        n1 = n2 && maybe_unify_ t1 t2) relems1 relems2)
                | ((relems1, _), (relems2, _)) ->
                    let have_all_matches = List.for_all (fun (n1, t1, v1opt) ->
                        (List.exists (fun (n2, t2, _) -> n1 = n2 && maybe_unify_ t1 t2) relems2)
                        || (Option.is_some v1opt)) relems1 in
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
        | ((TypArray (d1, et1)), (TypArray (d2, et2))) ->
            (d1 < 0 || d2 < 0 || d1 = d2) && (maybe_unify_ et1 et2)
        | ((TypApp (args1, id1)), (TypApp (args2, id2))) ->
            id1 = id2 && (List.for_all2 maybe_unify_ args1 args2)
        (* unify TypVar(_) with another TypVar(_): consider several cases *)
        | ((TypVar r1), (TypVar r2)) when r1 == r2 -> true
        | ((TypVar {contents = Some(t1_)}), _) -> maybe_unify_ t1_ t2
        | (_, (TypVar {contents = Some(t2_)})) -> maybe_unify_ t1 t2_
        | ((TypVar ({contents = None} as r1)), _) ->
            if occurs r1 t2 then false else
            (* This is the destructive unification step:
               if there is unknown type t1 = TypVar(ref None), and it's not
               equivalent to t2, neither is a part of t2, then we unify it with t2.
               Before that, we update the undo stack. There is no need
               to memorize the previous value of r1, because we know that it's None. *)
            ((match t2 with
            | TypErr -> ()
            | _ -> undo_stack := r1 :: !undo_stack; r1 := Some(t2)); true)
          (* symmetrical case *)
        | (_, (TypVar ({contents = None} as r2))) ->
            if occurs r2 t1 then false else
            ((match t1 with
            | TypErr -> ()
            | _ -> undo_stack := r2 :: !undo_stack; r2 := Some(t1)); true)
        (* a declaration cannot be unified with any non-decl type *)
        | (TypErr, _ ) -> true
        | (_, TypErr) -> true
        | (TypDecl, _) | (_, TypDecl) -> false
        (* in all other cases the types cannot be unified *)
        | (_, _) -> false) in

    let ok = maybe_unify_ (deref_typ t1) (deref_typ t2) in
    (*((printf "result (%B): " ok); pprint_typ_x t1; printf " and "; pprint_typ_x t2; printf ">>>\n");*)
    if ok && update_refs then () else
    (* restore the original types in the case of type unification failure
       or when update_refs=false *)
        ((List.iter (fun (r, prev) -> r := prev) !rec_undo_stack);
        (List.iter (fun r -> r := None) !undo_stack));
    ok

(* this is another flavor of type unification function;
   it throws an exception in the case of failure *)
let unify t1 t2 loc msg =
    if maybe_unify t1 t2 true then () else
    raise_compile_err loc msg

let coerce_types t1 t2 allow_tuples allow_fp =
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
        failwith "implicit type coercion for (int, uint32/uint64) pair is not allowed; use explicit type cast"
    | (TypSInt b), TypInt -> if b <= 32 then TypInt else TypSInt(b)
    | (TypUInt b), TypInt -> if b <= safe_max_ubits then TypInt else
        failwith "implicit type coercion for (int, uint32/uint64) pair is not allowed; use explicit type cast"
    | (TypSInt b1), (TypUInt b2) ->
        if b1 <= 32 && b2 <= safe_max_ubits then TypInt else
        failwith "implicit type coercion for this (signed, unsigned) pair of integer is not allowed; use explicit type cast"
    | (TypUInt b1), (TypSInt b2) ->
        if b1 <= safe_max_ubits && b2 <= 32 then TypInt else
        failwith "implicit type coercion for this (unsigned, signed) pair of integer is not allowed; use explicit type cast"
    | ((TypFloat b1), (TypFloat b2)) when allow_fp ->
        let max_b = max (max b1 b2) 32 in TypFloat(max_b)
    | ((TypFloat b), TypInt) when allow_fp -> TypFloat(max b 32)
    | ((TypFloat b), (TypSInt _)) when allow_fp -> TypFloat(max b 32)
    | ((TypFloat b), (TypUInt _)) when allow_fp -> TypFloat(max b 32)
    | (TypInt, (TypFloat b)) when allow_fp -> TypFloat(max b 32)
    | ((TypSInt _), (TypFloat b)) when allow_fp -> TypFloat(max b 32)
    | ((TypUInt _), (TypFloat b)) when allow_fp -> TypFloat(max b 32)
    | ((TypTuple tl1), (TypTuple tl2)) ->
        if not allow_tuples then
            failwith "tuples are not allowed in this operation"
        else if (List.length tl1) = (List.length tl2) then
            TypTuple (List.map2 (fun et1 et2 -> coerce_types_ et1 et2) tl1 tl2)
        else
            failwith "tuples have different number of elements"
    | _ -> failwith "the types cannot be implicitly coerced; use explicit type cast"
    in try Some(coerce_types_ (deref_typ t1) (deref_typ t2)) with Failure _ -> None

let find_all n env =
    match Env.find_opt n env with
    | Some(l) -> l
    | _ -> []

let typ2constr t rt =
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
            raise_compile_err loc (sprintf "duplicate record field '%s'" (id2str ni)))
        rfnames) rfnames

let finalize_record_typ rn t loc =
    match (deref_typ t) with
    | TypRecord ({contents=(relems, None)} as r) ->
        r := (relems, rn);
        List.iter (fun (n, t, v_opt) ->
            match v_opt with
            | Some(v) -> unify t (get_lit_typ v) loc
                (sprintf "type of the field '%s' and its initializer do not match" (pp_id2str n))
            | _ -> ()) relems;
        TypRecord(r)
    | t -> t

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
    raise_compile_err loc (sprintf "the appropriate match for %s is not found" (pp_id2str n))

let rec find_first n env loc pred =
    let rec find_next_ elist =
        match elist with
        | e :: rest ->
            (match (pred e) with
            | Some x -> Some x
            | _ -> find_next_ rest)
        | _ -> None in
    let rec search_path n_path env = (match n_path with
        | [] -> []
        | last_component :: [] -> find_all (get_id last_component) env
        | prefix :: rest ->
            match find_all (get_id prefix) env with
            | EnvId m :: _ ->
                (match (id_info m) with
                | IdModule {contents={dm_env}} -> search_path rest dm_env
                | _ -> [])
            | _ -> []) in
    let e_all = find_all n env in
    let e_all = if e_all = [] && (is_unique_id n) then
        (EnvId n) :: [] else e_all in
    let e_all = if e_all != [] then e_all else
            (let n_path = String.split_on_char '.' (pp_id2str n) in
            if (List.length n_path) <= 1 then []
            else search_path n_path env)
    in find_next_ e_all

let rec lookup_id n t env sc loc =
    match (find_first n env loc (fun e ->
        match e with
        | EnvId(Id.Name _) -> None
        | EnvId i ->
            (match id_info i with
            | IdVal {dv_typ} -> unify dv_typ t loc "incorrect value type"; Some(i)
            | IdFun ({contents={ df_templ_args; df_typ; df_templ_inst; df_env }} as df) ->
                if df_templ_args = [] then
                    if maybe_unify df_typ t true then
                        Some(i)
                    else
                        None
                else
                    let (ftyp, env1) = preprocess_templ_typ df_templ_args df_typ df_env sc loc in
                    if maybe_unify ftyp t true then
                        try
                            Some(List.find (fun inst ->
                            match id_info inst with
                            | IdFun {contents={df_typ=inst_typ}} ->
                                maybe_unify inst_typ t true
                            | _ -> raise_compile_err loc (sprintf "invalid (non-function) instance %s of template function %s" (id2str inst) (pp_id2str n))
                            ) df_templ_inst)
                        with Not_found ->
                            (* the generic function matches the requested type,
                               but there is no appropriate instance;
                               let's create a new one *)
                            let { df_name=inst_name; df_typ=inst_typ } = !(instantiate_fun df ftyp env1 sc loc) in
                            Some(inst_name)
                    else
                        None
            | IdModule _ -> unify TypModule t loc "unexpected module name"; Some(i)
            | IdExn {contents={ dexn_typ }} ->
                let ctyp = typ2constr dexn_typ TypExn in
                unify ctyp t loc "uncorrect type of exception constructor and/or its arguments";
                Some(i)
            | IdNone | IdTyp _ | IdVariant _
            | IdClass _ | IdInterface _ -> None)
        | EnvTyp _ -> None)) with
    | Some(x) -> x
    | None -> report_not_found n loc

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
            let t = typ2constr dexn_typ TypExn in
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
        | (TypTuple(tl) :: [], 1, n) when (List.length tl) = n -> tl
        | (_, _, 1) -> TypTuple(actual_ty_args) :: []
        | _ -> raise_compile_err inst_loc
        (sprintf "the number of actual type parameters and formal type parameters, as declared at\n\t%s,\n\tdo not match"
        (loc2str def_loc))) in
    List.fold_left2 (fun env n t -> add_typ_to_env n t env) env templ_args norm_ty_args

(*
    One of the main functions in type checker:
    inferes the proper type of expression and recursively process sub-expressions.

    Whenever possible, we try to do type unification before processing sub-expressions.
    This is because sometimes we know type of the outer expression (type of the expression result)
    and using this information we can possibly deduce the types of arguments.
    For example, in `5 :: []` operation we can figure out that the second argument has `int list` type,
    we do not have to write more explicit `5 :: ([]: int list)` expression.
    Or, in the case of some hypothetical "fun integrate(a: 't, b: 't, f: 't->'t)" we can just call it
    with `integrate(0.f, 10.f, Math.sin)` instead of more explicit `integrate(0.f, 10.f, (Math.sin: float->float))`.Bigarray

    Also, note that the function does not modify the environment. When a complex expression
    is analyzed (such as for loop), a temporary environment can be created with extra content
    (like iteration variables in the case of for loop), but that temporary environment
    is discared as soon the analysis is over.

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
        let (etyp, eloc) = get_exp_ctx e in
        let (ptyp, dims) = (match (e, (deref_typ etyp)) with
            | (ExpRange(_, _, _, _), _) -> (TypInt, 1)
            | (_, TypArray(d, et)) -> (et, d)
            | (_, TypList(et)) -> (et, 1)
            | (_, TypString) -> (TypChar, 1)
            | _ -> raise_compile_err eloc "unsupported iteration domain; it should be a range, array, list or a string") in
        let (p, env, idset, _) = check_pat p ptyp env idset IdSet.empty sc false true false in
        (p, e, dims, env, idset) in
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
        let n = lookup_id n etyp env sc eloc in
        ExpIdent(n, ctx)
    | ExpMem(e1, e2, _) ->
        (* in the case of '.' operation we do not check e2 immediately after e1,
           because e2 is a 'member' of a structure/module e1,
           so it cannot be independently analyzed *)
        let new_e1 = check_exp e1 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        (match ((deref_typ etyp1), new_e1, e2) with
        | (TypModule, ExpIdent(n1, _), ExpIdent(n2, (etyp2, eloc2))) ->
            let n1_env = get_module_env n1 in
            (*let _ = printf "looking for %s in module %s ...\n" (id2str n2) (id2str n1) in*)
            let new_n2 = lookup_id n2 etyp n1_env sc eloc in
            ExpIdent(new_n2, ctx)
        | ((TypTuple tl), _, ExpLit((LitInt idx), (etyp2, eloc2))) ->
            unify etyp2 TypInt eloc2 "index must be int!";
            (* we do not handle negative indices, because the parser would not allow that;
               ok, if it's wrong assumption, an exception will be thrown
               (and be catched at the higher level) anyway *)
            let et = (try List.nth tl (Int64.to_int idx) with Failure _ ->
                raise_compile_err eloc2 "the tuple index is out of range") in
            unify etyp et eloc "incorrect type of the tuple element";
            ExpMem(new_e1, e2, ctx)
        | (TypRecord {contents=(relems, _)}, _, ExpIdent(n2, (etyp2, eloc2))) ->
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
        let rec is_lvalue e = (match e with
            | ExpAt _ -> true (* an_arr[idx] = e2 *)
            | ExpDeref(_, _) -> true (* *a_ref = e2 *)
            | ExpIdent(n1, _) -> (* a_var = e2 *)
                (match (id_info n1) with
                | IdVal { dv_flags } -> (List.mem ValMutable dv_flags)
                | _ -> false)
            | ExpMem(rcrd, ExpIdent(_, _), _) -> is_lvalue rcrd
            | ExpMem(tup, ExpLit(_, _), _) -> is_lvalue tup
            | _ -> false) in
        if not (is_lvalue new_e1) then raise_compile_err eloc "the left side of assignment is not an l-value"
        else ();
        ExpAssign(new_e1, new_e2, eloc)

    | ExpBinOp(OpCons, e1, e2, _) ->
        let (etyp1, eloc1) = get_exp_ctx e1 in
        let (etyp2, eloc2) = get_exp_ctx e2 in
        let _ = unify etyp (TypList etyp1) eloc1 "'::' operation should produce a list" in
        let _ = unify etyp etyp2 eloc2 "incorrect type of the second argument of '::' operation" in
        let new_e1 = check_exp e1 env sc in
        let new_e2 = check_exp e2 env sc in
        ExpBinOp(OpCons, new_e1, new_e2, ctx)

    | ExpBinOp(bop, e1, e2, _) ->
        let new_e1 = check_exp e1 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        let etyp1_ = deref_typ etyp1 in
        let new_e2 = check_exp e2 env sc in
        let (etyp2, eloc2) = get_exp_ctx new_e2 in
        let etyp2_ = deref_typ etyp2 in

        (* depending on the operation, figure out the type of result
           (or set it to None if there is no embedded implementation)
           and also do some other op-specific checks *)
        let typ_opt =
        (match bop with
        | OpAdd | OpSub | OpMul | OpDiv | OpMod | OpPow | OpShiftLeft | OpShiftRight ->
            let allow_fp = bop != OpShiftLeft && bop != OpShiftRight in
            coerce_types etyp1_ etyp2_ true allow_fp
        | OpBitwiseAnd | OpBitwiseOr | OpBitwiseXor ->
            let rec check_bitwise t1 t2 =
                (match (t1, t2) with
                | (TypInt, TypInt) -> TypInt
                | (TypSInt(b1), TypSInt(b2)) when b1 = b2 -> TypSInt(b1)
                | (TypUInt(b1), TypUInt(b2)) when b1 = b2 -> TypUInt(b1)
                | (TypBool, TypBool) -> TypBool
                | (TypTuple tl1), (TypTuple tl2) ->
                    if (List.length tl1) != (List.length tl2) then
                        invalid_arg ""
                    else
                        TypTuple(List.map2 check_bitwise tl1 tl2)
                | _ -> invalid_arg "")
            in (try Some(check_bitwise etyp1_ etyp2_) with Failure _ -> None)
        | OpLogicAnd | OpLogicOr ->
            unify etyp1 TypBool eloc1 "arguments of logical operation must be boolean";
            unify etyp2 TypBool eloc2 "arguments of logical operation must be boolean";
            Some(TypBool)

            (* [TODO] comparison operations should be autogenerated for any pair of equal types:
            tuples and records must be compared lexicographically
            (where record fields are ordered by the declaration order, not by names),
            lists, strings and arrays - lexicographically as well,
            variants - using the tag precedence
            maybe references and cpointers - by address,
            maybe classes (lexicographically, member by member)
            *)
        | OpCompareEQ | OpCompareNE | OpCompareLT | OpCompareLE | OpCompareGT | OpCompareGE ->
            unify etyp1 etyp2 eloc "only equal types can be compared";
            (match (deref_typ etyp1) with
            | TypInt | TypSInt _ | TypUInt _ | TypFloat _ | TypBool -> Some(TypBool)
            | _ -> None)
        | _ -> raise_compile_err eloc (sprintf "unsupported binary operation %s" (binop_to_string bop))) in

        (match typ_opt with
        | Some(typ) ->
            unify typ etyp eloc "improper type of the arithmetic operation result";
            ExpBinOp(bop, new_e1, new_e2, ctx)
        | _ ->
            (* try to find an overloaded function that will handle such operation with combination of types, e.g.
               operator + (p: point, q: point) = point { p.x + q.x, p.y + q.y } *)
            let f_id = get_binop_fname bop in
            check_exp (ExpCall (ExpIdent(f_id, (make_new_typ(), eloc)), [new_e1; new_e2], ctx)) env sc)

    | ExpDeref(e1, _) ->
        let (etyp1, eloc1) = get_exp_ctx e1 in
        let _ = unify (TypRef etyp) etyp1 eloc "the types of unary '*' operation argument and result are inconsistent" in
        let new_e1 = check_exp e1 env sc in
        ExpDeref(new_e1, ctx)

    | ExpMakeRef(e1, _) ->
        let (etyp1, eloc1) = get_exp_ctx e1 in
        let _ = unify etyp (TypRef etyp1) eloc "the types of ref() operation argument and result are inconsistent" in
        let new_e1 = check_exp e1 env sc in
        ExpMakeRef(new_e1, ctx)

    | ExpThrow(e1, _) ->
        let (etyp1, eloc1) = get_exp_ctx e1 in
        let _ = unify TypExn etyp1 eloc "the argument of 'throw' operator must be an exception" in
        let new_e1 = check_exp e1 env sc in
        ExpThrow(new_e1, eloc)

    | ExpUnOp(uop, e1, _) ->
        let new_e1 = check_exp e1 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        (match uop with
        | OpNegate | OpPlus ->
            let t_opt = coerce_types etyp1 etyp1 true true in
            (match t_opt with
            | Some(t) ->
                unify etyp t eloc "improper type of the unary '-' operator result";
                ExpUnOp(uop, new_e1, ctx)
            | None ->
                let f_id = get_unop_fname uop in
                check_exp (ExpCall (ExpIdent(f_id, (make_new_typ(), eloc)), [new_e1], ctx)) env sc)
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
                ExpUnOp(uop, new_e1, ctx)
            with Failure _ ->
                let f_id = get_unop_fname uop in
                check_exp (ExpCall (ExpIdent(f_id, (make_new_typ(), eloc)), [new_e1], ctx)) env sc)
        | OpLogicNot ->
            unify etyp1 TypBool eloc1 "the argument of ! operator must be a boolean";
            unify etyp TypBool eloc "the result of ! operator must be a boolean";
            ExpUnOp(uop, new_e1, ctx)
        | OpExpand ->
            raise_compile_err eloc "the expand (\\...) operation is not implemented yet")
    | ExpSeq(eseq, _) ->
        let eseq_typ = get_eseq_typ eseq in
        let _ = unify etyp eseq_typ eloc "the expected type of block expression does not match its actual type" in
        let (eseq, _) = check_eseq eseq env sc true in
        ExpSeq(eseq, ctx)
    | ExpMkTuple(el, _) ->
        let tl = List.map (fun e -> get_exp_typ e) el in
        let _ = unify etyp (TypTuple tl) eloc "improper type of tuple elements or the number of elements" in
        ExpMkTuple((List.map (fun e -> check_exp e env sc) el), ctx)
    | ExpCall(f, args, _) ->
        (* [TODO] implement more sophisticated algorithm; try to look for possible overloaded functions first;
           if there is just one obvious match, take it first and use to figure out the argument types *)
        let arg_typs = List.map (fun a -> get_exp_typ a) args in
        let f_expected_typ = TypFun(arg_typs, etyp) in
        let (f_real_typ, floc) = get_exp_ctx f in
        let _ = unify f_real_typ f_expected_typ floc "the real and expected function type do not match" in
        let new_args = List.map (fun a -> check_exp a env sc) args in
        let new_f = check_exp f env sc in
        ExpCall(new_f, new_args, ctx)
    | ExpAt(arr, idxs, _) ->
        let new_arr = check_exp arr env sc in
        let (new_atyp, new_aloc) = get_exp_ctx new_arr in
        let et = make_new_typ() in
        (match idxs with
        (* flatten case "arr[:]" *)
        | ExpRange(None, None, None, _) :: [] ->
            unify new_atyp (TypArray(-1, et)) new_aloc "the argument of the flatten operation must be an array";
            unify etyp (TypArray(1, et)) eloc "the result of the flatten operation must be 1D array";
            ExpAt(new_arr, idxs, ctx)
        (* other cases: check each index, it should be either a scalar or a range;
           in the first case it should have integer type.
           If all the indices are scalars, then the result should have et type,
           otherwise it's an array of as high dimensionality as the number of range indices *)
        | _ ->
            let (new_idxs, nidx, nrange_idx) = List.fold_left (fun (new_idxs, nidx, nrange_idx) idx ->
                let new_idx = check_exp idx env sc in
                match new_idx with
                | ExpRange(_, _, _, _) -> (new_idx :: new_idxs, nidx+1, nrange_idx+1)
                | _ ->
                    let (new_ityp, new_iloc) = get_exp_ctx new_idx in
                    unify new_ityp TypInt new_iloc "each scalar index in array access op must be an integer";
                    (new_idx :: new_idxs, nidx+1, nrange_idx)) ([], 0, 0) idxs in
            unify new_atyp (TypArray(nidx, et)) new_aloc "the array dimensionality does not match the number of indices";
            (if nrange_idx = 0 then
                unify etyp et eloc "the type of array access expression does not match the array element type"
            else
                unify etyp (TypArray(nrange_idx, et)) eloc
                  "the number of ranges does not match dimensionality of the result, or the element type is incorrect");
            ExpAt(new_arr, (List.rev new_idxs), ctx))
    | ExpIf(c, e1, e2, _) ->
        let (ctyp, cloc) = get_exp_ctx c in
        let (typ1, loc1) = get_exp_ctx e1 in
        let (typ2, loc2) = get_exp_ctx e2 in
        let _ = unify ctyp TypBool cloc "if() condition should have 'bool' type" in
        let _ = unify typ1 typ2 loc2 "if() then- and else- branches should have the same type" in
        let _ = unify typ1 etyp eloc "if() expression should have the same type as its branches" in
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
        let new_body = check_exp body env sc in
        ExpWhile (new_c, new_body, eloc)
    | ExpDoWhile(c, body, _) ->
        let (ctyp, cloc) = get_exp_ctx c in
        let (btyp, bloc) = get_exp_ctx body in
        let _ = unify ctyp TypBool cloc "do-while() loop condition should have 'bool' type" in
        let _ = unify btyp TypVoid bloc "do-while() loop body should have 'void' type" in
        let new_c = check_exp c env sc in
        let new_body = check_exp body env sc in
        ExpDoWhile (new_c, new_body, eloc)
    | ExpFor(for_clauses, body, flags, _) ->
        let for_sc = new_block_scope() :: sc in
        let (for_clauses1, _, env1, _) = List.fold_left
            (fun (for_clauses1, dims1, env1, idset1) (pi, ei) ->
                let (pi, ei, dims, env1, idset1) = check_for_clause pi ei env1 idset1 for_sc in
                if dims1 != 0 && dims1 != dims then
                    raise_compile_err (get_exp_loc ei) "the dimensionalities of simultaneously iterated containers/ranges do not match"
                else ();
                ((pi, ei) :: for_clauses1, dims, env1, idset1)) ([], 0, env, IdSet.empty) for_clauses in
        let (btyp, bloc) = get_exp_ctx body in
        let _ = unify btyp TypVoid bloc "'for()' body should have 'void' type" in
        let new_body = check_exp body env1 for_sc in
        ExpFor((List.rev for_clauses1), new_body, flags, eloc)
    | ExpMap (map_clauses, body, flags, ctx) ->
        let make_list = List.mem ForMakeList flags in
        let for_sc = new_block_scope() :: sc in
        let (map_clauses1, total_dims, env1, _) = List.fold_left
            (fun (map_clauses1, total_dims, env1, idset1) (for_clauses, opt_c) ->
                let (for_clauses1, dims1, env1, idset1) = List.fold_left
                (fun (for_clauses1, dims1, env1, idset1) (pi, ei) ->
                    let (pi, ei, dims, env1, idset1) = check_for_clause pi ei env1 idset1 for_sc in
                    if dims1 != 0 && dims1 != dims then
                        raise_compile_err (get_exp_loc ei)
                            "the dimensionalities of simultaneously iterated containers/ranges do not match"
                    else ();
                ((pi, ei) :: for_clauses1, dims, env1, idset1)) ([], 0, env1, IdSet.empty) for_clauses in
                let new_opt_c = (match opt_c with
                    | Some(c) ->
                        let new_c = check_exp c env1 for_sc in
                        let (new_ctyp, new_cloc) = get_exp_ctx new_c in
                        unify new_ctyp TypBool new_cloc "the conditional clause in map must have 'bool' type";
                        Some(new_c)
                    | _ -> None) in
                (((List.rev for_clauses1), new_opt_c) :: map_clauses1,
                total_dims + dims1, env1, idset1)) ([], 0, env, IdSet.empty) map_clauses in
        let (btyp, bloc) = get_exp_ctx body in
        let _ = if make_list then
                (if total_dims = 1 then () else raise_compile_err eloc "the list comprehension should be 1-dimensional";
                unify etyp (TypList btyp) bloc "the map should return a list with elements of the same type as the map body")
            else
                unify etyp (TypArray(total_dims, btyp)) bloc
                (sprintf "the map should return %d-dimensional array with elements of the same type as the map body" total_dims) in
        let new_body = check_exp body env1 for_sc in
        ExpMap((List.rev map_clauses1), new_body, flags, ctx)
    | ExpBreak _ -> e
    | ExpContinue _ -> e
    | ExpMkArray (arows, _) ->
        (* [TODO] support extended syntax of array initialization *)
        let dims = if (List.length arows) > 1 then 2 else 1 in
        let elemtyp = make_new_typ() in
        let atyp = TypArray(dims, elemtyp) in
        let _ = unify atyp etyp eloc "the array literal should produce an array" in
        let arows = List.map (fun arow ->
            List.map (fun elem ->
                let (elemtyp1, eloc1) = get_exp_ctx elem in
                let _ = unify elemtyp elemtyp1 eloc1 "all the array literal elements should have the same type" in
                check_exp elem env sc) arow) arows in
        ExpMkArray(arows, ctx)
    | ExpMkRecord (r_e, r_initializers, _) ->
        let _ = check_for_rec_field_duplicates (List.map (fun (n, _) -> n) r_initializers) eloc in
        let (r_new_initializers, relems) = List.fold_left (fun (r_new_initializers, relems) (n, e) ->
            let e = check_exp e env sc in
            let etyp = get_exp_typ e in
            ((n, e) :: r_new_initializers, (n, etyp, (None : lit_t option)) :: relems)) ([], []) r_initializers in
        let rtyp = TypRecord(ref ((List.rev relems), None)) in
        let (r_etyp, r_eloc) = get_exp_ctx r_e in
        let r_expected_typ = TypFun(rtyp :: [], etyp) in
        let _ = unify r_etyp r_expected_typ r_eloc "there is no proper record constructor/function with record argument" in
        let new_r_e = check_exp r_e env sc in
        ExpMkRecord(new_r_e, (List.rev r_new_initializers), ctx)
    | ExpUpdateRecord (r_e, r_initializers, _) ->
        let _ = check_for_rec_field_duplicates (List.map (fun (n, _) -> n) r_initializers) eloc in
        let (rtyp, rloc) = get_exp_ctx r_e in
        let _ = unify rtyp etyp eloc "the types of the update-record argument and the result do not match" in
        let new_r_e = check_exp r_e env sc in
        let relems = (match (deref_typ rtyp) with
            | TypRecord {contents=(relems, rname_opt)} ->
                (match rname_opt with
                | Some _ -> ()
                | _ -> raise_compile_err rloc "the updated record is not completely defined. Use explicit type specification");
                relems
            | _ -> raise_compile_err rloc "the update-record operation argument is not a record") in
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
    | ExpTryCatch(e1, handlers, _) ->
        let (e1typ, e1loc) = get_exp_ctx e1 in
        let _ = unify etyp e1typ e1loc "try body type does match the whole try-catch type" in
        let new_e1 = check_exp e1 env sc in
        let new_handlers = check_handlers handlers TypExn etyp env sc eloc in
        ExpTryCatch(new_e1, new_handlers, ctx)
    | ExpMatch(e1, handlers, _) ->
        let new_e1 = check_exp e1 env sc in
        let new_e1typ = get_exp_typ new_e1 in
        let new_handlers = check_handlers handlers new_e1typ etyp env sc eloc in
        ExpMatch(new_e1, new_handlers, ctx)
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
    | ExpCCode(_, _) ->
        (match sc with
        | ScModule(_) :: _ -> ()
        | ScFun(_) :: _ -> ()
        | _ -> raise_compile_err eloc
            "ccode may be used only at the top (module level) or as a single expression in function definition");
        e
    (* all the declarations are checked in check_eseq *)
    | DefVal(_, _, _, _)
    | DefFun(_)
    | DefVariant(_)
    | DefClass(_)
    | DefInterface(_)
    | DefExn(_)
    | DefTyp(_)
    | DirImport(_, _)
    | DirImportFrom(_, _, _) ->
        raise_compile_err eloc "internal err: should not get here; all the declarations must be handled in check_eseq") in
    (*let _ = match new_e with ExpSeq _ -> () | _ -> (printf "\ninferenced type: \n";
        (pprint_typ_x etyp); printf "\n========\n") in*)
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
    let (eseq, env) = List.fold_left (fun (eseq, env) e ->
        try
            match e with
            | DirImport(_, _) | DirImportFrom(_, _, _)
            | DefTyp _ | DefVariant _ | DefClass _ | DefInterface _ | DefExn _ -> (e :: eseq, env)
            | DefVal (p, e, flags, loc) ->
                let is_mutable = List.mem ValMutable flags in
                let t = get_exp_typ e in
                let e1 = check_exp e env sc in
                let (p1, env1, _, _) = check_pat p t env IdSet.empty IdSet.empty sc false true is_mutable in
                (DefVal(p1, e1, flags, loc) :: eseq, env1)
            | DefFun (df) ->
                let _ = if !df.df_templ_args = [] then
                        (check_deffun df env)
                    else
                        (* update environment of the template function
                           to give it access to the above defined
                           non-template values *)
                        ((!df.df_env <- env); df) in
                (e :: eseq, env)
            | _ ->
                let e = check_exp e env sc in
                (e :: eseq, env)
        with CompileError(_, _) as err -> raise err(*; (e :: eseq, env)*))
        ([], env) eseq in

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
                match sc with
                | (ScModule(m) :: _) when parent_mod = noid || parent_mod = m ->
                    add_id_to_env key i env
                | _ -> env)
            | EnvTyp _ -> env) env (List.rev entries)) in

    let import_mod env alias m allow_duplicate_import loc =
        (if is_imported alias m env allow_duplicate_import loc then env
        else
            (* add the imported module id to the env *)
            let env = add_id_to_env alias m env in
            let menv = !(get_module m).dm_env in
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
                    import_mod env alias m false eloc
                with CompileError(_, _) -> env) env impdirs), mlist)
        | DirImportFrom(m, implist, eloc) ->
            let env =
            (try
                let menv = !(get_module m).dm_env in
                let keys = if implist != [] then implist else
                    (Env.fold (fun k ids l -> k :: l) menv []) in
                (*let _ = ((printf "imported from %s: " (id2str m)); (List.iter (fun k -> printf "%s, " (id2str k)) keys); (printf "\n")) in*)
                List.fold_left (fun env k ->
                    try
                        let entries = find_all k menv in
                        let _ = (if entries != [] then () else
                            raise_compile_err eloc
                                (sprintf "no symbol %s found in %s" (pp_id2str k) (pp_id2str m))) in
                        import_entries env m k entries eloc
                    with CompileError(_, _) -> env) env keys
            with CompileError(_, _) -> env) in
            (env, (m, eloc) :: mlist)
        | _ -> (env, mlist)) (env, []) eseq) in

    (* after processing explicit (e.g. specified by user) import directives,
       we also implicitly import each module "m" for which we have "from m import ...".
       Probably, this needs to be revised *)
    let env = List.fold_left (fun env (m, eloc) ->
        let alias = get_orig_id m in
        import_mod env alias m true eloc) env mlist in

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
            let dummy_constr = List.map (fun (n, _) -> n) dvar_cases in
            dvar := {dvar_name=dvar_name1; dvar_templ_args; dvar_alias=dvar_alias1; dvar_flags; dvar_cases;
                    dvar_constr=dummy_constr; dvar_templ_inst=[]; dvar_scope=sc; dvar_loc};
            set_id_entry dvar_name1 (IdVariant dvar);
            add_id_to_env_check dvar_name dvar_name1 env (check_for_duplicate_typ dvar_name sc dvar_loc)
        | DefClass {contents={dcl_loc=loc}} -> raise_compile_err loc "classes are not supported yet"
        | DefInterface {contents={di_loc=loc}} -> raise_compile_err loc "interfaces are not supported yet"
        | _ -> env) env eseq

and register_typ_constructor n templ_args arg_t rt env sc decl_loc =
    let (argtyps, ctyp) = match arg_t with
              TypVoid -> ([], rt)
            | TypTuple(tl) -> (tl, TypFun(tl, rt))
            | _ -> let argtyps = arg_t :: [] in (argtyps, TypFun(argtyps, rt)) in
    let args = List.mapi (fun i _ -> PatIdent((get_id ("arg" ^ (Int.to_string i))), decl_loc)) argtyps in
    let cname = dup_id n in
    let df = ref { df_name=cname; df_templ_args=templ_args;
            df_args=args; df_typ=ctyp;
            df_body=(ExpNop decl_loc); df_flags=FunConstr :: [];
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
            let dt_typ = finalize_record_typ (Some dt_name) dt_typ dt_loc in
            dt := {dt_name; dt_templ_args; dt_typ=dt_typ; dt_finalized=true; dt_scope; dt_loc};
            (* in the case of record we add the record constructor function
               to support the constructions 'record_name { f1=e1, f2=e2, ..., fn=en }' gracefully *)
            (match dt_typ with
            | TypRecord _ ->
                let (cname, ctyp) = register_typ_constructor dt_name dt_templ_args dt_typ dt_typ env1 dt_scope dt_loc in
                add_id_to_env_check (get_orig_id dt_name) cname env (check_for_duplicate_fun ctyp env dt_scope dt_loc)
            | _ -> env)
        | DefVariant dvar ->
            let _ = instantiate_variant [] dvar env sc (!dvar.dvar_loc) in
            let { dvar_name; dvar_templ_args; dvar_cases; dvar_constr; dvar_scope; dvar_loc } = !dvar in
            List.fold_left2 (fun env (n, t) cname ->
                let {df_name; df_templ_args; df_typ} = (match (id_info cname) with
                    | IdFun df -> !df
                    | _ -> raise_compile_err dvar_loc (sprintf "internal error: constructor %s is not a function" (id2str cname))) in
                let (t, _) = preprocess_templ_typ df_templ_args df_typ env sc dvar_loc in
                add_id_to_env_check n cname env (check_for_duplicate_fun t env sc dvar_loc)) env dvar_cases dvar_constr
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
    let rt = (match df_typ with TypFun(_, rt) -> rt | _ -> raise_compile_err df_loc "incorrect function type") in
    let df_sc = (ScFun df_name1) :: sc in
    let (args1, argtyps1, env1, idset1, templ_args1) = List.fold_left (fun (args1, argtyps1, env1, idset1, templ_args1) arg ->
            let t = make_new_typ() in
            let (arg1, env1, idset1, templ_args1) = check_pat arg t env1 idset1 templ_args1 df_sc true true false in
            ((arg1 :: args1), (t :: argtyps1), env1, idset1, templ_args1)) ([], [], env, IdSet.empty, IdSet.empty) df_args in
    let dummy_rt_pat = PatTyped(PatAny(df_loc), rt, df_loc) in
    let (dummy_rt_pat1, env1, idset1, templ_args1) = check_pat dummy_rt_pat (make_new_typ()) env1 idset1 templ_args1 df_sc true true false in
    let rt = match dummy_rt_pat1 with PatTyped(_, rt, _) -> rt | _ -> raise_compile_err df_loc "invalid return pattern after check" in
    let df_typ1 = TypFun((List.rev argtyps1), rt) in
    let env1 = add_id_to_env_check df_name df_name1 env1 (check_for_duplicate_fun df_typ1 env1 sc df_loc) in
    df := { df_name=df_name1; df_templ_args=(IdSet.elements templ_args1);
            df_args=(List.rev args1); df_typ=df_typ1;
            df_body; df_flags; df_scope=sc; df_loc; df_templ_inst=[]; df_env=env1 };
    (*let _ = (printf "\tfun after registration: "; pprint_exp_x (DefFun df); printf "\n~~~~~~~~~~~~~~~~\n") in*)
    set_id_entry df_name1 (IdFun df);
    env1

and check_defexn de env sc =
    let { dexn_name=alias; dexn_typ=t; dexn_loc=loc } = !de in
    let t = check_typ t env sc loc in
    let n = dup_id alias in
    let ftyp = typ2constr t TypExn in
    de := { dexn_name=n; dexn_typ=t; dexn_scope=sc; dexn_loc=loc };
    set_id_entry n (IdExn de);
    add_id_to_env_check alias n env (check_for_duplicate_fun ftyp env sc loc)

and check_deffun df env =
    let { df_typ; df_scope; df_loc } = !df in
    instantiate_fun df df_typ env df_scope df_loc

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
                            Some(check_typ dt_typ env1 sc loc)
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
        | TypRecord {contents=(relems, _)} ->
            check_for_rec_field_duplicates (List.map (fun (n, _, _) -> n) relems) loc;
            walk_typ t callb
        | _ -> walk_typ t callb) in
    let callb = { acb_typ=Some(check_typ_); acb_exp=None; acb_pat=None } in
    (check_typ_ t callb, !r_env)

and check_typ t env sc loc =
    let (t, _) = check_typ_and_collect_typ_vars t env None sc loc in t

and instantiate_fun templ_df inst_ftyp inst_env0 inst_sc inst_loc =
    let { df_name; df_templ_args; df_args; df_body; df_flags; df_scope; df_loc; df_templ_inst } = !templ_df in
    let inst_env = inst_env0 in
    let is_constr = List.mem FunConstr df_flags in
    (*let _ = (printf "before instantiation of %s %s with type<" (if is_constr then
        "constructor" else "function") (id2str df_name); pprint_typ_x inst_ftyp; printf ">:\n") in
    let _ = print_env "" inst_env inst_loc in*)
    let instantiate = inst_loc != df_loc in
    let (arg_typs, rt) = (match (deref_typ inst_ftyp) with
                    | TypFun(arg_typs, rt) -> (arg_typs, rt)
                    | rt -> if is_constr && df_args = [] then ([], rt)
                        else raise_compile_err inst_loc
                        "internal error: the type of instantiated function is not a function") in
    let inst_name = if instantiate then (dup_id df_name) else df_name in
    let fun_sc = (ScFun inst_name) :: inst_sc in
    let (df_inst_args, inst_env, _) = List.fold_left2
        (fun (df_inst_args, inst_env, idset) df_arg arg_typ ->
            let (df_inst_arg, inst_env, idset, _) = check_pat df_arg arg_typ inst_env idset IdSet.empty fun_sc false true false in
            ((df_inst_arg :: df_inst_args), inst_env, idset)) ([], inst_env, IdSet.empty) df_args arg_typs in
    let df_inst_args = List.rev df_inst_args in
    let rt = check_typ rt inst_env df_scope inst_loc in
    let inst_body = if instantiate then (dup_exp df_body) else df_body in
    (*let _ = ((printf "processing function %s " (id2str inst_name)); pprint_pat_x (PatTuple(df_inst_args, noloc)); printf "<";
            List.iteri (fun i n -> printf "%s%s" (if i=0 then "" else ", ") (id2str n)) df_templ_args; printf ">\n") in
    let _ = (printf "\tof typ: "; pprint_typ_x inst_ftyp; printf "\n") in
    let _ = (printf "\tfun before type checking: "; pprint_exp_x (DefFun templ_df); printf "\n~~~~~~~~~~~~~~~~\n") in
    let _ = print_env (sprintf "before processing function body of %s defined at %s" (id2str inst_name) (loc2str df_loc)) inst_env inst_loc in*)
    let (body_typ, body_loc) = get_exp_ctx inst_body in
    let _ = if is_constr then () else unify body_typ rt body_loc "the function body type does not match the function type" in
    let inst_df = ref { df_name=inst_name; df_templ_args=[]; df_args=df_inst_args;
                        df_typ=(deref_typ inst_ftyp); df_body=inst_body;
                        df_flags; df_scope=inst_sc; df_loc=inst_loc; df_templ_inst=[]; df_env=inst_env0 } in
    let _ = set_id_entry inst_name (IdFun inst_df) in
    let inst_or_templ_df = if instantiate then
        (!templ_df.df_templ_inst <- inst_name :: !templ_df.df_templ_inst; inst_df)
    else
        (templ_df := !inst_df; templ_df) in
    let inst_body = check_exp inst_body inst_env fun_sc in
    !inst_or_templ_df.df_body <- inst_body;
    (*(printf "<<<processed function:\n"; (pprint_exp_x (DefFun inst_or_templ_df)); printf "\n>>>\n");*)
    inst_or_templ_df

and instantiate_variant ty_args dvar env sc loc =
    let { dvar_name; dvar_templ_args; dvar_alias; dvar_flags; dvar_cases;
          dvar_constr; dvar_scope; dvar_loc } = !dvar in
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
                dvar_alias=inst_app_typ; dvar_flags; dvar_cases; dvar_constr;
                dvar_templ_inst=[]; dvar_scope; dvar_loc=loc }) in
    (* register the incomplete-yet variant in order to avoid inifinite loop *)
    let _ = if instantiate then
        (set_id_entry inst_name (IdVariant inst_dvar);
        !dvar.dvar_templ_inst <- inst_name :: !dvar.dvar_templ_inst) else () in
    let (inst_cases, inst_constr) = List.fold_left2 (fun (inst_cases, inst_constr) (n, t) cname ->
        let t = check_typ t env sc loc in
        let t = finalize_record_typ (Some cname) t loc in
        let (inst_cname, _) = register_typ_constructor cname (!inst_dvar.dvar_templ_args) t inst_app_typ env dvar_scope dvar_loc in
        if instantiate then
            match id_info cname with
            | IdFun c_def -> !c_def.df_templ_inst <- inst_cname :: !c_def.df_templ_inst
            | _ -> raise_compile_err loc (sprintf "invalid constructor %s of variant %s" (id2str cname) (id2str dvar_name))
        else ();
        ((n, t) :: inst_cases, inst_cname :: inst_constr)) ([], []) dvar_cases dvar_constr in
    !inst_dvar.dvar_cases <- List.rev inst_cases;
    !inst_dvar.dvar_constr <- List.rev inst_constr;
    (*printf "variant after instantiation: {{{ "; pprint_exp_x (DefVariant inst_dvar); printf " }}}\n";*)
    (inst_name, inst_app_typ)

and check_pat pat typ env idset typ_vars sc proto_mode simple_pat_mode is_mutable =
    let r_idset = ref idset in
    let r_env = ref env in
    let r_typ_vars = ref typ_vars in
    let captured_val_flags = if is_mutable then ValMutable :: [] else [] in
    let rec process_id i t loc =
        let i0 = get_orig_id i in
        (if (IdSet.mem i0 !r_idset) then
            raise_compile_err loc "duplicate identifier"
        else
            r_idset := IdSet.add i0 !r_idset;
        if proto_mode then i0 else
            (let j = dup_id i0 in
            (*let _ = printf "check_pat: pattern %s is replaced with %s at %s\n" (id2str i0) (id2str j) (loc2str loc) in*)
            let dv = { dv_name=j; dv_typ=t; dv_flags=captured_val_flags; dv_scope=sc; dv_loc=loc } in
            set_id_entry j (IdVal dv);
            r_env := add_id_to_env i0 j !r_env;
            j))
    and check_pat_ p t = (match p with
        | PatAny _ -> p
        | PatLit(l, loc) ->
            if simple_pat_mode then raise_compile_err loc "literals are not allowed here" else ();
            unify t (get_lit_typ l) loc "the literal of unexpected type";
            p
        | PatIdent(i, loc) ->
            PatIdent((process_id i t loc), loc)
        | PatTuple(pl, loc) ->
            let tl = List.map (fun p -> make_new_typ ()) pl in
            unify t (TypTuple tl) loc "improper type of the tuple pattern";
            PatTuple(List.rev (List.fold_left2 (fun pl_new p t -> (check_pat_ p t) :: pl_new) [] pl tl), loc)
        | PatVariant(v, pl, loc) ->
            if not proto_mode then
                (* [TODO] in the ideal case this branch should work fine in the prototype mode as well,
                   just need to make lookup_id smart enough (maybe add some extra parameters to
                   avoid preliminary type instantiation) *)
                let tl = List.map (fun p -> make_new_typ()) pl in
                let ctyp = match tl with [] -> t | _ -> TypFun(tl, t) in
                let v_new = lookup_id v ctyp !r_env sc loc in
                PatVariant(v_new, (List.map2 (fun p t -> check_pat_ p t) pl tl), loc)
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
                            (sprintf "a variant label '%s' with no arguments may not be used as a formal function parameter" (pp_id2str vi))
                        | _ -> 1) in
                        if ni != (List.length pl) then
                            raise_compile_err loc
                            (sprintf "the number of variant pattern arguments does not match to the description of variant case '%s'" (pp_id2str vi))
                            else ();
                        PatVariant(v, pl, loc)
                    with Not_found ->
                        raise_compile_err loc
                        (sprintf "the variant constructor '%s' is not found" (pp_id2str v)))
                | _ -> raise_compile_err loc "variant pattern is used with non-variant type")
            | _ -> raise_compile_err loc "variant pattern is used with non-variant type")
        | PatRec(r_opt, relems, loc) ->
            raise_compile_err loc "record patterns are not supported yet"
        | PatCons(p1, p2, loc) ->
            let t1 = make_new_typ() in
            let t2 = TypList t1 in
            if simple_pat_mode then raise_compile_err loc "'::' pattern is not allowed here" else ();
            unify t t2 loc "'::' pattern is used with non-list type";
            PatCons((check_pat_ p1 t1), (check_pat_ p2 t2), loc)
        | PatAs(p1, i, loc) ->
            let p1_new = check_pat_ p1 t in
            let i_new = process_id i t loc in
            PatAs(p1_new, i_new, loc)
        | PatTyped(p1, t1, loc) ->
            let (t1_new, env1) = check_typ_and_collect_typ_vars t1 !r_env
                (if proto_mode then Some(r_typ_vars) else None) sc loc in
            r_env := env1;
            unify t1_new t loc "inconsistent explicit type specification";
            PatTyped((check_pat_ p1 t), t1_new, loc)) in
    let pat_new = check_pat_ pat typ in
    (pat_new, !r_env, !r_idset, !r_typ_vars)

and check_handlers handlers inptyp outtyp env sc loc =
    List.map (fun (plist, e) ->
        let case_sc = new_block_scope() :: sc in
        let (plist1, env1, capt1) = List.fold_left (fun (plist1, env1, capt1) p ->
            let (p2, env2, capt2, _) = check_pat p inptyp env1 capt1 IdSet.empty case_sc false false false in
            (p2 :: plist1, env2, capt2)) ([], env, IdSet.empty) plist in
        if not (IdSet.is_empty capt1) && (List.length plist1) > 1 then
            raise_compile_err loc "captured variables may not be used in the case of several alternatives"
        else
            let (e1_typ, e1_loc) = get_exp_ctx e in
            unify e1_typ outtyp e1_loc
                "the case expression type does not match the whole expression type (or the type of previous case(s))";
            ((List.rev plist1), (check_exp e env1 case_sc))
        ) handlers

and check_mod m =
    let minfo = !(get_module m) in
    (*try*)
        let modsc = (ScModule m) :: [] in
        let (seq, env) = check_eseq minfo.dm_defs Env.empty modsc false in
        minfo.dm_defs <- seq;
        minfo.dm_env <- env;
    (*with e ->
        compile_errs := e :: !compile_errs*)
