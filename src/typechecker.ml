(* The type checker *)

open Syntax
open Options

(*
The type checker component performs semantical analysis and various
sanity checks of just parsed Ficus module(s).

In theory, this stage, together with the previous lexical and syntactic analysis stages,
ensures that the code is valid and can be compiled to a valid C/machine code.
In practice, however, some checks are better to be postponed to further stages.
Nevertheless, the first 3 stages (lexer, parser and typechecker) are responsible
for reporting majority of the problems in the code.

These are the tasks performed by type checker:
  * infere the missing type specifications. In Ficus one must to explicitly define function argument types,
    however value/variable types, as well as function return types can be omitted, e.g.
    fun hypot(a: float, b: float) = sqrt(a*a + b*b)
  * find the proper match for each abstract symbol referenced in the program. This part deals with
    overloaded functions, generic functions, overriden symbols, etc. It also recognizes notation
    "Modulename.symbolname" and looks inside imported modules to find the proper (possibly overloaded) symbol.
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

(*
Implementation plan:

+ raise_typecheck_err()
+ type unification
+ walk_exp, walk_expv, ...
+ make sure record types are handled properly (add ref)
+/- check_exp
- check_seq
- add_typ_to_env, add_to_env
- lookup_typ, lookup
- check_deffun
- check_defexn
- check_deftype
- check_pattern (with issimple flag)
- check_typespec
- directive handling (add the corresponding module(s) or module content to env)
- instantiate_func
- instantiate_type
- check_exn
- check_defvariant
- handle intrinsic functions/operators
- run it all together
*)

exception TypeCheckError of loc_t * string

let typecheck_errs = ref ([]: exn list)

let raise_typecheck_err loc msg =
    let whole_msg = Printf.sprintf "%s: %s\n" (loc2str loc) msg in
    let err = TypeCheckError(loc, whole_msg) in
    typecheck_errs := err :: !typecheck_errs;
    raise err

let pop_typecheck_err loc =
    match !typecheck_errs with
    | _ :: rest -> typecheck_errs := rest
    | _ -> raise_typecheck_err loc "attempt to pop non-existing typecheck error"

let print_typecheck_err err =
    match err with
    (* error message has been formatted already in raise_typecheck_err(); just print it *)
    | TypeCheckError(_, msg) -> print_string msg
    | Failure msg -> print_string msg
    | _ -> Printf.printf "\n\nException %s occured" (Printexc.to_string err)

type 'x ast_callb_t =
{
    acb_typ: (typ_t -> 'x ast_callb_t -> typ_t) option;
    acb_exp: (exp_t -> 'x ast_callb_t -> exp_t) option;
    acb_pat: (pat_t -> 'x ast_callb_t -> pat_t) option;
    mutable acb_res: 'x
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
    | TypOption t -> TypOption(walk_typ_ t)
    | TypArray(d, et) -> TypArray(d, walk_typ_ et)
    | TypRecord r ->
        let (relems, norm_flag) = !r in
        let new_relems = List.map (fun (n, t, v) -> (n, (walk_typ_ t), v)) relems in
        r := (new_relems, norm_flag); t
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

    (match e with
    | ExpNop _ -> e
    | ExpBreak _ -> e
    | ExpContinue _ -> e
    | ExpRange(e1_opt, e2_opt, e3_opt, ctx) ->
        ExpRange((walk_exp_opt_ e1_opt), (walk_exp_opt_ e2_opt),
                 (walk_exp_opt_ e3_opt), ctx)
    | ExpLit(_, _) -> e
    | ExpIdent(_, _) -> e
    | ExpBinOp(bop, e1, e2, ctx) ->
        ExpBinOp(bop, (walk_exp_ e1), (walk_exp_ e2), ctx)
    | ExpUnOp(uop, e, ctx) -> ExpUnOp(uop, (walk_exp_ e), ctx)
    | ExpSeq(elist, ctx) -> ExpSeq((walk_elist_ elist), ctx)
    | ExpMkTuple(elist, ctx) -> ExpMkTuple((walk_elist_ elist), ctx)
    | ExpMkArray(ell, ctx) -> ExpMkArray((List.map walk_elist_ ell), ctx)
    | ExpMkRecord(e, ne_l, ctx) -> ExpMkRecord((walk_exp_ e), (walk_ne_l_ ne_l), ctx)
    | ExpUpdateRecord(e, ne_l, ctx) -> ExpUpdateRecord((walk_exp_ e), (walk_ne_l_ ne_l), ctx)
    | ExpCall(f, args, ctx) -> ExpCall((walk_exp_ f), (walk_elist_ args), ctx)
    | ExpAt(arr, idx, ctx) -> ExpAt((walk_exp_ arr), (walk_elist_ idx), ctx)
    | ExpIf(c, then_e, else_e, ctx) ->
        ExpIf((walk_exp_ c), (walk_exp_ then_e), (walk_exp_ else_e), ctx)
    | ExpWhile(c, e, ctx) -> ExpWhile((walk_exp_ c), (walk_exp_ e), ctx)
    | ExpDoWhile(c, e, ctx) -> ExpWhile((walk_exp_ c), (walk_exp_ e), ctx)
    | ExpFor(pe_l, body, flags, ctx) ->
        ExpFor((walk_pe_l_ pe_l), (walk_exp_ body), flags, ctx)
    | ExpMap(pew_ll, body, flags, ctx) ->
        ExpMap((List.map (fun (pe_l, when_opt) ->
            (walk_pe_l_ pe_l), (walk_exp_opt_ when_opt)) pew_ll),
            (walk_exp_ body), flags, ctx)
    | ExpFold(pe0_opt, pe_l, body, ctx) ->
        let pe0_opt_ =
            (match pe0_opt with
            | Some (p0, e0) -> Some ((walk_pat_ p0), (walk_exp_ e0))
            | None -> None) in
        ExpFold(pe0_opt_, (walk_pe_l_ pe_l), (walk_exp_ body), ctx)
    | ExpTryCatch(e, handlers, ctx) ->
        ExpTryCatch((walk_exp_ e), (walk_handlers_ handlers), ctx)
    | ExpMatch(e, handlers, ctx) ->
        ExpMatch((walk_exp_ e), (walk_handlers_ handlers), ctx)
    | ExpCast(e, t, ctx) -> ExpCast((walk_exp_ e), (walk_typ_ t), ctx)
    | ExpTyped(e, t, ctx) -> ExpCast((walk_exp_ e), (walk_typ_ t), ctx)
    | ExpCCode(_, _) -> e
    | DefVal(p, v, flags, ctx) ->
        DefVal((walk_pat_ p), (walk_exp_ v), flags, ctx)
    | DefFun(df) ->
        if !df.df_templ_args != [] then e else
        (let { df_name; df_templ_args; df_args; df_typ; df_body;
               df_flags; df_scope; df_loc; df_templ_inst } = !df in
        df := { df_name; df_templ_args; df_args=(walk_plist_ df_args); df_typ=(walk_typ_ df_typ);
                df_body=(walk_exp_ df_body); df_flags; df_scope; df_loc; df_templ_inst };
        e)
    | DefExn(de) ->
        let { dexn_name; dexn_typ; dexn_scope; dexn_loc } = !de in
        de := { dexn_name; dexn_typ=(walk_typ_ dexn_typ); dexn_scope; dexn_loc };
        e
    | DefType(dt) ->
        if !dt.dt_templ_args != [] then e else
        (let { dt_name; dt_templ_args; dt_typ; dt_scope; dt_loc } = !dt in
        dt := { dt_name; dt_templ_args; dt_typ=(walk_typ_ dt_typ); dt_scope; dt_loc };
        e)
    | DefVariant(dvar) ->
        if !dvar.dvar_templ_args != [] then e else
        (let { dvar_name; dvar_templ_args; dvar_flags; dvar_members;
               dvar_constr; dvar_templ_inst; dvar_scope; dvar_loc } = !dvar in
        dvar := { dvar_name; dvar_templ_args; dvar_flags;
                  dvar_members=(List.map (fun (n, t) -> (n, walk_typ_ t)) dvar_members);
                  dvar_constr; dvar_templ_inst; dvar_scope; dvar_loc };
        e)
    | DefClass(dc) -> (* [TODO] *) e
    | DefInterface(di) -> (* [TODO] *) e
    | DirImport _ -> e
    | DirImportFrom _ -> e)

and walk_pat p callb =
    let walk_typ_ t = check_n_walk_typ t callb in
    let walk_pat_ p = check_n_walk_pat p callb in
    let walk_pl_ p = check_n_walk_plist p callb in
    let walk_exp_ e = check_n_walk_exp e callb in

    (match p with
    | PatAny _ -> p
    | PatLit _ -> p
    | PatIdent _ -> p
    | PatTuple(pl, loc) -> PatTuple((walk_pl_ pl), loc)
    | PatCtor(n, args, loc) -> PatCtor(n, (walk_pl_ args), loc)
    | PatRec(n_opt, np_l, loc) ->
        PatRec(n_opt, (List.map (fun (n, p) -> (n, (walk_pat_ p))) np_l), loc)
    | PatCons(p1, p2, loc) -> PatCons((walk_pat_ p1), (walk_pat_ p2), loc)
    | PatAs(p, n, loc) -> PatAs((walk_pat_ p), n, loc)
    | PatTyped(p, t, loc) -> PatTyped((walk_pat_ p), (walk_typ_ t), loc))

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
    let unify_undo_stack = ref [] in

    (* checks if a reference to undefined type (an argument of TypVar (ref None))
       occurs in the other type (t2). If yes, then we have a cyclic dependency
       between types and so they cannot in principle be unified (i.e. no type
       cannot be unified with a part of it).
       In all other cases the undefined type can be unified with t2. *)
    let rec occurs r1 t2 =
        (match t2 with
        | TypFun(args2, rt2) -> List.exists (occurs r1) args2 || occurs r1 rt2
        | TypList(et2) -> occurs r1 et2
        | TypTuple(tl2) -> List.exists (occurs r1) tl2
        | TypRef(drt2) -> occurs r1 drt2
        | TypArray(_, et2) -> occurs r1 et2
        | TypVar(r2) when r1 == r2 -> true
        | TypVar({ contents = None }) -> false
        | TypVar({ contents = Some(t2_) }) -> occurs r1 t2_
        | _ -> false) in
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
            (List.for_all2 maybe_unify_ args1 args2) && (maybe_unify_ rt1 rt2)
        | ((TypList et1), (TypList et2)) -> maybe_unify_ et1 et2
        | ((TypTuple tl1), (TypTuple tl2)) -> List.for_all2 maybe_unify_ tl1 tl2
        | ((TypRef drt1), (TypRef drt2)) -> maybe_unify_ drt1 drt2
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
            | _ -> unify_undo_stack := r1 :: !unify_undo_stack; r1 := Some(t2)); true)
          (* symmetrical case *)
        | (_, (TypVar ({contents = None} as r2))) ->
            if occurs r2 t1 then false else
            ((match t1 with
            | TypErr -> ()
            | _ -> unify_undo_stack := r2 :: !unify_undo_stack; r2 := Some(t1)); true)
        (* a declaration cannot be unified with any non-decl type *)
        | (TypErr, _ ) -> true
        | (_, TypErr) -> true
        | (TypDecl, _) | (_, TypDecl) -> false
        (* in all other cases the types cannot be unified *)
        | (_, _) -> false) in

    let ok = maybe_unify_ t1 t2 in
    if ok && update_refs then () else
    (* restore the original types in the case of type unification failure
       or when update_refs=false *)
    (List.iter (fun r -> r := None) !unify_undo_stack);
    ok

(* this is another flavor of type unification function;
   it throws an exception in the case of failure *)
let unify t1 t2 loc msg =
    if maybe_unify t1 t2 true then () else
    raise_typecheck_err loc msg

let deref_typ t shorten_paths =
    let rec deref_typ_ t =
    (match t with
    | TypFun(args, rt) -> TypFun((List.map deref_typ_ args), (deref_typ_ rt))
    | TypList(et) -> TypList(deref_typ_ et)
    | TypTuple(tl) -> TypTuple (List.map deref_typ_ tl)
    | TypRef(drt) -> TypRef(deref_typ_ drt)
    | TypArray(dims, et) -> TypArray(dims, (deref_typ_ et))
    | TypVar({ contents = None }) -> t
    | TypVar({ contents = Some(t2) } as r) ->
        let t2_ = deref_typ_ t2 in
        if shorten_paths then r := Some(t2_) else ();
        t2_
    | TypApp(args, i) -> TypApp((List.map deref_typ_ args), i)
    | _ -> t) in deref_typ_ t

let coerce_types t1 t2 allow_tuples allow_fp =
    let t1_ = deref_typ t1 false in
    let t2_ = deref_typ t2 false in
    let safe_max_ubits = if !options.arch64 then 32 else 16 in
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
    in try Some(coerce_types_ t1_ t2_) with Failure _ -> None

let find_all_ids n env =
    match Env.find_opt n env with
    | Some(l) -> l
    | _ -> []

let funarg2args t =
    match t with
    | TypVoid -> []
    | TypTuple(tl) -> tl
    | _ -> t :: []

let typ2constr t rt =
    match t with
    | TypVoid -> rt
    | _ -> TypFun((funarg2args t), rt)
    
let get_eseq_typ eseq =
    match eseq with
    | [] -> TypVoid
    | _ -> get_exp_typ(Utils.last_elem eseq)

let lookup_id n env loc =
    let rec lookup_ ids =
        (match ids with
        | i :: rest ->
            (match id_info i with
            | IdVal { dv_typ } -> (i, dv_typ)
            | IdFun {contents={ df_typ }} -> (i, df_typ)
            | IdModule _ -> (i, TypModule)
            | IdExn {contents={ dexn_typ }} -> (i, typ2constr dexn_typ TypExn)
            | IdNone | IdText _ | IdType _ | IdVariant _
            | IdClass _ | IdInterface _ -> lookup_ rest)
        | _ -> raise_typecheck_err loc (Printf.sprintf "%s not found" (pp_id2str n)))
    in lookup_ (find_all_ids n env)

let rec check_exp e env sc =
    let (etyp, eloc) as ctx = get_exp_ctx e in
    match e with
    | ExpNop(_) -> unify etyp TypVoid eloc "nop must have void type"; ExpNop(ctx)
    | ExpRange(e1_opt, e2_opt, e3_opt, _) ->
        let check_range_e e_opt =
            (match e_opt with
            | None -> None
            | Some(e) ->
                let new_e = check_exp e env sc in
                let (etyp1, eloc1) = get_exp_ctx new_e in
                (unify etyp1 TypInt eloc1 "explicitly specified component of a range must be an integer";
                Some new_e)) in
        let new_e1_opt = check_range_e e1_opt in
        let new_e2_opt = check_range_e e2_opt in
        let new_e3_opt = check_range_e e3_opt in
        unify etyp (TypTuple [TypInt;TypInt;TypInt]) eloc "the range type should have (int, int, int) type";
        ExpRange(new_e1_opt, new_e2_opt, new_e3_opt, ctx)
    | ExpLit(lit, _) -> unify etyp (get_lit_type lit) eloc "the literal has improper type"; e
    | ExpIdent(n, _) ->
        let (n, t) = lookup_id n env eloc in
        unify etyp t eloc "the indentifier has improper type";
        ExpIdent(n, ctx)
        (*
            '.' can be a tuple access operator (or a record access operator) or module access operator.
            if it's a tuple or record, find the proper field and unify the expression type with accordingly.
            if it's module access operator, try to find the proper match. If there are multiple possible matches,
            leave it as-is for now (???)
        *)
    | ExpBinOp(OpMem, e1, e2, _) ->
        (* in the case of '.' operation we do not check e2 immediately after e1,
           because e2 is a 'member' of a structure/module e1,
           so it cannot be independently analyzed *)
        let new_e1 = check_exp e1 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        (match ((deref_typ etyp1 false), new_e1, e2) with
        | (TypModule, ExpIdent(n1, _), ExpIdent(n2, (etyp2, eloc2))) ->
            let n1_info = get_module n1 in
            let n1_env = !n1_info.dm_env in
            let (new_n2, t) = lookup_id n2 n1_env eloc in
            unify etyp t eloc "the module element has improper type";
            ExpIdent(new_n2, ctx)
        | ((TypTuple tl), _, ExpLit((LitInt idx), (etyp2, eloc2))) ->
            unify etyp2 TypInt eloc2 "index must be int!";
            (* we do not handle negative indices, because the parser would not allow that;
               ok, if it's wrong assumption, an exception will be thrown
               (and be catched at the higher level) anyway *)
            let et = (try List.nth tl (Int64.to_int idx) with Failure _ ->
                raise_typecheck_err eloc2 "the tuple index is out of range") in
            unify etyp et eloc "incorrect type of the tuple element";
            ExpBinOp(OpMem, new_e1, e2, ctx)
        (* [TODO] add record handling *)
        )
    | ExpBinOp(bop, e1, e2, _) ->
        let new_e1 = check_exp e1 env sc in
        let (etyp1, eloc1) = get_exp_ctx new_e1 in
        let etyp1_ = deref_typ etyp1 false in
        let new_e2 = check_exp e2 env sc in
        let (etyp2, eloc2) = get_exp_ctx new_e2 in
        let etyp2_ = deref_typ etyp2 false in

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
            (where record fields are ordered as in the definition, but by names),
            lists, strings and arrays - lexicographically as well,
            variants - using the tag precedence
            maybe references and cpointers - by address,
            maybe classes (lexicographically, member by member)
            *)
        | OpCompareEQ | OpCompareNE | OpCompareLT | OpCompareLE | OpCompareGT | OpCompareGE ->
            unify etyp1 etyp2 eloc "only equal types can be compared";
            (match (deref_typ etyp1 false) with
            | TypInt | TypSInt _ | TypUInt _ | TypFloat _ | TypBool -> Some(TypBool)
            | _ -> None)
        | OpCons ->
            unify etyp2 (TypList etyp1) eloc "incorrect combination of types in '::' operation";
            Some(etyp2)
        | OpSet ->
            (* check that new_e1 is lvalue and that new_e1 and new_e2 have equal types;
               in future we can let etyp1_ and etyp2_ be different as long as the assignment
               is safe and does not loose precision, e.g. int8 to int, float to double etc. *)
            let is_lvalue = (match new_e1 with
            | ExpAt _ -> true (* an_arr[idx] = e2 *)
            | ExpUnOp(OpDeref, _, _) -> true (* *a_ref = e2 *)
            | ExpIdent(n1, _) -> (* a_var = e2 *)
                (match (id_info n1) with
                | IdVal { dv_typ; dv_flags } ->
                    unify etyp2 dv_typ eloc "the variable type does not match the assigned value";
                    (List.mem ValMutable dv_flags)
                | _ -> false)
            (* [TODO] probably, we should let user to modify individual fields of a record,
            as long as it's stored in a mutable place (variable, array, by reference) *)
            | _ -> false) in
            if not is_lvalue then raise_typecheck_err eloc "the left side of assignment is not an l-value"
            else
                unify etyp1 etyp2 eloc "the left and the right sides of the assignment must have the same type";
            Some(TypVoid)
        | _ -> raise_typecheck_err eloc "unsupported binary operation") in

        (match typ_opt with
        | Some(typ) ->
            unify typ etyp eloc "improper type of arithmetic operation";
            ExpBinOp(bop, new_e1, new_e2, ctx)
        | _ ->
            (* try to find an overloaded function that will handle such operation with combination of types, e.g.
               operator + (p: point, q: point) = point { p.x + q.x, p.y + q.y } *)
            let f_id = get_binop_fname bop in
            check_exp (ExpCall (ExpIdent(f_id, (make_new_typ(), eloc)), [new_e1; new_e2], ctx)) env sc)
    | ExpUnOp(uop, e1, _) ->
        (*
          [TODO]
          check e1;
          then proceed depending on the op:

          OpNegate | OpBitwiseNot | OpLogicNot | OpMakeRef | OpDeref | OpThrow

          - - check that the argument is a number or a tuple of signed numbers.
              find out the result type by coercing arg type with itself.
          ~ - check that the argument is a number/bool or a tuple of numbers/bool's.
              the result will have the same type.
          ! - check that the argument is bool. the result will have type bool.
          ref - unify arg type with et = TypVar(ref None). unify exp type with TypRef(et).
          * (deref) - unify arg type with TypRef(etyp).
          throw - do not unify etyp with anything, leave it as-is.
        *)
        raise_typecheck_err eloc "unsupported op"
    | ExpSeq(eseq, _) ->
        (*
          [TODO]
          run a separate function to check a sequence of expression (see below)
        *)
        let (eseq, _) = check_seq eseq env sc true in
        let eseq_typ = get_eseq_typ eseq in
        unify etyp eseq_typ eloc "the sequence type does not match the last expression type";
        ExpSeq(eseq, ctx)
        
    | ExpMkTuple(el, _) ->
        let (new_el, tl) = List.fold_left
            (fun (new_el, tl) elem ->
                let new_elem = check_exp elem env sc in
                let (new_etyp, _) = get_exp_ctx new_elem in
                ((new_elem :: new_el), (new_etyp :: tl))) ([], []) el in
        unify (TypTuple (List.rev tl)) etyp eloc "improper tuple type or the number of elements";
        ExpMkTuple ((List.rev new_el), ctx)
    | ExpCall(f, args, _) ->
        (*
          [TODO]
          * unify f typ with TypFun(args_types, rt), where each of arg types and rt = TypVar(ref None)
            (but all the types are different)
          * then try to find the proper f (run check_exp)
          * then check and unify types of all the arg types.
          * then repeat the search of f if needed.
          * unify the expression type with "rt".
        *)
        raise_typecheck_err eloc "unsupported op"
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
        let new_c = check_exp c env sc in
        let (new_ctyp, new_cloc) = get_exp_ctx new_c in
        let new_e1 = check_exp e1 env sc in
        let (new_typ1, new_loc1) = get_exp_ctx new_e1 in
        let new_e2 = check_exp e2 env sc in
        let (new_typ2, new_loc2) = get_exp_ctx new_e2 in
        unify new_ctyp TypBool new_cloc "if() condition should have boolean type";
        unify new_typ1 new_typ2 new_loc2 "if() then- and else- branches should have the same type";
        unify new_typ1 etyp eloc "if() expression should have the same type as its branches";
        ExpIf(new_c, new_e1, new_e2, ctx)
    | ExpWhile(c, body, _) ->
        let new_c = check_exp c env sc in
        let (new_ctyp, new_cloc) = get_exp_ctx new_c in
        let new_body = check_exp body env sc in
        let (new_btyp, new_bloc) = get_exp_ctx new_body in
        unify new_ctyp TypBool new_cloc "while() loop condition should have boolean type";
        unify new_btyp TypVoid new_cloc "while() loop body should have void type";
        unify etyp TypVoid eloc "while() loop should have void type";
        ExpWhile (new_c, new_body, ctx)
    | ExpDoWhile(c, body, _) ->
        let new_c = check_exp c env sc in
        let (new_ctyp, new_cloc) = get_exp_ctx new_c in
        let new_body = check_exp body env sc in
        let (new_btyp, new_bloc) = get_exp_ctx new_body in
        unify new_ctyp TypBool new_cloc "do while() loop condition should have boolean type";
        unify new_btyp TypVoid new_cloc "do while() loop body should have void type";
        unify etyp TypVoid eloc "do while() loop should have void type";
        ExpDoWhile (new_c, new_body, ctx)
    | ExpFor(for_clauses, body, flags, _) ->
        (*
          [TODO]
          * check each range of for loop; it should either be a range or some collection;
          * the pattern should take the appropriate type;
          * loop body should be unified with "void"
          * the whole loop should be unified with "void" as well
        *)
        raise_typecheck_err eloc "unsupported op"
    | ExpFold(fold_init, fold_clauses, body, _) ->
        (* [TODO] *)
        (* `fold (p=e0; ...) [fold (...) ...] e1`
              is transformed to
           `{
           var acc = e0
           for (...) [for(...) ...] { val p = acc; acc = e1 }
           acc
           }`
        *)
        (*let acc_tp = make_new_typ() in
        let acc_loc = curr_loc_n 2 (* pat location *) in
        let acc_id = get_unique_id "acc" true in
        let acc_ctx = (acc_tp, acc_loc) in
        let acc_exp = ExpIdent(acc_id, acc_ctx) in
        let acc_pat = PatIdent(acc_id, acc_loc) in
        let ((acc_pat0, fold_exp0), fold_cl) = $2 in
        let acc_decl = DefVal(acc_pat, fold_exp0, [ValMutable], acc_ctx) in
        let for_body = $3 in
        let for_body_loc  = get_exp_loc for_body in
        let acc_expand = DefVal(acc_pat0, acc_exp, [], (acc_tp, for_body_loc)) in
        let acc_update = ExpBinOp(OpSet, acc_exp, for_body, (TypVoid, for_body_loc)) in
        let new_for_body = ExpSeq([acc_expand; acc_update], (TypVoid, for_body_loc)) in
        let for_loc = curr_loc() in
        let new_for = ExpFor ({ for_cl = fold_cl; for_body = new_for_body }, (TypVoid, for_loc)) in
        ExpSeq([acc_decl; new_for; acc_exp], (acc_tp, for_loc))*)
        raise_typecheck_err eloc "unsupported op"
    | ExpTryCatch(try_e, handlers, _) ->
        (* [TODO] check try_e, check handlers, check that each handlers branch is unified with try_e;
           the overall trycatch should also be unified with try_e *)
        raise_typecheck_err eloc "unsupported op"
    | ExpCast(e1, t1, _) ->
        (* [TODO] check that e1 can be cast to t1 *)
        let new_t1 = check_typ t1 env sc in
        let new_e1 = check_exp e1 env sc in
        raise_typecheck_err eloc "unsupported op"
    | ExpTyped(e1, t1, _) ->
        let new_t1 = check_typ t1 env sc in
        let new_e1 = check_exp e1 env sc in
        let (new_etyp, new_eloc) = get_exp_ctx new_e1 in
        unify new_etyp new_t1 new_eloc "improper explicit type of the expression";
        unify etyp new_t1 new_eloc "improper explicit type of the expression";
        ExpTyped(new_e1, new_t1, ctx)
    | ExpCCode(_, _) ->
        (match sc with
        | ScModule(_) :: _ -> ()
        | ScFun(_) :: _ -> ()
        | _ -> raise_typecheck_err eloc
            "ccode may be used only at the top (module level) or as a single expression in function definition");
        e
    | DefVal(p, v, flags, _) -> e
        (*
          [TODO]
          * check v and get its type
          * check the pattern p and unify it with v type
          * if it's not a class (check sc) then add p elements to the environment
          * unify etyp with TypDecl
        *)
    | DefFun({contents = {df_templ_args}} as rdf) -> e
    | DefVariant(_) -> e
    | DefClass(_) -> raise_typecheck_err eloc "not implemented"
    | DefInterface(_) -> raise_typecheck_err eloc "not implemented"
    | DefExn(_) -> e
    | DefType(_) -> e
    | DirImport(_, _) -> e
    | DirImportFrom(_, _, _) -> e

and check_seq eseq env sc create_sc =
    (*
      [TODO]
      * if create_sc == true, create new block scope
      * scan eseq, look for import statements. Since the modules are topologically sorted using the
        dependency criteria, all the imported modules should have been analyzed already.
        ** If we have "import m [as m_alias]",
           we add either "m":"real_m_id" or "m_alias":"real_m_id" to the environment.
           We also need to scan m's env and import all the overloaded operators.
        ** If we have "from m import <id_list>|*", we scan m's env and import the requested
           id's. We also add "m" to env (and import all the overloaded opeators).
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
    failwith "check_seq not implemented"

and check_typ tp env sc =
    (*
      [TODO]
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
    failwith "check_typ not implemented"

and check_pat pat tp env id_set sc =
    (*
      [TODO]
      * "_" should probably be replaced with a temporary id
      * ident should be checked for the absence of duplicates.
      *
    | PatIdent of id_t * loc_t
    | PatTuple of pat_t list * loc_t
    | PatCtor of id_t * pat_t list * loc_t
    | PatTyped of pat_t * typ_t * loc_t
    *)
    failwith "check_pat not implemented"

and check_mod m =
    let minfo = !(get_module m) in
    try
        let modsc = (ScModule m) :: [] in
        let (seq, env) = check_seq minfo.dm_defs Env.empty modsc false in
        minfo.dm_defs <- seq;
        minfo.dm_env <- env;
    with e ->
        typecheck_errs := e :: !typecheck_errs
