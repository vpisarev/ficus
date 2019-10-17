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
+ type unification (extend it to handle all the types,
    maybe except for classes and interfaces for now)
+ walk_exp, ...
+ make sure record types are handled properly (add ref)
+/- check_exp
+ check_eseq
+ add_typ_to_env
+ add_id_to_env
+ lookup_typ, lookup_id
- check_deffun
+ check_defexn
+ check_deftyp
+/- check_defvariant
- check_pattern (with issimple flag)
+ check_typ
+ check_directives (i.e. import directives for now)
+ instantiate_fun
+/- instantiate_type
- instantiate_variant
- handle intrinsic functions/operators
- run it all together
*)

exception TypeCheckError of loc_t * string

let typecheck_errs = ref ([]: exn list)

let fname_always_import =
[
    fname_op_add; fname_op_sub; fname_op_mul;
    fname_op_div; fname_op_mod; fname_op_pow; fname_op_shl; fname_op_shr;
    fname_op_bit_and; fname_op_bit_or; fname_op_bit_xor; fname_op_eq;
    fname_op_ne; fname_op_lt; fname_op_gt; fname_op_le; fname_op_gt;

    fname_op_plus; fname_op_negate; fname_op_bit_not;

    fname_to_int; fname_to_uint8; fname_to_int8; fname_to_uint16; fname_to_int16;
    fname_to_uint32; fname_to_int32; fname_to_uint64; fname_to_int64;
    fname_to_float; fname_to_double; fname_to_bool; fname_to_string
]

let raise_typecheck_err_ err =
    typecheck_errs := err :: !typecheck_errs;
    raise err

let raise_typecheck_err loc msg =
    let whole_msg = sprintf "%s: %s\n" (loc2str loc) msg in
    let err = TypeCheckError(loc, whole_msg) in
    raise_typecheck_err_ err

let pop_typecheck_err loc =
    match !typecheck_errs with
    | _ :: rest -> typecheck_errs := rest
    | _ -> raise_typecheck_err loc "attempt to pop non-existing typecheck error"

let check_typecheck_errs () =
    match !typecheck_errs with
    | err :: _ -> raise err
    | _ -> ()

let print_typecheck_err err =
    match err with
    (* error message has been formatted already in raise_typecheck_err(); just print it *)
    | TypeCheckError(_, msg) -> print_string msg
    | Failure msg -> print_string msg
    | _ -> printf "\n\nException %s occured" (Printexc.to_string err)

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
    let walk_ctx_ (t, loc) = ((walk_typ_ t), loc) in

    (match e with
    | ExpNop (ctx) -> ExpNop(walk_ctx_ ctx)
    | ExpBreak (ctx) -> ExpBreak(walk_ctx_ ctx)
    | ExpContinue (ctx) -> ExpContinue(walk_ctx_ ctx)
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
    | ExpIf(c, then_e, else_e, ctx) ->
        ExpIf((walk_exp_ c), (walk_exp_ then_e), (walk_exp_ else_e), (walk_ctx_ ctx))
    | ExpWhile(c, e, ctx) -> ExpWhile((walk_exp_ c), (walk_exp_ e), (walk_ctx_ ctx))
    | ExpDoWhile(c, e, ctx) -> ExpWhile((walk_exp_ c), (walk_exp_ e), (walk_ctx_ ctx))
    | ExpFor(pe_l, body, flags, ctx) ->
        ExpFor((walk_pe_l_ pe_l), (walk_exp_ body), flags, (walk_ctx_ ctx))
    | ExpMap(pew_ll, body, flags, ctx) ->
        ExpMap((List.map (fun (pe_l, when_opt) ->
            (walk_pe_l_ pe_l), (walk_exp_opt_ when_opt)) pew_ll),
            (walk_exp_ body), flags, (walk_ctx_ ctx))
    | ExpTryCatch(e, handlers, ctx) ->
        ExpTryCatch((walk_exp_ e), (walk_handlers_ handlers), (walk_ctx_ ctx))
    | ExpMatch(e, handlers, ctx) ->
        ExpMatch((walk_exp_ e), (walk_handlers_ handlers), (walk_ctx_ ctx))
    | ExpCast(e, t, ctx) -> ExpCast((walk_exp_ e), (walk_typ_ t), (walk_ctx_ ctx))
    | ExpTyped(e, t, ctx) -> ExpCast((walk_exp_ e), (walk_typ_ t), (walk_ctx_ ctx))
    | ExpCCode(str, ctx) -> ExpCCode(str, (walk_ctx_ ctx))
    | DefVal(p, v, flags, ctx) ->
        DefVal((walk_pat_ p), (walk_exp_ v), flags, (walk_ctx_ ctx))
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
    | DefTyp(dt) ->
        if !dt.dt_templ_args != [] then e else
        (let { dt_name; dt_templ_args; dt_typ; dt_scope; dt_loc; dt_finalized } = !dt in
        dt := { dt_name; dt_templ_args; dt_typ=(walk_typ_ dt_typ); dt_scope; dt_loc; dt_finalized };
        e)
    | DefVariant(dvar) ->
        if !dvar.dvar_templ_args != [] then e else
        (let { dvar_name; dvar_templ_args; dvar_typ; dvar_flags; dvar_members;
               dvar_constr; dvar_templ_inst; dvar_scope; dvar_loc } = !dvar in
        dvar := { dvar_name; dvar_templ_args; dvar_typ=(walk_typ_ dvar_typ); dvar_flags;
                  dvar_members=(List.map (fun (n, t) -> (n, walk_typ_ t)) dvar_members);
                  dvar_constr; dvar_templ_inst; dvar_scope; dvar_loc };
        e)
    | DefClass(dc) -> (* [TODO] *) e
    | DefInterface(di) -> (* [TODO] *) e
    | DirImport (_, _) -> e
    | DirImportFrom (_, _, _) -> e)

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
    | PatVariant(n, args, loc) -> PatVariant(n, (walk_pl_ args), loc)
    | PatRec(n_opt, np_l, loc) ->
        PatRec(n_opt, (List.map (fun (n, p) -> (n, (walk_pat_ p))) np_l), loc)
    | PatCons(p1, p2, loc) -> PatCons((walk_pat_ p1), (walk_pat_ p2), loc)
    | PatAs(p, n, loc) -> PatAs((walk_pat_ p), n, loc)
    | PatTyped(p, t, loc) -> PatTyped((walk_pat_ p), (walk_typ_ t), loc))

let rec dup_typ_ t callb =
    match t with
    | TypVar r ->
        let t1_opt = (match !r with
        | Some t ->
            let t1 = dup_typ_ t callb in
            Some(t1)
        | _ -> None) in
        TypVar(ref t1_opt)
    | TypRecord(r) ->
        let (relems, norm_flag) = !r in
        let new_relems = List.map (fun (n, t, v) -> (n, (dup_typ_ t callb), v)) relems in
        TypRecord(ref (new_relems, norm_flag))
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
    acb_res = 0;
}

let dup_typ t = dup_typ_ t dup_callb
let dup_exp e = dup_exp_ e dup_callb
let dup_pat p = walk_pat p dup_callb

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
        | TypOption(t2_) -> occurs r1 t2_
        | TypRecord({ contents = (relems2, _) }) ->
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
            (List.for_all2 maybe_unify_ args1 args2) && (maybe_unify_ rt1 rt2)
        | ((TypList et1), (TypList et2)) -> maybe_unify_ et1 et2
        | ((TypTuple tl1), (TypTuple tl2)) -> List.for_all2 maybe_unify_ tl1 tl2
        | ((TypRef drt1), (TypRef drt2)) -> maybe_unify_ drt1 drt2
        | (TypRecord ({contents=(relems1, r1decl)} as r1), TypRecord ({contents=(relems2, r2decl)} as r2)) ->
            (match (r1decl, r2decl) with
            | (true, true) ->
                (* when both record types are known, then their i-th members must match *)
                (try
                    List.for_all2 (fun (n1, t1, _) (n2, t2, _) ->
                        n1 = n2 && (maybe_unify_ t1 t2)) relems1 relems2
                with Invalid_argument _ -> false)
            | (_, _) ->
                let (r1, r2) = if not r1decl then (r1, r2) else (r2, r1) in
                let ok = List.for_all (fun (n1, t1, _) ->
                    List.exists (fun (n2, t2, _) -> n1 = n2 && maybe_unify_ t1 t2) relems2) relems1 in
                if ok then
                    (rec_undo_stack := (r1, !r1) :: !rec_undo_stack; r1 := !r2)
                else ();
                ok)
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

    let ok = maybe_unify_ t1 t2 in
    if ok && update_refs then () else
    (* restore the original types in the case of type unification failure
       or when update_refs=false *)
    (List.iter (fun (r, prev) -> r := prev) !rec_undo_stack);
    (List.iter (fun r -> r := None) !undo_stack);
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

let finalize_record_typ t loc =
    match (deref_typ t false) with
    | TypRecord ({contents=(relems, false)} as r) ->
        r := (relems, true);
        List.iteri (fun i (n, t, v_opt) ->
            List.iteri (fun j (m, _, _) -> if j > i && m=n then
                raise_typecheck_err loc (sprintf "duplicate field '%s' in the record" (pp_id2str n))) relems;
            match v_opt with
            | Some(v) -> unify t (get_lit_typ v) loc
                (sprintf "type of the field '%s' and its initializer do not match" (pp_id2str n))
            | _ -> ()) relems;
        TypRecord(r)
    | t -> t

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
            raise_typecheck_err loc
                (sprintf "the type %s is re-declared in the same scope; the previous declaration is here %s"
                (pp_id2str key) (loc2str (get_loc i)))
        else ()
    | _ -> ())

let rec find_first n env loc pred =
    let rec find_next_ elist =
        match elist with
        | e :: rest ->
            (match (pred e) with
            | Some x -> x
            | _ -> find_next_ rest)
        | _ -> raise_typecheck_err loc (sprintf "the appropriate match for %s is not found" (pp_id2str n))
    in find_next_ (find_all n env)

let rec lookup_id n t env sc loc =
    find_first n env loc (fun e ->
        match e with
        | EnvId i ->
            (match id_info i with
            | IdVal {dv_typ} -> unify dv_typ t loc "incorrect value type"; Some(i)
            | IdFun {contents={ df_templ_args; df_typ; df_templ_inst } as df} ->
                if df_templ_args = [] then
                    if maybe_unify df_typ t true then
                        Some(i)
                    else
                        None
                else
                    let (ftyp, env1) = preprocess_templ_typ df_templ_args df_typ env sc loc in
                    if maybe_unify ftyp t true then
                        try
                            Some(List.find (fun inst ->
                            match id_info inst with
                            | IdFun {contents={df_typ=inst_typ}} ->
                                maybe_unify inst_typ t true
                            | _ -> raise_typecheck_err loc (sprintf "invalid (non-function) instance %s of template function %s" (id2str inst) (pp_id2str n))
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
            | IdNone | IdText _ | IdTyp _ | IdVariant _
            | IdClass _ | IdInterface _ -> None)
        | EnvTyp _ -> None)

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
            if maybe_unify t ftyp false then
                raise_typecheck_err loc
                    (sprintf "the type %s is re-declared in the same scope; the previous declaration is here %s"
                    (pp_id2str df_name) (loc2str df_loc))
            else ()
        else ()
    | IdExn {contents={dexn_name; dexn_typ; dexn_scope; dexn_loc}} ->
        if (List.hd dexn_scope) = (List.hd sc) then
            let t = typ2constr dexn_typ TypExn in
            if maybe_unify t ftyp false then
                raise_typecheck_err loc
                    (sprintf "the type %s is re-declared in the same scope; the previous declaration is here %s"
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
        | _ -> raise_typecheck_err inst_loc
        (sprintf "the number of actual type parameters and formal type parameters, as declared at\n\t%s,\n\tdo not match"
        (loc2str def_loc))) in
    List.fold_left2 (fun env n t -> add_typ_to_env n t env) env templ_args norm_ty_args

and check_exp e env sc =
    let (etyp, eloc) as ctx = get_exp_ctx e in
    (match e with
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
    | ExpLit(lit, _) -> unify etyp (get_lit_typ lit) eloc "the literal has improper type"; e
    | ExpIdent(n, _) ->
        let n = lookup_id n etyp env sc eloc in
        ExpIdent(n, ctx)
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
            let new_n2 = lookup_id n2 etyp n1_env sc eloc in
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
        | (TypRecord {contents=(relems, norm_flag)}, _, ExpIdent(n2, (etyp2, eloc2))) ->
            if not norm_flag then
                raise_typecheck_err eloc "trying to access a record with unknown type; use explicit record type specification"
            else
            (try
                let (_, t1, _) = List.find (fun (n1, t1, _) -> n1 = n2) relems in
                unify etyp t1 eloc "incorrect type of the record element";
                ExpBinOp(OpMem, new_e1, e2, ctx)
            with Not_found -> raise_typecheck_err eloc (sprintf "the record element %s is not found" (pp_id2str n2)))
        | _ -> raise_typecheck_err eloc "unsupported element access operation"
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
            (where record fields are ordered by the declaration order, not by names),
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
            unify typ etyp eloc "improper type of the arithmetic operation result";
            ExpBinOp(bop, new_e1, new_e2, ctx)
        | _ ->
            (* try to find an overloaded function that will handle such operation with combination of types, e.g.
               operator + (p: point, q: point) = point { p.x + q.x, p.y + q.y } *)
            let f_id = get_binop_fname bop in
            check_exp (ExpCall (ExpIdent(f_id, (make_new_typ(), eloc)), [new_e1; new_e2], ctx)) env sc)
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
                match t with
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
        | OpMakeRef ->
            unify etyp (TypRef etyp1) eloc "improper type of ref() operator result";
            ExpUnOp(uop, new_e1, ctx)
        | OpDeref ->
            unify (TypRef etyp) etyp1 eloc "improper type of the unary '*' operator result";
            ExpUnOp(uop, new_e1, ctx)
        | OpThrow ->
            unify etyp1 TypExn eloc "the argument of 'throw' operator must be an exception";
            ExpUnOp(uop, new_e1, ctx)
        | OpExpand ->
            raise_typecheck_err eloc "the expand (\\...) operation is not implemented yet")
    | ExpSeq(eseq, _) ->
        let (eseq, _) = check_eseq eseq env sc true in
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
    | ExpMap (_, _, _, _) ->
        raise_typecheck_err eloc "unsupported op"
    | ExpBreak _ | ExpContinue _ ->
        raise_typecheck_err eloc "unsupported op"
    | ExpMkArray (_, _) ->
        raise_typecheck_err eloc "unsupported op"
    | ExpMkRecord (_, _, _) | ExpUpdateRecord (_, _, _) ->
        raise_typecheck_err eloc "unsupported op"
    | ExpTryCatch(e1, handlers, _) ->
        (* [TODO] check try_e, check handlers, check that each handlers branch is unified with try_e;
           the overall trycatch should also be unified with try_e *)
        let new_e1 = check_exp e1 env sc in
        let (new_e1typ, new_e1loc) = get_exp_ctx new_e1 in
        let _ = unify etyp new_e1typ new_e1loc "try body type does match the whole try-catch type" in
        let new_handlers = check_handlers handlers TypExn etyp env sc eloc in
        ExpTryCatch(new_e1, new_handlers, ctx)
    | ExpMatch(e1, handlers, _) ->
        let new_e1 = check_exp e1 env sc in
        let (new_e1typ, new_e1loc) = get_exp_ctx new_e1 in
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
    | DefFun(_) -> e
    | DefVariant(_) -> e
    | DefClass(_) -> raise_typecheck_err eloc "not implemented"
    | DefInterface(_) -> raise_typecheck_err eloc "not implemented"
    | DefExn(_) -> e
    | DefTyp(_) -> e
    | DirImport(_, _) -> e
    | DirImportFrom(_, _, _) -> e)

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
    let env = check_types eseq env in

    (* register exceptions and function declarations *)
    let env = List.fold_left (fun env e ->
        match e with
        | DefFun (df) -> reg_deffun df env sc
        | DefExn (de) -> check_defexn de env sc
        | _ -> env) env eseq in

    (* finally, process everything: function  *)
    let (env, eseq) = List.fold_left (fun (env, eseq) e ->
        try
            match e with
            | DefVal (dv) ->
                let (env, dv, _) = check_defval dv env sc nmap_t.empty in
                (env, DefVal(dv) :: eseq)
            | DefFun (df) ->
                let env = check_deffun df env sc in
                (env, e :: eseq)
            | _ ->
                let e = check_exp e env sc in
                (env, e :: eseq)
        with TypeCheckError(_, _) -> (env, e :: eseq))
        (env, []) eseq in

    check_typecheck_errs();
    (env, List.rev eseq)

and check_directives eseq env sc =
    let is_imported alias n env allow_duplicate_import loc =
        (List.exists(fun entry ->
            match entry with
            | EnvId m ->
                (try
                    let minfo = get_module m in
                    if minfo.dm_name = n then
                        let astr = pp_id2str alias in
                        let mstr = pp_id2str minfo.dm_name in
                        if allow_duplicate_import then true
                        else if astr = mstr then
                            raise_typecheck_err loc (sprintf "duplicate import of %s" mstr)
                        else
                            raise_typecheck_err loc (sprintf "duplicate import of %s as %s" mstr astr)
                    else
                        raise_typecheck_err loc
                            (sprintf "another module %s has been already imported as %s"
                            (pp_id2str minfo.dm_name) (pp_id2str alias))
                with Failure _ -> false)
            | EnvTyp _ -> false) (find_all alias env)) in

    let import_entries env parent_mod key entries loc =
        (List.fold_left (fun env i ->
            match i with
            | EnvId(i) ->
                (let info = id_info i in
                match get_scope info with
                | (ScModule(m) :: _) when parent_mod = noid || parent_mod = m ->
                    add_id_to_env key i env
                | _ -> env)
            | EnvTyp _ -> env) env (List.rev entries)) in

    let import_mod env alias m allow_duplicate_import loc =
        (if is_imported alias m env allow_duplicate_import loc then env
        else
            (* add the imported module id to the env *)
            let env = add_id_to_env alias m env in
            let menv = (get_module m).dm_env in
            (* and also import all the overloaded operators from the module
               to make them usable in the corresponding arithmetic expressions *)
            List.fold_left (fun env op_name ->
                let entries = find_all op_name env in
                import_entries env n op_name entries loc)
            env fname_always_import) in

    let (env, mlist) = (List.fold_left (fun (env, mlist) e ->
        match e with
        | DirImport(impdirs, eloc) ->
            ((List.fold_left (fun env (m, alias) ->
                try
                    import_mod env alias m false eloc
                with TypeCheckError(_, _) -> env) env impdirs), mlist)
        | DirFromImport(m, implist, eloc) ->
            let env =
            (try
                let menv = (get_module m).dm_env in
                let keys = if implist != [] then implist else
                    (Env.fold (fun k ids l -> k :: l) menv []) in
                List.fold_left (fun env k ->
                    try
                        let entries = find_all k menv in
                        let _ = (if ids != [] then () else
                            raise_typecheck_err eloc
                                (sprintf "no symbol %s found in %s" (pp_id2str k) (pp_id2str m))) in
                        import_entries env m k entries false eloc
                    with TypeCheckError(_, _) -> env) env keys
            with TypeCheckError(_, _) -> env) in
            (env, (m, eloc) :: mlist)
        | _ -> (env, mlist)) (env, []) eseq) in

    (* after processing explicit (e.g. specified by user) import directives,
       we also implicitly import each module "m" for which we have "from m import ...".
       Probably, this needs to be revised *)
    let env = List.fold_left (fun env (m, eloc) ->
        let alias = get_orig_id m in
        import_mod env alias m true eloc) env mlist in

    check_typecheck_errs();
    env

(*
    create fresh unique name for each type and register it (put it to env)
*)
and reg_types eseq env sc =
    List.fold_left (fun env e ->
        match e with
        | DefTyp dt ->
            let {dt_name; dt_templ_args; dt_typ; dt_loc} = !dt in
            let dt_name1 = dup_id dt_name in
            dt := {dt_name=dt_name1; dt_templ_args; dt_typ; dt_scope=sc; dt_finalized=false; dt_loc};
            set_id_entry dt_name1 (IdTyp dt);
            add_id_to_env_check dt_name dt_name1 env (check_for_duplicate_typ dt_name sc dt_loc)
        | DefVariant dvar ->
            let { dvar_name; dvar_templ_args; dvar_typ;
                  dvar_flags; dvar_members;
                  dvar_constr; dvar_loc } = !dvar in
            let dvar_name1 = dup_id dvar_name in
            let dvar_typ1 = TypApp([], dvar_name1) in
            dvar := {dvar_name=dvar_name1; dvar_templ_args; dvar_typ=dvar_typ1; dvar_flags; dvar_members;
            dvar_constr; dvar_scope=sc; dvar_templ_inst=[]; dvar_loc};
            set_id_entry dvar_name1 (IdVariant dvar);
            add_id_to_env_check dvar_name dvar_name1 env (check_for_duplicate_typ dvar_name sc dvar_loc)
        | DefClass {contents={dcl_loc=loc}} -> raise_typecheck_err loc "classes are not supported yet"
        | DefInterface {contents={di_loc=loc}} -> raise_typecheck_err loc "interfaces are not supported yet"
        | _ -> env) env eseq

(*
    check the type definition body (for simple types and variant types)
*)
and check_types eseq env sc =
    List.fold_left (fun env e ->
        match e with
        | DefTyp dt ->
            let {dt_name; dt_templ_args; dt_typ; dt_scope; dt_loc} = !dt in
            let env1 = List.fold_left (fun env1 t_arg ->
                add_typ_to_env t_arg (TypApp([], t_arg)) env1) env dt_templ_args in
            let dt_typ1 = finalize_record_typ (check_typ dt_typ env1 dt_scope dt_loc) dt_loc in
            dt := {dt_name; dt_templ_args; dt_typ=dt_typ1; dt_scope; dt_finalized=true; dt_loc};
            env
        | DefVariant dvar ->
            let {dvar_loc} = !dvar in
            let _ = instantiate_variant [] dvar env sc dvar_loc in
            let { dvar_name; dvar_templ_args; dvar_typ; dvar_members; dvar_constr; dvar_scope; dvar_loc } = !dvar in
            List.fold_left2 (fun env (n, t) cn ->
                let {df_name; df_templ_args; df_typ} = !(match (id_info cn) with
                    | IdFun df -> df
                    | _ -> raise_typecheck_err dvar_loc (sprintf "internal error: constructor %s is not a function" (pp_id2str cn))) in
                let (t, _) = preprocess_templ_typ df_templ_args df_typ sc dvar_loc in
                add_id_to_env_check n cn env (check_for_duplicate_fun t env sc dvar_loc)) env dvar_members dvar_constr
        | _ -> env
    )

(*
    * create fresh unique name for the function
    * check its arguments (very light sanity check for template functions;
        full-scale check for non-template function)
    * update the function definition (put in the new name, verified args, verified function type, the correct scope)
    * put the entry into the global symbol table
    * add it into the environment
*)
and reg_deffun df env sc =
    let { df_name; df_templ_args; df_args; df_typ; df_body; df_flags; df_loc } = !df in
    let df_name1 = dup_id df_name in
    let (_, _, df_args1) = List.fold_left (fun (env, idset, df_args1) arg ->
            let arg, idset,
    let df_typ1 = df_typ in
    dt := {dt_name=dt_name1; dt_templ_args; dt_typ; dt_scope=sc; dt_finalized=false; dt_loc};
    set_id_entry dt_name1 (IdTyp dt);
    add_id_to_env_check dt_name dt_name1 env (check_for_duplicate_fun df_typ1 env sc df_loc)

(*
    dvar_flags; dvar_members;
            dvar_constr; dvar_scope; dvar_loc } = !dvar in

            let env1 = List.fold_left (fun env1 t_arg ->
                add_typ_to_env t_arg (TypApp([], t_arg)) env1) env dvar_templ_args in
            let (env2, constr2) = List.fold_left (fun (env2, constr2) (n, t) ->
                let n1 = dup_id n in
                let t1 = check_typ t env1 dvar_scope dvar_loc in




                  { dvar_name: id_t; dvar_templ_args: id_t list; dvar_typ: typ_t;
                  dvar_flags: variant_flag_t list; dvar_members: (id_t * typ_t) list;
                  dvar_constr: id_t list; mutable dvar_templ_inst: id_t list;
                  dvar_scope: scope_t list; dvar_loc: loc_t }
            let dvar_name1 = dup_id dvar_name in
            dvar := {dvar_name=dvar_name1; dvar_templ_args; dvar_typ; dvar_flags; dvar_members;
            dvar_constr; dvar_scope=sc; dvar_templ_inst=[]; dvar_loc};
            set_id_entry dvar_name1 (IdVariant dvar);
            add_id_to_env dvar_name dvar_name1 env true dvar_loc
        | DefClass {contents={dcl_loc=loc}} -> raise_typecheck_err loc "classes are not supported yet"
        | DefInterface {contents={di_loc=loc}} -> raise_typecheck_err loc "interfaces are not supported yet"
        | _ -> env) env eseq
*)
and check_defexn de env sc =
    let { dexn_name=alias; dexn_typ=t; dexn_loc=loc } = !de in
    let t = check_typ t env sc Env.empty loc in
    let n = dup_id alias in
    let _ = (de := { dexn=n; exn_typ=t; dexn_sc=sc; dexn_loc=loc }) in
    let ftyp = typ2constr t TypExn in
    set_id_entry n (IdExn de);
    add_id_to_env_check alias n env (check_for_duplicate_fun ftyp sc dexn_loc)

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
and check_typ t env sc loc =
    let rec check_typ_ t callb =
        (match t with
        | TypApp(ty_args, n) ->
            let ty_args = List.map (fun t -> check_typ_ t callb) ty_args in
            find_first n env loc (fun entry ->
                match entry with
                | EnvTyp(t) ->
                    if ty_args = [] then Some(t) else
                    raise_typecheck_err loc (sprintf "a concrete type (%s) cannot be further instantiated" (pp_id2str n))
                | EnvId(i) ->
                    match (id_info i) with
                    | IdNone | IdText _ | IdVal _ | IdFun _ | IdExn _ | IdModule _ -> None
                    | IdClass _ | IdInterface _ ->
                        (* [TODO] *)
                        raise_typecheck_err loc "classes & interfaces are not supported yet"
                    | IdTyp dt ->
                        let { dt_name; dt_templ_args; dt_typ; dt_scope; dt_finalized; dt_loc } = !dt in
                        let _ = if dt_finalized then () else
                            raise_typecheck_err loc
                            (sprintf "later declared non-variant type %s is referenced; try to reorder the type declarations"
                            (pp_id2str dt_name)) in
                        if dt_name = n && ty_args = [] then
                            t
                        else
                            let env = match_ty_templ_args ty_args dt_templ_args env dt_loc in
                            Some(check_typ dt_typ env sc loc)
                    | IdVariant dvar ->
                        let { dvar_name; dvar_templ_args; dvar_templ_inst; dvar_typ } = !dvar in
                        let t1 = TypApp(ty_args, n1) in
                        if ty_args = [] && dvar_templ_args = [] then
                            Some(t1)
                        else
                            Some(try
                                TypApp([], (List.find (fun inst ->
                                    match (id_info i) with
                                    | IdVariant dvar_inst ->
                                        let {dvar_typ=dvar_inst_typ} = !inst_dvar in
                                        maybe_unify t dvar_inst_typ true
                                    | _ -> raise_typecheck_err loc (sprintf "invalid type of variant instance %s (must be also a variant)" (id2str i)))
                                    dvar_templ_inst))
                            with Not_found ->
                                instantiate_variant ty_args dvar env sc loc))
        | _ -> walk_typ t callb) in
    let callb = { acb_typ=Some(check_typ_); acb_exp=None; acb_pat=None; acb_res=0 } in
    check_typ_ t callb

and instantiate_fun templ_df inst_ftyp inst_env inst_sc inst_loc =
    let { df_name; df_templ_args; df_args; df_body; df_flags; df_scope; df_loc; df_templ_inst } = !templ_df in
    let (args_typs, rt) = (match inst_ftyp with
                    | TypFun(arg_typs, rt) -> (arg_typs, rt)
                    | _ -> raise_typecheck_err inst_loc
                    "internal error: the type of instantiation function is not a function") in
    let inst_name = dup_id df_name in
    let fun_sc = (ScFun inst_name) :: inst_sc in
    let (df_inst_args, inst_env, _) = List.fold_left2
        (fun (df_inst_args, inst_env, id_set) df_arg arg_typ ->
            let (df_inst_arg, inst_env, id_set) = check_pat df_arg arg_typ inst_env id_set fun_sc in
            ((df_inst_arg :: df_inst_args), inst_env, id_set)) ([], inst_env, IdSet.empty) df_args arg_typs in
    let inst_body = check_exp (dup_exp df_body) inst_env fun_sc in
    let (body_typ, body_loc) = get_exp_ctx inst_body in
    let _ = unify body_typ rt body_loc "the function body type does not match the function type" in
    let inst_df = ref { df_name=inst_name; df_templ_args=[]; df_args=df_inst_args;
                        df_typ=(deref_typ inst_ftyp false); df_body=inst_body;
                        df_flags; df_scope; df_loc; df_templ_inst=[]} in
    set_id_entry inst_name (IdFun inst_df);
    !templ_df.df_templ_inst <- inst_name :: !templ_df.df_templ_inst;
    inst_df

and instantiate_variant ty_args dvar env sc loc =
    let { dvar_name; dvar_templ_args; dvar_flags; dvar_members;
          dvar_constr; dvar_templ_inst; dvar_scope; dvar_loc } = !dvar in
    let env = match_ty_templ_args ty_args dvar_templ_args env dvar_loc in
    if ty_args = [] then TypApp([], dvar_name) else
    let members = List.map (fun (n, t) -> n) in

    failwith "instantiate_variant not implemented"

and check_pat pat typ env id_set sc =
    let id_set_r = ref id_set in
    match
    (*
      [TODO]
      * "_" should probably be replaced with a temporary id
      * ident should be checked for the absence of duplicates.
      *
    | PatIdent of id_t * loc_t
    | PatTuple of pat_t list * loc_t
    | PatVariant of id_t * pat_t list * loc_t
    | PatTyped of pat_t * typ_t * loc_t
    *)
    failwith "check_pat not implemented"

and check_handlers handlers inptyp outtyp env sc loc =
    List.map (fun (plist, e) ->
        let case_sc = new_block_scope() :: sc in
        let (plist1, env1, capt1) = List.fold_left (fun (plist1, env1, capt1) p ->
            let (p2, env2, capt2) = check_pat p inptyp env1 capt1 case_sc in
            (p2 :: plist1, env2, capt2)) ([], env, IdSet.empty) plist in
        if not EnvId.is_empty capt1 && (List.length plist1) > 1 then
            raise_typecheck_err "captured variables may not be used in the case of several alternatives"
        else
            let (e1_typ, e1_loc) = get_exp_ctx e in
            unify e1_typ outtyp e1_loc "the case expression type does not match the whole expression type (or the type of previous case(s))";
            ((List.rev plist1), (check_exp e env1 case_sc))
        ) handlers

and check_mod m =
    let minfo = !(get_module m) in
    try
        let modsc = (ScModule m) :: [] in
        let (seq, env) = check_eseq minfo.dm_defs Env.empty modsc false in
        minfo.dm_defs <- seq;
        minfo.dm_env <- env;
    with e ->
        typecheck_errs := e :: !typecheck_errs
