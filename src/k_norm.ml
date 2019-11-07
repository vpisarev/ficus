open Ast
open K_form

let typ2ktyp t loc =
    let rec typ2ktyp_ t = match (Ast_typecheck.deref_typ t) with
    | TypVar {contents=Some(t)} -> typ2ktyp_ t
    | TypVar _ -> raise_compile_err loc "undefined type; use explicit type annotation"
    | TypInt -> KTypInt
    | TypSInt(b) -> KTypSInt(b)
    | TypUInt(b) -> KTypUInt(b)
    | TypFloat(b) -> KTypFloat(b)
    | TypString -> KTypString
    | TypChar -> KTypChar
    | TypBool -> KTypBool
    | TypVoid -> KTypVoid
    | TypExn -> KTypExn
    | TypErr -> KTypErr
    | TypCPointer -> KTypCPointer
    | TypDecl -> KTypVoid (* we will check explicitly that no declaration occurs in the end of each code block *)
    | TypModule -> KTypModule
    | TypList(t) -> KTypList(typ2ktyp_ t )
    | TypTuple(tl) -> KTypTuple(List.map typ2ktyp_ tl)
    | TypRef(t) -> KTypRef(typ2ktyp_ t)
    | TypArray(d, t) -> KTypArray(d, typ2ktyp_ t)
    | TypFun(args, rt) -> KTypFun((List.map typ2ktyp_ args), (typ2ktyp_ rt))
    | TypRecord {contents=(relems, Some(_))} ->
        KTypTuple(List.map (fun (_, t, _) -> typ2ktyp_ t) relems)
    | TypRecord _ ->
        raise_compile_err loc "the record type cannot be inferenced; use explicit type annotation"
    | TypApp(args, n) -> if is_unique_id n then () else
        raise_compile_err loc (sprintf "unknown type name '%s'" (id2str n));
        if args = [] then KTypName(n) else
        (match (Ast_typecheck.deref_typ (Ast_typecheck.check_typ t Env.empty (ScGlobal::[]) loc)) with
        | (TypApp(args1, n1)) as t1 ->
            if args1 = [] then KTypName(n1) else
            (match (id_info n1) with
            | IdVariant {contents={dvar_templ_inst}} ->
                (try
                    let found_inst = List.find (fun inst ->
                        match (id_info inst) with
                        | IdVariant {contents={dvar_alias}} ->
                            Ast_typecheck.maybe_unify t1 dvar_alias false
                        | _ -> raise_compile_err loc
                            (sprintf "instance '%s' of variant '%s' ~ '%s' is not a variant" (id2str inst) (id2str n) (id2str n1)))
                        dvar_templ_inst in
                    KTypName(found_inst)
                with Not_found ->
                    raise_compile_err loc
                        (sprintf "no proper instance of variant '%s' ~ '%s' is found" (id2str n) (id2str n1)))
            | _ -> raise_compile_err loc (sprintf "unsupported type '%s' ~ '%s'" (id2str n) (id2str n1)))
        | t -> typ2ktyp_ t)
    in typ2ktyp_ t

let rec exp2kexp e code lvflag sc =
    let (etyp, eloc) = get_exp_ctx e in
    let ktyp = typ2ktyp etyp eloc in
    let kctx = (ktyp, eloc) in
    match e with
    | ExpNop(loc) -> ((KExpNop loc), code)
    | ExpBreak(loc) -> ((KExpBreak loc), code)
    | ExpContinue(loc) -> ((KExpContinue loc), code)
    | ExpRange(e1_opt, e2_opt, e3_opt, _) ->
        let process_rpart e_opt code defval =
            match e_opt with
            | Some(e) -> exp2atom e code false sc
            | _ -> (defval, code) in
        let (a1, code) = process_rpart e1_opt code (Atom.Lit (LitInt 0L)) in
        let (a2, code) = process_rpart e2_opt code (Atom.Lit LitNil) in
        let (a3, code) = process_rpart e3_opt code (Atom.Lit (LitInt 1L)) in
        (KExpMkTuple(a1 :: a2 :: a3 :: [], kctx), code)
    | ExpLit(lit, _) -> (KExpAtom((Atom.Lit lit), kctx), code)
    | ExpIdent(n, _) -> (KExpAtom((Atom.Id n), kctx), code)
    | ExpBinOp(bop, e1, e2, _) ->
        let (a1, code) = exp2atom e1 code false sc in
        let (a2, code) = exp2atom e2 code false sc in
        (KExpBinOp(bop, a1, a2, kctx), code)
    | ExpUnOp(uop, e1, _) ->
        let (a1, code) = exp2atom e1 code false sc in
        (KExpUnOp(uop, a1, kctx), code)
    (*| ExpSeq(elist, _) ->
        let
    | ExpMkTuple of exp_t list * ctx_t
    | ExpMkArray of exp_t list list * ctx_t
    | ExpMkRecord of exp_t * (id_t * exp_t) list * ctx_t
    | ExpUpdateRecord of exp_t * (id_t * exp_t) list * ctx_t
    | ExpCall of exp_t * exp_t list * ctx_t
    | ExpAt of exp_t * exp_t list * ctx_t
    | ExpAssign of exp_t * exp_t * loc_t
    | ExpMem of exp_t * exp_t * ctx_t
    | ExpDeref of exp_t * ctx_t
    | ExpMakeRef of exp_t * ctx_t
    | ExpThrow of exp_t * loc_t
    | ExpIf of exp_t * exp_t * exp_t * ctx_t
    | ExpWhile of exp_t * exp_t * loc_t
    | ExpDoWhile of exp_t * exp_t * loc_t
    | ExpFor of (pat_t * exp_t) list * exp_t * for_flag_t list * loc_t
    | ExpMap of ((pat_t * exp_t) list * exp_t option) list * exp_t * for_flag_t list * ctx_t
    | ExpTryCatch of exp_t * (pat_t list * exp_t) list * ctx_t
    | ExpMatch of exp_t * (pat_t list * exp_t) list * ctx_t
    | ExpCast of exp_t * typ_t * ctx_t
    | ExpTyped of exp_t * typ_t * ctx_t
    | ExpCCode of string * ctx_t
    | DefVal of pat_t * exp_t * val_flag_t list * loc_t
    | DefFun of deffun_t ref
    | DefExn of defexn_t ref
    | DefTyp of deftyp_t ref
    | DefVariant of defvariant_t ref
    | DefClass of defclass_t ref
    | DefInterface of definterface_t ref
    | DirImport of (id_t * id_t) list * loc_t
    | DirImportFrom of id_t * id_t list * loc_t*)
    | _ -> raise_compile_err eloc "unsupported operator"

and exp2atom e code lvflag sc =
    let (e, code) = exp2kexp e code lvflag sc in
    let (t, eloc) = get_kexp_kctx e in
    match (t, e) with
    | (KTypVoid, _) -> raise_compile_err eloc "no-value operator or declaration cannot be represented as an atom"
    | (_, KExpAtom(a, _)) -> (a, code)
    | (_, _) ->
        let kv_name = get_temp_idk "v" in
        let dv = { kv_name; kv_typ=t; kv_scope=sc; kv_loc=eloc;
                   kv_flags=(if lvflag then ValTempRef :: [] else []) } in
        set_idk_entry kv_name (KVal dv);
        ((Atom.Id kv_name), (KDefVal (kv_name, e, eloc)) :: code)

let normalize_mod m = ([]: kexp_t list)
