open Ast
open K_form

let zero_env = (Env.empty : env_t)

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
        (match (Ast_typecheck.deref_typ (Ast_typecheck.check_typ t zero_env (ScGlobal::[]) loc)) with
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

let atom2id a loc msg = match a with
    | Atom.Id i -> i
    | Atom.Lit _ -> raise_compile_err loc msg

let code2kexp code loc = match code with
    | [] -> KExpNop(loc)
    | e :: [] -> e
    | e :: rest ->
        let t = get_kexp_ktyp e in
        KExpSeq((List.rev code), (t, loc))

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
    | ExpSeq(elist, _) ->
        let rec knorm_eseq elist code = match elist with
            | ei :: rest ->
                let (eki, code) = exp2kexp ei code false sc in
                if rest = [] then (eki, code) else
                let code = (match eki with
                      KExpNop _ -> code
                    | _ -> eki :: code) in
                knorm_eseq rest code
            | [] -> ((KExpNop eloc), code) in
        knorm_eseq elist code
    | ExpMkTuple(args, _) ->
        let (args, code) = List.fold_left (fun (args, code) ei ->
                let (ai, code) = exp2atom ei code false sc in
                (ai :: args, code)) ([], code) args in
        (KExpMkTuple((List.rev args), kctx), code)
    | ExpMkArray(erows, _) ->
        let nrows = List.length erows in
        if nrows = 0 then raise_compile_err eloc "empty arrays are not supported" else
        let ncols = List.length (List.hd erows) in
        if ncols = 0 then raise_compile_err eloc "empty arrays are not supported" else
        let (elems, code) = List.fold_left (fun (elems, code) erow ->
            List.fold_left (fun (elems, code) e ->
                let (a, code) = exp2atom e code false sc in (a :: elems, code))
            (elems, code) erow) ([], code) erows in
        let shape = if nrows = 1 then ncols :: [] else nrows :: ncols :: [] in
        (KExpMkArray(shape, (List.rev elems), kctx), code)
    | ExpMkRecord (rn, relems, _) -> (* [TODO] *) raise_compile_err eloc "records are not supported yet"
    | ExpUpdateRecord(e, relems, _) -> (* [TODO] *) raise_compile_err eloc "records are not supported yet"
    | ExpCall(f, args, _) ->
        let (f_a, code) = exp2atom f code false sc in
        let f_id = atom2id f_a (get_exp_loc f) "a function name cannot be a literal" in
        let (args, code) = List.fold_left (fun (args, code) ei ->
            let (ai, code) = exp2atom ei code false sc in (ai :: args, code)) ([], code) args in
        (KExpCall(f_id, (List.rev args), kctx), code)
    | ExpDeref(e, _) ->
        let (a, code) = exp2atom e code false sc in
        let a_id = atom2id a (get_exp_loc e) "a literal cannot be dereferenced" in
        (KExpDeref(a_id, lvflag, kctx), code)
    | ExpMakeRef(e, _) ->
        let (a, code) = exp2atom e code false sc in
        (KExpIntrin(IntrinMkRef, a :: [], kctx), code)
    | ExpThrow(e, _) ->
        let (a, code) = exp2atom e code false sc in
        let a_id = atom2id a (get_exp_loc e) "a literal cannot be thrown as exception" in
        (KExpThrow(a_id, eloc), code)
    | ExpIf(e1, e2, e3, _) ->
        let (c, code) = exp2kexp e1 code false sc in
        let loc2 = get_exp_loc e2 in
        let loc3 = get_exp_loc e3 in
        let (e2, code2) = exp2kexp e2 [] false sc in
        let (e3, code3) = exp2kexp e3 [] false sc in
        let if_then = code2kexp (e2 :: code2) loc2 in
        let if_else = code2kexp (e3 :: code3) loc3 in
        (KExpIf(c, if_then, if_else, kctx), code)
    | ExpWhile(e1, e2, _) ->
        let loc1 = get_exp_loc e1 in
        let loc2 = get_exp_loc e2 in
        let (e1, code1) = exp2kexp e1 [] false sc in
        let (e2, code2) = exp2kexp e2 [] false sc in
        let c = code2kexp (e1 :: code1) loc1 in
        let body = code2kexp (e1 :: code2) loc2 in
        (KExpWhile(c, body, eloc), code)
    | ExpDoWhile(e1, e2, _) ->
        let (e1, code1) = exp2kexp e1 [] false sc in
        let (e2, code2) = exp2kexp e2 (e1 :: code1) false sc in
        let body = code2kexp code2 eloc in
        (KExpDoWhile(body, e2, eloc), code)
    | ExpAt(e, idxlist, _) ->
        let (dlist, code) = List.fold_left (fun (dlist, code) idx ->
            match idx with
            | ExpRange _ ->
                let (dom, code) = range2dom idx code sc in
                (dom :: dlist, code)
            | _ ->
                let (i, code) = exp2atom idx code false sc in
                ((Domain.Elem i) :: dlist, code)) ([], code) idxlist in
        let (arr, code) = exp2atom e code true sc in
        (KExpAt(arr, (List.rev dlist), lvflag, kctx), code)
    | ExpAssign(e1, e2, _) ->
        let (e2, code) = exp2kexp e2 code false sc in
        let (a, code) = exp2atom e1 code true sc in
        let a_id = atom2id a (get_exp_loc e1) "a literal cannot be assigned" in
        (KExpAssign(a_id, e2, eloc), code)
    | ExpCast(e, t, _) ->
        let (a, code) = exp2atom e code false sc in
        let t = typ2ktyp t eloc in
        (KExpCast(a, t, kctx), code)
    | ExpTyped(e, t, _) ->
        let (a, code) = exp2atom e code false sc in
        let t = typ2ktyp t eloc in
        (KExpAtom(a, (t, eloc)), code)
    | ExpCCode(s, _) -> (KExpCCode(s, kctx), code)
    | DefTyp _ -> (KExpNop(eloc), code)
    | DefClass _ -> raise_compile_err eloc "classes are not supported yet"
    | DefInterface _ -> raise_compile_err eloc "interfaces are not supported yet"
    | DirImport _ -> (KExpNop(eloc), code)
    | DirImportFrom _ -> (KExpNop(eloc), code)

    (*| ExpMem of exp_t * exp_t * ctx_t
    | ExpFor of (pat_t * exp_t) list * exp_t * for_flag_t list * loc_t
    | ExpMap of ((pat_t * exp_t) list * exp_t option) list * exp_t * for_flag_t list * ctx_t
    | ExpTryCatch of exp_t * (pat_t list * exp_t) list * ctx_t
    | ExpMatch of exp_t * (pat_t list * exp_t) list * ctx_t
    | DefVal of pat_t * exp_t * val_flag_t list * loc_t
    | DefFun of deffun_t ref
    | DefExn of defexn_t ref
    | DefVariant of defvariant_t ref
    *)
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

and range2dom re code sc =
    let (rk, code) = exp2kexp re code false sc in
    match rk with
    | KExpMkTuple(a :: b :: c :: [], _) -> (Domain.Range(a, b, c), code)
    | _ -> raise_compile_err (get_exp_loc re) "the range was not converted to a 3-element tuple as expected"

let normalize_mod m = ([]: kexp_t list)
