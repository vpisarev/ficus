open Ast
open K_form

let deref_typ = Ast_typecheck.deref_typ
let zero_env = (Env.empty : env_t)

let typ2ktyp t loc =
    let rec typ2ktyp_ t = match (deref_typ t) with
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
    | TypApp(args, n) ->
        if is_unique_id n then () else
            raise_compile_err loc (sprintf "unknown type name '%s'" (id2str n));
        if args = [] then KTypName(n) else
        (match (deref_typ (Ast_typecheck.check_typ t zero_env (ScGlobal::[]) loc)) with
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

let filter_out_nops code =
    List.filter (fun e -> match e with
        | KExpNop _ -> false
        | _ -> true) code

let code2kexp code loc = match (filter_out_nops code) with
    | [] -> KExpNop(loc)
    | e :: [] -> e
    | e :: rest ->
        let t = get_kexp_ktyp e in
        KExpSeq((List.rev code), (t, loc))

let rec exp2kexp e code tref sc =
    let (etyp, eloc) = get_exp_ctx e in
    let ktyp = typ2ktyp etyp eloc in
    let kctx = (ktyp, eloc) in
    (*
        scans through (pi, ei) pairs in for(p1<-e1, p2<-e2, ..., pn<-en) operators;
        * updates the code that needs to be put before the for loop
          (all the ei needs to be computed there),
        * generates the pattern unpack code, which should be put into the beginning of the loop body
        * generates the proxy identifiers that are used in the corresponding KExpFor/KExpMap.
        For example, the following code:
            for ((r, g, b) <- GaussianBlur(img)) { ... }
        is converted to
            val temp123 = GaussianBlur(img)
            for (i124 <- temp123) { val r=i124.0, g=i124.1, b = i124.2; ... }
    *)
    let transform_for pe_l code sc body_sc =
        let (idom_list, code, body_code) =
            List.fold_left (fun (idom_list, code, body_code) (pi, ei) ->
                let (etyp, eloc) = get_exp_ctx ei in
                let (di, code) = exp2dom ei code sc in
                let ptyp = match (di, etyp) with
                    | (Domain.Range _, _) -> TypInt
                    | (_, TypArray(_, et)) -> et
                    | (_, TypList(et)) -> et
                    | (_, TypString) -> TypChar
                    | _ -> raise_compile_err eloc "unsupported domain expression in for loop" in
                let (i, body_code) = pat_simple_unpack pi ptyp None body_code "i" [] body_sc
                in ((i, di) :: idom_list, code, body_code))
            ([], code, []) pe_l in
        ((List.rev idom_list), code, body_code) in
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
    | ExpSeq(eseq, _) ->
        let sc = new_block_scope() :: sc in
        let code = eseq2code eseq code sc in
        (match code with
        | c :: code -> (c, code)
        | _ -> ((KExpNop eloc), code))
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
        (KExpDeref(a_id, kctx), code)
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
    | ExpFor(pe_l, body, flags, _) ->
        let body_sc = new_block_scope() :: sc in
        let (idom_list, code, body_code) = transform_for pe_l code sc body_sc in
        let (e, body_code) = exp2kexp body body_code false body_sc in
        let bloc = get_exp_loc body in
        let body_kexp = code2kexp body_code bloc in
        (KExpFor(idom_list, body_kexp, flags, eloc), code)
    | ExpDoWhile(e1, e2, _) ->
        let (e1, code1) = exp2kexp e1 [] false sc in
        let (e2, code2) = exp2kexp e2 (e1 :: code1) false sc in
        let body = code2kexp code2 eloc in
        (KExpDoWhile(body, e2, eloc), code)
    | ExpAt(e, idxlist, _) ->
        let (dlist, code) = List.fold_left (fun (dlist, code) idx ->
            let (d, code) = exp2dom idx code sc in
            (d :: dlist, code)) ([], code) idxlist in
        let (arr, code) = exp2atom e code true sc in
        (KExpAt(arr, (List.rev dlist), kctx), code)
    | ExpMem(e1, elem, _) ->
        let (e1typ, e1loc) = get_exp_ctx e1 in
        let (a, code) = exp2atom e1 code true sc in
        let a_id = atom2id a e1loc "a literal is not allowed here" in
        let i = (match ((deref_typ e1typ), elem) with
                | (TypTuple(tl), ExpLit((LitInt i_), (ityp, iloc))) ->
                    let i = Int64.to_int i_ in
                    let n = List.length tl in
                    if 0 <= i && i < n then () else
                        raise_compile_err iloc (sprintf "the tuple index is outside of the range [0, %d)" n);
                    i
                | (TypRecord {contents=(relems, Some(rn))}, ExpIdent(n, (_, nloc))) ->
                    let (i, j) = List.fold_left (fun (i, j) (ni, _, _) ->
                        if n = ni then (j, j+1) else (i, j+1)) (-1, 0) relems in
                    if i >= 0 then i else raise_compile_err nloc
                        (sprintf "there is no record field '%s' in the record '%s'" (id2str n) (id2str rn))
                | (TypRecord _, _) ->
                    raise_compile_err e1loc "accessing an unknown record. Please, use explicit type specification"
                | (_, _) ->
                    raise_compile_err e1loc "unsupported access operation") in
        (KExpMem(a_id, i, kctx), code)
    | ExpAssign(e1, e2, _) ->
        let (e2, code) = exp2kexp e2 code true sc in
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
    | DefVal(p, e2, flags, _) ->
        let (e2, code) = exp2kexp e2 code true sc in
        let (v, code) = pat_simple_unpack p etyp (Some e2) code "v" flags sc in
        (*  if pat_simple_unpack returns (noid, code), it means that the pattern p does
            not contain variables to capture, i.e. user wrote something like
                val _ = <exp> or
                val (_, (_, _)) = <exp> etc.,
            which means that the assignment was not generated, but we need to retain <exp>,
            because it likely has some side effects *)
        if v = noid then (e2, code) else ((KExpNop eloc), code)
    | DefFun df ->
        let code = transform_fun df code sc in ((KExpNop eloc), code)
    | DefTyp _ -> (KExpNop(eloc), code)
    | DefVariant _ -> (KExpNop(eloc), code) (* variant declarations are handled in batch in transform_all_types_and_cons *)
    | DefExn _ -> (KExpNop(eloc), code) (* exception declarations are handled in batch in transform_all_types_and_cons *)
    | DefClass _ -> raise_compile_err eloc "classes are not supported yet"
    | DefInterface _ -> raise_compile_err eloc "interfaces are not supported yet"
    | DirImport _ -> (KExpNop(eloc), code)
    | DirImportFrom _ -> (KExpNop(eloc), code)
    (*| ExpMap of ((pat_t * exp_t) list * exp_t option) list * exp_t * for_flag_t list * ctx_t
    | ExpTryCatch of exp_t * (pat_t list * exp_t) list * ctx_t
    | ExpMatch of exp_t * (pat_t list * exp_t) list * ctx_t
    *)
    | _ -> raise_compile_err eloc "unsupported operator"

and create_val name t flags sc loc =
    let dv = { kv_name=name; kv_typ=t; kv_flags=flags; kv_scope=sc; kv_loc=loc } in
    set_idk_entry name (KVal dv)

and exp2atom e code tref sc =
    let (e, code) = exp2kexp e code tref sc in
    let (t, eloc) = get_kexp_kctx e in
    match (t, e) with
    | (KTypVoid, _) -> raise_compile_err eloc "no-value operator or declaration cannot be represented as an atom"
    | (_, KExpAtom(a, _)) -> (a, code)
    | (_, _) ->
        let kv_name = gen_temp_idk "v" in
        create_val kv_name t (if tref then ValTempRef :: [] else []) sc eloc;
        ((Atom.Id kv_name), (KDefVal (kv_name, e, eloc)) :: code)

and exp2dom e code sc =
    match e with
    | ExpRange _ ->
        let (ek, code) = exp2kexp e code false sc in
        (match ek with
        | KExpMkTuple(a :: b :: c :: [], _) -> (Domain.Range(a, b, c), code)
        | _ -> raise_compile_err (get_exp_loc e) "the range was not converted to a 3-element tuple as expected")
    | _ ->
        let (i, code) = exp2atom e code false sc in
        (Domain.Elem i, code)

and eseq2code eseq code sc =
    let code = transform_all_types_and_cons eseq code sc in
    let rec knorm_eseq eseq code = (match eseq with
        | ei :: rest ->
            let (eki, code) = exp2kexp ei code false sc in
            let code = (match eki with
                | KExpNop _ -> code
                | _ -> eki :: code) in
            knorm_eseq rest code
        | [] -> code) in
    knorm_eseq eseq code

(* finds if the pattern contains variables to capture. We could have
   combined this and the next function into one, but then we would
   have to scan the whole pattern (or need more complex code
   to do early exit).
   Besides, most of the time (for value declarations, loop iteration variables,
   function arguments ...) we know already that a pattern does not need checks,
   so we just need have_variables for it *)
and pat_have_vars p = match p with
    | PatAny _ | PatLit _ -> false
    | PatIdent _ | PatAs _ -> true
    | PatCons(p1, p2, _) -> (pat_have_vars p1) || (pat_have_vars p2)
    | PatTyped(p, _, _) -> pat_have_vars p
    | PatTuple(pl, _) -> List.exists pat_have_vars pl
    | PatVariant(_, pl, _) -> List.exists pat_have_vars pl
    | PatRec(_, ip_l, _) -> List.exists (fun (_, pi) -> pat_have_vars pi) ip_l

and pat_needs_checks p = match p with
    | PatAny _ | PatIdent _ | PatAs _ -> false
    | PatLit _ -> true
    | PatCons(p1, p2, _) -> (pat_needs_checks p1) || (pat_needs_checks p2)
    | PatTyped(p, _, _) -> pat_needs_checks p
    | PatTuple(pl, _) -> List.exists pat_needs_checks pl
    | PatVariant(_, pl, _) ->
        (* [TODO] return true if variant tag needs to be checked *)
        List.exists pat_needs_checks pl
    | PatRec(_, ip_l, _) ->
        (* [TODO] return true if the record tag needs to be checked *)
        List.exists (fun (_, pi) -> pat_needs_checks pi) ip_l

and pat_skip_typed p = match p with
    | PatTyped(p, _, _) -> pat_skip_typed p
    | _ -> p

and pat_simple_propose_id p ptyp preferred_temp_prefix mutable_leaves sc =
    let p = pat_skip_typed p in
    match p with
    | PatAny _ -> (p, noid, false)
    | PatIdent(n, _) -> (p, n, false)
    | PatAs(p, n, ploc) ->
        if mutable_leaves then
            raise_compile_err ploc "'as' pattern cannot be used with var's, only with values"
        else ();
        ((pat_skip_typed p), n, true)
    | _ ->
        if (pat_have_vars p) then (p, (gen_temp_idk preferred_temp_prefix), true)
        else (p, noid, false)

and pat_simple_unpack p ptyp e_opt code preferred_temp_prefix flags sc =
    let mutable_leaves = List.mem ValMutable flags in
    let flags = List.filter (fun f -> f != ValMutable && f != ValTempRef) flags in
    let (p, n, tref) = pat_simple_propose_id p ptyp preferred_temp_prefix mutable_leaves sc in
    if n = noid then (n, code) else
    let loc = get_pat_loc p in
    let flags = if mutable_leaves && not tref then ValMutable :: flags
                else if tref then ValTempRef :: flags else flags in
    let ktyp = match e_opt with Some(e) -> get_kexp_ktyp e | _ -> typ2ktyp ptyp loc in
    let _ = create_val n ktyp flags sc loc in
    let code = match e_opt with
        | Some(e) -> KDefVal(n, e, loc) :: code
        | _ -> code in
    let code =
    (match p with
    | PatTuple(pl, loc) ->
        let tl = match ptyp with
                | TypTuple(tl) ->
                    if (List.length tl) != (List.length pl) then
                        raise_compile_err loc "the number of elements in the pattern and in the tuple type are different"
                    else
                        tl
                | _ -> raise_compile_err loc "invalid type of the tuple pattern (it must be a tuple as well)" in
        let (_, code) = List.fold_left2 (fun (idx, code) pi ti ->
            let loci = get_pat_loc pi in
            let ei = KExpMem(n, idx, ((typ2ktyp ti loci), loci)) in
            let (_, code) = pat_simple_unpack pi ti (Some ei) code preferred_temp_prefix flags sc in
            (idx + 1, code)) (0, code) pl tl in
        code
    | PatIdent _ -> code
    | PatVariant(_, _, loc) ->
        raise_compile_err loc "variant patterns are not supported yet"
    | PatRec(_, ip_l, loc) ->
        raise_compile_err loc "record patterns are not supported yet"
    | PatAs _ ->
        let e = KExpAtom(Atom.Id n, (ktyp, loc)) in
        let (_, code) = pat_simple_unpack p ptyp (Some e) code preferred_temp_prefix flags sc in
        code
    | _ ->
        printf "pattern: "; Ast_pp.pprint_pat_x p; printf "\n";
        raise_compile_err loc "this type of pattern cannot be used here") in
    (n, code)

and transform_fun df code sc =
    let {df_name; df_templ_args; df_templ_inst; df_loc} = !df in
    let inst_list = if df_templ_args = [] then df_name :: [] else df_templ_inst in
    List.fold_left (fun code inst ->
        match (id_info inst) with
        | IdFun {contents={df_name=inst_name; df_args=inst_args;
            df_typ=inst_typ; df_body=inst_body; df_flags=inst_flags; df_loc=inst_loc}} ->
            let (argtyps, rt) = match inst_typ with
                | TypFun(argtyps, rt) -> (argtyps, rt)
                | _ -> raise_compile_err inst_loc
                    (sprintf "the type of non-constructor function '%s' should be TypFun(_,_)" (id2str inst_name)) in
            let ktyp = typ2ktyp inst_typ df_loc in
            let nargs = List.length inst_args in
            let nargtypes = List.length argtyps in
            let _ = if nargs = nargtypes then () else
                raise_compile_err inst_loc
                    (sprintf "the number of argument patterns (%d) and the number of argument types (%d) do not match"
                    nargs nargtypes) in
            let body_sc = new_block_scope() :: sc in
            let (argids, body_code) = List.fold_left2 (fun (argids, body_code) pi ti ->
                let (i, body_code) = pat_simple_unpack pi ti None body_code "arg" [] body_sc in
                (i :: argids, body_code)) ([], []) inst_args argtyps in
            let body_loc = get_exp_loc inst_body in
            let (e, body_code) = exp2kexp inst_body body_code false body_sc in
            let kf = ref { kf_name=inst_name; kf_typ=ktyp; kf_args=(List.rev argids);
                kf_body=(code2kexp (e :: body_code) body_loc); kf_flags=inst_flags; kf_scope=sc; kf_loc=inst_loc } in
            set_idk_entry inst_name (KFun kf);
            KDefFun kf :: code
        | i -> raise_compile_err (get_idinfo_loc i)
            (sprintf "the entry '%s' (an instance of '%s'?) is supposed to be a function, but it's not" (id2str inst) (id2str df_name)))
    code inst_list

and transform_all_types_and_cons elist code sc =
    List.fold_left (fun code e -> match e with
        | DefVariant {contents={dvar_name; dvar_templ_args; dvar_templ_inst; dvar_loc}} ->
            let inst_list = if dvar_templ_args = [] then dvar_name :: [] else dvar_templ_inst in
            List.fold_left (fun code inst ->
                match (id_info inst) with
                | IdVariant {contents={dvar_name=inst_name; dvar_cases; dvar_constr; dvar_flags; dvar_scope; dvar_loc=inst_loc}} ->
                    let kvar = ref { kvar_name=inst_name; kvar_cases=List.map (fun (i, t) -> (i, typ2ktyp t inst_loc)) dvar_cases;
                                     kvar_constr=dvar_constr; kvar_flags=dvar_flags; kvar_scope=sc; kvar_loc=inst_loc } in
                    let _ = set_idk_entry inst_name (KVariant kvar) in
                    let code = (KDefVariant kvar) :: code in
                    List.fold_left (fun code constr ->
                        match (id_info constr) with
                        | IdFun {contents={df_name; df_typ}} ->
                            let kf_typ=(typ2ktyp df_typ dvar_loc) in
                            let argtypes = match kf_typ with
                                       | KTypFun(argtypes, _) -> argtypes
                                       | _ -> [] in
                            let kf = ref { kf_name=df_name; kf_typ=(typ2ktyp df_typ dvar_loc); kf_args=List.map (fun _ -> noid) argtypes;
                                           kf_body=KExpNop(dvar_loc); kf_flags=FunConstr :: []; kf_scope=sc; kf_loc=dvar_loc } in
                            set_idk_entry df_name (KFun kf);
                            (KDefFun kf) :: code
                        | _ -> raise_compile_err dvar_loc
                            (sprintf "the constructor '%s' of variant '%s' is not a function apparently" (id2str constr) (id2str inst)))
                    code dvar_constr
                | _ -> raise_compile_err dvar_loc
                        (sprintf "the instance '%s' of variant '%s' is not a variant" (id2str inst) (id2str dvar_name)))
            code inst_list
        | DefExn {contents={dexn_name; dexn_typ; dexn_loc}} ->
            let ke = ref { ke_name=dexn_name; ke_typ=(typ2ktyp dexn_typ dexn_loc); ke_scope=sc; ke_loc=dexn_loc } in
            set_idk_entry dexn_name (KExn ke);
            (KDefExn ke) :: code
        | _ -> code) code elist

let normalize_mod m =
    let minfo = !(get_module m) in
    let modsc = (ScModule m) :: [] in
    eseq2code (minfo.dm_defs) [] modsc
