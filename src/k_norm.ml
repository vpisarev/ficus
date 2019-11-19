open Ast
open K_form

(* the data type used for pattern matching transformation *)
type pat_info_t = { pinfo_p: pat_t; pinfo_typ: ktyp_t; pinfo_e: kexp_t; pinfo_tag: id_t }

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
    | TypRecord {contents=(relems, Some(rn))} ->
        KTypRecord(rn, List.map (fun (n, t, _) -> (n, (typ2ktyp_ t))) relems)
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
            val temp@@105 = GaussianBlur(img)
            for (i@@105 <- temp@@123) { val r=i@@105.0, g=i@@105.1, b = i@@105.2; ... }
    *)
    let transform_for pe_l code sc body_sc =
        let (idom_list, code, body_code) =
            List.fold_left (fun (idom_list, code, body_code) (pi, ei) ->
                let (di, code) = exp2dom ei code sc in
                let ptyp = match di with
                    | Domain.Range _ -> KTypInt
                    | Domain.Fast i | Domain.Elem i ->
                        match (get_atom_ktyp i) with
                        | KTypArray(_, et) -> et
                        | KTypList(et) -> et
                        | KTypString -> KTypChar
                        | _ -> raise_compile_err eloc "unsupported typ of the domain expression in for loop" in
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
    | ExpMkRecord (rn, rinitelems, _) ->
        let relems = match (deref_typ etyp) with
            | TypRecord {contents=(relems, Some(_))} -> relems
            | TypRecord _ -> raise_compile_err eloc
                "the record type cannot be inferenced. Please, use explicit type specification"
            | _ -> raise_compile_err eloc
                "the record construction expression should return a record type" in
        let (ratoms, code) = List.fold_left (fun (ratoms, code) (ni, ti, opt_vi) ->
            let (a, code) = try
                let (_, ej) = List.find (fun (nj, ej) -> ni = nj) rinitelems in
                exp2atom ej code false sc
            with Not_found ->
                (match opt_vi with
                | Some(vi) -> ((Atom.Lit vi), code)
                | _ -> raise_compile_err eloc
                    (sprintf
                    "there is no explicit inializer for the field '%s' nor there is default initializer for it"
                    (id2str ni)))
            in (a::ratoms, code)) ([], code) relems in
        (KExpMkRecord((List.rev ratoms), kctx), code)
    | ExpUpdateRecord(e, rupdelems, _) ->
        let (rec_n, code) = exp2id e code true sc "the updated record cannot be a literal" in
        let relems = match ktyp with
            | KTypRecord (rn, relems) -> relems
            | _ -> raise_compile_err eloc
                "the record construction expression should return a record type" in
        let (_, ratoms, code) = List.fold_left (fun (idx, ratoms, code) (ni, ti) ->
            let (a, code) = try
                let (_, ej) = List.find (fun (nj, ej) -> ni = nj) rupdelems in
                exp2atom ej code false sc
            with Not_found ->
                let ni_ = dup_idk ni in
                let get_ni = KExpMem(rec_n, idx, (ti, eloc)) in
                let code = create_defval ni_ ti (ValTempRef :: []) (Some get_ni) code sc eloc in
                ((Atom.Id ni_), code)
            in (idx + 1, a::ratoms, code)) (0, [], code) relems in
        (KExpMkRecord((List.rev ratoms), kctx), code)
    | ExpCall(f, args, _) ->
        let (f_id, code) = exp2id f code false sc "a function name cannot be a literal" in
        let (args, code) = List.fold_left (fun (args, code) ei ->
            let (ai, code) = exp2atom ei code false sc in (ai :: args, code)) ([], code) args in
        (KExpCall(f_id, (List.rev args), kctx), code)
    | ExpDeref(e, _) ->
        let (a_id, code) = exp2id e code false sc "a literal cannot be dereferenced" in
        (KExpDeref(a_id, kctx), code)
    | ExpMakeRef(e, _) ->
        let (a, code) = exp2atom e code false sc in
        (KExpIntrin(IntrinMkRef, a :: [], kctx), code)
    | ExpThrow(e, _) ->
        let (a_id, code) = exp2id e code false sc "a literal cannot be thrown as exception" in
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
    | ExpFor(pe_l, body, flags, _) ->
        let body_sc = new_block_scope() :: sc in
        let (idom_list, code, body_code) = transform_for pe_l code sc body_sc in
        let (last_e, body_code) = exp2kexp body body_code false body_sc in
        let bloc = get_exp_loc body in
        let body_kexp = code2kexp (last_e :: body_code) bloc in
        (KExpFor(idom_list, body_kexp, flags, eloc), code)
    | ExpMap(pew_ll, body, flags, _) ->
        (*
            process the nested for clauses. since there can be non-trivial patterns
            and non-trivial iteration domain expressions, transform_for will produce
            some "pre_code", i.e. the code that needs to be executed before (outside of)
            each clause of the nested loop and also the "body_code" that needs to be
            computed inside the loop (i.e. the pattern unpacking) in the beginning
            before all other expressions. In the case of nested loop in exp-map this
            body_code will actually become the outer code for the nested loop.
            So, it's passed to the next iteration of List.fold_left and will prepend
            the next "pre_code". Finally, the body_code from the last iteration, i.e.
            from the most inner for loop will actually become the prefix of the actual
            body code that is transformed after this List.fold_left.

            In addition, we handle clauses in certain way that is not 100% correct from
            the type consistence point of view, but it's fine and all the subsequent
            optimizations and the C code generator should handle it properly. That is,
            after unpacking the patterns inside loop for each "when <...>" clause we
            insert "if (<...>) {} else continue;" expression, e.g.:

            val upper_triangle_nz_elements = [for (i <- 0:m) for (j <- i:m when A[i,j] != 0) (i,j)]

            will be translated to

            vall odd_elements = [for (i <- 0:m) for (j <-i:m)
                { val temp=A[i,j]; if(temp != 0) {} else continue; (i, j)} ]
        *)
        let body_sc = new_block_scope() :: sc in
        let (pre_idom_ll, body_code) = List.fold_left
            (fun (pre_idom_ll, prev_body_code) (pe_l, when_opt) ->
                let (idom_list, pre_code, body_code) = transform_for pe_l prev_body_code sc body_sc in
                let body_code = match when_opt with
                    | Some(when_e) ->
                        let (e, body_code) = exp2kexp when_e body_code true body_sc in
                        let eloc = get_kexp_loc e in
                        let check_when = KExpIf(e, (KExpNop eloc), (KExpContinue eloc), (KTypVoid, eloc)) in
                        check_when :: body_code
                    | _ -> body_code in
                let (p, _) = List.hd pe_l in
                let ploc = get_pat_loc p in
                let pre_exp = code2kexp pre_code ploc in
                ((pre_exp, idom_list) :: pre_idom_ll, body_code)) ([], []) pew_ll in
        let (last_e, body_code) = exp2kexp body body_code false body_sc in
        let bloc = get_exp_loc body in
        let body_kexp = code2kexp (last_e :: body_code) bloc in
        (KExpMap(pre_idom_ll, body_kexp, flags, kctx), code)
    | ExpAt(e, idxlist, _) ->
        let (dlist, code) = List.fold_left (fun (dlist, code) idx ->
            let (d, code) = exp2dom idx code sc in
            (d :: dlist, code)) ([], code) idxlist in
        let (arr, code) = exp2atom e code true sc in
        (KExpAt(arr, (List.rev dlist), kctx), code)
    | ExpMem(e1, elem, _) ->
        let e1loc = get_exp_loc e1 in
        let (a_id, code) = exp2id e1 code true sc "the literal does not have members to access" in
        let ktyp = get_id_ktyp a_id in
        let i = (match (ktyp, elem) with
                | (KTypTuple(tl), ExpLit((LitInt i_), (ityp, iloc))) ->
                    let i = Int64.to_int i_ in
                    let n = List.length tl in
                    if 0 <= i && i < n then () else
                        raise_compile_err iloc (sprintf "the tuple index is outside of the range [0, %d)" n);
                    i
                | (KTypRecord(rn, relems), ExpIdent(n, (_, nloc))) ->
                    let (i, j) = List.fold_left (fun (i, j) (ni, _) ->
                        if n = ni then (j, j+1) else (i, j+1)) (-1, 0) relems in
                    if i >= 0 then i else raise_compile_err nloc
                        (sprintf "there is no record field '%s' in the record '%s'" (id2str n) (id2str rn))
                | (_, _) ->
                    raise_compile_err e1loc "unsupported access operation") in
        (KExpMem(a_id, i, kctx), code)
    | ExpAssign(e1, e2, _) ->
        let (e2, code) = exp2kexp e2 code true sc in
        let (a_id, code) = exp2id e1 code true sc "a literal cannot be assigned" in
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
    | ExpMatch(e1, handlers, _) ->
        let (a, code) = exp2atom e1 code false sc in
        let (k_handlers, code) = transform_pat_matching a handlers code sc eloc in
        (KExpMatch(k_handlers, kctx), code)
    | DefVal(p, e2, flags, _) ->
        let (e2, code) = exp2kexp e2 code true sc in
        let ktyp = get_kexp_ktyp e2 in
        let (v, code) = pat_simple_unpack p ktyp (Some e2) code "v" flags sc in
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
    (*
    | ExpTryCatch of exp_t * (pat_t list * exp_t) list * ctx_t
    *)
    | _ -> raise_compile_err eloc "unsupported operator"

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

and exp2id e code tref sc msg =
    let (a, code) = exp2atom e code tref sc in
    let i = (match a with
    | Atom.Id i -> i
    | Atom.Lit _ -> raise_compile_err (get_exp_loc e) msg)
    in (i, code)

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

and pat_need_checks p ptyp = match p with
    | PatAny _ | PatIdent _ | PatAs _ -> false
    | PatLit _ -> true
    | PatCons(_, _, _) -> true (* the check for non-empty list is needed *)
    | PatTyped(p, _, _) -> pat_need_checks p ptyp
    | PatTuple(pl, _) ->
        let tl = match ptyp with
            | KTypTuple(tl) -> tl
            | _ -> raise_compile_err (get_pat_loc p) "this pattern needs a tuple as argument" in
        List.exists2 (fun pi ti -> pat_need_checks pi ti) pl tl
    | PatVariant(_, _, _) ->
        true
        (* [TODO] it's not necessarily true if the variant has a single case *)
        (*List.exists pat_need_checks pl*)
    | PatRec(rn_opt, ip_l, _) ->
        (* [TODO] if rn_opt = Some(rn), the condition is not necessarily true,
                  because the variant may have a single case *)
        (*(Option.is_some rn_opt) || List.exists (fun (ni, pi) -> pat_need_checks pi) ip_l*)
        raise_compile_err (get_pat_loc p) "record patterns are not supported yet"

and pat_skip_typed p = match p with
    | PatTyped(p, _, _) -> pat_skip_typed p
    | _ -> p

and pat_propose_id p ptyp temp_prefix is_simple mutable_leaves sc =
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
        if (pat_have_vars p) || (not is_simple && (pat_need_checks p ptyp))
        then (p, (gen_temp_idk temp_prefix), true)
        else (p, noid, false)

and pat_simple_unpack p ptyp e_opt code temp_prefix flags sc =
    let mutable_leaves = List.mem ValMutable flags in
    let n_flags = List.filter (fun f -> f != ValMutable && f != ValTempRef) flags in
    let (p, n, tref) = pat_propose_id p ptyp temp_prefix true mutable_leaves sc in
    if n = noid then (n, code) else
    let loc = get_pat_loc p in
    let n_flags = if mutable_leaves && not tref then ValMutable :: n_flags
                else if tref then ValTempRef :: n_flags else n_flags in
    let code = create_defval n ptyp n_flags e_opt code sc loc in
    let code =
    (match p with
    | PatTuple(pl, loc) ->
        let tl = match ptyp with
                | KTypTuple(tl) ->
                    if (List.length tl) != (List.length pl) then
                        raise_compile_err loc "the number of elements in the pattern and in the tuple type are different"
                    else
                        tl
                | _ -> raise_compile_err loc "invalid type of the tuple pattern (it must be a tuple as well)" in
        let (_, code) = List.fold_left2 (fun (idx, code) pi ti ->
            let loci = get_pat_loc pi in
            let ei = KExpMem(n, idx, (ti, loci)) in
            let (_, code) = pat_simple_unpack pi ti (Some ei) code temp_prefix flags sc in
            (idx + 1, code)) (0, code) pl tl in
        code
    | PatIdent _ -> code
    | PatVariant(_, _, loc) ->
        raise_compile_err loc "variant patterns are not supported yet"
    | PatRec(_, ip_l, loc) ->
        raise_compile_err loc "record patterns are not supported yet"
    | PatAs _ ->
        let e = KExpAtom(Atom.Id n, (ptyp, loc)) in
        let (_, code) = pat_simple_unpack p ptyp (Some e) code temp_prefix flags sc in
        code
    | _ ->
        printf "pattern: "; Ast_pp.pprint_pat_x p; printf "\n";
        raise_compile_err loc "this type of pattern cannot be used here") in
    (n, code)

and transform_pat_matching a handlers code sc loc =
    (*
        We dynamically maintain 3 lists of the sub-patterns to consider next.
        Each new sub-pattern occuring during recursive processing of the top-level pattern
        is classified and is then either discarded or added to one of the 3 lists:
        * pl_c - the patterns that needs some checks to verify, but have no captured variables
        * pl_uc - need checks and have variables to capture
        * pl_u - need no checks, but have variables to capture.
        The first list pl_c grows from the both ends:
            * literals, as the easiest to check patterns, are added to the beginning of the list.
              So they get a higher priority.
            * other patterns are added to the end

        When we need to select the next sub-pattern to process, we first look at the first list (pl_c),
        if it's empty then we look at the second list (pl_uc) and finally we look at the third list (pl_u).
        Some sub-patterns in pl_uc could be then added to pl_c or pl_u (or discarded).

        We do such dispatching in order to minimize the number of read operations from a complex structure.
        That is, why capture a variable until all the checks are complete and we know we have a match.
        The algorithm does not always produce the most optimal sequence of operations
        (e.g. some checks are easier to do than the others etc., but it's probably good enough approximation)
    *)
    let dispatch_pat pinfo (pl_c, pl_cu, pl_u) =
        let { pinfo_p=p; pinfo_typ=ptyp } = pinfo in
        let need_checks = pat_need_checks p ptyp in
        let have_vars = pat_have_vars p in
        match (need_checks, have_vars) with
        | (true, false) ->
            (match p with
            | PatLit _ -> (pinfo :: pl_c, pl_cu, pl_u)
            | _ -> (pl_c @ (pinfo :: []), pl_cu, pl_u))
        | (true, true) ->
            (pl_c, pinfo :: pl_cu, pl_u)
        | (false, true) ->
            (pl_c, pl_cu, pinfo :: pl_u)
        | _ ->
            (* nothing to do with p, just discard it *)
            (pl_c, pl_cu, pl_u)
    in
    let rec process_next_subpat plists (checks, code) case_sc =
        let temp_prefix = "v" in
        let process_pltl tup_id pl tl plists alt_ei_opt =
            match pl with
            | PatAny _ :: [] -> plists
            | _ ->
                let _ = if (List.length tl) != (List.length pl) then
                    raise_compile_err (get_pat_loc (List.hd pl)) "wrong number of the pattern elements"
                else () in
                let (_, plists_delta) = List.fold_left2 (fun (idx, plists_delta) pi ti ->
                let loci = get_pat_loc pi in
                let ei = match alt_ei_opt with
                    | Some(ei) ->
                        if idx = 0 then () else raise_compile_err loci
                            "a code for singe-argument variant case handling is used with a case with multiple patterns";
                        ei
                    | _ ->
                        KExpMem(tup_id, idx, (ti, loci)) in
                let pinfo_i = {pinfo_p=pi; pinfo_typ=ti; pinfo_e=ei; pinfo_tag=noid} in
                (idx + 1, pinfo_i :: plists_delta)) (0, []) pl tl in
                let plists = List.fold_left (fun plists pinfo -> dispatch_pat pinfo plists) plists plists_delta in
                plists
        in
        let get_var_tag_cmp_and_extract n pinfo (checks, code) vn sc loc =
            (* [TODO] avoid tag check when the variant has just a single case *)
            let {pinfo_tag=var_tag0} = pinfo in
            let (tag_n, code) =
                if var_tag0 != noid then (var_tag0, code) else
                (let tag_n = gen_temp_idk "tag" in
                let extract_tag_exp = KExpIntrin(IntrinVariantTag, (Atom.Id n) :: [], (KTypInt, loc)) in
                let code = create_defval tag_n KTypInt [] (Some extract_tag_exp) code sc loc in
                (tag_n, code))
            in let cmp_tag_exp = KExpBinOp(OpCompareEQ, (Atom.Id tag_n), (Atom.Id vn), (KTypBool, loc)) in
            let checks = (code2kexp (cmp_tag_exp :: code) loc) :: checks in
            let c_args = match (kinfo vn) with
                | KFun {contents={kf_typ}} -> (match kf_typ with KTypFun(args, rt) -> args | _ -> [])
                | _ -> raise_compile_err loc "a variant constructor is expected here" in
            let (case_n, code, alt_e_opt) = match c_args with
                | [] -> (noid, [], None)
                | _ ->
                    let (is_tuple, case_typ) = match c_args with t :: [] -> (false, t) | _ -> (true, KTypTuple(c_args)) in
                    let extract_case_exp = KExpIntrin(IntrinVariantCase,
                        (Atom.Id n) :: (Atom.Id vn) :: [], (case_typ, loc)) in
                    if is_tuple then
                        let case_n = gen_temp_idk "case" in
                        let code = create_defval case_n case_typ (ValTempRef :: [])
                            (Some extract_case_exp) [] sc loc in
                        (case_n, code, None)
                    else
                        (noid, [], (Some extract_case_exp))
            in (case_n, c_args, checks, code, alt_e_opt)
        in
        let (p_opt, plists) = match plists with
            | (p :: pl_c, pl_cu, pl_u) -> ((Some p), (pl_c, pl_cu, pl_u))
            | ([], p :: pl_cu, pl_u) -> ((Some p), ([], pl_cu, pl_u))
            | ([], [], p :: pl_u) -> ((Some p), ([], [], pl_u))
            | _ -> (None, ([], [], [])) in
        match p_opt with
        | Some(pinfo) ->
            let {pinfo_p=p; pinfo_typ=ptyp; pinfo_e=ke; pinfo_tag=var_tag0} = pinfo in
            let (p, n, tref) = pat_propose_id p ptyp temp_prefix false false case_sc in
            if n = noid then process_next_subpat plists (checks, code) case_sc else
            let loc = get_pat_loc p in
            let (n, code) = match (ke, tref) with
                | (KExpAtom((Atom.Id n0), _), true) -> (n0, code)
                | _ ->
                    let flags = if tref then ValTempRef :: [] else [] in
                    let code = create_defval n ptyp flags (Some ke) code sc loc in
                    (n, code) in
            let (plists, checks, code) =
            (match p with
            | PatLit (l, _) ->
                let code = KExpBinOp(OpCompareEQ, (Atom.Id n), (Atom.Lit l), (KTypBool, loc)) :: code in
                let c_exp = code2kexp code loc in
                (plists, c_exp :: checks, [])
            | PatIdent _ -> (plists, checks, code)
            | PatCons(p1, p2, _) ->
                let code = KExpBinOp(OpCompareNE, (Atom.Id n), (Atom.Lit LitNil), (KTypBool, loc)) :: code in
                let c_exp = code2kexp code loc in
                let et = match ptyp with
                        | KTypList et -> et
                        | _ -> raise_compile_err loc "the pattern needs list type" in
                let get_hd_exp = KExpIntrin(IntrinListHead, (Atom.Id n) :: [], (et, loc)) in
                let get_tl_exp = KExpIntrin(IntrinListTail, (Atom.Id n) :: [], (ptyp, loc)) in
                let p_hd = {pinfo_p=p1; pinfo_typ=et; pinfo_e=get_hd_exp; pinfo_tag=noid} in
                let p_tl = {pinfo_p=p2; pinfo_typ=ptyp; pinfo_e=get_tl_exp; pinfo_tag=noid} in
                let plists = dispatch_pat p_hd plists in
                let plists = dispatch_pat p_tl plists in
                (plists, c_exp :: checks, [])
            | PatTuple(pl, loc) ->
                let tl = match ptyp with
                    | KTypTuple(tl) -> tl
                    | _ -> raise_compile_err loc "invalid type of the tuple pattern (it must be a tuple as well)" in
                let plists = process_pltl n pl tl plists None in
                (plists, checks, code)
            | PatVariant(vn, pl, loc) ->
                let (case_n, tl, checks, code, alt_e_opt) =
                    get_var_tag_cmp_and_extract n pinfo (checks, code) vn case_sc loc in
                let plists = if case_n = noid && (Option.is_none alt_e_opt) then plists
                             else process_pltl case_n pl tl plists alt_e_opt in
                (plists, checks, code)
            | PatRec(_, ip_l, loc) ->
                raise_compile_err loc "record patterns are not supported yet"
            | PatAs (p, _, _) ->
                let pinfo = {pinfo_p=p; pinfo_typ=ptyp; pinfo_e=KExpAtom((Atom.Id n), (ptyp, loc)); pinfo_tag=var_tag0} in
                let plists = dispatch_pat pinfo plists in
                (plists, checks, code)
            | _ ->
                printf "pattern: "; Ast_pp.pprint_pat_x p; printf "\n";
                raise_compile_err loc "this type of pattern is not supported yet")
            in process_next_subpat plists (checks, code) case_sc
        | _ -> (checks, code)
    in
    let atyp = get_atom_ktyp a in
    let is_variant = match atyp with
                | KTypExn -> true
                | KTypName(tname) -> (match (kinfo tname) with
                    | KVariant _ -> true
                    | _ -> false)
                | _ -> false in
    let (var_tag0, code) = if not is_variant then (noid, code) else
        (let tag_n = gen_temp_idk "tag" in
        let extract_tag_exp = KExpIntrin(IntrinVariantTag, a :: [], (KTypInt, loc)) in
        let code = create_defval tag_n KTypInt [] (Some extract_tag_exp) code sc loc in
        (tag_n, code)) in
    let k_handlers = List.map (fun (pl, e) ->
        let ncases = List.length pl in
        let _ = if ncases = 1 then () else
            raise_compile_err (get_pat_loc (List.hd pl))
                "multiple alternative patterns are not supported yet" in
        let pinfo={pinfo_p=(List.hd pl); pinfo_typ=atyp; pinfo_e=KExpAtom(a, (atyp, loc)); pinfo_tag=var_tag0} in
        let plists = dispatch_pat pinfo ([], [], []) in
        let case_sc = new_block_scope() :: sc in
        let (checks, case_code) = process_next_subpat plists ([], []) case_sc in
        let (ke, case_code) = exp2kexp e case_code false case_sc in
        let eloc = get_exp_loc e in
        let ke = code2kexp (ke :: case_code) eloc in
        ((List.rev checks), ke)) handlers in
    (k_handlers, code)

and transform_fun df code sc =
    let {df_name; df_templ_args; df_templ_inst; df_loc} = !df in
    let inst_list = if df_templ_args = [] then df_name :: [] else df_templ_inst in
    List.fold_left (fun code inst ->
        match (id_info inst) with
        | IdFun {contents={df_name=inst_name; df_args=inst_args;
            df_typ=inst_typ; df_body=inst_body; df_flags=inst_flags; df_loc=inst_loc}} ->
            let ktyp = typ2ktyp inst_typ df_loc in
            let (argtyps, rt) = match ktyp with
                | KTypFun(argtyps, rt) -> (argtyps, rt)
                | _ -> raise_compile_err inst_loc
                    (sprintf "the type of non-constructor function '%s' should be TypFun(_,_)" (id2str inst_name)) in
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
            (sprintf "the entry '%s' (an instance of '%s'?) is supposed to be a function, but it's not"
                (id2str inst) (id2str df_name)))
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
                            let kf = ref { kf_name=df_name; kf_typ=kf_typ; kf_args=List.map (fun _ -> noid) argtypes;
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
