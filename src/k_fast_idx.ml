(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* optimizes out index range checks when accessing arrays sequentially
  inside for-loops (including comprehensions).

  For example, the following code
  [for i <- 1:n-1 {(arr[i-1] + arr[i]*2 + arr[i+1])/4}]]

  is translated to the following pseudo-C code:

    <allocate output array "dst", initialize dstptr>
    for(int i=1; i<n-1; i++, dstptr++) {
        CHECK_IDX(arr, 0, i-1, loop_catch_label);
        int t0 = arr->data[i-1];
        CHECK_IDX(arr, 0, i, loop_catch_label);
        int t1 = arr->data[i];
        CHECK_IDX(arr, 0, i+1, loop_catch_label);
        int t2 = arr->data[i+1];
        *dstptr = (t0 + t1*2 + t2)/4;
    loop_catch_label:
        if(fx_result < 0) goto upper_catch_label;
    }
    ...
  upper_catch_label:
    ...

  whereas after the range check optimization the same ficus code is translated to:

    <allocate output array "dst", initialize dstptr>
    if (
        // 1-1 < 0 || // eliminated check
        ((n-1)-1)-1 >= dst->dim[0].size ||
        // 1 < 0 || // eliminated check
        ((n-1)-1) >= dst->dim[0].size ||
        // 1+1 < 0 || // eliminated check
        ((n-1)-1)+1 >= dst->dim[0].size )
        THROW_OUT_OF_RANGE(upper_catch_label);
    for(int i=1; i<n-1; i++, dstptr++) {
        int t0 = arr->data[i-1];
        int t1 = arr->data[i];
        int t2 = arr->data[i+1];
        *dstptr = (t0 + t1*2 + t2)/4;
    }
    ...
  upper_catch_label:
    ...

  that is, we notice that inside the loop we have unconditional accesses
  (i.e. not inside if/match etc.) to some arrays, and the indices are
  affine transformations of loop index, i.e. they have form 'alpha*i + beta',
  where 'alpha' and 'beta' are loop invariants and 'i' is the loop index. In this case
  we remove checks inside the loop.
  Instead, before the loop we check that the start and end value of the loop index
  (1 and (n-1)-1, respectively, in the example above)
  are valid index values. If not, we throw exception right away.

  Strictly speaking, this transformation does not preserve the semantics in all cases,
  because some weird code may intentionally throw out-of-range exception inside the loop.
  And then instead of throwing exception somewhere in the middle we now throw it before running the loop.
  But the benefits of this optimization outweight some minor side effect of this optimization.

  If such behaviour is absolutely necessary, user can replace for-loop with while-loop,
  where such optimization is not applied.
*)

open Ast
open K_form

type idx_class_t = IdxUnknown | IdxSimple of id_t * atom_t * atom_t | IdxComplex

type arr_access_t =
{
    aa_arr: id_t;
    aa_dim: int;
    aa_class: idx_class_t;
}

type loop_idx_t = LoopOverRange of atom_t*atom_t*atom_t | LoopOverArr of id_t*int
type affine_def_t = kexp_t * bool * idx_class_t

let optimize_idx_checks topcode =
    let is_loop_invariant a inloop_vals loc =
        match a with
        | Atom.Id i -> not ((IdSet.mem i inloop_vals) || (is_mutable i loc))
        | _ -> true
    in
    (*
        The algorithm outline:
        1. find all values declared inside the loop, including loop variables.
          loop invariants are immutable values that do not belong this set
          (the actual condition is more complex, but here we are interested in just
          array indices, i.e. integer expressions, and for them the condition is true)
        2. collect all in-loop-defined 'affine' sub-expressions
        3. collect some of for-loops indices into 'loop_idx' set;
          that includes iteration over closed ranges (a:b or a:b:delta, not a::delta)
          and array element indices ('a@idx' where a is array).
        4. scan the loop body and check all unconditional accesses to
          'val' arrays that are affine transformations of loop indices.
          Because the index expressions can be quite complex, we make use of 'affine_defs',
          collected at step 2.
        5. replace all those accesses with fast accesses: Domain.Elem a => Domain.Fast a
        6. update body of the loop with possibly updated index expressions
        7. for each optimized access operation insert a check before the loop.
           Group similar accesses to avoid duplicated checks.
    *)
    let rec optimize_for whole_e for_clauses body =
        let for_loc = get_kexp_loc whole_e in
        let all_accesses = ref ([]: arr_access_t list) in
        let pre_for_code = ref ([]: kexp_t list) in
        let update_affine_defs = ref false in
        (* step 1. find everything declared inside the loop *)
        let (_, inloop_vals) = used_decl_by_kexp whole_e in
        (* step 2. collect affine sub-expressions; we don't know yet which we will actually need,
           so we collect them all *)
        let affine_defs = ref (Env.empty : affine_def_t Env.t) in
        let collect_affine_defs_ktyp t loc callb = () in
        let collect_affine_defs_kexp e callb =
            match e with
            (* if rhs in 'val i = rhs' is different from a+b, a-b or a*b
               and if 'i' is used as array index, it means that
               the index is too complex expression,
               and so we cannot optimize the range check
               (unless i is defined outside of the loop, but we
               catch such cases separately) *)
            | KDefVal(i, ((KExpBinOp(bop, a, b, (t, _))) as rhs), loc)
                when t = KTypInt &&
                    (bop = OpAdd || bop = OpSub || bop = OpMul) &&
                    not (is_mutable i loc)
                ->
                affine_defs := Env.add i (rhs, false, IdxUnknown) !affine_defs
            | _ -> fold_kexp e callb
            in
        let collect_callb =
        {
            kcb_fold_kexp = Some(collect_affine_defs_kexp);
            kcb_fold_ktyp = Some(collect_affine_defs_ktyp);
            kcb_fold_atom = None;
            kcb_fold_result = 0
        } in
        let _ = collect_affine_defs_kexp whole_e collect_callb in

        (* step 3. collect loop indices; we collect only those indices,
           for which we can optimize the access operations *)
        let loop_idx = List.fold_left (fun loop_idx (e, idl, idxl) ->
            let (arr_id, loop_idx) = List.fold_left (fun (arr_id, loop_idx) (i, dom) ->
                match dom with
                | Domain.Range(a, b, delta) when
                    b <> Atom.Lit (LitNil) &&
                    (is_loop_invariant a inloop_vals for_loc) &&
                    (is_loop_invariant b inloop_vals for_loc) &&
                    (is_loop_invariant delta inloop_vals for_loc)
                    -> (arr_id, (Env.add i (LoopOverRange(a, b, delta)) loop_idx))
                | Domain.Elem(Atom.Id i) when
                    (match (get_idk_ktyp i for_loc) with KTypArray _ -> true | _ -> false) &&
                    (is_loop_invariant (Atom.Id i) inloop_vals for_loc)
                    -> (i, loop_idx)
                | _ -> (arr_id, loop_idx)) (noid, loop_idx) idl
                in
            let (_, loop_idx) = if arr_id = noid || idxl = [] then (0, loop_idx) else
                List.fold_left (fun (i, loop_idx) idx ->
                    (i+1, (Env.add idx (LoopOverArr(arr_id, i)) loop_idx))) (0, loop_idx) idxl
                in
            loop_idx) (Env.empty : loop_idx_t Env.t) for_clauses
            in

        let optimized_add a b loc =
            let ctx = (KTypInt, loc) in
            match (a, b) with
            | (Atom.Lit (LitInt ia), Atom.Lit (LitInt ib)) ->
                KExpAtom(Atom.Lit (LitInt (Int64.add ia ib)), ctx)
            | (Atom.Lit (LitInt 0L), b) -> KExpAtom(b, ctx)
            | (a, Atom.Lit (LitInt 0L)) -> KExpAtom(a, ctx)
            | _ -> KExpBinOp(OpAdd, a, b, ctx)
            in
        let optimized_sub a b loc =
            let ctx = (KTypInt, loc) in
            match (a, b) with
            | (Atom.Lit (LitInt ia), Atom.Lit (LitInt ib)) ->
                KExpAtom(Atom.Lit (LitInt (Int64.sub ia ib)), ctx)
            | (Atom.Lit (LitInt 0L), b) -> KExpUnOp(OpNegate, b, ctx)
            | (a, Atom.Lit (LitInt 0L)) -> KExpAtom(a, ctx)
            | _ -> KExpBinOp(OpSub, a, b, ctx)
            in
        let optimized_mul a b loc =
            let ctx = (KTypInt, loc) in
            match (a, b) with
            | (Atom.Lit (LitInt ia), Atom.Lit (LitInt ib)) ->
                KExpAtom(Atom.Lit (LitInt (Int64.mul ia ib)), ctx)
            | (Atom.Lit (LitInt 1L), b) -> KExpAtom(b, ctx)
            | (a, Atom.Lit (LitInt 1L)) -> KExpAtom(a, ctx)
            | _ -> KExpBinOp(OpMul, a, b, ctx)
            in
        let rec classify_idx a loc =
            match a with
            | Atom.Id i ->
                if Env.mem i loop_idx then
                    IdxSimple(i, Atom.Lit(LitInt 1L), Atom.Lit(LitInt 0L))
                else if (is_loop_invariant a inloop_vals loc) then
                    IdxSimple(noid, Atom.Lit(LitInt 0L), a)
                (* we analyze array index expression (including nested sub-expressions) and try to bring it to
                   the form 'array_idx = alpha*loop_idx + beta', where alpha and beta are loop invariants
                   and loop_idx is a loop index. The partial case is alpha == 0, i.e. when
                   array_idx is loop invariant. When we do it, we may need to introduce some temporary values
                   and redefine array_idx *)
                else (match (Env.find_opt i !affine_defs) with
                    | Some((KExpBinOp(bop, a_, b_, (t, loc)) as idx_exp0, _, IdxUnknown)) ->
                        let a_class = classify_idx a_ loc in
                        let b_class = classify_idx b_ loc in
                        let (c_idx, c_scale_exp, c_shift_exp) = match (bop, a_class, b_class) with
                            | ((OpAdd|OpSub), IdxSimple(a_idx, a_scale, a_shift), IdxSimple(b_idx, b_scale, b_shift)) when
                                a_idx = b_idx || a_idx = noid || b_idx = noid ->
                                let optimized_op = if bop = OpAdd then optimized_add else optimized_sub in
                                ((if a_idx = noid then b_idx else a_idx),
                                (Some(optimized_op a_scale b_scale loc)),
                                (Some(optimized_op a_shift b_shift loc)))
                            | (OpMul, IdxSimple(a_idx, a_scale, a_shift), IdxSimple(b_idx, b_scale, b_shift)) when
                                a_idx = noid || b_idx = noid ->
                                if a_idx = noid then
                                    (b_idx,
                                    (Some(optimized_mul a_shift b_scale loc)),
                                    (Some(optimized_mul a_shift b_shift loc)))
                                else
                                    (a_idx,
                                    (Some(optimized_mul b_shift a_scale loc)),
                                    (Some(optimized_mul b_shift a_shift loc)))
                            | _ -> (noid, None, None)
                            in
                        let (idx_exp, update_exp, idx_class) = match (c_idx, c_scale_exp, c_shift_exp) with
                            | (c_idx, Some(c_scale_exp), Some(c_shift_exp)) ->
                                let (c_scale, code) = kexp2atom "t" c_scale_exp false !pre_for_code in
                                let (c_shift, code) = kexp2atom "t" c_shift_exp false code in
                                let _ = pre_for_code := code in
                                let (idx_scaled_exp, idx_code) = if c_idx = noid then
                                        (KExpAtom(c_shift, (get_kexp_ctx c_shift_exp)), [])
                                    else
                                        let idx_scaled_exp = optimized_mul (Atom.Id c_idx) c_scale loc in
                                        (match c_shift with
                                        | Atom.Lit (LitInt 0L) -> (idx_scaled_exp, [])
                                        | _ ->
                                            let (idx_scaled, idx_code) = kexp2atom "t" idx_scaled_exp false [] in
                                            let idx_scaled_exp = optimized_add idx_scaled c_shift loc in
                                            (idx_scaled_exp, idx_code))
                                    in
                                let idx_class = IdxSimple(c_idx, c_scale, c_shift) in
                                (match (idx_scaled_exp, idx_code) with
                                | (KExpBinOp(bop1, a1, b1, _), []) when
                                    bop = bop1 && ((a1 = a_ && b1 = b_) ||
                                        ((bop = OpAdd || bop = OpMul) && a1 = b_ && b1 = a_))
                                    -> (idx_exp0, false, idx_class)
                                | _ ->
                                    update_affine_defs := true;
                                    ((rcode2kexp (idx_scaled_exp :: idx_code) loc), true, idx_class)
                                )
                            | _ -> (idx_exp0, false, IdxComplex)
                            in
                        affine_defs := Env.add i (idx_exp, update_exp, idx_class) !affine_defs;
                        idx_class
                    | Some((_, _, idx_class)) -> idx_class
                    | _ -> IdxComplex)
            | _ -> IdxSimple(noid, Atom.Lit (LitInt 0L), a)
            in
        let optimize_idx_ktyp t loc callb = t in
        let rec optimize_idx_kexp e callb =
            match e with
            | KExpIf(c, then_e, else_e, ctx) ->
                KExpIf((optimize_idx_kexp c callb), then_e, else_e, ctx)
            | KExpAt((Atom.Id arr), BorderNone, InterpNone, idxs, (t, loc)) when
                (is_loop_invariant (Atom.Id arr) inloop_vals loc) ->
                let (have_ranges, have_slow) = List.fold_left (fun (have_ranges, have_slow) idx ->
                    match idx with
                    | Domain.Range _ -> (true, have_slow)
                    | Domain.Elem _ -> (have_ranges, true)
                    | _ -> (have_ranges, have_slow)) (false, false) idxs
                    in
                if have_ranges then e
                else if not have_slow then e
                else
                    let new_idxs = List.mapi (fun i idx ->
                        match idx with
                        | Domain.Elem a ->
                            let aa_class = classify_idx a loc in
                            (match aa_class with
                            | IdxSimple _ ->
                                let aa_entry = {aa_arr=arr; aa_dim=i; aa_class} in
                                if (List.mem aa_entry !all_accesses) then () else
                                    all_accesses := aa_entry :: !all_accesses;
                                Domain.Fast a
                            | _ -> idx)
                        | _ -> idx) idxs
                        in
                    KExpAt((Atom.Id arr), BorderNone, InterpNone, new_idxs, (t, loc))
            | KExpMatch _ | KExpTryCatch _ | KExpWhile _ | KExpDoWhile _
            | KDefFun _ | KDefExn _ | KDefVariant _ | KDefTyp _ | KDefClosureVars _ -> e
            | _ -> walk_kexp e callb
            in
        let optimize_idx_callb =
            {
                kcb_ktyp = Some(optimize_idx_ktyp);
                kcb_kexp = Some(optimize_idx_kexp);
                kcb_atom = None;
            }
            in
        let update_affine_ktyp t loc callb = t in
        let update_affine_kexp e callb =
            match e with
            | KDefVal(i, rhs, loc) ->
                (match (Env.find_opt i !affine_defs) with
                | Some((new_rhs, true, _)) ->
                    KDefVal(i, new_rhs, loc)
                | _ -> e)
            | _ -> walk_kexp e callb
            in
        let update_affine_callb =
        {
            kcb_kexp = Some(update_affine_kexp);
            kcb_ktyp = Some(update_affine_ktyp);
            kcb_atom = None
        } in

        (* steps 4 & 5. optimize some of array acceses *)
        let for_clauses = List.map (fun (e, idl, idxl) ->
            let e = optimize_idx_kexp e optimize_idx_callb in
            (e, idl, idxl)) for_clauses
            in
        let body = optimize_idx_kexp body optimize_idx_callb in
        (* step 6. patch modified definitions, if any *)
        let (for_clauses, body) = if not !update_affine_defs then (for_clauses, body)
            else
                let for_clauses = List.map (fun (e, idl, idxl) ->
                    let e = update_affine_kexp e update_affine_callb in
                    (e, idl, idxl)) for_clauses in
                let body = update_affine_kexp body update_affine_callb in
                (for_clauses, body)
            in
        (* step 7. insert checks before the loop body *)
        let get_arrsz arr i arrsz_env pre_for_code =
            match (List.assoc_opt (arr, i) arrsz_env) with
            | Some(arrsz) -> (arrsz, arrsz_env, pre_for_code)
            | _ ->
                let arrsz = gen_temp_idk "sz" in
                let arrsz_exp = KExpIntrin(IntrinGetSize,
                    [(Atom.Id arr); (Atom.Lit (LitInt (Int64.of_int i)))],
                    (KTypInt, for_loc))
                    in
                let pre_for_code = create_kdefval arrsz KTypInt [ValTemp] (Some arrsz_exp) pre_for_code for_loc in
                (arrsz, (((arr, i), arrsz) :: arrsz_env), pre_for_code)
            in
        let pre_for_code = if !all_accesses = [] then !pre_for_code else
            let (_, pre_for_code) = List.fold_left
                (fun (arrsz_env, pre_for_code) {aa_arr; aa_dim; aa_class} ->
                match aa_class with
                | IdxSimple(i, scale, shift) ->
                    let (arrsz, arrsz_env, pre_for_code) = get_arrsz aa_arr aa_dim arrsz_env pre_for_code in
                    if i = noid then
                        let check_idx_exp = KExpIntrin(IntrinCheckIdx, [(Atom.Id arrsz); shift], (KTypVoid, for_loc)) in
                        (arrsz_env, (check_idx_exp :: pre_for_code))
                    else
                        let (a, b, delta, arrsz_env, pre_for_code) =
                            match (Env.find_opt i loop_idx) with
                            | Some(LoopOverRange(a, b, delta)) ->
                                (a, b, delta, arrsz_env, pre_for_code)
                            | Some(LoopOverArr(arr, j)) ->
                                let (arrsz2, arrsz_env, pre_for_code) = get_arrsz arr j arrsz_env pre_for_code in
                                let a = Atom.Lit (LitInt 0L) in
                                let b = Atom.Id arrsz2 in
                                let delta = Atom.Lit (LitInt 1L) in
                                (a, b, delta, arrsz_env, pre_for_code)
                            | _ -> raise_compile_err for_loc
                                (sprintf "fast_idx: index '%s' is not found in the loop_idx, but it should be there" (id2str i))
                            in
                        let check_idx_range_exp = KExpIntrin(IntrinCheckIdxRange,
                            [(Atom.Id arrsz); a; b; delta; scale; shift], (KTypVoid, for_loc)) in
                        (arrsz_env, (check_idx_range_exp :: pre_for_code))
                | _ -> (arrsz_env, pre_for_code)) ([], !pre_for_code) !all_accesses
                in
            pre_for_code
            in
        (pre_for_code, for_clauses, body)
    in
    let process_ktyp t loc callb = t in
    let rec process_kexp e callb =
        match e with
        | KExpFor(idl, idxl, body, flags, loc) ->
            let (pre_for_code, for_clauses, body) =
                optimize_for e [((KExpNop loc), idl, idxl)] body in

            (* process nested for's, if any, after the call to optimize_for;
               we do it in this order to put the 'batch range checks' as high as
               possible in the hierarchy of nested loops, e.g.:
                for i <- 0:N {
                    for j <- 0:i {
                        for k <- 0:j {
                            foo(a[i, k]*b[k, j]) // we want to
                                                // put the check for the whole "i" range (0:N) outside of the outermost i-loop,
                                                // instead of checking the k-loop-invariant "i" right before k-loop;
                                                // and put the check for "j" range (0:i) outside of the j-loop
                                                // instead of checking the k-loop-invariant "j" right before k-loop.
                        }
                    }
                }
            *)
            let body = process_kexp body callb in

            let e = match for_clauses with
                | ((KExpNop _), idl, idxl) :: [] ->
                    KExpFor(idl, idxl, body, flags, loc)
                | _ -> raise_compile_err loc
                    "fast_idx: unexpected output of optimize_for; should output single for_clause"
                in
            rcode2kexp (e :: pre_for_code) loc
        | KExpMap(for_clauses, body, flags, (t, loc)) ->
            let (pre_for_code, for_clauses, body) = optimize_for e for_clauses body in
            let body = process_kexp body callb in
            rcode2kexp ((KExpMap(for_clauses, body, flags, (t, loc))) :: pre_for_code) loc
        | _ -> walk_kexp e callb
        in
    let process_callb =
    {
        kcb_kexp = Some(process_kexp);
        kcb_ktyp = Some(process_ktyp);
        kcb_atom = None
    } in
    List.map (fun e -> process_kexp e process_callb) topcode
