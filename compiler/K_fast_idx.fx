/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* optimizes out index range checks when accessing arrays sequentially
  inside for-loops (including comprehensions).

  For example, without this optimization the following code
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
  where 'alpha' and 'beta' are loop invariants and 'i' is the loop index.
  In this case we remove checks inside the loop.

  Instead, before the loop we check that the start and end value of
  the loop index (1 and (n-1)-1, respectively, in the example above)
  are valid index values. If not, we throw exception right away.

  Strictly speaking, this transformation does not preserve the semantics
  in all cases, because some weird code may intentionally throw out-of-range
  exception inside the loop. And then instead of throwing exception somewhere
  in the middle we now throw it before running the loop. But the benefits
  of this optimization outweight such minor side effects.

  If the original behaviour is absolutely necessary, user can replace
  for-loop with while-loop, where such optimization is not applied.
*/
from Ast import *
from K_form import *

import Map, Set, Hashmap, Hashset

type idx_class_t =
    | IdxUnknown
    | IdxSimple: (id_t, atom_t, atom_t)
    | IdxComplex

type arr_access_t = {aa_arr: id_t; aa_dim: int; aa_class: idx_class_t}

type loop_idx_t =
    | LoopOverRange: (atom_t, atom_t, atom_t)
    | LoopOverArr: (id_t, int)

type affine_def_t = (kexp_t, bool, idx_class_t)
type affine_map_t = (id_t, affine_def_t) Map.t
type loop_idx_map_t = (id_t, loop_idx_t) Map.t

fun is_loop_invariant(a: atom_t, inloop_vals: id_hashset_t, loc: loc_t): bool =
    match a {
    | AtomId i => !(inloop_vals.mem(i) || is_mutable(i, loc))
    | _ => true
    }

fun optimize_idx_checks(km_idx: int, topcode: kcode_t)
{
    val throw_funcs = Hashmap.empty(256, noid, -1)

    /*
        Find if the expression, e.g. loop body,
        contains break's, continue's or throw's inside.
        If yes, then the index range check optimization
        would better not to be used.
        It may be too strict condition, but
        in order to relax it, we need to do quite
        complex control flow analysis
    */
    fun have_jumps(e: kexp_t)
    {
        var have_throw = false
        var have_jumps = false
        fun have_jumps_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
        fun have_jumps_kexp_(e: kexp_t, callb: k_fold_callb_t) =
            if !have_throw {
                match e {
                | KExpWhile(c, body, _) =>
                    have_jumps_kexp_(c, callb)
                    val have_jumps_saved = have_jumps
                    have_jumps_kexp_(body, callb)
                    have_jumps = have_jumps_saved
                | KExpDoWhile(body, c, _) =>
                    have_jumps_kexp_(c, callb)
                    val have_jumps_saved = have_jumps
                    have_jumps_kexp_(body, callb)
                    have_jumps = have_jumps_saved
                | KExpFor (_, _, body, _, _) =>
                    val have_jumps_saved = have_jumps
                    have_jumps_kexp_(body, callb)
                    have_jumps = have_jumps_saved
                | KExpMap ((pre1, _, _) :: _, body, _, _) =>
                    have_jumps_kexp_(pre1, callb)
                    val have_jumps_saved = have_jumps
                    have_jumps_kexp_(body, callb)
                    have_jumps = have_jumps_saved
                | KExpCall(fname, args, (_, loc)) =>
                    match kinfo_(fname, loc) {
                    | KFun (ref {kf_body}) =>
                        val idx = throw_funcs.find_idx_or_insert(fname)
                        val throw_status = throw_funcs.table[idx].data
                        if throw_status > 0 {have_throw = true}
                        else if throw_status < 0 {
                            // avoid infinite recusion
                            // when processing recursive functions
                            throw_funcs.table[idx].data = 1
                            val have_jumps_saved = have_jumps
                            have_jumps_kexp_(kf_body, callb)
                            throw_funcs.table[idx].data = int(have_throw)
                            have_jumps = have_jumps_saved
                        }
                    | _ => have_throw = true
                    }
                | KExpBreak _ | KExpContinue _ =>
                    have_jumps = true
                | KExpThrow _ =>
                    have_throw = true
                | _ => fold_kexp(e, callb)
                }
            }
        val have_jumps_callb = k_fold_callb_t
        {
            kcb_fold_atom=None,
            kcb_fold_ktyp=Some(have_jumps_ktyp_),
            kcb_fold_kexp=Some(have_jumps_kexp_)
        }
        have_jumps_kexp_(e, have_jumps_callb)
        have_jumps || have_throw
    }

    /*
        The algorithm outline:
        1. find all values declared inside the loop, including loop variables.
          loop invariants are immutable values that do not belong to this set
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
        5. replace all those accesses with fast accesses: DomainElem a => DomainFast a
        6. update body of the loop with possibly updated index expressions
        7. for each optimized access operation insert a check before the loop.
           Group similar accesses to avoid duplicated checks.
    */
    fun optimize_for_(whole_e: kexp_t, for_clauses, body: kexp_t)
    {
        val for_loc = get_kexp_loc(whole_e)
        var all_accesses: arr_access_t list = []
        var pre_for_code: kcode_t = []
        var arrsz_env: ((id_t, int), id_t) list = []
        var update_affine_defs = false
        val inloop_vals = declared(whole_e :., 256)
        var affine_defs: affine_map_t = Map.empty(cmp_id)

        fun get_arrsz(arr: id_t, i: int,
                      pre_for_code: kcode_t) =
            match arrsz_env.assoc_opt((arr, i)) {
            | Some arrsz => (arrsz, pre_for_code)
            | _ =>
                val arrsz = dup_idk(km_idx, std__size__)
                val arrsz_exp = KExpIntrin(IntrinGetSize,
                    [AtomId(arr), AtomLit(KLitInt(int64(i))) ], (KTypInt, for_loc))
                val pre_for_code = create_kdefval(arrsz, KTypInt, default_tempval_flags(),
                                                Some(arrsz_exp), pre_for_code, for_loc)
                arrsz_env = ((arr, i), arrsz) :: arrsz_env
                (arrsz, pre_for_code)
            }

        fun collect_affine_defs_ktyp(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
        fun collect_affine_defs_kexp(e: kexp_t, callb: k_fold_callb_t) =
            match e {
            /* if rhs in 'val i = rhs' is different from a+b, a-b or a*b
               and if 'i' is used as array index, it means that
               the index is too complex expression,
               and so we cannot optimize the range check
               (unless i is defined outside of the loop, but we
               catch such cases separately) */
            | KDefVal (i, KExpBinary (bop, a, b, (t, _)) as rhs, loc)
                when t == KTypInt &&
                    (bop == OpAdd || bop == OpSub || bop == OpMul) &&
                    !is_mutable(i, loc) =>
                affine_defs = affine_defs.add(i, (rhs, false, IdxUnknown))
            | _ => fold_kexp(e, callb)
            }

        val collect_callb = k_fold_callb_t
        {
            kcb_fold_kexp=Some(collect_affine_defs_kexp),
            kcb_fold_ktyp=Some(collect_affine_defs_ktyp),
            kcb_fold_atom=None
        }
        collect_affine_defs_kexp(whole_e, collect_callb)

        /* step 3. collect loop indices; we collect only those indices,
           for which we can optimize the access operations */
        val fold loop_idx = (Map.empty(cmp_id): loop_idx_map_t)
            for (e, idl, idxl) <- for_clauses {
            val fold arr_id = noid, loop_idx = loop_idx for (i, dom) <- idl {
                match dom {
                | DomainRange (a, b, delta)
                    when b != _ALitVoid &&
                    is_loop_invariant(a, inloop_vals, for_loc) &&
                    is_loop_invariant(b, inloop_vals, for_loc) &&
                    is_loop_invariant(delta, inloop_vals, for_loc) =>
                    (arr_id, loop_idx.add(i, LoopOverRange(a, b, delta)))
                | DomainElem(AtomId i)
                    when (match get_idk_ktyp(i, for_loc) {
                        | KTypArray _ => true
                        | _ => false
                        }) &&
                    is_loop_invariant(AtomId(i), inloop_vals, for_loc) =>
                    (i, loop_idx)
                | _ => (arr_id, loop_idx)
                }
            }
            val loop_idx =
            if arr_id == noid || idxl == [] {
                loop_idx
            } else {
                fold loop_idx = loop_idx for idx@i <- idxl {
                    loop_idx.add(idx, LoopOverArr(arr_id, i))
                }
            }
            loop_idx
        }

        fun get_loop_idx_range(i: id_t,
                pre_for_code: kcode_t, loc: loc_t) =
            match loop_idx.find_opt(i) {
            | Some(LoopOverRange (a, b, delta)) => (a, b, delta, pre_for_code)
            | Some(LoopOverArr (arr, j)) =>
                val (arrsz, pre_for_code) = get_arrsz(arr, j, pre_for_code)
                val a = AtomLit(KLitInt(0L))
                val b = AtomId(arrsz)
                val delta = AtomLit(KLitInt(1L))
                (a, b, delta, pre_for_code)
            | _ => throw compile_err(loc, f"fast_idx: index '{idk2str(i, loc)}' \
                        is not found in the loop_idx map, but it should be there")
            }

        fun optimized_add(a: atom_t, b: atom_t, loc: loc_t)
        {
            val ctx = (KTypInt, loc)
            match (a, b) {
            | (AtomLit(KLitInt ia), AtomLit(KLitInt ib)) =>
                KExpAtom(AtomLit(KLitInt(ia + ib)), ctx)
            | (AtomLit(KLitInt 0L), b) => KExpAtom(b, ctx)
            | (a, AtomLit(KLitInt 0L)) => KExpAtom(a, ctx)
            | _ => KExpBinary(OpAdd, a, b, ctx)
            }
        }

        fun optimized_sub(a: atom_t, b: atom_t, loc: loc_t)
        {
            val ctx = (KTypInt, loc)
            match (a, b) {
            | (AtomLit(KLitInt ia), AtomLit(KLitInt ib)) =>
                KExpAtom(AtomLit(KLitInt(ia - ib)), ctx)
            | (AtomLit(KLitInt(0L)), b) => KExpUnary(OpNegate, b, ctx)
            | (a, AtomLit(KLitInt(0L))) => KExpAtom(a, ctx)
            | _ => KExpBinary(OpSub, a, b, ctx)
            }
        }

        fun optimized_mul(a: atom_t, b: atom_t, loc: loc_t)
        {
            val ctx = (KTypInt, loc)
            match (a, b) {
            | (AtomLit(KLitInt ia), AtomLit(KLitInt ib)) =>
                KExpAtom(AtomLit(KLitInt(ia * ib)), ctx)
            | (AtomLit(KLitInt(1L)), b) => KExpAtom(b, ctx)
            | (a, AtomLit(KLitInt(1L))) => KExpAtom(a, ctx)
            | _ => KExpBinary(OpMul, a, b, ctx)
            }
        }

        fun classify_idx(a: atom_t, loc: loc_t) =
            match a {
            | AtomId i =>
                if loop_idx.mem(i) {
                    IdxSimple(i, AtomLit(KLitInt(1L)), AtomLit(KLitInt(0L)))
                } else if is_loop_invariant(a, inloop_vals, loc) {
                    IdxSimple(noid, AtomLit(KLitInt(0L)), a)
                } else {
                    /*
                       We analyze array index expression (including nested sub-expressions) and
                       try to bring it to the form 'array_idx = alpha*loop_idx + beta',
                       where alpha and beta are loop invariants and loop_idx is a loop index.
                       The partial case is alpha == 0, i.e. when array_idx is loop invariant.
                       When we do it, we may need to introduce some temporary values and redefine array_idx.
                    */
                    match affine_defs.find_opt(i) {
                    | Some((KExpBinary (bop, a_, b_, (t, loc)) as idx_exp0, _, IdxUnknown)) =>
                        val a_class = classify_idx(a_, loc)
                        val b_class = classify_idx(b_, loc)
                        val (c_idx, c_scale_exp, c_shift_exp) =
                        match (bop, a_class, b_class) {
                        | (bop, IdxSimple (a_idx, a_scale, a_shift), IdxSimple (b_idx, b_scale, b_shift))
                            when (bop == OpAdd || bop == OpSub) &&
                                (a_idx == b_idx || a_idx == noid || b_idx == noid) =>
                            val optimized_op =
                                if bop == OpAdd { optimized_add }
                                else { optimized_sub }
                                (if a_idx == noid { b_idx } else { a_idx },
                                Some(optimized_op(a_scale, b_scale, loc)),
                                Some(optimized_op(a_shift, b_shift, loc)))
                        | (OpMul, IdxSimple (a_idx, a_scale, a_shift), IdxSimple (b_idx, b_scale, b_shift))
                            when a_idx == noid || b_idx == noid =>
                            if a_idx == noid {
                                (b_idx, Some(optimized_mul(a_shift, b_scale, loc)),
                                        Some(optimized_mul(a_shift, b_shift, loc)))
                            } else {
                                (a_idx, Some(optimized_mul(b_shift, a_scale, loc)),
                                        Some(optimized_mul(b_shift, a_shift, loc)))
                            }
                        | _ => (noid, None, None)
                        }
                        val (idx_exp, update_exp, idx_class) =
                        match (c_idx, c_scale_exp, c_shift_exp) {
                        | (c_idx, Some c_scale_exp, Some c_shift_exp) =>
                            val (c_scale, code) = kexp2atom(km_idx, "t", c_scale_exp, false, pre_for_code)
                            val (c_shift, code) = kexp2atom(km_idx, "t", c_shift_exp, false, code)
                            pre_for_code = code
                            val (idx_scaled_exp, idx_code) =
                            if c_idx == noid {
                                (KExpAtom(c_shift, get_kexp_ctx(c_shift_exp)), [])
                            } else {
                                val idx_scaled_exp = optimized_mul(AtomId(c_idx), c_scale, loc)
                                match c_shift {
                                | AtomLit(KLitInt 0L) => (idx_scaled_exp, [])
                                | _ =>
                                    val (idx_scaled, idx_code) = kexp2atom(km_idx, "t",
                                                                    idx_scaled_exp, false, [])
                                    val idx_scaled_exp = optimized_add(idx_scaled, c_shift, loc)
                                    (idx_scaled_exp, idx_code)
                                }
                            }
                            val idx_class = IdxSimple(c_idx, c_scale, c_shift)
                            match (idx_scaled_exp, idx_code) {
                            | (KExpBinary (bop1, a1, b1, _), [])
                                when bop == bop1 && (a1 == a_ && b1 == b_ ||
                                (bop == OpAdd || bop == OpMul) && a1 == b_ && b1 == a_) =>
                                (idx_exp0, false, idx_class)
                            | _ =>
                                update_affine_defs = true
                                (rcode2kexp(idx_scaled_exp :: idx_code, loc), true, idx_class)
                            }
                        | _ =>
                            (idx_exp0, false, IdxComplex)
                        }
                        affine_defs = affine_defs.add(i, (idx_exp, update_exp, idx_class))
                        idx_class
                    | Some((_, _, idx_class)) => idx_class
                    | _ => IdxComplex
                    }
                }
            | _ => IdxSimple(noid, AtomLit(KLitInt(0L)), a)
            }

        fun optimize_idx_ktyp(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
        fun optimize_idx_kexp(e: kexp_t, callb: k_callb_t) =
            match e {
            // do not optimize then- and else- branches, since there can be
            // an invalid access to array in a branch that is not taken
            | KExpIf (c, then_e, else_e, ctx) =>
                KExpIf(optimize_idx_kexp(c, callb), then_e, else_e, ctx)
            | KExpAt (AtomId arr, BorderNone, InterpNone, idxs, (t, loc))
                when is_loop_invariant(AtomId(arr), inloop_vals, loc) =>
                val fold have_ranges = false, have_slow = false for idx <- idxs {
                    match idx {
                    | DomainRange _ => (true, have_slow)
                    | DomainElem _ => (have_ranges, true)
                    | _ => (have_ranges, have_slow)
                    }
                }
                if have_ranges || !have_slow { e }
                else {
                    val new_idxs =
                        [for idx@i <- idxs {
                            match idx {
                            | DomainElem a =>
                                val aa_class = classify_idx(a, loc)
                                match aa_class {
                                | IdxSimple _ =>
                                    val aa_entry = arr_access_t {
                                        aa_arr=arr, aa_dim=i, aa_class=aa_class}
                                    if !all_accesses.mem(aa_entry) {
                                        all_accesses = aa_entry :: all_accesses
                                    }
                                    DomainFast(a)
                                | _ => idx
                                }
                            | _ => idx
                            }
                        }]
                    KExpAt(AtomId(arr), BorderNone, InterpNone, new_idxs, (t, loc))
                }
            // if there is val arrsz = size(arr, dim_idx),
            // where arr is loop invariant and dim_idx is literal,
            // we move it outside of the loop and memorize it.
            | KDefVal(arrsz, KExpIntrin(IntrinGetSize, [AtomId(arr), AtomLit(KLitInt(dim))], _), loc)
                when is_loop_invariant(AtomId(arr), inloop_vals, loc) =>
                pre_for_code = e :: pre_for_code
                arrsz_env = ((arr, int(dim)), arrsz) :: arrsz_env
                KExpNop(loc)
            /*
                if we have check_idx_range(N, a, b, 1, D0, D1),
                where a == A0*i + A1, b == B0*i + B1,
                i is the loop's variable,
                A0, A1, B0, B1, D0 and D1 are all loop invariants,
                A0, B0 are non-negative literals (they could be values as well,
                but for values we cannot check their non-negativeness at compile time)
                we can try to move it outside of the loop.

                That is, we have some A0*i + A1 <= j < B0*i + B1 (A0 >= 0, B0 >= 0)
                If i varies as C0 <= i < C1 then
                In principle j varies as
                  A0*C0 + A1 <= j < B0*(i-1) + B1 ===
                  A0*C0 + A1 <= j < B0*C1 + (B1-B0),

                so we can move check_idx_range() outside of the loop and modify it as
                check_idx_range(N, A0*C0+A1, B0*C1 + (B1-B0), D0, D1)
            */
            | KExpIntrin(IntrinCheckIdxRange,
                [AtomId(arrsz), a, b, AtomLit(KLitInt 1L), scale, shift],
                (_, loc) )
                when (is_loop_invariant(scale, inloop_vals, loc) &&
                     is_loop_invariant(shift, inloop_vals, loc) &&
                     exists(for (_, arrsz_j) <- arrsz_env {arrsz_j == arrsz}))
                     =>
                //println(f"{loc}: a: {atom2str(a)}, b: {atom2str(b)}")
                val a_class = classify_idx(a, loc)
                val b_class = classify_idx(b, loc)
                match (a_class, b_class) {
                | (IdxSimple(a_i, AtomLit(KLitInt(a_scale)) as a_scale_atom, a_shift),
                   IdxSimple(b_i, AtomLit(KLitInt(b_scale)) as b_scale_atom, b_shift))
                   when a_scale >= 0L && b_scale >= 0L &&
                    (a_i == b_i || a_scale == 0L || b_scale == 0L) =>
                    val idx =
                        if a_scale != 0L {a_i}
                        else if b_scale != 0L {b_i}
                        else {noid};
                    val (new_e, new_pre_for_code) = if idx == noid {
                        // if j does not depend on the loop var,
                        // just lift the check w/o changes
                        (KExpNop(loc), e :: pre_for_code)
                    } else {
                        val (c0, c1, delta, pre_for_code) =
                            get_loop_idx_range(idx, pre_for_code, loc)
                        match delta {
                        | AtomLit(KLitInt 1L) =>
                            val int_ctx = (KTypInt, loc)
                            // c0*a_scale + a_shift
                            val (t, pre_for_code) = kexp2atom(km_idx, "t",
                                KExpBinary(OpMul, c0, a_scale_atom, int_ctx), false, pre_for_code)
                            val (new_a, pre_for_code) = kexp2atom(km_idx, "a",
                                KExpBinary(OpAdd, t, a_shift, int_ctx), false, pre_for_code)
                            // c1*b_scale + (b_shift - b_scale)
                            val (t, pre_for_code) = kexp2atom(km_idx, "t",
                                KExpBinary(OpMul, c1, b_scale_atom, int_ctx), false, pre_for_code)
                            val (t2, pre_for_code) = kexp2atom(km_idx, "t",
                                KExpBinary(OpSub, b_shift, b_scale_atom, int_ctx), false, pre_for_code)
                            val (new_b, pre_for_code) = kexp2atom(km_idx, "b",
                                KExpBinary(OpAdd, t, t2, int_ctx), false, pre_for_code)
                            // we don't care if those expressions include some sub-constant expressions -
                            // const folding stage will take care of that
                            val new_check = KExpIntrin(IntrinCheckIdxRange,
                                [AtomId(arrsz), new_a, new_b, AtomLit(KLitInt(1L)), scale, shift],
                                (KTypVoid, loc) )
                            (KExpNop(loc), new_check :: pre_for_code)
                        | _ => (e, pre_for_code)
                        }
                    }
                    pre_for_code = new_pre_for_code
                    new_e
                | _ => e
                }
            // similar to 'if', do not optimize other non-linear fragments of the loop body;
            // type/exception/fun declarations don't need to be processed either.
            | KExpMatch _ | KExpTryCatch _ | KExpWhile _ | KExpDoWhile _ | KDefFun _
            | KDefExn _ | KDefVariant _ | KDefTyp _ | KDefClosureVars _ => e
            | _ => walk_kexp(e, callb)
            }

        val optimize_idx_callb = k_callb_t
        {
            kcb_ktyp=Some(optimize_idx_ktyp),
            kcb_kexp=Some(optimize_idx_kexp),
            kcb_atom=None
        }
        fun update_affine_ktyp(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
        fun update_affine_kexp(e: kexp_t, callb: k_callb_t) =
            match e {
            | KDefVal (i, rhs, loc) =>
                match affine_defs.find_opt(i) {
                | Some((new_rhs, true, _)) => KDefVal(i, new_rhs, loc)
                | _ => e
                }
            | _ => walk_kexp(e, callb)
            }

        val update_affine_callb = k_callb_t
        {
            kcb_kexp=Some(update_affine_kexp),
            kcb_ktyp=Some(update_affine_ktyp),
            kcb_atom=None
        }

        /* steps 4 & 5. optimize some of array acceses */
        val for_clauses =
            [for (e, idl, idxl) <- for_clauses {
                val e = optimize_idx_kexp(e, optimize_idx_callb)
                (e, idl, idxl)
            }]
        val body = optimize_idx_kexp(body, optimize_idx_callb)

        /* step 6. patch modified definitions, if any */
        val (for_clauses, body) =
        if !update_affine_defs {
            (for_clauses, body)
        } else {
            val for_clauses =
                [for (e, idl, idxl) <- for_clauses {
                    val e = update_affine_kexp(e, update_affine_callb)
                    (e, idl, idxl)
                }]
            val body = update_affine_kexp(body, update_affine_callb)
            (for_clauses, body)
        }
        /* step 7. insert checks before the loop body */
        val pre_for_code =
        if all_accesses == [] {
            pre_for_code
        } else {
            fold pre_for_code = pre_for_code
                for {aa_arr, aa_dim, aa_class} <- all_accesses {
                match aa_class {
                | IdxSimple (i, scale, shift) =>
                    val (arrsz, pre_for_code) =
                        get_arrsz(aa_arr, aa_dim, pre_for_code)
                    if i == noid {
                        KExpIntrin(IntrinCheckIdx, [AtomId(arrsz), shift],
                            (KTypVoid, for_loc)) :: pre_for_code
                    } else {
                        val (a, b, delta, pre_for_code) =
                            get_loop_idx_range(i, pre_for_code, for_loc)
                        KExpIntrin( IntrinCheckIdxRange,
                                    [AtomId(arrsz), a, b, delta, scale, shift],
                                    (KTypVoid, for_loc) ) :: pre_for_code
                    }
                | _ => pre_for_code
                }
            }
        }
        (for_clauses, body, pre_for_code)
    }

    fun optimize_for(whole_e: kexp_t, for_clauses, body: kexp_t)
    {
        val skip = match whole_e {
            | KExpMap((pre1, _, _)::_, _, _, _) =>
                have_jumps(pre1)
            | _ => false
            }
        val skip = skip || have_jumps(body)
        if skip {(for_clauses, body, [])}
        else {optimize_for_(whole_e, for_clauses, body)}
    }

    fun process_ktyp(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun process_kexp(e: kexp_t, callb: k_callb_t) =
        match e {
        | KExpFor (idl, idxl, body, flags, loc) =>
            //print("before: "); KPP.pp_kexp(e); println()
            val (for_clauses, body, pre_for_code) = optimize_for(e, (KExpNop(loc), idl, idxl) :., body)
            /* process nested for's, if any, after the call to optimize_for;
               we do it in this order to put the 'batch range checks' as high as
               possible in the hierarchy of nested loops, e.g.:
                for i <- 0:N {
                    for j <- 0:i {
                        for k <- 0:j {
                            // we want to
                            // put the check for the whole "i" range (0:N) outside of the outermost i-loop,
                            // instead of checking the k-loop-invariant "i" right before k-loop;
                            // and put the check for "j" range (0:i) outside of the j-loop
                            // instead of checking the k-loop-invariant "j" right before k-loop.
                            foo(a[i, k]*b[k, j])
                        }
                    }
                }
            */
            val body = process_kexp(body, callb)
            val e =
            match for_clauses {
            | (KExpNop _, idl, idxl) :. => KExpFor(idl, idxl, body, flags, loc)
            | _ => throw compile_err(loc,
                "fast_idx: unexpected output of optimize_for; should output single for_clause")
            }
            rcode2kexp(e :: pre_for_code, loc)
            //print("after: "); KPP.pp_kexp(...); println()
        | KExpMap (for_clauses, body, flags, (t, loc)) =>
            val (for_clauses, body, pre_for_code) = optimize_for(e, for_clauses, body)
            val body = process_kexp(body, callb)
            rcode2kexp(KExpMap(for_clauses, body, flags, (t, loc)) :: pre_for_code, loc)
        | _ => walk_kexp(e, callb)
        }

    val process_callb = k_callb_t
    {
        kcb_kexp=Some(process_kexp),
        kcb_ktyp=Some(process_ktyp),
        kcb_atom=None
    }
    [for e <- topcode {
        process_kexp(e, process_callb)
    }]
}

fun optimize_idx_checks_all(kmods: kmodule_t list) =
    [for km <- kmods {
        val {km_idx, km_top} = km
        val new_top = optimize_idx_checks(km_idx, km_top)
        km.{km_top=new_top}
    }]

/*
    The function tries to speedup all array accesses within a loop
       arr[i0, i1, ..., i(n-2), i(n-1)] such that
    arr && i0 && ... && i(n-2) are all loop invariants.

    For such accesses it first calls get_slice() intrinsic
    before the loop that stores the pointer
    ptr = &arr[i0, i1, ..., i(n-2), 0]
    and then transformsarr[i0, i1, ..., i(n-2), 0]
*/
fun linearize_arrays_access_(km_idx: int, topcode: kexp_t list)
{
    fun optimize_for_linearize(whole_e: kexp_t, body: kexp_t, pre_for_code0: kexp_t list)
    {
        val for_loc = get_kexp_loc(whole_e)
        var all_slices: ((id_t, atom_t list), atom_t) list = []
        var pre_for_code = pre_for_code0
        val inloop_vals = declared(whole_e :., 256)

        fun linearize_idx_ktyp(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
        fun linearize_idx_kexp(e: kexp_t, callb: k_callb_t) =
            match e {
            | KExpAt (AtomId arr, BorderNone, InterpNone, idxs, (t, loc))
                when is_loop_invariant(AtomId(arr), inloop_vals, loc) &&
                     (match get_idk_ktyp(arr, loc) {KTypArray _ => true | _ => false}) =>
                val ndims = idxs.length()
                val fold have_ranges = false, have_slow = false,
                         have_non_invariants = false, idx_atoms = []
                    for idx@i <- idxs {
                        match idx {
                        | DomainRange _ => (true, have_slow, have_non_invariants, idx_atoms)
                        | DomainElem(x) => (have_ranges, true, have_non_invariants, x :: idx_atoms)
                        | DomainFast(x) =>
                            (have_ranges, have_slow, have_non_invariants ||
                                (i < ndims-1 && !is_loop_invariant(x, inloop_vals, loc)),
                            x :: idx_atoms)
                        }
                    }
                //println(f"{loc}: idx={dom2str(idxs.hd())}, have_ranges={have_ranges}, have_slow={have_slow}, have_non_invariants={have_non_invariants}")
                if have_ranges || have_slow || have_non_invariants { e }
                else {
                    val idxs = idx_atoms.tl().rev()
                    val last_idx = idx_atoms.hd()
                    val slice =
                        match all_slices.assoc_opt((arr, idxs)) {
                        | Some slice => slice
                        | _ =>
                            val get_slice_exp = KExpIntrin(IntrinGetSlice,
                                AtomId(arr) :: idxs, (KTypRawPointer(t), for_loc))
                            val slice_id = gen_idk(km_idx, "ptr")
                            val new_pre_for_code = create_kdefval(slice_id,
                                KTypRawPointer(t), default_val_flags(),
                                Some(get_slice_exp), pre_for_code, for_loc)
                            val slice = AtomId(slice_id)
                            all_slices = ((arr, idxs), slice) :: all_slices
                            pre_for_code = new_pre_for_code
                            slice
                        }
                    KExpIntrin(IntrinAccessSlice, [slice, last_idx], (t, loc))
                }
            // similar to 'if', do not optimize other non-linear fragments of the loop body;
            // type/exception/fun declarations don't need to be processed either.
            | KExpMatch _ | KExpTryCatch _ | KExpWhile _ | KExpDoWhile _ | KDefFun _
            | KDefExn _ | KDefVariant _ | KDefTyp _ | KDefClosureVars _ => e
            | _ => walk_kexp(e, callb)
            }

        val linearize_idx_callb = k_callb_t
        {
            kcb_ktyp=Some(linearize_idx_ktyp),
            kcb_kexp=Some(linearize_idx_kexp),
            kcb_atom=None
        }
        val body = linearize_idx_kexp(body, linearize_idx_callb)
        (body, pre_for_code)
    }

    fun process_ktyp(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun process_kexp(e: kexp_t, callb: k_callb_t) =
        match e {
        | KExpFor (idl, idxl, body, flags, loc) =>
            // in the case of nested loops we try to move
            // the slice pointer initialization as high as possible
            val (body, pre_for_code) = optimize_for_linearize(e, body, [])
            val body = process_kexp(body, callb)
            val e = KExpFor(idl, idxl, body, flags, loc)
            rcode2kexp(e :: pre_for_code, loc)
            //print("after: "); KPP.pp_kexp(...); println()
        | KExpMap (for_clauses, body, flags, (t, loc)) =>
            // let's just consider the inner-most for
            val rev_clauses = for_clauses.rev()
            val (pre_for_code0, idl, idxl) = rev_clauses.hd()
            // pretend we have a for-loop.
            // [TODO] if this code ever breaks because it's not a real 'for',
            // we just need to change optimize_for_linearize API to take the list
            // of loop variables (idl), '@' indices (idxl) and body
            // separately and add them into
            val (body, pre_for_code) = optimize_for_linearize(
                KExpFor(idl, idxl, body, flags, loc), body, kexp2code(pre_for_code0).rev())
            val ndims = for_clauses.length()
            val (outer_pre_for, pre_for_code) =
                if ndims == 1 { (pre_for_code, []) }
                else { ([], pre_for_code) }
            val new_clauses = ((rcode2kexp(pre_for_code, loc), idl, idxl) :: rev_clauses.tl()).rev()
            val body = process_kexp(body, callb)
            rcode2kexp(KExpMap(new_clauses, body, flags, (t, loc)) :: outer_pre_for, loc)
        | _ => walk_kexp(e, callb)
        }

    val process_callb = k_callb_t
    {
        kcb_kexp=Some(process_kexp),
        kcb_ktyp=Some(process_ktyp),
        kcb_atom=None
    }
    [for e <- topcode {
        process_kexp(e, process_callb)
    }]
}

fun linearize_arrays_access(kmods: kmodule_t list)
{
    [for km <- kmods {
        val {km_idx, km_top} = km
        val new_top = linearize_arrays_access_(km_idx, km_top)
        km.{km_top=new_top}
    }]
}