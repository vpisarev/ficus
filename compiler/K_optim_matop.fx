/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Substitues float matrix multiplications with IntrinGemm, which
    is low-level optimized universal version of multiplication
    offering transposition flags and subarraying.
*/

from Ast import *
from K_form import *
import K_pp
import K_cfold_dealias
import Math, Hashmap, Hashset

fun optimize_gemm(kmods: kmodule_t list)
{
    var curr_m_idx = -1

    type matrix_projection =
    {
        original_matrix: id_t = noid;
        row_range: (atom_t, atom_t, atom_t) = (AtomLit(KLitNil(KTypVoid)), AtomLit(KLitNil(KTypVoid)), AtomLit(KLitNil(KTypVoid)))
        col_range: (atom_t, atom_t, atom_t) = (AtomLit(KLitNil(KTypVoid)), AtomLit(KLitNil(KTypVoid)), AtomLit(KLitNil(KTypVoid)))
        is_transposed: bool = false;
    }

    fun m_pr_transposed(target: matrix_projection) =
        target.{is_transposed = !target.is_transposed}

    fun m_pr_sliced(target: matrix_projection, row_range: (atom_t, atom_t, atom_t), 
        col_range: (atom_t, atom_t, atom_t), ctx: kctx_t, code: kcode_t): (matrix_projection, kcode_t){
        //TODO: These functions must check if new border is inside of old range.
        //Check it in compile-time, if it's certain number. Or create runtime index check otherwise.
        //Stimulating example:
        //
        //val mothermat = random(rng, (10,10), -2., 2.)
        //val A = mothermat[2:8,:]
        //val B = A[0:7,:] // There must be error 
        //val D = B*C
        //
        //The best optimization with gemm will avoid B matrix and corresponding id check:
        //val D = __intrin_gemm__(mothermat[2:9,:], C)
        //So, code will work, when it haven't to.
        
        fun constriction_bop(bop: binary_t, constriction: atom_t, sec_operand: atom_t, ctx: kctx_t, code: kcode_t): 
                                                                                (atom_t, kcode_t) =
            match sec_operand {
            //It's obvious, that in bop=OpAdd case (constr + Nil) = constr;
            //But, luckily, we use bop=OpMul, only for multiplying constr on delta.
            //delta = Nil means delta = 1, so (constr * Nil) = constr too.
            |AtomLit(KLitNil _) => (constriction,code)
            |_ => 
                val (_, loc) = ctx
                val border_type = get_atom_ktyp(constriction, loc)
                match K_cfold_dealias.cfold_bop(bop, constriction, sec_operand, border_type, loc){
                |Some(KExpAtom(at,_)) => (at, code)
                |_ => 
                    val temp_bop_name = match (constriction, sec_operand) {
                        |(AtomId(nam1), _) => nam1
                        |(_, AtomId(nam2)) => nam2
                        |_ => throw compile_err(loc, f"Subarray border inference error")
                        }
                    val unfolded = KExpBinary(bop, constriction, sec_operand, ctx)
                    val new_constr_name = dup_idk(curr_m_idx, temp_bop_name)
                    val code = create_kdefval(new_constr_name, border_type, default_val_flags(), Some(unfolded), code, loc)
                    (AtomId(new_constr_name), code)
                }
            }
        
        fun static_check(cmp: cmpop_t, op1: atom_t, op2: atom_t, ctx: kctx_t){
            match (op1, op2){
            |(AtomLit(KLitNil _), _)  
            |(_, AtomLit(KLitNil _)) => {}
            |(AtomLit(_), AtomLit(_)) => 
                val (_, loc) = ctx
                match K_cfold_dealias.cfold_bop(OpCmp(cmp), op1, op2, KTypBool, loc){
                |Some(KExpAtom(AtomLit(KLitBool(false)), _)) => throw compile_err(loc, f"Nested subarray borders are inconsistent")
                |_ => {}
                }
            |_ => {}
            }
        }

        fun juxtapose_border(base_range: (atom_t, atom_t, atom_t), constriction: atom_t, is_end: bool,
                                                                     code: kcode_t): (atom_t, kcode_t){
            static_check(CmpLE, AtomLit(KLitInt(0L)), constriction, ctx)
            val (base, end, delta) = base_range 
            match (constriction, base, delta) {
            | (AtomLit(KLitNil _), _, _) => (if is_end {end} else {base}, code)
            | (_, AtomLit(KLitNil _), AtomLit(KLitNil _)) => (constriction, code)
            | _ =>
                val (multiplied_constr, code) = constriction_bop(OpMul, constriction, delta, ctx, code)
                val (res,code) = constriction_bop(OpAdd, multiplied_constr, base, ctx, code)
                //There we check, that new border is inside of old range. It works only for 
                //literal cases. Variable indexes will be checked only on run of gemm function(it's 
                //weak check, but for needs of optimization, we are forgetting about dynamic checks.
                static_check(if is_end {CmpLT} else {CmpLE}, base, res, ctx) 
                static_check(if is_end {CmpLE} else {CmpLT}, res,  end, ctx)
                (res,code)
            }
        }

        fun juxtapose_delta(base_range: (atom_t, atom_t, atom_t), ndelt: atom_t, code: kcode_t): (atom_t, kcode_t){
            static_check(CmpLE, AtomLit(KLitInt(0L)), ndelt, ctx) 
            val (_, _, delta) = base_range 
            match (ndelt, delta) {
            | (AtomLit(KLitNil _), _) => (delta, code)
            | (_, AtomLit(KLitNil _)) => (ndelt, code) 
            | _ => constriction_bop(OpMul, ndelt, delta, ctx, code)
            }
        }

        val (row_range, col_range) = if target.is_transposed {(col_range, row_range)} else {(row_range, col_range)}
        val (rsn, ren, rdn) = row_range
        val (csn, cen, cdn) = col_range
        val (rs,code) = juxtapose_border(target.row_range, rsn, false, code)
        val (re,code) = juxtapose_border(target.row_range, ren, true, code)
        val (rd,code) = juxtapose_delta(target.row_range, rdn, code)
        val (cs,code) = juxtapose_border(target.col_range, csn, false, code)
        val (ce,code) = juxtapose_border(target.col_range, cen, true, code)
        val (cd,code) = juxtapose_delta(target.col_range, cdn, code)
        val new_row_range = (rs, re, rd)
        val new_col_range = (cs, ce, cd)
        val new_target = target.{row_range = new_row_range, col_range = new_col_range}
        (new_target, code)
    }

    fun m_pr_composition(target: matrix_projection, applied: matrix_projection, 
                        ctx: kctx_t, code: kcode_t): (matrix_projection, kcode_t) {
        val {row_range, col_range, is_transposed} = applied
        val target = if is_transposed { m_pr_transposed(target) } else {target}
        m_pr_sliced(target, row_range, col_range, ctx, code)
    }

    fun m_prs2arglist(m_pr1: matrix_projection, m_pr2: matrix_projection){
        val (rs1,re1,rd1) = m_pr1.row_range
        val (cs1,ce1,cd1) = m_pr1.col_range
        val (rs2,re2,rd2) = m_pr2.row_range
        val (cs2,ce2,cd2) = m_pr2.col_range
        [AtomId(m_pr1.original_matrix), AtomLit(KLitBool(m_pr1.is_transposed)), rs1, re1, rd1, cs1, ce1, cd1,
         AtomId(m_pr2.original_matrix), AtomLit(KLitBool(m_pr2.is_transposed)), rs2, re2, rd2, cs2, ce2, cd2]
    }

    fun arglist2m_prs(arglist: atom_t list, loc: loc_t){
        match arglist {
        |AtomId(m1)::AtomLit(KLitBool(t1))::rs1::re1::rd1::cs1::ce1::cd1::
         AtomId(m2)::AtomLit(KLitBool(t2))::rs2::re2::rd2::cs2::ce2::cd2::[] =>
            val m_pr1 = matrix_projection {original_matrix = m1, is_transposed = t1,
                                            row_range = (rs1, re1, rd1), 
                                            col_range = (cs1, ce1, cd1)}
            val m_pr2 = matrix_projection {original_matrix = m2, is_transposed = t2,
                                            row_range = (rs2, re2, rd2), 
                                            col_range = (cs2, ce2, cd2)}
            (m_pr1, m_pr2)
        |_=> throw compile_err(loc, f"IntrinGEMM has wrong arguments")
        } 
    }

    var involved_matrixes = empty_id_hashset(256)

    fun form_involved_matrixes(ktop: kexp_t list) {

        involved_matrixes.clear()
        var matrix_dependencies = Hashmap.empty(1024, noid, noid)
        
        fun fold_matrdep_atom_(a: atom_t, loc: loc_t, callb: k_fold_callb_t) {}
        fun fold_matrdep_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
        fun fold_matrdep_kexp_(e: kexp_t, callb: k_fold_callb_t) {
            match e {
            | KDefVal (n, rhs_e, loc) =>
                if !is_mutable(n, loc) {
                    match rhs_e {
                    | KExpAtom (AtomId(a), (_, loc2)) =>
                        matrix_dependencies.add(n,a)
                    | KExpCall (fname, AtomId(matr)::[], (_, loc)) when
                            !is_mutable(matr,get_idk_loc(matr, noloc)) && 
                            pp(fname) == pp(fname_op_apos()) => //[TODO] Also check somehow original module(instead of module of instance!). We need "Builtins"
                        match get_idk_ktyp(matr, get_idk_loc(matr, noloc)){
                        |KTypArray(2,KTypFloat _) => matrix_dependencies.add(n, matr)
                        | _ => fold_kexp(e, callb)
                        }
                    | KExpAt (AtomId(matr),_,_, DomainRange(_,_,_)::DomainRange(_,_,_)::[], (_, loc) as ctx) when
                            !is_mutable(matr,get_idk_loc(matr, noloc)) => 
                        match (get_idk_ktyp(matr, get_idk_loc(matr, noloc))){
                        |(KTypArray(2,KTypFloat _)) => matrix_dependencies.add(n, matr)
                        | _ => fold_kexp(e, callb)
                        }
                    | _ => fold_kexp(e, callb)
                    }
                }
            | KExpIntrin (IntrinGEMM, args, (t, loc) as ctx) =>
                val (m_pr1, m_pr2) = arglist2m_prs(args, loc)
                involved_matrixes.add(m_pr1.original_matrix)
                involved_matrixes.add(m_pr2.original_matrix)
            | KExpCall (fname, AtomId(matr1)::AtomId(matr2)::[], (_, loc) as ctx) when
                    pp(fname) == pp(fname_op_mul()) =>//[TODO] Also check somehow original module(instead of module of instance!). We need "Builtins"
                val (matr1_t,matr2_t) = (get_idk_ktyp(matr1, get_idk_loc(matr1, noloc)),get_idk_ktyp(matr2, get_idk_loc(matr2, noloc)))
                match (matr1_t,matr2_t){
                |(KTypArray(2,KTypFloat bitt1), KTypArray(2,KTypFloat bitt2)) when 
                        bitt1 == bitt2 =>
                    involved_matrixes.add(matr1)
                    involved_matrixes.add(matr2)
                |_ => fold_kexp(e, callb)
                }
            | _ => fold_kexp(e, callb)
            }
        }

        val matrdep_callb = k_fold_callb_t
        {
            kcb_fold_atom=Some(fold_matrdep_atom_),
            kcb_fold_ktyp=Some(fold_matrdep_ktyp_),
            kcb_fold_kexp=Some(fold_matrdep_kexp_)
        }

        for e <- ktop { fold_matrdep_kexp_(e, matrdep_callb) }
        for matr <- involved_matrixes.list() { 
            fun collect_dep_nodes(m: id_t) = 
                match matrix_dependencies.find_opt(m){
                |Some(m2) => involved_matrixes.add(m); collect_dep_nodes(m2)
                |None => involved_matrixes.add(m)
                }
            collect_dep_nodes(matr)
        }
    }

    var mat_proj_map: (id_t, matrix_projection) Hashmap.t = Hashmap.empty(1024, noid, matrix_projection {})


    fun pp_range(range:(atom_t, atom_t, atom_t)){ //DUBUGGG!!! Delete
        val (rng1, rng2, rng3) = range
        print("(")
        K_pp.pp_atom(rng1,noloc)
        print(", ")
        K_pp.pp_atom(rng2,noloc)
        print(", ")
        K_pp.pp_atom(rng3,noloc)
        print(")")
    }

    fun print_m_pr(m_pr: matrix_projection){ //DUBUGGG!!!
        println(f"DUBUGGG: projected to:{m_pr.original_matrix}")
        print("         row_range: ")
        pp_range(m_pr.row_range)
        print("| col_range: ")
        pp_range(m_pr.col_range)
        println()
        println(f"         is_transposed: {m_pr.is_transposed}")
    }

    fun print_map(){
        println(f"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
        println(f"DUBUGGG: map print")
        for (who, m_pr) <- mat_proj_map.list(){
            println(f"DUBUGGG: consider {who} as:")    
            print_m_pr(m_pr)
        }
        println(f"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    }


    fun opg_atom_(a: atom_t, loc: loc_t, callb: k_callb_t) = a
    fun opg_ktyp_(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun opg_kexp_(e: kexp_t, callb: k_callb_t)
    {
        var extra_decls : kcode_t = []
        /* first, process all the sub-expressions; the only exception is KDefVal,
           which we handle separately */
        val e = match e { | KDefVal _ => e | _ => walk_kexp(e, callb) }
        val e = match e {
        | KDefVal (n, rhs_e, loc) =>
            val rhs_e = opg_kexp_(rhs_e, callb)
            val e = KDefVal(n, rhs_e, loc)
            if !is_mutable(n, loc) {
                match rhs_e {
                | KExpAtom (AtomId(a), (_, loc2)) when involved_matrixes.mem(a) =>
                    match mat_proj_map.find_opt(n){
                    |Some(mat_proj) => mat_proj_map.add(n, mat_proj)
                    |_ => {}
                    }
                | KExpCall (fname, AtomId(matr)::[], (_, loc)) when
                        !is_mutable(matr,get_idk_loc(matr, noloc)) && 
                        involved_matrixes.mem(matr) =>
                    val mat_proj = match mat_proj_map.find_opt(matr){
                        |Some(mat_proj) => mat_proj
                        |None => matrix_projection {original_matrix = matr}
                        }
                    mat_proj_map.add(n, m_pr_transposed(mat_proj))
                | KExpAt (AtomId(matr),_,_, DomainRange(rs,re,rd)::DomainRange(cs,ce,cd)::[], (_, loc) as ctx) when
                        !is_mutable(matr,get_idk_loc(matr, noloc)) &&
                        involved_matrixes.mem(matr) =>
                        val mat_proj = match mat_proj_map.find_opt(matr){
                            |Some(mat_proj) => mat_proj
                            |None => matrix_projection {original_matrix = matr}
                            }
                        val (new_mat_proj, new_extra_decls) = m_pr_sliced(mat_proj, (rs, re, rd), (cs, ce, cd), ctx, extra_decls)
                        extra_decls = new_extra_decls
                        mat_proj_map.add(n, new_mat_proj)
                        //TODO: Print there map for simplest double subarraying. Why some matrixes are handling on second pass?
                | _ => {}
                }
            };e
        | KExpIntrin (IntrinGEMM, args, (t, loc) as ctx) =>
            val (m_pr1, m_pr2) = arglist2m_prs(args, loc)
            fun process_matrix_info(m_pr: matrix_projection) = 
                match mat_proj_map.find_opt(m_pr.original_matrix){
                | Some(tar_m) => 
                    val (m_pr, new_extra_decls) = m_pr_composition(m_pr, tar_m, ctx, extra_decls)
                    extra_decls = new_extra_decls
                    m_pr
                | None => m_pr
                }
            val m_pr1 = process_matrix_info(m_pr1)
            val m_pr2 = process_matrix_info(m_pr2)
            KExpIntrin(IntrinGEMM, m_prs2arglist(m_pr1, m_pr2), ctx)
        | KExpCall (fname, AtomId(matr1)::AtomId(matr2)::[], (_, loc) as ctx) when
                pp(fname) == pp(fname_op_mul()) &&
                involved_matrixes.mem(matr1) &&
                involved_matrixes.mem(matr2) =>
            fun get_matrix_projection(matr: id_t) = match mat_proj_map.find_opt(matr){
                |Some(mat_proj) => mat_proj
                |None => matrix_projection {original_matrix = matr}
            }
            val (m_pr1, m_pr2) = (get_matrix_projection(matr1), get_matrix_projection(matr2))
            KExpIntrin(IntrinGEMM, m_prs2arglist(m_pr1, m_pr2), ctx)
        | _ => e
        }
        val e = if extra_decls == [] { e }
            else { rcode2kexp(e :: extra_decls, get_kexp_loc(e)) }
        e
    }

    val cfd_callb = k_callb_t
    {
        kcb_atom=Some(opg_atom_),
        kcb_ktyp=Some(opg_ktyp_),
        kcb_kexp=Some(opg_kexp_)
    }
    [for km <- kmods {
        val {km_idx, km_top=top_code} = km
        curr_m_idx = km_idx
        form_involved_matrixes(top_code)
        val top_code = [for e <- top_code { opg_kexp_(e, cfd_callb) } ]
        km.{km_top=top_code}
    }]
}
