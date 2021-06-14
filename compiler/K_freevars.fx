/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Module contains utlities for working with free variables.

    Free variable is variable which is used in function more deep
    than function where it was defined. Global variables is exception
    because they are accessed anywhere. Final C-code don't contain
    free variables, they are converted to arguments of function in
    one or the other view in process of declosuring or lambda lifting.
    Both processess uses collect_free_vars function.

    Since mutable variables must be handled in a special way in these
    processess, there also defined mutable_freevars_referencing function for
    converting each mutable free variable to immutable free variable and
    set of mutable usual variables. It create reference variable with value of
    original freevar and dereferencing it into local temporary variables(tempref)
    in start of each function using original freevar.
*/

from Ast import *
from K_form import *
import K_lift_simple
import Hashmap, Hashset

type fv_func_info_t =
{
    fv_fvars: id_hashset_t;
    fv_declared_inside: id_hashset_t;
    fv_called_funcs: id_hashset_t
}

type fv_env_t = (id_t, fv_func_info_t) Hashmap.t

fun empty_fv_env(size0: int){
    val fv_info0 = fv_func_info_t {
        fv_fvars = empty_id_hashset(1),
        fv_declared_inside = empty_id_hashset(1),
        fv_called_funcs = empty_id_hashset(1) }

    Hashmap.empty(size0, noid, fv_info0)
}

fun collect_free_vars(kmods: kmodule_t list, globals: id_t Hashset.t,
                                                collect_function_info:bool,
                                                only_mutable:bool,
                                                transitive_closure:bool): fv_env_t {
// collect_function_info - don't collect fv_called_funcs info if it's false.
// only_mutable - filter out immutable varibles
// transitive_closure - compute the transitive closure of the free var sets,
//     i.e. all the free variables of each function,
//     directly accessed or via other functions that the function calls.
//     The term 'closure' is used here with a different meaning,
//     it's not a function closure but rather a transitive closure of the set.
    var fv_env : fv_env_t = empty_fv_env(1024)

    fun fold_fv0_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun fold_fv0_kexp_(e: kexp_t, callb: k_fold_callb_t) {
        fold_kexp(e, callb)
        match e {
        | KDefFun (ref {kf_name, kf_loc}) =>
            if !globals.mem(kf_name) {
                val uv = used_by(e::[], 256)
                val dv = declared(e::[], 256)
                // from the set of free variables we exclude global functions, values and types
                // because they are not included into a closure anyway
                val called_funcs = empty_id_hashset(16)
                if(collect_function_info){
                    uv.app(fun (n) {
                        match kinfo_(n, kf_loc) {
                        | KFun _ => if !globals.mem(n) { called_funcs.add(n) }
                        | _ => {}
                        }})
                }
                val fv0 = empty_id_hashset(uv.size())
                uv.app(fun (fv) {
                    match kinfo_(fv, kf_loc) {
                    | KVal _ =>
                        if !dv.mem(fv) && !globals.mem(fv) {
                            if(!only_mutable || is_mutable(fv, get_idk_loc(fv, noloc))){
                                fv0.add(fv)
                            }
                        }
                    | _ => {}
                    }})
                fv_env.add(kf_name,
                    fv_func_info_t {
                        fv_fvars=fv0, fv_declared_inside=dv,
                        fv_called_funcs=called_funcs
                    })
            }
        | _ => {}
        }
    }

    val fv0_callb = k_fold_callb_t
    {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(fold_fv0_ktyp_),
        kcb_fold_kexp=Some(fold_fv0_kexp_)
    }

    for {km_top} <- kmods {
        for e <- km_top { fold_fv0_kexp_(e, fv0_callb) }
    }

    if (transitive_closure) {
        fun finalize_sets(iters: int, ll_all: id_t list)
        {
            var visited_funcs = empty_id_hashset(1024)
            val idset0 = empty_id_hashset(1)
            var changed = false
            if iters <= 0 {
                throw compile_err(noloc, "finalization of the free var sets takes too much iterations")
            }
            fun update_fvars(f: id_t): id_hashset_t =
                match fv_env.find_opt(f) {
                | Some ll_info =>
                    val {fv_fvars, fv_declared_inside, fv_called_funcs} = ll_info
                    if visited_funcs.mem(f) {
                        fv_fvars
                    } else {
                        visited_funcs.add(f)
                        val size0 = fv_fvars.size()
                        fv_called_funcs.app(
                            fun (called_f: id_t) {
                                val called_fvars = update_fvars(called_f)
                                called_fvars.app(fun (fv) {
                                    if !fv_declared_inside.mem(fv) {
                                        fv_fvars.add(fv)
                                    }})
                            })
                        val size1 = fv_fvars.size()
                        if size1 != size0 { changed = true }
                        fv_fvars
                    }
                | _ => idset0
                }

            for f <- ll_all { ignore(update_fvars(f)) }
            if !changed { iters - 1 }
            else { finalize_sets(iters - 1, ll_all.rev()) }
        }

        val iters0 = 10
        val ll_all = fv_env.foldl(fun (f, _, ll_all) { f :: ll_all }, []).rev()
        ignore(finalize_sets(iters0, ll_all))
    }

    fv_env
}

fun mutable_freevars_referencing(kmods: kmodule_t list) {

    val globals = empty_id_hashset(256)
    for {km_top} <- kmods {
        K_lift_simple.update_globals(km_top, globals)
    }

    var fv_env : fv_env_t = collect_free_vars(kmods, globals, false, true, false)

    //Key of map is original name of mutable vars. Value is immutable service reference,
    //used in function prolgues to initialize replacing temporary references.
    var ref_map = Hashmap.empty(1024, noid, noid)

    //Key of map is original name of mutable vars. Value is substitutional tempory reference.
    //It's different along function hierarchy
    var subst_map = Hashmap.empty(1024, noid, noid)

    val afv_fvars = empty_id_hashset(fv_env.size()*10)
    fv_env.app(fun (_, ll_info) {
            afv_fvars.union(ll_info.fv_fvars)
        })

    fun walk_atom_n_eliminate(a: atom_t, loc: loc_t, callb: k_callb_t) =
        match a {
        | AtomId({m=0}) => a
        | AtomId n =>
            match subst_map.find_opt(n) {
            | Some(replace) => AtomId(replace)
            | _ => a
            }
        | _ => a
        }

    fun walk_ktyp_n_eliminate(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t

    fun walk_kexp_n_eliminate(e: kexp_t, callb: k_callb_t) =
        match e {
        | KDefVal(kv_name, rhs, loc) =>
            val rhs = walk_kexp(rhs, callb)
            if afv_fvars.mem(kv_name) {
                // each mutable variable, which is a free variable of at least one function
                // needs to be converted into a reference. The reference (as a reference)
                // should be stored in the corresponding function closure,
                // but all other accesses to this variable will be done
                // via dereferencing operator, unary '*'.
                val kv = get_kval(kv_name, loc)
                val {kv_typ, kv_flags, kv_loc} = kv
                val m_idx = kv_name.m
                val new_kv_name = gen_idk(m_idx, pp(kv_name) + "_ref")
                val new_kv_typ = KTypRef(kv_typ)
                val new_kv_flags = kv_flags.{val_flag_mutable=false}
                val new_kv = kdefval_t { kv_name=new_kv_name,
                    kv_cname="", kv_typ=new_kv_typ,
                    kv_flags=new_kv_flags, kv_loc=kv_loc }
                val new_old_kv = kv.{kv_flags=kv_flags.{val_flag_tempref=true}}
                set_idk_entry(new_kv_name, KVal(new_kv))
                set_idk_entry(kv_name, KVal(new_old_kv))
                ref_map.add(kv_name, new_kv_name)
                val (a, code) = kexp2atom(m_idx, pp(kv_name) + "_arg", rhs, false, [])
                val code = KDefVal(new_kv_name, KExpUnary(OpMkRef, a, (new_kv_typ, loc)), loc) :: code
                val code = KDefVal(kv_name, KExpUnary(OpDeref, AtomId(new_kv_name), (kv_typ, loc)), loc) :: code
                KExpSeq(code.rev(), (KTypVoid, loc))
            } else {
                KDefVal(kv_name, rhs, loc)
            }
        | KDefFun kf =>
            val {kf_name, kf_loc, kf_body } = *kf
            val fvars = match fv_env.find_opt(kf_name) {
            | Some ll_info => ll_info.fv_fvars
            | _ =>  empty_id_hashset(1)
            }

            val subst_map_backup = if !fvars.empty() {subst_map.copy()} else {Hashmap.empty(1, noid, noid)}

            val fold prologue = [] for fv <- fvars.list() {
                val ref_fv = match ref_map.find_opt(fv) {
                    | Some ref_fv => ref_fv
                    | None => throw compile_err(kf_loc, f"free variable '{idk2str(fv, kf_loc)}' of function \
                                '{idk2str(kf_name, kf_loc)}' is not defined before the function body")
                    }

                val (t, kv_flags) =
                    match kinfo_(fv, kf_loc) {
                    | KVal ({kv_typ, kv_flags}) => (kv_typ, kv_flags)
                    | _ => throw compile_err(kf_loc, f"id '{idk2str(fv, kf_loc)}' was assumed to be a val, but it's not. Function \
                                '{idk2str(kf_name, kf_loc)}'.")
                    }
                val m_idx = fv.m
                val fv_proxy = dup_idk(m_idx, fv)
                val deref_exp = KExpUnary(OpDeref, AtomId(ref_fv), (t, kf_loc))
                subst_map.add(fv, fv_proxy)
                create_kdefval(fv_proxy, t, kv_flags, Some(deref_exp), prologue, kf_loc)
            }

            val body_loc = get_kexp_loc(kf_body)
            val body = walk_kexp_n_eliminate(kf_body, callb)
            val body = code2kexp(prologue.rev() + kexp2code(body), body_loc)
            if !fvars.empty() { subst_map = subst_map_backup }
            *kf = kf->{kf_body=body}
            KDefFun(kf)
        | _ => walk_kexp(e, callb)
        }

    val walk_n_eliminate_callb = k_callb_t
    {
        kcb_atom=Some(walk_atom_n_eliminate),
        kcb_ktyp=Some(walk_ktyp_n_eliminate),
        kcb_kexp=Some(walk_kexp_n_eliminate)
    }

    [for km <- kmods {
        val {km_top} = km
        val curr_top_code = [for e <- km_top {walk_kexp_n_eliminate(e, walk_n_eliminate_callb)}]
        km.{km_top=curr_top_code}
    }]
}

// Just to make sure that closure stays stable with minor modifications
// of the program all the free variables are sorted by name
// Used for declosuring and for closure field creation.
fun sort_freevars(fvars: id_hashset_t){
    val fvar_pairs_to_sort = fvars.foldl(
        fun (fv, fvars_to_sort) {
            (string(fv), fv) :: fvars_to_sort
        }, [])
    val fvar_pairs_sorted = fvar_pairs_to_sort.sort(fun ((a, _), (b, _)) { a < b })
    [for (_, fv) <- fvar_pairs_sorted {fv} ]
}