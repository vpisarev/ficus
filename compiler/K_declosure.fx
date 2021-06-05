/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    We call a declosuring an optimization used for functions that have free variables, but
    never used other way than calling(such as saving to variables or values or passing as 
    parameter). In this case we just adding pass free variables as arguments and avoiding any
    closure run-time overhead.

    Note: It's assumed there, that handled code don't have mutable freevars. That means, that
    declosure_all can be called only after K.freevars.mutable_freevars_referencing
*/


from Ast import *
from K_form import *
from K_freevars import *
import K_lift_simple
import Hashmap, Hashset

fun declosure_all(kmods: kmodule_t list){

    val globals = empty_id_hashset(256)
    for {km_top} <- kmods {
        K_lift_simple.update_globals(km_top, globals)
    }

    var fv_env : fv_env_t = collect_free_vars(kmods, globals, true, false, true)

    fun fold_filcl_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}

    fun fold_filcl_atom_(a: atom_t, loc: loc_t, callb: k_fold_callb_t): void  = 
        match a {
        | AtomId(n) => 
            match kinfo_(n, loc) {
            | KFun (ref {kf_name}) => fv_env.remove(kf_name)
            | _ => {}
            }
        | _ => {}
        }

    fun fold_filcl_kexp_(e: kexp_t, callb: k_fold_callb_t)
    {
        match e {
        | KDefFun kf =>
            val {kf_params, kf_body, kf_closure, kf_loc} = *kf
            val {kci_wrap_f} = kf_closure
            //We cannot use fold_kexp directly on e, because we don't want to erase just defined 
            //function via fold_filcl_atom_. Thus, we are folding different details separately.
            for arg <- kf_params {check_n_fold_id(arg, kf_loc, callb)}
            check_n_fold_id(kci_wrap_f, kf_loc, callb)
            fold_kexp(kf_body,callb)
        | KExpCall(_, args, (_, loc)) =>
            for arg <- args {check_n_fold_atom(arg, loc, callb)}
        | _ => fold_kexp(e, callb)
        }
    }

    val filcl_callb = k_fold_callb_t
    {
        kcb_fold_atom=Some(fold_filcl_atom_),
        kcb_fold_ktyp=Some(fold_filcl_ktyp_),
        kcb_fold_kexp=Some(fold_filcl_kexp_)
    }
    
    //Filter out functions met as values or arguments from fv_env
    //and functions don't have freevars. Also sort freevars names(future function arguments) 
    //for result code view stability.
    for km <- kmods {
        val {km_top} = km
        for e <- km_top {fold_filcl_kexp_(e, filcl_callb)}
    }
    val fv_env = fv_env.foldl(fun (kf_name, func_info, sorted_env: (id_t, id_t list) Hashmap.t){
            val {fv_fvars} = func_info
            if(!fv_fvars.empty()) {
                val sorted_fvs : id_t list = sort_freevars(fv_fvars)
                sorted_env.add(kf_name, sorted_fvs) } 
            sorted_env
        }, Hashmap.empty(1024, noid, ([]: id_t list)))
    //Key of map is original name of mutable vars. Value is substitutional tempory reference.
    //It's different along function hierarchy
    var subst_map = Hashmap.empty(1024, noid, noid)

    fun walk_atom_n_declojure(a: atom_t, loc: loc_t, callb: k_callb_t) =
        match a {
        | AtomId({m=0}) => a
        | AtomId n =>
            match subst_map.find_opt(n) {
            | Some(replace) => AtomId(replace)
            | _ => a
            }
        | _ => a
        }

    // pass-by processing types
    fun walk_ktyp_n_declojure(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun walk_kexp_n_declojure(e: kexp_t, callb: k_callb_t) = 
        match e {
        | KDefFun kf =>
            val {kf_name, kf_loc, kf_params, kf_body} = *kf
            match fv_env.find_opt(kf_name){
                | Some(fvars) =>
                    val m_idx = kf_name.m
                    val subst_map_backup = subst_map.copy()
                    val params_addition = [: for fv <- fvars { 
                            val {kv_typ, kv_flags, kv_loc} = get_kval(fv, kf_loc)
                            //Freevars are always in the same module, as a declosured function.
                            val new_fv = dup_idk(m_idx, fv)

                            //BUGREPORT: 
                            // val _ = create_kdefval(new_fv, kv_typ, kv_flags, None, [], kv_loc)
                            // Uncomment previous line and comment two equivalent nexts:
                            val dv = kdefval_t {kv_name=new_fv, kv_cname="", kv_typ=kv_typ, kv_flags=kv_flags, kv_loc=kv_loc}
                            set_idk_entry(new_fv, KVal(dv))
                            //and get this error:
                            //error: properties of non-type '_fx_Nt6option1N14K_form__kexp_t' cannot be requested

                            subst_map.add(fv,new_fv)
                            new_fv
                        } :]
                    val params = List.concat([: kf_params, params_addition :])
                    val body = walk_kexp_n_declojure(kf_body,callb)
                    subst_map = subst_map_backup
                    *kf=kf->{kf_params = params, kf_body = body}
                    KDefFun(kf)
                | _ => walk_kexp(e,callb)
            }
        | KExpCall(f, args, (_, loc) as kctx) =>
            match fv_env.find_opt(f){
                | Some(fvars) =>
                    val args = List.concat([: args, [: for fv <- fvars { AtomId(fv) } :] :])
                    val args = [: for a <- args { walk_atom_n_declojure(a, loc, callb) } :]
                    KExpCall(f, args, kctx)
                | _ => walk_kexp(e,callb)
            }
        | _ => walk_kexp(e,callb)
        }

    val walk_n_declojure_callb = k_callb_t
    {
        kcb_atom=Some(walk_atom_n_declojure),
        kcb_ktyp=Some(walk_ktyp_n_declojure),
        kcb_kexp=Some(walk_kexp_n_declojure)
    }

    [: for km <- kmods {
        val {km_top} = km
        val curr_top_code = [:for e <- km_top {walk_kexp_n_declojure(e, walk_n_declojure_callb)}:]
        km.{km_top=curr_top_code}
    } :]    
}