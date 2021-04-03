/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Do inline expansion of some function calls:
    1. find which functions can be inlined;
       * Inline functions cannot be inlined (including indirectly/mutually recursive functions).
       * Complex functions, i.e. functions that contain other functions, cannot be inlined.
       * Functions with C code bodies or type constructors cannot be inlined.
    2. compute approximate size of each function body. We cannot accurately estimate,
       how much bytes/machine instructions each function will take, since other optimizations
       are also applied after inline expansion, since there are different hardware architectures etc.
       So the calculated size of an expression is very approximate and is a weigted sum
       of K-form constructions.
    3. we compute how many times each function is mentioned (not just called, but mentioned).
       If it's mentioned once and this is the direct call of the function,
       we should always expand this call. If it's mentioned more than once,
       in each direct call we take into account the function size
       and the total size of caller function and the called function. If they are below certain
       thresholds, we do the inline expansion. If a function is declared as inline, we
       try it expand calls to this function before trying to expand calls from this function.
       Otherwise we can do the opposite thing.
*/

from Ast import *
from K_form import *

import Options
import Map, Set, Hashset, Hashmap

type func_info_t =
{
    fi_name: id_t;
    fi_can_inline: bool;
    fi_size: int;
    fi_nrefs: int;
    fi_flags: fun_flags_t
}

type subst_map_t = (id_t, atom_t) Hashmap.t
type fir_map_t = (id_t, func_info_t ref) Hashmap.t

fun find_recursive_funcs(km_name: id_t, top_code: kcode_t): kcode_t
{
    val idset0 = empty_id_hashset(1)
    var all_called: idset_hashmap_t = Hashmap.empty(256, noid, idset0, hash)
    var curr_called = empty_id_hashset(256)

    // For each function find the set of functions _from the same module_ it calls directly
    // Since there cannot be mutual dependency between modules, a recursive function
    // is a function which call graph has a loop all within the same module
    fun fold_recfun_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun fold_recfun_kexp_(e: kexp_t, callb: k_fold_callb_t) =
        match e {
        | KDefFun (ref {kf_name, kf_body, kf_scope, kf_loc}) =>
            val saved_called = curr_called
            val m = curr_module(kf_scope, kf_loc)
            if m != km_name { throw compile_err(kf_loc, f"function from module {pp(m)} resides in module {pp(km_name)}")}
            curr_called = empty_id_hashset(8)
            fold_recfun_kexp_(kf_body, callb)
            all_called.add(kf_name, curr_called)
            curr_called = saved_called
        | KExpCall (f, _, (_, loc)) =>
            match kinfo_(f, loc) {
            | KFun (ref {kf_scope, kf_loc}) =>
                if curr_module(kf_scope, kf_loc) == km_name {
                    curr_called.add(f)
                }
            | _ => {}
            }
        | _ => fold_kexp(e, callb)
        }

    val recfun_callb = k_fold_callb_t
    {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(fold_recfun_ktyp_),
        kcb_fold_kexp=Some(fold_recfun_kexp_)
    }
    for e <- top_code { fold_recfun_kexp_(e, recfun_callb) }

    val iters0 = 10
    val all_funcs = all_called.foldl(
        fun (f, _, all_funcs) { f :: all_funcs }, []).rev()
    val _ = calc_sets_closure(iters0, all_funcs, all_called)

    // now mark the recursive functions, i.e. functions that call themselves
    // directly or indirectly
    for f <- all_funcs {
        match (kinfo_(f, noloc), all_called.find_opt(f)) {
        | (KFun kf, Some fcalled) =>
            val {kf_flags} = *kf
            val flags = kf_flags.{fun_flag_recursive=fcalled.mem(f)}
            *kf = kf->{kf_flags=flags}
        | _ => {}
        }
    }
    top_code
}

fun find_recursive_funcs_all(kmods: kmodule_t list) =
    [: for km <- kmods {
        val {km_name, km_top} = km
        val new_top = find_recursive_funcs(km_name, km_top)
        km.{km_top=new_top}
    } :]

// Calculates the approximate size of each expression in some abstract units;
// of course, it may and should be tuned.
fun calc_exp_size(e: kexp_t)
{
    var sz = 0
    fun fold_size_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun fold_size_kexp_(e: kexp_t, callb: k_fold_callb_t) {
        fold_kexp(e, callb)
        val dsz =
        match e {
        | KExpNop _ | KExpAtom _ | KDefFun _ | KDefExn _
        | KDefVariant _ | KDefTyp _ | KDefInterface _ | KDefClosureVars _ => 0
        | KExpIntrin(IntrinStrConcat, args, _) => args.length()
        | KExpBinary _ | KExpUnary _ | KExpCast _ | KExpIntrin _
        | KExpBreak _ | KExpContinue _ | KExpMem _ | KExpAssign _ | KDefVal _ => 1
        | KExpSeq (elist, _) => elist.length()
        | KExpMkRecord (args, _) => args.length()
        | KExpMkTuple (args, _) => args.length()
        | KExpMkArray (args, _) =>
            fold s = 0 for al <- args {
                fold s = s for (f, a) <- al { s + (if f { 10 } else { 1 }) }
            }
        | KExpMkClosure (_, _, args, _) => args.length()
        | KExpCall (_, args, _) => 5 + args.length()
        | KExpICall (_, _, args, _) => 6 + args.length()
        | KExpAt (_, _, _, idxs, _) => idxs.length()
        | KExpCCode _ => 100
        | KExpThrow _ => 10
        | KExpWhile _ | KExpDoWhile _ | KExpIf _ => 2
        | KExpMatch (cases, _) =>
            fold total = 0 for (checks, _) <- cases {
                total + checks.length()
            }
        | KExpTryCatch _ => 10
        | KExpMap (e_idl_l, _, _, _) =>
            fold total = 0 for (_, idl, _) <- e_idl_l {
                10 + total + idl.length()
            }
        | KExpFor (idl, _, _, _, _) => 10 + idl.length()
        }
        sz += dsz
    }

    val size_callb = k_fold_callb_t {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(fold_size_ktyp_),
        kcb_fold_kexp=Some(fold_size_kexp_)
    }
    fold_size_kexp_(e, size_callb)
    sz
}

// Generates new names for all locally defined names in an expression.
// This is what we need to do when we do inline expansion of a function call
fun subst_names(e: kexp_t, subst_map0: subst_map_t, rename_locals: bool): kexp_t
{
    val subst_map =
    if !rename_locals {
        subst_map0
    } else {
        val decl_set = declared(e::[], 256)
        val loc = get_kexp_loc(e)
        val subst_map = subst_map0.copy()
        decl_set.app(fun (i) {
            match kinfo_(i, loc) {
            | KVal kv =>
                val {kv_name, kv_typ, kv_flags, kv_loc} = kv
                val new_name = dup_idk(kv_name)
                val _ =  create_kdefval(new_name, kv_typ, kv_flags, None, [], kv_loc)
                subst_map.add(kv_name, AtomId(new_name))
            | _ => {}
            }})
        subst_map
    }
    fun subst_atom_(a: atom_t, loc: loc_t, callb: k_callb_t) =
        match a {
        | AtomId i =>
            if i == noid { a }
            else {
                match subst_map.find_opt(i) {
                | Some new_a => new_a
                | _ => a
                }
            }
        | _ => a
        }
    fun subst_ktyp_(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun subst_kexp_(e: kexp_t, callb: k_callb_t) = walk_kexp(e, callb)
    val subst_callb = k_callb_t {
        kcb_atom=Some(subst_atom_),
        kcb_ktyp=Some(subst_ktyp_),
        kcb_kexp=Some(subst_kexp_)
    }
    subst_kexp_(e, subst_callb)
}

// The core of inline expansion
fun expand_call(e: kexp_t) =
    match e {
    | KExpCall (f, real_args, (_, loc)) =>
        match kinfo_(f, loc) {
        // expand only direct function calls, not via-pointer ones
        | KFun kf =>
            val {kf_args, kf_body} = *kf
            val subst_map = Hashmap.empty(256, noid, AtomId(noid), hash)
            for (formal_arg, _) <- kf_args, real_arg <- real_args {
                subst_map.add(formal_arg, real_arg)
            }
            val decl_set = declared(kf_body::[], 256)
            val have_bad_defs = decl_set.exists(
                fun (i) { match kinfo_(i, loc) { | KVal kv => false | _ => true }})
            if have_bad_defs {
                (e, false)
            } else {
                (subst_names(kf_body, subst_map, true), true)
            }
        | _ => (e, false)
        }
    | _ => (e, false)
    }

fun inline_some(kmods: kmodule_t list)
{
    fun gen_default_func_info(nrefs: int) =
        ref (func_info_t {
            fi_name=noid,
            fi_can_inline=false,
            fi_size=0,
            fi_nrefs=nrefs,
            fi_flags=default_fun_flags()
        })
    var all_funcs_info: fir_map_t = Hashmap.empty(1024, noid, gen_default_func_info(-1), hash)

    var curr_fi = gen_default_func_info(0)
    var curr_km_main = false
    fun fold_finfo_atom_(a: atom_t, loc: loc_t, callb: k_fold_callb_t) =
        match a {
        | AtomId f =>
            if f != noid {
                match kinfo_(f, loc) {
                | KFun _ =>
                    val idx = all_funcs_info.find_idx_or_insert(f)
                    var r_fi = all_funcs_info.r->table[idx].data
                    if r_fi->fi_nrefs == -1 {
                        r_fi = gen_default_func_info(0)
                        all_funcs_info.r->table[idx].data = r_fi
                    }
                    r_fi->fi_nrefs += 1
                | _ => {}
                }
            }
        | _ => {}
        }

    fun fold_finfo_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun fold_finfo_kexp_(e: kexp_t, callb: k_fold_callb_t): void =
        match e {
        | KDefFun (ref {kf_name, kf_body, kf_flags, kf_loc}) =>
            val saved_fi = curr_fi
            val idx = all_funcs_info.find_idx_or_insert(kf_name)
            val can_inline = !kf_flags.fun_flag_recursive && !kf_flags.fun_flag_ccode && kf_flags.fun_flag_ctor == CtorNone
            val fsize = calc_exp_size(kf_body)
            var r_fi = all_funcs_info.r->table[idx].data
            if r_fi->fi_nrefs == -1 {
                r_fi = gen_default_func_info(0)
                all_funcs_info.r->table[idx].data = r_fi
            }
            *r_fi = r_fi->{fi_name=kf_name, fi_can_inline=can_inline, fi_size=fsize, fi_flags=kf_flags}
            curr_fi = r_fi
            fold_kexp(e, callb)
            curr_fi = saved_fi
            // functions that contain other functions cannot be inlined
            *saved_fi = saved_fi->{fi_can_inline=false}
        | _ => fold_kexp(e, callb)
        }

    val finfo_callb = k_fold_callb_t {
        kcb_fold_atom=Some(fold_finfo_atom_),
        kcb_fold_ktyp=Some(fold_finfo_ktyp_),
        kcb_fold_kexp=Some(fold_finfo_kexp_)
    }

    /* step 2. try to actually expand some calls */
    fun inline_ktyp_(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun inline_kexp_(e: kexp_t, callb: k_callb_t) =
        match e {
        | KDefFun kf =>
            val {kf_name, kf_body, kf_loc} = *kf
            val saved_fi = curr_fi
            val r_fi =
            match all_funcs_info.find_opt(kf_name) {
            | Some r_fi => r_fi
            | _ => throw compile_err(kf_loc, "inline: function is not found the collected function database")
            }
            curr_fi = r_fi
            val new_body = inline_kexp_(kf_body, callb)
            *kf = kf->{kf_body=new_body}
            curr_fi = saved_fi
            e
        /* we do not expand inline calls at the top level of non-main module,
           because it may ruin some global variables */
        | KExpCall (f, real_args, _)
            when curr_km_main || curr_fi->fi_name != noid =>
            match all_funcs_info.find_opt(f) {
            | Some r_fi =>
                val {fi_can_inline=caller_can_inline, fi_size=caller_size, fi_flags=caller_flags} = *curr_fi
                val caller_is_inline = caller_can_inline && caller_flags.fun_flag_inline
                val inline_thresh = Options.opt.inline_thresh
                val max_caller_size =
                    if caller_is_inline { inline_thresh * 3 / 2 }
                    else { inline_thresh * 10 }
                val {fi_can_inline, fi_size, fi_flags} = *r_fi
                val f_is_inline = fi_can_inline && fi_flags.fun_flag_inline
                val f_max_size = if f_is_inline { inline_thresh * 3 / 2 }
                                 else { inline_thresh }
                val new_size = caller_size + fi_size - real_args.length() - 1
                val new_size = max(new_size, 0)
                if fi_can_inline && (fi_size <= f_max_size && new_size <= max_caller_size) {
                    val (new_e, inlined) = expand_call(e)
                    if inlined { *curr_fi = curr_fi->{fi_size=new_size} }
                    new_e
                } else { e }
            | _ => e
            }
        | _ => walk_kexp(e, callb)
        }

    val inline_callb = k_callb_t {
        kcb_atom=None,
        kcb_ktyp=Some(inline_ktyp_),
        kcb_kexp=Some(inline_kexp_)
    }
    val _ = find_recursive_funcs_all(kmods)
    for {km_top} <- kmods {
        for e <- km_top {
            fold_finfo_kexp_(e, finfo_callb)
        }
    }
    [: for km <- kmods {
        val {km_top, km_main} = km
        //val global_size = calc_exp_size(code2kexp(km_top, noloc))
        curr_fi = gen_default_func_info(0)
        curr_km_main = km_main
        val new_top = [: for e <- km_top { inline_kexp_(e, inline_callb) } :]
        km.{km_top=new_top}
    } :]
}
