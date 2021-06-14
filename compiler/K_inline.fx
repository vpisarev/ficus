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
    fi_flags: fun_flags_t;
    fi_km_idx: int;
}

type subst_map_t = (id_t, atom_t) Hashmap.t
type fir_map_t = (id_t, func_info_t ref) Hashmap.t

fun find_recursive_funcs(km_idx: int, top_code: kcode_t): kcode_t
{
    val idset0 = empty_id_hashset(1)
    var all_called: idset_hashmap_t = Hashmap.empty(256, noid, idset0)
    var curr_called = empty_id_hashset(256)

    // For each function find the set of functions _from the same module_ it calls directly
    // Since there cannot be mutual dependency between modules, a recursive function
    // is a function which call graph has a loop all within the same module
    fun fold_recfun_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun fold_recfun_kexp_(e: kexp_t, callb: k_fold_callb_t) =
        match e {
        | KDefFun (ref {kf_name, kf_body, kf_scope, kf_loc}) =>
            val saved_called = curr_called
            val m = curr_module(kf_scope)
            if m != km_idx { throw compile_err(kf_loc,
                f"function from module {pp(get_module_name(m))} \
                resides in module {pp(get_module_name(km_idx))}")}
            curr_called = empty_id_hashset(8)
            fold_recfun_kexp_(kf_body, callb)
            all_called.add(kf_name, curr_called)
            curr_called = saved_called
        | KExpCall (f, _, (_, loc)) =>
            match kinfo_(f, loc) {
            | KFun (ref {kf_scope, kf_loc}) =>
                if curr_module(kf_scope) == km_idx { curr_called.add(f) }
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
    [for km <- kmods {
        val {km_idx, km_top} = km
        val new_top = find_recursive_funcs(km_idx, km_top)
        km.{km_top=new_top}
    }]

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
        | KExpSync (_, e) => 10
        | KExpMkRecord (args, _) => args.length()
        | KExpMkTuple (args, _) => args.length()
        | KExpMkArray (all_literals, args, _) =>
            if all_literals {
                1
            } else {
                fold s = 0 for al <- args {
                    fold s = s for (f, a) <- al { s + (if f { 10 } else { 1 }) }
                }
            }
        | KExpMkVector (args, _) =>
            fold s = 0 for (f, a) <- args { s + (if f { 10 } else { 1 }) }
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
fun subst_names(km_idx: int, e: kexp_t, subst_map0: subst_map_t, rename: bool): (kexp_t, subst_map_t)
{
    var subst_map = subst_map0
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
    fun subst_id_(i: id_t, loc: loc_t) =
        match subst_map.find_opt(i) {
        | Some (AtomId(new_i)) => new_i
        | Some _ => throw compile_err(loc, f"id is expected to be produced after renaming '{idk2str(i, loc)}', not literal")
        | _ => i
        }
    fun subst_scope(sc: scope_t list, loc: loc_t): scope_t list =
        match sc {
        | ScFun(f) :: rest =>
            val f = subst_id_(f, loc)
            ScFun(f) :: subst_scope(rest, loc)
        | ScModule(m) :: rest =>
            ScModule(km_idx) :: subst_scope(rest, loc)
        | ScBlock(_) :: rest =>
            new_block_scope(km_idx) :: subst_scope(rest, loc)
        | ScLoop(nested, _) :: rest =>
            new_loop_scope(km_idx, nested) :: subst_scope(rest, loc)
        | ScMap(_) :: rest =>
            new_map_scope(km_idx) :: subst_scope(rest, loc)
        | ScArrMap(_) :: rest =>
            new_arr_map_scope(km_idx) :: subst_scope(rest, loc)
        | ScFold(_) :: rest =>
            new_fold_scope(km_idx) :: subst_scope(rest, loc)
        | ScTry(_) :: rest =>
            new_try_scope(km_idx) :: subst_scope(rest, loc)
        | sc :: rest => sc :: subst_scope(rest, loc)
        | _ => []
        }
    fun subst_kval_(i: id_t, loc: loc_t, callb: k_callb_t)
    {
        val kv = get_kval(i, loc)
        val {kv_typ, kv_flags} = kv
        val new_i = subst_id_(i, loc)
        val new_kv_typ = walk_ktyp(kv_typ, loc, callb)
        val new_kv_flags = kv_flags.{
                val_flag_global=subst_scope(kv_flags.val_flag_global, loc)
            }
        val new_kv = kv.{kv_name=new_i, kv_typ=new_kv_typ, kv_flags=new_kv_flags}
        set_idk_entry(new_i, KVal(new_kv))
        new_i
    }
    fun subst_kf_(kf: kdeffun_t ref, callb: k_callb_t)
    {
        val {kf_name, kf_params, kf_rt, kf_body, kf_closure, kf_scope, kf_loc} = *kf
        val {kci_arg, kci_fcv_t, kci_fp_typ, kci_make_fp, kci_wrap_f} = kf_closure
        ref (kf->{
            kf_name=subst_id_(kf_name, kf_loc),
            kf_params=[for a <- kf_params { subst_kval_(a, kf_loc, callb) } ],
            kf_rt = walk_ktyp(kf_rt, kf_loc, callb),
            kf_body = walk_kexp(kf_body, callb),
            kf_closure = kdefclosureinfo_t {
                kci_arg = subst_id_(kci_arg, kf_loc),
                kci_fcv_t = subst_id_(kci_fcv_t, kf_loc),
                kci_fp_typ = subst_id_(kci_fp_typ, kf_loc),
                kci_make_fp = subst_id_(kci_make_fp, kf_loc),
                kci_wrap_f = subst_id_(kci_wrap_f, kf_loc)
                },
            kf_scope = subst_scope(kf_scope, kf_loc)
            })
    }
    fun subst_kexp_(e: kexp_t, callb: k_callb_t)
    {
        fun subst_idlist_(nlist: id_t list, loc: loc_t) =
            [for n <- nlist {subst_id_(n, loc)} ]
        match e {
        /*| KDefVal (n, e, loc) =>
            val new_n = subst_id_(n, loc)
            val e = subst_kexp_(e, callb)
            KDefVal(new_n, e, loc)*/
        | KDefFun kf =>
            val new_kf = subst_kf_(kf, callb)
            set_idk_entry(new_kf->kf_name, KFun(new_kf))
            KDefFun(new_kf)
        | KDefVariant kvar =>
            val {kvar_name, kvar_cases, kvar_ifaces, kvar_ctors, kvar_scope, kvar_loc} = *kvar
            val new_kvar = ref(kvar->{
                kvar_name = subst_id_(kvar_name, kvar_loc),
                kvar_cases = [for (n, t) <- kvar_cases { (n, walk_ktyp(t, kvar_loc, callb)) } ],
                kvar_ifaces = [for (iname, meths) <- kvar_ifaces {
                    (subst_id_(iname, kvar_loc), subst_idlist_(meths, kvar_loc))}],
                kvar_ctors = subst_idlist_(kvar_ctors, kvar_loc),
                kvar_scope = subst_scope(kvar_scope, kvar_loc)
                })
            if kvar_name.m != km_idx && new_kvar->kvar_proto == noid {
                new_kvar->kvar_proto = kvar_name
            }
            set_idk_entry(new_kvar->kvar_name, KVariant(new_kvar))
            KDefVariant(new_kvar)
        | KDefTyp kt =>
            val {kt_name, kt_typ, kt_scope, kt_loc} = *kt
            val new_kt = ref(kt->{
                kt_name = subst_id_(kt_name, kt_loc),
                kt_typ = walk_ktyp(kt_typ, kt_loc, callb),
                kt_scope = subst_scope(kt_scope, kt_loc)
                })
            if kt_name.m != km_idx && new_kt->kt_proto == noid {
                new_kt->kt_proto = kt_name
            }
            set_idk_entry(new_kt->kt_name, KTyp(new_kt))
            KDefTyp(new_kt)
        | KDefInterface ki =>
            val {ki_name, ki_base, ki_id, ki_all_methods, ki_scope, ki_loc} = *ki
            val new_ki = ref(ki->{
                ki_name=subst_id_(ki_name, ki_loc),
                ki_base=subst_id_(ki_base, ki_loc),
                ki_id=subst_id_(ki_id, ki_loc),
                ki_all_methods=[for (f, t) <- ki_all_methods {(subst_id_(f, ki_loc), walk_ktyp(t, ki_loc, callb))} ],
                ki_scope = subst_scope(ki_scope, ki_loc)
                })
            set_idk_entry(new_ki->ki_name, KInterface(new_ki))
            KDefInterface(new_ki)
        | KDefClosureVars kcv =>
            val {kcv_name, kcv_freevars, kcv_orig_freevars, kcv_scope, kcv_loc} = *kcv
            val new_kcv = ref(kcv->{
                kcv_name = subst_id_(kcv_name, kcv_loc),
                kcv_freevars = [for (n, t) <- kcv_freevars {
                                (subst_id_(n, kcv_loc), walk_ktyp(t, kcv_loc, callb)) }],
                kcv_orig_freevars = subst_idlist_(kcv_orig_freevars, kcv_loc),
                kcv_scope = subst_scope(kcv_scope, kcv_loc)
                })
            set_idk_entry(new_kcv->kcv_name, KClosureVars(new_kcv))
            KDefClosureVars(new_kcv)
        | _ => walk_kexp(e, callb)
        }
    }
    val subst_callb = k_callb_t {
        kcb_atom=Some(subst_atom_),
        kcb_ktyp=None,
        kcb_kexp=Some(subst_kexp_)
    }
    if rename {
        val decl_set = declared(e::[], 256)
        //print_id_hashset("declared (to be renamed)", decl_set)
        val loc = get_kexp_loc(e)
        subst_map = subst_map0.copy()
        decl_set.app(fun (i) {
            if i.m > 0 {
                val new_name = dup_idk(km_idx, i)
                subst_map.add(i, AtomId(new_name))
            }})
        decl_set.app(fun (i) {
            match kinfo_(i, loc) {
            | KVal kv =>
                val { kv_name, kv_typ, kv_flags, kv_loc } = kv
                val new_kv = kdefval_t {
                    kv_name = subst_id_(kv_name, kv_loc),
                    kv_cname = "",
                    kv_typ = walk_ktyp(kv_typ, kv_loc, subst_callb),
                    kv_flags = kv_flags.{
                        val_flag_global=subst_scope(kv_flags.val_flag_global, kv_loc)
                        },
                    kv_loc = kv_loc
                    }
                set_idk_entry(new_kv.kv_name, KVal(new_kv))
            | _ => {}
            }})
    }
    (subst_kexp_(e, subst_callb), subst_map)
}

// The core of inline expansion
fun expand_call(km_idx: int, e: kexp_t) =
    match e {
    | KExpCall (f, real_args, (_, loc)) =>
        match kinfo_(f, loc) {
        // expand only direct function calls, not via-pointer ones
        | KFun kf =>
            val {kf_params, kf_body} = *kf
            val subst_map = Hashmap.empty(256, noid, AtomId(noid))
            for formal_arg <- kf_params, real_arg <- real_args {
                subst_map.add(formal_arg, real_arg)
            }
            val decl_set = declared(kf_body::[], 256)
            val have_bad_defs = decl_set.exists(
                fun (i) { match kinfo_(i, loc) { | KVal kv => false | _ => true }})
            if have_bad_defs {
                (e, false)
            } else {
                val (e, _) = subst_names(km_idx, kf_body, subst_map, true)
                (e, true)
            }
        | _ => (e, false)
        }
    | _ => (e, false)
    }

fun inline_some(kmods: kmodule_t list)
{
    var curr_km_idx = -1
    fun gen_default_func_info(nrefs: int) =
        ref (func_info_t {
            fi_name=noid,
            fi_can_inline=false,
            fi_size=0,
            fi_nrefs=nrefs,
            fi_flags=default_fun_flags(),
            fi_km_idx=curr_km_idx
        })
    var all_funcs_info: fir_map_t = Hashmap.empty(1024, noid, gen_default_func_info(-1))

    var curr_fi = gen_default_func_info(0)
    var curr_km_main = false

    fun fold_finfo_atom_(a: atom_t, loc: loc_t, callb: k_fold_callb_t) =
        match a {
        | AtomId f =>
            if f != noid {
                match kinfo_(f, loc) {
                | KFun _ =>
                    val idx = all_funcs_info.find_idx_or_insert(f)
                    var r_fi = all_funcs_info.table[idx].data
                    if r_fi->fi_nrefs == -1 {
                        r_fi = gen_default_func_info(0)
                        all_funcs_info.table[idx].data = r_fi
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
            val can_inline = !kf_flags.fun_flag_recursive &&
                             !kf_flags.fun_flag_ccode &&
                             kf_flags.fun_flag_ctor == CtorNone
            val fsize = calc_exp_size(kf_body)
            var r_fi = all_funcs_info.table[idx].data
            if r_fi->fi_nrefs == -1 {
                r_fi = gen_default_func_info(0)
                all_funcs_info.table[idx].data = r_fi
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
            | _ => throw compile_err(kf_loc,
                "inline: function is not found the collected function database")
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
            | Some r_fi when r_fi->fi_km_idx == curr_km_idx =>
                val { fi_can_inline=caller_can_inline,
                      fi_size=caller_size,
                      fi_flags=caller_flags } = *curr_fi
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
                    val (new_e, inlined) = expand_call(curr_km_idx, e)
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
    for {km_top, km_idx} <- kmods {
        curr_km_idx = km_idx
        for e <- km_top {
            fold_finfo_kexp_(e, finfo_callb)
        }
    }
    [for km <- kmods {
        val {km_top, km_idx, km_main} = km
        //val global_size = calc_exp_size(code2kexp(km_top, noloc))
        curr_fi = gen_default_func_info(0)
        curr_km_main = km_main
        curr_km_idx = km_idx
        val new_top = [for e <- km_top { inline_kexp_(e, inline_callb) } ]
        km.{km_top=new_top}
    }]
}
