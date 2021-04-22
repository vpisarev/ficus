/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    remove unused code from the K-form:
    * any type or exception that is declared but not referenced.
      that includes the types that reference itself (recursive variants),
      but are not referenced anywhere else.
    * any function that is declared but not called (including recursive functions).
      This rule should actually become somewhat more complex in the future, because:
      - for now mutually recursive functions that are not used anywhere
        are not removed because they call each other
      - in a per-module compilation mode (that is not implemented yet)
        only "static", i.e. module-local functions should be removed.
        other unused functions should be excluded at the link stage.
    * if there is a value that is defined but not used, we remove it as well.
      however, if the right-hand-side expression that initializes the value
      is impure, we retain this call, e.g. we replace
      'val usused_flag = func_with_side_effects(args)' with
      'func_with_side_effects(args)'.
      if it's not a temporary value, we issue a warning about unused value.
    * `if (true) a else b` is replaced with `a`
    * `if (false) a else b` is replaced with `b`
       if there is value/variable of type `void` that is assigned,
      `val a: void = f(...)`, it's replaced with `f(...)`
    * if there is a pure expression in the middle of expression sequence
      (i.e. it's not the last expression), we remove it as well.
*/

from Ast import *
from K_form import *
import Options
import Map, Set, Hashmap, Hashset

fun pure_kexp(e: kexp_t): bool
{
    var ispure = true
    var local_vars = empty_id_hashset(8)

    // skip type processing, it does not affect purity flag
    fun pure_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun pure_kexp_(e: kexp_t, callb: k_fold_callb_t) =
        match e {
        | KExpBreak _ => ispure = false
        | KExpContinue _ => ispure = false
        | KExpIntrin (intr, _, _) =>
            match intr {
            | IntrinPopExn | IntrinCheckIdx | IntrinCheckIdxRange => ispure = false
            | _ => {}
            }
        | KExpCall (f, _, (_, loc)) =>
            if !pure_fun(f, loc) { ispure = false }
        | KExpICall _ => ispure = false
        | KExpAssign (i, AtomId j, _) when i == j => {}
        | KExpAssign (i, _, _) =>
            if !local_vars.mem(i) { ispure = false }
        | KExpTryCatch _ => ispure = false
        | KExpThrow _ => ispure = false
        | KExpCCode _ => ispure = false
        | KExpSeq (elist, (_, loc)) =>
            /*
                if we have a block of code, this block,
                as a whole, can be a pure expression
                even if it uses mutable values (i.e. variables)
                internally, e.g. for accumulating results
                using transformed fold operation.
                For example, the following function body and
                hence the function are pure:

                fun sum(a: int []) {
                    var s = 0
                    for x <- a { s += x}
                    s
                }
                the check for tempref is also necessary:
                fun integral_sum(a: int []) {
                    var s = 0
                    for x@i <- a { s += x; a[i] = s }
                }
                This function is not pure anymore and here a[i] assignment
                will be done via creating a tempref
                ... @tempref int* r = &a[i]; *r = s;

                This check is not perfect, as tempref can actually
                be used to access a locally defined record or tuple.
                But it's safe - classifying pure expression as impure
                is safe, whereas the opposite thing is not.
            */
            val saved_local_vars = local_vars
            local_vars = empty_id_hashset(elist.length())
            for e <- elist {
                | KDefVal(i, _, loc) =>
                    match kinfo_(i, loc) {
                    | KVal ({kv_flags}) =>
                        if kv_flags.val_flag_mutable &&
                            !kv_flags.val_flag_tempref {
                                local_vars.add(i)
                        }
                    | _ => {}
                    }
                | _ => {}
                }
            for e <- elist { pure_kexp_(e, callb); if !ispure {break} }
            local_vars = saved_local_vars
        | _ =>
            // skip the check of the sub-expression if 'ispure' is false already
            if ispure { fold_kexp(e, callb) }
        }

    val pure_callb = k_fold_callb_t
    {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(pure_ktyp_),
        kcb_fold_kexp=Some(pure_kexp_)
    }
    pure_kexp_(e, pure_callb)
    ispure
}

fun pure_fun(f: id_t, loc: loc_t) =
    match kinfo_(f, loc) {
    | KFun df =>
        val {kf_body, kf_flags} = *df
        if kf_flags.fun_flag_pure > 0 ||
            (match kf_flags.fun_flag_ctor {
            | CtorNone => false
            | _ => true}) { true }
        else if kf_flags.fun_flag_pure == 0 || kf_flags.fun_flag_ccode { false }
        else {
            *df = df->{kf_flags=kf_flags.{fun_flag_pure=1}}
            val ispure = pure_kexp(kf_body)
            *df = df->{kf_flags=kf_flags.{fun_flag_pure=int(ispure)}}
            ispure
        }
    | KExn _ => true
    | _ => false
    }

fun reset_purity_flags(code: kexp_t list): void {
    // skip type processing, it does not affect purity flag
    fun reset_purity_flags_ktyp_(t: ktyp_t, loc: loc_t,
                                 callb: k_fold_callb_t) {}
    fun reset_purity_flags_kexp_(e: kexp_t, callb: k_fold_callb_t) =
        match e {
        | KDefFun df =>
            val {kf_flags} = *df
            *df = df->{kf_flags=kf_flags.{fun_flag_pure=-1}}
        | _ => fold_kexp(e, callb)
        }
    val callb = k_fold_callb_t
    {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(reset_purity_flags_ktyp_),
        kcb_fold_kexp=Some(reset_purity_flags_kexp_)
    }
    for e <- code {
        reset_purity_flags_kexp_(e, callb)
    }
}

fun remove_unused(kmods: kmodule_t list, initial: bool)
{
    for {km_top} <- kmods {
        reset_purity_flags(km_top)
    }
    val all_top = [: for {km_top} <- kmods {km_top} :].concat()
    val used_somewhere = used_by(all_top, 1024)

    var fold_pairs = empty_idmap
    var is_main = false
    var curr_m_idx = -1
    fun check_m_idx(i: id_t, loc: loc_t) =
        if i.m != curr_m_idx {
            throw compile_err(loc,
                if i.m > 0 {
                    f"the symbol '{pp(i)}' defined in module \
                    '{pp(get_module_name(curr_m_idx))}' is said to belong to module \
                    '{pp(get_module_name(i.m))}'"
                } else {
                    f"the symbol '{pp(i)}' defined in module \
                    '{pp(get_module_name(curr_m_idx))}' does not have a module assigned"
                })
        }

    // nothing to remove in a type
    fun remove_unused_ktyp_(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t

    fun remove_unused_kexp_(e: kexp_t, callb: k_callb_t): kexp_t =
        match e {
        | KDefVal (i, e, loc) =>
            check_m_idx(i, loc)
            val e = remove_unused_kexp_(e, callb)
            val is_ccode = match e { | KExpCCode _ => true | _ => false }
            match e {
            | KExpAtom (AtomId fr, _)
                when get_orig_id(fr) == std__fold_result__ =>
                    fold_pairs = fold_pairs.add(i, fr)
            | _ => {}
            }
            val is_really_used = used_somewhere.mem(i) || is_ccode
            if is_really_used {
                KDefVal(i, e, loc)
            } else {
                val {kv_flags} = get_kval(i, loc)
                if initial && Options.opt.W_unused && !is_val_global(kv_flags) &&
                   !kv_flags.val_flag_temp && !kv_flags.val_flag_tempref {
                    compile_warning(loc, f"'{pp(i)}' is declared but not used")
                }
                if !pure_kexp(e) {
                    e
                } else {
                    KExpNop(loc)
                }
            }
        | KDefFun kf =>
            val {kf_name, kf_body, kf_scope, kf_loc} = *kf
            check_m_idx(kf_name, kf_loc)
            if used_somewhere.mem(kf_name) {
                val new_body = remove_unused_kexp_(kf_body, callb)
                *kf = kf->{kf_body=new_body}
                e
            } else {
                if initial && Options.opt.W_unused && !is_global_scope(kf_scope) {
                    compile_warning(kf_loc, f"local function '{pp(kf_name)}' is declared but not used")
                }
                KExpNop(kf_loc)
            }
        | KDefExn ke =>
            val {ke_name, ke_loc} = *ke
            check_m_idx(ke_name, ke_loc)
            if used_somewhere.mem(ke_name) || !is_main { e }
            else { KExpNop(ke_loc) }
        | KDefVariant kvar =>
            /* if at least one of the variant constructors is used then
               the constructor call returns the variant type,
               i.e. the variant name gets used. So, we may be sure that
               we will never eliminate variant definition and yet retain
               some of its used constructors. */
            val {kvar_name, kvar_loc} = *kvar
            check_m_idx(kvar_name, kvar_loc)
            if used_somewhere.mem(kvar_name) {e} else {KExpNop(kvar_loc)}
        | KDefTyp kt =>
            val {kt_name, kt_loc} = *kt
            check_m_idx(kt_name, kt_loc)
            if used_somewhere.mem(kt_name) { e }
            else { KExpNop(kt_loc) }
        | KDefInterface ki =>
            val {ki_name, ki_loc} = *ki
            check_m_idx(ki_name, ki_loc)
            if used_somewhere.mem(ki_name) { e }
            else {
                KExpNop(ki_loc)
            }
        | KDefClosureVars kcv =>
            val {kcv_name, kcv_loc} = *kcv
            check_m_idx(kcv_name, kcv_loc)
            if used_somewhere.mem(kcv_name) { e }
            else { KExpNop(kcv_loc) }
        | KExpSeq (code, (ktyp, loc)) =>
            val code = remove_unused_(code, [], callb)
            match code.rev() {
            | (KExpAtom (AtomId j, _) :: KDefVal (i, rhs, _) :: rest) when i == j =>
                // replace { ... val i = <exp>; i } with { ... <exp> }
                rcode2kexp(rhs :: rest, loc)
            | _ => code2kexp(code, loc)
            }
        | _ => walk_kexp(e, callb)
        }
    fun remove_unused_(code: kexp_t list, result: kexp_t list,
                     callb: k_callb_t): kexp_t list =
        match code {
        | e :: rest =>
            val e = remove_unused_kexp_(e, callb)
            val result =
            match e {
            | KExpNop _ =>
                if rest == [] { e :: result } else { result }

            // special optimizaion of fold pass-through fold:
            // fold res = ... { if ... { foo(res) } else { res }}
            // will be transformed to imperative for loop where
            // val res = __fold_result__ will be inserted
            // in the beginning of the body loop
            // and __fold_result__ = res might be in the second branch
            // this effect-less assignment will be eliminated
            | KExpAssign (fr, AtomId r, loc)
                when get_orig_id(fr) == std__fold_result__ &&
                fold_pairs.find_opt(r).value_or(noid) == fr =>
                result

            | KDefVal _ | KDefFun _ | KDefExn _ | KDefVariant _
            | KDefTyp _ | KDefClosureVars _ | KDefInterface _ =>
                e :: result
            | _ =>
                if rest != [] && pure_kexp(e) { result }
                else { e :: result }
            }
            remove_unused_(rest, result, callb)
        | _ => result.rev()
        }

    val remove_callb = k_callb_t {
        kcb_ktyp=Some(remove_unused_ktyp_),
        kcb_kexp=Some(remove_unused_kexp_),
        kcb_atom=None
    }
    [: for km <- kmods {
        val {km_top, km_idx, km_main} = km
        is_main = km_main
        curr_m_idx = km_idx
        val new_top = remove_unused_(km_top, [], remove_callb)
        km.{km_top=new_top}
    } :]
}

fun remove_unused_by_main(kmods: kmodule_t list)
{
    val all_main_ids = empty_id_hashset(1024)
    val all_top_ids = empty_id_hashset(1024)
    var all_top: (id_t, kexp_t) list = []
    var all_actions: kexp_t list = []

    // Find all top-level declarations;
    // Also copy all top-level declarations from
    // the main module into a separate set
    for km <- kmods {
        val {km_top, km_main} = km
        for e <- km_top {
            | KDefVal (n, _, _) =>
                all_top_ids.add(n)
                all_top = (n, e) :: all_top
                if km_main { all_main_ids.add(n) }
            | KDefFun (ref {kf_name}) =>
                all_top_ids.add(kf_name)
                all_top = (kf_name, e) :: all_top
                if km_main { all_main_ids.add(kf_name) }
            | KDefTyp (ref {kt_name}) =>
                all_top_ids.add(kt_name)
                all_top = (kt_name, e) :: all_top
                if km_main { all_main_ids.add(kt_name) }
            | KDefVariant (ref {kvar_name}) =>
                all_top_ids.add(kvar_name)
                all_top = (kvar_name, e) :: all_top
                if km_main { all_main_ids.add(kvar_name) }
            | KDefInterface (ref {ki_name}) =>
                all_top_ids.add(ki_name)
                all_top = (ki_name, e) :: all_top
                if km_main { all_main_ids.add(ki_name) }
            | KDefClosureVars (ref {kcv_name}) =>
                all_top_ids.add(kcv_name)
                all_top = (kcv_name, e) :: all_top
                if km_main { all_main_ids.add(kcv_name) }
            | KDefExn (ref {ke_name, ke_tag}) =>
                all_top_ids.add(ke_name)
                all_top_ids.add(ke_tag)
                all_top = (ke_name, e) :: all_top
                all_main_ids.add(ke_name)
                all_main_ids.add(ke_tag)
            | _ =>
                // we assume that after the very first round of dead code elimination
                // all remaining top-level expressions which are not declarations
                // are some actions (print etc. statements), we need to retain them
                all_actions = e :: all_actions
        }
    }

    // For each symbol find its direct top-level dependencies
    val idset0 = empty_id_hashset(1)
    var all_deps = Hashmap.empty(1024, noid, idset0, hash)
    for (n, e) <- all_top {
        val deps = used_by(e :: [], 16)
        deps.intersect(all_top_ids)
        all_deps.add(n, deps.compress())
    }

    // calculate and add to the hash table all the direct dependencies
    // of top-level non-declarations (i.e. actions)
    val all_main_deps = all_main_ids.copy()
    val used_by_actions = used_by(all_actions, 1024)
    used_by_actions.intersect(all_top_ids)
    all_main_deps.union(used_by_actions)

    val main_id = get_id("__main__")
    all_deps.add(main_id, all_main_deps)

    // And then indirect top-level dependencies by calculating transitive closure
    val iters0 = 10
    val _ = calc_sets_closure(iters0, main_id :: [: for (n, _) <- all_top {n} :], all_deps)

    // Extract all dependencies for the main module
    val all_main_deps = match all_deps.find_opt(main_id) {
        | Some(deps) => deps
        | _ => all_main_deps // should not get here; probably need to report an error
        }

    // Finally, exclude everything that is not needed by the main module
    [: for km <- kmods {
        val {km_top, km_main} = km
        if km_main {
            km // retain everything in the main module
        } else {
            val fold new_top = [] for e <- km_top {
                | KDefVal (n, _, _) =>
                    if all_main_deps.mem(n) { e :: new_top } else { new_top }
                | KDefFun (ref {kf_name=n}) =>
                    if all_main_deps.mem(n) { e :: new_top } else { new_top }
                | KDefTyp (ref {kt_name=n}) =>
                    if all_main_deps.mem(n) { e :: new_top } else { new_top }
                | KDefVariant (ref {kvar_name=n}) =>
                    if all_main_deps.mem(n) { e :: new_top } else { new_top }
                | KDefInterface (ref {ki_name=n}) =>
                    if all_main_deps.mem(n) { e :: new_top } else { new_top }
                | KDefClosureVars (ref {kcv_name=n}) =>
                    if all_main_deps.mem(n) { e :: new_top } else { new_top }
                | KDefExn (ref {ke_name=n}) =>
                    // always include all the exceptions
                    e :: new_top
                | _ => e :: new_top
            }
            km.{km_top = new_top.rev()}
        }
    } :]
}
