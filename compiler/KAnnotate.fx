/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Finds out all the recursive types defined in the code.
    Also, for each type collects the list of all other types that it references,
    directly or indirectly.

    The algorithm is similar to one used in k_lift.ml:
        * we start with the initial sets of dependencies for each type
        * and then we recursively find closures of those sets
*/

from Ast import *
from KForm import *
import Map, Set

// For each type finds the set of its direct dependencies,
// i.e. the names of other types that are mentioned in its definition, e.g.
// type exp_t = ... | ExpTyped: (exp_t, typ_t, loc_t)
// typ_t and loc_t will be included into the set of exp_t's dependencies
fun get_typ_deps(n: id_t, loc: loc_t): idset_t
{
    fun get_ktyp_deps_(t: ktyp_t, deps: idset_t) =
        match t {
        | KTypInt | KTypCInt | KTypSInt _ | KTypUInt _ | KTypFloat _
        | KTypVoid | KTypBool | KTypChar | KTypString
        | KTypCPointer | KTypExn | KTypErr | KTypModule => deps
        | KTypFun (args, rt) =>
            fold deps = deps for t <- rt :: args { get_ktyp_deps_(t, deps) }
        | KTypTuple tl =>
            fold deps = deps for t <- tl { get_ktyp_deps_(t, deps) }
        | KTypRecord (rn, relems) =>
            fold deps = deps for (_, ti) <- relems { get_ktyp_deps_(ti, deps) }
        | KTypName i => deps.add(i)
        | KTypArray (_, et) => get_ktyp_deps_(et, deps)
        | KTypList et => get_ktyp_deps_(et, deps)
        | KTypRef et => get_ktyp_deps_(et, deps)
        }

    match kinfo_(n, loc) {
    | KVariant (ref {kvar_cases}) =>
        fold deps = empty_idset for (_, ti) <- kvar_cases { get_ktyp_deps_(ti, deps) }
    | KTyp (ref {kt_typ}) => get_ktyp_deps_(kt_typ, empty_idset)
    | _ => throw compile_err(loc, f"the symbol '{idk2str(n, loc)}' is not a type")
    }
}

type idref_map_t = (id_t, idset_t ref) Map.t
val empty_idref_map : idref_map_t = Map.empty(cmp_id)

fun find_recursive(top_code: kcode_t)
{
    var dep_env = empty_idref_map

    // form the initial sets of depndencies and collect them in a map for each type id
    fun fold_deps0_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun fold_deps0_kexp_(e: kexp_t, callb: k_fold_callb_t) =
        match e {
        | KDefVariant (ref {kvar_name, kvar_loc}) =>
            val deps = get_typ_deps(kvar_name, kvar_loc)
            dep_env = dep_env.add(kvar_name, ref deps)
        | KDefTyp (ref {kt_name, kt_loc}) =>
            val deps = get_typ_deps(kt_name, kt_loc)
            dep_env = dep_env.add(kt_name, ref deps)
        | _ => fold_kexp(e, callb)
        }

    val deps0_callb = k_fold_callb_t
    {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(fold_deps0_ktyp_),
        kcb_fold_kexp=Some(fold_deps0_kexp_)
    }

    for e <- top_code {
        fold_deps0_kexp_(e, deps0_callb)
    }

    // Now recursively update those sets,
    // i.e. compute the transitive closures of the sets.
    //
    // There is a little optimization. When we need to update
    // some dependency sets, we do not want to rebuild the whole path
    // (~log(N) nodes) of the tree in order to slightly update it:
    // retain the key and just augment its value.
    // Instead we use references for the values (idset_t ref), so
    // we just modify the values in-place.
    fun finalize_deps(iters: int, all_typs: id_t list): int
    {
        var visited_typs = empty_idset
        var changed = false
        if iters <= 0 {
            throw compile_err(noloc, "finalization of the defined types' dependency sets takes too much iterations")
        }
        fun update_deps(n: id_t): idset_t =
            match dep_env.find_opt(n) {
            // rdeps can be spelled as ref (dependencies) or as
            // recursive dependencies, but actually it's both.
            | Some rdeps =>
                if visited_typs.mem(n) {
                    *rdeps
                } else {
                    visited_typs = visited_typs.add(n)
                    val size0 = rdeps->size
                    val upd_deps = rdeps->foldl(
                        fun (d: id_t, deps: idset_t) {
                            if d == n {deps}
                            else {
                                val ddeps = update_deps(d)
                                deps.union(ddeps)
                            }
                        }, *rdeps)
                    val size1 = upd_deps.size
                    if size1 != size0 {
                        *rdeps = upd_deps
                        changed = true
                    }
                    upd_deps
                }
            | _ => empty_idset
            }

        for n <- all_typs { ignore(update_deps(n)) }
        if !changed { iters - 1 }
        else { finalize_deps(iters - 1, all_typs.rev()) }
    }

    val iters0 = 10
    val all_typs = dep_env.foldl(
        fun (n: id_t, _, all_typs: id_t list) {n :: all_typs}, []).rev()
    val _ = finalize_deps(iters0, all_typs)

    fun is_recursive(n: id_t): bool =
        match dep_env.find_opt(n) {
        | Some rdeps => rdeps->mem(n)
        | _ => false
        }

    for n <- all_typs {
        if is_recursive(n) {
            match kinfo_(n, noloc) {
            | KVariant kvar =>
                val {kvar_flags} = *kvar
                *kvar = kvar->{kvar_flags=kvar_flags.{var_flag_recursive=true}}
            | _ => {}
            }
        }
    }
}

fun get_ktprops(t: ktyp_t, loc: loc_t): ktprops_t
{
    var visited = empty_idset
    fun get_ktprops_(t: ktyp_t, loc: loc_t) =
        match t {
        | KTypInt | KTypCInt | KTypSInt _ | KTypUInt _ | KTypFloat _
        | KTypVoid | KTypBool | KTypChar | KTypErr  =>
            ktprops_t { ktp_complex=false, ktp_scalar=true, ktp_ptr=false,
                        ktp_pass_by_ref=false, ktp_custom_free=false,
                        ktp_custom_copy=false }
        | KTypString | KTypArray _ | KTypExn | KTypFun _ | KTypModule  =>
            ktprops_t { ktp_complex=true, ktp_scalar=false, ktp_ptr=false,
                        ktp_pass_by_ref=true, ktp_custom_free=false,
                        ktp_custom_copy=false }
        | KTypCPointer =>
            ktprops_t { ktp_complex=true, ktp_scalar=false, ktp_ptr=true,
                        ktp_pass_by_ref=false, ktp_custom_free=false,
                        ktp_custom_copy=false }
        | KTypTuple elems =>
            val have_complex = exists(for ti <- elems {get_ktprops_(ti, loc).ktp_complex})
            ktprops_t { ktp_complex=have_complex, ktp_scalar=false, ktp_ptr=false,
                        ktp_pass_by_ref=true, ktp_custom_free=have_complex,
                        ktp_custom_copy=have_complex }
        | KTypRecord (_, relems) =>
            val have_complex = exists(for (_, ti) <- relems {get_ktprops_(ti, loc).ktp_complex})
            ktprops_t { ktp_complex=have_complex, ktp_scalar=false, ktp_ptr=false,
                        ktp_pass_by_ref=true, ktp_custom_free=have_complex,
                        ktp_custom_copy=have_complex }
        | KTypList et =>
            val have_complex = get_ktprops_(et, loc).ktp_complex
            ktprops_t { ktp_complex=true, ktp_scalar=false, ktp_ptr=true,
                        ktp_pass_by_ref=false, ktp_custom_free=have_complex,
                        ktp_custom_copy=false }
        | KTypRef et =>
            val have_complex = get_ktprops_(et, loc).ktp_complex
            ktprops_t { ktp_complex=true, ktp_scalar=false, ktp_ptr=true,
                        ktp_pass_by_ref=false, ktp_custom_free=have_complex,
                        ktp_custom_copy=false }
        | KTypName n =>
            match kinfo_(n, loc) {
            | KVariant kvar =>
                val {kvar_name, kvar_cname, kvar_cases, kvar_props, kvar_flags, kvar_loc} = *kvar
                match kvar_props {
                | Some kvp => kvp
                | _ =>
                    val kvp =
                    if kvar_flags.var_flag_recursive {
                        ktprops_t { ktp_complex=true, ktp_scalar=false, ktp_ptr=true,
                                    ktp_pass_by_ref=false, ktp_custom_free=true,
                                    ktp_custom_copy=false }
                    } else {
                        if visited.mem(n) {
                            throw compile_err(loc,
                                f"unexpected recursive variant {idk2str(kvar_name, kvar_loc)}")
                        }
                        visited = visited.add(n)
                        val have_complex = exists(for (_, ti) <- kvar_cases {
                                get_ktprops_(ti, kvar_loc).ktp_complex
                            })
                        ktprops_t { ktp_complex=have_complex, ktp_scalar=false,
                                    ktp_ptr=false, ktp_pass_by_ref=true,
                                    ktp_custom_free=have_complex,
                                    ktp_custom_copy=have_complex }
                    }
                    *kvar = kvar->{kvar_props=Some(kvp)}
                    kvp
                }
            | KTyp kt =>
                val { kt_name, kt_cname, kt_typ, kt_props, kt_loc } = *kt
                match kt_props {
                | Some ktp => ktp
                | _ =>
                    if visited.mem(n) {
                        throw compile_err(loc, f"unexpected recursive type {idk2str(kt_name, kt_loc)}")
                    }
                    visited = visited.add(n)
                    val ktp = get_ktprops_(kt_typ, kt_loc)
                    *kt = kt->{kt_props=Some(ktp)}
                    ktp
                }
            | _ => throw compile_err(loc, f"unsupported named type '{idk2str(n, loc)}'")
            }
        }

    get_ktprops_(t, loc)
}

fun clear_typ_annotations(top_code: kcode_t) =
    for e <- top_code {
        | KDefVariant kvar => *kvar = kvar->{kvar_props=None, kvar_flags=default_variant_flags()}
        | KDefTyp kt => *kt = kt->{kt_props=None}
        | _ => {}
    }

fun annotate_types(kmods: kmodule_t list)
{
    val top_code = List.concat([: for {km_top} <- kmods {km_top} :])
    clear_typ_annotations(top_code)
    find_recursive(top_code)
    for e <- top_code {
        | KDefVariant kvar =>
            val {kvar_name, kvar_loc} = *kvar
            val _ = get_ktprops(KTypName(kvar_name), kvar_loc)
            val {kvar_cases, kvar_ctors, kvar_flags} = *kvar
            val is_recursive = kvar_flags.var_flag_recursive
            val ncases = kvar_cases.length()
            // Option-like variants are variants with exactly 2 cases,
            // where the first one is just label (i.e. of 'void' type).
            // in such variants valid tag values start with 0, not with 1.
            // In all other variants tag=0 means "uninitialized variant"
            val option_like = match kvar_cases {
                              | (_, KTypVoid ) :: (_, _) :: [] => true
                              | _ => false
                              }
            // Single-case variant can be recursive (see ycomb.fx for example) or not.
            // Non-recursive single-case variant, obviously, does not need a tag.
            // It's just filled with zeros by default, and does not need to be destructed.
            // Recursive single-case variant, just like any other recursive variant,
            // is represented by a pointer, and the null pointer means uninitialized variant.
            //
            // In the case of option-like recursive variant null pointer means both
            // uninitialized variant and the first case.
            // Non-zero pointer means the second case.
            //
            // So, for both of those types of variants we do not need a tag.
            val no_tag = ncases == 1 || (is_recursive && option_like)
            if !is_recursive {
                // if the variant is non-recursive then all its constructors
                // can be marked as "nothrow", because they do not allocate memory.
                for ctor <- kvar_ctors {
                    match kinfo_(ctor, kvar_loc) {
                    | KFun kf =>
                        val {kf_flags} = *kf
                        *kf = kf->{kf_flags=kf_flags.{fun_flag_nothrow=true}}
                    | _ => {}
                    }
                }
            }
            *kvar = kvar->{kvar_flags=kvar_flags.{var_flag_opt=option_like, var_flag_have_tag=!no_tag}}
        | KDefTyp (ref {kt_name, kt_loc}) => ignore(get_ktprops(KTypName(kt_name), kt_loc))
        | _ => {}
        }
    kmods
}
