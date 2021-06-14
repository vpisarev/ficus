/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Convert all non-primitive types to KTypName(...)
    (including lists, arrays, references, tuples, records etc.).
    This is a useful step to make K-form closer to the final C code.
*/

from Ast import *
from K_form import *
import Hashmap

type mangle_map_t = (string, id_t) Hashmap.t

fun mangle_mname(m: string): string = m.replace(".", "__")

fun mangle_scope(sc: scope_t list, result: string, loc: loc_t) =
    match sc {
    | ScModule m :: rest =>
        val mstr = mangle_mname(pp(get_module_name(m)))
        val result = if mstr == "Builtins" { result }
                     else if result == "" { mstr }
                     else { mstr + "__" + result }
        mangle_scope(rest, result, loc)
    | _ :: rest => mangle_scope(rest, result, loc)
    | _ => result
    }

/* [TODO] when we add support for non-English characters in the identifiers,
    they should be "transliterated" into English
    (however, it looks like recent versions of clang/GCC
    support Unicode identifiers) */
fun mangle_name(n: id_t, sc_opt: scope_t list?, loc: loc_t): string
{
    val sc = match sc_opt {
             | Some sc => sc
             | _ => get_kscope(kinfo_(n, loc))
             }
    val prefix = mangle_scope(sc, "", loc)
    val nstr = pp(n)
    if prefix == "" { nstr } else { prefix + "__" + nstr }
}

/* try to compress the name by encoding the module name just once;
   for now it's used only for functions */
fun compress_name(nstr: string, sc: scope_t list, loc: loc_t)
{
    val prefix = mangle_scope(sc, "", loc)
    if prefix == "" {
        nstr
    } else {
        val len = nstr.length()
        val prefix_ = prefix + "__"
        val prefix_len = prefix_.length()
        var i = 0, start = 0
        var result = ""

        while i < len {
            val c = nstr[i]; i += 1
            if c == 'F' || c == 'V' || c == 'R' {
                if i < len && nstr[i] == 't' { i += 1 }
                val j = i
                if i < len && nstr[i].isdigit() {
                    while true {
                        i += 1
                        if i >= len || !nstr[i].isdigit() { break }
                    }
                    if i+prefix_len <= len && nstr[i:i+prefix_len] == prefix_ {
                        val idlen0 = nstr[j:i].to_int_or(0)
                        val idlen1 = idlen0 - prefix_len
                        result += nstr[start:j] + "M" + string(idlen1) +
                                  nstr[i+prefix_len:i+idlen0]
                        i += idlen0
                        start = i
                    }
                }
            }
        }

        result += nstr[start:i]
        "M" + string(prefix.length()) + prefix + result
    }
}

/* Check if <prefix><nlen><name><suffix> is unique.
   If yes, add it to the set of mangled names and output.
   Otherwise, try <prefix><nlen+2><name>1_<suffix>,
   then <prefix><nlen+2><name>2_<suffix> etc.
   For example, with prefix="V", name="rbtree" and suffix="1i"
   first try V6rbtree1i, then V8rbtree1_1i,
      V8rbtree2_1i, ...,  V9rbtree10_1i etc.
   Note, that the name is preceded with its length
   (that includes the possible "1_" etc. in the end) */
fun mangle_make_unique( n_id: id_t, prefix: string, name: string,
    suffix: string, mangle_map: mangle_map_t): (string, string)
{
    fun make_unique_(idx: int) {
        val idxstr = if idx == 0 { "" } else { string(idx) + "_" }
        val name1 = name + idxstr
        val nlen = name1.length()
        val candidate_base_name = prefix + string(nlen) + name1
        val candidate = candidate_base_name + suffix
        if mangle_map.mem(candidate) {
            make_unique_(idx + 1)
        } else {
            mangle_map.add(candidate, n_id)
            (candidate_base_name, candidate)
        }
    }
    make_unique_(0)
}

fun add_fx(str: string) =
    if str.startswith("_fx_") { str } else { "_fx_" + str }
fun remove_fx(str: string) =
    if str.startswith("_fx_") { str[4:] } else { str }

/* Convert type to a string, i.e. mangle it.
   Use mangle_map to control uniqueness when
   mangling KTypName _ and KTypRecord _.
   Update the mangled names (cname), if needed,
   for those KTypName _ and KTypRecord _. */
fun mangle_ktyp(t: ktyp_t, mangle_map: mangle_map_t, loc: loc_t): string
{
    fun mangle_inst_(n_id: id_t, prefix: string, targs: ktyp_t list,
        name: id_t, sc: scope_t list): (string, string)
    {
        val nargs = targs.length()
        val fold result = [] for targ <- targs {
            mangle_ktyp_(targ, result)
        }
        val (prefix, suffix) =
            if nargs == 0 { (prefix, "") }
            else { (prefix + "t", string(nargs) + "".join(result.rev())) }
        val name = mangle_name(name, Some(sc), loc)
        val (mangled_basename, mangled_name) =
            mangle_make_unique(n_id, prefix, name, suffix, mangle_map)
        (mangled_basename, mangled_name)
    }
    fun mangle_typname_(n: id_t, result: string list): string list =
        match kinfo_(n, loc) {
        | KVariant kvar =>
            val {kvar_name, kvar_cname, kvar_proto, kvar_targs, kvar_scope, kvar_loc} = *kvar
            if kvar_cname == "" {
                val cname =
                    if kvar_proto != noid {
                        val _ = mangle_typname_(kvar_proto, [])
                        match kinfo_(kvar_proto, kvar_loc) {
                        | KVariant (ref {kvar_cname}) => remove_fx(kvar_cname)
                        | _ => throw compile_err(kvar_loc, "kvar_proto must be a variant")
                        }
                    } else {
                        mangle_inst_(kvar_name, "N", kvar_targs, kvar_name, kvar_scope).1
                    }
                *kvar = kvar->{kvar_cname=add_fx(cname)}
                cname :: result
            } else {
                remove_fx(kvar_cname) :: result
            }
        | KTyp((ref {kt_typ=KTypRecord (_, _)}) as kt) =>
            val {kt_name, kt_cname, kt_proto, kt_targs, kt_scope, kt_loc} = *kt
            if kt_cname == "" {
                val cname =
                    if kt_proto != noid {
                        val _ = mangle_typname_(kt_proto, [])
                        match kinfo_(kt_proto, kt_loc) {
                        | KTyp (ref {kt_typ=KTypRecord(_, _), kt_cname}) => remove_fx(kt_cname)
                        | _ => throw compile_err(kt_loc, "kt_proto must be a type (record) definition")
                        }
                    } else {
                        mangle_inst_(kt_name, "R", kt_targs, kt_name, kt_scope).1
                    }
                *kt = kt->{kt_cname=add_fx(cname)}
                cname :: result
            } else {
                remove_fx(kt_cname) :: result
            }
        | KTyp (ref {kt_cname}) =>
            if kt_cname == "" {
                throw compile_err(loc, "KTyp does not have a proper mangled name")
            } else {
                remove_fx(kt_cname) :: result
            }
        | KInterface ki =>
            val {ki_name, ki_cname, ki_id, ki_scope, ki_loc} = *ki
            if ki_cname == "" {
                val (_, cname) =
                    mangle_inst_(ki_name, "I", [], ki_name, ki_scope)
                *ki = ki->{ki_cname=add_fx(cname)}
                val kv = get_kval(ki_id, ki_loc)
                set_idk_entry(ki_id, KVal (kv.{kv_cname = "_FX_" + cname + "_id"}))
                cname :: result
            } else {
                remove_fx(ki_cname) :: result
            }
        | _ => throw compile_err(loc, f"unsupported type '{idk2str(n, loc)}' (should be variant or record)")
        }
    fun mangle_ktyp_(t: ktyp_t, result: string list): string list =
        match t {
        | KTypInt  => "i" :: result
        | KTypSInt 8 => "c" :: result
        | KTypSInt 16 => "s" :: result
        | KTypSInt 32 => "n" :: result
        // maybe it's not very good, but essentially CInt~"int" is equivalent to "int32_t"
        | KTypCInt  => "n" :: result
        | KTypSInt 64 => "l" :: result
        | KTypSInt n => throw compile_err(loc, f"unsupported typ KTypSInt({n})")
        | KTypUInt 8 => "b" :: result
        | KTypUInt 16 => "w" :: result
        | KTypUInt 32 => "u" :: result
        | KTypUInt 64 => "q" :: result
        | KTypUInt n => throw compile_err(loc, f"unsupported typ KTypUInt({n})")
        | KTypFloat 16 => "h" :: result
        | KTypFloat 32 => "f" :: result
        | KTypFloat 64 => "d" :: result
        | KTypFloat n => throw compile_err(loc, f"unsupported typ KTypFloat({n})")
        | KTypVoid  => "v" :: result
        | KTypBool  => "B" :: result
        | KTypChar  => "C" :: result
        | KTypString  => "S" :: result
        | KTypCPointer  => "p" :: result
        | KTypFun (args, rt) =>
            val result = mangle_ktyp_(rt, "FP" :: result)
            val result = string(args.length()) :: result
            fold result = result for a <- args {
                mangle_ktyp_(a, result)
            }
        | KTypTuple elems =>
            val nelems = elems.length()
            val nstr = string(nelems)
            match elems {
            | t0 :: rest =>
                if rest.all(fun (t) { t == t0 }) {
                    mangle_ktyp_(t0, nstr :: "Ta" :: result)
                } else {
                    fold result = nstr :: "T" :: result for t <- elems {
                        mangle_ktyp_(t, result)
                    }
                }
            | _ => throw compile_err(loc, "the tuple has 0 elements")
            }
        | KTypRecord (rn, _) => mangle_typname_(rn, result)
        | KTypName n => mangle_typname_(n, result)
        | KTypArray (dims, t) =>
            val result = string(dims) :: "A" :: result
            mangle_ktyp_(t, result)
        | KTypList t => mangle_ktyp_(t, "L" :: result)
        | KTypVector t => mangle_ktyp_(t, "V" :: result)
        | KTypRef t => mangle_ktyp_(t, "r" :: result)
        | KTypExn  => "E" :: result
        | KTypErr  => throw compile_err(loc, "KTypErr cannot be mangled")
        | KTypModule  => throw compile_err(loc, "KTypModule cannot be mangled")
        }
    "".join(mangle_ktyp_(t, []).rev())
}

fun mangle_all(kmods: kmodule_t list, final_mode: bool) {
    val mangle_map: mangle_map_t = Hashmap.empty(1024, "", noid)
    var curr_top_code: kcode_t = []
    var curr_km_idx = -1

    fun create_gen_typ(t: ktyp_t, name_prefix: string, loc: loc_t)
    {
        val cname = mangle_ktyp(t, mangle_map, loc)
        match mangle_map.find_opt(cname) {
        | Some(i) => KTypName(i)
        | _ =>
            val i = gen_idk(curr_km_idx, name_prefix)
            val kt = ref (kdeftyp_t {
                kt_name=i, kt_cname=add_fx(cname),
                kt_targs=[], kt_typ=t, kt_proto=noid,
                kt_props=None, kt_scope=[], kt_loc=loc})
            mangle_map.add(cname, i)
            set_idk_entry(i, KTyp(kt))
            curr_top_code = KDefTyp(kt) :: curr_top_code
            KTypName(i)
        }
    }

    fun walk_ktyp_n_mangle(t: ktyp_t, loc: loc_t, callb: k_callb_t): ktyp_t
    {
        val t = walk_ktyp(t, loc, callb)
        match t {
        | KTypInt | KTypCInt | KTypSInt _ | KTypUInt _ | KTypFloat _
        | KTypVoid | KTypBool | KTypChar | KTypString | KTypCPointer
        | KTypExn | KTypErr | KTypModule => t
        | KTypName n => ignore(mangle_ktyp(t, mangle_map, loc)); t
        | KTypRecord (rn, _) => ignore(mangle_ktyp(t, mangle_map, loc))
                                KTypName(rn)
        | KTypFun _ => if final_mode {create_gen_typ(t, "fun", loc)} else {t}
        | KTypTuple _ => if final_mode {create_gen_typ(t, "tup", loc)} else {t}
        | KTypArray _ => t
        | KTypVector _ => t
        | KTypList _ => if final_mode {create_gen_typ(t, "lst", loc)} else {t}
        | KTypRef _ => if final_mode {create_gen_typ(t, "ref", loc)} else {t}
        }
    }
    fun mangle_id_typ(i: id_t, loc: loc_t, callb: k_callb_t) =
        if i != noid {
            match kinfo_(i, loc) {
            | KVal kv =>
                val {kv_typ, kv_cname, kv_flags} = kv
                val t = walk_ktyp_n_mangle(kv_typ, loc, callb)
                if kv_cname == "" {
                    val cname =
                        match get_val_scope(kv_flags) {
                        | ScBlock _ :: _ => ""
                        | sc =>
                            val bare_name = mangle_name(i, Some(sc), loc)
                            val (_, cname) = mangle_make_unique(i, "_fx_g", bare_name, "", mangle_map)
                            cname
                        }
                    set_idk_entry(i, KVal(kv.{kv_typ=if final_mode {t} else {kv_typ}, kv_cname=cname}))
                }
                t
            | _ =>
                walk_ktyp_n_mangle(get_idk_ktyp(i, loc), loc, callb)
            }
        } else { KTypVoid }
    fun mangle_ktyp_retain_record(t: ktyp_t, loc: loc_t, callb: k_callb_t) =
        match t {
        | KTypRecord (rn, relems) =>
            KTypRecord(rn, [for (ni, ti) <- relems {
                                (ni, walk_ktyp_n_mangle(ti, loc, callb))
                            }])
        | t => walk_ktyp_n_mangle(t, loc, callb)
        }
    fun mangle_idoml(idoml: (id_t, dom_t) list, at_ids: id_t list,
                             loc: loc_t, callb: k_callb_t)
    {
        for i <- at_ids { ignore(mangle_id_typ(i, loc, callb)) }
        for (k, _) <- idoml { ignore(mangle_id_typ(k, loc, callb)) }
    }
    fun walk_kexp_n_mangle(e: kexp_t, callb: k_callb_t) =
        match e {
        | KDefVal (n, e, loc) =>
            val e = walk_kexp_n_mangle(e, callb)
            ignore(mangle_id_typ(n, loc, callb))
            KDefVal(n, e, loc)
        | KDefFun kf =>
            val {kf_name, kf_params, kf_rt, kf_body, kf_closure, kf_scope, kf_loc} = *kf
            val {kci_fcv_t} = kf_closure
            for a <- kf_params {
                ignore(mangle_id_typ(a, kf_loc, callb))
            }
            val rt = walk_ktyp_n_mangle(kf_rt, kf_loc, callb)
            val ktyp = get_kf_typ(kf_params, rt, kf_loc)
            val suffix = mangle_ktyp(ktyp, mangle_map, kf_loc)
            val suffix = suffix[2:]
            val new_body = walk_kexp_n_mangle(kf_body, callb)
            val bare_name = mangle_name(kf_name, Some(kf_scope), kf_loc)
            val (_, cname) = mangle_make_unique(kf_name, "F", bare_name, suffix, mangle_map)
            val cname = add_fx(compress_name(cname, kf_scope, kf_loc))
            val mangled_ktyp = walk_ktyp_n_mangle(ktyp, kf_loc, callb)
            val mangled_ktyp_id =
            match mangled_ktyp {
            | KTypName tn => tn
            | _ =>
                if !final_mode {noid} else {
                    throw compile_err(kf_loc, f"mangle: cannot mangle '{cname}' type down to alias")
                }
            }
            if kci_fcv_t != noid {
                match kinfo_(kci_fcv_t, kf_loc) {
                | KClosureVars kcv =>
                    val {kcv_freevars, kcv_loc} = *kcv
                    val cv_cname = cname + "_cldata_t"
                    val freevars = [for (n, t) <- kcv_freevars {
                                       (n, walk_ktyp_n_mangle(t, kcv_loc, callb))
                                    }]
                    *kcv = kcv->{kcv_cname=cv_cname,
                        kcv_freevars=if final_mode {freevars} else {kcv_freevars}}
                | _ => throw compile_err(kf_loc,
                        "mangle: invalid closure datatype (should be KClosureVars)")
                }
            }
            *kf = kf->{ kf_cname=cname, kf_rt=if final_mode {rt} else {kf_rt}, kf_body=new_body,
                        kf_closure=if final_mode {kf_closure.{kci_fp_typ=mangled_ktyp_id}} else {kf_closure} }
            e
        | KDefExn ke =>
            val {ke_name, ke_typ, ke_scope, ke_std, ke_tag, ke_loc} = *ke
            val t = mangle_ktyp_retain_record(ke_typ, ke_loc, callb)
            val suffix = mangle_ktyp(t, mangle_map, ke_loc)
            val bare_name = mangle_name(ke_name, Some(ke_scope), ke_loc)
            val (base_cname, cname) = mangle_make_unique(ke_name, "E", bare_name, suffix, mangle_map)
            val exn_cname = add_fx(cname)
            *ke = ke->{ke_cname=exn_cname, ke_typ=if final_mode {t} else {ke_typ}, ke_base_cname=base_cname}
            val tag_kv = get_kval(ke_tag, ke_loc)
            val tag_cname = if ke_std { "FX_EXN_" + pp(ke_name) }
                            else { "_FX_EXN_" + base_cname }
            val tag_kv = tag_kv.{kv_cname=tag_cname}
            set_idk_entry(ke_tag, KVal(tag_kv))
            e
        | KDefVariant kvar =>
            val {kvar_name, kvar_cases, kvar_loc} = *kvar
            val _ = mangle_ktyp(KTypName(kvar_name), mangle_map, kvar_loc)
            //val tag_base_name = "_FX_" + pp(kvar->kvar_basename) + "_"
            val var_cases =
            [for (ni, ti) <- kvar_cases {
                //val tag_name = tag_base_name + pp(ni)
                //val kv = get_kval(ni, kvar_loc)
                //set_idk_entry(ni, KVal(kv.{kv_cname=tag_name}))
                val ti =
                match deref_ktyp(ti, kvar_loc) {
                | KTypRecord (r_id, relems) =>
                    if r_id == noid {
                        match relems {
                        | (n, t) :: [] => t
                        | _ => KTypTuple([for (_, tj) <- relems { tj } ])
                        }
                    } else {
                        KTypName(r_id)
                    }
                | _ => ti
                }
                (ni, mangle_ktyp_retain_record(ti, kvar_loc, callb))
            }]
            if final_mode {
                *kvar = kvar->{kvar_cases=var_cases}
            }
            e
        | KDefInterface ki =>
            val {ki_name, ki_all_methods, ki_loc} = *ki
            val _ = mangle_ktyp(KTypName(ki_name), mangle_map, ki_loc)
            val all_methods =
            [for (fi, ti) <- ki_all_methods {
                val (args, rt) = match deref_ktyp(ti, ki_loc) {
                    | KTypFun(args, rt) => (args, rt)
                    | _ => throw compile_err(ki_loc,
                        f"method '{idk2str(ki_name, ki_loc)}.{pp(fi)}' has non-function type")
                    }
                val args = [for a <- args {
                        walk_ktyp_n_mangle(a, ki_loc, callb)
                    }]
                val rt = walk_ktyp_n_mangle(rt, ki_loc, callb)
                (fi, KTypFun(args, rt))
            }]
            if final_mode {
                *ki = ki->{ki_all_methods=all_methods}
            }
            e
        | KDefTyp((ref {kt_typ=KTypRecord (_, _)}) as kt) =>
            val {kt_name, kt_typ, kt_loc} = *kt
            // compute and set kt_cname
            val _ = mangle_ktyp(KTypName(kt_name), mangle_map, kt_loc)
            val ktyp =
                match mangle_ktyp_retain_record(kt_typ, kt_loc, callb) {
                | KTypRecord (_, _) as ktyp => ktyp
                | _ => throw compile_err(kt_loc, "after mangling record is not a record anymore")
                }
            if final_mode {
                *kt = kt->{kt_typ=ktyp}
            }
            e
        /* Normally, KMangle does not take KDefTyp with non-record rhs on input,
           it produces such types on output, but just in case leave such definitions
           as-is */
        | KDefTyp _ => e
        | KDefClosureVars kcv => e
        | KExpFor (idoml, at_ids, body, flags, loc) =>
            mangle_idoml(idoml, at_ids, loc, callb)
            walk_kexp(e, callb)
        | KExpMap (e_idoml_l, body, flags, (_, loc)) =>
            for (_, idoml, idx) <- e_idoml_l { mangle_idoml(idoml, idx, loc, callb) }
            walk_kexp(e, callb)
        | _ => walk_kexp(e, callb)
        }

    val walk_n_mangle_callb = k_callb_t
    {
        kcb_ktyp=Some(walk_ktyp_n_mangle),
        kcb_kexp=Some(walk_kexp_n_mangle),
        kcb_atom=None
    }

    [for km <- kmods {
        val {km_idx, km_top} = km
        mangle_map.clear()
        curr_top_code = []
        curr_km_idx = km_idx
        for e <- km_top {
            val e = walk_kexp_n_mangle(e, walk_n_mangle_callb)
            if final_mode {
                match e {
                | KDefVal (n, e, loc) =>
                    val kv = get_kval(n, loc)
                    val {kv_cname, kv_flags} = kv
                    if kv_cname == "" {
                        if !(kv_flags.val_flag_temp || kv_flags.val_flag_tempref) {
                            set_idk_entry(n, KVal(kv.{
                                kv_flags=kv_flags.{
                                    val_flag_global=ScModule(km_idx) :: []}}))
                            ignore(mangle_id_typ(n, loc, walk_n_mangle_callb))
                        }
                    }
                | _ => {}
                }
            }
            curr_top_code = e :: curr_top_code
        }
        km.{km_top=curr_top_code.rev()}
    }]
}

fun demangle_all(kmods: kmodule_t list)
{
    fun reset_kval_cname(i: id_t, loc: loc_t) {
        val kv = get_kval(i, loc)
        set_idk_entry(i, KVal(kv.{kv_cname=""}))
    }
    fun fold_ktyp_n_demangle(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun fold_kexp_n_demangle(e: kexp_t, callb: k_fold_callb_t) =
        match e {
        | KDefVal (n, e, loc) =>
            reset_kval_cname(n, loc)
            fold_kexp_n_demangle(e, callb)
        | KDefFun kf =>
            val {kf_params, kf_body, kf_loc} = *kf
            for x <- kf_params { reset_kval_cname(x, kf_loc) }
            fold_kexp_n_demangle(kf_body, callb)
            *kf = kf->{kf_cname=""}
        | KDefExn ke =>
            val {ke_loc, ke_tag} = *ke
            *ke = ke->{ke_cname="", ke_base_cname=""}
            val tag_kv = get_kval(ke_tag, ke_loc)
            set_idk_entry(ke_tag, KVal(tag_kv.{kv_cname=""}))
        | KDefVariant kvar =>
            val {kvar_proto, kvar_loc} = *kvar
            match (if kvar_proto != noid {kinfo_(kvar_proto, kvar_loc)} else {KNone}) {
            | KVariant kvar2 => *kvar2 = kvar2->{kvar_cname=""}
            | _ => {}
            }
            *kvar = kvar->{kvar_cname=""}
        | KDefInterface ki =>
            *ki = ki->{ki_cname=""}
        | KDefTyp kt =>
            val {kt_proto, kt_loc} = *kt
            match (if kt_proto != noid {kinfo_(kt_proto, kt_loc)} else {KNone}) {
            | KTyp kt2 => *kt2 = kt2->{kt_cname=""}
            | _ => {}
            }
            *kt = kt->{kt_cname=""}
        | KExpMap(clauses, _, _, (_, loc)) =>
            fold_kexp(e, callb)
            for (_, id_l, at_ids) <- clauses {
                for i <- at_ids { reset_kval_cname(i, loc) }
                for (i, _) <- id_l { reset_kval_cname(i, loc) }
            }
        | KExpFor(id_l, at_ids, _, _, loc) =>
            fold_kexp(e, callb)
            for i <- at_ids { reset_kval_cname(i, loc) }
            for (i, _) <- id_l { reset_kval_cname(i, loc) }
        | _ => fold_kexp(e, callb)
        }

    val fold_n_demangle_callb = k_fold_callb_t
    {
        kcb_fold_ktyp=Some(fold_ktyp_n_demangle),
        kcb_fold_kexp=Some(fold_kexp_n_demangle),
        kcb_fold_atom=None
    }

    for km <- kmods {
        val {km_top} = km
        for e <- km_top {
            fold_kexp_n_demangle(e, fold_n_demangle_callb)
        }
    }
    kmods
}

fun empty_int_map(size0: int) = Hashmap.empty(size0, 0, -1)

fun mangle_locals(kmods: kmodule_t list)
{
    var global_prefix_hash = empty_int_map(256)
    var prefix_hash = empty_int_map(256)

    fun gen_cname(n: id_t, global: bool) {
        val prefix = if global {get_id("g_" + all_names.data[n.i]).i} else {n.i}
        val idx = prefix_hash.find_idx_or_insert(prefix)
        val j1 = prefix_hash.table[idx].data + 1
        prefix_hash.table[idx].data = j1
        f"{all_names.data[prefix]}_{j1}"
    }

    fun gen_kval_cname(n: id_t, loc: loc_t, global: bool) =
        if n.m > 0 {
            val {kv_cname} as kv = get_kval(n, loc)
            if kv_cname == "" {
                val new_cname = gen_cname(n, global)
                set_idk_entry(n, KVal(kv.{kv_cname=new_cname}))
            }
        }

    fun mangle_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t): void {}
    fun mangle_kexp_(e: kexp_t, callb: k_fold_callb_t) =
        match e {
        | KDefVal(n, e, loc) =>
            mangle_kexp_(e, callb)
            gen_kval_cname(n, loc, false)
        | KDefFun kf =>
            val {kf_params, kf_body, kf_loc} = *kf
            val saved_hash = prefix_hash
            prefix_hash = global_prefix_hash.copy()
            for x <- kf_params { gen_kval_cname(x, kf_loc, false) }
            mangle_kexp_(kf_body, callb)
            prefix_hash = saved_hash
        | KExpMap(clauses, _, _, (_, loc)) =>
            fold_kexp(e, callb)
            for (_, id_l, at_ids) <- clauses {
                for i <- at_ids { gen_kval_cname(i, loc, false) }
                for (i, _) <- id_l { gen_kval_cname(i, loc, false) }
            }
        | KExpFor(id_l, at_ids, _, _, loc) =>
            fold_kexp(e, callb)
            for i <- at_ids { gen_kval_cname(i, loc, false) }
            for (i, _) <- id_l { gen_kval_cname(i, loc, false) }
        | _ => fold_kexp(e, callb)
        }

    val mangle_callb = k_fold_callb_t {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(mangle_ktyp_),
        kcb_fold_kexp=Some(mangle_kexp_)
    }

    // pass 1. set cname for global values first
    for km <- kmods {
        val {km_top} = km
        for e <- km_top {
            | KDefVal(n, _, loc) when n.m > 0 => gen_kval_cname(n, loc, true)
            | _ => {}
        }
    }
    global_prefix_hash = prefix_hash

    // pass 2. mangle local variables in each function in each module.
    // Use the global prefix hash as a starting point for each function.
    // This way we make sure that local values do not interfere with global values
    for km <- kmods {
        val {km_top} = km
        prefix_hash = global_prefix_hash.copy()
        for e <- km_top {
            mangle_kexp_(e, mangle_callb)
        }
    }
    kmods
}
