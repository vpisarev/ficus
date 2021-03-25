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
import Map

type mangle_map_t = (string, id_t) Map.t
val empty_mangle_map : mangle_map_t = Map.empty(String.cmp)

fun mangle_mname(m: string): string = m.replace(".", "__")

fun mangle_scope(sc: scope_t list, result: string, loc: loc_t) =
    match sc {
    | ScModule m :: rest =>
        val mstr = mangle_mname(pp(m))
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
    suffix: string, mangle_map: mangle_map_t ref): (string, string)
{
    fun make_unique_(idx: int) {
        val idxstr = if idx == 0 { "" } else { string(idx) + "_" }
        val name1 = name + idxstr
        val nlen = name1.length()
        val candidate_base_name = prefix + string(nlen) + name1
        val candidate = candidate_base_name + suffix
        if mangle_map->mem(candidate) {
            make_unique_(idx + 1)
        } else {
            *mangle_map = mangle_map->add(candidate, n_id)
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
fun mangle_ktyp(t: ktyp_t, mangle_map: mangle_map_t ref, loc: loc_t): string
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
            val {kvar_name, kvar_cname, kvar_targs, kvar_scope} = *kvar
            if kvar_cname == "" {
                val (base_name, cname) =
                    mangle_inst_(kvar_name, "V", kvar_targs, kvar_name, kvar_scope)
                *kvar = kvar->{kvar_cname=add_fx(cname), kvar_base_name=get_id(base_name)}
                cname :: result
            } else {
                remove_fx(kvar_cname) :: result
            }
        | KTyp((ref {kt_typ=KTypRecord (_, _)}) as kt) =>
            val {kt_name, kt_cname, kt_targs, kt_scope} = *kt
            if kt_cname == "" {
                val (_, cname) = mangle_inst_(kt_name, "R", kt_targs, kt_name, kt_scope)
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
        | KTypRef t => mangle_ktyp_(t, "r" :: result)
        | KTypExn  => "E" :: result
        | KTypErr  => throw compile_err(loc, "KTypErr cannot be mangled")
        | KTypModule  => throw compile_err(loc, "KTypModule cannot be mangled")
        }
    "".join(mangle_ktyp_(t, []).rev())
}

fun mangle_all(kmods: kmodule_t list) {
    val mangle_map = ref empty_mangle_map
    var curr_top_code: kcode_t = []

    fun create_gen_typ(t: ktyp_t, name_prefix: string, loc: loc_t)
    {
        val cname = mangle_ktyp(t, mangle_map, loc)
        match mangle_map->find_opt(cname) {
        | Some(i) => KTypName(i)
        | _ =>
            val i = gen_temp_idk(name_prefix)
            val kt = ref (kdeftyp_t {
                kt_name=i, kt_cname=add_fx(cname),
                kt_targs=[], kt_typ=t, kt_props=None,
                kt_scope=[], kt_loc=loc})
            *mangle_map = mangle_map->add(cname, i)
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
        | KTypFun _ => create_gen_typ(t, "fun", loc)
        | KTypTuple _ => create_gen_typ(t, "tup", loc)
        | KTypArray _ => t
        | KTypList _ => create_gen_typ(t, "lst", loc)
        | KTypRef _ => create_gen_typ(t, "ref", loc)
        }
    }
    fun mangle_id_typ(i: id_t, loc: loc_t, callb: k_callb_t) =
        if i != noid {
            match kinfo_(i, loc) {
            | KVal kv =>
                val {kv_typ, kv_flags} = kv
                val t = walk_ktyp_n_mangle(kv_typ, loc, callb)
                val cname =
                match get_val_scope(kv_flags) {
                | ScBlock _ :: _ => ""
                | sc =>
                    val bare_name = mangle_name(i, Some(sc), loc)
                    val (_, cname) = mangle_make_unique(i, "_fx_g", bare_name, "", mangle_map)
                    cname
                }
                set_idk_entry(i, KVal(kv.{kv_typ=t, kv_cname=cname}))
            | _ => {}
            }
        }
    fun mangle_ktyp_retain_record(t: ktyp_t, loc: loc_t, callb: k_callb_t) =
        match t {
        | KTypRecord (rn, relems) =>
            KTypRecord(rn, [: for (ni, ti) <- relems {
                                (ni, walk_ktyp_n_mangle(ti, loc, callb))
                            } :])
        | t => walk_ktyp_n_mangle(t, loc, callb)
        }
    fun mangle_idoml(idoml: (id_t, dom_t) list, at_ids: id_t list,
                             loc: loc_t, callb: k_callb_t)
    {
        for i <- at_ids { mangle_id_typ(i, loc, callb) }
        for (k, _) <- idoml { mangle_id_typ(k, loc, callb) }
    }
    fun walk_kexp_n_mangle(e: kexp_t, callb: k_callb_t) =
        match e {
        | KDefVal (n, e, loc) => val e = walk_kexp_n_mangle(e, callb)
                                 mangle_id_typ(n, loc, callb)
                                 KDefVal(n, e, loc)
        | KDefFun kf =>
            val {kf_name, kf_args, kf_rt, kf_body, kf_closure, kf_scope, kf_loc} = *kf
            val {kci_fcv_t} = kf_closure
            val args = [: for (a, t) <- kf_args {
                           mangle_id_typ(a, kf_loc, callb)
                           (a, get_idk_ktyp(a, kf_loc))
                        } :]
            val rt = walk_ktyp_n_mangle(kf_rt, kf_loc, callb)
            val ktyp = get_kf_typ(args, rt)
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
            | _ => throw compile_err(kf_loc,
                    f"mangle: cannot mangle '{cname}' type down to alias")
            }
            if kci_fcv_t != noid {
                match kinfo_(kci_fcv_t, kf_loc) {
                | KClosureVars kcv =>
                    val {kcv_freevars, kcv_loc} = *kcv
                    val cv_cname = cname + "_cldata_t"
                    val freevars = [: for (n, t) <- kcv_freevars {
                                       (n, walk_ktyp_n_mangle(t, kcv_loc, callb))
                                    } :]
                    *kcv = kcv->{kcv_cname=cv_cname, kcv_freevars=freevars}
                | _ => throw compile_err(kf_loc,
                        "mangle: invalid closure datatype (should be KClosureVars)")
                }
            }
            *kf = kf->{ kf_cname=cname, kf_args=args, kf_rt=rt, kf_body=new_body,
                        kf_closure=kf_closure.{kci_fp_typ=mangled_ktyp_id} }
            e
        | KDefExn ke =>
            val {ke_name, ke_typ, ke_scope, ke_std, ke_tag, ke_loc} = *ke
            val t = mangle_ktyp_retain_record(ke_typ, ke_loc, callb)
            val suffix = mangle_ktyp(t, mangle_map, ke_loc)
            val bare_name = mangle_name(ke_name, Some(ke_scope), ke_loc)
            val (base_cname, cname) = mangle_make_unique(ke_name, "E", bare_name, suffix, mangle_map)
            val exn_cname = add_fx(cname)
            *ke = ke->{ke_cname=exn_cname, ke_typ=t, ke_base_cname=base_cname}
            val tag_kv = get_kval(ke_tag, ke_loc)
            val tag_cname = if ke_std { "FX_EXN_" + pp(ke_name) }
                            else { "_FX_EXN_" + base_cname }
            val tag_kv = tag_kv.{kv_cname=tag_cname}
            set_idk_entry(ke_tag, KVal(tag_kv))
            e
        | KDefVariant kvar =>
            val {kvar_name, kvar_cases, kvar_loc} = *kvar
            val _ = mangle_ktyp(KTypName(kvar_name), mangle_map, kvar_loc)
            val tag_base_name = "_FX_" + pp(kvar->kvar_base_name) + "_"
            val var_cases =
            [: for (ni, ti) <- kvar_cases {
                val tag_name = tag_base_name + pp(ni)
                val kv = get_kval(ni, kvar_loc)
                set_idk_entry(ni, KVal(kv.{kv_cname=tag_name}))
                val ti =
                match deref_ktyp(ti, kvar_loc) {
                | KTypRecord (r_id, relems) =>
                    if r_id == noid {
                        match relems {
                        | (n, t) :: [] => t
                        | _ => KTypTuple([: for (_, tj) <- relems { tj } :])
                        }
                    } else {
                        KTypName(r_id)
                    }
                | _ => ti
                }
                (ni, mangle_ktyp_retain_record(ti, kvar_loc, callb))
            } :]
            *kvar = kvar->{kvar_cases=var_cases}
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
            *kt = kt->{kt_typ=ktyp}
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

    [: for km <- kmods {
        val {km_name, km_top} = km
        curr_top_code = []
        for e <- km_top {
            val e = walk_kexp_n_mangle(e, walk_n_mangle_callb)
            match e {
            | KDefVal (n, e, loc) =>
                val kv = get_kval(n, loc)
                val {kv_cname, kv_flags} = kv
                if kv_cname == "" {
                    if !(kv_flags.val_flag_temp || kv_flags.val_flag_tempref) {
                        set_idk_entry(n, KVal(kv.{
                            kv_flags=kv_flags.{
                                val_flag_global=ScModule(km_name) :: []}}))
                        mangle_id_typ(n, loc, walk_n_mangle_callb)
                    }
                }
            | _ => {}
            }
            curr_top_code = e :: curr_top_code
        }
        km.{km_top=curr_top_code.rev()}
    } :]
}
