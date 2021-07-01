/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    K-normal form (or K-form in short) definition.
    This is a greatly extended variation of K-normal form in min-caml:
    https://github.com/esumii/min-caml.

    Similarly to ficus AST, which is defined in Ast.fx,
    K-form is an hierarchical (tree-like) representation of
    the compiled code. However, it's much simpler and more
    suitable for intermediate optimizations and then for
    translation to some even lower-level representation, e.g. C code.

    In particular:

    * all the symbols in K-form are resolved and unique, e.g:
        type i = int
        fun foo(i: i) { val i = i+1; val i = i*2; for i<-0:i {println(i)} }
      is transformed to something like
        fun foo@999(i@1000: int): void {
          val i@1001: int = i@1000+1; val i@1002: int = i@1001*2
          for (i@1003:int) <- 0:i@1002 {println@56<int->void>(i@1003)}
        }
    * all the symbols have known type. If it cannot be figured out,
      type checker or the k-form generator (see KNormalize.fx) report compile error.
    * at once, all the types (typ_t) are converted to k-types (ktyp_t), i.e.
      all indirections are eliminated, instances of generic types
      (TypApp(<args...>, <some_generic_type_id>)) are replaced with concrete instances
      (KTypName(<instance_type_id>)) or even actual types where applicable
      (like the type alias 'i' above was replaced with it's actual meaning, i.e. 'int').
    * all complex expressions are broken down into sequences of basic operations
      with intermediate results stored in temporary values.
    * pattern matching is converted into sequences of nested if-expressions
    * import directives are removed; we've already resolved all the symbols
    * generic types and functions are removed. Their instances, generated
      by type checker, are retained though.
    * ...
*/
from Ast import *
import Dynvec, Set, Hashset

type ktprops_t =
{
    ktp_complex: bool;
    ktp_scalar: bool;
    ktp_ptr: bool;
    ktp_pass_by_ref: bool;
    ktp_custom_free: bool;
    ktp_custom_copy: bool
}

type ktyp_t =
    | KTypInt
    | KTypCInt
    | KTypSInt: int
    | KTypUInt: int
    | KTypFloat: int
    | KTypVoid
    | KTypBool
    | KTypChar
    | KTypString
    | KTypCPointer
    | KTypFun: (ktyp_t list, ktyp_t)
    | KTypTuple: ktyp_t list
    | KTypRecord: (id_t, (id_t, ktyp_t) list)
    | KTypName: id_t
    | KTypArray: (int, ktyp_t)
    | KTypVector: ktyp_t
    | KTypList: ktyp_t
    | KTypRef: ktyp_t
    | KTypExn
    | KTypErr
    | KTypModule

type klit_t =
    | KLitInt: int64
    | KLitSInt: (int, int64)
    | KLitUInt: (int, uint64)
    | KLitFloat: (int, double)
    | KLitString: string
    | KLitChar: char
    | KLitBool: bool
    | KLitNil: ktyp_t

type atom_t =
    | AtomId: id_t
    | AtomLit: klit_t

type dom_t =
    | DomainElem: atom_t
    | DomainFast: atom_t
    | DomainRange: (atom_t, atom_t, atom_t)

type kctx_t = (ktyp_t, loc_t)

type kexp_t =
    | KExpNop: loc_t
    | KExpBreak: loc_t
    | KExpContinue: loc_t
    | KExpReturn: (atom_t?, loc_t)
    | KExpAtom: (atom_t, kctx_t)
    | KExpBinary: (binary_t, atom_t, atom_t, kctx_t)
    | KExpUnary: (unary_t, atom_t, kctx_t)
    | KExpIntrin: (intrin_t, atom_t list, kctx_t)
    | KExpSync: (id_t, kexp_t)
    | KExpSeq: (kexp_t list, kctx_t)
    | KExpIf: (kexp_t, kexp_t, kexp_t, kctx_t)
    | KExpCall: (id_t, atom_t list, kctx_t)
    | KExpICall: (id_t, int, atom_t list, kctx_t)
    | KExpMkTuple: (atom_t list, kctx_t)
    | KExpMkRecord: (atom_t list, kctx_t)
    | KExpMkClosure: (id_t, id_t, atom_t list, kctx_t)
    | KExpMkArray: (bool, (bool, atom_t) list list, kctx_t)
    | KExpMkVector: ((bool, atom_t) list, kctx_t)
    | KExpAt: (atom_t, border_t, interpolate_t, dom_t list, kctx_t)
    | KExpMem: (id_t, int, kctx_t)
    | KExpAssign: (id_t, atom_t, loc_t)
    | KExpMatch: ((kexp_t list, kexp_t) list, kctx_t)
    | KExpTryCatch: (kexp_t, kexp_t, kctx_t)
    | KExpThrow: (id_t, bool, loc_t)
    | KExpCast: (atom_t, ktyp_t, loc_t)
    | KExpMap: ((kexp_t, (id_t, dom_t) list, id_t list) list, kexp_t, for_flags_t, kctx_t)
    | KExpFor: ((id_t, dom_t) list, id_t list, kexp_t, for_flags_t, loc_t)
    | KExpWhile: (kexp_t, kexp_t, loc_t)
    | KExpDoWhile: (kexp_t, kexp_t, loc_t)
    | KExpCCode: (string, kctx_t)
    | KDefVal: (id_t, kexp_t, loc_t)
    | KDefFun: kdeffun_t ref
    | KDefExn: kdefexn_t ref
    | KDefVariant: kdefvariant_t ref
    | KDefInterface: kdefinterface_t ref
    | KDefTyp: kdeftyp_t ref
    | KDefClosureVars: kdefclosurevars_t ref

type kcode_t = kexp_t list

type kdefval_t =
{
    kv_name: id_t;
    kv_cname: string;
    kv_typ: ktyp_t;
    kv_flags: val_flags_t;
    kv_loc: loc_t
}

type kdefclosureinfo_t =
{
    kci_arg: id_t;
    kci_fcv_t: id_t;
    kci_fp_typ: id_t;
    kci_make_fp: id_t;
    kci_make_fp_sr: id_t;
    kci_wrap_f: id_t
}

type kdeffun_t =
{
    kf_name: id_t;
    kf_cname: string;
    kf_params: id_t list;
    kf_rt: ktyp_t;
    kf_body: kexp_t;
    kf_flags: fun_flags_t;
    kf_closure: kdefclosureinfo_t;
    kf_scope: scope_t list;
    kf_loc: loc_t
}

type kdefexn_t =
{
    ke_name: id_t;
    ke_cname: string;
    ke_base_cname: string;
    ke_typ: ktyp_t;
    ke_std: bool;
    ke_tag: id_t;
    ke_make: id_t;
    ke_scope: scope_t list;
    ke_loc: loc_t
}

type kdefvariant_t =
{
    kvar_name: id_t;
    kvar_cname: string;
    kvar_proto: id_t;
    kvar_props: ktprops_t?;
    kvar_targs: ktyp_t list;
    kvar_cases: (id_t, ktyp_t) list;
    kvar_ctors: id_t list;
    kvar_flags: var_flags_t;
    kvar_ifaces: (id_t, id_t list) list;
    kvar_scope: scope_t list;
    kvar_loc: loc_t
}

type kdefinterface_t =
{
    ki_name: id_t;
    ki_base: id_t;
    ki_cname: string;
    ki_id: id_t;
    ki_all_methods: (id_t, ktyp_t) list;
    ki_scope: scope_t list;
    ki_loc: loc_t
}

type kdeftyp_t =
{
    kt_name: id_t;
    kt_cname: string;
    kt_proto: id_t;
    kt_props: ktprops_t?;
    kt_targs: ktyp_t list;
    kt_typ: ktyp_t;
    kt_scope: scope_t list;
    kt_loc: loc_t
}

type kdefclosurevars_t =
{
    kcv_name: id_t;
    kcv_cname: string;
    kcv_freevars: (id_t, ktyp_t) list;
    kcv_orig_freevars: id_t list;
    kcv_scope: scope_t list;
    kcv_loc: loc_t
}

type kmodule_t =
{
    km_name: id_t;
    km_idx: int;
    km_toposort_idx: int;
    km_cname: string;
    km_top: kexp_t list;
    km_deps: int list;
    km_skip: bool;
    km_main: bool;
    km_pragmas: pragmas_t
}

type kinfo_t =
    | KNone
    | KVal: kdefval_t
    | KFun: kdeffun_t ref
    | KExn: kdefexn_t ref
    | KVariant: kdefvariant_t ref
    | KClosureVars: kdefclosurevars_t ref
    | KInterface: kdefinterface_t ref
    | KTyp: kdeftyp_t ref

val _KLitVoid = KLitNil(KTypVoid)
val _ALitVoid = AtomLit(_KLitVoid)
var all_idks: kinfo_t Dynvec.t [] = []
var builtin_exn_NoMatchError = noid
var builtin_exn_OutOfRangeError = noid
var freeze_idks = false

fun new_idk_idx(m_idx: int): int {
    if freeze_idks {
        throw Fail("internal error: new idk is requested when they are frozen")
    }
    val new_idx = all_modules[m_idx].dm_table.push()
    val new_kidx = all_idks[m_idx].push()
    if new_idx != new_kidx {
        throw Fail("internal error: unsynchronized outputs from new_id_idx() and new_idk_idx()")
    }
    new_kidx
}

fun kinfo_(n: id_t, loc: loc_t) =
    if n.m == 0 {KNone}
    else {
        val (m, j) = id2idx_(n, loc)
        all_idks[m].data[j]
    }

fun dup_idk(m_idx: int, old_id: id_t): id_t
{
    val j = new_idk_idx(m_idx)
    id_t {m=m_idx, i=if old_id.i != 0 {old_id.i} else {1}, j=j}
}

fun gen_idk(m_idx: int, s: string): id_t
{
    val i = get_id_prefix(s)
    val j = new_idk_idx(m_idx)
    id_t {m=m_idx, i=i, j=j}
}

fun set_idk_entry(n: id_t, info: kinfo_t): void
{
    //val loc = get_kinfo_loc(info)
    val (m, j) = id2idx_(n, noloc)
    all_idks[m].data[j] = info
}

fun init_all_idks(): void
{
    freeze_ids = true
    freeze_idks = false
    all_idks = [| for dm <- all_modules {
        val sz = dm.dm_table.size()
        Dynvec.create(sz, KNone)
    } |]
}

fun get_kexp_ctx(e: kexp_t): kctx_t
{
    | KExpNop(l) => (KTypVoid, l)
    | KExpBreak(l) => (KTypVoid, l)
    | KExpContinue(l) => (KTypVoid, l)
    | KExpReturn(_, l) => (KTypVoid, l)
    | KExpAtom(_, c) => c
    | KExpBinary(_, _, _, c) => c
    | KExpUnary(_, _, c) => c
    | KExpIntrin(_, _, c) => c
    | KExpSeq(_, c) => c
    | KExpSync(_, e) => get_kexp_ctx(e)
    | KExpIf(_, _, _, c) => c
    | KExpCall(_, _, c) => c
    | KExpICall(_, _, _, c) => c
    | KExpMkTuple(_, c) => c
    | KExpMkRecord(_, c) => c
    | KExpMkClosure(_, _, _, c) => c
    | KExpMkArray(_, _, c) => c
    | KExpMkVector(_, c) => c
    | KExpAt(_, _, _, _, c) => c
    | KExpMem(_, _, c) => c
    | KExpAssign(_, _, l) => (KTypVoid, l)
    | KExpMatch(_, c) => c
    | KExpTryCatch(_, _, c) => c
    | KExpThrow(_, _, l) => (KTypErr, l)
    | KExpCast(_, t, l) => (t, l)
    | KExpMap(_, _, _, c) => c
    | KExpFor(_, _, _, _, l) => (KTypVoid, l)
    | KExpWhile(_, _, l) => (KTypVoid, l)
    | KExpDoWhile(_, _, l) => (KTypVoid, l)
    | KExpCCode(_, c) => c
    | KDefVal(_, _, l) => (KTypVoid, l)
    | KDefFun (ref {kf_loc}) => (KTypVoid, kf_loc)
    | KDefExn (ref {ke_loc}) => (KTypVoid, ke_loc)
    | KDefVariant (ref {kvar_loc}) => (KTypVoid, kvar_loc)
    | KDefTyp (ref {kt_loc}) => (KTypVoid, kt_loc)
    | KDefInterface (ref {ki_loc}) => (KTypVoid, ki_loc)
    | KDefClosureVars (ref {kcv_loc}) => (KTypVoid, kcv_loc)
}

fun get_kinfo_loc(info: kinfo_t): loc_t
{
    | KNone => noloc
    | KVal ({kv_loc}) => kv_loc
    | KFun (ref {kf_loc}) => kf_loc
    | KExn (ref {ke_loc}) => ke_loc
    | KVariant (ref {kvar_loc}) => kvar_loc
    | KTyp (ref {kt_loc}) => kt_loc
    | KInterface (ref {ki_loc}) => ki_loc
    | KClosureVars (ref {kcv_loc}) => kcv_loc
}

fun get_kexp_typ(e: kexp_t): ktyp_t = get_kexp_ctx(e).0
fun get_kexp_loc(e: kexp_t): loc_t = get_kexp_ctx(e).1

fun get_kexp_start(e: kexp_t): loc_t = get_start_loc(get_kexp_loc(e))
fun get_kexp_end(e: kexp_t): loc_t = get_end_loc(get_kexp_loc(e))

fun is_val_global(flags: val_flags_t): bool = flags.val_flag_global != []

fun get_val_scope(flags: val_flags_t): scope_t list =
    match flags.val_flag_global {
    | [] => ScBlock(0) :: []
    | sc => sc
    }

fun get_kscope(info: kinfo_t): scope_t list
{
    | KNone => []
    | KVal ({kv_flags}) => get_val_scope(kv_flags)
    | KFun (ref {kf_scope}) => kf_scope
    | KExn (ref {ke_scope}) => ke_scope
    | KVariant (ref {kvar_scope}) => kvar_scope
    | KInterface (ref {ki_scope}) => ki_scope
    | KClosureVars (ref {kcv_scope}) => kcv_scope
    | KTyp (ref {kt_scope}) => kt_scope
}

fun get_idk_scope(n: id_t, loc: loc_t): scope_t list = get_kscope(kinfo_(n, loc))

fun get_idk_loc(n: id_t, loc: loc_t): loc_t = get_kinfo_loc(kinfo_(n, loc))

fun check_kinfo(info: kinfo_t, n: id_t, loc: loc_t) =
    match info {
    | KNone => throw compile_err(loc, f"attempt to request information about uninitialized symbol '{n}'")
    | _ => {}
    }

fun get_kinfo_cname(info: kinfo_t, loc: loc_t): string =
    match info {
    | KNone => throw compile_err(loc, "attempt to request cname of uninitialized symbol")
    | KVal ({kv_cname}) => kv_cname
    | KFun (ref {kf_cname}) => kf_cname
    | KExn (ref {ke_cname}) => ke_cname
    | KVariant (ref {kvar_cname}) => kvar_cname
    | KInterface (ref {ki_cname}) => ki_cname
    | KClosureVars (ref {kcv_cname}) => kcv_cname
    | KTyp (ref {kt_cname}) => kt_cname
    }

fun get_idk_cname(n: id_t, loc: loc_t): string
{
    val info = kinfo_(n, loc)
    check_kinfo(info, n, loc)
    get_kinfo_cname(info, loc)
}

fun idk2str(n: id_t, loc: loc_t) =
    if n.m == 0 { string(n) }
    else {
        val cname = get_idk_cname(n, loc)
        if cname == "" {
            val sc = get_idk_scope(n, loc)
            get_qualified_name(string(n), sc)
        } else { cname }
    }

fun get_kf_typ(kf_params: id_t list, kf_rt: ktyp_t, loc: loc_t): ktyp_t =
    KTypFun([for a <- kf_params { get_kval(a, loc).kv_typ } ], kf_rt)

fun get_kinfo_typ(info: kinfo_t, n: id_t, loc: loc_t): ktyp_t
{
    check_kinfo(info, n, loc)
    match info {
    | KNone => KTypVoid
    | KVal ({kv_typ}) => kv_typ
    | KFun (ref {kf_params, kf_rt, kf_loc}) => get_kf_typ(kf_params, kf_rt, kf_loc)
    | KExn _ => KTypExn
    | KVariant (ref {kvar_name}) => KTypName(kvar_name)
    | KInterface (ref {ki_name}) => KTypName(ki_name)
    | KClosureVars (ref {kcv_name, kcv_freevars}) => KTypRecord(kcv_name, kcv_freevars)
    | KTyp (ref {kt_typ=KTypRecord(_, _) as kt_typ}) => kt_typ
    | KTyp (ref {kt_name}) => KTypName(kt_name)
    }
}

fun get_idk_ktyp(n: id_t, loc: loc_t): ktyp_t = get_kinfo_typ(kinfo_(n, loc), n, loc)

fun get_lit_ktyp(l: klit_t): ktyp_t
{
    | KLitInt _ => KTypInt
    | KLitSInt(b, _) => KTypSInt(b)
    | KLitUInt(b, _) => KTypUInt(b)
    | KLitFloat(b, _) => KTypFloat(b)
    | KLitString _ => KTypString
    | KLitChar _ => KTypChar
    | KLitBool _ => KTypBool
    | KLitNil(t) => t
}

fun get_atom_ktyp(a: atom_t, loc: loc_t): ktyp_t =
    match a {
    | AtomId(n) => get_idk_ktyp(n, loc)
    | AtomLit(l) => get_lit_ktyp(l)
    }

fun get_code_loc(code: kcode_t, default_loc: loc_t) =
    loclist2loc(code.map(get_kexp_loc), default_loc)

fun filter_out_nops(code: kcode_t): kexp_t list =
    code.filter(fun (e) { | KExpNop _ => false | _ => true })

fun code2kexp(code: kcode_t, loc: loc_t) =
    match filter_out_nops(code) {
    | [] => KExpNop(loc)
    | e :: [] => e
    | _ =>
        val t = get_kexp_typ(code.last())
        val final_loc = get_code_loc(code, loc)
        KExpSeq(code, (t, final_loc))
    }

fun rcode2kexp(code: kcode_t, loc: loc_t): kexp_t =
    match filter_out_nops(code) {
    | [] => KExpNop(loc)
    | e :: [] => e
    | e :: rest =>
        val t = get_kexp_typ(e)
        val final_loc = get_code_loc(code, loc)
        KExpSeq(code.rev(), (t, final_loc))
    }

fun kexp2code(e: kexp_t): kexp_t list
{
    | KExpNop _ => []
    | KExpSeq(elist, _) => elist
    | _ => e :: []
}

fun get_kval(n: id_t, loc: loc_t): kdefval_t
{
    val info = kinfo_(n, loc)
    check_kinfo(info, n, loc)
    match info {
    | KVal(kv) => kv
    | _ =>
        val loc = if loc != noloc { loc } else { get_kinfo_loc(info) }
        throw compile_err(loc, f"symbol '{n}' is expected to be KVal ...")
    }
}

fun get_kvariant(t: ktyp_t, loc: loc_t): kdefvariant_t ref
{
    val t = deref_ktyp(t, loc)
    match t {
    | KTypName(tn) =>
        match kinfo_(tn, loc) {
        | KVariant(kvar) => kvar
        | _ => throw compile_err(loc, f"type '{tn}' is expected to be a variant")
        }
    | _ => throw compile_err(loc, "variant (or sometimes an exception) is expected here")
    }
}

fun get_kinterface_opt(t: ktyp_t, loc: loc_t): kdefinterface_t ref?
{
    val t = deref_ktyp(t, loc)
    match t {
    | KTypName(tn) =>
        match kinfo_(tn, loc) {
        | KInterface(ki) => Some(ki)
        | _ => None
        }
    | _ => None
    }
}

/***************************************** walk through a K-normalized syntax tree and produce another tree *************************/

type k_callb_t =
{
    kcb_ktyp: ((ktyp_t, loc_t, k_callb_t) -> ktyp_t)?;
    kcb_kexp: ((kexp_t, k_callb_t) -> kexp_t)?;
    kcb_atom: ((atom_t, loc_t, k_callb_t) -> atom_t)?
}

fun check_n_walk_ktyp(t: ktyp_t, loc: loc_t, callb: k_callb_t): ktyp_t =
    match callb.kcb_ktyp {
    | Some(f) => f(t, loc, callb)
    | _ => walk_ktyp(t, loc, callb)
    }

fun check_n_walk_kexp(e: kexp_t, callb: k_callb_t): kexp_t =
    match callb.kcb_kexp {
    | Some(f) => f(e, callb)
    | _ => walk_kexp(e, callb)
    }

fun check_n_walk_atom(a: atom_t, loc: loc_t, callb: k_callb_t): atom_t =
    match callb.kcb_atom {
    | Some(f) => f(a, loc, callb)
    | _ =>
        match a {
        | AtomLit(KLitNil(t)) => AtomLit(KLitNil(check_n_walk_ktyp(t, loc, callb)))
        | _ => a
        }
    }

fun check_n_walk_al(al: atom_t list, loc: loc_t, callb: k_callb_t): atom_t list =
    [for a <- al {check_n_walk_atom(a, loc, callb)} ]

fun check_n_walk_dom(d: dom_t, loc: loc_t, callb: k_callb_t): dom_t =
    match d {
    | DomainElem(a) => DomainElem(check_n_walk_atom(a, loc, callb))
    | DomainFast(a) => DomainFast(check_n_walk_atom(a, loc, callb))
    | DomainRange(a, b, c) =>
        DomainRange(check_n_walk_atom(a, loc, callb),
                    check_n_walk_atom(b, loc, callb),
                    check_n_walk_atom(c, loc, callb))
    }

fun check_n_walk_id(n: id_t, loc: loc_t, callb: k_callb_t): id_t =
    match callb.kcb_atom {
    | Some(f) =>
        match f(AtomId(n), loc, callb) {
        | AtomId(n) => n
        | _ => throw compile_err(loc, "internal error: inside walk_id the callback returned a literal, not id, which is unexpected.")
        }
    | _ => n
    }

fun walk_ktyp(t: ktyp_t, loc: loc_t, callb: k_callb_t): ktyp_t
{
    fun walk_ktyp_(t: ktyp_t) = check_n_walk_ktyp(t, loc, callb)
    fun walk_ktl_(tl: ktyp_t list) = tl.map(walk_ktyp_)
    fun walk_id_(n: id_t) = check_n_walk_id(n, loc, callb)

    match t {
    | KTypInt | KTypCInt | KTypSInt _ | KTypUInt _
    | KTypFloat _ | KTypVoid | KTypBool | KTypChar
    | KTypString | KTypCPointer | KTypExn | KTypErr | KTypModule =>
        t
    | KTypFun(args, rt) => KTypFun(walk_ktl_(args), walk_ktyp_(rt))
    | KTypTuple(elems) => KTypTuple(walk_ktl_(elems))
    | KTypRecord(rn, relems) =>
        KTypRecord(walk_id_(rn),
            [for (ni, ti) <- relems { (walk_id_(ni), walk_ktyp_(ti)) } ])
    | KTypName(k) => KTypName(walk_id_(k))
    | KTypArray(d, t) => KTypArray(d, walk_ktyp_(t))
    | KTypVector(t) => KTypVector(walk_ktyp_(t))
    | KTypList(t) => KTypList(walk_ktyp_(t))
    | KTypRef(t) => KTypRef(walk_ktyp_(t))
    }
}

fun walk_kexp(e: kexp_t, callb: k_callb_t): kexp_t
{
    fun walk_atom_(a: atom_t, loc: loc_t): atom_t = check_n_walk_atom(a, loc, callb)
    fun walk_al_(al: atom_t list, loc: loc_t): atom_t list =
        [for a <- al {walk_atom_(a, loc)} ]
    fun walk_ktyp_(t: ktyp_t, loc: loc_t): ktyp_t = check_n_walk_ktyp(t, loc, callb)
    fun walk_id_(n: id_t, loc: loc_t): id_t = check_n_walk_id(n, loc, callb)
    fun walk_idlist_(nl: id_t list, loc: loc_t, update: bool): id_t list =
        [for n <- nl {
            if update {update_kval_(n, loc)} else {walk_id_(n, loc)}
        }]
    fun walk_kexp_(e: kexp_t): kexp_t = check_n_walk_kexp(e, callb)
    fun walk_kctx_((t: ktyp_t, loc: loc_t)): kctx_t = (walk_ktyp_(t, loc), loc)
    fun walk_dom_(d: dom_t, loc: loc_t): dom_t = check_n_walk_dom(d, loc, callb)
    fun walk_idomlist_(idoml: (id_t, dom_t) list, loc: loc_t): (id_t, dom_t) list =
        [for (n, d) <- idoml {
            val n = update_kval_(n, loc)
            (n, walk_dom_(d, loc))
        }]
    fun update_kval_(n: id_t, loc: loc_t) =
        if n.m > 0 && n.i > 0 {
            val kv = get_kval(n, loc)
            val {kv_name, kv_typ} = kv
            val new_kv_name = walk_id_(kv_name, loc)
            val new_kv_typ = walk_ktyp_(kv_typ, loc)
            set_idk_entry(new_kv_name, KVal(kv.{kv_name=new_kv_name, kv_typ=new_kv_typ}))
            new_kv_name
        } else {walk_id_(n, loc)}

    match e {
    | KExpNop _ => e
    | KExpBreak _ => e
    | KExpContinue _ => e
    | KExpReturn(a_opt, loc) =>
        KExpReturn(match a_opt {|Some(a) => Some(walk_atom_(a, loc)) | _ => None}, loc)
    | KExpAtom(a, (_, loc) as ctx) => KExpAtom(walk_atom_(a, loc), walk_kctx_(ctx))
    | KExpBinary(bop, a1, a2, (_, loc) as ctx) =>
        KExpBinary(bop, walk_atom_(a1, loc), walk_atom_(a2, loc), walk_kctx_(ctx))
    | KExpUnary(uop, a, (_, loc) as ctx) =>
        KExpUnary(uop, walk_atom_(a, loc), walk_kctx_(ctx))
    | KExpIntrin(iop, args, (_, loc) as ctx) =>
        KExpIntrin(iop, walk_al_(args, loc), walk_kctx_(ctx))
    | KExpIf(c, then_e, else_e, ctx) =>
        KExpIf(walk_kexp_(c), walk_kexp_(then_e), walk_kexp_(else_e), walk_kctx_(ctx))
    | KExpSeq(elist, ctx) =>
        fun process_elist(elist: kexp_t list, result: kexp_t list) =
            match elist {
            | e :: rest =>
                val new_e = walk_kexp_(e)
                val new_result =
                    match new_e {
                    | KExpNop _ => if rest != [] {result} else {new_e :: result}
                    | KExpSeq(el, _) => el.rev() + result
                    | _ => new_e :: result
                    }
                process_elist(rest, new_result)
            | _ => result.rev()
            }
        val new_elist = process_elist(elist, [])
        val (new_ktyp, loc) = walk_kctx_(ctx)
        match new_elist {
        | [] => KExpNop(loc)
        | e :: [] => e
        | _ => KExpSeq(new_elist, (new_ktyp, loc))
        }
    | KExpSync(n, e) => KExpSync(n, walk_kexp_(e))
    | KExpMkTuple(alist, (_, loc) as ctx) =>
        KExpMkTuple(walk_al_(alist, loc), walk_kctx_(ctx))
    | KExpMkRecord(alist, (_, loc) as ctx) =>
        KExpMkRecord(walk_al_(alist, loc), walk_kctx_(ctx))
    | KExpMkClosure(make_fp, f, args, (_, loc) as ctx) =>
        KExpMkClosure(walk_id_(make_fp, loc), walk_id_(f, loc),
                      walk_al_(args, loc), walk_kctx_(ctx))
    | KExpMkArray(all_literals, elems, (_, loc) as ctx) =>
        if all_literals {
            KExpMkArray(all_literals, elems, walk_kctx_(ctx))
        } else {
            KExpMkArray(false,
                [for row <- elems {
                    val fold new_row = [] for (f, a) <- row {
                        (f, walk_atom_(a, loc)) :: new_row
                    }
                    new_row.rev() }
                ], walk_kctx_(ctx))
        }
    | KExpMkVector(elems, (_, loc) as ctx) =>
        KExpMkVector(
            [for (f, a) <- elems { (f, walk_atom_(a, loc)) } ],
            walk_kctx_(ctx))
    | KExpCall(f, args, (_, loc) as ctx) =>
        KExpCall(walk_id_(f, loc), walk_al_(args, loc), walk_kctx_(ctx))
    | KExpICall(obj, meth, args, (_, loc) as ctx) =>
        KExpICall(walk_id_(obj, loc), meth, walk_al_(args, loc), walk_kctx_(ctx))
    | KExpAt(a, border, interp, idxs, (_, loc) as ctx) =>
        KExpAt(walk_atom_(a, loc), border, interp,
               [for idx <- idxs { walk_dom_(idx, loc) } ],
               walk_kctx_(ctx))
    | KExpAssign(lv, rv, loc) =>
        KExpAssign(walk_id_(lv, loc), walk_atom_(rv, loc), loc)
    | KExpMem(k, member, (_, loc) as ctx) =>
        KExpMem(walk_id_(k, loc), member, walk_kctx_(ctx))
    | KExpThrow(k, f, loc) => KExpThrow(walk_id_(k, loc), f, loc)
    | KExpWhile(c, e, loc) => KExpWhile(walk_kexp_(c), walk_kexp_(e), loc)
    | KExpDoWhile(e, c, loc) => KExpDoWhile(walk_kexp_(e), walk_kexp_(c), loc)
    | KExpFor(idoml, at_ids, body, flags, loc) =>
        KExpFor(walk_idomlist_(idoml, loc),
                walk_idlist_(at_ids, loc, true),
                walk_kexp_(body), flags, loc)
    | KExpMap(e_idoml_l, body, flags, (_, loc) as ctx) =>
        KExpMap([for (e, idoml, at_ids) <- e_idoml_l {
                       (walk_kexp_(e), walk_idomlist_(idoml, loc),
                       walk_idlist_(at_ids, loc, true))
                }],
                walk_kexp_(body), flags, walk_kctx_(ctx))
    | KExpMatch(cases, ctx) =>
        KExpMatch(
            [for (checks_i, ei) <- cases {
                ([for cij <- checks_i { walk_kexp_(cij) } ],
                walk_kexp_(ei))
            }], walk_kctx_(ctx))
    | KExpTryCatch(e1, e2, ctx) =>
        KExpTryCatch(walk_kexp_(e1), walk_kexp_(e2), walk_kctx_(ctx))
    | KExpCast(a, t, loc) => KExpCast(walk_atom_(a, loc), walk_ktyp_(t, loc), loc)
    | KExpCCode(str, ctx) => KExpCCode(str, walk_kctx_(ctx))
    | KDefVal(k, e, loc) =>
        val new_kv_name = update_kval_(k, loc)
        KDefVal(new_kv_name, walk_kexp_(e), loc)
    | KDefFun kf =>
        val {kf_name, kf_params, kf_rt, kf_body, kf_closure, kf_loc} = *kf
        val {kci_arg, kci_fcv_t, kci_fp_typ, kci_make_fp, kci_make_fp_sr, kci_wrap_f} = kf_closure
        val new_kf_name = walk_id_(kf_name, kf_loc)
        val new_kci_arg = update_kval_(kci_arg, kf_loc)
        val new_kf = kf->{
            kf_name=new_kf_name,
            kf_params=walk_idlist_(kf_params, kf_loc, true),
            kf_rt=walk_ktyp_(kf_rt, kf_loc),
            kf_body=walk_kexp_(kf_body),
            kf_closure= kdefclosureinfo_t {
                kci_arg=new_kci_arg,
                kci_fcv_t=walk_id_(kci_fcv_t, kf_loc),
                kci_fp_typ=walk_id_(kci_fp_typ, kf_loc),
                kci_make_fp=walk_id_(kci_make_fp, kf_loc),
                kci_make_fp_sr=walk_id_(kci_make_fp_sr, kf_loc),
                kci_wrap_f=walk_id_(kci_wrap_f, kf_loc)
                }
            }
        if new_kf_name == kf_name {
            *kf = new_kf; e
        } else {
            val new_kf = ref new_kf
            set_idk_entry(new_kf_name, KFun(new_kf))
            KDefFun(new_kf)
        }
    | KDefExn ke =>
        val {ke_name, ke_tag, ke_make, ke_typ, ke_loc} = *ke
        val new_ke_name = walk_id_(ke_name, ke_loc)
        val new_ke = ke->{
            ke_name=new_ke_name,
            ke_typ=walk_ktyp_(ke_typ, ke_loc),
            ke_tag=walk_id_(ke_tag, ke_loc),
            ke_make=walk_id_(ke_make, ke_loc)
            }
        if new_ke_name == ke_name {
            *ke = new_ke; e
        } else {
            val new_ke = ref new_ke
            set_idk_entry(new_ke_name, KExn(new_ke))
            KDefExn(new_ke)
        }
    | KDefVariant kvar =>
        val {kvar_name, kvar_cases, kvar_ifaces, kvar_ctors, kvar_loc} = *kvar
        val new_kvar_name = walk_id_(kvar_name, kvar_loc)
        val new_kvar = kvar->{
            kvar_name=new_kvar_name,
            kvar_cases=[for (n, t) <- kvar_cases {
                    (walk_id_(n, kvar_loc),
                    walk_ktyp_(t, kvar_loc))
                }],
            kvar_ifaces=[for (iname, meths) <- kvar_ifaces {
                    (walk_id_(iname, kvar_loc),
                    walk_idlist_(meths, kvar_loc, false))
                }],
            kvar_ctors=[
                for c <- kvar_ctors {
                    match kinfo_(c, kvar_loc) {
                    | KVal _ => update_kval_(c, kvar_loc)
                    | _ => walk_id_(c, kvar_loc)
                    }
                }]
            }
        if new_kvar_name == kvar_name {
            *kvar = new_kvar; e
        } else {
            val new_kvar = ref new_kvar
            set_idk_entry(new_kvar_name, KVariant(new_kvar))
            KDefVariant(new_kvar)
        }
    | KDefTyp kt =>
        val {kt_name, kt_typ, kt_loc} = *kt
        val new_kt_name = walk_id_(kt_name, kt_loc)
        val new_kt = kt->{
            kt_name=new_kt_name,
            kt_typ=walk_ktyp_(kt_typ, kt_loc)
            }
        if new_kt_name == kt_name {
            *kt = new_kt; e
        } else {
            val new_kt = ref new_kt
            set_idk_entry(new_kt_name, KTyp(new_kt))
            KDefTyp(new_kt)
        }
    | KDefInterface ki =>
        val {ki_name, ki_base, ki_id, ki_all_methods, ki_loc} = *ki
        val new_ki_name = walk_id_(ki_name, ki_loc)
        val new_ki = ki->{
            ki_name=new_ki_name,
            ki_base=walk_id_(ki_base, ki_loc),
            ki_id=walk_id_(ki_id, ki_loc),
            ki_all_methods=[for (f, t) <- ki_all_methods {
                    (walk_id_(f, ki_loc), walk_ktyp_(t, ki_loc))
                }]
            }
        if new_ki_name == ki_name {
            *ki = new_ki; e
        } else {
            val new_ki = ref new_ki
            set_idk_entry(new_ki_name, KInterface(new_ki))
            KDefInterface(new_ki)
        }
    | KDefClosureVars kcv =>
        val {kcv_name, kcv_freevars, kcv_orig_freevars, kcv_loc} = *kcv
        val new_kcv_name = walk_id_(kcv_name, kcv_loc)
        val new_kcv = kcv->{
            kcv_name=new_kcv_name,
            kcv_freevars=[for (n, t) <- kcv_freevars {
                    (walk_id_(n, kcv_loc), walk_ktyp_(t, kcv_loc))
                }],
            kcv_orig_freevars=walk_idlist_(kcv_orig_freevars, kcv_loc, false)
            }
        if new_kcv_name == kcv_name {
            *kcv = new_kcv; e
        } else {
            val new_kcv = ref new_kcv
            set_idk_entry(new_kcv_name, KClosureVars(new_kcv))
            KDefClosureVars(new_kcv)
        }
    }
}

/* walk through a K-normalized syntax tree and perform some actions;
   do not construct/return anything (though, it's expected that
   the callbacks collect some information about the tree) */

type k_fold_callb_t =
{
    kcb_fold_ktyp: ((ktyp_t, loc_t, k_fold_callb_t) -> void)?;
    kcb_fold_kexp: ((kexp_t, k_fold_callb_t) -> void)?;
    kcb_fold_atom: ((atom_t, loc_t, k_fold_callb_t) -> void)?
}

fun check_n_fold_ktyp(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t): void =
    match callb.kcb_fold_ktyp {
    | Some(f) => f(t, loc, callb)
    | _ => fold_ktyp(t, loc, callb)
    }
fun check_n_fold_kexp(e: kexp_t, callb: k_fold_callb_t): void =
    match callb.kcb_fold_kexp {
    | Some(f) => f(e, callb)
    | _ => fold_kexp(e, callb)
    }
fun check_n_fold_atom(a: atom_t, loc: loc_t, callb: k_fold_callb_t): void =
    match callb.kcb_fold_atom {
    | Some(f) => f(a, loc, callb)
    | _ => match a {
           | AtomLit(KLitNil(t)) => check_n_fold_ktyp(t, loc, callb)
           | _ => {}
           }
    }
fun check_n_fold_al(al: atom_t list, loc: loc_t, callb: k_fold_callb_t): void =
    for a <- al { check_n_fold_atom(a, loc, callb) }

fun check_n_fold_dom(d: dom_t, loc: loc_t, callb: k_fold_callb_t): void =
    match d {
    | DomainElem(a) => check_n_fold_atom(a, loc, callb)
    | DomainFast(a) => check_n_fold_atom(a, loc, callb)
    | DomainRange(a, b, c) => check_n_fold_atom(a, loc, callb)
                              check_n_fold_atom(b, loc, callb)
                              check_n_fold_atom(c, loc, callb)
    }

fun check_n_fold_id(k: id_t, loc: loc_t, callb: k_fold_callb_t) =
    match callb.kcb_fold_atom {
    | Some(f) when k != noid => f(AtomId(k), loc, callb)
    | _ => {}
    }

fun fold_ktyp(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t): void
{
    fun fold_ktyp_(t: ktyp_t) = check_n_fold_ktyp(t, loc, callb)
    fun fold_ktl_(tl: ktyp_t list) = tl.app(fold_ktyp_)
    fun fold_id_(n: id_t) = check_n_fold_id(n, loc, callb)
    match t {
    | KTypInt | KTypCInt | KTypSInt _ | KTypUInt _ | KTypFloat _ | KTypVoid
    | KTypBool | KTypChar | KTypString | KTypCPointer | KTypExn | KTypErr | KTypModule =>
        {}
    | KTypFun(args, rt) => fold_ktl_(args); fold_ktyp_(rt)
    | KTypTuple(elems) => fold_ktl_(elems)
    | KTypRecord(rn, relems) =>
        fold_id_(rn); for (ni, ti) <- relems { fold_id_(ni); fold_ktyp_(ti) }
    | KTypName(n) => fold_id_(n)
    | KTypArray(d, t) => fold_ktyp_(t)
    | KTypList(t) => fold_ktyp_(t)
    | KTypVector(t) => fold_ktyp_(t)
    | KTypRef(t) => fold_ktyp_(t)
    }
}

fun fold_kexp(e: kexp_t, callb: k_fold_callb_t): void
{
    fun fold_atom_(a: atom_t, loc: loc_t): void = check_n_fold_atom(a, loc, callb)
    fun fold_al_(al: atom_t list, loc: loc_t): void =
        for a <- al { fold_atom_(a, loc) }
    fun fold_ktyp_(t: ktyp_t, loc: loc_t): void =
        check_n_fold_ktyp(t, loc, callb)
    fun fold_id_(n: id_t, loc: loc_t): void =
        check_n_fold_id(n, loc, callb)
    fun fold_idlist_(nl: id_t list, loc: loc_t, kvals: bool): void =
        for n <- nl {
            if kvals {fold_kval_(n, loc)} else {fold_id_(n, loc)}
        }
    fun fold_kexp_(e: kexp_t): void = check_n_fold_kexp(e, callb)
    fun fold_dom_(d: dom_t, loc: loc_t) = check_n_fold_dom(d, loc, callb)
    fun fold_idoml_(idoml: (id_t, dom_t) list, loc: loc_t) =
        for (k, d) <- idoml { fold_kval_(k, loc); fold_dom_(d, loc) }
    fun fold_kval_(n: id_t, loc: loc_t) =
        if n.m > 0 && n.i > 0 {
            val {kv_typ} = get_kval(n, loc)
            fold_ktyp_(kv_typ, loc)
            fold_id_(n, loc)
        } else { fold_id_(n, loc) }
    match e {
    | KExpNop _ => {}
    | KExpBreak _ => {}
    | KExpContinue _ => {}
    | KExpReturn(a_opt, loc) => match a_opt {|Some(a)=>fold_atom_(a, loc)| _ => {}}
    | KExpAtom(a, (t, loc)) => fold_atom_(a, loc); fold_ktyp_(t, loc)
    | KExpBinary(_, a1, a2, (t, loc)) =>
        fold_atom_(a1, loc); fold_atom_(a2, loc); fold_ktyp_(t, loc)
    | KExpUnary(_, a, (t, loc)) => fold_atom_(a, loc); fold_ktyp_(t, loc)
    | KExpIntrin(_, args, (t, loc)) => fold_al_(args, loc); fold_ktyp_(t, loc)
    | KExpIf(c, then_e, else_e, (t, loc)) =>
        fold_kexp_(c); fold_kexp_(then_e)
        fold_kexp_(else_e); fold_ktyp_(t, loc)
    | KExpSeq(elist, (t, loc)) => elist.app(fold_kexp_); fold_ktyp_(t, loc)
    | KExpSync(n, e) => fold_kexp_(e)
    | KExpMkTuple(alist, (t, loc)) => fold_al_(alist, loc); fold_ktyp_(t, loc)
    | KExpMkRecord(alist, (t, loc)) => fold_al_(alist, loc); fold_ktyp_(t, loc)
    | KExpMkClosure(make_fp, f, args, (t, loc)) =>
        fold_id_(make_fp, loc); fold_id_(f, loc)
        fold_al_(args, loc); fold_ktyp_(t, loc)
    | KExpMkArray(all_literals, elems, (t, loc)) =>
        if !all_literals {
            for row <- elems { for (_, a) <- row { fold_atom_(a, loc) } }
        }
        fold_ktyp_(t, loc)
    | KExpMkVector(elems, (t, loc)) =>
        for (_, a) <- elems { fold_atom_(a, loc) }
        fold_ktyp_(t, loc)
    | KExpCall(f, args, (t, loc)) =>
        fold_id_(f, loc); fold_al_(args, loc); fold_ktyp_(t, loc)
    | KExpICall(obj, _, args, (t, loc)) =>
        fold_id_(obj, loc); fold_al_(args, loc); fold_ktyp_(t, loc)
    | KExpAt(a, border, interp, idxs, (t, loc)) =>
        fold_atom_(a, loc)
        for idx <- idxs { fold_dom_(idx, loc) }
        fold_ktyp_(t, loc)
    | KExpAssign(lv, rv, loc) =>
        fold_id_(lv, loc); fold_atom_(rv, loc)
    | KExpMem(k, _, (t, loc)) => fold_id_(k, loc); fold_ktyp_(t, loc)
    | KExpThrow(k, _, loc) => fold_id_(k, loc)
    | KExpWhile(c, e, _) =>
        fold_kexp_(c); fold_kexp_(e)
    | KExpDoWhile(e, c, _) =>
        fold_kexp_(e); fold_kexp_(c)
    | KExpFor(idoml, at_ids, body, _, loc) =>
        fold_idoml_(idoml, loc); fold_idlist_(at_ids, loc, true)
        fold_kexp_(body)
    | KExpMap(e_idoml_l, body, _, (t, loc)) =>
        for (e, idoml, at_ids) <- e_idoml_l {
            fold_kexp_(e); fold_idoml_(idoml, loc)
            fold_idlist_(at_ids, loc, true)
        }
        fold_kexp_(body)
        fold_ktyp_(t, loc)
    | KExpMatch(cases, (t, loc)) =>
        for (checks_i, ei) <- cases {
            for cij <- checks_i { fold_kexp_(cij) }
            fold_kexp_(ei)
        }
        fold_ktyp_(t, loc)
    | KExpTryCatch(e1, e2, (t, loc)) =>
        fold_kexp_(e1); fold_kexp_(e2); fold_ktyp_(t, loc)
    | KExpCast(a, t, loc) =>
        fold_atom_(a, loc); fold_ktyp_(t, loc)
    | KExpCCode(_, (t, loc)) => fold_ktyp_(t, loc)
    | KDefVal(k, e, loc) =>
        fold_kval_(k, loc); fold_kexp_(e)
    | KDefFun df =>
        val {kf_name, kf_params, kf_rt, kf_body, kf_closure, kf_loc} = *df
        val {kci_arg, kci_fcv_t, kci_fp_typ, kci_make_fp, kci_wrap_f} = kf_closure
        fold_id_(kf_name, kf_loc)
        fold_idlist_(kf_params, kf_loc, true)
        fold_ktyp_(kf_rt, kf_loc)
        fold_kval_(kci_arg, kf_loc)
        fold_id_(kci_fcv_t, kf_loc)
        fold_id_(kci_fp_typ, kf_loc)
        fold_id_(kci_make_fp, kf_loc)
        fold_id_(kci_wrap_f, kf_loc)
        fold_kexp_(kf_body)
    | KDefExn ke =>
        val {ke_name, ke_typ, ke_tag, ke_make, ke_loc} = *ke
        fold_id_(ke_name, ke_loc)
        fold_ktyp_(ke_typ, ke_loc)
        fold_id_(ke_tag, ke_loc)
        fold_id_(ke_make, ke_loc)
    | KDefVariant kvar =>
        val {kvar_name, kvar_cases, kvar_ctors, kvar_ifaces, kvar_loc} = *kvar
        fold_id_(kvar_name, kvar_loc)
        for (n, t) <- kvar_cases {
            fold_id_(n, kvar_loc)
            fold_ktyp_(t, kvar_loc)
        }
        for c <- kvar_ctors {
            match kinfo_(c, kvar_loc) {
            | KVal _ => fold_kval_(c, kvar_loc)
            | _ => fold_id_(c, kvar_loc)
            }
        }
        for (iname, meths) <- kvar_ifaces {
            fold_id_(iname, kvar_loc)
            fold_idlist_(meths, kvar_loc, false)
        }
    | KDefTyp kt =>
        val {kt_name, kt_typ, kt_loc} = *kt
        fold_id_(kt_name, kt_loc)
        fold_ktyp_(kt_typ, kt_loc)
    | KDefInterface ki =>
        val {ki_name, ki_base, ki_id, ki_all_methods, ki_loc} = *ki
        fold_id_(ki_name, ki_loc)
        fold_id_(ki_base, ki_loc)
        fold_id_(ki_id, ki_loc)
        for (f, t) <- ki_all_methods {
            fold_id_(f, ki_loc)
            fold_ktyp_(t, ki_loc)
        }
    | KDefClosureVars kcv =>
        val {kcv_name, kcv_freevars, kcv_orig_freevars, kcv_loc} = *kcv
        fold_id_(kcv_name, kcv_loc)
        for (n, t) <- kcv_freevars {
            fold_id_(n, kcv_loc)
            fold_ktyp_(t, kcv_loc)
        }
        fold_idlist_(kcv_orig_freevars, kcv_loc, false)
    }
}

fun used_by(code: kcode_t, size0: int): id_hashset_t
{
    val all_used: id_hashset_t = Hashset.empty(size0, noid)
    fun remove_unless(had_before: bool, n: id_t) =
        if !had_before { all_used.remove(n) }
    fun add_id(n: id_t) = if n.m > 0 { all_used.add(n) }
    fun used_by_atom_(a: atom_t, loc: loc_t, callb: k_fold_callb_t): void =
        match a {
        | AtomId(n) => add_id(n)
        | AtomLit(KLitNil(t)) => used_by_ktyp_(t, loc, callb)
        | _ => {}
        }
    fun used_by_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t): void = fold_ktyp(t, loc, callb)
    fun used_by_kexp_(e: kexp_t, callb: k_fold_callb_t): void =
        match e {
        | KDefVal(n, e, loc) =>
            val {kv_typ, kv_flags} = get_kval(n, loc)
            val is_ctor = kv_flags.val_flag_ctor > 0
            if !is_ctor {
                used_by_ktyp_(kv_typ, loc, callb)
                used_by_kexp_(e, callb)
            }
        | KDefFun (ref {kf_name, kf_params, kf_rt, kf_closure, kf_body, kf_flags, kf_loc}) =>
            val {kci_arg, kci_fcv_t} = kf_closure
            // if the function is constructor, we do not count as used its return type,
            // otherwise we get a dependency loop and any type with its constructor will
            // always survive, even if the code that uses the type and constructors is removed.
            val is_ctor = kf_flags.fun_flag_ctor != CtorNone
            val kf_typ = get_kf_typ(kf_params, if is_ctor {KTypVoid} else {kf_rt}, kf_loc)
            used_by_ktyp_(kf_typ, kf_loc, callb)
            val have_kf_name = all_used.mem(kf_name)
            if !is_ctor { used_by_kexp_(kf_body, callb) }
            remove_unless(have_kf_name, kf_name)
            add_id(kci_arg)
            add_id(kci_fcv_t)
        | KDefExn (ref {ke_name, ke_typ, ke_tag, ke_make, ke_loc}) =>
            used_by_ktyp_(ke_typ, ke_loc, callb)
            add_id(ke_tag)
            add_id(ke_make)
        | KDefVariant (ref {kvar_name, kvar_cases, kvar_ifaces, kvar_loc}) =>
            val have_kvar_name = all_used.mem(kvar_name)
            for (ni, ti) <- kvar_cases {
                add_id(ni)
                used_by_ktyp_(ti, kvar_loc, callb)
            }
            for (iname, methods) <- kvar_ifaces {
                add_id(iname)
                for m <- methods { add_id(m) }
            }
            remove_unless(have_kvar_name, kvar_name)
        | KDefClosureVars _ => {}
        | KDefInterface (ref {ki_name, ki_base, ki_id, ki_all_methods, ki_loc}) =>
            add_id(ki_base); add_id(ki_id)
            for (f, t) <- ki_all_methods {
                add_id(f); used_by_ktyp_(t, ki_loc, callb)
            }
        | KDefTyp (ref {kt_name, kt_typ, kt_loc}) =>
            val have_kt_name = all_used.mem(kt_name)
            used_by_ktyp_(kt_typ, kt_loc, callb)
            remove_unless(have_kt_name, kt_name)
        | _ => fold_kexp(e, callb)
        }
    val used_decl_callb = k_fold_callb_t
    {
        kcb_fold_atom=Some(used_by_atom_),
        kcb_fold_ktyp=Some(used_by_ktyp_),
        kcb_fold_kexp=Some(used_by_kexp_)
    }
    for e <- code {
        used_by_kexp_(e, used_decl_callb)
    }
    all_used
}

fun declared(code: kcode_t, size0: int): id_hashset_t
{
    val all_decl: id_hashset_t = Hashset.empty(size0, noid)
    fun add_id(n: id_t) = if n.m > 0 { all_decl.add(n) }
    fun decl_by_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t): void {}
    fun decl_by_kexp_(e: kexp_t, callb: k_fold_callb_t): void =
        match e {
        | KDefVal(n, e, _) =>
            add_id(n)
            decl_by_kexp_(e, callb)
        | KDefFun (ref {kf_name, kf_params, kf_closure, kf_body}) =>
            val {kci_arg} = kf_closure
            decl_by_kexp_(kf_body, callb)
            add_id(kci_arg)
            add_id(kf_name)
            for a <- kf_params { add_id(a) }
        | KDefClosureVars (ref {kcv_name}) => add_id(kcv_name)
        | KDefInterface (ref {ki_name}) => add_id(ki_name)
        | KDefExn (ref {ke_name}) =>
            add_id(ke_name)
        | KDefVariant (ref {kvar_name}) =>
            /*for (ni, _) <- kvar_cases {
                add_id(ni)
                //add_id(ci)
            }*/
            add_id(kvar_name)
        | KDefTyp (ref {kt_name}) =>
            add_id(kt_name)
        | KExpMap(clauses, body, _, (t, _)) =>
            fold_kexp(e, callb)
            for (_, id_l, at_ids) <- clauses {
                for i <- at_ids { add_id(i) }
                for (i, _) <- id_l { add_id(i) }
            }
        | KExpFor(id_l, at_ids, body, _, _) =>
            fold_kexp(e, callb)
            for i <- at_ids { add_id(i) }
            for (i, _) <- id_l { add_id(i) }
        | _ => fold_kexp(e, callb)
        }
    val decl_callb = k_fold_callb_t
    {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(decl_by_ktyp_),
        kcb_fold_kexp=Some(decl_by_kexp_)
    }
    for e <- code {
        decl_by_kexp_(e, decl_callb)
    }
    all_decl
}

fun is_mutable(n: id_t, loc: loc_t): bool
{
    val info = kinfo_(n, loc)
    check_kinfo(info, n, loc)
    match info {
    | KNone => false
    | KVal ({kv_flags}) => kv_flags.val_flag_mutable
    | KFun _ => false
    | KExn _ => false
    | KClosureVars _ | KVariant _ | KInterface _ | KTyp _ => false
    }
}

fun is_mutable_atom(a: atom_t, loc: loc_t): bool =
    match a {
    | AtomId(n) => is_mutable(n, loc)
    | _ => false
    }

fun is_subarray(n: id_t, loc: loc_t): bool
{
    val info = kinfo_(n, loc)
    check_kinfo(info, n, loc)
    match info {
    | KVal ({kv_flags}) => kv_flags.val_flag_subarray
    | _ => false
    }
}

fun get_closure_freevars(f: id_t, loc: loc_t): ((id_t, ktyp_t) list, id_t list) =
    match kinfo_(f, loc) {
    | KFun (ref {kf_closure={kci_fcv_t}}) =>
        if kci_fcv_t == noid {
            ([], [])
        } else {
            match kinfo_(kci_fcv_t, loc) {
            | KClosureVars (ref {kcv_freevars, kcv_orig_freevars}) => (kcv_freevars, kcv_orig_freevars)
            | _ => throw compile_err(loc, f"invalid description of a closure data '{kci_fcv_t}' (should KClosureVars ...)")
            }
        }
    | _ => throw compile_err(loc, f"get_closure_freevars argument '{f}' is not a function")
    }

fun make_empty_kf_closure(): kdefclosureinfo_t =
    kdefclosureinfo_t {
        kci_arg=noid, kci_fcv_t=noid,
        kci_fp_typ=noid, kci_make_fp=noid,
        kci_make_fp_sr=noid, kci_wrap_f=noid
    }

fun deref_ktyp(kt: ktyp_t, loc: loc_t): ktyp_t =
    match kt {
    | KTypName(n) =>
        match kinfo_(n, loc) {
        | KTyp (ref {kt_typ, kt_loc}) => deref_ktyp(kt_typ, kt_loc)
        | KVariant _ => kt
        | KInterface _ => kt
        | _ => throw compile_err(loc, f"named 'type' '{n}' does not represent a type")
        }
    | _ => kt
    }

fun is_ktyp_scalar(ktyp: ktyp_t): bool
{
    | KTypInt | KTypCInt | KTypSInt _ | KTypUInt _ | KTypFloat _ | KTypBool | KTypChar => true
    | _ => false
}

fun is_ktyp_integer(t: ktyp_t, allow_bool: bool) =
    match t {
    | KTypCInt | KTypInt | KTypSInt _ | KTypUInt _ => true
    | KTypBool => allow_bool
    | _ => false
    }

fun create_kdefval(n: id_t, ktyp: ktyp_t, flags: val_flags_t,
                   e_opt: kexp_t?, code: kcode_t, loc: loc_t): kcode_t
{
    val dv = kdefval_t {kv_name=n, kv_cname="", kv_typ=ktyp, kv_flags=flags, kv_loc=loc}
    match ktyp {
    | KTypVoid => throw compile_err(loc, "values of 'void' type are not allowed")
    | _ => {}
    }
    set_idk_entry(n, KVal(dv))
    match e_opt {
    | Some(e) => KDefVal(n, e, loc) :: code
    | _ => code
    }
}

fun kexp2atom(m_idx: int, prefix: string, e: kexp_t, tref: bool,
              code: kcode_t): (atom_t, kcode_t) =
    match e {
    | KExpAtom(a, _) => (a, code)
    | _ =>
        val tmp_id = gen_idk(m_idx, prefix)
        val (ktyp, kloc) = get_kexp_ctx(e)
        match ktyp {
        | KTypVoid =>
            throw compile_err(kloc, "'void' expression or declaration cannot be converted to an atom")
        | _ => {}}
        val tref =
            match e {
            | KExpMem(_, _, _) | KExpAt(_, BorderNone, InterpNone, _, _)
            | KExpUnary(OpDeref, _, _) | KExpIntrin(IntrinVariantCase, _, _) => tref
            | _ => false
            }
        val code = create_kdefval(tmp_id, ktyp, default_val_flags().{
            val_flag_tempref=tref, val_flag_temp=!tref}, Some(e), code, kloc)
        (AtomId(tmp_id), code)
    }

fun atom2id(a: atom_t, loc: loc_t, msg: string) =
    match a {
    | AtomId(n) => n
    | AtomLit _ => throw compile_err(loc, msg)
    }

fun kexp2id(m_idx: int, prefix: string, e: kexp_t, tref: bool,
            code: kcode_t, msg: string): (id_t, kcode_t)
{
    val (a, code) = kexp2atom(m_idx, prefix, e, tref, code)
    val i = atom2id(a, get_kexp_loc(e), msg)
    (i, code)
}

fun create_kdeffun(n: id_t, params: id_t list, rt: ktyp_t, flags: fun_flags_t,
                   body_opt: kexp_t?, code: kcode_t, sc: scope_t list, loc: loc_t): kcode_t
{
    val body = match body_opt { | Some(body) => body | _ => KExpNop(loc) }
    val kf = ref (kdeffun_t {
        kf_name=n, kf_cname="", kf_params=params, kf_rt=rt, kf_body=body, kf_flags=flags,
        kf_closure=make_empty_kf_closure(), kf_scope=sc, kf_loc=loc
        })
    set_idk_entry(n, KFun(kf))
    KDefFun(kf) :: code
}

fun create_kdefconstr(n: id_t, paramtyps: ktyp_t list, rt: ktyp_t, ctor: fun_constr_t,
                      isinstance: bool, code: kcode_t, sc: scope_t list, loc: loc_t): kexp_t list
{
    val km_idx = curr_module(sc)
    val params = [for t@idx <- paramtyps {
        val p = gen_idk(km_idx, f"arg{idx}")
        val _ = create_kdefval(p, t, default_val_flags().{val_flag_arg=true}, None, [], loc)
        p
    }]
    create_kdeffun(n, params, rt, default_fun_flags().{fun_flag_ctor=ctor,
                    fun_flag_instance=isinstance}, None, code, sc, loc)
}

fun string(t: ktyp_t): string
{
    fun ktyp2str_(t: ktyp_t, nf: bool) =
        match t {
        | KTypInt => "int"
        | KTypCInt => "int32_t"
        | KTypSInt(n) => f"int{n}_t"
        | KTypUInt(n) => f"uint{n}_t"
        | KTypFloat(16) => "half"
        | KTypFloat(32) => "float"
        | KTypFloat(64) => "double"
        | KTypFloat(n) => throw compile_err(noloc, f"unsupported {n}-bit floating-point type")
        | KTypVoid => "void"
        | KTypBool => "bool"
        | KTypChar => "char"
        | KTypString => "string"
        | KTypCPointer => "cptr"
        | KTypFun(argtyps, rt) =>
            val argtyps_str =
                match argtyps {
                | [] => "void"
                | t :: [] => ktyp2str_(t, true)
                | _ => ktl2str(argtyps)
                }
            val rt_str = ktyp2str_(rt, true)
            if nf { f"({argtyps_str} -> {rt_str})" }
            else { f"{argtyps_str} -> {rt_str}" }
        | KTypTuple(tl) => ktl2str(tl)
        | KTypRecord(n, _) => f"{idk2str(n, noloc)} {{...}}"
        | KTypName(n) => idk2str(n, noloc)
        | KTypArray(d, t) =>
            val commas = ','*(d-1); f"{ktyp2str_(t, true)} [{commas}]"
        | KTypVector(t) => f"{ktyp2str_(t, true)} vector"
        | KTypList(t) => f"{ktyp2str_(t, true)} list"
        | KTypRef(t) => f"{ktyp2str_(t, true)} ref"
        | KTypExn => "exn"
        | KTypErr => "<err>"
        | KTypModule => "<module>"
        }
    ktyp2str_(t, false)
}

fun flt2str(v: double, suffix: string)
{
    val vstr = string(v)
    if vstr[0].isdigit() {vstr + suffix}
    else if vstr.contains("inf") || vstr.contains("INF") {
        if vstr[0] == '-' {"-INFINITY"} else {"INFINITY"}
    } else if vstr.contains("nan") || vstr.contains("NAN") {
        "NAN"
    } else {
        vstr + suffix
    }
}

fun klit2str(lit: klit_t, cmode: bool, loc: loc_t): string
{
    match lit {
    | KLitInt(v) => f"{v}"
    | KLitSInt(64, v) =>
        if cmode { f"{v}LL" } else { f"{v}i{64}" }
    | KLitUInt(64, v) =>
        if cmode { f"{v}ULL" } else { f"{v}i{64}" }
    | KLitSInt(b, v) =>
        if cmode { f"{v}" } else { f"{v}i{b}" }
    | KLitUInt(b, v) =>
        if cmode { f"{v}u" } else { f"{v}u{b}" }
    | KLitFloat(16, v) => if !cmode {f"{v}f"} else {flt2str(v, "f")}
    | KLitFloat(32, v) => if !cmode {f"{v}f"} else {flt2str(v, "f")}
    | KLitFloat(64, v) => if !cmode {f"{v}"} else {flt2str(v, "")}
    | KLitFloat(b, v) => throw compile_err(loc, f"invalid literal LitFloat({b}, {v})")
    | KLitString(s) => s.escaped(quotes=true)
    | KLitChar(c) => repr(c)
    | KLitBool(true) => "true"
    | KLitBool(false) => "false"
    | KLitNil KTypCPointer => "null"
    | KLitNil _ => "[]"
    }
}

fun ktl2str(tl: ktyp_t list): string =
    join_embrace("(", ")", ", ", [| for t <- tl { string(t) } |])

fun atom2str(a: atom_t): string
{
    | AtomId(n) => idk2str(n, noloc)
    | AtomLit(l) => klit2str(l, false, noloc)
}

fun print_idset(setname: string, s: idset_t) {
    print(f"{setname}:[")
    s.app(fun (i) { println(f" {idk2str(i, noloc)}") })
    print(" ]\n")
}

fun print_id_hashset(setname: string, s: id_hashset_t) {
    print(f"{setname}:[")
    s.app(fun (i) { println(f" {idk2str(i, noloc)}") })
    print(" ]\n")
}
