/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    C code represented in a hierarhical form (just like Ast or K_form).

    The main differences from K-form are:
    * there is no nested functions; at Lambda lifting step, all the nested functions
      are converted to closures (if needed) and moved to the top level
    * we add the closure pointer to the list of parameters in most functions
      (i.e. the pointer to the structure that contains 'free variables':
      non-local and yet non-global variables accessed by the function).
      Many of the functions do need a closure, but there is still such a parameter,
      it's just not used. The parameter is needed because when we call a function indirectly,
      via pointer, we don't know whether it needs closure or not. See k_lift module.
    * the type system is further shrinked:
      * Tuples, records, list cells, reference cells, recursive and non-recursive variants,
        "closure variables" data, function pointers (closures themselves) etc.
        are all converted to C structures.
        For some complex data types, such as strings, arrays, exceptions there are
        already standard structures defined in Ficus runtime,
        so no new structures are generated for them.
        For other complex types a unique name (signature) is generated and is used
        to reference the type and name the corresponding C structure.
        For example, KTypList(KTypInt) becomes _fx_Li_t,
        KTypTuple(KTypFloat :: KTypFloat :: KTypFloat :: []) becomes _fx_Ta3f etc.
        See k_mangle module.
    * the memory is now managed manually.
      Reference counting is involved when copying and releasing smart pointers to actual data
      (for those data structures that need it: arrays, strings, references, lists,
      recursive variants, exceptions, closures, smart "C" pointers).
      Cleanup blocks are added to each function (and often to its nested blocks, such as loops,
      "match" cases, try-blocks etc.) to free the allocated objects that are not used anymore.
    * all the data types are classified into 2 categories: dynamic and static.
      * Static types are allocated on stack.
        Those are primitive types (numbers, bool, char), tuples, records, non-recursive variants,
        arrays (their headers, not data), strings (their headers and maybe data),
        exceptions (their headers), closures (their headers).
      * Dynamic types are allocated on heap and are referenced by their pointers.
        There is also a reference counter used to track the number of 'users'
        that share the pointer. The dynamic structures are lists, references and recursive variants,
      The situation is actually more complex than that:
        * Array elements, string characters, closure free variables, exception parameters and some
          other "variable-size" data that are "underwater parts" of some data type' "icebergs",
          are also stored in the heap and supplied with the associated reference counters.
        * Even the static but non-primitive types,
          are passed to functions via pointers. They all, except for arrays,
          are passed as 'const' pointers, e.g.
          int _fx_print_vec(fx_ctx_t* fx_ctx, const _fx_v3f_t* mytup) { ... }
        * Static data types may have fields that are represented by dynamic data types.
          For example, KTypTuple(KTypBool :: KTypList(KTypInt) :: KTypList(KTypInt) :: []).
    * an expression does not represent any element of the code anymore.
      There are now expressions and statements, since it's C/C++.
    * the complex (nested) expressions are re-introduced.
      This is needed to make the final C code more readable
      and to avoid eccessive use of temporary variables. For example,
      `foo((n+1)*2)` looks much better than
      `int t0=n+1; int t1=t0*2; foo(t1)`.
      Of course, the use of expressions is limited to scalar values and
      to the cases when no exceptions may occur when computing them.
    * there is no exceptions anymore; after each function that may throw an exception
      (by itself or from within the nested calls) is called, a error check is added.
      So far, we do not use 'zero-cost exceptions' or such. This is probably TBD.
    * all the multi-dimensional array access operations are converted to the raw 1D accesses
      with proper range checks where needed.
    * comprehensions are reduced to for-loops:
      * array comprehensions are replaced with for-loops over pre-allocated arrays;
      * list comprehensions are replaced with a for-loop that constructs the output list on-fly.
*/

from Ast import *
from K_form import *

type cbinary_t =
    | COpAdd | COpSub | COpMul | COpDiv | COpMod | COpShiftLeft | COpShiftRight
    | COpBitwiseAnd | COpBitwiseOr | COpBitwiseXor | COpLogicAnd | COpLogicOr
    | COpCmp: cmpop_t | COpArrayElem | COpAssign
    | COpAugAdd | COpAugSub | COpAugMul | COpAugDiv | COpAugMod | COpAugSHL | COpAugSHR
    | COpAugBitwiseAnd | COpAugBitwiseOr | COpAugBitwiseXor

type cunary_t =
    | COpPlus | COpNegate | COpBitwiseNot | COpLogicNot
    | COpDeref | COpGetAddr | COpPrefixInc | COpPrefixDec
    | COpSuffixInc | COpSuffixDec

type ctyp_attr_t = | CTypConst | CTypVolatile | CTypStatic

type carg_attr_t = | CArgPassByPtr | CArgRetVal | CArgFV

type clit_t = klit_t

type ctprops_t =
{
    ctp_scalar: bool;
    ctp_complex: bool;
    ctp_ptr: bool;
    ctp_pass_by_ref: bool;
    ctp_make: id_t list;
    ctp_free: (id_t, id_t);
    ctp_copy: (id_t, id_t)
}

type ctyp_t =

    | CTypInt /* this is a direct mapping from TypInt and KTypInt.
                It's ~ ptrdiff_t - a signed version of size_t, i.e.
                32-bit on 32-bit platforms, 64-bit on 64-bit platforms. */
    | CTypCInt /* this is 'int' in C. It's almost always 32-bit */
    | CTypSize_t
    | CTypSInt: int
    | CTypUInt: int
    | CTypFloat: int
    | CTypVoid
    | CTypBool
    | CTypUniChar
    | CTypCSmartPtr
    | CTypString
    | CTypExn
    | CTypStruct: (id_t?, (id_t, ctyp_t) list)
    | CTypUnion: (id_t?, (id_t, ctyp_t) list)
    | CTypFunRawPtr: (ctyp_t list, ctyp_t)
    | CTypRawPtr: (ctyp_attr_t list, ctyp_t)
    | CTypRawArray: (ctyp_attr_t list, ctyp_t)
    | CTypArray: (int, ctyp_t)
    | CTypVector: ctyp_t
    | CTypName: id_t
    | CTypLabel
    | CTypAny

type cctx_t = (ctyp_t, loc_t)

type cexp_t =
    | CExpIdent: (id_t, cctx_t)
    | CExpLit: (clit_t, cctx_t)
    | CExpBinary: (cbinary_t, cexp_t, cexp_t, cctx_t)
    | CExpUnary: (cunary_t, cexp_t, cctx_t)
    | CExpMem: (cexp_t, id_t, cctx_t)
    | CExpArrow: (cexp_t, id_t, cctx_t)
    | CExpCast: (cexp_t, ctyp_t, loc_t)
    | CExpTernary: (cexp_t, cexp_t, cexp_t, cctx_t)
    | CExpCall: (cexp_t, cexp_t list, cctx_t)
    | CExpInit: (cexp_t list, cctx_t)
    | CExpData: (string, string, cctx_t)
    | CExpTyp: (ctyp_t, loc_t)
    /* we don't parse and don't process the inline C code; just retain it as-is */
    | CExpCCode: (string, loc_t)

type cstmt_t =
    | CStmtNop: loc_t
    | CComment: (string, loc_t)
    | CExp: cexp_t
    | CStmtBreak: loc_t
    | CStmtContinue: loc_t
    | CStmtReturn: (cexp_t?, loc_t)
    | CStmtBlock: (cstmt_t list, loc_t)
    | CStmtSync: (id_t, cstmt_t)
    | CStmtIf: (cexp_t, cstmt_t, cstmt_t, loc_t)
    | CStmtGoto: (id_t, loc_t)
    | CStmtLabel: (id_t, loc_t)
    | CStmtFor: (ctyp_t?, cexp_t list, cexp_t?, cexp_t list, cstmt_t, loc_t)
    | CStmtWhile: (cexp_t, cstmt_t, loc_t)
    | CStmtDoWhile: (cstmt_t, cexp_t, loc_t)
    | CStmtSwitch: (cexp_t, (cexp_t list, cstmt_t list) list, loc_t)
    | CDefVal: (ctyp_t, id_t, cexp_t?, loc_t)
    | CDefFun: cdeffun_t ref
    | CDefTyp: cdeftyp_t ref
    | CDefForwardSym: (id_t, loc_t)
    | CDefForwardTyp: (id_t, loc_t)
    | CDefEnum: cdefenum_t ref
    | CDefInterface: cdefinterface_t ref
    | CMacroDef: cdefmacro_t ref
    | CMacroUndef: (id_t, loc_t)
    | CMacroIf: ((cexp_t, cstmt_t list) list, cstmt_t list, loc_t)
    | CMacroInclude: (string, loc_t)
    | CMacroPragma: (string, loc_t)

type ccode_t = cstmt_t list

type cdefval_t =
{
    cv_name: id_t; cv_typ: ctyp_t; cv_cname: string;
    cv_flags: val_flags_t; cv_loc: loc_t
}

type cdeffun_t =
{
    cf_name: id_t; cf_cname: string; cf_args: (id_t, ctyp_t, carg_attr_t list) list;
    cf_rt: ctyp_t; cf_body: cstmt_t list; cf_flags: fun_flags_t;
    cf_scope: scope_t list; cf_loc: loc_t
}

type cdeftyp_t =
{
    ct_name: id_t; ct_typ: ctyp_t; ct_cname: string; ct_props: ctprops_t;
    ct_data_start: int; ct_enum: id_t; ct_ifaces: id_t list; ct_ifaces_id: id_t;
    ct_scope: scope_t list; ct_loc: loc_t
}

type cdefenum_t =
{
    cenum_name: id_t; cenum_members: (id_t, cexp_t?) list;
    cenum_cname: string; cenum_scope: scope_t list; cenum_loc: loc_t
}

type cdeflabel_t =
{
    cl_name: id_t; cl_cname: string; cl_loc: loc_t
}

type cdefmacro_t =
{
    cm_name: id_t; cm_cname: string; cm_args: id_t list;
    cm_body: cstmt_t list; cm_scope: scope_t list; cm_loc: loc_t
}

type cdefexn_t =
{
    cexn_name: id_t; cexn_cname: string; cexn_base_cname: string; cexn_typ: ctyp_t;
    cexn_std: bool; cexn_tag: id_t; cexn_data: id_t; cexn_info: id_t;
    cexn_make: id_t; cexn_scope: scope_t list; cexn_loc: loc_t
}

/*
   For each interface there are 3 entities created in the C code:

    1. The table of functions, C structure containing fields matching the interface methods names.
       In the case of overloading the names are mangled a bit with suffices (_1, _2, etc.)
       Note that if object implements interface, it means that it automatically implements
       all the parent interfaces (a.k.a. base interfaces).
       There is just one table for all of them, and the methods from base interfaces are placed first.

       typedef _fx_I8my_iface_vtbl_t
       {
           ...
       } _fx_I8my_iface_vtbl_t;

    2. id of the interface. Similar to exceptions, we cannot use any particualar unique constant for each interface in advance,
       (unless it's some Microsoft's-like GUID, which takes some space and some time to compare for identity)
       because new modules can be added to the program, modules can get new dependencies,
       new interfaces can be added to the user-created modules as well as the standard lib.
       So we would have to renumerate interfaces all the time.
       Instead, we always reference interfaces by dedicated integer variable,
       defined in the module where interface is defined (which is very similar to cexn_tag for exceptions).

       ...
       int _FX_I8my_iface_id = -1;

       In the module initialization function we call fx_reg_interface(&my_iface_id, ...),
       where some global counter is incremented and assgined to my_iface_id.
    3. The structure representing a concrete instance of an object casted to the interface.
        typedef struct _fx_I8my_iface_t
        {
           const _fx_I8my_iface_vtbl_t* vtbl;
           fx_object_t* obj; // pointer to the dummy structure that contains
                             // reference counter and pointer to the collection
                             // of interface id's with associated vtbl's
        } _fx_I8my_iface_t;
        it's copied with FX_COPY_IFACE(src, dst) and dereferenced with FX_FREE_IFACE(iface)
        (which calls destructor once the reference counter reaches 0)

    If a value has type 'the interface my_iface' then, of course,
    in C code it means the third entity. Correspondingly, ci_name and ci_cname refer to the 3rd entity.
    ci_id refers to the second entity (separately described using CDefVal and CVal)
    and ci_vtbl refers to the table of methods.
*/
type cdefinterface_t =
{
    ci_name: id_t;
    ci_cname: string;
    ci_id: id_t;
    ci_vtbl: id_t;
    ci_base: id_t;
    ci_all_methods: (id_t, ctyp_t) list;
    ci_scope: scope_t list;
    ci_loc: loc_t;
}

type cmodule_t =
{
    cmod_name: id_t; cmod_cname: string; cmod_ccode: cstmt_t list;
    cmod_main: bool; cmod_recompile: bool; cmod_pragmas: pragmas_t
}

type cinfo_t =
    | CNone
    | CVal: cdefval_t
    | CFun: cdeffun_t ref
    | CTyp: cdeftyp_t ref
    | CExn: cdefexn_t ref
    | CInterface: cdefinterface_t ref
    | CEnum: cdefenum_t ref
    | CLabel: cdeflabel_t
    | CMacro: cdefmacro_t ref

val all_idcs = dynvec_create(CNone)
var freeze_idcs = true

fun new_idc_idx(): int {
    if freeze_idcs {
        throw Fail("internal error: attempt to add new idc when they are frozen")
    }
    val new_idx = dynvec_push(all_ids)
    val new_kidx = dynvec_push(K_form.all_idks)
    val new_cidx = dynvec_push(all_idcs)
    if new_idx == new_kidx && new_idx == new_cidx {
        new_idx
    } else {
        throw Fail("internal error: unsynchronized outputs from new_id_idx(), new_idk_idx() and new_idc_idx()")
    }
}

fun cinfo_(i: id_t, loc: loc_t) = dynvec_get(all_idcs, id2idx_(i, loc))

fun gen_temp_idc(s: string): id_t
{
    val i_name = get_id_prefix(s)
    val i_real = new_idc_idx()
    IdTemp(i_name, i_real)
}

fun gen_idc(s: string): id_t
{
    val i_name = get_id_prefix(s)
    val i_real = new_idc_idx()
    IdVal(i_name, i_real)
}

fun dup_idc(old_id: id_t): id_t
{
    val k = new_idc_idx()
    match old_id {
    | IdName i => IdVal(i, k)
    | IdVal (i, j) => IdVal(i, k)
    | IdTemp (i, j) => IdTemp(i, k)
    }
}

fun set_idc_entry(i: id_t, entry: cinfo_t)
{
    val idx = id2idx(i)
    dynvec_set(all_idcs, idx, entry)
}

fun init_all_idcs()
{
    freeze_ids = true
    freeze_idks = true
    freeze_idcs = false
    dynvec_init(all_idcs, K_form.all_idks->count)
}

fun get_cexp_ctx(e: cexp_t): cctx_t
{
    | CExpIdent (_, c) => c
    | CExpLit (_, c) => c
    | CExpBinary (_, _, _, c) => c
    | CExpUnary (_, _, c) => c
    | CExpMem (_, _, c) => c
    | CExpArrow (_, _, c) => c
    | CExpCast (_, t, l) => (t, l)
    | CExpTernary (_, _, _, c) => c
    | CExpCall (_, _, c) => c
    | CExpInit (_, c) => c
    | CExpData (_, _, c) => c
    | CExpTyp (t, l) => (t, l)
    | CExpCCode (_, l) => (CTypAny, l)
}

fun get_cexp_typ(e: cexp_t): ctyp_t = get_cexp_ctx(e).0
fun get_cexp_loc(e: cexp_t): loc_t = get_cexp_ctx(e).1

fun get_cstmt_loc(s: cstmt_t)
{
    | CStmtNop l => l
    | CComment (_, l) => l
    | CExp e => get_cexp_loc(e)
    | CStmtBreak l => l
    | CStmtContinue l => l
    | CStmtReturn (_, l) => l
    | CStmtBlock (_, l) => l
    | CStmtSync (_, s) => get_cstmt_loc(s)
    | CStmtIf (_, _, _, l) => l
    | CStmtGoto (_, l) => l
    | CStmtLabel (_, l) => l
    | CStmtFor (_, _, _, _, _, l) => l
    | CStmtWhile (_, _, l) => l
    | CStmtDoWhile (_, _, l) => l
    | CStmtSwitch (_, _, l) => l
    | CDefVal (_, _, _, l) => l
    | CDefFun (ref {cf_loc}) => cf_loc
    | CDefTyp (ref {ct_loc}) => ct_loc
    | CDefForwardSym (_, cff_loc) => cff_loc
    | CDefForwardTyp (_, cft_loc) => cft_loc
    | CDefEnum (ref {cenum_loc}) => cenum_loc
    | CDefInterface (ref {ci_loc}) => ci_loc
    | CMacroDef (ref {cm_loc}) => cm_loc
    | CMacroUndef (_, l) => l
    | CMacroIf (_, _, l) => l
    | CMacroInclude (_, l) => l
    | CMacroPragma (_, l) => l
}

fun get_cinfo_loc(info: cinfo_t): loc_t
{
    | CNone => noloc
    | CVal ({cv_loc}) => cv_loc
    | CFun (ref {cf_loc}) => cf_loc
    | CTyp (ref {ct_loc}) => ct_loc
    | CExn (ref {cexn_loc}) => cexn_loc
    | CEnum (ref {cenum_loc}) => cenum_loc
    | CInterface (ref {ci_loc}) => ci_loc
    | CLabel ({cl_loc}) => cl_loc
    | CMacro (ref {cm_loc}) => cm_loc
}

fun get_idc_loc(i: id_t, loc: loc_t) = get_cinfo_loc(cinfo_(i, loc))

fun check_cinfo(info: cinfo_t, i: id_t, loc: loc_t) =
    match info {
    | CNone => throw compile_err(loc,
        f"check_cinfo: attempt to request type of non-existing symbol '{i}'")
    | _ => {}
    }

fun get_cinfo_typ(info: cinfo_t, i: id_t, loc: loc_t)
{
    check_cinfo(info, i, loc)
    match info {
    | CNone => CTypAny
    | CVal ({cv_typ}) => cv_typ
    | CFun (ref {cf_args, cf_rt}) =>
        CTypFunRawPtr([: for (_, t, _) <- cf_args {t} :], cf_rt)
    | CTyp (ref {ct_typ}) => ct_typ
    | CExn _ => CTypExn
    | CInterface (ref {ci_name}) => CTypName(ci_name)
    | CMacro (ref {cm_args}) =>
        match cm_args {
        | [] => CTypAny
        | _ => CTypFunRawPtr([: for a <- cm_args {CTypAny} :], CTypAny)
        }
    | CLabel _ => CTypLabel
    | CEnum _ => CTypCInt
    }
}

fun get_idc_typ(i: id_t, loc: loc_t) =
    match i {
    | IdName _ => CTypAny
    | _ => get_cinfo_typ(cinfo_(i, loc), i, loc)
    }

fun get_idc_cname(i: id_t, loc: loc_t) =
    match i {
    | IdName _ => pp(i)
    | _ =>
        match cinfo_(i, loc) {
        | CNone => ""
        | CVal ({cv_cname}) => cv_cname
        | CFun (ref {cf_cname}) => cf_cname
        | CTyp (ref {ct_cname}) => ct_cname
        | CLabel ({cl_cname}) => cl_cname
        | CEnum (ref {cenum_cname}) => cenum_cname
        | CExn (ref {cexn_cname}) => cexn_cname
        | CInterface (ref {ci_cname}) => ci_cname
        | CMacro (ref {cm_cname}) => cm_cname
        }
    }

fun get_lit_ctyp(l: klit_t): ctyp_t =
    match l {
    | KLitInt _ => CTypInt
    | KLitSInt (b, _) => CTypSInt(b)
    | KLitUInt (b, _) => CTypUInt(b)
    | KLitFloat (b, _) => CTypFloat(b)
    | KLitString _ => CTypString
    | KLitChar _ => CTypUniChar
    | KLitBool _ => CTypBool
    | KLitNil t =>
        match t {
        | KTypName n => CTypName(n)
        | _ => CTypRawPtr([], CTypVoid)
        }
    }

fun create_cdefval(n: id_t, t: ctyp_t, flags: val_flags_t,
                   cname: string, e_opt: cexp_t?, code: ccode_t, loc: loc_t)
{
    val dv = cdefval_t {cv_name=n, cv_typ=t, cv_cname=cname, cv_flags=flags, cv_loc=loc}
    match t {
    | CTypVoid => throw compile_err(loc, "values of 'void' type are not allowed")
    | _ =>
        set_idc_entry(n, CVal(dv))
        (CExpIdent(n, (t, loc)), CDefVal(t, n, e_opt, loc) :: code)
    }
}

fun add_cf_arg(v: id_t, ctyp: ctyp_t, cname: string, loc: loc_t)
{
    val cv = cdefval_t {cv_name=v, cv_typ=ctyp, cv_cname=cname,
        cv_flags=default_val_flags().{val_flag_arg=true}, cv_loc=loc}
    set_idc_entry(v, CVal(cv))
}

fun get_ccode_loc(ccode: ccode_t, default_loc: loc_t) =
    loclist2loc(ccode.map(get_cstmt_loc), default_loc)

fun filter_out_nops(code: ccode_t) =
    code.filter(fun (s) { | CStmtNop _ => false | _ => true })

fun ccode2stmt(code: ccode_t, loc: loc_t) =
    match filter_out_nops(code) {
    | [] => CStmtNop(loc)
    | s :: [] => s
    | _ =>
        val final_loc = get_ccode_loc(code, loc)
        CStmtBlock(code, final_loc)
    }

fun rccode2stmt(code: ccode_t, loc: loc_t) =
    match filter_out_nops(code) {
    | [] => CStmtNop(loc)
    | s :: [] => s
    | _ => val final_loc = get_ccode_loc(code, loc)
           CStmtBlock(code.rev(), final_loc)
    }

fun stmt2ccode(s: cstmt_t) =
    match s {
    | CStmtNop _ => []
    | CStmtBlock (slist, _) => slist
    | _ => s :: []
    }

fun cexp2stmt(e: cexp_t) =
    match e {
    | CExpInit ([], (CTypVoid, loc)) => CStmtNop(loc)
    | _ => CExp(e)
    }

fun get_cinterface_opt(t: ctyp_t, loc: loc_t): cdefinterface_t ref?
{
    match t {
    | CTypName(tn) =>
        match cinfo_(tn, loc) {
        | CInterface(ci) => Some(ci)
        | _ => None
        }
    | _ => None
    }
}

type c_callb_t =
{
    ccb_ident: ((id_t, c_callb_t) -> id_t)?;
    ccb_typ: ((ctyp_t, c_callb_t) -> ctyp_t)?;
    ccb_exp: ((cexp_t, c_callb_t) -> cexp_t)?;
    ccb_stmt: ((cstmt_t, c_callb_t) -> cstmt_t)?
}

fun check_n_walk_ident(n: id_t, callb: c_callb_t): id_t =
    match callb.ccb_ident {
    | Some f => f(n, callb)
    | _ => n
    }

fun check_n_walk_ctyp(t: ctyp_t, callb: c_callb_t): ctyp_t =
    match callb.ccb_typ {
    | Some f => f(t, callb)
    | _ => walk_ctyp(t, callb)
    }

fun check_n_walk_cexp(e: cexp_t, callb: c_callb_t) =
    match callb.ccb_exp {
    | Some f => f(e, callb)
    | _ => walk_cexp(e, callb)
    }

fun check_n_walk_cstmt(s: cstmt_t, callb: c_callb_t) =
    match callb.ccb_stmt {
    | Some f => f(s, callb)
    | _ => walk_cstmt(s, callb)
    }

fun walk_ctyp(t: ctyp_t, callb: c_callb_t)
{
    fun walk_id_(n: id_t) = check_n_walk_ident(n, callb)
    fun walk_id_opt_(n_opt: id_t?) =
        match n_opt {
        | Some n => Some(walk_id_(n))
        | _ => None
        }
    fun walk_ctyp_(t: ctyp_t) = check_n_walk_ctyp(t, callb)

    match t {
    | CTypInt | CTypCInt | CTypSInt _ | CTypUInt _ | CTypFloat _
    | CTypSize_t | CTypVoid | CTypBool | CTypExn | CTypAny
    | CTypUniChar | CTypCSmartPtr | CTypString => t
    | CTypStruct (n_opt, selems) =>
        CTypStruct(walk_id_opt_(n_opt),
            [: for (n, t) <- selems { (walk_id_(n), walk_ctyp_(t)) } :])
    | CTypUnion (n_opt, uelems) =>
        CTypUnion(walk_id_opt_(n_opt),
            [: for (n, t) <- uelems { (walk_id_(n), walk_ctyp_(t)) } :])
    | CTypFunRawPtr (args, rt) => CTypFunRawPtr(args.map(walk_ctyp_), walk_ctyp_(rt))
    | CTypArray (d, et) => CTypArray(d, walk_ctyp_(et))
    | CTypVector (et) => CTypVector(walk_ctyp_(et))
    | CTypRawPtr (attrs, t) => CTypRawPtr(attrs, walk_ctyp_(t))
    | CTypRawArray (attrs, et) => CTypRawArray(attrs, walk_ctyp_(et))
    | CTypName n => CTypName(walk_id_(n))
    | CTypLabel => t
    }
}

fun walk_cexp(e: cexp_t, callb: c_callb_t)
{
    fun walk_id_(n: id_t) = check_n_walk_ident(n, callb)
    fun walk_ctyp_(t: ctyp_t) = check_n_walk_ctyp(t, callb)
    fun walk_cexp_(e: cexp_t) = check_n_walk_cexp(e, callb)
    fun walk_ctx_((t: ctyp_t, loc: loc_t)) = (walk_ctyp_(t), loc)

    match e {
    | CExpIdent(n, ctx) => CExpIdent(walk_id_(n), walk_ctx_(ctx))
    | CExpLit(KLitNil(KTypName n), ctx) => CExpLit(KLitNil(KTypName(walk_id_(n))), walk_ctx_(ctx))
    | CExpLit(lit, ctx) => CExpLit(lit, walk_ctx_(ctx))
    | CExpBinary(bop, e1, e2, ctx) => CExpBinary(bop, walk_cexp_(e1), walk_cexp_(e2), walk_ctx_(ctx))
    | CExpUnary(uop, e, ctx) => CExpUnary(uop, walk_cexp_(e), walk_ctx_(ctx))
    | CExpMem(e, m, ctx) => CExpMem(walk_cexp_(e), m, walk_ctx_(ctx))
    | CExpArrow(e, m, ctx) => CExpArrow(walk_cexp_(e), m, walk_ctx_(ctx))
    | CExpCast(e, t, loc) => CExpCast(walk_cexp_(e), walk_ctyp_(t), loc)
    | CExpTernary(e1, e2, e3, ctx) => CExpTernary(walk_cexp_(e1), walk_cexp_(e2), walk_cexp_(e3), walk_ctx_(ctx))
    | CExpTyp(t, loc) => CExpTyp(walk_ctyp_(t), loc)
    | CExpCall(f, args, ctx) => CExpCall(walk_cexp_(f), args.map(walk_cexp_), walk_ctx_(ctx))
    | CExpInit(eseq, ctx) => CExpInit(eseq.map(walk_cexp_), walk_ctx_(ctx))
    | CExpData(kind, fname, ctx) => CExpData(kind, fname, walk_ctx_(ctx))
    | CExpCCode(s, loc) => e
    }
}

fun walk_cstmt(s: cstmt_t, callb: c_callb_t)
{
    fun walk_id_(n: id_t) = check_n_walk_ident(n, callb)
    fun walk_ctyp_(t: ctyp_t) = check_n_walk_ctyp(t, callb)
    fun walk_ctyp_opt_(t_opt: ctyp_t?) =
        match t_opt {
        | Some t => Some(walk_ctyp_(t))
        | _ => t_opt
        }
    fun walk_cexp_(e: cexp_t) = check_n_walk_cexp(e, callb)
    fun walk_cel_(el: cexp_t list) = el.map(walk_cexp_)
    fun walk_cstmt_(s: cstmt_t) = check_n_walk_cstmt(s, callb)
    fun walk_csl_(sl: cstmt_t list) = sl.map(walk_cstmt_)
    fun walk_cexp_opt_(e_opt: cexp_t?) =
        match e_opt {
        | Some e => Some(check_n_walk_cexp(e, callb))
        | _ => e_opt
        }

    match s {
    | CStmtNop _ => s
    | CComment _ => s
    | CExp e => CExp(walk_cexp_(e))
    | CStmtBreak _ => s
    | CStmtContinue _ => s
    | CStmtReturn (e_opt, l) => CStmtReturn(walk_cexp_opt_(e_opt), l)
    | CStmtBlock (sl, l) => CStmtBlock(walk_csl_(sl), l)
    | CStmtSync (n, s) => CStmtSync(n, walk_cstmt_(s))
    | CStmtIf (e, s1, s2, l) => CStmtIf(walk_cexp_(e), walk_cstmt_(s1), walk_cstmt_(s2), l)
    | CStmtGoto (n, l) => CStmtGoto(walk_id_(n), l)
    | CStmtLabel (n, l) => CStmtLabel(walk_id_(n), l)
    | CStmtFor (t_opt, e1, e2_opt, e3, body, l) =>
        CStmtFor(walk_ctyp_opt_(t_opt), walk_cel_(e1), walk_cexp_opt_(e2_opt), walk_cel_(e3), walk_cstmt_(body), l)
    | CStmtWhile (e, body, l) => CStmtWhile(walk_cexp_(e), walk_cstmt_(body), l)
    | CStmtDoWhile (body, e, l) => CStmtDoWhile(walk_cstmt_(body), walk_cexp_(e), l)
    | CStmtSwitch (e, cases, l) =>
        CStmtSwitch(walk_cexp_(e), [: for (ll, sl) <- cases {(walk_cel_(ll), walk_csl_(sl))} :], l)
    | CDefVal (t, n, e_opt, l) => CDefVal(walk_ctyp_(t), walk_id_(n), walk_cexp_opt_(e_opt), l)
    | CDefFun cf =>
        val {cf_name, cf_args, cf_rt, cf_body} = *cf
        *cf = cf->{
            cf_name=walk_id_(cf_name),
            cf_args=[: for (a, t, flags) <- cf_args {
                        (walk_id_(a), walk_ctyp_(t), flags)
                    } :],
            cf_rt=walk_ctyp_(cf_rt),
            cf_body=walk_csl_(cf_body)
        }
        s
    | CDefTyp ct =>
        val {ct_name, ct_typ, ct_enum} = *ct
        *ct = ct->{ct_name=walk_id_(ct_name), ct_typ=walk_ctyp_(ct_typ), ct_enum=walk_id_(ct_enum)}
        s
    | CDefForwardSym (n, loc) => CDefForwardSym(walk_id_(n), loc)
    | CDefForwardTyp (n, loc) => CDefForwardTyp(walk_id_(n), loc)
    | CDefEnum ce =>
        val {cenum_name, cenum_members} = *ce
        *ce = ce->{
            cenum_name=walk_id_(cenum_name),
            cenum_members=[: for (n, e_opt) <- cenum_members {
                                (walk_id_(n), walk_cexp_opt_(e_opt))
                            } :]
        }
        s
    | CDefInterface ci =>
        val {ci_name, ci_base, ci_id, ci_vtbl, ci_all_methods} = *ci
        *ci = ci->{
            ci_name=walk_id_(ci_name), ci_base=walk_id_(ci_base),
            ci_id=walk_id_(ci_id), ci_vtbl=walk_id_(ci_vtbl),
            ci_all_methods=[: for (f, ctyp) <- ci_all_methods {
                                (walk_id_(f), walk_ctyp_(ctyp))
                            } :]
        }
        s
    | CMacroDef cm =>
        val {cm_name, cm_args, cm_body} = *cm
        *cm = cm->{cm_name=walk_id_(cm_name), cm_args=cm_args.map(walk_id_),
                cm_body=cm_body.map(walk_cstmt_)}
        s
    | CMacroUndef (n, l) => CMacroUndef(walk_id_(n), l)
    | CMacroIf (cs_l, else_l, l) =>
        CMacroIf([: for (c, sl) <- cs_l {
                (walk_cexp_(c), walk_csl_(sl))
            } :], walk_csl_(else_l), l)
    | CMacroInclude _ => s
    | CMacroPragma _ => s
    }
}

type c_fold_callb_t =
{
    ccb_fold_ident: ((id_t, c_fold_callb_t) -> void)?;
    ccb_fold_typ: ((ctyp_t, c_fold_callb_t) -> void)?;
    ccb_fold_exp: ((cexp_t, c_fold_callb_t) -> void)?;
    ccb_fold_stmt: ((cstmt_t, c_fold_callb_t) -> void)?
}

fun check_n_fold_ctyp(t: ctyp_t, callb: c_fold_callb_t) =
    match callb.ccb_fold_typ {
    | Some f => f(t, callb)
    | _ => fold_ctyp(t, callb)
    }

fun check_n_fold_cexp(e: cexp_t, callb: c_fold_callb_t) =
    match callb.ccb_fold_exp {
    | Some f => f(e, callb)
    | _ => fold_cexp(e, callb)
    }

fun check_n_fold_cstmt(s: cstmt_t, callb: c_fold_callb_t) =
    match callb.ccb_fold_stmt {
    | Some f => f(s, callb)
    | _ => fold_cstmt(s, callb)
    }

fun check_n_fold_id(n: id_t, callb: c_fold_callb_t) =
    match callb.ccb_fold_ident {
    | Some f => f(n, callb)
    | _ => {}
    }

fun fold_ctyp(t: ctyp_t, callb: c_fold_callb_t)
{
    fun fold_ctyp_(t: ctyp_t) = check_n_fold_ctyp(t, callb)
    fun fold_tl_(tl: ctyp_t list) = tl.app(fold_ctyp_)
    fun fold_id_(i: id_t) = check_n_fold_id(i, callb)
    fun fold_id_opt_(i_opt: id_t?) =
        match i_opt {
        | Some i => check_n_fold_id(i, callb)
        | _ => {}
        }

    match t {
    | CTypInt | CTypCInt | CTypSInt _ | CTypUInt _ | CTypFloat _
    | CTypSize_t | CTypVoid | CTypBool | CTypExn | CTypAny
    | CTypUniChar | CTypString | CTypCSmartPtr =>
        {}
    | CTypStruct (n_opt, selems) =>
        fold_id_opt_(n_opt)
        for (n, t) <- selems { fold_id_(n); fold_ctyp_(t) }
    | CTypUnion (n_opt, uelems) =>
        fold_id_opt_(n_opt)
        for (n, t) <- uelems { fold_id_(n); fold_ctyp_(t) }
    | CTypFunRawPtr (args, rt) =>
        fold_tl_(args); fold_ctyp_(rt)
    | CTypRawPtr (_, t) => fold_ctyp_(t)
    | CTypRawArray (_, et) => fold_ctyp_(et)
    | CTypArray (_, t) => fold_ctyp_(t)
    | CTypVector (t) => fold_ctyp_(t)
    | CTypName n => fold_id_(n)
    | CTypLabel => {}
    }
}

fun fold_cexp(e: cexp_t, callb: c_fold_callb_t)
{
    fun fold_ctyp_(t: ctyp_t) = check_n_fold_ctyp(t, callb)
    fun fold_id_(i: id_t) = check_n_fold_id(i, callb)
    fun fold_cexp_(e: cexp_t) = check_n_fold_cexp(e, callb)
    fun fold_ctx_((t: ctyp_t, _: loc_t)) = fold_ctyp_(t)

    fold_ctx_(
        match e {
        | CExpIdent (n, ctx) => fold_id_(n); ctx
        | CExpLit (KLitNil(KTypName n), ctx) => fold_id_(n); ctx
        | CExpLit (_, ctx) => ctx
        | CExpBinary (_, e1, e2, ctx) =>
            fold_cexp_(e1); fold_cexp_(e2); ctx
        | CExpUnary (_, e, ctx) => fold_cexp_(e); ctx
        | CExpMem (e, _, ctx) => fold_cexp_(e); ctx
        | CExpArrow (e, _, ctx) => fold_cexp_(e); ctx
        | CExpCast (e, t, loc) => fold_cexp_(e); (t, loc)
        | CExpTernary (e1, e2, e3, ctx) =>
            fold_cexp_(e1);fold_cexp_(e2); fold_cexp_(e3); ctx
        | CExpCall (f, args, ctx) =>
            fold_cexp_(f); args.app(fold_cexp_); ctx
        | CExpInit (eseq, ctx) => eseq.app(fold_cexp_); ctx
        | CExpData (_, _, ctx) => ctx
        | CExpTyp (t, loc) => (t, loc)
        | CExpCCode (s, loc) => (CTypAny, loc)
        })
}

fun fold_cstmt(s: cstmt_t, callb: c_fold_callb_t)
{
    fun fold_cstmt_(s: cstmt_t) = check_n_fold_cstmt(s, callb)
    fun fold_csl_(sl: cstmt_t list) = sl.app(fold_cstmt_)
    fun fold_ctyp_(t: ctyp_t) = check_n_fold_ctyp(t, callb)
    fun fold_id_(n: id_t) = check_n_fold_id(n, callb)
    fun fold_cexp_(e: cexp_t) = check_n_fold_cexp(e, callb)
    fun fold_cel_(el: cexp_t list) = el.app(fold_cexp_)
    fun fold_cexp_opt_(e_opt: cexp_t?) =
        match e_opt { | Some e => fold_cexp_(e) | _ => {} }

    match s {
    | CStmtNop _ => {}
    | CComment _ => {}
    | CExp e => fold_cexp_(e)
    | CStmtBreak _ => {}
    | CStmtContinue _ => {}
    | CStmtReturn (e_opt, _) => fold_cexp_opt_(e_opt)
    | CStmtBlock (sl, _) => fold_csl_(sl)
    | CStmtSync (_, s) => fold_cstmt_(s)
    | CStmtIf (e, s1, s2, _) => fold_cexp_(e); fold_cstmt_(s1); fold_cstmt_(s2)
    | CStmtGoto (n, _) => fold_id_(n)
    | CStmtLabel (n, _) => fold_id_(n)
    | CStmtFor (t_opt, e1, e2_opt, e3, body, _) =>
        match t_opt {
        | Some t => fold_ctyp_(t)
        | _ => {}
        }
        fold_cel_(e1)
        fold_cexp_opt_(e2_opt)
        fold_cel_(e3)
        fold_cstmt_(body)
    | CStmtWhile (e, body, _) => fold_cexp_(e); fold_cstmt_(body)
    | CStmtDoWhile (body, e, _) => fold_cstmt_(body); fold_cexp_(e)
    | CStmtSwitch (e, cases, l) =>
        fold_cexp_(e)
        for (ll, sl) <- cases { fold_cel_(ll); fold_csl_(sl) }
    | CDefVal (t, n, e_opt, _) =>
        fold_ctyp_(t); fold_id_(n); fold_cexp_opt_(e_opt)
    | CDefFun cf =>
        val {cf_name, cf_args, cf_rt, cf_body} = *cf
        fold_id_(cf_name)
        for (a, t, _) <- cf_args { fold_id_(a); fold_ctyp_(t) }
        fold_ctyp_(cf_rt)
        fold_csl_(cf_body)
    | CDefTyp ct =>
        val {ct_name, ct_typ, ct_enum} = *ct
        fold_id_(ct_name)
        fold_ctyp_(ct_typ)
        fold_id_(ct_enum)
    | CDefForwardSym (n, _) => fold_id_(n)
    | CDefForwardTyp (n, _) => fold_id_(n)
    | CDefEnum ce =>
        val {cenum_name, cenum_members} = *ce
        fold_id_(cenum_name)
        for (n, e_opt) <- cenum_members { fold_id_(n); fold_cexp_opt_(e_opt) }
    | CDefInterface ci =>
        val {ci_name, ci_base, ci_id, ci_vtbl, ci_all_methods} = *ci
        fold_id_(ci_name); fold_id_(ci_base); fold_id_(ci_id); fold_id_(ci_vtbl)
        for (f, ctyp) <- ci_all_methods { fold_id_(f); fold_ctyp_(ctyp) }
    | CMacroDef cm =>
        val {cm_name, cm_args, cm_body} = *cm
        fold_id_(cm_name)
        cm_args.app(fold_id_)
        cm_body.app(fold_cstmt_)
    | CMacroUndef (n, _) => fold_id_(n)
    | CMacroIf (cs_l, else_l, _) =>
        for (c, sl) <- cs_l { fold_cexp_(c); fold_csl_(sl) }
        fold_csl_(else_l)
    | CMacroInclude _ => {}
    | CMacroPragma _ => {}
    }
}

fun ctyp2str(t: ctyp_t, loc: loc_t) =
    match t {
    | CTypInt => ("int_", noid)
    | CTypCInt => ("int", noid)
    | CTypSize_t => ("size_t", noid)
    | CTypSInt b => (f"int{b}_t", noid)
    | CTypUInt b => (f"uint{b}_t", noid)
    | CTypFloat 16 => ("float16_t", noid)
    | CTypFloat 32 => ("float", noid)
    | CTypFloat 64 => ("double", noid)
    | CTypFloat b => throw compile_err(loc, f"invalid type CTypFloat({b})")
    | CTypString => ("fx_str_t", noid)
    | CTypUniChar => ("char_", noid)
    | CTypBool => ("bool", noid)
    | CTypVoid => ("void", noid)
    | CTypExn => ("fx_exn_t", noid)
    | CTypFunRawPtr (args, rt) =>
        throw compile_err(loc, "ctyp2str: raw function pointer type is not supported; use CTypName(...) instead")
    | CTypCSmartPtr => ("fx_cptr_t", noid)
    | CTypStruct (_, _) =>
        throw compile_err(loc, "ctyp2str: CTypStruct(...) is not supported; use CTypName(...) instead")
    | CTypUnion (_, _) =>
        throw compile_err(loc, "ctyp2str: CTypUnion(...) is not supported; use CTypName(...) instead")
    | CTypRawPtr (attrs, t) =>
        val (s, _) = ctyp2str(t, loc)
        val s = if attrs.mem(CTypStatic) { "static " + s } else { s }
        val s = if attrs.mem(CTypConst) { "const " + s } else { s }
        val s = if attrs.mem(CTypVolatile) { "volatile " + s } else { s }
        (s + "*", noid)
    | CTypRawArray (attrs, t) =>
        val (s, _) = ctyp2str(t, loc)
        val s = if attrs.mem(CTypStatic) { "static " + s } else { s }
        val s = if attrs.mem(CTypConst) { "const " + s } else { s }
        val s = if attrs.mem(CTypVolatile) { "volatile " + s } else { s }
        (s + " []", noid)
    | CTypArray _ => ("fx_arr_t", noid)
    | CTypVector _ => ("fx_vec_t", noid)
    | CTypName n => val cname = get_idc_cname(n, loc); (cname, n)
    | CTypLabel => throw compile_err(loc, "ctyp2str: CTypLabel is not supported")
    | CTypAny => throw compile_err(loc, "ctyp2str: CTypAny is not supported")
    }

fun idc2str(n: id_t, loc: loc_t) {
    val cname = get_idc_cname(n, loc)
    if cname != "" { cname }
    else {
        val (infix, prefix, suffix) =
        match n {
        | IdName(i) => ("", i, 1234567890)
        | IdVal(i, j) => ("_", i, j)
        | IdTemp(i, j) => ("_", i, j)
        }
        val prefix = dynvec_get(all_strings, prefix)
        f"{prefix}{infix}{suffix}"
    }
}

fun ctyp2str_(t: ctyp_t, loc: loc_t): string = ctyp2str(t, loc).0

fun make_ptr(t: ctyp_t) =
    match t {
    | CTypAny => CTypRawPtr([], CTypVoid)
    | _ => CTypRawPtr([], t)
    }

fun make_const_ptr(t: ctyp_t) =
    match t {
    | CTypAny => CTypRawPtr(CTypConst :: [], CTypVoid)
    | _ => CTypRawPtr(CTypConst :: [], t)
    }

val std_CTypVoidPtr = make_ptr(CTypVoid)
val std_CTypConstVoidPtr = make_const_ptr(CTypVoid)
val std_CTypAnyArray = CTypArray(0, CTypAny)
val std_CTypAnyVector = CTypVector(CTypAny)

fun make_lit_exp(l: clit_t, loc: loc_t) {
    val t = get_lit_ctyp(l)
    CExpLit(l, (t, loc))
}

fun make_int__exp(i: int64, loc: loc_t): cexp_t = CExpLit(KLitInt(i), (CTypInt, loc))
fun make_int_exp(i: int, loc: loc_t): cexp_t = CExpLit(KLitInt(int64(i)), (CTypInt, loc))
fun make_bool_exp(b: bool, loc: loc_t) = CExpLit(KLitBool(b), (CTypBool, loc))
fun make_nullptr(loc: loc_t) = CExpLit(KLitNil(KTypVoid), (std_CTypVoidPtr, loc))

fun make_id_exp(i: id_t, loc: loc_t) {
    val t = get_idc_typ(i, loc)
    CExpIdent(i, (t, loc))
}

fun make_id_t_exp(i: id_t, t: ctyp_t, loc: loc_t) = CExpIdent(i, (t, loc))

fun make_label(basename: string, loc: loc_t)
{
    val basename =
        if basename.startswith("_fx_") { basename }
        else { "_fx_" + basename }
    val li = gen_temp_idc(basename)
    val cname = if basename == "_fx_cleanup" { basename }
                else { "" }
    set_idc_entry(li, CLabel (cdeflabel_t {cl_name=li, cl_cname=cname, cl_loc=loc}) )
    li
}

fun make_call(f: id_t, args: cexp_t list, rt: ctyp_t, loc: loc_t) {
    val f_exp = make_id_exp(f, loc)
    CExpCall(f_exp, args, (rt, loc))
}

fun make_dummy_exp(loc: loc_t) = CExpInit([], (CTypVoid, loc))

fun make_assign(lhs: cexp_t, rhs: cexp_t) {
    val loc = get_cexp_loc(rhs)
    CExpBinary(COpAssign, lhs, rhs, (CTypVoid, loc))
}

fun cexp_get_addr(e: cexp_t)
{
    | CExpUnary (COpDeref, x, _) => x
    | CExpBinary (COpArrayElem, x, i, (t, loc)) => CExpBinary(COpAdd, x, i, (make_ptr(t), loc))
    | _ =>
        val (t, loc) = get_cexp_ctx(e)
        val t = match t { | CTypAny => CTypVoid | _ => t }
        val t = CTypRawPtr([], t)
        CExpUnary(COpGetAddr, e, (t, loc))
}

fun cexp_deref_typ(t: ctyp_t)
{
    | CTypRawPtr (_, CTypVoid) => CTypAny
    | CTypRawPtr (_, t) => t
    | _ => CTypAny
}

fun cexp_deref(e: cexp_t)
{
    | CExpUnary(COpGetAddr, x, _) => x
    | CExpBinary(COpAdd, x, i, (t, loc)) =>
        CExpBinary(COpArrayElem, x, i, (cexp_deref_typ(t), loc))
    | _ =>
        val (t, loc) = get_cexp_ctx(e)
        val t = cexp_deref_typ(t)
        CExpUnary(COpDeref, e, (t, loc))
}

fun cexp_arrow(e: cexp_t, m_id: id_t, t: ctyp_t): cexp_t
{
    val loc = get_cexp_loc(e)
    match e {
    | CExpUnary(COpGetAddr, x, _) => CExpMem(x, m_id, (t, loc))
    | _ => CExpArrow(e, m_id, (t, loc))
    }
}

fun cexp_mem(e: cexp_t, m_id: id_t, t: ctyp_t): cexp_t {
    val loc = get_cexp_loc(e)
    match e {
    | CExpUnary(COpDeref, x, _) => CExpArrow(x, m_id, (t, loc))
    | _ => CExpMem(e, m_id, (t, loc))
    }
}

val std_FX_MAX_DIMS = 5
var std_sizeof = noid
var std_fx_malloc = noid
var std_fx_free = noid
var std_fx_free_t = CTypVoid
var std_fx_copy_t = CTypVoid
var std_FX_INCREF = noid
var std_FX_DECREF = noid
var std_FX_REC_VARIANT_TAG = noid
var std_FX_MAKE_RECURSIVE_VARIANT_IMPL_START = noid
var std_FX_MAKE_FP_IMPL_START = noid
var std_FX_CALL = noid
var std_FX_COPY_PTR = noid
var std_FX_COPY_SIMPLE = noid
var std_FX_COPY_SIMPLE_BY_PTR = noid
var std_FX_NOP = noid
var std_FX_BREAK = noid
var std_FX_CONTINUE = noid
var std_FX_CHECK_BREAK = noid
var std_FX_CHECK_BREAK_ND = noid
var std_FX_CHECK_CONTINUE = noid
var std_FX_CHECK_EXN = noid
var std_FX_CHECK_ZERO_STEP = noid
var std_FX_LOOP_COUNT = noid
var std_FX_CHECK_EQ_SIZE = noid
var std_fx_copy_ptr = noid
var std_FX_STR_LENGTH = noid
var std_FX_STR_CHKIDX = noid
var std_FX_STR_ELEM = noid
var std_FX_STR_ELEM_CLIP = noid
var std_FX_STR_ELEM_ZERO = noid
var std_FX_MAKE_STR = noid
var std_FX_FREE_STR = noid
var std_fx_free_str = noid
var std_FX_COPY_STR = noid
var std_fx_copy_str = noid
var std_fx_substr = noid
var std_fx_exn_info_t = CTypVoid
var std_FX_REG_SIMPLE_EXN = noid
var std_FX_REG_SIMPLE_STD_EXN = noid
var std_FX_REG_EXN = noid
var std_FX_MAKE_EXN_IMPL_START = noid
var std_FX_THROW = noid
var std_FX_FAST_THROW = noid
var std_FX_FREE_EXN = noid
var std_FX_COPY_EXN = noid
var std_FX_MAKE_EXN_IMPL = noid
var std_fx_free_exn = noid
var std_fx_copy_exn = noid
var std_FX_RETHROW = noid
var std_FX_FREE_LIST_SIMPLE = noid
var std_fx_free_list_simple = noid
var std_fx_list_length = noid
var std_FX_FREE_LIST_IMPL = noid
var std_FX_MAKE_LIST_IMPL = noid
var std_FX_LIST_APPEND = noid
var std_FX_MOVE_LIST = noid
var std_FX_CHKIDX1 = noid
var std_FX_CHKIDX = noid
var std_FX_PTR_xD = ([]: id_t list)
var std_FX_PTR_xD_CLIP = ([]: id_t list)
var std_FX_PTR_xD_ZERO = ([]: id_t list)
var std_FX_ARR_SIZE = noid
var std_FX_FREE_ARR = noid
var std_FX_MOVE_ARR = noid
var std_fx_free_arr = noid
var std_fx_copy_arr = noid
var std_fx_copy_arr_data = noid
var std_fx_make_arr = noid
var std_fx_subarr = noid
var std_FX_FREE_VEC = noid
var std_fx_free_vec = noid
var std_fx_copy_vec = noid
var std_fx_make_vec = noid
var std_FX_FREE_REF_SIMPLE = noid
var std_fx_free_ref_simple = noid
var std_FX_FREE_REF_IMPL = noid
var std_FX_MAKE_REF_IMPL = noid
var std_FX_FREE_FP = noid
var std_FX_COPY_FP = noid
var std_fx_free_fp = noid
var std_fx_copy_fp = noid
var std_fx_free_cptr = noid
var std_fx_copy_cptr = noid
var std_fx_ifaces_t_cptr = CTypVoid
var std_FX_COPY_IFACE = noid
var std_FX_FREE_IFACE = noid
var std_fx_copy_iface = noid
var std_fx_free_iface = noid
var std_fx_query_iface = noid
var std_fx_get_object = noid
var std_fx_make_iface = noid
