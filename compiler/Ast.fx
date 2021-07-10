/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

//////// ficus abstract syntax definition + helper structures and functions ////////

import Dynvec, Map, Set, Hashmap, Hashset
import File, Filename, Options, Sys

/*
   All the symbols (except for modules) in the code are represented using id_t, a tuple of integers <m, i, j>.
   Initially, m=j=0, which means that the symbol is simply a textual name which is not resolved yet.
   After type checker the id=<0,i,0> is transformed to some <m, i, j> which means symbol #j from module #m.
   The index 'i' of the original symbolic name is preserved for displaying, debugging and some other operations.
   Module m=0 is reserved for textual names, i.e. it's a dummy module, which contains nothing but the symbol table.
   Module m=1 is reserved for ficus runtime.
   Module m=2 is used for the main parsed module supplied by the user, the main program.
   A few next values of m: 3, 4, 5, ... are usually used for 'Builtins' and other standard modules
   that are imported by default (unless user specified -no-preamble option) and the subsequent values
   are used for other, explicitly imported modules, stardard or not. Note that the modules are not
   topologically sorted by a.

   Each module has its own symbol table so that some compiler stages can process different modules
   independently, in parallel. Besides, a separate symbol table for each module helps to make the produced
   K-form and the subsequent C code more or less independent from the other modules (of course, compiler
   can inline some calls of functions from other modules, so there may be some dependency, not it's still
   much weaker than in the case of a single global symbol table).
*/

type id_t = {m: int; i: int; j: int}

fun cmp_id(a: id_t, b: id_t)
{
    val dm = a.m <=> b.m
    if dm != 0 {dm} else {
        val di = a.i <=> b.i
        if di != 0 {di} else {a.j <=> b.j}
    }
}

fun std_id(name: string, bn: (string list, int)): (id_t, (string list, int)) =
    (id_t {m=0, i=bn.1, j=0}, (name :: bn.0, bn.1+1))
val builtin_ids = ([], 0)
val (noid, builtin_ids) = std_id("<noid>", builtin_ids)

operator <=> (a: id_t, b: id_t) = cmp_id(a, b)
operator == (a: id_t, b: id_t) = (a.m == b.m) & (a.i == b.i) && ((a.m == 0) | (a.j == b.j))

type scope_t =
    | ScBlock: int
    | ScLoop: (bool, int)
    | ScFold: int
    | ScArrMap: int
    | ScMap: int
    | ScTry: int
    | ScFun: id_t
    | ScClass: id_t
    | ScInterface: id_t
    | ScModule: int

type loc_t =
{
    m_idx: int;
    line0: int;
    col0: int;
    line1: int;
    col1: int
}

val noloc = loc_t {m_idx=-1, line0=0, col0=0, line1=0, col1=0}

fun loclist2loc(llist: loc_t list, default_loc: loc_t) =
    fold loc = default_loc for loci <- llist {
        val {m_idx, line0, col0, line1, col1} = loc
        val {m_idx=loci_m_idx,
            line0=loci_line0, col0=loci_col0,
            line1=loci_line1, col1=loci_col1} = loci
        if m_idx != loci_m_idx {
            if m_idx <= 0 { loci } else { loc }
        } else {
            loc_t
            {
                m_idx=m_idx,
                line0=min(line0, loci_line0),
                col0=min(col0, loci_col0),
                line1=max(line1, loci_line1),
                col1=max(col1, loci_col1)
            }
        }
    }

fun get_start_loc(loc: loc_t) {
    val {m_idx, line0, col0} = loc
    loc_t {m_idx=m_idx, line0=line0, col0=col0, line1=line0, col1=col0}
}

fun get_end_loc(loc: loc_t) {
    val {m_idx, line1, col1} = loc
    loc_t {m_idx=m_idx, line0=line1, col0=col1, line1=line1, col1=col1}
}

exception CompileError: (loc_t, string)
exception PropagateCompileError

type lit_t =
    | LitInt: int64
    | LitSInt: (int, int64)
    | LitUInt: (int, uint64)
    | LitFloat: (int, double)
    | LitString: string
    | LitChar: char
    | LitBool: bool
    | LitEmpty
    | LitNull

type initializer_t = InitId: id_t | InitLit: lit_t

type typ_t =
    | TypVar: typ_t? ref
    | TypVarTuple: typ_t?
    | TypVarArray: typ_t
    | TypVarRecord
    | TypInt
    | TypSInt: int
    | TypUInt: int
    | TypFloat: int
    | TypString
    | TypChar
    | TypBool
    | TypVoid
    | TypFun: (typ_t list, typ_t)
    | TypList: typ_t
    | TypVector: typ_t
    | TypTuple: typ_t list
    | TypRef: typ_t
    | TypArray: (int, typ_t)
    | TypRecord: ((val_flags_t, id_t, typ_t, exp_t) list, bool) ref
    | TypExn
    | TypErr
    | TypCPointer
    | TypApp: (typ_t list, id_t)
    | TypDecl
    | TypModule

fun make_new_typ() = TypVar(ref None)
fun make_new_ctx(l: loc_t) = (make_new_typ(), l)

type op_assoc_t = AssocLeft | AssocRight
type cmpop_t = CmpEQ | CmpNE | CmpLT | CmpLE | CmpGE | CmpGT
type binary_t =
    | OpAdd | OpSub | OpMul | OpDiv | OpRDiv | OpMod | OpPow
    | OpShiftLeft | OpShiftRight | OpDotAdd | OpDotSub
    | OpDotMul | OpDotDiv | OpDotMod | OpDotPow
    | OpBitwiseAnd | OpLogicAnd | OpBitwiseOr | OpLogicOr | OpBitwiseXor
    | OpCmp: cmpop_t | OpDotCmp: cmpop_t | OpSpaceship | OpDotSpaceship | OpSame | OpCons

type unary_t = OpPlus | OpNegate | OpDotMinus | OpBitwiseNot | OpLogicNot
    | OpMkRef | OpDeref | OpExpand | OpApos

type intrin_t =
    | IntrinPopExn
    | IntrinVariantTag
    | IntrinVariantCase
    | IntrinQueryIface
    | IntrinGetObject
    | IntrinListHead
    | IntrinListTail
    | IntrinStrConcat
    | IntrinGetSize
    | IntrinCheckIdx
    | IntrinCheckIdxRange
    | IntrinMakeFPbyFCV
    | IntrinMath: id_t

type val_flags_t =
{
    val_flag_arg: bool = false;
    val_flag_mutable: bool = false;
    val_flag_temp: bool = false;
    val_flag_tempref: bool = false;
    val_flag_private: bool = false;
    val_flag_subarray: bool = false;
    val_flag_instance: bool = false;
    val_flag_method: (id_t, int);
    val_flag_ctor: int = 0;
    val_flag_global: scope_t list = []
}

fun default_val_flags() = val_flags_t {val_flag_method=(noid, -1)}
fun default_arg_flags() = default_val_flags().{val_flag_arg=true}
fun default_var_flags() = default_val_flags().{val_flag_mutable=true}
fun default_tempval_flags() = default_val_flags().{val_flag_temp=true}
fun default_tempref_flags() = default_val_flags().{val_flag_tempref=true}
fun default_tempvar_flags() = default_tempval_flags().{val_flag_mutable=true}

type fun_constr_t =
    | CtorNone
    | CtorStruct
    | CtorVariant: int
    | CtorFP: id_t
    | CtorExn: id_t

type fun_flags_t =
{
    fun_flag_pure: int=-1;
    fun_flag_ccode: bool=false;
    fun_flag_have_keywords: bool=false;
    fun_flag_inline: bool=false;
    fun_flag_nothrow: bool=false;
    fun_flag_really_nothrow: bool=false;
    fun_flag_private: bool=false;
    fun_flag_ctor: fun_constr_t;
    fun_flag_method_of: id_t;
    fun_flag_uses_fv: bool=false;
    fun_flag_recursive: bool=false;
    fun_flag_instance: bool=false;
}

fun default_fun_flags() = fun_flags_t {fun_flag_ctor=CtorNone, fun_flag_method_of=noid}

type for_make_t = ForMakeNone | ForMakeArray | ForMakeList | ForMakeVector | ForMakeTuple

type for_flags_t =
{
    for_flag_parallel: bool = false;
    for_flag_make: for_make_t;
    for_flag_unzip: bool = false;
    for_flag_fold: bool = false;
    for_flag_nested: bool = false
}

fun default_for_flags() = for_flags_t
{
    for_flag_parallel=false,
    for_flag_make=ForMakeNone,
    for_flag_unzip=false,
    for_flag_fold=false,
    for_flag_nested=false
}

type border_t =
    | BorderNone
    | BorderClip
    | BorderWrap
    | BorderZero

type interpolate_t =
    | InterpNone
    | InterpLinear

val max_zerobuf_size = 256

type var_flags_t =
{
    var_flag_class_from: int=0;
    var_flag_record: bool = false;
    var_flag_recursive: bool = false;
    var_flag_have_tag: bool = true;
    var_flag_have_mutable: bool = false;
    var_flag_opt: bool = false;
    var_flag_instance: bool = false;
}

fun default_variant_flags() = var_flags_t {}

type ctx_t = (typ_t, loc_t)

type exp_t =
    | ExpNop: loc_t
    | ExpBreak: (bool, loc_t)
    | ExpContinue: loc_t
    | ExpReturn: (exp_t?, loc_t)
    | ExpRange: (exp_t?, exp_t?, exp_t?, ctx_t)
    | ExpLit: (lit_t, ctx_t)
    | ExpIdent: (id_t, ctx_t)
    | ExpBinary: (binary_t, exp_t, exp_t, ctx_t)
    | ExpUnary: (unary_t, exp_t, ctx_t)
    | ExpIntrin: (intrin_t, exp_t list, ctx_t)
    | ExpSync: (id_t, exp_t)
    | ExpSeq: (exp_t list, ctx_t)
    | ExpMkTuple: (exp_t list, ctx_t)
    | ExpMkArray: (exp_t list list, ctx_t)
    | ExpMkVector: (exp_t list, ctx_t)
    | ExpMkRecord: (exp_t, (id_t, exp_t) list, ctx_t)
    | ExpUpdateRecord: (exp_t, (id_t, exp_t) list, ctx_t)
    | ExpCall: (exp_t, exp_t list, ctx_t)
    | ExpAt: (exp_t, border_t, interpolate_t, exp_t list, ctx_t)
    | ExpAssign: (exp_t, exp_t, loc_t)
    | ExpMem: (exp_t, exp_t, ctx_t)
    | ExpThrow: (exp_t, loc_t)
    | ExpIf: (exp_t, exp_t, exp_t, ctx_t)
    | ExpWhile: (exp_t, exp_t, loc_t)
    | ExpDoWhile: (exp_t, exp_t, loc_t)
    | ExpFor: ((pat_t, exp_t) list, pat_t, exp_t, for_flags_t, loc_t)
    | ExpMap: (((pat_t, exp_t) list, pat_t) list, exp_t, for_flags_t, ctx_t)
    | ExpTryCatch: (exp_t, (pat_t, exp_t) list, ctx_t)
    | ExpMatch: (exp_t, (pat_t, exp_t) list, ctx_t)
    | ExpCast: (exp_t, typ_t, ctx_t)
    | ExpTyped: (exp_t, typ_t, ctx_t)
    | ExpCCode: (string, ctx_t)
    | ExpData: (string, string, ctx_t)
    | DefVal: (pat_t, exp_t, val_flags_t, loc_t)
    | DefFun: deffun_t ref
    | DefExn: defexn_t ref
    | DefTyp: deftyp_t ref
    | DefVariant: defvariant_t ref
    | DefInterface: definterface_t ref
    | DirImport: ((int, id_t) list, loc_t)
    | DirImportFrom: (int, id_t list, loc_t)
    | DirPragma: (string list, loc_t)

type pat_t =
    | PatAny: loc_t
    | PatLit: (lit_t, loc_t)
    | PatIdent: (id_t, loc_t)
    | PatTuple: (pat_t list, loc_t)
    | PatVariant: (id_t, pat_t list, loc_t)
    | PatRecord: (id_t?, (id_t, pat_t) list, loc_t)
    | PatCons: (pat_t, pat_t, loc_t)
    | PatAs: (pat_t, id_t, loc_t)
    | PatTyped: (pat_t, typ_t, loc_t)
    | PatWhen: (pat_t, exp_t, loc_t)
    | PatAlt: (pat_t list, loc_t)
    | PatRef: (pat_t, loc_t)

type env_entry_t =
    | EnvId: id_t
    | EnvTyp: typ_t

/*
  Environment (env_t) is the mapping from id_t to env_entry_t list.
  It's the key data structure used by the type checker.

  That is, for each id Id.Name(i) (which corresponds to some abstract symbol <i>)
  we store a list of possible matches Id.Val(i, j) - the really defined symbols.
  When the key is Id.Temp(prefix, k), we can only have a single match Id.Temp(prefix, k).

  Why have a list of possible matches? Because:
   * in the nested scopes we can redefine a symbol from the outer scope
   * a defined value can be redefined later in the same block: val a = 5; val a = a + 1
   * we can have overloaded functions, e.g. sin:float->float and sin:double->double
   * types and functions/values with the same name can co-exist without conflicts, e.g.
     string type and string:'t->string functions.

  Note that we use purely-functional data structure (Map) to store the environment.
  Such immutable data structure let us forget about the neccesity to "undo" environment changes
  when we get back from the nested expression analysis, at the expense of slight loss in efficiency
  (however, the type checker is very inexpensive compiler stage, even in "-O0" compile mode)
*/
type env_t = (id_t, env_entry_t list) Map.t
type idmap_t = (id_t, id_t) Map.t
type idset_t = id_t Set.t
val empty_env: env_t = Map.empty(cmp_id)
val empty_idset: idset_t = Set.empty(cmp_id)
val empty_idmap: idmap_t = Map.empty(cmp_id)

type id_hashset_t = id_t Hashset.t
type str_hashset_t = string Hashset.t
fun hash(i: id_t): hash_t =
    (((FNV_1A_OFFSET ^ uint64(i.m))*FNV_1A_PRIME ^ uint64(i.i))*FNV_1A_PRIME ^ uint64(i.j))

fun empty_id_hashset(size0: int): id_t Hashset.t = Hashset.empty(size0, noid)
fun empty_str_hashset(size0: int): string Hashset.t = Hashset.empty(size0, "")
fun id_hashset(s: idset_t) {
    val hs = empty_id_hashset(s.size*2)
    s.app(fun (x) {hs.add(x)})
    hs
}

type defval_t =
{
    dv_name: id_t;
    dv_typ: typ_t;
    dv_flags: val_flags_t;
    dv_scope: scope_t list;
    dv_loc: loc_t
}

type deffun_t =
{
    df_name: id_t; df_templ_args: id_t list;
    df_args: pat_t list; df_typ: typ_t; df_body: exp_t;
    df_flags: fun_flags_t; df_scope: scope_t list;
    df_loc: loc_t; df_templ_inst: id_t list ref; df_env: env_t
}

type defexn_t =
{
    dexn_name: id_t; dexn_typ: typ_t;
    dexn_scope: scope_t list; dexn_loc: loc_t
}

type deftyp_t =
{
    dt_name: id_t; dt_templ_args: id_t list;
    dt_typ: typ_t; dt_finalized: bool;
    dt_scope: scope_t list; dt_loc: loc_t
}

/* variants are represented in a seemingly complex but logical way;
   the structure contains the list of variant cases (dvar_cases, a list of (id_t, typ_t) pairs),
   as well as the variant constructors (dvar_ctors). Think of the id_t's in dvar_cases
   as of enumeration elements that represent different "tag" values.
   Whereas the constructors will eventually (in the final compiled code)
   become real functions that take the case arguments on input and return the variant instance.

   If the variant is generic, then it may have some instances,
   which are listed in dvar_templ_inst.
   In this case each constructor is also a generic function,
   which contains the corresponding list of instances as well.
   When variant type is instantiated, its instance is added to dvar_templ_list.
   Also, instances of all its constructors are created as well and added
   to the corresponding df_templ_inst' lists of the generic constructors.
*/
type defvariant_t =
{
    dvar_name: id_t; dvar_templ_args: id_t list;
    dvar_alias: typ_t; dvar_flags: var_flags_t;
    dvar_cases: (id_t, typ_t) list;
    dvar_ctors: id_t list; dvar_templ_inst: id_t list ref;
    dvar_ifaces: (id_t, (id_t, id_t) list) list;
    dvar_scope: scope_t list; dvar_loc: loc_t
}

type definterface_t =
{
    di_name: id_t; di_base: id_t;
    di_new_methods: (id_t, typ_t, fun_flags_t) list;
    di_all_methods: (id_t, typ_t, fun_flags_t) list;
    di_scope: scope_t list; di_loc: loc_t
}

type pragmas_t =
{
    pragma_cpp: bool;
    pragma_clibs: (string, loc_t) list
}

type id_info_t =
    | IdNone
    | IdDVal: defval_t
    | IdFun: deffun_t ref
    | IdExn: defexn_t ref
    | IdTyp: deftyp_t ref
    | IdVariant: defvariant_t ref
    | IdInterface: definterface_t ref
    | IdModule: int

type defmodule_t =
{
    dm_name: id_t
    dm_filename: string
    dm_idx: int
    dm_real: bool
    var dm_defs: exp_t list
    var dm_deps: int list
    var dm_env: env_t
    var dm_parsed: bool
    var dm_block_idx: int
    dm_table: id_info_t Dynvec.t
}
fun default_module() =
    defmodule_t {
        dm_name=noid, dm_filename="",
        dm_defs=[], dm_idx=-1,
        dm_deps=[], dm_env=empty_env,
        dm_parsed=false, dm_real=false,
        dm_table=Dynvec.create(0, IdNone),
        dm_block_idx=-1
    }

var freeze_ids = false
var lock_all_names = 0
var all_names = Dynvec.create(0, "")
var all_strhash: (string, int) Hashmap.t = Hashmap.empty(1024, "", -1)
var all_modules_hash: (string, int) Hashmap.t = Hashmap.empty(1024, "", -1)
var all_modules: defmodule_t [] = []
var all_modules_sorted: int list = []
var builtin_exceptions = empty_idmap
var all_compile_errs: exn list = []
var all_compile_err_ctx: string list = []
var all_func_ctx: (id_t, typ_t, loc_t) list = []

fun string(loc: loc_t)
{
    val fname = if loc.m_idx >= 0 {all_modules[loc.m_idx].dm_filename} else {"unknown"}
    f"{fname}:{loc.line0}:{loc.col0}"
}

fun new_id_idx(midx: int) {
    if freeze_ids {
        throw Fail("internal error: attempt to add new AST id during K-phase or C code generation phase")
    }
    all_modules[midx].dm_table.push()
}

fun dump_id(i: id_t) {
    | {m=0, i} => f"name({i})"
    | {m, i, j} => f"id({m}, {i}, {j})"
}

fun id2str_(i: id_t, pp: bool): string =
    if i == noid { "<noid>" }
    else {
        val prefix = all_names.data[i.i]
        if pp || i.m == 0 { prefix }
        else { f"{prefix}@{i.j}" }
    }

fun id2str_m(i: id_t): string =
    if i == noid { "<noid>" }
    else {
        val prefix = all_names.data[i.i]
        if i.m == 0 { prefix }
        else {
            val mprefix = pp(get_module_name(i.m))
            val mprefix = if mprefix == "Builtins" {""} else {mprefix + "."}
            val prefix = all_names.data[i.i]
            f"{mprefix}{prefix}@{i.j}"
        }
    }

fun string(i: id_t): string = id2str_(i, false)
fun pp(i: id_t): string = id2str_(i, true)

fun compile_err(loc: loc_t, msg: string) {
    val whole_msg = f"{loc}: error: {msg}"
    val whole_msg = match all_compile_err_ctx {
        | [] => whole_msg
        | ctx => "\n\t".join(whole_msg :: ctx)
        }
    CompileError(loc, whole_msg)
}

fun compile_warning(loc: loc_t, msg: string) {
    val whole_msg = f"{loc}: warning: {msg}"
    println(whole_msg)
}

fun push_compile_err(err: exn) { all_compile_errs = err :: all_compile_errs }

fun check_compile_errs() =
    match all_compile_errs {
        | err :: _ => throw PropagateCompileError
        | _ => {}
    }

fun print_compile_err(err: exn) {
    | CompileError(loc, msg) => println(msg)
    | Fail(msg) => println(f"Failure: {msg}")
    | _ => println("\n\nException {err} occured")
}

fun pr_verbose(str: string): void =
    if Options.opt.verbose {
        val eol = if str.endswith("\n") {""} else {"\n"}
        print(f"{str}{eol}")
        File.stdout.flush()
    }

fun id2idx_(id: id_t, loc: loc_t) =
    match id {
    | {m=0, i} => throw compile_err(loc,
        f"attempt to query information about unresolved '{pp(id)}'")
    | {m, j} => (m, j)
    }

fun id2idx(i: id_t) = id2idx_(i, noloc)
fun id_info(i: id_t, loc: loc_t) =
    if i.m == 0 {IdNone}
    else {
        val (m, j) = id2idx_(i, loc)
        all_modules[m].dm_table.data[j]
    }

fun is_unique_id(i: id_t) = i.m > 0

fun get_id_prefix(s: string): int
{
    val h_idx = all_strhash.find_idx_or_insert(s)
    val idx = all_strhash.table[h_idx].data
    if idx >= 0 { idx }
    else if lock_all_names == 0 {
        val idx = all_names.push(s)
        all_strhash.table[h_idx].data = idx
        idx
    } else {
        throw compile_err(noloc, "'all_names' are locked. Attempt to call get_id()")
    }
}

fun get_id(s: string): id_t =
    id_t {m=0, i=get_id_prefix(s), j=0}
fun gen_id(m_idx: int, s: string) =
    id_t {m=m_idx, i=get_id_prefix(s), j=new_id_idx(m_idx)}

fun dup_id(m_idx: int, old_id: id_t)
{
    val j = new_id_idx(m_idx)
    id_t {m=m_idx, i=old_id.i, j=j}
}

fun get_orig_id(i: id_t) = id_t {m=0, i=i.i, j=0}

fun set_id_entry(i: id_t, n: id_info_t)
{
    val loc = get_idinfo_loc(n)
    val (m_idx, idx) = id2idx_(i, loc)
    all_modules[m_idx].dm_table.data[idx] = n
}

fun get_exp_ctx(e: exp_t)
{
    | ExpNop(l) => (TypVoid, l)
    | ExpBreak(_, l) => (TypVoid, l)
    | ExpContinue(l) => (TypVoid, l)
    | ExpReturn(_, l) => (TypVoid, l)
    | ExpRange(_, _, _, c) => c
    | ExpLit(_, c) => c
    | ExpIdent(_, c) => c
    | ExpBinary(_, _, _, c) => c
    | ExpUnary(_, _, c) => c
    | ExpIntrin(_, _, c) => c
    | ExpSync(_, e) => get_exp_ctx(e)
    | ExpSeq(_, c) => c
    | ExpMkTuple(_, c) => c
    | ExpMkRecord(_, _, c) => c
    | ExpMkArray(_, c) => c
    | ExpMkVector(_, c) => c
    | ExpUpdateRecord(_, _, c) => c
    | ExpCall(_, _, c) => c
    | ExpAt(_, _, _, _, c) => c
    | ExpAssign(_, _, l) => (TypVoid, l)
    | ExpMem(_, _, c) => c
    | ExpThrow(_, l) => (TypErr, l)
    | ExpIf(_, _, _, c) => c
    | ExpWhile(_, _, l) => (TypVoid, l)
    | ExpDoWhile(_, _, l) => (TypVoid, l)
    | ExpFor(_, _, _, _, l) => (TypVoid, l)
    | ExpMap(_, _, _, c) => c
    | ExpTryCatch(_, _, c) => c
    | ExpMatch(_, _, c) => c
    | ExpCast(_, _, c) => c
    | ExpTyped(_, _, c) => c
    | ExpCCode(_, c) => c
    | ExpData(_, _, c) => c
    | DefVal(_, _, _, dv_loc) => (TypDecl, dv_loc)
    | DefFun (ref {df_loc}) => (TypDecl, df_loc)
    | DefExn (ref {dexn_loc}) => (TypDecl, dexn_loc)
    | DefTyp (ref {dt_loc}) => (TypDecl, dt_loc)
    | DefVariant (ref {dvar_loc}) => (TypDecl, dvar_loc)
    | DefInterface (ref {di_loc}) => (TypDecl, di_loc)
    | DirImport(_, l) => (TypDecl, l)
    | DirImportFrom(_, _, l) => (TypDecl, l)
    | DirPragma(_, l) => (TypDecl, l)
}

fun get_exp_typ(e: exp_t) = get_exp_ctx(e).0
fun get_exp_loc(e: exp_t) = get_exp_ctx(e).1

fun get_pat_loc(p: pat_t) {
    | PatAny(l) => l
    | PatLit(_, l) => l
    | PatIdent(_, l) => l
    | PatTuple(_, l) => l
    | PatVariant(_, _, l) => l
    | PatRecord(_, _, l) => l
    | PatCons(_, _, l) => l
    | PatAs(_, _, l) => l
    | PatTyped(_, _, l) => l
    | PatRef(_, l) => l
    | PatWhen(_, _, l) => l
    | PatAlt(_, l) => l
}

fun pat_skip_typed(p: pat_t) {
    | PatTyped(p, _, _) => pat_skip_typed(p)
    | _ => p
}

fun get_module(m: int): defmodule_t = all_modules[m]
fun get_module(m_id: id_t, loc: loc_t): defmodule_t =
    match id_info(m_id, loc) {
    | IdModule m_idx => get_module(m_idx)
    | _ => throw compile_err(loc, f"identifier '{pp(m_id)}' is not a module")
    }
fun get_module_name(m: int): id_t = all_modules[m].dm_name
fun get_module_env(m: int): env_t = all_modules[m].dm_env

fun find_module(mname: id_t, mfname: string) =
    match all_modules_hash.find_opt(mfname) {
    | Some(m_idx) => m_idx
    | _ =>
        val m_idx = size(all_modules)
        val newmodule = defmodule_t {
            dm_name=mname, dm_filename=mfname,
            dm_idx=m_idx, dm_defs=[], dm_deps=[],
            dm_env=empty_env, dm_parsed=false, dm_real=true,
            dm_table=Dynvec.create(0, IdNone),
            dm_block_idx=-1
        }
        val saved_modules = all_modules
        all_modules =
            [| for i <- 0:m_idx+1 {
                if i < m_idx { saved_modules[i] } else { newmodule }
            } |]
        all_modules_hash.add(mfname, m_idx)
        m_idx
    }

fun new_block_idx(m_idx: int)
{
    val new_block_idx = all_modules[m_idx].dm_block_idx + 1
    all_modules[m_idx].dm_block_idx = new_block_idx
    new_block_idx
}

fun new_block_scope(m_idx: int) = ScBlock(new_block_idx(m_idx))
fun new_loop_scope(m_idx: int, nested: bool) = ScLoop(nested, new_block_idx(m_idx))
fun new_map_scope(m_idx: int) = ScMap(new_block_idx(m_idx))
fun new_arr_map_scope(m_idx: int) = ScArrMap(new_block_idx(m_idx))
fun new_fold_scope(m_idx: int) = ScFold(new_block_idx(m_idx))
fun new_try_scope(m_idx: int) = ScTry(new_block_idx(m_idx))

fun scope2str(sc: scope_t list) {
    | scj :: rest =>
        val prefix = match scj {
            | ScBlock(b) => f"block({b})"
            | ScLoop(f, b) =>
                val nested = if f {"nested_"} else {""}
                f"{nested}loop_block({b})"
            | ScArrMap(b) => f"arr_map_block({b})"
            | ScMap(b) => f"map_block({b})"
            | ScFold(b) => f"fold_block({b})"
            | ScTry(b) => f"try_block({b})"
            | ScFun(f) => f"fun({f})"
            | ScClass(c) => f"class({c})"
            | ScInterface(i) => f"interface({i})"
            | ScModule(m) => f"mod({m})"
        }
        match rest {
        | _ :: _ => prefix + "." + scope2str(rest)
        | _ => prefix
        }
    | _ => ""
}

fun get_module_scope(sc: scope_t list)
{
    | ScModule _ :: _ => sc
    | _ :: rest => get_module_scope(rest)
    | _ => []
}

fun curr_module(sc: scope_t list)
{
    | ScModule(m) :: _ => m
    | _ :: rest => curr_module(rest)
    | _ => -1
}

fun is_global_scope(sc: scope_t list)
{
    | ScModule _ :: _ => true
    | [] => true
    | _ => false
}

fun get_qualified_name(name: string, sc: scope_t list) =
    match sc {
    | (ScModule(m) :: _) when pp(get_module_name(m)) == "Builtins" => name
    | ScModule(m) :: r => get_qualified_name(pp(get_module_name(m)) + "." + name, r)
    | [] => name
    | sc_top :: r => get_qualified_name(name, r)
    }

// out of 'A.B.C.D' we leave just 'D'. just 'D' stays 'D'
fun get_bare_name(n: id_t): id_t
{
    val n_str = pp(n)
    val dot_pos = n_str.rfind('.')
    get_id(if dot_pos < 0 {n_str} else {n_str[dot_pos+1:]})
}

fun get_scope(id_info: id_info_t) {
    | IdNone => []
    | IdDVal ({dv_scope}) => dv_scope
    | IdFun (ref {df_scope}) => df_scope
    | IdExn (ref {dexn_scope}) => dexn_scope
    | IdTyp (ref {dt_scope}) => dt_scope
    | IdVariant (ref {dvar_scope}) => dvar_scope
    | IdInterface (ref {di_scope}) => di_scope
    | IdModule _ => []
    }

fun get_idinfo_loc(id_info: id_info_t) {
    | IdNone | IdModule _ => noloc
    | IdDVal ({dv_loc}) => dv_loc
    | IdFun (ref {df_loc}) => df_loc
    | IdExn (ref {dexn_loc}) => dexn_loc
    | IdTyp (ref {dt_loc}) => dt_loc
    | IdVariant (ref {dvar_loc}) => dvar_loc
    | IdInterface (ref {di_loc}) => di_loc
    | _ => noloc
    }

fun get_idinfo_typ(id_info: id_info_t, loc: loc_t): typ_t =
    match id_info {
    | IdModule _ => TypModule
    | IdDVal ({dv_typ}) => dv_typ
    | IdFun (ref {df_typ}) => df_typ
    | IdExn (ref {dexn_typ}) => dexn_typ
    | IdTyp (ref {dt_typ}) => dt_typ
    | IdVariant (ref {dvar_alias}) => dvar_alias
    | IdInterface (ref {di_name}) => TypApp([], di_name)
    | IdNone => throw compile_err(loc, "ast: attempt to request type of non-existing symbol")
    }

fun get_idinfo_private_flag(id_info: id_info_t) {
    | IdNone => true
    | IdDVal ({dv_flags}) =>
        dv_flags.val_flag_private ||
        dv_flags.val_flag_temp ||
        dv_flags.val_flag_tempref
    | IdFun (ref {df_flags}) => df_flags.fun_flag_private
    | IdExn _ => false
    | IdTyp _ => false
    | IdVariant _ => false
    | IdInterface _ => false
    | IdModule _ => true
    }

fun get_id_typ(i: id_t, loc: loc_t) =
    if i.m == 0 {make_new_typ()}
    else {get_idinfo_typ(id_info(i, loc), loc)}

fun get_lit_typ(l: lit_t) {
    | LitInt _ => TypInt
    | LitSInt(b, _) => TypSInt(b)
    | LitUInt(b, _) => TypUInt(b)
    | LitFloat(b, _) => TypFloat(b)
    | LitString _ => TypString
    | LitChar _ => TypChar
    | LitBool _ => TypBool
    | LitEmpty => make_new_typ()
    | LitNull => TypCPointer
}

/* shorten type specification by redirecting the references in TypVar
   to the "root" of each connected component tree - a cluster of equivalent/unified types.
   In other words, if we had
   t -> t2 -> t3 ... -> root
   before the call, after the call we will have
   t -> root, t2 -> root, t3 -> root, ...
   Returns the root. */
fun deref_typ(t: typ_t): typ_t {
    | TypVar _ =>
        fun find_root(t: typ_t) {
            | TypVar (ref Some(TypVarArray _)) => t
            | TypVar (ref Some(TypVarTuple _)) => t
            | TypVar (ref Some(TypVarRecord)) => t
            | TypVar (ref Some(t2)) => find_root(t2)
            | _ => t
        }

        fun update_refs(t: typ_t, root: typ_t): typ_t =
            match t {
            | TypVar((ref Some(TypVar(ref Some _) as t1)) as r) =>
                *r = Some(root); update_refs(t1, root)
            | _ => root
            }
        update_refs(t, find_root(t))
    | _ => t
}

fun is_typ_scalar(t: typ_t): bool {
    | TypInt | TypSInt _ | TypUInt _ | TypFloat _ | TypBool | TypChar => true
    | TypVar (ref Some(t)) => is_typ_scalar(t)
    | _ => false
}

fun get_numeric_typ_size(t: typ_t, allow_tuples: bool): int =
    match deref_typ(t) {
    | TypInt => 8 // assume 64 bits for simplicity
    | TypSInt b => b/8
    | TypUInt b => b/8
    | TypFloat b => b/8
    | TypBool => 1
    | TypChar => 4
    | TypVar (ref Some(t)) => get_numeric_typ_size(t, allow_tuples)
    | TypTuple(tl) =>
        if !allow_tuples {-1}
        else {
            fold sz=0 for t<-tl {
                val szj = get_numeric_typ_size(t, true)
                if szj < 0 || sz < 0 {-1} else {sz + szj}
            }
        }
    | _ => -1
    }

fun string(fm: for_make_t)
{
    | ForMakeNone => "ForMakeNone"
    | ForMakeArray => "ForMakeArray"
    | ForMakeList => "ForMakeList"
    | ForMakeTuple => "ForMakeTuple"
    | _ => "ForMake???"
}

fun string(c: cmpop_t) {
    | CmpEQ => "=="
    | CmpNE => "!="
    | CmpLT => "<"
    | CmpLE => "<="
    | CmpGE => ">="
    | CmpGT => ">"
}

fun string(bop: binary_t) {
    | OpAdd => "+"
    | OpSub => "-"
    | OpMul => "*"
    | OpDiv => "/"
    | OpRDiv => "\\"
    | OpMod => "%"
    | OpPow => "**"
    | OpDotAdd => ".+"
    | OpDotSub => ".-"
    | OpDotMul => ".*"
    | OpDotDiv => "./"
    | OpDotMod => ".%"
    | OpDotPow => ".**"
    | OpShiftLeft => "<<"
    | OpShiftRight => ">>"
    | OpBitwiseAnd => "&"
    | OpLogicAnd => "&&"
    | OpBitwiseOr => "|"
    | OpLogicOr => "||"
    | OpBitwiseXor => "^"
    | OpSpaceship => "<=>"
    | OpDotSpaceship => ".<=>"
    | OpSame => "==="
    | OpCmp(c) => string(c)
    | OpDotCmp(c) => "."+string(c)
    | OpCons => "::"
}

fun string(uop: unary_t) {
    | OpPlus => "+"
    | OpNegate => "-"
    | OpDotMinus => ".-"
    | OpBitwiseNot => "~"
    | OpLogicNot => "!"
    | OpExpand => "\\"
    | OpMkRef => "REF"
    | OpDeref => "*"
    | OpApos => "'"
}

fun string(iop: intrin_t): string
{
    | IntrinPopExn => "__intrin_pop_exn__"
    | IntrinVariantTag => "__intrin_variant_tag__"
    | IntrinVariantCase => "__intrin_variant_case__"
    | IntrinQueryIface => "__intrin_query_iface__"
    | IntrinGetObject => "__intrin_get_object__"
    | IntrinListHead => "__intrin_hd__"
    | IntrinListTail => "__intrin_tl__"
    | IntrinStrConcat => "__intrin_str_concat__"
    | IntrinGetSize => "__intrin_size__"
    | IntrinCheckIdx => "__intrin_check_idx__"
    | IntrinCheckIdxRange => "__intrin_check_range__"
    | IntrinMakeFPbyFCV => "__intrin_make_fp_by_fcv__"
    | IntrinMath(f) => f"__intrin_{pp(f)}__"
}

fun border2str(border: border_t, f: bool) {
    val pt = if f {"."} else {""}
    match border {
    | BorderNone => ""
    | BorderClip => pt + ".clip"
    | BorderWrap => pt + ".wrap"
    | BorderZero => pt + ".zero"
    }
}

fun interp2str(interp: interpolate_t, f: bool) {
    val pt = if f {"."} else {""}
    match interp {
    | InterpNone => ""
    | InterpLinear => pt + "linear"
    }
}

fun fname_op_apos() = get_id("__apos__")
fun fname_op_add() = get_id("__add__")
fun fname_op_sub() = get_id("__sub__")
fun fname_op_mul() = get_id("__mul__")
fun fname_op_div() = get_id("__div__")
fun fname_op_rdiv() = get_id("__rdiv__")
fun fname_op_mod() = get_id("__mod__")
fun fname_op_pow() = get_id("__pow__")
fun fname_op_dot_add() = get_id("__dot_add__")
fun fname_op_dot_sub() = get_id("__dot_sub__")
fun fname_op_dot_mul() = get_id("__dot_mul__")
fun fname_op_dot_div() = get_id("__dot_div__")
fun fname_op_dot_mod() = get_id("__dot_mod__")
fun fname_op_dot_pow() = get_id("__dot_pow__")
fun fname_op_shl() = get_id("__shl__")
fun fname_op_shr() = get_id("__shr__")
fun fname_op_bit_and() = get_id("__bit_and__")
fun fname_op_bit_or() = get_id("__bit_or__")
fun fname_op_bit_xor() = get_id("__bit_xor__")
fun fname_op_cmp() = get_id("__cmp__")
fun fname_op_eq() = get_id("__eq__")
fun fname_op_ne() = get_id("__ne__")
fun fname_op_lt() = get_id("__lt__")
fun fname_op_gt() = get_id("__gt__")
fun fname_op_le() = get_id("__le__")
fun fname_op_ge() = get_id("__ge__")
fun fname_op_dot_cmp() = get_id("__dot_cmp__")
fun fname_op_dot_eq() = get_id("__dot_eq__")
fun fname_op_dot_ne() = get_id("__dot_ne__")
fun fname_op_dot_lt() = get_id("__dot_lt__")
fun fname_op_dot_gt() = get_id("__dot_gt__")
fun fname_op_dot_le() = get_id("__dot_le__")
fun fname_op_dot_ge() = get_id("__dot_ge__")
fun fname_op_same() = get_id("__same__")
fun fname_op_plus() = get_id("__plus__")
fun fname_op_negate() = get_id("__negate__")
fun fname_op_dot_minus() = get_id("__dot_minus__")
fun fname_op_bit_not() = get_id("__bit_not__")
fun fname_to_int() = get_id("int")
fun fname_to_uint8() = get_id("uint8")
fun fname_to_int8() = get_id("int8")
fun fname_to_uint16() = get_id("uint16")
fun fname_to_int16() = get_id("int16")
fun fname_to_uint32() = get_id("uint32")
fun fname_to_int32() = get_id("int32")
fun fname_to_uint64() = get_id("uint64")
fun fname_to_int64() = get_id("int64")
fun fname_to_float() = get_id("float")
fun fname_to_double() = get_id("double")
fun fname_to_bool() = get_id("bool")
fun fname_string() = get_id("string")
fun fname_print() = get_id("print")
fun fname_repr() = get_id("repr")
fun fname_hash() = get_id("hash")

fun binary_try_remove_dot(bop: binary_t) =
    match bop {
    | OpDotMul => OpMul
    | OpDotDiv => OpDiv
    | OpDotMod => OpMod
    | OpDotPow => OpPow
    | OpDotSpaceship => OpSpaceship
    | OpDotCmp(c) => OpCmp(c)
    | _ => bop
    }

fun get_binary_fname(bop: binary_t, loc: loc_t) =
    match bop {
    | OpAdd => fname_op_add()
    | OpSub => fname_op_sub()
    | OpMul => fname_op_mul()
    | OpDiv => fname_op_div()
    | OpRDiv => fname_op_rdiv()
    | OpMod => fname_op_mod()
    | OpPow => fname_op_pow()
    | OpDotAdd => fname_op_dot_add()
    | OpDotSub => fname_op_dot_sub()
    | OpDotMul => fname_op_dot_mul()
    | OpDotDiv => fname_op_dot_div()
    | OpDotMod => fname_op_dot_mod()
    | OpDotPow => fname_op_dot_pow()
    | OpShiftLeft => fname_op_shl()
    | OpShiftRight => fname_op_shr()
    | OpBitwiseAnd => fname_op_bit_and()
    | OpBitwiseOr => fname_op_bit_or()
    | OpBitwiseXor => fname_op_bit_xor()
    | OpSpaceship => fname_op_cmp()
    | OpSame => fname_op_same()
    | OpCmp(CmpEQ) => fname_op_eq()
    | OpCmp(CmpNE) => fname_op_ne()
    | OpCmp(CmpLT) => fname_op_lt()
    | OpCmp(CmpLE) => fname_op_le()
    | OpCmp(CmpGE) => fname_op_ge()
    | OpCmp(CmpGT) => fname_op_gt()
    | OpDotSpaceship => fname_op_dot_cmp()
    | OpDotCmp(CmpEQ) => fname_op_dot_eq()
    | OpDotCmp(CmpNE) => fname_op_dot_ne()
    | OpDotCmp(CmpLT) => fname_op_dot_lt()
    | OpDotCmp(CmpLE) => fname_op_dot_le()
    | OpDotCmp(CmpGE) => fname_op_dot_ge()
    | OpDotCmp(CmpGT) => fname_op_dot_gt()
    | OpLogicAnd | OpLogicOr | OpCons =>
        throw compile_err(loc,
            f"for binary operation \"{bop}\" there is no corresponding function")
    }

fun get_unary_fname(uop: unary_t, loc: loc_t) =
    match uop {
    | OpApos => fname_op_apos()
    | OpPlus => fname_op_plus()
    | OpNegate => fname_op_negate()
    | OpDotMinus => fname_op_dot_minus()
    | OpBitwiseNot => fname_op_bit_not()
    | OpLogicNot | OpExpand | OpMkRef | OpDeref =>
        throw compile_err(loc,
            f"for unary operation \"{uop}\" there is no corresponding function")
    }

fun fname_always_import(): id_t list = [
    fname_op_add(), fname_op_sub(), fname_op_mul(), fname_op_div(), fname_op_rdiv(),
    fname_op_mod(), fname_op_pow(), fname_op_dot_add(), fname_op_dot_sub(),
    fname_op_dot_mul(), fname_op_dot_div(), fname_op_dot_mod(), fname_op_dot_pow(),
    fname_op_shl(), fname_op_shr(), fname_op_bit_and(), fname_op_bit_or(), fname_op_bit_xor(),
    fname_op_cmp(), fname_op_dot_cmp(), fname_op_same(), fname_op_eq(), fname_op_ne(),
    fname_op_le(), fname_op_ge(), fname_op_lt(), fname_op_gt(),
    fname_op_dot_eq(), fname_op_dot_ne(), fname_op_dot_le(),
    fname_op_dot_ge(), fname_op_dot_lt(), fname_op_dot_gt(),
    fname_op_plus(), fname_op_negate(), fname_op_dot_minus(),
    fname_op_bit_not(), fname_op_apos(),
    fname_to_int(), fname_to_uint8(), fname_to_int8(), fname_to_uint16(),
    fname_to_int16(), fname_to_uint32(), fname_to_int32(), fname_to_uint64(),
    fname_to_int64(), fname_to_float(), fname_to_double(), fname_to_bool(),
    fname_string(), fname_print(), fname_repr(), fname_hash()
]

fun get_cast_fname(t: typ_t, loc: loc_t) =
    match deref_typ(t) {
    | TypInt => fname_to_int()
    | TypSInt(8) => fname_to_int8()
    | TypSInt(16) => fname_to_int16()
    | TypSInt(32) => fname_to_int32()
    | TypSInt(64) => fname_to_int64()
    | TypUInt(8) => fname_to_uint8()
    | TypUInt(16) => fname_to_uint16()
    | TypUInt(32) => fname_to_uint32()
    | TypUInt(64) => fname_to_uint64()
    | TypFloat(32) => fname_to_float()
    | TypFloat(64) => fname_to_double()
    | TypBool => fname_to_bool()
    | TypString => fname_string()
    | _ => throw CompileError(loc, f"for type '{typ2str(t)}' there is no corresponding cast function")
    }

val reserved_keywords = Set.from_list(String.cmp, ["fx_result", "fx_status", "fx_fv"])

fun get_builtin_exception(n0: id_t, loc: loc_t) =
    match builtin_exceptions.find_opt(n0) {
    | Some(n) => n
    | _ => throw compile_err(loc, f"cannot find built-in exception '{n0}'")
    }

fun is_constructor(flags: fun_flags_t) =
    match flags.fun_flag_ctor {
    | CtorNone => false
    | _ => true
    }

fun ctor2str(f: fun_constr_t) {
    | CtorNone => "not_a_constructor"
    | CtorStruct => "Constructor(record_or_tuple)"
    | CtorVariant(i) => f"Constructor(variant({i}))"
    | CtorFP(i) => f"Constructor(fp({i}))"
    | CtorExn(i) => f"Constructor(exn({i}))"
}

fun lit2str(c: lit_t) {
    fun add_dot(s: string, suffix: string) =
        if s.contains('.') || s.contains('e') { s+suffix }
        else { s + "." + suffix }

    match c {
    | LitInt(v) => string(v)
    | LitSInt(64, v) => f"{v}L"
    | LitUInt(64, v) => f"{v}UL"
    | LitSInt(b, v) => f"{v}i{b}"
    | LitUInt(b, v) => f"{v}u{b}"
    | LitFloat(16, v) => add_dot(f"{float(v)}", "h")
    | LitFloat(32, v) => add_dot(f"{float(v)}", "f")
    | LitFloat(_, v) => add_dot(string(v), "")
    | LitString(s) => s.escaped(quotes=true)
    | LitChar(c) => "'" + string(c).escaped(quotes=false) + "'"
    | LitBool(true) => "true"
    | LitBool(false) => "false"
    | LitEmpty => "[]"
    | LitNull => "null"
    }
}

fun ref2str(r: 't ref): string = @ccode
{
    char buf[32];
    sprintf(buf, "%p", r);
    return fx_cstr2str(buf, -1, fx_result);
}

fun typ2str(t: typ_t): string {
    | TypVarTuple(Some(t)) => f"({typ2str(t)} ...)"
    | TypVarTuple _ => "(...)"
    | TypVarArray(t) => f"{typ2str(t)} [+]"
    | TypVarRecord => "{...}"
    | TypVar ((ref Some(t)) as r) => f"{typ2str(t)}"
    | TypVar (r) => "<unknown>"
    | TypApp([], i) => string(i)
    | TypApp(tl, i) => f"{tl2str(tl)} {i}"
    | TypInt => "int"
    | TypSInt(n) => f"int{n}"
    | TypUInt(n) => f"uint{n}"
    | TypFloat(n) =>
        match n {
        | 16 => "half"
        | 32 => "float"
        | 64 => "double"
        | _ => throw compile_err(noloc, f"bad floating-point type TypFloat({n})")
        }
    | TypVoid => "void"
    | TypBool => "bool"
    | TypChar => "char"
    | TypString => "string"
    | TypCPointer => "cptr"
    | TypFun(argtyps, rt) => f"({tl2str(argtyps)} -> {typ2str(rt)})"
    | TypTuple(tl) =>
        val s = tl2str(tl)
        match tl {
        | x :: [] => "(" + s + ",)"
        | _ => s
        }
    | TypRecord (ref (relems, _)) =>
        join_embrace("{", "}", "; ",
            [| for (flags, i, t, _) <- relems {
                val prefix = if flags.val_flag_mutable { "var " } else {""}
                f"{prefix}{i}: {typ2str(t)}"
            } |])
    | TypArray(d, t) => f"{typ2str(t)} [{','*(d-1)}]"
    | TypList(t) => f"{typ2str(t)} list"
    | TypVector(t) => f"{typ2str(t)} vector"
    | TypRef(t) => f"{typ2str(t)} ref"
    | TypExn => "exn"
    | TypErr => "<err>"
    | TypDecl => "<decl>"
    | TypModule => "<module>"
}

fun tl2str(tl: typ_t list): string {
    val (begin, end) = match tl {
    | x :: [] => ("", "")
    | _ => ("(", ")")
    }
    join_embrace(begin, end, ", ", [| for t <- tl { typ2str(t) } |])
}

fun parse_pragmas(prl: (string, loc_t) list): pragmas_t {
    fun parse(prl: (string, loc_t) list, result: pragmas_t) =
        match prl {
        | (pr, loc) :: rest =>
            if pr == "c++" || pr == "C++" {
                parse(rest, result.{pragma_cpp=true})
            } else if pr.startswith("clib") {
                val p = pr.find(":")
                val libname = if p >= 0 { pr[p+1:].strip() } else {
                    throw compile_err(loc, f"invalid format of clib pragma \"{pr}\", it should be \"clib: <libname>\"")
                }
                parse(rest, result.{pragma_clibs=(libname, loc) :: result.pragma_clibs})
            } else {
                throw compile_err(loc, f"unrecognized pragma \"{pr}\"")
            }
        | [] => result
        }

    parse(prl, pragmas_t {pragma_cpp=false, pragma_clibs=[]})
}

type ast_callb_t =
{
    ast_cb_typ: ((typ_t, ast_callb_t) -> typ_t)?;
    ast_cb_exp: ((exp_t, ast_callb_t) -> exp_t)?;
    ast_cb_pat: ((pat_t, ast_callb_t) -> pat_t)?
}

fun check_n_walk_typ(t: typ_t, callb: ast_callb_t) =
    match callb.ast_cb_typ {
    | Some(f) => f(t, callb)
    | _ => walk_typ(t, callb)
    }
fun check_n_walk_tlist(tlist: typ_t list, callb: ast_callb_t) =
    [for t <- tlist {check_n_walk_typ(t, callb)}]
fun check_n_walk_exp(e: exp_t, callb: ast_callb_t) =
    match callb.ast_cb_exp {
    | Some(f) => f(e, callb)
    | _ => walk_exp(e, callb)
    }
fun check_n_walk_elist(elist: exp_t list, callb: ast_callb_t): exp_t list =
    [for e <- elist { check_n_walk_exp(e, callb) }]
fun check_n_walk_pat(p: pat_t, callb: ast_callb_t) =
    match callb.ast_cb_pat {
    | Some(f) => f(p, callb)
    | _ => walk_pat(p, callb)
    }
fun check_n_walk_plist(plist: pat_t list, callb: ast_callb_t) =
    [for p <- plist { check_n_walk_pat(p, callb) }]

fun walk_typ(t: typ_t, callb: ast_callb_t) =
    match t {
    | TypVar(r) => match *r { | Some(t) => *r = Some(check_n_walk_typ(t, callb)) | _ => {} }; t
    | TypInt => t
    | TypSInt _ => t
    | TypUInt _ => t
    | TypFloat _ => t
    | TypString => t
    | TypChar => t
    | TypBool => t
    | TypVoid => t
    | TypFun(args, rt) => TypFun(check_n_walk_tlist(args, callb), check_n_walk_typ(rt, callb))
    | TypList(t) => TypList(check_n_walk_typ(t, callb))
    | TypVector(t) => TypVector(check_n_walk_typ(t, callb))
    | TypTuple(tl) => TypTuple(check_n_walk_tlist(tl, callb))
    | TypVarTuple(t_opt) =>
        TypVarTuple(match t_opt { | Some(t) => Some(check_n_walk_typ(t, callb)) | _ => None})
    | TypRef(t) => TypRef(check_n_walk_typ(t, callb))
    | TypArray(d, et) => TypArray(d, check_n_walk_typ(et, callb))
    | TypVarArray(et) => TypVarArray(check_n_walk_typ(et, callb))
    | TypVarRecord => t
    | TypRecord((ref (relems, ordered)) as r) =>
        val new_relems = [ for (flags, n, t, v) <- relems {
            (flags, n, check_n_walk_typ(t, callb), check_n_walk_exp(v, callb))} ]
        *r = (new_relems, ordered)
        t
    | TypExn => t
    | TypErr => t
    | TypCPointer => t
    | TypApp(ty_args, n) => TypApp(check_n_walk_tlist(ty_args, callb), n)
    | TypDecl => t
    | TypModule => t
    }

fun walk_exp(e: exp_t, callb: ast_callb_t) {
    fun walk_typ_(t: typ_t) = check_n_walk_typ(t, callb)
    fun walk_exp_(e: exp_t) = check_n_walk_exp(e, callb)
    fun walk_elist_(el: exp_t list) = check_n_walk_elist(el, callb)
    fun walk_pat_(p: pat_t) = check_n_walk_pat(p, callb)
    fun walk_plist_(pl: pat_t list) = check_n_walk_plist(pl, callb)
    fun walk_pe_l_(pe_l: (pat_t, exp_t) list) = [for (p, e) <- pe_l { (walk_pat_(p), walk_exp_(e)) }]
    fun walk_ne_l_(ne_l: (id_t, exp_t) list) = [for (n, e) <- ne_l { (n, walk_exp_(e)) }]
    fun walk_cases_(pe_l: (pat_t, exp_t) list) = [for (p, e) <- pe_l { (walk_pat_(p), walk_exp_(e)) }]
    fun walk_exp_opt_(e_opt: exp_t?) {
        | Some(e) => Some(walk_exp_(e))
        | _ => None
    }
    fun walk_ctx_((t, loc): ctx_t) = (walk_typ_(t), loc)

    match e {
    | ExpNop _ => e
    | ExpBreak(_, _) => e
    | ExpContinue _ => e
    | ExpReturn(e_opt, loc) => ExpReturn(walk_exp_opt_(e_opt), loc)
    | ExpRange(e1_opt, e2_opt, e3_opt, ctx) => ExpRange(walk_exp_opt_(e1_opt), walk_exp_opt_(e2_opt), walk_exp_opt_(e3_opt), walk_ctx_(ctx))
    | ExpLit(l, ctx) => ExpLit(l, walk_ctx_(ctx))
    | ExpIdent(n, ctx) => ExpIdent(n, walk_ctx_(ctx))
    | ExpBinary(bop, e1, e2, ctx) => ExpBinary(bop, walk_exp_(e1), walk_exp_(e2), walk_ctx_(ctx))
    | ExpUnary(uop, e, ctx) => ExpUnary(uop, walk_exp_(e), walk_ctx_(ctx))
    | ExpIntrin(iop, args, ctx) => ExpIntrin(iop, walk_elist_(args), walk_ctx_(ctx))
    | ExpSeq(elist, ctx) => ExpSeq(walk_elist_(elist), walk_ctx_(ctx))
    | ExpSync(n, e) => ExpSync(n, walk_exp_(e))
    | ExpMkTuple(elist, ctx) => ExpMkTuple(walk_elist_(elist), walk_ctx_(ctx))
    | ExpMkArray(ell, ctx) => ExpMkArray([for el <- ell {walk_elist_(el)}], walk_ctx_(ctx))
    | ExpMkVector(elist, ctx) => ExpMkVector(walk_elist_(elist), walk_ctx_(ctx))
    | ExpMkRecord(e, ne_l, ctx) => ExpMkRecord(walk_exp_(e), walk_ne_l_(ne_l), walk_ctx_(ctx))
    | ExpUpdateRecord(e, ne_l, ctx) => ExpUpdateRecord(walk_exp_(e), walk_ne_l_(ne_l), walk_ctx_(ctx))
    | ExpCall(f, args, ctx) => ExpCall(walk_exp_(f), walk_elist_(args), walk_ctx_(ctx))
    | ExpAt(arr, border, interp, idx, ctx) => ExpAt(walk_exp_(arr), border, interp, walk_elist_(idx), walk_ctx_(ctx))
    | ExpAssign(lv, rv, loc) => ExpAssign(walk_exp_(lv), walk_exp_(rv), loc)
    | ExpMem(a, member, ctx) => ExpMem(walk_exp_(a), walk_exp_(member), walk_ctx_(ctx))
    | ExpThrow(a, loc) => ExpThrow(walk_exp_(a), loc)
    | ExpIf(c, then_e, else_e, ctx) => ExpIf(walk_exp_(c), walk_exp_(then_e), walk_exp_(else_e), walk_ctx_(ctx))
    | ExpWhile(c, e, loc) => ExpWhile(walk_exp_(c), walk_exp_(e), loc)
    | ExpDoWhile(e, c, loc) => ExpDoWhile(walk_exp_(e), walk_exp_(c), loc)
    | ExpFor(pe_l, idx_pat, body, flags, loc) =>
        ExpFor(walk_pe_l_(pe_l), walk_pat_(idx_pat), walk_exp_(body), flags, loc)
    | ExpMap(pew_ll, body, flags, ctx) =>
        ExpMap([for (pe_l, idx_pat) <- pew_ll { (walk_pe_l_(pe_l), walk_pat_(idx_pat)) }],
            walk_exp_(body), flags, walk_ctx_(ctx))
    | ExpTryCatch(e, cases, ctx) => ExpTryCatch(walk_exp_(e), walk_cases_(cases), walk_ctx_(ctx))
    | ExpMatch(e, cases, ctx) => ExpMatch(walk_exp_(e), walk_cases_(cases), walk_ctx_(ctx))
    | ExpCast(e, t, ctx) => ExpCast(walk_exp_(e), walk_typ_(t), walk_ctx_(ctx))
    | ExpTyped(e, t, ctx) => ExpTyped(walk_exp_(e), walk_typ_(t), walk_ctx_(ctx))
    | ExpCCode(str, ctx) => ExpCCode(str, walk_ctx_(ctx))
    | ExpData(kind, fname, ctx) => ExpData(kind, fname, walk_ctx_(ctx))
    | DefVal(p, v, flags, loc) => DefVal(walk_pat_(p), walk_exp_(v), flags, loc)
    | DefFun(df) =>
        if df->df_templ_args != [] {
            e
        } else {
            val {df_args, df_typ, df_body} = *df
            *df = df->{df_args=walk_plist_(df_args),
                df_typ=walk_typ_(df_typ),
                df_body=walk_exp_(df_body)
            }
            e
        }
    | DefExn(de) =>
        val {dexn_typ} = *de
        *de = de->{dexn_typ=walk_typ_(dexn_typ)}
        e
    | DefTyp(dt) =>
        val {dt_typ} = *dt
        *dt = dt->{dt_typ=walk_typ_(dt_typ)}
        e
    | DefVariant(dvar) =>
        val {dvar_alias, dvar_cases} = *dvar
        *dvar = dvar->{
            dvar_alias=walk_typ_(dvar_alias),
            dvar_cases=[ for (n, t) <- dvar_cases {(n, walk_typ_(t))} ]
        }
        e
    | DefInterface(di) => e
    | DirImport(_, _) => e
    | DirImportFrom(_, _, _) => e
    | DirPragma(_, _) => e
    }
}

fun walk_pat(p: pat_t, callb: ast_callb_t) {
    fun walk_typ_(t: typ_t) = check_n_walk_typ(t, callb)
    fun walk_pat_(p: pat_t) = check_n_walk_pat(p, callb)
    fun walk_pl_(pl: pat_t list) = check_n_walk_plist(pl, callb)

    match p {
    | PatAny _ => p
    | PatLit(_, _) => p
    | PatIdent(_, _) => p
    | PatTuple(pl, loc) => PatTuple(walk_pl_(pl), loc)
    | PatVariant(n, args, loc) => PatVariant(n, walk_pl_(args), loc)
    | PatRecord(n_opt, np_l, loc) => PatRecord(n_opt,
            [for (n, p) <- np_l {(n, walk_pat_(p))}], loc)
    | PatCons(p1, p2, loc) => PatCons(walk_pat_(p1), walk_pat_(p2), loc)
    | PatAs(p, n, loc) => PatAs(walk_pat_(p), n, loc)
    | PatTyped(p, t, loc) => PatTyped(walk_pat_(p), walk_typ_(t), loc)
    | PatWhen(p, e, loc) => PatWhen(walk_pat_(p), check_n_walk_exp(e, callb), loc)
    | PatAlt(pl, loc) => PatAlt(walk_pl_(pl), loc)
    | PatRef(p, loc) => PatRef(walk_pat_(p), loc)
    }
}

fun dup_typ_(t: typ_t, callb: ast_callb_t): typ_t =
    match t {
    | TypVar (ref Some(t1)) => TypVar(ref Some(dup_typ_(t1, callb)))
    | TypVar  _ => TypVar(ref None)
    | TypRecord(r) =>
        val (relems, ordered) = *r
        val new_relems = [for (flags, n, t, v) <- relems {(flags, n, dup_typ_(t, callb), dup_exp_(v, callb))}]
        TypRecord(ref (new_relems, ordered))
    | _ => walk_typ(t, callb)
    }

fun dup_exp_(e: exp_t, callb: ast_callb_t): exp_t =
    match e {
    | DefFun(r) => walk_exp(DefFun(ref *r), callb)
    | DefExn(r) => walk_exp(DefExn(ref *r), callb)
    | DefTyp(r) => walk_exp(DefTyp(ref *r), callb)
    | DefVariant(r) => walk_exp(DefVariant(ref *r), callb)
    | DefInterface(r) => walk_exp(DefInterface(ref *r), callb)
    | _ => walk_exp(e, callb)
    }

val dup_callb = ast_callb_t {ast_cb_typ=Some(dup_typ_), ast_cb_exp=Some(dup_exp_), ast_cb_pat=None}
fun dup_typ(t: typ_t) = dup_typ_(t, dup_callb)
fun dup_exp(e: exp_t) = dup_exp_(e, dup_callb)
fun dup_pat(p: pat_t) = walk_pat(p, dup_callb)

fun deref_typ_rec(t: typ_t)
{
    fun deref_typ_rec_(t: typ_t, callb: ast_callb_t): typ_t {
        val t = deref_typ(t)
        walk_typ(t, callb)
    }
    val deref_callb = ast_callb_t {ast_cb_typ=Some(deref_typ_rec_), ast_cb_exp=None, ast_cb_pat=None}
    deref_typ_rec_(t, deref_callb)
}

fun is_fixed_typ (t: typ_t): bool
{
    fun is_fixed_typ_(t: typ_t,  callb: ast_callb_t) =
        match deref_typ(t) {
        | TypVar (ref None) => throw Break
        | t => walk_typ(t, callb)
        }
    val callb = ast_callb_t
    {
        ast_cb_typ = Some(is_fixed_typ_),
        ast_cb_exp = None,
        ast_cb_pat = None
    }
    try {
        val _ = is_fixed_typ_(t, callb);
        true
    } catch { | Break => false }
}

type idset_hashmap_t = (id_t, id_hashset_t) Hashmap.t

// Compute closure of the sets, i.e.
// for each id we find a set of id's which it references directly on indirectly
fun calc_sets_closure(iters: int, all_ids: id_t list, all_sets: idset_hashmap_t): int
{
    var done_i = -1
    var all_ids = all_ids
    val visited_ids = empty_id_hashset(256)
    val empty_idset = empty_id_hashset(1)

    for iter <- 0:iters {
        var changed = false

        fun update_sets(n: id_t): id_hashset_t =
            match all_sets.find_opt(n) {
            | Some set_n =>
                if visited_ids.mem(n) {
                    set_n
                } else {
                    visited_ids.add(n)
                    val size0 = set_n.size()
                    set_n.app(
                        fun (m) {
                            if m != n {
                                val set_m = update_sets(m)
                                set_n.union(set_m)
                            }
                        })
                    val size1 = set_n.size()
                    if size1 != size0 { changed = true }
                    set_n
                }
            | _ => empty_idset
            }

        for n <- all_ids { ignore(update_sets(n)) }
        if !changed { done_i = iter+1; break }
        all_ids = all_ids.rev()
        visited_ids.clear()
    }
    if done_i <= 0 {
        throw compile_err(noloc,
        "calculation of sets' closures takes too much iterations")
    }
    done_i
}

fun get_iface(iface: id_t, loc: loc_t) =
    match id_info(iface, loc) {
    | IdInterface di => di
    | _ => throw compile_err(loc, f"'{pp(iface)}' is not an interface")
    }

fun same_or_parent(iface: id_t, maybe_parent: id_t, loc: loc_t) =
    if iface == maybe_parent {true} else {
        val di_base = get_iface(iface, loc)->di_base
        if di_base == noid { false }
        else {same_or_parent(di_base, maybe_parent, loc)}
    }

val (dummyid, builtin_ids) = std_id("_", builtin_ids)
val (std__fold_result__, builtin_ids) = std_id("__fold_result__", builtin_ids)
val (std__tag__, builtin_ids) = std_id("__tag__", builtin_ids)
val (std__self__, builtin_ids) = std_id("self", builtin_ids)
val (std__lambda__, builtin_ids) = std_id("__lambda__", builtin_ids)
val (std__kwargs__, builtin_ids) = std_id("__kwargs__", builtin_ids)
val (std__pat__, builtin_ids) = std_id("__pat__", builtin_ids)
val (std__result__, builtin_ids) = std_id("result", builtin_ids)
val (std__size__, builtin_ids) = std_id("size", builtin_ids)
val (std__tmp__, builtin_ids) = std_id("t", builtin_ids)
val (std__Names__, builtin_ids) = std_id("__Names__", builtin_ids)
val (std__Runtime__, builtin_ids) = std_id("__Runtime__", builtin_ids)
val (std__Builtins__, builtin_ids) = std_id("Builtins", builtin_ids)
val (std__List__, builtin_ids) = std_id("List", builtin_ids)
val (std__String__, builtin_ids) = std_id("String", builtin_ids)
val (std__Char__, builtin_ids) = std_id("Char", builtin_ids)
val (std__Array__, builtin_ids) = std_id("Array", builtin_ids)
val (std__Vector__, builtin_ids) = std_id("Vector", builtin_ids)

fun init_all(): void
{
    freeze_ids = false
    all_names.clear()
    all_strhash.clear()
    for i <- builtin_ids.0.rev() { ignore(get_id(i)) }
    ignore(fname_always_import())
    all_modules_hash.clear()
    all_modules = []
    ignore(find_module(std__Names__, "<Names>"))
    ignore(find_module(std__Runtime__, "<Runtime>"))
    all_modules[0].dm_real = false
    all_modules[1].dm_real = false
    all_modules_sorted = []
    builtin_exceptions = Map.empty(cmp_id)
    all_compile_errs = []
    all_compile_err_ctx = []
}
