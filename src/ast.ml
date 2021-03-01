(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* ficus abstract syntax definition + helper structures and functions *)

(*
   we represent all the symbols in the code using Id.t variant, essentially, by a pair of integers <a, b>.
   Initially, b=0 (i.e. id ~ Id.Name(a)) and it represents the textual name as it occurs in the code, e.g. "i".
   "a" is the index in the array of strings that contains identifiers occured in the program.
   Later on, the type checker resolves the names and replaces b with
   some unique integer representing the particular object ("i") referenced in the particular
   context. For example:

   val i = 5
   val i = i + 1

   =>

   val i@100 : int = 5
   val i@101 : int = i@100 : int + 1

   (here i@100 means Id.Val(i, 100) and foo@101 means Id.Val(foo, 357), where i & foo are indices in the global
    table of symbolic names that corresponds to the abstract names "i", "foo", whereas 100 and 357
    are indices in the same table corresponding to the actually defined values.
    Internally the compiler uses unique names i@100 etc. to distinguish between values
    with the same name, but it uses the original names in error messages; it also uses the original name (i)
    to search for the symbol, i.e. Id.Name(f) can be matched with Id.Val(f, 12345) or Id.Val(f, 9876) or
    some other Id.Val(f, ...), depending on the current environment).

   There is a big global table of all symbols, so for each symbol we can retrieve its properties:
   the original name, inferred type, when and how it's defined.

   Sometimes we need a temporary value or a temporary function, i.e. when we do not have a user-specified
   name. In this case we use some common prefix, e.g. "t" for intermediate results in complex expressions,
   "lambda" for anonymous functions etc. but we represent the id as Id.Temp(prefix, N). Such id is
   always displayed as prefix@@N, e.g. "t@@1000" or "lambda@@777", and it can only be matched with
   itself (prefix@@N). That is, it's really unique.
*)

let pair_compare ((i1: int), (j1: int)) ((i2: int), (j2: int)) = compare (i1, j1) (i2, j2)

module Id = struct
    type t = Name of int | Val of int * int | Temp of int * int
    let compare a b =
        let ap = match a with Name(i) -> (-1, i) | Val(_, i) -> (0, i) | Temp(_, i) -> (0, i) in
        let bp = match b with Name(i) -> (-1, i) | Val(_, i) -> (0, i) | Temp(_, i) -> (0, i) in
        pair_compare ap bp
end

type id_t = Id.t
let noid = Id.Name(0)
let dummyid = Id.Name(1)

(*
  Environment (Env.t) is the mapping from id_t to env_entry_t list. It's the key data structure used
  by the type checker.

  That is, for each id key Id.Name(i) (which corresponds to some abstract symbol <i>)
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
*)
module Env = Map.Make(Id)
module IdSet = Set.Make(Id)

(*
   Scope of the definition

*)
type scope_t =
    | ScBlock of int | ScLoop of bool*int | ScFold of int | ScArrMap of int | ScMap of int
    | ScTry of int | ScFun of id_t | ScClass of id_t
    | ScInterface of id_t | ScModule of id_t | ScGlobal

type loc_t = { loc_fname: id_t; loc_line0: int; loc_pos0: int; loc_line1: int; loc_pos1: int }
let noloc = { loc_fname=noid; loc_line0=0; loc_pos0=0; loc_line1=0; loc_pos1=0 }

let loclist2loc llist default_loc =
    List.fold_left (fun loc loci ->
        let { loc_fname; loc_line0; loc_pos0; loc_line1; loc_pos1 } = loc in
        let { loc_fname=loci_fname; loc_line0=loci_line0;
            loc_pos0=loci_pos0; loc_line1=loci_line1; loc_pos1=loci_pos1 } = loci in
        if loc_fname != loci_fname then
            (if loc_fname = noid then loci else loc)
        else
        {
            loc_fname;
            loc_line0=min loc_line0 loci_line0;
            loc_pos0=min loc_pos0 loci_pos0;
            loc_line1=max loc_line1 loci_line1;
            loc_pos1=max loc_pos1 loci_pos1
        }) default_loc llist
let get_start_loc loc =
    let { loc_fname; loc_line0; loc_pos0; loc_line1; loc_pos1 } = loc in
    { loc_fname; loc_line0; loc_pos0; loc_line1=loc_line0; loc_pos1=loc_pos0 }
let get_end_loc loc =
    let { loc_fname; loc_line0; loc_pos0; loc_line1; loc_pos1 } = loc in
    { loc_fname; loc_line0=loc_line1; loc_pos0=loc_pos1; loc_line1; loc_pos1 }

(* the primitive objects in the programs, literals, that represent themselves *)
type lit_t =
    | LitInt of int64 (* "int" (a signed integer that can be stored in CPU register,
                          64-bit on 64-bit platforms, 32-bit on 32-bit platforms) *)
    | LitSInt of int * int64 (* int8, int16, int32, int64 *)
    | LitUInt of int * int64 (* uint8, uint16, uint32, uint64.
      Ocaml does not provide unsigned 64-bit integer in its standard library
      (even though there is external UInt64 module). Let's use int64 to represent unsigned integers.
      (uint8, uint16 and uint32 will fit it easily, and operations on uint64 can be
      emulated using int64 ops (or even map to them 1:1) *)
    | LitFloat of int * float (* float, double, potentially half too *)
    | LitString of string (* UTF-8 string *)
    | LitChar of string (* a single character may require multiple bytes, so we use OCaml string for it *)
    | LitBool of bool
    | LitNil (* can be used as stub initializer for C pointers, interfaces,
                recursive variants, empty lists etc. *)

(* type of an expression *)
type typ_t =
    | TypVar of typ_t option ref (* this is for type unification.
                        Initially most expressions are given TypVar (ref None) type,
                        and during type unification we
                        equalize types by redirecting those references to
                        other types (i.e. we use so-called destructive
                        type unification algorithm) *)
    | TypVarTuple of typ_t option  (* (...) or ('t...) *)
    | TypVarArray of typ_t (* 't [+] *)
    | TypVarRecord
    | TypInt
    | TypSInt of int
    | TypUInt of int
    | TypFloat of int
    | TypString
    | TypChar
    | TypBool
    | TypVoid
    | TypFun of typ_t list * typ_t (* unlike some classical ML languages, we initially let
                                    functions to have multiple arguments, not just one *)
    | TypList of typ_t
    | TypTuple of typ_t list
    | TypRef of typ_t
    | TypArray of int * typ_t
    | TypRecord of ((id_t * typ_t * lit_t option) list * bool) ref
    | TypExn
    | TypErr (* a thrown exception; can be unified with any other type (except for declaration) *)
    | TypCPointer (* smart pointer to a C structure; we use it for file handles, mutexes etc. *)
    | TypApp of typ_t list * id_t (* a generic type instance or a type alias (when typ_t list is empty) *)
    | TypDecl (* since declarations are also expressions, they should have some type;
                 and this is not "void" by the way *)
    | TypModule

let make_new_typ () = TypVar (ref (None: typ_t option))

type binop_t =
    | OpAdd | OpSub | OpMul | OpDiv | OpMod | OpPow | OpShiftLeft | OpShiftRight
    | OpDotMul | OpDotDiv | OpDotMod | OpDotPow
    | OpBitwiseAnd | OpLogicAnd | OpBitwiseOr | OpLogicOr | OpBitwiseXor
    | OpCompareEQ | OpCompareNE | OpCompareLT | OpCompareLE | OpCompareGT | OpCompareGE
    | OpDotCompareEQ | OpDotCompareNE | OpDotCompareLT | OpDotCompareLE | OpDotCompareGT | OpDotCompareGE
    | OpSpaceship | OpDotSpaceship | OpCons

type unop_t = OpPlus | OpNegate | OpDotMinus | OpBitwiseNot | OpLogicNot | OpMkRef | OpDeref | OpExpand | OpApos

type val_flags_t =
{
    val_flag_arg: bool;
    val_flag_mutable: bool;
    val_flag_temp: bool;
    val_flag_tempref: bool;
    val_flag_private: bool;
    val_flag_subarray: bool;
    val_flag_ctor: id_t; (* when the value is 0-parameter constructor of variant (or exception) *)
    val_flag_global: scope_t list; (* global public values at module level or public static class members *)
}

let default_val_flags() =
{
    val_flag_arg=false;
    val_flag_mutable=false;
    val_flag_temp=false;
    val_flag_tempref=false;
    val_flag_private=false;
    val_flag_subarray=false;
    val_flag_ctor=noid;
    val_flag_global=[]
}
let default_arg_flags() = {(default_val_flags()) with val_flag_arg=true}
let default_var_flags() = {(default_val_flags()) with val_flag_mutable=true}
let default_tempval_flags() = {(default_val_flags()) with val_flag_temp=true}
let default_tempref_flags() = {(default_val_flags()) with val_flag_tempref=true}
let default_tempvaR_flags() = {(default_tempval_flags()) with val_flag_mutable=true}

type fun_constr_t = CtorNone | CtorStruct | CtorVariant of id_t | CtorFP of id_t | CtorExn of id_t
type fun_flags_t =
{
    fun_flag_pure: int;
    fun_flag_ccode: bool;
    fun_flag_has_keywords: bool;
    fun_flag_inline: bool;
    fun_flag_nothrow: bool;
    fun_flag_really_nothrow: bool;
    fun_flag_private: bool;
    fun_flag_ctor: fun_constr_t;
    fun_flag_uses_fv: bool;
    fun_flag_recursive: bool
}

let default_fun_flags() =
{
    fun_flag_pure = -1;
    fun_flag_ccode=false;
    fun_flag_has_keywords=false;
    fun_flag_inline=false;
    fun_flag_nothrow=false;
    fun_flag_really_nothrow=false;
    fun_flag_private=false;
    fun_flag_ctor=CtorNone;
    fun_flag_uses_fv=false;
    fun_flag_recursive=false
}

type for_make_t = ForMakeArray | ForMakeList | ForMakeTuple
type for_flags_t =
{
    for_flag_parallel: bool;
    for_flag_make: for_make_t;
    for_flag_unzip: bool;
    for_flag_fold: bool;
    for_flag_nested: bool;
}
let default_for_flags() =
{
    for_flag_parallel=false;
    for_flag_make=ForMakeArray;
    for_flag_unzip=false;
    for_flag_fold=false;
    for_flag_nested=false
}

type border_t = BorderNone | BorderClip | BorderZero
type interpolate_t = InterpNone | InterpLinear

type var_flags_t = { var_flag_object: id_t; var_flag_record: bool; var_flag_recursive: bool; var_flag_have_tag: bool; var_flag_opt: bool }
let default_variant_flags() = {var_flag_object=noid; var_flag_record=false; var_flag_recursive=false; var_flag_have_tag=true; var_flag_opt=false}

type ctx_t = typ_t * loc_t

type exp_t =
    | ExpNop of loc_t (* empty expression {} *)
    | ExpBreak of bool * loc_t
    | ExpContinue of loc_t
    | ExpRange of exp_t option * exp_t option * exp_t option * ctx_t
    | ExpLit of lit_t * ctx_t
    | ExpIdent of id_t * ctx_t
    | ExpBinOp of binop_t * exp_t * exp_t * ctx_t
    | ExpUnOp of unop_t * exp_t * ctx_t
    | ExpSeq of exp_t list * ctx_t
    | ExpMkTuple of exp_t list * ctx_t
    | ExpMkArray of exp_t list list * ctx_t
    | ExpMkRecord of exp_t * (id_t * exp_t) list * ctx_t
    | ExpUpdateRecord of exp_t * (id_t * exp_t) list * ctx_t
    | ExpCall of exp_t * exp_t list * ctx_t
    | ExpAt of exp_t * border_t * interpolate_t * exp_t list * ctx_t
    | ExpAssign of exp_t * exp_t * loc_t
    | ExpMem of exp_t * exp_t * ctx_t
    | ExpThrow of exp_t * loc_t
    | ExpIf of exp_t * exp_t * exp_t * ctx_t
    | ExpWhile of exp_t * exp_t * loc_t
    | ExpDoWhile of exp_t * exp_t * loc_t
    | ExpFor of (pat_t * exp_t) list * pat_t * exp_t * for_flags_t * loc_t
    | ExpMap of ((pat_t * exp_t) list * pat_t) list * exp_t * for_flags_t * ctx_t
    | ExpTryCatch of exp_t * (pat_t list * exp_t) list * ctx_t
    | ExpMatch of exp_t * (pat_t list * exp_t) list * ctx_t
    | ExpCast of exp_t * typ_t * ctx_t
    | ExpTyped of exp_t * typ_t * ctx_t
    | ExpCCode of string * ctx_t
    | DefVal of pat_t * exp_t * val_flags_t * loc_t
    | DefFun of deffun_t ref
    | DefExn of defexn_t ref
    | DefTyp of deftyp_t ref
    | DefVariant of defvariant_t ref
    | DefClass of defclass_t ref
    | DefInterface of definterface_t ref
    | DirImport of (id_t * id_t) list * loc_t
    | DirImportFrom of id_t * id_t list * loc_t
    | DirPragma of string list * loc_t
and pat_t =
    | PatAny of loc_t
    | PatLit of lit_t * loc_t
    | PatIdent of id_t * loc_t
    | PatTuple of pat_t list * loc_t
    | PatVariant of id_t * pat_t list * loc_t
    | PatRecord of id_t option * (id_t * pat_t) list * loc_t
    | PatCons of pat_t * pat_t * loc_t
    | PatAs of pat_t * id_t * loc_t
    | PatTyped of pat_t * typ_t * loc_t
    | PatWhen of pat_t * exp_t * loc_t
    | PatRef of pat_t * loc_t
and env_entry_t = EnvId of id_t | EnvTyp of typ_t
and env_t = env_entry_t list Env.t
and defval_t = { dv_name: id_t; dv_typ: typ_t; dv_flags: val_flags_t;
                 dv_scope: scope_t list; dv_loc: loc_t }
and deffun_t = { df_name: id_t; df_templ_args: id_t list; df_args: pat_t list; df_typ: typ_t;
                 df_body: exp_t; df_flags: fun_flags_t; df_scope: scope_t list; df_loc: loc_t;
                 df_templ_inst: id_t list; df_env: env_t }
and defexn_t = { dexn_name: id_t; dexn_typ: typ_t; dexn_scope: scope_t list; dexn_loc: loc_t }
and deftyp_t = { dt_name: id_t; dt_templ_args: id_t list; dt_typ: typ_t;
                 dt_finalized: bool; dt_scope: scope_t list; dt_loc: loc_t }
(* variants are represented in a seemingly complex but logical way;
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
*)
and defvariant_t = { dvar_name: id_t; dvar_templ_args: id_t list; dvar_alias: typ_t;
                     dvar_flags: var_flags_t; mutable dvar_cases: (id_t * typ_t) list;
                     mutable dvar_ctors: id_t list; mutable dvar_templ_inst: id_t list;
                     dvar_scope: scope_t list; dvar_loc: loc_t }
and defclass_t = { dcl_name: id_t; dcl_templ_args: id_t list; dcl_typ: typ_t;
                   dcl_ifaces: id_t list; dcl_args: pat_t list;
                   dcl_members: exp_t list; mutable dcl_templ_inst: id_t list;
                   dcl_scope: scope_t list; dcl_loc: loc_t }
and definterface_t = { di_name: id_t; di_base: id_t; di_members: exp_t list;
                       di_scope: scope_t list; di_loc: loc_t }

type defmodule_t = { dm_name: id_t; dm_filename: string; mutable dm_defs: exp_t list;
                    mutable dm_idx: int; mutable dm_deps: id_t list;
                    mutable dm_env: env_t; mutable dm_parsed: bool }

type pragmas_t = { pragma_cpp: bool; pragma_clibs: (string*loc_t) list }

type id_info_t =
    | IdNone | IdVal of defval_t | IdFun of deffun_t ref
    | IdExn of defexn_t ref | IdTyp of deftyp_t ref | IdVariant of defvariant_t ref
    | IdClass of defclass_t ref | IdInterface of definterface_t ref | IdModule of defmodule_t ref

type 't dynvec_t = { mutable dynvec_count: int; mutable dynvec_data: 't array; dynvec_val0: 't }

let dynvec_create (v0: 't) = { dynvec_count=0; dynvec_data=([||]: 't array); dynvec_val0=v0 }
let dynvec_isempty v = v.dynvec_count = 0
let dynvec_clear v = v.dynvec_count <- 0; v.dynvec_data <- [||]
let dynvec_init v n = v.dynvec_count <- n; v.dynvec_data <- Array.make n v.dynvec_val0
let dynvec_push v =
    let _ = if (Array.length v.dynvec_data) <= v.dynvec_count then
        let delta_n = max v.dynvec_count 128 in
        let new_data = Array.make delta_n v.dynvec_val0 in
        v.dynvec_data <- Array.append v.dynvec_data new_data
    else () in
    let i = v.dynvec_count in
    (v.dynvec_count <- v.dynvec_count + 1; i)
let dynvec_get v i = v.dynvec_data.(i)
let dynvec_set v i newv = v.dynvec_data.(i) <- newv

let all_ids = dynvec_create IdNone
let all_strhash: (string, int) Hashtbl.t = Hashtbl.create 1000
let all_strings = dynvec_create ""

let all_modules: (string, id_t) Hashtbl.t = Hashtbl.create 100
let sorted_modules: id_t list ref = ref []

let sprintf = Printf.sprintf
let printf = Printf.printf

let ids_frozen = ref true
let freeze_ids f = ids_frozen := f

let new_id_idx() =
    if not !ids_frozen then () else
        failwith "internal error: attempt to add new AST id during K-phase of C code generation phase";
    dynvec_push all_ids

let dump_id i = match i with
    | Id.Name(i) -> (sprintf "Id.Name(%d)" i)
    | Id.Val(i, j) -> (sprintf "Id.Val(%d, %d)" i j)
    | Id.Temp(i, j) -> (sprintf "Id.Temp(%d, %d)" i j)

let id2str__ i pp val_decor temp_decor =
    if i = noid then "<noid>"
    else
        let (infix, prefix, suffix) = match i with
            | Id.Name(i) -> ("", i, -1)
            | Id.Val(i, j) -> (val_decor, i, j)
            | Id.Temp(i, j) -> (temp_decor, i, j) in
        let prefix = dynvec_get all_strings prefix in
        if pp || suffix < 0 then (sprintf "%s" prefix)
        else (sprintf "%s%s%d" prefix infix suffix)

let id2str_ i pp = id2str__ i pp "@" "@@"

let id2str i = id2str_ i false
let pp_id2str i = id2str_ i true

(* used by the parser *)
exception SyntaxError of string * Lexing.position*Lexing.position
exception SyntaxErrorLoc of string * loc_t
let loc2str loc = sprintf "%s: %d" (pp_id2str loc.loc_fname) loc.loc_line0

exception CompileError of loc_t * string
exception PropagateCompileError

let compile_errs = ref ([]: exn list)
let compile_err_ctx = ref ([]: string list)

let raise_compile_err loc msg =
    let whole_msg = sprintf "%s: error: %s" (loc2str loc) msg in
    let whole_msg = match !compile_err_ctx with
        | [] -> whole_msg
        | ctx -> String.concat "\n\t" (whole_msg :: ctx)
        in
    raise (CompileError(loc, whole_msg))

let push_compile_err err =
    compile_errs := err :: !compile_errs

let pop_compile_err loc =
    match !compile_errs with
    | _ :: rest -> compile_errs := rest
    | _ -> raise_compile_err loc "attempt to pop non-existing compile error"

let check_compile_errs () =
    match !compile_errs with
    | err :: _ -> raise PropagateCompileError
    | _ -> ()

let print_compile_err err =
    match err with
    (* error message has been formatted already in raise_typecheck_err(); just print it *)
    | CompileError(loc, msg) -> printf "%s\n" msg
    | Failure msg -> printf "Failure: %s\n" msg
    | _ -> printf "\n\nException %s occured\n" (Printexc.to_string err)

let pr_verbose str =
    if Options.options.verbose then
        (printf "%s%s" str (if Utils.ends_with str "\n" then "" else "\n");
        flush_all())
    else ()

let id2prefix i =
    let prefix = match i with
    | Id.Name(i) -> i
    | Id.Val(i, _) -> i
    | Id.Temp(i, _) -> i in
    dynvec_get all_strings prefix

let id2idx_ i loc = match i with
    | Id.Name _ ->
        raise_compile_err loc (sprintf "attempt to query information about unresolved '%s'" (id2str i))
    | Id.Val(_, i_real) -> i_real
    | Id.Temp(_, i_real) -> i_real

let id2idx i = id2idx_ i noloc

let id_info i = dynvec_get all_ids (id2idx i)
let is_unique_id i = match i with Id.Name _ -> false | _ -> true

let get_id_prefix s =
    match Hashtbl.find_all all_strhash s with
    | idx :: _ -> idx
    | _ -> let idx = dynvec_push all_strings in
        Hashtbl.add all_strhash s idx;
        dynvec_set all_strings idx s;
        idx

let get_id s =
    let i = get_id_prefix s in Id.Name(i)

let gen_temp_id s =
    let i_name = get_id_prefix s in
    let i_real = new_id_idx() in
    Id.Temp(i_name, i_real)

let dup_id old_id =
    let k = new_id_idx() in
    match old_id with
    | Id.Name(i) -> Id.Val(i, k)
    | Id.Val(i, _) -> Id.Val(i, k)
    | Id.Temp(i, _) -> Id.Temp(i, k)

let get_orig_id i =
    match i with
    | Id.Name _ -> i
    | Id.Val(i, _) -> Id.Name(i)
    | Id.Temp(_, _) -> i

let set_id_entry i n =
    let idx = id2idx i in dynvec_set all_ids idx n

let get_exp_ctx e = match e with
    | ExpNop(l) -> (TypVoid, l)
    | ExpBreak(_, l) -> (TypVoid, l)
    | ExpContinue(l) -> (TypVoid, l)
    | ExpRange(_, _, _, c) -> c
    | ExpLit(_, c) -> c
    | ExpIdent(_, c) -> c
    | ExpBinOp(_, _, _, c) -> c
    | ExpUnOp(_, _, c) -> c
    | ExpSeq(_, c) -> c
    | ExpMkTuple(_, c) -> c
    | ExpMkRecord(_, _, c) -> c
    | ExpMkArray(_, c) -> c
    | ExpUpdateRecord(_, _, c) -> c
    | ExpCall(_, _, c) -> c
    | ExpAt(_, _, _, _, c) -> c
    | ExpAssign(_, _, l) -> (TypVoid, l)
    | ExpMem(_, _, c) -> c
    | ExpThrow(_, l) -> (TypErr, l)
    | ExpIf(_, _, _, c) -> c
    | ExpWhile(_, _, l) -> (TypVoid, l)
    | ExpDoWhile(_, _, l) -> (TypVoid, l)
    | ExpFor(_, _, _, _, l) -> (TypVoid, l)
    | ExpMap(_, _, _, c) -> c
    | ExpTryCatch(_, _, c) -> c
    | ExpMatch(_, _, c) -> c
    | ExpCast(_, _, c) -> c
    | ExpTyped(_, _, c) -> c
    | ExpCCode(_, c) -> c
    | DefVal(_, _, _, dv_loc) -> (TypDecl, dv_loc)
    | DefFun {contents = { df_loc }} -> (TypDecl, df_loc)
    | DefExn {contents = { dexn_loc }} -> (TypDecl, dexn_loc)
    | DefTyp {contents = { dt_loc }} -> (TypDecl, dt_loc)
    | DefVariant {contents = { dvar_loc }} -> (TypDecl, dvar_loc)
    | DefClass {contents = { dcl_loc }} -> (TypDecl, dcl_loc)
    | DefInterface {contents = { di_loc }} -> (TypDecl, di_loc)
    | DirImport(_, l) -> (TypDecl, l)
    | DirImportFrom(_, _, l) -> (TypDecl, l)
    | DirPragma(_, l) -> (TypDecl, l)

let get_exp_typ e = let (t, l) = (get_exp_ctx e) in t
let get_exp_loc e = let (t, l) = (get_exp_ctx e) in l

let get_pat_loc p = match p with
    | PatAny l -> l
    | PatLit(_, l) -> l
    | PatIdent(_, l) -> l
    | PatTuple(_, l) -> l
    | PatVariant(_, _, l) -> l
    | PatRecord(_, _, l) -> l
    | PatCons(_, _, l) -> l
    | PatAs(_, _, l) -> l
    | PatTyped(_, _, l) -> l
    | PatRef(_, l) -> l
    | PatWhen(_, _, l) -> l

let rec pat_skip_typed p = match p with
    | PatTyped(p, _, _) -> pat_skip_typed p
    | _ -> p

let get_module m =
    match id_info m with
    | IdModule minfo -> minfo
    | _ -> failwith (sprintf "internal error in process_all: %s is not a module" (pp_id2str m))

let get_module_env m = !(get_module m).dm_env

let find_module mname_id mfname =
    try get_module (Hashtbl.find all_modules mfname) with
    Not_found ->
        let m_fresh_id = dup_id mname_id in
        let newmodule = ref { dm_name=m_fresh_id; dm_filename=mfname; dm_idx= -1; dm_defs=[];
                              dm_deps=[]; dm_env=Env.empty; dm_parsed=false } in
        set_id_entry m_fresh_id (IdModule newmodule);
        Hashtbl.add all_modules mfname m_fresh_id;
        newmodule

let block_scope_idx = ref (-1)
let new_block_scope () =
    block_scope_idx := !block_scope_idx + 1;
    ScBlock !block_scope_idx
let new_loop_scope nested =
    block_scope_idx := !block_scope_idx + 1;
    ScLoop (nested, !block_scope_idx)
let new_map_scope () =
    block_scope_idx := !block_scope_idx + 1;
    ScMap !block_scope_idx
let new_arr_map_scope () =
    block_scope_idx := !block_scope_idx + 1;
    ScArrMap !block_scope_idx
let new_fold_scope () =
    block_scope_idx := !block_scope_idx + 1;
    ScFold !block_scope_idx
let new_try_scope () =
    block_scope_idx := !block_scope_idx + 1;
    ScTry !block_scope_idx

let rec scope2str sc =
    match sc with
    | ScBlock b :: r -> (sprintf "block(%d)." b) ^ (scope2str r)
    | ScLoop (f, b) :: r -> (sprintf "%sloop_block(%d)." (if f then "nested" else "") b) ^ (scope2str r)
    | ScArrMap b :: r -> (sprintf "arr_map_block(%d)." b) ^ (scope2str r)
    | ScMap b :: r -> (sprintf "map_block(%d)." b) ^ (scope2str r)
    | ScFold b :: r -> (sprintf "fold_block(%d)." b) ^ (scope2str r)
    | ScTry b :: r -> (sprintf "try_block(%d)." b) ^ (scope2str r)
    | ScFun f :: r -> (sprintf "fun(%s)." (id2str f)) ^ (scope2str r)
    | ScClass c :: r -> (sprintf "class(%s)." (id2str c)) ^ (scope2str r)
    | ScInterface i :: r -> (sprintf "interface(%s)." (id2str i)) ^ (scope2str r)
    | ScModule m :: r -> (sprintf "mod(%s)." (id2str m)) ^ (scope2str r)
    | ScGlobal :: r -> "global." ^ (scope2str r)
    | [] -> ""

let rec get_module_scope sc =
    match sc with
    | ScModule _ :: _ -> sc | ScGlobal :: _ | [] -> sc
    | sc_top :: r -> get_module_scope r

let rec get_qualified_name name sc =
    match sc with
    | ScModule m :: _ when (pp_id2str m) = "Builtins" -> name
    | ScModule m :: r -> get_qualified_name ((pp_id2str m) ^ "." ^ name) r
    | ScGlobal :: _ | [] -> name
    | sc_top :: r -> get_qualified_name name r

let get_scope id_info = match id_info with
    | IdNone -> ScGlobal :: []
    | IdVal {dv_scope} -> dv_scope
    | IdFun {contents = {df_scope}} -> df_scope
    | IdExn {contents = {dexn_scope}} -> dexn_scope
    | IdTyp {contents = {dt_scope}} -> dt_scope
    | IdVariant {contents = {dvar_scope}} -> dvar_scope
    | IdClass {contents = {dcl_scope}} -> dcl_scope
    | IdInterface {contents = {di_scope}} -> di_scope
    | IdModule _ -> ScGlobal :: []

(* retains meaningful scope for name mangling *)
let simplify_scope sc =
    let sc1 =
    List.filter (function
    | ScModule _ | ScClass _ | ScInterface _ | ScGlobal -> true
    | ScBlock _ | ScLoop _ | ScFold _ | ScArrMap _ | ScMap _
    | ScTry _ | ScFun _ -> false) sc in
    let is_private = (List.length sc) > (List.length sc1) in
    (is_private, (match sc1 with
    | ScGlobal :: rest -> rest
    | _ -> sc1))

let get_idinfo_loc id_info = match id_info with
    | IdNone | IdModule _ -> noloc
    | IdVal {dv_loc} -> dv_loc
    | IdFun {contents = {df_loc}} -> df_loc
    | IdExn {contents = {dexn_loc}} -> dexn_loc
    | IdTyp {contents = {dt_loc}} -> dt_loc
    | IdVariant {contents = {dvar_loc}} -> dvar_loc
    | IdClass {contents = {dcl_loc}} -> dcl_loc
    | IdInterface {contents = {di_loc}} -> di_loc

let get_idinfo_typ id_info loc = match id_info with
    | IdNone -> raise_compile_err loc "ast: attempt to request type of non-existing symbol"
    | IdModule _ -> TypModule
    | IdVal {dv_typ} -> dv_typ
    | IdFun {contents = {df_typ}} -> df_typ
    | IdExn {contents = {dexn_typ}} -> dexn_typ
    | IdTyp {contents = {dt_typ}} -> dt_typ
    | IdVariant {contents = {dvar_alias}} -> dvar_alias
    | IdClass {contents = {dcl_typ}} -> dcl_typ
    | IdInterface {contents = {di_name}} -> TypApp([], di_name)

let get_id_typ i loc = match i with Id.Name _ -> TypVar (ref None) | _ -> get_idinfo_typ (id_info i) loc

(* used by the type checker *)
let get_lit_typ l = match l with
    | LitInt(_) -> TypInt
    | LitSInt(b, _) -> TypSInt(b)
    | LitUInt(b, _) -> TypUInt(b)
    | LitFloat(b, _) -> TypFloat(b)
    | LitString(_) -> TypString
    | LitChar(_) -> TypChar
    | LitBool(_) -> TypBool
    (* in the case of NIL ([]) we cannot infere the type; we postpone this step *)
    | LitNil -> TypList(TypVar(ref None))

(* shorten type specification by redirecting the references in TypVar
   to the "root" of each connected component tree - a cluster of equivalent/unified types.
   In other words, if we had
   t -> t2 -> t3 ... -> root
   before the call, after the call we will have
   t -> root, t2 -> root, t3 -> root, ...
   Returns the root. *)
let rec deref_typ t =
    match t with
    | TypVar _ ->
        let rec find_root t = match t with
            | TypVar {contents=Some(TypVarArray _)} -> t
            | TypVar {contents=Some(TypVarTuple _)} -> t
            | TypVar {contents=Some(TypVarRecord)} -> t
            | TypVar {contents=Some(t2)} -> find_root t2
            | _ -> t in
        let root = find_root t in
        let rec update_refs t =
            match t with
            | TypVar ({contents=Some((TypVar {contents=(Some _)}) as t1)} as r) ->
                r := Some(root); update_refs t1
            | _ -> root
        in update_refs t
    | _ -> t

let is_typ_scalar t =
    match (deref_typ t) with
    | TypInt | TypSInt _ | TypUInt _ | TypFloat _ | TypBool | TypChar -> true
    | _ -> false

let rec is_typ_numeric t allow_vec_tuples =
    match (deref_typ t) with
    | TypInt | TypSInt _ | TypUInt _ | TypFloat _ -> true
    | TypTuple(t0 :: trest) -> allow_vec_tuples && (is_typ_numeric t0 true) &&
        List.for_all (fun t -> (deref_typ t) = (deref_typ t0)) trest
    | _ -> false

let binop_to_string bop = match bop with
    | OpAdd -> "+"
    | OpSub -> "-"
    | OpMul -> "*"
    | OpDiv -> "/"
    | OpMod -> "%"
    | OpPow -> "**"
    | OpDotMul -> ".*"
    | OpDotDiv -> "./"
    | OpDotMod -> ".%"
    | OpDotPow -> ".**"
    | OpShiftLeft -> "<<"
    | OpShiftRight -> ">>"
    | OpBitwiseAnd -> "&"
    | OpLogicAnd -> "&&"
    | OpBitwiseOr -> "|"
    | OpLogicOr -> "||"
    | OpBitwiseXor -> "^"
    | OpSpaceship -> "<=>"
    | OpCompareEQ -> "=="
    | OpCompareNE -> "!="
    | OpCompareLE -> "<="
    | OpCompareGE -> ">="
    | OpCompareLT -> "<"
    | OpCompareGT -> ">"
    | OpDotSpaceship -> ".<=>"
    | OpDotCompareEQ -> ".=="
    | OpDotCompareNE -> ".!="
    | OpDotCompareLE -> ".<="
    | OpDotCompareGE -> ".>="
    | OpDotCompareLT -> ".<"
    | OpDotCompareGT -> ".>"
    | OpCons -> "::"

let unop_to_string uop = match uop with
    | OpPlus -> "+"
    | OpNegate -> "-"
    | OpDotMinus -> ".-"
    | OpBitwiseNot -> "~"
    | OpLogicNot -> "!"
    | OpExpand -> "\\"
    | OpMkRef -> "REF"
    | OpDeref -> "*"
    | OpApos -> "'"

let border2str border f =
    let pt = if f then "." else "" in
    match (border, f) with
    | (BorderNone, true) -> ""
    | (BorderNone, _) -> "NONE"
    | (BorderClip, _) -> pt ^ "CLIP"
    | (BorderZero, _) -> pt ^ "ZERO"

let interp2str interp f =
    let pt = if f then "." else "" in
    match (interp, f) with
    | (InterpNone, true) -> ""
    | (InterpNone, _) -> "NONE"
    | (InterpLinear, _) -> pt ^ "LINEAR"

let fname_op_apos() = get_id "__apos__"
let fname_op_add() = get_id "__add__"
let fname_op_sub() = get_id "__sub__"
let fname_op_mul() = get_id "__mul__"
let fname_op_div() = get_id "__div__"
let fname_op_mod() = get_id "__mod__"
let fname_op_pow() = get_id "__pow__"
let fname_op_dot_mul() = get_id "__dot_mul__"
let fname_op_dot_div() = get_id "__dot_div__"
let fname_op_dot_mod() = get_id "__dot_mod__"
let fname_op_dot_pow() = get_id "__dot_pow__"
let fname_op_shl() = get_id "__shl__"
let fname_op_shr() = get_id "__shr__"
let fname_op_bit_and() = get_id "__bit_and__"
let fname_op_bit_or() = get_id "__bit_or__"
let fname_op_bit_xor() = get_id "__bit_xor__"
let fname_op_cmp() = get_id "__cmp__"
let fname_op_eq() = get_id "__eq__"
let fname_op_ne() = get_id "__ne__"
let fname_op_lt() = get_id "__lt__"
let fname_op_gt() = get_id "__gt__"
let fname_op_le() = get_id "__le__"
let fname_op_ge() = get_id "__ge__"
let fname_op_dot_cmp() = get_id "__dot_cmp__"
let fname_op_dot_eq() = get_id "__dot_eq__"
let fname_op_dot_ne() = get_id "__dot_ne__"
let fname_op_dot_lt() = get_id "__dot_lt__"
let fname_op_dot_gt() = get_id "__dot_gt__"
let fname_op_dot_le() = get_id "__dot_le__"
let fname_op_dot_ge() = get_id "__dot_ge__"

let fname_op_plus() = get_id "__plus__"
let fname_op_negate() = get_id "__negate__"
let fname_op_dot_minus() = get_id "__dot_minus__"
let fname_op_bit_not() = get_id "__bit_not__"

let fname_to_int() = get_id "int"
let fname_to_uint8() = get_id "uint8"
let fname_to_int8() = get_id "int8"
let fname_to_uint16() = get_id "uint16"
let fname_to_int16() = get_id "int16"
let fname_to_uint32() = get_id "uint32"
let fname_to_int32() = get_id "int32"
let fname_to_uint64() = get_id "uint64"
let fname_to_int64() = get_id "int64"
let fname_to_float() = get_id "float"
let fname_to_double() = get_id "double"
let fname_to_bool() = get_id "bool"
let fname_string() = get_id "string"
let fname_repr() = get_id "repr"

let binop_try_remove_dot bop =
    match bop with
    | OpDotMul -> OpMul
    | OpDotDiv -> OpDiv
    | OpDotMod -> OpMod
    | OpDotPow -> OpPow
    | OpDotSpaceship -> OpSpaceship
    | OpDotCompareEQ -> OpCompareEQ
    | OpDotCompareNE -> OpCompareNE
    | OpDotCompareLE -> OpCompareLE
    | OpDotCompareGE -> OpCompareGE
    | OpDotCompareLT -> OpCompareLT
    | OpDotCompareGT -> OpCompareGT
    | _ -> bop

let get_binop_fname bop loc =
    match bop with
    | OpAdd -> fname_op_add()
    | OpSub -> fname_op_sub()
    | OpMul -> fname_op_mul()
    | OpDiv -> fname_op_div()
    | OpMod -> fname_op_mod()
    | OpPow -> fname_op_pow()
    | OpDotMul -> fname_op_dot_mul()
    | OpDotDiv -> fname_op_dot_div()
    | OpDotMod -> fname_op_dot_mod()
    | OpDotPow -> fname_op_dot_pow()
    | OpShiftLeft -> fname_op_shl()
    | OpShiftRight -> fname_op_shr()
    | OpBitwiseAnd -> fname_op_bit_and()
    | OpBitwiseOr -> fname_op_bit_or()
    | OpBitwiseXor -> fname_op_bit_xor()
    | OpSpaceship -> fname_op_cmp()
    | OpCompareEQ -> fname_op_eq()
    | OpCompareNE -> fname_op_ne()
    | OpCompareLE -> fname_op_le()
    | OpCompareGE -> fname_op_ge()
    | OpCompareLT -> fname_op_lt()
    | OpCompareGT -> fname_op_gt()
    | OpDotSpaceship -> fname_op_dot_cmp()
    | OpDotCompareEQ -> fname_op_dot_eq()
    | OpDotCompareNE -> fname_op_dot_ne()
    | OpDotCompareLE -> fname_op_dot_le()
    | OpDotCompareGE -> fname_op_dot_ge()
    | OpDotCompareLT -> fname_op_dot_lt()
    | OpDotCompareGT -> fname_op_dot_gt()
    | OpLogicAnd | OpLogicOr | OpCons ->
        raise_compile_err loc
            (sprintf "for binary operation \"%s\" there is no corresponding function" (binop_to_string bop))

let get_unop_fname uop loc =
    match uop with
    | OpApos -> fname_op_apos()
    | OpPlus -> fname_op_plus()
    | OpNegate -> fname_op_negate()
    | OpDotMinus -> fname_op_dot_minus()
    | OpBitwiseNot -> fname_op_bit_not()
    | OpLogicNot | OpExpand | OpMkRef | OpDeref ->
        raise_compile_err loc
            (sprintf "for unary operation \"%s\" there is no corresponding function" (unop_to_string uop))

let fname_always_import () =
[
    fname_op_add(); fname_op_sub();
    fname_op_mul(); fname_op_div(); fname_op_mod(); fname_op_pow();
    fname_op_dot_mul(); fname_op_dot_div(); fname_op_dot_mod(); fname_op_dot_pow();
    fname_op_shl(); fname_op_shr();
    fname_op_bit_and(); fname_op_bit_or(); fname_op_bit_xor();
    fname_op_cmp(); fname_op_dot_cmp();
    fname_op_eq(); fname_op_ne(); fname_op_le(); fname_op_ge(); fname_op_lt(); fname_op_gt();
    fname_op_dot_eq(); fname_op_dot_ne(); fname_op_dot_le();
    fname_op_dot_ge(); fname_op_dot_lt(); fname_op_dot_gt();

    fname_op_plus(); fname_op_negate(); fname_op_dot_minus(); fname_op_bit_not(); fname_op_apos();

    fname_to_int(); fname_to_uint8(); fname_to_int8(); fname_to_uint16(); fname_to_int16();
    fname_to_uint32(); fname_to_int32(); fname_to_uint64(); fname_to_int64();
    fname_to_float(); fname_to_double(); fname_to_bool(); fname_string(); fname_repr()
]

let init_all_ids () =
    freeze_ids false;
    dynvec_clear all_ids;
    (Hashtbl.reset all_strhash);
    let _ = get_id_prefix "" in
    let _ = get_id_prefix "_" in
    fname_always_import()

let reserved_keywords = Hashtbl.create 1001
let _ = List.iter (fun kwd -> Hashtbl.add reserved_keywords kwd 1)
    [(*"alignas"; "alignof"; "and"; "and_eq"; "asm"; "atomic_cancel";
    "atomic_commit"; "atomic_noexcept"; "auto"; "bitand"; "bitor";
    "break"; "case"; "catch"; "char8_t"; "char16_t";
    "char32_t"; "class"; "compl"; "concept"; "const"; "consteval";
    "constexpr"; "constinit"; "const_cast"; "continue"; "co_await";
    "co_return"; "co_yield"; "decltype"; "default"; "delete"; "do";
    "dynamic_cast"; "else"; "enum"; "explicit"; "export";
    "extern"; "false"; "for"; "friend"; "goto"; "if"; "inline";
    "long"; "mutable"; "namespace"; "new"; "noexcept"; "not";
    "not"; "not_eq"; "nullptr"; "operator"; "or"; "or_eq"; "private";
    "protected"; "public"; "reflexpr"; "register"; "reinterpret_cast";
    "requires"; "restricted"; "return"; "short"; "signed"; "sizeof"; "static";
    "static_assert"; "static_cast"; "struct"; "switch"; "synchronized";
    "template"; "this"; "thread_local"; "throw"; "true"; "try"; "typedef";
    "typeid"; "typename"; "union"; "unsigned"; "using"; "virtual";
    "volatile"; "wchar_t"; "while"; "xor"; "xor_eq";

    "_Alignas"; "_Alignof"; "_Atomic"; "_Bool"; "_Complex";
    "_Generic"; "_Imaginary"; "_Noreturn"; "_Static_assert";
    "_Thread_local";

    "int8_t"; "uint8_t"; "int16_t"; "uint16_t"; "int32_t"; "uint32_t";
    "int64_t"; "uint64_t"; "char_";*) "fx_result"; "fx_status"; "fx_fv"]

let builtin_exceptions = ((Hashtbl.create 101): (id_t, id_t) Hashtbl.t)
let get_builtin_exception n0 loc =
    match Hashtbl.find_all builtin_exceptions n0 with
    | n :: _ -> n
    | _ -> raise_compile_err loc (sprintf "cannot find built-in exception '%s'" (id2str n0))

let remove_flag f0 flags =
    List.filter (fun f -> f <> f0) flags

let add_flag f0 flags =
    if List.mem f0 flags then flags else f0 :: flags

let get_val_ctor flags = flags.val_flag_ctor

let get_fun_ctor flags = flags.fun_flag_ctor
let is_fun_ctor flags = flags.fun_flag_ctor <> CtorNone

let ctor2str f =
    let s = match f with
    | CtorNone -> ""
    | CtorStruct -> "record_or_tuple"
    | CtorVariant i -> sprintf "variant(%s)" (id2str i)
    | CtorFP i -> sprintf "fp(%s)" (id2str i)
    | CtorExn i -> sprintf "exn(%s)" (id2str i)
    in if f = CtorNone then "not_a_constructor" else
    ("Constructor(" ^ s ^ ")")

let lit2str c cmode loc =
    let add_dot s suffix =
        (if (String.contains s '.') || (String.contains s 'e') then s else s ^ ".") ^ suffix
    in
    match c with
    | LitInt(v) -> sprintf "%Li" v
    | LitSInt(64, v) -> if cmode then sprintf "%LiLL" v else sprintf "%Lii%d" v 64
    | LitUInt(64, v) -> if cmode then sprintf "%LiULL" v else sprintf "%Lii%d" v 64
    | LitSInt(b, v) -> if cmode then sprintf "%Li" v else sprintf "%Lii%d" v b
    | LitUInt(b, v) -> if cmode then sprintf "%LuU" v else sprintf "%Luu%d" v b
    | LitFloat(16, v) -> let s = sprintf "%.4g" v in (add_dot s "h")
    | LitFloat(32, v) -> let s = sprintf "%.8g" v in (add_dot s "f")
    | LitFloat(64, v) -> let s = sprintf "%.16g" v in (add_dot s "")
    | LitFloat(b, v) -> raise_compile_err loc (sprintf "invalid literal LitFloat(%d, %.16g)" b v)
    | LitString(s) -> "\"" ^ (Utils.escaped_uni s) ^ "\""
    | LitChar(c) -> "\'" ^ (Utils.escaped_uni c) ^ "\'"
    | LitBool(true) -> "true"
    | LitBool(false) -> "false"
    | LitNil -> "nullptr"

let print_idset setname s =
    printf "%s:[" setname;
    IdSet.iter (fun i -> printf " %s" (id2str i)) s;
    printf " ]\n"

let parser_ctx_module = ref noid
let parser_ctx_file = ref noid
let parser_ctx_deps = ref ([] : id_t list)
let parser_ctx_inc_dirs= ref ([] : string list)
let parser_ctx_module_idx = ref (-1)

let add_to_imported_modules mname_id (pos0, pos1) =
    let mname = pp_id2str mname_id in
    match Utils.locate_module_file mname !parser_ctx_inc_dirs with
    | Some(mfname) ->
        let dep_minfo = find_module mname_id mfname in
        let mname_unique_id = !dep_minfo.dm_name in
        (parser_ctx_deps := mname_unique_id :: !parser_ctx_deps;
        mname_unique_id)
    | _ -> raise (SyntaxError(("module " ^ mname ^ " is not found"), pos0, pos1))

let rec typ2str t =
    match t with
    | TypVarTuple(Some(t)) -> sprintf "(%s ...)" (typ2str t)
    | TypVarTuple _ -> "(...)"
    | TypVarArray(t) -> sprintf "%s [+]" (typ2str t)
    | TypVarRecord -> "{...}"
    | TypVar {contents=Some(t)} -> typ2str t
    | TypVar _ -> "<unknown>"
    | TypApp([], i) -> (id2str i)
    | TypApp(tl, i) -> sprintf "%s %s" (tl2str tl) (id2str i)
    | TypInt -> "int"
    | TypSInt n -> sprintf "int%d" n
    | TypUInt n -> sprintf "uint%d" n
    | TypFloat n -> (match n with 16 -> "half" | 32 -> "float" | 64 -> "double" | _ ->
            raise_compile_err noloc (sprintf "bad floating-point type TypFloat(%d)" n))
    | TypVoid -> "void"
    | TypBool -> "bool"
    | TypChar -> "char"
    | TypString -> "string"
    | TypCPointer -> "cptr"
    | TypFun(argtyps, rt) ->
        "(" ^ (tl2str argtyps) ^
        " -> " ^ (typ2str rt) ^ ")"
    | TypTuple tl ->
        let s = tl2str tl in
        (match tl with
        | x :: [] -> "(" ^ s ^ ",)"
        | _ -> s)
    | TypRecord {contents=(relems, _)} ->
        "{" ^
        (String.concat "; " (List.map (fun (i, t, _) ->
            (id2str i) ^ ": " ^ (typ2str t)) relems)) ^ "}"
    | TypArray (d, t) -> sprintf "%s [%s]" (typ2str t) (String.make (d-1) ',')
    | TypList t -> (typ2str t) ^ " list"
    | TypRef t -> (typ2str t) ^ " ref"
    | TypExn -> "exn"
    | TypErr -> "<err>"
    | TypDecl -> "<decl>"
    | TypModule -> "<module>"
and tl2str tl =
    let s = String.concat ", " (List.map (fun t -> typ2str t) tl) in
    match tl with
    | x :: [] -> s
    | _ -> "(" ^ s ^ ")"

let parse_pragmas prl =
    let clib_regexp = Str.regexp "clib *: *\\([_A-Za-z0-9\\-\\.]+\\) *" in
    let rec parse prl result =
        match prl with
        | (pr, loc) :: rest ->
            if pr = "c++" || pr == "C++" then
                parse rest {result with pragma_cpp=true}
            else if (Str.string_match clib_regexp pr 0) then
                let libname = Str.matched_group 1 pr in
                parse rest {result with pragma_clibs=(libname, loc) :: result.pragma_clibs}
            else
                raise_compile_err loc (sprintf "unrecognized pragma '%s'" pr)
        | [] -> result
        in
    parse prl {pragma_cpp=false; pragma_clibs=[]}

type ast_callb_t =
{
    acb_typ: (typ_t -> ast_callb_t -> typ_t) option;
    acb_exp: (exp_t -> ast_callb_t -> exp_t) option;
    acb_pat: (pat_t -> ast_callb_t -> pat_t) option;
}

let rec check_n_walk_typ t callb =
    match callb.acb_typ with
    | Some(f) -> f t callb
    | _ -> walk_typ t callb
and check_n_walk_tlist tlist callb =
    List.map (fun t -> check_n_walk_typ t callb) tlist

and check_n_walk_exp e callb =
    match callb.acb_exp with
    | Some(f) -> f e callb
    | _ -> walk_exp e callb
and check_n_walk_elist elist callb =
    List.map (fun e -> check_n_walk_exp e callb) elist

and check_n_walk_pat p callb =
    match callb.acb_pat with
    | Some(f) -> f p callb
    | _ -> walk_pat p callb
and check_n_walk_plist plist callb =
    List.map (fun p -> check_n_walk_pat p callb) plist

and walk_typ t callb =
    let walk_typ_ t = check_n_walk_typ t callb in
    let walk_tl_ tl = check_n_walk_tlist tl callb in
    (match t with
    | TypVar r ->
        (match !r with
        | Some t ->
            let t1 = walk_typ_ t in
            r := Some t1
        | _ -> ());
        t
    | TypInt -> t
    | TypSInt _ -> t
    | TypUInt _ -> t
    | TypFloat _ -> t
    | TypString -> t
    | TypChar -> t
    | TypBool -> t
    | TypVoid -> t
    | TypFun(args, rt) -> TypFun((walk_tl_ args), (walk_typ_ rt))
    | TypList t -> TypList(walk_typ_ t)
    | TypTuple tl -> TypTuple(walk_tl_ tl)
    | TypVarTuple t_opt -> TypVarTuple(match t_opt with Some t -> Some (walk_typ_ t) | _ -> None)
    | TypRef t -> TypRef(walk_typ_ t)
    | TypArray(d, et) -> TypArray(d, walk_typ_ et)
    | TypVarArray et -> TypVarArray(walk_typ_ et)
    | TypVarRecord -> t
    | TypRecord ({contents=(relems, ordered)} as r) ->
        let new_relems = List.map (fun (n, t, v) -> (n, (walk_typ_ t), v)) relems in
        r := (new_relems, ordered); t
    | TypExn -> t
    | TypErr -> t
    | TypCPointer -> t
    | TypApp(ty_args, n) -> TypApp((walk_tl_ ty_args), n)
    | TypDecl -> t
    | TypModule -> t)

and walk_exp e callb =
    let walk_typ_ t = check_n_walk_typ t callb in
    let walk_exp_ e = check_n_walk_exp e callb in
    let walk_elist_ el = check_n_walk_elist el callb in
    let walk_pat_ p = check_n_walk_pat p callb in
    let walk_plist_ pl = check_n_walk_plist pl callb in
    let walk_pe_l_ pe_l = List.map (fun (p, e) -> ((walk_pat_ p), (walk_exp_ e))) pe_l in
    let walk_ne_l_ ne_l = List.map (fun (n, e) -> (n, (walk_exp_ e))) ne_l in
    let walk_cases_ ple_l =
        List.map (fun (pl, e) -> ((walk_plist_ pl), (walk_exp_ e))) ple_l in
    let walk_exp_opt_ e_opt =
        (match e_opt with
        | Some e -> Some(walk_exp_ e)
        | None -> None) in
    let walk_ctx_ (t, loc) = ((walk_typ_ t), loc) in
    (match e with
    | ExpNop (_) -> e
    | ExpBreak _ -> e
    | ExpContinue _ -> e
    | ExpRange(e1_opt, e2_opt, e3_opt, ctx) ->
        ExpRange((walk_exp_opt_ e1_opt), (walk_exp_opt_ e2_opt),
                    (walk_exp_opt_ e3_opt), (walk_ctx_ ctx))
    | ExpLit(l, ctx) -> ExpLit(l, (walk_ctx_ ctx))
    | ExpIdent(n, ctx) -> ExpIdent(n, (walk_ctx_ ctx))
    | ExpBinOp(bop, e1, e2, ctx) ->
        ExpBinOp(bop, (walk_exp_ e1), (walk_exp_ e2), (walk_ctx_ ctx))
    | ExpUnOp(uop, e, ctx) -> ExpUnOp(uop, (walk_exp_ e), (walk_ctx_ ctx))
    | ExpSeq(elist, ctx) -> ExpSeq((walk_elist_ elist), (walk_ctx_ ctx))
    | ExpMkTuple(elist, ctx) -> ExpMkTuple((walk_elist_ elist), (walk_ctx_ ctx))
    | ExpMkArray(ell, ctx) -> ExpMkArray((List.map walk_elist_ ell), (walk_ctx_ ctx))
    | ExpMkRecord(e, ne_l, ctx) -> ExpMkRecord((walk_exp_ e), (walk_ne_l_ ne_l), (walk_ctx_ ctx))
    | ExpUpdateRecord(e, ne_l, ctx) -> ExpUpdateRecord((walk_exp_ e), (walk_ne_l_ ne_l), (walk_ctx_ ctx))
    | ExpCall(f, args, ctx) -> ExpCall((walk_exp_ f), (walk_elist_ args), (walk_ctx_ ctx))
    | ExpAt(arr, border, interp, idx, ctx) -> ExpAt((walk_exp_ arr), border, interp, (walk_elist_ idx), (walk_ctx_ ctx))
    | ExpAssign(lv, rv, loc) -> ExpAssign((walk_exp_ lv), (walk_exp_ rv), loc)
    | ExpMem(a, member, ctx) -> ExpMem((walk_exp_ a), (walk_exp_ member), (walk_ctx_ ctx))
    | ExpThrow(a, loc) -> ExpThrow((walk_exp_ a), loc)
    | ExpIf(c, then_e, else_e, ctx) ->
        ExpIf((walk_exp_ c), (walk_exp_ then_e), (walk_exp_ else_e), (walk_ctx_ ctx))
    | ExpWhile(c, e, loc) -> ExpWhile((walk_exp_ c), (walk_exp_ e), loc)
    | ExpDoWhile(e, c, loc) -> ExpDoWhile((walk_exp_ e), (walk_exp_ c), loc)
    | ExpFor(pe_l, idx_pat, body, flags, loc) ->
        ExpFor((walk_pe_l_ pe_l), (walk_pat_ idx_pat), (walk_exp_ body), flags, loc)
    | ExpMap(pew_ll, body, flags, ctx) ->
        ExpMap((List.map (fun (pe_l, idx_pat) ->
            (walk_pe_l_ pe_l), (walk_pat_ idx_pat)) pew_ll),
            (walk_exp_ body), flags, (walk_ctx_ ctx))
    | ExpTryCatch(e, cases, ctx) ->
        ExpTryCatch((walk_exp_ e), (walk_cases_ cases), (walk_ctx_ ctx))
    | ExpMatch(e, cases, ctx) ->
        ExpMatch((walk_exp_ e), (walk_cases_ cases), (walk_ctx_ ctx))
    | ExpCast(e, t, ctx) -> ExpCast((walk_exp_ e), (walk_typ_ t), (walk_ctx_ ctx))
    | ExpTyped(e, t, ctx) -> ExpTyped((walk_exp_ e), (walk_typ_ t), (walk_ctx_ ctx))
    | ExpCCode(str, ctx) -> ExpCCode(str, (walk_ctx_ ctx))
    | DefVal(p, v, flags, loc) ->
        DefVal((walk_pat_ p), (walk_exp_ v), flags, loc)
    | DefFun(df) ->
        if !df.df_templ_args != [] then e else
        (let { df_args; df_typ; df_body } = !df in
        df := { !df with df_args=(walk_plist_ df_args); df_typ=(walk_typ_ df_typ);
                df_body=(walk_exp_ df_body) };
        e)
    | DefExn(de) ->
        let { dexn_typ } = !de in
        de := { !de with dexn_typ=(walk_typ_ dexn_typ) };
        e
    | DefTyp(dt) ->
        let { dt_typ } = !dt in
        dt := { !dt with dt_typ=(walk_typ_ dt_typ) };
        e
    | DefVariant(dvar) ->
        let { dvar_alias; dvar_cases } = !dvar in
        dvar := { !dvar with dvar_alias=(walk_typ_ dvar_alias);
                    dvar_cases=(List.map (fun (n, t) -> (n, walk_typ_ t)) dvar_cases) };
        e
    | DefClass(dc) -> (* [TODO] *) e
    | DefInterface(di) -> (* [TODO] *) e
    | DirImport (_, _) -> e
    | DirImportFrom (_, _, _) -> e
    | DirPragma (_, _) -> e)

and walk_pat p callb =
    let walk_typ_ t = check_n_walk_typ t callb in
    let walk_pat_ p = check_n_walk_pat p callb in
    let walk_pl_ p = check_n_walk_plist p callb in
    (match p with
    | PatAny _ -> p
    | PatLit _ -> p
    | PatIdent _ -> p
    | PatTuple(pl, loc) -> PatTuple((walk_pl_ pl), loc)
    | PatVariant(n, args, loc) -> PatVariant(n, (walk_pl_ args), loc)
    | PatRecord(n_opt, np_l, loc) ->
        PatRecord(n_opt, (List.map (fun (n, p) -> (n, (walk_pat_ p))) np_l), loc)
    | PatCons(p1, p2, loc) -> PatCons((walk_pat_ p1), (walk_pat_ p2), loc)
    | PatAs(p, n, loc) -> PatAs((walk_pat_ p), n, loc)
    | PatTyped(p, t, loc) -> PatTyped((walk_pat_ p), (walk_typ_ t), loc)
    | PatWhen(p, e, loc) -> PatWhen((walk_pat_ p), (check_n_walk_exp e callb), loc)
    | PatRef(p, loc) -> PatRef((walk_pat_ p), loc))

let rec dup_typ_ t callb =
    match t with
    | TypVar {contents=Some(t1)} ->
        TypVar (ref (Some(dup_typ_ t1 callb)))
    | TypVar _ -> TypVar (ref None)
    | TypRecord r ->
        let (relems, ordered) = !r in
        let new_relems = List.map (fun (n, t, v) -> (n, (dup_typ_ t callb), v)) relems in
        TypRecord(ref (new_relems, ordered))
    | _ -> walk_typ t callb

let rec dup_exp_ e callb =
    match e with
    | DefFun (r) -> walk_exp (DefFun(ref (!r))) callb
    | DefExn (r) -> walk_exp (DefExn(ref (!r))) callb
    | DefTyp (r) -> walk_exp (DefTyp(ref (!r))) callb
    | DefVariant (r) -> walk_exp (DefVariant(ref (!r))) callb
    | DefClass (r) -> walk_exp (DefClass(ref (!r))) callb
    | DefInterface (r) -> walk_exp (DefInterface(ref (!r))) callb
    | _ -> walk_exp e callb

let dup_callb =
{
    acb_typ = Some(dup_typ_);
    acb_exp = Some(dup_exp_);
    acb_pat = None;
}

let dup_typ t = dup_typ_ t dup_callb
let dup_exp e = dup_exp_ e dup_callb
let dup_pat p = walk_pat p dup_callb

let deref_typ_rec t =
    let deref_typ_rec_ t callb =
        let t = deref_typ t in
        walk_typ t callb
        in
    let deref_callb =
    {
        acb_typ = Some(deref_typ_rec_);
        acb_exp = None;
        acb_pat = None
    } in
    deref_typ_rec_ t deref_callb
