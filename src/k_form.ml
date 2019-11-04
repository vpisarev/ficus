(*
  K-normal form (or K-form in short) definition.
  This is a greatly extended variant of K-normal form in min-caml:
  https://github.com/esumii/min-caml.

  Similarly to ficus AST, which is defined in ast.ml,
  K-form is an hierarchical (tree-like) representation of
  the compiled code. However, it's much simpler and more
  suitable for intermediate optimizations and then for
  translation to some even lower-level representation, e.g. C code.

  In particular:

  * all the symbols in K-form are resolved and unique, e.g:
      fun foo(i: int) { val i = i+1; val i = i*2; for(i<-0:i) println(i) }
    is transformed to something like
      fun foo@999(i@1000: int): int {
        val i@1001: int = i@1000+1; val i@1002: int = i@1001*2
        for ((i@1003:int) <- 0:i@1002) println@56<int->void>(i@1003)
      }
  * all the symbols have known type. If it cannot be figured out,
    type checker or the k-form generator (see k_norm.ml) report compile error.
  * all complex expressions are broken down into sequences of basic operations
    with intermediate results stored in temporary values.
  * records are converted to tuples
  * pattern matching is converted into a sequences of nested if-expressions
  * import directives are removed; we've already resolved all the symbols
  * generic types and functions are removed. Their instances, generated
    by type checker, are retained though.
  * ...
*)
open Ast

(* key_t for K-normal forms is the same thing as id_t for AST (see ast.ml).
   Why do we need a different entity?
   This is to make sure that we do not misuse id_t and its respective functions (dup_id, get_id etc.).
   During generation of the K-form and its further transformations we may introduce
   extra temporary values for which we do not have counterparts in the original AST.
   After dead code elimination we may want to renumerate the remaining id's to improve
   the cache locality, so that the indices in the global symbol table will not match
   the original symbol table. With the new type we will get a compile error if we
   try to use id_t after K-normalization. *)
module Key = struct
    type t = Name of int * int | Temp of int * int

    let compare a b =
        let a_idx = match (a) with Name(_, idx) -> idx | Temp(_, idx) -> idx in
        let b_idx = match (b) with Name(_, idx) -> idx | Temp(_, idx) -> idx in
        if a_idx < b_idx then -1 else if a_idx > b_idx then 1 else 0
end

type key_t = Key.t

(* it looks like OCaml generates some default compare operation for the types,
   so we do not have to define it explicitly *)
module Atom = struct
    type t = Val of key_t | Lit of lit_t
end

type atom_t = Atom.t

module Domain = struct
    type t = Elem of atom_t | Fast of atom_t | Range of atom_t * atom_t * atom_t
end

type dom_t = Domain.t

type kscope_t = KScBlock of int | KScFun of id_t | KScClass of id_t
            | KScInterface of id_t | KScModule of id_t | KScGlobal

type ktyp_t =
    | KTypInt
    | KTypSInt of int
    | KTypUInt of int
    | KTypFloat of int
    | KTypVoid
    | KTypNil
    | KTypBool
    | KTypChar
    | KTypString
    | KTypCPointer
    | KTypFun of ktyp_t list * ktyp_t
    | KTypTuple of ktyp_t list
    | KTypName of key_t
    | KTypArray of int * ktyp_t
    | KTypList of ktyp_t
    | KTypRef of ktyp_t
    | KTypExn
    | KTypErr
and kctx_t = ktyp_t * loc_t
and kexp_t =
    | KExpNop of loc_t
    | KExpBreak of loc_t
    | KExpContinue of loc_t
    | KExpAtom of atom_t * kctx_t
    | KExpUnOp of un_op_t * atom_t * kctx_t
    | KExpBinOp of bin_op_t * atom_t * atom_t * kctx_t
    | KExpSeq of kexp_t list * kctx_t
    | KExpIf of kexp_t list * kexp_t * kexp_t * kctx_t
    | KExpCall of key_t * atom_t list * kctx_t
    | KExpMkTuple of atom_t list * kctx_t
    | KExpMkArray of int list * atom_t list * kctx_t
    | KExpAt of atom_t * dom_t list * bool * kctx_t
    | KExpMem of key_t * int * bool * kctx_t
    | KExpDeref of key_t * bool * kctx_t
    | KExpMakeRef of atom_t * kctx_t
    | KExpAssign of key_t * atom_t * loc_t
    | KExpTry of kexp_t * kexp_t * kctx_t
    | KExpThrow of key_t * loc_t
    | KExpCast of atom_t * ktyp_t * kctx_t
    | KExpMap of (kexp_t * (key_t * dom_t) list) list * kexp_t * for_flag_t list * kctx_t
    | KExpFor of (key_t * dom_t) list * kexp_t * for_flag_t list * loc_t
    | KExpWhile of kexp_t * kexp_t * loc_t
    | KExpDoWhile of kexp_t * kexp_t * loc_t
    | KExpCCode of string * kctx_t
    | KDefVal of key_t * kexp_t * val_flag_t list * loc_t
    | KDefFun of kdeffun_t ref
    | KDefExn of kdefexn_t ref
    | KDefVariant of kdefvariant_t ref
and kdefval_t = { kv_name: key_t; kv_typ: ktyp_t; kv_flags: val_flag_t list; kv_scope: kscope_t list; kv_loc: loc_t }
and kdeffun_t = { kf_name: key_t; kf_typ: ktyp_t; kf_args: key_t list; kf_body: kexp_t; kf_flags: fun_flag_t list; kf_scope: kscope_t list; kf_loc: loc_t }
and kdefexn_t = { ke_name: key_t; ke_typ: ktyp_t; ke_scope: kscope_t list; ke_loc: loc_t }
and kdefvariant_t = { kvar_name: key_t; kvar_cases: (key_t * ktyp_t) list; kvar_constr: key_t list;
                      kvar_flags: variant_flag_t list; kvar_scope: kscope_t list; kvar_loc: loc_t }

type kinfo_t =
    | KNone | KText of string | KVal of kdefval_t | KFun of kdeffun_t ref
    | KExn of kdefexn_t ref | KVariant of kdefvariant_t ref

let all_nkeys = ref 0
let all_keys : kinfo_t array ref = ref [||]
let all_kstrings: (string, int) Hashtbl.t = Hashtbl.create 1000

let sprintf = Printf.sprintf
let printf = Printf.printf

let new_key_idx() =
    let _ = if (Array.length !all_keys) <= !all_nkeys then
        let delta_nkeys = max !all_nkeys 128 in
        let new_keys = Array.make delta_nkeys KNone in
        all_keys := Array.append !all_keys new_keys
    else () in
    let i = !all_nkeys in
    (all_nkeys := !all_nkeys + 1; i)

let dump_key i = match i with Key.Name(i, j) -> (sprintf "Key.Name(%d, %d)" i j)
                | Key.Temp(i, j) -> (sprintf "Key.Temp(%d, %d)" i j)
let key2idx i = match i with Key.Name(_, i_real) -> i_real | Key.Temp(_, i_real) -> i_real
let key2str_ i pp =
    let (temp, prefix, suffix) =
        match i with
        | Key.Name(i_name, i_real) -> (false, i_name, i_real)
        | Key.Temp(i_prefix, i_real) -> (true, i_prefix, i_real) in
    let s = (match (!all_keys).(prefix) with
        | KText(s) -> s
        | _ -> failwith (sprintf
            "The first element of id=%s does not represent a string\n" (dump_key i))) in
    if temp then (sprintf "%s@@%d" s suffix) else
    if pp || prefix = suffix then s else (sprintf "%s@%d" s suffix)

let key2str i = key2str_ i false
let pp_key2str i = key2str_ i true

let kinfo i = (!all_keys).(key2idx i)

let get_key_ s =
    let idx =
    (match Hashtbl.find_all all_kstrings s with
    | x :: _ -> x
    | _ -> let i = new_key_idx() in
            (Hashtbl.add all_kstrings s i;
            (!all_keys).(i) <- KText(s);
            i)) in idx

let get_key s =
    let i = get_key_ s in Key.Name(i, i)

let get_temp_key s =
    let i_name = get_key_ s in
    let i_real = new_key_idx() in
    Key.Temp(i_name, i_real)

let dup_key old_key =
    let k = new_key_idx() in
    match old_key with
    | Key.Name(i, j) -> Key.Name(i, k)
    | Key.Temp(i, j) -> Key.Temp(i, k)

let get_orig_key i =
    match i with
    | Key.Name(i, j) -> Key.Name(i, i)
    | Key.Temp(_, _) -> i

let set_key_entry i n =
    let idx = key2idx i in (!all_keys).(idx) <- n

let get_kexp_ctx e = match e with
    | KExpNop(l) -> (KTypVoid, l)
    | KExpBreak(l) -> (KTypVoid, l)
    | KExpContinue(l) -> (KTypVoid, l)
    | KExpAtom(_, c) -> c
    | KExpUnOp(_, _, c) -> c
    | KExpBinOp(_, _, _, c) -> c
    | KExpSeq(_, c) -> c
    | KExpIf(_, _, _, c) -> c
    | KExpCall(_, _, c) -> c
    | KExpMkTuple(_, c) -> c
    | KExpMkArray(_, _, c) -> c
    | KExpAt(_, _, _, c) -> c
    | KExpMem(_, _, _, c) -> c
    | KExpDeref(_, _, c) -> c
    | KExpMakeRef(_, c) -> c
    | KExpAssign(_, _, l) -> (KTypVoid, l)
    | KExpTry(_, _, c) -> c
    | KExpThrow(_, l) -> (KTypErr, l)
    | KExpCast(_, _, c) -> c
    | KExpMap(_, _, _, c) -> c
    | KExpFor(_, _, _, l) -> (KTypVoid, l)
    | KExpWhile(_, _, l) -> (KTypVoid, l)
    | KExpDoWhile(_, _, l) -> (KTypVoid, l)
    | KExpCCode(_, c) -> c
    | KDefVal (_, _, _, l) -> (KTypVoid, l)
    | KDefFun {contents={kf_loc}} -> (KTypVoid, kf_loc)
    | KDefExn {contents={ke_loc}} -> (KTypVoid, ke_loc)
    | KDefVariant {contents={kvar_loc}} -> (KTypVoid, kvar_loc)

let get_kexp_typ e = let (t, l) = (get_kexp_ctx e) in t
let get_kexp_loc e = let (t, l) = (get_kexp_ctx e) in l

let block_kscope_idx = ref (-1)
let new_block_kscope () =
    block_kscope_idx := !block_kscope_idx + 1;
    KScBlock !block_kscope_idx
let rec kscope2str sc =
    match sc with
    | KScBlock b :: r -> (sprintf "block(%d)." b) ^ (kscope2str r)
    | KScFun f :: r -> (sprintf "fun(%s)." (id2str f)) ^ (kscope2str r)
    | KScClass c :: r -> (sprintf "class(%s)." (id2str c)) ^ (kscope2str r)
    | KScInterface i :: r -> (sprintf "interface(%s)." (id2str i)) ^ (kscope2str r)
    | KScModule m :: r -> (sprintf "mod(%s)." (id2str m)) ^ (kscope2str r)
    | KScGlobal :: r -> "global." ^ (kscope2str r)
    | [] -> ""

let get_kscope info = match info with
    | KNone -> KScGlobal :: []
    | KText _ -> KScGlobal :: []
    | KVal {kv_scope} -> kv_scope
    | KFun {contents = {kf_scope}} -> kf_scope
    | KExn {contents = {ke_scope}} -> ke_scope
    | KVariant {contents = {kvar_scope}} -> kvar_scope

let get_kinfo_loc info = match info with
    | KNone | KText _ -> noloc
    | KVal {kv_loc} -> kv_loc
    | KFun {contents = {kf_loc}} -> kf_loc
    | KExn {contents = {ke_loc}} -> ke_loc
    | KVariant {contents = {kvar_loc}} -> kvar_loc

let get_key_loc i = get_kinfo_loc (kinfo i)

let get_kinfo_ktyp info = match info with
    | KNone -> failwith "attempt to request type of non-existing symbol"
    | KText s -> failwith (sprintf "attempt to request type of symbol '%s'" s)
    | KVal {kv_typ} -> kv_typ
    | KFun {contents = {kf_typ}} -> kf_typ
    | KExn {contents = {ke_typ}} -> ke_typ
    | KVariant {contents = {kvar_name}} -> KTypName(kvar_name)

let get_key_ktyp i = get_kinfo_ktyp (kinfo i)

(* used by the type checker *)
let get_lit_ktyp l = match l with
    | LitInt(_) -> KTypInt
    | LitSInt(b, _) -> KTypSInt(b)
    | LitUInt(b, _) -> KTypUInt(b)
    | LitFloat(b, _) -> KTypFloat(b)
    | LitString(_) -> KTypString
    | LitChar(_) -> KTypChar
    | LitBool(_) -> KTypBool
    | LitNil -> KTypNil
