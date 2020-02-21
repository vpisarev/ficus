(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    C code represented in a hierarhical form (just like Ast or K_form).

    The main differences from K-form are:
    * there is no nested functions; the functions are converted to closures and moved to the top level.
    * we add fx_ctx and also the closure pointer to the list of parameters of each function
      (i.e. the pointer to the structure that contains 'free variables':
      non-local and yet non-global variables accessed by the function).
      If some function does not need a closure, there is still such a parameter, but it'is not used.
      The parameter is needed because when we call a function indirectly,
      via pointer, we don't know whether it needs closure or not.
      ==
      [TODO] We can analyze the code and check whether we call some function only directly or not.
      If we call the function only directly and it does not need closure,
      we can eliminate the extra parameter. Or we can have a 'bare' variant of the function without
      the closure parameter, and use this variant when we call it directly; and then have
      a wrapper with the closure parameter which we use for indirect calls.
      ==
    * the type system is further shrinked:
      * tuples are converted to records (C structures).
      * for each complex type (where complex means "non-primitive",
        i.e. a list, reference, array, tuple, structure, variant etc.)
        a unique name (signature) is generated and is used to reference the type.
        For example, KTypList(KTypInt) becomes _fx_Li_t,
        KTypTuple(KTypFloat :: KTypFloat :: KTypFloat :: []) becomes _fx_Ta3f etc.
    * the memory is now managed manually.
      Reference counting is involved when copying and releasing the references to objects.
      Cleanup blocks are added to each function (including 'main') to try to reclaim
      the allocated yet unused memory.
    * all the data types are classified into 2 categories: dynamic and static.
      * Static types are allocated on stack.
        Those are primitive types (numbers, bool), tuples, records, non-recursive variants,
        arrays (their headers, not data) and strings (their headers and maybe data).
      * Dynamic types are allocated on heap and are referenced by their pointer.
        There is also a reference counter used to track down the number of 'users'
        sharing the pointer. The dynamic structures are lists, references and recursive variants.
      The situation is actually more complex than that:
        * The string characters may be allocated on the heap or
          stack (attached to the header), depending on the string size.
        * The array elements are always allocated on the heap and bundled with the reference counter.
        * Even the static types, when they are non-primitive (numbers, characters or booleans),
          are passed to functions via pointers. They all, except for arrays,
          are passed as 'const' pointers, e.g.
          int _fx_print_vec(fx_ctx_t* fx_ctx, const _fx_v3f_t* mytup) { ... }
        * Static data types may have fields that are represented by dynamic data types.
          For example, KTypTuple(KTypBool :: KTypList(KTypInt) :: KTypList(KTypInt) :: []).
    * Expression does not represent any element of the code anymore.
      There are now expressions and statements, just like in C/C++.
    * the complex (nested) expressions are re-introduced.
      This is needed to make the final C code more readable
      and to avoid eccessive use of temporary variables. For example,
      `foo((n+1)*2)` looks much better than
      `val t0 = n+1, t1=t0*2; foo(t1)`.
      Of course, the use of expressions is limited to the cases where
      no exceptions may occur when computing them.
    * there is no exceptions; after each function that may throw an exception
      (by itself or from within the nested calls) a error check is added.
      So far, we do not use 'zero-cost exceptions' or such. This is probably TBD.
    * all the multi-dimensional array access operations are converted to the raw 1D accesses
      with proper range checks where needed.
    * comprehensions are reduced to for-loops:
      * array comprehensions are replaced with for-loops over pre-allocated arrays;
      * list comprehensions are replaced with a for-loop that constructs the output list.
*)

open Ast
open K_form

type cbinop_t =
    | COpAdd | COpSub | COpMul | COpDiv | COpMod | COpShiftLeft | COpShiftRight
    | COpBitwiseAnd | COpBitwiseOr | COpBitwiseXor | COpLogicAnd | COpLogicOr
    | COpCompareEQ | COpCompareNE | COpCompareLT | COpCompareLE
    | COpCompareGT | COpCompareGE | COpArrayElem | COpAssign
    | COpAugAdd | COpAugSub | COpAugMul | COpAugDiv | COpAugMod
    | COpAugSHL | COpAugSHR | COpAugBitwiseAnd
    | COpAugBitwiseOr | COpAugBitwiseXor | COpMacroConcat

type cunop_t =
    | COpNegate | COpBitwiseNot | COpLogicNot | COpDeref | COpGetAddr
    | COpPrefixInc | COpPrefixDec | COpSuffixInc | COpSuffixDec | COpMacroName | COpMacroDefined

type ctyp_attr_t = CTypConst | CTypVolatile
type ctyp_kind_t =
    | CKindPrimitive (* the type is an integer or floating-point number, bool or char *)
    | CKindSimple (* the type is a tuple, record or non-recursive variant, where
                    all the elements are primitive or simple *)
    | CKindStruct (* the type is a tuple, record or non-recursive variant, which elements
                     may require a special handling *)
    | CKindSimpleCollection (* array, which elements are primitive or simple, or a string *)
    | CKindComplexCollection (* array, which elements are not simple *)
    | CKindSmartPointer (* the type, which instances are dynamically allocated and
            handled using smart pointers: list, reference, recursive variant, cptr *)

type ctyp_flag_t =
    | CTypFlagVariantNilCase of int (* if the type is a recursive variant and one of its cases has "void" type,
                                       e.g. type tree_t = Empty | None : (int, tree_t, tree_t),
                                       it makes sense to use a null pointer to identify this case
                                       reduce the amount of memory allocations
                                       (in the case of binary tree it's basically 2x reduction).
                                       We can call this variant case "nullable",
                                       and its index is stored with the flag *)
    | CTypFlagVariantHasTag (* indicates that the variant has a tag. Single-case variants do not need a tag.
                               Recursive variants with just 2 cases, where one of them is "nullable" (see above),
                               do not need a tag either. *)

type ctyp_t =
    | CTypInt (* this is a direct mapping from TypInt and CTypInt.
                It's ~ ptrdiff_t - a signed version of size_t, i.e.
                32-bit on 32-bit platforms, 64-bit on 64-bit platforms. *)
    | CTypCInt (* this is 'int' in C. It's almost always 32-bit *)
    | CTypSize_t
    | CTypSInt of int
    | CTypUInt of int
    | CTypFloat of int
    | CTypVoid
    | CTypNil
    | CTypBool
    | CTypWChar
    | CTypCSmartPointer
    | CTypString
    | CTypExn
    | CTypStruct of id_t option * (id_t * ctyp_t) list
    | CTypUnion of id_t option * (id_t * ctyp_t) list
    | CTypFun of ctyp_t list * ctyp_t
    | CTypFunPtr of ctyp_t list * ctyp_t
    | CTypRawPointer of ctyp_attr_t list * ctyp_t
    | CTypArray of int * ctyp_t
    | CTypName of id_t
    | CTypCName of string
    | CTypLabel
    | CTypAny
and cctx_t = ctyp_t * loc_t
and cexp_t =
    | CExpIdent of id_t * cctx_t
    | CExpLit of lit_t * cctx_t
    | CExpBinOp of cbinop_t * cexp_t * cexp_t * cctx_t
    | CExpUnOp of cunop_t * cexp_t * cctx_t
    | CExpMem of cexp_t * id_t * cctx_t
    | CExpArrow of cexp_t * id_t * cctx_t
    | CExpCast of cexp_t * ctyp_t * loc_t
    | CExpTernary of cexp_t * cexp_t * cexp_t * cctx_t
    | CExpCall of cexp_t * cexp_t list * cctx_t
    | CExpSeq of cexp_t list * cctx_t (* this is to pass a sequence of expressions to a macro *)
and cstmt_t =
    | CStmtNop of loc_t
    | CComment of string * loc_t
    | CExp of cexp_t
    | CStmtBreak of loc_t
    | CStmtContinue of loc_t
    | CStmtReturn of cexp_t option * loc_t
    | CStmtBlock of cstmt_t list * loc_t
    | CStmtIf of cexp_t * cstmt_t * cstmt_t * loc_t
    | CStmtGoto of id_t * loc_t
    | CStmtLabel of id_t * loc_t
    | CStmtFor of cexp_t list * cexp_t option * cexp_t list * cstmt_t * loc_t
    | CStmtWhile of cexp_t * cstmt_t * loc_t
    | CStmtDoWhile of cstmt_t * cexp_t * loc_t
    (* we don't parse and don't process the inline C code; just retain it as-is *)
    | CStmtCCode of string * loc_t
    | CDefVal of ctyp_t * id_t * cexp_t option * loc_t
    | CDefFun of cdeffun_t ref
    | CDefTyp of cdeftyp_t ref
    | CDefForwardFun of id_t * loc_t
    | CDefForwardTyp of id_t * loc_t
    | CDefEnum of cdefenum_t ref
    | CMacroDef of cdefmacro_t ref
    | CMacroUndef of id_t * loc_t
    (* this is not universal representation of the conditional macro directives,
       because they do not have to follow the code structure,
       but it's probably good enough for our purposes *)
    | CMacroIf of (cexp_t * cstmt_t list) list * cstmt_t list * loc_t
    | CMacroInclude of string * loc_t
and cdefval_t = { cv_name: id_t; cv_typ: ctyp_t; cv_cname: string; cv_flags: val_flag_t list;
                  cv_arrdata: id_t; cv_scope: scope_t list; cv_loc: loc_t }
and cdeffun_t = { cf_name: id_t; cf_typ: ctyp_t; cf_cname: string;
                  cf_args: id_t list; cf_body: cstmt_t;
                  cf_flags: fun_flag_t list; cf_scope: scope_t list; cf_loc: loc_t }
and cdeftyp_t = { ct_name: id_t; ct_typ: ctyp_t; ct_ktyp: ktyp_t; ct_cname: string;
                  ct_kind: ctyp_kind_t; ct_flags: ctyp_flag_t list;
                  ct_make: id_t; ct_free: id_t; ct_copy: id_t;
                  ct_scope: scope_t list; ct_loc: loc_t }
and cdefenum_t = { ce_name: id_t; ce_members: (id_t * cexp_t option) list; ce_cname: string;
                   ce_scope: scope_t list; ce_loc: loc_t }
and cdeflabel_t = { cl_name: id_t; cl_cname: string; cl_scope: scope_t list; cl_loc: loc_t }
and cdefmacro_t = { cm_name: id_t; cm_cname: string; cm_args: id_t list; cm_body: cstmt_t list;
                    cm_scope: scope_t list; cm_loc: loc_t }

type cinfo_t =
    | CNone | CText of string | CVal of cdefval_t | CFun of cdeffun_t ref
    | CTyp of cdeftyp_t ref | CEnum of cdefenum_t ref | CLabel of cdeflabel_t
    | CMacro of cdefmacro_t ref

let all_idcs = dynvec_create CNone

let new_idc_idx() =
    let new_idx = dynvec_push all_ids in
    let new_kidx = dynvec_push K_form.all_idks in
    let new_cidx = dynvec_push all_idcs in
    if new_idx = new_kidx && new_idx = new_cidx then new_idx else
        failwith "internal error: unsynchronized outputs from new_id_idx(), new_idk_idx() and new_idc_idx()"

let cinfo i = dynvec_get all_idcs (id2idx i)

let gen_temp_idc s =
    let i_name = get_id_prefix s in
    let i_real = new_idc_idx() in
    Id.Temp(i_name, i_real)

let dup_idc old_id =
    let k = new_idc_idx() in
    match old_id with
    | Id.Name(i) -> Id.Val(i, k)
    | Id.Val(i, j) -> Id.Val(i, k)
    | Id.Temp(i, j) -> Id.Temp(i, k)

let set_idc_entry i n =
    let idx = id2idx i in dynvec_set all_idcs idx n

let init_all_idcs () =
    dynvec_init all_idcs K_form.all_idks.dynvec_count

let get_cexp_ctx e = match e with
    | CExpIdent(_, c) -> c
    | CExpLit(_, c) -> c
    | CExpBinOp(_, _, _, c) -> c
    | CExpUnOp(_, _, c) -> c
    | CExpMem(_, _, c) -> c
    | CExpArrow(_, _, c) -> c
    | CExpCast(_, t, l) -> (t, l)
    | CExpTernary(_, _, _, c) -> c
    | CExpCall(_, _, c) -> c
    | CExpSeq(_, c) -> c

let get_cexp_typ e = let (t, l) = (get_cexp_ctx e) in t
let get_cexp_loc e = let (t, l) = (get_cexp_ctx e) in l

let get_cstmt_loc s = match s with
    | CStmtNop l -> l
    | CComment (_, l) -> l
    | CExp e -> get_cexp_loc e
    | CStmtBreak l -> l
    | CStmtContinue l -> l
    | CStmtReturn (_, l) -> l
    | CStmtBlock (_, l) -> l
    | CStmtIf (_, _, _, l) -> l
    | CStmtGoto (_, l) -> l
    | CStmtLabel (_, l) -> l
    | CStmtFor (_, _, _, _, l) -> l
    | CStmtWhile (_, _, l) -> l
    | CStmtDoWhile (_, _, l) -> l
    | CStmtCCode (_, l) -> l
    | CDefVal (_, _, _, l) -> l
    | CDefFun {contents={cf_loc}} -> cf_loc
    | CDefTyp {contents={ct_loc}} -> ct_loc
    | CDefForwardFun (_, cff_loc) -> cff_loc
    | CDefForwardTyp (_, cft_loc) -> cft_loc
    | CDefEnum {contents={ce_loc}} -> ce_loc
    | CMacroDef {contents={cm_loc}} -> cm_loc
    | CMacroUndef (_, l) -> l
    | CMacroIf (_, _, l) -> l
    | CMacroInclude (_, l) -> l

let get_cscope info =
    match info with
    | CNone -> ScGlobal :: []
    | CText _ -> ScGlobal :: []
    | CVal {cv_scope} -> cv_scope
    | CFun {contents = {cf_scope}} -> cf_scope
    | CTyp {contents = {ct_scope}} -> ct_scope
    | CEnum {contents = {ce_scope}} -> ce_scope
    | CLabel {cl_scope} -> cl_scope
    | CMacro {contents={cm_scope}} -> cm_scope

let get_cinfo_loc info =
    match info with
    | CNone | CText _ -> noloc
    | CVal {cv_loc} -> cv_loc
    | CFun {contents = {cf_loc}} -> cf_loc
    | CTyp {contents = {ct_loc}} -> ct_loc
    | CEnum {contents = {ce_loc}} -> ce_loc
    | CLabel {cl_loc} -> cl_loc
    | CMacro {contents={cm_loc}} -> cm_loc

let get_idc_loc i = get_cinfo_loc (cinfo i)

let check_cinfo info i loc =
    match info with
    | CNone -> raise_compile_err loc (sprintf "attempt to request type of non-existing symbol '%s'" (id2str i))
    | CText s -> raise_compile_err loc (sprintf "attempt to request type of symbol '%s'" s)
    | _ -> ()

let get_cinfo_typ info i loc =
    check_cinfo info i loc;
    match info with
    | CNone -> CTypNil
    | CText _ -> CTypNil
    | CVal {cv_typ} -> cv_typ
    | CFun {contents = {cf_typ}} -> cf_typ
    | CTyp {contents = {ct_typ}} -> ct_typ
    | CMacro {contents = {cm_args}} ->
        (match cm_args with
        | [] -> CTypAny
        | _ -> CTypFun((List.map (fun a -> CTypAny) cm_args), CTypAny))
    | CLabel _ -> CTypLabel
    | CEnum _ -> CTypCInt

let get_idc_typ i loc = get_cinfo_typ (cinfo i) i loc

let get_idc_cname i = match (cinfo i) with
    | CNone -> ""
    | CText _ -> ""
    | CVal {cv_cname} -> cv_cname
    | CFun {contents = {cf_cname}} -> cf_cname
    | CTyp {contents = {ct_cname}} -> ct_cname
    | CLabel {cl_cname} -> cl_cname
    | CEnum {contents = {ce_cname}} -> ce_cname
    | CMacro {contents = {cm_cname}} -> cm_cname

(* used by the type checker *)
let get_lit_ctyp l = match l with
    | LitInt(_) -> CTypInt
    | LitSInt(b, _) -> CTypSInt(b)
    | LitUInt(b, _) -> CTypUInt(b)
    | LitFloat(b, _) -> CTypFloat(b)
    | LitString(_) -> CTypString
    | LitChar(_) -> CTypWChar
    | LitBool(_) -> CTypBool
    | LitNil -> CTypNil

let create_cdefval n t flags e_opt code sc loc =
    let dv = { cv_name=n; cv_typ=t; cv_cname=""; cv_flags=flags; cv_arrdata=noid; cv_scope=sc; cv_loc=loc } in
    match t with
    | CTypVoid -> raise_compile_err loc "values of `void` type are not allowed"
    | _ -> ();
    set_idc_entry n (CVal dv);
    (CDefVal(t, n, e_opt, loc)) :: code

let get_ccode_loc ccode default_loc =
    loclist2loc (List.map get_cstmt_loc ccode) default_loc

let code2stmt code loc =
    match code with
    | [] -> CStmtNop(loc)
    | s :: [] -> s
    | _ ->
        let final_loc = get_ccode_loc code loc in
        CStmtBlock(code, final_loc)

let filter_out_nops code =
    List.filter (fun s -> match s with
        | CStmtNop _ -> false
        | _ -> true) code

let rcode2stmt code loc = match (filter_out_nops code) with
    | [] -> CStmtNop loc
    | s :: [] -> s
    | _ ->
        let final_loc = get_ccode_loc code loc in
        CStmtBlock((List.rev code), final_loc)

let stmt2code s =
    match s with
    | CStmtNop _ -> []
    | CStmtBlock(slist, _) -> slist
    | _ -> s :: []

(* walk through a C-form and produce another one *)

type c_callb_t =
{
    ccb_ident: (id_t -> c_callb_t -> id_t) option;
    ccb_typ: (ctyp_t -> c_callb_t -> ctyp_t) option;
    ccb_exp: (cexp_t -> c_callb_t -> cexp_t) option;
    ccb_stmt: (cstmt_t -> c_callb_t -> cstmt_t) option;
}

let rec check_n_walk_ident n callb =
    match callb.ccb_ident with
    | Some(f) -> f n callb
    | _ -> n

and check_n_walk_ctyp t callb =
    match callb.ccb_typ with
    | Some(f) -> f t callb
    | _ -> walk_ctyp t callb

and check_n_walk_cexp e callb =
    match callb.ccb_exp with
    | Some(f) -> f e callb
    | _ -> walk_cexp e callb

and check_n_walk_cstmt s callb =
    match callb.ccb_stmt with
    | Some(f) -> f s callb
    | _ -> walk_cstmt s callb

and walk_ctyp t callb =
    let walk_id_ n = check_n_walk_ident n callb in
    let walk_id_opt_ n_opt = match n_opt with Some n -> Some (walk_id_ n) | _ -> None in
    let walk_ctyp_ t = check_n_walk_ctyp t callb in
    (match t with
    | CTypInt | CTypCInt | CTypSInt _ | CTypUInt _ | CTypFloat _
    | CTypSize_t | CTypVoid | CTypNil | CTypBool | CTypExn | CTypAny
    | CTypWChar | CTypCSmartPointer | CTypString -> t
    | CTypStruct (n_opt, selems) ->
        CTypStruct((walk_id_opt_ n_opt), (List.map (fun (n, t) -> ((walk_id_ n), (walk_ctyp_ t))) selems))
    | CTypUnion (n_opt, uelems) ->
        CTypUnion((walk_id_opt_ n_opt), (List.map (fun (n, t) -> ((walk_id_ n), (walk_ctyp_ t))) uelems))
    | CTypFun (args, rt) -> CTypFun((List.map walk_ctyp_ args), (walk_ctyp_ rt))
    | CTypFunPtr (args, rt) -> CTypFunPtr((List.map walk_ctyp_ args), (walk_ctyp_ rt))
    | CTypArray(d, et) -> CTypArray(d, walk_ctyp_ et)
    | CTypRawPointer(attrs, t) -> CTypRawPointer(attrs, (walk_ctyp_ t))
    | CTypName n -> CTypName(walk_id_ n)
    | CTypCName _ -> t
    | CTypLabel -> t)

and walk_cexp e callb =
    let walk_id_ n = check_n_walk_ident n callb in
    let walk_ctyp_ t = check_n_walk_ctyp t callb in
    let walk_cexp_ e = check_n_walk_cexp e callb in
    let walk_ctx_ (t, loc) = ((walk_ctyp_ t), loc) in
    (match e with
    | CExpIdent (n, ctx) -> CExpIdent((walk_id_ n), (walk_ctx_ ctx))
    | CExpLit (lit, ctx) -> CExpLit(lit, (walk_ctx_ ctx))
    | CExpBinOp (bop, e1, e2, ctx) -> CExpBinOp(bop, (walk_cexp_ e1), (walk_cexp_ e2), (walk_ctx_ ctx))
    | CExpUnOp (uop, e, ctx) -> CExpUnOp(uop, (walk_cexp_ e), (walk_ctx_ ctx))
    (* we exclude the second arguments of CExpMem/CExpArrow from the traversal procedure,
       because they are not real id's; they are just symbolic representation of the accessed record fields *)
    | CExpMem (e, m, ctx) -> CExpMem((walk_cexp_ e), m, (walk_ctx_ ctx))
    | CExpArrow (e, m, ctx) -> CExpArrow((walk_cexp_ e), m, (walk_ctx_ ctx))
    | CExpCast (e, t, loc) -> CExpCast((walk_cexp_ e), (walk_ctyp_ t), loc)
    | CExpTernary (e1, e2, e3, ctx) -> CExpTernary((walk_cexp_ e1), (walk_cexp_ e2), (walk_cexp_ e3), (walk_ctx_ ctx))
    | CExpCall (f, args, ctx) -> CExpCall((walk_cexp_ f), (List.map walk_cexp_ args), (walk_ctx_ ctx))
    | CExpSeq (eseq, ctx) -> CExpSeq((List.map walk_cexp_ eseq), (walk_ctx_ ctx)))

and walk_cstmt s callb =
    let walk_id_ n = check_n_walk_ident n callb in
    let walk_ctyp_ t = check_n_walk_ctyp t callb in
    let walk_cexp_ e = check_n_walk_cexp e callb in
    let walk_cel_ el = List.map walk_cexp_ el in
    let walk_cstmt_ s = check_n_walk_cstmt s callb in
    let walk_csl_ sl = List.map walk_cstmt_ sl in
    let walk_cexp_opt_ e_opt = match e_opt with
        | Some e -> Some (check_n_walk_cexp e callb)
        | _ -> e_opt in
    match s with
    | CStmtNop _ -> s
    | CComment _ -> s
    | CExp e -> CExp (walk_cexp_ e)
    | CStmtBreak _ -> s
    | CStmtContinue _ -> s
    | CStmtReturn (e_opt, l) -> CStmtReturn ((walk_cexp_opt_ e_opt), l)
    | CStmtBlock (sl, l) -> CStmtBlock ((walk_csl_ sl), l)
    | CStmtIf (e, s1, s2, l) -> CStmtIf ((walk_cexp_ e), (walk_cstmt_ s1), (walk_cstmt_ s2), l)
    | CStmtGoto (n, l) -> CStmtGoto ((walk_id_ n), l)
    | CStmtLabel (n, l) -> CStmtLabel ((walk_id_ n), l)
    | CStmtFor (e1, e2_opt, e3, body, l) ->
        CStmtFor((walk_cel_ e1), (walk_cexp_opt_ e2_opt), (walk_cel_ e3), (walk_cstmt_ body), l)
    | CStmtWhile (e, body, l) ->
        CStmtWhile((walk_cexp_ e), (walk_cstmt_ body), l)
    | CStmtDoWhile (body, e, l) ->
        CStmtDoWhile((walk_cstmt_ body), (walk_cexp_ e), l)
    | CStmtCCode (ccode, l) -> s
    | CDefVal (t, n, e_opt, l) -> CDefVal((walk_ctyp_ t), (walk_id_ n), (walk_cexp_opt_ e_opt), l)
    | CDefFun cf ->
        let { cf_name; cf_typ; cf_args; cf_body } = !cf in
        cf := { !cf with
            cf_name = (walk_id_ cf_name);
            cf_typ = (walk_ctyp_ cf_typ);
            cf_args = (List.map walk_id_ cf_args);
            cf_body = (walk_cstmt_ cf_body) };
        s
    | CDefTyp ct ->
        let { ct_name; ct_typ } = !ct in
        ct := { !ct with
            ct_name = (walk_id_ ct_name);
            ct_typ = (walk_ctyp_ ct_typ) };
        s
    | CDefForwardFun (n, loc) ->
        CDefForwardFun (walk_id_ n, loc)
    | CDefForwardTyp (n, loc) ->
        CDefForwardTyp (walk_id_ n, loc)
    | CDefEnum ce ->
        let { ce_name; ce_members } = !ce in
        ce := { !ce with
            ce_name = (walk_id_ ce_name);
            ce_members = (List.map (fun (n, e_opt) -> ((walk_id_ n), (walk_cexp_opt_ e_opt))) ce_members) };
        s
    | CMacroDef cm ->
        let { cm_name; cm_args; cm_body } = !cm in
        cm := { !cm with cm_name = (walk_id_ cm_name);
            cm_args = (List.map walk_id_ cm_args);
            cm_body = (List.map walk_cstmt_ cm_body) };
        s
    | CMacroUndef (n, l) -> CMacroUndef((walk_id_ n), l)
    | CMacroIf (cs_l, else_l, l) ->
        CMacroIf((List.map (fun (c, sl) -> ((walk_cexp_ c), (walk_csl_ sl))) cs_l), (walk_csl_ else_l), l)
    | CMacroInclude (_, l) -> s

(* walk through a K-normalized syntax tree and perform some actions;
   do not construct/return anything (though, it's expected that
   the callbacks collect some information about the tree) *)

type 'x c_fold_callb_t =
{
    ccb_fold_ident: (id_t -> 'x c_fold_callb_t -> unit) option;
    ccb_fold_typ: (ctyp_t -> 'x c_fold_callb_t -> unit) option;
    ccb_fold_exp: (cexp_t -> 'x c_fold_callb_t -> unit) option;
    ccb_fold_stmt: (cstmt_t -> 'x c_fold_callb_t -> unit) option;
    mutable ccb_fold_result: 'x;
}

let rec check_n_fold_ctyp t callb =
    match callb.ccb_fold_typ with
    | Some(f) -> f t callb
    | _ -> fold_ctyp t callb

and check_n_fold_cexp e callb =
    match callb.ccb_fold_exp with
    | Some(f) -> f e callb
    | _ -> fold_cexp e callb

and check_n_fold_cstmt s callb =
    match callb.ccb_fold_stmt with
    | Some(f) -> f s callb
    | _ -> fold_cstmt s callb

and check_n_fold_id n callb =
    match callb.ccb_fold_ident with
    | Some(f) -> f n callb
    | _ -> ()

and fold_ctyp t callb =
    let fold_ctyp_ t = check_n_fold_ctyp t callb in
    let fold_tl_ tl = List.iter fold_ctyp_ tl in
    let fold_id_ i = check_n_fold_id i callb in
    let fold_id_opt_ i_opt = match i_opt with Some(i) -> check_n_fold_id i callb | _ -> () in
    (match t with
    | CTypInt | CTypCInt | CTypSInt _ | CTypUInt _ | CTypFloat _
    | CTypSize_t | CTypVoid | CTypNil | CTypBool | CTypExn | CTypAny
    | CTypWChar | CTypString | CTypCSmartPointer -> ()
    | CTypStruct (n_opt, selems) ->
        fold_id_opt_ n_opt; List.iter (fun (n, t) -> fold_id_ n; fold_ctyp_ t) selems
    | CTypUnion (n_opt, uelems) ->
        fold_id_opt_ n_opt; List.iter (fun (n, t) -> fold_id_ n; fold_ctyp_ t) uelems
    | CTypFun (args, rt) -> fold_tl_ args; fold_ctyp_ rt
    | CTypFunPtr (args, rt) -> fold_tl_ args; fold_ctyp_ rt
    | CTypRawPointer(_, t) -> fold_ctyp_ t
    | CTypArray(_, t) -> fold_ctyp_ t
    | CTypName n -> fold_id_ n
    | CTypCName _ -> ()
    | CTypLabel -> ())

and fold_cexp e callb =
    let fold_ctyp_ t = check_n_fold_ctyp t callb in
    let fold_id_ i = check_n_fold_id i callb in
    let fold_cexp_ e = check_n_fold_cexp e callb in
    let fold_ctx_ (t, _) = fold_ctyp_ t in
    fold_ctx_ (match e with
    | CExpIdent (n, ctx) -> fold_id_ n; ctx
    | CExpLit (_, ctx) -> ctx
    | CExpBinOp (_, e1, e2, ctx) -> fold_cexp_ e1; fold_cexp_ e2; ctx
    | CExpUnOp (_, e, ctx) -> fold_cexp_ e; ctx
    | CExpMem (e, _, ctx) -> fold_cexp_ e; ctx
    | CExpArrow (e, _, ctx) -> fold_cexp_ e; ctx
    | CExpCast (e, t, loc) -> fold_cexp_ e; (t, loc)
    | CExpTernary (e1, e2, e3, ctx) -> fold_cexp_ e1; fold_cexp_ e2; fold_cexp_ e3; ctx
    | CExpCall (f, args, ctx) -> fold_cexp_ f; List.iter fold_cexp_ args; ctx
    | CExpSeq (eseq, ctx) -> List.iter fold_cexp_ eseq; ctx)

and fold_cstmt s callb =
    let fold_cstmt_ s = check_n_fold_cstmt s callb in
    let fold_csl_ sl = List.iter fold_cstmt_ sl in
    let fold_ctyp_ t = check_n_fold_ctyp t callb in
    let fold_id_ k = check_n_fold_id k callb in
    let fold_cexp_ e = check_n_fold_cexp e callb in
    let fold_cel_ el = List.iter fold_cexp_ el in
    let fold_cexp_opt_ e_opt = match e_opt with
        | Some e -> fold_cexp_ e
        | _ -> () in
    match s with
    | CStmtNop _ -> ()
    | CComment _ -> ()
    | CExp e -> fold_cexp_ e
    | CStmtBreak _ -> ()
    | CStmtContinue _ -> ()
    | CStmtReturn (e_opt, _) -> fold_cexp_opt_ e_opt
    | CStmtBlock (sl, _) -> fold_csl_ sl
    | CStmtIf (e, s1, s2, _) -> fold_cexp_ e; fold_cstmt_ s1; fold_cstmt_ s2
    | CStmtGoto (n, _) -> fold_id_ n
    | CStmtLabel (n, _) -> fold_id_ n
    | CStmtFor (e1, e2_opt, e3, body, _) ->
        fold_cel_ e1; fold_cexp_opt_ e2_opt; fold_cel_ e3; fold_cstmt_ body
    | CStmtWhile (e, body, _) ->
        fold_cexp_ e; fold_cstmt_ body
    | CStmtDoWhile (body, e, _) ->
        fold_cstmt_ body; fold_cexp_ e
    | CStmtCCode (ccode, _) -> ()
    | CDefVal (t, n, e_opt, _) ->
        fold_ctyp_ t; fold_id_ n; fold_cexp_opt_ e_opt
    | CDefFun cf ->
        let { cf_name; cf_typ; cf_args; cf_body } = !cf in
        fold_id_ cf_name; fold_ctyp_ cf_typ;
        List.iter fold_id_ cf_args; fold_cstmt_ cf_body
    | CDefTyp ct ->
        let { ct_name; ct_typ } = !ct in
        fold_id_ ct_name; fold_ctyp_ ct_typ
    | CDefForwardFun (n, _) ->
        fold_id_ n
    | CDefForwardTyp (n, _) ->
        fold_id_ n
    | CDefEnum ce ->
        let { ce_name; ce_members } = !ce in
        fold_id_ ce_name;
        List.iter (fun (n, e_opt) -> fold_id_ n; fold_cexp_opt_ e_opt) ce_members
    | CMacroDef cm ->
        let { cm_name; cm_args; cm_body } = !cm in
        fold_id_ cm_name; List.iter fold_id_ cm_args; List.iter fold_cstmt_ cm_body
    | CMacroUndef (n, _) -> fold_id_ n
    | CMacroIf (cs_l, else_l, _) ->
        List.iter (fun (c, sl) -> fold_cexp_ c; fold_csl_ sl) cs_l;
        fold_csl_ else_l
    | CMacroInclude (_, l) -> ()

let make_ptr t = CTypRawPointer([], t)
let make_const_ptr t = CTypRawPointer((CTypConst :: []), t)
let make_int_exp i loc = CExpLit ((LitInt i), (CTypInt, loc))
let make_id_exp i loc = let t = get_idc_typ i loc in CExpIdent(i, (t, loc))
let make_cid prefix ctyp e_opt code sc loc =
    let n = gen_temp_idc prefix in
    let code = create_cdefval n ctyp (ValMutable :: []) e_opt code sc loc in
    (n, code)

let std_CTypVoidPtr = make_ptr CTypVoid
let std_CTypConstVoidPtr = make_const_ptr CTypVoid
let std_CTypAnyArray = CTypArray(0, CTypInt)
let std_CTypAnyPtr = make_ptr CTypAny
let std_CTypConstAnyPtr = make_const_ptr CTypAny

let make_cfor_inc i ityp a b delta body loc =
    let i_exp = CExpIdent(i, (ityp, loc)) in
    let e0 = CExpBinOp(COpAssign, i_exp, a, (ityp, loc)) in
    let e1 = CExpBinOp(COpCompareLT, i_exp, b, (CTypBool, loc)) in
    let e2 = CExpUnOp(COpSuffixInc, i_exp, (ityp, loc)) in
    CStmtFor ((e0 :: []), (Some e1), (e2 :: []), body, loc)

let std_fx_free_elem_t = CTypCName "fx_free_elem_t"
let std_fx_copy_elem_t = CTypCName "fx_copy_elem_t"

let curr_exn_val = ref (-1024)
let std_FX_MAX_DIMS = 5

let std_fx_alloc = ref noid
let std_fx_free = ref noid

let std_FX_CALL = ref noid
let std_FX_COPY_PTR = ref noid
let std_FX_COPY_SIMPLE = ref noid
let std_FX_NO_FREE = ref noid

let std_fx_free_str = ref noid
let std_fx_copy_str = ref noid

let std_FX_THROW_LIGHT = ref noid
let std_FX_FREE_EXN = ref noid
let std_FX_COPY_EXN = ref noid
let std_FX_EXN_MAKE_IMPL = ref noid
let std_fx_free_exn = ref noid
let std_fx_copy_exn = ref noid

let std_FX_LIST_FREE_IMPL = ref noid
let std_FX_LIST_MAKE_IMPL = ref noid

let std_FX_CHKIDX_xD = ref ([] : id_t list)
let std_FX_EPTR_xD_ = ref ([] : id_t list)
let std_FX_EPTR_xD = ref ([] : id_t list)

let std_fx_free_arr = ref noid
let std_fx_copy_arr = ref noid
let std_fx_make_arr = ref noid
let std_fx_make_arrxd = ref ([] : id_t list)

let std_FX_REF_FREE_IMPL = ref noid
let std_FX_REF_MAKE_IMPL = ref noid

let std_FX_FREE_FP = ref noid
let std_FX_COPY_FP = ref noid

let std_fx_free_cptr = ref noid
let std_fx_copy_cptr = ref noid

let make_call f args rt loc =
    let f_exp = make_id_exp f loc in
    CExpCall(f_exp, args, (rt, loc))

let make_ccall_catch f args catch_label loc =
    let f_call = make_call f args CTypCInt loc in
    let cl_exp = CExpIdent(catch_label, (CTypLabel, loc)) in
    make_call !std_FX_CALL (f_call :: cl_exp :: []) CTypVoid loc
