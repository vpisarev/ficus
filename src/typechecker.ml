(* The type checker *)

open Syntax

(*
The type checker component performs semantical analysis and various
sanity checks of just parsed Ficus module(s).

In theory, this stage, together with the previous lexical and syntactic analysis stages,
ensures that the code is valid and can be compiled to a valid C/machine code.
In practice, however, some checks are better to be postponed to further stages.
Nevertheless, the first 3 stages (lexer, parser and typechecker) are responsible
for reporting majority of the problems in the code.

These are the tasks performed by type checker:
  * infere the missing type specifications. In Ficus one must to explicitly define function argument types,
    however value/variable types, as well as function return types can be omitted, e.g.
    fun hypot(a: float, b: float) = sqrt(a*a + b*b)
  * find the proper match for each abstract symbol referenced in the program. This part deals with
    overloaded functions, generic functions, overriden symbols, etc. It also recognizes notation
    "Modulename.symbolname" and looks inside imported modules to find the proper (possibly overloaded) symbol.
  * as a part of the previous step, instantiate generic functions and types.
    Unlike some functional languages, such as OCaml, and similar to some other languages with generics, like C++,
    Ficus compiler deals with generic definitions only at the type checker stage. Later on, only the
    concrete instances, used by the rest of the code, survive.
    They are processed further and put into the final C code/binary.
  * make sure that explicitly specified and inferenced types do not contract and follow the language semantics.
    For example, condition in "if" statement must have type "bool",
    "then-" and "else-" branches must have the same type, etc.
  * do various sanity checks, e.g.
    ** a type with the same name cannot be redefined in the same scope,
    ** a function with the same name and same types of input parameters cannot be redefined in the same scope,
    ** there cannot be duplicated names in function arguments and in any other pattern.
    ** etc.
*)

(*
Implementation plan:

+ raise_typecheck_err()
+ type unification
+/--- checkexp
- checkexpseq(toplevel:bool)
- check_deffun
- check_defexn
- check_deftype
- check_pattern
- check_typespec
- instantiate_func
- instantiate_type
*)
exception TypeCheckError of loc_t * string

let raise_typecheck_err loc msg =
    Printf.printf "%s: %s\n" (loc2str loc) msg;
    raise (TypeCheckError(loc, msg))

(*
  Try to match (i.e. unify) two types, possibly indirectly represented or unknown/undefined.
  We use slightly extended type unification algorithm from min-caml.
  In fact, it's [almost] canonical type unification algorithm
  using so-called destructive unification. The only difference from
  the canonical algorithm is that we memorize all the destructive
  changes in unify_undo_stack, so that we can restore all the
  unsuccessfully matched types. Why do we need to recover from
  the unsuccessful unification?
  Because Ficus allows overloaded functions, so that we
  should iterate through a list of candidates and
  try to unify them one by one with the searched type.
  We only throw a type unification error
  (or rather "overloaded function not found" error)
  in the very end, when we are out of candidates.
*)
let maybe_unify t1 t2 =
    let unify_undo_stack = ref [] in

    (* checks if a reference to undefined type (an argument of TypVar (ref None))
       occurs in the other type (t2). If yes, then we have a cyclic dependency
       between types and so they cannot in principle be unified (i.e. no type
       cannot be unified with a part of it).
       In all other cases the undefined type can be unified with t2. *)
    let rec occurs r1 t2 =
        (match t2 with
        | TypFun(args2, rt2) -> List.exists (occurs r1) args2 || occurs r1 rt2
        | TypList(et2) -> occurs r1 et2
        | TypTuple(tl2) -> List.exists (occurs r1) tl2
        | TypRef(drt2) -> occurs r1 drt2
        | TypArray(_, et2) -> occurs r1 et2
        | TypVar(r2) when r1 == r2 -> true
        | TypVar({ contents = None }) -> false
        | TypVar({ contents = Some(t2_) }) -> occurs r1 t2_
        | _ -> false) in
    let rec maybe_unify_ t1 t2 =
        (match (t1, t2) with
        (* a type should be unified successfully with itself *)
        | (TypInt, TypInt) | (TypString, TypString) | (TypChar, TypChar)
        | (TypBool, TypBool) | (TypVoid, TypVoid) | (TypExn, TypExn)
        | (TypCPointer, TypCPointer) | (TypDecl, TypDecl) -> true
        | ((TypSInt bits1), (TypSInt bits2)) -> bits1 = bits2
        | ((TypUInt bits1), (TypUInt bits2)) -> bits1 = bits2
        | ((TypFloat bits1), (TypFloat bits2)) -> bits1 = bits2
        | ((TypFun (args1, rt1)), (TypFun (args2, rt2))) ->
            (List.for_all2 maybe_unify_ args1 args2) && (maybe_unify_ rt1 rt2)
        | ((TypList et1), (TypList et2)) -> maybe_unify_ et1 et2
        | ((TypTuple tl1), (TypTuple tl2)) -> List.for_all2 maybe_unify_ tl1 tl2
        | ((TypRef drt1), (TypRef drt2)) -> maybe_unify_ drt1 drt2
        | ((TypArray (d1, et1)), (TypArray (d2, et2))) ->
            (d1 < 0 || d2 < 0 || d1 = d2) && (maybe_unify_ et1 et2)
        | ((TypApp (args1, id1)), (TypApp (args2, id2))) ->
            id1 = id2 && (List.for_all2 maybe_unify_ args1 args2)
        (* unify TypVar(_) with another TypVar(_): consider several cases *)
        | ((TypVar r1), (TypVar r2)) when r1 == r2 -> true
        | ((TypVar {contents = Some(t1_)}), _) -> maybe_unify_ t1_ t2
        | (_, (TypVar {contents = Some(t2_)})) -> maybe_unify_ t1 t2_
        | ((TypVar ({contents = None} as r1)), _) ->
            if occurs r1 t2 then false else
            (* This is the destructive unification step:
               if there is unknown type t1 = TypVar(ref None), and it's not
               equivalent to t2, neither is a part of t2, then we unify it with t2.
               Before that, we update the undo stack. There is no need
               to memorize the previous value of r1, because we know that it's None. *)
            (unify_undo_stack := r1 :: !unify_undo_stack;
            r1 := Some(t2); true)
          (* symmetrical case *)
        | (_, (TypVar ({contents = None} as r2))) ->
            if occurs r2 t1 then false else
            (unify_undo_stack := r2 :: !unify_undo_stack;
            r2 := Some(t1); true)
        (* a declaration cannot be unified with any non-decl type *)
        | (TypDecl, _) | (_, TypDecl) -> false
        (* in all other cases the types cannot be unified *)
        | (_, _) -> false) in

    if maybe_unify_ t1 t2 then true else
    (* restore the original types in the case of unification failure *)
    (List.iter (fun r -> r := None) !unify_undo_stack; false)

(* this is another flavor of type unification function;
   it throws an exception in the case of failure *)
let unify t1 t2 loc msg =
    if maybe_unify t1 t2 then () else
    raise_typecheck_err loc msg

let coerce_types t1 t2 loc msg =
    match (t1, t2) with
    | (TypInt, TypInt) -> TypInt
    | ()

let rec check_exp e env sc =
    let (etyp, eloc) as ctx = get_exp_ctx e in
    match e with
    | ExpNop(_) -> unify etyp TypVoid eloc "nop must have void type"; (ExpNop(ctx), env)
    | ExpRange(e1_opt, e2_opt, e3_opt, _) ->
        let check_range_e e_opt =
            match e_opt with
            | None -> None
            | Some(e) ->
                let (new_e, _) = check_exp e env sc in
                let (etyp1, eloc1) = get_exp_ctx new_e in
                (unify etyp1 TypInt eloc1 "explicitly specified component of a range must be an integer";
                Some(new_e)) in
        let new_e1_opt = check_range_e e1_opt in
        let new_e2_opt = check_range_e e2_opt in
        let new_e3_opt = check_range_e e3_opt in
        (ExpRange(new_e1_opt, new_e2_opt, new_e3_opt, ctx), env)
    | ExpLit(lit, _) -> unify etyp (get_lit_type lit) eloc "the literal has improper type"; (e, env)
    | ExpIdent(n, _) ->
        (*
          [TODO]
          * look for the specific id "n" in the current environment.
          * If there is a single match, replace the id with the found one and unify the expression type
          with the type of id.
          * Otherwise, leave it as-is (???)
        *)
        (e, env)
    | ExpBinOp(bop, e1, e2, _)
        (*
          [TODO]
          check e1 and e2;
          then proceed depending on the op:

          +, -, *, /, %, ** - coerce the argument types.
              if the types are not primitive (numbers or tuples of numbers),
              then transform it to a function call and then check it as a function call.
          <<, >> - if the arguments are not integers, transform it to a function call.
          &, |, ^ - if the arguments are not integers, transform it to a function call.
                    these bitwise operations do not perform any type coercion.
                    small numeric constants can be implicitly cast to the proper type.
          &&, || - the arguments should be boolean, as well as the overal exp type
          >, >=, ==, !=, <=, < - the arguments should have the same type, the result will have bool type
          :: - the first argument should be unified with et=TypVar(ref None), the second with TypList(et);
               the overall expression - with TypList(et).
          = - the first argument should be a variable or array access operator. Unify it properly.
              The expression will have type TypVoid.
          . (OpDot) - it can be tuple access operator (or record access operator) or module access operator.
              if it's a tuple or record, find the proper field and unify the expression type with accordingly.
              if it's module access operator, try to find the proper match. If there are multiple possible matches,
              leave it as-is for now (???)
        *)
    | ExpUnOp(uop, e1, _)
        (*
          [TODO]
          check e1;
          then proceed depending on the op:

          OpNegate | OpBitwiseNot | OpLogicNot | OpMakeRef | OpDeref | OpThrow

          - - check that the argument is a number or a tuple of signed numbers.
              find out the result type by coercing arg type with itself.
          ~ - check that the argument is a number/bool or a tuple of numbers/bool's.
              the result will have the same type.
          ! - check that the argument is bool. the result will have type bool.
          ref - unify arg type with et = TypVar(ref None). unify exp type with TypRef(et).
          * (deref) - unify arg type with TypRef(etyp).
          throw - do not unify etyp with anything, leave it as-is.
        *)
    | ExpSeq(el, _)
        (*
          [TODO]
          run a separate function to check a sequence of expression (see below)
        *)
    | ExpMkTuple(el, _) ->
        let (new_el, tl) = List.fold_left
            (fun (new_el, tl) elem ->
                let (new_elem, _) = check_exp elem env sc in
                let (new_etyp, _) = get_exp_ctx new_elem in
                (new_elem :: new_el), (new_etyp :: tl)) ([], []) el) in
        unify (TypTuple (List.rev tl)) etyp eloc "the improper tuple type or the number of elements";
        (ExpMkTuple ((List.rev new_el), ctx), env)
    | ExpCall(f, args, _)
        (*
          [TODO]
          * unify f typ with TypFun(args_types, rt), where each of arg types and rt = TypVar(ref None)
            (but all the types are different)
          * then try to find the proper f (run check_exp)
          * then check and unify types of all the arg types.
          * then repeat the search of f if needed.
          * unify the expression type with "rt".
        *)
    | ExpAt(ae, idxs, _)
        (*
          [TODO]
          * consider a special "flatten" case: there is one idx and it's ":" range. Then
             ae should be unified with TypArray( *, et ) (where et = TypVar(ref None)) and
             the expression type is TypArray(1, et).
          * otherwise, unify ae with TypArray(dims, et), where dims is the number of idxs and et=TypVar(ref None)
          * check each index; it should either be an expression of type int or ExpRange(...)
          * if all the indices are int's when et should be unified with etyp.
          * otherwise the expression type is unified with TypArray(dims-scalar_dims, et),
             where scalar_dims is the number of scalar indices
        *)
    | ExpIf(c, e1, e2, _) ->
        (*
          [TODO]
          * check c, e1 and e2
          * unify c with bool
          * unify e1 and e2
          * unify etyp with e1
        *)
        let new_c = check_exp c env sc in
        let new_e1 = check_exp e1 env sc in
        let new_e2 = check_exp e2 env sc in
    | ExpWhile(c, body, _) ->
        let new_c = check_exp c env sc in
        let (new_ctyp, new_cloc) = get_exp_ctx new_c in
        let new_body = check_exp body env sc in
        let (new_btyp, new_bloc) = get_exp_ctx new_body in
        unify new_ctyp TypBool new_cloc "the while() loop condition should have boolean type";
        unify new_btyp TypVoid new_cloc "the while() loop body should have void type";
        unify etyp TypVoid eloc "the while() loop should have void type";
        (ExpWhile (new_c, new_body, ctx), env)
    | ExpFor() of forexp_t * ctx_t
        (*
          [TODO]
          * check each range of for loop; it should either be a range or some collection;
          * the pattern should take the appropriate type;
          * loop body should be unified with "void"
          * the whole loop should be unified with "void" as well
        *)
    | ExpTryCatch(try_e, handlers, _)
        (* [TODO] check try_e, check handlers, check that each handlers branch is unified with try_e;
           the overall trycatch should also be unified with try_e *)
    | ExpCast(e1, t1, _) ->
        (* [TODO] check that e1 can be cast to t1 *)
        let new_t1 = check_typ t1 env sc in
        let new_e1 = check_exp e1 env sc in
    | ExpTyped(e1, t1, _) ->
        let new_t1 = check_typ t1 env sc in
        let new_e1 = check_exp e1 env sc in
        let (new_etyp, new_eloc) = get_exp_ctx new_e1 in
        unify new_etyp new_t1 new_eloc "improper explicit type of the expression";
        unify etyp new_t1 new_eloc "improper explicit type of the expression";
        (ExpTyped (new_e1, new_t1), env)
    | ExpCCode(_, _) ->
        (match sc with
        | ScModule(_) -> ()
        | ScFun(_) -> ()
        | _ -> raise_typecheck_err eloc
            "ccode may be used only at the top (module level) or as a single expression in function definition")
    | DefVal(p, v, flags, _) ->
        (*
          [TODO]
          * check v and get its type
          * check the pattern p and unify it with v type
          * if it's not a class (check sc) then add p elements to the environment
          * unify etyp with TypDecl
        *)
    | DefFun({contents = {}} as rdf)
        (*
          [TODO]
          * the function prototype is analyzed separately in check_seq
          * check the function body, unify it with rt
        *)
    | DefExn({contents = {}} as rde)
        (*
          [TODO]
          * skip it; the exceptions are checked in check_seq
        *)
    | DefType({contents = {}} as rdt)
        (*
          [TODO]
          * skip it; the type definitions are checked in check_seq
        *)
    | DirImport(imp_list, _)
        (*
          [TODO]
          * skip it; import directives are checked in check_seq
        *)
    | DirImportFrom(m, imp_list, _)
        (*
          [TODO]
          * skip it; import directives are checked in check_seq
        *)

and check_seq eseq env sc =
    ...

and check_typ tp env sc =
    ...

and check_simple_pat pat env sc =
    ...
