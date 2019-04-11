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
    Ficus compiler deals with generic definitions only at the type checker stage. Later on, only
    concrete instances, used by the rest of the code, survive.
    They are processed further and put into the final C code/binary.
  * make sure that explicitly specified and inferenced types do not contradict each other
    and follow the language semantics.
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

(* [TODO] *)
let coerce_types t1 t2 loc msg =
    match (t1, t2) with
    | (TypInt, TypInt) -> TypInt
    | ()

let rec check_exp e env sc =
    let (etyp, eloc) as ctx = get_exp_ctx e in
    match e with
    | ExpNop(_) -> unify etyp TypVoid eloc "nop must have void type"; ExpNop(ctx)
    | ExpRange(e1_opt, e2_opt, e3_opt, _) ->
        let check_range_e e_opt =
            (match e_opt with
            | None -> None
            | Some(e) ->
                let new_e = check_exp e env sc in
                let (etyp1, eloc1) = get_exp_ctx new_e in
                (unify etyp1 TypInt eloc1 "explicitly specified component of a range must be an integer";
                Some(new_e))) in
        let new_e1_opt = check_range_e e1_opt in
        let new_e2_opt = check_range_e e2_opt in
        let new_e3_opt = check_range_e e3_opt in
        unify etyp (TypTuple [TypInt;TypInt;TypInt]) eloc "the range type should have (int, int, int) type";
        ExpRange(new_e1_opt, new_e2_opt, new_e3_opt, ctx)
    | ExpLit(lit, _) -> unify etyp (get_lit_type lit) eloc "the literal has improper type"; e
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
                let new_elem = check_exp elem env sc in
                let (new_etyp, _) = get_exp_ctx new_elem in
                (new_elem :: new_el), (new_etyp :: tl)) ([], []) el) in
        unify (TypTuple (List.rev tl)) etyp eloc "the improper tuple type or the number of elements";
        ExpMkTuple ((List.rev new_el), ctx)
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
    | ExpAt(arr, idxs, _)
        let new_arr = check_exp arr env sc in
        let (new_atyp, new_aloc) = get_exp_ctx new_arr in
        let et = make_new_typ() in
        (match idxs with
        (* flatten case "arr[:]" *)
        | ExpRange(None, None, None, _) :: [] ->
            unify new_atyp (TypArray(-1, et)) new_aloc "the argument of the flatten operation must be an array";
            unify etyp (TypArray(1, et)) eloc "the result of the flatten operation must be 1D array";
            ExpAt(new_arr, idxs, ctx)
        (* other cases: check each index, it should be either a scalar or a range;
           in the first case it should have integer type.
           If all the indices are scalars, then the result should have et type,
           otherwise it's an array of as high dimensionality as the number of range indices *)
        | _ ->
            let (new_idxs, nidx, nrange_idx) = List.fold_left (fun idx (new_idxs, nidx, nrange_idx) ->
                let new_idx = check_exp idx env sc in
                match new_idx with
                | ExpRange(_, _, _, _) -> (new_idx :: new_idxs, nidx+1, nrange_idx+1)
                | _ ->
                    let (new_ityp, new_iloc) = get_exp_ctx new_idx in
                    unify new_ityp TypInt new_iloc "each scalar index in array access op must be an integer";
                    (new_idx :: new_idxs, nidx+1, nrange_idx)) idxs in
            unify new_atyp (TypArray(nidx, et)) new_aloc "the array dimensionality does not match the number of indices";
            (if nrange_idx = 0 then
                unify etyp et eloc "the array access expression type does not match the array element type"
            else
                unify etyp (TypArray(nrange_idx, et)) eloc
                  "the number of ranges does not match dimensionality of the result, or the element type is incorrect");
            ExpAt(new_arr, (List.rev new_idxs), ctx))
    | ExpIf(c, e1, e2, _) ->
        let new_c = check_exp c env sc in
        let (new_ctyp, new_cloc) = get_exp_ctx new_c in
        let new_e1 = check_exp e1 env sc in
        let (new_typ1, new_loc1) = get_exp_ctx new_e1 in
        let new_e2 = check_exp e2 env sc in
        let (new_typ2, new_loc2) = get_exp_ctx new_e2 in
        unify new_ctyp TypBool new_cloc "if() condition should have boolean type";
        unify new_typ1 new_typ2 new_loc2 "if() then- and else- branches should have the same type";
        unify new_typ1 etyp eloc "if() expression should have the same type as its branches";
        ExpIf(new_c, new_e1, new_e2, ctx)
    | ExpWhile(c, body, _) ->
        let new_c = check_exp c env sc in
        let (new_ctyp, new_cloc) = get_exp_ctx new_c in
        let new_body = check_exp body env sc in
        let (new_btyp, new_bloc) = get_exp_ctx new_body in
        unify new_ctyp TypBool new_cloc "the while() loop condition should have boolean type";
        unify new_btyp TypVoid new_cloc "the while() loop body should have void type";
        unify etyp TypVoid eloc "the while() loop should have void type";
        ExpWhile (new_c, new_body, ctx)
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
        ExpTyped (new_e1, new_t1)
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

and check_seq eseq env sc create_sc =
    (*
      [TODO]
      * if create_sc == true, create new block scope
      * scan eseq, look for import statements. Since the modules are topologically sorted using the
        dependency criteria, all the imported modules should have been analyzed already.
        ** If we have "import m [as m_alias]",
           we add either "m":"real_m_id" or "m_alias":"real_m_id" to the environment.
           We also need to scan m's env and import all the overloaded operators.
        ** If we have "from m import <id_list>|*", we scan m's env and import the requested
           id's. We also add "m" to env (and import all the overloaded opeators).
      * scan eseq, look for type declarations. Give all the types fresh names,
        update the env, add the type declaration "headers" to the global symbol table.
        check that there are no other types with the same name in the same scope.
      * scan eseq again, now process all the type declaration bodies, resolve the types.
      * scan eseq, look for function definitions and exception definitions. Give all the
        exceptions and all the functions' fresh names, analyze their types. Since there can be
        mutually recursive functions with unspecified return types, we are not always able to
        figure out the return type at this point. So, some return types may be set to TypVar(ref None).
        For each exception we introduce a function (or a value for an exception without parameters)
        that constucts the exception. For each function definition we check that we do not have duplicates
        in the argument names. We also make sure that each argument has a type and it's not ambiguous.
        [probably, we should check whether there are other overloaded functions similar to the current one;
        if there are other functions in the same scope with the same types of input arguments, we should
        issue an error]
      * [TODO] when we add classes, if eseq is the class body, we'd need to scan it again
        to get all the members declarations and add them to env. This is needed because methods
        can access members that are declared after methods.
      * scan eseq one more time, process value definitions an functions bodies.
        ** For each value we introduce a fresh name, add it to env and to the global sym table.
        [if a value with the same name is defined in the same scope, we should issue an optional
        "value redefinition" warning (if it's a class scope, probably we should issue an error)]
        Before that, we need to analyze the pattern to handle cases like "val (x, y, _) = some_3d_point".
        Underscores in such cases are replaced with temporary id's. They will be excluded later on at the
        unused code elimination stage.
        ** For each function we add all the arguments to the env, form a new scope and check the body
        within this scope and using this special "inside-function" env.
      * return the final env. It's especially useful when processing modules (top-level definitions) and classes,
        because this env is then stored inside the module/class structure.
    *)

and check_typ tp env sc =
    (*
      [TODO]
      * leave simple types as-is
      * in the case of complex type (ref, array, tuple, list, fun, ...) process the nested types.
      * leave TypVar(ref None) as is
      * process t inside TypVar(ref Some(t))
      * the most sophisticated case: TypApp(args, id)
        ** process all the args
        ** look for id in the env; if there is type definition with the same id,
           1. check that the number of args is the same OR that there is 1-to-n or n-to-1 relationship.
              (that is, a type with a single arg can be instantiated using a tuple parameter,
              or a type with multiple args is instantiated with a single tuple argument
              (of as many elements as the number of parameters))
           2. if number of args is wrong, report an error
           3. otherwise, create a temporary environment where type arguments are replaced with the actual types.
              process the type definition body using this temporary environment.
           3.1. maybe in some cases before doing 3 need to iterate through a list of type instances and try to find
              the proper match. this step is optional for now, but it will become a mandatory when handling complex types,
              such as variants, classes and maybe records. It can also help to save some time and some space
    *)

and check_simple_pat pat tp env id_set sc =
    (*
      [TODO]
      * "_" should probably be replaced with a temporary id
      * ident should be checked for the absence of duplicates.
      *
    | PatIdent of id_t * loc_t
    | PatTuple of pat_t list * loc_t
    | PatCtor of id_t * pat_t list * loc_t
    | PatTyped of pat_t * type_t * loc_t
    *)
