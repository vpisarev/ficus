/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

///////////////////////// The type checker //////////////////////

from Ast import *
import Options

/*
The type checker component performs semantical analysis and various
sanity checks of just parsed Ficus module(s).

In theory, this stage, together with the previous lexical and syntactic analysis stages,
ensures that the code is valid, typesafe and can be compiled to a valid C/machine code.
In practice, however, some checks are better to be postponed to further stages.
Nevertheless, the first 3 stages (lexer, parser and the typechecker) are responsible
for reporting majority of the problems in the code.

These are the tasks performed by type checker:
  * infere the missing type specifications. In Ficus one must explicitly define function argument types
    and may also optionally specify function return types, types of the declared values/variables and
    even types of some expressions using (exp: type) syntax (e.g. (None: int option)). Unop completion of
    the type checker stage, each symbol is given the proper (and the only possible) type.
    When the type cannot be inferenced, an error is reported by the type checker or at the subsequent
    K-normalization step.
  * find the proper match for each abstract symbol referenced in the program. This part deals with
    overloaded functions, generic functions, overriden symbols, etc. It also recognizes '.' notation,
    such as "Modulename.symbolname" or "record_instance_name.record_field" and looks inside imported modules
    or records to find the proper match.
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
*/

/*val pprint_typ_x = Ast_pp.pprint_typ_x
val pprint_exp_x = Ast_pp.pprint_exp_x
val pprint_pat_x = Ast_pp.pprint_pat_x
fun print_env(msg, env, loc) {
    printf("%s. env at %s [\n", msg, loc2str(loc))
    Env.iter(
        fun (k, entries) {
            printf("\t%s:", id2str(k))
            for e <- entries {
                match e {
                | EnvId(n) =>
                    printf(" %s", id2str(n))
                    val templ_inst =
                    match id_info(n) {
                    | IdFun (ref {df_templ_inst}) => n :: df_templ_inst
                    | IdVariant (ref {dvar_templ_inst}) => n :: dvar_templ_inst
                    | _ => []
                    }
                    if templ_inst == [] {}
                    else {
                        for ni@ni <- templ_inst {
                            val t =
                            match id_info(ni) {
                            | IdFun (ref {df_typ}) => df_typ
                            | IdVariant (ref {dvar_alias}) => dvar_alias
                            | IdTyp (ref {dt_typ}) => dt_typ
                            | _ => TypVar(ref None)
                            }
                            printf("%s%s<", if i == 0 {
                                                " => ["
                                } else {
                                    ", "
                                }, id2str(ni))
                            pprint_typ_x(t, loc)
                            printf(">")
                        }
                        printf("]")
                    }
                | EnvTyp(t) => printf("<some type>")
                }
            }
            printf(";\n")
        },
        env)
    printf("]\n")
}*/

/*
  Try to match (i.e. unify) two types,
  possibly indirectly represented or unknown/undefined.
  We use slightly extended type unification algorithm from min-caml.
  In fact, it's [almost] canonical type unification algorithm
  using so-called destructive unification. The only difference from
  the canonical algorithm is that we memorize all the destructive
  changes in unify_undo_stack, so that we can restore all the
  unsuccessfully matched types. Why do we need to recover from
  the unsuccessful unification?
  This is because Ficus allows overloaded functions, so that we
  should iterate through a list of candidates and
  try to unify them one by one with the searched type.
  We only throw a type unification error
  (or rather "overloaded function not found" error)
  in the very end, when we are out of candidates.
*/
type rec_elem_t = (id_t, typ_t, lit_t?)
type rec_data_t = (rec_elem_t list, bool)

fun maybe_unify(t1: typ_t, t2: typ_t, update_refs: bool) {
    var undo_stack: (typ_t? ref, typ_t?) list = []
    var rec_undo_stack: (rec_data_t ref, rec_data_t) list = []
    /* checks if a reference to undefined type (an argument of TypVar (ref None))
       occurs in the other type (t2). If yes, then we have a cyclic dependency
       between types and so they cannot in principle be unified (i.e. no type
       cannot be unified with a part of it).
       In all other cases the undefined type can be unified with t2. */
    fun occurs(r1: typ_t? ref, t2: typ_t): bool =
        match t2 {
        | TypFun(args2, rt2) => occurs(r1, args2) || occurs(r1, rt2)
        | TypList(t2_) => occurs(r1, t2_)
        | TypTuple(tl2) => occurs(r1, tl2)
        | TypVarTuple(Some(t2_)) => occurs(r1, t2_)
        | TypVarTuple(_) => false
        | TypRef(t2_) => occurs(r1, t2_)
        | TypArray(_, et2) => occurs(r1, et2)
        | TypVarArray(et2) => occurs(r1, et2)
        | TypRecord (ref (relems2, _)) => occurs(r1, relems2)
        | TypVar(r2) when r1 == r2 => true
        | TypVar((ref None)) => false
        | TypVar((ref Some(t2_))) => occurs(r1, t2_)
        | TypApp(tl2, _) => occurs(r1, tl2)
        | TypInt | TypSInt(_) | TypUInt(_) | TypFloat(_) | TypString
        | TypChar | TypBool | TypVoid | TypExn | TypErr
        | TypCPointer | TypDecl | TypModule | TypVarRecord =>
            false
        }
    fun occurs(r1: typ_t? ref, tl: typ_t list): bool = tl.exists(fun (t: typ_t) {occurs(r1, t)})
    fun occurs(r1: typ_t? ref, relems: rec_elem_t list): bool = relems.exists(fun (r: rec_elem_t) {occurs(r1, r.1)})

    fun maybe_unify_(tl1: typ_t list, tl2: typ_t list) =
        tl1.length() == tl2.length() &&
        all(for t1 <- tl1, t2 <- tl2 {maybe_unify_(t1, t2)})

    fun maybe_unify_(t1: typ_t, t2: typ_t): bool =
        match (t1, t2) {
        | (TypInt, TypInt) | (TypString, TypString) | (TypChar, TypChar) | (TypBool, TypBool) | (TypVoid, TypVoid)
        | (TypExn, TypExn) | (TypCPointer, TypCPointer) | (TypDecl, TypDecl) | (TypModule, TypModule) => true
        | (TypSInt(bits1), TypSInt(bits2)) => bits1 == bits2
        | (TypUInt(bits1), TypUInt(bits2)) => bits1 == bits2
        | (TypFloat(bits1), TypFloat(bits2)) => bits1 == bits2
        | (TypFun(args1, rt1), TypFun(args2, rt2)) =>
            maybe_unify_(args1, args2) && maybe_unify_(rt1, rt2)
        | (TypList(et1), TypList(et2)) => maybe_unify_(et1, et2)
        | (TypTuple(tl1), TypTuple(tl2)) => maybe_unify_(tl1, tl2)
        | (TypVar((ref Some(TypVarTuple(t1_opt))) as r1), TypVar((ref Some(TypVarTuple(t2_opt))) as r2)) =>
            if r1 == r2 { true }
            else if occurs(r2, t1) || occurs(r1, t2) { false }
            else {
                val ok = match (t1_opt, t2_opt) {
                    | (Some(t1_), Some(t2_)) => maybe_unify_(t1_, t2_)
                    | _ => true
                    }
                if ok {
                    undo_stack = (r2, *r2) :: undo_stack
                    *r2 = Some(t1)
                }
                ok
            }
        | (TypVar (ref Some(TypVarTuple(_))), TypVar (ref Some(t2_))) => maybe_unify_(t2_, t1)
        | (TypVar (ref Some(TypVarTuple(_))), TypTuple(_)) => maybe_unify_(t2, t1)
        | (TypTuple(tl1), TypVar((ref Some(TypVarTuple(t2_opt))) as r2)) =>
            if exists(for t <- tl1 {occurs(r2, t)}) { false }
            else {
                val ok = match t2_opt {
                    | Some(t2_) => tl1.all(fun (t: typ_t) {maybe_unify_(t2_, t)})
                    | _ => true
                }
                if ok {
                    undo_stack = (r2, *r2) :: undo_stack
                    *r2 = Some(t1)
                }
                ok
            }
        | (TypVar((ref Some(TypVarRecord)) as r1), t2) =>
            val t2 = deref_typ(t2)
            match t2 {
            | TypVar((ref Some(TypVarRecord)) as r2) =>
                if r1 == r2 {}
                else { undo_stack = (r2, *r2) :: undo_stack; *r2 = Some(t1) }
                true
            | TypVar((ref None) as r2) =>
                undo_stack = (r2, *r2) :: undo_stack
                *r2 = Some(t1)
                true
            | TypRecord(_) =>
                undo_stack = (r1, *r1) :: undo_stack
                *r1 = Some(t2)
                true
            | TypApp(t_args, tn) =>
                match id_info(tn) {
                | IdVariant (ref (defvariant_t {dvar_cases=(_, TypRecord(_)) :: []})) =>
                    undo_stack = (r1, *r1) :: undo_stack
                    *r1 = Some(t2)
                    true
                | _ => false
                }
            | _ => false
            }
        | (_, TypVar (ref Some(TypVarRecord))) => maybe_unify_(t2, t1)
        | (TypRef(drt1), TypRef(drt2)) => maybe_unify_(drt1, drt2)
        | (TypRecord(r1), TypRecord(r2)) when r1 == r2 => true
        | (TypRecord (ref (_, false)), TypRecord (ref (_, true))) => maybe_unify_(t2, t1)
        | (TypRecord(r1), TypRecord(r2)) =>
            val ok = match (*r1, *r2) {
                | ((relems1, true), (relems2, true)) =>
                    relems1.length() == relems2.length()
                        &&
                    List.all2(relems1, relems2,
                        fun ((n1, t1, _): rec_elem_t, (n2, t2, _): rec_elem_t) {
                            n1 == n2 && maybe_unify_(t1, t2) })
                | ((relems1, _), (relems2, _)) =>
                    val have_all_matches = relems1.all(
                        fun ((n1, t1, v1opt): rec_elem_t) {
                            relems2.exists(fun ((n2, t2, _): rec_elem_t) {
                                n1 == n2 && maybe_unify_(t1, t2)}) || v1opt.issome()}
                        )
                    /*
                        if both the record types are unknown then all the v1opt's in relems1
                        are None's. Since we do not have duplicates, which is checked by the parser,
                        then if for each record field in relems1 we have a match in relems2
                        then len(relems2) >= len(relems1).
                        So, below we put the opposite check,
                        which ensures that len(relems1) = len(relems2).
                    */
                    have_all_matches && relems1.length() >= relems2.length()
                }
            if ok {
                rec_undo_stack = (r2, *r2) :: rec_undo_stack
                *r2 = *r1
            }
            ok
        | (TypArray(d1, et1), TypArray(d2, et2)) => d1 == d2 && maybe_unify_(et1, et2)
        | (TypVar((ref Some(TypVarArray(t1_))) as r1), TypVar((ref Some(TypVarArray(t2_))) as r2)) =>
            if r1 == r2 { true }
            else if occurs(r2, t1) || occurs(r1, t2) { false }
            else if maybe_unify_(t1_, t2_) { undo_stack = (r2, *r2) :: undo_stack; *r2 = Some(t1); true }
            else { false }
        | (TypVar (ref Some(TypVarArray(_))), TypVar (ref Some(t2_))) => maybe_unify_(t2_, t1)
        | (TypVar (ref Some(TypVarArray(_))), TypArray(_, _)) => maybe_unify_(t2, t1)
        | (TypArray(d1, et1), TypVar((ref Some(TypVarArray(et2))) as r2)) =>
            if occurs(r2, et1) { false }
            else if maybe_unify_(et1, et2) { undo_stack = (r2, *r2) :: undo_stack; *r2 = Some(t1); true }
            else { false }
        | (TypApp(args1, id1), TypApp(args2, id2)) =>
            if id1 != id2 { false }
            else {
                val t1 = match args1 { | [] => TypVoid | t :: [] => t | _ => TypTuple(args1) }
                val t2 = match args2 { | [] => TypVoid | t :: [] => t | _ => TypTuple(args2) }
                maybe_unify_(t1, t2)
            }
        /* unify TypVar(_) with another TypVar(_): consider several cases */
        | (TypVar(r1), TypVar(r2)) when r1 == r2 => true
        | (TypVar (ref Some(t1_)), _) => maybe_unify_(t1_, t2)
        | (_, TypVar (ref Some(t2_))) => maybe_unify_(t1, t2_)
        | (TypVar((ref None) as r1), _) =>
            if occurs(r1, t2) { false }
            /* This is the destructive unification step:
               if there is unknown type t1 = TypVar(ref None), and it's not
               equivalent to t2, neither is a part of t2, then we unify it with t2.
               Before that, we update the undo stack. There is no need
               to memorize the previous value of r1, because we know that it's None. */
            else {
                match t2 {
                | TypErr => {}
                | _ => undo_stack = (r1, *r1) :: undo_stack; *r1 = Some(t2)
                }
                true
            }
        /* symmetrical case */
        | (_, TypVar((ref None) as r2)) =>
            if occurs(r2, t1) { false }
            else {
                match t1 {
                | TypErr => {}
                | _ => undo_stack = (r2, *r2) :: undo_stack; *r2 = Some(t1)
                }
                true
            }
        /* 'throw e' expression has pseudo-type TypErr, which shall be unified successfully with any other type:
            [TODO]: probably when unifying TypErr with TypVar, the latter should not be updated */
        | (TypErr, _) => true
        | (_, TypErr) => true
        /* a declaration cannot be unified with any non-decl type */
        | (TypDecl, _) | (_, TypDecl) => false
        /* in all other cases the types cannot be unified */
        | (_, _) => false
        }

    val ok = maybe_unify_(t1, t2)
    if ok && update_refs {}
    else {
        /* restore the original types in the case of type unification failure
           or when update_refs=false */
        for (r, prev) <- rec_undo_stack { *r = prev }
        for (r, old_val) <- undo_stack { *r = old_val }
    }
    ok
}

/* this is another flavor of type unification function;
   it throws an exception in the case of failure */
fun unify(t1: typ_t, t2: typ_t, loc: loc_t, msg: string): void =
    if maybe_unify(t1, t2, true) {}
    else { throw compile_err(loc, msg) }

fun coerce_types(t1: typ_t, t2: typ_t, allow_tuples: bool, allow_fp: bool, is_shift: bool, loc: loc_t) {
    val safe_max_ubits = if Options.opt.arch64 { 32 } else { 16 }
    fun coerce_types_(t1, t2) =
        match (t1, t2) {
        | (TypInt, TypInt) => TypInt
        | (TypSInt(b1), TypSInt(b2)) =>
            val b = max(b1, b2)
            if b <= 32 { TypInt } else { TypSInt(64) }
        | (TypUInt(b1), TypUInt(b2)) =>
            val b = max(b1, b2)
            if b <= safe_max_ubits { TypInt } else { TypUInt(b) }
        | (TypInt, TypSInt(b)) =>
            if b <= 32 { TypInt } else { TypSInt(b) }
        | (TypInt, TypUInt(b)) =>
            if b <= safe_max_ubits { TypInt }
            else { throw compile_err(loc, "implicit type coercion for (int, uint32/uint64) pair is not allowed; use explicit type cast") }
        | (TypSInt(b), TypInt) =>
            if is_shift { TypSInt(b) } else if b <= 32 { TypInt } else { TypSInt(b) }
        | (TypUInt(b), TypInt) =>
            if is_shift { TypUInt(b) } else if b <= safe_max_ubits { TypInt } else {
                throw compile_err(loc, "implicit type coercion for (int, uint32/uint64) pair is not allowed; use explicit type cast")
            }
        | (TypSInt(b1), TypUInt(b2)) =>
            if b1 <= 32 && b2 <= safe_max_ubits { TypInt } else {
                throw compile_err(loc, "implicit type coercion for this (signed, unsigned) pair of integer is not allowed; use explicit type cast")
            }
        | (TypUInt(b1), TypSInt(b2)) =>
            if b1 <= safe_max_ubits && b2 <= 32 { TypInt } else {
                throw compile_err(loc, "implicit type coercion for this (unsigned, signed) pair of integer is not allowed; use explicit type cast")
            }
        | (TypFloat(b1), TypFloat(b2)) when allow_fp => val max_b = max(max(b1, b2), 32); TypFloat(max_b)
        | (TypFloat(b), TypInt) when allow_fp => TypFloat(max(b, 32))
        | (TypFloat(b), TypSInt(_)) when allow_fp => TypFloat(max(b, 32))
        | (TypFloat(b), TypUInt(_)) when allow_fp => TypFloat(max(b, 32))
        | (TypInt, TypFloat(b)) when allow_fp => TypFloat(max(b, 32))
        | (TypSInt(_), TypFloat(b)) when allow_fp => TypFloat(max(b, 32))
        | (TypUInt(_), TypFloat(b)) when allow_fp => TypFloat(max(b, 32))
        | (TypBool, TypBool) => TypInt
        | (TypBool, t2) => coerce_types_(TypInt, t2)
        | (t1, TypBool) => coerce_types_(t1, TypInt)
        | (TypTuple(tl1), TypTuple(tl2)) =>
            if !allow_tuples {
                throw compile_err(loc, "tuples are not allowed in this operation")
            } else if tl1.length() == tl2.length() {
                TypTuple([: for et1 <- tl1, et2 <- tl2 { coerce_types_(et1, et2) } :])
            } else {
                throw compile_err(loc, "tuples have different number of elements")
            }
        | _ => throw compile_err(loc, "the types cannot be implicitly coerced; use explicit type cast")
        }

    try {
        Some(coerce_types_(deref_typ(t1), deref_typ(t2)))
    } catch { | CompileError(_, _) => None }
}

fun find_all(n: id_t, env: env_t): env_entry_t list =
    match env.find_opt(n) {
    | Some(l) => l
    | _ => []
    }

fun typ2constr(t: typ_t, rt: typ_t, loc: loc_t): typ_t =
    match t {
    | TypVoid => rt
    | _ => TypFun(match t {
            | TypTuple(tl) => tl
            | _ => t :: [] }, rt)
    }

fun get_eseq_typ(eseq: exp_t list): typ_t {
    | [] => TypVoid
    | _ => get_exp_typ(eseq.last())
    }

fun add_typ_to_env(key: id_t, t: typ_t, env: env_t) {
    val entries = find_all(key, env)
    env.add(key, EnvTyp(t) :: entries)
}

fun add_id_to_env(key: id_t, i: id_t, env: env_t) {
    val entries = find_all(key, env)
    val entries = [: for e <- entries { if e == EnvId(i) {continue}; e } :]
    env.add(key, EnvId(i) :: entries)
}

fun add_id_to_env_check(key: id_t, i: id_t, env: env_t, check: id_info_t->void) {
    val entries = find_all(key, env)
    val entries = [: for e <- entries {
        match e {
        | EnvId(j) =>
            if i == j { continue }
            check(id_info(j))
        | _ => {}
        }
        e } :]
    env.add(key, EnvId(i) :: entries)
}

fun inst_merge_env(env_from: env_t, env_to: env_t): env_t =
    env_from.foldl(
        fun (i: id_t, entries: env_entry_t list, new_env: env_t) {
            match env_to.find_opt(i) {
            | Some(prev_entries) =>
                val fold extra_entries = [] for e <- entries {
                    match e {
                    | EnvId(i) =>
                        val add = match id_info(i) { | IdFun(_) => true | _ => false }
                        if add && !prev_entries.mem(e) {
                            e :: extra_entries
                        } else {
                            extra_entries
                        }
                    | _ => extra_entries
                    }
                }
                /* add only those symbols from env_from which also exist in env_to;
                   the goal is not to implement dynamic scope; the goal is to make
                   function instantiation work correctly in the presense of
                   locally overloaded functions */
                if extra_entries.empty() {
                    new_env
                } else {
                    val new_entries = extra_entries.rev() + prev_entries
                    new_env.add(i, new_entries)
                }
            | _ => new_env
            }
        },
        env_to)

fun check_for_duplicate_typ(key: id_t, sc: scope_t list, loc: loc_t) =
    fun (i: id_info_t) {
        | IdTyp(_) | IdVariant(_) | IdClass(_) | IdInterface(_) =>
            if get_scope(i).hd() == sc.hd() {
                throw compile_err( loc,
                    f"the type {pp_id2str(key)} is re-declared in the same scope; the previous declaration is here {get_idinfo_loc(i)}")
            }
        | _ => {}
    }

fun check_for_rec_field_duplicates(rfnames: id_t list, loc: loc_t): void =
    for ni@i <- rfnames {
        for nj@j <- rfnames {
            if i > j && ni == nj {
                throw compile_err(loc, f"duplicate record field '{id2str(ni)}'")
            }
        }
    }

fun finalize_record_typ(t: typ_t, loc: loc_t): typ_t {
    val t = deref_typ(dup_typ(t))
    match t {
    | TypRecord((ref (relems, _)) as r) =>
        for (n, t, v_opt) <- relems {
            match v_opt {
            | Some(v) => unify(t, get_lit_typ(v), loc, f"type of the field '{pp_id2str(n)}' and its initializer do not match")
            | _ => {}
            }
        }
        TypRecord(r)
    | _ => t
    }
}

fun find_typ_instance(t: typ_t, loc: loc_t): typ_t? {
    val t = deref_typ(t)
    match t {
    | TypApp(targs, n) =>
        if targs.empty() {
            Some(t)
        } else {
            val inst_list = match id_info(n) {
                | IdVariant (ref {dvar_templ_inst}) => *dvar_templ_inst
                | _ => []
                }
            match find_opt(for inst <- inst_list {
                match id_info(inst) {
                | IdVariant (ref {dvar_alias=inst_alias}) =>
                    maybe_unify(t, inst_alias, false)
                | _ => false
                }}) {
            | Some inst => Some(TypApp([], inst))
            | _ => None
            }
        }
    | _ => Some(t)
    }
}

fun get_record_elems(vn_opt: id_t?, t: typ_t, loc: loc_t) {
    val t = deref_typ(t)
    val input_vn = match vn_opt { | Some(vn) => get_orig_id(vn) | _ => noid }
    match t {
    | TypRecord (ref (relems, true)) => (noid, relems)
    | TypRecord(_) => throw compile_err(loc, "the records, which elements we request, is not finalized")
    | TypApp(_, _) =>
        match find_typ_instance(t, loc) {
        | Some(TypApp([], n)) =>
            match id_info(n) {
            | IdVariant (ref {dvar_flags, dvar_cases=(vn0, TypRecord (ref (relems, true))) :: []}) when dvar_flags.var_flag_record =>
                if input_vn == noid || input_vn == get_orig_id(vn0) {}
                else {
                    throw compile_err(loc, f"mismatch in the record name: given 'pp_id2str(input_vn)', expected 'pp_id2str(vn0)'")
                }
                (noid, relems)
            | IdVariant (ref {dvar_name, dvar_cases, dvar_ctors}) =>
                match find_opt(for (vn, t) <- dvar_cases, c_id <- dvar_ctors {
                        get_orig_id(vn) == get_orig_id(input_vn)
                    }) {
                | Some(((_, TypRecord(ref (relems, true))), ctor)) => (ctor, relems)
                | Some(_) => throw compile_err(loc, f"variant '{pp_id2str(dvar_name)}' is not a record")
                | _ => throw compile_err(loc, f"'{pp_id2str(input_vn)}' is not found in '{pp_id2str(dvar_name)}'")
                }
            | _ => throw compile_err(loc, f"cannot find a proper record constructor in type '{pp_id2str(n)}'")
            }
        | _ => throw compile_err(loc, "proper instance of the template [record?] type is not found")
        }
    | _ => throw compile_err(loc, "attempt to treat non-record and non-variant as a record")
    }
}

fun is_real_typ(t: typ_t) {
    var have_typ_vars = false
    fun is_real_typ_(t: typ_t, callb: ast_callb_t) =
        match t {
        | TypApp([], IdName(_)) => have_typ_vars = true; t
        | TypApp([], _) => t
        | TypVar (ref None) => have_typ_vars = true; t
        | _ => walk_typ(t, callb)
        }

    val callb = ast_callb_t {ast_cb_typ=Some(is_real_typ_), ast_cb_exp=None, ast_cb_pat=None}
    val _ = is_real_typ_(t, callb)
    !have_typ_vars
}

fun report_not_found(n: id_t, loc: loc_t) = compile_err(loc, f"the appropriate match for '{pp_id2str(n)}' is not found")

fun report_not_found_typed(n: id_t, t: typ_t, loc: loc_t) =
    compile_err(loc, f"the appropriate match for '{pp_id2str(n)}' of type '{typ2str(t)}' is not found")

fun find_first(n: id_t, env: env_t, loc: loc_t, pred: env_entry_t -> 't?): 't? {
    fun find_next_(elist: env_entry_t list): 't? {
        | e :: rest => match pred(e) {
            | None => find_next_(rest)
            | some_x => some_x
            }
        | _ => None
        }

    fun search_path(n_path: string list, env: env_t): env_entry_t list =
        match n_path {
        | [] => []
        | last_component :: [] => find_all(get_id(last_component), env)
        | prefix :: rest =>
            match find_all(get_id(prefix), env) {
            | EnvId(m) :: _ =>
                match id_info(m) {
                | IdModule (ref {dm_env}) => search_path(rest, dm_env)
                | _ => []
                }
            | _ => []
            }
        }

    val e_all = find_all(n, env)
    val e_all = if e_all.empty() && is_unique_id(n) { EnvId(n) :: [] } else { e_all }
    val e_all = if !e_all.empty() { e_all } else {
        val n_path = pp_id2str(n).split('.')
        if n_path.length() <= 1 { [] } else { search_path(n_path, env) }
    }
    find_next_(e_all)
}

fun lookup_id(n: id_t, t: typ_t, env: env_t, sc: scope_t list, loc: loc_t): id_t
{
    val i_opt = find_first(n, env, loc, fun (e: env_entry_t): id_t? {
        | EnvId(IdName(_)) => None
        | EnvId(i) =>
            match id_info(i) {
            | IdDVal ({dv_typ}) => unify(dv_typ, t, loc, "incorrect value type"); Some(i)
            | IdFun(df) =>
                val {df_templ_args, df_typ, df_flags, df_env, df_scope} = *df
                val t = match (df_flags.fun_flag_has_keywords, deref_typ(t)) {
                    | (true, TypFun(argtyps, rt)) =>
                        val argtyps = match argtyps.rev().map(deref_typ) {
                            | TypRecord (ref (_, false)) :: _ => argtyps
                            | _ => argtyps + [: TypRecord(ref ([], false)) :]
                        }
                        TypFun(argtyps, rt)
                    | _ => t
                    }
                if df_templ_args.empty() {
                    if maybe_unify(df_typ, t, true) { Some(i) } else { None }
                } else {
                    val (ftyp, env1) = preprocess_templ_typ(df_templ_args, df_typ, df_env, sc, loc)
                    if maybe_unify(ftyp, t, true) {
                        /* a necessary extra step to do before function instantiation;
                        if it's a constructor, we first need to check the return type.
                        this check may implicitly instantiate generic variant type, and so
                        the df_templ_inst list of this constructor will be expanded with new instance.
                        In theory, with such an extra step we should never enter
                        'instantiate_fun' for constructors, because they all will be
                        instantiated from check_typ => instantiate_variant => register_typ_constructor. */
                        if !is_fun_ctor(df_flags) {}
                        else {
                            val rt = match ftyp { | TypFun(_, rt) => rt | rt => rt }
                            ignore(check_typ(rt, env1, sc, loc))
                        }

                        val i_opt = df->df_templ_inst->find_opt(fun (inst: id_t) {
                            match id_info(inst) {
                            | IdFun (ref {df_typ=inst_typ}) => maybe_unify(inst_typ, t, true)
                            | _ => throw compile_err(loc, f"invalid (non-function) instance {id2str(inst)} of template function {pp_id2str(n)}")
                            }})
                        match i_opt {
                        | None =>
                            /* the generic function matches the requested type,
                            but there is no appropriate instance;
                            let's create a new one */
                            val inst_env = inst_merge_env(env, env1)
                            val {df_name=inst_name, df_typ=inst_typ} = *instantiate_fun(df, ftyp, inst_env, sc, loc, true)
                            unify(inst_typ, t, loc, "inconsistent type of the instantiated function")
                            Some(inst_name)
                        | _ => i_opt
                        }
                    } else {
                        None
                    }
                }
            | IdModule(_) => unify(TypModule, t, loc, "unexpected module name"); Some(i)
            | IdExn (ref {dexn_typ, dexn_loc}) =>
                val ctyp = typ2constr(dexn_typ, TypExn, dexn_loc)
                unify(ctyp, t, loc, "uncorrect type of exception constructor and/or its arguments")
                Some(i)
            | IdNone | IdTyp(_) | IdVariant(_) | IdClass(_) | IdInterface(_) => None
            }
        | EnvTyp(_) => None
        })

    match i_opt {
    | Some(i) => i
    | None =>
        match try_autogen_symbols(n, t, env, sc, loc) {
        | Some(n1) => n1
        | _ => throw report_not_found_typed(n, t, loc)
        }
    }
}

fun try_autogen_symbols(n: id_t, t: typ_t, env: env_t, sc: scope_t list, loc: loc_t): id_t? {
    val nstr = id2str(n)
    match (nstr, deref_typ_rec(t)) {
    | ("__eq__", TypFun(TypApp([], n1) :: TypApp([], n2) :: [], TypBool))
        when n1 == n2 && (match id_info(n1) { | IdVariant(_) => true | _ => false}) =>
        Some(lookup_id(get_id("__eq_variants__"), t, env, sc, loc))
    | _ => None
    }
}

fun preprocess_templ_typ(templ_args: id_t list, typ: typ_t, env: env_t, sc: scope_t list, loc: loc_t) {
    val t = dup_typ(typ)
    val fold env1 = env for nt <- templ_args {
        val t = make_new_typ()
        add_typ_to_env(nt, t, env1)
    }
    (check_typ(t, env1, sc, loc), env1)
}

fun check_for_duplicate_fun(ftyp: typ_t, env: env_t, sc: scope_t list, loc: loc_t) =
    fun (i: id_info_t) {
        | IdFun (ref {df_name, df_templ_args, df_typ, df_scope, df_loc}) =>
            if df_scope.hd() == sc.hd() {
                val (t, _) = preprocess_templ_typ(df_templ_args, df_typ, env, sc, loc)
                if maybe_unify(t, ftyp, false) && df_templ_args.empty() {
                    throw compile_err( loc,
                        f"the symbol {pp_id2str(df_name)} is re-declared in the same scope; the previous declaration is here: {df_loc}")
                }
            }
        | IdExn (ref {dexn_name, dexn_typ, dexn_scope, dexn_loc}) =>
            if dexn_scope.hd() == sc.hd() {
                val t = typ2constr(dexn_typ, TypExn, dexn_loc)
                if maybe_unify(t, ftyp, false) {
                    throw compile_err(loc,
                        f"the symbol '{pp_id2str(dexn_name)}' is re-declared in the same scope; the previous declaration is here {dexn_loc}")
                }
            }
        | _ => {}
    }

fun match_ty_templ_args(actual_ty_args: typ_t list, templ_args: id_t list, env: env_t, def_loc: loc_t, inst_loc: loc_t) {
    val n_aty_args = actual_ty_args.length()
    val n_templ_args = templ_args.length()
    val norm_ty_args = match (actual_ty_args, n_aty_args, n_templ_args) {
        | (_, m, n) when m == n => actual_ty_args
        | (TypTuple(tl) :: [], 1, n) when tl.length() == n => tl
        | (_, _, 1) => TypTuple(actual_ty_args) :: []
        | _ =>
            throw compile_err(inst_loc,
            f"the number of actual type parameters and formal type parameters, as declared at\n\t{def_loc},\n\tdo not match")
        }

    fold env=env for n <- templ_args, t <- norm_ty_args {
        add_typ_to_env(n, t, env)
    }
}

/*
    One of the main functions in type checker:
    infers the proper type of expression and recursively processes sub-expressions.

    Whenever possible, we try to do type unification before processing sub-expressions.
    This is because sometimes we know type of the outer expression (type of the expression result)
    and using this information we can possibly deduce the types of arguments.
    For example, in `5 :: []` operation we can figure out that the second argument has `int list` type,
    we do not have to write more explicit `5 :: ([]: int list)` expression.
    Or, in the case of some hypothetical "fun integrate(a: 't, b: 't, f: 't->'t)" we can just call it
    with `integrate(0.f, 10.f, Math.sin)` instead of more explicit `integrate(0.f, 10.f, (Math.sin: float->float))`.

    Also, note that the function does not modify the environment. When a complex expression
    is analyzed (such as for-loop), a temporary environment can be created with extra content
    (like iteration variables in the case of for loop), but that temporary environment
    is discared as soon the analysis is over. But the function may alter the global symbol table.

    The declarations should modify the environment, e.g. `val (a, b) = (cos(0.5), sin(0.5))` should introduce
    some fresh ids like `a12345`, `b12346`, add the pairs (a, a12345) and (b, b12346) into the environment
    and then analyze the subsequent expressions in the same scope using this augmented environment,
    but this is done in check_eseq() function.
*/

fun check_exp(e: exp_t, env: env_t, sc: scope_t list) {
    val (etyp, eloc) as ctx = get_exp_ctx(e)

    fun check_for_clause(p: pat_t, e: exp_t, env: env_t, idset: idset_t, sc: scope_t list) {
        val e = check_exp(e, env, sc)
        val is_range = match e { | ExpRange(_, _, _, _) => true | _ => false }
        val (etyp, eloc) = get_exp_ctx(e)
        val etyp = deref_typ(etyp)
        val (_, relems) = match etyp {
            | TypApp(_, _) => try { get_record_elems(None, etyp, eloc) } catch { | CompileError(_, _) => (noid, []) }
            | _ => (noid, [])
            }
        match (is_range, etyp, relems) {
        | (false, TypTuple(tl), _) =>
            val tup_pat = PatIdent(gen_temp_id("tup"), eloc)
            val (tup_pat, env, idset, _) = check_pat(tup_pat, etyp, env, idset, make_empty_idset(), sc, false, true, false)
            val def_e = DefVal(tup_pat, e, default_tempval_flags(), eloc)
            val tup_e =
            match tup_pat {
            | PatIdent(i, _) => ExpIdent(i, (etyp, eloc))
            | PatTyped(PatIdent(i, _), _, _) => ExpIdent(i, (etyp, eloc))
            | _ => throw compile_err(eloc, "for over tuple: invalid result of check_pat result (id is expected)")
            }
            (tl.length(), (p, tup_e), [: def_e :], 1, env, idset)
        | (false, _, relems) when relems != [] =>
            val rec_pat = PatIdent(gen_temp_id("rec"), eloc)
            val (rec_pat, env, idset, _) = check_pat(rec_pat, etyp, env, idset, make_empty_idset(), sc, false, true, false)
            val def_e = DefVal(rec_pat, e, default_tempval_flags(), eloc)
            val rec_e =
            match rec_pat {
            | PatIdent(i, _) => ExpIdent(i, (etyp, eloc))
            | PatTyped(PatIdent(i, _), _, _) => ExpIdent(i, (etyp, eloc))
            | _ => throw compile_err(eloc, "for over record: invalid result of check_pat result (id is expected)")
            }
            (relems.length(), (p, rec_e), [: def_e :], 1, env, idset)
        | _ =>
            val (ptyp, dims) =
            match (e, etyp) {
            | (ExpRange(_, _, _, _), _) => (TypInt, 1)
            | (_, TypArray(d, et)) => (et, d)
            | (_, TypList(et)) => (et, 1)
            | (_, TypString) => (TypChar, 1)
            | _ => throw compile_err(eloc, "unsupported iteration domain; it should be a range, array, list, string, tuple or a record")
            }
            val (p, env, idset, _) = check_pat(p, ptyp, env, idset, make_empty_idset(), sc, false, true, false)
            (0, (p, e), [], dims, env, idset)
        }
    }

    fun check_for_clauses(for_clauses: (pat_t, exp_t) list, idx_pat: pat_t, env: env_t, idset: idset_t, for_sc: scope_t list) {
        val (_, trsz, for_clauses, code, dims, env, idset) =
        fold (idx, trsz, for_clauses, code, dims, env, idset) = (0, 0, [], [], 0, env, idset) for (pi, ei) <- for_clauses {
            val (trszj, (pi, ei), code_i, dims_i, env, idset) = check_for_clause(pi, ei, env, idset, for_sc)
            if idx > 0 && dims_i != dims {
                throw compile_err(get_exp_loc(ei), "the dimensionalities of simultaneously iterated containers/ranges do not match")
            }
            if idx > 0 && trszj != trsz {
                throw compile_err(
                    get_exp_loc(ei),
                    if trsz == 0 || trszj == 0 {
                        "simultaneous iteration over tuples/records and some other containers or ranges is not supported yet"
                    } else {
                        "cannot iterate over tuples/records of different size"
                    })
            }
            (idx + 1, trszj, (pi, ei) :: for_clauses, code_i + code, dims_i, env, idset)
        }
        val idx_typ = if dims == 1 { TypInt } else { TypTuple([: for _ <- 0:dims {TypInt} :]) }
        val (idx_pat, env, idset) =
        if trsz != 0 {
            (idx_pat, env, idset)
        } else {
            match idx_pat {
            | PatAny(_) => (idx_pat, env, idset)
            | _ =>
                val (idx_pat, env, idset, _) = check_pat(idx_pat, idx_typ, env, idset, make_empty_idset(), for_sc, false, true, false)
                (PatTyped(idx_pat, idx_typ, get_pat_loc(idx_pat)), env, idset)
            }
        }
        (trsz, code.rev(), for_clauses.rev(), idx_pat, dims, env, idset)
    }

    fun gen_for_in_tuprec_it(idx: int, for_clauses: (pat_t, exp_t) list, idx_pat: pat_t, body: exp_t, env: env_t, for_sc: scope_t list) {
        val sc = new_block_scope() :: for_sc
        val (_, code, env, idset) =
        fold (j, code, env, idset) = (0, [], env, make_empty_idset()) for (pj, trj) <- for_clauses {
            val (ttrj, locj) = get_exp_ctx(trj)
            match ttrj {
            | TypTuple(tl) =>
                val tj = List.nth(tl, idx)
                val ej = ExpMem(trj, ExpLit(LitInt(int64(idx)), (TypInt, locj)), (tj, locj))
                val pj = dup_pat(pj)
                val (pj, env, idset, _) = check_pat(pj, tj, env, idset, make_empty_idset(), sc, false, true, false)
                val def_pj = DefVal(pj, ej, default_tempval_flags(), locj)
                (j + 1, def_pj :: code, env, idset)
            | _ =>
                val (_, relems) = get_record_elems(None, ttrj, locj)
                val (nj, tj, _) = List.nth(relems, idx)
                val ej = ExpMem(trj, ExpIdent(nj, (TypString, locj)), (tj, locj))
                val pj = dup_pat(pj)
                val (pnj, pvj) =
                match pat_skip_typed(pj) {
                | PatTuple(pnj :: pvj :: [], _) => (pnj, pvj)
                | PatAs(PatTuple(pnj :: pvj :: [], _), as_id, _)
                    when (match as_id {| IdTemp(_,_) => true | _ => false}) => (pnj, pvj)
                | _ => throw compile_err(eloc, "when iterating through record, a 2-element tuple should be used as pattern")
                }
                val (pnj, env, idset, _) = check_pat(pnj, TypString, env, idset, make_empty_idset(), sc, false, true, false)
                val (pvj, env, idset, _) = check_pat(pvj, tj, env, idset, make_empty_idset(), sc, false, true, false)
                val def_pvj = DefVal(pvj, ej, default_tempval_flags(), locj)
                val def_pnj = DefVal(pnj, ExpLit(LitString(pp_id2str(nj)), (TypString, locj)), default_tempval_flags(), locj)
                (j + 1, def_pvj :: def_pnj :: code, env, idset)
            }
        }
        val (code, env, idset) =
        match idx_pat {
        | PatAny(_) => (code, env, idset)
        | _ =>
            val idx_pat = dup_pat(idx_pat)
            val (idx_pat, env, idset, _) = check_pat(idx_pat, TypInt, env, idset, make_empty_idset(), sc, false, true, false)
            val idx_loc = get_pat_loc(idx_pat)
            val def_idx = DefVal(idx_pat, ExpLit(LitInt(int64(idx)), (TypInt, idx_loc)), default_tempval_flags(), idx_loc)
            (def_idx :: code, env, idset)
        }
        val body = check_exp(dup_exp(body), env, sc)
        val (body_typ, body_loc) = get_exp_ctx(body)
        ExpSeq((body :: code).rev(), (body_typ, body_loc))
    }

    fun check_inside_for(expect_fold_loop: bool, isbr: bool, sc: scope_t list) {
        val kw = if !isbr { "continue" } else if expect_fold_loop { "fold_break" } else { "break" }
        fun check_inside_(sc: scope_t list) {
            | ScTry(_) :: _ => throw compile_err(eloc, f"cannot use '{kw}' inside 'try-catch' block")
            | ScFun(_) :: _ | ScModule(_) :: _ | ScGlobal :: _ | ScClass(_) :: _ | ScInterface(_) :: _ | [] =>
                throw compile_err(eloc, f"cannot use '{kw}' outside of loop")
            | ScLoop(nested, _) :: _ =>
                if expect_fold_loop {
                    throw compile_err(eloc, f"'kw' can only be used inside 'fold' loop")
                } else if isbr && nested {
                    throw compile_err(
                        eloc,
                        "break cannot be used inside nested for-loop because of ambiguity.\n" +
                        "\tUse explicit curly braces, e.g. 'for ... { for ... { for ... { break }}} to'\n" +
                        "\texit a single for-loop, or use exceptions, e.g. standard Break exception to exit nested loops.")
                }
            | ScFold(_) :: _ =>
                if expect_fold_loop {}
                else { throw compile_err(eloc, f"cannot use '{kw}' inside 'fold' loop") }
            | ScArrMap(_) :: _ => throw compile_err(eloc, f"cannot use '{kw}' inside array comprehension")
            | ScMap(_) :: _ =>
                if expect_fold_loop {
                    throw compile_err(eloc, f"'{kw}' can only be used inside 'fold' loop")
                }
            | ScBlock(_) :: outer_sc => check_inside_(outer_sc)
        }
        check_inside_(sc)
    }

    /* find the proper function and make "call" expression
       given that all the parameters are already type-checked */
    fun check_and_make_call(f_id: id_t, args: exp_t list) {
        val arg_typs = [: for a <- args { get_exp_typ(a) } :]
        val f_expected_typ = TypFun(arg_typs, etyp)
        val f_exp = ExpIdent(f_id, (make_new_typ(), eloc))
        val (f_real_typ, floc) = get_exp_ctx(f_exp)
        unify(f_real_typ, f_expected_typ, floc, "the real and expected function type do not match")
        val new_f = check_exp(f_exp, env, sc)
        ExpCall(new_f, args, ctx)
    }

    val new_e =
    match e {
    | ExpNop(_) => e
    | ExpRange(e1_opt, e2_opt, e3_opt, _) =>
        fun check_range_e(e_opt: exp_t?): exp_t? =
            match e_opt {
            | None => None
            | Some(e) =>
                val (etyp1, eloc1) = get_exp_ctx(e)
                unify(etyp1, TypInt, eloc1, "explicitly specified component of a range must be an integer")
                val new_e = check_exp(e, env, sc)
                Some(new_e)
            }

        val new_e1_opt = check_range_e(e1_opt)
        val new_e2_opt = check_range_e(e2_opt)
        val new_e3_opt = match e3_opt {
                         | Some(_) => check_range_e(e3_opt)
                         | _ => Some(ExpLit(LitInt(1L), (TypInt, eloc)))
                         }
        unify(etyp, TypTuple([: TypInt, TypInt, TypInt :]), eloc, "the range type should have (int, int, int) type")
        ExpRange(new_e1_opt, new_e2_opt, new_e3_opt, ctx)
    | ExpLit(lit, _) => unify(etyp, get_lit_typ(lit), eloc, "the literal has improper type")
                        e
    | ExpIdent(n, _) =>
        val n = lookup_id(n, etyp, env, sc, eloc)
        val etyp = match id_info(n) {
                   | IdFun(_) | IdDVal(_) => get_id_typ(n, eloc)
                   | _ => etyp
                   }
        ExpIdent(n, (etyp, eloc))
    | ExpMem(e1, e2, _) =>
        /* in the case of '.' operation we do not check e2 immediately after e1,
           because e2 is a 'member' of a structure/module e1,
           so it cannot be independently analyzed */
        val new_e1 = check_exp(e1, env, sc)
        val (etyp1, eloc1) = get_exp_ctx(new_e1)
        val etyp1 = deref_typ(etyp1)
        match (etyp1, new_e1, e2) {
        | (TypModule, ExpIdent(n1, _), ExpIdent(n2, (etyp2, eloc2))) =>
            val n1_env = get_module_env(n1)
            val new_n2 = lookup_id(n2, etyp, n1_env, sc, eloc)
            val etyp = match id_info(new_n2) {
                       | IdFun(_) | IdDVal(_) => get_id_typ(new_n2, eloc)
                       | _ => etyp
                       }
            ExpIdent(new_n2, (etyp, eloc))
        | (TypTuple(tl), _, ExpLit(LitInt(idx), (etyp2, eloc2))) =>
            /* enclose the tuple access int try-catch, because an out-of-range
               index could be provided by user */
            unify(etyp2, TypInt, eloc2, "index must be int!")
            val et = try { tl.nth(int(idx)) } catch { | OutOfRangeError => throw compile_err(eloc2, "the tuple index is out of range") }
            unify(etyp, et, eloc, "incorrect type of the tuple element")
            ExpMem(new_e1, e2, ctx)
        | (TypApp(_, vn), _, ExpIdent(n2, (etyp2, eloc2))) when pp_id2str(n2) == "__tag__" =>
            match id_info(vn) {
            | IdVariant(_) => unify(etyp, TypInt, eloc, "variant tag is integer, but the other type is expected")
                              ExpMem(new_e1, e2, ctx)
            | _ => throw compile_err(eloc, "__tag__ can only be requestd for variants")
            }
        | (_, _, ExpIdent(n2, (etyp2, eloc2))) =>
            val (_, relems) = get_record_elems(None, etyp1, eloc1)
            match relems.find_opt(fun ((n1, _, _): rec_elem_t) {n1 == n2}) {
            | Some((_, t, _)) =>
                unify(etyp, t, eloc, "incorrect type of the record element")
                ExpMem(new_e1, e2, ctx)
            | _ => throw compile_err(eloc, f"the record element {pp_id2str(n2)} is not found")
            }
        | _ => throw compile_err(eloc, "unsupported element access operation")
        }
    | ExpAssign(e1, e2, _) =>
        val (etyp1, eloc1) = get_exp_ctx(e1)
        val (etyp2, eloc2) = get_exp_ctx(e2)
        unify(etyp1, etyp2, eloc, "the left and the right sides of the assignment must have the same type")
        val new_e1 = check_exp(e1, env, sc)
        val new_e2 = check_exp(e2, env, sc)
        /* check that new_e1 is lvalue and that new_e1 and new_e2 have equal types;
           in future we can let etyp1_ and etyp2_ be different as long as the assignment
           is safe and does not loose precision, e.g. int8 to int, float to double etc. */
        fun is_lvalue(need_mutable_id: bool, e: exp_t) =
            match e {
            | ExpAt(arr, BorderNone, InterpNone, _, _) => is_lvalue(false, arr)
            | ExpUnary(OpDeref, r, _) => is_lvalue(false, r)
            | ExpIdent(n1, _) => !need_mutable_id ||
                (match id_info(n1) {
                | IdDVal ({dv_flags}) => dv_flags.val_flag_mutable
                | _ => false
                })
            | ExpMem(rcrd, ExpIdent(_, _), _) => is_lvalue(need_mutable_id, rcrd)
            | ExpMem(tup, ExpLit(_, _), _) => is_lvalue(need_mutable_id, tup)
            | _ => false
            }

        if !is_lvalue(true, new_e1) {
            throw compile_err(eloc, "the left side of assignment is not an l-value")
        }
        ExpAssign(new_e1, new_e2, eloc)
    | ExpBinary(OpCons, e1, e2, _) =>
        val new_e1 = check_exp(e1, env, sc)
        val new_e2 = check_exp(e2, env, sc)
        val (etyp1, eloc1) = get_exp_ctx(new_e1)
        val (etyp2, eloc2) = get_exp_ctx(new_e2)
        if maybe_unify(etyp, TypList(make_new_typ()), false) || maybe_unify(etyp2, TypList(make_new_typ()), false) {
            unify(etyp, TypList(etyp1), eloc1, "'::' operation should produce a list")
            unify(etyp2, TypList(etyp1), eloc2, "incorrect type of the second argument of '::' operation")
            ExpBinary(OpCons, new_e1, new_e2, ctx)
        } else {
            unify(etyp1, TypInt, eloc1, "explicitly specified component of a range must be an integer")
            unify(etyp2, TypInt, eloc2, "explicitly specified component of a range must be an integer")
            unify(etyp, TypTuple([: TypInt, TypInt, TypInt :]), eloc, "the range type should have (int, int, int) type")
            ExpRange(Some(new_e1), None, Some(new_e2), ctx)
        }
    | ExpBinary(bop, e1, e2, _) =>
        val new_e1 = check_exp(e1, env, sc)
        val (etyp1, eloc1) = get_exp_ctx(new_e1)
        val new_e2 = check_exp(e2, env, sc)
        val (etyp2, eloc2) = get_exp_ctx(new_e2)
        val bop_wo_dot = binary_try_remove_dot(bop)

        /* depending on the operation, figure out the type of result
           (or set it to None if there is no embedded implementation)
           and also do some other op-specific checks */
        val (bop, typ_opt) =
        match bop {
        | OpAdd | OpSub | OpMul | OpDiv | OpMod | OpPow | OpShiftLeft | OpShiftRight =>
            val is_shift = bop == OpShiftLeft || bop == OpShiftRight
            val allow_fp = !is_shift
            val typ_opt = coerce_types(etyp1, etyp2, false, allow_fp, is_shift, eloc)
            (bop, typ_opt)
        | OpDotMul | OpDotDiv | OpDotMod | OpDotPow =>
            val typ_opt = coerce_types(etyp1, etyp2, false, true, false, eloc)
            match typ_opt {
            | Some(_) => (bop_wo_dot, typ_opt)
            | _ => (bop, typ_opt)
            }
        | OpBitwiseAnd | OpBitwiseOr | OpBitwiseXor =>
            fun check_bitwise(t1: typ_t, t2: typ_t): typ_t =
                match (deref_typ(t1), deref_typ(t2)) {
                | (TypInt, TypInt) => TypInt
                | (TypSInt(b1), TypSInt(b2)) when b1 == b2 => TypSInt(b1)
                | (TypUInt(b1), TypUInt(b2)) when b1 == b2 => TypUInt(b1)
                | (TypBool, TypBool) => TypBool
                | _ => throw BadArgError
                }

            val typ_opt = try { Some(check_bitwise(etyp1, etyp2)) } catch { | BadArgError => None }
            (bop, typ_opt)
        | OpLogicAnd | OpLogicOr =>
            unify(etyp1, TypBool, eloc1, "arguments of logical operation must be boolean")
            unify(etyp2, TypBool, eloc2, "arguments of logical operation must be boolean")
            (bop, Some(TypBool))
        /* [TODO] comparison operations are now implemented for anything but variants (except for OpCompareEQ & OpCompareNE, which are implemented) */
        | OpCmp _ | OpDotCmp _ =>
            match bop {
            | OpDotCmp _ => {}
            | _ =>
                unify(etyp1, etyp2, eloc, "the compared elements must have the same type")
                unify(etyp, TypBool, eloc, f"result of comparison operation '{bop}' must be bool")
            }
            if is_typ_scalar(etyp1) && is_typ_scalar(etyp2) {
                unify(etyp1, etyp2, eloc, "only equal types can be compared")
                (bop_wo_dot, Some(TypBool))
            } else if etyp1 == TypString {
                (bop_wo_dot, None)
            } else {
                (bop, None)
            }
        | OpSpaceship | OpDotSpaceship =>
            if is_typ_scalar(etyp1) && is_typ_scalar(etyp2) || deref_typ(etyp1) == TypString && deref_typ(etyp2) == TypString {
                (bop_wo_dot, None)
            } else {
                (bop, None)
            }
        | OpCons => throw compile_err(eloc, f"unsupported binary operation {bop}")
        }
        match (typ_opt, bop, deref_typ(etyp1), deref_typ(etyp2), e1, e2) {
        | (Some(typ), _, _, _, _, _) => unify(typ, etyp, eloc, "improper type of the arithmetic operation result")
                                        ExpBinary(bop, new_e1, new_e2, ctx)
        | (_, OpAdd, TypString, TypString, _, _) | (_, OpAdd, TypString, TypChar, _, _) | (_, OpAdd, TypChar, TypString, _, _) | (_, OpAdd,
                                                                    TypChar, TypChar, _, _) =>
            unify(TypString, etyp, eloc, "improper type of the string concatenation operation (string is expected)")
            ExpBinary(bop, new_e1, new_e2, ctx)
        | (_, OpAdd, TypList(_), TypList(_), ExpBinary(OpAdd, sub_e1, sub_e2, _), _) =>
            /* make list concatenation right-associative instead of left-associative */
            val sub_e2_loc = get_exp_loc(sub_e2)
            val e2_loc = loclist2loc([: sub_e2_loc, eloc2 :], eloc2)
            /* [TODO: not that relevant anymore]
            fix the general bug with repetitive check of the same expression (since the check has some side effects) */
            val e2_ = ExpBinary(OpAdd, sub_e2, e2, (make_new_typ(), e2_loc))
            check_exp(ExpBinary(OpAdd, sub_e1, e2_, (etyp, eloc)), env, sc)
        | _ =>
            /* try to find an overloaded function that will handle such operation with combination of types, e.g.
               operator + (p: point, q: point) = point { p.x + q.x, p.y + q.y } */
            val f_id = get_binary_fname(bop, eloc)
            check_and_make_call(f_id, [: new_e1, new_e2 :])
        }
    | ExpThrow(e1, _) =>
        val (etyp1, eloc1) = get_exp_ctx(e1)
        unify(TypExn, etyp1, eloc, "the argument of 'throw' operator must be an exception")
        val new_e1 = check_exp(e1, env, sc)
        ExpThrow(new_e1, eloc)
    | ExpUnary(OpMkRef, e1, _) =>
        val (etyp1, eloc1) = get_exp_ctx(e1)
        unify(etyp, TypRef(etyp1), eloc, "the types of ref() operation argument and result are inconsistent")
        val new_e1 = check_exp(e1, env, sc)
        ExpUnary(OpMkRef, new_e1, ctx)
    | ExpUnary(OpDeref, e1, _) =>
        val (etyp1, eloc1) = get_exp_ctx(e1)
        unify(TypRef(etyp), etyp1, eloc, "the types of unary '*' operation argument and result are inconsistent")
        val new_e1 = check_exp(e1, env, sc)
        ExpUnary(OpDeref, new_e1, ctx)
    | ExpUnary(uop, e1, _) =>
        val new_e1 = check_exp(e1, env, sc)
        val (etyp1, eloc1) = get_exp_ctx(new_e1)
        match uop {
        | OpNegate | OpPlus | OpDotMinus =>
            val allow_fp = uop == OpNegate || uop == OpPlus
            val t_opt = coerce_types(etyp1, etyp1, false, allow_fp, false, eloc)
            match t_opt {
            | Some(t) => unify(etyp, t, eloc, "improper type of the unary '-' operator result")
                         ExpUnary(uop, new_e1, ctx)
            | None => val f_id = get_unary_fname(uop, eloc)
                      check_and_make_call(f_id, [: new_e1 :])
            }
        | OpBitwiseNot =>
            fun check_unary_bitwise(t: typ_t): typ_t =
                match deref_typ(t) {
                | TypInt => TypInt
                | TypSInt(b) => TypSInt(b)
                | TypUInt(b) => TypUInt(b)
                | TypBool => TypBool
                | TypTuple(tl) => TypTuple(tl.map(check_unary_bitwise))
                | _ => throw BadArgError
                }

            try {
                unify(etyp, check_unary_bitwise(etyp1), eloc, "invalid type of bitwise-not result")
                ExpUnary(uop, new_e1, ctx)
            } catch {
            | BadArgError =>
                val f_id = get_unary_fname(uop, eloc)
                check_and_make_call(f_id, [: new_e1 :])
            }
        | OpLogicNot =>
            unify(etyp1, TypBool, eloc1, "the argument of ! operator must be a boolean")
            unify(etyp, TypBool, eloc, "the result of ! operator must be a boolean")
            ExpUnary(uop, new_e1, ctx)
        | OpApos =>
            val f_id = get_unary_fname(uop, eloc)
            check_and_make_call(f_id, [: new_e1 :])
        | _ => throw compile_err(eloc, f"unsupported unary operation '{uop}'")
        }
    | ExpSeq(eseq, _) =>
        val eseq_typ = get_eseq_typ(eseq)
        unify(etyp, eseq_typ, eloc, "the expected type of block expression does not match its actual type")
        val (eseq, _) = check_eseq(eseq, env, sc, true)
        ExpSeq(eseq, ctx)
    | ExpMkTuple(el, _) =>
        val tl = [: for e <- el { get_exp_typ(e) } :]
        unify(etyp, TypTuple(tl), eloc, "improper type of tuple elements or the number of elements")
        ExpMkTuple([: for e <- el { check_exp(e, env, sc) } :], ctx)
    | ExpCall(f0, args0, _) =>
        /* [TODO] implement more sophisticated algorithm; try to look for possible overloaded functions first;
           if there is just one obvious match, take it first and use to figure out the argument types */
        val f = dup_exp(f0)
        val args = args0.map(dup_exp)
        val arg_typs = [: for a <- args { get_exp_typ(a) } :]
        val f_expected_typ = TypFun(arg_typs, etyp)
        val (f_real_typ, floc) = get_exp_ctx(f)
        unify(f_real_typ, f_expected_typ, floc, "the real and expected function type do not match")
        val new_args = [: for a <- args { check_exp(a, env, sc) } :]
        try {
            val new_f = check_exp(f, env, sc)
            val new_args =
            match deref_typ(get_exp_typ(new_f)) {
            | TypFun(argtyps, rt) =>
                if argtyps.length() == new_args.length() {
                    new_args
                } else {
                    val last_typ = argtyps.last()
                    val mkrec = ExpMkRecord(ExpNop(eloc), [], (last_typ, eloc))
                    new_args + [: mkrec :]
                }
            | _ => new_args
            }
            ExpCall(new_f, new_args, ctx)
        } catch {
        | CompileError(_, _) as ex =>
            /* fallback for so called "object types": if we have expression like
                some_exp.foo(args) where some_exp's type is "object type"
                (i.e. "list", "string" or some user type declared in some <Module>
                as "object type = ...") then the call is transformed to
                <Module>.foo(some_exp, args)
            */
            match f0 {
            | ExpMem(r0, ExpIdent(mem_f, _) as mem_f_exp, mem_ctx) =>
                val r = check_exp(dup_exp(r0), env, sc)
                val r_t = get_exp_typ(r)
                val mstr =  match deref_typ(r_t) {
                            | TypList(_) => "List"
                            | TypString => "String"
                            | TypChar => "Char"
                            | TypApp(_, tn) =>
                                match id_info(tn) {
                                | IdVariant (ref {dvar_flags={var_flag_object=m}}) when m != noid => pp_id2str(m)
                                | _ => ""
                                }
                            | _ => ""
                            }
                val new_f = if mstr == "Builtins" { mem_f_exp }
                            else {
                                val m_id = if mstr != "" { get_id(mstr) } else { throw ex }
                                val (ftyp, floc) = mem_ctx
                                ExpMem(ExpIdent(m_id, (make_new_typ(), floc)), mem_f_exp, mem_ctx)
                            }
                val new_exp = ExpCall(new_f, r0 :: args0, ctx)
                check_exp(new_exp, env, sc)
            | _ => throw ex
            }
        }
    | ExpAt(arr, border, interp, idxs, _) =>
        val new_arr = check_exp(arr, env, sc)
        val (new_atyp, new_aloc) = get_exp_ctx(new_arr)
        if border != BorderNone {
            throw compile_err(eloc, "border extrapolation is not supported yet")
        }
        if interp != InterpNone {
            throw compile_err(eloc, "inter-element interpolation is not supported yet")
        }
        match idxs {
        /* the 'flatten' case: "arr[:]" */
        | ExpRange(None, None, None, _) :: [] =>
            val new_idx = check_exp(idxs.hd(), env, sc)
            match deref_typ(new_atyp) {
            | TypArray(d, et) =>
                unify(etyp, TypArray(1, et), eloc,
                    "the result of flatten operation ([:]) applied to N-D array must be 1D array with elements of the same type as input array")
                ExpAt(new_arr, BorderNone, InterpNone, new_idx :: [], ctx)
            | TypString =>
                unify(etyp, TypString, eloc, "the result of flatten operation ([:]) applied to string must be string")
                new_arr
            | _ => throw compile_err(eloc, "the argument of the flatten operation must be an array")
            }
        /* other cases: check each index, it should be either a scalar or a range;
           in the first case it should have integer type.
           If all the indices are scalars, then the result should have et type,
           otherwise it's an array of as high dimensionality as the number of range indices */
        | _ =>
            val fold (new_idxs, ndims, nfirst_scalars, nranges) = ([], 0, 0, 0) for idx <- idxs {
                val new_idx = check_exp(idx, env, sc)
                match new_idx {
                | ExpRange(_, _, _, _) =>
                    (new_idx :: new_idxs, ndims + 1, nfirst_scalars, nranges + 1)
                | _ =>
                    val (new_ityp, new_iloc) = get_exp_ctx(new_idx)
                    val new_idx =
                    if maybe_unify(new_ityp, TypInt, true) {
                        new_idx
                    } else {
                        val possible_idx_typs =
                        [: TypBool, TypUInt(8), TypSInt(8), TypUInt(16), TypSInt(16), TypUInt(32), TypSInt(32), TypUInt(64), TypSInt(64) :]
                        if possible_idx_typs.exists(fun (t: typ_t) {maybe_unify(new_ityp, t, true)}) {
                            ExpCast(new_idx, TypInt, (TypInt, new_iloc))
                        } else if interp == InterpLinear && (maybe_unify(new_ityp, TypFloat(32), true) || maybe_unify(new_ityp, TypFloat(64), true)) {
                            new_idx
                        } else {
                            throw compile_err(
                                new_iloc,
                                "each scalar index in array access op must have some integer type or bool; " +
                                "in the case of interpolation it can also be float or double")
                        }
                    }
                    val nfirst_scalars = if nranges == 0 { nfirst_scalars + 1 } else { nfirst_scalars }
                    (new_idx :: new_idxs, ndims + 1, nfirst_scalars, nranges)
                }
            }
            match (ndims, nranges, deref_typ(new_atyp)) {
            | (1, 0, TypString) => unify(etyp, TypChar, new_aloc, "indexing string should give a char")
            | (1, 1, TypString) => unify(etyp, TypString, new_aloc, "indexing string with a range should give a string")
            | _ =>
                val et = make_new_typ()
                unify(new_atyp, TypArray(ndims, et), new_aloc, "the array dimensionality does not match the number of indices")
                if nranges == 0 {
                    unify(etyp, et, eloc, "the type of array access expression does not match the array element type")
                } else {
                    unify(
                        etyp,
                        TypArray(ndims - nfirst_scalars, et),
                        eloc,
                        "the number of ranges does not match dimensionality of the result, or the element type is incorrect")
                }
            }
            if interp != InterpLinear {}
            else if is_typ_numeric(etyp, true) {}
            else {
                throw compile_err(eloc, "in the case of interpolation the array type should be numeric")
            }
            ExpAt(new_arr, border, interp, new_idxs.rev(), ctx)
        }
    | ExpIf(c, e1, e2, _) =>
        val (ctyp, cloc) = get_exp_ctx(c)
        val (typ1, loc1) = get_exp_ctx(e1)
        val (typ2, loc2) = get_exp_ctx(e2)
        unify(ctyp, TypBool, cloc, "if() condition should have 'bool' type")
        unify(typ1, etyp, loc1, "if() expression should have the same type as its branches")
        unify(typ2, etyp, loc2, "if() expression should have the same type as its branches")
        val new_c = check_exp(c, env, sc)
        val new_e1 = check_exp(e1, env, sc)
        val new_e2 = check_exp(e2, env, sc)
        ExpIf(new_c, new_e1, new_e2, ctx)
    | ExpWhile(c, body, _) =>
        val (ctyp, cloc) = get_exp_ctx(c)
        val (btyp, bloc) = get_exp_ctx(body)
        unify(ctyp, TypBool, cloc, "while() loop condition should have 'bool' type")
        unify(btyp, TypVoid, bloc, "while() loop body should have 'void' type")
        val new_c = check_exp(c, env, sc)
        val loop_sc = new_loop_scope(false) :: sc
        val new_body = check_exp(body, env, loop_sc)
        ExpWhile(new_c, new_body, eloc)
    | ExpDoWhile(body, c, _) =>
        val (ctyp, cloc) = get_exp_ctx(c)
        val (btyp, bloc) = get_exp_ctx(body)
        unify(ctyp, TypBool, cloc, "do-while() loop condition should have 'bool' type")
        unify(btyp, TypVoid, bloc, "do-while() loop body should have 'void' type")
        val new_c = check_exp(c, env, sc)
        val loop_sc = new_loop_scope(false) :: sc
        val new_body = check_exp(body, env, loop_sc)
        ExpDoWhile(new_body, new_c, eloc)
    | ExpFor(for_clauses, idx_pat, body, flags, _) =>
        val is_fold = flags.for_flag_fold
        val is_nested = flags.for_flag_nested
        val for_sc = (if is_fold {new_fold_scope()} else {new_loop_scope(is_nested)}) :: sc
        val (trsz, pre_code, for_clauses, idx_pat, dims, env, _) =
            check_for_clauses(for_clauses, idx_pat, env, make_empty_idset(), for_sc)
        if trsz > 0 {
            /* iteration over tuple(s) is replaced with unrolled code.
               * this is possible, since we know the tuple size at compile time
               * this is necessary, because tuple elements may have different type,
                 so for each "iteration" potentially a completely different code could be generated */
            val code = [: for idx <- 0:trsz {
                    val it_j = gen_for_in_tuprec_it(idx, for_clauses, idx_pat, body, env, for_sc)
                    val (tj, locj) = get_exp_ctx(it_j)
                    unify(tj, TypVoid, locj, "'for()' body should have 'void' type")
                    it_j
                } :]
            ExpSeq(pre_code + code, (TypVoid, eloc))
        } else {
            val (btyp, bloc) = get_exp_ctx(body)
            unify(btyp, TypVoid, bloc, "'for()' body should have 'void' type")
            val new_body = check_exp(body, env, for_sc)
            ExpFor(for_clauses, idx_pat, new_body, flags, eloc)
        }
    | ExpMap(map_clauses, body, flags, ctx) =>
        val make_list = flags.for_flag_make == ForMakeList
        val make_tuple = flags.for_flag_make == ForMakeTuple
        val for_sc = (if make_tuple { new_block_scope() } else if make_list { new_map_scope() } else { new_arr_map_scope() }) :: sc
        val (trsz, pre_code, map_clauses, total_dims, env, _) =
        fold (trsz, pre_code, map_clauses, total_dims, env, idset) = (0, [], [], 0, env, make_empty_idset()) for (for_clauses, idx_pat) <- map_clauses {
            val (trsz_k, pre_code_k, for_clauses, idx_pat, dims, env, idset) = check_for_clauses(for_clauses, idx_pat, env, idset, for_sc)
            (trsz + trsz_k, pre_code_k + pre_code, (for_clauses, idx_pat) :: map_clauses, total_dims + dims, env, idset)
        }
        if trsz > 0 || !make_tuple {}
        else {
            throw compile_err(eloc, "tuple comprehension with iteration over non-tuples and non-records is not supported")
        }
        val coll_name = if make_list { "list" } else if make_tuple { "tuple" } else { "array" }
        if trsz > 0 {
            val (for_clauses, idx_pat) = match map_clauses {
                | (for_clauses, idx_pat) :: [] => (for_clauses, idx_pat)
                | _ => throw compile_err(eloc, "tuple comprehension with nested for is not supported yet")
                }
            val elem_typ = make_new_typ()
            val elems = [: for idx <- 0:trsz {
                    val it_j = gen_for_in_tuprec_it(idx, for_clauses, idx_pat, body, env, for_sc)
                    val (tj, locj) = get_exp_ctx(it_j)
                    if !make_tuple {
                        unify(tj, elem_typ, locj, f"{coll_name} comprehension should produce elements of the same type")
                    }
                    it_j
                } :]
            val mk_struct_exp =
            if make_tuple {
                val tl = elems.map(get_exp_typ)
                ExpMkTuple(elems, (TypTuple(tl), eloc))
            } else if make_list {
                val ltyp = TypList(elem_typ)
                fold l_exp = ExpLit(LitNil, (ltyp, eloc)) for ej <- elems.rev() {
                    ExpBinary(OpCons, ej, l_exp, (ltyp, eloc))
                }
            } else {
                ExpMkArray(elems :: [], (TypArray(1, elem_typ), eloc))
            }
            val coll_typ = get_exp_typ(mk_struct_exp)
            unify(etyp, coll_typ, eloc, f"inconsistent type of the constructed {coll_name}")
            ExpSeq(pre_code + [: mk_struct_exp :], (coll_typ, eloc))
        } else {
            val (btyp, bloc) = get_exp_ctx(body)
            if make_list {
                if total_dims == 1 {}
                else { throw compile_err(eloc, "the list comprehension should be 1-dimensional") }
                unify(etyp, TypList(btyp), bloc, "the map should return a list with elements of the same type as the map body")
            } else {
                unify(etyp, TypArray(total_dims, btyp), bloc,
                    f"the map should return {total_dims}-dimensional array with elements of the same type as the map body")
            }
            val new_body = check_exp(body, env, for_sc)
            if deref_typ(get_exp_typ(new_body)) == TypVoid {
                throw compile_err(eloc, "array/list comprehension body cannot have 'void' type")
            }

            ExpMap(map_clauses.rev(), new_body, flags, ctx)
        }
    | ExpBreak(f, _) => check_inside_for(f, true, sc); e
    | ExpContinue(_) => check_inside_for(false, false, sc); e
    | ExpMkArray(arows, _) =>
        val elemtyp = make_new_typ()
        val (_, _, arows, have_expanded, dims) =
        fold (k, ncols, arows, have_expanded, dims) = (0, 0, ([]: exp_t list list), false, -1) for arow <- arows {
            val fold (have_expanded_i, row_dims, arow) = (false, -1, []) for elem <- arow {
                val (is_expanded, elem_dims, elem1, elem_loc) =
                match elem {
                | ExpUnary(OpExpand, e1, (t, loc)) =>
                    val e1 = check_exp(e1, env, sc)
                    val (arrtyp1, eloc1) = get_exp_ctx(e1)
                    unify(t, arrtyp1, loc, "incorrect type of expanded collection")
                    val (colname, d, elemtyp1) = match deref_typ(arrtyp1) {
                        | TypArray(d, elemtyp1) => ("array", d, elemtyp1)
                        | TypList(elemtyp1) => ("list", 1, elemtyp1)
                        | TypString => ("string", 1, TypChar)
                        | _ => throw compile_err(loc, "incorrect type of expanded collection (it should be an array, list or string)")
                        }
                    if d > 2 {
                        throw compile_err(loc, "currently expansion of more than 2-dimensional arrays is not supported")
                    }
                    unify(elemtyp, elemtyp1, eloc1, f"the expanded {colname} elem type does not match the previous elements")
                    (true, d, ExpUnary(OpExpand, e1, (arrtyp1, loc)), loc)
                | _ =>
                    val (elemtyp1, eloc1) = get_exp_ctx(elem)
                    unify(elemtyp, elemtyp1, eloc1, "all the array literal elements should have the same type")
                    (false, 1, check_exp(elem, env, sc), eloc1)
                }
                val row_dims = if row_dims >= 0 { row_dims } else { elem_dims }
                if row_dims == elem_dims {}
                else {
                    throw compile_err( elem_loc,
                        f"dimensionality of array element (={elem_dims}) does not match the previous elements dimensionality (={row_dims}) in the same row")
                }
                (have_expanded_i || is_expanded, row_dims, elem1 :: arow)
            }
            val ncols_i = arow.length()
            val elem_loc = match arow {
                | e :: _ => get_exp_loc(e)
                | _ =>
                    match arows {
                    | r :: _ => get_exp_loc(r.last())
                    | _ => eloc
                    }
                }
            if ncols_i != 0 {}
            else { throw compile_err(elem_loc, f"the {k + 1}-{String.num_suffix(k+1)} matrix row is empty") }
            val have_expanded = have_expanded || have_expanded_i
            if have_expanded || ncols == 0 || ncols == ncols_i {}
            else { throw compile_err(elem_loc, f"the {k + 1}-{String.num_suffix(k+1)} matrix row contains a different number of elements") }
            val dims = if dims < 0 { row_dims } else { 2 }
            (k + 1, if have_expanded_i { ncols_i } else { ncols }, arow.rev() :: arows, have_expanded, dims)
        }
        val atyp = TypArray(dims, elemtyp)
        unify(atyp, etyp, eloc, "the array literal should produce an array")
        ExpMkArray(arows.rev(), ctx)
    | ExpMkRecord(r_e, r_initializers, _) =>
        check_for_rec_field_duplicates([: for (n, _) <- r_initializers {n} :], eloc)
        val fold (r_new_initializers, relems) = ([], []) for (n, e) <- r_initializers {
            val e = check_exp(e, env, sc)
            val etyp = get_exp_typ(e)
            ((n, e) :: r_new_initializers, (n, etyp, None) :: relems)
        }
        val rtyp = TypRecord(ref (relems.rev(), false))
        match r_e {
        | ExpNop(nop_loc) =>
            unify(etyp, rtyp, eloc, "unexpected record type")
            ExpMkRecord(r_e, r_new_initializers.rev(), ctx)
        | _ =>
            val (r_etyp, r_eloc) = get_exp_ctx(r_e)
            val r_expected_typ = TypFun(rtyp :: [], etyp)
            unify(r_etyp, r_expected_typ, r_eloc,
                "there is no proper record constructor/function with record argument")
            val new_r_e = check_exp(r_e, env, sc)
            ExpMkRecord(new_r_e, r_new_initializers.rev(), ctx)
        }
    | ExpUpdateRecord(r_e, r_initializers, _) =>
        check_for_rec_field_duplicates([: for (n, _) <- r_initializers {n} :], eloc)
        val (rtyp, rloc) = get_exp_ctx(r_e)
        unify(rtyp, etyp, eloc, "the types of the update-record argument and the result do not match")
        val new_r_e = check_exp(r_e, env, sc)
        val (_, relems) = get_record_elems(None, rtyp, rloc)
        val new_r_initializers =
        [: for (ni, ei) <- r_initializers {
            val (ei_typ, ei_loc) = get_exp_ctx(ei)
            match relems.find_opt(fun ((nj, _, _): rec_elem_t) {ni == nj}) {
            | Some ((_, ti, _)) =>
                unify(ti, ei_typ, ei_loc,
                    f"invalid type of the initializer of record field '{pp_id2str(ni)}'")
                val new_ei = check_exp(ei, env, sc)
                (ni, new_ei)
            | _ =>
                throw compile_err(ei_loc,
                    f"there is no record field '{pp_id2str(ni)}' in the updated record")
            }
        } :]
        ExpUpdateRecord(new_r_e, new_r_initializers, ctx)
    | ExpTryCatch(e1, cases, _) =>
        val sc = new_try_scope() :: sc
        val (e1typ, e1loc) = get_exp_ctx(e1)
        unify(etyp, e1typ, e1loc, "try body type does match the whole try-catch type")
        val new_e1 = check_exp(e1, env, sc)
        val new_cases = check_cases(cases, TypExn, etyp, env, sc, eloc)
        ExpTryCatch(new_e1, new_cases, ctx)
    | ExpMatch(e1, cases, _) =>
        val new_e1 = check_exp(e1, env, sc)
        val new_e1typ = get_exp_typ(new_e1)
        val new_cases = check_cases(cases, new_e1typ, etyp, env, sc, eloc)
        ExpMatch(new_e1, new_cases, ctx)
    | ExpCast(e1, t1, _) =>
        /* [TODO] check that e1 can be cast to t1 */
        val new_t1 = check_typ(t1, env, sc, eloc)
        unify(etyp, new_t1, eloc, "unexpected type of cast operation")
        val new_e1 = check_exp(e1, env, sc)
        ExpCast(new_e1, new_t1, ctx)
    | ExpTyped(e1, t1, _) =>
        val new_t1 = check_typ(t1, env, sc, eloc)
        val (e1typ, e1loc) = get_exp_ctx(e1)
        unify(etyp, new_t1, eloc, "improper explicit type of the expression")
        unify(e1typ, new_t1, e1loc, "improper explicit type of the expression")
        val new_e1 = check_exp(e1, env, sc)
        ExpTyped(new_e1, new_t1, ctx)
    | ExpCCode(str, _) =>
        match sc {
        | ScModule(_) :: _ => e
        | ScFun(_) :: _ =>
            /* do some favor for those who start to forget to put the final ';' before } in ccode */
            val str = str.strip()
            val str = if str.endswith("}") || str.endswith(";") { str } else { str + ";" }
            ExpCCode(str, ctx)
        | _ =>
            throw compile_err(eloc,
                "ccode may be used only at the top (module level) or as a single expression in function definition")
        }
    /* all the declarations are checked in check_eseq */
    | DefVal(_, _, _, _) | DefFun(_) | DefVariant(_) | DefClass(_) | DefInterface(_)
    | DefExn(_) | DefTyp(_) | DirImport(_, _) | DirImportFrom(_, _, _) | DirPragma(_, _) =>
        throw compile_err(eloc, "internal err: should not get here; all the declarations and directives must be handled in check_eseq")
    }
    new_e
}

fun check_eseq(eseq: exp_t list, env: env_t, sc: scope_t list, create_sc: bool): (exp_t list, env_t) {
    /*
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
    */

    // create the nested block scope if requested
    val sc = if create_sc { new_block_scope() :: sc } else { sc }
    // process directives (currently they are just import directives)
    val env = check_directives(eseq, env, sc)
    // process type declarations: use 2-pass procedure
    // in order to handle mutually-recursive types properly
    val env = reg_types(eseq, env, sc)
    val env = check_types(eseq, env, sc)

    // register exceptions and function declarations
    val fold env = env for e <- eseq {
        match e {
        | DefFun(df) => reg_deffun(df, env, sc)
        | DefExn(de) => check_defexn(de, env, sc)
        | _ => env
        }
    }
    val nexps = eseq.length()

    // finally, process everything else:
    // function bodies, values declarations as well as normal expressions/statements
    val (eseq, env, _) =
    fold (eseq, env, idx) = ([], env, 0) for e <- eseq {
        val (eseq1, env1) =
        try {
            match e {
            | DirImport(_, _) | DirImportFrom(_, _, _) | DirPragma(_, _) | DefTyp(_) | DefVariant(_) | DefClass(_) | DefInterface(_) | DefExn(_) =>
                (e :: eseq, env)
            | DefVal(p, e, flags, loc) =>
                val is_mutable = flags.val_flag_mutable
                val t = get_exp_typ(e)
                val p = match p {
                | PatTyped(p1, t1, loc) =>
                    val t1 = check_typ(t1, env, sc, loc)
                    unify(t, t1, loc,
                        "explicit type specification of the defined value does not match the assigned expression type")
                    p1
                | _ => p
                }
                val e1 = try { check_exp(e, env, sc) } catch { | CompileError(_, _) as err => push_compile_err(err); e }
                val (p1, env1, _, _) = check_pat(p, t, env, make_empty_idset(), make_empty_idset(), sc, false, true, is_mutable)
                (DefVal(p1, e1, flags, loc) :: eseq, env1)
            | DefFun(df) =>
                val df =
                    if df->df_templ_args.empty() { check_deffun(df, env) }
                    else {
                        // update environment of the template function
                        // to give it access to the above defined
                        // non-template values
                        *df = df->{df_env=env}
                        df
                    }
                    (DefFun(df) :: eseq, env)
            | _ =>
                val e = check_exp(e, env, sc)
                val (etyp, eloc) = get_exp_ctx(e)
                match e {
                | ExpNop(_) => if nexps != 1 { throw compile_err(eloc, "there cannot be {} operators inside code blocks") }
                | ExpBreak(_, _) | ExpContinue(_) =>
                    if idx != nexps - 1 {
                        throw compile_err( eloc,
                            "break/continue operator should not be followed by any other operators in the same linear code sequence")
                    }
                | ExpCCode(_, _) => {}
                | _ =>
                    match deref_typ(etyp) {
                    | TypVoid | TypDecl => {}
                    | TypVar (ref None) => {}
                    | _ =>
                        if idx != nexps - 1 {
                            throw compile_err( eloc,
                                "non-void expression occurs before the end of code block. Check the line breaks; if it's valid, use ignore() function to dismiss the error")
                        }
                    }
                }
                (e :: eseq, env)
            }
        } catch {
            | CompileError(_, _) as err =>
                push_compile_err(err)
                (e :: eseq, env)
            | PropagateCompileError => (e :: eseq, env)
        }
        (eseq1, env1, idx + 1)
    }
    check_compile_errs()
    (eseq.rev(), env)
}

fun check_directives(eseq: exp_t list, env: env_t, sc: scope_t list) {
    fun is_imported(alias: id_t, n: id_t, env: env_t, allow_duplicate_import: bool, loc: loc_t) =
        find_all(alias, env).exists( fun (entry: env_entry_t) {
            | EnvId(m) =>
                try {
                    val minfo = get_module(m)
                    val mname = minfo->dm_name
                    if mname == n {
                        val astr = pp_id2str(alias)
                        val mstr = pp_id2str(mname)
                        if allow_duplicate_import { true }
                        else if astr == mstr { throw compile_err(loc, f"duplicate import of {mstr}") }
                        else { throw compile_err(loc, f"duplicate import of {mstr} as {astr}") }
                    } else {
                        throw compile_err(loc, f"another module {pp_id2str(mname)} has been already imported as {pp_id2str(alias)}")
                    }
                } catch { | Fail(_) => false }
            | EnvTyp(_) => false
        })

    fun import_entries(env: env_t, parent_mod: id_t, key: id_t, entries: env_entry_t list, loc: loc_t) =
        fold env = env for i <- entries.rev() {
            match i {
            | EnvId(i) =>
                val info = id_info(i)
                val sc = get_scope(info)
                match (info, sc) {
                | (_, (ScModule(m) :: _)) when parent_mod == noid || parent_mod == m => add_id_to_env(key, i, env)
                | (IdModule _, _) => add_id_to_env(key, i, env)
                | _ => env
                }
            | EnvTyp(_) => env
            }
        }

    fun import_mod(env: env_t, alias: id_t, m: id_t, allow_duplicate_import: bool, loc: loc_t) =
        if is_imported(alias, m, env, allow_duplicate_import, loc) {
            env
        } else {
            // add the imported module id to the env
            val env = add_id_to_env(alias, m, env)
            val menv = get_module(m)->dm_env
            // and also import all the overloaded operators from the module
            // to make them usable in the corresponding arithmetic expressions
            fold env = env for op_name <- fname_always_import() {
                val entries = find_all(op_name, menv)
                import_entries(env, m, op_name, entries, loc)
            }
        }

    val fold (env, mlist) = (env, []) for e <- eseq {
        match e {
        | DirImport(impdirs, eloc) =>
            (fold env = env for (m, alias) <- impdirs {
                try {
                    import_mod(env, alias, m, true, eloc)
                } catch { | CompileError(_, _) as err => push_compile_err(err); env }
            }, mlist)
        | DirImportFrom(m, implist, eloc) =>
            val env =
            try {
                val menv = get_module(m)->dm_env
                val keys = if implist != [] { implist }
                    else { menv.foldl(fun (k: id_t, _: env_entry_t list, l: id_t list) { k :: l }, ([]: id_t list)) }
                val fold env = env for k <- keys {
                    try {
                        val entries = find_all(k, menv)
                        if entries != [] {}
                        else { throw compile_err(eloc, f"no symbol {pp_id2str(k)} found in {pp_id2str(m)}") }
                        import_entries(env, m, k, entries, eloc)
                    } catch { | CompileError(_, _) as err => push_compile_err(err); env }
                }
                // after processing "from m import ..." we also implicitly insert "import m"
                // [TODO] for each 'imported from' variant type we also need to import all its constructors
                val alias = get_orig_id(m)
                import_mod(env, alias, m, true, eloc)
            } catch { | CompileError(_, _) as err => push_compile_err(err);  env }
            (env, (m, eloc) :: mlist)
        | DirPragma(prl, eloc) => (env, mlist)
        | _ => (env, mlist)
        }
    }
    check_compile_errs()
    env
}

// create fresh unique name for each type and register it (put it to env)
fun reg_types(eseq: exp_t list, env: env_t, sc: scope_t list) {
    fun make_default_alias(templ_args: id_t list, tn: id_t) = TypApp([: for targ <- templ_args { TypApp([], targ) } :], tn)

    fold env = env for e <- eseq {
        match e {
        | DefTyp(dt) =>
            val {dt_name, dt_templ_args, dt_typ, dt_loc} = *dt
            val dt_name1 = dup_id(dt_name)
            *dt = deftyp_t {dt_name=dt_name1, dt_templ_args=dt_templ_args, dt_typ=dt_typ, dt_finalized=false, dt_scope=sc, dt_loc=dt_loc}
            set_id_entry(dt_name1, IdTyp(dt))
            add_id_to_env_check(dt_name, dt_name1, env, check_for_duplicate_typ(dt_name, sc, dt_loc))
        | DefVariant(dvar) =>
            val {dvar_name, dvar_templ_args, dvar_flags, dvar_cases, dvar_loc} = *dvar
            val dvar_name1 = dup_id(dvar_name)
            val dvar_alias1 = make_default_alias(dvar_templ_args, dvar_name1)
            val dummy_ctors = [: for (n, _) <- dvar_cases {n} :]
            *dvar = defvariant_t {
                dvar_name=dvar_name1,
                dvar_templ_args=dvar_templ_args,
                dvar_alias=dvar_alias1,
                dvar_flags=dvar_flags,
                dvar_cases=dvar_cases,
                dvar_ctors=dummy_ctors,
                dvar_templ_inst=ref [],
                dvar_scope=sc,
                dvar_loc=dvar_loc
                }
            set_id_entry(dvar_name1, IdVariant(dvar))
            add_id_to_env_check(dvar_name, dvar_name1, env, check_for_duplicate_typ(dvar_name, sc, dvar_loc))
        | DefClass (ref {dcl_loc=loc}) => throw compile_err(loc, "classes are not supported yet")
        | DefInterface (ref {di_loc=loc}) => throw compile_err(loc, "interfaces are not supported yet")
        | _ => env
        }
    }
}

fun register_typ_constructor(n: id_t, ctor: fun_constr_t, templ_args: id_t list, argtyps: typ_t list,
                            rt: typ_t, env: env_t, sc: scope_t list, decl_loc: loc_t) {
    val ctor_typ = match argtyps { | [] => rt | _ => TypFun(argtyps, rt) }
    val args = [: for _@i <- argtyps { PatIdent(get_id(f"arg{i}"), decl_loc) } :]
    val ctor_name = dup_id(n)
    val df = ref (deffun_t {
        df_name=ctor_name,
        df_templ_args=templ_args,
        df_args=args,
        df_typ=ctor_typ,
        df_body=ExpNop(decl_loc),
        df_flags=default_fun_flags().{fun_flag_ctor=ctor},
        df_templ_inst=ref [],
        df_scope=sc,
        df_env=env,
        df_loc=decl_loc
        })
    set_id_entry(ctor_name, IdFun(df))
    (ctor_name, ctor_typ)
}

// check the type definition body (for simple types, including records, and also variant types)
fun check_types(eseq: exp_t list, env: env_t, sc: scope_t list) =
    fold env = env for e <- eseq {
        match e {
        | DefTyp(dt) =>
            val {dt_name, dt_templ_args, dt_typ, dt_scope, dt_loc} = *dt
            val fold env1 = env for t_arg <- dt_templ_args {
                add_typ_to_env(t_arg, TypApp([], t_arg), env1)
            }
            val dt_typ = deref_typ(check_typ(dt_typ, env1, dt_scope, dt_loc))
            val dt_typ = finalize_record_typ(dt_typ, dt_loc)
            *dt = deftyp_t {dt_name=dt_name, dt_templ_args=dt_templ_args, dt_typ=dt_typ, dt_finalized=true, dt_scope=dt_scope, dt_loc=dt_loc}
            // in the case of record we add the record constructor function
            // to support the constructions 'record_name { f1=e1, f2=e2, ..., fn=en }' gracefully
            match dt_typ {
            | TypRecord(_) =>
                val (cname, ctyp) = register_typ_constructor(dt_name, CtorStruct, dt_templ_args, dt_typ :: [], dt_typ, env1, dt_scope, dt_loc)
                add_id_to_env_check(get_orig_id(dt_name), cname, env, check_for_duplicate_fun(ctyp, env, dt_scope, dt_loc))
            | _ => env
            }
        | DefVariant(dvar) =>
            instantiate_variant(([]: typ_t list), dvar, env, sc, dvar->dvar_loc)
            val {dvar_name, dvar_templ_args, dvar_cases, dvar_ctors, dvar_scope, dvar_loc} = *dvar
            fold env=env for (n, t) <- dvar_cases, ctor_name <- dvar_ctors {
                val {df_name, df_templ_args, df_typ} =
                    match id_info(ctor_name) {
                    | IdFun(df) => *df
                    | _ => throw compile_err(dvar_loc, f"internal error: constructor {id2str(ctor_name)} is not a function")
                    }
                val (t, _) = preprocess_templ_typ(df_templ_args, df_typ, env, sc, dvar_loc)
                add_id_to_env_check(n, ctor_name, env, check_for_duplicate_fun(t, env, sc, dvar_loc))
            }
        | _ => env
        }
    }

/*
    * create fresh unique name for the function
    * check its arguments (very light sanity check for template functions;
        full-scale check for non-template function)
    * update the function definition (put in the new name, verified args, verified function type, the correct scope)
    * put the entry into the global symbol table
    * add it into the environment
*/
fun reg_deffun(df: deffun_t ref, env: env_t, sc: scope_t list) {
    val {df_name, df_args, df_typ, df_body, df_flags, df_loc} = *df
    val df_name1 = dup_id(df_name)
    val rt = match df_typ {
            | TypFun(_, rt) => rt
            | _ => throw compile_err(df_loc, "incorrect function type")
            }
    val df_sc = ScFun(df_name1) :: sc
    val fold (args1, argtyps1, env1, idset1, templ_args1) = ([], [], env, make_empty_idset(), make_empty_idset()) for arg <- df_args {
        val t = make_new_typ()
        val (arg1, env1, idset1, templ_args1) = check_pat(arg, t, env1, idset1, templ_args1, df_sc, true, true, false)
        (arg1 :: args1, t :: argtyps1, env1, idset1, templ_args1)
    }
    val dummy_rt_pat = PatTyped(PatAny(df_loc), rt, df_loc)
    val (dummy_rt_pat1, env1, idset1, templ_args1) = check_pat(dummy_rt_pat, make_new_typ(), env1, idset1, templ_args1, df_sc, true, true, false)
    val rt = match dummy_rt_pat1 {
            | PatTyped(_, rt, _) => rt
            | _ => throw compile_err(df_loc, "invalid return pattern after check")
            }
    val df_typ1 = TypFun(argtyps1.rev(), rt)
    val env1 = add_id_to_env_check(df_name, df_name1, env1, check_for_duplicate_fun(df_typ1, env1, sc, df_loc))
    *df = deffun_t {
        df_name=df_name1,
        df_templ_args=templ_args1.list(),
        df_args=args1.rev(),
        df_typ=df_typ1,
        df_body=df_body,
        df_flags=df_flags,
        df_scope=sc,
        df_loc=df_loc,
        df_templ_inst=ref [],
        df_env=env1
        }
    set_id_entry(df_name1, IdFun(df))
    env1
}

fun check_defexn(de: defexn_t ref, env: env_t, sc: scope_t list) {
    val {dexn_name=n0, dexn_typ=t, dexn_loc=loc} = *de
    match sc {
    | ScModule(_) :: _ => {}
    | _ => throw compile_err(loc, "exceptions can only be defined at a module level")
    }
    val t = check_typ(t, env, sc, loc)
    val n = dup_id(n0)
    val ftyp = typ2constr(t, TypExn, loc)
    *de = defexn_t {dexn_name=n, dexn_typ=t, dexn_scope=sc, dexn_loc=loc}
    set_id_entry(n, IdExn(de))
    match sc {
    | (ScModule(m) :: _) when pp_id2str(m) == "Builtins" =>
        // add both forward and inverse mapping
        builtin_exceptions = builtin_exceptions.add(n0, n)
        builtin_exceptions = builtin_exceptions.add(n, n0)
    | _ => {}
    }
    add_id_to_env_check(n0, n, env, check_for_duplicate_fun(ftyp, env, sc, loc))
}

fun check_deffun(df: deffun_t ref, env: env_t) {
    val {df_typ, df_scope, df_loc} = *df
    instantiate_fun(df, df_typ, env, df_scope, df_loc, false)
}

/*
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
*/
fun check_typ_and_collect_typ_vars(t: typ_t, env: env_t, r_opt_typ_vars: idset_t ref?, sc: scope_t list, loc: loc_t) {
    var r_env = env
    fun check_typ_(t: typ_t, callb: ast_callb_t) =
        match t {
        | TypApp(ty_args, n) =>
            val ty_args = [: for t <- ty_args { check_typ_(t, callb) } :]
            val ty_args_are_real = ty_args.all(is_real_typ)
            val found_typ_opt = find_first(n, r_env, loc, fun (entry: env_entry_t): typ_t? {
                    | EnvTyp(t) =>
                        if ty_args.empty() { Some(t) }
                        else { throw compile_err(loc, f"a concrete type '{pp_id2str(n)}' cannot be further instantiated") }
                    | EnvId(IdName(_)) => None
                    | EnvId(i) =>
                        match id_info(i) {
                        | IdNone | IdDVal(_) | IdFun(_) | IdExn(_) | IdModule(_) => None
                        | IdClass(_) | IdInterface(_) => throw compile_err(loc, "classes & interfaces are not supported yet")
                        | IdTyp(dt) =>
                            val {dt_name, dt_templ_args, dt_typ, dt_scope, dt_finalized, dt_loc} = *dt
                            if dt_finalized {}
                            else {
                                throw compile_err( loc,
                                    f"later declared non-variant type '{pp_id2str(dt_name)}' is referenced; try to reorder the type declarations")
                            }
                            if dt_name == n && ty_args.empty() { Some(t) }
                            else {
                                val env1 = match_ty_templ_args(ty_args, dt_templ_args, r_env, dt_loc, loc)
                                Some(check_typ(dup_typ(dt_typ), env1, sc, loc))
                            }
                        | IdVariant(dvar) =>
                            val {dvar_name, dvar_templ_args, dvar_alias, dvar_templ_inst} = *dvar
                            Some(
                                if dvar_templ_args.empty() {
                                    dvar_alias
                                } else if ty_args_are_real {
                                    val t1 = TypApp(ty_args, dvar_name)
                                    match dvar_templ_inst->find_opt(
                                        fun (inst) {
                                            match id_info(inst) {
                                            | IdVariant(dvar_inst) =>
                                                val {dvar_alias=dvar_inst_alias} = *dvar_inst
                                                maybe_unify(t1, dvar_inst_alias, true)
                                            | _ =>
                                                throw compile_err(loc, f"invalid type of variant instance {id2str(i)} (must be also a variant)")
                                            }
                                        }) {
                                    | Some _ => t1
                                    | _ =>
                                        val (_, inst_app_typ) = instantiate_variant(ty_args, dvar, r_env, sc, loc);
                                        inst_app_typ
                                    }
                                } else {
                                    TypApp(ty_args, dvar_name)
                                })
                        }
                    })
            match (r_opt_typ_vars, ty_args, found_typ_opt) {
            | (_, _, Some(new_t)) => new_t
            | (Some(r_typ_vars), [], _) when id2str(n).startswith("'") =>
                *r_typ_vars = r_typ_vars->add(n)
                r_env = add_typ_to_env(n, t, r_env)
                t
            | _ => throw report_not_found(n, loc)
            }
        | TypVarTuple(t_opt) =>
            match r_opt_typ_vars {
            | Some(r_typ_vars) => *r_typ_vars = r_typ_vars->add(get_id("__var_tuple__"))
            | _ => {}
            }
            walk_typ(t, callb)
        | TypVarArray(t_opt) =>
            match r_opt_typ_vars {
            | Some(r_typ_vars) => *r_typ_vars = r_typ_vars->add(get_id("__var_array__"))
            | _ => {}
            }
            walk_typ(t, callb)
        | TypVarRecord =>
            match r_opt_typ_vars {
            | Some(r_typ_vars) => *r_typ_vars = r_typ_vars->add(get_id("__var_record__"))
            | _ => {}
            }
            walk_typ(t, callb)
        | TypRecord (ref (relems, _)) =>
            check_for_rec_field_duplicates([: for (n, _, _) <- relems {n} :], loc)
            walk_typ(t, callb)
        | _ => walk_typ(t, callb)
        }

    val callb = ast_callb_t {ast_cb_typ=Some(check_typ_), ast_cb_exp=None, ast_cb_pat=None}
    (check_typ_(t, callb), r_env)
}

fun check_typ(t: typ_t, env: env_t, sc: scope_t list, loc: loc_t): typ_t =
    check_typ_and_collect_typ_vars(t, env, None, sc, loc).0

fun instantiate_fun(templ_df: deffun_t ref, inst_ftyp: typ_t, inst_env0: env_t,
                    inst_sc: scope_t list, inst_loc: loc_t, instantiate: bool): deffun_t ref {
    val {df_name} = *templ_df
    if instantiate { compile_err_ctx = f"when instantiating '{pp_id2str(df_name)}' at {inst_loc}" :: compile_err_ctx }
    try {
        val inst_df = instantiate_fun_(templ_df, inst_ftyp, inst_env0, inst_sc, inst_loc, instantiate)
        inst_df
    } finally {
        if instantiate { compile_err_ctx = compile_err_ctx.tl() }
    }
}

fun instantiate_fun_(templ_df: deffun_t ref, inst_ftyp: typ_t, inst_env0: env_t,
                     inst_sc: scope_t list, inst_loc: loc_t, instantiate: bool): deffun_t ref {
    val {df_name, df_templ_args, df_args, df_body, df_flags, df_scope, df_loc, df_templ_inst} = *templ_df
    val is_constr = is_fun_ctor(df_flags)
    if !is_constr {}
    else {
        throw compile_err( inst_loc,
            f"internal error: attempt to instantiate constructor '{id2str(df_name)}'. it should be instantiated in a different way. try to use explicit type specification somewhere")
    }
    val nargs = df_args.length()
    val inst_env = inst_env0
    val inst_ftyp = deref_typ(inst_ftyp)
    val (arg_typs, rt) = match inst_ftyp {
        | TypFun(TypVoid :: [], rt) => ([], rt)
        | TypFun(arg_typs, rt) => (arg_typs, rt)
        | rt => if is_constr { ([], rt) } else {
                    throw compile_err(inst_loc, "internal error: the type of instantiated function is not a function")
                }
        }
    val ninst_args = arg_typs.length()
    val arg_typs = if nargs == ninst_args { arg_typs }
            else if nargs == 1 { TypTuple(arg_typs) :: [] }
            else {
                match arg_typs {
                | (TypTuple(elems) :: []) when elems.length() == nargs => elems
                | _ =>
                    throw compile_err( df_loc,
                        f"during instantion at {inst_loc}: incorrect number of actual parameters ={ninst_args} (vs expected {nargs})")
                }
            }
    val inst_name = if instantiate { dup_id(df_name) } else { df_name }
    val fun_sc = ScFun(inst_name) :: inst_sc
    val fold (df_inst_args, inst_env, tmp_idset) = ([], inst_env, make_empty_idset()) for df_arg <- df_args, arg_typ <- arg_typs {
        val (df_inst_arg, inst_env, tmp_idset, _) = check_pat(dup_pat(df_arg), arg_typ, inst_env, tmp_idset, make_empty_idset(), fun_sc, false, true, false)
        (df_inst_arg :: df_inst_args, inst_env, tmp_idset)
    }
    val df_inst_args = df_inst_args.rev()
    val rt = check_typ(rt, inst_env, df_scope, inst_loc)
    val inst_body = if instantiate { dup_exp(df_body) } else { df_body }
    val (body_typ, body_loc) = get_exp_ctx(inst_body)
    if is_constr {} else { unify(body_typ, rt, body_loc, "the function body type does not match the function type") }
    val inst_ftyp = match (arg_typs, is_constr) {
                    | ([], true) => rt
                    | _ => TypFun(arg_typs, rt)
                    }
    val inst_df = ref (deffun_t {
        df_name=inst_name,
        df_templ_args=[],
        df_args=df_inst_args,
        df_typ=inst_ftyp,
        df_body=inst_body,
        df_flags=df_flags,
        df_scope=inst_sc,
        df_loc=inst_loc,
        df_templ_inst=ref [],
        df_env=inst_env0
        })
    set_id_entry(inst_name, IdFun(inst_df))
    if instantiate { *templ_df->df_templ_inst = inst_name :: *templ_df->df_templ_inst }
    val inst_body = instantiate_fun_body(inst_name, inst_ftyp, df_inst_args, inst_body, inst_env, fun_sc, inst_loc)
    // update the function return type
    unify(get_exp_typ(inst_body), rt, inst_loc, "the function body has inconsistent type")
    val inst_ftyp = match (arg_typs, is_constr) {
                    | ([], true) => rt
                    | _ => TypFun(arg_typs, rt)
                    }
    *inst_df = inst_df->{df_body=inst_body, df_typ=inst_ftyp}
    inst_df
}

fun instantiate_fun_body(inst_name: id_t, inst_ftyp: typ_t, inst_args: pat_t list, inst_body: exp_t,
                        inst_env: env_t, fun_sc: scope_t list, inst_loc: loc_t) {
    val ftyp = deref_typ_rec(inst_ftyp)
    val body_loc = get_exp_loc(inst_body)
    match pp_id2str(inst_name) {
    | "__eq_variants__" =>
        match (ftyp, inst_args) {
        | (TypFun(TypApp([], n1) :: TypApp([], n2) :: [], TypBool), PatTyped(PatIdent(a, _), _, _) :: PatTyped(PatIdent(b, _), _, _) :: [])
              when n1 == n2 && (match id_info(n1) {
                               | IdVariant(_) => true
                               | _ => false
                               }) =>
            /*
               if the variant is an instance of template variant,
               we take the variant cases from the original prototype, not from the instance.
               This is because we need to correctly calculate the number of parameters for each
               variant constructor.

               e.g. type 't option = None | Some: 't
               type_t i2_opt = (int, int) option
               Some() for i2_opt will still have 1 parameter.
            */
            val argtyp = TypApp([], n1)
            val astr = pp_id2str(a)
            val bstr = pp_id2str(b)
            val (var_ctors, proto_cases) =
            match id_info(n1) {
            | IdVariant (ref {dvar_ctors, dvar_alias=TypApp(_, proto_n)}) =>
                match id_info(proto_n) {
                | IdVariant (ref {dvar_cases=proto_cases}) => (dvar_ctors, proto_cases)
                | _ => throw compile_err(inst_loc, "the prototype of variant instance should be a variant")
                }
            | _ => throw compile_err(inst_loc, "variant is expected here")
            }
            val fold complex_cases = ([]: (pat_t list, exp_t) list) for n <- var_ctors, (n_orig, t_orig) <- proto_cases {
                val t = deref_typ_rec(t_orig)
                match t {
                | TypVoid => complex_cases
                | TypRecord (ref (relems, _)) =>
                    val (_, al, bl, cmp_code) =
                    fold (idx, al, bl, cmp_code) = (1, [], [], ExpNop(body_loc)) for (rn, _, _) <- relems {
                        val ai = get_id(f"{astr}{idx}")
                        val bi = get_id(f"{bstr}{idx}")
                        val cmp_ab =
                        ExpBinary(OpCmp(CmpEQ),
                            ExpIdent(ai, (make_new_typ(), body_loc)),
                            ExpIdent(bi, (make_new_typ(), body_loc)),
                            (TypBool, body_loc))
                        val cmp_code = if idx == 1 { cmp_ab }
                                    else { ExpBinary(OpBitwiseAnd, cmp_code, cmp_ab, (TypBool, body_loc)) }
                        (idx + 1, (rn, PatIdent(ai, body_loc)) :: al, (rn, PatIdent(bi, body_loc)) :: bl, cmp_code)
                    }
                    val a_case_pat = PatRecord(Some(n), al.rev(), body_loc)
                    val b_case_pat = PatRecord(Some(n), bl.rev(), body_loc)
                    val ab_case_pat = PatTuple([: a_case_pat, b_case_pat :], body_loc)
                    (ab_case_pat :: [], cmp_code) :: complex_cases
                | _ =>
                    val args = match t {
                                | TypTuple(tl) => tl
                                | _ => t :: []
                                }
                    val (_, al, bl, cmp_code) =
                    fold (idx, al, bl, cmp_code) = (1, [], [], ExpNop(body_loc)) for _ <- args {
                        val ai = get_id(f"{astr}{idx}")
                        val bi = get_id(f"{bstr}{idx}")
                        val cmp_ab = ExpBinary(OpCmp(CmpEQ),
                            ExpIdent(ai, (make_new_typ(), body_loc)),
                            ExpIdent(bi, (make_new_typ(), body_loc)),
                            (TypBool, body_loc))
                        val cmp_code = if idx == 1 { cmp_ab }
                                else { ExpBinary(OpBitwiseAnd, cmp_code, cmp_ab, (TypBool, body_loc)) }
                        (idx + 1, PatIdent(ai, body_loc) :: al, PatIdent(bi, body_loc) :: bl, cmp_code)
                    }
                    val a_case_pat = PatVariant(n, al.rev(), body_loc)
                    val b_case_pat = PatVariant(n, bl.rev(), body_loc)
                    val ab_case_pat = PatTuple([: a_case_pat, b_case_pat :], body_loc)
                    (ab_case_pat :: [], cmp_code) :: complex_cases
                }
            }
            val a = ExpIdent(get_id(astr), (argtyp, body_loc))
            val b = ExpIdent(get_id(bstr), (argtyp, body_loc))
            val tag = ExpIdent(get_id("__tag__"), (TypString, body_loc))
            val a_tag = ExpMem(a, tag, (TypInt, body_loc))
            val b_tag = ExpMem(b, tag, (TypInt, body_loc))
            val cmp_tags = ExpBinary(OpCmp(CmpEQ), a_tag, b_tag, (TypBool, body_loc))
            val inst_body =
            match complex_cases {
            | [] => cmp_tags
            | _ =>
                val default_case = (PatAny(body_loc) :: [], cmp_tags)
                val ab = ExpMkTuple([: a, b :], (TypTuple([: argtyp, argtyp :]), body_loc))
                ExpMatch(ab, (default_case :: complex_cases).rev(), (TypBool, body_loc))
            }
            val body = check_exp(inst_body, inst_env, fun_sc)
            body
        | _ => throw compile_err(inst_loc, "__eq_variants__ can only be applied to 2 variants of the same type")
        }
    | _ => check_exp(inst_body, inst_env, fun_sc)
    }
}

fun instantiate_variant(ty_args: typ_t list, dvar: defvariant_t ref, env: env_t, sc: scope_t list, loc: loc_t) {
    val {dvar_name, dvar_templ_args, dvar_alias, dvar_flags, dvar_cases, dvar_ctors, dvar_scope, dvar_loc} = *dvar
    /*
       env - the input environment updated with the mappings from the formal variant type parameters to the actual values;
       inst_typ - the return type in the variant constructors; it is used to inference the return type of constructors.
       inst_app_typ - typ used to reference the original template type from the instance
            (or reference the template type from itself). it's used to search for the proper instance of the variant type.
    */
    val (instantiate, env, inst_name, inst_app_typ, inst_dvar) =
        match ty_args {
        | [] => val fold env = env for tn <- dvar_templ_args { add_typ_to_env(tn, TypApp([], tn), env) }
                (false, env, dvar_name, dvar_alias, dvar)
        | _ =>
            val inst_name = dup_id(dvar_name)
            val inst_app_typ = TypApp(ty_args, dvar_name)
            (true,
            match_ty_templ_args(ty_args, dvar_templ_args, env, dvar_loc, loc),
            inst_name, inst_app_typ,
            ref (defvariant_t {
                    dvar_name=inst_name,
                    dvar_templ_args=[],
                    dvar_alias=inst_app_typ,
                    dvar_flags=dvar_flags,
                    dvar_cases=dvar_cases,
                    dvar_ctors=dvar_ctors,
                    dvar_templ_inst=ref [],
                    dvar_scope=dvar_scope,
                    dvar_loc=loc
                    }))
        }
    // register the incomplete-yet variant in order to avoid inifinite loop
    if instantiate {
        set_id_entry(inst_name, IdVariant(inst_dvar))
        *dvar->dvar_templ_inst = inst_name :: *dvar->dvar_templ_inst
    }

    val fold (idx, inst_cases, inst_ctors) =
        (0, [], []) for (n, t) <- dvar_cases, ctor_name <- dvar_ctors {
        val nargs = match t {
                    | TypTuple(telems) => telems.length()
                    | TypVoid => 0
                    | _ => 1
                    }
        val t = check_typ(dup_typ(t), env, sc, loc)
        val t = finalize_record_typ(t, loc)
        val nrealargs = match t {
                        | TypTuple(telems) => telems.length()
                        | TypVoid => 0
                        | _ => 1
                        }
        val argtyps =
        match (t, nargs) {
        | (TypVoid, 0) => []
        | (_, 1) => t :: []
        | (TypTuple(telems), _) when nrealargs == nargs => telems
        | _ =>
            throw compile_err( loc,
                f"cannot instantiate case '{id2str(ctor_name)}' of variant '{id2str(dvar_name)}' defined at '{dvar_loc}':"
                + f"wrong number of actual parameters {nrealargs} (vs {nargs} expected)" )
        }
        val (inst_cname, _) =
        register_typ_constructor(ctor_name, CtorVariant(n), inst_dvar->dvar_templ_args, argtyps, inst_app_typ, env, dvar_scope, dvar_loc)
        if instantiate {
            match id_info(ctor_name) {
            | IdFun(c_def) => *c_def->df_templ_inst = inst_cname :: *c_def->df_templ_inst
            | _ => throw compile_err(loc, f"invalid constructor {id2str(ctor_name)} of variant {id2str(dvar_name)}")
            }
        }
        (idx + 1, (n, t) :: inst_cases, inst_cname :: inst_ctors)
    }
    *inst_dvar = inst_dvar->{dvar_cases=inst_cases.rev(), dvar_ctors=inst_ctors.rev()}
    (inst_name, inst_app_typ)
}

fun check_pat(pat: pat_t, typ: typ_t, env: env_t, idset: idset_t, typ_vars: idset_t,
              sc: scope_t list, proto_mode: bool, simple_pat_mode: bool, is_mutable: bool) {
    var r_idset = idset
    var r_env = env
    val r_typ_vars = ref typ_vars
    val captured_val_flags = default_val_flags().{val_flag_mutable=is_mutable}
    fun process_id(i, t, loc: loc_t) {
        val i0 = get_orig_id(i)
        if r_idset.mem(i0) {
            throw compile_err(loc, f"duplicate identifier '{id2str(i0)}' in the pattern")
        } else {
            r_idset = r_idset.add(i0)
        }
        if proto_mode {
            i0
        } else {
            val j = dup_id(i0)
            val dv = defval_t {dv_name=j, dv_typ=t, dv_flags=captured_val_flags, dv_scope=sc, dv_loc=loc}
            set_id_entry(j, IdDVal(dv))
            r_env = add_id_to_env(i0, j, r_env)
            j
        }
    }
    fun check_pat_(p: pat_t, t: typ_t) =
        match p {
        | PatAny(_) => p
        | PatLit(l, loc) =>
            if simple_pat_mode {
                throw compile_err(loc, "literals are not allowed here")
            }
            unify(t, get_lit_typ(l), loc, "the literal of unexpected type")
            p
        | PatIdent(i, loc) =>
            if pp_id2str(i) == "_" { throw compile_err(loc, "'_' occured in PatIdent()") }
            PatIdent(process_id(i, t, loc), loc)
        | PatTuple(pl, loc) =>
            val tl = [: for p <- pl { make_new_typ() } :]
            unify(t, TypTuple(tl), loc, "improper type of the tuple pattern")
            PatTuple([: for p <- pl, t <- tl {check_pat_(p, t)} :], loc)
        | PatVariant(v, pl, loc) =>
            if !proto_mode {
                /* [TODO] in the ideal case this branch should work fine in the prototype mode as well,
                   just need to make lookup_id smart enough (maybe add some extra parameters to
                   avoid preliminary type instantiation) */
                val tl = [: for p <- pl { make_new_typ() } :]
                val ctyp = match tl { | [] => t | _ => TypFun(tl, t) }
                val v_new = lookup_id(v, ctyp, r_env, sc, loc)
                PatVariant(v_new, [: for p <- pl, t <- tl { check_pat_(p, t) } :], loc)
            } else {
                match deref_typ(t) {
                | TypApp(ty_args, n) =>
                    match id_info(n) {
                    | IdVariant(dv) =>
                        val {dvar_cases} = *dv
                        if dvar_cases.length() != 1 {
                            throw compile_err(loc, "a label of multi-case variant may not be used in a formal function parameter")
                        } else {
                            val ni = match dvar_cases.assoc_opt(v) {
                                | Some(TypTuple(tl)) => tl.length()
                                | Some(TypVoid) => throw compile_err(loc,
                                    f"a variant label '{pp_id2str(v)}' with no arguments may not be used in a formal function parameter")
                                | Some(_) => 1
                                | _ => throw compile_err(loc, f"the variant constructor '{pp_id2str(v)}' is not found")
                                }
                            if ni != pl.length() {
                                throw compile_err( loc,
                                    f"the number of variant pattern arguments does not match to the description of variant case '{pp_id2str(v)}'")
                            }
                            PatVariant(v, pl, loc)
                        }
                    | _ => throw compile_err(loc, "variant pattern is used with non-variant type")
                    }
                | _ => throw compile_err(loc, "variant pattern is used with non-variant type")
                }
            }
        | PatRecord(rn_opt, relems, loc) =>
            if !proto_mode {
                /* [TODO] in the ideal case this branch should work fine in the prototype mode as well,
                   just need to make lookup_id smart enough (maybe add some extra parameters to
                   avoid preliminary type instantiation) */
                val (ctor, relems_found) = get_record_elems(rn_opt, t, loc)
                val fold new_relems = [] for (n, p) <- relems {
                    val n_orig = get_orig_id(n)
                    match relems_found.find_opt(fun ((nj, tj, _): rec_elem_t) { get_orig_id(nj) == n_orig }) {
                    | Some((nj, tj, _)) =>
                        val p = check_pat_(p, tj)
                        (n, p) :: new_relems
                    | _ =>
                        throw compile_err( loc,
                            f"element '{pp_id2str(n)}' is not found in the record '{pp_id2str(rn_opt.getsome(noid))}'")
                    }
                }
                PatRecord(if ctor != noid {Some(ctor)} else {None}, new_relems.rev(), loc)
            } else {
                throw compile_err(loc, "record patterns are not supported in function prototypes yet")
            }
        | PatCons(p1, p2, loc) =>
            val t1 = make_new_typ()
            val t2 = TypList(t1)
            if simple_pat_mode { throw compile_err(loc, "'::' pattern is not allowed here") }
            unify(t, t2, loc, "'::' pattern is used with non-list type")
            PatCons(check_pat_(p1, t1), check_pat_(p2, t2), loc)
        | PatAs(p1, i, loc) =>
            val p1_new = check_pat_(p1, t)
            val i_new = process_id(i, t, loc)
            PatAs(p1_new, i_new, loc)
        | PatTyped(p1, t1, loc) =>
            val (t1_new, env1) = check_typ_and_collect_typ_vars(t1, r_env,
                    if proto_mode {Some(r_typ_vars)} else {None}, sc, loc)
            r_env = env1
            unify(t1_new, t, loc, "inconsistent explicit type specification")
            PatTyped(check_pat_(p1, t), t1_new, loc)
        | PatRef(p1, loc) =>
            val t1 = make_new_typ()
            unify(t, TypRef(t1), loc, "'ref' pattern is used with non-reference type")
            PatRef(check_pat_(p1, t1), loc)
        | PatWhen(p1, e1, loc) =>
            val p1 = check_pat_(p1, t)
            val (etyp, eloc) = get_exp_ctx(e1)
            unify(etyp, TypBool, loc, "'when' clause should have boolean type")
            val e1 = check_exp(e1, r_env, sc)
            PatWhen(p1, e1, loc)
        }

    val pat_new = check_pat_(pat, typ)
    (pat_new, r_env, r_idset, *r_typ_vars)
}

fun check_cases(cases: (pat_t list, exp_t) list, inptyp: typ_t, outtyp: typ_t, env: env_t, sc: scope_t list, loc: loc_t) =
    [: for (plist, e) <- cases {
        val case_sc = new_block_scope() :: sc
        val fold (plist1, env1, capt1) = ([], env, make_empty_idset()) for p <- plist {
            val (p2, env2, capt2, _) = check_pat(p, inptyp, env1, capt1, make_empty_idset(), case_sc, false, false, false)
            (p2 :: plist1, env2, capt2)
        }
        val (plist1, env1, capt1) =
        if plist1.length() == 1 {
            (plist1, env1, capt1)
        } else {
            if capt1.empty() {}
            else {
                throw compile_err(loc, "captured variables may not be used in the case of multiple alternatives ('|' pattern)")
            }
            val temp_id = gen_temp_id("p")
            val temp_dv = defval_t {dv_name=temp_id, dv_typ=inptyp, dv_flags=default_tempval_flags(), dv_scope=sc, dv_loc=loc}
            set_id_entry(temp_id, IdDVal(temp_dv))
            val capt1 = capt1.add(temp_id)
            val env1 = add_id_to_env(temp_id, temp_id, env1)
            val temp_pat = PatIdent(temp_id, loc)
            val temp_id_exp = ExpIdent(temp_id, (inptyp, loc))
            val bool_ctx = (TypBool, loc)
            val when_cases =
            [: for p <- plist1 {
                match p {
                | PatAny(_) | PatIdent(_, _) =>
                    throw compile_err(loc, "in the case of multiple alternatives ('|' pattern) '_' or indent cannot be used")
                | _ => ([: p :], ExpLit(LitBool(true), bool_ctx))
                }
            } :]
            val when_cases = when_cases + [: ([: PatAny(loc) :], ExpLit(LitBool(false), bool_ctx)) :]
            val when_pat = PatWhen(temp_pat, ExpMatch(temp_id_exp, when_cases, bool_ctx), loc)
            ([: when_pat :], env1, capt1)
        }
        val (e1_typ, e1_loc) = get_exp_ctx(e)
        unify(e1_typ, outtyp, e1_loc, "the case expression type does not match the whole expression type (or the type of previous case(s))")
        (plist1.rev(), check_exp(e, env1, case_sc))
    } :]

fun check_mod(m: id_t) {
    val minfo = get_module(m)
    try {
        val modsc = ScModule(m) :: []
        val (seq, env) = check_eseq(minfo->dm_defs, make_empty_env(), modsc, false)
        minfo->dm_defs = seq
        minfo->dm_env = env
    } catch { | CompileError(_, _) as err => push_compile_err(err) | PropagateCompileError => {} }
}
