/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    very simple variant of 'lambda-lifting' optimization step,
    which is performed in the very beginning after the
    initial dead code elimination step.

    It does the following:
    * moves all the type definitions to the top level (i.e. global/module level).
    * moves all the exception declarations to the top level.
    * moves all the nested functions that do not access local variables/parameters
      of the outer functions to the top level. That is, those are functions that
      certainly do not need closures. If a function calls functions or accesses values from
      the top/module level, it's not a problem, since it does not require a closure.
      In particular, this step moves "inline C" functions to the top level.

    Why is this step needed? This step is needed to simplify the
    inline function expansion step, i.e. it increases the number of functions that
    can potentially be inlined. It also reduces the amount of work needed to
    be done by the full-scale lambda lifting step that is perfomed before
    translation to C/machine code.
*/

from Ast import *
from K_form import *
import Hashset

fun update_globals(top_code: kcode_t, globals: id_hashset_t) =
    for e <- top_code {
        val n_list =
        match e {
        | KDefVal (n, e, _) => n :: []
        | KDefFun (ref {kf_name}) => kf_name :: []
        | KDefExn (ref {ke_name, ke_tag}) => ke_name :: ke_tag :: []
        | KDefVariant (ref {kvar_name, kvar_cases}) =>
            kvar_name :: [: for (n, _) <- kvar_cases {n} :]
        | KDefTyp (ref {kt_name}) => kt_name :: []
        | KDefInterface (ref {ki_name, ki_all_methods}) =>
            ki_name :: [: for (f, _) <- ki_all_methods {f} :]
        | _ => []
        }
        globals.add_list(n_list)
    }

fun lift(kmods: kmodule_t list) {
    // first, let's see which definitions are already at the top level
    var new_top_code: kcode_t = []
    val globals = empty_id_hashset(256)
    for {km_top} <- kmods {
        update_globals(km_top, globals)
    }

    fun add_to_globals_and_lift(i: id_t, e: kexp_t, loc: loc_t)
    {
        globals.add(i)
        new_top_code = e :: new_top_code
        KExpNop(loc)
    }

    /* a function can be lifted to the top level (i.e. become global) if all its
       "free variables" are either global (or have been just promoted there) or
       type names, constructor names or C functions, i.e. they will
       definitely can be promoted to the top level. */
    fun can_lift_fun(kf: kdeffun_t ref): bool
    {
        val {kf_name, kf_loc} = *kf
        val code = KDefFun(kf) :: []
        val uv = used_by(code, 256)
        val dv = declared(code, 256)
        uv.all(fun (n: id_t) {
            dv.mem(n) || globals.mem(n) ||
            (match kinfo_(n, kf_loc) {
            | KExn _ => true
            | KVariant _ => true
            | KInterface _ => true
            | KTyp _ => true
            | KVal _ => false
            | KFun (ref {kf_flags}) => kf_flags.fun_flag_ccode || is_constructor(kf_flags)
            | KClosureVars (ref {kcv_loc}) =>
                throw compile_err( kf_loc,
                    f"simple LL: KClosureVars '{idk2str(n, kcv_loc)}' is not expected at this step, it should appear in the end after full-scale lambda lifting")
            | KNone  =>
                throw compile_err(kf_loc,
                    f"simple LL: attempt to request type of non-existing symbol '{n}' when checking free variables of function '{idk2str(kf_name, kf_loc)}'")
            })
        })
    }

    fun walk_ktyp_n_lift(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun walk_kexp_n_lift(e: kexp_t, callb: k_callb_t): kexp_t =
        match e {
        | KDefVariant (ref {kvar_name, kvar_loc}) =>
            if globals.mem(kvar_name) { e }
            else { add_to_globals_and_lift(kvar_name, e, kvar_loc) }
        | KDefTyp (ref {kt_name, kt_loc}) =>
            if globals.mem(kt_name) { e }
            else { add_to_globals_and_lift(kt_name, e, kt_loc) }
        | KDefExn (ref {ke_name, ke_loc}) =>
            if globals.mem(ke_name) { e }
            else { add_to_globals_and_lift(ke_name, e, ke_loc) }
        | KDefVal (i, rhs, loc) =>
            val {kv_flags} = get_kval(i, loc)
            if globals.mem(i) {
                e
            } else if kv_flags.val_flag_ctor != noid ||
                      (match rhs { | KExpData _ => true | _ => false }) {
                add_to_globals_and_lift(i, e, loc)
            } else {
                e
            }
        | KDefFun kf =>
            val {kf_name, kf_body, kf_flags, kf_loc} = *kf
            val new_body = walk_kexp_n_lift(kf_body, callb)
            *kf = kf->{kf_body=new_body}
            if globals.mem(kf_name) { e }
            else if !can_lift_fun(kf) { e }
            else {
                // @private flag is important at type/semantical check stage,
                // where we need to control the access.
                // Here, at K-form, it's not that important anymore.
                //
                // [TODO] Still, when we produce shared libraries out
                // of generated .c files, we may hide private symbols,
                // so we may want to keep the original private flag value
                *kf = kf->{kf_flags=kf_flags.{fun_flag_private=false}}
                add_to_globals_and_lift(kf_name, e, kf_loc)
            }
        | _ => walk_kexp(e, callb)
        }

    val walk_n_lift_callb = k_callb_t
    {
        kcb_atom=None,
        kcb_ktyp=Some(walk_ktyp_n_lift),
        kcb_kexp=Some(walk_kexp_n_lift)
    }

    fun process(top_code: kcode_t) {
        new_top_code = []
        for e <- top_code {
            val new_e = walk_kexp_n_lift(e, walk_n_lift_callb)
            match new_e {
            | KExpNop _ => {}
            | _ => new_top_code = new_e :: new_top_code
            }
        }
        new_top_code.rev()
    }

    [: for km <- kmods {
        val {km_top=top_code} = km
        /* process each module twice since in each module there can be cyclic dependencies
           between functions (but not inter-module cyclic dependencies,
           so we process each module separately) */
        val top_code = process(top_code)
        val top_code = process(top_code)
        km.{km_top=top_code}
    } :]
}
