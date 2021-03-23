/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Transforms tail-recursive functions into loops.

    Note that some functions may call itself several times,
    where some of the calls may be tail calls whereas the others may be
    non-tail calls. The classical example is qsort. In those cases
    the algorithm retains non-tail calls as recursive calls and
    transforms the function body in such a way that all tail calls
    update the parameters and 'jump' into the beginning of the function.
*/

from Ast import *
from K_form import *

fun tailrec2loop(kf: kdeffun_t ref): void
{
    val {kf_name, kf_args, kf_rt=rt, kf_body, kf_loc} = *kf
    fun have_tailrec_calls_(e: kexp_t): bool =
        match e {
        | KExpSeq (elist, _) =>
            !elist.empty() && have_tailrec_calls_(elist.last())
        | KExpIf (_, then_e, else_e, _) =>
            have_tailrec_calls_(then_e) || have_tailrec_calls_(else_e)
        | KExpCall (f, _, _) =>
            f == kf_name
        | KExpMatch (cases, _) =>
            cases.exists(fun ((_, e)) { have_tailrec_calls_(e) })
        | _ => false
        }

    if have_tailrec_calls_(kf_body) {
        /* the function has some tail recursive calls;
           let's transform the whole loop body into a loop
           where the tail calls update the parameter values and
           jump to the beginning of the loop */
        val (res_n, f_init_code) =
        match rt {
        | KTypVoid  =>
            (noid, [])
        | _ =>
            val res_n = gen_temp_idk("res")
            val a0 = match is_ktyp_scalar(rt) {
                     | true => AtomLit(KLitInt(0L))
                     | _ => AtomLit(KLitNil(rt))
                     }
            val res_val0 = KExpAtom(a0, (rt, kf_loc))
            val f_init_code = create_kdefval(res_n, rt, default_tempvar_flags(),
                                             Some(res_val0), [], kf_loc)
            (res_n, f_init_code)
        }

        /* For each function argument we create 2 aliases.
            The first will become the new function formal argument.
            The second will become a variable to which we assign a new value in the case of 'tail call'.

            before:
            fun foo(a: at, b: bt, c: ct) {
                if (a < 0) {
                    ...
                    foo(a+1, b-1, c*2) // tail-recursive call
                }
                else {
                    ...
                    bar(b+c) // return the value
                }
            }

            after:
            // rename a->a', b->b', c->c'; a', b' and 'c are still immutable
            fun foo(a': at, b': bt, c': ct) {
                var res = nil // initialize the function result (non-void function case)
                // and these are mutable copies of the arguments
                var a'' = a', b'' = b', c'' = c'
                while(true) {
                    // before processing save them as immutable values,
                    // so that the body semantics is preserved
                    val a=a'', b=b'', c=c''
                    // the body is mostly retained as-is
                    if (a < 0) {
                        ...
                        // replace tail-recursive calls
                        a''=a+1; b''=b-1; c''=c*2;
                        // continue; // continue is not needed here,
                                     // because it's the tail call
                    }
                    else {
                        ...
                        res = bar(b+c); break
                        // or 'bar(b+c); break' in the case of void function
                    }
                }
                res // return the final result
            }
            Why have two aliases and not just turn a, b, c into variables?
            This is because we want to keep the original semantics of the body code
            (where each function argument is an immutable value) and also want to
            enable all the optimizations that compiler can do with immutable values.
        */
        val fold new_kf_args = [], trec_args = [], f_init_code = f_init_code,
            loop_init_code = [] for (ai, ti) <- kf_args {
            val dv0 = get_kval(ai, kf_loc)
            val a1i = dup_idk(ai)
            val a2i = dup_idk(ai)
            val dv1 = dv0.{kv_name=a1i}
            set_idk_entry(a1i, KVal(dv1))
            val a1i_as_exp = KExpAtom(AtomId(a1i), (ti, kf_loc))
            val a2i_as_exp = KExpAtom(AtomId(a2i), (ti, kf_loc))
            val f_init_code = create_kdefval(a2i, ti, default_tempvar_flags(),
                                             Some(a1i_as_exp), f_init_code, kf_loc)
            val loop_init_code = create_kdefval(ai, ti, default_tempval_flags(),
                                                Some(a2i_as_exp), loop_init_code, kf_loc)
            ((a1i, ti) :: new_kf_args, (a2i, ti) :: trec_args, f_init_code, loop_init_code)
        }
        val new_kf_args = new_kf_args.rev()
        val trec_args = trec_args.rev()

        /* the function prologue is ready; the loop prologue is ready;
           now let's transform the function body */
        fun process_func_ending(final_e: kexp_t)
        {
            | KExpThrow _ => final_e
            | _ =>
                val (ktyp, kloc) = get_kexp_ctx(final_e)
                val (final_e, code) =
                if res_n == noid {
                    (final_e, [])
                } else {
                    val (final_atom, code) = kexp2atom("result", final_e, !is_ktyp_scalar(ktyp), [])
                    (KExpAssign(res_n, final_atom, kloc), code)
                }
                val code = KExpBreak(kloc) :: final_e :: code
                rcode2kexp(code, kloc)
        }

        fun transform_tcalls(e: kexp_t)
        {
            val eloc = get_kexp_loc(e)
            val new_ctx = (KTypVoid, eloc)
            match e {
            | KExpSeq (elist, (_, eloc)) =>
                val rcode = match elist.rev() {
                            | [] => KExpBreak(eloc) :: []
                            | final_e :: rest => transform_tcalls(final_e) :: rest
                            }
                rcode2kexp(rcode, eloc)
            | KExpIf (c, then_e, else_e, _) =>
                val then_e = transform_tcalls(then_e)
                val else_e = transform_tcalls(else_e)
                KExpIf(c, then_e, else_e, new_ctx)
            | KExpCall (f, real_args, (_, eloc)) =>
                if f == kf_name {
                    val fold tcall_rcode = []
                        for (trec_ai, ti) <- trec_args, real_ai <- real_args {
                            val set_new = KExpAssign(trec_ai, real_ai, eloc)
                            set_new :: tcall_rcode
                        }
                    rcode2kexp(tcall_rcode, eloc)
                } else {
                    process_func_ending(e)
                }
            | KExpMatch (cases, _) =>
                val cases = [: for (checks_i, e_i) <- cases {
                                val e_i = transform_tcalls(e_i)
                                (checks_i, e_i)
                            } :]
                KExpMatch(cases, new_ctx)
            | _ => process_func_ending(e)
            }
        }

        val body_ = transform_tcalls(kf_body)
        val loop_body = rcode2kexp(body_ :: loop_init_code, kf_loc)
        val lloc = get_kexp_loc(loop_body)
        val loop_exp = KExpWhile(KExpAtom(AtomLit(KLitBool(true)), (KTypBool, lloc)), loop_body, lloc)
        val f_code =
            if res_n == noid { [] }
            else { KExpAtom(AtomId(res_n), (rt, kf_loc)) :: [] }
        val f_code = f_code + (loop_exp :: f_init_code)
        val new_kf_body = rcode2kexp(f_code, kf_loc)
        *kf = kf->{kf_args=new_kf_args, kf_body=new_kf_body}
    }
}

fun tailrec2loops_all(kmods: kmodule_t list): kmodule_t list
{
    fun tailrec2loop_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    fun tailrec2loop_kexp_(e: kexp_t, callb: k_fold_callb_t)
    {
        fold_kexp(e, callb)
        match e {
        | KDefFun kf => tailrec2loop(kf)
        | _ => {}
        }
    }

    val trec2loop_callb = k_fold_callb_t
    {
        kcb_fold_atom=None,
        kcb_fold_ktyp=Some(tailrec2loop_ktyp_),
        kcb_fold_kexp=Some(tailrec2loop_kexp_)
    }

    for {km_top} <- kmods {
        for e <- km_top {
            tailrec2loop_kexp_(e, trec2loop_callb)
        }
    }
    kmods
}
