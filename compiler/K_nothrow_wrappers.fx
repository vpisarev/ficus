/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    nothrow-functions, like all other, can be saved to variable. But to
    the same variable can be saved non-nothrow functions with same signature.
    Example:

    @pure @nothrow fun foo(i: int): int @ccode {...}
    fun bar(i: int): int {...}
    val predicate = if (whistle_of_crayfish_on_a_mount) {foo} else {bar}
    val denied_ips = [1, 2, 3, 4, 5, 6, 7, 8, ... ]
    denied_ips.filter(predicate)

    predicate fucntion must be called independently of containment.

    Usual functions in final C-code return exception information, when
    formal result are transmitting through "result" argument.
    nothrow-functions are called in different way - they transmit formal
    result through return and don't have "result" argument.

    There we creating general view wrapper functions around nothrow, which contains
    only call of wrapped function.

    Such wrapper save original function in kf_closure.kci_wrap_f field. That helps
    substitute wrapper with original in case of direct call(without any intermediate
    variables).
*/

from Ast import *
from K_form import *

fun make_wrappers_for_nothrow(kmods: kmodule_t list)
{
    var curr_m_idx = -1
    fun wrapf_atom(a: atom_t, loc: loc_t, callb: k_callb_t) =
        match a {
        | AtomId n =>
            match kinfo_(n, loc) {
            | KFun (ref {kf_closure={kci_wrap_f}}) =>
                if kci_wrap_f == noid { a } else { AtomId(kci_wrap_f) }
            | _ => a
            }
        | _ => a
        }
    fun wrapf_ktyp_(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun wrapf_kexp_(e: kexp_t, callb: k_callb_t) =
        match e {
        | KDefFun kf =>
            val {kf_name, kf_params, kf_rt, kf_flags, kf_body, kf_closure, kf_scope, kf_loc} = *kf
            val new_body = wrapf_kexp_(kf_body, callb)
            *kf = kf->{kf_body=new_body}
            if !kf_flags.fun_flag_nothrow || is_constructor(kf_flags) || kf_closure.kci_wrap_f != noid {
                e
            } else {
                val w_name = gen_idk(curr_m_idx, pp(kf_name) + "_w")
                *kf = kf->{kf_closure=kf_closure.{kci_wrap_f=w_name}}
                val w_flags = kf_flags.{fun_flag_nothrow=false}
                val w_params =
                [for a <- kf_params {
                    val w_a = dup_idk(curr_m_idx, a)
                    val {kv_typ=t} = get_kval(a, kf_loc)
                    val _ = create_kdefval(w_a, t, default_arg_flags(), None, [], kf_loc)
                    w_a
                }]
                val w_body = KExpCall(kf_name, [for i <- w_params {AtomId(i)} ], (kf_rt, kf_loc))
                val code = create_kdeffun(w_name, w_params, kf_rt, w_flags,
                                    Some(w_body), e :: [], kf_scope, kf_loc)
                rcode2kexp(code, kf_loc)
            }
        | KExpCall(f, args, (t, loc)) =>
            val args = [for a <- args { wrapf_atom(a, loc, callb) } ]
            KExpCall(f, args, (t, loc))
        | _ => walk_kexp(e, callb)
        }

    val callb = k_callb_t {
        kcb_ktyp=Some(wrapf_ktyp_),
        kcb_kexp=Some(wrapf_kexp_),
        kcb_atom=Some(wrapf_atom)
        }

    [for km <- kmods {
        val {km_top,km_idx} = km
        curr_m_idx = km_idx
        val top_kexp = code2kexp(km_top, noloc)
        // do 2 passes to cover both forward and backward references
        val top_kexp = wrapf_kexp_(top_kexp, callb)
        val top_kexp = wrapf_kexp_(top_kexp, callb)
        km.{km_top=kexp2code(top_kexp)}
    }]
}