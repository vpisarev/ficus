/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    try to move as much code as possible into the upper-level
    expression sequence without changing the semantics and
    without affecting possible side effects of the code. That is:
    * `val a = {b; c; d}` is replaced with `b; c; val a = d`
    * `if({a; b; c}) e1 else e2` is replaced with `a; b; if(c) e1 else e2`.
    etc.
    part of those transformations are done at the K-normalization step,
    but this is a dedicated step that can be useful together with other
    transformations to keep the code structure as 'flat' as possible
    (hence the name `flatten`).
*/
from Ast import *
from K_form import *

@private fun flatten_ktyp_(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
@private fun flatten_kexp_(e: kexp_t, callb: k_callb_t) =
    match e {
    | KExpSeq (elist, (_, loc)) =>
        val new_rcode = flatten(elist, callb)
        rcode2kexp(new_rcode, loc)
    | KExpIf (c, then_e, else_e, (_, loc) as kctx) =>
        val (c, code) = try_flatten(c, [], callb)
        val then_e = flatten_kexp_(then_e, callb)
        val else_e = flatten_kexp_(else_e, callb)
        val new_if = KExpIf(c, then_e, else_e, kctx)
        rcode2kexp(new_if :: code, loc)
    | KExpMatch _ =>
        /* just call walk_kexp to flatten all
            the nested expressions with 1 line of code */
        val e = walk_kexp(e, callb)
        /* now let's move up all but the last one
           expressions in the very first check of the
           very first match case.
           That's all we can do here */
        match e {
        | KExpMatch ((c0 :: crest0, e0) :: other_cases, (_, loc) as kctx) =>
            val (c0, code) = try_flatten(c0, [], callb)
            val new_match = KExpMatch((c0 :: crest0, e0) :: other_cases, kctx)
            rcode2kexp(new_match :: code, loc)
        | _ => e
        }
    | KExpMap _ =>
        val e = walk_kexp(e, callb)
        // do the same thing as with KExpMatch
        match e {
        | KExpMap ((e0, for_iters0, at_ids0) :: other_map_clauses, body, flags, (_, loc) as kctx) =>
            val code = kexp2code(e0).rev()
            val new_map = KExpMap((KExpNop(get_kexp_loc(e0)), for_iters0, at_ids0) :: other_map_clauses, body, flags, kctx)
            rcode2kexp(new_map :: code, loc)
        | _ => e
        }
    | KExpDoWhile (body, c, loc) =>
        val body = flatten_kexp_(body, callb)
        val body_code = kexp2code(body).rev()
        val (c, body_code) = try_flatten(c, body_code, callb)
        val body = rcode2kexp(body_code, loc)
        KExpDoWhile(body, c, loc)
    | KDefVal (n, e, loc) =>
        val (e, code) = try_flatten(e, [], callb)
        val new_defval = KDefVal(n, e, loc)
        rcode2kexp(new_defval :: code, loc)
    | _ => walk_kexp(e, callb)
    }

@private fun try_flatten(e: kexp_t, code: kcode_t, callb: k_callb_t)
{
    val new_e = flatten_kexp_(e, callb)
    match new_e {
    | KExpSeq ([], (_, loc)) => (KExpNop(loc), code)
    | KExpSeq ([:: e], _) => (e, code)
    | KExpSeq ((e :: rest) as nested_list, _) =>
        val rnested = nested_list.rev()
        (rnested.hd(), rnested.tl() + code)
    | _ => (new_e, code)
    }
}

@private fun flatten(code: kcode_t, callb: k_callb_t) =
    fold code = [] for e <- code {
        val new_e = flatten_kexp_(e, callb)
        match new_e {
        | KExpSeq (nested_elist, _) => nested_elist.rev() + code
        | _ => new_e :: code
        }
    }

fun flatten_all(kmods: kmodule_t list)
{
    val callb = k_callb_t
    {
        kcb_ktyp=Some(flatten_ktyp_),
        kcb_kexp=Some(flatten_kexp_),
        kcb_atom=None
    }
    [:: for km <- kmods {
        val {km_top} = km
        val new_top = flatten(km_top, callb).rev()
        km.{km_top=new_top}
    }]
}
