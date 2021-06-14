/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    try to move loop invariants outside of the loops.
*/

from Ast import *
from K_form import *
import K_remove_unused

import Hashset

fun move_loop_invs(code: kcode_t)
{
    var curr_inloop = empty_id_hashset(256)
    var curr_moved: kcode_t = []

    fun is_loop_invariant(e: kexp_t)
    {
        var isinv = true
        fun isinv_atom_(a: atom_t, loc: loc_t, callb: k_fold_callb_t) =
            match a {
            | AtomId ({m=0}) => {}
            | AtomId i =>
                if  is_mutable(i, loc) ||
                    !is_ktyp_scalar(get_idk_ktyp(i, loc)) ||
                    curr_inloop.mem(i) {
                    isinv = false
                }
            | _ => {}
            }

        fun isinv_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
        fun isinv_kexp_(e: kexp_t, callb: k_fold_callb_t) =
            if isinv {
                fold_kexp(e, callb)
            }
        val isinv_callb = k_fold_callb_t {
            kcb_fold_atom=Some(isinv_atom_),
            kcb_fold_ktyp=Some(isinv_ktyp_),
            kcb_fold_kexp=Some(isinv_kexp_)
        }

        is_ktyp_scalar(get_kexp_typ(e)) &&
        K_remove_unused.pure_kexp(e) &&
        (match e { | KExpAt _ => false | _ => true }) &&
        ({
            isinv_kexp_(e, isinv_callb)
            isinv
        })
    }

    fun mli_ktyp(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun mli_process_loop(e_idl_l: (kexp_t, (id_t, dom_t) list, id_t list) list,
                         body: kexp_t, loc: loc_t, callb: k_callb_t)
    {
        val saved_inloop = curr_inloop.copy()
        val saved_moved = curr_moved
        curr_inloop = declared(body::[], 256)
        val (outer_moved, new_e_idl_l, new_body) =
            fold nested_elist = kexp2code(body), e_idl_l = [], body = KExpNop(loc)
            for (pre_e, idl, idxl) <- e_idl_l.rev() {
                val in_pre = declared(pre_e :: [], 256)
                for (i, _) <- idl { curr_inloop.add(i) }
                for i <- idxl { curr_inloop.add(i) }
                curr_moved = []
                val nested_e = mli_kexp(code2kexp(nested_elist, loc), callb)
                val new_elist = kexp2code(pre_e) + curr_moved.rev()
                curr_inloop.union(in_pre)
                val (e_idl_l, new_body) =
                    match e_idl_l {
                    | (_, prev_idl, prev_idxl) :: rest => ((nested_e, prev_idl, prev_idxl) :: rest, body)
                    | _ => ([], nested_e)
                    }
                val new_e_idl_l = (KExpNop(loc), idl, idxl) :: e_idl_l
                (new_elist, new_e_idl_l, new_body)
        }
        curr_inloop = saved_inloop
        curr_moved = saved_moved
        (outer_moved, new_e_idl_l, new_body)
    }
    fun mli_kexp(e: kexp_t, callb: k_callb_t)
    {
        val e = walk_kexp(e, callb)
        match e {
        | KExpFor (idl, idxl, body, flags, loc) =>
            val (outer_moved, _, body) = mli_process_loop([(KExpNop(loc), idl, idxl) ], body, loc, callb)
            code2kexp(outer_moved + [KExpFor(idl, idxl, body, flags, loc) ], loc)
        | KExpMap (e_idl_l, body, flags, (t, loc)) =>
            val (outer_moved, e_idl_l, body) = mli_process_loop(e_idl_l, body, loc, callb)
            code2kexp(outer_moved + [KExpMap(e_idl_l, body, flags, (t, loc)) ], loc)
        | KExpWhile (c, body, loc) =>
            val (outer_moved_c, _, c) = mli_process_loop([(KExpNop(loc), [], []) ], c, loc, callb)
            val (outer_moved, _, body) = mli_process_loop([(KExpNop(loc), [], []) ], body, loc, callb)
            code2kexp(outer_moved_c + (outer_moved + [KExpWhile(c, body, loc) ]), loc)
        | KExpDoWhile (body, c, loc) =>
            val (outer_moved, _, body) = mli_process_loop([(KExpNop(loc), [], []) ], body, loc, callb)
            val (outer_moved_c, _, c) = mli_process_loop([(KExpNop(loc), [], []) ], c, loc, callb)
            code2kexp(outer_moved + (outer_moved_c + [KExpDoWhile(body, c, loc) ]), loc)
        | KDefVal (n, rhs, loc) =>
            if !curr_inloop.mem(n) || is_mutable(n, loc) || !is_loop_invariant(rhs) {
                e
            } else {
                curr_moved = e :: curr_moved
                KExpNop(loc)
            }
        | _ => e
        }
    }

    val mli_callb = k_callb_t {
        kcb_ktyp=Some(mli_ktyp),
        kcb_kexp=Some(mli_kexp),
        kcb_atom=None
    }

    [for e <- code { mli_kexp(e, mli_callb) } ]
}

fun move_loop_invs_all(kmods: kmodule_t list) =
    [for km <- kmods {
        val {km_top} = km
        val new_top = move_loop_invs(km_top)
        km.{km_top=new_top}
    }]
