/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    try to fuse sequential loops/comprehensions inside basic blocks.
    [TODO] handle nested-for array comprehensions.
       Currently only single for's are supported (even though they can be over multi-dimensional arrays)
*/

from Ast import *
from K_form import *
import K_remove_unused

import Map

type arr_info_t =
{
    arr_nused: int;
    arr_nused_for: int;
    arr_idl: (id_t, dom_t) list;
    arr_body: kexp_t;
    arr_map_flags: for_flags_t
}

type ainfo_map_t = (id_t, arr_info_t ref) Map.t
type arr_fuse_map_t = (id_t, id_t) Map.t

fun fuse_loops(code: kcode_t)
{
    val fold nmaps = 0, nfors = 0 for e <- code {
        | KDefVal(_, KExpMap _, _) => (nmaps + 1, nfors)
        | KExpMap _ => (nmaps + 1, nfors)
        | KExpFor _ => (nmaps, nfors + 1)
        | _ => (nmaps, nfors)
    }
    if nmaps >= 1 && nmaps + nfors >= 2 {
        fuse_loops_(code)
    } else {
        code
    }
}

fun fuse_loops_(code: kcode_t)
{
    var counters: ainfo_map_t = Map.empty(cmp_id)
    fun process_atom(a: atom_t, loc: loc_t, callb: k_fold_callb_t) =
        match a {
        | AtomId i =>
            match counters.find_opt(i) {
            | Some ainfo => ainfo->arr_nused += 1
            | _ => {}
            }
        | _ => {}
        }
    fun process_ktyp_(t: ktyp_t, loc: loc_t, callb: k_fold_callb_t) {}
    val process_callb = k_fold_callb_t {
        kcb_fold_atom=Some(process_atom),
        kcb_fold_ktyp=Some(process_ktyp_),
        kcb_fold_kexp=None
    }

    fun process_idl(inside_for: bool, idl: (id_t, dom_t) list) =
        for (_, dom) <- idl {
            match dom {
            | DomainElem(AtomId col) =>
                match counters.find_opt(col) {
                | Some ainfo =>
                    val is_parallel = ainfo->arr_map_flags.for_flag_parallel
                    if !(inside_for && is_parallel) {
                        ainfo->arr_nused_for += 1
                    }
                | _ => {}
                }
            | _ => {}
            }
        }

    for e <- code {
        fold_kexp(e, process_callb)
        match e {
        | KDefVal (i, KExpMap ((_, idl, []) :: [], body, flags, (KTypArray _, _)), loc) =>
            if K_remove_unused.pure_kexp(body) && !flags.for_flag_unzip {
                val ainfo = ref (arr_info_t {
                    arr_nused=0, arr_nused_for=0,
                    arr_idl=idl, arr_body=body, arr_map_flags=flags})
                counters = counters.add(i, ainfo)
                process_idl(false, idl)
            }
        | KExpFor (idl, [], _, _, _) => process_idl(true, idl)
        | KExpMap ((_, idl, []) :: [], _, _, _) => process_idl(false, idl)
        | _ => {}
        }
    }
    val arrs_to_fuse = counters.filter(
        fun (i: id_t, ainfo: arr_info_t ref) {
            match *ainfo {
            | {arr_nused=1, arr_nused_for=1} => true
            | _ => false
            }})

    fun fuse_for(idl: (id_t, dom_t) list, body: kexp_t, loc: loc_t)
    {
        val fold arr_fuse_map = (Map.empty(cmp_id): arr_fuse_map_t), a2f = [] for (i, dom) <- idl {
            match dom {
            | DomainElem(AtomId arr) =>
                val arr_fuse_map = arr_fuse_map.add(arr, i)
                match arrs_to_fuse.find_opt(arr) {
                | Some ainfo => (arr_fuse_map, (arr, ainfo) :: a2f)
                | _ => (arr_fuse_map, a2f)
                }
            | _ => (arr_fuse_map, a2f)
            }
        }

        /*
            The key idea of loop fusion is to:

            1. collect array comprehensions constructed
            in the middle of the code block, without any side effects in their bodies
            and used just once.

            (suppose that they look like
            val temp_arr = [| for i1 <- A1, in <- An { foo(i1, ..., in) } |])

            2. and then find this single for-loop or array comprehension where
            this comprehension is used, e.g.

            [| for ..., x <- temp_arr { bar(..., x, ...) } |]

            and do the following substitution

            [| for ..., i1 <- A1, ..., in <- An {
                val x = foo(i1, ..., in)
                bar(..., x, ...) } |]

            and then the preceding loop can be removed. Instead of 2 loops we have just one.

            This key idea looks quite straitforward, but there are some fine details,
            e.g. ij <- Aj might already occur in the second comprehension/loop,
            and then we try to avoid duplicates. Also, this second comprehension may
            also be the subject to fusion, and so we want to update information
            about this loop etc.
        */
        val (new_idl, pbody, _) =
        fold new_idl = [], pbody = [], arr_fuse_map = arr_fuse_map
        for (i, dom) <- idl {
            match dom {
            | DomainElem(AtomId arr) =>
                match find_opt(for (arr2, _) <- a2f {arr == arr2}) {
                | Some((_, ref {arr_idl, arr_body})) =>
                    val fold new_idl2 = new_idl, pbody2 = pbody, new_fuse_map = arr_fuse_map
                        for (nested_i, nested_dom) <- arr_idl {
                        match nested_dom {
                        | DomainElem(AtomId nested_arr) =>
                            match arr_fuse_map.find_opt(nested_arr) {
                            | Some outer_i =>
                                val t = get_idk_ktyp(outer_i, loc)
                                val pbody2 = create_kdefval(nested_i, t, default_tempval_flags(),
                                    Some(KExpAtom(AtomId(outer_i), (t, loc))), pbody2, loc)
                                (new_idl2, pbody2, new_fuse_map)
                            | _ =>
                                val new_fuse_map = new_fuse_map.add(nested_arr, nested_i)
                                ((nested_i, nested_dom) :: new_idl2, pbody2, new_fuse_map)
                            }
                        | _ => ((nested_i, nested_dom) :: new_idl2, pbody2, new_fuse_map)
                        }
                    }
                    val t = get_kexp_typ(arr_body)
                    val pbody2 = create_kdefval(i, t, default_tempval_flags(), Some(arr_body), pbody2, loc)
                    (new_idl2, pbody2, new_fuse_map)
                | _ =>
                    val arr_fuse_map = arr_fuse_map.add(arr, i)
                    ((i, dom) :: new_idl, pbody, arr_fuse_map)
                }
            | _ => ((i, dom) :: new_idl, pbody, arr_fuse_map)
            }
        }
        val new_body = rcode2kexp(body :: pbody, loc)
        (new_idl, new_body)
    }

    fun fuse_ktyp_(t: ktyp_t, loc: loc_t, callb: k_callb_t) = t
    fun fuse_kexp_(e: kexp_t, callb: k_callb_t)
    {
        val e = walk_kexp(e, callb)
        match e {
        | KExpSeq(elist, (t, loc)) => code2kexp(fuse_loops(elist), loc)
        | KDefVal(i, KExpMap ((e0, idl, idxs) :: [], body, _, _), loc) =>
            match arrs_to_fuse.find_opt(i) {
            | Some ainfo =>
                ainfo->arr_idl = idl
                ainfo->arr_body = body
                KExpNop(loc)
            | _ => e
            }
        | KExpFor (idl, [], body, flags, loc) =>
            val (new_idl, new_body) = fuse_for(idl, body, loc)
            KExpFor(new_idl.rev(), [], new_body, flags, loc)
        | KExpMap ((e0, idl, []) :: [], body, flags, (map_result_type, loc)) =>
            val (new_idl, new_body) = fuse_for(idl, body, loc)
            KExpMap((e0, new_idl.rev(), []) :: [], new_body, flags, (map_result_type, loc))
        | _ => e
        }
    }

    val fuse_callb = k_callb_t {
        kcb_ktyp=Some(fuse_ktyp_),
        kcb_kexp=Some(fuse_kexp_),
        kcb_atom=None
    }
    [for e <- code { fuse_kexp_(e, fuse_callb) } ]
}

fun fuse_loops_all(kmods: kmodule_t list) =
    [for km <- kmods {
        val {km_top} = km
        val new_top = fuse_loops(km_top)
        km.{km_top=new_top}
    }]
