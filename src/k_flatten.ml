(*
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
*)
open Ast
open K_form

let rec flatten_ktyp_ t callb = t
and flatten_kexp_ e callb =
    match e with
    | KExpSeq(elist, (_, loc)) ->
        let new_rcode = flatten_ elist callb in
        rcode2kexp new_rcode loc
    | KExpIf(c, then_e, else_e, ((_, loc) as kctx)) ->
        let (c, code) = try_flatten c [] callb in
        let then_e = flatten_kexp_ then_e callb in
        let else_e = flatten_kexp_ else_e callb in
        let new_if = KExpIf(c, then_e, else_e, kctx) in
        rcode2kexp (new_if :: code) loc
    | KExpAssign(n, e, loc) ->
        let (e, code) = try_flatten e [] callb in
        let new_assign = KExpAssign(n, e, loc) in
        rcode2kexp (new_assign :: code) loc
    | KExpMatch _ ->
        (* just call walk_kexp to flatten all
            the nested expressions with 1 line of code *)
        let e = walk_kexp e callb in
        (* now let's move up all but the last one
           expressions in the very first check of the
           very first match case.
           That's all we can do here *)
        (match e with
        | KExpMatch(((c0 :: crest0), e0) :: other_cases, ((_, loc) as kctx)) ->
            let (c0, code) = try_flatten c0 [] callb in
            let new_match = KExpMatch(((c0 :: crest0), e0) :: other_cases, kctx) in
            rcode2kexp (new_match :: code) loc
        | _ -> e)
    | KExpMap _ ->
        (* just call walk_kexp to flatten all
        the nested expressions with 1 line of code *)
        let e = walk_kexp e callb in
        (* now let's move up all but the last one
           expressions in the very first check of the
           very first match case.
           That's all we can do here *)
        (match e with
        | KExpMap((e0, for_iters0) :: other_map_clauses, body, flags, ((_, loc) as kctx)) ->
            let code = List.rev (kexp2code e0) in
            let new_map = KExpMap(((KExpNop (get_kexp_loc e0)), for_iters0) :: other_map_clauses, body, flags, kctx) in
            rcode2kexp (new_map :: code) loc
        | _ -> e)
    | KExpDoWhile(body, c, loc) ->
        let body = flatten_kexp_ body callb in
        let body_code = List.rev (kexp2code body) in
        let (c, body_code) = try_flatten c body_code callb in
        let body = rcode2kexp body_code loc in
        KExpDoWhile(body, c, loc)
    | KDefVal(n, e, loc) ->
        let (e, code) = try_flatten e [] callb in
        let new_defval = KDefVal(n, e, loc) in
        rcode2kexp (new_defval :: code) loc
    | _ -> walk_kexp e callb
and try_flatten e code callb =
    let new_e = flatten_kexp_ e callb in
    match new_e with
    | KExpSeq([], (_, loc)) -> ((KExpNop loc), code)
    | KExpSeq((e :: []), _) -> (e, code)
    | KExpSeq(((e :: rest) as nested_list), _) ->
        let rnested = List.rev nested_list in
        ((List.hd rnested), ((List.tl rnested) @ code))
    | _ -> (new_e, code)
and flatten_ code callb =
    List.fold_left (fun code e ->
        let new_e = flatten_kexp_ e callb in
        match new_e with
        | KExpSeq(nested_elist, _) ->
            (List.rev nested_elist) @ code
        | _ -> new_e :: code) [] code

let flatten top_code =
    let callb =
    {
        kcb_typ=Some(flatten_ktyp_);
        kcb_exp=Some(flatten_kexp_);
        kcb_atom=None
    }
    in List.rev (flatten_ top_code callb)
