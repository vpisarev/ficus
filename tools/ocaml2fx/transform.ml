(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* does various transformations of Ocaml tree to make it closer to Ficus *)

open Syntax

(*
    Replaces

    let v1 = e1 in
    let rec f2 a21 a22 ... = e2 in
    ...
    let _ = ek in
    ...
    let vn = en in
    final_e

    with pseudo-Ocaml syntax

    begin
        let v1 = e1;
        let rec f2 a21 a22 ... = e2;
        ...
        ek;
        ...
        let vn = en;
        final_e
    end

    which is further printed as the following Ficus code

    {
        val v1 = e1
        fun f2(a21, a22, ...) = e2
        ...
        ek
        val vn = en
        final_e
    }

    that is, the chain of let-in's is replaced with a block (CeBlock) of
    "let vi = ei"
    "let rec fj args_j = ej"
    definitions intermixed with other expressions.
    "let _ = ek in" is recognized and replaced with "ek"
*)
let transform_let code =
    let rec trlet_ e =
        match e with
        | CeLit _ -> e
        | CeIdent _ -> e
        | CeTyped (e, t) -> CeTyped((trlet_ e), t)
        | CeUnary(uop, e1) -> CeUnary(uop, (trlet_ e1))
        | CeBinary(bop, e1, e2) -> CeBinary(bop, (trlet_ e1), (trlet_ e2))
        | CeIf(e1, e2, e3) -> CeIf((trlet_ e1), (trlet_ e2), (trlet_ e3))
        | CeLambda(pl, e) -> CeLambda(pl, (trlet_ e))
        | CeLambdaCases(cases) -> CeLambdaCases(trlet_cases_ cases)
        | CeCall(f, args) -> CeCall((trlet_ f), (trlet_list_ args))
        | CeMkTuple(args) -> CeMkTuple(trlet_list_ args)
        | CeMkList(args) -> CeMkList(trlet_list_ args)
        | CeMkVector(args) -> CeMkVector(trlet_list_ args)
        | CeMkRecord(relems) -> CeMkRecord(trlet_relems_ relems)
        | CeUpdateRecord(e, relems) -> CeUpdateRecord((trlet_ e), (trlet_relems_ relems))
        | CeMatch(e, cases) -> CeMatch((trlet_ e), (trlet_cases_ cases))
        | CeTry(e, cases) -> CeTry((trlet_ e), (trlet_cases_ cases))
        | CeRaise(e) -> CeRaise(trlet_ e)
        | CeWhile(e1, e2) -> CeWhile((trlet_ e1), (trlet_ e2))
        | CeFor(asc, n, e1, e2, e3) -> CeFor(asc, n, (trlet_ e1), (trlet_ e2), (trlet_ e3))
        | CeBlock(el) ->
            let new_el = List.fold_left (fun el e ->
                let e = trlet_ e in
                (* expand the nested blocks *)
                match e with
                | CeBlock(el1) -> (List.rev el1) @ el
                | _ -> e :: el) [] el
                in
            (match new_el with
            | e :: [] -> e
            | _ -> CeBlock(List.rev new_el))
        | CeLet(p, e1, e2_opt) ->
            let e1 = match p with
                | CpAny | CpLit(ClUnit) -> e1
                | _ -> CeLet(p, e1, None)
                in
            (match e2_opt with
            | Some(e2) ->
                let e2 = trlet_ e2 in
                (match e2 with
                | CeBlock(el) -> CeBlock(e1 :: el)
                | _ -> CeBlock([e1; e2]))
            | _ -> e1)
        | CeLetRec(fdefs, e2_opt) ->
            let fdefs = List.map (fun fdef ->
                let {ocf_name; ocf_args; ocf_body} = fdef in
                let body = trlet_ ocf_body in
                {ocf_name; ocf_args; ocf_body=body}) fdefs in
            let e1 = CeLetRec(fdefs, None) in
            (match e2_opt with
            | Some(e2) ->
                let e2 = trlet_ e2 in
                (match e2 with
                | CeBlock(el) -> CeBlock(e1 :: el)
                | _ -> CeBlock([e1; e2]))
            | _ -> e1)
        | CeDefTyp _ -> e
        | CeDefExn _ -> e
        | CeOpen _ -> e
    and trlet_relems_ relems = List.map (fun (n, e) -> (n, trlet_ e)) relems
    and trlet_list_ el =
        List.map trlet_ el
    and trlet_cases_ cases =
        List.map (fun (pl, e) -> (pl, trlet_ e)) cases
    in
    let b = CeBlock(code) in
    match (trlet_ b) with
    | CeBlock(el) -> el
    | e -> e :: []
