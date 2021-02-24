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

    that is, the chain of let-in's is replaced with a block (EBlock) of
    "let vi = ei"
    "let rec fj args_j = ej"
    definitions intermixed with other expressions.
    "let _ = ek in" is recognized and replaced with "ek"
*)
let transform_let code =
    let rec trlet_ e =
        match e with
        | ELit _ -> e
        | EIdent _ -> e
        | ETyped (e, t) -> ETyped((trlet_ e), t)
        | EUnary(uop, e1) -> EUnary(uop, (trlet_ e1))
        | EBinary(bop, e1, e2) -> EBinary(bop, (trlet_ e1), (trlet_ e2))
        | EIf(e1, e2, e3) -> EIf((trlet_ e1), (trlet_ e2), (trlet_ e3))
        | ELambda(pl, e) -> ELambda(pl, (trlet_ e))
        | ELambdaCases(cases) -> ELambdaCases(trlet_cases_ cases)
        | ECall(f, args) -> ECall((trlet_ f), (trlet_list_ args))
        | EMkTuple(args) -> EMkTuple(trlet_list_ args)
        | EMkList(args) -> EMkList(trlet_list_ args)
        | EMkVector(args) -> EMkVector(trlet_list_ args)
        | EMkRecord(relems) -> EMkRecord(trlet_relems_ relems)
        | EUpdateRecord(e, relems) -> EUpdateRecord((trlet_ e), (trlet_relems_ relems))
        | EMatch(e, cases) -> EMatch((trlet_ e), (trlet_cases_ cases))
        | ETry(e, cases) -> ETry((trlet_ e), (trlet_cases_ cases))
        | ERaise(e) -> ERaise(trlet_ e)
        | EWhile(e1, e2) -> EWhile((trlet_ e1), (trlet_ e2))
        | EFor(asc, n, e1, e2, e3) -> EFor(asc, n, (trlet_ e1), (trlet_ e2), (trlet_ e3))
        | EBlock(el) ->
            let new_el = List.fold_left (fun el e ->
                let e = trlet_ e in
                (* expand the nested blocks *)
                match e with
                | EBlock(el1) -> (List.rev el1) @ el
                | _ -> e :: el) [] el
                in
            (match new_el with
            | e :: [] -> e
            | _ -> EBlock(List.rev new_el))
        | ELet(p, e1, e2_opt) ->
            (*let rec let2str e =
                match e with
                | ELet(p, e1, e2_opt) -> sprintf "LET %s=...%s" (pat2str p)
                    (match e2_opt with
                    | Some((ELet _) as e2) -> " IN " ^ (let2str e2)
                    | Some _ -> " IN ..."
                    | _ -> "")
                | _ -> "..."
                in
            let _ = printf "\nTRANSFORM: %s\n" (let2str e) in*)
            let e1 = trlet_ e1 in
            let e1 = match p with
                | PAny | PLit(LUnit) -> e1
                | _ -> ELet(p, e1, None)
                in
            (match e2_opt with
            | Some(e2) ->
                let e2 = trlet_ e2 in
                (match e2 with
                | EBlock(el) -> EBlock(e1 :: el)
                | _ -> EBlock([e1; e2]))
            | _ -> e1)
        | ELetRec(fdefs, e2_opt) ->
            let fdefs = List.map (fun fdef ->
                let {ocf_name; ocf_args; ocf_body} = fdef in
                let body = trlet_ ocf_body in
                {ocf_name; ocf_args; ocf_body=body}) fdefs in
            let e1 = ELetRec(fdefs, None) in
            (match e2_opt with
            | Some(e2) ->
                let e2 = trlet_ e2 in
                (match e2 with
                | EBlock(el) -> EBlock(e1 :: el)
                | _ -> EBlock([e1; e2]))
            | _ -> e1)
        | EDefTyp _ -> e
        | EDefExn _ -> e
        | EOpen _ -> e
    and trlet_relems_ relems = List.map (fun (n, e) -> (n, trlet_ e)) relems
    and trlet_list_ el =
        List.map trlet_ el
    and trlet_cases_ cases =
        List.map (fun (pl, e) -> (pl, trlet_ e)) cases
    in
    let b = EBlock(code) in
    match (trlet_ b) with
    | EBlock(el) -> el
    | e -> e :: []
