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
let transform_let_comp code =
    let rec trexp_ e =
        match e with
        | ELit _ -> e
        | EIdent _ -> e
        | ETyped (e, t) -> ETyped((trexp_ e), t)
        | EUnary(uop, e1) -> EUnary(uop, (trexp_ e1))
        | EBinary(OpMem, EUnary(OpDeref, e1), e2) -> EBinary(OpArrow, e1, e2)
        | EBinary(bop, e1, e2) -> EBinary(bop, (trexp_ e1), (trexp_ e2))
        | EIf(e1, e2, e3) -> EIf((trexp_ e1), (trexp_ e2), (trexp_ e3))
        | ELambda(pl, e) -> ELambda(pl, (trexp_ e))
        | ELambdaCases(cases) -> ELambdaCases(trexp_cases_ cases)
        | ECall(f, args) ->
            let f = trexp_ f in
            let args = trexp_list_ args in
            (match (f, args) with
            | (EIdent("List.map"), ELambda(p :: [], body) :: lst :: []) ->
                EMap(p, lst, body)
            | (EIdent("List.map"), f :: lst :: []) ->
                ECall(EBinary(OpMem, lst, EIdent("map")), f :: [])
            | (EIdent("List.iter"), ELambda(p :: [], body) :: lst :: []) ->
                EForEach(p, PAny, lst, body)
            | (EIdent("List.iter"), f :: lst :: []) ->
                ECall(EBinary(OpMem, lst, EIdent("app")), f :: [])
            | (EIdent("List.iteri"), ELambda(i :: p :: [], body) :: lst :: []) ->
                EForEach(p, i, lst, body)
            | (EIdent("List.fold_left"), ELambda(acc :: p :: [], body) :: acc0 :: lst :: []) ->
                EFold(false, (acc, acc0), (p, lst), body)
            | (EIdent("List.rev"), lst :: []) ->
                ECall(EBinary(OpMem, lst, EIdent("rev")), [])
            | (EIdent("List.length"), lst :: []) ->
                ECall(EBinary(OpMem, lst, EIdent("length")), [])
            | (EIdent("List.nth"), lst :: idx :: []) ->
                ECall(EBinary(OpMem, lst, EIdent("nth")), [idx])
            | (EIdent("String.concat"), s :: lst :: []) ->
                ECall(EBinary(OpMem, s, EIdent("join")), lst :: [])
            | (f, EMkTuple(args) :: []) -> ECall(f, args)
            | _ -> ECall(f, args))
        | EMkTuple(args) -> EMkTuple(trexp_list_ args)
        | EMkList(args) -> EMkList(trexp_list_ args)
        | EMkVector(args) -> EMkVector(trexp_list_ args)
        | EMkRecord(relems) -> EMkRecord(trexp_relems_ relems)
        | EUpdateRecord(e, relems) -> EUpdateRecord((trexp_ e), (trexp_relems_ relems))
        | EMatch(e, cases) -> EMatch((trexp_ e), (trexp_cases_ cases))
        | ETry(e, cases) -> ETry((trexp_ e), (trexp_cases_ cases))
        | ERaise(e) -> ERaise(trexp_ e)
        | EWhile(e1, e2) -> EWhile((trexp_ e1), (trexp_ e2))
        | EFor(asc, n, e1, e2, e3) -> EFor(asc, n, (trexp_ e1), (trexp_ e2), (trexp_ e3))
        | EForEach(p, idx, lst, body) -> EForEach(p, idx, (trexp_ lst), (trexp_ body))
        | EMap(p, lst, body) -> EMap(p, (trexp_ lst), (trexp_ body))
        | EFold(defv, (acc, acc0), (p, lst), body) ->
            EFold(defv, (acc, (trexp_ acc0)), (p, (trexp_ lst)), (trexp_ body))
        | EBlock(el) ->
            let new_el = List.fold_left (fun el e ->
                let e = trexp_ e in
                (* expand the nested blocks *)
                match e with
                | EBlock(el1) -> (List.rev el1) @ el
                | _ -> e :: el) [] el
                in
            (match new_el with
            | e :: [] -> e
            | _ -> EBlock(List.rev new_el))
        | ELet(p, e1, e2_opt) ->
            let e1 = trexp_ e1 in
            let e1 = match (p, e1) with
                | (PAny, _) | (PLit(LUnit), _) -> e1
                | (p, EFold(false, (acc, acc0), (iter_p, lst), body)) when p = acc ->
                    EFold(true, (acc, acc0), (iter_p, lst), body)
                | _ -> ELet(p, e1, None)
                in
            (match e2_opt with
            | Some(e2) ->
                let e2 = trexp_ e2 in
                (match e2 with
                | EBlock(el) -> EBlock(e1 :: el)
                | _ -> EBlock([e1; e2]))
            | _ -> e1)
        | ELetRec(fdefs, e2_opt) ->
            let fdefs = List.map (fun fdef ->
                let {ocf_name; ocf_args; ocf_body} = fdef in
                let body = trexp_ ocf_body in
                {ocf_name; ocf_args; ocf_body=body}) fdefs in
            let e1 = ELetRec(fdefs, None) in
            (match e2_opt with
            | Some(e2) ->
                let e2 = trexp_ e2 in
                (match e2 with
                | EBlock(el) -> EBlock(e1 :: el)
                | _ -> EBlock([e1; e2]))
            | _ -> e1)
        | EDefTyp _ -> e
        | EDefExn _ -> e
        | EOpen _ -> e
    and trexp_relems_ relems = List.map (fun (n, e) -> (n, trexp_ e)) relems
    and trexp_list_ el =
        List.map trexp_ el
    and trexp_cases_ cases =
        List.map (fun (pl, e) -> (pl, trexp_ e)) cases
    in
    let b = EBlock(code) in
    match (trexp_ b) with
    | EBlock(el) -> el
    | e -> e :: []
