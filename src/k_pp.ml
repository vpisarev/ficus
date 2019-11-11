open Ast
open K_form

let base_indent = ref 3

let pstr = Format.print_string
let pspace = Format.print_space
let pcut = Format.print_cut
let pbreak () = Format.print_break 1 0
let pbreak_indent () = Format.print_break 1 (!base_indent)
let obox () = Format.open_box (!base_indent)
let obox_indent () = Format.open_box (!base_indent)
let cbox = Format.close_box
let ovbox () = Format.open_vbox (!base_indent)
let ohvbox () = Format.open_hvbox 0
let ohvbox_indent () = Format.open_hvbox (!base_indent)

let pprint_id x = pstr (id2str x)

type ktyp_pr_t = KTypPr0 | KTypPrFun | KTypPrComplex | KTypPrBase

let rec get_ktyp_pr t = match t with
    | KTypInt | KTypSInt(_) | KTypUInt(_) | KTypFloat(_)
    | KTypString | KTypChar | KTypBool | KTypVoid | KTypExn
    | KTypErr | KTypCPointer | KTypNil | KTypModule | KTypName(_) -> KTypPrBase
    | KTypTuple(_) | KTypRecord(_, _) -> KTypPrBase
    | KTypList(_) | KTypRef(_) | KTypArray(_, _) -> KTypPrComplex
    | KTypFun(_, _) -> KTypPrFun

let need_parens p p1 = p1 > p
let opt_parens p p1 = if (need_parens p p1) then ("(", ")") else ("", "")

let rec ppktyp_ t p1 =
    let p = get_ktyp_pr t in
    let ppktypsuf t1 suf =
        let (lp, rp) = opt_parens p p1 in
        (obox(); pstr lp; ppktyp_ t1 p; pstr rp; pstr " "; pstr suf; cbox()) in
    let ppktyplist_ prefix args =
        pstr prefix; pcut(); obox();
        (List.iteri (fun i t -> if i = 0 then () else (pstr ","; pspace()); ppktyp_ t KTypPr0) args);
        cbox(); pcut(); pstr ")" in
    match t with
    | KTypInt -> pstr "Int"
    | KTypSInt(b) -> pstr ("Int" ^ (string_of_int b))
    | KTypUInt(b) -> pstr ("UInt" ^ (string_of_int b))
    | KTypFloat(16) -> pstr "Half"
    | KTypFloat(32) -> pstr "Float"
    | KTypFloat(64) -> pstr "Double"
    | KTypFloat(b) -> failwith (sprintf "invalid type TypFloat(%d)" b)
    | KTypString -> pstr "String"
    | KTypChar -> pstr "Char"
    | KTypBool -> pstr "Bool"
    | KTypVoid -> pstr "Void"
    | KTypNil -> pstr "Nil"
    | KTypFun(tl, t2) ->
            (*let (lp, rp) = opt_parens p p1 in*)
            obox(); pstr "(";
            (match tl with
            | [] -> pstr "Void"
            | t1 :: [] -> ppktyp_ t1 p
            | _ -> ppktyp_ (KTypTuple tl) p);
            pspace(); pstr "->";
            ppktyp_ t2 p;
            pstr ")"; cbox()
    | KTypList(t1) -> ppktypsuf t1 "List"
    | KTypRef(t1) -> ppktypsuf t1 "Ref"
    | KTypArray(d, t1) -> ppktypsuf t1 ("[" ^ (String.make (d - 1) ',') ^ "]")
    | KTypName(n) -> pprint_id n
    | KTypTuple(tl) -> ppktyplist_ "(" tl
    | KTypRecord(rn, relems) -> obox(); pprint_id rn; pstr " {";
        List.iteri (fun i (ni, ti) -> if i = 0 then () else pstr ", ";
            pprint_id ni; pstr ": "; ppktyp_ ti KTypPr0) relems; pstr "}"; cbox()
    | KTypExn -> pstr "Exn"
    | KTypErr -> pstr "Err"
    | KTypCPointer -> pstr "CPtr"
    | KTypModule -> pstr "Module"

let pprint_ktyp t = ppktyp_ t KTypPr0
let pprint_atom a = match a with
    | Atom.Id k -> pprint_id k
    | Atom.Lit l -> pstr (Ast_pp.lit_to_string l)
let pprint_dom r = match r with
    | Domain.Elem(a) -> pprint_atom a
    | Domain.Fast(a) -> pstr "<"; pprint_atom a; pstr ">"
    | Domain.Range(i,j,k) -> pprint_atom i; pstr ":";
          (match j with
          | Atom.Lit(LitNil) -> ()
          | _ -> pprint_atom j);
          (match k with
          | Atom.Lit(LitNil) -> ()
          | Atom.Lit(LitInt 1L) -> ()
          | _ -> pstr ":"; pprint_atom k)

let rec pprint_kexp e =
    let t = get_kexp_ktyp e in
    let obox_cnt = ref 0 in
    let obox_() = obox(); pstr "<"; ppktyp_ t KTypPr0; pstr ">"; obox_cnt := !obox_cnt + 1 in
    let cbox_() = if !obox_cnt <> 0 then (cbox(); obox_cnt := !obox_cnt - 1) else () in
    match e with
    | KDefVal(n, e0, loc) -> obox();
        let (vt, vflags) = match (kinfo n) with
            | KVal {kv_typ; kv_flags} -> (kv_typ, kv_flags)
            | _ -> raise_compile_err loc (sprintf "there is no information about defined value '%s'" (id2str n)) in
        (List.iter (fun vf -> match vf with
        | ValTempRef -> pstr "TEMP_REF"; pspace()
        | ValMutable -> pstr "MUTABLE"; pspace()
        | ValArg -> pstr "ARG"; pspace()) vflags);
        pstr "VAL"; pspace(); pprint_id n; pstr ": "; pprint_ktyp vt; pspace(); pstr "="; pspace(); pprint_kexp e0; cbox()
    | KDefFun {contents={kf_name; kf_args; kf_typ; kf_body; kf_flags }} ->
        let fkind = ref "FUN" in
        let is_constr = List.mem FunConstr kf_flags in
        (obox(); (List.iter (fun ff -> match ff with
                    | FunPure -> pstr "PURE"; pspace()
                    | FunImpure -> pstr "IMPURE"; pspace()
                    | FunInline -> pstr "INLINE"; pspace()
                    | FunNoThrow -> pstr "NOTHROW"; pspace()
                    | FunStatic -> pstr "STATIC"; pspace()
                    | FunConstr -> ()
                    | FunInC -> pstr "C_FUNC"; pspace()) kf_flags);
        pstr (!fkind); pspace(); pprint_id kf_name; pspace();
        pstr "("; pcut(); obox();
        (List.iteri (fun i n -> if i = 0 then () else (pstr ","; pspace()); pprint_id n) kf_args);
        cbox(); pcut(); pstr ")";
        pspace(); pstr ":"; pspace(); pprint_ktyp kf_typ; pspace();
        pstr "="; pspace(); if is_constr then pstr "Constructor" else pprint_kexp kf_body; cbox())
    | KDefExn { contents = {ke_name; ke_typ } } ->
        obox(); pstr "EXCEPTION"; pspace(); pprint_id ke_name;
        (match ke_typ with
        | KTypVoid -> ()
        | _ -> pspace(); pstr "OF"; pspace(); pprint_ktyp ke_typ); cbox()
    | KDefVariant { contents = {kvar_name; kvar_cases; kvar_constr} } ->
        obox(); pstr "TYPE"; pspace(); pprint_id kvar_name;
        pspace(); pstr "="; pspace(); (List.iteri (fun i ((v, t), c) ->
            if i = 0 then () else pstr " | "; pprint_id v;
            pstr "<"; pprint_id c; pstr ": "; pprint_ktyp (get_id_ktyp c); pstr ">: "; pprint_ktyp t)
            (Utils.zip kvar_cases (if kvar_constr != [] then kvar_constr else (List.map (fun (v, _) -> v) kvar_cases))));
        cbox()
    | KExpSeq(el, _) -> pprint_kexpseq el true
    | _ -> obox_(); (match e with
        | KExpNop(_) -> pstr "{}"
        | KExpBreak(_) -> pstr "BREAK"
        | KExpContinue(_) -> pstr "CONTINUE"
        | KExpAtom(a, _) -> pprint_atom a
        | KExpBinOp(o, a, b, _) ->
            let ostr = binop_to_string o in
            pprint_atom a; pspace(); pstr ostr; pspace(); pprint_atom b
        | KExpAssign(n, e, _) -> pprint_id n; pspace(); pstr "="; pspace(); pprint_kexp e
        | KExpMem(n, i, _) -> pprint_id n; pstr "."; pstr (string_of_int i)
        | KExpUnOp(o, a, _) ->
            let ostr = unop_to_string o in
            pstr ostr; pprint_atom a
        | KExpDeref(n, _) -> pstr "*"; pprint_id n
        | KExpIntrin(iop, args, _) ->
            pstr (intrin2str iop);
            List.iteri (fun i a -> if i = 0 then pstr "(" else (pstr ","; pspace()); pprint_atom a) args;
            pstr ")"
        | KExpThrow(n, _) -> pstr "THROW "; pprint_id n
        | KExpMkTuple(al, _) ->
            pstr "("; obox();
            (List.iteri (fun i a ->
                if i = 0 then () else (pstr ","; pspace()); pprint_atom a) al);
            (match al with
            a :: [] -> pstr ","
            | _ -> ()); cbox(); pstr ")"
        | KExpMkRecord(al, _) ->
            pstr "{"; obox();
            (List.iteri (fun i a ->
                if i = 0 then () else (pstr ","; pspace()); pprint_atom a) al);
            (match al with
            a :: [] -> pstr ","
            | _ -> ()); cbox(); pstr "}"
        | KExpMkArray(shape, elems, (_, l)) ->
            let total_elems = List.length elems in
            let total_elems2 = List.fold_left (fun p di -> p*di) 1 shape in
            let cols = Utils.last_elem shape in
            let _ = if total_elems != total_elems2 then
                failwith (sprintf "%s: the number of array elements does not match the product of dimensions (%d vs %d)" (loc2str l) total_elems total_elems2)
                else () in
            obox(); pstr "["; obox();
            (List.iteri (fun i a ->
                if i mod cols != 0 || i = 0 then () else (cbox(); pstr ";"; pcut(); obox());
                pprint_atom a) elems);
            cbox(); pstr "]"; cbox()
        | KExpCall(f, args, _) ->
            obox(); pprint_id f; pstr "(";
            (List.iteri (fun i a ->
                if i = 0 then () else (pstr ","; pspace()); pprint_atom a) args);
            pstr ")"; cbox();
        | KExpAt(a, args, _) ->
            pprint_atom a; pstr "[";
            obox(); (List.iteri (fun i dom ->
                if i = 0 then () else (pstr ","; pspace()); pprint_dom dom) args);
            cbox(); pstr "]"
        | KExpIf(if_c, if_then, if_else, _) ->
            obox(); pstr "IF ("; pprint_kexp if_c; pstr ")";
            pspace(); pprint_kexp if_then; pspace();
            pstr "ELSE"; pspace(); pprint_kexp if_else; cbox()
        | KExpWhile(c, body, _) ->
            obox(); pstr "WHILE ("; pprint_kexp c; pstr ")"; pspace(); pprint_kexp body; cbox()
        | KExpDoWhile(body, c, _) ->
            obox(); pstr "DO"; pprint_kexp body; pspace(); pstr "WHILE ("; pprint_kexp c; pstr ")"; cbox()
        | KExpFor (for_cl, for_body, flags, _) ->
            obox(); Ast_pp.pprint_for_flags flags;
            pstr "FOR ("; (List.iteri (fun i (n, dom) -> if i = 0 then () else (pstr ","; pspace());
                pprint_id n; pspace(); pstr "<-"; pspace(); pprint_dom dom) for_cl);
            pstr ")"; pspace(); pprint_kexp for_body; cbox()
        | KExpMap(map_cl, map_body, flags, _) ->
            obox(); Ast_pp.pprint_for_flags flags;
            pstr "["; if (List.mem ForMakeList flags) then pstr ":: " else ();
            (List.iter (fun (pre, pe_l) -> pstr "FOR ("; pprint_kexp pre; pstr ";"; pcut();
              (List.iteri (fun i (n, dom) -> if i = 0 then () else (pstr ","; pspace());
              pprint_id n; pspace(); pstr "<-"; pspace(); pprint_dom dom) pe_l);
              pstr ")"; pspace()) map_cl);
            pprint_kexp map_body; pstr "]"; cbox()
        | KExpMatch (handlers, _) ->
            obox(); List.iteri (fun i (checks_i, e_i) ->
                pstr (if i = 0 then "IF (" else if checks_i = [] then "ELSE" else "ELSE IF (");
                List.iteri (fun j cj ->
                    if j = 0 then () else (pstr " &&"; pspace());
                    pprint_kexp cj) checks_i;
                if checks_i = [] then () else pstr ")";
                pspace(); pprint_kexp e_i) handlers;
            cbox();
        | KExpTryCatch(e1, e2, _) ->
            obox(); pstr "TRY"; pspace(); pprint_kexp e1; pspace();
            pstr "CATCH"; pprint_kexp e2; cbox()
        | KExpCast(a, t, _) -> pstr "("; obox(); pprint_atom a; pspace(); pstr ":>"; pspace(); pprint_ktyp t; cbox(); pstr ")"
        | KExpCCode(s, _) -> pstr "CCODE"; pspace(); pstr "\"\"\""; pstr s; pstr "\"\"\""
        | _ -> printf "\n\n\n\nunknown exp!!!!!!!!!!!!!!!!!!!!\n\n\n\n"; failwith "unknown exp"
        ); cbox_()
and pprint_kexp_as_seq e = match e with
    | KExpSeq(es, _) -> pprint_kexpseq es false
    | _ -> pprint_kexp e
and pprint_kexpseq el braces =
    if braces then pstr "{" else ();
    ohvbox();
    (List.iteri (fun i e -> if i=0 then () else (pstr ";"; pspace()); pprint_kexp e) el); cbox();
    if braces then pstr "}" else ()

let pprint_ktyp_x t = Format.print_flush (); Format.open_box 0; pprint_ktyp t; Format.close_box(); Format.print_flush ()
let pprint_kexp_x e = Format.print_flush (); Format.open_box 0; pprint_kexp e; Format.close_box(); Format.print_flush ()
let pprint_top code = Format.print_flush (); Format.open_box 0; pprint_kexpseq code false; Format.close_box(); pbreak(); Format.print_flush ()
