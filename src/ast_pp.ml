open Ast

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

let lit_to_string c = match c with
    | LitInt(v) -> sprintf "%Li" v
    | LitSInt(b, v) -> sprintf "%Lii%d" v b
    | LitUInt(b, v) -> sprintf "%Luu%d" v b
    | LitFloat(16, v) -> sprintf "%.4gh" v
    | LitFloat(32, v) -> sprintf "%.8gf" v
    | LitFloat(64, v) -> sprintf "%.16g" v
    | LitFloat(b, v) -> failwith (sprintf "invalid literal LitFloat(%d, %.16g)" b v)
    | LitString(s) -> "\"" ^ (String.escaped s) ^ "\""
    | LitChar(c) -> "\'" ^ (String.escaped c) ^ "\'"
    | LitBool(true) -> "True"
    | LitBool(false) -> "False"
    | LitNil -> "Nil"

let pprint_lit x = pstr (lit_to_string x)
let pprint_id x = pstr (match x with Id.Name(0) -> "__" | _ -> id2str x)

type typ_pr_t = TypPr0 | TypPrFun | TypPrComplex | TypPrBase

let rec get_typ_pr t = match t with
    | TypVar {contents=None} -> TypPrBase
    | TypVar {contents=Some(t1)} -> get_typ_pr t1
    | TypInt | TypSInt(_) | TypUInt(_) | TypFloat(_)
    | TypString | TypChar | TypBool | TypVoid | TypExn
    | TypErr | TypCPointer | TypDecl | TypModule -> TypPrBase
    | TypApp([], _) -> TypPrBase
    | TypTuple(_) -> TypPrBase
    | TypRecord(_) -> TypPrBase
    | TypList(_) | TypRef(_) | TypArray(_, _) | TypApp(_, _) -> TypPrComplex
    | TypFun(_, _) -> TypPrFun

let need_parens p p1 = p1 > p
let opt_parens p p1 = if (need_parens p p1) then ("(", ")") else ("", "")

let rec pptype_ t p1 =
    let p = get_typ_pr t in
    let pptypsuf t1 suf =
        let (lp, rp) = opt_parens p p1 in
        (obox(); pstr lp; pptype_ t1 p; pstr rp; pstr " "; pstr suf; cbox()) in
    let pptypelist_ prefix args =
        pstr prefix; pcut(); obox();
        (List.iteri (fun i t -> if i = 0 then () else (pstr ","; pspace()); pptype_ t TypPr0) args);
        cbox(); pcut(); pstr ")" in
    match t with
    | TypVar {contents=None} -> pstr "Auto"
    | TypVar {contents=Some(t1)} -> pptype_ t1 p1
    | TypInt -> pstr "Int"
    | TypSInt(b) -> pstr ("Int" ^ (string_of_int b))
    | TypUInt(b) -> pstr ("UInt" ^ (string_of_int b))
    | TypFloat(16) -> pstr "Half"
    | TypFloat(32) -> pstr "Float"
    | TypFloat(64) -> pstr "Double"
    | TypFloat(b) -> failwith (sprintf "invalid type TypFloat(%d)" b)
    | TypString -> pstr "String"
    | TypChar -> pstr "Char"
    | TypBool -> pstr "Bool"
    | TypVoid -> pstr "Void"
    | TypFun(tl, t2) ->
            (*let (lp, rp) = opt_parens p p1 in*)
            obox(); pstr "(";
            (match tl with
            | [] -> pstr "Void"
            | t1 :: [] -> pptype_ t1 p
            | _ -> pptype_ (TypTuple tl) p);
            pspace(); pstr "->";
            pptype_ t2 p;
            pstr ")"; cbox()
    | TypList(t1) -> pptypsuf t1 "List"
    | TypRef(t1) -> pptypsuf t1 "Ref"
    | TypArray(d, t1) -> pptypsuf t1 ("[" ^ (String.make (d - 1) ',') ^ "]")
    | TypApp([], n) -> pprint_id n
    | TypApp(t1 :: [], n) -> pptypsuf t1 (id2str n)
    | TypApp(tl, n) -> pptypsuf (TypTuple tl) (id2str n)
    | TypTuple(tl) -> pptypelist_ "(" tl
    | TypRecord {contents=(rec_elems, name_opt)} ->
            let name = match name_opt with Some(i) -> (id2str i) | _ -> "??" in
            pstr name; pstr " {"; pcut(); obox();
            (List.iteri (fun i (n,t,v0_opt) -> if i = 0 then () else (pstr ";"; pspace());
                pprint_id n; pstr ":"; pspace(); pptype_ t TypPr0;
                match v0_opt with Some(v0) -> pprint_lit v0 | _ -> ()) rec_elems);
            cbox(); pcut(); pstr "}"
    | TypExn -> pstr "Exn"
    | TypErr -> pstr "Err"
    | TypCPointer -> pstr "CPtr"
    | TypDecl -> pstr "Declaration"
    | TypModule -> pstr "Module"

let pprint_typ t = pptype_ t TypPr0
let pprint_templ_args tt = match tt with
    | [] -> ()
    | t :: [] -> pprint_id t; pspace()
    | _ -> pstr "(";
        (List.iteri (fun i t -> if i = 0 then () else (pstr ","; pspace()); pprint_id t) tt);
        pstr ")"

let pprint_for_flags flags =
    match flags with
    | [] -> ()
    | _ -> pstr "<"; (List.iter (fun f ->
        pstr (match f with
        | ForParallel -> "PARALLEL, "
        | ForUnzip -> "UNZIP, "
        | _ -> "")) flags); pstr ">"; pspace()

let rec pprint_exp e =
    let t = get_exp_typ e in
    let obox_cnt = ref 0 in
    let obox_() = obox(); pstr "<"; pptype_ t TypPr0; pstr ">"; obox_cnt := !obox_cnt + 1 in
    let cbox_() = if !obox_cnt <> 0 then (cbox(); obox_cnt := !obox_cnt - 1) else () in
    let pphandlers pe_l = pstr "{"; pcut(); obox(); (List.iter (fun (pl, e) ->
            (List.iter (fun p -> pspace(); pstr "|"; pspace(); pprint_pat p) pl);
            pspace(); pstr "=>"; pspace(); pprint_exp_as_seq e) pe_l); pcut(); cbox(); pstr "}" in
    match e with
    | DefVal(p, e0, vflags, _) -> obox(); (List.iter (fun vf -> match vf with
        | ValTempRef -> pstr "TEMP_REF"; pspace()
        | ValMutable -> pstr "MUTABLE"; pspace()
        | ValArg -> pstr "ARG"; pspace()) vflags);
        pstr "VAL"; pspace(); pprint_pat p; pspace(); pstr "="; pspace(); pprint_exp e0; cbox()
    | DefFun {contents={df_name; df_templ_args; df_args; df_typ;
                df_body; df_flags; df_templ_inst }} ->
        let fkind = ref "FUN" in
        let is_constr = List.mem FunConstr df_flags in
        (obox(); (List.iter (fun ff -> match ff with
                    | FunPure -> pstr "PURE"; pspace()
                    | FunImpure -> pstr "IMPURE"; pspace()
                    | FunInline -> pstr "INLINE"; pspace()
                    | FunNoThrow -> pstr "NOTHROW"; pspace()
                    | FunStatic -> pstr "STATIC"; pspace()
                    | FunConstr -> ()
                    | FunInC -> pstr "C_FUNC"; pspace()) df_flags);
        pstr (!fkind); pspace(); pprint_templ_args df_templ_args; pprint_id df_name; pspace();
        pstr "("; pcut(); obox();
        (List.iteri (fun i p -> if i = 0 then () else (pstr ","; pspace()); pprint_pat p) df_args);
        cbox(); pcut(); pstr ")";
        pspace(); pstr ":"; pspace(); pprint_typ df_typ; pspace();
        pstr "="; pspace(); if is_constr then pstr "Constructor" else pprint_exp df_body; cbox())
    | DefExn { contents = {dexn_name; dexn_typ } } ->
        obox(); pstr "EXCEPTION"; pspace(); pprint_id dexn_name;
        (match dexn_typ with
        | TypVoid -> ()
        | _ -> pspace(); pstr "OF"; pspace(); pprint_typ dexn_typ); cbox()
    | DefTyp { contents = {dt_name; dt_templ_args; dt_typ }} ->
        obox(); pstr "TYPE"; pspace(); pprint_templ_args dt_templ_args; pprint_id dt_name;
        pspace(); pstr "="; pspace(); pprint_typ dt_typ; cbox()
    | DefVariant { contents = {dvar_name; dvar_templ_args; dvar_alias; dvar_cases; dvar_constr; dvar_templ_inst} } ->
        obox(); pstr "TYPE"; pspace(); pprint_templ_args dvar_templ_args; pprint_id dvar_name; pstr "<";
        pprint_typ dvar_alias; pstr ">";
        pspace(); pstr "="; pspace(); (List.iteri (fun i ((v, t), c) ->
            if i = 0 then () else pstr " | "; pprint_id v;
            pstr "<"; pprint_id c; pstr ": "; pprint_typ (get_id_typ c); pstr ">: "; pprint_typ t)
            (Utils.zip dvar_cases (if dvar_constr != [] then dvar_constr else (List.map (fun (v, _) -> v) dvar_cases))));
        cbox()
    | DirImport(ml, _) -> pstr "IMPORT"; pspace();
        obox(); (List.iteri (fun i (n1, n2) -> if i = 0 then () else (pstr ","; pspace()); pprint_id n1;
                    if n1 = n2 then () else (pspace(); pstr "AS"; pspace(); pprint_id n2)) ml); cbox()
    | DirImportFrom(m, nl, _) ->
        obox(); pstr "FROM"; pspace(); pprint_id m; pspace(); pstr "IMPORT"; pspace();
        (match nl with
        | [] -> pstr "*"
        | _ -> List.iteri (fun i n -> if i = 0 then () else (pstr ","; pspace()); pprint_id n) nl); cbox()
    | ExpSeq(el, _) -> pprint_expseq el true
    | _ -> obox_(); (match e with
        | ExpNop(_) -> pstr "{}"
        | ExpBreak(_) -> pstr "BREAK"
        | ExpContinue(_) -> pstr "CONTINUE"
        | ExpRange(e1_opt, e2_opt, e3_opt, _) ->
            pstr "(";
            (match e1_opt with
            | Some(e1) -> pprint_exp e1
            | None -> ());
            pstr ":";
            (match e2_opt with
            | Some(e2) -> pprint_exp e2
            | None -> ());
            (match e3_opt with
            | Some(e3) -> pstr ":"; pprint_exp e3
            | None -> ());
            pstr ")"
        | ExpLit(x, _) -> pprint_lit x
        | ExpIdent(n, _) -> pprint_id n
        | ExpBinOp(o, e1, e2, _) ->
            let ostr = binop_to_string o in
            pstr "("; pprint_exp e1; pspace(); pstr ostr; pspace(); pprint_exp e2; pstr ")"
        | ExpAssign(e1, e2, _) -> pprint_exp e1; pspace(); pstr "="; pspace(); pprint_exp e2
        | ExpMem(e1, e2, _) -> pprint_exp e1; pstr "."; pprint_exp e2
        | ExpUnOp(o, e1, _) ->
            let ostr = unop_to_string o in
            pstr "("; pstr ostr; pspace(); pprint_exp e1; pstr ")"
        | ExpDeref(e1, _) -> pstr "*"; pprint_exp e1
        | ExpMakeRef(e1, _) -> pstr "REF ("; pprint_exp e1; pstr ")"
        | ExpThrow(e1, _) -> pstr "THROW ("; pprint_exp e1; pstr ")"
        | ExpMkTuple(el, _) ->
            pstr "("; obox();
            (List.iteri (fun i e ->
                if i = 0 then () else (pstr ","; pspace()); pprint_exp e) el);
            (match el with
            e :: [] -> pstr ","
            | _ -> ()); cbox(); pstr ")"
        | ExpMkRecord(rn, relems, _) ->
            obox(); pprint_exp rn; pstr "{"; obox();
            (List.iteri (fun i (n,v) ->
                if i = 0 then () else (pstr ","; pspace()); pprint_id n; pstr "="; pprint_exp v) relems);
            cbox(); pstr "}"; cbox()
        | ExpUpdateRecord(e, relems, _) ->
            obox(); pprint_exp e; pstr " WITH {"; obox();
            (List.iteri (fun i (n,v) ->
                if i = 0 then () else (pstr ","; pspace()); pprint_id n; pstr "="; pprint_exp v) relems);
            cbox(); pstr "}"; cbox()
        | ExpMkArray(arows, _) ->
            obox(); pstr "[";
            (List.iteri (fun i acols ->
                if i = 0 then () else (pstr ";"; pspace()); obox();
                (List.iteri (fun i a -> if i = 0 then () else (pstr ","; pspace()); pprint_exp a) acols);
                cbox()) arows);
            pstr "]"; cbox()
        | ExpCall(f, args, _) ->
            obox(); pprint_exp f; pstr "(";
            (List.iteri (fun i e ->
                if i = 0 then () else (pstr ","; pspace()); pprint_exp e) args);
            pstr ")"; cbox();
        | ExpAt(a, args, _) ->
            pprint_exp a; pstr "[";
            obox(); (List.iteri (fun i e ->
                if i = 0 then () else (pstr ","; pspace()); pprint_exp e) args);
            cbox(); pstr "]"
        | ExpIf(if_seq, if_then, if_else, _) ->
            obox(); pstr "IF ("; pprint_exp if_seq; pstr ")"; pspace(); pprint_exp if_then; pspace();
            pstr "ELSE"; pspace(); pprint_exp if_else; cbox()
        | ExpWhile(c, body, _) ->
            obox(); pstr "WHILE ("; pprint_exp c; pstr ")"; pspace(); pprint_exp body; cbox()
        | ExpDoWhile(body, c, _) ->
            obox(); pstr "DO"; pprint_exp body; pspace(); pstr "WHILE ("; pprint_exp c; pstr ")"; cbox()
        | ExpFor (for_cl, for_body, flags, _) ->
            obox(); pprint_for_flags flags;
            pstr "FOR ("; (List.iteri (fun i (p, e) -> if i = 0 then () else (pstr ","; pspace());
                pprint_pat p; pspace(); pstr "IN"; pspace(); pprint_exp e) for_cl);
            pstr ")"; pspace(); pprint_exp for_body; cbox()
        | ExpMap(map_cl, map_body, flags, _) ->
            obox(); pprint_for_flags flags;
            pstr "["; if (List.mem ForMakeList flags) then pstr ":: " else ();
            (List.iter (fun (pe_l, opt_when) -> pstr "FOR ("; (List.iteri (fun i (p, e) -> if i = 0 then () else (pstr ","; pspace());
            pprint_pat p; pspace(); pstr "IN"; pspace(); pprint_exp e) pe_l);
            pstr ")"; pspace(); match opt_when with Some(e) -> pstr "WHEN "; pprint_exp e; pspace() | _ -> ()) map_cl);
            pprint_exp map_body; pstr "]"; cbox()
        | ExpMatch(e, pe_l, _) ->
            obox(); pstr "MATCH ("; pprint_exp e; pstr ")"; pspace();
            pphandlers pe_l; cbox()
        | ExpTryCatch(e, pe_l, _) ->
            obox(); pstr "TRY"; pspace(); pprint_exp e; pspace();
            pstr "CATCH"; pphandlers pe_l; cbox()
        | ExpCast(e, t, _) -> pstr "("; obox(); pprint_exp e; pspace(); pstr ":>"; pspace(); pprint_typ t; cbox(); pstr ")"
        | ExpTyped(e, t, _) -> pstr "("; obox(); pprint_exp e; pspace(); pstr ":"; pspace(); pprint_typ t; cbox(); pstr ")"
        | ExpCCode(s, _) -> pstr "CCODE"; pspace(); pstr "\"\"\""; pstr s; pstr "\"\"\""
        | _ -> printf "\n\n\n\nunknown exp!!!!!!!!!!!!!!!!!!!!\n\n\n\n"; failwith "unknown exp"
        ); cbox_()
and pprint_exp_as_seq e = match e with
    | ExpSeq(es, _) -> pprint_expseq es false
    | _ -> pprint_exp e
and pprint_expseq el braces =
    if braces then pstr "{" else ();
    ohvbox();
    (List.iteri (fun i e -> if i=0 then () else (pstr ";"; pspace()); pprint_exp e) el); cbox();
    if braces then pstr "}" else ()
and pprint_pat p = match p with
    | PatAny(_) -> pstr "_<ANY>"
    | PatAs(p, n, _) -> pstr "("; pprint_pat p; pspace(); pstr "AS"; pspace(); pprint_id n; pstr ")"
    | PatLit(c, _) -> pprint_lit c
    | PatCons(p1, p2, _) -> pstr "("; pprint_pat p1; pspace(); pstr "::"; pspace(); pprint_pat p2; pstr ")"
    | PatIdent(n, _) -> pprint_id n
    | PatTuple(pl, _) -> pstr "("; obox();
        (List.iteri (fun i p -> if i = 0 then () else (pstr ","; pspace()); pprint_pat p) pl);
        cbox(); pstr ")"
    | PatVariant(n, elems, loc) -> pprint_id n; pprint_pat (PatTuple (elems, loc))
    | PatRec(n_opt, elems, loc) ->
        obox(); (match n_opt with Some(n) -> pprint_id n; pspace() | _ -> ()); pstr "{";
        List.iteri (fun i (n, p) -> if i = 0 then () else (pstr ","; pspace());
                    pprint_id n; pstr "="; pprint_pat p) elems;
        pstr "}"; cbox()
    | PatTyped(p, t, _) -> pprint_pat p; pstr ":"; pspace(); pprint_typ t

let pprint_mod { dm_name; dm_filename; dm_defs; dm_deps } =
    Format.print_flush ();
    Format.open_vbox 0;
    pcut();
    pstr dm_filename;
    (match dm_defs with
    | [] -> ()
    | _ -> pstr ":"; obox();
        pspace(); (match dm_deps with [] -> pstr "<no deps>"
        | _ -> List.iteri (fun i n -> if i = 0 then () else (pstr ","; pspace()); pprint_id n) dm_deps); cbox());
    pcut();
    pstr "---------------------------------------------------------"; pcut();
    (List.iter (fun e -> pprint_exp e; pstr ";"; pcut()) dm_defs);
    Format.close_box();
    Format.print_flush ()

let pprint_typ_x t = Format.print_flush (); Format.open_box 0; pprint_typ t; Format.close_box(); Format.print_flush ()
let pprint_exp_x e = Format.print_flush (); Format.open_box 0; pprint_exp e; Format.close_box(); Format.print_flush ()
let pprint_pat_x p = Format.print_flush (); Format.open_box 0; pprint_pat p; Format.close_box(); Format.print_flush ()