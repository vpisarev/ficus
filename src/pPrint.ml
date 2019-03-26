open Syntax

let base_indent = ref 3

let pstr = Format.print_string
let pspace = Format.print_space
let pcut = Format.print_cut
let obox () = Format.open_box (!base_indent)
let cbox = Format.close_box
let ovbox () = Format.open_vbox (!base_indent)
let cvbox = Format.close_box

let lit_to_string c = match c with
    | LitInt(v) -> (Int64.to_string v)
    | LitSInt(b, v) -> Printf.sprintf "%Lii%d" v b
    | LitUInt(b, v) -> Printf.sprintf "%Luu%d" v b
    | LitFloat(16, v) -> Printf.sprintf "%.4gh" v
    | LitFloat(32, v) -> Printf.sprintf "%.8gf" v
    | LitFloat(64, v) -> Printf.sprintf "%.16g" v
    | LitFloat(b, v) -> failwith (Printf.sprintf "invalid literal LitFloat(%d, %.16g)" b v)
    | LitString(s) -> "\"" ^ (String.escaped s) ^ "\""
    | LitChar(c) -> "\'" ^ (String.escaped c) ^ "\'"
    | LitBool(true) -> "true"
    | LitBool(false) -> "false"
    | LitUnit -> "{}"
    | LitNil -> "[]"

let pprint_lit x = pstr (lit_to_string x)
let pprint_id x = pstr (pp_id2str x)

type type_pr_t = TypPr0 | TypPrFun | TypPrComplex | TypPrBase

let rec get_type_pr t = match t with
    | TypVar {contents=None} -> TypPrBase
    | TypVar {contents=Some(t1)} -> get_type_pr t1
    | TypInt | TypSInt(_) | TypUInt(_) | TypFloat(_)
    | TypString | TypChar | TypBool | TypVoid | TypExc
    | TypThrow | TypCPointer | TypDecl -> TypPrBase
    | TypApp([], _) -> TypPrBase
    | TypTuple(_) -> TypPrBase
    | TypList(_) | TypRef(_) | TypArray(_) | TypApp(_, _) -> TypPrComplex
    | TypFun(_, _) -> TypPrFun

let need_parens p p1 = p1 > p
let opt_parens p p1 = if (need_parens p p1) then ("(", ")") else ("", "")

let rec pptype_ t p1 =
    let p = get_type_pr t in
    let pptypsuf t1 suf =
        let (lp, rp) = opt_parens p p1 in
        (obox(); pstr lp; pptype_ t1 p; pstr rp; pspace(); pstr suf; cbox()) in
    let pptypelist_ prefix args =
        pstr prefix; pcut(); obox();
        (List.iteri (fun i t -> if i = 0 then () else (pstr ","; pspace()); pptype_ t TypPr0) args);
        cbox(); pcut(); pstr ")" in
    let pprec_ prefix args =
        pstr prefix; pcut(); obox();
        (List.iteri (fun i (n1, t1, opt_x1) ->
            if i = 0 then () else (pstr ","; pspace());
            pprint_id n1; pstr ":"; pspace(); pptype_ t1 TypPr0;
            (match opt_x1 with
            | None -> ()
            | Some(x1) -> pstr "="; pcut(); pprint_lit x1)) args); cbox(); pcut(); pstr "}" in
    match t with
    | TypVar {contents=None} -> pstr "Auto"
    | TypVar {contents=Some(t1)} -> pptype_ t1 p1
    | TypInt -> pstr "Int"
    | TypSInt(b) -> pstr ("Int" ^ (string_of_int b))
    | TypUInt(b) -> pstr ("UInt" ^ (string_of_int b))
    | TypFloat(16) -> pstr "Half"
    | TypFloat(32) -> pstr "Float"
    | TypFloat(64) -> pstr "Double"
    | TypFloat(b) -> failwith (Printf.sprintf "invalid type TypFloat(%d)" b)
    | TypString -> pstr "String"
    | TypChar -> pstr "Char"
    | TypBool -> pstr "Bool"
    | TypVoid -> pstr "Void"
    | TypFun(tl, t2) ->
            let (lp, rp) = opt_parens p p1 in
            obox(); pstr lp;
            (match tl with
            | [] -> pstr "Void"
            | t1 :: [] -> pptype_ t1 p
            | _ -> pptype_ (TypTuple tl) p);
            pspace(); pstr "->";
            pptype_ t2 p;
            pstr rp; cbox()
    | TypList(t1) -> pptypsuf t1 "List"
    | TypRef(t1) -> pptypsuf t1 "Ref"
    | TypArray(d, t1) -> pptypsuf t1 (String.make (d - 1) ',')
    | TypApp([], n) -> pprint_id n
    | TypApp(t1 :: [], n) -> pptypsuf t1 (pp_id2str n)
    | TypApp(tl, n) -> pptypsuf (TypTuple tl) (pp_id2str n)
    | TypTuple(tl) -> pptypelist_ "(" tl
    | TypExc -> pstr "Exn"
    | TypThrow -> pstr "<Thrown exn>"
    | TypCPointer -> pstr "CPtr"
    | TypDecl -> pstr "<Declaration>"

let pprint_type t = pptype_ t TypPr0
let pprint_template_args tt = match tt with
    | [] -> ()
    | t :: [] -> pprint_id t; pspace()
    | _ -> pstr "(";
        (List.iteri (fun i t -> if i = 0 then () else (pstr ","; pspace()); pprint_id t) tt);
        pstr ")"

let bin_op_to_string op = match op with
    | OpAdd -> "+"
    | OpSub -> "-"
    | OpMul -> "*"
    | OpDiv -> "/"
    | OpMod -> "%"
    | OpPow -> "**"
    | OpShiftLeft -> "<<"
    | OpShiftRight -> ">>"
    | OpBitwiseAnd -> "&"
    | OpLogicAnd -> "&&"
    | OpBitwiseOr -> "|"
    | OpLogicOr -> "||"
    | OpBitwiseXor -> "^"
    | OpCompareEQ -> "=="
    | OpCompareNE -> "!="
    | OpCompareLT -> "<"
    | OpCompareLE -> "<="
    | OpCompareGT -> ">"
    | OpCompareGE -> ">="
    | OpCons -> "::"
    | OpDot -> "."
    | OpSet -> "="

let un_op_to_string op = match op with
    | OpNegate -> "-"
    | OpBitwiseNot -> "~"
    | OpLogicNot -> "!"
    | OpMakeRef -> "ref"
    | OpDeref -> "*"
    | OpThrow -> "throw"

let rec pprint_exp e =
    let t = get_exp_type e in
    let obox_() = obox(); pstr "<"; pptype_ t TypPr0; pstr ">"; in
    let pphandlers pe_l = (List.iter (fun (pl, e) ->
            (List.iter (fun p -> pspace(); pstr "|"; pspace(); pprint_pat p) pl);
            pspace(); pstr "=>"; pspace(); obox(); pprint_exp_as_seq e; cbox()) pe_l); pspace(); pstr "END" in
    match e with
    | DefVal(p, e0, vflags, _) -> obox(); (List.iter (fun vf -> match vf with
        | ValMutable -> pstr "MUTABLE"; pspace()
        | ValArg -> pstr "ARG"; pspace()) vflags);
        pstr "VAL"; pspace(); pprint_pat p; pcut(); pstr "="; pprint_exp e0; cbox()
    | DefFun {contents={df_name; df_template_args; df_args; df_rt;
                df_body; df_flags; df_template_inst }} ->
        let fkind = ref "FUN" in
        (obox(); (List.iter (fun ff -> match ff with
                    | FuncPure -> pstr "PURE"; pspace()
                    | FuncImpure -> pstr "IMPURE"; pspace()
                    | FuncInC -> pstr "C_FUNC"; pspace()) df_flags);
        pstr (!fkind); pspace(); pprint_template_args df_template_args; pprint_id df_name; pspace();
        pstr "("; pcut(); obox();
        (List.iteri (fun i p -> if i = 0 then () else (pstr ","; pspace()); pprint_pat p) df_args);
        cbox(); pcut(); pstr ")";
        pspace(); pstr ":"; pspace(); pprint_type df_rt; pspace(); pstr "="; pspace();
        (match df_body with
        | ExpSeq(_, _) -> pprint_exp df_body
        | _ -> obox(); pprint_exp df_body; cbox());
        cbox())
    | DefExc { contents = {dexc_name; dexc_tp } } ->
        pstr "EXCEPTION"; pspace(); pprint_id dexc_name;
        (match dexc_tp with
        | TypVoid -> ()
        | _ -> pspace(); pstr "OF"; pspace(); pprint_type dexc_tp)
    | DefType { contents = {dt_name; dt_template_args; dt_body }} ->
        pstr "TYPE"; pspace(); pprint_template_args dt_template_args; pprint_id dt_name;
        pspace(); pstr "="; pspace(); pprint_type dt_body
    | DirImport(ml, _) -> pstr "IMPORT"; pspace();
        (List.iteri (fun i (n1, n2) -> if i = 0 then () else (pstr ","; pspace()); pprint_id n1;
                    if n1 = n2 then () else (pspace(); pstr "AS"; pspace(); pprint_id n2)) ml)
    | DirImportFrom(m, nl, _) ->
        pstr "FROM"; pspace(); pprint_id m; pspace(); pstr "IMPORT"; pspace();
        (match nl with
        | [] -> pstr "*"
        | _ -> List.iteri (fun i n -> if i = 0 then () else (pstr ","; pspace()); pprint_id n) nl)
    | ExpSeq(el, _) -> pprint_expseq el true
    | _ -> obox_(); (match e with
        | ExpNop(_) -> pstr "{}"
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
            let ostr = bin_op_to_string o in
            pstr "("; pprint_exp e1; pspace(); pstr ostr; pspace(); pprint_exp e2; pstr ")"
        | ExpUnOp(o, e1, _) ->
            let ostr = un_op_to_string o in
            pstr "("; pstr ostr; pspace(); pprint_exp e1; pstr ")"
        | ExpMkTuple(el, _) ->
            pstr "("; obox(); (List.iteri (fun i e ->
                if i = 0 then () else (pstr ","; pspace()); pprint_exp e) el);
                (match el with
                e :: [] -> pstr ","
                | _ -> ()); cbox(); pstr ")"
        | ExpCall(f, args, _) ->
            pprint_exp f; pstr "(";
            obox(); (List.iteri (fun i e ->
                if i = 0 then () else (pstr ","; pspace()); pprint_exp e) args);
            cbox(); pstr ")"
        | ExpAt(a, args, _) ->
            pprint_exp a; pstr "[";
            obox(); (List.iteri (fun i e ->
                if i = 0 then () else (pstr ","; pspace()); pprint_exp e) args);
            cbox(); pstr "]"
        | ExpIf(if_c, if_then, opt_if_else, _) ->
            pstr "IF ("; pprint_exp if_c; pstr ")";
            pspace(); pprint_exp if_then; pspace();
            (match opt_if_else with
            | None -> pstr "ELSE {}"
            | Some(else_e) -> pstr "ELSE"; pspace(); pprint_exp else_e)
        | ExpWhile(c, body, _) -> pstr "WHILE ("; pprint_exp c; pstr ")"; pspace(); pprint_exp e
        | ExpFor ({for_cl; for_body}, _) ->
            pcut(); (List.iter (fun pe_l -> pstr "FOR("; pspace();
                (List.iteri (fun i (p, e) -> if i = 0 then () else (pstr ","; pspace());
                pprint_pat p; pspace(); pstr "IN"; pspace(); pprint_exp e; pspace()) pe_l);
                pstr ")") for_cl);
            pcut(); pprint_exp for_body
        | ExpTryCatch(e, pe_l, _) -> pstr "TRY"; pspace(); pprint_exp e; pspace();
                                     pstr "CATCH"; pphandlers pe_l
        | ExpCast(e, t, _) -> pstr "("; obox(); pprint_exp e; pspace(); pstr ":>"; pspace(); pprint_type t; cbox(); pstr ")"
        | ExpTyped(e, t, _) -> pstr "("; obox(); pprint_exp e; pspace(); pstr ":"; pspace(); pprint_type t; cbox(); pstr ")"
        | ExpCCode(s, _) -> pstr "CCODE"; pspace(); pstr "\"\"\""; pstr s; pstr "\"\"\""
        | _ -> failwith "unknown exp"
        ); cbox();
and pprint_exp_as_seq e = match e with
    | ExpSeq(es, _) -> pprint_expseq es false
    | _ -> pprint_exp e
and pprint_expseq el need_braces =
    if need_braces then pstr "{" else (); pcut(); ovbox();
    (List.iter (fun e -> pprint_exp e; pstr ";"; pcut()) el); cvbox(); pcut();
    if need_braces then pstr "{" else ()
and pprint_pat p = match p with
    | PatAny(_) -> pstr "_"
    | PatIdent(n, _) -> pprint_id n
    | PatTuple(pl, _) -> pstr "("; obox();
        (List.iteri (fun i p -> if i = 0 then () else (pstr ","; pspace()); pprint_pat p) pl);
        cbox(); pstr ")"
    | PatCtor(n, elems, loc) -> pprint_id n; pprint_pat (PatTuple (elems, loc))
    | PatTyped(p, t, _) -> pprint_pat p; pstr ":"; pspace(); pprint_type t

let pprint_mod { dm_name; dm_filename; dm_defs; dm_deps } =
    Format.open_vbox 0;
    pstr dm_filename;
    (match dm_defs with
    | [] -> ()
    | _ -> pstr ":"; obox();
        pspace(); (List.iteri (fun i n -> if i = 0 then () else (pstr ","; pspace()); pprint_id n) dm_deps); cbox());
    pcut();
    pstr "---------------------------------------------------------"; pcut();
    (List.iter (fun e -> pprint_exp e; pstr ";"; pcut()) dm_defs);
    Format.close_box()
