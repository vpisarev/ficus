(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

open Ast
open C_form

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
let ohbox () = Format.open_hbox ()

type assoc_t = AssocLeft | AssocRight

let binop2str_ bop = match bop with
    | COpArrayElem -> ("", 1400, AssocLeft)
    | COpMul -> ("*", 1200, AssocLeft)
    | COpDiv -> ("/", 1200, AssocLeft)
    | COpMod -> ("%", 1200, AssocLeft)
    | COpAdd -> ("+", 1100, AssocLeft)
    | COpSub -> ("-", 1100, AssocLeft)
    | COpShiftLeft -> ("<<", 1000, AssocLeft)
    | COpShiftRight -> (">>", 1000, AssocLeft)
    | COpCompareLT -> ("<", 900, AssocLeft)
    | COpCompareLE -> ("<=", 900, AssocLeft)
    | COpCompareGT -> (">", 900, AssocLeft)
    | COpCompareGE -> (">=", 900, AssocLeft)
    | COpCompareEQ -> ("==", 800, AssocLeft)
    | COpCompareNE -> ("!=", 800, AssocLeft)
    | COpBitwiseAnd -> ("&", 700, AssocLeft)
    | COpBitwiseXor -> ("^", 600, AssocLeft)
    | COpBitwiseOr -> ("|", 500, AssocLeft)
    | COpLogicAnd -> ("&&", 400, AssocLeft)
    | COpLogicOr -> ("||", 300, AssocLeft)
    (* 200 for ternary operation *)
    | COpAssign -> ("=", 100, AssocRight)
    | COpAugAdd -> ("+=", 100, AssocRight)
    | COpAugSub -> ("-=", 100, AssocRight)
    | COpAugMul -> ("*=", 100, AssocRight)
    | COpAugDiv -> ("/=", 100, AssocRight)
    | COpAugMod -> ("%=", 100, AssocRight)
    | COpAugSHL -> ("<<=", 100, AssocRight)
    | COpAugSHR -> (">>=", 100, AssocRight)
    | COpAugBitwiseAnd -> ("&=", 100, AssocRight)
    | COpAugBitwiseOr -> ("|=", 100, AssocRight)
    | COpAugBitwiseXor -> ("^=", 100, AssocRight)
    | COpMacroConcat -> ("##", 1500, AssocLeft)

let unop2str_ uop = match uop with
    | COpNegate -> ("-", 1300, AssocRight)
    | COpBitwiseNot -> ("~", 1300, AssocRight)
    | COpLogicNot -> ("!", 1300, AssocRight)
    | COpDeref -> ("*", 1300, AssocRight)
    | COpGetAddr -> ("&", 1300, AssocRight)
    | COpPrefixInc -> ("++", 1300, AssocRight)
    | COpPrefixDec -> ("--", 1300, AssocRight)
    | COpSuffixInc -> ("++", 1400, AssocLeft)
    | COpSuffixDec -> ("--", 1400, AssocLeft)
    | COpMacroName -> ("#", 1600, AssocLeft)
    | COpMacroDefined -> ("defined ", 1600, AssocLeft)

let pprint_id n =
    let cname = get_idc_cname n in
    pstr (if cname = "" then (pp_id2str n) else cname)

let rec pprint_ctyp_ t id_opt =
    let pr_id_opt_ add_space = match id_opt with
        | Some(i) -> if add_space then pspace() else (); pprint_id i
        | _ -> () in
    let pr_id_opt () = pr_id_opt_ true in
    match t with
    | CTypInt -> pstr "int_"; pr_id_opt ()
    | CTypCInt -> pstr "int"; pr_id_opt ()
    | CTypSize_t -> pstr "size_t"; pr_id_opt ()
    | CTypSInt(b) -> pstr ("int" ^ (string_of_int b) ^ "_t"); pr_id_opt ()
    | CTypUInt(b) -> pstr ("unt" ^ (string_of_int b) ^ "_t"); pr_id_opt ()
    | CTypFloat(16) -> pstr "float16_t"; pr_id_opt ()
    | CTypFloat(32) -> pstr "float"; pr_id_opt ()
    | CTypFloat(64) -> pstr "double"; pr_id_opt ()
    | CTypFloat(b) -> failwith (sprintf "invalid type CTypFloat(%d)" b)
    | CTypString -> pstr "fx_str_t"; pr_id_opt ()
    | CTypWChar -> pstr "char_"; pr_id_opt ()
    | CTypBool -> pstr "bool_"; pr_id_opt ()
    | CTypVoid -> pstr "void";
        (match id_opt with
        | Some _ -> raise_compile_err noloc "c_pp.ml: void cannot be used with id"
        | _ -> ())
    | CTypNil -> pstr "nil_t"; pr_id_opt ()
    | CTypExn -> pstr "fx_exn_t"; pr_id_opt ()
    | CTypFun(args, rt) ->
        obox(); pprint_ctyp_ rt None;
        pspace(); pstr "(*";
        pr_id_opt_ false; pstr ")(";
        obox();
        (match args with
        | [] -> pstr "void"
        | t :: [] -> pprint_ctyp_ t None
        | _ -> List.iteri (fun i ti -> if i = 0 then () else (pstr ","; pspace()); pprint_ctyp_ ti None) args);
        cbox(); pstr ")"; cbox()
    | CTypFunRawPtr(args, rt) ->
        obox(); pprint_ctyp_ rt None;
        pspace(); pstr "(*";
        pr_id_opt_ false; pstr ")(";
        obox();
        (match args with
        | [] -> pstr "void"
        | t :: [] -> pprint_ctyp_ t None
        | _ -> List.iteri (fun i ti -> if i = 0 then () else (pstr ","; pspace()); pprint_ctyp_ ti None) args);
        cbox(); pstr ")"; cbox()
    | CTypCSmartPtr -> pstr "fx_cptr_t"; pr_id_opt ()
    | CTypStruct (n_opt, selems) ->
        obox(); pstr "struct ";
        (match n_opt with
        | Some n -> pprint_id n; pstr " "
        | _ -> ()
        );
        pstr "{"; ovbox(); pspace();
        List.iter (fun (ni, ti) -> pprint_ctyp_ ti (Some ni); pstr ";"; pspace()) selems;
        cbox(); pstr "}"; cbox(); pr_id_opt()
    | CTypUnion (n_opt, uelems) ->
        obox(); pstr "union ";
        (match n_opt with
        | Some n -> pprint_id n; pstr " "
        | _ -> ()
        );
        pstr "{"; ovbox(); pspace();
        List.iter (fun (ni, ti) -> pprint_ctyp_ ti (Some ni); pstr ";"; pspace()) uelems;
        cbox(); pstr "}"; cbox(); pr_id_opt()
    | CTypRawPtr (attrs, t) ->
        obox();
        if (List.mem CTypVolatile attrs) then pstr "volatile " else ();
        if (List.mem CTypConst attrs) then pstr "const " else ();
        pprint_ctyp_ t None;
        pstr "*"; pr_id_opt();
        cbox()
    | CTypArray (d, _) -> pstr (sprintf "fx_arr%d_t" d)
    | CTypName n -> pprint_id n; pr_id_opt()
    | CTypCName n -> pstr (pp_id2str n); pr_id_opt()
    | CTypLabel -> pstr "/*<label>*/"; pr_id_opt()
    | CTypAny -> pstr "void"; pr_id_opt()

and pprint_cexp_ e pr =
    match e with
    | CExpIdent(i, _) -> pprint_id i
    | CExpLit(l, _) -> pstr (Ast_pp.lit_to_string l)
    | CExpBinOp (COpArrayElem as bop, a, b, _) ->
        let (_, pr0, _) = binop2str_ bop in
        obox(); pprint_cexp_ a pr0; pstr "["; pcut();
        pprint_cexp_ b 0; pcut(); pstr "]"; cbox()
    | CExpBinOp (COpMacroConcat, a, b, _) ->
        pprint_cexp_ a 0; pstr "##"; pprint_cexp_ b 0
    | CExpBinOp (bop, a, b, _) ->
        let (bop_str, pr0, assoc) = binop2str_ bop in
        obox(); if pr0 < pr then (pstr "("; pcut()) else ();
        pprint_cexp_ a (if assoc=AssocLeft then pr0 else pr0+1);
        pspace(); pstr bop_str; pspace();
        pprint_cexp_ b (if assoc=AssocRight then pr0 else pr0+1);
        if pr0 < pr then (pcut(); pstr ")") else (); cbox()
    | CExpUnOp (uop, e, _) ->
        let (uop_str, pr0, _) = unop2str_ uop in
        obox(); if pr0 < pr then (pstr "("; pcut()) else ();
        (match uop with
        | COpSuffixInc | COpSuffixDec ->
            pprint_cexp_ e pr0;
            pstr uop_str
        | _ ->
            pstr uop_str; pprint_cexp_ e pr0);
        if pr0 < pr then (pcut(); pstr ")") else (); cbox()
    | CExpMem(e, m, _) ->
        pprint_cexp_ e 1400; pstr "."; pprint_id m
    | CExpArrow(e, m, _) ->
        pprint_cexp_ e 1400; pstr "->"; pprint_id m
    | CExpCast(e, t, _) ->
        obox(); pstr "("; pprint_ctyp_ t None; pstr ")"; pcut(); pprint_cexp_ e 1301; cbox()
    | CExpTernary (e1, e2, e3, _) ->
        let pr0 = 200 in
        obox(); (if pr0 < pr then (pstr "("; pcut()) else ());
        pprint_cexp_ e1 0; pspace(); pstr "?"; pspace();
        pprint_cexp_ e2 0; pspace(); pstr ":"; pspace();
        pprint_cexp_ e3 0;
        (if pr0 < pr then (pcut(); pstr ")") else ()); cbox()
    | CExpCall(f, args, _) ->
        obox(); pprint_cexp_ f 1400; pstr "("; pprint_elist args; pstr ")"; cbox()
    | CExpSeq(eseq, _) ->
        obox();
        List.iteri (fun i e ->
            if i = 0 then pcut() else (pstr ";"; pspace());
            pprint_cexp_ e 0) eseq;
        cbox()

and pprint_elist el =
    (obox();
    List.iteri (fun i e ->
        if i = 0 then pcut() else (pstr ","; pspace());
        pprint_cexp_ e 0) el; cbox())

and pprint_fun_hdr fname semicolon loc =
    let { cf_name; cf_typ; cf_args; cf_body; cf_flags; cf_loc } = match (cinfo fname) with
        | CFun cf -> !cf
        | _ -> raise_compile_err loc (sprintf "the forward declaration of %s does not reference a function" (pp_id2str fname))
    in
    let (argtyps, rt) = (match cf_typ with
        | CTypFun(argtyps, rt) -> argtyps, rt
        | _ ->
            raise_compile_err cf_loc "invalid function type (should be a function)") in
    let typed_args = Utils.zip cf_args argtyps in
    obox();
    if List.mem FunStatic cf_flags then pstr "static " else ();
    if List.mem FunInline cf_flags then pstr "inline " else ();
    pprint_ctyp_ rt None;
    pspace();
    pprint_id cf_name;
    pstr "(";
    obox();
    List.iteri (fun i (n, t) ->
        if i = 0 then () else (pstr ","; pspace());
        pprint_ctyp_ t (Some n)) typed_args;
    cbox(); pstr (")" ^ (if semicolon then ";" else "")); cbox();
    pbreak()

and pprint_cstmt s =
    obox();
    (match s with
    | CStmtNop _ -> pstr "{}"
    | CComment (s, _) -> pstr s
    | CExp e -> pprint_cexp_ e 0; pstr ";"
    | CStmtBreak _ -> pstr "break;"
    | CStmtContinue _ -> pstr "continue;"
    | CStmtReturn (e_opt, l) -> pstr "return";
        (match e_opt with
        | Some e -> pspace(); pprint_cexp_ e 0
        | _ -> ());
        pstr ";"
    | CStmtBlock (sl, _) ->
        (match sl with
        | [] -> pstr "{}"
        | s :: [] -> pprint_cstmt s
        | _ ->  pstr "{"; ovbox();
            List.iter (fun s -> pprint_cstmt s) sl;
            cbox(); pstr "}")
    | CStmtIf (e, s1, s2, _) ->
        pstr "if ("; pprint_cexp_ e 0; pstr ")";
        obox(); pprint_cstmt s1; cbox();
        (match s1 with
        | CStmtNop _ | CStmtBlock ([], _) -> ()
        | _ -> pstr "else"; obox(); pprint_cstmt s1; cbox())
    | CStmtGoto(n, _) -> pstr "goto "; pprint_id n
    | CStmtLabel (n, _) -> pbreak(); pprint_id n; pstr ":"
    | CStmtFor(e1, e2_opt, e3, body, _) ->
        pstr "for (";
        (match e1 with
        | [] -> ();
        | _ -> pprint_elist e1);
        pstr ";";
        (match e2_opt with
        | Some e2 -> pspace(); pprint_cexp_ e2 0
        | _ -> ());
        pstr ";";
        (match e3 with
        | [] -> ();
        | _ -> pprint_elist e3);
        pstr ")";
        obox(); pprint_cstmt body; cbox()
    | CStmtWhile (e, body, _) ->
        pstr "while ("; pprint_cexp_ e 0; pstr ")"; obox(); pspace(); pprint_cstmt body; cbox()
    | CStmtDoWhile (body, e, _) ->
        pstr "do"; obox(); pspace(); pprint_cstmt body; pspace(); cbox();
        pstr "while ("; pprint_cexp_ e 0; pstr ");"
    | CStmtCCode (ccode, l) -> pstr ccode; pstr ";"
    | CDefVal (t, n, e_opt, l) ->
        pprint_ctyp_ t (Some n);
        (match e_opt with
        | Some e -> pspace(); pstr "="; pspace(); pprint_cexp_ e 0
        | _ -> ());
        pstr ";"
    | CDefFun cf ->
        let { cf_name; cf_typ; cf_args; cf_body; cf_flags; cf_loc } = !cf in
        pprint_fun_hdr cf_name false cf_loc;
        obox(); pstr "{"; ovbox(); pbreak();
        List.iter (fun s -> pprint_cstmt s; pbreak()) cf_body; pbreak();
        cbox(); pstr "}"; cbox();
        pbreak()
    | CDefForwardFun (cf_name, cf_loc) ->
        pprint_fun_hdr cf_name true cf_loc
    | CDefTyp ct ->
        let { ct_name; ct_typ } = !ct in
        pstr "typedef "; pprint_ctyp_ ct_typ (Some ct_name); pstr ";"
    | CDefForwardTyp (n, _) ->
        pstr "struct "; pprint_id n; pstr ";"
    | CDefEnum ce ->
        let { ce_name; ce_members } = !ce in
        pstr "typedef enum {";
        pbreak();
        obox();
        List.iteri (fun i (n, e_opt) ->
            if i = 0 then () else (pstr ","; pspace());
            pprint_id n;
            match e_opt with
            | Some e -> pstr "="; pprint_cexp_ e 0
            | _ -> ()) ce_members;
        cbox(); pbreak(); pstr "} "; pprint_id ce_name; pstr ";";
        pbreak()
    | CMacroDef cm ->
        let {cm_name=n; cm_args=args; cm_body=body} = !cm in
        pstr "#define "; pprint_id n;
        (match args with
        | [] -> ()
        | _ ->
            pstr "(";
            List.iteri (fun i a ->
                if i = 0 then () else pstr ", ";
                pprint_id a) args;
            pstr ")");
        (match body with
        | [] -> ()
        | _ -> List.iter (fun s ->
            pspace();
            pstr "\\";
            pbreak();
            pstr "    ";
            pprint_cstmt s) body)
    | CMacroUndef (n, _) -> ohbox(); pstr "#undef "; pprint_id n; cbox()
    | CMacroIf (cs_l, else_l, _) ->
        List.iteri (fun i (c, sl) ->
            pbreak(); ohbox();
            pstr (if i = 0 then "#if " else "#elif "); pprint_cexp_ c 0;
            cbox(); pbreak();
            List.iter (fun s -> pprint_cstmt s) sl) cs_l;
        (match else_l with
        | [] -> ()
        | _ -> pbreak(); ohbox(); pstr "#else"; cbox(); pbreak();
            List.iter (fun s -> pprint_cstmt s) else_l);
        pbreak();
        ohbox(); pstr "#endif";
        pbreak()
    | CMacroInclude (s, _) -> pbreak(); ohbox(); pstr "#include "; pstr s; cbox(); pbreak());
    cbox()

let pprint_ktyp_x t = Format.print_flush (); Format.open_box 0; pprint_ctyp_ t None; Format.close_box(); Format.print_flush ()
let pprint_cexp_x e = Format.print_flush (); Format.open_box 0; pprint_cexp_ e 0; Format.close_box(); Format.print_flush ()
let pprint_cstmt_x s = Format.print_flush (); Format.open_box 0; pprint_cstmt s; Format.close_box(); Format.print_flush ()
let pprint_top code = Format.print_flush (); Format.open_vbox 0; List.iter pprint_cstmt code; Format.close_box(); pbreak(); Format.print_flush ()
