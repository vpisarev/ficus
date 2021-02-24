(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    C-form pretty printer.

    Unlike Ast_pp or K_pp, the output of this module
    is not just free-form.

    It should output valid, and yet preferably
    well-formatted beautifully-looking C code.
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
let ovbox_brace () = Format.open_vbox (!base_indent-1)
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
    | COpPlus -> ("+", 1300, AssocRight)
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

let pprint_id2str n loc =
    let cname = get_idc_cname n loc in
    if cname = "" then (id2str__ n false "_" "_") else cname

let pprint_id n loc = pstr (pprint_id2str n loc)

let rec pprint_ctyp__ prefix0 t id_opt fwd_mode loc =
    let pr_id_opt_ add_space = match id_opt with
        | Some(i) -> if add_space then pspace() else (); pprint_id i loc
        | _ -> () in
    let pr_id_opt () = pr_id_opt_ true in
    let pr_struct prefix n_opt elems suffix =
        (ohbox();
        pstr (prefix ^ " ");
        (match n_opt with
        | Some n -> pprint_id n loc; pstr " "
        | _ -> ()
        );
        cbox(); pspace(); pstr "{"; ovbox_brace();
        List.iter (fun (ni, ti) -> pbreak();
            let need_nested_box = match ti with
                | CTypStruct _ | CTypUnion _ | CTypRawPtr(_, CTypStruct _) -> false
                | _ -> true in
            if need_nested_box then ohbox() else ();
            pprint_ctyp__ "" ti (Some ni) true loc; pstr ";";
            if need_nested_box then cbox() else ()) elems;
        cbox(); pbreak(); ohbox(); pstr ("} " ^ suffix); pr_id_opt_ false; cbox()) in
    (match t with
    | CTypInt | CTypCInt | CTypSize_t | CTypSInt _ | CTypUInt _ | CTypFloat _
    | CTypString | CTypUniChar | CTypBool | CTypExn | CTypCSmartPtr | CTypArray(_, _) ->
        let (cname, _) = ctyp2str t loc in
        pstr cname; pr_id_opt ()
    | CTypVoid -> pstr "void";
        (match id_opt with
        | Some i -> raise_compile_err loc (sprintf "c_pp.ml: void cannot be used with id '%s'" (id2str i))
        | _ -> ())
    | CTypFunRawPtr(args, rt) ->
        obox(); pprint_ctyp__ "" rt None true loc;
        pspace(); pstr "(*";
        pr_id_opt_ false; pstr ")("; pcut();
        obox();
        (match args with
        | [] -> pstr "void"
        | t :: [] -> pprint_ctyp_ t None loc
        | _ -> List.iteri (fun i ti -> if i = 0 then () else (pstr ","; pspace()); pprint_ctyp__ "" ti None true loc) args);
        cbox(); pstr ")"; cbox()
    | CTypStruct (n_opt, selems) -> pr_struct (prefix0 ^ "struct") n_opt selems ""
    | CTypRawPtr ([], CTypStruct(n_opt, selems)) ->
        let suffix = match n_opt with
            | Some n -> (pprint_id2str n loc) ^ ", *"
            | _ -> "*"
        in
        pr_struct (prefix0 ^ "struct") n_opt selems suffix
    | CTypUnion (n_opt, uelems) -> pr_struct (prefix0 ^ "union") n_opt uelems ""
    | CTypRawPtr (attrs, t) ->
        obox();
        if (List.mem CTypVolatile attrs) then pstr "volatile " else ();

        (* for now disable constant pointers to avoid warnings from C compilers *)
        (*if (List.mem CTypConst attrs) then pstr "const " else ();*)
        pprint_ctyp__ "" t None fwd_mode loc;
        pstr "*"; pr_id_opt();
        cbox()
    | CTypRawArray (attrs, et) ->
        obox();
        if (List.mem CTypVolatile attrs) then pstr "volatile " else ();
        if (List.mem CTypConst attrs) then pstr "const " else ();
        pprint_ctyp__ "" et None fwd_mode loc;
        pspace();
        pr_id_opt_ false; pstr "[]";
        cbox()
    | CTypName n ->
        (match (fwd_mode, n) with
        | (false, _) -> pprint_id n loc
        | (true, Id.Name _) -> pprint_id n loc
        | _ -> (match (cinfo_ n loc) with
            | CTyp {contents={ct_typ=CTypRawPtr(_, CTypStruct((Some struct_id), _))}} ->
                pstr "struct "; pprint_id struct_id loc; pstr "*"
            | _ ->
                if prefix0 <> "" then (pstr prefix0; pspace()) else ();
                pprint_id n loc));
        pr_id_opt()
    | CTypLabel -> pstr "/*<label>*/"; pr_id_opt()
    | CTypAny -> pstr "void"; pr_id_opt())

and pprint_ctyp_ t id_opt loc = pprint_ctyp__ "" t id_opt false loc

and pprint_cexp_ e pr =
    match e with
    | CExpIdent(i, (_, loc)) -> pprint_id i loc
    | CExpLit(l, (_, loc)) ->
        let s = match l with
        | LitNil -> "0"
        | LitChar c -> "FX_CHAR('" ^ (Utils.escaped_uni c) ^ "')"
        | _ -> lit2str l true loc in
        pstr s
    | CExpBinOp (COpArrayElem as bop, a, b, _) ->
        let (_, pr0, _) = binop2str_ bop in
        obox(); pprint_cexp_ a pr0; pstr "["; pcut();
        pprint_cexp_ b 0; pcut(); pstr "]"; cbox()
    | CExpBinOp (COpMacroConcat, a, b, _) ->
        pprint_cexp_ a 0; pstr "##"; pprint_cexp_ b 0
    | CExpBinOp (bop, a, b, _) ->
        let (bop_str, pr0, assoc) = binop2str_ bop in
        obox(); if pr0 < pr then (pstr "("; pcut()) else ();
        let is_shift = bop = COpShiftLeft || bop = COpShiftRight in
        let a_pr = if is_shift then 1350 else if assoc=AssocLeft then pr0 else pr0+1 in
        let b_pr = if is_shift then 1350 else if assoc=AssocRight then pr0 else pr0+1 in
        pprint_cexp_ a a_pr;
        pspace(); pstr bop_str; pspace();
        pprint_cexp_ b b_pr;
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
    | CExpMem(e, m, (_, loc)) ->
        pprint_cexp_ e 1400; pstr "."; pprint_id m loc
    | CExpArrow(e, m, (_, loc)) ->
        pprint_cexp_ e 1400; pstr "->"; pprint_id m loc
    | CExpCast(e, t, loc) ->
        obox(); pstr "("; pprint_ctyp_ t None loc; pstr ")"; pcut(); pprint_cexp_ e 1301; cbox()
    | CExpTernary (e1, e2, e3, _) ->
        let pr0 = 200 in
        obox(); (if pr0 < pr then (pstr "("; pcut()) else ());
        pprint_cexp_ e1 0; pspace(); pstr "?"; pspace();
        pprint_cexp_ e2 0; pspace(); pstr ":"; pspace();
        pprint_cexp_ e3 0;
        (if pr0 < pr then (pcut(); pstr ")") else ()); cbox()
    | CExpCall(f, args, _) ->
        obox(); pprint_cexp_ f 1400; pstr "("; pcut(); pprint_elist args; pstr ")"; cbox()
    | CExpInit(eseq, _) ->
        obox();
        pstr "{"; pcut();
        (if eseq=[] then ()
        else
            List.iteri (fun i e ->
                if i = 0 then pcut() else (pstr ","; pspace());
                pprint_cexp_ e 0) eseq);
        pcut(); pstr "}";
        cbox()
    | CExpTyp(t, loc) ->
        obox();
        pprint_ctyp_ t None loc;
        cbox()
    | CExpCCode (ccode, l) ->
        obox(); pstr ccode; cbox()

and pprint_elist el =
    (obox();
    List.iteri (fun i e ->
        if i = 0 then pcut() else (pstr ","; pspace());
        pprint_cexp_ e 0) el; cbox())

and pprint_fun_hdr fname semicolon loc fwd_mode =
    let { cf_args; cf_rt; cf_cname; cf_body; cf_flags; cf_loc } =
        match (cinfo_ fname loc) with
        | CFun cf -> !cf
        | _ -> raise_compile_err loc (sprintf "the forward declaration of %s does not reference a function" (pp_id2str fname))
    in
    obox();
    if cf_flags.fun_flag_private then pstr "static " else pstr "FX_EXTERN_C ";
    (* if all the calls of an inline function were expanded — good;
       if not, it will still be called from the module where it is defined.
       So, we should not declare it as inline at C/C++ level *)
    (*if List.mem FunInline cf_flags then pstr "inline " else ();*)
    pprint_ctyp_ cf_rt None cf_loc;
    pspace();
    pstr cf_cname;
    pstr "(";
    pcut();
    (match cf_args with
    | [] -> pstr "void"; pcut()
    | _ ->
        Format.open_vbox 0;
        List.iteri (fun i (n, t, _) ->
            if i = 0 then () else (pstr ","; pspace());
            ohbox(); pprint_ctyp__ "" t (Some n) true cf_loc; cbox()) cf_args;
        cbox());
    pstr (")" ^ (if semicolon then ";" else "")); cbox();
    pbreak()

and pprint_cstmt_or_block_cbox s =
    match s with
    | CStmtBlock (sl, _) ->
        pstr "{"; pbreak(); ohvbox();
        List.iteri (fun i s -> if i = 0 then () else pbreak(); pprint_cstmt s) sl;
        cbox(); cbox(); pbreak(); pstr "}"
    | _ -> pprint_cstmt s; cbox()

and pprint_cstmt s =
    (match s with
    | CStmtNop _ -> pstr "{}"
    | CComment (s, _) -> pstr s
    | CExp e ->
        pprint_cexp_ e 0;
        (match e with
        | CExpCCode _ -> ()
        | _ -> pstr ";")
    | CStmtBreak _ -> pstr "break;"
    | CStmtContinue _ -> pstr "continue;"
    | CStmtReturn (e_opt, l) -> obox(); pstr "return";
        (match e_opt with
        | Some e -> pspace(); pprint_cexp_ e 0
        | _ -> ());
        pstr ";";
        cbox()
    | CStmtBlock (sl, _) ->
        (match sl with
        | [] -> pstr "{}"
        | s :: [] -> pprint_cstmt s
        | _ ->  pstr "{"; pbreak(); ohvbox();
            List.iteri (fun i s -> if i = 0 then () else pbreak(); pprint_cstmt s) sl;
            cbox(); pbreak(); pstr "}")
    | CStmtIf (e, s1, s2, _) ->
        let rec print_cascade_if prefix e s1 s2 =
            obox();
            pstr (prefix ^ " ("); pprint_cexp_ e 0; pstr ")";
            pspace();
            pprint_cstmt_or_block_cbox s1;
            (match s2 with
            | CStmtNop _ | CStmtBlock ([], _) -> ()
            | CStmtIf(e_, s1_, s2_, _) -> pspace(); print_cascade_if "else if" e_ s1_ s2_
            | _ -> pspace(); obox(); pstr "else"; pspace(); pprint_cstmt_or_block_cbox s2)
            in
        print_cascade_if "if" e s1 s2
    | CStmtGoto(n, loc) -> obox(); pstr "goto"; pspace(); pprint_id n loc; pstr ";"; cbox()
    | CStmtLabel (n, loc) ->
        Format.print_break 1 (- !base_indent);
        pprint_id n loc; pstr ": ;"
    | CStmtFor(t_opt, e1, e2_opt, e3, body, loc) ->
        obox();
        pstr "for (";
        pcut();
        (match e1 with
        | [] -> ()
        | _ ->
            (match t_opt with
            | Some t -> pprint_ctyp_ t None loc; pspace()
            | _ -> ());
            pprint_elist e1);
        pstr ";";
        (match e2_opt with
        | Some e2 -> pspace(); pprint_cexp_ e2 0
        | _ -> ());
        pstr ";";
        (match e3 with
        | [] -> ()
        | _ -> pspace(); pprint_elist e3);
        pcut();
        pstr ")"; pspace();
        pprint_cstmt_or_block_cbox body
    | CStmtWhile (e, body, _) ->
        obox();
        pstr "while ("; pcut(); pprint_cexp_ e 0; pcut(); pstr ")";
        pspace(); pprint_cstmt_or_block_cbox body
    | CStmtDoWhile (body, e, _) ->
        obox(); pstr "do"; pprint_cstmt_or_block_cbox body;
        obox(); pstr "while ("; pcut(); pprint_cexp_ e 0; pcut(); pstr ");"; cbox()
    | CStmtSwitch (e, cases, _) ->
        ohbox(); pstr "switch ("; obox(); pprint_cexp_ e 0; cbox();
        pstr ") {"; cbox(); pbreak();
        List.iter (fun (labels, code) ->
            ohbox();
            let isdefault = (match labels with
            | [] -> pstr "default:"; true
            | _ ->
                List.iter (fun l -> pstr "case "; pprint_cexp_ l 0; pstr ":"; pspace()) labels; false) in
            cbox();
            pbreak();
            ovbox();
            let codelen = (List.length code) + (if isdefault then 0 else 1) in
            List.iteri (fun i s -> if i = 0 then pstr "   " else (); pprint_cstmt s; if i < codelen-1 then pbreak() else ()) code;
            if isdefault then (if code=[] then pstr "   ;" else ()) else pstr "break;";
            cbox();
            pbreak()
            ) cases;
        pstr "}"
    | CDefVal (t, n, e_opt, loc) ->
        let is_private = match (cinfo_ n loc) with CVal {cv_flags} -> cv_flags.val_flag_private | _ -> false in
        obox();
        if is_private then (pstr "static"; pspace()) else ();
        pprint_ctyp_ t (Some n) loc;
        (match e_opt with
        | Some e -> pspace(); pstr "="; pspace(); pprint_cexp_ e 0
        | _ -> ());
        pstr ";";
        cbox()
    | CDefFun cf ->
        let { cf_name; cf_body; cf_loc } = !cf in
        pprint_fun_hdr cf_name false cf_loc false;
        ovbox(); pstr "{";
        List.iter (fun s -> pbreak(); pprint_cstmt s) cf_body;
        cbox(); pbreak(); pstr "}"; pbreak()
    | CDefForwardSym (cf_name, cf_loc) ->
        (match (cinfo_ cf_name cf_loc) with
        | CFun _ -> pprint_fun_hdr cf_name true cf_loc true
        | CVal {cv_typ} ->
            ohbox();
            pstr "FX_EXTERN_C_VAL("; pcut();
            pprint_ctyp__ "" cv_typ (Some cf_name) true cf_loc;
            pstr ")";
            cbox()
        | _ ->
            raise_compile_err cf_loc (sprintf "the forward declaration of %s does not reference a function or a value" (pp_id2str cf_name)))
    | CDefTyp ct ->
        let { ct_name; ct_typ; ct_loc } = !ct in
        pprint_ctyp__ "typedef " ct_typ (Some ct_name) true ct_loc; pstr ";"; pbreak()
    | CDefForwardTyp (n, loc) ->
        pstr "struct "; pprint_id n loc; pstr ";"
    | CDefEnum ce ->
        let { cenum_cname; cenum_members; cenum_loc } = !ce in
        ohbox();
        pstr "typedef enum {";
        cbox();
        pbreak();
        ovbox();
        List.iteri (fun i (n, e_opt) ->
            if i = 0 then pstr "   " else (pstr ","; pspace());
            pprint_id n cenum_loc;
            match e_opt with
            | Some e -> pstr "="; pprint_cexp_ e 0
            | _ -> ()) cenum_members;
        cbox(); pbreak(); pstr "} "; pstr cenum_cname; pstr ";"; pbreak()
    | CMacroDef cm ->
        let {cm_cname; cm_args; cm_body; cm_loc} = !cm in
        pstr "#define "; pstr cm_cname;
        (match cm_args with
        | [] -> ()
        | _ ->
            pstr "(";
            List.iteri (fun i a ->
                if i = 0 then () else pstr ", ";
                pprint_id a cm_loc) cm_args;
            pstr ")");
        (match cm_body with
        | [] -> ()
        | _ -> List.iter (fun s ->
            pspace();
            pstr "\\";
            pbreak();
            pstr "    ";
            pprint_cstmt s) cm_body)
    | CMacroUndef (n, loc) -> ohbox(); pstr "#undef "; pprint_id n loc; cbox()
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
        ohbox(); pstr "#endif"
    | CMacroInclude (s, _) -> pbreak(); ohbox(); pstr "#include "; pstr s; cbox()
    | CMacroPragma (s, _) -> pbreak(); ohbox(); pstr "#pragma "; pstr s; cbox())

let pprint_ctyp_x t loc = Format.print_flush (); Format.open_box 0; pprint_ctyp_ t None loc; Format.close_box(); Format.print_flush ()
let pprint_cexp_x e = Format.print_flush (); Format.open_box 0; pprint_cexp_ e 0; Format.close_box(); Format.print_flush ()
let pprint_cstmt_x s = Format.print_flush (); Format.open_box 0; pprint_cstmt s; Format.close_box(); Format.print_flush ()
let pprint_top code = Format.print_flush (); Format.open_vbox 0; List.iter (fun s -> pprint_cstmt s; pbreak()) code; Format.close_box(); pbreak(); Format.print_flush ()
let pprint_top_to_file filename code =
    (let outch = open_out filename in
    Format.set_formatter_out_channel outch;
    pprint_top code;
    Format.print_flush();
    Format.set_formatter_out_channel stdout;
    close_out outch;
    true)
let pprint_top_to_string code =
    (let (std_print, std_flush) = Format.get_formatter_output_functions () in
    let all_lines = ref ([]: string list) in
    let str_print s p n = all_lines := (String.sub s p n) :: !all_lines in
    let str_flush () = () in
    Format.set_formatter_output_functions str_print str_flush;
    pprint_top code;
    Format.print_flush();
    Format.set_formatter_output_functions std_print std_flush;
    let rec remove_trailing_empty_lines all_lines =
        match all_lines with
        | "" :: rest | "\n" :: rest -> remove_trailing_empty_lines rest
        | l::rest -> (if (Utils.ends_with l "\n") then l else l ^ "\n") :: rest
        | _ -> all_lines
        in
    String.concat "" (List.rev (remove_trailing_empty_lines !all_lines)))
