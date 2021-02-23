(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* Ocaml => Ficus pretty printer. *)

open Syntax

let base_indent = ref 4

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
let highest_unary_pr = 1700

let binop2str_ bop = match bop with
    | COpAt -> ("", highest_unary_pr, AssocLeft)
    | COpMul -> ("*", 1200, AssocLeft)
    | COpDiv -> ("/", 1200, AssocLeft)
    | COpMod -> ("%", 1200, AssocLeft)
    | COpAdd -> ("+", 1100, AssocLeft)
    | COpSub -> ("-", 1100, AssocLeft)
    | COpSHL -> ("<<", 1000, AssocLeft)
    | COpSHR -> (">>", 1000, AssocLeft)
    | COpLT -> ("<", 900, AssocLeft)
    | COpLE -> ("<=", 900, AssocLeft)
    | COpGT -> (">", 900, AssocLeft)
    | COpGE -> (">=", 900, AssocLeft)
    | COpEQ -> ("==", 800, AssocLeft)
    | COpNE -> ("!=", 800, AssocLeft)
    | COpCons -> ("::", 950, AssocRight)
    | COpLogicAnd -> ("&&", 400, AssocLeft)
    | COpLogicOr -> ("||", 300, AssocLeft)
    | COpConcat -> ("+", 1100, AssocLeft)
    | COpAssign -> ("=", 100, AssocRight)

let unop2str_ uop = match uop with
    | COpNeg -> ("-", 1300, AssocRight)
    | COpNot -> ("!", 1300, AssocRight)
    | COpDeref -> ("*", 1300, AssocRight)
    | COpMkRef -> ("ref", 1600, AssocRight)

let pprint_id2str n = n
let pprint_id n = pstr (pprint_id2str n)

let rec pprint_octyp_ t fparen =
    (match t with
    | CtInt | CtFloat | CtString | CtChar | CtBool | CtUnit -> pstr (octyp2str t)
    | CtList(t) | CtRef(t) ->
        let n = match t with CtList _ -> "list" | _ -> "ref" in
        obox(); pprint_octyp_ t true; pspace(); pstr n; cbox()
    | CtTuple(tl) ->
        obox(); pstr "("; pcut(); List.iteri (fun i t -> if i = 0 then () else (pstr ","; pspace());
          pprint_octyp t) tl; pprint_octyp t; pcut(); pstr ")"; cbox()
    | CtName(n) -> pstr n
    | CtFun(arg, rt) ->
        obox();
        if fparen then (pstr "("; pcut()) else ();
        pprint_octyp_ arg true;
        pspace(); pstr "->"; pspace();
        pprint_octyp_ rt false;
        if fparen then (pcut(); pstr ")") else ();
        cbox()
    | CtRecord(relems) ->
        obox(); pstr "{"; pcut();
        List.iteri (fun i (m, n, t) ->
            let t = if m then CtRef(t) else t in
            if i = 0 then () else (pstr ";"; pspace());
            pstr n; pstr ":"; pspace(); pprint_octyp t) relems;
        pcut(); pstr "}"; cbox()
    | CtApp(args, n) ->
        obox();
        let argt = match args with t :: [] -> t | _ -> CtTuple(args) in
        pprint_octyp_ argt true;
        pspace();
        pstr n;
        cbox())

and pprint_octyp t = pprint_octyp_ t false

and pprint_pat_ p parens =
    let oboxp () = obox(); if parens then (pstr "("; pcut()) else () in
    let cboxp () = if parens then (pcut(); pstr ")") else (); cbox() in
    match p with
    | CpLit(l) -> pstr (lit2str l)
    | CpIdent(n) -> pstr n
    | CpAny -> pstr "<Any>"
    | CpTuple(pl) ->
        obox(); pstr "("; pcut();
        List.iteri (fun i p ->
            if i = 0 then () else (pstr ","; pspace());
            pprint_pat_ p false) pl;
        pcut(); pstr ")";
        cbox()
    | CpVariant(vn, pl) ->
        obox(); pstr vn; pspace();
        let (p, need_parens) =
            match pl with
            | CpLit _ :: [] | CpIdent _ :: [] | CpAny :: [] -> ((List.hd pl), false)
            | p :: [] -> (p, true)
            | _ -> (CpTuple(pl), false)
            in
        if need_parens then (pstr "("; pcut()) else ();
        pprint_pat_ p false;
        if need_parens then (pcut(); pstr ")") else ();
        cbox()
    | CpRecord (vn, relems) ->
        obox(); if vn = noid then () else (pstr vn; pspace()); pstr "{"; pcut();
        List.iteri (fun i (n, p) ->
            if i = 0 then () else (pstr ","; pspace());
            if n = "contents" then
                (pstr "ref"; pspace(); pprint_pat_ p true)
            else
              (pstr "n";
              match p with
              | CpIdent(n1) when n1=n -> ()
              | _ -> pstr "="; pcut(); pprint_pat_ p false)
        ) relems;
        pcut(); pstr "}"; cbox()
    | CpCons(p1, p2) ->
        oboxp();
        pprint_pat_ p1 true;
        pspace(); pstr "::"; pspace();
        pprint_pat_ p2 (match p2 with CpCons _ -> false | _ -> true);
        cboxp()
    | CpAs(p, n) ->
        oboxp();
        pprint_pat_ p true;
        pspace(); pstr ("as " ^ n);
        cboxp();
    | CpWhen(p, e) ->
        oboxp();
        pprint_pat_ p true;
        pspace();
        pprint_ocexp_ e 0;
        cboxp()
    | CpTyped(p, t) ->
        oboxp();
        pprint_pat_ p true;
        pstr ":"; pspace();
        pprint_octyp t;
        cboxp()

and check_if_none p e_opt =
    match e_opt with
    | None -> ()
    | _ -> failwith (sprintf "met 'let %s= ... in' with non-empty 'in' part" (pat2str p))

and pprint_ocexp_ e pr : unit =
    match e with
    | CeLit(lit) -> pstr (lit2str lit)
    | CeIdent(i) -> pstr i
    | CeUnary(uop, e) ->
        let (uop_str, pr0, _) = unop2str_ uop in
        obox(); if pr0 < pr then (pstr "("; pcut()) else ();
        pstr uop_str; pprint_ocexp_ e pr0;
        if pr0 < pr then (pcut(); pstr ")") else (); cbox()
    | CeBinary(bop, e1, e2) ->
        let (bop_str, pr0, assoc) = binop2str_ bop in
        obox(); if pr0 < pr then (pstr "("; pcut()) else ();
        let is_shift = match bop with COpSHL | COpSHR -> true | _ -> false in
        let pr1 = if is_shift then 1350 else if assoc=AssocLeft then pr0 else pr0+1 in
        let pr2 = if is_shift then 1350 else if assoc=AssocRight then pr0 else pr0+1 in
        pprint_ocexp_ e1 pr1;
        pspace(); pstr bop_str; pspace();
        pprint_ocexp_ e2 pr2;
        if pr0 < pr then (pcut(); pstr ")") else (); cbox()
    | CeIf(e, e1, e2) ->
        let rec print_cascade_if prefix e e1 e2 =
            obox();
            pstr prefix; pspace(); pprint_ocexp_ e 0; pspace();
            pprint_block_cbox e1;
            (match e2 with
            | CeLit(ClUnit) -> ()
            | CeIf(e_, e1_, e2_) -> pspace(); obox(); print_cascade_if "else if" e_ e1_ e2_
            | _ -> pspace(); obox(); pstr "else"; pspace(); pprint_block_cbox e2)
            in
        print_cascade_if "if" e e1 e2
    | CeLet(p, e1, e2_opt) ->
        check_if_none p e2_opt;
        obox();
        pstr "val"; pspace(); pprint_pat_ p false;
        pspace(); pstr "="; pspace();
        pprint_ocexp_ e1 0;
        cbox()
    | CeLetRec(fdefs, e2_opt) ->
        check_if_none (CpIdent "<funcs...>") e2_opt;
        List.iter (fun fdef ->
            let {ocf_name; ocf_args; ocf_body} = fdef in
            obox(); obox(); pstr ("fun " ^ ocf_name); pcut();
            pprint_pat_ (CpTuple ocf_args) false;
            (match ocf_body with
            | CeBlock _ -> cbox(); pspace(); pprint_block_cbox ocf_body
            | _ -> pspace(); pstr "="; cbox(); pspace(); pprint_ocexp_ ocf_body 0; cbox());
            pbreak()) fdefs
    | CeLambda(args, body) ->
        obox(); pstr "("; pcut();
        pstr "fun "; pprint_pat_ (CpTuple args) false;
        pspace(); pprint_block_cbox_ body "{" "})"
    | CeLambdaCases(cases) ->
        obox(); pstr "("; pcut();
        pstr "fun (...)"; pspace(); pprint_cases_cbox_ cases "{" "})"
    | CeTyped(e, t) ->
        obox(); pstr "("; pcut();
        pprint_ocexp_ e 0; pstr ":"; pspace();
        pprint_octyp_ t false; pcut(); pstr ")"; cbox()
    | CeCall(f, args) ->
        obox(); pprint_ocexp_ f highest_unary_pr;
        pstr "("; pcut(); obox();
        pprint_elist args; cbox(); pcut(); pstr ")"; cbox()
    | CeMkTuple(el) ->
        obox(); pstr "("; pcut();
        pprint_elist el; pcut(); pstr ")"; cbox()
    | CeMkList(el) ->
        obox(); pstr "[:"; pspace();
        pprint_elist el; pspace(); pstr ":]"; cbox()
    | CeMkVector(el) ->
        obox(); pstr "["; pcut();
        pprint_elist el; pcut(); pstr "]"; cbox()
    | CeMkRecord(relems) ->
        obox(); pprint_relems_ relems; cbox()
    | CeUpdateRecord(r, relems) ->
        obox(); pprint_ocexp_ r highest_unary_pr; pstr ".";
        pprint_relems_ relems; cbox()
    | CeMatch(e, cases) ->
        obox(); pstr "match"; pspace(); pprint_ocexp_ e 0; pspace();
        pprint_cases_cbox cases
    | CeTry(e, cases) ->
        obox(); pstr "try"; pcut(); pprint_block_cbox e; obox(); pstr "catch"; pcut();
        pprint_cases_cbox cases
    | CeRaise(e) ->
        obox(); pstr "throw"; pspace(); pprint_ocexp_ e 0; cbox()
    | CeWhile(e1, e2) ->
        obox();
        pstr "while"; pspace(); pprint_ocexp_ e1 0; pspace();
        pprint_block_cbox e2
    | CeFor(asc, i, e1, e2, body) ->
        obox();
        pstr ("for " ^ i ^ " <-");
        pspace(); pprint_ocexp_ e1 0;
        pcut(); pstr ":"; pcut();
        pspace(); pprint_ocexp_ e2 1100;
        (match asc with
        | true -> pstr "+1";
        | false -> pstr "-1:-1");
        pspace(); pprint_block_cbox body
    | CeBlock _ -> obox(); pprint_block_cbox_ e "({" "})"
    | CeDefTyp(tdefs) ->
        List.iter (fun tdef ->
            (match tdef with
            | DefTyp dt ->
                let {oct_name; oct_args; oct_body} = dt in
                pprint_tdef_hdr oct_args oct_name;
                pspace();
                pprint_octyp_ oct_body false
            | DefVariant dv ->
                let {ocv_name; ocv_args; ocv_cases} = dv in
                pprint_tdef_hdr ocv_args ocv_name;
                pspace();
                obox();
                List.iter (fun (n, t) ->
                    pstr "| "; pprint_octyp_ t false; pspace()) ocv_cases;
                cbox()); pbreak()) tdefs
    | CeDefExn(i, t) ->
        obox(); pstr ("exception " ^ i);
        (match t with
        | CtUnit -> ()
        | _ -> pstr ":"; pspace(); pprint_octyp t);
        cbox()
    | CeOpen(mods) ->
        List.iter (fun m -> ohbox(); pstr ("from " ^ m ^ " import *"); cbox(); pbreak()) mods

and pprint_elist el =
    List.iteri (fun i e ->
        if i = 0 then () else (pstr ","; pspace());
        pprint_ocexp_ e 0) el

and pprint_relems_ relems =
    pstr "{"; pcut();
    List.iteri (fun i (n, e) -> if i = 0 then () else (pstr ","; pspace());
            pstr (n ^ "="); pcut(); pprint_ocexp_ e 0) relems;
    pcut(); pstr "}"

and pprint_tdef_hdr targs tn =
    obox(); pstr "type ";
    (match targs with
    | [] -> ()
    | t :: [] -> pstr t; pspace()
    | _ -> pstr ("(" ^ (String.concat ", " targs) ^ ")"); pspace());
    pstr (tn ^ " =");
    cbox()

and pprint_block_cbox e = pprint_block_cbox_ e "{" "}"

and pprint_block_cbox_ e opening closing =
    let el = match e with CeBlock(el) -> el | _ -> e :: [] in
    pstr opening; pbreak(); ovbox();
    List.iteri (fun i e -> if i = 0 then () else pbreak(); pprint_ocexp_ e 0) el;
    cbox(); cbox(); pbreak(); pstr closing

and pprint_cases_cbox cases = pprint_cases_cbox_ cases "{" "})"

and pprint_cases_cbox_ cases opening closing =
    pstr opening; pbreak(); ovbox();
    List.iter (fun (pl, e) ->
        obox(); List.iter (fun p -> pstr "| "; pprint_pat_ p false; pspace()) pl; cbox();
        pstr "->"; pspace();
        ovbox();
        let el = match e with CeBlock(el) -> el | _ -> e :: [] in
        List.iter (fun e -> pbreak(); pprint_ocexp_ e 0) el;
        cbox(); cbox()) cases;
    cbox(); cbox(); pbreak(); pstr closing

and print_top code =
    ovbox();
    List.iter (fun e -> pprint_ocexp_ e 0; pbreak()) code;
    cbox()
