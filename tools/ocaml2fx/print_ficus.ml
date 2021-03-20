(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* Ocaml => Ficus pretty printer. *)

open Syntax

let margin = 150
let base_indent = ref 4

let pstr = Format.print_string
let pspace = Format.print_space
let pcut = Format.print_cut
let pbreak () = Format.print_break 1 0
let pbreak_indent () = Format.print_break 1 (!base_indent)
let obox () = Format.open_box (!base_indent)
let obox_indent () = Format.open_box (!base_indent)
let cbox = Format.close_box
let ovbox () = Format.open_vbox 0
let ovbox_indent () = Format.open_vbox (!base_indent)
let ovbox_brace () = Format.open_vbox (!base_indent-1)
let ohvbox () = Format.open_hvbox 0
let ohvbox_indent () = Format.open_hvbox (!base_indent)
let ohbox () = Format.open_hbox ()

type assoc_t = AssocLeft | AssocRight
let highest_unary_pr = 1700

let binop2str_ bop = match bop with
    | OpAt -> ("", highest_unary_pr, AssocLeft)
    | OpMem -> (".", highest_unary_pr, AssocLeft)
    | OpArrow -> ("->", highest_unary_pr, AssocLeft)
    | OpMul -> ("*", 1200, AssocLeft)
    | OpDiv -> ("/", 1200, AssocLeft)
    | OpMod -> ("%", 1200, AssocLeft)
    | OpAdd -> ("+", 1100, AssocLeft)
    | OpSub -> ("-", 1100, AssocLeft)
    | OpSHL -> ("<<", 1000, AssocLeft)
    | OpSHR -> (">>", 1000, AssocLeft)
    | OpLT -> ("<", 900, AssocLeft)
    | OpLE -> ("<=", 900, AssocLeft)
    | OpGT -> (">", 900, AssocLeft)
    | OpGE -> (">=", 900, AssocLeft)
    | OpEQ -> ("==", 800, AssocLeft)
    | OpNE -> ("!=", 800, AssocLeft)
    | OpCons -> ("::", 950, AssocRight)
    | OpLogicAnd -> ("&&", 400, AssocLeft)
    | OpLogicOr -> ("||", 300, AssocLeft)
    | OpConcat -> ("+", 1100, AssocLeft)
    | OpAssign -> ("=", 100, AssocRight)

let unop2str_ uop = match uop with
    | OpNeg -> ("-", 1300, AssocRight)
    | OpNot -> ("!", 1300, AssocRight)
    | OpDeref -> ("*", 1300, AssocRight)
    | OpMkRef -> ("ref ", 1600, AssocRight)

let pprint_id2str n = n
let pprint_id n = pstr (pprint_id2str n)

let rec pprint_octyp_ t fparen =
    (match t with
    | TInt | TFloat | TString | TChar | TBool | TUnit -> pstr (octyp2str t)
    | TList(t) -> obox(); pprint_octyp_ t true; pspace(); pstr "list"; cbox()
    | TRef(t) -> obox(); pprint_octyp_ t true; pspace(); pstr "ref"; cbox()
    | TTuple(tl) ->
        obox(); pstr "("; pcut(); List.iteri (fun i t -> if i = 0 then () else (pstr ","; pspace());
          pprint_octyp t) tl; pcut(); pstr ")"; cbox()
    | TName(n) -> pstr n
    | TFun(arg, rt) ->
        obox();
        if fparen then (pstr "("; pcut()) else ();
        pprint_octyp_ arg true;
        pspace(); pstr "->"; pspace();
        pprint_octyp_ rt false;
        if fparen then (pcut(); pstr ")") else ();
        cbox()
    | TRecord(relems) ->
        obox(); pstr "{"; pcut();
        List.iteri (fun i (m, n, t) ->
            let t = if m then TRef(t) else t in
            if i = 0 then () else (pstr ";"; pspace());
            pstr n; pstr ":"; pspace(); pprint_octyp t) relems;
        pcut(); pstr "}"; cbox()
    | TApp(args, n) ->
        obox();
        let argt = match args with t :: [] -> t | _ -> TTuple(args) in
        pprint_octyp_ argt true;
        (match n with
        | "option" -> pstr "?";
        | _ -> pspace(); pstr n);
        cbox())

and pprint_octyp t = pprint_octyp_ t false

and pprint_pat_ p parens =
    let oboxp () = obox(); if parens then (pstr "("; pcut()) else () in
    let cboxp () = if parens then (pcut(); pstr ")") else (); cbox() in
    match p with
    | PLit(l) -> pstr (lit2str l)
    | PIdent(n) -> pstr n
    | PAny -> pstr "_"
    | PTuple(pl) ->
        obox(); pstr "("; pcut();
        List.iteri (fun i p ->
            if i = 0 then () else (pstr ","; pspace());
            pprint_pat_ p false) pl;
        pcut(); pstr ")";
        cbox()
    | PVariant(vn, pl) ->
        obox(); pstr vn; pcut();
        let (p, need_parens) =
            match pl with
            | PLit _ :: [] | PIdent _ :: [] | PAny :: [] -> ((List.hd pl), false)
            | p :: [] -> (p, true)
            | _ -> (PTuple(pl), false)
            in
        if need_parens then (pstr "("; pcut()) else if pl = [] then pspace() else ();
        if pl = [] then () else pprint_pat_ p false;
        if need_parens then (pcut(); pstr ")") else ();
        cbox()
    | PRecord (vn, relems) ->
        let is_ref = match relems with
            | ("contents", _) :: _ -> true
            | _ -> false
            in
        obox(); if vn = noid then () else (pstr vn; pspace());
        pstr (if is_ref then "(" else "{"); pcut();
        List.iteri (fun i (n, p) ->
            if i = 0 then () else (pstr ","; pspace());
            if n = "contents" then
                (pstr "ref"; pspace(); pprint_pat_ p true)
            else
              (pstr n;
              match p with
              | PIdent(n1) when n1=n -> ()
              | _ -> pstr "="; pcut(); pprint_pat_ p false)
        ) relems;
        pcut(); pstr (if is_ref then ")" else "}"); cbox()
    | PCons(p1, p2) ->
        oboxp();
        pprint_pat_ p1 true;
        pspace(); pstr "::"; pspace();
        pprint_pat_ p2 (match p2 with PCons _ -> false | _ -> true);
        cboxp()
    | PAs(p, n) ->
        oboxp();
        pprint_pat_ p true;
        pspace(); pstr ("as " ^ n);
        cboxp();
    | PWhen(p, e) ->
        oboxp();
        pprint_pat_ p true;
        pspace(); pstr "when ";
        pprint_ocexp_ e 0;
        cboxp()
    | PTyped(p, t) ->
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
    | ELit(lit) -> pstr (lit2str lit)
    | EIdent(i) -> pstr i
    | EUnary(uop, e) ->
        let (uop_str, pr0, _) = unop2str_ uop in
        ohvbox_indent(); if pr0 < pr then (pstr "("; pcut()) else ();
        pstr uop_str; pprint_ocexp_ e pr0;
        if pr0 < pr then (pcut(); pstr ")") else (); cbox()
    | EBinary(OpAssign, e1, e2) ->
        ohvbox_indent(); pstr "*"; pprint_ocexp_ e1 1300;
        pspace(); pstr "="; pspace();
        pprint_ocexp_ e2 0; cbox()
    | EBinary(OpAt, e1, e2) ->
        ohvbox_indent(); pprint_ocexp_ e1 highest_unary_pr;
        pstr "["; pcut(); pprint_ocexp_ e2 0; pstr "]"; cbox()
    | EBinary(OpMem, e1, e2) ->
        ohvbox_indent();
        (match e1 with
        | EUnary(OpDeref, at_e1) ->
            pprint_ocexp_ at_e1 highest_unary_pr;
            pstr "->"
        | _ ->
            pprint_ocexp_ e1 highest_unary_pr;
            pstr ".");
        pprint_ocexp_ e2 0; cbox()
    | EBinary(bop, e1, e2) ->
        let (bop_str, pr0, assoc) = binop2str_ bop in
        ohvbox_indent(); if pr0 < pr then (pstr "("; pcut()) else ();
        let is_shift = match bop with OpSHL | OpSHR -> true | _ -> false in
        let pr1 = if is_shift then 1350 else if assoc=AssocLeft then pr0 else pr0+1 in
        let pr2 = if is_shift then 1350 else if assoc=AssocRight then pr0 else pr0+1 in
        pprint_ocexp_ e1 pr1;
        if bop_str = "." then (pstr bop_str) else
        (pspace(); pstr bop_str; pspace());
        pprint_ocexp_ e2 pr2;
        if pr0 < pr then (pcut(); pstr ")") else (); cbox()
    | EIf(e, e1, e2) ->
        let rec print_cascade_if prefix e e1 e2 =
            pstr prefix; pspace(); pprint_ocexp_ e 0; pspace();
            pprint_block_ e1 "{" "";
            (match e2 with
            | ELit(LUnit) -> pstr "}"
            | EIf(e_, e1_, e2_) ->
                ovbox_indent(); ohbox();
                (match e1 with
                | ELit(LUnit) -> ()
                | _ -> pstr "} ");
                print_cascade_if "else if" e_ e1_ e2_
            | _ ->
                ovbox_indent();
                ohbox();
                (match e1 with
                | ELit(LUnit) -> ()
                | _ -> pstr "} ");
                pstr "else"; pspace();
                pprint_block e2)
            in
        ovbox_indent(); ohbox();
        print_cascade_if "if" e e1 e2
    | ELet(p, e1, e2_opt) ->
        check_if_none p e2_opt;
        ohvbox(); ohbox(); pstr "val"; pspace(); pprint_pat_ p false;
        pspace(); pstr "="; cbox(); pspace();
        pprint_ocexp_ e1 0; cbox()
    | ELetRec(fdefs, e2_opt) ->
        check_if_none (PIdent "<funcs...>") e2_opt;
        List.iter (fun fdef ->
            let {ocf_name; ocf_args; ocf_body} = fdef in
            let is_block = match ocf_body with EBlock _ -> true | _ -> false in
            if is_block then ovbox_indent() else ohvbox_indent();
            ohbox(); pstr ("fun " ^ ocf_name); pcut();
            pprint_pat_ (PTuple ocf_args) false;
            if is_block || (match ocf_body with ELit(LUnit) -> true | _ -> false) then
                (pspace(); pprint_block ocf_body)
            else
                (pstr " ="; cbox(); pspace(); pprint_ocexp_ ocf_body 0; cbox());
            pbreak()) fdefs
    | ELambda(args, body) ->
        ovbox_indent(); ohbox(); pstr "fun "; pprint_pat_ (PTuple args) false;
        pspace(); pprint_block body
    | ELambdaCases(cases) ->
        ovbox_indent(); ohbox(); pstr "fun (...)"; pspace(); pstr "{"; cbox(); pprint_cases cases
    | ETyped(e, t) ->
        ohvbox_indent(); pstr "("; pcut();
        pprint_ocexp_ e 0; pstr ":"; pspace();
        pprint_octyp_ t false; pcut(); pstr ")"; cbox()
    | ECall(f, args) ->
        ohvbox_indent();
        ohbox(); pprint_ocexp_ f highest_unary_pr; pstr "("; cbox(); pcut();
        pprint_elist args; pstr ")"; cbox();
    | EMkTuple(el) ->
        ohvbox_indent(); pstr "("; pcut();
        pprint_elist el; pcut(); pstr ")"; cbox()
    | EMkList(el) ->
        ohvbox_indent(); pstr "[:"; pspace();
        pprint_elist el; pspace(); pstr ":]"; cbox()
    | EMkVector(el) ->
        ohvbox_indent(); pstr "["; pcut();
        pprint_elist el; pcut(); pstr "]"; cbox()
    | EMkRecord(relems) ->
        ohvbox_indent(); pprint_relems_ relems; cbox()
    | EUpdateRecord(r, relems) ->
        ohvbox_indent();
        (match r with
        | EUnary(OpDeref, at_r) ->
            pprint_ocexp_ at_r highest_unary_pr; pstr "->"
        | _ ->
            pprint_ocexp_ r highest_unary_pr; pstr ".");
        pprint_relems_ relems; cbox()
    | EMatch(e, cases) ->
        ovbox(); ohbox(); pstr "match"; pspace(); pprint_ocexp_ e 0; pspace(); pstr "{"; cbox();
        pprint_cases cases
    | ETry(e, cases) ->
        ovbox_indent(); ohbox(); pstr "try"; pspace(); pprint_block_ e "{" "";
        ohvbox(); ohbox(); pstr "} catch"; pspace(); pstr "{"; cbox();
        pprint_cases cases
    | ERaise(e) ->
        ohvbox_indent(); pstr "throw"; pspace(); pprint_ocexp_ e 0; cbox()
    | EWhile(e1, e2) ->
        ovbox_indent();
        pstr "while"; pspace(); pprint_ocexp_ e1 0; pspace();
        pprint_block e2
    | EFor(asc, i, e1, e2, body) ->
        ovbox_indent(); ohbox();
        pstr ("for " ^ i ^ " <-");
        pspace(); pprint_ocexp_ e1 0;
        pcut(); pstr ":"; pcut();
        pspace(); pprint_ocexp_ e2 1100;
        (match asc with
        | true -> pstr "+1";
        | false -> pstr "-1:-1");
        pspace(); pprint_block body
    | EForEach(p, idx, lst, body) ->
        ovbox_indent(); ohbox();
        pstr "for"; pspace(); pprint_pat_ p false;
        (match idx with
        | PAny -> ()
        | _ -> pstr "@"; pprint_pat_ idx false);
        pspace(); pstr "<-";
        pspace(); pprint_ocexp_ lst 0;
        pspace(); pprint_block body
    | EMap(p, lst, body) ->
        ovbox_indent(); ohbox();
        pstr "[: for"; pspace(); pprint_pat_ p false; pspace(); pstr "<-";
        pspace(); pprint_ocexp_ lst 0;
        pspace(); pprint_block_ body "{" "} :]"
    | EFold(defv, (acc, acc0), (p, lst), body) ->
        ovbox_indent(); ohbox();
        if defv then pstr "val fold" else pstr "fold";
        pspace();
        let (pl, el) = match (acc, acc0) with
            | (PTuple(pl), EMkTuple(el)) -> (pl, el)
            | _ -> (acc :: [], acc0 :: [])
            in
        let _ = List.fold_left2 (fun idx p e ->
            if idx > 0 then (pstr ","; pspace()) else ();
            pprint_pat_ p false;
            pstr " ="; pspace(); pprint_ocexp_ e 0;
            idx+1) 0 pl el in
        pspace(); pstr "for"; pspace(); pprint_pat_ p false;
        pspace(); pstr "<-";
        pspace(); pprint_ocexp_ lst 0;
        pspace(); pprint_block body
    | EBlock _ -> ovbox_indent(); ohbox(); pprint_block_ e "({" "})"
    | EDefTyp(tdefs) ->
        List.iter (fun tdef ->
            ovbox_indent();
            (match tdef with
            | DefTyp dt ->
                let {oct_name; oct_args; oct_body} = dt in
                pprint_tdef_hdr oct_args oct_name;
                pspace();
                pprint_octyp_ oct_body false;
            | DefVariant dv ->
                let {ocv_name; ocv_args; ocv_cases} = dv in
                pprint_tdef_hdr ocv_args ocv_name;
                pspace();
                ovbox();
                List.iter (fun (n, t) ->
                    pstr "| "; pstr n;
                    if t = TUnit then () else
                        (pstr ": "; pprint_octyp_ t false);
                    pspace()) ocv_cases;
                cbox());
            cbox(); pbreak()) tdefs
    | EDefExn(i, t) ->
        ohbox(); pstr ("exception " ^ i);
        (match t with
        | TUnit -> ()
        | _ -> pstr ":"; pspace(); pprint_octyp t);
        cbox()
    | EOpen(mods) ->
        List.iter (fun m -> ohbox(); pstr ("from " ^ m ^ " import *"); cbox(); pbreak()) mods

and pprint_elist el =
    List.iteri (fun i e ->
        if i = 0 then () else (pstr ","; pspace());
        pprint_ocexp_ e 0) el

and pprint_relems_ relems =
    pstr "{"; pcut();
    List.iteri (fun i (n, e) -> if i = 0 then () else (pstr ","; pspace());
            ohbox(); pstr (n ^ "="); pcut(); pprint_ocexp_ e 0; cbox()) relems;
    pcut(); pstr "}"

and pprint_tdef_hdr targs tn =
    ohbox(); pstr "type ";
    (match targs with
    | [] -> ()
    | t :: [] -> pstr t; pspace()
    | _ -> pstr ("(" ^ (String.concat ", " targs) ^ ")"); pspace());
    pstr (tn ^ " =");
    cbox()

and pprint_block e = pprint_block_ e "{" "}"

and pprint_block_ e opening closing =
    let el = match e with EBlock(el) -> el | _ -> e :: [] in
    pstr opening;
    (match el with
    | [] | ELit(LUnit) :: [] ->
        pstr (if closing <> "" then closing else "}"); cbox(); cbox();
        (*pbreak()*)
        Format.force_newline()
    | _ ->
        cbox(); pbreak();
        List.iteri (fun i e -> if i = 0 then () else pbreak(); pprint_ocexp_ e 0) el;
        cbox(); (*pbreak();*)
        Format.force_newline();
        if closing = "" then () else pstr closing)

and pprint_cases cases =
    List.iter (fun (pl, e) ->
        pbreak();
        ohvbox_indent();
        ohbox(); List.iter (fun p -> pstr "| "; pprint_pat_ p false; pspace()) pl;
        pstr "=>";
        cbox(); pbreak();
        ovbox();
        let el = match e with EBlock(el) -> el | _ -> e :: [] in
        List.iteri (fun j e -> if j = 0 then () else pbreak();
            pprint_ocexp_ e 0) el;
        cbox(); cbox()) cases;
    pbreak(); pstr "}"; cbox()

and pprint_top code =
    Format.set_margin margin;
    ovbox();
    List.iter (fun e -> pprint_ocexp_ e 0; pbreak()) code;
    cbox()

let pprint_top_to_file filename code =
    let outch = open_out filename in
    Format.set_formatter_out_channel outch;
    pprint_top code;
    Format.print_flush();
    Format.set_formatter_out_channel stdout;
    close_out outch

let pprint_exp_to_string e =
    (let (std_print, std_flush) = Format.get_formatter_output_functions () in
    let all_lines = ref ([]: string list) in
    let str_print s p n = all_lines := (String.sub s p n) :: !all_lines in
    let str_flush () = () in
    let prev_margin = Format.get_margin() in
    Format.set_formatter_output_functions str_print str_flush;
    Format.set_margin margin;
    pprint_ocexp_ e 0;
    Format.print_flush();
    Format.set_margin prev_margin;
    Format.set_formatter_output_functions std_print std_flush;
    let rec remove_trailing_empty_lines all_lines =
        match all_lines with
        | "" :: rest | "\n" :: rest -> remove_trailing_empty_lines rest
        | l::rest -> (if (Utils.ends_with l "\n") then l else l ^ "\n") :: rest
        | _ -> all_lines
        in
    String.concat "" (List.rev (remove_trailing_empty_lines !all_lines)))
