(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    K-form pretty printer.
    Can be used to debug the K-normalization step,
    as well as all the further K-form transformations
*)

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

let pprint_id n loc = pstr (idk2str n loc)

type ktyp_pr_t = KTypPr0 | KTypPrFun | KTypPrComplex | KTypPrBase

let rec get_ktyp_pr t = match t with
    | KTypInt | KTypCInt | KTypSInt _ | KTypUInt _ | KTypFloat _
    | KTypString | KTypChar | KTypBool | KTypVoid | KTypExn
    | KTypErr | KTypCPointer | KTypNil | KTypModule | KTypName _ -> KTypPrBase
    | KTypTuple _ | KTypRecord _ -> KTypPrBase
    | KTypList _ | KTypRef _ | KTypArray _ -> KTypPrComplex
    | KTypFun _ -> KTypPrFun

let need_parens p p1 = p1 > p
let opt_parens p p1 = if (need_parens p p1) then ("(", ")") else ("", "")

let rec ppktyp_ t p1 loc =
    let prec = get_ktyp_pr t in
    let ppktypsuf t1 suf =
        let (lp, rp) = opt_parens prec p1 in
        (obox(); pstr lp; ppktyp_ t1 prec loc; pstr rp; pstr " "; pstr suf; cbox()) in
    let ppktyplist_ prefix args =
        pstr prefix; pcut(); obox();
        (List.iteri (fun i t -> if i = 0 then () else (pstr ","; pspace()); ppktyp_ t KTypPr0 loc) args);
        cbox(); pcut(); pstr ")" in
    match t with
    | KTypInt -> pstr "Int"
    | KTypCInt -> pstr "CInt"
    | KTypSInt(b) -> pstr ("Int" ^ (string_of_int b))
    | KTypUInt(b) -> pstr ("UInt" ^ (string_of_int b))
    | KTypFloat(16) -> pstr "Half"
    | KTypFloat(32) -> pstr "Float"
    | KTypFloat(64) -> pstr "Double"
    | KTypFloat(b) -> raise_compile_err loc (sprintf "K_pp: invalid type TypFloat(%d)" b)
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
            | t1 :: [] -> ppktyp_ t1 prec loc
            | _ -> ppktyp_ (KTypTuple tl) prec loc);
            pspace(); pstr "->"; pspace();
            ppktyp_ t2 prec loc;
            pstr ")"; cbox()
    | KTypList(t1) -> ppktypsuf t1 "List"
    | KTypRef(t1) -> ppktypsuf t1 "Ref"
    | KTypArray(d, t1) -> ppktypsuf t1 ("[" ^ (String.make (d - 1) ',') ^ "]")
    | KTypName(n) -> pstr (idk2str n loc)
    | KTypTuple(tl) -> ppktyplist_ "(" tl
    | KTypRecord(rn, relems) -> obox(); pprint_id rn loc; pstr " {";
        List.iteri (fun i (ni, ti) -> if i = 0 then () else pstr ", ";
            pprint_id ni loc; pstr ": "; ppktyp_ ti KTypPr0 loc) relems; pstr "}"; cbox()
    | KTypExn -> pstr "Exn"
    | KTypErr -> pstr "Err"
    | KTypCPointer -> pstr "CPtr"
    | KTypModule -> pstr "Module"

let pprint_ktyp t loc = ppktyp_ t KTypPr0 loc
let pprint_atom a loc = match a with
    | Atom.Id ((Id.Name _) as k) -> pprint_id k loc
    | Atom.Id k -> pprint_id k loc
    | Atom.Lit l -> pstr (lit2str l false loc)
let pprint_dom r loc = match r with
    | Domain.Elem(a) -> pprint_atom a loc
    | Domain.Fast(a) -> pstr "<"; pprint_atom a loc; pstr ">"
    | Domain.Range(i,j,k) ->
        (match i with
        | Atom.Lit(LitNil) -> ()
        | _ -> pprint_atom i loc);
        pstr ":";
        (match j with
        | Atom.Lit(LitNil) -> ()
        | _ -> pprint_atom j loc);
        (match k with
        | Atom.Lit(LitNil) -> ()
        | Atom.Lit(LitInt 1L) -> ()
        | _ -> pstr ":"; pprint_atom k loc)

let rec pprint_kexp e = pprint_kexp_ e true
and pprint_for_hdr pre_exp for_cl at_ids eloc =
    pstr "FOR ("; pcut();
    (match pre_exp with
    | KExpNop _ -> ()
    | _ -> pprint_kexp_ pre_exp false; pstr ";"; pspace());
    (List.iteri (fun i (n, dom) -> if i = 0 then () else (pstr ","; pspace());
    pprint_id n eloc; pspace(); pstr "<-"; pspace(); pprint_dom dom eloc) for_cl);
    if at_ids = [] then () else
    (pspace(); pstr "@(";
    List.iteri (fun i at_id ->
        if i = 0 then pcut() else (pstr ","; pspace());
        pprint_id at_id eloc) at_ids;
    pcut(); pstr ")");
    pcut(); pstr ")"
and pprint_kexp_ e prtyp =
    (*let _ = printf "printing: %s\n" (kexp2str e) in*)
    let t = get_kexp_typ e in
    let eloc = get_kexp_loc e in
    let ppktp ktp = match ktp with
        Some({ktp_complex}) -> pstr (if ktp_complex then "COMPLEX" else "SIMPLE"); pspace()
        | _ -> () in
    let pprint_atom_ a = pprint_atom a eloc in
    let pprint_dom_ r = pprint_dom r eloc in
    let pprint_id_ i = pprint_atom_ (Atom.Id i) in
    let pprint_id_label i = pstr (idk2str i eloc) in
    let obox_cnt = ref 0 in
    let obox_() = obox(); if prtyp then (pstr "<"; ppktyp_ t KTypPr0 eloc; pstr ">") else (); obox_cnt := !obox_cnt + 1 in
    let cbox_() = if !obox_cnt <> 0 then (cbox(); obox_cnt := !obox_cnt - 1) else () in
    match e with
    | KDefVal(n, e0, loc) -> obox();
        let {kv_typ; kv_flags} = get_kval n loc in
        (List.iter (fun vf -> match vf with
        | ValTempRef -> pstr "TEMP_REF"; pspace()
        | ValTemp -> pstr "TEMP"; pspace()
        | ValMutable -> pstr "MUTABLE"; pspace()
        | ValPrivate -> pstr "PRIVATE"; pspace()
        | ValSubArray -> pstr "SUB_ARRAY"; pspace()
        | ValGlobal _ -> pstr "GLOBAL"; pspace()
        | ValCtor _ -> ()
        | ValArg -> pstr "ARG"; pspace()) kv_flags);
        pstr "VAL"; pspace(); pprint_id_label n; pstr ": "; pprint_ktyp kv_typ loc; pspace(); pstr "="; pspace();
        pprint_kexp_ e0 false; cbox()
    | KDefFun {contents={kf_name; kf_args; kf_rt; kf_body; kf_closure; kf_flags; kf_loc }} ->
        let {kci_arg; kci_fcv_t} = kf_closure in
        let nargs = List.length kf_args in
        let fkind = ref "FUN" in
        let ctor_id = get_fun_ctor kf_flags in
        (obox(); (List.iter (fun ff -> match ff with
                    | FunPure -> pstr "PURE"; pspace()
                    | FunImpure -> pstr "IMPURE"; pspace()
                    | FunInline -> pstr "INLINE"; pspace()
                    | FunNoThrow -> pstr "NOTHROW"; pspace()
                    | FunReallyNoThrow -> pstr "REALLY_NOTHROW"; pspace()
                    | FunPrivate -> pstr "PRIVATE"; pspace()
                    | FunStd -> pstr "STANDARD"; pspace()
                    | FunUseFV -> pstr "USE_FV"; pspace()
                    | FunRecursive -> pstr "RECURSIVE"; pspace()
                    | FunCtor _ -> ()
                    | FunInC -> pstr "C_FUNC"; pspace()) kf_flags);
        pstr (!fkind); pspace(); pprint_id_label kf_name; pspace();
        pstr "("; pcut(); obox();
        List.iteri (fun i (n, t) ->
            if i > 0 then (pstr ","; pspace()) else ();
            pprint_id n kf_loc; pstr ":"; pspace(); pprint_ktyp t kf_loc) kf_args;
        if kci_arg = noid then () else
            (if nargs > 0 then pstr ";" else (); pspace();
            pprint_id kci_arg kf_loc; pstr ":"; pspace();
            pprint_id_ kci_fcv_t);
        cbox(); pcut();
        pstr ")";
        pspace(); pstr ":"; pspace(); pprint_ktyp kf_rt kf_loc; pspace();
        pstr "="; pspace(); if ctor_id <> CtorNone then pstr (ctor2str ctor_id) else pprint_kexp kf_body; cbox())
    | KDefExn { contents = {ke_name; ke_typ; ke_loc} } ->
        obox(); pstr "EXCEPTION"; pspace(); pprint_id_label ke_name;
        (match ke_typ with
        | KTypVoid -> ()
        | _ -> pspace(); pstr "OF"; pspace(); pprint_ktyp ke_typ ke_loc); cbox()
    | KDefTyp { contents = {kt_name; kt_typ; kt_props; kt_loc} } ->
        obox(); ppktp kt_props;
        pstr "TYPE"; pspace(); pprint_id_label kt_name;
        pspace(); pstr "="; pspace(); pprint_ktyp kt_typ kt_loc; cbox()
    | KDefVariant { contents = {kvar_name; kvar_cases; kvar_props; kvar_constr; kvar_flags; kvar_loc} } ->
        let is_rec_opt0 = List.mem VariantRecOpt kvar_flags in
        let is_recursive = List.mem VariantRecursive kvar_flags in
        obox(); ppktp kvar_props; if is_rec_opt0 then pstr "RECURSIVE_OPTION " else
            if is_recursive then pstr "RECURSIVE " else ();
        if (List.mem VariantNoTag kvar_flags) && not is_rec_opt0 then pstr "NO_TAG " else ();
        pstr "TYPE"; pspace(); pprint_id_label kvar_name;
        pspace(); pstr "="; pspace(); (List.iteri (fun i ((v, t), c) ->
            if i = 0 then () else pstr " | "; pprint_id v kvar_loc;
            pstr "<"; pprint_id c kvar_loc; pstr ": "; pprint_ktyp (get_idk_ktyp c kvar_loc)
            kvar_loc; pstr ">: "; pprint_ktyp t kvar_loc)
            (Utils.zip kvar_cases (if kvar_constr != [] then kvar_constr
            else (List.map (fun (v, _) -> v) kvar_cases))));
        cbox()
    | KDefClosureVars { contents = {kcv_name; kcv_freevars; kcv_loc} } ->
        obox(); pstr "CLOSURE_DATA"; pspace(); pprint_id_ kcv_name;
        pspace(); pstr "="; pspace(); pstr "{ ";
        List.iteri (fun i (n, t) ->
            if i = 0 then () else (pstr ","; pspace());
            pprint_id n kcv_loc; pstr ": "; pspace(); pprint_ktyp t kcv_loc)
        kcv_freevars;
        pstr " }"; cbox()
    | KExpSeq(el, _) -> pprint_kexpseq el true
    | _ -> obox_(); (match e with
        | KExpNop(_) -> pstr "{}"
        | KExpBreak(_) -> pstr "BREAK"
        | KExpContinue(_) -> pstr "CONTINUE"
        | KExpAtom(a, _) -> pprint_atom_ a
        | KExpBinOp(o, a, b, _) ->
            let ostr = binop_to_string o in
            pprint_atom_ a; pspace(); pstr ostr; pspace(); pprint_atom_ b
        | KExpAssign(n, a, _) -> pprint_id_ n; pspace(); pstr "="; pspace(); pprint_atom_ a
        | KExpMem(n, i, (_, loc)) ->
            pprint_id n loc; pstr ".";
            (match (get_idk_ktyp n loc) with
            | KTypRecord(rn, relems) ->
                let (ni, _) = List.nth relems i in pstr (pp_id2str ni)
            | _ -> pstr (string_of_int i))
        | KExpUnOp(OpDeref, (Atom.Id n), (_, loc)) ->
            pstr "*"; pprint_id n loc
        | KExpUnOp(OpMkRef, a, _) ->
            pstr "MAKE_REF "; pprint_atom_ a
        | KExpUnOp(o, a, _) ->
            let ostr = unop_to_string o in
            pstr ostr; pprint_atom_ a
        | KExpIntrin(iop, args, _) ->
            pstr (intrin2str iop); pstr "(";
            List.iteri (fun i a -> if i = 0 then () else (pstr ","; pspace()); pprint_atom_ a) args;
            pstr ")"
        | KExpThrow(n, f, _) -> pstr (if f then "RETHROW " else "THROW "); pprint_id_ n
        | KExpMkTuple(al, _) ->
            pstr "("; obox();
            (List.iteri (fun i a ->
                if i = 0 then () else (pstr ","; pspace()); pprint_atom_ a) al);
            (match al with
            a :: [] -> pstr ","
            | _ -> ()); cbox(); pstr ")"
        | KExpMkRecord(al, (t, loc)) ->
            let (rn, relems) = match t with
                | KTypRecord(rn, relems) -> (rn, relems)
                | KTypName n -> (match (kinfo_ n loc) with
                    | KTyp {contents={kt_name; kt_typ=KTypRecord(_, rec_elems)}} -> (kt_name, rec_elems)
                    | _ -> raise_compile_err loc "K_pp: invalid record type in KExpMkRecord(...)")
                | _ -> raise_compile_err loc "K_pp: invalid record type in KExpMkRecord(...)" in
            let ant = Utils.zip al relems in
            pstr "MAKE_RECORD "; pprint_id rn loc; pstr " {"; obox();
            List.iteri (fun i (a, (n, t)) ->
                if i = 0 then () else (pstr ","; pspace()); pprint_id n loc; pstr "="; pprint_atom_ a) ant;
            (match al with
            a :: [] -> pstr ","
            | _ -> ()); cbox(); pstr "}"
        | KExpMkClosure(make_fp, f, args, (_, loc)) ->
            obox();
            pstr "MAKE_CLOSURE ("; pcut();
            pprint_id_ make_fp; pstr ","; pspace(); pprint_id_ f;
            if make_fp = noid then () else
                (let (fvars, _) = get_closure_freevars f loc in
                let ant = Utils.zip args fvars in
                pstr ";"; pspace(); pstr "{"; pcut();
                List.iteri (fun i (a, (n, _)) ->
                if i = 0 then () else (pstr ","; pspace()); pprint_id n loc; pstr "="; pprint_atom_ a) ant;
                pcut(); pstr "}");
            pcut(); pstr ")"; cbox()
        | KExpMkArray(elems, (_, l)) ->
            obox(); pstr "["; obox();
            List.iteri (fun i arow ->
                if i > 0 then (pstr ";"; pspace()) else ();
                obox(); List.iteri (fun j (f, a) ->
                    if i > 0 then (pstr ","; pspace()) else ();
                    if f then pstr "\\" else ();
                    pprint_atom_ a) arow;
                cbox()) elems;
            cbox(); pstr "]"; cbox()
        | KExpCall(f, args, (_, loc)) ->
            obox(); pprint_id_ f;
            (match (kinfo_ f loc) with
            | KFun _ -> ()
            | KExn _ -> ()
            | _ -> pstr "~");
            pstr "(";
            (List.iteri (fun i a ->
                if i = 0 then () else (pstr ","; pspace()); pprint_atom_ a) args);
            pstr ")"; cbox();
        | KExpAt(a, border, interp, args, _) ->
            pprint_atom_ a;
            pstr (border2str border true);
            pstr (interp2str interp true);
            pstr "[";
            obox(); (List.iteri (fun i dom ->
                if i = 0 then () else (pstr ","; pspace()); pprint_dom_ dom) args);
            cbox(); pstr "]"
        | KExpIf(if_c, if_then, if_else, _) ->
            obox(); pstr "IF ("; pprint_kexp if_c; pstr ")";
            pspace(); pprint_kexp if_then; pspace();
            pstr "ELSE"; pspace(); pprint_kexp if_else; cbox()
        | KExpWhile(c, body, _) ->
            obox(); pstr "WHILE ("; pprint_kexp c; pstr ")"; pspace(); pprint_kexp body; cbox()
        | KExpDoWhile(body, c, _) ->
            obox(); pstr "DO"; pprint_kexp body; pspace(); pstr "WHILE ("; pprint_kexp c; pstr ")"; cbox()
        | KExpFor (for_cl, at_ids, for_body, flags, loc) ->
            obox(); Ast_pp.pprint_for_flags flags;
            pprint_for_hdr (KExpNop loc) for_cl at_ids eloc;
            pspace(); pprint_kexp for_body; cbox()
        | KExpMap(map_cl, map_body, flags, (_, loc)) ->
            obox(); Ast_pp.pprint_for_flags flags;
            pstr "["; if (List.mem ForMakeList flags) then pstr ":: " else ();
            (List.iter (fun (pre_e, pe_l, at_ids) ->
              pprint_for_hdr pre_e pe_l at_ids eloc; pspace ()) map_cl);
            pprint_kexp map_body; pstr "]"; cbox()
        | KExpMatch (cases, _) ->
            obox(); List.iteri (fun i (checks_i, e_i) ->
                pstr (if i = 0 then "IF (" else if checks_i = [] then "ELSE" else "ELSE IF (");
                List.iteri (fun j cj ->
                    if j = 0 then () else (pstr " &&"; pspace());
                    pprint_kexp cj) checks_i;
                if checks_i = [] then () else pstr ")";
                pspace(); pprint_kexp e_i; pspace()) cases;
            cbox();
        | KExpTryCatch(e1, e2, _) ->
            obox(); pstr "TRY"; pspace(); pprint_kexp e1; pspace();
            pstr "CATCH"; pprint_kexp e2; cbox()
        | KExpCast(a, t, loc) -> pstr "("; obox(); pprint_atom_ a; pspace(); pstr ":>"; pspace(); pprint_ktyp t loc; cbox(); pstr ")"
        | KExpCCode(s, _) -> pstr "CCODE"; pspace(); pstr "\"\"\""; pstr s; pstr "\"\"\""
        (* those are already handled above; duplicate it here once again to get compile-time warning
           about uncovered constructions *)
        | KDefVal _ | KDefFun _ | KDefTyp _ | KDefVariant _
        | KDefClosureVars _  | KDefExn _ | KExpSeq _ -> ()); cbox_()

and pprint_kexp_as_seq e = match e with
    | KExpSeq(es, _) -> pprint_kexpseq es false
    | _ -> pprint_kexp e
and pprint_kexpseq el braces =
    if braces then pstr "{" else ();
    ohvbox();
    (List.iteri (fun i e -> if i=0 then () else (pstr ";"; pspace()); pprint_kexp e) el); cbox();
    if braces then pstr "}" else ()

let pprint_atom_x a loc = Format.print_flush (); Format.open_box 0; pprint_atom a loc; Format.close_box(); Format.print_flush ()
let pprint_ktyp_x t loc = Format.print_flush (); Format.open_box 0; pprint_ktyp t loc; Format.close_box(); Format.print_flush ()
let pprint_kexp_x e = Format.print_flush (); Format.open_box 0; pprint_kexp e; Format.close_box(); Format.print_flush ()
let pprint_top code = Format.print_flush (); Format.open_box 0; pprint_kexpseq code false; Format.close_box(); pbreak(); Format.print_flush ()
let pprint_kinfo_x ki =
    (Format.print_flush (); Format.open_box 0;
    (match ki with
    | KNone -> pstr "KNone"
    | KVal {kv_name; kv_typ; kv_loc} ->
        pstr "KVal: "; pprint_id kv_name kv_loc; pstr ": "; pprint_ktyp kv_typ kv_loc
    | KFun kf -> pstr "KFun: "; pprint_kexp (KDefFun kf)
    | KVariant kvar -> pstr "KVar: "; pprint_kexp (KDefVariant kvar)
    | KExn ke -> pstr "KExn: "; pprint_kexp (KDefExn ke)
    | KClosureVars kcv -> pstr "KClosureVars: "; pprint_kexp (KDefClosureVars kcv)
    | KTyp kt -> pstr "KTyp: "; pprint_kexp (KDefTyp kt));
    Format.close_box(); Format.print_flush ())
