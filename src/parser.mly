%{
(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* ficus parser *)
open Ast

let get_fold_result() = get_id "__fold_result__"

let make_loc(pos0, pos1) =
    let { Lexing.pos_lnum=l0; Lexing.pos_bol=b0; Lexing.pos_cnum=c0 } = pos0 in
    let { Lexing.pos_lnum=l1; Lexing.pos_bol=b1; Lexing.pos_cnum=c1 } = pos1 in
    if c0 <= c1 then
        { loc_fname = !parser_ctx_file; loc_line0 = l0;
          loc_pos0 = c0 - b0+1; loc_line1 = l1; loc_pos1 = c1 - b1+1 }
    else
        { loc_fname = !parser_ctx_file; loc_line0 = l1;
          loc_pos0 = c1 - b1+1; loc_line1 = l0; loc_pos1 = c0 - b0+1 }

let curr_loc() = make_loc(Parsing.symbol_start_pos(), Parsing.symbol_end_pos())
let curr_loc_n n = make_loc((Parsing.rhs_start_pos n), (Parsing.rhs_end_pos n))

let make_ident i loc = ExpIdent(i, (make_new_typ(), loc))
let make_typed_ident i t loc = ExpIdent(i, (t, loc))
let make_new_ctx () = (make_new_typ(), curr_loc())

let make_bin_op(op, a, b) = ExpBinary(op, a, b, make_new_ctx())
let make_un_op(op, a) = ExpUnary(op, a, make_new_ctx())
let make_int_lit i n = ExpLit((LitInt i), (TypInt, curr_loc_n n))

let expseq2exp eseq n = match eseq with
    | [] -> ExpNop (curr_loc_n n)
    | e::[] -> e
    | _ -> ExpSeq(eseq, (make_new_typ(), (curr_loc_n n)))

let expseq2exp_loc eseq loc = match eseq with
    | [] -> ExpNop loc
    | e::[] -> e
    | _ -> ExpSeq(eseq, (make_new_typ(), loc))

let exp2expseq e = match e with
    | ExpNop _ -> []
    | ExpSeq(eseq, _) -> eseq
    | _ -> e :: []

let explist2exp el =
    match el with
    | e :: [] -> e
    | _ ->
        let floc = get_exp_loc (List.hd el) in
        let lloc = get_exp_loc (Utils.last_elem el) in
        let loc = loclist2loc [floc; lloc] floc in
        ExpMkTuple(el, (make_new_typ(), loc))

let raise_syntax_err msg =
    raise (SyntaxError (msg, Parsing.symbol_start_pos(), Parsing.symbol_end_pos()))

let raise_syntax_err_loc loc msg =
    raise (SyntaxErrorLoc (msg, loc))

type parser_fun_arg_t =
    | PrFunArgPat of pat_t
    | PrFunArgKW of id_t * typ_t * lit_t option

let plist2exp pl prefix loc =
    let rec plist2exp_ plist0 = List.fold_right (fun p (plist, elist) ->
        let (p_, e_) = pat2exp_ p in (p_::plist, e_::elist)) plist0 ([], [])
    and pat2exp_ p =
        match p with
        | PatAny(loc) ->
            let arg_id = gen_temp_id prefix in
            (PatIdent(arg_id, loc), (make_ident arg_id loc))
        | PatIdent(i, loc) -> (p, (make_ident i loc))
        | PatAs(p, i, loc) -> (p, (make_ident i loc))
        | PatTyped(p, t, loc) ->
            let (p, e) = pat2exp_ p in
            (PatTyped(p, t, loc), e)
        | _ ->
            let loc = get_pat_loc p in
            let arg_id = gen_temp_id prefix in
            (PatAs(p, arg_id, loc), (make_ident arg_id loc))
        in
    let (plist, elist) = plist2exp_ pl in
    let match_arg = match elist with
        | e :: [] -> e
        | _ -> ExpMkTuple(elist, (make_new_typ(), loc)) in
    (plist, match_arg)

let good_variant_name s =
    let c0 = String.get s 0 in
    ('A' <= c0 && c0 <= 'Z') || (String.contains s '.')

let make_variant_type (targs, tname) var_elems0 is_record is_mod_typ =
    let (pos0, pos1) = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) in
    let loc = make_loc(pos0, pos1) in
    let var_elems = List.map (fun (n, t) ->
        let nstr = pp_id2str n in
        if is_record || (good_variant_name nstr) then (n, t) else
        raise_syntax_err_loc loc (sprintf
            "syntax error: variant tag '%s' does not start with a capital latin letter" nstr))
        var_elems0 in
    let dv = { dvar_name=tname; dvar_templ_args=targs; dvar_alias=make_new_typ();
               dvar_flags={(default_variant_flags()) with var_flag_record=is_record;
               var_flag_object=is_mod_typ};
               dvar_cases=var_elems; dvar_ctors=[];
               dvar_templ_inst=[]; dvar_scope=ScGlobal::[]; dvar_loc=loc } in
    DefVariant (ref dv)

let make_deffun fname args rt body flags loc =
    let (args, argtp, kwargs) = List.fold_left (fun (args, argtp, kwargs) f_arg ->
        match f_arg with
        | PrFunArgPat(p) ->
            if kwargs = [] then (p :: args, (make_new_typ()) :: argtp, kwargs)
            else raise_syntax_err "keyword and positional parameters cannot be interleaved"
        | PrFunArgKW(i, t, defval) ->
            (args, argtp, (i, t, defval) :: kwargs))
        ([], [], []) args
        in
    let (args, argtp, body) = if kwargs = [] then (args, argtp, body) else
        let recarg = gen_temp_id "__kwargs__" in
        let kwargs = List.rev kwargs in
        let rectyp = TypRecord (ref (kwargs, true)) in
        let recpat = PatRecord(None, List.map (fun (i, _, _) -> (i, PatIdent(i, loc))) kwargs, loc) in
        let unpack_rec = DefVal(recpat, (make_ident recarg loc), default_tempval_flags(), loc) in
        let body_ctx = get_exp_ctx body in
        let body_seq = exp2expseq body in
        let body = ExpSeq(unpack_rec :: body_seq, body_ctx) in
        (PatTyped(PatIdent(recarg, loc), rectyp, loc) :: args,
            (make_new_typ()) :: argtp, body)
        in
    let df = DefFun (ref { df_name=fname; df_templ_args=[]; df_args=(List.rev args);
        df_typ=TypFun((List.rev argtp), rt); df_body=body;
        df_flags={flags with fun_flag_has_keywords = (kwargs != [])};
        df_scope=ScGlobal :: []; df_loc=loc;
        df_templ_inst=[]; df_env=Env.empty }) in
    [df]

let make_pmatch_deffun (flags, fname) (args, rt) pmatch_clauses args_pos pmatch_pos =
    let args_pl = List.fold_right (fun fun_arg args_pl ->
            match fun_arg with
            | PrFunArgPat(p) -> p :: args_pl
            | PrFunArgKW _ -> raise_syntax_err_loc (curr_loc_n args_pos)
                "match-based function may not use keyword arguments. Use explicit match {} expression")
            args []
        in
    let (args_upd, match_arg) = plist2exp args_pl "arg" (curr_loc_n args_pos) in
    let args_upd = List.map (fun p -> PrFunArgPat p) args_upd in
    let match_e = ExpMatch(match_arg, (List.rev pmatch_clauses), make_new_ctx()) in
    make_deffun fname args_upd rt match_e flags (curr_loc())

let forever_for(n) =
    let loc = curr_loc_n n in
    ((PatAny loc), (PatAny loc), ExpRange(Some(ExpLit(LitInt 0L, (TypInt, loc))), None, None, (make_new_typ(), loc))) :: []

let process_for_clauses for_cl loc =
    List.fold_left (fun (for_cl_, idx_pat) (p, idxp, e) ->
        let idx_pat = match (idxp, idx_pat) with
            | (PatAny _, idx_pat) -> idx_pat
            | (_, PatAny _) -> idxp
            | _ -> raise_syntax_err_loc (get_pat_loc idxp)
                "@ is used more than once, which does not make sence and is not supported"
            in
        (((p, e) :: for_cl_), idx_pat)) ([], PatAny loc) for_cl

type for_body_t = ForBody of exp_t | ForMatchBody of (pat_t list * exp_t) list * loc_t

let process_nested_for nested_for body =
    let (glob_el, nested_for_cl) = List.fold_left (fun
        (glob_el, nested_for_cl) (loc, for_cl) ->
        let (glob_el, for_cl_, idx_pat) =
            List.fold_left (fun (glob_el, for_cl_, idx_pat) (p, idxp, e) ->
                let (p_, p_e) = plist2exp [p] "x" (get_pat_loc p) in
                let p = List.hd p_ in
                match (idxp, idx_pat) with
                | (PatAny _, idx_pat) -> (p_e :: glob_el, (p, e) :: for_cl_, idx_pat)
                | (_, PatAny _) ->
                    let (idxp_, idxp_e) = plist2exp [idxp] "i" (get_pat_loc idxp) in
                    (p_e :: idxp_e :: glob_el, (p, e) :: for_cl_, idxp)
                | _ -> raise_syntax_err_loc (get_pat_loc idxp)
                "@ is used more than once, which does not make sence and is not supported")
            (glob_el, [], PatAny loc) for_cl
            in
        (glob_el, (for_cl_, idx_pat) :: nested_for_cl)) ([], []) nested_for
        in
    let for_iter_e = explist2exp glob_el in
    let body =
        match body with
        | ForBody body -> body
        | ForMatchBody (pmatch_clauses, clauses_loc) ->
            ExpMatch(for_iter_e, (List.rev pmatch_clauses),
                    (make_new_typ(), clauses_loc))
        in
    (for_iter_e, nested_for_cl, body)

let make_for nested_for body flags =
    let (_, for_e) = List.fold_right (fun (for_cl, idx_pat) (idx, for_e) ->
        let curr_flags = {flags with for_flag_nested = idx > 0} in
        let (p, _) = List.hd for_cl in
        let loc = get_pat_loc p in
        (idx-1, ExpFor(for_cl, idx_pat, for_e, curr_flags, loc)))
        nested_for ((List.length nested_for) - 1, body)
    in for_e

let transform_fold_exp special fold_pat fold_pat_n fold_init_exp nested_fold fold_body =
    let acc_loc = get_pat_loc fold_pat in
    let fr_id = get_fold_result() in
    let fr_exp = make_ident fr_id acc_loc in
    let (for_iter_e, nested_fold_cl, fold_body) = process_nested_for nested_fold fold_body in
    let body_loc = get_exp_loc fold_body in
    let body_end_loc = get_end_loc body_loc in
    let void_ctx = (TypVoid, body_loc) in
    let bool_ctx = (TypBool, body_loc) in
    let (fr_decl, new_body, fr_exp) =
        match special with
        | "" ->
            (* `fold p=e0 for ... e1`
                    is transformed to
                `{
                var __fold_result__ = e0
                for ... { val p=__fold_result__; __fold_result__ = e1 }
                __fold_result__
                }`
            *)
            let fr_decl = DefVal(PatIdent(fr_id, acc_loc),
                fold_init_exp, default_var_flags(), acc_loc) in
            let acc_decl = DefVal(fold_pat, fr_exp, default_tempval_flags(), acc_loc) in
            let update_fr = ExpAssign(fr_exp, fold_body, body_loc) in
            let new_body = ExpSeq([acc_decl; update_fr], void_ctx) in
            (fr_decl, new_body, fr_exp)
        | "all" | "any" ->
            let is_any = special = "any" in
            let fr_decl = DefVal(PatIdent(fr_id, acc_loc),
                ExpLit(LitBool(not is_any), (TypBool, acc_loc)),
                default_var_flags(), acc_loc) in
            let break_exp = ExpSeq([ExpAssign(fr_exp, ExpLit(LitBool(is_any), bool_ctx), body_loc);
                                    ExpBreak(true, body_loc)], void_ctx) in
            let predicate_exp = if is_any then fold_body else
                ExpUnary(OpLogicNot, fold_body, bool_ctx) in
            let new_body = ExpIf(predicate_exp, break_exp, ExpNop(body_loc), void_ctx) in
            (fr_decl, new_body, fr_exp)
        | "find" | "find_opt" ->
            let none = get_id "None" in
            let some = get_id "Some" in
            let fr_decl = DefVal(PatIdent(fr_id, acc_loc), (make_ident none acc_loc),
                default_var_flags(), acc_loc) in
            let mksome_exp = ExpCall((make_ident some body_loc),
                [for_iter_e], (make_new_typ(), body_loc)) in
            let break_exp = ExpSeq([ExpAssign(fr_exp, mksome_exp, body_loc);
                                    ExpBreak(true, body_loc)], void_ctx) in
            let new_body = ExpIf(fold_body, break_exp, ExpNop(body_loc), void_ctx) in
            let new_fr_exp = match special with
                | "find" ->
                    let x = get_id "x" in
                    let some_pattern_clause =
                        ([PatVariant(some, [PatIdent(x, body_end_loc)], body_end_loc)],
                        (make_ident x body_end_loc)) in
                    let else_pattern_clause = ([PatAny body_end_loc], ExpThrow((make_typed_ident
                        (get_id "NotFoundError") TypExn body_end_loc), body_end_loc)) in
                    ExpMatch(fr_exp, [some_pattern_clause; else_pattern_clause],
                        (make_new_typ(), body_end_loc))
                | _ -> fr_exp
                in
            (fr_decl, new_body, new_fr_exp)
        | f ->
            raise_syntax_err_loc acc_loc (sprintf "unknown fold variation '%s'" f)
        in
    let for_exp = make_for nested_fold_cl new_body {(default_for_flags()) with for_flag_fold=true} in
    ExpSeq([fr_decl; for_exp; fr_exp], (make_new_typ(), curr_loc()))

let make_chained_cmp chain = match chain with
    | (_, e1) :: [] -> e1
    | (cmpop, e2) :: (_, e1) :: [] -> ExpBinary(cmpop, e1, e2, (TypBool, curr_loc()))
    | _ ->
        (*
            `first_e cmpop1 e1 cmpop2 ... last_cmpop last_e`
            is transformed to
            { val t1=e1
              ...
              val tn=en
              first_e cmpop1 t1 && t1 cmpop2 t2 ... && tn last_cmpop last_e
            }
            if ej is a literal or identifier, it does not need tj, it's used as-is
        *)
        let (last_cmpop, last_e) = List.hd chain in
        let chain = List.rev (List.tl chain) in
        let (_, first_e) = List.hd chain in
        let chain = List.tl chain in
        let (chain, code) = List.fold_left (fun (chain, code) (cmpop, e) ->
            let (new_e, code) = match e with
                    | (ExpLit _) | (ExpIdent _) -> (e, code)
                    | _ ->
                        let e_loc = get_exp_loc e in
                        let tmp_id = gen_temp_id "t" in
                        let tmp_decl = DefVal(PatIdent(tmp_id, e_loc), e,
                            (default_tempval_flags()), e_loc) in
                        ((make_ident tmp_id e_loc), (tmp_decl :: code))
            in (((cmpop, new_e) :: chain), code)) ([], []) (List.rev chain) in
        let rec process_chain result a chain =
            let (cmpop, b, rest) = match chain with
                | (cmpop, b) :: rest -> (cmpop, b, rest)
                | _ -> (last_cmpop, last_e, [])
                in
            let cmp_e_loc = loclist2loc [get_exp_loc a] (get_exp_loc b) in
            let cmp_e = ExpBinary(cmpop, a, b, (TypBool, cmp_e_loc)) in
            let result = match result with
                | ExpNop _ -> cmp_e
                | _ ->
                    let result_loc = loclist2loc [get_exp_loc result] cmp_e_loc in
                    ExpBinary(OpBitwiseAnd, result, cmp_e, (TypBool, result_loc))
                in
            if chain = [] then result else process_chain result b rest
        in
        let chained_cmp_e = process_chain (ExpNop noloc) first_e chain in
        expseq2exp (code @ (chained_cmp_e :: [])) 1

(*
    try {
        block
    } finally {
        final_code
    }

    is converted to

    try {
        val v = block
        final_code
        v
    } catch {
        | e => final_code; throw e
    }

    i.e. the 'final_code' is duplicated twice, but normally this is fine.
    [TODO] It's possible to avoid this duplication by using extra data type and
    more complex construction:

    type 't finally_t = FinallyOk: 't | FinallyExn: exn // this can be put into Builtins.fx
    val v = try {
        FinallyOk(block)
    } catch {
        e => FinallyExn(e)
    }
    final_code
    match v {
    | FinallyOK(v) => v
    | FinallyExn(e) => throw e
    }
*)
let make_finally e final_e loc =
    let eloc = get_exp_loc e in
    let fe_loc = get_exp_loc final_e in
    let ll = loclist2loc [eloc; fe_loc] eloc in
    let tmp = gen_temp_id "v" in
    let def_tmp = DefVal(PatIdent(tmp, eloc), e, (default_tempval_flags()), loc) in
    let try_block = (def_tmp :: (exp2expseq final_e)) @ [make_ident tmp loc] in
    let try_block = expseq2exp_loc try_block loc in
    let some_exn = gen_temp_id "e" in
    let some_exn_pat = PatIdent(some_exn, fe_loc) in
    let rethrow_exn = ExpThrow((make_typed_ident some_exn TypExn fe_loc), fe_loc) in
    let catch_block = (exp2expseq (dup_exp final_e)) @ [rethrow_exn] in
    let catch_block = expseq2exp_loc catch_block fe_loc in
    ExpTryCatch(try_block, [([some_exn_pat], catch_block)], (make_new_typ(), ll))

%}

%token TRUE FALSE
%token <int64> INT
%token <int * int64> SINT
%token <int * int64> UINT
%token <int * float> FLOAT
%token <string> FLOAT_LIKE
%token <string> IDENT
%token <string> B_IDENT
%token <string> STRING
%token <string> CHAR
%token <string> TYVAR

/* keywords */
%token AS BREAK CATCH CCODE CLASS CONTINUE DATA DO ELSE EXCEPTION EXTENDS
%token FINALLY FOLD B_FOR FOR FROM FUN IF IMPLEMENTS B_IMPORT IMPORT INLINE INTERFACE
%token MATCH NOTHROW OBJECT OPERATOR PARALLEL PRAGMA PRIVATE PURE REF REF_TYPE
%token THROW TRY TYPE VAL VAR WHEN B_WHILE WHILE WITH UNZIP

/* reserved/internal-use keywords */
%token FOLD_RESULT

/* parens/delimiters */
%token B_LPAREN LPAREN STR_INTERP_LPAREN RPAREN B_LSQUARE LSQUARE RSQUARE LBRACE RBRACE LLIST RLIST
%token COMMA DOT SEMICOLON COLON BAR BACKSLASH QUESTION ARROW DOUBLE_ARROW BACK_ARROW EOF AT ELLIPSIS

/* operations */
%token B_MINUS MINUS B_PLUS PLUS
%token B_STAR STAR SLASH PERCENT
%token B_POWER POWER SHIFT_RIGHT SHIFT_LEFT
%token B_DOT_MINUS DOT_STAR DOT_SLASH DOT_PERCENT DOT_POWER
%token BITWISE_AND BITWISE_OR BITWISE_XOR TILDE
%token APOS CONS CAST EXPAND
%token LOGICAL_AND LOGICAL_OR LOGICAL_NOT
%token EQUAL PLUS_EQUAL MINUS_EQUAL STAR_EQUAL SLASH_EQUAL DOT_EQUAL PERCENT_EQUAL
%token AND_EQUAL OR_EQUAL XOR_EQUAL SHIFT_LEFT_EQUAL SHIFT_RIGHT_EQUAL
%token DOT_STAR_EQUAL DOT_SLASH_EQUAL DOT_PERCENT_EQUAL
%token CMP_EQ CMP_NE CMP_LE CMP_GE CMP_LT CMP_GT SPACESHIP SAME
%token DOT_CMP_EQ DOT_CMP_NE DOT_CMP_LE DOT_CMP_GE DOT_CMP_LT DOT_CMP_GT DOT_SPACESHIP

%right SEMICOLON
%left COMMA
%right DOUBLE_ARROW
%left BAR
%right THROW
%right EQUAL PLUS_EQUAL MINUS_EQUAL STAR_EQUAL SLASH_EQUAL DOT_EQUAL MOD_EQUAL AND_EQUAL OR_EQUAL XOR_EQUAL SHIFT_LEFT_EQUAL SHIFT_RIGHT_EQUAL DOT_STAR_EQUAL DOT_SLASH_EQUAL DOT_PERCENT_EQUAL
%left WHEN
%right CONS
%left LOGICAL_OR
%left LOGICAL_AND
%left COLON CAST
%left BITWISE_OR
%left BITWISE_XOR
%left BITWISE_AND
%left AS
%left CMP_EQ CMP_NE CMP_LE CMP_GE CMP_LT CMP_GT SAME
%left SPACESHIP
%left DOT_CMP_EQ DOT_CMP_NE DOT_CMP_LE DOT_CMP_GE DOT_CMP_LT DOT_CMP_GT DOT_SPACESHIP
%left SHIFT_LEFT SHIFT_RIGHT
%left PLUS MINUS
%left STAR SLASH PERCENT DOT_STAR DOT_SLASH DOT_PERCENT
%right POWER DOT_POWER
%left BACKSLASH
%right B_MINUS B_DOT_MINUS B_PLUS TILDE LOGICAL_NOT REF EXPAND
%left APOS
%right deref_prec
%right ARROW
%left lsquare_prec fcall_prec
%left app_type_prec arr_type_prec option_type_prec ref_type_prec
%left DOT

%type <Ast.exp_t list> ficus_module
%start ficus_module

%%

ficus_module:
| top_level_seq_ { List.rev $1 }
| top_level_seq_ SEMICOLON { List.rev $1 }
| /* empty */ { [] }

top_level_seq_:
| top_level_seq_ top_level_exp { $2 @ $1 }
| top_level_seq_ SEMICOLON top_level_exp { $3 @ $1 }
| top_level_exp { (List.rev $1) }

top_level_exp:
| stmt { $1 :: [] }
| decl { $1 }
| ccode_exp { ExpCCode($1, (TypVoid, curr_loc())) :: [] }
| PRAGMA string_list { DirPragma ((List.rev $2), curr_loc()) :: [] }
| B_IMPORT module_name_list_
    {
        let (pos0, pos1) = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) in
        [DirImport ((List.map (fun (a, b) ->
        let a1 = add_to_imported_modules a (pos0, pos1) in (a1, b)) $2), curr_loc())]
    }
| FROM dot_ident any_import B_STAR
    {
        let (pos0, pos1) = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) in
        let a = get_id $2 in
        let a1 = add_to_imported_modules a (pos0, pos1) in
        [DirImportFrom (a1, [], curr_loc())]
    }
| FROM dot_ident any_import ident_list_
    {
        let (pos0, pos1) = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) in
        let a = get_id $2 in
        let a1 = add_to_imported_modules a (pos0, pos1) in
        [DirImportFrom (a1, (List.rev $4), curr_loc())]
    }
| error
    { raise_syntax_err "syntax error: unrecognized/unsupported expression, check punctuation and parentheses" }

exp_seq_:
| exp_seq_ stmt { $2 :: $1 }
| exp_seq_ decl { $2 @ $1 }
| exp_seq_ SEMICOLON stmt { $3 :: $1 }
| exp_seq_ SEMICOLON decl { $3 @ $1 }
| stmt { $1 :: [] }
| decl { $1 }

decl:
| val_spec_list_ val_decls_
    {
        let vflags = $1 in
        List.map (fun (p, e, ctx) -> DefVal(p, e, vflags, ctx)) $2
    }
| fun_decl_start fun_args EQUAL stmt_or_ccode
    {
        let (flags, fname) = $1 in
        let (args, rt) = $2 in
        let body = expseq2exp [$4] 4 in
        make_deffun fname args rt body flags (curr_loc())
    }
| fun_decl_start fun_args block
    {
        let (flags, fname) = $1 in
        let (args, rt) = $2 in
        let body = expseq2exp (exp2expseq $3) 3 in
        make_deffun fname args rt body flags (curr_loc())
    }
| fun_decl_start fun_args LBRACE BAR pattern_matching_clauses_ RBRACE
    {
        make_pmatch_deffun $1 $2 $5 2 5
    }
| simple_type_decl { [$1] }
| exception_decl { [$1] }
| CLASS type_lhs implemented_ifaces constr_args LBRACE class_members RBRACE
    {
        let (cl_templ_args, cl_name) = $2 in
        let cl_ifaces = List.rev $3 in
        let cl_args = $4 in
        let cl_def = { dcl_name=cl_name; dcl_templ_args=cl_templ_args;
                       dcl_typ=TypApp([], cl_name); dcl_ifaces=cl_ifaces; dcl_args=cl_args;
                       dcl_members=$6; dcl_templ_inst=[]; dcl_scope=ScGlobal::[]; dcl_loc=curr_loc() } in
        [DefClass(ref cl_def)]
    }
| INTERFACE B_IDENT base_iface LBRACE iface_members RBRACE
    {
        let iname = get_id $2 in
        let base_iface = $3 in
        let iface_def = { di_name=iname; di_base=base_iface; di_members=[]; di_scope=ScGlobal::[]; di_loc=curr_loc() } in
        [DefInterface(ref iface_def)]
    }

simple_type_decl:
| typ_attr type_lhs EQUAL typespec_or_record
    {
        let (targs, i) = $2 in
        let dt_body = $4 in
        match dt_body with
        | TypRecord _ ->
            make_variant_type (targs, i) ((i, dt_body) :: []) true $1
        | _ ->
            if $1 = noid then () else
                raise_syntax_err "type alias cannot be an object type, only a record or variant could be";
            let dt = { dt_name=i; dt_templ_args=targs; dt_typ=$4; dt_finalized=false;
                       dt_scope=ScGlobal :: []; dt_loc=curr_loc() } in
            DefTyp (ref dt)
    }
| typ_attr type_lhs EQUAL variant_decl_start BITWISE_OR variant_elems_
    {
        make_variant_type $2 (($4, TypVoid) :: (List.rev $6)) false $1
    }
| typ_attr type_lhs EQUAL variant_decl_start COLON typespec_or_record BITWISE_OR variant_elems_
    {
        make_variant_type $2 (($4, $6) :: (List.rev $8)) false $1
    }
| typ_attr type_lhs EQUAL variant_decl_start COLON typespec_or_record
    {
        make_variant_type $2 (($4, $6) :: []) false $1
    }

typ_attr:
| TYPE { noid }
| OBJECT TYPE { !parser_ctx_module }

variant_decl_start:
| B_IDENT { get_id $1 }
| BITWISE_OR B_IDENT { get_id $2 }

exception_decl:
| EXCEPTION B_IDENT
    {
        DefExn(ref { dexn_name=(get_id $2); dexn_typ=TypVoid; dexn_scope=ScGlobal :: []; dexn_loc=curr_loc() })
    }
| EXCEPTION B_IDENT COLON typespec_or_record
    {
        DefExn(ref { dexn_name=(get_id $2); dexn_typ = $4; dexn_scope=ScGlobal :: []; dexn_loc=curr_loc() })
    }

stmt:
| BREAK { ExpBreak (false, curr_loc()) }
| CONTINUE { ExpContinue(curr_loc()) }
| THROW exp { ExpThrow($2, curr_loc()) }
| deref_exp EQUAL complex_exp { ExpAssign($1, $3, curr_loc()) }
| deref_exp aug_op complex_exp
    {
        let (tp, loc) = make_new_ctx() in
        ExpAssign($1, ExpBinary($2, $1, $3, (tp, loc)), loc)
    }
| deref_exp DOT_EQUAL LBRACE id_exp_list_ RBRACE
    {
        let (tp, loc) = make_new_ctx() in
        ExpAssign($1, ExpUpdateRecord($1, (List.rev $4), (tp, loc)), loc)
    }
| B_WHILE exp_or_block block { ExpWhile($2, $3, curr_loc()) }
| DO block any_while exp_or_block { ExpDoWhile($2, $4, curr_loc()) }
| for_flags B_FOR nested_for_ for_block
    {
        let (_, nested_for_cl, body) = process_nested_for $3 $4 in
        make_for nested_for_cl body $1
    }
| complex_exp { $1 }

ccode_exp:
| CCODE STRING { $2 }

stmt_or_ccode:
| stmt { $1 }
| ccode_exp { ExpCCode($1, make_new_ctx()) }

simple_exp:
| B_IDENT { make_ident (get_id $1) (curr_loc()) }
| B_LPAREN op_name RPAREN { make_ident $2 (curr_loc()) }
| literal
    {
        let lit = $1 in
        let typ = get_lit_typ lit in
        ExpLit(lit, (typ, curr_loc()))
    }
| simple_exp DOT B_IDENT { ExpMem($1, (make_ident (get_id $3) (curr_loc_n 3)), make_new_ctx()) }
| simple_exp DOT INT { ExpMem($1, (make_int_lit $3 3), make_new_ctx()) }
| simple_exp DOT FLOAT_LIKE %prec DOT
    {
        let ab = String.split_on_char '.' $3 in
        let a = Int64.of_string (List.nth ab 0) in
        let b = Int64.of_string (List.nth ab 1) in
        let mem1 = ExpMem($1, (make_int_lit a 3), make_new_ctx()) in
        ExpMem(mem1, (make_int_lit b 3), make_new_ctx())
    }
| simple_exp DOT LBRACE id_exp_list_ RBRACE { ExpUpdateRecord($1, (List.rev $4), make_new_ctx()) }
| simple_exp ARROW B_IDENT %prec DOT {
    ExpMem(ExpUnary(OpDeref, $1, (make_new_typ(), curr_loc_n 1)),
        (make_ident (get_id $3) (curr_loc_n 3)), make_new_ctx()) }
| simple_exp ARROW INT %prec DOT
    {
        let deref_exp = ExpUnary(OpDeref, $1, (make_new_typ(), curr_loc_n 1)) in
        ExpMem(deref_exp, (make_int_lit $3 3), make_new_ctx())
    }
| simple_exp ARROW FLOAT_LIKE %prec DOT
    {
        let ab = String.split_on_char '.' $3 in
        let a = Int64.of_string (List.nth ab 0) in
        let b = Int64.of_string (List.nth ab 1) in
        let deref_exp = ExpUnary(OpDeref, $1, (make_new_typ(), curr_loc_n 1)) in
        let mem1 = ExpMem(deref_exp, (make_int_lit a 3), make_new_ctx()) in
        ExpMem(mem1, (make_int_lit b 3), make_new_ctx())
    }
| simple_exp ARROW LBRACE id_exp_list_ RBRACE %prec DOT {
    ExpUpdateRecord(ExpUnary(OpDeref, $1, (make_new_typ(), curr_loc_n 1)),
        (List.rev $4), make_new_ctx()) }
| B_LPAREN complex_exp RPAREN { $2 }
| B_LPAREN block RPAREN { $2 }
| B_LPAREN complex_exp COMMA exp_list RPAREN { ExpMkTuple(($2 :: $4), make_new_ctx()) }
| B_LPAREN typed_exp RPAREN { $2 }
| B_LPAREN B_FOR nested_for_ block RPAREN
    {
        let (_, map_clauses, body) = process_nested_for $3 (ForBody $4) in
        ExpMap(map_clauses, body, {(default_for_flags()) with for_flag_make=ForMakeTuple}, make_new_ctx())
    }
| B_LSQUARE for_flags B_FOR nested_for_ for_block RSQUARE
    {
        let (_, map_clauses, body) = process_nested_for $4 $5 in
        ExpMap(map_clauses, body, {$2 with for_flag_make=ForMakeArray}, make_new_ctx())
    }
| LLIST for_flags B_FOR nested_for_ for_block RLIST
    {
        let (_, map_clauses, body) = process_nested_for $4 $5 in
        ExpMap(map_clauses, body, {$2 with for_flag_make=ForMakeList}, make_new_ctx())
    }
| B_LSQUARE array_elems_ RSQUARE
    {
        let ae = List.rev $2 in
        ExpMkArray(ae, make_new_ctx())
    }
| LLIST complex_exp COMMA exp_list RLIST
    {
        let l = List.rev ($2 :: $4) in
        let e0 = ExpLit(LitNil, ((TypList (make_new_typ())), curr_loc_n 5)) in
        List.fold_left (fun e i -> make_bin_op(OpCons, i, e)) e0 l
    }
| LLIST complex_exp RLIST
    {
        let l = $2 :: [] in
        let e0 = ExpLit(LitNil, ((TypList (make_new_typ())), curr_loc_n 3)) in
        List.fold_left (fun e i -> make_bin_op(OpCons, i, e)) e0 l
    }
| simple_exp LPAREN B_FOR nested_for_ for_block RPAREN
    %prec fcall_prec
    {
        match $1 with
        | ExpIdent(i, _) ->
            transform_fold_exp (pp_id2str i) (PatAny(curr_loc_n 1)) 1 (ExpNop (curr_loc())) $4 $5
        | _ -> raise_syntax_err
            ("unsupported for-comprehension processing expression " ^
            "(no regular function can be applied to a for-loop)")
    }
| simple_exp LPAREN actual_args RPAREN
    %prec fcall_prec
    { ExpCall($1, $3, make_new_ctx()) }
| simple_exp LSQUARE idx_list_ RSQUARE
    %prec lsquare_prec
    { ExpAt($1, BorderNone, InterpNone, (List.rev $3), make_new_ctx()) }

for_block:
| block { ForBody $1 }
| LBRACE BAR pattern_matching_clauses_ RBRACE { ForMatchBody($3, curr_loc_n 1) }

deref_exp:
| simple_exp { $1 }
| B_STAR deref_exp %prec deref_prec { make_un_op(OpDeref, $2) }
| B_POWER deref_exp %prec deref_prec { make_un_op(OpDeref, make_un_op(OpDeref, $2)) }

apos_exp:
| apos_exp APOS { make_un_op(OpApos, $1) }
| deref_exp { $1 }

for_flags:
| for_flags PARALLEL { {$1 with for_flag_parallel=true} }
| for_flags UNZIP { {$1 with for_flag_unzip=true} }
| /* empty */ { default_for_flags() }

complex_exp:
| IF elif_seq {
    let rec make_if elif_seq else_exp = match elif_seq with
        | (c, a) :: rest ->
            let if_loc = loclist2loc [get_exp_loc c] (get_exp_loc else_exp) in
            let new_else = ExpIf(c, a, else_exp, (make_new_typ(), if_loc)) in
            make_if rest new_else
        | _ -> else_exp
    in
    let (elif_seq, else_exp) = $2 in make_if elif_seq else_exp }
| TRY exp_or_block CATCH LBRACE pattern_matching_clauses_with_opt_bar RBRACE
{
    ExpTryCatch ($2, $5, make_new_ctx())
}
| TRY exp_or_block CATCH LBRACE pattern_matching_clauses_with_opt_bar RBRACE FINALLY block
{
    let loc = curr_loc() in
    make_finally (ExpTryCatch ($2, $5, make_new_ctx())) $8 loc
}
| TRY exp_or_block FINALLY block
{
    make_finally $2 $4 (curr_loc())
}
| MATCH exp_or_block LBRACE pattern_matching_clauses_with_opt_bar RBRACE
    {
        ExpMatch ($2, $4, make_new_ctx())
    }
| FOLD fold_clause for_block
    {
        let ((fold_pat, fold_init_exp), fold_cl) = $2 in
        transform_fold_exp "" fold_pat 2 fold_init_exp fold_cl $3
    }
| FUN fun_args block
    {
        let (args, rt) = $2 in
        let body = expseq2exp (exp2expseq $3) 3 in
        let fname = gen_temp_id "lambda" in
        let df = make_deffun fname args rt body (default_fun_flags()) (curr_loc()) in
        ExpSeq(df @ [make_ident fname (curr_loc_n 1)], make_new_ctx())
    }
| FUN fun_args LBRACE BAR pattern_matching_clauses_ RBRACE
    {
        let fname = gen_temp_id "lambda" in
        let df = make_pmatch_deffun (default_fun_flags(), fname) $2 $5 2 5 in
        ExpSeq(df @ [make_ident fname (curr_loc_n 1)], make_new_ctx())
    }
| simple_exp LBRACE id_exp_list_ RBRACE
    %prec fcall_prec
    { ExpMkRecord($1, (List.rev $3), make_new_ctx()) }
| simple_exp LBRACE RBRACE
    %prec fcall_prec
    { ExpMkRecord($1, [], make_new_ctx()) }
| exp { $1 }

typed_exp:
| complex_exp COLON typespec { ExpTyped($1, $3, make_new_ctx()) }
| complex_exp CAST typespec { ExpCast($1, $3, make_new_ctx()) }

unary_exp:
| apos_exp { $1 }
| REF unary_exp { make_un_op(OpMkRef, $2) }
| B_MINUS unary_exp { make_un_op(OpNegate, $2) }
| B_PLUS unary_exp { make_un_op(OpPlus, $2) }
| TILDE unary_exp { make_un_op(OpBitwiseNot, $2) }
| EXPAND unary_exp { make_un_op(OpExpand, $2) }

binary_exp:
| binary_exp PLUS binary_exp { make_bin_op(OpAdd, $1, $3) }
| binary_exp MINUS binary_exp { make_bin_op(OpSub, $1, $3) }
| binary_exp STAR binary_exp { make_bin_op(OpMul, $1, $3) }
| binary_exp SLASH binary_exp { make_bin_op(OpDiv, $1, $3) }
| binary_exp PERCENT binary_exp { make_bin_op(OpMod, $1, $3) }
| binary_exp POWER binary_exp { make_bin_op(OpPow, $1, $3) }
| binary_exp SHIFT_LEFT binary_exp { make_bin_op(OpShiftLeft, $1, $3) }
| binary_exp SHIFT_RIGHT binary_exp { make_bin_op(OpShiftRight, $1, $3) }
| binary_exp BITWISE_AND binary_exp { make_bin_op(OpBitwiseAnd, $1, $3) }
| binary_exp BITWISE_OR binary_exp { make_bin_op(OpBitwiseOr, $1, $3) }
| binary_exp BITWISE_XOR binary_exp { make_bin_op(OpBitwiseXor, $1, $3) }
| binary_exp CONS binary_exp { make_bin_op(OpCons, $1, $3) }
| binary_exp DOT_STAR binary_exp { make_bin_op(OpDotMul, $1, $3) }
| binary_exp DOT_SLASH binary_exp { make_bin_op(OpDotDiv, $1, $3) }
| binary_exp DOT_PERCENT binary_exp { make_bin_op(OpDotMod, $1, $3) }
| binary_exp DOT_POWER binary_exp { make_bin_op(OpDotPow, $1, $3) }
| binary_exp SPACESHIP binary_exp { make_bin_op(OpSpaceship, $1, $3) }
| binary_exp SAME binary_exp {
    ExpCall((make_ident (get_id "__same__") (curr_loc_n 2)),
            [$1; $3], make_new_ctx()) }
| binary_exp DOT_SPACESHIP binary_exp { make_bin_op(OpDotSpaceship, $1, $3) }
| binary_exp DOT_CMP_EQ binary_exp { make_bin_op(OpDotCmp(CmpEQ), $1, $3) }
| binary_exp DOT_CMP_NE binary_exp { make_bin_op(OpDotCmp(CmpNE), $1, $3) }
| binary_exp DOT_CMP_LE binary_exp { make_bin_op(OpDotCmp(CmpLE), $1, $3) }
| binary_exp DOT_CMP_GE binary_exp { make_bin_op(OpDotCmp(CmpGE), $1, $3) }
| binary_exp DOT_CMP_LT binary_exp { make_bin_op(OpDotCmp(CmpLT), $1, $3) }
| binary_exp DOT_CMP_GT binary_exp { make_bin_op(OpDotCmp(CmpGT), $1, $3) }
| unary_exp { $1 }

chained_cmp_exp:
| chained_cmp_exp CMP_EQ binary_exp { (OpCmp(CmpEQ), $3) :: $1 }
| chained_cmp_exp CMP_NE binary_exp { (OpCmp(CmpNE), $3) :: $1 }
| chained_cmp_exp CMP_LE binary_exp { (OpCmp(CmpLE), $3) :: $1 }
| chained_cmp_exp CMP_GE binary_exp { (OpCmp(CmpGE), $3) :: $1 }
| chained_cmp_exp CMP_LT binary_exp { (OpCmp(CmpLT), $3) :: $1 }
| chained_cmp_exp CMP_GT binary_exp { (OpCmp(CmpGT), $3) :: $1 }
| binary_exp
    {
        (* the actual operation is not used here; just put some weird one *)
        (OpCons, $1) :: []
    }

exp:
| LOGICAL_NOT exp { make_un_op(OpLogicNot, $2) }
| exp LOGICAL_OR exp { make_bin_op(OpLogicOr, $1, $3) }
| exp LOGICAL_AND exp { make_bin_op(OpLogicAnd, $1, $3) }
| chained_cmp_exp { make_chained_cmp($1) }

exp_or_block:
| exp { $1 }
| block { $1 }

complex_exp_or_block:
| complex_exp { $1 }
| block { $1 }

block:
| LBRACE RBRACE { ExpNop(curr_loc()) }
| LBRACE exp_seq_ RBRACE { expseq2exp (List.rev $2) 2 }
| LBRACE exp_seq_ SEMICOLON RBRACE { expseq2exp (List.rev $2) 2 }

literal:
| INT { LitInt $1 }
| SINT { let (b, v) = $1 in LitSInt (b, v) }
| UINT { let (b, v) = $1 in LitUInt (b, v) }
| FLOAT { let (b, v) = $1 in LitFloat (b, v) }
| FLOAT_LIKE { let v = float_of_string $1 in LitFloat (64, v) }
| STRING { LitString $1 }
| CHAR { LitChar $1 }
| TRUE { LitBool true }
| FALSE { LitBool false }
| B_LSQUARE RSQUARE { LitNil }

module_name_list_:
| module_name_list_ COMMA B_IDENT { let i=get_id $3 in (i, i) :: $1 }
| module_name_list_ COMMA B_IDENT AS B_IDENT { let i = get_id $3 in let j = get_id $5 in (i, j) :: $1 }
| B_IDENT { let i=get_id $1 in (i, i) :: [] }
| B_IDENT AS B_IDENT { let i=get_id $1 in let j = get_id $3 in (i, j) :: [] }

ident_list_:
| ident_list_ COMMA B_IDENT { (get_id $3) :: $1 }
| B_IDENT { (get_id $1) :: [] }

exp_list:
| exp_list_ { List.rev $1 }
| exp_list_ COMMA { List.rev $1 }
| /* empty */ { [] }

exp_list_:
| exp_list_ COMMA complex_exp { $3 :: $1 }
| complex_exp { $1 :: [] }

array_elems_:
| array_elems_ SEMICOLON exp_list_ { (List.rev $3) :: $1 }
| exp_list_ { (List.rev $1) :: [] }

id_exp_list_:
| id_exp_list_ COMMA B_IDENT EQUAL complex_exp { (get_id $3, $5) :: $1 }
| id_exp_list_ COMMA B_IDENT EQUAL complex_exp COMMA { (get_id $3, $5) :: $1 }
| B_IDENT EQUAL complex_exp { (get_id $1, $3) :: [] }
| B_IDENT EQUAL complex_exp COMMA { (get_id $1, $3) :: [] }

op_name:
| APOS { fname_op_apos() }
| PLUS { fname_op_add() }
| MINUS  { fname_op_sub() }
| STAR  { fname_op_mul() }
| SLASH  { fname_op_div() }
| PERCENT  { fname_op_mod() }
| POWER  { fname_op_pow() }
| DOT_STAR  { fname_op_dot_mul() }
| DOT_SLASH  { fname_op_dot_div() }
| DOT_PERCENT  { fname_op_dot_mod() }
| DOT_POWER  { fname_op_dot_pow() }
| SHIFT_LEFT  { fname_op_shl() }
| SHIFT_RIGHT  { fname_op_shr() }
| BITWISE_AND  { fname_op_bit_and() }
| BITWISE_OR   { fname_op_bit_or() }
| BITWISE_XOR  { fname_op_bit_xor() }
| TILDE  { fname_op_bit_not() }
| SPACESHIP  { fname_op_cmp() }
| SAME    { fname_op_same() }
| CMP_EQ  { fname_op_eq() }
| CMP_NE  { fname_op_ne() }
| CMP_LE  { fname_op_le() }
| CMP_GE  { fname_op_ge() }
| CMP_LT  { fname_op_lt() }
| CMP_GT  { fname_op_gt() }
| DOT_SPACESHIP  { fname_op_dot_cmp() }
| DOT_CMP_EQ  { fname_op_dot_eq() }
| DOT_CMP_NE  { fname_op_dot_ne() }
| DOT_CMP_LE  { fname_op_dot_le() }
| DOT_CMP_GE  { fname_op_dot_ge() }
| DOT_CMP_LT  { fname_op_dot_lt() }
| DOT_CMP_GT  { fname_op_dot_gt() }

aug_op:
| PLUS_EQUAL { OpAdd }
| MINUS_EQUAL { OpSub }
| STAR_EQUAL { OpMul }
| SLASH_EQUAL { OpDiv }
| MOD_EQUAL { OpMod }
| DOT_STAR_EQUAL { OpDotMul }
| DOT_SLASH_EQUAL { OpDotDiv }
| DOT_PERCENT_EQUAL { OpDotMod }
| AND_EQUAL { OpBitwiseAnd }
| OR_EQUAL { OpBitwiseOr }
| XOR_EQUAL { OpBitwiseXor }
| SHIFT_LEFT_EQUAL { OpShiftLeft }
| SHIFT_RIGHT_EQUAL { OpShiftRight }

elif_seq:
| elif_seq_ ELSE IF exp_or_block block { ((($4, $5) :: $1), ExpNop(curr_loc_n 5)) }
| elif_seq_ ELSE block { ($1, $3) }
| exp_or_block block { ((($1, $2) :: []), ExpNop(curr_loc_n 2)) }

elif_seq_:
| elif_seq_ ELSE IF exp_or_block block { ($4, $5) :: $1 }
| exp_or_block block { ($1, $2) :: [] }

nested_for_:
| nested_for_ any_for for_in_list_ { ((curr_loc_n 2), $3) :: $1 }
| nested_for_ any_for ELLIPSIS { ((curr_loc_n 2), (forever_for 3)) :: $1 }
| for_in_list_ { ((curr_loc_n 1), $1) :: [] }
| ELLIPSIS { ((curr_loc_n 1), (forever_for 1)) :: [] }

for_in_list_:
| for_in_list_ COMMA simple_pat BACK_ARROW loop_range_exp { ($3, PatAny(curr_loc_n 3), $5) :: $1 }
| for_in_list_ COMMA simple_pat AT simple_pat BACK_ARROW loop_range_exp { ($3, $5, $7) :: $1 }
| simple_pat BACK_ARROW loop_range_exp { ($1, PatAny(curr_loc_n 1), $3) :: [] }
| simple_pat AT simple_pat BACK_ARROW loop_range_exp { ($1, $3, $5) :: [] }

fold_clause:
| simple_pat EQUAL complex_exp any_for nested_for_ { (($1, $3), $5) }

loop_range_exp:
| exp { $1 }
| CONS exp { ExpRange(None, None, Some($2), make_new_ctx()) }
| exp COLON { ExpRange(Some($1), None, None, make_new_ctx()) }
| exp COLON exp { ExpRange(Some($1), Some($3), None, make_new_ctx()) }
| exp COLON exp COLON exp { ExpRange(Some($1), Some($3), Some($5), make_new_ctx()) }
| exp COLON COLON exp { ExpRange(Some($1), None, Some($4), make_new_ctx()) }

range_exp:
| B_DOT_MINUS simple_exp { ExpUnary(OpDotMinus, $2, make_new_ctx()) }
| complex_exp { $1 }
| opt_exp COLON opt_exp { ExpRange($1, $3, None, make_new_ctx()) }
| opt_exp COLON opt_exp COLON exp { ExpRange($1, $3, Some($5), make_new_ctx()) }
| CONS complex_exp { ExpRange(None, None, Some($2), make_new_ctx()) }

opt_exp:
| B_DOT_MINUS simple_exp { Some(ExpUnary(OpDotMinus, $2, make_new_ctx())) }
| complex_exp { Some($1) }
| /* empty */ { None }

idx_list_:
| idx_list_ COMMA range_exp { $3 :: $1 }
| range_exp { $1 :: [] }

pattern_matching_clauses_with_opt_bar:
| pattern_matching_clauses_ { List.rev $1 }
| BAR pattern_matching_clauses_ { List.rev $2 }

pattern_matching_clauses_:
| pattern_matching_clauses_ BAR matching_patterns_ DOUBLE_ARROW exp_seq_or_none
{ ((List.rev $3), $5) :: $1 }
| matching_patterns_ DOUBLE_ARROW exp_seq_or_none
{ ((List.rev $1), $3) :: [] }

matching_patterns_:
| matching_patterns_ BAR pat_when { $3 :: $1 }
| pat_when { $1 :: [] }

exp_seq_or_none:
| exp_seq_ { expseq2exp (List.rev $1) 1 }
| exp_seq_ SEMICOLON { expseq2exp (List.rev $1) 1 }
| LBRACE RBRACE { expseq2exp [] 1 }

simple_pat:
| B_IDENT
    {
        let loc = curr_loc() in
        match $1 with
        | "_" -> PatAny(loc)
        | _ -> PatIdent((get_id $1), loc)
    }
| B_LPAREN simple_pat_list_ RPAREN
    {
        match $2 with
        | p :: [] -> p
        | _ -> PatTuple((List.rev $2), curr_loc())
    }
| LBRACE id_simple_pat_list_ RBRACE { PatRecord(None, (List.rev $2), curr_loc()) }
| dot_ident LBRACE id_simple_pat_list_ RBRACE { PatRecord(Some(get_id $1), (List.rev $3), curr_loc()) }
| dot_ident LPAREN simple_pat_list_ RPAREN { PatVariant((get_id $1), (List.rev $3), curr_loc()) }
| dot_ident IDENT
    {
        let arg_loc = curr_loc_n 2 in
        let arg = match $2 with
            | "_" -> PatAny(arg_loc)
            | _ -> PatIdent((get_id $2), arg_loc)
            in
        PatVariant((get_id $1), [arg], curr_loc())
    }
| simple_pat COLON typespec { PatTyped($1, $3, curr_loc()) }
| simple_pat AS B_IDENT { PatAs($1, (get_id $3), curr_loc()) }

simple_pat_list:
| simple_pat_list_ { List.rev $1 }
| /* empty */ { [] }

simple_pat_list_:
| simple_pat_list_ COMMA simple_pat { $3 :: $1 }
| simple_pat { $1 :: [] }

id_simple_pat_list_:
| id_simple_pat_list_ COMMA id_simple_pat { $3 :: $1 }
| id_simple_pat { $1 :: [] }

id_simple_pat:
| B_IDENT EQUAL simple_pat { (get_id $1, $3) }
| B_IDENT { let n = (get_id $1) in
            let p = PatIdent(n, curr_loc()) in (n, p) }

pat:
| dot_ident
    {
        let loc = curr_loc() in
        match $1 with
        | "_" -> PatAny(loc)
        | _ ->
            let i = get_id $1 in
            if (try String.index $1 '.' with Not_found -> -1) >= 0 ||
               (good_variant_name $1) then
                PatVariant(i, [], loc)
            else
                PatIdent(i, loc)
    }
| literal { PatLit($1, curr_loc()) }
| B_LPAREN pat_list_ RPAREN
    {
        match $2 with
        | p :: [] -> p
        | _ -> PatTuple((List.rev $2), curr_loc())
    }
| pat CONS pat { PatCons($1, $3, curr_loc()) }
| pat AS B_IDENT { PatAs($1, (get_id $3), curr_loc()) }
| dot_ident LBRACE id_pat_list_ RBRACE { PatRecord(Some(get_id $1), (List.rev $3), curr_loc()) }
| LBRACE id_pat_list_ RBRACE { PatRecord(None, (List.rev $2), curr_loc()) }
| dot_ident LPAREN pat_list_ RPAREN { PatVariant((get_id $1), (List.rev $3), curr_loc()) }
| dot_ident IDENT
    {
        let arg_loc = curr_loc_n 2 in
        let arg = match $2 with
            | "_" -> PatAny(arg_loc)
            | _ -> PatIdent((get_id $2), arg_loc)
            in
        PatVariant((get_id $1), [arg], curr_loc())
    }
| dot_ident literal { PatVariant((get_id $1), [PatLit($2, (curr_loc_n 2))], curr_loc()) }
| pat COLON typespec { PatTyped($1, $3, curr_loc()) }
| REF pat { PatRef ($2, curr_loc()) }

pat_list_:
| pat_list_ COMMA pat { $3 :: $1 }
| pat { $1 :: [] }

id_pat_list_:
| id_pat_list_ COMMA id_pat { $3 :: $1 }
| id_pat { $1 :: [] }

id_pat:
| B_IDENT EQUAL pat { (get_id $1, $3) }
| B_IDENT { let n = (get_id $1) in
            let p = PatIdent(n, curr_loc()) in (n, p) }

pat_when:
| pat { $1 }
| pat WHEN exp { PatWhen($1, $3, curr_loc()) }

val_spec_list_:
| VAL { default_val_flags() }
| VAR { {(default_val_flags()) with val_flag_mutable=true} }

val_decls_:
| val_decls_ COMMA val_decl { $3 :: $1 }
| val_decl { $1 :: [] }

val_decl:
| simple_pat EQUAL complex_exp_or_block { ($1, $3, curr_loc()) }
| simple_pat EQUAL ccode_exp { ($1, (ExpCCode($3, make_new_ctx())), curr_loc()) }
| FOLD fold_clause for_block
    {
        let ((fold_pat, fold_init_exp), fold_cl) = $2 in
        let e = transform_fold_exp "" fold_pat 2 fold_init_exp fold_cl $3 in
        (fold_pat, e, curr_loc())
    }

fun_decl_start:
| fun_flags_ FUN B_IDENT { ($1, get_id $3) }
| FUN B_IDENT { ((default_fun_flags()), get_id $2) }
| fun_flags_ OPERATOR op_name { ($1, $3) }
| OPERATOR op_name { ((default_fun_flags()), $2) }

fun_flags_:
| fun_flags_ INLINE { {$1 with fun_flag_inline=true} }
| fun_flags_ NOTHROW { {$1 with fun_flag_nothrow=true} }
| fun_flags_ PURE { {$1 with fun_flag_pure=1} }
| fun_flags_ PRIVATE { {$1 with fun_flag_private=true} }
| INLINE { {(default_fun_flags()) with fun_flag_inline=true} }
| NOTHROW { {(default_fun_flags()) with fun_flag_nothrow=true} }
| PURE { {(default_fun_flags()) with fun_flag_pure=1} }
| PRIVATE { {(default_fun_flags()) with fun_flag_private=true} }

fun_args:
| lparen fun_arg_list RPAREN opt_typespec { ((List.rev $2), $4) }
| lparen RPAREN opt_typespec { ([], $3) }

fun_arg_list:
| fun_arg_list COMMA fun_arg_decl { $3 :: $1 }
| fun_arg_decl { $1 :: [] }

fun_arg_decl:
| simple_pat { PrFunArgPat($1) }
| TILDE B_IDENT COLON typespec EQUAL literal { PrFunArgKW((get_id $2), $4, Some($6)) }
| TILDE B_IDENT COLON typespec { PrFunArgKW((get_id $2), $4, None) }

actual_args:
| actual_arg_list_
{
    let (args, kwargs, kwloc) = List.fold_left (
        fun (args, kwargs, kwloc) (i_opt, e, loc) ->
        match i_opt with
        | Some i -> (args, (i, e) :: kwargs, if kwloc=noloc then loc else kwloc)
        | _ ->
            if kwargs = [] then (e :: args, kwargs, loc) else
                raise_syntax_err_loc loc "keyword and positional arguments cannot be interleaved")
        ([], [], noloc) (List.rev $1)
        in
    let args = if kwargs = [] then args else
        let mkrec = ExpMkRecord((ExpNop kwloc), (List.rev kwargs), (make_new_typ(), kwloc)) in
        mkrec :: args
        in
    List.rev args
}
| /* empty */ { [] }

actual_arg_list_:
| actual_arg_list_ COMMA actual_arg { $3 :: $1 }
| actual_arg { $1 :: [] }

actual_arg:
| complex_exp { (None, $1, curr_loc()) }
| typed_exp { (None, $1, curr_loc()) }
| B_IDENT EQUAL complex_exp { (Some(get_id $1), $3, curr_loc()) }

opt_typespec:
| COLON typespec { $2 }
| /* empty */ { make_new_typ() }

type_lhs:
| B_LPAREN tyvar_list_ RPAREN ident { ((List.rev $2), (get_id $4)) }
| TYVAR ident { ((get_id $1) :: [], (get_id $2)) }
| ident { ([], (get_id $1)) }

tyvar_list_:
| tyvar_list_ COMMA TYVAR { (get_id $3) :: $1 }
| TYVAR { (get_id $1) :: [] }

implemented_ifaces:
| INTERFACE dot_ident_list_ { List.rev $2 }
| /* empty */ { [] }

dot_ident_list_:
| dot_ident_list_ COMMA dot_ident { (get_id $3) :: $1 }
| dot_ident { (get_id $1) :: [] }

constr_args:
| lparen simple_pat_list RPAREN { $2 }
| /* empty */ { [] }

class_members:
| class_members_ { List.rev $1 }
| class_members_ SEMICOLON { List.rev $1 }

class_members_:
| class_members_ decl { $2 @ $1 }
| class_members_ SEMICOLON decl { $3 @ $1 }
| decl { $1 }

base_iface:
| EXTENDS dot_ident { get_id $2 }
| /* empty */ { noid }

iface_members:
| iface_members_ { List.rev $1 }
| iface_members_ SEMICOLON { List.rev $1 }

iface_members_:
| iface_members_ iface_decl { $2 @ $1 }
| iface_members_ SEMICOLON iface_decl { $3 @ $1 }

iface_decl:
| simple_type_decl { [$1] }
| exception_decl { [$1] }
| fun_decl_start fun_args
    {
        let (flags, fname) = $1 in
        let (args, rt) = $2 in
        make_deffun fname args rt (ExpNop (curr_loc_n 1)) flags (curr_loc())
    }

typespec_or_record:
| typespec { $1 }
| LBRACE id_typ_list_ RBRACE { TypRecord {contents=((List.rev $2), true)} }
| LBRACE id_typ_list_ SEMICOLON RBRACE { TypRecord {contents=((List.rev $2), true)} }

typespec:
| typespec_nf { $1 }
| typespec_nf ARROW typespec { TypFun((match $1 with TypVoid -> [] | TypTuple(args) -> args | _ -> [$1]), $3) }

typespec_nf:
| dot_ident
{
    match $1 with
    | "int" -> TypInt
    | "int8" -> TypSInt(8)
    | "uint8" -> TypUInt(8)
    | "int16" -> TypSInt(16)
    | "uint16" -> TypUInt(16)
    | "int32" -> TypSInt(32)
    | "uint32" -> TypUInt(32)
    | "int64" -> TypSInt(64)
    | "uint64" -> TypUInt(64)
    | "half" -> TypFloat(16)
    | "float" -> TypFloat(32)
    | "double" -> TypFloat(64)
    | "char" -> TypChar
    | "string" -> TypString
    | "bool" -> TypBool
    | "void" -> TypVoid
    | "exn" -> TypExn
    | "cptr" -> TypCPointer
    | _ -> TypApp([], get_id $1)
}
| TYVAR { TypApp([], get_id $1) }
| B_LPAREN typespec_list_ RPAREN { match $2 with t::[] -> t | _ -> TypTuple(List.rev $2) }
| B_LPAREN typespec_list_ COMMA RPAREN { TypTuple(List.rev $2) }
| B_LPAREN typespec_nf STAR INT RPAREN { TypTuple(List.init (Int64.to_int $4) (fun i -> $2)) }
| B_LPAREN INT STAR typespec_nf RPAREN { TypTuple(List.init (Int64.to_int $2) (fun i -> $4)) }
| B_LPAREN ELLIPSIS RPAREN { TypVar {contents=Some (TypVarTuple None)} }
| LBRACE ELLIPSIS RBRACE { TypVar {contents=Some (TypVarRecord)} }
| B_LPAREN typespec ELLIPSIS RPAREN { TypVar {contents=Some (TypVarTuple (Some $2))} }
| typespec_nf nobreak_dot_ident
%prec app_type_prec
{ match ($1, $2) with
  | (x, "list") -> TypList(x)
  | (TypTuple(args), y) -> TypApp(args, get_id y)
  | (x, y) -> TypApp(x :: [], get_id y)
}
| typespec_nf QUESTION
%prec option_type_prec
{
    TypApp((match $1 with
            | TypTuple(args) -> args
            | x -> x :: []),
            get_id "option")
}
| typespec_nf LSQUARE shapespec RSQUARE
%prec arr_type_prec
{ TypArray($3, $1) }
| typespec_nf LSQUARE B_PLUS RSQUARE
%prec arr_type_prec
{ TypVar {contents=Some (TypVarArray($1))} }
| typespec_nf REF_TYPE
%prec ref_type_prec
{ TypRef($1) }

typespec_list_:
| typespec_list_ COMMA typespec { $3 :: $1 }
| typespec { $1 :: [] }

shapespec:
| shapespec COMMA { $1 + 1 }
| /* empty */ { 1 }

dot_ident:
| dot_ident DOT B_IDENT { $1 ^ "." ^ $3 }
| B_IDENT { $1 }

nobreak_dot_ident:
| nobreak_dot_ident DOT B_IDENT { $1 ^ "." ^ $3 }
| IDENT { $1 }

id_typ_list_:
| id_typ_list_ SEMICOLON id_typ_elem { $3 :: $1 }
| id_typ_elem { $1 :: [] }

id_typ_elem:
| B_IDENT COLON typespec { (get_id $1, $3, None) }
| B_IDENT COLON typespec EQUAL literal { (get_id $1, $3, Some($5)) }

variant_elems_:
| variant_elems_ BITWISE_OR variant_elem { $3 :: $1 }
| variant_elem { $1 :: [] }

variant_elem:
| B_IDENT COLON typespec_or_record { ((get_id $1), $3) }
| B_IDENT { ((get_id $1), TypVoid) }

string_list:
| string_list COMMA STRING { $3 :: $1 }
| STRING { $1 :: [] }

any_for:
| B_FOR { 0 }
| FOR { 0 }

any_import:
| B_IMPORT { 0 }
| IMPORT { 0 }

any_while:
| B_WHILE { 0 }
| WHILE { 0 }

lparen:
| B_LPAREN { 0 }
| LPAREN { 0 }

ident:
| B_IDENT { $1 }
| IDENT { $1 }
