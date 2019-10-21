%{
(* ficus parser *)
open Syntax

let make_loc(pos0, pos1) =
    let { Lexing.pos_lnum=l0; Lexing.pos_bol=b0; Lexing.pos_cnum=c0 } = pos0 in
    let { Lexing.pos_lnum=l1; Lexing.pos_bol=b1; Lexing.pos_cnum=c1 } = pos1 in
    if c0 <= c1 then
        { loc_fname = !Utils.parser_ctx_file; loc_line0 = l0;
          loc_pos0 = c0 - b0+1; loc_line1 = l1; loc_pos1 = c1 - b1+1 }
    else
        { loc_fname = !Utils.parser_ctx_file; loc_line0 = l1;
          loc_pos0 = c1 - b1+1; loc_line1 = l0; loc_pos1 = c0 - b0+1 }

let curr_loc() = make_loc(Parsing.symbol_start_pos(), Parsing.symbol_end_pos())
let curr_loc_n n = make_loc((Parsing.rhs_start_pos n), (Parsing.rhs_end_pos n))

let make_new_ctx () = (make_new_typ(), curr_loc())

let make_bin_op(op, a, b) = ExpBinOp(op, a, b, make_new_ctx())
let make_un_op(op, a) = ExpUnOp(op, a, make_new_ctx())

let expseq2exp es n = match es with
      [] -> ExpNop(TypVoid, curr_loc_n(n))
    | e::[] -> e
    | _ -> ExpSeq(es, (make_new_typ(), (curr_loc_n n)))

let plist2exp args n =
    let rec plist2exp_ plist0 =
      List.fold_right (fun p (plist, elist) -> let (p_, e_) = pat2exp_ p in ((p_::plist), (e_::elist))) plist0 ([], [])
    and pat2exp_ p =
      (match p with
      | PatAny(loc) ->
          let arg_tp = make_new_typ() in
          let arg_id = get_temp_id "arg" in
          (PatIdent(arg_id, loc), ExpIdent(arg_id, (arg_tp, loc)))
      | PatIdent(i, loc) -> (p, ExpIdent(i, (make_new_typ(), loc)))
      | PatTuple(plist, loc) ->
          let (plist_new, elist_new) = plist2exp_ plist in
          (PatTuple(plist_new, loc), (expseq2exp elist_new n))
      | PatTyped(p, t, loc) -> pat2exp_ p
      | _ -> raise (SyntaxError
            ("unsupported arg pattern in the pattern-matching function",
             Parsing.symbol_start_pos(),
             Parsing.symbol_end_pos()))) in
    let (plist, elist) = plist2exp_ args in
    (plist, expseq2exp elist n)

let make_deffun fname args rt body flags loc =
    let argtp = List.map (fun _ -> make_new_typ()) args in
    { df_name=fname; df_templ_args=[]; df_args=args; df_typ=TypFun(argtp, rt);
      df_body=body; df_flags=flags; df_scope=ScGlobal :: [];
      df_loc=loc; df_templ_inst=[] }

let make_variant_type (targs, tname) var_elems0 =
    let (pos0, pos1) = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) in
    let loc = make_loc(pos0, pos1) in
    let var_elems = List.map (fun (n, t) -> if (good_variant_name n) then (get_id n, t) else
        raise (SyntaxError ((sprintf "syntax error: variant tag '%s' does not start with a capital latin letter" n), pos0, pos1))) var_elems0 in
    let dv = { dvar_name=tname; dvar_templ_args=targs; dvar_typ=make_new_typ(); dvar_flags=[]; dvar_members=var_elems;
               dvar_constr=[]; dvar_templ_inst=[]; dvar_scope=ScGlobal::[]; dvar_loc=loc } in
    DefVariant (ref dv)

let transform_fold_exp fold_pat fold_pat_n fold_init_exp fold_cl fold_body =
    (* `fold (p=e0; ...) e1`
            is transformed to
        `{
        var p = e0
        for (...) e1
        p
        }`
    *)
    let rec fold_pat2exp p =
        (let (ptype, ploc) as ctx = (make_new_typ(), get_pat_loc p) in
        match p with
        | PatTuple(pl, _) -> ExpMkTuple((List.map fold_pat2exp pl), ctx)
        | PatTyped(p, t, _) -> ExpTyped((fold_pat2exp p), t, ctx)
        | PatIdent(i, _) -> ExpIdent(i, ctx)
        | PatVariant(f, pl, _) ->
            ExpCall(ExpIdent(f, (make_new_typ(), ploc)), (List.map fold_pat2exp pl), ctx)
        | _ ->
            let (pos0, pos1) = ((Parsing.rhs_start_pos fold_pat_n), (Parsing.rhs_end_pos fold_pat_n)) in
            raise (SyntaxError
                (("unsupported accumulator pattern in the fold() expression; " ^
                "only identifiers, tuples, single-case variants, type annotations " ^
                "and various combinations of those are currently supported."),
                pos0, pos1))) in
    let acc_loc = get_pat_loc fold_pat in
    let acc_decl = DefVal(fold_pat, fold_init_exp, [ValMutable], acc_loc) in
    let (p0, _) = List.hd fold_cl in
    let { loc_fname; loc_line0; loc_pos0 } = get_pat_loc p0 in
    let { loc_line1; loc_pos1 } = get_exp_loc fold_body in
    let for_loc = { loc_fname; loc_line0; loc_pos0; loc_line1; loc_pos1 } in
    let for_exp = ExpFor (fold_cl, fold_body, [], (TypVoid, for_loc)) in
    let acc_exp = fold_pat2exp fold_pat in
    ExpSeq([acc_decl; for_exp; acc_exp], (make_new_typ(), curr_loc()))

let rec compress_nested_map_exp l e = match e with
    | ExpFor(for_clauses, body, _, _) -> compress_nested_map_exp ((for_clauses, None) :: l) body
    | _ -> ((List.rev l), e)

%}

%token TRUE FALSE NONE
%token <Int64.t> INT
%token <int * Int64.t> SINT
%token <int * Int64.t> UINT
%token <int * float> FLOAT
%token <string> IDENT
%token <string> B_IDENT
%token <string> STRING
%token <string> CHAR
%token <string> TYVAR

/* keywords */
%token AS BREAK CATCH CCODE CLASS CONTINUE DO ELSE EXCEPTION
%token EXTENDS FOLD FOR FROM FUN IF IMPLEMENTS IMPORT IN INLINE INTERFACE
%token MATCH NOTHROW OPERATOR PARALLEL PURE REF REF_TYPE STATIC
%token THROW TRY TYPE VAL VAR WHEN WHILE WITH

/* parens/delimiters */
%token B_LPAREN LPAREN STR_INTERP_LPAREN RPAREN B_LSQUARE LSQUARE RSQUARE LBRACE RBRACE
%token COMMA DOT SEMICOLON COLON BAR CONS CAST EXPAND ARROW DOUBLE_ARROW EOF

/* operations */
%token B_MINUS MINUS B_PLUS PLUS
%token B_STAR STAR SLASH MOD
%token B_POWER POWER SHIFT_RIGHT SHIFT_LEFT
%token BITWISE_AND BITWISE_OR BITWISE_XOR BITWISE_NOT
%token LOGICAL_AND LOGICAL_OR LOGICAL_NOT
%token EQUAL PLUS_EQUAL MINUS_EQUAL STAR_EQUAL SLASH_EQUAL MOD_EQUAL
%token AND_EQUAL OR_EQUAL XOR_EQUAL SHIFT_LEFT_EQUAL SHIFT_RIGHT_EQUAL
%token EQUAL_TO NOT_EQUAL LESS_EQUAL GREATER_EQUAL LESS GREATER

%right SEMICOLON
%left COMMA
%right DOUBLE_ARROW
%left BAR
%right THROW
%right EQUAL PLUS_EQUAL MINUS_EQUAL STAR_EQUAL SLASH_EQUAL MOD_EQUAL AND_EQUAL OR_EQUAL XOR_EQUAL SHIFT_LEFT_EQUAL SHIFT_RIGHT_EQUAL
%left WHEN
%right CONS
%left LOGICAL_OR
%left LOGICAL_AND
%left COLON
%left BITWISE_OR
%left BITWISE_XOR
%left BITWISE_AND
%left AS
%left EQUAL_TO NOT_EQUAL LESS_EQUAL GREATER_EQUAL LESS GREATER
%left SHIFT_LEFT SHIFT_RIGHT
%left PLUS MINUS
%left STAR SLASH MOD
%right POWER
%left WITH
%right B_MINUS B_PLUS BITWISE_NOT LOGICAL_NOT deref_prec REF EXPAND
%right ARROW
%left lsquare_prec fcall_prec
%left app_type_prec arr_type_prec option_type_prec ref_type_prec
%left DOT

%type <Syntax.exp_t list> ficus_module
%start ficus_module

%%

ficus_module:
| top_level_seq_ { List.rev $1 }
| top_level_seq_ SEMICOLON { List.rev $1 }
| /* empty */ { [] }

top_level_seq_:
| top_level_seq_ top_level_exp { (List.rev $2) @ $1 }
| top_level_seq_ SEMICOLON top_level_exp { (List.rev $3) @ $1 }
| top_level_exp { (List.rev $1) }

top_level_exp:
| stmt { $1 :: [] }
| decl { $1 }
| IMPORT module_name_list_
    {
        let (pos0, pos1) = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) in
        [DirImport ((List.map (fun (a, b) ->
        let a1 = Utils.update_imported_modules a (pos0, pos1) in (a1, b)) $2), curr_loc())]
    }
| FROM dot_ident IMPORT STAR
    {
        let (pos0, pos1) = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) in
        let a = get_id $2 in
        let a1 = Utils.update_imported_modules a (pos0, pos1) in
        [DirImportFrom (a1, [], curr_loc())]
    }
| FROM dot_ident IMPORT ident_list_
    {
        let (pos0, pos1) = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) in
        let a = get_id $2 in
        let a1 = Utils.update_imported_modules a (pos0, pos1) in
        [DirImportFrom (a1, (List.rev $4), curr_loc())]
    }
| error
    { raise (SyntaxError ("syntax error", Parsing.symbol_start_pos(), Parsing.symbol_end_pos())) }

exp_seq_:
| exp_seq_ stmt { $2 :: $1 }
| exp_seq_ decl { (List.rev $2) @ $1 }
| exp_seq_ SEMICOLON stmt { $3 :: $1 }
| exp_seq_ SEMICOLON decl { (List.rev $3) @ $1 }
| stmt { $1 :: [] }
| decl { $1 }

decl:
| val_spec_list_ val_decls_
    {
        let vflags = List.rev $1 in
        List.map (fun (p, e, ctx) -> DefVal(p, e, vflags, ctx)) $2
    }
| fun_decl_start fun_args EQUAL stmt
    {
        let (flags, fname) = $1 in
        let (args, rt, prologue) = $2 in
        let body = expseq2exp (prologue @ [$4]) 4 in
        [DefFun (ref (make_deffun fname args rt body flags (curr_loc())))]
    }
| fun_decl_start fun_args block
    {
        let (flags, fname) = $1 in
        let (args, rt, prologue) = $2 in
        let body = expseq2exp (prologue @ $3) 3 in
        [DefFun (ref (make_deffun fname args rt body flags (curr_loc())))]
    }
| fun_decl_start fun_args LBRACE BAR pattern_matching_clauses_ RBRACE
    {
        let (flags, fname) = $1 in
        let (args, rt, prologue) = $2 in
        let (args_upd, match_arg) = plist2exp args 2 in
        let match_e = ExpMatch(match_arg, $5, make_new_ctx()) in
        let body = expseq2exp (prologue @ [match_e]) 5 in
        [DefFun (ref (make_deffun fname args_upd rt body flags (curr_loc())))]
    }
| simple_type_decl { [$1] }
| exception_decl { [$1] }
| CLASS type_lhs implemented_ifaces constr_args LBRACE class_members RBRACE
    {
        let (cl_templ_args, cl_name) = $2 in
        let cl_ifaces = List.rev $3 in
        let cl_args = $4 in
        let cl_def = { dcl_name=cl_name; dcl_templ_args=cl_templ_args; dcl_ifaces=cl_ifaces; dcl_args=cl_args;
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
| TYPE type_lhs EQUAL typespec
    {
        let (targs, i) = $2 in
        let dt = { dt_name=i; dt_templ_args=targs; dt_typ=$4; dt_scope=ScGlobal :: [];
                    dt_finalized=false; dt_loc=curr_loc() } in
        DefTyp (ref dt)
    }
| TYPE type_lhs EQUAL B_IDENT BITWISE_OR variant_elems_
    {
        make_variant_type $2 (($4, TypVoid) :: (List.rev $6))
    }
| TYPE type_lhs EQUAL B_IDENT COLON typespec BITWISE_OR variant_elems_
    {
        make_variant_type $2 (($4, $6) :: (List.rev $8))
    }
| TYPE type_lhs EQUAL B_IDENT COLON typespec
    {
        make_variant_type $2 (($4, $6) :: [])
    }

exception_decl:
| EXCEPTION B_IDENT
    {
        DefExn(ref { dexn_name=(get_id $2); dexn_typ=TypVoid; dexn_scope=ScGlobal :: []; dexn_loc=curr_loc() })
    }
| EXCEPTION B_IDENT COLON typespec
    {
        DefExn(ref { dexn_name=(get_id $2); dexn_typ = $4; dexn_scope=ScGlobal :: []; dexn_loc=curr_loc() })
    }

stmt:
| BREAK { ExpBreak((TypVoid, curr_loc())) }
| CONTINUE { ExpContinue((TypVoid, curr_loc())) }
| THROW exp { ExpUnOp(OpThrow, $2, (TypErr, curr_loc())) }
| simple_exp EQUAL complex_exp { ExpBinOp(OpSet, $1, $3, (TypVoid, curr_loc())) }
| simple_exp aug_op complex_exp
    {
        let (tp, loc) = make_new_ctx() in
        ExpBinOp(OpSet, $1, ExpBinOp($2, $1, $3, (tp, loc)), (TypVoid, loc))
    }
| WHILE lparen exp_or_block RPAREN exp_or_block { ExpWhile ($3, $5, make_new_ctx()) }
| DO exp_or_block WHILE lparen exp_or_block RPAREN { ExpDoWhile ($2, $5, make_new_ctx()) }
| for_flags FOR lparen for_in_list_ RPAREN exp_or_block
    {
        let for_cl_ = List.rev $4 in
        let for_body_ = $6 in
        ExpFor (for_cl_, for_body_, $1, make_new_ctx())
    }
| CCODE STRING { ExpCCode($2, make_new_ctx()) }
| FUN fun_args DOUBLE_ARROW stmt
    {
        let ctx = make_new_ctx() in
        let (args, rt, prologue) = $2 in
        let body = expseq2exp (prologue @ [$4]) 4 in
        let fname = get_temp_id "lambda" in
        let df = make_deffun fname args rt body [] (curr_loc()) in
        ExpSeq([DefFun (ref df); ExpIdent (fname, ctx)], ctx)
    }
| FUN fun_args block
    {
        let ctx = make_new_ctx() in
        let (args, rt, prologue) = $2 in
        let body = expseq2exp (prologue @ $3) 3 in
        let fname = get_temp_id "lambda" in
        let df = make_deffun fname args rt body [] (curr_loc()) in
        ExpSeq([DefFun (ref df); ExpIdent (fname, ctx)], ctx)
    }
| complex_exp { $1 }

simple_exp:
| B_IDENT { ExpIdent((get_id $1), make_new_ctx()) }
| B_LPAREN op_name RPAREN { ExpIdent($2, make_new_ctx()) }
| literal { ExpLit($1, make_new_ctx()) }
| simple_exp DOT B_IDENT { make_bin_op(OpMem, $1, ExpIdent((get_id $3), (make_new_typ(), curr_loc_n 3))) }
| simple_exp DOT INT { make_bin_op(OpMem, $1, ExpLit((LitInt $3), (make_new_typ(), curr_loc_n 3))) }
| B_LPAREN exp_or_block RPAREN { $2 }
| B_LPAREN complex_exp COMMA exp_list RPAREN { ExpMkTuple(($2 :: $4), make_new_ctx()) }
| B_LPAREN exp COLON typespec RPAREN { ExpTyped($2, $4, make_new_ctx()) }
| B_LPAREN exp CAST typespec RPAREN { ExpCast($2, $4, make_new_ctx()) }
| B_LSQUARE for_flags FOR lparen for_in_list_ RPAREN exp_or_block RSQUARE
    {
        let (map_clauses, body) = compress_nested_map_exp (((List.rev $5), None) :: []) $7 in
        ExpMap(map_clauses, body, ForMakeArray :: $2, make_new_ctx())
    }
| B_LSQUARE CONS for_flags FOR lparen for_in_list_ RPAREN exp_or_block RSQUARE
    {
        let (map_clauses, body) = compress_nested_map_exp (((List.rev $6), None) :: []) $8 in
        ExpMap(map_clauses, body, ForMakeList :: $3, make_new_ctx())
    }
| B_LSQUARE array_elems_ RSQUARE
    {
        let ae = List.rev $2 in
        ExpMkArray(ae, make_new_ctx())
    }
| B_LSQUARE CONS complex_exp COMMA exp_list RSQUARE
    {
        let l = List.rev ($3 :: $5) in
        let e0 = ExpLit(LitNil, (make_new_typ(), curr_loc_n 6)) in
        List.fold_left (fun e i -> make_bin_op(OpCons, i, e)) e0 l
    }
| simple_exp LPAREN exp_list RPAREN
    %prec fcall_prec
    { ExpCall($1, $3, make_new_ctx()) }
| simple_exp LSQUARE idx_list_ RSQUARE
    %prec lsquare_prec
    { ExpAt($1, (List.rev $3), make_new_ctx()) }
| simple_exp LBRACE id_exp_list_ RBRACE
    %prec fcall_prec
    { ExpMkRecord($1, (List.rev $3), make_new_ctx()) }
| simple_exp LBRACE RBRACE
    %prec fcall_prec
    { ExpMkRecord($1, [], make_new_ctx()) }

for_flags:
| PARALLEL { [ForParallel] }
| /* empty */ { [] }

complex_exp:
| IF B_LPAREN exp_or_block RPAREN exp_or_block ELSE exp_or_block { ExpIf ($3, $5, $7, make_new_ctx()) }
| IF B_LPAREN exp_or_block RPAREN exp_or_block { ExpIf ($3, $5, ExpNop(TypVoid, curr_loc_n 5), make_new_ctx()) }
| TRY exp_or_block CATCH LBRACE BAR pattern_matching_clauses_ RBRACE { ExpTryCatch ($2, (List.rev $6), make_new_ctx()) }
| MATCH B_LPAREN exp_list_ RPAREN LBRACE BAR pattern_matching_clauses_ RBRACE
    {
        let e = (match $3 with
        | e :: [] -> e
        | l -> ExpMkTuple(l, (make_new_typ(), (curr_loc_n 3)))) in
        ExpMatch (e, (List.rev $7), make_new_ctx())
    }
| FOLD fold_clause exp_or_block
    {
        let ((fold_pat, fold_init_exp), fold_cl) = $2 in
        transform_fold_exp fold_pat 2 fold_init_exp fold_cl $3
    }
| exp { $1 }

exp:
| simple_exp { $1 }
| exp PLUS exp { make_bin_op(OpAdd, $1, $3) }
| exp MINUS exp { make_bin_op(OpSub, $1, $3) }
| exp STAR exp { make_bin_op(OpMul, $1, $3) }
| exp SLASH exp { make_bin_op(OpDiv, $1, $3) }
| exp MOD exp { make_bin_op(OpMod, $1, $3) }
| exp POWER exp { make_bin_op(OpPow, $1, $3) }
| exp SHIFT_LEFT exp { make_bin_op(OpShiftLeft, $1, $3) }
| exp SHIFT_RIGHT exp { make_bin_op(OpShiftRight, $1, $3) }
| exp LOGICAL_AND exp { make_bin_op(OpLogicAnd, $1, $3) }
| exp BITWISE_AND exp { make_bin_op(OpBitwiseAnd, $1, $3) }
| exp LOGICAL_OR exp { make_bin_op(OpLogicOr, $1, $3) }
| exp BITWISE_OR exp { make_bin_op(OpBitwiseOr, $1, $3) }
| exp BITWISE_XOR exp { make_bin_op(OpBitwiseXor, $1, $3) }
| exp EQUAL_TO exp { make_bin_op(OpCompareEQ, $1, $3) }
| exp NOT_EQUAL exp { make_bin_op(OpCompareNE, $1, $3) }
| exp LESS exp { make_bin_op(OpCompareLT, $1, $3) }
| exp LESS_EQUAL exp { make_bin_op(OpCompareLE, $1, $3) }
| exp GREATER exp { make_bin_op(OpCompareGT, $1, $3) }
| exp GREATER_EQUAL exp { make_bin_op(OpCompareGE, $1, $3) }
| exp CONS exp { make_bin_op(OpCons, $1, $3) }
| exp WITH LBRACE id_exp_list_ RBRACE { ExpUpdateRecord($1, (List.rev $4), make_new_ctx()) }
| B_STAR exp %prec deref_prec { make_un_op(OpDeref, $2) }
| B_POWER exp %prec deref_prec { make_un_op(OpDeref, make_un_op(OpDeref, $2)) }
| REF exp { make_un_op(OpMakeRef, $2) }
| B_MINUS exp { make_un_op(OpNegate, $2) }
| B_PLUS exp { make_un_op(OpPlus, $2) }
| LOGICAL_NOT exp { make_un_op(OpLogicNot, $2) }
| BITWISE_NOT exp { make_un_op(OpBitwiseNot, $2) }
| EXPAND exp { make_un_op(OpExpand, $2) }

exp_or_block:
| stmt { $1 }
| block { expseq2exp $1 1 }

block:
| LBRACE RBRACE { [] }
| LBRACE exp_seq_ RBRACE { List.rev $2 }
| LBRACE exp_seq_ SEMICOLON RBRACE { List.rev $2 }

literal:
| INT { LitInt $1 }
| SINT { let (b, v) = $1 in LitSInt (b, v) }
| UINT { let (b, v) = $1 in LitUInt (b, v) }
| FLOAT { let (b, v) = $1 in LitFloat (b, v) }
| STRING { LitString $1 }
| CHAR { LitChar $1 }
| TRUE { LitBool true }
| FALSE { LitBool false }
| B_LSQUARE RSQUARE { LitNil }
| NONE { LitNone }

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
| /* empty */ { [] }

exp_list_:
| exp_list_ COMMA complex_exp { $3 :: $1 }
| complex_exp { $1 :: [] }

array_elems_:
| array_elems_ SEMICOLON exp_list_ { (List.rev $3) :: $1 }
| exp_list_ { (List.rev $1) :: [] }

id_exp_list_:
| id_exp_list_ COMMA B_IDENT EQUAL complex_exp { (get_id $3, $5) :: $1 }
| B_IDENT EQUAL complex_exp { (get_id $1, $3) :: [] }

op_name:
| B_PLUS { fname_op_add }
| B_MINUS  { fname_op_sub }
| B_STAR  { fname_op_mul }
| SLASH  { fname_op_div }
| MOD  { fname_op_mod }
| B_POWER  { fname_op_pow }
| SHIFT_LEFT  { fname_op_shl }
| SHIFT_RIGHT  { fname_op_shr }
| BITWISE_AND  { fname_op_bit_and }
| BITWISE_OR   { fname_op_bit_or }
| BITWISE_XOR  { fname_op_bit_xor }
| BITWISE_NOT  { fname_op_bit_not }
| EQUAL_TO  { fname_op_eq }
| NOT_EQUAL  { fname_op_ne }
| LESS  { fname_op_lt }
| LESS_EQUAL  { fname_op_le }
| GREATER  { fname_op_gt }
| GREATER_EQUAL  { fname_op_ge }

aug_op:
| PLUS_EQUAL { OpAdd }
| MINUS_EQUAL { OpSub }
| STAR_EQUAL { OpMul }
| SLASH_EQUAL { OpDiv }
| MOD_EQUAL { OpMod }
| AND_EQUAL { OpBitwiseAnd }
| OR_EQUAL { OpBitwiseOr }
| XOR_EQUAL { OpBitwiseXor }
| SHIFT_LEFT_EQUAL { OpShiftLeft }
| SHIFT_RIGHT_EQUAL { OpShiftRight }

for_in_list_:
| for_in_list_ COMMA simple_pat IN loop_range_exp { ($3, $5) :: $1 }
| simple_pat IN loop_range_exp { ($1, $3) :: [] }

fold_clause:
| lparen simple_pat EQUAL exp SEMICOLON for_in_list_ RPAREN { (($2, $4), (List.rev $6)) }

loop_range_exp:
| exp { $1 }
| exp COLON exp { ExpRange(Some($1), Some($3), None, make_new_ctx()) }
| exp COLON exp COLON exp { ExpRange(Some($1), Some($3), Some($5), make_new_ctx()) }

range_exp:
| exp { $1 }
| opt_exp COLON opt_exp { ExpRange($1, $3, None, make_new_ctx()) }
| opt_exp COLON opt_exp COLON exp { ExpRange($1, $3, Some($5), make_new_ctx()) }

opt_exp:
| exp { Some($1) }
| /* empty */ { None }

idx_list_:
| idx_list_ COMMA range_exp { $3 :: $1 }
| range_exp { $1 :: [] }

pattern_matching_clauses_:
| pattern_matching_clauses_ BAR matching_patterns_ DOUBLE_ARROW exp_seq_or_none { ((List.rev $3), $5) :: $1 }
| matching_patterns_ DOUBLE_ARROW exp_seq_or_none { ((List.rev $1), $3) :: [] }

matching_patterns_:
| matching_patterns_ BAR pat { $3 :: $1 }
| pat { $1 :: [] }

exp_seq_or_none:
| exp_seq_ { expseq2exp (List.rev $1) 1 }
| LBRACE RBRACE { expseq2exp [] 1 }

simple_pat:
| B_IDENT
  {
      let loc = curr_loc() in
      match $1 with
          "_" -> PatAny(loc)
         | _ -> PatIdent((get_id $1), loc)
  }
| B_LPAREN simple_pat_list_ RPAREN
  {
      match $2 with
        p :: [] -> p
      | _ -> PatTuple((List.rev $2), curr_loc())
  }
| LBRACE id_simple_pat_list_ RBRACE { PatRec(None, (List.rev $2), curr_loc()) }
| dot_ident LBRACE id_simple_pat_list_ RBRACE { PatRec(Some(get_id $1), (List.rev $3), curr_loc()) }
| dot_ident LPAREN simple_pat_list_ RPAREN { PatVariant((get_id $1), (List.rev $3), curr_loc()) }
| dot_ident IDENT { PatVariant((get_id $1), [PatIdent((get_id $2), (curr_loc_n 2))], curr_loc()) }
| simple_pat COLON typespec { PatTyped($1, $3, curr_loc()) }

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
| B_IDENT
  {
      let loc = curr_loc() in
      match $1 with
          "_" -> PatAny(loc)
         | _ -> PatIdent((get_id $1), loc)
  }
| literal { PatLit($1, curr_loc()) }
| B_LPAREN pat_list_ RPAREN
  {
      match $2 with
        p :: [] -> p
      | _ -> PatTuple((List.rev $2), curr_loc())
  }
| pat CONS pat { PatCons($1, $3, curr_loc()) }
| pat AS B_IDENT { PatAs($1, (get_id $3), curr_loc()) }
| dot_ident LBRACE id_pat_list_ RBRACE { PatRec(Some(get_id $1), (List.rev $3), curr_loc()) }
| LBRACE id_pat_list_ RBRACE { PatRec(None, (List.rev $2), curr_loc()) }
| dot_ident LPAREN pat_list_ RPAREN { PatVariant((get_id $1), (List.rev $3), curr_loc()) }
| dot_ident IDENT { PatVariant((get_id $1), [PatIdent((get_id $2), (curr_loc_n 2))], curr_loc()) }
| dot_ident literal { PatVariant((get_id $1), [PatLit($2, (curr_loc_n 2))], curr_loc()) }
| pat COLON typespec { PatTyped($1, $3, curr_loc()) }

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

val_spec_list_:
| VAL { [] }
| VAR { ValMutable :: [] }

val_decls_:
| val_decls_ COMMA val_decl { $3 :: $1 }
| val_decl { $1 :: [] }

val_decl:
| simple_pat EQUAL exp_or_block { ($1, $3, curr_loc()) }
| FOLD fold_clause exp_or_block
    {
        let ((fold_pat, fold_init_exp), fold_cl) = $2 in
        let e = transform_fold_exp fold_pat 2 fold_init_exp fold_cl $3 in
        (fold_pat, e, curr_loc())
    }

fun_decl_start:
| fun_flags_ FUN B_IDENT { ((List.rev $1), get_id $3) }
| FUN B_IDENT { ([], get_id $2) }
| fun_flags_ OPERATOR op_name { ((List.rev $1), $3) }
| OPERATOR op_name { ([], $2) }

fun_flags_:
| fun_flags_ fun_flag { $2 :: $1 }
| fun_flag { $1 :: [] }

fun_flag:
| INLINE { FunInline }
| NOTHROW { FunNoThrow }
| PURE { FunPure }
| STATIC { FunStatic }

fun_args:
| lparen simple_pat_list RPAREN opt_typespec { ($2, $4, []) }

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
| class_members_ decl { (List.rev $2) @ $1 }
| class_members_ SEMICOLON decl { (List.rev $3) @ $1 }
| decl { List.rev $1 }

base_iface:
| EXTENDS dot_ident { get_id $2 }
| /* empty */ { noid }

iface_members:
| iface_members_ { List.rev $1 }
| iface_members_ SEMICOLON { List.rev $1 }

iface_members_:
| iface_members_ iface_decl { $2 :: $1 }
| iface_members_ SEMICOLON iface_decl { $3 :: $1 }

iface_decl:
| simple_type_decl { $1 }
| exception_decl { $1 }
| fun_decl_start fun_args
    {
        let (flags, fname) = $1 in
        let (args, rt, _) = $2 in
        DefFun (ref (make_deffun fname args rt (ExpNop(TypVoid, curr_loc_n 1)) flags (curr_loc())))
    }

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
| B_LPAREN typespec_list_ RPAREN { match $2 with t::[] -> t | _ -> TypTuple($2) }
| B_LPAREN typespec_list_ COMMA RPAREN { TypTuple(List.rev $2) }
| typespec_nf nobreak_dot_ident
%prec app_type_prec
{ match ($1, $2) with
  | (x, "list") -> TypList(x)
  | (TypTuple(args), y) -> TypApp(args, get_id y)
  | (x, y) -> TypApp(x :: [], get_id y)
}
| typespec_nf LSQUARE shapespec RSQUARE
%prec arr_type_prec
{ TypArray($3, $1) }
| typespec_nf REF_TYPE
%prec ref_type_prec
{ TypRef($1) }
| LBRACE id_typ_list_ RBRACE { TypRecord(ref (List.rev $2, false)) }

typespec_list_:
| typespec_list_ COMMA typespec { $3 :: $1 }
| typespec { $1 :: [] }

shapespec:
| shapespec COMMA { $1 + 1 }
| /* empty */ { 1 }

dot_ident:
| B_IDENT { $1 }
| B_IDENT DOT nobreak_dot_ident { $1 ^ "." ^ $3 }

nobreak_dot_ident:
| nobreak_dot_ident DOT ident { $1 ^ "." ^ $3 }
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
| B_IDENT COLON typespec { ($1, $3) }
| B_IDENT { ($1, TypVoid) }

lparen:
| B_LPAREN { 0 }
| LPAREN { 0 }

ident:
| B_IDENT { $1 }
| IDENT { $1 }
