%{
(* ficus parser *)
open Syntax

let make_loc(pos0, pos1) =
    let { Lexing.pos_lnum=l0; Lexing.pos_bol=b0; Lexing.pos_cnum=c0 } = pos0 in
    let { Lexing.pos_lnum=l1; Lexing.pos_bol=b1; Lexing.pos_cnum=c1 } = pos1 in
    if c0 <= c1 then
        { loc_fname = !current_file_id; loc_line0 = l0;
          loc_pos0 = c0 - b0+1; loc_line1 = l1; loc_pos1 = c1 - b1+1 }
    else
        { loc_fname = !current_file_id; loc_line0 = l1;
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

let make_deffun fname args rt body flags loc tmp =
    let i = if tmp then (get_unique_id fname tmp) else (get_id fname) in
    { df_name=i; df_template_args=[]; df_args=args; df_rt=rt;
      df_body=body; df_flags=flags; df_scope=ScGlobal;
      df_loc=loc; df_template_inst=[] }

%}

%token TRUE FALSE NIL
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
%token AS CATCH CCODE ELSE EXCEPTION
%token FOLD FOR FROM FUN IF B_IMPORT IMPORT IN
%token OPERATOR B_REF REF THROW TRY TYPE VAL VAR WHILE

/* parens/delimiters */
%token B_LPAREN LPAREN RPAREN B_LSQUARE LSQUARE RSQUARE LBRACE RBRACE COMMA DOT
%token SEMICOLON COLON BAR CONS CAST DOUBLE_ARROW ARROW EOF

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
%right B_MINUS B_PLUS BITWISE_NOT LOGICAL_NOT B_POWER B_STAR B_REF
%right ARROW
%left lsquare_prec fcall_prec
%left app_type_prec arr_type_prec option_type_prec ref_type_prec
%left DOT

%type <Syntax.exp_t list * Syntax.id_t list> ficus_module
%start ficus_module

%%

ficus_module:
| top_level_seq_ { ((List.rev $1), !current_imported_modules) }
| top_level_seq_ SEMICOLON { ((List.rev $1), !current_imported_modules) }
| /* empty */ { ([], []) }

top_level_seq_:
| top_level_seq_ top_level_exp { (List.rev $2) @ $1 }
| top_level_seq_ SEMICOLON top_level_exp { (List.rev $3) @ $1 }
| top_level_exp { (List.rev $1) }

top_level_exp:
| complex_exp { $1 :: [] }
| decl { $1 }
| B_IMPORT module_name_list_
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
| exp_seq_ complex_exp { $2 :: $1 }
| exp_seq_ decl { (List.rev $2) @ $1 }
| exp_seq_ SEMICOLON complex_exp { $3 :: $1 }
| exp_seq_ SEMICOLON decl { (List.rev $3) @ $1 }
| complex_exp { $1 :: [] }
| decl { $1 }

decl:
| val_spec_list_ val_decls_
    {
        let vflags = List.rev $1 in
        List.map (fun (p, e, ctx) -> DefVal(p, e, vflags, ctx)) $2
    }
| fun_decl_start fun_args EQUAL complex_exp
    {
        let (flags, fname) = $1 in
        let (args, rt, prologue) = $2 in
        let body = expseq2exp (prologue @ [$4]) 4 in
        [DefFun (ref (make_deffun fname args rt body flags (curr_loc()) false))]
    }
| fun_decl_start fun_args block
    {
        let (flags, fname) = $1 in
        let (args, rt, prologue) = $2 in
        let body = expseq2exp (prologue @ (List.rev $3)) 3 in
        [DefFun (ref (make_deffun fname args rt body flags (curr_loc()) false))]
    }
| typedef_lhs EQUAL typespec
    {
        let (targs, i) = $1 in
        let dtp = { dt_name=i; dt_template_args=targs; dt_body=$3; dt_scope=ScGlobal; dt_loc=curr_loc() } in
        [DefType (ref dtp)]
    }
| EXCEPTION B_IDENT
    {
        [DefExn(ref { dexn_name=(get_id $2); dexn_tp=TypVoid; dexn_scope=ScGlobal; dexn_loc=curr_loc() })]
    }
| EXCEPTION B_IDENT COLON typespec
    {
        [DefExn(ref { dexn_name=(get_id $2); dexn_tp = $4; dexn_scope=ScGlobal; dexn_loc=curr_loc() })]
    }

complex_exp:
| exp EQUAL complex_exp { ExpBinOp(OpSet, $1, $3, (TypVoid, curr_loc())) }
| exp aug_op complex_exp
    {
        let (tp, loc) = make_new_ctx() in
        ExpBinOp(OpSet, $1, ExpBinOp($2, $1, $3, (tp, loc)), (TypVoid, loc))
    }
| IF B_LPAREN exp_or_block RPAREN exp_or_block ELSE exp_or_block { ExpIf ($3, $5, $7, make_new_ctx()) }
| IF B_LPAREN exp_or_block RPAREN exp_or_block { ExpIf ($3, $5, ExpNop(TypVoid, curr_loc_n 5), make_new_ctx()) }
| WHILE lparen exp RPAREN exp_or_block { ExpWhile ($3, $5, make_new_ctx()) }
| FOR for_clauses exp_or_block
    {
        let for_cl_ = $2 in
        let for_body_ = $3 in
        ExpFor ({ for_cl = for_cl_; for_body = for_body_ }, make_new_ctx())
    }
| FOLD fold_clauses exp_or_block
    {
        (* `fold (p=e0; ...) [fold (...) ...] e1`
              is transformed to
           `{
           var acc = e0
           for (...) [for(...) ...] { val p = acc; acc = e1 }
           acc
           }`
        *)
        let acc_tp = make_new_typ() in
        let acc_loc = curr_loc_n 2 (* pat location *) in
        let acc_id = get_unique_id "acc" true in
        let acc_ctx = (acc_tp, acc_loc) in
        let acc_exp = ExpIdent(acc_id, acc_ctx) in
        let acc_pat = PatIdent(acc_id, acc_loc) in
        let ((acc_pat0, fold_exp0), fold_cl) = $2 in
        let acc_decl = DefVal(acc_pat, fold_exp0, [ValMutable], acc_ctx) in
        let for_body = $3 in
        let for_body_loc  = get_exp_loc for_body in
        let acc_expand = DefVal(acc_pat0, acc_exp, [], (acc_tp, for_body_loc)) in
        let acc_update = ExpBinOp(OpSet, acc_exp, for_body, (TypVoid, for_body_loc)) in
        let new_for_body = ExpSeq([acc_expand; acc_update], (TypVoid, for_body_loc)) in
        let for_loc = curr_loc() in
        let new_for = ExpFor ({ for_cl = fold_cl; for_body = new_for_body }, (TypVoid, for_loc)) in
        ExpSeq([acc_decl; new_for; acc_exp], (acc_tp, for_loc))
    }
| TRY exp_or_block CATCH LBRACE BAR pattern_matching_clauses_ RBRACE { ExpTryCatch ($2, (List.rev $6), make_new_ctx()) }
| CCODE STRING { ExpCCode($2, make_new_ctx()) }
| FUN fun_args DOUBLE_ARROW complex_exp
    {
        let ctx = make_new_ctx() in
        let (args, rt, prologue) = $2 in
        let body = expseq2exp (prologue @ [$4]) 4 in
        let df = make_deffun "lambda" args rt body [] (curr_loc()) true in
        let {df_name=f_id} = df in
        ExpSeq([DefFun (ref df); ExpIdent (f_id, ctx)], ctx)
    }
| FUN fun_args block
    {
        let ctx = make_new_ctx() in
        let (args, rt, prologue) = $2 in
        let body = expseq2exp (prologue @ $3) 3 in
        let df = make_deffun "lambda" args rt body [] (curr_loc()) true in
        let {df_name=f_id} = df in
        ExpSeq([DefFun (ref df); ExpIdent (f_id, ctx)], ctx)
    }
| exp { $1 }

simple_exp:
| B_IDENT { ExpIdent((get_id $1), make_new_ctx()) }
| B_LPAREN op_name RPAREN { ExpIdent((get_id $2), make_new_ctx()) }
| literal { ExpLit($1, make_new_ctx()) }
| simple_exp DOT B_IDENT { make_bin_op(OpDot, $1, ExpIdent((get_id $3), (make_new_typ(), curr_loc_n 3))) }
| simple_exp DOT INT { make_bin_op(OpDot, $1, ExpLit((LitInt $3), (make_new_typ(), curr_loc_n 3))) }
| B_LPAREN exp_or_block RPAREN { $2 }
| B_LPAREN complex_exp COMMA exp_list RPAREN { ExpMkTuple(($2 :: $4), make_new_ctx()) }
| B_LPAREN exp COLON typespec RPAREN { ExpTyped($2, $4, make_new_ctx()) }
| B_LPAREN exp CAST typespec RPAREN { ExpCast($2, $4, make_new_ctx()) }
| simple_exp LPAREN exp_list RPAREN
    %prec fcall_prec
    { ExpCall($1, $3, make_new_ctx()) }
| simple_exp LSQUARE idx_list_ RSQUARE
    %prec lsquare_prec
    { ExpAt($1, (List.rev $3), make_new_ctx()) }

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
| B_STAR exp { make_un_op(OpDeref, $2) }
| B_POWER exp { make_un_op(OpDeref, make_un_op(OpDeref, $2)) }
| B_REF exp { make_un_op(OpMakeRef, $2) }
| B_MINUS exp { make_un_op(OpNegate, $2) }
| THROW exp { make_un_op(OpThrow, $2) }
| LOGICAL_NOT exp { make_un_op(OpLogicNot, $2) }
| BITWISE_NOT exp { make_un_op(OpBitwiseNot, $2) }

exp_or_block:
| exp { $1 }
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

op_name:
| B_PLUS { "__add__" }
| B_MINUS  { "__sub__" }
| B_STAR  { "__mul__" }
| SLASH  { "__div__" }
| MOD  { "__mod__" }
| B_POWER  { "__pow__" }
| SHIFT_LEFT  { "__shl__" }
| SHIFT_RIGHT  { "__shr__" }
| BITWISE_AND  { "__bit_and__" }
| BITWISE_OR   { "__bit_or__" }
| BITWISE_XOR  { "__bit_xor__" }
| BITWISE_NOT  { "__bit_not__" }
| EQUAL_TO  { "__eq__" }
| NOT_EQUAL  { "__ne__" }
| LESS  { "__lt__" }
| LESS_EQUAL  { "__le__" }
| GREATER  { "__gt__" }
| GREATER_EQUAL  { "__ge__" }

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

for_clauses:
| for_clauses_ { (List.rev $1) }

for_clauses_:
| for_clauses_ FOR for_clause  { $3 :: $1 }
| for_clause { $1 :: [] }

for_clause:
| lparen for_in_list_ RPAREN { (List.rev $2) }

for_in_list_:
| for_in_list_ COMMA simple_pat IN loop_range_exp { ($3, $5) :: $1 }
| simple_pat IN loop_range_exp { ($1, $3) :: [] }

fold_clauses:
| first_fold_clause { let (e0, cl0) = $1 in (e0, (cl0 :: [])) }
| first_fold_clause FOLD fold_clauses_ { let (e0, cl0) = $1 in (e0, (cl0 :: (List.rev $3))) }

fold_clauses_:
| fold_clauses_ FOLD for_clause  { $3 :: $1 }
| for_clause { $1 :: [] }

first_fold_clause:
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
| pattern_matching_clauses_ BAR matching_patterns_ DOUBLE_ARROW exp_or_block { ((List.rev $3), $5) :: $1 }
| matching_patterns_ DOUBLE_ARROW exp_or_block { ((List.rev $1), $3) :: [] }

matching_patterns_:
| matching_patterns_ BAR simple_pat { $3 :: $1 }
| simple_pat { $1 :: [] }

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
| dot_ident LPAREN simple_pat_list_ RPAREN { PatCtor((get_id $1), (List.rev $3), curr_loc()) }
| dot_ident IDENT { PatCtor((get_id $1), [PatIdent((get_id $2), (curr_loc_n 2))], curr_loc()) }
| simple_pat COLON typespec { PatTyped($1, $3, curr_loc()) }

simple_pat_list:
| simple_pat_list_ { List.rev $1 }
| /* empty */ { [] }

simple_pat_list_:
| simple_pat_list_ COMMA simple_pat { $3 :: $1 }
| simple_pat { $1 :: [] }

val_spec_list_:
| VAL { [] }
| VAR { ValMutable :: [] }

val_decls_:
| val_decls_ COMMA val_decl { $3 :: $1 }
| val_decl { $1 :: [] }

val_decl:
| simple_pat EQUAL exp_or_block { ($1, $3, make_new_ctx()) }

fun_decl_start:
| FUN B_IDENT { ([], $2) }
| OPERATOR op_name { ([], $2) }

fun_args:
| lparen simple_pat_list RPAREN opt_typespec { ($2, $4, []) }

opt_typespec:
| COLON typespec { $2 }
| /* empty */ { make_new_typ() }

typedef_lhs:
| TYPE B_LPAREN tyvar_list_ RPAREN ident { ((List.rev $3), (get_id $5)) }
| TYPE ident { ([], (get_id $2)) }

tyvar_list_:
| tyvar_list_ COMMA TYVAR { (get_id $3) :: $1 }
| TYVAR { (get_id $1) :: [] }

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
| typespec_nf REF
%prec ref_type_prec
{ TypRef($1) }

typespec_list_:
| typespec_list_ COMMA typespec { $3 :: $1 }
| typespec { $1 :: [] }

shapespec:
| shapespec COMMA { $1 + 1 }
| /* empty */ { 0 }

dot_ident:
| B_IDENT { $1 }
| B_IDENT DOT nobreak_dot_ident { $1 ^ "." ^ $3 }

nobreak_dot_ident:
| nobreak_dot_ident DOT ident { $1 ^ "." ^ $3 }
| IDENT { $1 }

lparen:
| B_LPAREN { 0 }
| LPAREN { 0 }

ident:
| B_IDENT { $1 }
| IDENT { $1 }
