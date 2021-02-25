%{
(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* parser for a subset of Ocaml, which is used to implement Ficus compiler *)
open Syntax

let raise_syntax_err msg =
    raise (SyntaxError (msg, Parsing.symbol_start_pos(), Parsing.symbol_end_pos()))

let make_fundef f args body =
    let args = match args with
        | PLit(LUnit) :: [] -> []
        | _ -> List.rev args
        in
    {ocf_name = f; ocf_args = args; ocf_body = body }

let make_variant (tyvars, n) cases =
    List.iter (fun (nj, _) ->
        if good_variant_name nj then ()
        else raise_syntax_err (sprintf "the name '%s' of variant case is invalid" nj)) cases;
    DefVariant { ocv_name=n; ocv_args=tyvars; ocv_cases=cases }

let make_variant_pat vn args =
    if good_variant_name vn then () else
        raise_syntax_err (sprintf "'%s' is not an appropriate variant name" vn);
    PVariant(vn, args)

%}

/* tokens with attributes */
%token <char> CHAR
%token <float> FLOAT
%token <int> INT
%token <int> INT64
%token <string> STRING
%token <string> TYVAR
%token <string> IDENT

/* keywords */
%token AND AS BEGIN DO DONE DOWNTO ELSE END EXCEPTION FALSE FOR
%token FUN FUNCTION IF IN LET LSL LSR MATCH MOD MUTABLE NOT OF OPEN
%token RAISE REC REF THEN TO TRUE TRY TYPE VAL WHEN WHILE WITH

/* symbols */
%token LPAREN RPAREN DOT_LPAREN LSQUARE RSQUARE LSQUARE_VEC RSQUARE_VEC LBRACE RBRACE
%token PLUS MINUS STAR SLASH MINUS_DOT PLUS_DOT STAR_DOT SLASH_DOT
%token LOGICAL_AND LOGICAL_OR STRING_CONCAT LIST_CONCAT CONS
%token EQUAL NOT_EQUAL LESS_EQUAL GREATER_EQUAL LESS GREATER COMMA ASSIGN
%token EXCLAMATION DOT ARROW BACK_ARROW COLON SEMICOLON BAR EOF

%nonassoc IN
%right prec_let
%left SEMICOLON
%right RAISE
%right prec_if
%right BACK_ARROW ASSIGN
%left prec_tuple
%left COMMA
%left WHEN
%right CONS
%left LOGICAL_OR
%left LOGICAL_AND
%left AS
%left EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL
%left LSL LSR
%right LIST_CONCAT
%left PLUS MINUS PLUS_DOT MINUS_DOT STRING_CONCAT
%left STAR SLASH MOD STAR_DOT SLASH_DOT
%left type_app
%right prec_app NOT EXCLAMATION
%left DOT DOT_LPAREN

%type <Syntax.ocexp_t list> toplevel
%type <Syntax.ocexp_t> exp, simple_exp
%type <Syntax.ocpat_t> pat
%type <Syntax.octyp_t> typespec
%start toplevel

%%

toplevel:
| toplevel_defs { List.rev $1 }
| /* empty */ { [] }

toplevel_defs:
| toplevel_defs toplevel_def { $2 :: $1 }
| toplevel_def { $1 :: [] }

toplevel_def:
| EXCEPTION IDENT { EDefExn($2, TUnit) }
| EXCEPTION IDENT OF typespec { EDefExn($2, $4) }
| OPEN idlist_ { EOpen(List.rev $2) }
| TYPE typedefs_ { EDefTyp(List.rev $2) }
| LET REC fundefs_ { ELetRec(List.rev $3, None) }
| LET IDENT simple_pats_ EQUAL exp_seq { ELetRec((make_fundef $2 $3 $5)::[], None) }
| LET simple_pat EQUAL exp_seq { ELet($2, $4, None) }
| LET simple_pat COLON typespec EQUAL exp_seq { ELet($2, ETyped($6, $4), None) }

literal:
| LPAREN RPAREN { LUnit }
| TRUE { LBool(true) }
| FALSE { LBool(false) }
| INT { LInt($1) }
| INT64 { LInt64($1) }
| FLOAT { LFloat($1) }
| STRING { LString($1) }
| CHAR { LChar($1) }
| LSQUARE RSQUARE { LNil }

simple_exp:
| literal { ELit($1) }
| IDENT { EIdent($1) }
| LPAREN exp_seq RPAREN { $2 }
| LPAREN complex_exp COMMA exp_list_ RPAREN
    { EMkTuple($2 :: (List.rev $4)) }
| LPAREN exp COLON typespec RPAREN { ETyped($2, $4) }
| LBRACE rec_init_elems_ RBRACE { EMkRecord(List.rev $2) }
| LBRACE rec_init_elems_ SEMICOLON RBRACE { EMkRecord(List.rev $2) }
| LBRACE simple_exp WITH rec_init_elems_ RBRACE { EUpdateRecord($2, (List.rev $4)) }
| LSQUARE exp_seq_ RSQUARE { EMkList(List.rev $2) }
| LSQUARE_VEC exp_seq_ RSQUARE_VEC { EMkVector(List.rev $2) }
| simple_exp DOT IDENT
    {
        let e = $1 in let m = $3 in
        match e with
        | EIdent n -> EIdent(n ^ "." ^ m)
        | _ -> EBinary(OpMem, e, EIdent(m))
    }
| simple_exp DOT_LPAREN exp RPAREN { EBinary(OpAt, $1, $3) }
| EXCLAMATION simple_exp
    %prec prec_app
    { EUnary(OpDeref, $2) }

exp:
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { EUnary(OpNot, $2) }
| MINUS exp
    %prec prec_app
    { EUnary(OpNeg, $2) }
| exp LIST_CONCAT exp
    { EBinary(OpConcat, $1, $3) }
| exp STRING_CONCAT exp
    { EBinary(OpConcat, $1, $3) }
| exp PLUS exp
    { EBinary(OpAdd, $1, $3) }
| exp MINUS exp
    { EBinary(OpSub, $1, $3) }
| exp STAR exp
    { EBinary(OpMul, $1, $3) }
| exp SLASH exp
    { EBinary(OpDiv, $1, $3) }
| exp PLUS_DOT exp
    { EBinary(OpAdd, $1, $3) }
| exp MINUS_DOT exp
    { EBinary(OpSub, $1, $3) }
| exp STAR_DOT exp
    { EBinary(OpMul, $1, $3) }
| exp SLASH_DOT exp
    { EBinary(OpDiv, $1, $3) }
| exp MOD exp
    { EBinary(OpMod, $1, $3) }
| exp LSL exp
    { EBinary(OpSHL, $1, $3) }
| exp LSR exp
    { EBinary(OpSHR, $1, $3) }
| exp EQUAL exp
    { EBinary(OpEQ, $1, $3) }
| exp NOT_EQUAL exp
    { EBinary(OpNE, $1, $3) }
| exp LESS exp
    { EBinary(OpLT, $1, $3) }
| exp GREATER exp
    { EBinary(OpGT, $1, $3) }
| exp LESS_EQUAL exp
    { EBinary(OpLE, $1, $3) }
| exp GREATER_EQUAL exp
    { EBinary(OpGE, $1, $3) }
| exp CONS exp
    { EBinary(OpCons, $1, $3) }
| exp LOGICAL_AND exp
    { EBinary(OpLogicAnd, $1, $3) }
| exp LOGICAL_OR exp
    { EBinary(OpLogicOr, $1, $3) }
| MINUS_DOT exp
    %prec prec_app
    { EUnary(OpNeg, $2) }
| REF exp
    %prec prec_app
    { EUnary(OpMkRef, $2) }
| RAISE simple_exp { ERaise($2) }
| WHILE exp DO exp_seq DONE { EWhile($2, $4) }
| FOR IDENT EQUAL exp TO exp DO exp_seq DONE { EFor(true, $2, $4, $6, $8) }
| FOR IDENT EQUAL exp DOWNTO exp DO exp_seq DONE { EFor(false, $2, $4, $6, $8) }
| simple_exp actual_args_
    %prec prec_app
    {
        let args = match $2 with
            | ELit(LUnit) :: [] -> []
            | args -> List.rev args
            in
        ECall($1, args)
    }
| error
    { raise_syntax_err "unexpected token" }

complex_exp:
| exp { $1 }
| exp BACK_ARROW complex_exp
    { EBinary(OpAssign, $1, $3) }
| exp ASSIGN complex_exp
    { EBinary(OpAssign, $1, $3) }
| IF exp THEN complex_exp ELSE complex_exp
    %prec prec_if
    { EIf($2, $4, $6) }
| LET simple_pat EQUAL complex_exp IN exp_seq
    %prec prec_let
    { ELet($2, $4, Some $6) }
| LET IDENT simple_pats_ EQUAL exp_seq IN exp_seq
    %prec prec_let
    { ELetRec((make_fundef $2 $3 $5)::[], (Some $7)) }
| LET REC fundefs_ IN exp_seq
    %prec prec_let
    { ELetRec((List.rev $3), Some $5) }
| MATCH exp WITH match_cases { EMatch($2, $4) }
| TRY exp_seq WITH match_cases { ETry($2, $4) }
| FUNCTION match_cases { ELambdaCases($2) }
| FUN simple_pats_ ARROW exp_seq { ELambda((List.rev $2), $4) }

exp_seq:
| exp_seq_
{
    match $1 with
    | x :: [] -> x
    | xs -> EBlock(List.rev xs)
}

exp_seq_:
| exp_seq_ SEMICOLON complex_exp { $3 :: $1 }
| complex_exp { $1 :: [] }

exp_list_:
| exp_list_ COMMA complex_exp { $3 :: $1 }
| complex_exp { $1 :: [] }

fundefs_:
| fundefs_ AND fundef { $3 :: $1 }
| fundef { $1 :: [] }

fundef:
| IDENT simple_pats_ EQUAL exp_seq { make_fundef $1 $2 $4 }

actual_args_:
| actual_args_ simple_exp
    %prec prec_app
    { $2 :: $1 }
| simple_exp
    %prec prec_app
    { $1 :: [] }

rec_init_elems_:
| rec_init_elems_ SEMICOLON rec_init_elem { $3 :: $1 }
| rec_init_elem { $1 :: [] }

rec_init_elem:
| IDENT EQUAL exp { ($1, $3) }
| IDENT { ($1, EIdent($1)) }

match_cases:
| match_cases_ { List.rev $1 }
| BAR match_cases_ { List.rev $2 }

match_cases_:
| match_cases_ BAR match_case { $3 :: $1 }
| match_case { $1 :: [] }

match_case:
| pat_options_ ARROW exp_seq { (List.rev $1, $3) }

pat_options_:
| pat_options_ BAR patx { $3 :: $1 }
| patx { $1 :: [] }

simple_pats_:
| simple_pats_ simple_pat { $2 :: $1 }
| simple_pat { $1 :: [] }

patx:
| pat_list_
{
    match $1 with
    | p :: [] -> p
    | ps -> PTuple(List.rev ps)
}

pat_list_:
| pat_list_ COMMA pat { $3 :: $1 }
| pat { $1 :: [] }

rec_pat_list_:
| rec_pat_list_ SEMICOLON rec_pat_elem { $3 :: $1 }
| rec_pat_elem { $1 :: [] }

rec_pat_elem:
| IDENT { ($1, PIdent($1)) }
| IDENT EQUAL pat { ($1, $3) }

pat:
| literal { PLit($1) }
| ident_
{
    let i = $1 in
    match i with
    | "_" -> PAny
    | _ -> if good_variant_name i then PVariant(i, []) else PIdent(i)
}
| LPAREN patx RPAREN { $2 }
| LBRACE rec_pat_list_ RBRACE { PRecord(noid, (List.rev $2)) }
| ident_ IDENT { make_variant_pat $1 ((PIdent $2) :: []) }
| ident_ literal { make_variant_pat $1 ((PLit $2) :: []) }
| ident_ LPAREN pat RPAREN { make_variant_pat $1 ($3 :: []) }
| ident_ LPAREN pat COMMA pat_list_ RPAREN { make_variant_pat $1 ($3 :: (List.rev $5)) }
| ident_ LBRACE rec_pat_list_ RBRACE
{
    let vn = $1 in
    if good_variant_name vn then () else
        raise_syntax_err (sprintf "'%s' is not an appropriate variant name" vn);
    PRecord(vn, (List.rev $3))
}
| pat AS IDENT { PAs($1, $3) }
| pat WHEN exp { PWhen($1, $3) }
| pat CONS pat { PCons($1, $3) }
| LPAREN pat COLON typespec RPAREN { PTyped($2, $4) }

simple_pat:
| ident_
    {
        let i = $1 in
        match i with
        | "_" -> PAny
        | _ -> PIdent($1)
    }
| simple_pat AS IDENT { PAs($1, $3) }
| LPAREN RPAREN { PLit(LUnit) }
| LPAREN simple_pat COMMA simple_pat_list_ RPAREN { PTuple($2 :: (List.rev $4)) }
| LPAREN simple_pat COLON typespec RPAREN { PTyped($2, $4) }
| LBRACE simple_rec_pat_list_ RBRACE { PRecord(noid, (List.rev $2)) }

simple_pat_list_:
| simple_pat_list_ COMMA simple_pat { $3 :: $1 }
| simple_pat { $1 :: [] }

simple_rec_pat_list_:
| simple_rec_pat_list_ SEMICOLON simple_rec_pat_elem { $3 :: $1 }
| simple_rec_pat_elem { $1 :: [] }

simple_rec_pat_elem:
| IDENT { ($1, PIdent($1)) }
| IDENT EQUAL simple_pat { ($1, $3) }

typedefs_:
| typedefs_ AND typedef { $3 :: $1 }
| typedef { $1 :: [] }

typedef:
| typedef_start EQUAL typespec
    {
        let (tyvars, n) = $1 in
        DefTyp { oct_name=n; oct_args=tyvars; oct_body=$3 }
    }
| typedef_start EQUAL var_case BAR var_cases_
    {
        make_variant $1 ($3 :: (List.rev $5))
    }
| typedef_start EQUAL BAR var_cases_
    {
        make_variant $1 (List.rev $4)
    }

typedef_start:
| IDENT { ([], $1) }
| TYVAR IDENT { ($1::[], $2) }
| LPAREN tyvar_list_ RPAREN IDENT { ((List.rev $2), $4) }

tyvar_list_:
| tyvar_list_ COMMA TYVAR { $3 :: $1 }
| TYVAR { $1 :: [] }

typespec:
| typespec_t { $1 }
| typespec_t ARROW typespec { TFun($1, $3) }

typespec_t:
| typespec_t_
    {
        match $1 with
        | t :: [] -> t
        | _ -> TTuple(List.rev $1)
    }

typespec_t_:
| typespec_t_ STAR typespec_nf { $3 :: $1 }
| typespec_nf { $1 :: [] }

typespec_nf:
| LPAREN typespec RPAREN { $2 }
| ident_
    {
        match $1 with
        | "int" -> TInt
        | "bool" -> TBool
        | "float" -> TFloat
        | "string" -> TString
        | "char" -> TChar
        | "unit" -> TUnit
        | _ -> TName $1
    }
| TYVAR { TName $1 }
| typespec_nf ident_
%prec type_app
    {
        let t = $1 in
        let n = $2 in
        match n with
        | "list" -> TList(t)
        | _ -> TApp(t :: [], n)
    }
| typespec_nf REF
%prec type_app
    {
        let t = $1 in
        TRef(t)
    }
| LPAREN typespec COMMA typespec_list_ RPAREN ident_
%prec prec_app
    {
        let tl = $2 :: (List.rev $4) in
        let n = $6 in
        TApp(tl, n)
    }
| LBRACE rec_elems_decls_ RBRACE { TRecord(List.rev $2) }
| LBRACE rec_elems_decls_ SEMICOLON RBRACE { TRecord(List.rev $2) }

typespec_list_:
| typespec_list_ COMMA typespec { $3 :: $1 }
| typespec { $1 :: [] }

rec_elems_decls_:
| rec_elems_decls_ SEMICOLON rec_elem_decl { $3 :: $1 }
| rec_elem_decl { $1 :: [] }

rec_elem_decl:
| IDENT COLON typespec { (false, $1, $3) }
| MUTABLE IDENT COLON typespec { (true, $2, $4) }

var_cases_:
| var_cases_ BAR var_case { $3 :: $1 }
| var_case { $1 :: [] }

var_case:
| IDENT { ($1, TUnit) }
| IDENT OF typespec { ($1, $3) }

ident_:
| ident_ DOT IDENT { $1 ^ "." ^ $3 }
| IDENT { $1 }

idlist_:
| idlist_ COMMA ident_ { $3 :: $1 }
| ident_ { $1 :: [] }
