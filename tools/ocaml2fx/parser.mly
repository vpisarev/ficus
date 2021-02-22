%{
(* parser for a subset of Ocaml, which is used to implement Ficus compiler *)
open Syntax

let raise_syntax_err msg =
    raise (SyntaxError (msg, Parsing.symbol_start_pos(), Parsing.symbol_end_pos()))

let make_fundef f args body =
    let args = match args with
        | CpLit(ClUnit) :: [] -> []
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
    CpVariant(vn, args)

%}

/* tokens with attributes */
%token <bool> BOOL
%token <char> CHAR
%token <float> FLOAT
%token <int> INT
%token <string> STRING
%token <string> TYVAR
%token <string> IDENT

/* keywords */
%token AND AS BEGIN DO DONE DOWNTO ELSE END EXCEPTION FALSE FOR
%token FUN FUNCTION IF IN LET LSL LSR MATCH MOD MUTABLE NOT OF OPEN
%token REC REF THEN TO TRUE TRY TYPE VAL WHEN WHILE WITH

/* symbols */
%token LPAREN RPAREN DOT_LPAREN LSQUARE RSQUARE LSQUARE_VEC RSQUARE_VEC LBRACE RBRACE
%token PLUS MINUS STAR SLASH MINUS_DOT PLUS_DOT STAR_DOT SLASH_DOT
%token LOGICAL_AND LOGICAL_OR LOGICAL_NOT STRING_CONCAT LIST_CONCAT CONS
%token EQUAL NOT_EQUAL LESS_EQUAL GREATER_EQUAL LESS GREATER COMMA ASSIGN
%token EXCLAMATION DOT ARROW BACK_ARROW COLON SEMICOLON BAR EOF

%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right BACK_ARROW ASSIGN
%nonassoc prec_tuple
%left COMMA
%left WHEN
%right CONS
%left LOGICAL_OR
%left LOGICAL_AND
%left AS
%left EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL
%left LSL LSR
%left PLUS MINUS PLUS_DOT MINUS_DOT STRING_CONCAT LIST_CONCAT
%left STAR SLASH MOD STAR_DOT SLASH_DOT
%right prec_app LOGICAL_NOT EXCLAMATION

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
| EXCEPTION IDENT { CeDefExn($2, CtUnit) }
| EXCEPTION IDENT OF typespec { CeDefExn($2, $4) }
| OPEN idlist_ { CeOpen(List.rev $2) }
| TYPE typedefs_ { CeDefTyp(List.rev $2) }
| LET REC fundefs_ { CeLetRec(List.rev $3, None) }
| LET IDENT simple_pats_ EQUAL exp_seq { CeLetRec((make_fundef $2 $3 $5)::[], None) }
| LET simple_pat EQUAL exp_seq { CeLet($2, $4, None) }

literal:
| LPAREN RPAREN { ClUnit }
| BOOL { ClBool($1) }
| INT { ClInt($1) }
| FLOAT { ClFloat($1) }
| STRING { ClString($1) }
| LSQUARE RSQUARE { ClNil }

simple_exp:
| literal { CeLit($1) }
| LPAREN exp_seq RPAREN { $2 }
| LPAREN complex_exp COMMA exp_list_ RPAREN
    { CeMkTuple($2 :: (List.rev $4)) }
| LPAREN exp COLON typespec RPAREN { $2 }
| LBRACE rec_init_elems_ RPAREN { CeMkRecord(List.rev $2) }
| LBRACE simple_exp WITH rec_init_elems_ RPAREN { CeUpdateRecord($2, (List.rev $4)) }
| LSQUARE exp_seq_ RPAREN { CeMkList(List.rev $2) }
| LSQUARE_VEC exp_seq_ RSQUARE_VEC { CeMkVector(List.rev $2) }
| ident_ { CeIdent($1) }
| simple_exp DOT_LPAREN exp RPAREN { CeBinary(COpAt, $1, $3) }

exp:
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { CeUnary(COpNot, $2) }
| MINUS exp
    %prec prec_app
    { CeUnary(COpNeg, $2) }
| exp PLUS exp
    { CeBinary(COpAdd, $1, $3) }
| exp MINUS exp
    { CeBinary(COpSub, $1, $3) }
| exp STAR exp
    { CeBinary(COpMul, $1, $3) }
| exp SLASH exp
    { CeBinary(COpDiv, $1, $3) }
| exp PLUS_DOT exp
    { CeBinary(COpAdd, $1, $3) }
| exp MINUS_DOT exp
    { CeBinary(COpSub, $1, $3) }
| exp STAR_DOT exp
    { CeBinary(COpMul, $1, $3) }
| exp SLASH_DOT exp
    { CeBinary(COpDiv, $1, $3) }
| exp MOD exp
    { CeBinary(COpMod, $1, $3) }
| exp LSL exp
    { CeBinary(COpSHL, $1, $3) }
| exp LSR exp
    { CeBinary(COpSHR, $1, $3) }
| exp EQUAL exp
    { CeBinary(COpEQ, $1, $3) }
| exp NOT_EQUAL exp
    { CeBinary(COpNE, $1, $3) }
| exp LESS exp
    { CeBinary(COpLT, $1, $3) }
| exp GREATER exp
    { CeBinary(COpGT, $1, $3) }
| exp LESS_EQUAL exp
    { CeBinary(COpLE, $1, $3) }
| exp GREATER_EQUAL exp
    { CeBinary(COpGE, $1, $3) }
| exp CONS exp
    { CeBinary(COpCons, $1, $3) }
| exp LOGICAL_AND exp
    { CeBinary(COpLogicAnd, $1, $3) }
| exp LOGICAL_OR exp
    { CeBinary(COpLogicOr, $1, $3) }
| exp BACK_ARROW exp
    { CeBinary(COpAssign, $1, $3) }
| exp ASSIGN exp
    { CeBinary(COpAssign, $1, $3) }
| MINUS_DOT exp
    %prec prec_app
    { CeUnary(COpNeg, $2) }
| EXCLAMATION exp
    %prec prec_app
    { CeUnary(COpDeref, $2) }
| REF exp
    %prec prec_app
    { CeUnary(COpMkRef, $2) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { CeIf($2, $4, $6) }
| WHILE exp DO exp_seq DONE { CeWhile($2, $4) }
| FOR IDENT EQUAL exp TO exp DO exp_seq DONE { CeFor(true, $2, $4, $6, $8) }
| FOR IDENT EQUAL exp DOWNTO exp DO exp_seq DONE { CeFor(false, $2, $4, $6, $8) }
| simple_exp actual_args_
    %prec prec_app
    {
        let args = match $2 with
            | CeLit(ClUnit) :: [] -> []
            | args -> args
            in
        CeCall($1, args)
    }
| error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

complex_exp:
| exp { $1 }
| LET simple_pat EQUAL exp IN exp_seq
    %prec prec_let
    { CeLet($2, $4, Some $6) }
| LET IDENT simple_pats_ EQUAL exp_seq IN exp_seq
    %prec prec_let
    { CeLetRec((make_fundef $2 $3 $5)::[], (Some $7)) }
| LET REC fundefs_ IN exp_seq
    %prec prec_let
    { CeLetRec((List.rev $3), Some $5) }
| MATCH exp WITH match_cases { CeMatch($2, $4) }
| TRY exp WITH match_cases { CeTry($2, $4) }
| FUNCTION match_cases { CeLambdaCases($2) }
| FUN simple_pats_ ARROW exp_seq { CeLambda($2, $4) }

exp_seq:
| exp_seq_
{
    match $1 with
    | x :: [] -> x
    | xs -> CeBlock(List.rev xs)
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
| IDENT simple_pats_ EQUAL exp { make_fundef $1 $2 $4 }

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
    | ps -> CpTuple(List.rev ps)
}

pat_list_:
| pat_list_ COMMA pat { $3 :: $1 }
| pat { $1 :: [] }

rec_pat_list_:
| rec_pat_list_ SEMICOLON rec_pat_elem { $3 :: $1 }
| rec_pat_elem { $1 :: [] }

rec_pat_elem:
| IDENT { ($1, CpIdent($1)) }
| IDENT EQUAL pat { ($1, $3) }

pat:
| literal { CpLit($1) }
| ident_
{
    let i = $1 in
    match i with
    | "_" -> CpAny
    | _ -> if good_variant_name i then CpVariant(i, []) else CpIdent(i)
}
| LPAREN patx RPAREN { $2 }
| LBRACE rec_pat_list_ RPAREN { CpRecord(noid, (List.rev $2)) }
| ident_ IDENT { make_variant_pat $1 ((CpIdent $2) :: []) }
| ident_ literal { make_variant_pat $1 ((CpLit $2) :: []) }
| ident_ LPAREN pat RPAREN { make_variant_pat $1 ($3 :: []) }
| ident_ LPAREN pat COMMA pat_list_ RPAREN { make_variant_pat $1 ($3 :: (List.rev $5)) }
| ident_ LBRACE rec_pat_list_ RPAREN
{
    let vn = $1 in
    if good_variant_name vn then () else
        raise_syntax_err (sprintf "'%s' is not an appropriate variant name" vn);
    CpRecord(vn, (List.rev $3))
}
| pat AS IDENT { CpAs($1, $3) }
| pat WHEN exp { CpWhen($1, $3) }
| pat CONS pat { CpCons($1, $3) }
| LPAREN pat COLON typespec RPAREN { CpTyped($2, $4) }

simple_pat:
| ident_ { CpIdent($1) }
| simple_pat AS IDENT { CpAs($1, $3) }
| LPAREN simple_pat COMMA simple_pat_list_ RPAREN { CpTuple($2 :: (List.rev $4)) }
| LPAREN simple_pat COLON typespec RPAREN { CpTyped($2, $4) }
| LBRACE simple_rec_pat_list_ RPAREN { CpRecord(noid, (List.rev $2)) }

simple_pat_list_:
| simple_pat_list_ COMMA simple_pat { $3 :: $1 }
| simple_pat { $1 :: [] }

simple_rec_pat_list_:
| simple_rec_pat_list_ SEMICOLON simple_rec_pat_elem { $3 :: $1 }
| simple_rec_pat_elem { $1 :: [] }

simple_rec_pat_elem:
| IDENT { ($1, CpIdent($1)) }
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
| typespec_nf { $1 }
| typespec_nf ARROW typespec { CtFun($1, $3) }

typespec_nf:
| LPAREN typespec_list_ RPAREN
{
    match $2 with
    | t :: [] -> t
    | tl -> CtTuple tl
}
| ident_
{
    match $1 with
    | "int" -> CtInt
    | "bool" -> CtBool
    | "float" -> CtFloat
    | "string" -> CtString
    | "char" -> CtChar
    | "unit" -> CtUnit
    | _ -> CtName $1
}
| TYVAR { CtName $1 }
| typespec_nf IDENT
    {
        let t = $1 in
        let n = $2 in
        match n with
        | "list" -> CtList(t)
        | "ref" -> CtRef(t)
        | _ ->
            let tl = match t with
            | CtTuple(tl) -> tl
            | _ -> t :: []
            in CtApp(tl, n)
    }
| LBRACE rec_elems_decls_ RBRACE { CtRecord(List.rev $2) }

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
| IDENT { ($1, CtUnit) }
| IDENT OF typespec { ($1, $3) }

ident_:
| ident_ DOT IDENT { $1 ^ "." ^ $3 }
| IDENT { $1 }

idlist_:
| idlist_ COMMA ident_ { $3 :: $1 }
| ident_ { $1 :: [] }
