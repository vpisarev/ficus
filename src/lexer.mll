{
(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* lexer *)
open Ast
open Parser
open Lexing
open Printf

let token2str t = match t with
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | INT(i) -> sprintf "INT(%Ld)" i
    | SINT(b, i) -> sprintf "SINT(%d, %Ld)" b i
    | UINT(b, i) -> sprintf "UINT(%d, %Ld)" b i
    | FLOAT(b, f) -> sprintf "FLOAT(%d, %g)" b f
    | FLOAT_LIKE(s) -> sprintf "FLOAT_LIKE(%s)" s
    | IDENT(s) -> sprintf "IDENT(%s)" s
    | B_IDENT(s) -> sprintf "B_IDENT(%s)" s
    | STRING(s) -> sprintf "STRING(%s)" s
    | CHAR(s) -> sprintf "CHAR(%s)" s
    | TYVAR(s) -> sprintf "TYVAR(%s)" s
    | AS -> "AS"
    | AT -> "AT"
    | BREAK -> "BREAK"
    | CATCH -> "CATCH"
    | CCODE -> "CCODE"
    | CLASS -> "CLASS"
    | CONTINUE -> "CONTINUE"
    | DO -> "DO"
    | ELLIPSIS -> "ELLIPSIS"
    | ELSE -> "ELSE"
    | EXCEPTION -> "EXCEPTION"
    | EXTENDS -> "EXTENDS"
    | FOLD -> "FOLD"
    | B_FOR -> "B_FOR"
    | FOR -> "FOR"
    | FROM -> "FROM"
    | FUN -> "FUN"
    | IF -> "IF"
    | IMPLEMENTS -> "IMPLEMENTS"
    | B_IMPORT -> "B_IMPORT"
    | IMPORT -> "IMPORT"
    | INLINE -> "INLINE"
    | INTERFACE -> "INTERFACE"
    | MATCH -> "MATCH"
    | NOTHROW -> "NOTHROW"
    | OPERATOR -> "OPERATOR"
    | PARALLEL -> "PARALLEL"
    | PURE -> "PURE"
    | REF -> "MAKE_REF"
    | REF_TYPE -> "REF_TYPE"
    | STATIC -> "STATIC"
    | THROW -> "THROW"
    | TRY -> "TRY"
    | TYPE -> "TYPE"
    | VAL -> "VAL"
    | VAR -> "VAR"
    | WHEN -> "WHEN"
    | WITH -> "WITH"
    | B_WHILE -> "B_WHILE"
    | WHILE -> "WHILE"
    | B_LPAREN -> "B_LPAREN"
    | LPAREN -> "LPAREN"
    | STR_INTERP_LPAREN -> "STR_INTERP_LPAREN"
    | RPAREN -> "RPAREN"
    | B_LSQUARE -> "B_LSQUARE"
    | LSQUARE -> "LSQUARE"
    | RSQUARE -> "RSQUARE"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | LLIST -> "LLIST"
    | RLIST -> "RLIST"
    | COMMA -> "COMMA"
    | DOT -> "DOT"
    | SEMICOLON -> "SEMICOLON"
    | COLON -> "COLON"
    | BAR -> "BAR"
    | CONS -> "CONS"
    | CAST -> "CAST"
    | BACKSLASH -> "BACKSLASH"
    | BACK_ARROW -> "BACK_ARROW"
    | DOUBLE_ARROW -> "DOUBLE_ARROW"
    | ARROW -> "ARROW"
    | QUESTION -> "QUESTION"
    | EOF -> "EOF"
    | B_MINUS -> "B_MINUS"
    | MINUS -> "MINUS"
    | B_PLUS -> "B_PLUS"
    | PLUS -> "PLUS"
    | B_STAR -> "B_STAR"
    | STAR -> "STAR"
    | SLASH -> "SLASH"
    | MOD -> "MOD"
    | B_POWER -> "B_POWER"
    | POWER -> "POWER"
    | DOT_STAR -> "DOT_STAR"
    | B_DOT_MINUS -> "B_DOT_MINUS"
    | DOT_SLASH -> "DOT_SLASH"
    | DOT_MOD -> "DOT_MOD"
    | DOT_POWER -> "DOT_POWER"
    | SHIFT_RIGHT -> "SHIFT_RIGHT"
    | SHIFT_LEFT -> "SHIFT_LEFT"
    | BITWISE_AND -> "BITWISE_AND"
    | BITWISE_XOR -> "BITWISE_XOR"
    | BITWISE_OR -> "BITWISE_OR"
    | BITWISE_NOT -> "BITWISE_NOT"
    | LOGICAL_AND -> "LOGICAL_AND"
    | LOGICAL_OR -> "LOGICAL_OR"
    | LOGICAL_NOT -> "LOGICAL_NOT"
    | EQUAL -> "EQUAL"
    | PLUS_EQUAL -> "PLUS_EQUAL"
    | MINUS_EQUAL -> "MINUS_EQUAL"
    | STAR_EQUAL -> "STAR_EQUAL"
    | SLASH_EQUAL -> "SLASH_EQUAL"
    | DOT_EQUAL -> "DOT_EQUAL"
    | MOD_EQUAL -> "MOD_EQUAL"
    | AND_EQUAL -> "AND_EQUAL"
    | OR_EQUAL -> "OR_EQUAL"
    | XOR_EQUAL -> "XOR_EQUAL"
    | SHIFT_LEFT_EQUAL -> "SHIFT_LEFT_EQUAL"
    | SHIFT_RIGHT_EQUAL -> "SHIFT_RIGHT_EQUAL"
    | DOT_STAR_EQUAL -> "DOT_STAR_EQUAL"
    | DOT_SLASH_EQUAL -> "DOT_SLASH_EQUAL"
    | DOT_MOD_EQUAL -> "DOT_MOD_EQUAL"
    | SPACESHIP -> "SPACESHIP"
    | CMP_EQ -> "CMP_EQ"
    | CMP_NE -> "CMP_NE"
    | CMP_LE -> "CMP_LE"
    | CMP_GE -> "CMP_GE"
    | CMP_LT -> "CMP_LT"
    | CMP_GT -> "CMP_GT"
    | DOT_SPACESHIP -> "DOT_SPACESHIP"
    | DOT_CMP_EQ -> "DOT_CMP_EQ"
    | DOT_CMP_NE -> "DOT_CMP_NE"
    | DOT_CMP_LE -> "DOT_CMP_LE"
    | DOT_CMP_GE -> "DOT_CMP_GE"
    | DOT_CMP_LT -> "DOT_CMP_LT"
    | DOT_CMP_GT -> "DOT_CMP_GT"
    | FOLD_RESULT -> "FOLD_RESULT"

let token2str_pp t =
    "'" ^ (match t with
    | LPAREN -> "("
    | LSQUARE -> "["
    | LBRACE -> "{"
    | RPAREN -> ")"
    | RSQUARE -> "]"
    | RBRACE -> "}"
    | SEMICOLON -> ";"
    | DOUBLE_ARROW -> "=>"
    | _ -> String.lowercase_ascii (token2str t)) ^ "'"

let keywords = Hashtbl.create 101
let fname = ref "unknown"

(* kwtyp:
   0 - single-word keyword
   1 - a keyword that cannot start a new expression, but
       it links previous part of expression with the subsequent one;
       so it can immediately follow expression (be placed on the same line),
       e.g. "else" in if-then expression
   2 - a keyword that starts a new expression; it cannot follow another expression without
       some explicit operator or a separator
   3 - a keyword that can play a role of a connector (type 1) or an expression beginning (type 2),
       depending on context
*)
let _ = List.iter (fun(kwd, tok, kwtyp) -> Hashtbl.add keywords kwd (tok, kwtyp))
    [
        ("as", AS, 1); ("break", BREAK, 0); ("catch", CATCH, 1); ("ccode", CCODE, 2);
        ("class", CLASS, 2); ("continue", CONTINUE, 0); ("do", DO, 2);
        ("else", ELSE, 1); ("exception", EXCEPTION, 2); ("extends", EXTENDS, 1); ("false", FALSE, 0);
        ("fold", FOLD, 2); ("for", FOR, 2); ("from", FROM, 2); ("fun", FUN, 2);
        ("if", IF, 2); ("implements", IMPLEMENTS, 1); ("import", IMPORT, 3); ("inline", INLINE, 2);
        ("interface", INTERFACE, 2); ("match", MATCH, 2); ("nothrow", NOTHROW, 2); ("operator", OPERATOR, 2);
        ("parallel", PARALLEL, 2); ("pure", PURE, 2); ("ref", REF, 3); ("static", STATIC, 2);
        ("throw", THROW, 2); ("true", TRUE, 0); ("try", TRY, 2); ("type", TYPE, 2);
        ("val", VAL, 2); ("var", VAR, 2); ("when", WHEN, 1); ("while", WHILE, 2); ("with", WITH, 1);
        ("__fold_result__", FOLD_RESULT, -1)
    ]

let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
        pos_fname = !fname;
        pos_lnum = pos.pos_lnum + 1;
        pos_bol = pos.pos_cnum;
    }

exception LexError of string * (position * position)
let get_token_pos lexbuf = (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let pos2str pos print_fname =
    let { pos_fname; pos_lnum; pos_bol; pos_cnum } = pos in
    if print_fname then
        sprintf "%s:%d:%d" (if pos_fname <> "" then pos_fname else !fname) pos_lnum (pos_cnum - pos_bol + 1)
    else
        sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let lexErrAt msg pos = LexError (msg, pos)

let lexErr msg lexbuf =
    LexError(msg, (get_token_pos lexbuf))

let string_literal = ref ""
let string_start = ref dummy_pos
let string_interp_elem = ref 0
let string_tokens = ref ([] : token list)

let comment_start = ref dummy_pos
let comments_level = ref 0

let ccode_mode = ref false
let ccode_literal = ref ""
let ccode_start = ref dummy_pos
let ccode_string_quote = ref ' '
let ccode_string_start = ref dummy_pos

let add_ccode s = ccode_literal := !ccode_literal ^ s
let add_ccode_char c = ccode_literal := !ccode_literal ^ (String.make 1 c)

let new_exp = ref true

(* the stack of tokens. Can contain the following characters:
   LPAREN, LSQUARE, LBRACE - corresponds to various parentheses;
                    wait for the corresponding closing bracket.
   when } is met, remove the corresponding { or { | from stack.
   when '|' is met and { | is on the top, return BAR token.
      otherwise return BITWISE_OR token.
   when '\n', '\r\n' or '\r' is met and if we are inside (), [], newline is ignored (yet we increment lineno).
   when ')' is met, remove => (if any) and the matching '('.
*)
let paren_stack = ref []

let push_paren_stack tk lexbuf =
    paren_stack := (tk, lexbuf.lex_start_p) :: !paren_stack

let unmatchedTokenMsg t0 expected_list =
    let expected_str = List.fold_left (fun str t -> let t_str = token2str_pp t in
                if str = "" then t_str else str ^ "/" ^ t_str) "" expected_list in
    let found_in_stack = expected_str = "" ||
        (List.exists (fun (t, _) -> List.exists (fun t1 -> t = t1) expected_list) !paren_stack) in
    if not found_in_stack then
        sprintf "%s without preceding %s." (token2str_pp t0) expected_str
    else
    match !paren_stack with
    | (_, p) :: _ ->
        let (kw, expected) =
            (match !paren_stack with
            | (LPAREN, _) :: _ -> ("Unmatched '('", "")
            | (LSQUARE, _) :: _ -> ("Unmatched '['", "")
            | (LBRACE, _) :: _ -> ("Unmatched '{'", "")
            | (LLIST, _) :: _ -> ("Unmatched '[:'", "")
            | (t1, _) :: _ -> ((token2str t1), "")
            | _ -> ("<START>", ""))
        in let expected_msg = if expected <> "" then ", " ^ expected ^ " is expected" else "" in
        sprintf "%s at %s%s." kw (pos2str p false) expected_msg
    | _ -> sprintf "Unexpected %s." (token2str_pp t0)

let check_ne lexbuf =
    if !new_exp then
        ()
    else
        raise (lexErr "Missing separator" lexbuf)

let decode_special_char lexbuf c = match c with
    | "\\\"" -> "\""
    | "\\\\" -> "\\"
    | "\\n" -> "\n"
    | "\\t" -> "\t"
    | "\\r" -> "\r"
    | "\\b" -> "\b"
    | "\\{" -> "{"
    | "\\0" -> "\x00"
    | _ -> raise (lexErr (sprintf "Invalid control character '%s'" c) lexbuf)

let decode_hex_char hexcode =
    String.make 1 (Char.chr (int_of_string ("0" ^ (String.sub hexcode 1 ((String.length hexcode)-1)))))

let decode_oct_char octcode =
    String.make 1 (Char.chr (int_of_string ("0o" ^ (String.sub octcode 1 ((String.length octcode)-1)))))

let make_char_literal lexbuf c =
    check_ne(lexbuf);
    new_exp := false;
    [CHAR c]

}

let newline = '\n' | '\r' | "\r\n"
let space = [' ' '\t' '\012']
let digit = ['0'-'9']
let octdigit = ['0' - '7']
let hexdigit = ['0' - '9' 'a' - 'f' 'A' - 'F']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let special_char = "\\'" | "\\\"" | "\\n" | "\\t" | "\\r" | "\\b" | "\\0" | "\\\\" | "\\{"
let hexcode = "\\x" hexdigit hexdigit
let octcode = "\\" octdigit octdigit octdigit

rule tokens = parse
    | newline
        {
            (* inside () or [] parentheses newline characters do not start a new statement/expression
               (because the current one is not finished until we close all the opened parentheses) *)
            (match !paren_stack with
            | (LPAREN, _) :: _ | (LSQUARE, _) :: _ -> ()
            | _ -> new_exp := true);
            incr_lineno lexbuf; tokens lexbuf
        }
    | space +  { tokens lexbuf }

    | "/*/" { tokens lexbuf }
    | "/*"
        {
            comment_start := lexbuf.lex_start_p;
            comments_level := 1;
            comments lexbuf;
            tokens lexbuf
        }
    | "//"  { if (eol_comments lexbuf) = EOF then [EOF] else tokens lexbuf }

    | "\""
        {
            check_ne(lexbuf);
            if !string_interp_elem <> 0 then
               raise (lexErr "Unexpected '\"'; nested interpolations are not allowed" lexbuf)
            else ();
            (match !paren_stack with
            | (CCODE, _) :: _ ->
                raise (lexErr "Unexpected string literal, after 'ccode' it should be code block in C" lexbuf)
            | _ -> ());
            string_start := lexbuf.lex_start_p;
            string_literal := "" ;
            strings lexbuf;
            !string_tokens
        }

    | "'" (['\032' - '\127'] as c) "'" { make_char_literal lexbuf (String.make 1 c) }
    | "'" (special_char as c) "'" { make_char_literal lexbuf (decode_special_char lexbuf c) }
    | "'" (hexcode as hc) "'" { make_char_literal lexbuf (decode_hex_char hc) }
    | "'" (octcode as oc) "'" { make_char_literal lexbuf (decode_oct_char oc) }
    | "'" ((['\192' - '\247'] ['\128' - '\191']+) as uc) "'" { make_char_literal lexbuf uc }

    | "\\" space* newline
        {   (* '\' just before the end of line means that we should ignore the subsequent line break *)
            incr_lineno lexbuf; tokens lexbuf
        }

    | '('
        {
            push_paren_stack LPAREN lexbuf;
            let t = if !new_exp then [B_LPAREN] else [LPAREN] in (new_exp := true; t)
        }
    | ')'
        {
            (match (!paren_stack) with
            | (LPAREN, _) :: rest ->
                paren_stack := rest; new_exp := false; [RPAREN]
            | _ -> raise (lexErr "Unexpected ')', check parens" lexbuf));
        }
    | '['
        {
            push_paren_stack LSQUARE lexbuf;
            let t = if !new_exp then [B_LSQUARE] else [LSQUARE] in (new_exp := true; t)
        }
    | ']'
        {
            (match (!paren_stack) with
            | (LSQUARE, _) :: rest -> paren_stack := rest
            | _ -> raise (lexErr "Unexpected ']', check parens" lexbuf));
            new_exp := false;
            [RSQUARE]
        }
    | "[:"
        {
            let tl = if !new_exp then [LLIST] else [LSQUARE; COLON] in
            push_paren_stack (List.hd tl) lexbuf;
            new_exp := true;
            tl
        }
    | ":]"
        {
            let tl =
                match (!paren_stack) with
                | (LLIST, _) :: rest -> paren_stack := rest; [RLIST]
                | (LSQUARE, _) :: rest -> paren_stack := rest; [COLON; RSQUARE]
                | _ -> raise (lexErr "Unexpected ':]', check parens" lexbuf)
                in
            new_exp := false;
            tl
        }
    | '{'
        {
            push_paren_stack LBRACE lexbuf;
            new_exp := true;
            match !paren_stack with
            | (LBRACE, _) :: (CCODE, _) :: _ ->
                ccode_literal := "";
                ccode_start := lexbuf.lex_curr_p;
                ccode_mode := true;
                ccode lexbuf
            | _ ->
                let ts = tokens lexbuf in
                LBRACE ::
                (* if '|' follows immediately after '{', we emit BAR token (instead of BITWISE_OR) and
                put it to the stack to mark that we are inside a pattern matching clause *)
                (match ts with
                | BITWISE_OR :: rest ->
                    paren_stack := (BAR, lexbuf.lex_curr_p) :: !paren_stack; BAR :: rest
                | _ -> ts)
        }
    | '}'
        {
            match (!paren_stack) with
            (* handle string interpolation e.g. "f({x})={f(x)}" *)
            | (STR_INTERP_LPAREN, _) :: rest ->
                paren_stack := rest; strings lexbuf; RPAREN :: PLUS :: (!string_tokens)
            | ((BAR, _) :: (LBRACE, _) :: rest) | ((LBRACE, _) :: rest) ->
                paren_stack := rest;
                new_exp := false;
                [RBRACE]
            | _ -> raise (lexErr "Unexpected '}', check parens" lexbuf)
        }

    | (((('0' ['x' 'X'] hexdigit+) | ('0' ['b' 'B'] ['0'-'1']+) | (['1'-'9'] digit*)) as num_) | ((['0'] octdigit*) as octnum_))
      (((['i' 'u' 'U' 'I'] digit+) | "L" | "UL") as suffix_)?
        {
            check_ne(lexbuf); new_exp := false;
            let suffix = match suffix_ with Some(x) -> x | _ -> "" in
            let v =
                try
                    match (num_, octnum_) with
                    | (Some(num), _) -> Scanf.sscanf num "%Li" (fun v -> v)
                    | (_, Some(octnum)) -> Scanf.sscanf octnum "%Lo" (fun v -> v)
                    | _ -> raise (lexErr "Invalid integer literal" lexbuf)
                with _ -> raise (lexErr "Invalid numeric literal" lexbuf) in
            [match suffix with
            | "i8" | "I8" -> SINT(8, v)
            | "u8" | "U8" -> UINT(8, v)
            | "i16" | "I16" -> SINT(16, v)
            | "u16" | "U16" -> UINT(16, v)
            | "i32" | "I32" -> SINT(32, v)
            | "u32" | "U32" -> UINT(32, v)
            | "i64" | "I64" | "L" -> SINT(64, v)
            | "u64" | "U64" | "UL" -> UINT(64, v)
            | "" -> INT(v)
            | _ -> raise (lexErr (sprintf "Invalid suffix '%s'" suffix) lexbuf)]
        }

    | ((digit+ '.' (digit* as frac) (['e' 'E'] ['+' '-']? digit+)?) as num) (['f' 'F']? as suffix)
        {
            check_ne(lexbuf); new_exp := false;
            let is_float = (String.contains num 'e') ||
                (String.contains num 'E') || suffix <> "" || frac = "" in
            if is_float then
                [FLOAT((if suffix = "" then 64 else 32), float_of_string (num))]
            else
                [FLOAT_LIKE(num)]
        }

    | ((digit+ ['e' 'E'] ['+' '-']? digit+) as num) (['f' 'F']? as suffix)
        { check_ne(lexbuf); new_exp := false; [FLOAT((if suffix = "" then 64 else 32), float_of_string (num))] }

    | ("\'" ? as prefix) ((['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*) as ident)
        {
            if prefix <> "" then (check_ne(lexbuf); new_exp := false; [TYVAR ("\'" ^ ident)]) else
            (try
                let (tok, toktype) as tokdata = Hashtbl.find keywords ident in
                match tokdata with
                | (CCODE, _) ->
                    check_ne(lexbuf);
                    push_paren_stack CCODE lexbuf;
                    new_exp := true; [CCODE]
                | (FOR, _) ->
                    let t = if !new_exp then B_FOR else FOR in
                    new_exp := true; [t]
                | (IMPORT, _) ->
                    let t = if !new_exp then B_IMPORT else IMPORT in
                    new_exp := true; [t]
                | (WHILE, _) ->
                    let t = if !new_exp then B_WHILE else WHILE in
                    new_exp := true; [t]
                | (REF, _) -> let t = if !new_exp then [REF] else [REF_TYPE] in t
                | (t, -1) ->
                    raise (lexErr (sprintf
                        "the identifier '%s' is reserved and cannot be used"
                        ident) lexbuf)
                | (t, 0) -> check_ne(lexbuf); new_exp := false; [t]
                | (t, 1) -> new_exp := true; [t]
                | (t, 2) -> check_ne(lexbuf); new_exp := true; [t]
                | _ -> raise (lexErr (sprintf "Unexpected keyword '%s'" ident) lexbuf)
            with Not_found ->
                (try
                    if (Hashtbl.find reserved_keywords ident) == 1 then
                        raise (lexErr (sprintf
                            "the identifier '%s' cannot be used; it's reserved by C++, C or by Ficus compiler"
                            ident) lexbuf)
                    else ()
                with Not_found -> ());
                let t = if !new_exp then [B_IDENT ident] else [IDENT ident] in (new_exp := false; t))
        }
    | '+'   { let t = if !new_exp then [B_PLUS] else [PLUS] in (new_exp := true; t) }
    | '-'   { let t = if !new_exp then [B_MINUS] else [MINUS] in (new_exp := true; t) }
    | '*'   { let t = if !new_exp then [B_STAR] else [STAR] in (new_exp := true; t) }
    | '/'   { new_exp := true; [SLASH] }
    | "%"   { new_exp := true; [MOD] }
    | "**"  { let t = if !new_exp then [B_POWER] else [POWER] in (new_exp := true; t) }
    | ">>"  { new_exp := true; [SHIFT_RIGHT] }
    | "<<"  { new_exp := true; [SHIFT_LEFT] }
    | '&'   { new_exp := true; [BITWISE_AND] }
    | '|'
        {
            new_exp := true;
            match !paren_stack with
            | (BAR, _) :: (LBRACE, _) :: _ -> [BAR]
            | _ -> [BITWISE_OR]
        }
    | '^'   { new_exp := true; [BITWISE_XOR] }
    | '~'   { check_ne(lexbuf); [BITWISE_NOT] }
    | "&&"  { new_exp := true; [LOGICAL_AND] }
    | "||"  { new_exp := true; [LOGICAL_OR] }
    | '!'   { check_ne(lexbuf); [LOGICAL_NOT] }
    | '='
        {
            new_exp := true;
            [EQUAL]
        }
    | "+="  { new_exp := true; [PLUS_EQUAL] }
    | "-="  { new_exp := true; [MINUS_EQUAL] }
    | "*="  { new_exp := true; [STAR_EQUAL] }
    | "/="  { new_exp := true; [SLASH_EQUAL] }
    | ".="  { new_exp := true; [DOT_EQUAL] }
    | "%="  { new_exp := true; [MOD_EQUAL] }
    | "&="  { new_exp := true; [AND_EQUAL] }
    | "|="  { new_exp := true; [OR_EQUAL] }
    | "^="  { new_exp := true; [XOR_EQUAL] }
    | "<<=" { new_exp := true; [SHIFT_LEFT_EQUAL] }
    | ">>=" { new_exp := true; [SHIFT_RIGHT_EQUAL] }

    | "<=>" { new_exp := true; [SPACESHIP] }
    | "=="  { new_exp := true; [CMP_EQ] }
    | "!="  { new_exp := true; [CMP_NE] }
    | "<="  { new_exp := true; [CMP_LE] }
    | ">="  { new_exp := true; [CMP_GE] }
    | '<'   { new_exp := true; [CMP_LT] }
    | '>'   { new_exp := true; [CMP_GT] }

    | ".-"  { check_ne(lexbuf); [B_DOT_MINUS] }
    | ".*"  { new_exp := true; [DOT_STAR] }
    | "./"  { new_exp := true; [DOT_SLASH] }
    | ".%"  { new_exp := true; [DOT_MOD] }
    | ".**" { new_exp := true; [DOT_POWER] }

    | ".<=>" { new_exp := true; [DOT_SPACESHIP] }
    | ".=="  { new_exp := true; [DOT_CMP_EQ] }
    | ".!="  { new_exp := true; [DOT_CMP_NE] }
    | ".<="  { new_exp := true; [DOT_CMP_LE] }
    | ".>="  { new_exp := true; [DOT_CMP_GE] }
    | ".<"   { new_exp := true; [DOT_CMP_LT] }
    | ".>"   { new_exp := true; [DOT_CMP_GT] }

    | ".*="  { new_exp := true; [DOT_STAR_EQUAL] }
    | "./="  { new_exp := true; [DOT_SLASH_EQUAL] }
    | ".%="  { new_exp := true; [DOT_MOD_EQUAL] }

    | ','   { new_exp := true; [COMMA] }
    | '.'   { new_exp := true; [DOT] }
    | ';'   { new_exp := true; [SEMICOLON] }
    | ':'   { new_exp := true; [COLON] }
    | "::"  { new_exp := true; [CONS] }
    | "\\"  { new_exp := true; [BACKSLASH] }
    | ":>"  { new_exp := true; [CAST] }
    | "<-"  { new_exp := true; [BACK_ARROW] }
    | "=>"  { new_exp := true; [DOUBLE_ARROW] }
    | "->"  { new_exp := true; [ARROW] }
    | "?"   { new_exp := false; [QUESTION] }
    | "@"   { new_exp := true; [AT] }
    | "..."   { new_exp := true; [ELLIPSIS] }

    | eof   { [EOF] }
    | _ as s { raise (lexErr (sprintf "Illegal character '%s'" (Char.escaped s)) lexbuf) }

and strings = parse
    | (([^ '\"' '\\' '{' '\n' '\r']+) as string_part)
        { string_literal := !string_literal ^ string_part; strings lexbuf }
    | ("\"" | "{\"" ) as s
        {
            let s = if s = "{\"" then "{" else "" in
            let string_lit = STRING (!string_literal ^ s) in
            if !string_interp_elem = 0 then
                string_tokens := [string_lit]
            else
                string_tokens := [string_lit; RPAREN];
            string_literal := "";
            string_interp_elem := 0;
            new_exp := false
            (* return to 'tokens' rule *)
        }
    | "{"
        {
            push_paren_stack STR_INTERP_LPAREN lexbuf;
            string_tokens := [(STRING !string_literal); PLUS; (B_IDENT "string"); LPAREN];
            if !string_interp_elem = 0 then
                string_tokens := B_LPAREN :: !string_tokens
            else ();
            string_literal := "";
            string_interp_elem := !string_interp_elem + 1;
            new_exp := true
            (* return to 'tokens' rule *)
        }

    (* we want the produced string literal to be the same, regardless of the actual EOL encoding,
       so we always add '\n', not the occured <newline> character(s) *)
    | newline { incr_lineno lexbuf; string_literal := !string_literal ^ "\n"; strings lexbuf }

    | special_char as c
        { string_literal := !string_literal ^ (decode_special_char lexbuf c); strings lexbuf }
    | hexcode as hc
        { string_literal := !string_literal ^ (decode_hex_char hc); strings lexbuf }
    | octcode as oc
        { string_literal := !string_literal ^ (decode_oct_char oc); strings lexbuf }
    | "\\" (_ as s)
        { raise (lexErrAt (sprintf "Illegal escape \\%s" (Char.escaped s)) (!string_start, lexbuf.lex_curr_p)) }
    | eof
        { raise (lexErrAt "Unterminated string" (!string_start, lexbuf.lex_curr_p)) }
    | _ as c
        { raise (lexErr (sprintf "Illegal character '%s' inside string literal" (Char.escaped c)) lexbuf) }

and comments = parse
    | "*/"
        {
            comments_level := !comments_level - 1;
            if !comments_level = 0 then
                (* returns to tokens or ccode or the outer comment *)
                ()
            else comments lexbuf
        }

    | "/*"
        {
            if !ccode_mode then () else comments_level := !comments_level + 1;
            comments lexbuf
        }
    | newline { incr_lineno lexbuf; comments lexbuf }
    | eof  { raise (lexErrAt "Unterminated comment" (!comment_start, lexbuf.lex_curr_p)) }
    | _  { comments lexbuf }

and eol_comments = parse
    | newline { incr_lineno lexbuf; new_exp := true; DOT }
    | eof  { EOF }
    | _  { eol_comments lexbuf }

and ccode = parse
    | (([^ '(' ')' '{' '}' '[' ']' '\"' '\'' '\n' '\r' '/']+) as ccode_part)
        { add_ccode ccode_part; ccode lexbuf }
    | (['{' '[' '(']) as c
        {
            let tk = if c = '{' then LBRACE else if c = '[' then LSQUARE else LPAREN in
            push_paren_stack tk lexbuf;
            add_ccode_char c;
            ccode lexbuf
        }
    | ([']' ')' '}']) as c
        {
            let expected_tk = if c = '}' then LBRACE else if c = ']' then LSQUARE else LPAREN in
            match (!paren_stack) with
            | (LBRACE, _) :: (CCODE, _) :: rest when expected_tk = LBRACE ->
                paren_stack := rest;
                ccode_mode := false;
                new_exp := false;
                (* return the fetched ccode string; the terminal '}' is not included *)
                [STRING(!ccode_literal)]
            | (tk, _) :: rest when tk = expected_tk ->
                paren_stack := rest;
                add_ccode_char c;
                ccode lexbuf
            | _ -> raise (lexErr (sprintf "Unexpected '%c', check parens" c) lexbuf)
        }
    | newline
        {
            incr_lineno lexbuf;
            add_ccode "\n";
            ccode lexbuf
        }
    | "/*"
        {
            comments_level := 1;
            comment_start := lexbuf.lex_start_p;
            comments lexbuf;
            ccode lexbuf
        }
    | "//"
        {
            if (eol_comments lexbuf) = EOF then
                raise (lexErrAt "Unterminated ccode block" (!ccode_start, lexbuf.lex_curr_p))
            else
                ccode lexbuf
        }

    | ['\'' '\"' ] as c
        {
            ccode_string_start := lexbuf.lex_start_p;
            ccode_string_quote := c;
            add_ccode_char c;
            ccode_strings lexbuf;
            ccode lexbuf
        }
    | eof
        { raise (lexErrAt "Unterminated ccode block" (!ccode_start, lexbuf.lex_curr_p)) }
    | _ as c
        {
            add_ccode_char c;
            ccode lexbuf
        }

and ccode_strings = parse
    | (([^ '\"' '\'' '\\' '\n' '\r']+) as ccode_part)
        { add_ccode ccode_part; ccode_strings lexbuf }
    | ['\"' '\''] as c
        {
            add_ccode_char c;
            if c = !ccode_string_quote then
                (* exit from the ccode_strings back to ccode *)
                ()
            else
                ccode_strings lexbuf
        }

    (* we want the produced string literal to be the same, regardless of the actual EOL encoding,
       so we always add '\n', not the occured <newline> character(s) *)
    | newline
        {
            incr_lineno lexbuf; add_ccode "\n"; ccode_strings lexbuf
        }

    | ("\\\'" | "\\\"") as ccode_part
        {
            add_ccode ccode_part;
            ccode_strings lexbuf
        }

    | eof
        { raise (lexErrAt "Unterminated string in ccode block" (!ccode_string_start, lexbuf.lex_curr_p)) }

    | _ as c
        {
            add_ccode_char c;
            ccode_strings lexbuf
        }
