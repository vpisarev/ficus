{
(* lexer *)
open Parser
open Lexing

let keywords = Hashtbl.create 101;;
let fname = ref "unknown";;

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
   4 - a keyword 'kwd' that start a construction: 'kwd' (expr) ...
        Such keywords must be handled in a special way.
        Normally, after ')' there can be no new expressions/statements without a separator,
        but not in the case of whose constructions: 'kwd' (...) new_expression.
        So, as soon as we encountered such special keyword, we start counting parentheses and
        set new_exp flag as soon as we closed all the parentheses. See paren_stack use.
*)
let _ = List.iter (fun(kwd, tok, kwtyp) -> Hashtbl.add keywords kwd (tok, kwtyp))
    [
        ("as", AS, 1); ("catch", CATCH, 1); ("ccode", CCODE, 2); ("else", ELSE, 1);
        ("exception", EXCEPTION, 2); ("false", FALSE, 0); ("fold", FOLD, 4);
        ("for", FOR, 4); ("from", FROM, 2); ("fun", FUN, 2); ("if", IF, 4);
        ("import", IMPORT, 3); ("in", IN, 1); ("operator", OPERATOR, 2); ("ref", REF, 3);
        ("throw", THROW, 2); ("true", TRUE, 0); ("try", TRY, 2); ("type", TYPE, 2);
        ("val", VAL, 2); ("var", VAR, 2); ("while", WHILE, 4);
    ]
;;

let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = !fname;
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
;;

type error =
    | IllegalCharacter of char
    | IllegalEscape of string
    | InvalidIntegerLiteral
    | MissingSeparator
    | UnexpectedKeyword of string
    | UnterminatedString
    | UnterminatedComment
;;

exception Error of error * position * position;;

let pos2str pos =
    let { pos_fname; pos_lnum; pos_bol; pos_cnum } = pos in
    Printf.sprintf "%s:%d:%d" pos_fname pos_lnum (pos_cnum - pos_bol + 1)

let error2str = function
    | IllegalCharacter c ->
      Printf.sprintf "Illegal character (%C)" c
    | IllegalEscape s ->
      Printf.sprintf "Illegal escape (%S)" s
    | InvalidIntegerLiteral ->
      Printf.sprintf "Invalid integer literal"
    | MissingSeparator ->
      Printf.sprintf "Missing separator"
    | UnexpectedKeyword s ->
      Printf.sprintf "Unexpected keyword (%S)" s
    | UnterminatedString ->
      Printf.sprintf "Unterminated string"
    | UnterminatedComment ->
      Printf.sprintf "Unterminated comment"
;;

let string_literal = ref "";;
let string_start = ref dummy_pos;;
let comment_start = ref dummy_pos;;
let new_exp = ref true;;
let paren_stack = ref [];;

let check_ne lexbuf =
    if !new_exp then
        ()
    else
        raise (Error (MissingSeparator, lexbuf.lex_start_p, lexbuf.lex_curr_p))

}

let newline = '\n' | '\r' | "\r\n"
let space = [' ' '\t' '\012']
let digit = [ '0'-'9' ]
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule tokens = parse
    | newline   { incr_lineno lexbuf; new_exp := true; tokens lexbuf }
    | space +  { tokens lexbuf }

    | "/*/" { tokens lexbuf }
    | "/*"  { comment_start := lexbuf.lex_start_p; comments 0 lexbuf }
    | "//"  { eol_comments lexbuf }

    | "\""
      {
          check_ne(lexbuf);
          new_exp := false;
          string_start := lexbuf.lex_start_p;
          string_literal := "" ; strings lexbuf;
          [STRING !string_literal]
      }

    | "\\" space * newline
      { incr_lineno lexbuf; tokens lexbuf }

    | '('
      {
          (match (!paren_stack) with
          | x :: rest -> paren_stack := (x+1) :: rest
          | _ -> ());
          let t = if !new_exp then [B_LPAREN] else [LPAREN] in (new_exp := true; t)
      }
    | ')'
      {
          (match (!paren_stack) with
          | x :: rest -> paren_stack := (if x > 1 then (x-1) :: rest else rest); new_exp := x <= 1
          | _ -> new_exp := false);
          [RPAREN]
      }
    | '['   { let t = if !new_exp then [B_LSQUARE] else [LSQUARE] in (new_exp := true; t) }
    | ']'   { new_exp := false; [RSQUARE] }
    | '{'   { new_exp := true; [LBRACE] }
    | '}'   { new_exp := false; [RBRACE] }

    | ((('0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+) | (digit+)) as num) (((['i' 'u' 'U' 'I'] digit+) | "L" | "UL") as suffix_)?
      {
          check_ne(lexbuf); new_exp := false;
          let suffix = match suffix_ with Some(x) -> x | _ -> "" in
          let v =
               try Scanf.sscanf num "%Lu" (fun v -> v)
               with _ -> raise (Error (InvalidIntegerLiteral, lexbuf.lex_start_p, lexbuf.lex_curr_p)) in
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
          | _ -> raise (Error (InvalidIntegerLiteral, lexbuf.lex_start_p, lexbuf.lex_curr_p))]
      }
    | ((digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?) as num) (['f' 'F']? as suffix)
        { check_ne(lexbuf); new_exp := false; [FLOAT((if suffix = "" then 64 else 32), float_of_string (num))] }

    | ("\'" ? as prefix) ((['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*) as ident)
      {
          if prefix <> "" then (check_ne(lexbuf); [TYVAR ("\'" ^ ident)]) else
          (try
              match Hashtbl.find keywords ident with
              | (t, 0) -> check_ne(lexbuf); new_exp := false; [t]
              | (t, 1) -> new_exp := true; [t]
              | (t, 2) -> check_ne(lexbuf); new_exp := true; [t]
              | (t, 4) -> check_ne(lexbuf); paren_stack := 0 :: !paren_stack; new_exp := true; [t]
              | (IMPORT, _) -> let t = if !new_exp then [B_IMPORT] else [IMPORT] in (new_exp := true; t)
              | (REF, _) -> let t = if !new_exp then [B_REF] else [REF] in t
              | _ -> raise (Error ((UnexpectedKeyword ident),
                             lexbuf.lex_start_p, lexbuf.lex_curr_p))
           with Not_found ->
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
    | '|'   { new_exp := true; [BAR] }
    | '^'   { new_exp := true; [BITWISE_XOR] }
    | '~'   { new_exp := true; [BITWISE_NOT] }
    | "&&"  { new_exp := true; [LOGICAL_AND] }
    | "||"  { new_exp := true; [LOGICAL_OR] }
    | '!'   { new_exp := true; [LOGICAL_NOT] }
    | '='   { new_exp := true; [EQUAL] }
    | "+="  { new_exp := true; [PLUS_EQUAL] }
    | "-="  { new_exp := true; [MINUS_EQUAL] }
    | "*="  { new_exp := true; [STAR_EQUAL] }
    | "/="  { new_exp := true; [SLASH_EQUAL] }
    | "%="  { new_exp := true; [MOD_EQUAL] }
    | "&="  { new_exp := true; [AND_EQUAL] }
    | "|="  { new_exp := true; [OR_EQUAL] }
    | "^="  { new_exp := true; [XOR_EQUAL] }
    | "<<=" { new_exp := true; [SHIFT_LEFT_EQUAL] }
    | ">>=" { new_exp := true; [SHIFT_RIGHT_EQUAL] }

    | "=="  { new_exp := true; [EQUAL_TO] }
    | "!="  { new_exp := true; [NOT_EQUAL] }
    | "<="  { new_exp := true; [LESS_EQUAL] }
    | ">="  { new_exp := true; [GREATER_EQUAL] }
    | '<'   { new_exp := true; [LESS] }
    | '>'   { new_exp := true; [GREATER] }
    | ','   { new_exp := true; [COMMA] }
    | '.'   { new_exp := true; [DOT] }
    | ';'   { new_exp := true; [SEMICOLON] }
    | ':'   { new_exp := true; [COLON] }
    | "::"  { new_exp := true; [CONS] }
    | ":>"  { new_exp := true; [CAST] }
    | "=>"  { new_exp := true; [DOUBLE_ARROW] }
    | "->"  { new_exp := true; [ARROW] }
    | eof   { [EOF] }
    | _ { raise (Error (IllegalCharacter (lexeme_char lexbuf 0),
            lexbuf.lex_start_p, lexbuf.lex_curr_p)) }

and strings = parse
    | (([^ '\"' '\\' '\n' '\r']+) as string_part)
        { string_literal := !string_literal ^ string_part; strings lexbuf }
    | '\"' { () }

    (* we want the produced string literal to be the same, regardless of the actual EOL encoding,
       so we always add '\n', not the occured <newline> character(s) *)
    | newline { incr_lineno lexbuf; string_literal := !string_literal ^ "\n"; strings lexbuf }

    | "\\'" { string_literal := !string_literal ^ "\'"; strings lexbuf }
    | "\\\"" { string_literal := !string_literal ^ "\""; strings lexbuf }
    | "\\n" { string_literal := !string_literal ^ "\n"; strings lexbuf }
    | "\\t" { string_literal := !string_literal ^ "\t"; strings lexbuf }
    | "\\r" { string_literal := !string_literal ^ "\r"; strings lexbuf }
    | "\\b" { string_literal := !string_literal ^ "\b"; strings lexbuf }
    | "\\0" { string_literal := !string_literal ^ "\x00"; strings lexbuf }
    | "\\" (('x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']) as hexcode)
      { string_literal := !string_literal ^ (String.make 1 (Char.chr (int_of_string ("0" ^ hexcode)))); strings lexbuf }
    | '\\' _
      { raise
          (Error
             (IllegalEscape (lexeme lexbuf),
             !string_start,
             lexbuf.lex_curr_p))
      }
    | eof
      { raise (Error (UnterminatedString, !string_start, lexbuf.lex_curr_p)) }
    | _
      { raise (Error (IllegalCharacter (lexeme_char lexbuf 0),
               lexbuf.lex_start_p, lexbuf.lex_curr_p)) }

and comments level = parse
 | "*/" { if level = 0 then tokens lexbuf else comments (level-1) lexbuf }
 | "/*" { comments (level+1) lexbuf }
 | newline { incr_lineno lexbuf; comments level lexbuf }
 | eof  { raise (Error (UnterminatedComment, !comment_start, lexbuf.lex_curr_p)) }
 | _  { comments level lexbuf }

and eol_comments = parse
 | newline { incr_lineno lexbuf; new_exp := true; tokens lexbuf }
 | eof  { [EOF] }
 | _  { eol_comments lexbuf }
