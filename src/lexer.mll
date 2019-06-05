{
(* lexer *)
open Parser
open Lexing
open Printf

let token2str t = match t with
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | NIL -> "NIL"
    | NONE -> "NONE"
    | INT(i) -> sprintf "INT(%Ld)" i
    | SINT(b, i) -> sprintf "SINT(%d, %Ld)" b i
    | UINT(b, i) -> sprintf "UINT(%d, %Ld)" b i
    | FLOAT(b, f) -> sprintf "FLOAT(%d, %g)" b f
    | IDENT(s) -> sprintf "IDENT(%s)" s
    | B_IDENT(s) -> sprintf "B_IDENT(%s)" s
    | STRING(s) -> sprintf "STRING(%s)" s
    | CHAR(s) -> sprintf "CHAR(%s)" s
    | TYVAR(s) -> sprintf "TYVAR(%s)" s    
    | AS -> "AS"
    | CATCH -> "CATCH"
    | CCODE -> "CCODE"
    | CLASS -> "CLASS"
    | DO -> "DO"
    | DO_W -> "DO_W"
    | DONE -> "DONE"
    | ELIF -> "ELIF"
    | ELSE -> "ELSE"
    | END -> "END"
    | EXCEPTION -> "EXCEPTION"
    | EXTENDS -> "EXTENDS"
    | FI -> "FI"
    | FOR -> "FOR"
    | FROM -> "FROM"
    | FUN -> "FUN"
    | IF -> "IF"
    | IMPLEMENTS -> "IMPLEMENTS"
    | IMPORT -> "IMPORT"
    | IMPORT_NAMES -> "IMPORT_NAMES"
    | IN -> "IN"
    | INLINE -> "INLINE"
    | INTERFACE -> "INTERFACE"
    | IS -> "IS"
    | MATCH -> "MATCH"
    | NOTHROW -> "NOTHROW"
    | OPERATOR -> "OPERATOR"
    | PARALLEL -> "PASS"
    | PASS -> "PASS"
    | PURE -> "PURE"
    | REF -> "MAKE_REF"
    | REF_TYPE -> "REF_TYPE"
    | STATIC -> "STATIC"
    | THEN -> "THEN"
    | THROW -> "THROW"
    | TRY -> "TRY"
    | TYPE -> "TYPE"
    | UPDATE -> "UPDATE"
    | VAL -> "VAL"
    | VAR -> "VAR"
    | WHILE -> "WHILE"
    | WITH -> "WITH"
    | B_LPAREN -> "B_LPAREN"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | B_LSQUARE -> "B_LSQUARE"
    | LSQUARE -> "LSQUARE"
    | RSQUARE -> "RSQUARE"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | COMMA -> "COMMA"
    | DOT -> "DOT"
    | SEMICOLON -> "SEMICOLON"
    | COLON -> "COLON"
    | BAR -> "BAR"
    | CONS -> "CONS"
    | CAST -> "CAST"
    | DOUBLE_ARROW -> "DOUBLE_ARROW"
    | ARROW -> "ARROW"
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
    | MOD_EQUAL -> "MOD_EQUAL"
    | AND_EQUAL -> "AND_EQUAL"
    | OR_EQUAL -> "OR_EQUAL"
    | XOR_EQUAL -> "XOR_EQUAL"
    | SHIFT_LEFT_EQUAL -> "SHIFT_LEFT_EQUAL"
    | SHIFT_RIGHT_EQUAL -> "SHIFT_RIGHT_EQUAL"
    | EQUAL_TO -> "EQUAL_TO"
    | NOT_EQUAL -> "NOT_EQUAL"
    | LESS_EQUAL -> "LESS_EQUAL"
    | GREATER_EQUAL -> "GREATER_EQUAL"
    | LESS -> "LESS"
    | GREATER -> "GREATER"

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
   0 - a keyword that is a single-word expression
   1 - a keyword that starts a new expression; it cannot follow another expression
       without some explicit operator or a separator
   2 - a keyword that cannot start a new expression, but
       it links previous part of expression with the subsequent one;
       so it can immediately follow expression (be placed on the same line),
       e.g. "else" in if-then expression
   3 - a keyword that ends a compound expression
   4 - a keyword that can start an expression (type 2) or be a connector (type 1),
       depending on the context
*)
let _ = List.iter (fun(kwd, tok, kwtyp) -> Hashtbl.add keywords kwd (tok, kwtyp))
[
    ("and", AND, 2); ("as", AS, 2); ("catch", CATCH, 2); ("ccode", CCODE, 1); ("class", CLASS, 1);
    ("do", DO, 4); ("done", DONE, 3); ("elif", ELIF, 2); ("else", ELSE, 2);
    ("end", END, 3); ("exception", EXCEPTION, 1); ("extends", EXTENDS, 2);
    ("false", FALSE, 0); ("fi", FI, 3); ("for", FOR, 4); ("from", FROM, 1); ("fun", FUN, 1);
    ("if", IF, 1); ("implements", IMPLEMENTS, 2); ("import", IMPORT, 4);
    ("in", IN, 2); ("inline", INLINE, 1); ("interface", INTERFACE, 1); ("is", IS, 1); 
    ("match", MATCH, 1); ("nil", NIL, 0); ("None", NONE, 0); ("nothrow", NOTHROW, 1);
    ("operator", OPERATOR, 1); ("parallel", PARALLEL, 1); ("pass", PASS, 0); ("pure", PURE, 1);
    ("ref", REF, 4); ("static", STATIC, 1); ("then", THEN, 2); ("throw", THROW, 1);
    ("true", TRUE, 0); ("try", TRY, 1); ("type", TYPE, 1); ("update", UPDATE, 2);
    ("val", VAL, 1); ("var", VAR, 1); ("when", WHEN, 2); ("while", WHILE, 1); ("with", WITH, 2)
];;

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
        sprintf "%s:%d:%d" pos_fname pos_lnum (pos_cnum - pos_bol + 1)
    else
        sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let lexErrAt msg pos = LexError (msg, pos)

let lexErr msg lexbuf =
    LexError(msg, (get_token_pos lexbuf))

let string_literal = ref ""
let string_start = ref dummy_pos
let comment_start = ref dummy_pos
let new_exp = ref true

(* the stack of tokens. Can contain the following characters:
   LPAREN, LSQUARE, LBRACE - corresponds to various parentheses;
                    wait for the corresponding closing bracket.
   IF, ELIF - wait for THEN
   THEN - wait for ELIF, ELSE or FI
   DO_W - wait for WHILE
   DO - wait for DONE
   CLASS, INTERFACE - wait for END
   IS - wait for END
   FROM - wait for IMPORT
   FUN - waiting for IS or '=' or '|' or '=>'.
       when IS is met, remove FUN from stack, put IS to stack
       when '=' or '=>' is met, remove FUN from stack
       when '|' is met, remove FUN from stack, put WITH to stack
   MATCH - wait for WITH
       when WITH is met, remove MATCH, put WITH to stack.
   UPDATE - wait for WITH
       when WITH is met, remove UPDATE from stack.
   TRY - wait for CATCH
   CATCH - wait for END
   WITH - wait for END
   when IMPORT is met, remove FROM from the stack (if)
   when END is met, remove the corresponding IS, WITH, CLASS or INTERFACE from stack.
   when '|' is met and WITH is on the top, return BAR token.
      otherwise return BITWISE_OR token.
   when '=>' is met:
      if 'FOR' :: '[' :: _ is on top, we put '=>' to stack; that means array comprehension.
      if 'FUN' :: '(' :: _ are on top, remove 'FUN', put '=>' to stack; that means lambda function
      otherwise do nothing
   when '=' is met:
      if 'FUN' is on top, remove 'FUN'
      otherwise do nothing
   when '|' is met:
      if 'FUN' is on top, remove 'FUN' and put 'WITH'
   when '\n', '\r\n' or '\r' is met and if we are inside (), [] or {}, newline is ignored (yet we increment lineno).
   when ']' is met, remove => (if any) and the matching '['.
   when ')' is met, remove => (if any) and the matching '('.
   when WITH is met:
      if MATCH is on top of the stack, remove MATCH, put WITH
      if UPDATE is on top of the stack, remove UPDATE
      if '{' on top of the stack, do nothing
      otherwise report an error.
*)
let paren_stack = ref []

let unmatchedTokenMsg t0 expected_list =
    let expected_str = List.fold_left (fun str t -> let t_str = token2str_pp t in
                if str = "" then t_str else str ^ "/" ^ t_str) expected_list in
    let found_in_stack = expected_str = "" or
        List.exist (fun (t, _) -> List.exist (fun t1 -> t = t1) expected_list) !paren_stack in
    if not found_in_stack then
        sprintf "'%s' without preceding %s." (token2str_pp t0) expected_str
    else
    match !paren_stack with
    | (_, p) :: _ ->
        let (kw, expected) = 
            (match !paren_stack with
            | (LPAREN, _) :: _ -> ("Unmatched '('", "")
            | (LSQUARE, _) :: _ -> ("Unmatched '['", "")
            | (LBRACE, _) :: _ -> ("Unmatched '{'", "")
            | (IF, _) :: _ -> ("Unmatched 'if'", "'then'")
            | (ELIF, _) :: _ -> ("Unmatched 'elif'", "'then'")
            | (ELSE, _) :: _ -> ("Unmatched 'else'", "'fi'")
            | (THEN, _) :: _ -> ("Unfinished 'then' clause", "'elif'/'else'/'fi'")
            | (DO, _) :: _ -> ("Unfinished loop body", "'done'")
            | (DO_W, _) :: _ -> ("Unfinished do-while body", "'while'")
            | (CLASS, _) :: _ -> ("Unfinished class", "'end'")
            | (INTERFACE, _) :: _ -> ("Unfinished interface", "'end'")
            | (IS, _) :: _ -> ("Unfinished 'is' clause", "'end'")
            | (FROM, _) :: _ -> ("Unfinished 'from'", "'import'")
            | (FUN, _) :: _ -> ("Unfinished 'fun'", "function body")
            | (FOR, _) :: _ -> ("Unfinished 'for'", "'do'")
            | (WHILE, _) :: _ -> ("Unfinished 'while'", "'do'")
            | (MATCH, _) :: _ -> ("Unfinished 'match'", "'with'")
            | (UPDATE, _) :: _ -> ("Unfinished 'for-update'", "'with'")
            | (WITH, _) :: _ -> ("Unfinished pattern matching clause", "'end'")
            | (TRY, _) :: _ -> ("Unfinished 'try'", "catch")
            | (CATCH, _) :: _ -> ("Unfinished exception handling clause", "'end'")
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
}

let newline = '\n' | '\r' | "\r\n"
let space = [' ' '\t' '\012']
let digit = [ '0'-'9' ]
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule tokens = parse
    | newline
      {
          (* inside (), [] or {} parentheses newline characters do not start a new statement/expression
             (because the current one is not finished until we close all the opened parentheses) *)
          (match !paren_stack with
          | (LPAREN, _) :: _ | (LSQUARE, _) :: _ | (LBRACE, _) :: _ -> ()
          | _ -> new_exp := true);
          incr_lineno lexbuf; tokens lexbuf
      }
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
      {   (* '\' just before the end of line means that we should ignore the subsequent line break *)
          incr_lineno lexbuf; tokens lexbuf }

    | '('
      {
          paren_stack := (LPAREN, lexbuf.lex_start_p) :: !paren_stack;
          let t = if !new_exp then [B_LPAREN] else [LPAREN] in (new_exp := true; t)
      }
    | ')'
      {
          (match (!paren_stack) with
          | (LPAREN, _) :: rest -> paren_stack := rest
          | (DOUBLE_ARROW, _) :: (LPAREN, _) :: rest -> paren_stack := rest (* handle lambda functions: (fun () => ...) *)
          | _ -> raise (lexErr "Unexpected ')'" lexbuf));
          new_exp := false;
          [RPAREN]
      }
    | '['
      {
          paren_stack := (LSQUARE, lexbuf.lex_start_p) :: !paren_stack;
          let t = if !new_exp then [B_LSQUARE] else [LSQUARE] in (new_exp := true; t)
      }
    | ']'
      {
          (match (!paren_stack) with
          | (LSQUARE, _) :: rest -> paren_stack := rest
          | (DOUBLE_ARROW, _) :: (LSQUARE, _) :: rest -> paren_stack := rest (* handle array comprehensions: [for ... => ...] *)
          | _ -> raise (lexErr "Unexpected ']'" lexbuf));
          new_exp := false;
          [RSQUARE]
      }
    | '{'  { paren_stack := (LBRACE, lexbuf.lex_start_p) :: !paren_stack; new_exp := true; [LBRACE] }
    | '}'
      {
          (match (!paren_stack) with
          | (LBRACE, _) :: rest -> paren_stack := rest
          | _ -> raise (lexErr "Unexpected '}'" lexbuf));
          new_exp := false;
          [BRACE]
      }

    | ((('0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+) | (digit+)) as num) (((['i' 'u' 'U' 'I'] digit+) | "L" | "UL") as suffix_)?
      {
          check_ne(lexbuf); new_exp := false;
          let suffix = match suffix_ with Some(x) -> x | _ -> "" in
          let v =
               try Scanf.sscanf num "%Lu" (fun v -> v)
               with _ -> raise (lexErr (sprintf "Invalid integer literal '%s'" num) lexbuf) in
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
    | ((digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?) as num) (['f' 'F']? as suffix)
        { check_ne(lexbuf); new_exp := false; [FLOAT((if suffix = "" then 64 else 32), float_of_string (num))] }

    | ("\'" ? as prefix) ((['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*) as ident)
      {
          let (p0, p1) = get_token_pos lexbuf in
          if prefix <> "" then (check_ne(lexbuf); [TYVAR ("\'" ^ ident)]) else
          (try
              let tok = Hashtbl.find keywords ident in
              match tok with
              | (TRY, _) | (IF, _) | (FUN, _) | (CLASS, _) | (INTERFACE, _) | (MATCH, _) | (FROM, _) ->
                  check_ne lexbuf;
                  paren_stack := (tok, p0) :: !paren_stack; new_exp := true; [tok]
              | (IS, _) ->
                  (match !paren_stack with
                  | (FUN, _) :: rest -> paren_stack := (IS, p0) :: rest
                  | rest -> paren_stack := (IS, p0) :: rest);
                  new_exp := true; [tok]
              | (CATCH, _) ->
                  (match !paren_stack with
                  | (TRY, _) :: rest -> paren_stack := (CATCH, p0) :: rest
                  | _ -> raise (lexErr (unmatchedTokenMsg tok [TRY]) lexbuf));
                  new_exp := true; [tok]
              | (ELIF, _) | (ELSE, _) ->
                  (match !paren_stack with
                  | (THEN, _) :: rest -> paren_stack := (tok, p0) :: rest
                  | _ -> raise (lexErr (unmatchedTokenMsg tok [THEN]) lexbuf));
                  new_exp := true; [tok]
              | (THEN, _) ->
                  (match !paren_stack with
                  | (IF, _) :: rest | (ELIF, _) :: rest -> paren_stack := (THEN, p0) :: rest
                  | _ -> raise (lexErr (unmatchedTokenMsg tok [IF;ELIF]) lexbuf));
                  new_exp := true; [tok]
              | (FI, _) ->
                  (match !paren_stack with
                  | (THEN, _) :: rest | (ELSE, _) :: rest -> paren_stack := rest
                  | _ -> raise (lexErr (unmatchedTokenMsg tok [THEN;ELSE]) lexbuf));
                  new_exp := false; [tok]
              | (FOR, _) ->
                  (match !paren_stack with
                  | (FOR, _) :: rest ->
                      paren_stack := (tok, p0) :: rest (* replace 'for' in the stack with the nested for
                                                          in order to provide better error diagnostics *)
                  | rest ->
                      check_ne lexbuf; (* if the 'for' is not nested, it should start a new expression *)
                      paren_stack := (tok, p0) :: rest);
                  new_exp := true; [tok]
              | (WHILE, _) ->
                  (match !paren_stack with
                  | (DO_W, _) :: rest ->
                      paren_stack := rest; (* end of the do-while loop *)
                  | rest ->
                      check_ne lexbuf; (* if the 'while' does not finish do-while loop, it should start a new expression *)
                      paren_stack := (tok, p0) :: rest);
                  new_exp := true; [tok]
              | (DO, _) ->
                  (match !paren_stack with
                  | (WHILE, _) :: rest | (FOR, _) :: rest ->
                      paren_stack := (tok, p0) :: rest;
                      new_exp := true; [DO]
                  | rest ->
                      check_ne lexbuf; (* start of do-while loop *)
                      paren_stack := (DO_W, p0) :: rest;
                      new_exp := true; [DO_W])
              | (UPDATE, _) ->
                  (match !paren_stack with
                  | (FOR, _) :: rest ->
                      paren_stack := (tok, p0) :: rest
                  | _ ->
                      raise (lexErr (unmatchedTokenMsg tok [FOR]) lexbuf));
                  new_exp := true; [tok]
              | (WITH, _) ->
                  (match !paren_stack with
                  | (MATCH, _) :: rest ->
                      paren_stack := (tok, p0) :: rest
                  | (UPDATE, _) :: rest ->
                      paren_stack := (tok, p0) :: !paren_stack
                  | (LBRACE, _) :: rest -> ()
                  | _ -> raise (lexErr (unmatchedTokenMsg tok [MATCH; UPDATE; LBRACE]) lexbuf));
                  new_exp := true; [tok]
              | (DONE, _) ->
                  (match !paren_stack with
                  | (DO, _) :: rest ->
                      paren_stack := rest
                  | (WITH, _) :: (UPDATE, _) :: rest ->
                      paren_stack := rest
                  | _ -> raise (lexErr (unmatchedTokenMsg tok [DO; WITH]) lexbuf));
                  new_exp := false; [tok]
              | (END, _) ->
                  (match !paren_stack with
                  | (IS, _) :: rest | (WITH, _) :: rest | (CLASS, _) :: rest | (INTERFACE, _) :: rest ->
                      paren_stack := rest
                  | _ -> raise (lexErr (unmatchedTokenMsg tok [IS; WITH; CLASS; INTERFACE]) lexbuf));
                  new_exp := false; [tok]
              | (IMPORT, _) ->
                  (match !paren_stack with
                  | (FROM, _) :: rest ->
                      paren_stack := rest
                  | _ ->
                      check_ne lexbuf);
                  new_exp := true; [tok]
              | (REF, _) -> let t = if !new_exp then [REF] else [REF_TYPE] in t
              | (t, 0) -> check_ne(lexbuf); new_exp := false; [t]
              | (t, 1) -> check_ne(lexbuf); new_exp := true; [t]
              | (t, 2) -> new_exp := true; [t]
              | _ -> raise (lexErr (sprintf "Unexpected keyword '%s'" ident) lexbuf)
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
    | '|'
        {
            new_exp := true;
            match !paren_stack with
            | (WITH, _) :: (UPDATE, _) :: _ -> [BITWISE_OR]
            | (WITH, _) :: _ -> [BAR]
            | (FUN, _) :: rest -> paren_stack := (WITH, lexbuf.lex_start_p) :: rest; [BAR]
            | _ -> [BITWISE_OR]
        }
    | '^'   { new_exp := true; [BITWISE_XOR] }
    | '~'   { new_exp := true; [BITWISE_NOT] }
    | "&&"  { new_exp := true; [LOGICAL_AND] }
    | "||"  { new_exp := true; [LOGICAL_OR] }
    | '!'   { new_exp := true; [LOGICAL_NOT] }
    | '='
        {
            new_exp := true;
            (match !paren_stack with
            | (FUN, _) :: rest -> paren_stack := rest
            | _ -> ());
            [EQUAL]
        }
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
    | "=>"
        {
            new_exp := true;
            let (p0, _) = get_token_pos lexbuf in
            (match !paren_stack with
            | (FOR, _) :: rest -> paren_stack := (DOUBLE_ARROW, p0) :: rest
            | (FUN, _) :: rest -> paren_stack := (DOUBLE_ARROW, p0) :: rest
            | _ -> ());
            [DOUBLE_ARROW]
        }
    | "->"  { new_exp := true; [ARROW] }
    | eof   { [EOF] }
    | _ as s { raise (lexErr (sprintf "Illegal character '%s'" (Char.escaped s)) lexbuf) }

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
    | '\\' (_ as s)
      { raise (lexErrAt (sprintf "Illegal escape \\%s" (Char.escaped s)) (!string_start, lexbuf.lex_curr_p)) }
    | eof
      { raise (lexErrAt "Unterminated string" (!string_start, lexbuf.lex_curr_p)) }
    | _ as s
      { raise (lexErr (sprintf "Illegal character '%s' inside string literal" (Char.escaped s)) lexbuf) }

and comments level = parse
 | "*/" { if level = 0 then tokens lexbuf else comments (level-1) lexbuf }
 | "/*" { comments (level+1) lexbuf }
 | newline { incr_lineno lexbuf; comments level lexbuf }
 | eof  { raise (lexErrAt "Unterminated comment" (!comment_start, lexbuf.lex_curr_p)) }
 | _  { comments level lexbuf }

and eol_comments = parse
 | newline { incr_lineno lexbuf; new_exp := true; tokens lexbuf }
 | eof  { [EOF] }
 | _  { eol_comments lexbuf }
