{
open Parser
open Syntax
open Lexing

let string_literal = ref ([] : string list)

let keywords = Hashtbl.create 101
let _ = List.iter (fun(kwd, tok) -> Hashtbl.add keywords kwd tok)
    [
      ("and", AND); ("as", AS); ("begin", BEGIN); ("do", DO); ("done", DONE);
      ("downto", DOWNTO); ("else", ELSE); ("end", END); ("exception", EXCEPTION);
      ("false", FALSE); ("for", FOR); ("fun", FUN); ("function", FUNCTION);
      ("if", IF); ("in", IN); ("let", LET); ("lsl", LSL); ("lsr", LSR);
      ("match", MATCH); ("mod", MOD); ("mutable", MUTABLE); ("not", NOT);
      ("of", OF); ("open", OPEN); ("raise", RAISE); ("rec", REC); ("ref", REF);
      ("then", THEN); ("to", TO); ("true", TRUE); ("try", TRY); ("type", TYPE);
      ("val", VAL); ("when", WHEN); ("while", WHILE); ("with", WITH)
    ]

exception LexError of string * (position * position)
let get_token_pos lexbuf = (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let raise_lexer_err lexbuf msg =
    raise (LexError(msg, (get_token_pos lexbuf)))

let token2str t = match t with
    | AND -> "AND"
    | AS -> "AS"
    | BEGIN -> "BEGIN"
    | DO -> "DO"
    | DONE -> "DONE"
    | DOWNTO -> "DOWNTO"
    | ELSE -> "ELSE"
    | END -> "END"
    | EXCEPTION -> "EXCEPTION"
    | FALSE -> "FALSE"
    | FOR -> "FOR"
    | FUN -> "FUN"
    | FUNCTION -> "FUNCTION"
    | IF -> "IF"
    | IN -> "IN"
    | LET -> "LET"
    | LSL -> "LSL"
    | LSR -> "LSR"
    | MATCH -> "MATCH"
    | MOD -> "MOD"
    | MUTABLE -> "MUTABLE"
    | NOT -> "NOT"
    | OF -> "OF"
    | OPEN -> "OPEN"
    | RAISE -> "RAISE"
    | REC -> "REC"
    | REF -> "REF"
    | THEN -> "THEN"
    | TO -> "TO"
    | TRUE -> "TRUE"
    | TRY -> "TRY"
    | TYPE -> "TYPE"
    | VAL -> "VAL"
    | WHEN -> "WHEN"
    | WHILE -> "WHILE"
    | WITH -> "WITH"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | DOT_LPAREN -> "DOT_LPAREN"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | LSQUARE -> "LSQUARE"
    | RSQUARE -> "RSQUARE"
    | LSQUARE_VEC -> "LSQUARE_REC"
    | RSQUARE_VEC -> "RSQUARE_REC"
    | MINUS -> "MINUS"
    | PLUS -> "PLUS"
    | STAR -> "STAR"
    | SLASH -> "SLASH"
    | MINUS_DOT -> "MINUS_DOT"
    | PLUS_DOT -> "PLUS_DOT"
    | STAR_DOT -> "STAR_DOT"
    | SLASH_DOT -> "SLASH_DOT"
    | LOGICAL_AND -> "LOGICAL_AND"
    | LOGICAL_OR -> "LOGICAL_OR"
    | STRING_CONCAT -> "STRING_CONCAT"
    | LIST_CONCAT -> "LIST_CONCAT"
    | EQUAL -> "EQUAL"
    | NOT_EQUAL -> "NOT_EQUAL"
    | LESS_EQUAL -> "LESS_EQUAL"
    | GREATER_EQUAL -> "GREATER_EQUAL"
    | LESS -> "LESS"
    | GREATER -> "GREATER"
    | COMMA -> "COMMA"
    | CONS -> "CONS"
    | ASSIGN -> "ASSIGN"
    | EXCLAMATION -> "EXCLAMATION"
    | DOT -> "DOT"
    | BACK_ARROW -> "BACK_ARROW"
    | ARROW -> "ARROW"
    | COLON -> "COLON"
    | SEMICOLON -> "SEMICOLON"
    | BAR -> "BAR"
    | EOF -> "EOF"
    | CHAR(c) -> sprintf "CHAR('%c')" c
    | STRING(s) -> sprintf "STRING(\"%s\")" s
    | IDENT(s) -> sprintf "IDENT(%s)" s
    | INT(i) -> sprintf "INT(%d)" i
    | INT64(i) -> sprintf "INT64(%d)" i
    | FLOAT(f) -> sprintf "FLOAT(%g)" f
    | TYVAR(s) -> sprintf "TYVAR(%s)" s

}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| "\n" | "\r\n" { lineno := !lineno + 1; token lexbuf }
| [' ' '\t']+ { token lexbuf }
| "(*" { comment lexbuf; token lexbuf }
| '"' { string_literal := []; strings lexbuf }
| "'" ([^ '\n' '\r' '\t'] as c) "'" { CHAR(c) }
| "'\\" ([^ '\n' '\r' '\t'] as c) "'"
    {
        let c = match c with
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | '\'' -> '\''
            | '\"' -> '\"'
            | '\\' -> '\\'
            | '0' -> '\000'
            | _ -> raise_lexer_err lexbuf "invalid escape symbol"
            in
        CHAR(c)
    }
| "'\\" (['0' - '7']['0' - '7']['0' - '7'] as code) "'"
{
    let c0 = Char.code(String.get code 0) - Char.code('0') in
    let c1 = Char.code(String.get code 1) - Char.code('0') in
    let c2 = Char.code(String.get code 2) - Char.code('0') in
    CHAR(Char.chr(c0*64+c1*8+c2))
}
| '(' { LPAREN }
| ')' { RPAREN }
| ".(" { DOT_LPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LSQUARE }
| ']' { RSQUARE }
| "[|" { LSQUARE_VEC }
| "|]" { RSQUARE_VEC }
| (digit+ as num)("L" as suffix)?
    {
        let num = int_of_string num in
        match suffix with Some _ -> INT64(num) | _ -> INT(num)
    }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-' { MINUS }
| '+' { PLUS }
| "*" { STAR }
| "/" { SLASH }
| "-." { MINUS_DOT }
| "+." { PLUS_DOT }
| "*." { STAR_DOT }
| "/." { SLASH_DOT }
| "&&" { LOGICAL_AND }
| "||" { LOGICAL_OR }
| "^" { STRING_CONCAT }
| "@" { LIST_CONCAT }
| '=' { EQUAL }
| "==" { EQUAL }
| "<>" { NOT_EQUAL }
| "!=" { NOT_EQUAL }
| "<=" { LESS_EQUAL }
| ">=" { GREATER_EQUAL }
| '<' { LESS }
| '>' { GREATER }
| ',' { COMMA }
| "::" { CONS }
| ":=" { ASSIGN }
| "!" { EXCLAMATION }
| (lower|upper|"_") (lower|upper|digit|"_")*
{
    let k = Lexing.lexeme lexbuf in
    match Hashtbl.find_opt keywords k with
    | Some kw -> kw
    | _ -> IDENT(k)
}
| '\'' (lower|upper) (lower|upper|digit|"_")* { TYVAR(Lexing.lexeme lexbuf) }
| '.' { DOT }
| "<-" { BACK_ARROW }
| "->" { ARROW }
| ':' { COLON }
| ';' { SEMICOLON }
| '|' { BAR }
| eof { EOF }
| _ { raise_lexer_err lexbuf (sprintf "unknown token %s" (Lexing.lexeme lexbuf)) }

and comment = parse
| "\n" | "\r\n" { lineno := !lineno + 1; comment lexbuf }
| "*)"
    { () }
| eof
    { raise_lexer_err lexbuf "warning: unterminated comment@." }
| _
    { comment lexbuf }

and strings = parse
| "\\\"" { string_literal := "\\\"" :: !string_literal; strings lexbuf }
| "\\\\" { string_literal := "\\\\" :: !string_literal; strings lexbuf }
| "\\" { string_literal := "\\" :: !string_literal; strings lexbuf }
| "\"" { STRING(String.concat "" (List.rev !string_literal)) }
| eof
    { raise_lexer_err lexbuf "error: unterminated comment."; }
| _
    { string_literal := (Lexing.lexeme lexbuf) :: !string_literal; strings lexbuf }
