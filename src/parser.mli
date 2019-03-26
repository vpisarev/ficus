type token =
  | TRUE
  | FALSE
  | NIL
  | INT of (Int64.t)
  | SINT of (int * Int64.t)
  | UINT of (int * Int64.t)
  | FLOAT of (int * float)
  | IDENT of (string)
  | B_IDENT of (string)
  | STRING of (string)
  | CHAR of (string)
  | TYVAR of (string)
  | AS
  | CATCH
  | CCODE
  | ELSE
  | EXCEPTION
  | B_FOLD
  | FOLD
  | B_FOR
  | FOR
  | FROM
  | FUN
  | IF
  | B_IMPORT
  | IMPORT
  | IN
  | OPERATOR
  | B_REF
  | REF
  | THROW
  | TRY
  | TYPE
  | VAL
  | VAR
  | WHILE
  | B_LPAREN
  | LPAREN
  | RPAREN
  | B_LSQUARE
  | LSQUARE
  | RSQUARE
  | B_LBRACE
  | LBRACE
  | RBRACE
  | COMMA
  | DOT
  | SEMICOLON
  | COLON
  | BAR
  | CONS
  | CAST
  | DOUBLE_ARROW
  | ARROW
  | EOF
  | B_MINUS
  | MINUS
  | B_PLUS
  | PLUS
  | B_STAR
  | STAR
  | SLASH
  | MOD
  | B_POWER
  | POWER
  | SHIFT_RIGHT
  | SHIFT_LEFT
  | BITWISE_AND
  | BITWISE_XOR
  | BITWISE_NOT
  | LOGICAL_AND
  | LOGICAL_OR
  | LOGICAL_NOT
  | EQUAL
  | PLUS_EQUAL
  | MINUS_EQUAL
  | STAR_EQUAL
  | SLASH_EQUAL
  | MOD_EQUAL
  | AND_EQUAL
  | OR_EQUAL
  | XOR_EQUAL
  | SHIFT_LEFT_EQUAL
  | SHIFT_RIGHT_EQUAL
  | EQUAL_TO
  | NOT_EQUAL
  | LESS_EQUAL
  | GREATER_EQUAL
  | LESS
  | GREATER

val ficus_module :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.exp_t list * Syntax.id_t list
