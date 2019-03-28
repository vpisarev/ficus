open Syntax
exception LexerEOF

let token2str t = match t with
  | Parser.TRUE -> "TRUE"
  | Parser.FALSE -> "FALSE"
  | Parser.NIL -> "NIL"
  | Parser.INT(i) -> Printf.sprintf "INT(%Ld)" i
  | Parser.SINT(b, i) -> Printf.sprintf "SINT(%d, %Ld)" b i
  | Parser.UINT(b, i) -> Printf.sprintf "UINT(%d, %Ld)" b i
  | Parser.FLOAT(b, f) -> Printf.sprintf "FLOAT(%d, %g)" b f
  | Parser.IDENT(s) -> Printf.sprintf "IDENT(%s)" s
  | Parser.B_IDENT(s) -> Printf.sprintf "B_IDENT(%s)" s
  | Parser.STRING(s) -> Printf.sprintf "STRING(%s)" s
  | Parser.CHAR(s) -> Printf.sprintf "CHAR(%s)" s
  | Parser.TYVAR(s) -> Printf.sprintf "TYVAR(%s)" s
  | Parser.AS -> "AS"
  | Parser.CATCH -> "CATCH"
  | Parser.CCODE -> "CCODE"
  | Parser.ELSE -> "ELSE"
  | Parser.EXCEPTION -> "EXCEPTION"
  | Parser.FOR -> "FOR"
  | Parser.FOLD -> "FOLD"
  | Parser.FROM -> "FROM"
  | Parser.FUN -> "FUN"
  | Parser.IF -> "IF"
  | Parser.B_IMPORT -> "B_IMPORT"
  | Parser.IMPORT -> "IMPORT"
  | Parser.IN -> "IN"
  | Parser.OPERATOR -> "OPERATOR"
  | Parser.B_REF -> "B_REF"
  | Parser.REF -> "REF"
  | Parser.THROW -> "THROW"
  | Parser.TRY -> "TRY"
  | Parser.TYPE -> "TYPE"
  | Parser.VAL -> "VAL"
  | Parser.VAR -> "VAR"
  | Parser.WHILE -> "WHILE"
  | Parser.B_LPAREN -> "B_LPAREN"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.B_LSQUARE -> "B_LSQUARE"
  | Parser.LSQUARE -> "LSQUARE"
  | Parser.RSQUARE -> "RSQUARE"
  | Parser.LBRACE -> "LBRACE"
  | Parser.RBRACE -> "RBRACE"
  | Parser.COMMA -> "COMMA"
  | Parser.DOT -> "DOT"
  | Parser.SEMICOLON -> "SEMICOLON"
  | Parser.COLON -> "COLON"
  | Parser.BAR -> "BAR"
  | Parser.CONS -> "CONS"
  | Parser.CAST -> "CAST"
  | Parser.DOUBLE_ARROW -> "DOUBLE_ARROW"
  | Parser.ARROW -> "ARROW"
  | Parser.EOF -> "EOF"
  | Parser.B_MINUS -> "B_MINUS"
  | Parser.MINUS -> "MINUS"
  | Parser.B_PLUS -> "B_PLUS"
  | Parser.PLUS -> "PLUS"
  | Parser.B_STAR -> "B_STAR"
  | Parser.STAR -> "STAR"
  | Parser.SLASH -> "SLASH"
  | Parser.MOD -> "MOD"
  | Parser.B_POWER -> "B_POWER"
  | Parser.POWER -> "POWER"
  | Parser.SHIFT_RIGHT -> "SHIFT_RIGHT"
  | Parser.SHIFT_LEFT -> "SHIFT_LEFT"
  | Parser.BITWISE_AND -> "BITWISE_AND"
  | Parser.BITWISE_XOR -> "BITWISE_XOR"
  | Parser.BITWISE_OR -> "BITWISE_OR"
  | Parser.BITWISE_NOT -> "BITWISE_NOT"
  | Parser.LOGICAL_AND -> "LOGICAL_AND"
  | Parser.LOGICAL_OR -> "LOGICAL_OR"
  | Parser.LOGICAL_NOT -> "LOGICAL_NOT"
  | Parser.EQUAL -> "EQUAL"
  | Parser.PLUS_EQUAL -> "PLUS_EQUAL"
  | Parser.MINUS_EQUAL -> "MINUS_EQUAL"
  | Parser.STAR_EQUAL -> "STAR_EQUAL"
  | Parser.SLASH_EQUAL -> "SLASH_EQUAL"
  | Parser.MOD_EQUAL -> "MOD_EQUAL"
  | Parser.AND_EQUAL -> "AND_EQUAL"
  | Parser.OR_EQUAL -> "OR_EQUAL"
  | Parser.XOR_EQUAL -> "XOR_EQUAL"
  | Parser.SHIFT_LEFT_EQUAL -> "SHIFT_LEFT_EQUAL"
  | Parser.SHIFT_RIGHT_EQUAL -> "SHIFT_RIGHT_EQUAL"
  | Parser.EQUAL_TO -> "EQUAL_TO"
  | Parser.NOT_EQUAL -> "NOT_EQUAL"
  | Parser.LESS_EQUAL -> "LESS_EQUAL"
  | Parser.GREATER_EQUAL -> "GREATER_EQUAL"
  | Parser.LESS -> "LESS"
  | Parser.GREATER -> "GREATER"

let print_token t =
    let s = token2str t in
    print_string ("t: " ^ s ^ "\n")

let make_lexer fname =
    let _ = Lexer.fname := fname in
    let tokenbuf = ref [] in
    (fun lexbuf -> let t = match !tokenbuf with
        | t::rest -> tokenbuf := rest; t
        | _ -> (match Lexer.tokens lexbuf with
                | t::rest -> tokenbuf := rest; t
                | _ -> failwith "unexpected end of stream")
        in (print_token t; t))

let process_buf outchan l fname =
    let lexer = make_lexer fname in
    let ast = try
            Some(Parser.ficus_module lexer l)
        with
          Lexer.Error(err, p0, p1) ->
            Printf.printf "Lexer error: %s at %s\n" err (Lexer.pos2str p0);
            None
        | e -> print_string "Unknown error occurred during parsing\n"; raise e
    in ast

(*let process_buf outchan l =
    let lexer = make_lexer() in
    try while true do
        let t = lexer l in
        print_token t;
        match t with EOF -> raise LexerEOF | _ -> ()
    done with LexerEOF -> ()*)

let process_string s = process_buf stdout (Lexing.from_string s) ""

let trim_ext s = try let ridx = String.rindex s '.' in String.sub s 0 ridx with _ -> s;;

let process_file fname dump_to_stdout =
  let fname_bare = trim_ext fname in
  let inchan = open_in (fname) in
  let outchan = if dump_to_stdout then stdout else open_out (fname_bare ^ ".dump") in
  let ast = process_buf outchan (Lexing.from_channel inchan) fname in
  (*try*)
    (close_in inchan;
    if not dump_to_stdout then close_out outchan else ();
    (match ast with
    | Some((defs, deps)) ->
        let m = { dm_name=noid; dm_filename=fname; dm_defs=defs; dm_deps=deps } in
        PPrint.pprint_mod m
    | None -> print_string "some error occured during parsing\n")
    )
  (*with e -> (close_in inchan; close_out outchan; raise e)*)

let () =
  let files = ref [] in
  Arg.parse
    []
    (fun s -> files := !files @ [s])
    ("Ficus Compiler v0.1\n" ^
     Printf.sprintf "usage: %s file1.fx [file2.fx ...]" Sys.argv.(0));
  List.iter
    (fun f -> ignore (process_file f true))
    !files
