open Syntax
open Lexing
exception LexerEOF

let make_lexer fname =
    let _ = Lexer.fname := fname in
    let prev_lnum = ref 0 in
    let tokenbuf = ref [] in
    let print_token lexbuf t =
      (let s = Lexer.token2str t in
       let pos_lnum = lexbuf.lex_curr_p.pos_lnum in
       if pos_lnum > !prev_lnum then
          ((Printf.printf "\n%s (%d): %s" fname pos_lnum s);
          prev_lnum := pos_lnum)
       else print_string (" " ^ s);
       match t with
       Parser.EOF -> print_string "\n"
       | _ -> ()) in
    (fun lexbuf -> let t = match !tokenbuf with
        | t::rest -> tokenbuf := rest; t
        | _ -> (match Lexer.tokens lexbuf with
                | t::rest -> tokenbuf := rest; t
                | _ -> failwith "unexpected end of stream")
        in (if !Options.v.print_tokens then print_token lexbuf t else ()); t)

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
  let ok = Options.parse_options() in
  if ok then
  ignore(process_file (!Options.v.filename) true)
  else ()
