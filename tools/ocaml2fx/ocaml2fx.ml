(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

let printf = Format.printf

let make_lexer () =
    if !Syntax.opt_pr_tokens then
        (fun lexbuf ->
            let t = Lexer.token lexbuf in
            printf "%s " (Lexer.token2str t); t)
    else
        Lexer.token

let parse_ocaml fname =
    let inchan = open_in fname in
    try
        let _ = Syntax.lineno := 1 in
        let lexer = make_lexer () in
        let t = Parser.toplevel lexer (Lexing.from_channel inchan) in
        close_in inchan;
        t
    with e ->
        (close_in inchan;
        (match e with
        | Lexer.LexError(msg, (p1, p2)) ->
            printf "\n%s:%d: error: Lexer error '%s'\n" fname !Syntax.lineno msg
        | Syntax.SyntaxError(msg, p1, p2) ->
            printf "\n%s:%d: error: Parser error '%s'\n" fname !Syntax.lineno msg
        | _ -> printf "\n%s:%d: error: Unknown exception\n" fname !Syntax.lineno);
        raise e)

let convert_ocaml fname =
    let ocode = parse_ocaml fname in
    let ocode = Transform.transform_let ocode in
    let ofname = Filename.basename fname in
    let ofname = try Filename.chop_extension ofname with Invalid_argument _ -> ofname in
    let ofname = ofname ^ ".fx" in
    let outchan = open_out ofname in
    try
        Print_ficus.print_top ocode;
        close_out outchan
    with e -> (close_out outchan; raise e)

let _ =
  let files = ref [] in
  Arg.parse
      [("-infer-types", Arg.Unit(fun () -> Syntax.opt_infer_types := true), "try to infer some types");
      ("-pr-tokens", Arg.Unit(fun () -> Syntax.opt_pr_tokens := true), "print tokens when parsing .ml files")]
      (fun s -> files := !files @ [s])
      ("Ocaml to Ficus converter.\n"
      ^"Usage: ocaml2fx [-infer_types] filename1 ...\n");
  List.iter convert_ocaml !files
