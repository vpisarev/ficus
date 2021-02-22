let parse_ocaml fname =
    let inchan = open_in (fname ^ ".ml") in
    try
        let t = Parser.toplevel Lexer.token (Lexing.from_channel inchan) in
        close_in inchan;
        t
    with e -> (close_in inchan; raise e)

let convert_ocaml fname =
    let ocode = parse_ocaml fname in
    let ofname = Filename.basename fname in
    let ofname = try Filename.chop_extension ofname with Invalid_argument _ -> ofname in
    let ofname = ofname ^ ".fx" in
    let outchan = open_out ofname in
    try
        Print_ficus.print_code ocode;
        close_out outchan
    with e -> (close_out outchan; raise e)

let _ =
  let files = ref [] in
  Arg.parse
      [("-infer_types", Arg.Unit(fun () -> Syntax.opt_infer_types := true), "try to infer some types")]
      (fun s -> files := !files @ [s])
      ("Ocaml to Ficus converter.\n"
      ^"Usage: ocaml2fx [-infer_types] filename1 ...\n");
  List.iter convert_ocaml !files
