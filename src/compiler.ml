open Lexing
open Options
open Syntax
open Utils

exception CumulativeParseError

let make_lexer fname =
    let _ = Lexer.fname := fname in
    let bare_name = Utils.remove_extension (Filename.basename fname) in
    let prev_lnum = ref 0 in
    (* the standard preamble *)
    let tokenbuf = (if bare_name = "Builtin" then ref [] else
        ref [Parser.FROM; Parser.B_IDENT "Builtin"; Parser.IMPORT; Parser.STAR; Parser.SEMICOLON]) in
    let print_token lexbuf t =
      (let s = Lexer.token2str t in
       let pos_lnum = lexbuf.lex_curr_p.pos_lnum in
       if pos_lnum > !prev_lnum then
          ((printf "\n%s (%d): %s" fname pos_lnum s);
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
        in (if !options.print_tokens then print_token lexbuf t else ()); t)

let parse_file fname inc_dirs =
    let fname_id = get_id fname in
    let lexer = make_lexer fname in
    let inchan = open_in fname in
    let l = Lexing.from_channel inchan in
    let _ = (Utils.parser_ctx_file := fname_id) in
    let _ = (Utils.parser_ctx_deps := []) in
    let _ = (Utils.parser_ctx_inc_dirs := inc_dirs) in
    try
        let ast = Parser.ficus_module lexer l in
        close_in inchan; ast
    with
    | e -> close_in inchan; raise e

let parse_all _fname0 =
    let cwd = Sys.getcwd() in
    let fname0 = normalize_path cwd _fname0 in
    let dir0 = Filename.dirname fname0 in
    let inc_dirs0 = (if dir0 = cwd then [cwd] else [dir0; cwd]) @ !options.include_path in
    let inc_dirs0 = List.map (fun d -> normalize_path cwd d) inc_dirs0 in
    let _ = print_string ("Module search path:\n\t" ^ (String.concat ",\n\t" inc_dirs0) ^ "\n") in
    let name0_id = get_id (Utils.remove_extension (Filename.basename fname0)) in
    let minfo = find_module name0_id fname0 in
    let queue = ref [!minfo.dm_name] in
    let ok = ref true in
    while !queue != [] do
        let mname = List.hd (!queue) in
        let _ = queue := List.tl (!queue) in
        let minfo = get_module mname in
        let mfname = !minfo.dm_filename in
        if !minfo.dm_parsed then ()
        else
        (try
            let dir1 = Filename.dirname mfname in
            let inc_dirs = (if dir1 = dir0 then [] else [dir1]) @ inc_dirs0 in
            let defs = parse_file mfname inc_dirs in
            let deps = !Utils.parser_ctx_deps in
            let _ = (!minfo.dm_defs <- defs) in
            let _ = (!minfo.dm_parsed <- true) in
            let _ = (!minfo.dm_deps <- deps) in
            (* locate the deps, update the list of deps using proper ID's of real modules *)
            List.iter (fun dep ->
                let dep_minfo = get_module dep in
                if not !dep_minfo.dm_parsed then
                    queue := !dep_minfo.dm_name :: !queue
                else ()) deps
        with
        | Lexer.LexError(err, (p0, p1)) ->
            printf "%s: %s\n" (Lexer.pos2str p0 true) err; ok := false
        | SyntaxError(err, p0, p1) ->
            printf "%s: %s\n" (Lexer.pos2str p0 true) err; ok := false
        | Failure(msg) -> (printf "%s: %s\n" mfname msg); ok := false
        | e -> (printf "%s: exception %s occured" mfname (Printexc.to_string e)); ok := false)
    done;
    !ok

let init () =
    ignore(init_all_ids ());
    (Hashtbl.reset all_modules)

(*
  Sort the modules topologically using the algorithm from
  https://stackoverflow.com/questions/4653914/topological-sort-in-ocaml
  Big thanks to Victor Nicollet for the code.
*)
let toposort graph =
    let dfs graph visited start_node =
        let rec explore path visited node =
            if List.mem node path then
                let msg = (sprintf "error: cylic module dependency: %s\n" (String.concat " " (List.map pp_id2str path))) in
                failwith msg
            else if List.mem node visited then visited else
                let new_path = node :: path in
                let edges = List.assoc node graph in
                let visited = List.fold_left (explore new_path) visited edges in
                node :: visited
        in explore [] visited start_node in
    List.fold_left (fun visited (node,_) -> dfs graph visited node) [] graph

let typecheck_all modules =
    let _ = (Typechecker.typecheck_errs := []) in
    let _ = (List.iter (fun m -> Typechecker.check_mod m(*; printf "typed module:\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
            let minfo = get_module m in PPrint.pprint_mod !minfo; printf "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"*)) modules) in
    let errs = !Typechecker.typecheck_errs in
    List.iter (fun err -> Typechecker.print_typecheck_err err) errs;
    errs

let process_all fname0 =
    init();
    try
        let _ = if (parse_all fname0) then () else raise CumulativeParseError in
        let graph = Hashtbl.fold (fun mfname m gr ->
            let minfo = get_module m in
            (m, !minfo.dm_deps) :: gr) all_modules [] in
        let _ = (sorted_modules := List.rev (toposort graph)) in
        let _ = (printf "Sorted modules: %s\n" (String.concat ", " (List.map id2str !sorted_modules))) in
        (*let _ = (List.iter (fun m -> let minfo = get_module m in PPrint.pprint_mod !minfo) !sorted_modules) in*)
        let typecheck_errs = typecheck_all !sorted_modules in
        let errcount = List.length typecheck_errs in
        if errcount != 0 then
            (List.iter (fun err -> Typechecker.print_typecheck_err err) !Typechecker.typecheck_errs;
            printf "\n\n%d errors occured during type checking.\n" errcount;
            false)
        else
            (if not !options.print_ast then () else
            (List.iter (fun m -> let minfo = get_module m in PPrint.pprint_mod !minfo) !sorted_modules);
            true)
    with
    | Failure msg -> print_string msg; false
    | e -> (printf "\n\nException %s occured" (Printexc.to_string e)); false
