open Lexing
open Options
open Syntax
open Utils

exception CumulativeParseError

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
        in (if !options.print_tokens then print_token lexbuf t else ()); t)

let parse_file fname =
    let fname_id = get_id fname in
    let lexer = make_lexer fname in
    let inchan = open_in fname in
    let l = Lexing.from_channel inchan in
    let _ = (current_file_id := fname_id) in
    let _ = (current_imported_modules := []) in
    try
        let ast = Parser.ficus_module lexer l in
        close_in inchan; ast
    with
    | e -> (Printf.printf "error occured when parsing %s :(\n" fname); close_in inchan; raise e

let dot_regexp = Str.regexp "\\."

let rec locate_module dep inc_dirs =
    let mname = pp_id2str dep in
    let mfname = (Str.global_replace dot_regexp Filename.dir_sep mname) ^ ".fx" in
    try Some(Filename.concat
                (List.find (fun d -> Sys.file_exists (Filename.concat d mfname)) inc_dirs)
            mfname)
    with Not_found -> None

let parse_all _fname0 =
    let cwd = Sys.getcwd() in
    let fname0 = normalize_path cwd _fname0 in
    let dir0 = Filename.dirname fname0 in
    let inc_dirs0 = (if dir0 = cwd then [cwd] else [dir0; cwd]) @ !options.include_path in
    let inc_dirs0 = List.map (fun d -> normalize_path cwd d) inc_dirs0 in
    let _ = print_string ("Module search path:\n\t" ^ (String.concat ",\n\t" inc_dirs0) ^ "\n") in
    let default_mods = [get_id "Builtin"] in
    let queue = ref [fname0] in
    let ok = ref true in
    let find_module mfname =
        (try get_module (Hashtbl.find all_modules mfname) with
          Not_found ->
            let bare_mfname = Utils.remove_extension (Filename.basename mfname) in
            let mname_id = get_unique_id bare_mfname false in
            let newmodule = ref { dm_name=mname_id; dm_filename=mfname; dm_defs=[];
                                  dm_deps=[]; dm_env=Env.empty; dm_parsed=false } in
            let _ = set_id_entry mname_id (IdModule newmodule) in
            let _ = Hashtbl.add all_modules mfname mname_id in
            newmodule)
    in
    while (!queue)!=[] do
        let mfname = List.hd (!queue) in
        let bare_mfname = Utils.remove_extension (Filename.basename mfname) in
        let _ = queue := List.tl (!queue) in
        let minfo = find_module mfname in
        if !minfo.dm_parsed then ()
        else
        (try
            let (defs, deps) = parse_file mfname in
            let _ = (!minfo.dm_defs <- defs) in
            let deps = (if bare_mfname = "Builtin" then [] else default_mods) @ deps in
            let dir1 = Filename.dirname mfname in
            let inc_dirs = (if dir1 = dir0 then [] else [dir1]) @ inc_dirs0 in
            (* locate the deps, update the list of deps using proper ID's of real modules *)
            let deps = List.map (fun dep -> match locate_module dep inc_dirs with
                  | Some(dep_fname) ->
                      let dep_fname = normalize_path cwd dep_fname in
                      let dep_minfo = find_module dep_fname in
                      if !dep_minfo.dm_parsed then () else queue := dep_fname :: !queue;
                      !dep_minfo.dm_name
                  | _ -> (Printf.printf "%s: error: module %s cannot be located\n"
                       bare_mfname (pp_id2str dep)); ok := false; dep) deps in
            !minfo.dm_deps <- deps
        with
        | Lexer.Error(err, p0, p1) ->
            Printf.printf "Lexer error: %s at %s\n" err (Lexer.pos2str p0); ok := false
        | e -> Printf.printf "Syntax error when processing %s" bare_mfname; ok := false)
    done;
    !ok

let init () =
    all_nids := 0;
    all_ids := [||];
    (Hashtbl.reset all_strings);
    ignore (get_id_ "");
    ignore (get_id_ "_");
    ignore (get_id_ "Builtin");
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
                let msg = (Printf.sprintf "error: cylic module dependency: %s\n" (String.concat " " (List.map pp_id2str path))) in
                failwith msg
            else if List.mem node visited then visited else
                let new_path = node :: path in
                let edges = List.assoc node graph in
                let visited = List.fold_left (explore new_path) visited edges in
                node :: visited
        in explore [] visited start_node in
    List.fold_left (fun visited (node,_) -> dfs graph visited node) [] graph

let process_all fname0 =
    init();
    try
        let _ = if (parse_all fname0) then () else raise CumulativeParseError in
        let graph = Hashtbl.fold (fun mfname m gr ->
            let minfo = get_module m in
            (m, !minfo.dm_deps) :: gr) all_modules [] in
        sorted_modules := List.rev (toposort graph);
        Printf.printf "Sorted modules: %s\n" (String.concat ", " (List.map id2str !sorted_modules));
        if not !options.print_ast then () else
        (List.iter (fun m -> let minfo = get_module m in PPrint.pprint_mod !minfo) !sorted_modules);
        true
    with
    | Failure msg -> print_string msg; false
    | e -> false
