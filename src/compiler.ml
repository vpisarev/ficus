(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(*
    The top-level "driver" module that
    performs all the compilation steps
    in the certain order:

    lexical + syntactic analysis (parsing) =>
    type checking =>
    k-normalization =>
    iterative k-form optimization =>
    final k-form preparation (lambda lifting, name mangling ...) =>
    C code generation =>
    [optional C compiler invocation to process the produced C code]
*)

open Lexing
open Options
open Ast
open K_form
open C_form
open Utils

exception CumulativeParseError

let make_lexer fname =
    let _ = Lexer.fname := fname in
    let bare_name = Utils.remove_extension (Filename.basename fname) in
    let prev_lnum = ref 0 in
    (* the standard preamble *)
    let tokenbuf = (if bare_name = "Builtins" then ref [] else
        let from_builtins_import_all = [Parser.FROM; Parser.B_IDENT "Builtins"; Parser.IMPORT; Parser.STAR; Parser.SEMICOLON] in
        let import_list = if bare_name = "List" then []
            else [Parser.B_IMPORT; Parser.B_IDENT "List"; Parser.SEMICOLON] in
        let import_string = if bare_name = "List" || bare_name = "String" then []
            else [Parser.B_IMPORT; Parser.B_IDENT "String"; Parser.SEMICOLON] in
        let import_basics = from_builtins_import_all @ import_list @ import_string in
        ref import_basics) in
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
        in (if options.print_tokens then print_token lexbuf t else ()); t)

let parse_file mname_id fname inc_dirs =
    let fname_id = get_id fname in
    let lexer = make_lexer fname in
    let use_stdin = fname = "stdin" in
    let inchan = if use_stdin then stdin else open_in fname in
    let l = Lexing.from_channel inchan in
    let _ = (parser_ctx_module := mname_id) in
    let _ = (parser_ctx_file := fname_id) in
    let _ = (parser_ctx_deps := []) in
    let _ = (parser_ctx_inc_dirs := inc_dirs) in
    try
        let ast = Parser.ficus_module lexer l in
        (if use_stdin then () else close_in inchan;
        ast)
    with
    | e -> close_in inchan; raise e

let parse_all _fname0 =
    let cwd = Sys.getcwd() in
    let fname0 = normalize_path cwd _fname0 in
    let dir0 = Filename.dirname fname0 in
    let fname0 = if _fname0 = "stdin" then _fname0 else fname0 in
    let inc_dirs0 = (if dir0 = cwd then [cwd] else [dir0; cwd]) @ options.include_path in
    let inc_dirs0 = List.map (fun d -> normalize_path cwd d) inc_dirs0 in
    (*let _ = print_string ("Module search path:\n\t" ^ (String.concat ",\n\t" inc_dirs0) ^ "\n") in*)
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
            let defs = parse_file mname mfname inc_dirs in
            let deps = !parser_ctx_deps in
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
        | SyntaxErrorLoc(err, loc) ->
            printf "%s: %s\n" (loc2str loc) err; ok := false
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
    let _ = (compile_errs := []) in
    let _ = (List.iter Ast_typecheck.check_mod modules) in
    !compile_errs = []

let k_normalize_all modules =
    let _ = (compile_errs := []) in
    let _ = K_form.init_all_idks() in
    let n = List.length modules in
    let (_, kmods) = List.fold_left (fun (i, kmods) m ->
        let km = K_normalize.normalize_mod m (i+1=n) in
        (i+1, (km :: kmods))) (0, []) modules in
    (List.rev kmods, !compile_errs = [])

let k_optimize_all kmods =
    let _ = (compile_errs := []) in
    let niters = 5 in
    let temp_kmods = ref kmods in
    for i = 0 to niters-1 do
        temp_kmods := K_deadcode_elim.elim_unused !temp_kmods;
        if i <= 1 then
            (temp_kmods := K_simple_ll.lift !temp_kmods;
            temp_kmods := K_annotate_types.annotate_types !temp_kmods)
        else ();
        temp_kmods := K_tailrec.tailrec2loops !temp_kmods;
        (*temp_kmods := K_loop_inv.move_loop_invs !temp_kmods;
        if options.inline_thresh > 0 then temp_kmods := K_inline.inline_some !temp_kmods else ();*)
        temp_kmods := K_flatten.flatten !temp_kmods;
        (*temp_kmods := K_fuse_loops.fuse_loops !temp_kmods;
        temp_kmods := K_fast_idx.optimize_idx_checks !temp_kmods;*)
        temp_kmods := K_cfold_dealias.cfold_dealias !temp_kmods
    done;
    temp_kmods := K_lift.lift_all !temp_kmods;
    temp_kmods := K_flatten.flatten !temp_kmods;
    temp_kmods := K_deadcode_elim.elim_unused !temp_kmods;
    temp_kmods := K_mangle.mangle_all !temp_kmods;
    temp_kmods := K_deadcode_elim.elim_unused !temp_kmods;
    (*temp_kmods := K_inline.find_recursive_funcs !temp_kmods;*)
    temp_kmods := K_annotate_types.annotate_types !temp_kmods;
    (!temp_kmods, !compile_errs = [])

let k2c_all kmods =
    let _ = (compile_errs := []) in
    let _ = C_form.init_all_idcs() in
    let _ = C_gen_std.init_std_names() in
    let cmods = C_gen_code.gen_ccode_all kmods in
    let cmods = C_post_rename_locals.rename_locals cmods in
    let cmods = if options.compile_by_cpp then C_post_adjust_decls.adjust_decls cmods else cmods in
    (cmods, !compile_errs = [])

let emit_c_files fname0 cmods =
    let build_root_dir = Unix.getcwd() in
    let build_root_dir = Utils.normalize_path build_root_dir "__build__" in
    let _ = try Unix.mkdir build_root_dir 0o755 with Unix.Unix_error(Unix.EEXIST, _, _) -> () in
    let output_dir = Filename.basename fname0 in
    let output_dir = Utils.remove_extension output_dir in
    let output_dir = Utils.normalize_path build_root_dir output_dir in
    (try Unix.mkdir output_dir 0o755 with Unix.Unix_error(Unix.EEXIST, _, _) -> ());
    let ok = List.fold_left (fun ok cmod ->
        let {cmod_cname; cmod_ccode} = cmod in
        let output_fname = Filename.basename cmod_cname in
        let output_fname = (Utils.remove_extension output_fname) ^ ".c" in
        let output_fname = Utils.normalize_path output_dir output_fname in
        let ok = if ok then C_pp.pprint_top_to_file output_fname cmod_ccode else ok in
        ok) true cmods in
    (cmods, output_dir, ok)

let run_compiler cmods output_dir =
    let opt_level = options.optimize_level in
    let cmd = if options.compile_by_cpp then "cc -x c++ -std=c++11" else "cc" in
    let cmd = cmd ^ " -Wno-unknown-warning-option" in
    let cmd = cmd ^ " -Wno-dangling-else" in
    let cmd = cmd ^ (sprintf " -O%d%s" opt_level (if opt_level = 0 then " -ggdb" else "")) in
    let cmd = cmd ^ " -I" ^ options.runtime_path in
    let custom_cflags = try " " ^ (Sys.getenv "FICUS_CFLAGS") with Not_found -> "" in
    let custom_cflags = if options.cflags = "" then custom_cflags else options.cflags ^ " " ^ custom_cflags in
    let cmd = cmd ^ custom_cflags in
    let (ok, objs) = List.fold_left (fun (ok, objs) {cmod_cname} ->
        let cname = Utils.normalize_path output_dir cmod_cname in
        let cmd = cmd ^ " -o " ^ cname ^ ".o" in
        let cmd = cmd ^ " -c " ^ cname ^ ".c" in
        let _ = (printf "%s\n" cmd; flush stdout) in
        let ok_j = (Sys.command cmd) = 0 in
        ((ok && ok_j), ((cname ^ ".o") :: objs))) (true, []) cmods
        in
    if not ok then ok else
        let cmd = "cc -o " ^ options.app_filename in
        let cmd = cmd ^ " " ^ (String.concat " " objs) in
        let custom_linked_libs = try " " ^ (Sys.getenv "FICUS_LINK_LIBRARIES") with Not_found -> "" in
        let custom_linked_libs = if options.clibs = "" then custom_linked_libs else options.clibs ^ " " ^ custom_linked_libs in
        let cmd = cmd ^ custom_linked_libs in
        let cmd = cmd ^ " -lm" ^ (if options.compile_by_cpp then " -lstdc++" else "") in
        let _ = (printf "%s\n" cmd; flush stdout) in
        let ok = (Sys.command cmd) = 0 in
        ok

let run_app () =
    let cmd = String.concat " " (options.app_filename :: options.app_args) in
    let ok = (Sys.command cmd) = 0 in
    if options.make_app then () else Sys.remove options.app_filename;
    ok

let print_all_compile_errs () =
    let nerrs = List.length !compile_errs in
    if nerrs = 0 then ()
    else
        (List.iter print_compile_err (List.rev !compile_errs);
        printf "\n%d errors occured during type checking.\n" nerrs)

let process_all fname0 =
    init();
    let ok =
    try
        let _ = if (parse_all fname0) then () else raise CumulativeParseError in
        let graph = Hashtbl.fold (fun mfname m gr ->
            let minfo = get_module m in
            (m, !minfo.dm_deps) :: gr) all_modules [] in
        let _ = (sorted_modules := List.rev (toposort graph)) in
        (*let _ = (printf "Sorted modules: %s\n" (String.concat ", " (List.map id2str !sorted_modules))) in*)
        (*let _ = if options.print_ast then
            (List.iter (fun m -> let minfo = get_module m in Ast_pp.pprint_mod !minfo) !sorted_modules) else () in*)
        let ok = typecheck_all !sorted_modules in
        let _ = if ok && options.print_ast then
            (List.iter (fun m -> let minfo = get_module m in Ast_pp.pprint_mod !minfo) !sorted_modules) else () in
        let (kmods, ok) = if ok then k_normalize_all !sorted_modules else ([], false) in
        (*let _ = if ok && options.print_k then (K_pp.pprint_top code) else () in*)
        let (kmods, ok) = if ok then k_optimize_all kmods else ([], false) in
        let _ = if ok && options.print_k then (K_pp.pprint_kmods kmods) else () in
        if not options.gen_c then ok else
            let (cmods, ok) = if ok then k2c_all kmods else ([], false) in
            let (cmods, builddir, ok) = if ok then emit_c_files fname0 cmods else (cmods, ".", ok) in
            let ok = if ok && (options.make_app || options.run_app) then (run_compiler cmods builddir) else ok in
            (*let ok = if ok && options.run_app then run_app() else ok in*)
            ok
    with
    | Failure msg -> print_string msg; false
    | e -> (printf "\n\nException %s occured" (Printexc.to_string e)); false
    in if not ok then
        print_all_compile_errs()
    else (); ok
