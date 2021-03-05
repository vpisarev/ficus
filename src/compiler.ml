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
    let (preamble, _) = List.fold_left (fun (preamble, found) (mname, from_import) ->
        if found then (preamble, found) else if bare_name = mname then (preamble, true)
        else if from_import then
            ((preamble @ [Parser.FROM; Parser.B_IDENT mname;
            Parser.IMPORT; Parser.B_STAR; Parser.SEMICOLON]), false)
        else
            ((preamble @ [Parser.B_IMPORT; Parser.B_IDENT mname; Parser.SEMICOLON]), false))
        ([], false) [("Builtins", true); ("List", false); ("Char", false); ("String", false)] in
    let preamble = if bare_name <> "Builtins" then preamble else
        [Parser.VAL; Parser.B_IDENT("__ficus_git_commit__");
        Parser.EQUAL; Parser.STRING(Config.git_commit); Parser.SEMICOLON] @ preamble in
    let tokenbuf = ref preamble in
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
    let _ = pr_verbose (sprintf "Parsing %s" fname) in
    let fname_id = get_id fname in
    let lexer = make_lexer fname in
    let use_stdin = fname = "stdin" in
    let inchan = if use_stdin then stdin else open_in fname in
    let l = Lexing.from_channel inchan in
    let _ = (parser_ctx_module := mname_id) in
    let _ = (parser_ctx_file := fname_id) in
    let _ = (parser_ctx_deps := []) in
    let _ = (parser_ctx_inc_dirs := inc_dirs) in
    let _ = (parser_ctx_module_idx := !parser_ctx_module_idx + 1) in
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
            let deps = List.rev (!parser_ctx_deps) in
            let _ = (!minfo.dm_defs <- defs) in
            let _ = (!minfo.dm_parsed <- true) in
            let _ = (!minfo.dm_deps <- deps) in
            let _ = (!minfo.dm_idx <- !parser_ctx_module_idx) in
            (* locate the deps, update the list of deps using proper ID's of real modules *)
            let queue_delta = List.fold_left (fun queue_delta dep ->
                let dep_minfo = get_module dep in
                if not !dep_minfo.dm_parsed then
                    !dep_minfo.dm_name :: queue_delta
                else
                    queue_delta) [] deps
                in
            queue := (List.rev queue_delta) @ !queue
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
    let graph = List.sort (fun (i, _, _) (j, _, _) -> i - j) graph in
    let rec loop remaining result =
        let rec find_next analyzed rest =
            match rest with
            | (i, m, deps) :: rest ->
                if List.for_all (fun d -> List.mem d result) deps then
                    (m, ((List.rev analyzed) @ rest))
                else
                    find_next ((i, m, deps) :: analyzed) rest
            | _ ->
                let msg = sprintf "error: cylic module dependency: %s\n" (String.concat " "
                    (List.map (fun (_, m, _) -> pp_id2str m) (List.rev analyzed))) in
                failwith msg
            in
        match remaining with
        | [] -> (List.rev result)
        | _ ->
            let (next_m, remaining) = find_next [] remaining in
            loop remaining (next_m :: result)
        in
    loop graph []

let typecheck_all modules =
    let _ = (compile_errs := []) in
    let _ = (List.iter Ast_typecheck.check_mod modules) in
    !compile_errs = []

let k_normalize_all modules =
    let _ = (compile_errs := []) in
    let _ = K_form.init_all_idks() in
    let kmods = K_normalize.normalize_all_modules modules in
    (List.rev kmods, !compile_errs = [])

let prf str = pr_verbose (sprintf "\t%s" str)

let k_optimize_all kmods =
    let _ = (compile_errs := []) in
    let niters = options.optim_iters in
    let temp_kmods = ref kmods in
    prf "initial dead code elim";
    temp_kmods := K_deadcode_elim.elim_unused !temp_kmods;
    for i = 1 to niters do
        pr_verbose (sprintf "Optimization pass #%d:" i);
        if i <= 2 then
            (prf "simple lifting"; temp_kmods := K_simple_ll.lift !temp_kmods;
            prf "annotate types"; temp_kmods := K_annotate_types.annotate_types !temp_kmods)
        else ();
        prf "tailrec";
        temp_kmods := K_tailrec.tailrec2loops_all !temp_kmods;
        prf "loop inv";
        temp_kmods := K_loop_inv.move_loop_invs_all !temp_kmods;
        prf "inline";
        if options.inline_thresh > 0 then temp_kmods := K_inline.inline_some !temp_kmods else ();
        prf "flatten";
        temp_kmods := K_flatten.flatten_all !temp_kmods;
        prf "fuse loops";
        temp_kmods := K_fuse_loops.fuse_loops_all !temp_kmods;
        prf "fast idx";
        temp_kmods := K_fast_idx.optimize_idx_checks_all !temp_kmods;
        (* [TODO] Dealiasing phase may produce wrong code,
           but the subsequent deadcode elimination fixes the problems (so far).
           It's better to fix cfold_dealias so that there is no such
           critical dependency. *)
        prf "const folding";
        temp_kmods := K_cfold_dealias.cfold_dealias !temp_kmods;
        prf "dead code elim";
        temp_kmods := K_deadcode_elim.elim_unused !temp_kmods
    done;
    pr_verbose "Finalizing K-form:";
    (*(K_pp.pprint_kmods !temp_kmods);*)
    prf "lambda lifting";
    temp_kmods := K_lift.lift_all !temp_kmods;
    prf "flatten";
    temp_kmods := K_flatten.flatten_all !temp_kmods;
    prf "dead code elim";
    temp_kmods := K_deadcode_elim.elim_unused !temp_kmods;
    prf "mangle";
    temp_kmods := K_mangle.mangle_all !temp_kmods;
    prf "dead code elim";
    (* name mangling may create some unused type definitions *)
    temp_kmods := K_deadcode_elim.elim_unused !temp_kmods;
    prf "mark recursive";
    temp_kmods := K_inline.find_recursive_funcs_all !temp_kmods;
    prf "annotate types";
    temp_kmods := K_annotate_types.annotate_types !temp_kmods;
    (!temp_kmods, !compile_errs = [])

let k2c_all kmods =
    pr_verbose "Generating C code:";
    let _ = (compile_errs := []) in
    let _ = C_form.init_all_idcs() in
    let _ = C_gen_std.init_std_names() in
    let _ = pr_verbose "\tstd calls initialized" in
    let cmods = C_gen_code.gen_ccode_all kmods in
    let _ = pr_verbose "C code generated" in
    let cmods = C_post_rename_locals.rename_locals cmods in
    let _ = pr_verbose "\tlocal variables renamed" in
    let cmods = List.map (fun cmod ->
        let is_cpp = options.compile_by_cpp || cmod.cmod_pragmas.pragma_cpp in
        if is_cpp then C_post_adjust_decls.adjust_decls_ cmod else cmod) cmods in
    let _ = pr_verbose "\tConversion to C-form complete" in
    (cmods, !compile_errs = [])

let emit_c_files fname0 cmods =
    let build_root_dir = options.build_rootdir in
    let _ = try Unix.mkdir build_root_dir 0o755 with Unix.Unix_error(Unix.EEXIST, _, _) -> () in
    let build_dir = options.build_dir in
    (try Unix.mkdir build_dir 0o755 with Unix.Unix_error(Unix.EEXIST, _, _) -> ());
    let (new_cmods, ok) = List.fold_left (fun (new_cmods, ok) cmod ->
        let {cmod_cname; cmod_ccode; cmod_pragmas={pragma_cpp}} = cmod in
        let output_fname = Filename.basename cmod_cname in
        let is_cpp = options.compile_by_cpp || pragma_cpp in
        let ext = if is_cpp then ".cpp" else ".c" in
        let output_fname = (Utils.remove_extension output_fname) ^ ext in
        let output_fname = Utils.normalize_path build_dir output_fname in
        (*let ok = if ok then C_pp.pprint_top_to_file output_fname cmod_ccode else ok in*)
        let (new_cmod, ok) = if ok then
                let str_new = C_pp.pprint_top_to_string cmod_ccode in
                let str_old = Utils.file2str output_fname in
                let (recompile, ok) = if str_new = str_old then (false, ok) else
                    (true, (Utils.str2file str_new output_fname))
                    in
                ({cmod with cmod_recompile=recompile}, ok)
            else (cmod, ok)
            in
        ((new_cmod :: new_cmods), ok)) ([], true) cmods in
    let _ = pr_verbose "C files are written" in
    ((List.rev new_cmods), build_dir, ok)

let print_if_verbose s =
    if options.verbose then (print_string s; flush stdout) else ()

let run_compiler cmods output_dir =
    let opt_level = options.optimize_level in
    let c_comp = "cc" in
    let cpp_comp = "c++ -std=c++11" in
    let cmd = " -Wno-unknown-warning-option" in
    let cmd = cmd ^ " -Wno-dangling-else" in
    let cmd = cmd ^ (sprintf " -O%d%s" opt_level (if opt_level = 0 then " -ggdb" else "")) in
    let cmd = cmd ^ " -I" ^ options.runtime_path in
    let custom_cflags = try " " ^ (Sys.getenv "FICUS_CFLAGS") with Not_found -> "" in
    let custom_cflags = if options.cflags = "" then custom_cflags else " " ^ options.cflags ^ custom_cflags in
    let cmd = cmd ^ custom_cflags in
    let (any_cpp, any_recompiled, all_clibs, ok, objs) = List.fold_left
        (fun (any_cpp, any_recompiled, all_clibs, ok, objs)
        {cmod_cname; cmod_recompile; cmod_pragmas={pragma_cpp; pragma_clibs}} ->
        let cname = Utils.normalize_path output_dir cmod_cname in
        let is_cpp = options.compile_by_cpp || pragma_cpp in
        let ext = if is_cpp then ".cpp" else ".c" in
        let comp = if is_cpp then cpp_comp else c_comp in
        let c_filename = cname ^ ext in
        let obj_filename = cname ^ ".o" in
        let _ = print_if_verbose (sprintf "CC %s:\n" c_filename) in
        let cmd = comp ^ cmd ^ " -o " ^ obj_filename in
        let cmd = cmd ^ " -c " ^ c_filename in
        let (ok_j, recompiled) =
            if cmod_recompile || not (Sys.file_exists obj_filename) then
                (print_if_verbose (sprintf "\t%s\n" cmd);
                (((Sys.command cmd) = 0), true))
            else
                (print_if_verbose (sprintf "\t%s is up-to-date\n" obj_filename);
                (true, false))
            in
        let clibs = List.rev (List.map (fun (l, _) -> l) pragma_clibs) in
        ((any_cpp || is_cpp), (any_recompiled || recompiled),
        (clibs @ all_clibs), (ok && ok_j), (obj_filename :: objs)))
        (false, false, [], true, []) cmods
        in
    if ok && (not any_recompiled) && (Sys.file_exists options.app_filename) then
        (print_if_verbose (sprintf "%s is up-to-date\n" options.app_filename); ok)
    else if not ok then ok else
        let cmd = c_comp ^ " -o " ^ options.app_filename in
        let cmd = cmd ^ " " ^ (String.concat " " objs) in
        let linked_libs = try " " ^ (Sys.getenv "FICUS_LINK_LIBRARIES") with Not_found -> "" in
        let linked_libs = if options.clibs = "" then linked_libs else linked_libs ^ " " ^ options.clibs in
        let linked_libs = if all_clibs = [] then linked_libs else
            linked_libs ^ " " ^ (String.concat " " (List.map (fun l -> "-l" ^ l) (List.rev all_clibs)))
            in
        let cmd = cmd ^ linked_libs in
        let cmd = cmd ^ " -lm" ^ (if any_cpp then " -lstdc++" else "") in
        let _ = print_if_verbose (sprintf "%s\n" cmd) in
        let ok = (Sys.command cmd) = 0 in
        ok

let run_app () =
    let cmd = String.concat " " (options.app_filename :: options.app_args) in
    let ok = (Sys.command cmd) = 0 in
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
            (!minfo.dm_idx, m, !minfo.dm_deps) :: gr) all_modules [] in
        let _ = sorted_modules := toposort graph in
        let _ = if options.print_ast0 then
            (List.iter (fun m -> let minfo = get_module m in Ast_pp.pprint_mod !minfo) !sorted_modules) else () in
        let _ = pr_verbose (sprintf "Parsing complete. Modules used: %s"
            (String.concat ", " (List.map id2str !sorted_modules))) in
        let ok = typecheck_all !sorted_modules in
        let _ = pr_verbose "Type checking complete" in
        let _ = if ok && options.print_ast then
            (List.iter (fun m -> let minfo = get_module m in Ast_pp.pprint_mod !minfo) !sorted_modules) else () in
        let (kmods, ok) = if ok then k_normalize_all !sorted_modules else ([], false) in
        let _ = pr_verbose "K-normalization complete" in
        let _ = pr_verbose "K-form optimization started" in
        let _ = if ok && options.print_k0 then (K_pp.pprint_kmods kmods) else () in
        let (kmods, ok) = if ok then k_optimize_all kmods else ([], false) in
        let _ = pr_verbose "K-form optimization complete" in
        let _ = if ok && options.print_k then (K_pp.pprint_kmods kmods) else () in
        if not options.gen_c then ok else
            let (cmods, ok) = if ok then k2c_all kmods else ([], false) in
            let (cmods, builddir, ok) = if ok then emit_c_files fname0 cmods else (cmods, ".", ok) in
            let ok = if ok && (options.make_app || options.run_app) then (run_compiler cmods builddir) else ok in
            let ok = if ok && options.run_app then run_app() else ok in
            ok
    with
    | Failure msg -> print_string msg; false
    | Ast.CompileError (loc, msg) as e -> Ast.print_compile_err e; false
    | e -> (printf "\n\nException %s occured" (Printexc.to_string e)); false
    in if not ok then
        print_all_compile_errs()
    else (); ok
