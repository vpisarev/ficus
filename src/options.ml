(*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*)

(* Various compile-time options *)
open Printf

type options_t =
{
    mutable arch64: bool;
    mutable include_path: string list;
    mutable runtime_path: string;
    mutable optimize_level: int;
    mutable output_name: string;
    mutable print_tokens: bool;
    mutable print_ast: bool;
    mutable print_k: bool;
    mutable gen_c: bool;
    mutable write_c: bool;
    mutable make_app: bool;
    mutable run_app: bool;
    mutable filename: string;
    mutable c_filename: string;
    mutable app_filename: string;
    mutable app_args: string list;
}

let options =
{
    (* [TODO] change it to max_int > 2147483647 after rewriting compiler in ficus
       when on 32-bit platforms we will be using all 32 bits for int's *)
    arch64 = (max_int > 1073741823);
    include_path = [];
    runtime_path = "";
    optimize_level = 1;
    output_name = "";
    print_tokens = false;
    print_ast = false;
    print_k = false;
    gen_c = true;
    write_c = false;
    make_app = false;
    run_app = false;
    filename = "";
    c_filename = "";
    app_filename = "";
    app_args = []
}

let parse_options () =
    let curr_dir = Sys.getcwd() in
    let abs_ficus_path = Utils.normalize_path curr_dir (Sys.argv.(0)) in
    let abs_ficus_dir = Filename.dirname abs_ficus_path in
    let stdlib_dir = Utils.normalize_path abs_ficus_dir "../lib" in
    let _files = ref [] in
    options.include_path <- stdlib_dir :: [];
    options.runtime_path <- Utils.normalize_path abs_ficus_dir "../runtime";
    try
        Arg.parse
        [("-pr-tokens", (Arg.Unit (fun f -> options.print_tokens <- true)), "   Print all the tokens in parsed files");
        ("-pr-ast", (Arg.Unit (fun f -> options.print_ast <- true)), "   Print typechecked AST of the parsed files");
        ("-pr-k", (Arg.Unit (fun f -> options.print_k <- true)), "   Print the generated and optimized K-form");
        ("-no-c", (Arg.Unit (fun f -> options.gen_c <- false)), "   Do not generate C code");
        ("-c", (Arg.Unit (fun f -> options.write_c <- true)), "   Write .c file (by default, if -app or -run are specified, .c file is not stored)");
        ("-app", (Arg.Unit (fun f -> options.make_app <- true)), "   Compile and store application");
        ("-run", (Arg.Unit (fun f -> options.run_app <- true)), "   Compile and run application");
        ("-O0", (Arg.Unit (fun () -> options.optimize_level <- 0)), "   Optimization level 0: disable optimizations except for the most essential ones");
        ("-O1", (Arg.Unit (fun () -> options.optimize_level <- 1)), "   Optimization level 1: enable most of optimizations");
        ("-O3", (Arg.Unit (fun () -> options.optimize_level <- 3)), "   Optimization level 3: enable all optimizations");
        ("-o", (Arg.String (fun s -> options.output_name <- s)), "<output_filename>    Output file name");
        ("-I", (Arg.String (fun ipath -> options.include_path <- options.include_path @ [ipath])), "<path>    Add directory to the module search path");
        ("--", (Arg.Rest (fun s -> options.app_args <- s :: options.app_args)), "Specify the application parameters (e.g. './ficus -run myprog.fx -- arg1 arg2')")
        ]
        (fun s -> _files := !_files @ [s])
        ("Ficus Compiler v0.1\n" ^
        sprintf "Usage: %s [options ...] [input_file.fx]" Sys.argv.(0));

        let use_stdin = !_files = [] in

        options.filename <- (match !_files with
            | f :: [] -> Utils.normalize_path curr_dir f
            | [] -> "stdin"
            | _ -> raise (Arg.Bad "multiple input files are specified"));

        if (options.make_app || options.run_app || options.write_c) && not options.gen_c then
            raise (Arg.Bad "-no-c option cannot be used together with -app, -run or -c")
        else ();

        if options.gen_c && (not options.make_app) && (not options.run_app) then
            options.write_c <- true
        else ();

        let bare_filename = Filename.basename options.filename in
        let output_name = if options.output_name <> "" then options.output_name else bare_filename in
        let output_name = Utils.remove_extension output_name in
        let output_name = Utils.normalize_path curr_dir output_name in

        options.c_filename <-
            if options.write_c then
                (if use_stdin && options.output_name = "" then
                    Utils.normalize_path curr_dir "a.out.c"
                else
                    output_name ^ ".c")
            else
                Filename.temp_file (Filename.basename output_name) ".c";

        options.app_filename <-
            if options.make_app then
                (if use_stdin && options.output_name = "" then
                    Utils.normalize_path curr_dir "a.out.c"
                else
                    output_name)
            else
                Filename.temp_file (Filename.basename output_name) "";

        options.app_args <- List.rev options.app_args;
        true
    with Arg.Bad msg -> print_string ("error: " ^ msg ^ "\n"); false
