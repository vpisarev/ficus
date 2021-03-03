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
    mutable inline_thresh: int;
    mutable output_name: string;
    mutable print_tokens: bool;
    mutable print_ast0: bool;
    mutable print_ast: bool;
    mutable print_k0: bool;
    mutable print_k: bool;
    mutable gen_c: bool;
    mutable make_app: bool;
    mutable run_app: bool;
    mutable filename: string;
    mutable app_filename: string;
    mutable app_args: string list;
    mutable osname: string;
    mutable compile_by_cpp: bool;
    mutable build_rootdir: string;
    mutable build_dir: string;
    mutable cflags: string;
    mutable clibs: string;
    mutable verbose: bool;
}

let options =
{
    (* [TODO] change it to max_int > 2147483647 after rewriting compiler in ficus
       when on 32-bit platforms we will be using all 32 bits for int's *)
    arch64 = (max_int > 1073741823);
    include_path = [];
    runtime_path = "";
    optimize_level = 1;
    inline_thresh = 30;
    output_name = "";
    print_tokens = false;
    print_ast0 = false;
    print_ast = false;
    print_k0 = false;
    print_k = false;
    gen_c = true;
    make_app = true;
    run_app = false;
    filename = "";
    app_filename = "";
    app_args = [];
    osname = "*nix";
    compile_by_cpp = false;
    build_rootdir = ".";
    build_dir = "";
    cflags = "";
    clibs = "";
    verbose = false;
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
        ("-pr-ast0", (Arg.Unit (fun f -> options.print_ast0 <- true)), "   Print retrieved AST of the parsed files");
        ("-pr-ast", (Arg.Unit (fun f -> options.print_ast <- true)), "   Print AST after type checker");
        ("-pr-k0", (Arg.Unit (fun f -> options.print_k0 <- true)), "   Print the generated from AST K-form");
        ("-pr-k", (Arg.Unit (fun f -> options.print_k <- true)), "   Print the generated K-form after optimization");
        ("-no-c", (Arg.Unit (fun f -> options.gen_c <- false)), "   Do not generate C code");
        ("-app", (Arg.Unit (fun f -> options.make_app <- true)), "   Build application (set by default)");
        ("-run", (Arg.Unit (fun f -> options.run_app <- true)), "   Build application and run it");
        ("-O0", (Arg.Unit (fun () -> options.optimize_level <- 0)), "   Optimization level 0: disable optimizations except for the most essential ones");
        ("-O1", (Arg.Unit (fun () -> options.optimize_level <- 1)), "   Optimization level 1: enable most of optimizations");
        ("-O3", (Arg.Unit (fun () -> options.optimize_level <- 3)), "   Optimization level 3: enable all optimizations");
        ("-inline-threshold", (Arg.Int (fun i -> options.inline_thresh <- i)), "<n>   Inline threshold (100 by default); the higher it is, the bigger functions are inlined; --inline-thresh=0 disables inline expansion");
        ("-o", (Arg.String (fun s -> options.output_name <- s)), "<output_filename>    Output file name");
        ("-I", (Arg.String (fun ipath -> options.include_path <- options.include_path @ [ipath])), "<path>    Add directory to the module search path");
        ("-B", (Arg.String (fun s -> options.build_rootdir <- s)), "<build_parent_dir> The parent directory where __build__/appname subdirectory will be created");
        ("-c++", (Arg.Unit (fun f -> options.compile_by_cpp <- true)), "   Use C++ instead of C for compilation");
        ("-cflags", (Arg.String (fun s -> options.cflags <- s)), "<cflags>   Pass the specified flags, e.g. \"-mavx2\", to C/C++ compiler (after $FICUS_CFLAGS)");
        ("-clibs", (Arg.String (fun s -> options.clibs <- s)), "<clibs>   Pass the specified libs/linker flags to C/C++ compiler (before $FICUS_LINK_LIBRARIES)");
        ("-verbose", (Arg.Unit (fun f -> options.verbose <- true)), "  Display various info during build time");
        ("--", (Arg.Rest (fun s -> options.app_args <- s :: options.app_args)), "Specify the application parameters (e.g. './ficus -run myprog.fx -- arg1 arg2')")
        ]
        (fun s -> _files := !_files @ [s])
        ("Ficus Compiler v0.1\n" ^
        sprintf "Usage: %s [options ...] [input_file.fx]" Sys.argv.(0));

        if options.optimize_level = 0 then
            options.inline_thresh <- 1
        else ();

        options.filename <- (match !_files with
            | f :: [] -> Utils.normalize_path curr_dir f
            | _ -> raise (Arg.Bad "there should be exactly one input file"));

        if (options.run_app || options.compile_by_cpp) && not options.gen_c then
            raise (Arg.Bad "-no-c option cannot be used together with -run or -c++")
        else ();

        let output_name = Filename.basename options.filename in
        let output_name = Utils.remove_extension output_name in
        options.build_rootdir <- Utils.normalize_path
            (Utils.normalize_path curr_dir options.build_rootdir) "__build__";
        options.build_dir <- Utils.normalize_path options.build_rootdir output_name;

        options.app_filename <-
            if options.output_name <> "" then
                (Utils.normalize_path curr_dir options.output_name)
            else
                (Utils.normalize_path options.build_dir output_name);
        options.app_args <- List.rev options.app_args;

        let ch = Unix.open_process_in "uname" in
        let uname_output = input_line ch in
        let _ = close_in ch in
        options.osname <- String.trim uname_output;

        true
    with Arg.Bad msg -> print_string ("error: " ^ msg ^ "\n"); false
