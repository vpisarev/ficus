(* Various compile-time options *)
open Printf

type options_t =
{
    mutable arch64: bool;
    mutable include_path: string list;
    mutable optimize_level: int;
    mutable output_name: string;
    mutable print_tokens: bool;
    mutable print_ast: bool;
    mutable print_orig_k: bool;
    mutable filename: string;
}

let options =
{
    arch64 = (max_int > 1073741823);
    include_path = [];
    optimize_level = 1;
    output_name = "";
    print_tokens = false;
    print_ast = false;
    print_orig_k = false;
    filename = ""
}

let parse_options () =
    let abs_ficus_path = Utils.normalize_path (Sys.getcwd()) (Sys.argv.(0)) in
    let stdlib_dir = Utils.normalize_path (Filename.dirname abs_ficus_path) "../lib" in
    let _files = ref [] in
    options.include_path <- stdlib_dir :: [];
    try
        Arg.parse
        [("-pr-tokens", (Arg.Unit (fun f -> options.print_tokens <- true)), "   Print all the tokens in parsed files");
        ("-pr-ast", (Arg.Unit (fun f -> options.print_ast <- true)), "   Print typechecked AST of the parsed files");
        ("-pr-orig-k", (Arg.Unit (fun f -> options.print_orig_k <- true)), "   Print generated K-form");
        ("-O0", (Arg.Unit (fun () -> options.optimize_level <- 0)), "   Optimization level 0: disable optimizations except for some essential ones");
        ("-O1", (Arg.Unit (fun () -> options.optimize_level <- 1)), "   Optimization level 1: enable most of optimizations");
        ("-O3", (Arg.Unit (fun () -> options.optimize_level <- 3)), "   Optimization level 3: enable all optimizations");
        ("-o", (Arg.String (fun s -> options.output_name <- s)), "<output_filename>    Output file name");
        ("-I", (Arg.String (fun ipath -> options.include_path <- options.include_path @ [ipath])), "<path>    Add directory to the module search path");
        ]
        (fun s -> _files := !_files @ [s])
        ("Ficus Compiler v0.1\n" ^
        sprintf "usage: %s [options ...] input_file.fx" Sys.argv.(0));

        options.filename <- (match !_files with
            | f :: [] -> f
            | [] -> raise (Arg.Bad "no input file is specified")
            | _ -> raise (Arg.Bad "multiple input files are specified"));
        true
    with Arg.Bad msg -> print_string ("error: " ^ msg ^ "\n"); false
