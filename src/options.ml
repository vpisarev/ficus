(* Various compile-time options *)

type compiler_options_t =
{
    arch64: bool;
    include_path: string list;
    optimize_level: int;
    output_name: string;
    print_tokens: bool;
    print_ast: bool;

    filename: string
}

let options = ref
{
    arch64 = max_int > 1073741823;
    include_path = [];
    optimize_level = 1;
    output_name = "";
    print_tokens = false;
    print_ast = false;
    filename = ""
}

let parse_options () =
    let inc_path = !options.include_path in
    let abs_ficus_path = Utils.normalize_path (Sys.getcwd()) (Sys.argv.(0)) in
    let stdlib_dir = Utils.normalize_path (Filename.dirname abs_ficus_path) "../lib" in
    let inc_path = stdlib_dir :: inc_path in
    let _arch64 = ref !options.arch64 in
    let _include_path = ref inc_path in
    let _optimize_level = ref !options.optimize_level in
    let _output_name = ref !options.output_name in
    let _print_tokens = ref !options.print_tokens in
    let _print_ast = ref !options.print_ast in
    let _files = ref [] in
    try
        Arg.parse
        [("-pr-tokens", (Arg.Set _print_tokens), "   Print all the tokens in parsed files");
        ("-pr-ast", (Arg.Set _print_ast), "   Print AST of the parsed files");
        ("-O0", (Arg.Unit (fun () -> _optimize_level := 0)), "   Optimization level 0: disable optimizations except for some essential ones");
        ("-O1", (Arg.Unit (fun () -> _optimize_level := 1)), "   Optimization level 1: enable most of optimizations");
        ("-O3", (Arg.Unit (fun () -> _optimize_level := 3)), "   Optimization level 3: enable all optimizations");
        ("-o", (Arg.Set_string _output_name), "<output_filename>    Output file name");
        ("-I", (Arg.String (fun ipath -> _include_path := !_include_path @ [ipath])), "<path>    Add directory to the module search path");
        ("-64", (Arg.Unit (fun () -> _arch64 := true)), "   Generate 64-bit C source/binary (by default, it depends on the current arch)");
        ("-32", (Arg.Unit (fun () -> _arch64 := false)), "   Generate 32-bit C source/binary");
        ]
        (fun s -> _files := !_files @ [s])
        ("Ficus Compiler v0.1\n" ^
        Printf.sprintf "usage: %s [options ...] input_file.fx" Sys.argv.(0));

        options :=
        {
            arch64 = !_arch64;
            include_path = !_include_path;
            optimize_level = !_optimize_level;
            output_name = !_output_name;
            print_tokens = !_print_tokens;
            print_ast = !_print_ast;

            filename = (match !_files with
                        | f :: [] -> f
                        | [] -> raise (Arg.Bad "no input file is specified")
                        | _ -> raise (Arg.Bad "multiple input files are specified"))
        };
        true
    with Arg.Bad msg -> print_string ("error: " ^ msg ^ "\n"); false
