/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Filename, Sys

type options_t =
{
    app_args: string list = [];
    app_filename: string = "";
    arch64: bool = true;
    build_dir: string = "";
    build_rootdir: string = "";
    cflags: string = "";
    clibs: string = "";
    compile_by_cpp: bool = false;
    filename: string = "";
    gen_c: bool = true;
    include_path: string list = [];
    inline_thresh: int = 100;
    make_app: bool = true;
    optimize_level: int = 1;
    osname: string = "*nix";
    output_name: string = "";
    print_ast: bool = false;
    print_k: bool = false;
    print_tokens: bool = false;
    run_app: bool = false;
    runtime_path: string = "";
    verbose: bool = false
}

fun default_options() = options_t {}
var opt = default_options()

fun print_help() {
    "Ficus Compiler v0.1\n" + sprintf("Usage: %s [options ...] [input_file.fx]", Sys.argv  0))
    Arg.parse(
            [:
                ("-pr-tokens", Arg.Unit(fun (f) {
                                            opt.print_tokens = true
                                   }), "   Print all the tokens in parsed files"),
                ("-pr-ast", Arg.Unit(fun (f) {
                                         opt.print_ast = true
                                }), "   Print typechecked AST of the parsed files"),
                ("-pr-k", Arg.Unit(fun (f) {
                                       opt.print_k = true
                              }), "   Print the generated and optimized K-form"),
                ("-no-c", Arg.Unit(fun (f) {
                                       opt.gen_c = false
                              }), "   Do not generate C code"),
                ("-app", Arg.Unit(fun (f) {
                                      opt.make_app = true
                             }), "   Build application (set by default)"),
                ("-run", Arg.Unit(fun (f) {
                                      opt.run_app = true
                             }), "   Build application and run it"),
                (
                    "-O0",
                    Arg.Unit(fun ({}) {
                                 opt.optimize_level = 0
                        }),
                    "   Optimization level 0: disable optimizations except for the most essential ones"
                    ),
                ("-O1", Arg.Unit(fun ({}) {
                                     opt.optimize_level = 1
                            }), "   Optimization level 1: enable most of optimizations"),
                ("-O3", Arg.Unit(fun ({}) {
                                     opt.optimize_level = 3
                            }), "   Optimization level 3: enable all optimizations"),
                (
                    "-inline-threshold",
                    Arg.Int(fun (i) {
                                opt.inline_thresh = i
                        }),
                    "<n>   Inline threshold (100 by default); the higher it is, the bigger functions are inlined; --inline-thresh=0 disables inline expansion"
                    ),
                ("-o", Arg.String(fun (s) {
                                      opt.output_name = s
                           }), "<output_filename>    Output file name"),
                (
                    "-I",
                    Arg.String(fun (ipath) {
                                   opt.include_path = options.include_path + [: ipath :]
                        }),
                    "<path>    Add directory to the module search path"
                    ),
                (
                    "-B",
                    Arg.String(fun (s) {
                                   opt.build_rootdir = s
                        }),
                    "<build_parent_dir> The parent directory where __build__/appname subdirectory will be created"
                    ),
                ("-c++", Arg.Unit(fun (f) {
                                      opt.compile_by_cpp = true
                             }), "   Use C++ instead of C for compilation"),
                (
                    "-cflags",
                    Arg.String(fun (s) {
                                   opt.cflags = s
                        }),
                    "<cflags>   Pass the specified flags, e.g. \"-mavx2\", to C/C++ compiler (after $FICUS_CFLAGS)"
                    ),
                (
                    "-clibs",
                    Arg.String(fun (s) {
                                   opt.clibs = s
                        }),
                    "<clibs>   Pass the specified libs/linker flags to C/C++ compiler (before $FICUS_LINK_LIBRARIES)"
                    ),
                ("-verbose", Arg.Unit(fun (f) {
                                          opt.verbose = true
                                 }), "  Display various info during build time"),
                (
                    "--",
                    Arg.Rest(fun (s) {
                                 opt.app_args = s :: options.app_args
                        }),
                    "Specify the application parameters (e.g. './ficus -run myprog.fx -- arg1 arg2')"
                    )
                :],
            fun (s) {
                *_files = *_files + [: s :]
            },
            "Ficus Compiler v0.1\n" + sprintf("Usage: %s [options ...] [input_file.fx]", Sys.argv  0))
        if options.optimize_level == 0 {


}

fun parse_options() {
    opt = default_options()
    val is_arch64 : bool = ccode {(bool)(sizeof(void*) > 4)}
    val curr_dir = Sys.getcwd()
    val abs_ficus_path = Filename.normalize(curr_dir, Sys.argv.hd())
    val abs_ficus_dir = Filename.dirname(abs_ficus_path)
    val stdlib_dir = Filename.normalize(abs_ficus_dir, "../lib")
    val _files = ref []
    opt.include_path = stdlib_dir :: []
    opt.runtime_path = Filename.normalize(abs_ficus_dir, "../runtime")
    var args = Sys.argv
    val ok = try {
        while !args.empty() {
            args = match args {
                | "-pr-tokens" :: next =>
                    opt.print_tokens = true; next
                | "-pr-ast" :: next =>
                    opt.print_ast = true; next
                | "-pr-k" :: next =>
                    opt.print_k = true; next
                | "-no-c" :: next =>
                    opt.gen_c = false; next
                | "-app" :: next =>
                    opt.make_app = true; next
                | "-run" :: next =>
                    opt.run_app = true; next
                | "-O0" :: next =>
                    opt.optimize_level = 0; next
                | "-O1" :: next =>
                    opt.optimize_level = 1; next
                | "-O3" :: next =>
                    opt.optimize_level = 3; next
                | "-inline-threshold" :: i :: next =>
                    match i.to_int() {
                        | Some(i) when i >= 0 => opt.inline_thresh = i
                        | _ => throw Fail("invalid inline threshold: must be a non-negative integer")
                    }
            }
        }
    } catch {
        | Fail(msg) => println(f"error: {msg}"); false
        | e => println(f"error: exception {e} occurred when parsing command line parameters"); false
    }
    if !ok {
        print_help(); ok
    } else {




("-no-c", Arg.Unit(fun (f) {
                                       opt.gen_c = false
                              }), "   Do not generate C code"),
                ("-app", Arg.Unit(fun (f) {
                                      opt.make_app = true
                             }), "   Build application (set by default)"),
                ("-run", Arg.Unit(fun (f) {
                                      opt.run_app = true
                             }), "   Build application and run it"),
                (
                    "-O0",
                    Arg.Unit(fun ({}) {
                                 opt.optimize_level = 0
                        }),
                    "   Optimization level 0: disable optimizations except for the most essential ones"
                    ),
                ("-O1", Arg.Unit(fun ({}) {
                                     opt.optimize_level = 1
                            }), "   Optimization level 1: enable most of optimizations"),
                ("-O3", Arg.Unit(fun ({}) {
                                     opt.optimize_level = 3
                            }), "   Optimization level 3: enable all optimizations"),
                (
                    "-inline-threshold",
                    Arg.Int(fun (i) {
                                opt.inline_thresh = i
                        }),
                    "<n>   Inline threshold (100 by default); the higher it is, the bigger functions are inlined; --inline-thresh=0 disables inline expansion"
                    ),
                ("-o", Arg.String(fun (s) {
                                      opt.output_name = s
                           }), "<output_filename>    Output file name"),
                (
                    "-I",
                    Arg.String(fun (ipath) {
                                   opt.include_path = options.include_path + [: ipath :]
                        }),
                    "<path>    Add directory to the module search path"
                    ),
                (
                    "-B",
                    Arg.String(fun (s) {
                                   opt.build_rootdir = s
                        }),
                    "<build_parent_dir> The parent directory where __build__/appname subdirectory will be created"
                    ),
                ("-c++", Arg.Unit(fun (f) {
                                      opt.compile_by_cpp = true
                             }), "   Use C++ instead of C for compilation"),
                (
                    "-cflags",
                    Arg.String(fun (s) {
                                   opt.cflags = s
                        }),
                    "<cflags>   Pass the specified flags, e.g. \"-mavx2\", to C/C++ compiler (after $FICUS_CFLAGS)"
                    ),
                (
                    "-clibs",
                    Arg.String(fun (s) {
                                   opt.clibs = s
                        }),
                    "<clibs>   Pass the specified libs/linker flags to C/C++ compiler (before $FICUS_LINK_LIBRARIES)"
                    ),
                ("-verbose", Arg.Unit(fun (f) {
                                          opt.verbose = true
                                 }), "  Display various info during build time"),
                (
                    "--",


        }
    }
    try {
        Arg.parse(
            [:
                ("-pr-tokens", Arg.Unit(fun (f) {
                                            opt.print_tokens = true
                                   }), "   Print all the tokens in parsed files"),
                ("-pr-ast", Arg.Unit(fun (f) {
                                         opt.print_ast = true
                                }), "   Print typechecked AST of the parsed files"),
                ("-pr-k", Arg.Unit(fun (f) {
                                       opt.print_k = true
                              }), "   Print the generated and optimized K-form"),
                ("-no-c", Arg.Unit(fun (f) {
                                       opt.gen_c = false
                              }), "   Do not generate C code"),
                ("-app", Arg.Unit(fun (f) {
                                      opt.make_app = true
                             }), "   Build application (set by default)"),
                ("-run", Arg.Unit(fun (f) {
                                      opt.run_app = true
                             }), "   Build application and run it"),
                (
                    "-O0",
                    Arg.Unit(fun ({}) {
                                 opt.optimize_level = 0
                        }),
                    "   Optimization level 0: disable optimizations except for the most essential ones"
                    ),
                ("-O1", Arg.Unit(fun ({}) {
                                     opt.optimize_level = 1
                            }), "   Optimization level 1: enable most of optimizations"),
                ("-O3", Arg.Unit(fun ({}) {
                                     opt.optimize_level = 3
                            }), "   Optimization level 3: enable all optimizations"),
                (
                    "-inline-threshold",
                    Arg.Int(fun (i) {
                                opt.inline_thresh = i
                        }),
                    "<n>   Inline threshold (100 by default); the higher it is, the bigger functions are inlined; --inline-thresh=0 disables inline expansion"
                    ),
                ("-o", Arg.String(fun (s) {
                                      opt.output_name = s
                           }), "<output_filename>    Output file name"),
                (
                    "-I",
                    Arg.String(fun (ipath) {
                                   opt.include_path = options.include_path + [: ipath :]
                        }),
                    "<path>    Add directory to the module search path"
                    ),
                (
                    "-B",
                    Arg.String(fun (s) {
                                   opt.build_rootdir = s
                        }),
                    "<build_parent_dir> The parent directory where __build__/appname subdirectory will be created"
                    ),
                ("-c++", Arg.Unit(fun (f) {
                                      opt.compile_by_cpp = true
                             }), "   Use C++ instead of C for compilation"),
                (
                    "-cflags",
                    Arg.String(fun (s) {
                                   opt.cflags = s
                        }),
                    "<cflags>   Pass the specified flags, e.g. \"-mavx2\", to C/C++ compiler (after $FICUS_CFLAGS)"
                    ),
                (
                    "-clibs",
                    Arg.String(fun (s) {
                                   opt.clibs = s
                        }),
                    "<clibs>   Pass the specified libs/linker flags to C/C++ compiler (before $FICUS_LINK_LIBRARIES)"
                    ),
                ("-verbose", Arg.Unit(fun (f) {
                                          opt.verbose = true
                                 }), "  Display various info during build time"),
                (
                    "--",
                    Arg.Rest(fun (s) {
                                 opt.app_args = s :: options.app_args
                        }),
                    "Specify the application parameters (e.g. './ficus -run myprog.fx -- arg1 arg2')"
                    )
                :],
            fun (s) {
                *_files = *_files + [: s :]
            },
            "Ficus Compiler v0.1\n" + sprintf("Usage: %s [options ...] [input_file.fx]", Sys.argv  0))
        if options.optimize_level == 0 {
            opt.inline_thresh = 1

        opt.filename
            =
            match *_files {
            | f :: [] => Utils.normalize_path(curr_dir, f)
            | _ => throw Arg.Bad("there should be exactly one input file")
            }
        if (options.run_app || options.compile_by_cpp) && !options.gen_c {
            throw Arg.Bad("-no-c option cannot be used together with -run or -c++")

        val output_name = Filename.basename(options.filename)
        val output_name = Utils.remove_extension(output_name)
        opt.build_rootdir = Utils.normalize_path(Utils.normalize_path(curr_dir, options.build_rootdir), "__build__")
        opt.build_dir = Utils.normalize_path(options.build_rootdir, output_name)
        opt.app_filename
            =
            if options.output_name != "" {
                Utils.normalize_path(curr_dir, options.output_name)
            } else {
                Utils.normalize_path(options.build_dir, output_name)
            }
        opt.app_args = options.app_args.rev()
        val ch = Unix.open_process_in("uname")
        val uname_output = input_line(ch)
        close_in(ch)
        opt.osname = String.trim(uname_output)
        true
    } catch { | Arg.Bad(msg) => print_string("error: " + msg + "\n")
                                false }
}
