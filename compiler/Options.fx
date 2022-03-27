/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Filename, Sys

type optval_t = OptBool: bool | OptInt: int | OptString: string

type options_t =
{
    app_args: string list = [];
    app_filename: string = "";
    arch64: bool = true;
    force_rebuild: bool = false;
    build_dir: string = "";
    build_rootdir: string = "";
    cflags: string = "";
    clibs: string = "";
    compile_by_cpp: bool = false;
    filename: string = "";
    gen_c: bool = true;
    include_path: string list = [];
    debug: bool = false;
    defines: (string, optval_t) list = [];
    optim_iters: int = 0;
    inline_thresh: int = 100;
    enable_openmp: bool = true;
    relax: bool = false;
    use_preamble: bool = true;
    make_app: bool = true;
    optimize_level: int = 1;
    output_name: string = "";
    print_ast0: bool = false;
    print_ast: bool = false;
    print_k0: bool = false;
    print_k: bool = false;
    print_tokens: bool = false;
    run_app: bool = false;
    verbose: bool = false;
    W_unused: bool = true
}

fun default_options() = options_t {}
var opt = default_options()

fun print_help(detailed: bool) {
    val fxname = Filename.basename(Sys.argv.hd())
    println(f"Ficus compiler v{__ficus_version_str__} (git {__ficus_git_commit__})")

    if !detailed {
        println(f"
Usage: {fxname} [-pr-tokens | -pr-ast0 | -pr-ast | -pr-k0 | -pr-k | -no-c
    | -app | -run | -O0 | -O1 | -O3 | -inline-threshold <n> | -no-openmp
    | -o <output_name> | -I <incdir> | -B <build_root>
    | -c++ | -cflags <cflags> | -clibs <clibs>
    | -verbose | -h | -v ] <input_file>.fx [-- <app_args ...>]

Run '{fxname} -h' to get more detailed help")
    } else {
        println(f"
Usage: {fxname} [options ...] <input_file.fx> [-- <app_args ...>]

where options can be some of:
    -rebuild        Ignore cached files; rebuild everything from scratch
    -pr-tokens      Print all the tokens in parsed files
    -pr-ast0        Print AST right after parsing
    -pr-ast         Print typechecked AST of the parsed files
    -pr-k0          Print just generated K-form
    -pr-k           Print optimized K-form of the parsed files
                    (only a part of the generated K-form is retained
                    because of the dead code elimination step)
    -no-c           Do not generate C code
    -app            Build application (default mode)
    -run            Build application and run it
    -O0             Optimization level 0: disable all optimizations
                                         except for the most essential ones
    -O1             Optimization level 1 (default): enable most of the optimizations
    -O3             Optimization level 3: enable all optimizations
    -Ofast          Optimization level 3; passes '-Ofast' to C/C++ compiler
    -no-openmp      Disable OpenMP (OpenMP is enabled by default)
    -debug          Turn on debug information, disable optimizations
                    (but it can be overwritten with further -On)
    -optim-iters    The number of optimization iterations to perform (2 or 3 by default, depending on -O<n>)
    -inline-threshold  Inline threshold (100 by default); the higher it is,
                    the bigger functions are inlined;
                    --inline-thresh=0 disables inline expansion
    -relax          Do not require explicit typing of all global functions' parameters
    -no-preamble    Do not auto-import 'Builtins', 'List', 'String' and
                    a few other standard modules into each compiled module.
    -Wno-unused     Do not report errors about unused values/functions
    -o <output_name> Output file name (by default it matches the
                    input filename without .fx extension)
    -D symbol       Define 'symbol=true' for preprocessor
    -D symbol=value Define 'symbol=value' for preprocessor.
                    The value may be one of the following:
                      * a boolean: true (or on), false (or off),
                      * an integer (if the value starts with a digit, '+' or '-' and
                                    contains 1 or more decimal digits)
                      * text string: everything between double quotes or
                                     any combination of characters that does not
                                     contain spaces, '\' or quotes and does not start with a digit or '-'
    -I <dir>        Add specified directory to the module search path
    -B <build_root> Specifies the parent directory <build_root> where subdirectory
                    <build_root>/__fxbuild__/<app_build_dir> with the generated files will be created.
                    By default, <build_root> is the current working directory.
    -c++            Use C++ compiler instead of C to compile the generated sources.
                    'pragma \"c++\"' in .fx file also instructs ficus compiler to use C++.
    -cflags <cflags> Pass the specified flags, e.g. \"-mavx2\", to C/C++ compiler.
                    If environment variable FICUS_CFLAGS is set,
                    its value is inserted before <cflags>
    -clibs <clibs>  Pass the specified libs/linker flags to C/C++ compiler.
                    If environment variable FICUS_LINK_LIBRARIES is set,
                    its value is inserted after <clibs>
    -verbose        Display various info during the build
    -h or -help or --help  Display this information
    -v or -version  Display information about compiler and the platform, then exit.
    --              Specify the application parameters when '-run' flag is used,
                    e.g. './ficus -run myprog.fx -- arg1 arg2'
")}
}

val is_arch64 : bool = @ccode {(bool)(sizeof(void*) > 4)}

fun parse_options(): bool {
    val error = "\33[31;1merror:\33[0m"
    opt = default_options()
    opt.arch64 = is_arch64
    val curr_dir = Filename.getcwd()
    var args = Sys.argv.tl()
    var inputfile = ""
    var prhelp = 0
    var prver = false
    var ok = true
    while args != [] {
        args = match args {
            | "-no-preamble" :: next =>
                opt.use_preamble = false; next
            | "-rebuild" :: next =>
                opt.force_rebuild = true; next
            | "-pr-tokens" :: next =>
                opt.print_tokens = true; next
            | "-pr-ast0" :: next =>
                opt.print_ast0 = true; next
            | "-pr-ast" :: next =>
                opt.print_ast = true; next
            | "-pr-k0" :: next =>
                opt.print_k0 = true; next
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
            | "-Ofast" :: next =>
                opt.optimize_level = 100; next
            | "-no-openmp" :: next =>
                opt.enable_openmp = false; next
            | "-debug" :: next =>
                opt.debug = true; next
            | "-optim-iters" :: i :: next =>
                val i = i.to_int_or(-1)
                if i >= 0 {
                    opt.optim_iters = i; next
                } else {
                    println(f"{error} invalid -optim-iters argument; must be a non-negative integer")
                    ok = false; []
                }
            | "-inline-threshold" :: i :: next =>
                val i = i.to_int_or(-1)
                if i >= 0 {
                    opt.optim_iters = i; next
                } else {
                    println(f"{error} invalid -inline-threshold argument; must be a non-negative integer")
                    ok = false; []
                }
            | "-relax" :: next =>
                opt.relax = true; next
            | "-Wno-unused" :: next =>
                opt.W_unused = false; next
            | "-verbose" :: next =>
                opt.verbose = true; next
            | "-o" :: oname :: next =>
                opt.output_name = oname; next
            | "-D" :: nameval :: next =>
                val p1 = nameval.find('=')
                val (name, value) = if p1 < 0 {(nameval, "true")}
                                    else {(nameval[:p1], nameval[p1+1:])}
                val errval = OptBool(false)
                val value =
                    if name != "" && (name[0].isalpha() || name[0]=='_') &&
                        all(for c <- name {c.isalnum() || c == '_'}) {
                        if value == "TRUE" || value == "true" || value == "ON" || value == "on" {OptBool(true)}
                        else if value == "FALSE" || value == "false" || value == "OFF" || value == "off" {OptBool(false)}
                        else if value == "" {
                            println(f"a value should follow after '{name}='")
                            ok = false
                            errval
                        } else if value[0] == '+' || value[0] == '-' || value[0].isdigit() {
                            match value.to_int() {
                            | Some(x) => OptInt(x)
                            | _ =>
                                println(f"invalid numerical value '{value}' of a symbol '{name}'; \
                                          if you meant a string, enclose it in double quotes")
                                ok = false
                                errval
                            }
                        } else if value.startswith('\"') {
                            if !value.endswith('\"') {
                                println(f"the value {value} starts with '\"', but does not terminate with '\"'")
                                ok = false
                                errval
                            } else {
                                OptString(value[1:-1])
                            }
                        } else {
                            OptString(value)
                        }
                    } else {
                        println(f"identifier '{name}' contains incorrect characters")
                        ok = false; errval
                    }
                if ok { opt.defines = (name, value) :: opt.defines; next }
                else { [] }
            | "-I" :: incdir :: next =>
                opt.include_path = opt.include_path + ([:: incdir]); next
            | "-B" :: bdir :: next =>
                opt.build_rootdir = bdir; next
            | "-c++" :: next =>
                opt.compile_by_cpp = true; next
            | "-cflags" :: cflags :: next =>
                opt.cflags = if opt.cflags == "" {cflags} else {opt.cflags + " " + cflags}
                next
            | "-clibs" :: clibs :: next =>
                opt.clibs = if opt.clibs == "" {clibs} else {opt.clibs + " " + clibs}
                next
            | "-h" :: _ | "-help" :: _ | "--help" :: _ =>
                prhelp = 2; []
            | "-v" :: _ | "-version" :: _ | "--version" :: _ =>
                prver = true; []
            | "--" :: next =>
                opt.app_args = next; []
            | a :: next =>
                if a.startswith("-") {
                    if [::"-inline-threshold", "-o", "-B", "-cflags", "-clibs"].mem(a) {
                        println(f"{error} option {a} needs an argument")
                    } else {
                        println(f"{error} unrecognized option {a}")
                    }
                    ok = false; []
                } else if inputfile == "" {
                    inputfile = a; next
                } else {
                    println(f"{error} more than one input file is specified: {[:: inputfile, a]}")
                    ok = false; []
                }
        }
    }

    if opt.optim_iters <= 0 {
        opt.optim_iters = if opt.optimize_level >= 3 {3} else {2}
    }
    opt.optim_iters = max(opt.optim_iters, 2)

    if !prver && prhelp == 0 && ok {
        if inputfile == "" {
            if Sys.argv.tl() != [] {
                println(f"{error} input file name is missing")
                ok = false
            } else {
                prhelp = 1
            }
        }
        if (opt.run_app || opt.compile_by_cpp) && !opt.gen_c {
            println(f"{error} -no-c option cannot be used together with -run or -c++")
            ok = false
        }
    }
    if prver {
        println(f"Ficus version: {__ficus_version_str__} (git commit: {__ficus_git_commit__})")
        println(f"Platform: {Sys.osname(true)}")
        println(f"C/C++ Compiler: {Sys.cc_version()}")
        false
    } else if prhelp > 0 {
        print_help(prhelp > 1)
        false
    } else if ok {
        if opt.optimize_level == 0 {
            opt.inline_thresh = 1
        }
        opt.filename = Filename.normalize(curr_dir, inputfile)
        val default_output_name = Filename.basename(opt.filename)
        val default_output_name = Filename.remove_extension(default_output_name)
        opt.build_rootdir = Filename.normalize(curr_dir, opt.build_rootdir)
        opt.build_rootdir = Filename.normalize(opt.build_rootdir, "__fxbuild__")
        opt.app_filename = if opt.output_name != "" { opt.output_name } else { default_output_name }
        opt.build_dir = Filename.normalize(opt.build_rootdir, Filename.basename(opt.app_filename))
        if opt.output_name == "" { opt.app_filename = Filename.normalize(opt.build_dir, opt.app_filename) }
        true
    } else { false }
}
