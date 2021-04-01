/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Ficus compiler, the driving part
// (calls all other parts of the compiler in the proper order)

import Filename, File, Sys, Hashmap
import Ast, Ast_pp, Lexer, Parser, Options
import Ast_typecheck
import K_form, K_pp, K_normalize, K_annotate, K_mangle
import K_remove_unused, K_lift_simple, K_flatten, K_tailrec
import K_cfold_dealias, K_lift, K_fast_idx, K_inline, K_loop_inv, K_fuse_loops
import C_form, C_gen_std, C_gen_code, C_pp
import C_post_rename_locals, C_post_adjust_decls

exception CumulativeParseError

type id_t = Ast.id_t
type kmodule_t = K_form.kmodule_t
val pr_verbose = Ast.pr_verbose

type msgcolor_t = MsgRed | MsgGreen | MsgBlue
fun clrmsg(clr: msgcolor_t, msg: string)
{
    val esc = match clr {
        | MsgRed => "\33[31;1m"
        | MsgGreen => "\33[32;1m"
        | MsgBlue => "\033[34;1m"
        | _ => ""
    }
    f"{esc}{msg}\33[0m"
}
val error = clrmsg(MsgRed, "error")

fun get_preamble(mfname: string): Lexer.token_t list {
    if Options.opt.use_preamble {
        val bare_name = Filename.remove_extension(Filename.basename(mfname))
        val (preamble, _) = fold (preamble, found) = ([], false)
            for (mname, from_import) <- [: ("Builtins", true), ("List", false),
                                            ("Char", false), ("String", false),
                                            ("Math", true) :] {
            if found {
                (preamble, found)
            } else if bare_name == mname {
                (preamble, true)
            } else if from_import {
                (preamble + [: Lexer.FROM, Lexer.IDENT(true, mname), Lexer.IMPORT(false), Lexer.STAR(true), Lexer.SEMICOLON :], false)
            } else {
                (preamble + [: Lexer.IMPORT(true), Lexer.IDENT(true, mname), Lexer.SEMICOLON :], false)
            }
        }
        preamble
    } else { [] }
}

fun find_ficus_dirs(): (string, string list)
{
    var ficus_path = Sys.getpath("FICUS_PATH")
    // if 'ficus' is '<ficus_root>/bin/ficus'
    val ficus_app_path = Filename.dirname(Filename.normalize(Sys.getcwd(), Sys.argv.hd()))
    // if 'ficus' is '<ficus_root>/__fxbuild__/fx/fx'
    val ficus_pp_path = Filename.dirname(Filename.dirname(ficus_app_path))
    // if 'ficus' is '{/usr|/usr/local|/opt}/bin/ficus'
    val ficus_inst_path = Filename.normalize(Filename.dirname(ficus_app_path),
                            f"lib/ficus-{__ficus_major__}.{__ficus_minor__}")
    val std_ficus_path = [: Filename.normalize(Filename.dirname(ficus_app_path), "lib"),
                            Filename.normalize(ficus_pp_path, "lib"),
                            Filename.normalize(ficus_inst_path, "lib") :]
    val std_ficus_path_len = std_ficus_path.length()
    val search_path = std_ficus_path + ficus_path
    var found = ""
    for d@i <- search_path {
        val builtins_fx = Filename.normalize(d, "Builtins.fx")
        val ficus_h = Filename.normalize(d, "../runtime/ficus/ficus.h")
        if Sys.file_exists(builtins_fx) && Sys.file_exists(ficus_h) {
            found = Filename.dirname(d)
            if i < std_ficus_path_len {
                // unless Builtins.fx is already found in FICUS_PATH,
                // we add it to the end of FICUS_PATH
                ficus_path = ficus_path + (d::[])
            }
            break
        }
    }
    (found, ficus_path)
}

fun parse_all(fname0: string, ficus_path: string list): bool
{
    val cwd = Sys.getcwd()
    val fname0 = Filename.normalize(cwd, fname0)
    val dir0 = Filename.dirname(fname0)
    val inc_dirs0 = if dir0 == cwd { cwd :: [] } else { dir0 :: cwd :: [] }
    val inc_dirs0 = inc_dirs0 + Options.opt.include_path
    val inc_dirs0 = inc_dirs0 + ficus_path
    val inc_dirs0 = [: for d <- inc_dirs0 { Filename.normalize(cwd, d) } :]
    val name0_id = Ast.get_id(Filename.remove_extension(Filename.basename(fname0)))
    val minfo = Ast.find_module(name0_id, fname0)
    var queue = minfo->dm_name :: []
    var ok = true
    var module_idx = 0
    while queue != [] {
        val mname = queue.hd()
        queue = queue.tl()
        val minfo = Ast.get_module(mname)
        val mfname = minfo->dm_filename
        if !minfo->dm_parsed {
            try {
                minfo->dm_idx = module_idx
                module_idx += 1
                val dir1 = Filename.dirname(mfname)
                val inc_dirs = (if dir1 == dir0 {[]} else {dir1 :: []}) + inc_dirs0
                val preamble = get_preamble(mfname)
                ok &= Parser.parse(minfo, preamble, inc_dirs)
                for dep <- minfo->dm_deps.rev() {
                    val dep_minfo = Ast.get_module(dep)
                    if !dep_minfo->dm_parsed {
                        queue = dep_minfo->dm_name :: queue
                    }
                }
            }
            catch {
            | Lexer.LexerError((l, c), msg) =>
                println(f"{mfname}:{l}:{c}: error: {msg}\n"); ok = false
            | Parser.ParseError(loc, msg) =>
                println(f"{loc}: error: {msg}\n"); ok = false
            | e => println(f"{mfname}: exception {e} occured"); ok = false
            }
        }
    }
    ok
}

type dep_graph_t = (int, id_t, id_t list) list

fun toposort(graph: dep_graph_t): id_t list
{
    val graph = graph.sort(fun ((i, _, _), (j, _, _)) { i < j })
    fun loop(remaining: dep_graph_t, result: id_t list): id_t list
    {
        fun find_next(analyzed: dep_graph_t, rest: dep_graph_t): (id_t, dep_graph_t) =
            match rest {
            | (i, m, deps) :: rest =>
                if all(for d <- deps {result.mem(d)}) {
                    (m, analyzed.rev() + rest)
                } else {
                    find_next((i, m, deps) :: analyzed, rest)
                }
            | _ =>
                val cycle = ", ".join([: for (_, m, _) <- analyzed.rev() {Ast.pp(m)} :])
                throw Fail(f"error: cylic module dependency between {cycle}")
            }
        match remaining {
        | [] => result.rev()
        | _ =>
            val (next_m, remaining) = find_next([], remaining)
            loop(remaining, next_m :: result)
        }
    }
    loop(graph, [])
}

fun typecheck_all(modules: id_t list): bool
{
    Ast.all_compile_errs = []
    for m <- modules {Ast_typecheck.check_mod(m)}
    Ast.all_compile_errs == []
}

fun k_normalize_all(modules: id_t list): (kmodule_t list, bool)
{
    Ast.all_compile_errs = []
    K_form.init_all_idks()
    val kmods = K_normalize.normalize_all_modules(modules)
    (kmods, Ast.all_compile_errs == [])
}

fun prf(str: string) = pr_verbose(f"\t{str}")

fun k_optimize_all(kmods: kmodule_t list): (kmodule_t list, bool) {
    Ast.all_compile_errs = []
    val niters = Options.opt.optim_iters
    var temp_kmods = kmods
    prf("initial unused code removal")
    temp_kmods = K_remove_unused.remove_unused(temp_kmods, true)
    for i <- 1: niters+1 {
        pr_verbose(f"Optimization pass #{i}:")
        if i <= 2 {
            prf("simple lifting")
            temp_kmods = K_lift_simple.lift(temp_kmods)
            prf("annotate types")
            temp_kmods = K_annotate.annotate_types(temp_kmods)
        }
        prf("tailrec")
        temp_kmods = K_tailrec.tailrec2loops_all(temp_kmods)
        prf("loop inv")
        temp_kmods = K_loop_inv.move_loop_invs_all(temp_kmods)
        prf("inline")
        if Options.opt.inline_thresh > 0 {
            temp_kmods = K_inline.inline_some(temp_kmods)
        }
        prf("flatten")
        temp_kmods = K_flatten.flatten_all(temp_kmods)
        prf("fuse loops")
        temp_kmods = K_fuse_loops.fuse_loops_all(temp_kmods)
        prf("fast idx")
        temp_kmods = K_fast_idx.optimize_idx_checks_all(temp_kmods)
        prf("const folding")
        temp_kmods = K_cfold_dealias.cfold_dealias(temp_kmods)
        prf("remove unused")
        temp_kmods = K_remove_unused.remove_unused(temp_kmods, false)
    }
    pr_verbose("Finalizing K-form:")
    prf("lambda lifting")
    temp_kmods = K_lift.lift_all(temp_kmods)
    prf("flatten")
    temp_kmods = K_flatten.flatten_all(temp_kmods)
    prf("remove unused")
    temp_kmods = K_remove_unused.remove_unused(temp_kmods, false)
    prf("mangle")
    temp_kmods = K_mangle.mangle_all(temp_kmods)
    prf("remove unused")
    temp_kmods = K_remove_unused.remove_unused(temp_kmods, false)
    prf("mark recursive")
    temp_kmods = K_inline.find_recursive_funcs_all(temp_kmods)
    prf("annotate types")
    temp_kmods = K_annotate.annotate_types(temp_kmods)
    (temp_kmods, Ast.all_compile_errs == [])
}

fun k2c_all(kmods: kmodule_t list)
{
    pr_verbose(clrmsg(MsgBlue, "Generating C code"))
    Ast.all_compile_errs = []
    C_form.init_all_idcs()
    C_gen_std.init_std_names()
    val cmods = C_gen_code.gen_ccode_all(kmods)
    pr_verbose(clrmsg(MsgBlue, "C code generated"))
    val cmods = C_post_rename_locals.rename_locals(cmods)
    val cmods = [: for cmod <- cmods {
        val is_cpp = Options.opt.compile_by_cpp || cmod.cmod_pragmas.pragma_cpp
        if is_cpp { C_post_adjust_decls.adjust_decls(cmod) }
        else { cmod }
        } :]
    pr_verbose("\tConversion to C-form complete")
    (cmods, Ast.all_compile_errs == [])
}

// [TODO] add proper support for Windows
fun run_cc(cmods: C_form.cmodule_t list, ficus_root: string) {
    val osinfo = Sys.osname(true)
    val opt_level = Options.opt.optimize_level
    val enable_openmp = Options.opt.enable_openmp
    val runtime_include_path = Filename.normalize(ficus_root, "runtime")
    val runtime_lib_path = Filename.normalize(ficus_root, "runtime/lib")
    val build_root_dir = Options.opt.build_rootdir
    val ok = Sys.mkdir(build_root_dir, 0755)
    val build_dir = Options.opt.build_dir
    val ok = ok && Sys.mkdir(build_dir, 0755)

    val (_, c_comp, cpp_comp, obj_ext, obj_opt, appname_opt, link_lib_opt, cflags, clibs) =
        if Sys.win32 {
            val omp_flag = if enable_openmp {" /openmp"} else {""}
            val opt_flags =
                if opt_level == 0 {
                    " /Od /MTd /GF"
                } else {
                    " /MT" + (if opt_level == 1 {"/O1"} else {"/O2"})
                }
            val cflags = f"/nologo{opt_flags}{omp_flag} /I {runtime_include_path}"
            ("win", "cl", "cl", ".obj", "/c /Fo", "/Fe", "", cflags, "")
        } else {
            // unix or hopefully something more or less compatible with it
            val (os, libpath, cflags, clibs) =
            if osinfo.contains("Darwin") {
                val (omp_cflags, omp_lib) =
                    if enable_openmp { ("-Xclang -fopenmp", " -lomp") }
                    else { ("", "") }
                val (libpath, cflags, clibs) =
                if osinfo.contains("x86_64") {
                    ("macos_x64", omp_cflags,
                        " " + omp_cflags + omp_lib
                    )
                } else if osinfo.contains ("arm64") {
                    ("", "", "")
                } else {
                    ("", "", "")
                }
                ("macos", libpath, cflags, clibs)
            } else if osinfo.contains("Linux") {
                val omp_flags = if enable_openmp {" -fopenmp"} else {""}
                ("linux", "", omp_flags, omp_flags)
            } else if Sys.unix {
                ("unix", "", "", "")
            } else {
                ("", "", "", "")
            }
            val c_comp = "cc"
            val cpp_comp = "c++ -std=c++11"
            val common_cflags = "-Wno-unknown-warning-option -Wno-dangling-else -Wno-static-in-inline"
            val ggdb_opt = if opt_level == 0 { " -ggdb" } else { "" }
            val cflags = f"-O{opt_level}{ggdb_opt} {cflags} {common_cflags} -I{runtime_include_path}"
            val clibs = (if libpath!="" {f"-L{runtime_lib_path}/{libpath} "} else {""}) + f"-lm {clibs}"
            (os, c_comp, cpp_comp, ".o", "-c -o", "-o", "-l", cflags, clibs)
        }

    val custom_cflags = Sys.getenv("FICUS_CFLAGS")
    val custom_cflags = if Options.opt.cflags == "" { custom_cflags }
                        else { Options.opt.cflags + " " + custom_cflags }
    val cflags = cflags + " " + custom_cflags
    pr_verbose(clrmsg(MsgBlue, f"Compiling .c/.cpp files with cflags={cflags}"))
    val results = [| @parallel for
        {cmod_cname, cmod_ccode, cmod_pragmas={pragma_cpp, pragma_clibs}} <- array(cmods) {
        val output_fname = Filename.basename(cmod_cname)
        val is_cpp = Options.opt.compile_by_cpp || pragma_cpp
        val ext = if is_cpp { ".cpp" } else { ".c" }
        val output_fname = output_fname + ext
        val output_fname = Filename.normalize(build_dir, output_fname)
        val str_new = C_pp.pprint_top_to_string(cmod_ccode)
        val str_old = if Options.opt.force_rebuild {""} else {
            try
                File.read_utf8(output_fname)
            catch {
            | IOError | FileOpenError => ""
            }
        }
        val (ok_j, recompile, skipped_status) =
            if str_new == str_old {
                (ok, false, "skipped")
            } else {
                val well_written =
                    try {
                        File.write_utf8(output_fname, str_new)
                        true
                    }
                    catch {
                    | IOError | FileOpenError => false
                    }
                (well_written, well_written,
                if well_written {""} else {clrmsg(MsgRed, "failed to write .c")})
            }
        val cname = Filename.normalize(build_dir, cmod_cname)
        val is_cpp = Options.opt.compile_by_cpp || pragma_cpp
        val (comp, ext) = if is_cpp { (cpp_comp, ".cpp") } else { (c_comp, ".c") }
        val c_filename = cname + ext
        val obj_filename = cname + obj_ext
        val (ok_j, recompiled, status_j) =
            if ok_j && (recompile || !Sys.file_exists(obj_filename)) {
                val cmd = f"{comp} {cflags} {obj_opt} {obj_filename} {c_filename}"
                val result = Sys.command(cmd) == 0
                val status = if result {clrmsg(MsgGreen, "ok")} else {clrmsg(MsgRed, "fail")}
                (result, true, status)
            } else {
                (ok_j, false, skipped_status)
            }
        pr_verbose(f"CC {c_filename}: {status_j}")
        val clibs = [: for (l, _) <- pragma_clibs { l } :].rev()
        (is_cpp, recompiled, clibs, ok_j, obj_filename)
    } |]

    val fold (any_cpp, any_recompiled, all_clibs, ok, objs) = (false, false, [], ok, [])
        for (is_cpp, is_recompiled, clibs_j, ok_j, obj) <- results {
            (any_cpp | is_cpp, any_recompiled | is_recompiled, clibs_j + all_clibs, ok & ok_j, obj :: objs)
        }
    if ok && !any_recompiled && Sys.file_exists(Options.opt.app_filename) {
        pr_verbose(f"{Options.opt.app_filename} is up-to-date\n")
        ok
    } else if !ok {
        ok
    } else {
        val custom_clibs = Sys.getenv("FICUS_LINK_LIBRARIES")
        val custom_clibs = if Options.opt.clibs == "" { custom_clibs }
                      else { custom_clibs + " " + Options.opt.clibs }
        val custom_clibs =
            if all_clibs == [] { custom_clibs }
            else {
                custom_clibs + " " +
                " ".join([: for l <- all_clibs.rev() {link_lib_opt + l} :])
            }
        val clibs = clibs + " " + custom_clibs
        pr_verbose(f"Linking the app with flags={clibs}")
        val cmd = (if any_cpp {cpp_comp} else {c_comp}) + " " + appname_opt + " " + Options.opt.app_filename
        val cmd = cmd + " " + " ".join(objs) + " " + clibs
        //pr_verbose(f"{cmd}\n")
        val ok = Sys.command(cmd) == 0
        ok
    }
}

fun run_app(): bool
{
    val appname = Options.opt.app_filename
    val appname = Filename.normalize(Sys.getcwd(), appname)
    val cmd = " ".join(appname :: Options.opt.app_args)
    Sys.command(cmd) == 0
}

fun print_all_compile_errs()
{
    val nerrs = Ast.all_compile_errs.length()
    if nerrs != 0 {
        Ast.all_compile_errs.rev().app(Ast.print_compile_err)
        println(f"\n{nerrs} errors occured during type checking.")
    }
}

fun process_all(fname0: string): bool {
    Ast.init_all()
    try {
        val (ficus_root, ficus_path) = find_ficus_dirs()
        if ficus_root == "" { throw Fail(
f"Ficus root directory is not found.
Please, add the directory 'lib' containing Builtins.fx to
'FICUS_PATH' environment variable or make sure that either
1. 'ficus' executable is put in a directory <ficus_root>/bin
and there are <ficus_root>/runtime and <ficus_root>/lib.
2. or 'ficus' executable is in (/usr|/usr/local|/opt|...)/bin and
   there are (/usr|...)/lib/ficus-{__ficus_major__}.{__ficus_minor__}/{{runtime, lib}}") }
        val ok = parse_all(fname0, ficus_path)
        if !ok { throw CumulativeParseError }
        val graph = [: for (mfname, m) <- Ast.all_modules.list() {
                        val minfo = Ast.get_module(m)
                        (minfo->dm_idx, m, minfo->dm_deps)
                    } :]
        Ast.all_modules_sorted = toposort(graph)
        if Options.opt.print_ast0 {
            for m <- Ast.all_modules_sorted {
                val minfo = Ast.get_module(m)
                Ast_pp.pprint_mod(minfo)
            }
        }
        val modules_used = ", ".join(Ast.all_modules_sorted.map(Ast.pp))
        val parsing_complete = clrmsg(MsgBlue, "Parsing complete")
        pr_verbose(f"{parsing_complete}. Modules used: {modules_used}")
        val ok = typecheck_all(Ast.all_modules_sorted)
        pr_verbose(clrmsg(MsgBlue, "Type checking complete"))
        if ok && Options.opt.print_ast {
            for m <- Ast.all_modules_sorted {
                val minfo = Ast.get_module(m)
                Ast_pp.pprint_mod(minfo)
            }
        }
        val (kmods, ok) = if ok { k_normalize_all(Ast.all_modules_sorted) } else { ([], false) }
        pr_verbose(clrmsg(MsgBlue, "K-normalization complete"))
        if ok && Options.opt.print_k0 { K_pp.pp_kmods(kmods) }
        pr_verbose(clrmsg(MsgBlue, "K-form optimization started"))
        val (kmods, ok) = if ok { k_optimize_all(kmods) } else { ([], false) }
        if ok { pr_verbose(clrmsg(MsgBlue, "K-form optimization complete")) }
        if ok && Options.opt.print_k { K_pp.pp_kmods(kmods) }
        val ok = if !Options.opt.gen_c { ok } else {
            val (cmods, ok) = if ok { k2c_all(kmods) } else { ([], false) }
            val ok =
                if ok && (Options.opt.make_app || Options.opt.run_app) {
                    run_cc(cmods, ficus_root)
                } else { ok }
            val ok = if ok && Options.opt.run_app { run_app() } else { ok }
            ok
        }
        if !ok { print_all_compile_errs() }
        ok
    } catch {
    | e =>
        print_all_compile_errs()
        match e {
        | Fail(msg) => println(f"{error}: {msg}")
        | Ast.CompileError(loc, msg) as e => Ast.print_compile_err(e)
        | CumulativeParseError => {}
        | _ => println(f"\n\n{error}: Exception {e} occured")
        }
        false
    }
}
