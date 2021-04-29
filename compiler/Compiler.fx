/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Ficus compiler, the driving part
// (calls all other parts of the compiler in the proper order)

import Filename, File, Sys, Hashmap, LexerUtils as Lxu
import Ast, Ast_pp, Lexer, Parser, Options
import Ast_typecheck
import K_form, K_pp, K_normalize, K_annotate, K_mangle
import K_remove_unused, K_lift_simple, K_flatten, K_tailrec, K_copy_n_skip
import K_cfold_dealias, K_lift, K_fast_idx, K_inline, K_loop_inv, K_fuse_loops
import C_form, C_gen_std, C_gen_code, C_pp
import C_post_rename_locals, C_post_adjust_decls

exception CumulativeParseError

type id_t = Ast.id_t
type kmodule_t = K_form.kmodule_t
val pr_verbose = Ast.pr_verbose

val iscolorterm = Sys.colorterm()

type msgcolor_t = MsgRed | MsgGreen | MsgBlue
fun clrmsg(clr: msgcolor_t, msg: string)
{
    if iscolorterm {
        val esc = match clr {
            | MsgRed => "\33[31;1m"
            | MsgGreen => "\33[32;1m"
            | MsgBlue => "\033[34;1m"
            | _ => ""
        }
        f"{esc}{msg}\33[0m"
    } else {
        msg
    }
}

val error = clrmsg(MsgRed, "error")

fun get_preamble(mfname: string): Lexer.token_t list {
    if Options.opt.use_preamble {
        val bare_name = Filename.remove_extension(Filename.basename(mfname))
        val (preamble, _) = fold (preamble, found) = ([], false)
            for (mname, from_import) <- [: ("Builtins", true), ("Array", false),
                                            ("List", false), ("Vector", false),
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
    val ficus_app_path = Filename.dirname(Filename.normalize(Filename.getcwd(), Sys.argv.hd()))
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
        if Filename.exists(builtins_fx) && Filename.exists(ficus_h) {
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
    val cwd = Filename.getcwd()
    val fname0 = Filename.normalize(cwd, fname0)
    val dir0 = Filename.dirname(fname0)
    val inc_dirs0 = if dir0 == cwd { cwd :: [] } else { dir0 :: cwd :: [] }
    val inc_dirs0 = inc_dirs0 + Options.opt.include_path
    val inc_dirs0 = inc_dirs0 + ficus_path
    val inc_dirs0 = [: for d <- inc_dirs0 { Filename.normalize(cwd, d) } :]
    val name0_id = Ast.get_id(Filename.remove_extension(Filename.basename(fname0)))
    val m_idx = Ast.find_module(name0_id, fname0)
    var queue = m_idx :: []
    var ok = true
    while queue != [] {
        val m_idx = queue.hd()
        queue = queue.tl()
        val minfo = Ast.all_modules[m_idx]
        val mfname = minfo.dm_filename
        if !minfo.dm_parsed {
            try {
                // prevent from repeated parsing
                Ast.all_modules[m_idx].dm_parsed = true
                val dir1 = Filename.dirname(mfname)
                val inc_dirs = (if dir1 == dir0 {[]} else {dir1 :: []}) + inc_dirs0
                val preamble = get_preamble(mfname)
                ok &= Parser.parse(m_idx, preamble, inc_dirs)
                for dep <- Ast.all_modules[m_idx].dm_deps.rev() {
                    val dep_minfo = Ast.get_module(dep)
                    if !dep_minfo.dm_parsed {
                        queue = dep :: queue
                    }
                }
            }
            catch {
            | Lxu.LexerError((l, c), msg) =>
                println(f"{mfname}:{l}:{c}: error: {msg}\n"); ok = false
            | Parser.ParseError(loc, msg) =>
                println(f"{loc}: error: {msg}\n"); ok = false
            | e => println(f"{mfname}: exception {e} occured"); ok = false
            }
        }
    }
    ok
}

type dep_graph_t = (int, int list) list

fun toposort(graph: dep_graph_t): int list
{
    //print("before toposort: ")
    //println([: for (i, _) <- graph {(Ast.pp(Ast.get_module_name(i)), i)} :])
    val graph = [for (_, deps) <- graph {deps}], nvtx = size(graph)
    val processed = array(nvtx, false)
    var result: int list = []

    fun dfs(i: int, visited: int list) {
        val deps = graph[i]
        if visited.mem(i) {
            val vlist = ", ".join([: for j <- visited { Ast.pp(Ast.get_module_name(j)) } :])
            throw Fail(f"error: cyclib dependency between the modules: {vlist}")
        }
        val visited = i :: visited
        for j <- deps {
            if processed[j] {continue}
            dfs(j, visited)
        }
        result = i :: result
        processed[i] = true
    }

    for i <- 0:nvtx {
        if processed[i] { continue }
        dfs(i, [])
    }

    result.rev()
}

fun typecheck_all(modules: int list): bool
{
    Ast.all_compile_errs = []
    for m <- modules {Ast_typecheck.check_mod(m)}
    Ast.all_compile_errs == []
}

fun k_normalize_all(modules: int list): (kmodule_t list, bool)
{
    Ast.all_compile_errs = []
    K_form.init_all_idks()
    val kmods = K_normalize.normalize_all_modules(modules)
    (kmods, Ast.all_compile_errs == [])
}

fun k_skip_some(kmods: kmodule_t list)
{
    val skip_flags = array(size(Ast.all_modules), false)
    val build_root_dir = Options.opt.build_rootdir
    val ok = Sys.mkdir(build_root_dir, 0755)
    val build_dir = Options.opt.build_dir
    var ok = ok && Sys.mkdir(build_dir, 0755)
    val obj_ext = if Sys.win32 {".obj"} else {".o"}

    val kmods = [: for km <- kmods {
        val {km_idx, km_cname, km_top, km_deps, km_pragmas} = km
        val is_cpp = Options.opt.compile_by_cpp || km_pragmas.pragma_cpp
        val ext = if is_cpp { ".cpp" } else { ".c" }
        val mname = K_mangle.mangle_mname(km_cname)
        val cname = Filename.normalize(build_dir, mname)
        val k_filename = cname + ".k"
        val c_filename = cname + ext
        val o_filename = cname + obj_ext

        val new_kform = K_pp.pp_top_to_string(km_top)
        val have_k = Filename.exists(k_filename)
        val have_c = Filename.exists(c_filename)
        val have_o = Filename.exists(o_filename)
        val have_all = have_k & have_c & have_o

        val old_kform =
            if Options.opt.force_rebuild || !have_all {""}
            else {
                try
                    File.read_utf8(k_filename)
                catch {
                | IOError | FileOpenError => ""
                }
            }
        val (ok_j, same_kform, status_j) =
            if new_kform == old_kform {
                (true, true, "")
            } else {
                val well_written =
                    try {
                        File.write_utf8(k_filename, new_kform)
                        true
                    }
                    catch {
                    | IOError | FileOpenError => false
                    }
                (well_written, false,
                if well_written {""} else {clrmsg(MsgRed, "failed to write .k")})
            }
        ok = ok & ok_j
        if !same_kform {
            if have_c { Sys.remove(c_filename) }
            if have_o { Sys.remove(o_filename) }
        }
        // [TODO] with properly constructed K-form dump format it should be
        // not necessary to check the dependencies. Types of the dependencies from
        // other modules (basically, their API) could be included into the dump.
        val skip_module = same_kform && all(for d <- km_deps {skip_flags[d]})
        val status_j = if status_j != "" {status_j} else if skip_module {"skip"} else {clrmsg(MsgBlue, "process")}
        pr_verbose(f"K {km_cname}: {status_j}")
        if skip_module {
            for e <- km_top {
                | K_form.KDefFun kf when kf->kf_flags.fun_flag_ctor == Ast.CtorNone =>
                    val {kf_flags, kf_rt, kf_loc} = *kf
                    *kf = kf->{
                        kf_flags=kf_flags.{
                            fun_flag_ccode=true,
                            fun_flag_inline=false,
                            },
                        kf_body=K_form.KExpCCode("", (kf_rt, kf_loc)),
                        }
                | _ => {}
            }
        }
        skip_flags[km_idx] = skip_module
        km.{km_skip=skip_module}
    } :]

    if !ok {throw Fail("failed to write some k-forms")}
    kmods
}

fun prf(str: string) = pr_verbose(f"\t{str}")

fun k_optimize_all(kmods: kmodule_t list): (kmodule_t list, bool) {
    Ast.all_compile_errs = []
    val niters = Options.opt.optim_iters
    var temp_kmods = kmods
    prf("remove unused")
    temp_kmods = K_remove_unused.remove_unused(temp_kmods, true)
    prf("annotate types")
    temp_kmods = K_annotate.annotate_types(temp_kmods)
    prf("copy generic/inline functions")
    temp_kmods = K_copy_n_skip.copy_some(temp_kmods)
    prf("remove unused by main")
    temp_kmods = K_remove_unused.remove_unused_by_main(temp_kmods)
    prf("mangle & dump intermediate K-forms")
    temp_kmods = K_mangle.mangle_all(temp_kmods, false)
    temp_kmods = K_mangle.mangle_locals(temp_kmods)
    temp_kmods = k_skip_some(temp_kmods)
    prf("demangle")
    temp_kmods = K_mangle.demangle_all(temp_kmods)
    for i <- 1: niters+1 {
        pr_verbose(f"Optimization pass #{i}:")
        if i <= 2 {
            prf("simple lambda lifting")
            temp_kmods = K_lift_simple.lift(temp_kmods)
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
    temp_kmods = K_mangle.mangle_all(temp_kmods, true)
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
            val omp_flag = ""//if enable_openmp {" /openmp"} else {""}
            val opt_flags =
                if opt_level == 0 {
                    " /MTd /Od /GF"
                } else {
                    " /MT " + (if opt_level == 1 {"/O1"} else {"/O2"})
                }
            val cflags = f"/nologo{opt_flags}{omp_flag} /I{runtime_include_path}"
            ("win", "cl", "cl", ".obj", "/c /Fo", "/Fe", "", cflags, "kernel32.lib advapi32.lib")
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
                    ("macos_arm64", omp_cflags,
                        " " + omp_cflags + omp_lib
                    )
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
            (os, c_comp, cpp_comp, ".o", "-c -o ", "-o ", "-l", cflags, clibs)
        }

    val custom_cflags = Sys.getenv("FICUS_CFLAGS")
    val custom_cflags = if Options.opt.cflags == "" { custom_cflags }
                        else { Options.opt.cflags + " " + custom_cflags }
    val cflags = cflags + " " + custom_cflags
    pr_verbose(clrmsg(MsgBlue, f"Compiling .c/.cpp files with cflags={cflags}"))
    val results = [| @parallel for
        {cmod_cname, cmod_ccode, cmod_skip, cmod_pragmas={pragma_cpp, pragma_clibs}} <- array(cmods) {
        val output_fname = Filename.basename(cmod_cname)
        val is_cpp = Options.opt.compile_by_cpp || pragma_cpp
        val (comp, ext) = if is_cpp { (cpp_comp, ".cpp") } else { (c_comp, ".c") }
        val output_fname = output_fname + ext
        val output_fname = Filename.normalize(build_dir, output_fname)
        val (ok_j, reprocess, status_j) =
            if cmod_skip { (true, false, "skipped") }
            else {
                val str_new = C_pp.pprint_top_to_string(cmod_ccode)
                val str_old = if Options.opt.force_rebuild {""} else {
                    try
                        File.read_utf8(output_fname)
                    catch {
                    | IOError | FileOpenError => ""
                    }
                }
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
            }
        val cname = Filename.normalize(build_dir, cmod_cname)
        val c_filename = cname + ext
        val obj_filename = cname + obj_ext
        val (ok_j, recompiled, status_j) =
            if ok_j && (reprocess || !Filename.exists(obj_filename)) {
                val cmd = f"{comp} {cflags} {obj_opt}{obj_filename} {c_filename}"
                val result = Sys.command(cmd) == 0
                val status = if result {clrmsg(MsgGreen, "ok")} else {clrmsg(MsgRed, "fail")}
                (result, true, status)
            } else {
                (ok_j, false, status_j)
            }
        pr_verbose(f"CC {c_filename}: {status_j}")
        val clibs = [: for (l, _) <- pragma_clibs { l } :].rev()
        (is_cpp, recompiled, clibs, ok_j, obj_filename)
    } |]

    val fold (any_cpp, any_recompiled, all_clibs, ok, objs) = (false, false, [], ok, [])
        for (is_cpp, is_recompiled, clibs_j, ok_j, obj) <- results {
            (any_cpp | is_cpp, any_recompiled | is_recompiled, clibs_j + all_clibs, ok & ok_j, obj :: objs)
        }
    if ok && !any_recompiled && Filename.exists(Options.opt.app_filename) {
        pr_verbose(f"{Options.opt.app_filename} is up-to-date\n")
        ok
    } else if !ok {
        ok
    } else {
        val custom_clibs = Sys.getenv("FICUS_LINK_LIBRARIES")
        val custom_clibs =
            if Options.opt.clibs == "" { custom_clibs }
            else { custom_clibs + " " + Options.opt.clibs }
        val custom_clibs =
            if all_clibs == [] { custom_clibs }
            else {
                custom_clibs + " " +
                " ".join([: for l <- all_clibs.rev() {link_lib_opt + l} :])
            }
        val clibs = clibs + " " + custom_clibs
        pr_verbose(f"Linking the app with flags={clibs}")
        val cmd = (if any_cpp {cpp_comp} else {c_comp}) + " " + appname_opt + Options.opt.app_filename
        val cmd = cmd + " " + " ".join(objs) + " " + clibs
        //pr_verbose(f"{cmd}\n")
        val ok = Sys.command(cmd) == 0
        ok
    }
}

fun run_app(): bool
{
    val appname = Options.opt.app_filename
    val appname = Filename.normalize(Filename.getcwd(), appname)
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
        val graph = [: for minfo <- Ast.all_modules {
                        (minfo.dm_idx, minfo.dm_deps)
                    } :]
        Ast.all_modules_sorted = toposort(graph).tl().tl()
        if Options.opt.print_ast0 {
            for m <- Ast.all_modules_sorted {
                val minfo = Ast.get_module(m)
                Ast_pp.pprint_mod(minfo)
            }
        }
        val modules_used = ", ".join([: for m_idx <- Ast.all_modules_sorted { Ast.pp(Ast.get_module_name(m_idx)) } :])
        val parsing_complete = clrmsg(MsgBlue, "Parsing complete")
        pr_verbose(f"{parsing_complete}. Modules used: {modules_used}")
        val ok = typecheck_all(Ast.all_modules_sorted)
        if ok {
            pr_verbose(clrmsg(MsgBlue, "Type checking complete"))
            if Options.opt.print_ast {
                for m <- Ast.all_modules_sorted {
                    val minfo = Ast.get_module(m)
                    Ast_pp.pprint_mod(minfo)
                }
            }
        }
        val (kmods, ok) = if ok { k_normalize_all(Ast.all_modules_sorted) } else { ([], false) }
        if ok {
            pr_verbose(clrmsg(MsgBlue, "K-normalization complete"))
            if Options.opt.print_k0 { K_pp.pp_kmods(kmods) }
        }
        val (kmods, ok) = if ok {
            pr_verbose(clrmsg(MsgBlue, "K-form optimization started"))
            k_optimize_all(kmods)
        } else { ([], false) }
        if ok {
            pr_verbose(clrmsg(MsgBlue, "K-form optimization complete"))
            if Options.opt.print_k { K_pp.pp_kmods(kmods) }
        }
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
