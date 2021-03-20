/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Ficus compiler, the driving part
// (calls all other parts of the compiler in the proper order)

import Filename, Sys, Map
import Ast, AstPP, Lexer, Parser, Options
import AstTypeChecker
import KForm, KPP, KNormalize, KAnnotate, KMangle
import KRemoveUnused, KLiftSimple, KFlatten, KTailRec

exception CumulativeParseError

type id_t = Ast.id_t
type kmodule_t = KForm.kmodule_t
val pr_verbose = Ast.pr_verbose

fun get_preamble(mfname: string): Lexer.token_t list {
    val bare_name = Filename.remove_extension(Filename.basename(mfname))
    val fold (preamble, found) = ([], false) for (mname, from_import) <- [: ("Builtins", true), ("List", false), ("Char", false), ("String", false) :] {
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
    val preamble =
        if bare_name != "Builtins" { preamble }
        else {
            // [TODO] insert proper git hash
            [: //Lexer.IMPORT(true), Lexer.IDENT(true, "Config"), Lexer.SEMICOLON,
            Lexer.VAL, Lexer.IDENT(true, "__ficus_git_commit__"), Lexer.EQUAL,
            Lexer.LITERAL(Ast.LitString("123456789")), Lexer.SEMICOLON :] + preamble
        }
    preamble
}

fun parse_all(fname0: string): bool
{
    val cwd = Sys.getcwd()
    val fname0 = Filename.normalize(cwd, fname0)
    val dir0 = Filename.dirname(fname0)
    val inc_dirs0 = if dir0 == cwd { cwd :: [] } else { dir0 :: cwd :: [] }
    val inc_dirs0 = inc_dirs0 + Options.opt.include_path
    val inc_dirs0 = [: for d <- inc_dirs0 { Filename.normalize(cwd, d) } :]
    val name0_id = Ast.get_id(Filename.remove_extension(Filename.basename(fname0)))
    val minfo = Ast.find_module(name0_id, fname0)
    var queue = minfo->dm_name :: []
    var ok = true
    var module_idx = 0
    while !queue.empty() {
        val mname = List.hd(queue)
        queue = List.tl(queue)
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
    for m <- modules {AstTypeChecker.check_mod(m)}
    Ast.all_compile_errs.empty()
}

fun k_normalize_all(modules: id_t list): (kmodule_t list, bool)
{
    Ast.all_compile_errs = []
    KForm.init_all_idks()
    val kmods = KNormalize.normalize_all_modules(modules)
    (kmods, Ast.all_compile_errs.empty())
}

fun prf(str: string) = pr_verbose(f"\t{str}")

fun k_optimize_all(kmods: kmodule_t list): (kmodule_t list, bool) {
    var compile_errs = []
    val niters = Options.opt.optim_iters
    var temp_kmods = kmods
    prf("initial dead code elim")
    temp_kmods = KRemoveUnused.remove_unused(temp_kmods, true)
    for i <- 1: niters+1 {
        pr_verbose(f"Optimization pass #{i}:")
        if i <= 2 {
            prf("simple lifting")
            temp_kmods = KLiftSimple.lift(temp_kmods)
            prf("annotate types")
            temp_kmods = KAnnotate.annotate_types(temp_kmods)
        }
        prf("tailrec")
        temp_kmods = KTailRec.tailrec2loops_all(temp_kmods)
        //prf("loop inv")
        //temp_kmods = KLoopInv.move_loop_invs_all(temp_kmods)
        //prf("inline")
        //if Options.opt.inline_thresh > 0 {
        //    temp_kmods = K_inline.inline_some(temp_kmods)
        //}
        prf("flatten")
        temp_kmods = KFlatten.flatten_all(temp_kmods)
        //prf("fuse loops")
        //temp_kmods = KFuseLoops.fuse_loops_all(temp_kmods)
        //prf("fast idx")
        //temp_kmods = KFastIdx.optimize_idx_checks_all(temp_kmods)
        //prf("const folding")
        //temp_kmods = KConstFoldDealias.cfold_dealias(temp_kmods)
        prf("dead code elim")
        temp_kmods = KRemoveUnused.remove_unused(temp_kmods, false)
    }
    pr_verbose("Finalizing K-form:")
    //prf("lambda lifting")
    //temp_kmods = KLift.lift_all(temp_kmods)
    prf("flatten")
    temp_kmods = KFlatten.flatten_all(temp_kmods)
    prf("dead code elim")
    temp_kmods = KRemoveUnused.remove_unused(temp_kmods, false)
    prf("mangle")
    temp_kmods = KMangle.mangle_all(temp_kmods)
    prf("dead code elim")
    temp_kmods = KRemoveUnused.remove_unused(temp_kmods, false)
    //prf("mark recursive")
    //temp_kmods = KInline.find_recursive_funcs_all(temp_kmods)
    prf("annotate types")
    temp_kmods = KAnnotate.annotate_types(temp_kmods)
    (temp_kmods, compile_errs.empty())
}

/*
fun k2c_all(kmods) {
    pr_verbose("Generating C code:")
    *compile_errs = []
    C_form.init_all_idcs()
    C_gen_std.init_std_names()
    pr_verbose("\tstd calls initialized")
    val cmods = C_gen_code.gen_ccode_all(kmods)
    pr_verbose("C code generated")
    val cmods = C_post_rename_locals.rename_locals(cmods)
    pr_verbose("\tlocal variables renamed")
    val cmods =
    [: for cmod <- cmods {
        val is_cpp = options.compile_by_cpp || cmod.cmod_pragmas.pragma_cpp
        if is_cpp {
            C_post_adjust_decls.adjust_decls_(cmod)
        } else {
            cmod
        }
    } :]
    pr_verbose("\tConversion to C-form complete")
    (cmods, *compile_errs == [])
}
fun emit_c_files(fname0, cmods) {
    val build_root_dir = options.build_rootdir
    try {
        Unix.mkdir(build_root_dir, 0, o755)
    } catch { | Unix.Unix_error(Unix.EEXIST, _, _) => {} }
    val build_dir = options.build_dir
    try {
        Unix.mkdir(build_dir, 0, o755)
    } catch { | Unix.Unix_error(Unix.EEXIST, _, _) => {} }
    val fold (new_cmods, ok) = ([], true) for cmod <- cmods {
        val {cmod_cname, cmod_ccode, cmod_pragmas={pragma_cpp}} = cmod
        val output_fname = Filename.basename(cmod_cname)
        val is_cpp = options.compile_by_cpp || pragma_cpp
        val ext = if is_cpp {
                      ".cpp"
        } else {
            ".c"
        }
        val output_fname = output_fname + ext
        val output_fname = Utils.normalize_path(build_dir, output_fname)
        val (new_cmod, ok) =
        if ok {
            val str_new = C_pp.pprint_top_to_string(cmod_ccode)
            val str_old = Utils.file2str(output_fname)
            val (recompile, ok) = if str_new == str_old {
                                      (false, ok)
            } else {
                (true, Utils.str2file(str_new, output_fname))
            }
            (cmod.{cmod_recompile=recompile}, ok)
        } else {
            (cmod, ok)
        }
        (new_cmod :: new_cmods, ok)
    }
    pr_verbose("C files are written")
    (new_cmods.rev(), build_dir, ok)
}

fun run_cc(cmods, output_dir) {
    val opt_level = options.optimize_level
    val c_comp = "cc"
    val cpp_comp = "c++ -std=c++11"
    val cmd = " -Wno-unknown-warning-option"
    val cmd = cmd + " -Wno-dangling-else"
    val ggdb_opt = if opt_level == 0 { " -ggdb" } else { "" }
    val cmd = cmd + f" -O{opt_level}{ggbd_opt}"
    val cmd = cmd + " -I" + options.runtime_path
    val custom_cflags = " " + Sys.getenv("FICUS_CFLAGS")
    val custom_cflags = if options.cflags == "" { custom_cflags }
                        else { " " + options.cflags + custom_cflags }
    val cmd = cmd + custom_cflags
    val fold (any_cpp, any_recompiled, all_clibs, ok, objs) = (false, false, [], true, []) for
        {cmod_cname, cmod_recompile, cmod_pragmas={pragma_cpp, pragma_clibs}} <- cmods {
        val cname = Utils.normalize_path(output_dir, cmod_cname)
        val is_cpp = options.compile_by_cpp || pragma_cpp
        val (comp, ext) = if is_cpp { (cpp_comp, ".cpp") } else { (c_comp, ".c") }
        val c_filename = cname + ext
        val obj_filename = cname + ".o"
        print_if_verbose(f"CC {c_filename}:\n")
        val cmd = comp + cmd + " -o " + obj_filename
        val cmd = cmd + " -c " + c_filename
        val (ok_j, recompiled) =
        if cmod_recompile || !Sys.file_exists(obj_filename) {
            print_if_verbose(f"\t{cmd}\n")
            (Sys.command(cmd) == 0, true)
        } else {
            print_if_verbose(f"\t{obj_filename} is up-to-date\n")
            (true, false)
        }
        val clibs = [: for (l, _) <- pragma_clibs { l } :].rev()
        (any_cpp || is_cpp, any_recompiled || recompiled, clibs + all_clibs, ok && ok_j, obj_filename :: objs)
    }
    if ok && !any_recompiled && Sys.file_exists(options.app_filename) {
        print_if_verbose(f"{options.app_filename} is up-to-date\n")
        ok
    } else if !ok {
        ok
    } else {
        val cmd = c_comp + " -o " + options.app_filename
        val cmd = cmd + " " + " ".join(objs)
        val linked_libs = Sys.getenv("FICUS_LINK_LIBRARIES")
        val linked_libs = if options.clibs == "" { linked_libs }
                          else { linked_libs + " " + options.clibs }
        val linked_libs = if all_clibs == [] { linked_libs }
                          else { linked_libs + " " + " ".join([: for l <- all_clibs.rev() {"-l" + l} :]) }
        val cmd = cmd + linked_libs
        val cmd = cmd + " -lm" + (if any_cpp { " -lstdc++" } else { "" })
        print_if_verbose(f"{cmd}\n")
        val ok = Sys.command(cmd) == 0
        ok
    }
}

fun run_app(): bool
{
    val cmd = " ".join(options.app_filename :: options.app_args)
    Sys.command(cmd) == 0
}*/

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
        val ok = parse_all(fname0)
        if !ok { throw CumulativeParseError }
        val graph = [: for (mfname, m) <- Ast.all_modules.list() {
                            val minfo = Ast.get_module(m)
                            (minfo->dm_idx, m, minfo->dm_deps)
                    } :]
        Ast.all_modules_sorted = toposort(graph)
        if Options.opt.print_ast0 {
            for m <- Ast.all_modules_sorted {
                val minfo = Ast.get_module(m)
                AstPP.pprint_mod(minfo)
            }
        }
        val modules_used = ", ".join(Ast.all_modules_sorted.map(Ast.pp))
        pr_verbose(f"Parsing complete. Modules used: {modules_used}")
        val ok = typecheck_all(Ast.all_modules_sorted)
        pr_verbose("Type checking complete")
        if ok && Options.opt.print_ast {
            for m <- Ast.all_modules_sorted {
                val minfo = Ast.get_module(m)
                AstPP.pprint_mod(minfo)
            }
        }
        val (kmods, ok) = if ok { k_normalize_all(Ast.all_modules_sorted) } else { ([], false) }
        pr_verbose("K-normalization complete")
        if ok && Options.opt.print_k0 { KPP.pp_kmods(kmods) }
        pr_verbose("K-form optimization started")
        val (kmods, ok) = if ok { k_optimize_all(kmods) } else { ([], false) }
        if ok { pr_verbose("K-form optimization complete") }
        if ok && Options.opt.print_k { KPP.pp_kmods(kmods) }
        /*if !options.gen_c { ok } else {
            val (cmods, ok) = if ok { k2c_all(kmods) } else { ([], false) }
            val (cmods, builddir, ok) = if ok { emit_c_files(fname0, cmods) } else { (cmods, ".", ok) }
            val ok = if ok && (options.make_app || options.run_app) { run_compiler(cmods, builddir) } else { ok }
            val ok = if ok && options.run_app { run_app() } else { ok }
            ok
        }*/
        if !ok { print_all_compile_errs() }
        ok
    } catch {
    | e =>
        print_all_compile_errs()
        match e {
        | Fail(msg) => println(f"Failure: {msg}")
        | Ast.CompileError(loc, msg) as e => Ast.print_compile_err(e)
        | CumulativeParseError => {}
        | _ => println(f"\n\nException {e} occured")
        }
        false
    }
}
