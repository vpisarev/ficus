/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Unit test engine for Ficus

import Sys

exception TestAssertError
exception TestFailure: string

type test_options_t =
{
    filter: string = ""
}

type test_info_t =
{
    name: string;
    f: void->void
}

type test_rng_t =
{
    state: uint64 ref
}

type 't errctx = ('t, string, string, int)

fun test_rng_int(rng: test_rng_t, a: int, b: int)
{
    val s = *rng.state
    val s = (s :> uint32) * 4197999714u64 + (s >> 32)
    val (a, b) = (min(a, b), max(a, b))
    val diff = b - a
    val x = ((s :> uint32) % (diff :> uint32) :> int) + a
    *rng.state = s
    (x :> int)
}

fun test_rng_copy(rng: test_rng_t)
{
    test_rng_t {state = ref(*rng.state)}
}

type test_state_t =
{
    currstatus: bool;
    rng: test_rng_t
}

var g_test_all_registered = ([]: test_info_t list)
val g_test_rng0 = test_rng_t {state=ref(0x123456789u64)}

fun test_init_state() = test_state_t {
    currstatus=true,
    rng=test_rng_copy(g_test_rng0)
}

fun test_init_state_before_test() = test_init_state()

var g_test_state = test_init_state()

fun TEST(name: string, f: void->void)
{
    g_test_all_registered = (test_info_t {name=name, f=f}) :: g_test_all_registered
}

fun test_nomsg(): string = ""
fun test_msg(s: string) = (fun() {s})

fun ASSERT(c: bool, msg: void->string)
{
    if(!c) {
        val m = msg()
        if (m == "") {
            println(f"Assertion failed.")
        } else {
            println(f"Assertion failed: {m}")
        }
        throw TestAssertError
    }
}

fun ASSERT((c, c_str, fname, lineno): (bool, string, string, int))
{
    if(!c) {
        println(f"{fname}:{lineno}: Assertion '{c_str}' failed")
        throw TestAssertError
    }
}

fun ASSERT(c: bool) = ASSERT(c, test_msg(""))

fun test_failed_assert_cmp(a: 't, b: 't, op: string)
{
    print(f"Assertion failed: <Actual> {op} <Expected>.\nActual: ")
    println(a)
    print("Expected: ")
    println(b)
    throw TestAssertError
}

fun test_failed_assert_cmp(a: 't errctx, b: 't errctx, op: string)
{
    val b_str = if b.1 != "" {b.1} else {"<Expected>"}
    print(f"{a.2}:{a.3}: Assertion failed: {a.1} {op} {b_str}.\nActual: ")
    println(a.0)
    print("Expected: ")
    println(b.0)
    throw TestAssertError
}

fun ASSERT_EQ(a: 't, b: 't) = if a == b {} else {test_failed_assert_cmp(a, b, "==")}
fun ASSERT_NE(a: 't, b: 't) = if a != b {} else {test_failed_assert_cmp(a, b, "!=")}
fun ASSERT_LT(a: 't, b: 't) = if a < b {} else {test_failed_assert_cmp(a, b, "<")}
fun ASSERT_LE(a: 't, b: 't) = if a <= b {} else {test_failed_assert_cmp(a, b, "<=")}
fun ASSERT_GT(a: 't, b: 't) = if a > b {} else {test_failed_assert_cmp(a, b, ">")}
fun ASSERT_GE(a: 't, b: 't) = if a >= b {} else {test_failed_assert_cmp(a, b, ">=")}

fun ASSERT_EQ(a: 't errctx, b: 't errctx) = if a.0 == b.0 {} else {test_failed_assert_cmp(a, b, "==")}
fun ASSERT_NE(a: 't errctx, b: 't errctx) = if a.0 != b.0 {} else {test_failed_assert_cmp(a, b, "!=")}
fun ASSERT_LT(a: 't errctx, b: 't errctx) = if a.0 < b.0 {} else {test_failed_assert_cmp(a, b, "<")}
fun ASSERT_LE(a: 't errctx, b: 't errctx) = if a.0 <= b.0 {} else {test_failed_assert_cmp(a, b, "<=")}
fun ASSERT_GT(a: 't errctx, b: 't errctx) = if a.0 > b.0 {} else {test_failed_assert_cmp(a, b, ">")}
fun ASSERT_GE(a: 't errctx, b: 't errctx) = if a.0 >= b.0 {} else {test_failed_assert_cmp(a, b, ">=")}

fun ASSERT_EQ(a: 't errctx, b: 't) = if a.0 == b {} else {test_failed_assert_cmp(a, (b, "", "", 0), "==")}
fun ASSERT_NE(a: 't errctx, b: 't) = if a.0 != b {} else {test_failed_assert_cmp(a, (b, "", "", 0), "!=")}
fun ASSERT_LT(a: 't errctx, b: 't) = if a.0 < b {} else {test_failed_assert_cmp(a, (b, "", "", 0), "<")}
fun ASSERT_LE(a: 't errctx, b: 't) = if a.0 <= b {} else {test_failed_assert_cmp(a, (b, "", "", 0), "<=")}
fun ASSERT_GT(a: 't errctx, b: 't) = if a.0 > b {} else {test_failed_assert_cmp(a, (b, "", "", 0), ">")}
fun ASSERT_GE(a: 't errctx, b: 't) = if a.0 >= b {} else {test_failed_assert_cmp(a, (b, "", "", 0), ">=")}

fun ASSERT_NEAR(a: 't errctx, b: 't errctx, eps: 't) =
    if b - eps <= a <= b + eps {} else {
        println(f"Assertion abs(<Actual> - <Expected>) <= {eps} failed.\nActual: ")
        println(a)
        print("Expected: ")
        println(b)
        throw TestAssertError
    }

fun EXPECT(c: bool, msg: void->string)
{
    if(!c) {
        val m = msg()
        if (m == "") {
            println(f"Error. EXPECT(...) is not satisfied.")
        } else {
            println(f"Error: {m}.")
        }
        g_test_state.currstatus = false
    }
}

fun EXPECT(c: bool) = EXPECT(c, test_nomsg)

fun test_failed_expect_cmp(a: 't, b: 't, op: string)
{
    print(f"Unexpected result of comparison <Actual> {op} <Expected>:\nActual: ")
    println(a)
    print("Expected: ")
    println(b)
    g_test_state.currstatus = false
}

fun test_failed_expect_cmp(a: 't errctx, b: 't errctx, op: string)
{
    val b_str = if b.1 != "" {b.1} else {"<Expected>"}
    print(f"{a.2}:{a.3}: Unexpected result of comparison {a.1} {op} {b_str}.\nActual: ")
    println(a.0)
    print(f"Expected: ")
    println(b.0)
    throw TestAssertError
}

fun test_failed_expect_near(a: 't, b: 't, idx:'idx?, eps: 't)
{
    print(f"Unexpected result of comparison abs(<Actual> - <Expected>) <= {eps}")
    match idx {
    | Some(idx) => print(f" at {idx}")
    | _ => {}
    }
    print(".\nActual: ")
    println(a)
    print(f"Expected: ")
    println(b)
    g_test_state.currstatus = false
}

fun test_failed_expect_near(a: 't errctx, b: 't errctx, idx:'idx?, eps: 't)
{
    val a_str = if a.1 != "" {a.1} else {"<Actual>"}
    val b_str = if b.1 != "" {b.1} else {"<Expected>"}
    print(f"{a.2}:{a.3}: Unexpected result of comparison abs({a_str} - {b_str}) <= {eps}")
    match idx {
    | Some(idx) => print(f" at {idx}")
    | _ => {}
    }
    print(".\nActual: ")
    println(a)
    print(f"Expected: ")
    println(b)
    g_test_state.currstatus = false
}

fun EXPECT_EQ(a: 't, b: 't) = if a == b {} else {test_failed_expect_cmp(a, b, "")}
fun EXPECT_NE(a: 't, b: 't) = if a != b {} else {test_failed_expect_cmp(a, b, "!=")}
fun EXPECT_LT(a: 't, b: 't) = if a < b {} else {test_failed_expect_cmp(a, b, "<")}
fun EXPECT_LE(a: 't, b: 't) = if a <= b {} else {test_failed_expect_cmp(a, b, "<=")}
fun EXPECT_GT(a: 't, b: 't) = if a > b {} else {test_failed_expect_cmp(a, b, ">")}
fun EXPECT_GE(a: 't, b: 't) = if a >= b {} else {test_failed_expect_cmp(a, b, ">=")}

fun EXPECT_EQ(a: 't errctx, b: 't errctx) = if a.0 == b.0 {} else {test_failed_expect_cmp(a, b, "==")}
fun EXPECT_NE(a: 't errctx, b: 't errctx) = if a.0 != b.0 {} else {test_failed_expect_cmp(a, b, "!=")}
fun EXPECT_LT(a: 't errctx, b: 't errctx) = if a.0 < b.0 {} else {test_failed_expect_cmp(a, b, "<")}
fun EXPECT_LE(a: 't errctx, b: 't errctx) = if a.0 <= b.0 {} else {test_failed_expect_cmp(a, b, "<=")}
fun EXPECT_GT(a: 't errctx, b: 't errctx) = if a.0 > b.0 {} else {test_failed_expect_cmp(a, b, ">")}
fun EXPECT_GE(a: 't errctx, b: 't errctx) = if a.0 >= b.0 {} else {test_failed_expect_cmp(a, b, ">=")}

fun EXPECT_EQ(a: 't errctx, b: 't) = if a.0 == b {} else {test_failed_expect_cmp(a, (b, "", "", 0), "==")}
fun EXPECT_NE(a: 't errctx, b: 't) = if a.0 != b {} else {test_failed_expect_cmp(a, (b, "", "", 0), "!=")}
fun EXPECT_LT(a: 't errctx, b: 't) = if a.0 < b {} else {test_failed_expect_cmp(a, (b, "", "", 0), "<")}
fun EXPECT_LE(a: 't errctx, b: 't) = if a.0 <= b {} else {test_failed_expect_cmp(a, (b, "", "", 0), "<=")}
fun EXPECT_GT(a: 't errctx, b: 't) = if a.0 > b {} else {test_failed_expect_cmp(a, (b, "", "", 0), ">")}
fun EXPECT_GE(a: 't errctx, b: 't) = if a.0 >= b {} else {test_failed_expect_cmp(a, (b, "", "", 0), ">=")}

fun EXPECT_NEAR(a: 't, b: 't, eps: 't) =
    if b - eps <= a <= b + eps {} else {test_failed_expect_near(a, b, (None: int?), eps)}

fun EXPECT_NEAR(a: 't errctx, b: 't errctx, eps: 't) =
    if b.0 - eps <= a.0 <= b.0 + eps {} else {test_failed_expect_near(a, b, (None: int?), eps)}

fun EXPECT_NEAR(a: 't errctx, b: 't, eps: 't) =
    if b - eps <= a.0 <= b + eps {} else {test_failed_expect_near(a, (b, "", "", 0), (None: int?), eps)}

fun EXPECT_NEAR(a: 't [+], b: 't [+], eps: 't) =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a, bi <- b {!(bi - eps <= ai <= bi + eps)})
        test_failed_expect_near(ai, bi, Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_NEAR(a: ('t [+], string, string, int), b: ('t [+], string, string, int), eps: 't) =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a.0, bi <- b.0 {!(bi - eps <= ai <= bi + eps)})
        test_failed_expect_near((ai, "", a.2, a.3), (bi, "", "", 0), Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_NEAR(a: 't list, b: 't list, eps: 't) =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a, bi <- b {!(bi - eps <= ai <= bi + eps)})
        test_failed_expect_near(ai, bi, Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_NEAR(a: ('t list, string, string, int), b: 't list, eps: 't) =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a.0, bi <- b {!(bi - eps <= ai <= bi + eps)})
        test_failed_expect_near((ai, "", a.2, a.3), (bi, "", "", 0), Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_THROWS(f: (void->void, string, string, int), ref_exn: exn, ~msg: string="") =
    try {
        f.0()
        val msg = if msg != "" {f": '{msg}'"} else {""}
        println(f"{f.2}:{f.3}: EXPECT_THROWS({f.1}) failed{msg}")
        println("Actual: Does not throw an exception")
        println("Expected: Throws an exception")
        g_test_state.currstatus = false
    } catch {
        | e when e.__tag__ == ref_exn.__tag__ => {}
        | e => println(f"{f.2}:{f.3}: EXPECT_THROWS({f.1}) failed on '{msg}'")
            println(f"Actual: Throws a different exception: '{e}'")
            println(f"Expected: Throws exception '{ref_exn}'")
            g_test_state.currstatus = false
    }

fun EXPECT_THROWS(f: void->void, ref_exn: exn, ~msg: string="") =
    try {
        f()
        val msg = if msg != "" {f": '{msg}'"} else {""}
        println(f"EXPECT_THROWS failed{msg}")
        println("Actual: Does not throw an exception")
        println("Expected: Throws an exception")
        g_test_state.currstatus = false
    } catch {
        | e when e.__tag__ == ref_exn.__tag__ => {}
        | e => println(f"EXPECT_THROWS failed on '{msg}'")
            println(f"Actual: Throws a different exception: '{e}'")
            println(f"Expected: Throws exception '{ref_exn}'")
            g_test_state.currstatus = false
    }

fun EXPECT_NO_THROWS(f: (void->void, string, string, int), ~msg: string="") =
    try {
        f.0()
    } catch {
        | e =>
            val msg = if msg != "" {f": '{msg}'"} else {""}
            println(f"{f.2}:{f.3}: EXPECT_THROWS({f.1}) failed{msg}")
            println(f"EXPECT_NO_THROWS failed: '{msg}'")
            println(f"Actual: Throws the exception '{e}'")
            println("Expected: Does not throw an exception")
            g_test_state.currstatus = false
    }

fun EXPECT_NO_THROWS(f: void->void, ~msg: string="") =
    try {
        f()
    } catch {
        | e =>
            val msg = if msg != "" {f": '{msg}'"} else {""}
            println(f"EXPECT_THROWS failed{msg}")
            println(f"EXPECT_NO_THROWS failed: '{msg}'")
            println(f"Actual: Throws the exception '{e}'")
            println("Expected: Does not throw an exception")
            g_test_state.currstatus = false
    }

fun test_duration2str(ts_diff: double)
{
    val ts_rdiff = round(ts_diff)
    if ts_rdiff < 1 {"<1 ms"} else {string(ts_rdiff) + " ms"}
}

fun test_run_all(opts: test_options_t)
{
    val filter = opts.filter
    val (inverse_test, filter) = if filter.startswith("^") {(true, filter[1:])} else {(false, filter)}
    val (startswithstar, filter) = if filter.startswith("*") {(true, filter[1:])} else {(false, filter)}
    val (endswithstar, filter) = if filter.endswith("*") {(true, filter[:.-1])} else {(false, filter)}
    if filter.find("*") >= 0 {
        throw TestFailure("test filter with '*' inside is currently unsupported")
    }
    val iscolor = Sys.colorterm()
    val (Red, Green, Normal) = if iscolor {("\33[31;1m", "\33[32;1m", "\33[0m")} else {("", "", "")}
    val ts_scale = 1000./Sys.tick_frequency()
    var nexecuted = 0
    val ts0_start = Sys.tick_count()
    val fold failed = ([]: string list) for t <- g_test_all_registered.rev() {
        val name = t.name
        val matches =
            filter == "" ||
            name == filter ||
            (startswithstar && name.endswith(filter)) ||
            (endswithstar && name.startswith(filter)) ||
            (startswithstar && endswithstar && name.find(filter) != -1)
        if (!matches ^ inverse_test) {failed}
        else {
            nexecuted += 1
            println(f"{Green}[ RUN      ]{Normal} {name}")
            g_test_state = test_init_state_before_test()
            val ts_start = Sys.tick_count()
            try {
                t.f()
            } catch {
            | TestAssertError =>
                g_test_state.currstatus = false
            | TestFailure(msg) =>
                println(msg)
                g_test_state.currstatus = false
            | e =>
                println(f"Exception {e} occured.")
                g_test_state.currstatus = false
            }
            val ts_end = Sys.tick_count()
            val ok = g_test_state.currstatus
            val ok_fail = if ok {f"{Green}[       OK ]{Normal}"}
                          else {f"{Red}[     FAIL ]{Normal}"}
            val ts_diff_str = test_duration2str((ts_end - ts_start)*ts_scale)
            println(f"{ok_fail} {name} ({ts_diff_str})\n")
            if ok {failed} else {name :: failed}
        }
    }
    val ts0_end = Sys.tick_count()
    val ts0_diff_str = test_duration2str((ts0_end - ts0_start)*ts_scale)
    val nfailed = failed.length()
    val npassed = nexecuted - nfailed
    println(f"[==========] {nexecuted} test(s) ran ({ts0_diff_str})")
    println(f"{Green}[  PASSED  ]{Normal} {npassed} test(s)")
    if nfailed > 0 {
        println(f"{Red}[  FAILED ]{Normal} {nfailed} test(s):")
        for i <- failed.rev() {println(i)}
    }
}

fun test_print_options(title: string, more_opts: string) {
    if title != "" {
        println(title);
        if !title.endswith("\n") { println() }
    }
    println("The available options are:
-f \"filter\" - specifies glob-like regular expression for the names of tests to run.
            May include '*' in the beginning or in the end.
            '^' in the beginning means that the filter should be inversed.
            For example,
                ./ficus -run ../test/test_all.fx -- -f \"basic.*\"
            compiles and runs all the ficus unit tests which names start with \"basic.\"
-l - list the available tests.
-h or -help or --help - prints this information.")
    if more_opts != "" {println(more_opts)}
}

fun test_parse_options(args: string list, title: string, more_opts: string) {
    var options = test_options_t {}

    fun parse(args: string list) {
        | "-f" :: filter :: rest =>
            options.filter = filter
            parse(rest)
        | "-l" :: _ =>
            for t <- g_test_all_registered.rev() {
                println(t.name)
            }
            (false, options)
        | "-h" :: _ | "-help" :: _ | "--help" :: _ =>
            test_print_options(title, more_opts)
            (false, options)
        | [] => (true, options)
        | o :: _ =>
            println("Error: Invalid option.")
            test_print_options("", more_opts)
            (false, options)
    }
    parse(args)
}
