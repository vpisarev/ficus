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
    state: ref[uint64]
}

type errctx[T] = (T, string, string, int)

fun test_rng_int(rng: test_rng_t, a: int, b: int): int
{
    val s = *rng.state
    val s = (s :> uint32) * 4197999714u64 + (s >> 32)
    val (a, b) = (min(a, b), max(a, b))
    val diff = b - a
    val x = ((s :> uint32) % (diff :> uint32) :> int) + a
    *rng.state = s
    (x :> int)
}

fun test_rng_copy(rng: test_rng_t): UTest.test_rng_t
{
    test_rng_t {state = ref(*rng.state)}
}

type test_state_t =
{
    currstatus: bool;
    rng: test_rng_t
}

var g_test_all_registered = ([]: list[test_info_t])
val g_test_rng0 = test_rng_t {state=ref(0x123456789u64)}

fun test_init_state(): UTest.test_state_t = test_state_t {
    currstatus=true,
    rng=test_rng_copy(g_test_rng0)
}

fun test_init_state_before_test(): UTest.test_state_t = test_init_state()

var g_test_state = test_init_state()

fun TEST(name: string, f: void->void): void
{
    g_test_all_registered = (test_info_t {name=name, f=f}) :: g_test_all_registered
}

fun test_nomsg(): string = ""
fun test_msg(s: string): (void -> string) = (fun() {s})

fun ASSERT(c: bool, msg: void->string): void
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

fun ASSERT((c, c_str, fname, lineno): (bool, string, string, int)): void
{
    if(!c) {
        println(f"{fname}:{lineno}: Assertion '{c_str}' failed")
        throw TestAssertError
    }
}

fun ASSERT(c: bool): void = ASSERT(c, test_msg(""))

fun test_failed_assert_cmp[T](a: T, b: T, op: string): void
{
    print(f"Assertion failed: <Actual> {op} <Expected>.\nActual: ")
    println(a)
    print("Expected: ")
    println(b)
    throw TestAssertError
}

fun test_failed_assert_cmp[T](a: errctx[T], b: errctx[T], op: string): void
{
    val b_str = if b.1 != "" {b.1} else {"<Expected>"}
    print(f"{a.2}:{a.3}: Assertion failed: {a.1} {op} {b_str}.\nActual: ")
    println(a.0)
    print("Expected: ")
    println(b.0)
    throw TestAssertError
}

fun ASSERT_EQ[T](a: T, b: T): void = if a == b {} else {test_failed_assert_cmp(a, b, "==")}
fun ASSERT_NE[T](a: T, b: T): void = if a != b {} else {test_failed_assert_cmp(a, b, "!=")}
fun ASSERT_LT[T](a: T, b: T): void = if a < b {} else {test_failed_assert_cmp(a, b, "<")}
fun ASSERT_LE[T](a: T, b: T): void = if a <= b {} else {test_failed_assert_cmp(a, b, "<=")}
fun ASSERT_GT[T](a: T, b: T): void = if a > b {} else {test_failed_assert_cmp(a, b, ">")}
fun ASSERT_GE[T](a: T, b: T): void = if a >= b {} else {test_failed_assert_cmp(a, b, ">=")}

fun ASSERT_EQ[T](a: errctx[T], b: errctx[T]): void = if a.0 == b.0 {} else {test_failed_assert_cmp(a, b, "==")}
fun ASSERT_NE[T](a: errctx[T], b: errctx[T]): void = if a.0 != b.0 {} else {test_failed_assert_cmp(a, b, "!=")}
fun ASSERT_LT[T](a: errctx[T], b: errctx[T]): void = if a.0 < b.0 {} else {test_failed_assert_cmp(a, b, "<")}
fun ASSERT_LE[T](a: errctx[T], b: errctx[T]): void = if a.0 <= b.0 {} else {test_failed_assert_cmp(a, b, "<=")}
fun ASSERT_GT[T](a: errctx[T], b: errctx[T]): void = if a.0 > b.0 {} else {test_failed_assert_cmp(a, b, ">")}
fun ASSERT_GE[T](a: errctx[T], b: errctx[T]): void = if a.0 >= b.0 {} else {test_failed_assert_cmp(a, b, ">=")}

fun ASSERT_EQ[T](a: errctx[T], b: T): void = if a.0 == b {} else {test_failed_assert_cmp(a, (b, "", "", 0), "==")}
fun ASSERT_NE[T](a: errctx[T], b: T): void = if a.0 != b {} else {test_failed_assert_cmp(a, (b, "", "", 0), "!=")}
fun ASSERT_LT[T](a: errctx[T], b: T): void = if a.0 < b {} else {test_failed_assert_cmp(a, (b, "", "", 0), "<")}
fun ASSERT_LE[T](a: errctx[T], b: T): void = if a.0 <= b {} else {test_failed_assert_cmp(a, (b, "", "", 0), "<=")}
fun ASSERT_GT[T](a: errctx[T], b: T): void = if a.0 > b {} else {test_failed_assert_cmp(a, (b, "", "", 0), ">")}
fun ASSERT_GE[T](a: errctx[T], b: T): void = if a.0 >= b {} else {test_failed_assert_cmp(a, (b, "", "", 0), ">=")}

fun ASSERT_NEAR[T](a: errctx[T], b: errctx[T], eps: T): void =
    if b - eps <= a <= b + eps {} else {
        println(f"Assertion abs(<Actual> - <Expected>) <= {eps} failed.\nActual: ")
        println(a)
        print("Expected: ")
        println(b)
        throw TestAssertError
    }

fun EXPECT(c: bool, msg: void->string): void
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

fun EXPECT(c: bool): void = EXPECT(c, test_nomsg)

fun test_failed_expect_cmp[T](a: T, b: T, op: string): void
{
    print(f"Unexpected result of comparison <Actual> {op} <Expected>:\nActual: ")
    println(a)
    print("Expected: ")
    println(b)
    g_test_state.currstatus = false
}

fun test_failed_expect_cmp[T](a: errctx[T], b: errctx[T], op: string): void
{
    val b_str = if b.1 != "" {b.1} else {"<Expected>"}
    print(f"{a.2}:{a.3}: Unexpected result of comparison {a.1} {op} {b_str}.\nActual: ")
    println(a.0)
    print(f"Expected: ")
    println(b.0)
    throw TestAssertError
}

fun test_failed_expect_near[T, Idx](a: T, b: T, idx: Idx?, eps: T): void
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

fun test_failed_expect_near[T, Idx](a: (T...), b: (T...), idx: Idx?, eps: T): void
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

fun test_failed_expect_near[T, Idx](a: errctx[T], b: errctx[T], idx: Idx?, eps: T): void
{
    val a_str = if a.1 != "" {a.1} else {"<Actual>"}
    val b_str = if b.1 != "" {b.1} else {"<Expected>"}
    print(f"{a.2}:{a.3}: Unexpected result of comparison abs({a_str} - {b_str}) <= {eps}")
    match idx {
    | Some(idx) => print(f" at {idx}")
    | _ => {}
    }
    print(".\nActual: ")
    println(a.0)
    print(f"Expected: ")
    println(b.0)
    g_test_state.currstatus = false
}

fun EXPECT_EQ[T](a: T, b: T): void = if a == b {} else {test_failed_expect_cmp(a, b, "")}
fun EXPECT_NE[T](a: T, b: T): void = if a != b {} else {test_failed_expect_cmp(a, b, "!=")}
fun EXPECT_LT[T](a: T, b: T): void = if a < b {} else {test_failed_expect_cmp(a, b, "<")}
fun EXPECT_LE[T](a: T, b: T): void = if a <= b {} else {test_failed_expect_cmp(a, b, "<=")}
fun EXPECT_GT[T](a: T, b: T): void = if a > b {} else {test_failed_expect_cmp(a, b, ">")}
fun EXPECT_GE[T](a: T, b: T): void = if a >= b {} else {test_failed_expect_cmp(a, b, ">=")}

fun EXPECT_EQ[T](a: errctx[T], b: errctx[T]): void = if a.0 == b.0 {} else {test_failed_expect_cmp(a, b, "==")}
fun EXPECT_NE[T](a: errctx[T], b: errctx[T]): void = if a.0 != b.0 {} else {test_failed_expect_cmp(a, b, "!=")}
fun EXPECT_LT[T](a: errctx[T], b: errctx[T]): void = if a.0 < b.0 {} else {test_failed_expect_cmp(a, b, "<")}
fun EXPECT_LE[T](a: errctx[T], b: errctx[T]): void = if a.0 <= b.0 {} else {test_failed_expect_cmp(a, b, "<=")}
fun EXPECT_GT[T](a: errctx[T], b: errctx[T]): void = if a.0 > b.0 {} else {test_failed_expect_cmp(a, b, ">")}
fun EXPECT_GE[T](a: errctx[T], b: errctx[T]): void = if a.0 >= b.0 {} else {test_failed_expect_cmp(a, b, ">=")}

fun EXPECT_EQ[T](a: errctx[T], b: T): void = if a.0 == b {} else {test_failed_expect_cmp(a, (b, "", "", 0), "==")}
fun EXPECT_NE[T](a: errctx[T], b: T): void = if a.0 != b {} else {test_failed_expect_cmp(a, (b, "", "", 0), "!=")}
fun EXPECT_LT[T](a: errctx[T], b: T): void = if a.0 < b {} else {test_failed_expect_cmp(a, (b, "", "", 0), "<")}
fun EXPECT_LE[T](a: errctx[T], b: T): void = if a.0 <= b {} else {test_failed_expect_cmp(a, (b, "", "", 0), "<=")}
fun EXPECT_GT[T](a: errctx[T], b: T): void = if a.0 > b {} else {test_failed_expect_cmp(a, (b, "", "", 0), ">")}
fun EXPECT_GE[T](a: errctx[T], b: T): void = if a.0 >= b {} else {test_failed_expect_cmp(a, (b, "", "", 0), ">=")}

fun EXPECT_NEAR[T](a: T, b: T, eps: T): void =
    if normInf(a, b) > eps {test_failed_expect_near(a, b, (None: int?), eps)}

fun EXPECT_NEAR[T](a: errctx[T], b: errctx[T], eps: T): void =
    if normInf(a.0, b.0) > eps {test_failed_expect_near(a, b, (None: int?), eps)}

fun EXPECT_NEAR[T](a: errctx[T], b: T, eps: T): void =
    if normInf(a.0, b) > eps {test_failed_expect_near(a, (b, "", "", 0), (None: int?), eps)}

fun EXPECT_NEAR[T](a: T [+], b: T [+], eps: T): void =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a, bi <- b {normInf(ai, bi) > eps})
        test_failed_expect_near(ai, bi, Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_NEAR[T](a: (T...) [+], b: (T...) [+], eps: T): void =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a, bi <- b {normInf(ai, bi) > eps})
        test_failed_expect_near(ai, bi, Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_NEAR[T](a: (T [+], string, string, int), b: (T [+], string, string, int), eps: T): void =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a.0, bi <- b.0 {normInf(ai, bi) > eps})
        test_failed_expect_near((ai, "", a.2, a.3), (bi, "", "", 0), Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_NEAR[T](a: ((T...) [+], string, string, int), b: ((T...) [+], string, string, int), eps: T): void =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a.0, bi <- b.0 {normInf(ai, bi) > eps})
        test_failed_expect_near((ai, "", a.2, a.3), (bi, "", "", 0), Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_NEAR[T](a: list[T], b: list[T], eps: T): void =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a, bi <- b {normInf(ai, bi) > eps})
        test_failed_expect_near(ai, bi, Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_NEAR[T](a: (list[T], string, string, int), b: list[T], eps: T): void =
    try {
        val (ai, idx, bi) = find(
            for ai@idx <- a.0, bi <- b {normInf(ai, bi) > eps})
        test_failed_expect_near((ai, "", a.2, a.3), (bi, "", "", 0), Some(idx), eps)
    }
    catch {
    | NotFoundError => {}
    }

fun EXPECT_THROWS(f: (void->void, string, string, int), ref_exn: exn, ~msg: string=""): void =
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

fun EXPECT_THROWS(f: void->void, ref_exn: exn, ~msg: string=""): void =
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

fun EXPECT_NO_THROWS(f: (void->void, string, string, int), ~msg: string=""): void =
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

fun EXPECT_NO_THROWS(f: void->void, ~msg: string=""): void =
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

fun test_duration2str(ts_diff: double): string
{
    val ts_rdiff = round(ts_diff)
    if ts_rdiff < 1 {"<1 ms"} else {string(ts_rdiff) + " ms"}
}

fun test_run_all(opts: test_options_t): void
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
    val fold failed = ([]: list[string]) for t <- g_test_all_registered.rev() {
        val name = t.name
        val matches =
            filter == "" ||
            name == filter ||
            (startswithstar && name.endswith(filter)) ||
            (endswithstar && name.startswith(filter)) ||
            (startswithstar && endswithstar && name.find(filter) != -1)
        if (!matches ^ inverse_test) {}
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
            if !ok {failed = name :: failed}
        }
    }
    val ts0_end = Sys.tick_count()
    val ts0_diff_str = test_duration2str((ts0_end - ts0_start)*ts_scale)
    val nfailed = failed.length()
    val npassed = nexecuted - nfailed
    println(f"[==========] {nexecuted} test(s) ran ({ts0_diff_str})")
    println(f"{Green}[  PASSED  ]{Normal} {npassed} test(s)")
    if nfailed > 0 {
        println(f"{Red}[  FAILED  ]{Normal} {nfailed} test(s):")
        for i <- failed.rev() {println(i)}
    }
}

fun test_print_options(title: string, more_opts: string): void {
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

fun test_parse_options(args: list[string], title: string, more_opts: string): (bool, UTest.test_options_t) {
    var options = test_options_t {}

    fun parse(args: list[string]) {
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

/* ============ macro-1: backtick-free EXPECT_ / ASSERT_ macros ============
   These capture the call site (@file/@line) and the compared expressions'
   source text (@string) automatically, so a test writes `EXPECT_EQ_(f(x), y)`
   with no backtick `...` context quoting. Each macro is a thin wrapper that
   evaluates every argument EXACTLY ONCE by passing it, once, to a backing
   helper (the helper's parameters are the single evaluation).

   Name-resolution rule (macro_design.md section 4): a macro expands into, and
   resolves in, the CALLER's environment. Because UTest is NOT a base
   auto-imported module, the macros MUST qualify every UTest name they use
   (`UTest.test_report_cmp_`, `UTest.g_test_state`) -- otherwise the expansion
   would only compile where the caller did `from UTest import *`. BUT a qualified
   call to a GENERIC helper would pin that helper's body to UTest's scope, so a
   user's LOCAL `string`/`==` overload for the compared type (e.g. a hand-written
   `string` for a recursive variant) would be invisible. The resolution: do the
   overload-dependent work -- the comparison `a == b` and the value formatting
   `string(a)` -- INLINE in the macro, using Builtins operations (auto-imported
   everywhere), so it resolves at the CALL site and finds the local overloads;
   then hand only the already-stringified, NON-generic report to a qualified
   UTest helper. `string(a)` sits inside the failure branch, so it runs only on
   failure. ONE helper serves EXPECT_ (non-fatal) and ASSERT_ (fatal, throws)
   via a `fatal` flag. Optional trailing `msg` is a separate arity (variant B). */

fun test_report_cmp_(astr: string, op: string, bstr: string, aval: string, bval: string,
                     fname: string, lineno: int, fatal: bool): void
{
    print(f"{fname}:{lineno}: comparison '{astr} {op} {bstr}' failed.\n")
    print(f"  actual:   {aval}\n  expected: {bval}\n")
    if fatal { throw TestAssertError } else { g_test_state.currstatus = false }
}

fun test_report_near_(astr: string, bstr: string, epsval: string, aval: string, bval: string,
                      fname: string, lineno: int, fatal: bool): void
{
    print(f"{fname}:{lineno}: comparison 'abs({astr} - {bstr}) <= {epsval}' failed.\n")
    print(f"  actual:   {aval}\n  expected: {bval}\n")
    if fatal { throw TestAssertError } else { g_test_state.currstatus = false }
}

fun test_report_bool_(cstr: string, msg: string, fname: string, lineno: int, fatal: bool): void
{
    val extra = if msg != "" { f": {msg}" } else { "" }
    println(f"{fname}:{lineno}: '{cstr}' failed{extra}")
    if fatal { throw TestAssertError } else { g_test_state.currstatus = false }
}

// near-comparison predicate -- normInf is not user-overloaded, so a qualified
// call is safe. scalar / array / tuple reach normInf(a, b) directly; arrays of
// tuples and lists compare element-wise (normInf over the whole thing is a
// tuple / undefined).
fun test_is_near_[T](a: T, b: T, eps: T): bool = normInf(a, b) <= eps
fun test_is_near_[T](a: T [+], b: T [+], eps: T): bool = normInf(a, b) <= eps
fun test_is_near_[T](a: (T...), b: (T...), eps: T): bool = normInf(a, b) <= eps
fun test_is_near_[T](a: (T...) [+], b: (T...) [+], eps: T): bool =
    size(a) == size(b) && !exists(for ai <- a, bi <- b {normInf(ai, bi) > eps})
fun test_is_near_[T](a: list[T], b: list[T], eps: T): bool =
    a.length() == b.length() && !exists(for ai <- a, bi <- b {normInf(ai, bi) > eps})

fun test_throws_(f: void->void, ref_exn: exn, fstr: string, msg: string,
                 fname: string, lineno: int, fatal: bool): void
{
    val extra = if msg != "" { f": {msg}" } else { "" }
    try {
        f()
        println(f"{fname}:{lineno}: '{fstr}' did not throw{extra}")
        println(f"  expected: throws '{ref_exn}'")
        if fatal { throw TestAssertError } else { g_test_state.currstatus = false }
    } catch {
    | e when e.__tag__ == ref_exn.__tag__ => {}
    | e =>
        println(f"{fname}:{lineno}: '{fstr}' threw the wrong exception{extra}")
        println(f"  actual:   '{e}'\n  expected: '{ref_exn}'")
        if fatal { throw TestAssertError } else { g_test_state.currstatus = false }
    }
}

fun test_no_throws_(f: void->void, fstr: string, msg: string,
                    fname: string, lineno: int, fatal: bool): void
{
    val extra = if msg != "" { f": {msg}" } else { "" }
    try { f() }
    catch { | e =>
        println(f"{fname}:{lineno}: '{fstr}' threw '{e}'{extra}")
        if fatal { throw TestAssertError } else { g_test_state.currstatus = false }
    }
}

/* The comparison macros bind each argument once (a_/b_ are hygienic), compare
   and stringify INLINE (call-site overloads), and report via the qualified
   non-generic helper only on failure. */
macro EXPECT_EQ_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ == b_ {} else { UTest.test_report_cmp_(@string(a), "==", @string(b), string(a_), string(b_), @file, @line, false) } }
macro EXPECT_NE_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ != b_ {} else { UTest.test_report_cmp_(@string(a), "!=", @string(b), string(a_), string(b_), @file, @line, false) } }
macro EXPECT_LT_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ <  b_ {} else { UTest.test_report_cmp_(@string(a), "<",  @string(b), string(a_), string(b_), @file, @line, false) } }
macro EXPECT_LE_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ <= b_ {} else { UTest.test_report_cmp_(@string(a), "<=", @string(b), string(a_), string(b_), @file, @line, false) } }
macro EXPECT_GT_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ >  b_ {} else { UTest.test_report_cmp_(@string(a), ">",  @string(b), string(a_), string(b_), @file, @line, false) } }
macro EXPECT_GE_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ >= b_ {} else { UTest.test_report_cmp_(@string(a), ">=", @string(b), string(a_), string(b_), @file, @line, false) } }
macro ASSERT_EQ_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ == b_ {} else { UTest.test_report_cmp_(@string(a), "==", @string(b), string(a_), string(b_), @file, @line, true) } }
macro ASSERT_NE_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ != b_ {} else { UTest.test_report_cmp_(@string(a), "!=", @string(b), string(a_), string(b_), @file, @line, true) } }
macro ASSERT_LT_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ <  b_ {} else { UTest.test_report_cmp_(@string(a), "<",  @string(b), string(a_), string(b_), @file, @line, true) } }
macro ASSERT_LE_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ <= b_ {} else { UTest.test_report_cmp_(@string(a), "<=", @string(b), string(a_), string(b_), @file, @line, true) } }
macro ASSERT_GT_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ >  b_ {} else { UTest.test_report_cmp_(@string(a), ">",  @string(b), string(a_), string(b_), @file, @line, true) } }
macro ASSERT_GE_(a: @expr, b: @expr): void { val a_ = a, b_ = b; if a_ >= b_ {} else { UTest.test_report_cmp_(@string(a), ">=", @string(b), string(a_), string(b_), @file, @line, true) } }

macro EXPECT_NEAR_(a: @expr, b: @expr, eps: @expr): void { val a_ = a, b_ = b, e_ = eps; if UTest.test_is_near_(a_, b_, e_) {} else { UTest.test_report_near_(@string(a), @string(b), string(e_), string(a_), string(b_), @file, @line, false) } }
macro ASSERT_NEAR_(a: @expr, b: @expr, eps: @expr): void { val a_ = a, b_ = b, e_ = eps; if UTest.test_is_near_(a_, b_, e_) {} else { UTest.test_report_near_(@string(a), @string(b), string(e_), string(a_), string(b_), @file, @line, true) } }

// --- bool family, with an optional trailing message (arity overload) ---
macro EXPECT_(c: @expr): void { val c_ = c; if c_ {} else { UTest.test_report_bool_(@string(c), "", @file, @line, false) } }
macro EXPECT_(c: @expr, msg: @expr): void { val c_ = c; if c_ {} else { UTest.test_report_bool_(@string(c), msg, @file, @line, false) } }
macro ASSERT_(c: @expr): void { val c_ = c; if c_ {} else { UTest.test_report_bool_(@string(c), "", @file, @line, true) } }
macro ASSERT_(c: @expr, msg: @expr): void { val c_ = c; if c_ {} else { UTest.test_report_bool_(@string(c), msg, @file, @line, true) } }

// --- exception family (the argument is a void->void thunk; exn printing is not
//     user-overloaded, so the whole check delegates to a qualified helper) ---
macro EXPECT_THROWS_(f: @expr, ref_exn: @expr): void { UTest.test_throws_(f, ref_exn, @string(f), "", @file, @line, false) }
macro EXPECT_THROWS_(f: @expr, ref_exn: @expr, msg: @expr): void { UTest.test_throws_(f, ref_exn, @string(f), msg, @file, @line, false) }
macro EXPECT_NO_THROWS_(f: @expr): void { UTest.test_no_throws_(f, @string(f), "", @file, @line, false) }
macro EXPECT_NO_THROWS_(f: @expr, msg: @expr): void { UTest.test_no_throws_(f, @string(f), msg, @file, @line, false) }
