/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* Unit test system for Ficus */
import Sys

exception TestAssertError
exception TestFailure: string

type test_info_t =
{
    name: string,
    f: void->void
}

type test_rng_t =
{
    state: uint64 ref
}

fun test_rng_int(rng: test_rng_t, a: int, b: int)
{
    val s = *rng.state
    val s = (s :> uint32) * 4197999714u64 + (s >> 32)
    val (a, b) = (min(a, b), max(a, b))
    val diff = b - a
    val x = (s :> uint32) % diff + a
    *rng.state = s
    (x :> int)
}

fun test_rng_copy(rng: test_rng_t)
{
    test_rng_t {state = ref(*rng.state)}
}

type test_state_t =
{
    currstatus: bool,
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
            println(f"Error: Assertion failed.")
        } else {
            println(f"Error: Assertion failed: {m}")
        }
        throw TestAssertError
    }
}

fun ASSERT(c: bool) = ASSERT(c, test_msg(""))

fun test_failed_assert_cmp(a: 't, b: 't, op: string)
{
    println("Error: Assertion \"computed:'{a}' {op} reference:'{b}'\" failed.")
    throw TestAssertError
}

fun ASSERT_EQ(a: 't, b: 't) = if a == b {} else {test_failed_assert_cmp(a, b, "==")}
fun ASSERT_NE(a: 't, b: 't) = if a != b {} else {test_failed_assert_cmp(a, b, "!=")}
fun ASSERT_LT(a: 't, b: 't) = if a < b {} else {test_failed_assert_cmp(a, b, "<")}
fun ASSERT_LE(a: 't, b: 't) = if a <= b {} else {test_failed_assert_cmp(a, b, "<=")}
fun ASSERT_GT(a: 't, b: 't) = if a > b {} else {test_failed_assert_cmp(a, b, ">")}
fun ASSERT_GE(a: 't, b: 't) = if a >= b {} else {test_failed_assert_cmp(a, b, ">=")}

fun ASSERT_NEAR(a: 't, b: 't, eps: 't) =
    if b - eps <= a <= b + eps {} else {
        println("Error: Assertion 'computed:{a} ≈ reference:{b}' with eps={eps} failed.")
        throw TestAssertError
    }

fun EXPECT(c: bool, msg: void->string)
{
    if(!c) {
        val m = msg()
        if (m == "") {
            println(f"Error: Expected: true, Computed: false.")
        } else {
            println(f"Error: {m}.")
        }
        g_test_state.currstatus = false
    }
}

fun test_failed_expect_cmp(a: 't, b: 't, op: string)
{
    println("Error: Expected \"computed:'{a}' {op} reference:'{b}'\".")
    g_test_state.currstatus = false
}

fun EXPECT_EQ(a: 't, b: 't) = if a == b {} else {test_failed_expect_cmp(a, b, "==")}
fun EXPECT_NE(a: 't, b: 't) = if a != b {} else {test_failed_expect_cmp(a, b, "!=")}
fun EXPECT_LT(a: 't, b: 't) = if a < b {} else {test_failed_expect_cmp(a, b, "<")}
fun EXPECT_LE(a: 't, b: 't) = if a <= b {} else {test_failed_expect_cmp(a, b, "<=")}
fun EXPECT_GT(a: 't, b: 't) = if a > b {} else {test_failed_expect_cmp(a, b, ">")}
fun EXPECT_GE(a: 't, b: 't) = if a >= b {} else {test_failed_expect_cmp(a, b, ">=")}

fun EXPECT_NEAR(a: 't, b: 't, eps: 't) =
    if b - eps <= a <= b + eps {} else {
        println("Error: Expected \"computed:'{a}' ≈ reference:'{b}'\" with eps={eps}.")
        g_test_state.currstatus = false
    }

fun test_run_all(filter:string)
{
    val (startswithstar, filter) = if filter.startswith("*") {(true, filter[1:])} else {(false, filter)}
    val (endswithstar, filter) = if filter.endswith("*") {(true, filter[:-1])} else {(false, filter)}
    if filter.find("*") >= 0 {
        throw TestFailure("test filter with '*' inside is currently unsupported")
    }
    val ts_scale = 1000./Sys.getTickFrequency()
    var nexecuted = 0
    val ts0_start = Sys.getTickCount()
    val fold failed = ([]: string list) for t <- g_test_all_registered.rev() {
        val name = t.name
        val matches =
            filter == "" ||
            name == filter ||
            (startswithstar && name.endswith(filter)) ||
            (endswithstar && name.startswith(filter)) ||
            (startswithstar && endswithstar && name.find(filter) != -1)
        if (!matches) {failed}
        else {
            nexecuted += 1
            println(f"[ RUN      ] {name}")
            g_test_state = test_init_state_before_test()
            val ts_start = Sys.getTickCount()
            try {
                t.f()
            } catch {
            | TestAssertError =>
                g_test_state.currstatus = false
            | e =>
                println("Exception {e} occured.")
                g_test_state.currstatus = false
            }
            val ts_end = Sys.getTickCount()
            val ok = g_test_state.currstatus
            val ok_fail = if ok {"[       OK ]"} else {"[     FAIL ]"}
            println(f"{ok_fail} {name} ({(ts_end - ts_start)*ts_scale} ms)\n")
            if ok {failed} else {name :: failed}
        }
    }
    val ts0_end = Sys.getTickCount()
    val nfailed = failed.length()
    val npassed = nexecuted - nfailed
    println(f"[==========] {nexecuted} test(s) ran ({(ts0_end - ts0_start)*ts_scale} ms total)\n")
    println(f"[  PASSED  ] {npassed} test(s)")
    if nfailed > 0 {
        println(f"[  FAILED ] {nfailed} test(s):")
        for i <- failed.rev() {println(i)}
    }
}
