/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// T5 randomized suite: integer/float arithmetic vs scalar reference expressions.
// Every case is reproducible: a failure prints the per-case seed (mk_rng(seed)
// replays it) and the case index.  Base seed comes from env FXTEST_SEED.

from UTest import *
from RandUtil import *

// splitmix64 reference vectors for seed 0 (locks the whole uint64 pipeline and
// the FB-002 logical-shift workaround; if either regresses, this fails loudly).
TEST("rand.arith.splitmix64_vectors", fun() {
    val rng = mk_rng(0u64)
    val expected = [16294208416658607535u64, 7960286522194355700u64, 487617019471545679u64]
    for e@i <- expected {
        val got = next_u64(rng)
        if got != e {println(f"  [repro] splitmix64 rrbvec {i}: got={got} expected={e}")}
        EXPECT_EQ(got, e)
    }
})

TEST("rand.arith.divmod_identity", fun() {
    val name = "rand.arith.divmod_identity"
    for i <- 0:1000 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val a = next_int(rng, -100000, 100000)
        val b0 = next_int(rng, -1000, 1000)
        val b = if b0 == 0 {1} else {b0}
        val q = a / b, r = a % b
        val ok = q*b + r == a && abs(r) < abs(b)
        if !ok {println(f"  [repro] {name} case={i} seed={seed}: a={a} b={b} q={q} r={r}")}
        EXPECT(ok)
    }
})

TEST("rand.arith.u8_wrap", fun() {
    val name = "rand.arith.u8_wrap"
    for i <- 0:500 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val a = next_int(rng, 0, 255), b = next_int(rng, 0, 255)
        val au = (a :> uint8), bu = (b :> uint8)
        val got = ((au + bu) :> uint8)                 // narrowing add wraps mod 256
        val rf = (((a + b) & 0xff) :> uint8)
        if got != rf {println(f"  [repro] {name} case={i} seed={seed}: {a}+{b} got={got} rf={rf}")}
        EXPECT_EQ(got, rf)
    }
})

TEST("rand.arith.u32_shift", fun() {
    val name = "rand.arith.u32_shift"
    for i <- 0:500 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val x = next_int(rng, 0, 0x7fffffff)
        val n = next_int(rng, 0, 31)
        val xu = (x :> uint32)
        val got = ((xu >> n) :> int)                   // uint32 >> is logical (OK)
        val rf = x >> n                               // x >= 0, int >> is logical too
        if got != rf {println(f"  [repro] {name} case={i} seed={seed}: {x}>>{n} got={got} rf={rf}")}
        EXPECT_EQ(got, rf)
    }
})

TEST("rand.arith.sat_casts", fun() {
    val name = "rand.arith.sat_casts"
    for i <- 0:1000 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val x = next_int(rng, -400, 400)
        val g8 = sat_uint8(x),  r8 = ((max(0, min(255, x))) :> uint8)
        val gi8 = sat_int8(x),  ri8 = ((max(-128, min(127, x))) :> int8)
        val g16 = sat_uint16(x), r16 = ((max(0, min(65535, x))) :> uint16)
        if g8 != r8 || gi8 != ri8 || g16 != r16 {
            println(f"  [repro] {name} case={i} seed={seed}: x={x}")
        }
        EXPECT_EQ(g8, r8)
        EXPECT_EQ(gi8, ri8)
        EXPECT_EQ(g16, r16)
    }
})

TEST("rand.arith.float_to_int_trunc", fun() {
    val name = "rand.arith.float_to_int_trunc"
    for i <- 0:1000 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val x = next_double(rng, -1000.0, 1000.0)
        val got = int(x)                               // truncates toward zero
        val rf = if x >= 0.0 {int(floor(x))} else {-int(floor(-x))}
        if got != rf {println(f"  [repro] {name} case={i} seed={seed}: x={x} got={got} rf={rf}")}
        EXPECT_EQ(got, rf)
    }
})

TEST("rand.arith.nan_inf", fun() {
    val name = "rand.arith.nan_inf"
    val nanv = 0.0/0.0, pinf = 1.0/0.0, ninf = -1.0/0.0
    EXPECT(isnan(nanv))
    EXPECT(isinf(pinf) && isinf(ninf))
    EXPECT(!isnan(pinf) && !isinf(nanv))
    for i <- 0:500 {
        val seed = case_seed(name, i)
        val rng = mk_rng(seed)
        val x = next_double(rng, -1e6, 1e6)
        val ok = !isnan(x) && !isinf(x) && isnan(x + nanv) && isinf(x + pinf)
        if !ok {println(f"  [repro] {name} case={i} seed={seed}: x={x}")}
        EXPECT(ok)
    }
})
