/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// tests for the deque type
from UTest import *
import DSP.Fft

TEST("fft.fwd_inv", fun() {
    val rng = RNG(0x34598u64)
    for n <- [1, 2, 4, 8, 16, 1024, 1<<20] {
        val signal = rng.uniform(n, -1.f, 1.f)
        val spectrum = DSP.Fft.fwd(signal)
        if n > 1 {
            EXPECT_NE(signal, spectrum)
        }
        val restored = DSP.Fft.inv(spectrum, scale=1.f/n)
        EXPECT_NEAR(`signal`, `restored`, 1e-5f)
    }
})
