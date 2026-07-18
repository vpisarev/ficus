/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// types-1: the two 16-bit float types fp16 (IEEE half) and bf16 (bfloat16).
// Both are 16-bit storage types whose arithmetic promotes to float32; they
// differ only in the bit layout / rounding. bf16 reuses the float-type
// constructor with the width code 17 (see Ast.BF16), fp16 is the real 16.
// Literals: <float>h => fp16, <float>bf => bf16 (LexerUtils suffixes).
// Rounding tests read values through a runtime array so the cast is NOT
// constant-folded (the folder does not model 16-bit rounding, same as fp16).

from UTest import *

// literal suffixes parse to the right type and value (exact values).
TEST("fp16bf16.literals", fun() {
    val h = 1.5h
    val b = 2.25bf
    EXPECT_EQ(float(h), 1.5f)
    EXPECT_EQ(float(b), 2.25f)
    EXPECT_EQ(scalar_type(h), Type_F16)
    EXPECT_EQ(scalar_type(b), Type_BF16)
})

// arithmetic promotes to float32 and is exact for small exact operands.
TEST("fp16bf16.arith", fun() {
    val hs = 1.0h + 2.0h        // fp16 operands, computed in float32
    val bs = 1.0bf + 2.0bf      // bf16 operands, computed in float32
    EXPECT_EQ(float(hs), 3.0f)
    EXPECT_EQ(float(bs), 3.0f)
})

// bf16 has an 8-bit significand: 257 is not representable and rounds to 258;
// fp16 has an 11-bit significand and represents 257 exactly. Values are read
// from a runtime array to defeat constant folding.
TEST("fp16bf16.rounding", fun() {
    val src = [257.0f, 1.1f, 3.5f]
    val as_fp16 = [for v <- src { float(fp16(v)) }]
    val as_bf16 = [for v <- src { float(bf16(v)) }]
    EXPECT_EQ(as_fp16[0], 257.0f)         // fp16: exact
    EXPECT_EQ(as_bf16[0], 258.0f)         // bf16: 257 -> 258
    EXPECT_EQ(as_fp16[1], 1.0996094f)     // fp16 rounding of 1.1
    EXPECT_EQ(as_bf16[1], 1.1015625f)     // bf16 rounding of 1.1
    EXPECT_EQ(as_fp16[2], 3.5f)           // exact in both
    EXPECT_EQ(as_bf16[2], 3.5f)
})

// arrays of the 16-bit float types build and read back.
TEST("fp16bf16.arrays", fun() {
    val ha = fp16([1.0f, 2.0f, 3.5f])
    val ba = bf16([1.0f, 2.0f, 3.5f])
    EXPECT_EQ(float(ha[2]), 3.5f)
    EXPECT_EQ(float(ba[2]), 3.5f)
    EXPECT_EQ(size(ha), 3)
    EXPECT_EQ(size(ba), 3)
})

// inf / nan survive the float32<->16-bit round trip and print as such.
TEST("fp16bf16.inf_nan", fun() {
    val src = [1.0f, 0.0f]
    val one = src[0], zero = src[1]
    val hinf = fp16(one/zero),  binf = bf16(one/zero)
    val hnan = fp16(zero/zero),  bnan = bf16(zero/zero)
    EXPECT_EQ(string(hinf), "inf");  EXPECT_EQ(string(binf), "inf")
    EXPECT_EQ(string(hnan), "nan");  EXPECT_EQ(string(bnan), "nan")
    // an infinite fp16/bf16 converts back to an infinite float32
    EXPECT_EQ(float(hinf) > 1e30f, true)
    EXPECT_EQ(float(binf) > 1e30f, true)
})

// string() of an exact value.
TEST("fp16bf16.string", fun() {
    EXPECT_EQ(string(1.5h), "1.5")
    EXPECT_EQ(string(2.0bf), "2.0")
})
