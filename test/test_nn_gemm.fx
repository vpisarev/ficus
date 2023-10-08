/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some NN/ONNX ops tests, extracted from ONNX specification:
// https://github.com/onnx/onnx/blob/main/docs/Operators.md

from UTest import *
import NN.Ast as Ast, NN.OpGemm as OpGemm

TEST("NN.Gemm.basic", fun()
{
    val M = 2, N = 3, K = 4

    // attrs
    val alpha = 1.f
    val beta = 1.f
    val trans_a = false
    val trans_b = false

    // inputs
    val A = float([7.0, 0.0, 8.0, 4.0, 2.0, 2.0, 6.0, 8.0]).reshape(M, K)
    val B = float([5.0, 6.0, 6.0, 3.0, 2.0, 7.0, 8.0, 5.0, 8.0, 1.0, 2.0, 7.0]).reshape(K, N)
    val C = float([5.0, 0.0, 1.0, 8.0, 7.0, 9.0]).reshape(M, N)

    // output ref
    val Y_ref = float([108.0, 90.0, 135.0, 80.0, 69.0, 139.0]).reshape(M, N)

    // run gemm[p']
    val Y = array(M * N, 0.f).reshape(M, N)
    OpGemm.run_gemm(Ast.mktensor(A), Ast.mktensor(B), Ast.mktensor(C), Ast.mktensor(Y), alpha, beta, trans_a, trans_b, 8)

    EXPECT_EQ(Y, Y_ref)
})
