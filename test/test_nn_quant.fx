/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some NN/ONNX ops tests, extracted from ONNX specification:
// https://github.com/onnx/onnx/blob/main/docs/Operators.md

from UTest import *
import NN.Ast as Ast, NN.OpQuantized as OpQuantized, NN.OpGemm as OpGemm

TEST("NN.Quantized.quantizeLinear", fun()
{
    val n = 11
    val x = float([0, 2, 3, 1000, -254, -1000, 500, 128, -100, 1, 5]).reshape(1, n)
    val y_scale = float([2])
    val y_zero_point = uint8([128])
    val y_ref = uint8([128, 129, 130, 255, 1, 0, 255, 192, 78, 128, 130]).reshape(1, n)
    val y = array(n, 0u8).reshape(1, n)
    val axis = 1
    OpQuantized.run_quantize(Ast.mktensor(x, layout=Ast.NN_Layout_ND),
                            Ast.mktensor(y_scale, layout=Ast.NN_Layout_ND),
                            Ast.mktensor(y_zero_point), Ast.mktensor(y), axis, 4)
    EXPECT_EQ(y, y_ref)
})

TEST("NN.Quantized.dequantizeLinear", fun()
{
    val n = 11
    val x = uint8([128, 129, 130, 255, 1, 0, 255, 192, 78, 128, 130]).reshape(1, n)
    val xs = int8([-128, -10, 13, 127, 1, 0, 100, -100, -78, 127, -128]).reshape(1, n)
    val y_scale = float([2])
    val y_zero_point = uint8([128])
    val ys_zero_point = int8([3])
    val y_ref = float([0, 2, 4, 254, -254, -256, 254, 128, -100, 0, 4]).reshape(1, n)
    val ys_ref = float([-262, -26, 20, 248, -4, -6, 194, -206, -162, 248, -262]).reshape(1, n)
    val y = array(n, 0.f).reshape(1, n)
    val ys = array(n, 0.f).reshape(1, n)
    val ys_16 = array(n, 0.h).reshape(1, n)
    val axis = 1
    OpQuantized.run_dequantize(Ast.mktensor(x, layout=Ast.NN_Layout_ND),
                               Ast.mktensor(y_scale),
                               Ast.mktensor(y_zero_point),
                               Ast.mktensor(y, layout=Ast.NN_Layout_ND), axis, 4)
    OpQuantized.run_dequantize(Ast.mktensor(xs, layout=Ast.NN_Layout_ND),
                               Ast.mktensor(y_scale),
                               Ast.mktensor(ys_zero_point),
                               Ast.mktensor(ys, layout=Ast.NN_Layout_ND), axis, 4)
    OpQuantized.run_dequantize(Ast.mktensor(xs, layout=Ast.NN_Layout_ND),
                               Ast.mktensor(y_scale),
                               Ast.mktensor(ys_zero_point),
                               Ast.mktensor(ys_16, layout=Ast.NN_Layout_ND), axis, 4)
    EXPECT_NEAR(y, y_ref, 1e-5f)
    EXPECT_NEAR(ys, ys_ref, 1e-5f)
    EXPECT_NEAR(float(ys_16), ys_ref, 1e-5f)
})

TEST("NN.Quantized.add", fun()
{
    val n = 11
    val x1 = uint8([128, 129, 130, 255,   1, 0, 255, 192,  78, 128, 130]).reshape(1, 1, n)
    val x2 = uint8([130, 128,  78, 192, 255, 0,   1, 255, 130, 129, 128]).reshape(1, 1, n)
    val x1_scale = float([128])
    val x2_scale = float([100])
    val x1_zp = uint8([127])
    val x2_zp = uint8([120])
    val y_scale = float([127])
    val y_zp = uint8([128])
    val y_ref = [for x1v <- x1, x2v <- x2 {
            sat_uint8(((x1v - x1_zp[0])*x1_scale[0] +
                       (x2v - x2_zp[0])*x2_scale[0])/y_scale[0] + y_zp[0])
        }]
    val y = array(n, 0u8).reshape(1, 1, n)
    OpQuantized.run_qbinary(Ast.NN_Add, Ast.mktensor(x1), Ast.mktensor(x2), Ast.mktensor(y),
                            Ast.mktensor(x1_scale), Ast.mktensor(x1_zp),
                            Ast.mktensor(x2_scale), Ast.mktensor(x2_zp),
                            Ast.mktensor(y_scale), Ast.mktensor(y_zp), 4)
    EXPECT_NEAR(int(y), int(y_ref), 1)
})

TEST("NN.Quantized.globalAvgPool", fun()
{
    val N = 2, C = 5, H = 135, W = 101
    val rng = RNG(-1)
    val x = rng.uniform((N, C, H, W), 0u8, 255u8)
    val x_scale = float([128])
    val x_zp = uint8([100])
    val y_scale = float([110])
    val y_zp = uint8([127])
    val y_ref = [for n <- 0:N for c <- 0:C {
            val xsc = x_scale[0]
            val xzp = x_zp[0]
            val fold s = 0. for i <- 0:H for j <- 0:W {s + (x[n, c, i, j] - xzp)*xsc}
            sat_uint8(s/(y_scale[0]*H*W) + y_zp[0])
        }].reshape(N, C, 1, 1)
    val y = array(N*C, 0u8).reshape(N, C, 1, 1)
    OpQuantized.run_qglobal_avgpool(Ast.mktensor(x),
                            Ast.mktensor(x_scale), Ast.mktensor(x_zp),
                            Ast.mktensor(y_scale), Ast.mktensor(y_zp),
                            Ast.mktensor(y), false, 4)
    EXPECT_NEAR(int(y[:]), int(y_ref[:]), 1)
})

TEST("NN.Quantized.matmul", fun()
{
    val a = uint8([208, 236, 0, 238;
                   3, 214, 255, 29])
    val a_scale = float([0.0066])
    val a_zp = uint8([113])
    val b = uint8([152, 51, 244;
                  60, 26, 255;
                  0, 127, 246;
                  127, 254, 247])
    val b_scale = float([0.00705])
    val b_zp = uint8([114])
    val y_scale = float([0.0107])
    val y_zp = uint8([118])
    val y_ref = uint8([168, 115, 255;
                       1, 66, 151])
    val y = array(y_ref.size(), 0u8)
    OpGemm.run_qmatmul(Ast.mktensor(a), Ast.mktensor(b), Ast.mktensor(a_scale),
                       Ast.mktensor(a_zp), Ast.mktensor(b_scale), Ast.mktensor(b_zp),
                       Ast.mktensor(y_scale), Ast.mktensor(y_zp), Ast.mktensor(y), 4)
    EXPECT_EQ(int(y), int(y_ref))
})
