/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some NN/ONNX ops tests, extracted from ONNX specification:
// https://github.com/onnx/onnx/blob/main/docs/Operators.md

from UTest import *
import NN.Ast as Ast, NN.OpQuantized as OpQuantized

TEST("NN.Quantized.quantizeLinear", fun()
{
    val n = 11
    val x = float([0, 2, 3, 1000, -254, -1000, 500, 128, -100, 1, 5]).reshape(1, 1, n)
    val y_scale = float([2])
    val y_zero_point = uint8([128])
    val y_ref = uint8([128, 129, 130, 255, 1, 0, 255, 192, 78, 128, 130]).reshape(1, 1, n)
    val y = array(n, 0u8).reshape(1, 1, n)
    val axis = 1
    OpQuantized.run_quantize(Ast.mktensor(x), Ast.mktensor(y_scale),
                            Ast.mktensor(y_zero_point), Ast.mktensor(y), axis, 4)
    EXPECT_EQ(y, y_ref)
})

TEST("NN.Quantized.dequantizeLinear", fun()
{
    val n = 11
    val x = uint8([128, 129, 130, 255, 1, 0, 255, 192, 78, 128, 130]).reshape(1, 1, n)
    val xs = int8([-128, -10, 13, 127, 1, 0, 100, -100, -78, 127, -128]).reshape(1, 1, n)
    val y_scale = float([2])
    val y_zero_point = uint8([128])
    val ys_zero_point = int8([3])
    val y_ref = float([0, 2, 4, 254, -254, -256, 254, 128, -100, 0, 4]).reshape(1, 1, n)
    val ys_ref = float([-262, -26, 20, 248, -4, -6, 194, -206, -162, 248, -262]).reshape(1, 1, n)
    val y = array(n, 0.f).reshape(1, 1, n)
    val ys = array(n, 0.f).reshape(1, 1, n)
    val ys_16 = array(n, 0.h).reshape(1, 1, n)
    val axis = 1
    OpQuantized.run_dequantize(Ast.mktensor(x), Ast.mktensor(y_scale),
                               Ast.mktensor(y_zero_point), Ast.mktensor(y), axis, 4)
    OpQuantized.run_dequantize(Ast.mktensor(xs), Ast.mktensor(y_scale),
                               Ast.mktensor(ys_zero_point), Ast.mktensor(ys), axis, 4)
    OpQuantized.run_dequantize(Ast.mktensor(xs), Ast.mktensor(y_scale),
                               Ast.mktensor(ys_zero_point), Ast.mktensor(ys_16), axis, 4)
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
