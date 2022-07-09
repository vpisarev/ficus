/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some NN/ONNX ops tests, extracted from ONNX specification:
// https://github.com/onnx/onnx/blob/main/docs/Operators.md

from UTest import *
import NN.Ast as Ast, NN.OpPermute as OpPermute

TEST("NN.Gather.basic", fun()
{
    // case 0
    val axis = 0
    val data = float([
        1.0, 1.2,
        2.3, 3.4,
        4.5, 5.7])
    val ind = int32([
        0, 1,
        1, 2])
    val out_shape = [2, 2, 2]
    val out0 = array(8, 0.f)
    OpPermute.run_gather(axis, [3, 2], Ast.NN_Data_FP32(data),
               [2, 2], Ast.NN_Data_I32(ind),
               out_shape, Ast.NN_Data_FP32(out0), 4)
    EXPECT_EQ(out0.reshape(out_shape[0], out_shape[1], out_shape[2]), float([
          1.0, 1.2,
          2.3, 3.4,
          2.3, 3.4,
          4.5, 5.7]).reshape(out_shape[0], out_shape[1], out_shape[2]))
    // case 1
    val axis = 1
    val data = float([
        1.0, 1.2, 1.9,
        2.3, 3.4, 3.9,
        4.5, 5.7, 5.9])
    val ind = [0L, 2L]
    val out1 = array(6, 0.f)
    val out_shape = [3, 1, 2]
    OpPermute.run_gather(axis, [3, 3], Ast.NN_Data_FP32(data),
               [1, 2], Ast.NN_Data_I64(ind),
               out_shape, Ast.NN_Data_FP32(out1), 4)
    EXPECT_EQ(out1.reshape(out_shape[0], out_shape[1], out_shape[2]), float([
            1.0, 1.9,
            2.3, 3.9,
            4.5, 5.9]).reshape(out_shape[0], out_shape[1], out_shape[2]))
})
