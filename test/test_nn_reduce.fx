/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some NN/ONNX ops tests, extracted from ONNX specification:
// https://github.com/onnx/onnx/blob/main/docs/Operators.md

from UTest import *
import NN.Ast as Ast, NN.OpReduce as OpReduce

TEST("NN.TopK", fun()
{
    val axis = 1
    val k = 3
    val inp_shape = [3, 4]
    val inp_data = Ast.NN_Data_FP32 (float([0, 1, 2, 3, 4, 5, 6, 7, 11, 10, 9, 8]))
    val out_shape = [3, k]
    val out_data = Ast.NN_Data_FP32 (array(3*k, 0.f))
    val out_ind = Ast.NN_Data_I64 (array(3*k, 0L))

    val largest = true
    OpReduce.run_top_k(inp_shape, inp_data, out_shape, out_data, out_ind, axis, largest, true, k, 4)
    EXPECT_EQ(`float(out_data).reshape(3, k)`,
        [3.0f, 2.0f, 1.0f; 7.0f, 6.0f, 5.0f; 11.0f, 10.0f, 9.0f])
    EXPECT_EQ(`int(out_ind).reshape(3, k)`,
        [3, 2, 1; 3, 2, 1; 0, 1, 2])
    val largest = false
    OpReduce.run_top_k(inp_shape, inp_data, out_shape, out_data, out_ind, axis, largest, true, k, 4)
    EXPECT_EQ(`float(out_data).reshape(3, k)`,
        [0.0f, 1.0f, 2.0f; 4.0f, 5.0f, 6.0f; 8.0f, 9.0f, 10.0f])
    EXPECT_EQ(`int(out_ind).reshape(3, k)`,
        [0, 1, 2; 0, 1, 2; 3, 2, 1])
})

TEST("NN.NonZero", fun()
{
    val inp_shape = [2, 2]
    var inp_data = Ast.NN_Data_FP32 ([1.f, 0.f, 1.f, 1.f])
    val (nz, coords, buf) = OpReduce.run_nonzero(inp_shape, inp_data, [], 4)
    EXPECT_EQ(nz, 3)
    EXPECT_EQ(`coords.reshape(2, 3)`, [0L, 1L, 1L; 0L, 0L, 1L])
    EXPECT_EQ(buf.size(), 48)
})
