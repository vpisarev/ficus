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
    val inp = Ast.mktensor(float([0, 1, 2, 3; 4, 5, 6, 7; 11, 10, 9, 8]))
    val out = Ast.mktensor(array((3, k), 0.f))
    val out_ind = Ast.mktensor(array((3, k), 0L))

    val largest = true
    OpReduce.run_top_k(inp, out, out_ind, axis, largest, true, k, 4)
    EXPECT_EQ(`float(out.data).reshape(3, k)`,
        [3.0f, 2.0f, 1.0f; 7.0f, 6.0f, 5.0f; 11.0f, 10.0f, 9.0f])
    EXPECT_EQ(`int(out_ind.data).reshape(3, k)`,
        [3, 2, 1; 3, 2, 1; 0, 1, 2])
    val largest = false
    OpReduce.run_top_k(inp, out, out_ind, axis, largest, true, k, 4)
    EXPECT_EQ(`float(out.data).reshape(3, k)`,
        [0.0f, 1.0f, 2.0f; 4.0f, 5.0f, 6.0f; 8.0f, 9.0f, 10.0f])
    EXPECT_EQ(`int(out_ind.data).reshape(3, k)`,
        [0, 1, 2; 0, 1, 2; 3, 2, 1])
})

TEST("NN.NonZero", fun()
{
    val inp = Ast.mktensor([1.f, 0.f; 1.f, 1.f])
    val (nz, coords, buf) = OpReduce.run_nonzero(inp, [], 4)
    EXPECT_EQ(nz, 3)
    EXPECT_EQ(`coords.reshape(2, 3)`, [0L, 1L, 1L; 0L, 0L, 1L])
    EXPECT_EQ(buf.size(), 48)
})

TEST("NN.Reduce", fun()
{
    val a=Ast.mktensor(float([-4, -3, -2, -1;
                  1, 1, 2, 3;
                  4, 5, 6, 7]))
    val ref_data = [
        [39.0],
        [13.076696],
        [19.0],
        [120960.0],
        [-4.0],
        [7.0],
        [1.5833334],
        [9.0, 9.0, 10.0, 11.0],
        [5.7445626, 5.91608, 6.6332498, 7.6811457],
        [1.0, 3.0, 6.0, 9.0],
        [-16.0, -15.0, -24.0, -21.0],
        [-4.0, -3.0, -2.0, -1.0],
        [4.0, 5.0, 6.0, 7.0],
        [0.33333334, 1.0, 2.0, 3.0],
        [10.0, 7.0, 22.0],
        [5.4772258, 3.8729835, 11.224972],
        [-10.0, 7.0, 22.0],
        [24.0, 6.0, 840.0],
        [-4.0, 1.0, 4.0],
        [-1.0, 3.0, 7.0],
        [-2.5, 1.75, 5.5]]

    val a_shape = a.shape.shape
    var test_case_idx = 0
    for k <- 0:3 {
        val (axes, out_shape) =
            if k == 0 {([]: int [], []: int [])}
            else if k == 1 {([0], [a_shape[1]])}
            else {([1], [a_shape[0]])}
        val keepdims = false
        val out = Ast.mktensor(Ast.nnshape_t {layout=Ast.NN_Layout_Unknown, shape=out_shape}, a.elemtype())
        for rop <- [Ast.NN_ReduceL1, Ast.NN_ReduceL2, Ast.NN_ReduceSum, Ast.NN_ReduceProd,
                    Ast.NN_ReduceMin, Ast.NN_ReduceMax, Ast.NN_ReduceMean] {
            OpReduce.run_reduce(a, out, axes, keepdims, rop, 4)
            val out_data = match out.data {
                | Ast.NN_Data_FP32 out_data => out_data
                | _ => ([]: float [])
            }
            EXPECT_NEAR(`out_data`, `float(ref_data[test_case_idx])`, 1e-3f)
            test_case_idx += 1
        }
    }
})
