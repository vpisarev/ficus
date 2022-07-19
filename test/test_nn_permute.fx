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
        1.0, 1.2;
        2.3, 3.4;
        4.5, 5.7])
    val ind = int32([
        0, 1;
        1, 2])
    val out0 = array(8, 0.f).reshape(2, 2, 2)
    OpPermute.run_gather(axis, Ast.mktensor(data), Ast.mktensor(ind),
                         Ast.mktensor(out0), 4)
    EXPECT_EQ(out0.reshape(2, 2, 2), float([
          1.0, 1.2,
          2.3, 3.4,
          2.3, 3.4,
          4.5, 5.7]).reshape(2, 2, 2))
    // case 1
    val axis = 1
    val data = float([
        1.0, 1.2, 1.9;
        2.3, 3.4, 3.9;
        4.5, 5.7, 5.9])
    val ind = [0L, 2L]
    val out1 = array(6, 0.f).reshape(3, 1, 2)
    OpPermute.run_gather(axis, Ast.mktensor(data),
               Ast.mktensor(ind.reshape(1, 2)),
               Ast.mktensor(out1), 4)
    EXPECT_EQ(out1.reshape(3, 1, 2), float([
            1.0, 1.9,
            2.3, 3.9,
            4.5, 5.9]).reshape(3, 1, 2))
})
