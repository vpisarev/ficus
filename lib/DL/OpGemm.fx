/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

fun run_gemm(A: 't [], A_shape: Ast.dlshape_t,
             B: 't [], B_shape: Ast.dlshape_t,
             bias: 't [], C_shape: Ast.dlshape_t,
             out: 't [], alpha: float, beta: float,
             transA: bool, transB: bool)
{
    for _@idx <- out {out[idx] = (0 :> 't)}
}

fun run_gemm(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_Gemm {alpha, beta, transA, transB, t_A, t_B, t_bias, t_out} =>
    val A = net.get_tensor(t_A), B = net.get_tensor(t_B)
    val bias = net.get_tensor(t_bias)
    val out = net.get_tensor(t_out)
    match (A.data, B.data, bias.data, out.data) {
    | (Ast.DL_Data_FP32 a_data, Ast.DL_Data_FP32 b_data, Ast.DL_Data_FP32 bias_data, Ast.DL_Data_FP32 out_data) =>
        run_gemm(a_data, A.shape, b_data, B.shape, bias_data, bias.shape, out_data, alpha, beta, transA, transB)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unsupported operation {op.name()}")
}
