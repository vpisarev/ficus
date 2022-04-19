/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

fun run_gemm(A: float [], A_shape: Ast.dlshape_t,
             B: float [], B_shape: Ast.dlshape_t,
             bias: float [], bias_shape: Ast.dlshape_t,
             out: float [], out_shape: Ast.dlshape_t,
             alpha: float, beta: float,
             transA: bool, transB: bool): void
@ccode {
    const int ntasks = 4;
    const int_* A_shape_ = (const int_*)A_shape->shape.data;
    const int_* B_shape_ = (const int_*)B_shape->shape.data;
    const int_* bias_shape_ = (const int_*)bias_shape->shape.data;
    const int_* out_shape_ = (const int_*)out_shape->shape.data;
    int_ bias_ndims = bias_shape->shape.dim[0].size;
    const float* A_data = (const float*)A->data;
    const float* B_data = (const float*)B->data;
    const float* bias_data = (const float*)bias->data;
    float* out_data = (float*)out->data;

    if (A_shape->shape.dim[0].size != 2 ||
        B_shape->shape.dim[0].size != 2 ||
        out_shape->shape.dim[0].size != 2 ||
        (bias_ndims != 2 && bias_ndims != 1 && bias_ndims != 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeError);

    if (bias_ndims == 1 && (bias_shape_[0] != out_shape_[1]))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    if (bias_ndims == 2 &&
        (bias_shape_[0] != out_shape_[0] ||
         bias_shape_[1] != out_shape_[1]))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    if (bias_ndims == 0 || beta == 0.f) {
        beta = 0.f;
    } else {
        int_ cols = out_shape_[1];
        for (int_ i = 0; i < out_shape_[0]; i++) {
            const float* b_i = bias_data + (bias_ndims == 1 ? 0 : cols)*i;
            float* out_i = out_data + cols*i;
            if (beta == 1.f)
                memcpy(out_i, b_i, cols*sizeof(out_i[0]));
            else {
                for(int_ j = 0; j < cols; j++)
                    out_i[j] = beta*b_i[j];
            }
        }
        beta = 1.f;
    }

    return fx_sgemm(transA, transB, alpha, beta,
            (int)A_shape_[0], (int)A_shape_[1], A_data, (int)A_shape_[1], 1,
            (int)B_shape_[0], (int)B_shape_[1], B_data, (int)B_shape_[1], 1,
            out_data, (int)out_shape_[1], ntasks);
}

fun run_gemm(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_Gemm {alpha, beta, transA, transB, t_A, t_B, t_bias, t_out} =>
    val A = net.get_tensor(t_A), B = net.get_tensor(t_B)
    val bias = net.get_tensor(t_bias)
    val out = net.get_tensor(t_out)
    match (A.data, B.data, bias.data, out.data) {
    | (Ast.DL_Data_FP32 a_data, Ast.DL_Data_FP32 b_data,
       Ast.DL_Data_FP32 bias_data, Ast.DL_Data_FP32 out_data) =>
        run_gemm(a_data, A.shape, b_data, B.shape, bias_data, bias.shape,
                 out_data, out.shape, alpha, beta, transA, transB)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unsupported operation {op.name()}")
}
