/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

fun run_softmax(inp: 't [], inp_shape: Ast.nnshape_t, out: 't [])
{
    val N = inp_shape.shape[0], C = inp_shape.shape[1]
    @parallel for i <- 0:N {
        val ofs = i*C
        val fold maxval = inp[ofs] for j <- 1:C {max(maxval, inp[ofs+j])}
        var s = 0.
        val tab = [for j <- 0:C {val t = exp(inp[ofs+j] - maxval); s += t; t}]
        val s = (1/s :> 't)
        for j <- 0:C {out[ofs+j] = tab[j]*s}
    }
}

fun run_softmax(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_SoftMax {axis, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    assert(`inp.shape.shape.size() == 2`)
    assert(`axis == 1 || axis == -1`)
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_softmax(inp_data, inp.shape, out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

@private fun run_batchnorm(inp_shape: int [], inp_data: float [],
                           inp_mean_data: float [], inp_var_data: float [],
                           scale_data: float [], B_data: float [],
                           out_shape: int [], out_data: float [],
                           epsilon: float, ntasks: int): void
@ccode {
    int_ i, ndims = inp_shape->dim[0].size;
    const int_* inp_shape_ = (const int_*)inp_shape->data;
    int_ N = ndims > 0 ? inp_shape_[0] : 1;
    int_ C = ndims > 1 ? inp_shape_[1] : 1;
    int_ out_ndims = out_shape->dim[0].size;
    const int_* out_shape_ = (const int_*)out_shape->data;
    const float* inptr0 = (const float*)inp_data->data;
    float* outptr0 = (float*)out_data->data;
    const float* inp_mean = (const float*)inp_mean_data->data;
    const float* inp_var = (const float*)inp_var_data->data;
    const float* scales = (const float*)scale_data->data;
    const float* biases = (const float*)B_data->data;
    size_t planesize = 1;

    if (ndims != out_ndims || ndims < 2 ||
        inp_mean_data->dim[0].size != C ||
        inp_var_data->dim[0].size != C ||
        scale_data->dim[0].size != C ||
        B_data->dim[0].size != C)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    for (i = 0; i < ndims; i++) {
        if (inp_shape_[i] != out_shape_[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (i > 1) planesize *= (size_t)inp_shape_[i];
    }

    #pragma omp parallel for num_threads(ntasks)
    for (i = 0; i < N*C; i++) {
        int_ c = i % C;
        float alpha = scales[c]/sqrt(inp_var[c] + epsilon);
        float beta = biases[c] - inp_mean[c]*alpha;
        const float* inptr = inptr0 + i*planesize;
        float* outptr = outptr0 + i*planesize;
        for(size_t j = 0; j < planesize; j++)
            outptr[j] = inptr[j]*alpha + beta;
    }

    return FX_OK;
}

fun run_batchnorm(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_BatchNorm {epsilon, momentum,
        t_inp, t_scale, t_B, t_mean, t_var, t_out} =>
    val inp = model.get_tensor(t_out)
    val inp_mean = model.get_tensor(t_mean)
    val inp_var = model.get_tensor(t_var)
    val scale = model.get_tensor(t_scale)
    val B = model.get_tensor(t_B)
    val out = model.get_tensor(t_out)
    match (inp.data, inp_mean.data, inp_var.data, scale.data, B.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data,
       Ast.NN_Data_FP32 inp_mean_data, Ast.NN_Data_FP32 inp_var_data,
       Ast.NN_Data_FP32 scale_data, Ast.NN_Data_FP32 B_data,
       Ast.NN_Data_FP32 out_data) =>
        run_batchnorm(inp.shape.shape, inp_data, inp_mean_data,
                      inp_var_data, scale_data, B_data,
                      out.shape.shape, out_data, epsilon, *model.ntasks)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_gemm(A: float [], A_shape: Ast.nnshape_t,
             B: float [], B_shape: Ast.nnshape_t,
             bias: float [], bias_shape: Ast.nnshape_t,
             out: float [], out_shape: Ast.nnshape_t,
             alpha: float, beta: float,
             transA: bool, transB: bool, ntasks: int): void
@ccode {
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
            out_data, (int)out_shape_[1], (int)ntasks);
}

fun run_gemm(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Gemm {alpha, beta, transA, transB, t_A, t_B, t_bias, t_out} =>
    val A = model.get_tensor(t_A), B = model.get_tensor(t_B)
    val bias = model.get_tensor(t_bias)
    val out = model.get_tensor(t_out)
    match (A.data, B.data, bias.data, out.data) {
    | (Ast.NN_Data_FP32 a_data, Ast.NN_Data_FP32 b_data,
       Ast.NN_Data_FP32 bias_data, Ast.NN_Data_FP32 out_data) =>
        run_gemm(a_data, A.shape, b_data, B.shape, bias_data, bias.shape,
                 out_data, out.shape, alpha, beta, transA, transB, *model.ntasks)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
