/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include <alloca.h>
}

fun run_gemm(A: Ast.nntensor_t, B: Ast.nntensor_t, C: Ast.nntensor_t,
             out: Ast.nntensor_t, alpha: float, beta: float,
             transA: bool, transB: bool, ntasks: int): void
@ccode {
    const int_* A_shape = (const int_*)A->shape.shape.data;
    const int_* B_shape = (const int_*)B->shape.shape.data;
    const int_* C_shape = (const int_*)C->shape.shape.data;
    const int_* out_shape = (const int_*)out->shape.shape.data;
    const fx_arr_t* A_data = &A->data.u.NN_Data_I8;
    const fx_arr_t* B_data = &B->data.u.NN_Data_I8;
    const fx_arr_t* C_data = &C->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    int A_typ = A->data.tag, B_typ = B->data.tag;
    int C_typ = C->data.tag, out_typ = out->data.tag;
    int_ C_ndims = C_typ == FX_Notype ? 0 : C->shape.shape.dim[0].size;

    if ((A_typ != FX_F32 && A_typ != FX_F16) ||
        (B_typ != FX_F32 && B_typ != FX_F16) ||
        (C_typ != FX_F32 && C_typ != FX_F16 && C_typ != FX_Notype) ||
        (out_typ != FX_F32 && out_typ != FX_F16))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (A->shape.shape.dim[0].size != 2 ||
        B->shape.shape.dim[0].size != 2 ||
        out->shape.shape.dim[0].size != 2 ||
        (C_ndims != 2 && C_ndims != 1 && C_ndims != 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeError);

    if (C_ndims == 1 && (C_shape[0] != out_shape[1]))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    if (C_ndims == 2 &&
        (C_shape[0] != out_shape[0] ||
         C_shape[1] != out_shape[1]))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    if (C_ndims == 0 || beta == 0.f || C_typ == FX_Notype) {
        beta = 0.f;
    } else {
        int_ nrows = out_shape[0], ncols = out_shape[1];
        size_t out_esz = out_data->dim[1].step;
        // [TODO] provide convenient Ficus C API to convert
        // arrays from one type to another with broadcasting support
        for (int_ i = 0; i < nrows; i++) {
            const char* c_i_ = C_data->data + (C_ndims == 2 ? C_data->dim[0].step : 0)*i;
            char* out_i_ = out_data->data + out_data->dim[0].step*i;
            if (out_typ == C_typ) {
                if (out_i_ != c_i_)
                    memcpy(out_i_, c_i_, ncols*out_esz);
            } else if (C_typ == FX_F16 && out_typ == FX_F32) {
                const fx_f16* c_i = (const fx_f16*)c_i_;
                float* out_i = (float*)out_i_;
                for (int_ j = 0; j < ncols; j++)
                    out_i[j] = FX_FLOAT(c_i[j]);
            } else if (C_typ == FX_F32 && out_typ == FX_F16) {
                const float* c_i = (const float*)c_i_;
                fx_f16* out_i = (fx_f16*)out_i_;
                for (int_ j = 0; j < ncols; j++)
                    out_i[j] = FX_FLOAT16(c_i[j]);
            } else {
                return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
            }
        }
    }

    return fx_mpgemm(transA, transB, alpha, beta,
            A_shape[0], A_shape[1], A_typ, A_data->data, A_shape[1], 1,
            B_shape[0], B_shape[1], B_typ, B_data->data, B_shape[1], 1,
            out_typ, out_data->data, out_shape[1], (int)ntasks);
}

fun run_gemm(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Gemm {alpha, beta, transA, transB, t_A, t_B, t_bias, t_out} =>
    val A = model.get_tensor(t_A), B = model.get_tensor(t_B)
    val bias = model.get_tensor(t_bias)
    val out = model.get_tensor(t_out)
    run_gemm(A, B, bias, out, alpha, beta, transA, transB, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_qmatmul(A: Ast.nntensor_t, B: Ast.nntensor_t,
                A_scale: Ast.nntensor_t, A_zp: Ast.nntensor_t,
                B_scale: Ast.nntensor_t, B_zp: Ast.nntensor_t,
                out_scale: Ast.nntensor_t, out_zp: Ast.nntensor_t,
                out: Ast.nntensor_t, ntasks: int): void
@ccode {
    fx_arr_t* A_shape_ = &A->shape.shape;
    fx_arr_t* B_shape_ = &B->shape.shape;
    fx_arr_t* out_shape_ = &out->shape.shape;
    fx_arr_t* A_sc_shape_ = &A_scale->shape.shape;
    fx_arr_t* B_sc_shape_ = &B_scale->shape.shape;
    fx_arr_t* out_sc_shape_ = &out_scale->shape.shape;
    fx_arr_t* A_zp_shape_ = &A_zp->shape.shape;
    fx_arr_t* B_zp_shape_ = &B_zp->shape.shape;
    fx_arr_t* out_zp_shape_ = &out_zp->shape.shape;
    int A_typ = A->data.tag, B_typ = B->data.tag, out_typ = out->data.tag;
    int A_sc_typ = A_scale->data.tag, A_zp_typ = A_zp->data.tag;
    int B_sc_typ = B_scale->data.tag, B_zp_typ = B_zp->data.tag;
    int out_sc_typ = out_scale->data.tag, out_zp_typ = out_zp->data.tag;
    fx_arr_t* A_data = &A->data.u.NN_Data_I8;
    fx_arr_t* B_data = &B->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    fx_arr_t* A_sc_data = &A_scale->data.u.NN_Data_I8;
    fx_arr_t* B_sc_data = &B_scale->data.u.NN_Data_I8;
    fx_arr_t* out_sc_data = &out_scale->data.u.NN_Data_I8;
    fx_arr_t* A_zp_data = &A_zp->data.u.NN_Data_I8;
    fx_arr_t* B_zp_data = &B_zp->data.u.NN_Data_I8;
    fx_arr_t* out_zp_data = &out_zp->data.u.NN_Data_I8;
    const uint8_t* Aptr0 = (const uint8_t*)A_data->data;
    const uint8_t* Bptr0 = (const uint8_t*)B_data->data;
    uint8_t* outptr0 = (uint8_t*)out_data->data;
    int A_mask = A_typ == FX_I8 ? 128 : 0;
    int B_mask = B_typ == FX_I8 ? 128 : 0;
    int out_mask = out_typ == FX_I8 ? 128 : 0;
    int_ M, N, K, nsubtasks = 1, Nblocks = 1;
    int_ A_nscales, B_nscales, out_nscales;

    if ((A_typ != FX_I8 && A_typ != FX_U8) ||
        (B_typ != FX_I8 && B_typ != FX_U8) ||
        (out_typ != FX_I8 && out_typ != FX_U8))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if ((A_sc_typ != FX_F32 && A_sc_typ != FX_F16) ||
        (B_sc_typ != FX_F32 && B_sc_typ != FX_F16) ||
        (out_sc_typ != FX_F32 && out_sc_typ != FX_F16))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if ((A_zp_typ != A_typ) || (B_zp_typ != B_typ) || (out_zp_typ != out_typ))
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (A_shape_->dim[0].size != 2 || B_shape_->dim[0].size != 2 ||
        out_shape_->dim[0].size != 2)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    M = ((int_*)out_shape_->data)[0];
    N = ((int_*)out_shape_->data)[1];
    K = ((int_*)A_shape_->data)[1];
    if (M != ((int_*)A_shape_->data)[0] ||
        N != ((int_*)B_shape_->data)[1] ||
        K != ((int_*)B_shape_->data)[0])
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    A_nscales = A_sc_data->dim[0].size;
    B_nscales = B_sc_data->dim[0].size;
    out_nscales = out_sc_data->dim[0].size;
    if ((A_nscales != 1 && A_nscales != M) ||
        A_nscales != A_zp_data->dim[0].size ||
        (B_nscales != 1 && B_nscales != N) ||
        B_nscales != B_zp_data->dim[0].size ||
        out_nscales != 1 ||
        out_nscales != out_zp_data->dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    if (ntasks > 1 && (uint64_t)M*N*K >= 100000) {
        if (M < ntasks)
            Nblocks = ntasks/M;
    } else {
        ntasks = 1;
    }
    nsubtasks = M*Nblocks;

    {
    double out_sc0 = 1./(out_sc_typ == FX_F32 ? *(float*)out_sc_data->data :
                                  FX_FLOAT(*(fx_f16*)out_sc_data->data));
    int out_zp0 = out_zp_typ == FX_I8 ? *(int8_t*)out_zp_data->data + 128 :
                                        (int)*(uint8_t*)out_zp_data->data;
    float* A_sc_buf = (float*)alloca((M + N)*2*sizeof(A_sc_buf[0]));
    float* B_sc_buf = A_sc_buf + M;
    int* A_zp_buf = (int*)(B_sc_buf + N);
    int* B_zp_buf = A_zp_buf + M;
    if (A_nscales == 1) {
        float A_sc0 = A_sc_typ == FX_F32 ? *(float*)A_sc_data->data :
                                  FX_FLOAT(*(fx_f16*)A_sc_data->data);
        int A_zp0 = A_zp_typ == FX_I8 ? *(int8_t*)A_zp_data->data + 128 :
                                        (int)*(uint8_t*)A_zp_data->data;
        for (int_ i = 0; i < M; i++) {
            A_sc_buf[i] = A_sc0;
            A_zp_buf[i] = A_zp0;
        }
    } else {
        for (int_ i = 0; i < M; i++) {
            float A_sci = A_sc_typ == FX_F32 ? ((float*)A_sc_data->data)[i] :
                                  FX_FLOAT(((fx_f16*)A_sc_data->data)[i]);
            int A_zpi = A_zp_typ == FX_I8 ? ((int8_t*)A_zp_data->data)[i] + 128 :
                                        (int)((uint8_t*)A_zp_data->data)[i];
            A_sc_buf[i] = A_sci;
            A_zp_buf[i] = A_zpi;
        }
    }
    if (B_nscales == 1) {
        float B_sc0 = B_sc_typ == FX_F32 ? *(float*)B_sc_data->data :
                                  FX_FLOAT(*(fx_f16*)B_sc_data->data);
        int B_zp0 = B_zp_typ == FX_I8 ? *(int8_t*)B_zp_data->data + 128 :
                                        (int)*(uint8_t*)B_zp_data->data;
        for (int_ j = 0; j < N; j++) {
            B_sc_buf[j] = B_sc0;
            B_zp_buf[j] = B_zp0;
        }
    } else {
        for (int_ j = 0; j < N; j++) {
            float B_scj = B_sc_typ == FX_F32 ? ((float*)B_sc_data->data)[j] :
                                  FX_FLOAT(((fx_f16*)B_sc_data->data)[j]);
            int B_zpj = B_zp_typ == FX_I8 ? ((int8_t*)B_zp_data->data)[j] + 128 :
                                        (int)((uint8_t*)B_zp_data->data)[j];
            B_sc_buf[j] = B_scj;
            B_zp_buf[j] = B_zpj;
        }
    }
    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++)
    {
        /*
        A[i, j] = sum_k {(A[i, k] - A_zp[i])*A_sc[i]*
                            (B[k, j] - B_zp[j])*B_sc[j]} / out_scale + out_zp =
            (A_sc[i]*B_sc[j]/out_scale)*
            ((sum_k A[i,k]*B[k,j]) - A_zp[i]*(sum_k B[k, j]) -
              B_zp[j]*(sum_k A[i, k]) + K*A_zp[i]*B_zp[j]) + out_zp =
            (A_sc[i]*B_sc[j]/out_scale)*(sum_k {(A[i,k] - A_zp[i])*B[k,j]}) -
              B_zp[j]*(sum_k (A[i, k] - A_zp[i]))) + out_zp
        */
        int_ max_block_size = (N + Nblocks - 1)/Nblocks;
        int* buf = (int*)alloca(max_block_size*sizeof(buf[0]));
        int_ tile0 = task_id*nsubtasks/ntasks;
        int_ tile1 = (task_id + 1)*nsubtasks/ntasks;
        for( ; tile0 < tile1; tile0++ ) {
            int_ i = tile0/Nblocks;
            int_ nb = tile0 - i*Nblocks;
            int_ j0 = nb*N/Nblocks, j1 = (nb+1)*N/Nblocks;
            int_ j, k;
            uint8_t* out_i = outptr0 + i*N;
            const uint8_t* A_i = Aptr0 + i*K;
            float A_sc_i = A_sc_buf[i];
            int A_zp_i = A_zp_buf[i];
            int A_sum = 0;
            memset(buf, 0, (j1 - j0)*sizeof(buf[0]));
            for( k = 0; k < K; k++ ) {
                const uint8_t* b_k = Bptr0 + k*N;
                int aval = (A_i[k] ^ A_mask) - A_zp_i;
                A_sum += aval;
                for( j = j0; j < j1; j++ )
                    buf[j - j0] += aval*(b_k[j] ^ B_mask);
            }
            double A_i_out = A_sc_i*out_sc0;

            #undef FX_SATURATE
            #define FX_SATURATE(x, mask) \
                (uint8_t)((((x) & ~255) == 0 ? (x) : (x) < 0 ? 0 : 255) ^ (mask))

            for (j = j0; j < j1; j++) {
                double out_ij = A_i_out*(B_sc_buf[j]*(buf[j - j0] -
                    (double)A_sum*B_zp_buf[j])) + out_zp0;
                int iout_ij = (int)lrint(out_ij);
                out_i[j] = FX_SATURATE(iout_ij, out_mask);
            }
        }
    }
    }

    return FX_OK;
}

fun run_qmatmul(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_QLinearMatMul {t_A, t_B, t_A_scale, t_A_zp, t_B_scale, t_B_zp,
                        t_out_scale, t_out_zp, t_out} =>
    val A = model.get_tensor(t_A), B = model.get_tensor(t_B)
    val A_scale = model.get_tensor(t_A_scale)
    val A_zp = model.get_tensor(t_A_zp)
    val B_scale = model.get_tensor(t_B_scale)
    val B_zp = model.get_tensor(t_B_zp)
    val out_scale = model.get_tensor(t_out_scale)
    val out_zp = model.get_tensor(t_out_zp)
    val out = model.get_tensor(t_out)
    run_qmatmul(A, B, A_scale, A_zp, B_scale, B_zp,
                out_scale, out_zp, out, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
