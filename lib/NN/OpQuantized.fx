/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include "ficus_nn_common.h"
}

fun run_quantize(inp: Ast.nntensor_t, out: Ast.nntensor_t,
                scale: Ast.nntensor_t, zp: Ast.nntensor_t,
                axis: int, ntasks: int): void
@ccode {
    fx_arr_t* inp_shape_ = &inp->shape.shape;
    fx_arr_t* out_shape_ = &out->shape.shape;
    fx_arr_t* sc_shape_ = &scale->shape.shape;
    fx_arr_t* zp_shape_ = &zp->shape.shape;
    const int_* inp_shape = (const int_*)(inp_shape_->data);
    const int_* out_shape = (const int_*)(out_shape_->data);
    const int_* sc_shape = (const int_*)(sc_shape_->data);
    const int_* zp_shape = (const int_*)(zp_shape_->data);
    fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    fx_arr_t* sc_data = &scale->data.u.NN_Data_I8;
    fx_arr_t* zp_data = &zp->data.u.NN_Data_I8;
    int inp_typ = inp->data.tag, out_typ = out->data.tag;
    int sc_typ = scale->data.tag, zp_typ = zp->data.tag;
    int_ N, C, plane_size = 1;
    int_ ndims = inp_shape_->dim[0].size;

    /*if (inp->shape.layout.tag != _FX_NN_Layout_NCHW)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (ndims < 3)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);

    if (ndims != out_shape_->dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    if ((inp_typ != FX_F32 && inp_typ != FX_F16) || out_typ != inp_typ)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    N = inp_shape[0];
    C = inp_shape[1];
    for (int_ i = 0; i < ndims; i++) {
        if (inp_shape[i] != out_shape[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (i >= 2) plane_size *= inp_shape[i];
    }

    if (plane_size*N < 100000) ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++)
    {
        double* buf = (double*)alloca((C+1)*(sizeof(double) + sizeof(float)));
        double *sqsumbuf = buf;
        float* inpbuf = (float*)(sqsumbuf + C + 1);
        sqsumbuf[0] = 0.;
        int_ pix0 = task_id*(N*plane_size)/ntasks;
        int_ pix1 = (task_id+1)*(N*plane_size)/ntasks;
        for (; pix0 < pix1; pix0++) {
            int_ sample_idx = pix0 / plane_size;
            int_ ofs = pix0 - sample_idx * plane_size;
            ofs += sample_idx * (plane_size * C);

            if (inp_typ == FX_F32) {
                const float* inptr = (const float*)inp_data->data + ofs;
                float* outptr = (float*)out_data->data + ofs;
                double sq = 0;

                for (int_ c = 0; c < C; c++) {
                    float x = inptr[c*plane_size];
                    sq += x*x;
                    inpbuf[c] = x;
                    sqsumbuf[c+1] = sq;
                }
                if (b_0_75) {
                    for (int_ c = 0; c < C; c++) {
                        int_ c0 = c - dc0, c1 = c + dc1;
                        c0 = c0 >= 0 ? c0 : 0;
                        c1 = c1 <= C ? c1 : C;
                        float sqsum = (float)(sqsumbuf[c1] - sqsumbuf[c0]);
                        float x = bias + scale*sqsum;
                        outptr[c*plane_size] = (inpbuf[c]/x)*sqrtf(sqrtf(x));
                    }
                } else {
                    for (int_ c = 0; c < C; c++) {
                        int_ c0 = c - dc0, c1 = c + dc1;
                        c0 = c0 >= 0 ? c0 : 0;
                        c1 = c1 <= C ? c1 : C;
                        float sqsum = (float)(sqsumbuf[c1] - sqsumbuf[c0]);
                        float x = bias + scale*sqsum;
                        outptr[c*plane_size] = inpbuf[c]*powf(x, -beta);
                    }
                }
            } else {
                const fx_f16* inptr = (const fx_f16*)inp_data->data + ofs;
                fx_f16* outptr = (fx_f16*)out_data->data + ofs;
                double sq = 0;

                for (int_ c = 0; c < C; c++) {
                    float x = FX_FLOAT(inptr[c*plane_size]);
                    sq += x*x;
                    inpbuf[c] = x;
                    sqsumbuf[c+1] = sq;
                }
                if (b_0_75) {
                    for (int_ c = 0; c < C; c++) {
                        int_ c0 = c - dc0, c1 = c + dc1;
                        c0 = c0 >= 0 ? c0 : 0;
                        c1 = c1 <= C ? c1 : C;
                        float sqsum = (float)(sqsumbuf[c1] - sqsumbuf[c0]);
                        float x = bias + scale*sqsum;
                        outptr[c*plane_size] = FX_FLOAT16((inpbuf[c]/x)*sqrtf(sqrtf(x)));
                    }
                } else {
                    for (int_ c = 0; c < C; c++) {
                        int_ c0 = c - dc0, c1 = c + dc1;
                        c0 = c0 >= 0 ? c0 : 0;
                        c1 = c1 <= C ? c1 : C;
                        float sqsum = (float)(sqsumbuf[c1] - sqsumbuf[c0]);
                        float x = bias + scale*sqsum;
                        outptr[c*plane_size] = FX_FLOAT16(inpbuf[c]*powf(x, -beta));
                    }
                }
            }
        }
    }*/
    return FX_OK;
}

fun run_quantize(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_QuantizeLinear {axis, t_inp, t_scale, t_zp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val scale = model.get_tensor(t_scale)
    val zp = model.get_tensor(t_zp)
    run_quantize(inp, scale, zp, out, axis, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_dequantize(inp: Ast.nntensor_t, out: Ast.nntensor_t,
                   scale: Ast.nntensor_t, zp: Ast.nntensor_t,
                   axis: int, ntasks: int): void
@ccode {
    fx_arr_t* inp_shape_ = &inp->shape.shape;
    fx_arr_t* out_shape_ = &out->shape.shape;
    fx_arr_t* sc_shape_ = &scale->shape.shape;
    fx_arr_t* zp_shape_ = &zp->shape.shape;
    const int_* inp_shape = (const int_*)(inp_shape_->data);
    const int_* out_shape = (const int_*)(out_shape_->data);
    const int_* sc_shape = (const int_*)(sc_shape_->data);
    const int_* zp_shape = (const int_*)(zp_shape_->data);
    fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    fx_arr_t* sc_data = &scale->data.u.NN_Data_I8;
    fx_arr_t* zp_data = &zp->data.u.NN_Data_I8;
    int inp_typ = inp->data.tag, out_typ = out->data.tag;
    int sc_typ = scale->data.tag, zp_typ = zp->data.tag;
    int_ N, C, plane_size = 1;
    int_ ndims = inp_shape_->dim[0].size;

    /*if (inp->shape.layout.tag != _FX_NN_Layout_NCHW)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (ndims < 3)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);

    if (ndims != out_shape_->dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    if ((inp_typ != FX_F32 && inp_typ != FX_F16) || out_typ != inp_typ)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    N = inp_shape[0];
    C = inp_shape[1];
    for (int_ i = 0; i < ndims; i++) {
        if (inp_shape[i] != out_shape[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (i >= 2) plane_size *= inp_shape[i];
    }

    if (plane_size*N < 100000) ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++)
    {
        double* buf = (double*)alloca((C+1)*(sizeof(double) + sizeof(float)));
        double *sqsumbuf = buf;
        float* inpbuf = (float*)(sqsumbuf + C + 1);
        sqsumbuf[0] = 0.;
        int_ pix0 = task_id*(N*plane_size)/ntasks;
        int_ pix1 = (task_id+1)*(N*plane_size)/ntasks;
        for (; pix0 < pix1; pix0++) {
            int_ sample_idx = pix0 / plane_size;
            int_ ofs = pix0 - sample_idx * plane_size;
            ofs += sample_idx * (plane_size * C);

            if (inp_typ == FX_F32) {
                const float* inptr = (const float*)inp_data->data + ofs;
                float* outptr = (float*)out_data->data + ofs;
                double sq = 0;

                for (int_ c = 0; c < C; c++) {
                    float x = inptr[c*plane_size];
                    sq += x*x;
                    inpbuf[c] = x;
                    sqsumbuf[c+1] = sq;
                }
                if (b_0_75) {
                    for (int_ c = 0; c < C; c++) {
                        int_ c0 = c - dc0, c1 = c + dc1;
                        c0 = c0 >= 0 ? c0 : 0;
                        c1 = c1 <= C ? c1 : C;
                        float sqsum = (float)(sqsumbuf[c1] - sqsumbuf[c0]);
                        float x = bias + scale*sqsum;
                        outptr[c*plane_size] = (inpbuf[c]/x)*sqrtf(sqrtf(x));
                    }
                } else {
                    for (int_ c = 0; c < C; c++) {
                        int_ c0 = c - dc0, c1 = c + dc1;
                        c0 = c0 >= 0 ? c0 : 0;
                        c1 = c1 <= C ? c1 : C;
                        float sqsum = (float)(sqsumbuf[c1] - sqsumbuf[c0]);
                        float x = bias + scale*sqsum;
                        outptr[c*plane_size] = inpbuf[c]*powf(x, -beta);
                    }
                }
            } else {
                const fx_f16* inptr = (const fx_f16*)inp_data->data + ofs;
                fx_f16* outptr = (fx_f16*)out_data->data + ofs;
                double sq = 0;

                for (int_ c = 0; c < C; c++) {
                    float x = FX_FLOAT(inptr[c*plane_size]);
                    sq += x*x;
                    inpbuf[c] = x;
                    sqsumbuf[c+1] = sq;
                }
                if (b_0_75) {
                    for (int_ c = 0; c < C; c++) {
                        int_ c0 = c - dc0, c1 = c + dc1;
                        c0 = c0 >= 0 ? c0 : 0;
                        c1 = c1 <= C ? c1 : C;
                        float sqsum = (float)(sqsumbuf[c1] - sqsumbuf[c0]);
                        float x = bias + scale*sqsum;
                        outptr[c*plane_size] = FX_FLOAT16((inpbuf[c]/x)*sqrtf(sqrtf(x)));
                    }
                } else {
                    for (int_ c = 0; c < C; c++) {
                        int_ c0 = c - dc0, c1 = c + dc1;
                        c0 = c0 >= 0 ? c0 : 0;
                        c1 = c1 <= C ? c1 : C;
                        float sqsum = (float)(sqsumbuf[c1] - sqsumbuf[c0]);
                        float x = bias + scale*sqsum;
                        outptr[c*plane_size] = FX_FLOAT16(inpbuf[c]*powf(x, -beta));
                    }
                }
            }
        }
    }*/
    return FX_OK;
}

fun run_dequantize(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_DequantizeLinear {axis, t_inp, t_scale, t_zp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val scale = model.get_tensor(t_scale)
    val zp = model.get_tensor(t_zp)
    run_dequantize(inp, scale, zp, out, axis, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
