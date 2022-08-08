/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include "ficus_nn_common.h"
}

fun run_quantize(inp: Ast.nntensor_t, scale: Ast.nntensor_t, zp: Ast.nntensor_t,
                 out: Ast.nntensor_t, axis: int, ntasks: int): void
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
    int sc_typ = scale->data.tag, zp_typ = zp_data->data ? zp->data.tag : FX_U8;
    int_ N, C, NC, plane_size = 1;
    int_ ndims = inp_shape_->dim[0].size;
    int zp_delta = zp_typ == FX_I8 ? 128 : 0;

    if (inp_typ != FX_F32 && inp_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if ((out_typ != FX_I8 && out_typ != FX_U8) || zp_typ != out_typ)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (sc_typ != FX_F32 && sc_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims < 3)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    if (ndims != out_shape_->dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (axis < 0)
        axis += ndims;
    if (axis != 1)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    N = inp_shape[0];
    C = inp_shape[axis];
    if ((sc_data->dim[0].size != 1 && sc_data->dim[0].size != C) ||
        (zp_data->data != 0 && zp_data->dim[0].size != sc_data->dim[0].size))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    for (int_ i = 0; i < ndims; i++) {
        if (inp_shape[i] != out_shape[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (i != 0 && i != axis) plane_size *= inp_shape[i];
    }
    NC = N*C;
    if (plane_size*NC < 100000) ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ nc0 = task_id*NC/ntasks, nc1 = (task_id+1)*NC/ntasks;
        for (; nc0 < nc1; nc0++) {
            int_ c = nc0 % C;
            int_ sc_c = sc_data->dim[0].size == 1 ? 0 : c;
            float sc = 1.f/(sc_typ == FX_F32 ? ((float*)sc_data->data)[sc_c] :
                        FX_FLOAT(((fx_f16*)sc_data->data)[sc_c]));
            int zp = !zp_data->data ? 0 :
                zp_typ == FX_I8 ? ((int8_t*)zp_data->data)[sc_c] + zp_delta :
                                  (int)(((uint8_t*)zp_data->data)[sc_c]);
            uint8_t* outptr = (uint8_t*)out_data->data + nc0*plane_size;
            if (inp_typ == FX_F32) {
                const float* inptr = (const float*)inp_data->data + nc0*plane_size;
                int j = 0;
            #ifdef __ARM_NEON
                float32x4_t vscale = vdupq_n_f32(sc);
                int32x4_t vzp = vdupq_n_s32(zp);
                uint8x8_t vmask = vdup_n_u8((uint8_t)zp_delta);

                for (; j < plane_size; j += 8) {
                    if (j + 8 > plane_size) {
                        if (j == 0) break;
                        j = plane_size - 8;
                    }
                    float32x4_t v0 = vld1q_f32(inptr + j);
                    float32x4_t v1 = vld1q_f32(inptr + j + 4);
                    int32x4_t i0 = vcvtnq_s32_f32(vmulq_f32(v0, vscale));
                    int32x4_t i1 = vcvtnq_s32_f32(vmulq_f32(v1, vscale));
                    uint16x4_t w0 = vqmovun_s32(vaddq_s32(i0, vzp));
                    uint16x4_t w1 = vqmovun_s32(vaddq_s32(i1, vzp));
                    uint8x8_t b = vqmovn_u16(vcombine_u16(w0, w1));
                    b = veor_u8(b, vmask);

                    vst1_u8(outptr + j, b);
                }
            #endif
                for (; j < plane_size; j++) {
                    int v = (int)lrint(inptr[j]*sc) + zp;
                    uint8_t b = (uint8_t)(((v & ~255) == 0 ? v : v < 0 ? 0 : 255) ^ zp_delta);
                    outptr[j] = b;
                }
            } else {
                const fx_f16* inptr = (const fx_f16*)inp_data->data + nc0*plane_size;
                int j = 0;
            #ifdef __ARM_NEON
                float32x4_t vscale = vdupq_n_f32(sc);
                int32x4_t vzp = vdupq_n_s32(zp);
                uint8x8_t vmask = vdup_n_u8((uint8_t)zp_delta);

                for (; j < plane_size; j += 8) {
                    if (j + 8 > plane_size) {
                        if (j == 0) break;
                        j = plane_size - 8;
                    }
                    float16x8_t v = vld1q_f16(inptr + j);
                    float32x4_t v0 = vcvt_f32_f16(vget_low_f16(v));
                    float32x4_t v1 = vcvt_f32_f16(vget_high_f16(v));
                    int32x4_t i0 = vcvtnq_s32_f32(vmulq_f32(v0, vscale));
                    int32x4_t i1 = vcvtnq_s32_f32(vmulq_f32(v1, vscale));
                    uint16x4_t w0 = vqmovun_s32(vaddq_s32(i0, vzp));
                    uint16x4_t w1 = vqmovun_s32(vaddq_s32(i1, vzp));
                    uint8x8_t b = vqmovn_u16(vcombine_u16(w0, w1));
                    b = veor_u8(b, vmask);

                    vst1_u8(outptr + j, b);
                }
            #endif
                for (; j < plane_size; j++) {
                    int v = (int)lrint(FX_FLOAT(inptr[j])*sc) + zp;
                    uint8_t b = (uint8_t)(((v & ~255) == 0 ? v : v < 0 ? 0 : 255) ^ zp_delta);
                    outptr[j] = b;
                }
            }
        }
    }
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

fun run_dequantize(inp: Ast.nntensor_t, scale: Ast.nntensor_t, zp: Ast.nntensor_t,
                   out: Ast.nntensor_t, axis: int, ntasks: int): void
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
    int_ N, C, NC, plane_size = 1;
    int_ ndims = inp_shape_->dim[0].size;
    int zp_delta = inp_typ == FX_I8 ? 128 : 0;

    if (inp_typ != FX_I8 && inp_typ != FX_U8)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (out_typ != FX_F32 && out_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (sc_typ != FX_F32 && sc_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims < 3)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    if (ndims != out_shape_->dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (axis < 0)
        axis += ndims;
    if (axis != 1)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    N = inp_shape[0];
    C = inp_shape[axis];
    if (sc_data->dim[0].size != 1 && sc_data->dim[0].size != C)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (zp_data->data != 0) {
        if (zp_typ != inp_typ)
            return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
        if(zp_data->dim[0].size != sc_data->dim[0].size)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    }
    for (int_ i = 0; i < ndims; i++) {
        if (inp_shape[i] != out_shape[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (i != 0 && i != axis) plane_size *= inp_shape[i];
    }
    NC = N*C;
    if (plane_size*NC < 100000) ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ nc0 = task_id*NC/ntasks, nc1 = (task_id+1)*NC/ntasks;
        for (; nc0 < nc1; nc0++) {
            int_ c = nc0 % C;
            int_ sc_c = sc_data->dim[0].size == 1 ? 0 : c;
            float sc = sc_typ == FX_F32 ? ((float*)sc_data->data)[sc_c] :
                        FX_FLOAT(((fx_f16*)sc_data->data)[sc_c]);
            int zp = !zp_data->data ? zp_delta :
                zp_typ == FX_I8 ? ((int8_t*)zp_data->data)[sc_c] + zp_delta :
                                  (int)(((uint8_t*)zp_data->data)[sc_c]);
            const uint8_t* inptr = (uint8_t*)inp_data->data + nc0*plane_size;
            if (out_typ == FX_F32) {
                float* outptr = (float*)out_data->data + nc0*plane_size;
                int j = 0;
            #ifdef __ARM_NEON
                float32x4_t vscale = vdupq_n_f32(sc);
                int16x8_t vzp = vdupq_n_s16(zp);
                uint8x8_t vmask = vdup_n_u8((uint8_t)zp_delta);

                for (; j < plane_size; j += 8) {
                    if (j + 8 > plane_size) {
                        if (j == 0) break;
                        j = plane_size - 8;
                    }
                    uint8x8_t b = veor_u8(vld1_u8(inptr + j), vmask);
                    int16x8_t w = vsubq_s16(vreinterpretq_s16_u16(vmovl_u8(b)), vzp);
                    int32x4_t i0 = vmovl_s16(vget_low_s16(w));
                    int32x4_t i1 = vmovl_high_s16(w);
                    float32x4_t v0 = vcvtq_f32_s32(i0);
                    float32x4_t v1 = vcvtq_f32_s32(i1);
                    v0 = vmulq_f32(v0, vscale);
                    v1 = vmulq_f32(v1, vscale);
                    vst1q_f32(outptr + j, v0);
                    vst1q_f32(outptr + j + 4, v1);
                }
            #endif
                for (; j < plane_size; j++) {
                    float v = ((inptr[j]^zp_delta) - zp)*sc;
                    outptr[j] = v;
                }
            } else {
                fx_f16* outptr = (fx_f16*)out_data->data + nc0*plane_size;
                int j = 0;
            #ifdef __ARM_NEON
                float32x4_t vscale = vdupq_n_f32(sc);
                int16x8_t vzp = vdupq_n_s16(zp);
                uint8x8_t vmask = vdup_n_u8((uint8_t)zp_delta);

                for (; j < plane_size; j += 8) {
                    if (j + 8 > plane_size) {
                        if (j == 0) break;
                        j = plane_size - 8;
                    }
                    uint8x8_t b = veor_u8(vld1_u8(inptr + j), vmask);
                    int16x8_t w = vsubq_s16(vreinterpretq_s16_u16(vmovl_u8(b)), vzp);
                    int32x4_t i0 = vmovl_s16(vget_low_s16(w));
                    int32x4_t i1 = vmovl_high_s16(w);
                    float32x4_t v0 = vcvtq_f32_s32(i0);
                    float32x4_t v1 = vcvtq_f32_s32(i1);
                    v0 = vmulq_f32(v0, vscale);
                    v1 = vmulq_f32(v1, vscale);
                    float16x8_t v = vcombine_f16(vcvt_f16_f32(v0), vcvt_f16_f32(v1));
                    vst1q_f16(outptr + j, v);
                }
            #endif
                for (; j < plane_size; j++) {
                    float v = ((inptr[j]^zp_delta) - zp)*sc;
                    outptr[j] = FX_FLOAT16(v);
                }
            }
        }
    }
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

fun run_qbinary(el_op_: Ast.nnelwise_t, inp1: Ast.nntensor_t,
                inp2: Ast.nntensor_t, out: Ast.nntensor_t,
                inp1_scale: Ast.nntensor_t, inp1_zp: Ast.nntensor_t,
                inp2_scale: Ast.nntensor_t, inp2_zp: Ast.nntensor_t,
                out_scale: Ast.nntensor_t, out_zp: Ast.nntensor_t,
                ntasks: int): void
@ccode {
    enum {_FX_ELEMWISE_MAX_DIMS=5};
    int_ shape1[_FX_ELEMWISE_MAX_DIMS], shape2[_FX_ELEMWISE_MAX_DIMS], shape[_FX_ELEMWISE_MAX_DIMS];
    size_t step1[_FX_ELEMWISE_MAX_DIMS], step2[_FX_ELEMWISE_MAX_DIMS], step[_FX_ELEMWISE_MAX_DIMS];
    fx_arr_t* inp1_shape_ = &inp1->shape.shape;
    fx_arr_t* inp2_shape_ = &inp2->shape.shape;
    fx_arr_t* out_shape_ = &out->shape.shape;
    fx_arr_t* inp1_sc_shape_ = &inp1_scale->shape.shape;
    fx_arr_t* inp2_sc_shape_ = &inp2_scale->shape.shape;
    fx_arr_t* out_sc_shape_ = &out_scale->shape.shape;
    fx_arr_t* inp1_zp_shape_ = &inp1_zp->shape.shape;
    fx_arr_t* inp2_zp_shape_ = &inp2_zp->shape.shape;
    fx_arr_t* out_zp_shape_ = &out_zp->shape.shape;
    int el_op = el_op_->tag, inp_typ = inp1->data.tag;
    int inp2_typ = inp2->data.tag, out_typ = out->data.tag;
    fx_arr_t* inp1_data = &inp1->data.u.NN_Data_I8;
    fx_arr_t* inp2_data = &inp2->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    fx_arr_t* inp1_sc_data = &inp1_scale->data.u.NN_Data_I8;
    fx_arr_t* inp2_sc_data = &inp2_scale->data.u.NN_Data_I8;
    fx_arr_t* out_sc_data = &out_scale->data.u.NN_Data_I8;
    fx_arr_t* inp1_zp_data = &inp1_zp->data.u.NN_Data_I8;
    fx_arr_t* inp2_zp_data = &inp2_zp->data.u.NN_Data_I8;
    fx_arr_t* out_zp_data = &out_zp->data.u.NN_Data_I8;

    int all_ndims[] = {
        (int)inp1_shape_->dim[0].size,
        (int)inp2_shape_->dim[0].size,
        (int)out_shape_->dim[0].size
    };
    const int_* orig_shapes[] = {
        (int_*)inp1_shape_->data,
        (int_*)inp2_shape_->data,
        (int_*)out_shape_->data
    };
    int_* shapes[] = {shape1, shape2, shape};
    size_t* steps[] = {step1, step2, step};

    for (int i = 0; i < 3; i++) {
        if (all_ndims[i] > _FX_ELEMWISE_MAX_DIMS)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }

    // some of inputs are empty => result is empty
    if (!_fx_prepare_for_broadcast_op(3, _FX_ELEMWISE_MAX_DIMS,
                                  all_ndims, orig_shapes,
                                  shapes, steps))
        return FX_OK;

    /*{
    size_t dp1 = step1[_FX_ELEMWISE_MAX_DIMS-1];
    size_t dp2 = step2[_FX_ELEMWISE_MAX_DIMS-1];
    size_t dp = step[_FX_ELEMWISE_MAX_DIMS-1];
    size_t rowstep1 = step1[_FX_ELEMWISE_MAX_DIMS-2];
    size_t rowstep2 = step2[_FX_ELEMWISE_MAX_DIMS-2];
    size_t rowstep = step[_FX_ELEMWISE_MAX_DIMS-2];
    char* data1 = inp1_data->data;
    char* data2 = inp2_data->data;
    char* data = out_data->data;
    size_t esz1 = inp1_data->dim[0].step;
    size_t esz2 = inp2_data->dim[0].step;
    size_t esz = out_data->dim[0].step;
    int_ nrows = shape[_FX_ELEMWISE_MAX_DIMS-2];
    int_ ncols = shape[_FX_ELEMWISE_MAX_DIMS-1];
    size_t plane_idx, nplanes = 1;
    _fx_nn_elemwise_binary_func_t processing_func =
        _fx_get_elemwise_binary_func(el_op, inp_typ, inp2_typ);

    if (!processing_func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    for (int k = 0; k < _FX_ELEMWISE_MAX_DIMS-2; k++) nplanes *= shape[k];

    for (plane_idx = 0; plane_idx < nplanes; plane_idx++) {
        size_t ofs1 = 0, ofs2 = 0, ofs = 0;
        size_t idx = plane_idx;
        for (int k = _FX_ELEMWISE_MAX_DIMS-3; k >= 0; k--) {
            size_t prev_idx = idx/shape[k];
            size_t i_k = idx - prev_idx*shape[k];
            ofs1 += i_k*step1[k];
            ofs2 += i_k*step2[k];
            ofs += i_k*step[k];
            idx = prev_idx;
        }

        processing_func(data1 + ofs1*esz1, rowstep1, dp1,
                        data2 + ofs2*esz2, rowstep2, dp2,
                        data + ofs*esz, rowstep, dp,
                        nrows, ncols, param);
    }
    }*/

    return FX_OK;
}

fun run_qadd(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_QLinearAdd {t_A, t_A_scale, t_A_zp, t_B, t_B_scale, t_B_zp,
                    t_out_scale, t_out_zp, t_out} =>
    val A = model.get_tensor(t_A)
    val A_scale = model.get_tensor(t_A_scale)
    val A_zp = model.get_tensor(t_A_zp)
    val B = model.get_tensor(t_B)
    val B_scale = model.get_tensor(t_B_scale)
    val B_zp = model.get_tensor(t_B_zp)
    val out = model.get_tensor(t_out)
    val out_scale = model.get_tensor(t_out_scale)
    val out_zp = model.get_tensor(t_out_zp)
    run_qbinary(Ast.NN_Add, A, B, out, A_scale, A_zp,
                B_scale, B_zp, out_scale, out_zp, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
