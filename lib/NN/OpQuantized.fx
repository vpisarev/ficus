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
    if (ndims < 2)
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
                    int v = (int)lrintf(inptr[j]*sc) + zp;
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
                    int v = (int)lrintf(FX_FLOAT(inptr[j])*sc) + zp;
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
    if (ndims < 2)
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

@ccode {

typedef struct _fx_nn_qbinary_params_t
{
    float sc1, sc2, sc;
    int zp1, zp2, zp;
    int mask;
} _fx_nn_qbinary_params_t;

/*
    out[j] = saturate(((inp1[j] - zp1)*sc1 + (inp2[j] - zp2)*sc2)*sc + zp) =
        saturate(inp1[j]*(sc1*sc) + inp2[j]*(sc2*sc) +
                 (zp - zp1*(sc1*sc) - zp2*(sc2*sc)))
*/
static void _fx_nn_qadd_u8(const uint8_t* data1, size_t rowstep1, size_t dp1,
                           const uint8_t* data2, size_t rowstep2, size_t dp2,
                           uint8_t* data, size_t rowstep, size_t dp,
                           int_ nrows, int_ ncols,
                           const _fx_nn_qbinary_params_t* params)
{
    float sc = params->sc, sc1 = params->sc1*sc, sc2 = params->sc2*sc;
    int zp1 = params->zp1, zp2 = params->zp2, zp = params->zp;
    float bias = zp - sc1*zp1 - sc2*zp2;
    int mask = params->mask;
    for (int_ i = 0; i < nrows; i++) {
        const uint8_t* ptr1 = data1 + rowstep1*i;
        const uint8_t* ptr2 = data2 + rowstep2*i;
        uint8_t* ptr = (uint8_t*)data + rowstep*i;
        if (dp1 == 1 && dp2 == 1 && dp == 1) {
            int_ j = 0;
        #ifdef __ARM_NEON
            float32x4_t vsc1 = vdupq_n_f32(sc1), vsc2 = vdupq_n_f32(sc2);
            float32x4_t vbias = vdupq_n_f32(bias);
            uint8x8_t vmask = vdup_n_u8(mask);
            for(; j < ncols; j += 8) {
                if (j > ncols) {
                    if (j == 0)
                        break;
                    j = ncols - 8;
                }
                uint8x8_t x1 = veor_u8(vld1_u8(ptr1 + j), vmask);
                uint8x8_t x2 = veor_u8(vld1_u8(ptr2 + j), vmask);
                int16x8_t x1w = vreinterpretq_s16_u16(vmovl_u8(x1));
                int16x8_t x2w = vreinterpretq_s16_u16(vmovl_u8(x2));
                float32x4_t x1f, x2f, y0, y1;
                x1f = vcvtq_f32_s32(vmovl_s16(vget_low_s16(x1w)));
                x2f = vcvtq_f32_s32(vmovl_s16(vget_low_s16(x2w)));
                y0 = vfmaq_f32(vbias, x1f, vsc1);
                y0 = vfmaq_f32(y0, x2f, vsc2);
                x1f = vcvtq_f32_s32(vmovl_high_s16(x1w));
                x2f = vcvtq_f32_s32(vmovl_high_s16(x2w));
                y1 = vfmaq_f32(vbias, x1f, vsc1);
                y1 = vfmaq_f32(y1, x2f, vsc2);
                int32x4_t y0i = vcvtnq_s32_f32(y0);
                int32x4_t y1i = vcvtnq_s32_f32(y1);
                uint16x8_t yw = vcombine_u16(vqmovun_s32(y0i), vqmovun_s32(y1i));
                uint8x8_t y = veor_u8(vqmovn_u16(yw), vmask);
                vst1_u8(ptr + j, y);
            }
        #endif
            for(; j < ncols; j++) {
                int x1 = ptr1[j] ^ mask, x2 = ptr2[j] ^ mask;
                int y = (int)lrintf(x1*sc1 + x2*sc2 + bias);
                ptr[j] = FX_SATURATE(y, mask);
            }
        } else if (dp1 == 1 && dp2 == 0 && dp == 1) {
            float x2_sc2_bias = (*ptr2 ^ mask)*sc2 + bias;
            for(int_ j = 0; j < ncols; j++) {
                int x1 = ptr1[j] ^ mask;
                int y = (int)lrintf(x1*sc1 + x2_sc2_bias);
                ptr[j] = FX_SATURATE(y, mask);
            }
        } else if (dp1 == 0 && dp2 == 1 && dp == 1) {
            float x1_sc1_bias = (*ptr1 ^ mask)*sc1 + bias;
            for(int_ j = 0; j < ncols; j++) {
                int x2 = ptr2[j] ^ mask;
                int y = (int)lrintf(x2*sc2 + x1_sc1_bias);
                ptr[j] = FX_SATURATE(y, mask);
            }
        } else {
            for(int_ j = 0; j < ncols; j++, ptr1 += dp1, ptr2 += dp2, ptr += dp) {
                int x1 = *ptr1 ^ mask, x2 = *ptr2 ^ mask;
                int y = (int)lrintf(x1*sc1 + x2*sc2 + bias);
                *ptr = FX_SATURATE(y, mask);
            }
        }
    }
}

typedef void (*_fx_nn_qbinary_func_t)(
    const uint8_t* data1, size_t rowstep1, size_t dp1,
    const uint8_t* data2, size_t rowstep2, size_t dp2,
    uint8_t* data, size_t rowstep, size_t dp,
    int_ nrows, int_ ncols,
    const _fx_nn_qbinary_params_t* params);

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
    int el_op = el_op_->tag, inp1_typ = inp1->data.tag;
    int inp2_typ = inp2->data.tag, out_typ = out->data.tag;
    int sc1_typ = inp1_scale->data.tag, zp1_typ = inp1_zp->data.tag;
    int sc2_typ = inp2_scale->data.tag, zp2_typ = inp2_zp->data.tag;
    int sc_typ = out_scale->data.tag, zp_typ = out_zp->data.tag;
    fx_arr_t* inp1_data = &inp1->data.u.NN_Data_I8;
    fx_arr_t* inp2_data = &inp2->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    fx_arr_t* sc1_data = &inp1_scale->data.u.NN_Data_I8;
    fx_arr_t* sc2_data = &inp2_scale->data.u.NN_Data_I8;
    fx_arr_t* sc_data = &out_scale->data.u.NN_Data_I8;
    fx_arr_t* zp1_data = &inp1_zp->data.u.NN_Data_I8;
    fx_arr_t* zp2_data = &inp2_zp->data.u.NN_Data_I8;
    fx_arr_t* zp_data = &out_zp->data.u.NN_Data_I8;
    int mask = inp1_typ == FX_I8 ? 128 : 0;

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

    int zp_delta = inp1_typ == FX_I8 ? 128 : 0;

    if ((inp1_typ != FX_I8 && inp1_typ != FX_U8) ||
        (inp2_typ != FX_I8 && inp2_typ != FX_U8) ||
        (out_typ != FX_I8 && out_typ != FX_U8))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if ((sc1_typ != FX_F32 && sc1_typ != FX_F16) ||
        (sc2_typ != FX_F32 && sc2_typ != FX_F16) ||
        (sc_typ != FX_F32 && sc_typ != FX_F16))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (inp1_typ != inp2_typ || inp1_typ != out_typ)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if ((zp1_typ > 1 && zp1_typ != inp1_typ) ||
        (zp2_typ > 1 && zp2_typ != inp2_typ) ||
        (zp_typ > 1 && zp_typ != out_typ))
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (sc1_data->dim[0].size != 1 || sc2_data->dim[0].size != 1 ||
        sc_data->dim[0].size != 1)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if ((zp1_data->data && zp1_data->dim[0].size != 1) ||
        (zp2_data->data && zp2_data->dim[0].size != 1) ||
        (zp_data->data && zp_data->dim[0].size != 1))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    for (int i = 0; i < 3; i++) {
        if (all_ndims[i] > _FX_ELEMWISE_MAX_DIMS)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }

    // some of inputs are empty => result is empty
    if (!_fx_prepare_for_broadcast_op(3, _FX_ELEMWISE_MAX_DIMS,
                                  all_ndims, orig_shapes,
                                  shapes, steps))
        return FX_OK;

    {
    size_t dp1 = step1[_FX_ELEMWISE_MAX_DIMS-1];
    size_t dp2 = step2[_FX_ELEMWISE_MAX_DIMS-1];
    size_t dp = step[_FX_ELEMWISE_MAX_DIMS-1];
    size_t rowstep1 = step1[_FX_ELEMWISE_MAX_DIMS-2];
    size_t rowstep2 = step2[_FX_ELEMWISE_MAX_DIMS-2];
    size_t rowstep = step[_FX_ELEMWISE_MAX_DIMS-2];
    const uint8_t* data1 = (const uint8_t*)inp1_data->data;
    const uint8_t* data2 = (const uint8_t*)inp2_data->data;
    uint8_t* data = (uint8_t*)out_data->data;
    size_t esz1 = inp1_data->dim[0].step;
    size_t esz2 = inp2_data->dim[0].step;
    size_t esz = out_data->dim[0].step;
    int_ nrows = shape[_FX_ELEMWISE_MAX_DIMS-2];
    int_ ncols = shape[_FX_ELEMWISE_MAX_DIMS-1];
    size_t plane_idx, nplanes = 1;
    _fx_nn_qbinary_func_t processing_func =
        el_op == _FX_NN_Add ? _fx_nn_qadd_u8 : 0;
    _fx_nn_qbinary_params_t params;
    params.sc1 = sc1_typ == FX_F32 ? *(float*)sc1_data->data :
                            FX_FLOAT(*(fx_f16*)sc1_data->data);
    params.sc2 = sc2_typ == FX_F32 ? *(float*)sc2_data->data :
                            FX_FLOAT(*(fx_f16*)sc2_data->data);
    params.sc = 1.f/(sc1_typ == FX_F32 ? *(float*)sc_data->data :
                           FX_FLOAT(*(fx_f16*)sc_data->data));
    params.zp1 = zp1_data->data == 0 ? mask :
                 zp1_typ == FX_I8 ? *((int8_t*)zp1_data->data) + mask :
                               (int)*((uint8_t*)zp1_data->data);
    params.zp2 = zp2_data->data == 0 ? mask :
                 zp2_typ == FX_I8 ? *((int8_t*)zp2_data->data) + mask :
                            (int)*((uint8_t*)zp2_data->data);
    params.zp = zp_data->data == 0 ? mask :
                zp_typ == FX_I8 ? *((int8_t*)zp_data->data) + mask :
                            (int)*((uint8_t*)zp_data->data);
    params.mask = mask;

    if (!processing_func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    for (int k = 0; k < _FX_ELEMWISE_MAX_DIMS-2; k++) nplanes *= shape[k];

    #pragma omp parallel for num_threads(ntasks) if (nplanes >= ntasks)
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
                        nrows, ncols, &params);
    }
    }

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

fun run_qglobal_avgpool(inp: Ast.nntensor_t,
                        inp_scale: Ast.nntensor_t, inp_zp: Ast.nntensor_t,
                        out_scale: Ast.nntensor_t, out_zp: Ast.nntensor_t,
                        out: Ast.nntensor_t, channels_last: bool, ntasks: int): void
@ccode {
    int inp_typ = inp->data.tag, out_typ = out->data.tag;
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    const int_* inp_shape = (const int_*)inp->shape.shape.data;
    const int_* out_shape = (const int_*)out->shape.shape.data;
    fx_arr_t* inp_sc_shape_ = &inp_scale->shape.shape;
    fx_arr_t* out_sc_shape_ = &out_scale->shape.shape;
    fx_arr_t* inp_zp_shape_ = &inp_zp->shape.shape;
    fx_arr_t* out_zp_shape_ = &out_zp->shape.shape;
    int inp_sc_typ = inp_scale->data.tag, inp_zp_typ = inp_zp->data.tag;
    int out_sc_typ = out_scale->data.tag, out_zp_typ = out_zp->data.tag;
    fx_arr_t* inp_sc_data = &inp_scale->data.u.NN_Data_I8;
    fx_arr_t* out_sc_data = &out_scale->data.u.NN_Data_I8;
    fx_arr_t* inp_zp_data = &inp_zp->data.u.NN_Data_I8;
    fx_arr_t* out_zp_data = &out_zp->data.u.NN_Data_I8;
    int_ inp_ndims = inp->shape.shape.dim[0].size;
    int_ out_ndims = out->shape.shape.dim[0].size;
    int_ NC, planesize = 1;
    int mask = inp_typ == FX_I8 ? 128 : 0;
    double avg_scale, inp_scale0, out_scale0, bias, inp_out_scale;
    int inp_zp0, out_zp0;

    if (channels_last)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (inp_typ != out_typ || (inp_typ != FX_I8 && inp_typ != FX_U8))
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (inp_ndims < 3 || inp_ndims != out_ndims)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if ((inp_sc_typ != FX_F32 && inp_sc_typ != FX_F16) ||
        (out_sc_typ != FX_F32 && out_sc_typ != FX_F16))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if ((inp_zp_typ > 1 && inp_zp_typ != inp_typ) ||
        (out_zp_typ > 0 && out_zp_typ != out_typ))
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (inp_sc_data->dim[0].size != 1 || out_sc_data->dim[0].size != 1)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if ((inp_zp_data->data && inp_zp_data->dim[0].size != 1) ||
        (out_zp_data->data && out_zp_data->dim[0].size != 1))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    NC = inp_shape[0]*inp_shape[1];
    for (int_ i = 0; i < inp_ndims; i++) {
        if (i < 2) {
            if (inp_shape[i] != out_shape[i])
                return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
            continue;
        }
        planesize *= inp_shape[i];
        if (out_shape[i] != 1)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    }

    inp_scale0 = inp_sc_typ == FX_F32 ? *(float*)inp_sc_data->data :
                            FX_FLOAT(*(fx_f16*)inp_sc_data->data);
    out_scale0 = out_sc_typ == FX_F32 ? *(float*)out_sc_data->data :
                            FX_FLOAT(*(fx_f16*)out_sc_data->data);
    inp_zp0 = inp_zp_data->data == 0 ? mask :
              inp_zp_typ == FX_I8 ? *((int8_t*)inp_zp_data->data) + mask :
                               (int)*((uint8_t*)inp_zp_data->data);
    out_zp0 = out_zp_data->data == 0 ? mask :
              out_zp_typ == FX_I8 ? *((int8_t*)out_zp_data->data) + mask :
                            (int)*((uint8_t*)out_zp_data->data);
    inp_out_scale = inp_scale0 / out_scale0;
    avg_scale = planesize != 0 ? inp_out_scale/planesize : 0.;
    bias = out_zp0 - inp_zp0*inp_out_scale;
    /*printf("inp_typ=%d, planesize=%d, NC=%d, inp_shape=[%d, %d, %d, %d]\n",
        inp_typ, (int)planesize, (int)NC,
        (int)inp_shape[0], (int)inp_shape[1], (int)inp_shape[2], (int)inp_shape[3]);*/

    if (NC*planesize < 100000)
        ntasks = 1;
    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ nc0 = task_id*NC/ntasks, nc1 = (task_id + 1)*NC/ntasks;
        for (; nc0 < nc1; nc0++) {
            const uint8_t* inptr = (const uint8_t*)inp_data->data + nc0*planesize;
            uint8_t* outptr = (uint8_t*)out_data->data + nc0;
            const int_ block_size = 1<<20;
            int64_t total = 0;
            /* (sum_j ((inp[j] - inp_zp)*inp_sc))/(out_sc*planesize) + out_zp =
               (inp_sc/(out_sc*planesize)*((sum_j inp[j]) - inp_zp*planesize) + out_zp =
               inp_sc/(out_sc*planesize)*(sum_j inp[j]) + out_zp - inp_zp*inp_sc/out_sc */
            for (int_ j = 0; j < planesize; ) {
                int_ block_end = j + block_size < planesize ? j + block_size : planesize;
                int s = 0;
                for (; j < block_end; j++)
                    s += inptr[j];
                total += s;
            }
            int iavg = (int)lrint(total*avg_scale + bias);
            *outptr = FX_SATURATE(iavg, mask);
        }
    }
    return FX_OK;
}

fun run_qglobal_avgpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_QLinearGlobalAvgPool {channels_last, t_inp, t_inp_scale, t_inp_zp,
                               t_out_scale, t_out_zp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val inp_scale = model.get_tensor(t_inp_scale)
    val inp_zp = model.get_tensor(t_inp_zp)
    val out_scale = model.get_tensor(t_out_scale)
    val out_zp = model.get_tensor(t_out_zp)
    val out = model.get_tensor(t_out)
    run_qglobal_avgpool(inp, inp_scale, inp_zp, out_scale, out_zp,
                        out, channels_last, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
