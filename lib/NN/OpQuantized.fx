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
    int esz = fx_elemsize(inp_typ);
    int sc_typ = scale->data.tag, zp_typ = zp_data->data ? zp->data.tag : FX_U8;
    int_ N, C, NC, C0, C1 = 1, plane_size = 1;
    int_ ndims = inp_shape_->dim[0].size;
    int out_mask = zp_typ == FX_I8 ? 128 : 0;

    if (inp_typ != FX_F32 && inp_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if ((out_typ != FX_I8 && out_typ != FX_U8) || zp_typ != out_typ)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (sc_typ != FX_F32 && sc_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims < 2)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    if (axis < 0)
        axis += ndims;
    if (axis != 1)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    N = inp_shape[0];
    C = inp_shape[axis];
    if (ndims == 2) {
        if (ndims != out_shape_->dim[0].size)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    } else {
        if (ndims+1 != out_shape_->dim[0].size)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        C1 = out_shape[ndims];
        if (C1 != 4)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }
    if ((sc_data->dim[0].size != 1 && sc_data->dim[0].size != C) ||
        (zp_data->data != 0 && zp_data->dim[0].size != sc_data->dim[0].size))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    for (int_ i = 0; i < ndims; i++) {
        if (i == axis) {
            if ((inp_shape[i] + C1 - 1)/C1 != out_shape[i])
                return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
            continue;
        }
        if (inp_shape[i] != out_shape[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (i != 0) plane_size *= inp_shape[i];
    }
    C0 = (C + C1 - 1)/C1;
    NC = N*C0;
    if (plane_size*NC < 100000) ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ nc0 = task_id*NC/ntasks, nc1 = (task_id+1)*NC/ntasks;
        for (; nc0 < nc1; nc0++) {
            int n = nc0 / C0;
            int_ c_out = nc0 % C0;
            int_ c0 = c_out*C1;
            float sc[4] = {0.f, 0.f, 0.f, 0.f};
            int j = 0, zp[4] = {0, 0, 0, 0};
            bool is_scalar_scale = sc_data->dim[0].size == 1;
            for (j = 0; j < C1; j++) {
                int_ sc_c = is_scalar_scale ? 0 : c0 + j;
                if (!is_scalar_scale && c0 + j >= C)
                    continue;
                sc[j] = 1.f/(sc_typ == FX_F32 ? ((float*)sc_data->data)[sc_c] :
                        FX_FLOAT(((fx_f16*)sc_data->data)[sc_c]));
                zp[j] = !zp_data->data ? 0 :
                        zp_typ == FX_I8 ? ((int8_t*)zp_data->data)[sc_c] + out_mask :
                                  (int)(((uint8_t*)zp_data->data)[sc_c]);
            }
            uint8_t* outptr = (uint8_t*)out_data->data + (n*C0 + c_out)*(plane_size*C1);
            int c0_1 = c0 + 1 < C ? c0 + 1 : C - 1;
            int c0_2 = c0 + 2 < C ? c0 + 2 : C - 1;
            int c0_3 = c0 + 3 < C ? c0 + 3 : C - 1;

            /*printf("nc0=%d. sc_c=%d, sc=%.5f, zp=%d, plane_size=%d, NC=%d, C=%d\n",
                   (int)nc0, (int)sc_c, sc, zp, (int)plane_size, (int)NC, (int)C);*/
            const char* inptr0 = inp_data->data + (n*C + c0)*plane_size*esz;
            const char* inptr1 = inp_data->data + (n*C + c0_1)*plane_size*esz;
            const char* inptr2 = inp_data->data + (n*C + c0_2)*plane_size*esz;
            const char* inptr3 = inp_data->data + (n*C + c0_3)*plane_size*esz;

            j = 0;
            if (C1 == 4) {
            #ifdef __ARM_NEON
                float32x4_t vscale = vld1q_f32(sc);
                int32x4_t vzp = vld1q_s32(zp);
                uint8x16_t vmask = vdupq_n_u8((uint8_t)out_mask);

                for (; j < plane_size; j += 4) {
                    if (j + 4 > plane_size) {
                        if (j == 0) break;
                        j = plane_size - 4;
                    }
                    float32x4_t v0, v1, v2, v3;
                    if (inp_typ == FX_F32) {
                        v0 = vld1q_f32((float*)inptr0 + j);
                        v1 = vld1q_f32((float*)inptr1 + j);
                        v2 = vld1q_f32((float*)inptr2 + j);
                        v3 = vld1q_f32((float*)inptr3 + j);
                    } else {
                        v0 = vcvt_f32_f16(vld1_f16((fx_f16*)inptr0 + j));
                        v1 = vcvt_f32_f16(vld1_f16((fx_f16*)inptr1 + j));
                        v2 = vcvt_f32_f16(vld1_f16((fx_f16*)inptr2 + j));
                        v3 = vcvt_f32_f16(vld1_f16((fx_f16*)inptr3 + j));
                    }

                    float32x4x2_t tr0, tr1;
                    tr0 = vtrnq_f32(v0, v1);
                    tr1 = vtrnq_f32(v2, v3);
                    v0 = vcombine_f32(vget_low_f32(tr0.val[0]), vget_low_f32(tr1.val[0]));
                    v1 = vcombine_f32(vget_low_f32(tr0.val[1]), vget_low_f32(tr1.val[1]));
                    v2 = vcombine_f32(vget_high_f32(tr0.val[0]), vget_high_f32(tr1.val[0]));
                    v3 = vcombine_f32(vget_high_f32(tr0.val[1]), vget_high_f32(tr1.val[1]));

                    int32x4_t i0 = vcvtnq_s32_f32(vmulq_f32(v0, vscale));
                    int32x4_t i1 = vcvtnq_s32_f32(vmulq_f32(v1, vscale));
                    int32x4_t i2 = vcvtnq_s32_f32(vmulq_f32(v2, vscale));
                    int32x4_t i3 = vcvtnq_s32_f32(vmulq_f32(v3, vscale));
                    uint16x4_t w0 = vqmovun_s32(vaddq_s32(i0, vzp));
                    uint16x4_t w1 = vqmovun_s32(vaddq_s32(i1, vzp));
                    uint16x4_t w2 = vqmovun_s32(vaddq_s32(i2, vzp));
                    uint16x4_t w3 = vqmovun_s32(vaddq_s32(i3, vzp));
                    uint8x8_t b0 = vqmovn_u16(vcombine_u16(w0, w1));
                    uint8x8_t b1 = vqmovn_u16(vcombine_u16(w2, w3));
                    vst1q_u8(outptr + j*4, veorq_u8(vcombine_u8(b0, b1), vmask));
                }
            #endif
                for (; j < plane_size; j++) {
                    float v0, v1, v2, v3;
                    if (inp_typ == FX_F32) {
                        v0 = ((float*)inptr0)[j];
                        v1 = ((float*)inptr1)[j];
                        v2 = ((float*)inptr2)[j];
                        v3 = ((float*)inptr3)[j];
                    } else {
                        v0 = FX_FLOAT(((fx_f16*)inptr0)[j]);
                        v1 = FX_FLOAT(((fx_f16*)inptr1)[j]);
                        v2 = FX_FLOAT(((fx_f16*)inptr2)[j]);
                        v3 = FX_FLOAT(((fx_f16*)inptr3)[j]);
                    }
                    int i0 = (int)lrintf(v0*sc[0]) + zp[0];
                    int i1 = (int)lrintf(v1*sc[1]) + zp[1];
                    int i2 = (int)lrintf(v2*sc[2]) + zp[2];
                    int i3 = (int)lrintf(v3*sc[3]) + zp[3];
                    outptr[j*C1] = (uint8_t)FX_SATURATE(i0, out_mask);
                    outptr[j*C1+1] = (uint8_t)FX_SATURATE(i0, out_mask);
                    outptr[j*C1+2] = (uint8_t)FX_SATURATE(i0, out_mask);
                    outptr[j*C1+3] = (uint8_t)FX_SATURATE(i0, out_mask);
                }
            } else {
                assert(C1 == 1);
            #ifdef __ARM_NEON
                float32x4_t vscale = vdupq_n_f32(sc[0]);
                int32x4_t vzp = vdupq_n_s32(zp[0]);
                uint8x8_t vmask = vdup_n_u8((uint8_t)out_mask);

                for (; j < plane_size; j += 8) {
                    if (j + 8 > plane_size) {
                        if (j == 0) break;
                        j = plane_size - 8;
                    }
                    float32x4_t v0, v1;
                    if (inp_typ == FX_F32) {
                        v0 = vld1q_f32((float*)inptr0 + j);
                        v1 = vld1q_f32((float*)inptr0 + j + 4);
                    } else {
                        v0 = vcvt_f32_f16(vld1_f16((fx_f16*)inptr0 + j));
                        v1 = vcvt_f32_f16(vld1_f16((fx_f16*)inptr0 + j + 4));
                    }

                    int32x4_t i0 = vcvtnq_s32_f32(vmulq_f32(v0, vscale));
                    int32x4_t i1 = vcvtnq_s32_f32(vmulq_f32(v1, vscale));
                    uint16x4_t w0 = vqmovun_s32(vaddq_s32(i0, vzp));
                    uint16x4_t w1 = vqmovun_s32(vaddq_s32(i1, vzp));
                    uint8x8_t b0 = vqmovn_u16(vcombine_u16(w0, w1));
                    vst1_u8(outptr + j, veor_u8(b0, vmask));
                }
            #endif
                for (; j < plane_size; j++) {
                    float v0;
                    if (inp_typ == FX_F32) {
                        v0 = ((float*)inptr0)[j];
                    } else {
                        v0 = FX_FLOAT(((fx_f16*)inptr0)[j]);
                    }
                    int i0 = (int)lrintf(v0*sc[0]) + zp[0];
                    outptr[j] = (uint8_t)FX_SATURATE(i0, out_mask);
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
    int esz = fx_elemsize(out_typ);
    int sc_typ = scale->data.tag, zp_typ = zp->data.tag;
    int_ N, C, NC, C0, C1 = 1, plane_size = 1;
    int_ ndims = inp_shape_->dim[0].size;
    int inp_mask = inp_typ == FX_I8 ? 128 : 0;

    if (inp_typ != FX_I8 && inp_typ != FX_U8)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (out_typ != FX_F32 && out_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (sc_typ != FX_F32 && sc_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (axis < 0)
        axis += ndims;
    if (axis != 1)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims < 2)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    N = inp_shape[0];
    C = out_shape[axis];
    C0 = inp_shape[axis];
    if (ndims != out_shape_->dim[0].size) {
        if (ndims < 3)
            return FX_SET_EXN_FAST(FX_EXN_SizeError);
        if (ndims != out_shape_->dim[0].size+1)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        C1 = inp_shape[ndims-1];
        if (C1 != 4)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }

    if (sc_data->dim[0].size != 1 && sc_data->dim[0].size != C)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (zp_data->data != 0) {
        if (zp_typ != inp_typ)
            return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
        if(zp_data->dim[0].size != sc_data->dim[0].size)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    }
    for (int_ i = 0; i < ndims-1; i++) {
        if (i == axis) {
            if ((out_shape[i] + C1-1)/C1 != inp_shape[i])
                return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
            continue;
        }
        if (inp_shape[i] != out_shape[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (i != 0) plane_size *= inp_shape[i];
    }
    NC = N*C0;
    if (plane_size*NC < 100000) ntasks = 1;

    //printf("C0=%d, C1=%d, inp_mask=%d, inp_typ=%d, zp_typ=%d, out_typ=%d, NC=%d, ntasks=%d, plane_size=%d, esz=%d\n",
    //    (int)C0, (int)C1, inp_mask, inp_typ, zp_typ, out_typ, (int)NC, (int)ntasks, (int)plane_size, (int)esz);

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ nc0 = task_id*NC/ntasks, nc1 = (task_id+1)*NC/ntasks;
        for (; nc0 < nc1; nc0++) {
            int j, n = nc0 / C0;
            int_ c_inp = nc0 % C0;
            int_ c0 = c_inp*C1;
            float sc[4] = {0.f, 0.f, 0.f, 0.f};
            int zp[4] = {0, 0, 0, 0};
            bool is_scalar_scale = sc_data->dim[0].size == 1;
            for (j = 0; j < C1; j++) {
                int_ sc_c = is_scalar_scale ? 0 : c0 + j;
                if (!is_scalar_scale && c0 + j >= C)
                    continue;
                sc[j] = sc_typ == FX_F32 ? ((float*)sc_data->data)[sc_c] :
                        FX_FLOAT(((fx_f16*)sc_data->data)[sc_c]);
                zp[j] = !zp_data->data ? inp_mask :
                        zp_typ == FX_I8 ? ((int8_t*)zp_data->data)[sc_c] + inp_mask :
                                  (int)(((uint8_t*)zp_data->data)[sc_c]);
            }
            //printf("nc0=%d, zp0=%d, sc0=%f\n", (int)nc0, zp[0], sc[0]);
            const uint8_t* inptr = (uint8_t*)inp_data->data + nc0*(plane_size*C1);
            int c0_1 = c0 + 1 < C ? c0 + 1 : C - 1;
            int c0_2 = c0 + 2 < C ? c0 + 2 : C - 1;
            int c0_3 = c0 + 3 < C ? c0 + 3 : C - 1;

            char* outptr0 = out_data->data + (n*C + c0)*plane_size*esz;
            char* outptr1 = out_data->data + (n*C + c0_1)*plane_size*esz;
            char* outptr2 = out_data->data + (n*C + c0_2)*plane_size*esz;
            char* outptr3 = out_data->data + (n*C + c0_3)*plane_size*esz;
            j = 0;

            if (C1 == 4) {
            #ifdef __ARM_NEON
                float32x4_t vs0 = vdupq_n_f32(sc[0]);
                float32x4_t vs1 = vdupq_n_f32(sc[1]);
                float32x4_t vs2 = vdupq_n_f32(sc[2]);
                float32x4_t vs3 = vdupq_n_f32(sc[3]);
                int16x8_t vzp0 = vdupq_n_s16(zp[0]);
                int16x8_t vzp1 = vdupq_n_s16(zp[1]);
                int16x8_t vzp2 = vdupq_n_s16(zp[2]);
                int16x8_t vzp3 = vdupq_n_s16(zp[3]);
                uint8x8_t vmask = vdup_n_u8((uint8_t)inp_mask);

                for (; j < plane_size; j += 8) {
                    if (j + 8 > plane_size) {
                        if (j == 0) break;
                        j = plane_size - 8;
                    }
                    uint8x8x4_t b = vld4_u8(inptr + j*4);
                    b.val[0] = veor_u8(b.val[0], vmask);
                    b.val[1] = veor_u8(b.val[1], vmask);
                    b.val[2] = veor_u8(b.val[2], vmask);
                    b.val[3] = veor_u8(b.val[3], vmask);
                    int16x8_t w0 = vsubq_s16(vreinterpretq_s16_u16(vmovl_u8(b.val[0])), vzp0);
                    int16x8_t w1 = vsubq_s16(vreinterpretq_s16_u16(vmovl_u8(b.val[1])), vzp1);
                    int16x8_t w2 = vsubq_s16(vreinterpretq_s16_u16(vmovl_u8(b.val[2])), vzp2);
                    int16x8_t w3 = vsubq_s16(vreinterpretq_s16_u16(vmovl_u8(b.val[3])), vzp3);
                    float32x4_t v00 = vcvtq_f32_s32(vmovl_s16(vget_low_s16(w0)));
                    float32x4_t v01 = vcvtq_f32_s32(vmovl_high_s16(w0));
                    v00 = vmulq_f32(v00, vs0);
                    v01 = vmulq_f32(v01, vs0);
                    float32x4_t v10 = vcvtq_f32_s32(vmovl_s16(vget_low_s16(w1)));
                    float32x4_t v11 = vcvtq_f32_s32(vmovl_high_s16(w1));
                    v10 = vmulq_f32(v10, vs1);
                    v11 = vmulq_f32(v11, vs1);
                    float32x4_t v20 = vcvtq_f32_s32(vmovl_s16(vget_low_s16(w2)));
                    float32x4_t v21 = vcvtq_f32_s32(vmovl_high_s16(w2));
                    v20 = vmulq_f32(v20, vs2);
                    v21 = vmulq_f32(v21, vs2);
                    float32x4_t v30 = vcvtq_f32_s32(vmovl_s16(vget_low_s16(w3)));
                    float32x4_t v31 = vcvtq_f32_s32(vmovl_high_s16(w3));
                    v30 = vmulq_f32(v30, vs3);
                    v31 = vmulq_f32(v31, vs3);
                #if _FX_NN_ENABLE_FP16
                    if (out_typ == FX_F16) {
                        vst1q_f16((fx_f16*)outptr0 + j, vcombine_f16(vcvt_f16_f32(v00), vcvt_f16_f32(v01)));
                        vst1q_f16((fx_f16*)outptr1 + j, vcombine_f16(vcvt_f16_f32(v10), vcvt_f16_f32(v11)));
                        vst1q_f16((fx_f16*)outptr2 + j, vcombine_f16(vcvt_f16_f32(v20), vcvt_f16_f32(v21)));
                        vst1q_f16((fx_f16*)outptr3 + j, vcombine_f16(vcvt_f16_f32(v30), vcvt_f16_f32(v31)));
                    } else
                #endif
                    {
                        vst1q_f32((float*)outptr0 + j, v00);
                        vst1q_f32((float*)outptr0 + j + 4, v01);
                        vst1q_f32((float*)outptr1 + j, v10);
                        vst1q_f32((float*)outptr1 + j + 4, v11);
                        vst1q_f32((float*)outptr2 + j, v20);
                        vst1q_f32((float*)outptr2 + j + 4, v21);
                        vst1q_f32((float*)outptr3 + j, v30);
                        vst1q_f32((float*)outptr3 + j + 4, v31);
                    }
                }
            #endif
                for (; j < plane_size; j++) {
                    float v0 = ((inptr[j*4] ^ inp_mask) - zp[0])*sc[0];
                    float v1 = ((inptr[j*4+1] ^ inp_mask) - zp[1])*sc[1];
                    float v2 = ((inptr[j*4+2] ^ inp_mask) - zp[2])*sc[2];
                    float v3 = ((inptr[j*4+3] ^ inp_mask) - zp[3])*sc[3];
                #if _FX_NN_ENABLE_FP16
                    if (out_typ == FX_F16) {
                        ((fx_f16*)outptr0)[j] = FX_FLOAT16(v0);
                        ((fx_f16*)outptr1)[j] = FX_FLOAT16(v1);
                        ((fx_f16*)outptr2)[j] = FX_FLOAT16(v2);
                        ((fx_f16*)outptr3)[j] = FX_FLOAT16(v3);
                    } else
                #endif
                    {
                        ((float*)outptr0)[j] = v0;
                        ((float*)outptr1)[j] = v1;
                        ((float*)outptr2)[j] = v2;
                        ((float*)outptr3)[j] = v3;
                    }
                }
            } else {
                assert(C1 == 1);
            #ifdef __ARM_NEON
                float32x4_t vs0 = vdupq_n_f32(sc[0]);
                int16x8_t vzp0 = vdupq_n_s16(zp[0]);
                uint8x8_t vmask = vdup_n_u8((uint8_t)inp_mask);

                for (; j < plane_size; j += 8) {
                    if (j + 8 > plane_size) {
                        if (j == 0) break;
                        j = plane_size - 8;
                    }
                    uint8x8_t b0 = veor_u8(vld1_u8(inptr + j), vmask);
                    int16x8_t w0 = vsubq_s16(vreinterpretq_s16_u16(vmovl_u8(b0)), vzp0);
                    float32x4_t v0 = vcvtq_f32_s32(vmovl_s16(vget_low_s16(w0)));
                    float32x4_t v1 = vcvtq_f32_s32(vmovl_high_s16(w0));
                    v0 = vmulq_f32(v0, vs0);
                    v1 = vmulq_f32(v1, vs0);
                #if _FX_NN_ENABLE_FP16
                    if (out_typ == FX_F16) {
                        vst1q_f16((fx_f16*)outptr0 + j, vcombine_f16(vcvt_f16_f32(v0), vcvt_f16_f32(v1)));
                    } else
                #endif
                    {
                        vst1q_f32((float*)outptr0 + j, v0);
                        vst1q_f32((float*)outptr0 + j + 4, v1);
                    }
                }
            #endif
            #if _FX_NN_ENABLE_FP16
                if (out_typ == FX_F16) {
                    for (; j < plane_size; j++) {
                        float v0 = ((inptr[j] ^ inp_mask) - zp[0])*sc[0];
                        ((fx_f16*)outptr0)[j] = FX_FLOAT16(v0);
                    }
                } else
            #endif
                {
                    for (; j < plane_size; j++) {
                        float v0 = ((inptr[j] ^ inp_mask) - zp[0])*sc[0];
                        ((float*)outptr0)[j] = v0;
                        //printf("%d. inp=%d, out=%.2f\n", j, inptr[j], v0);
                    }
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

    if (nplanes == 1) {
        bool same_shapes = true;
        for (int k = 0; k < _FX_ELEMWISE_MAX_DIMS; k++) {
            if (shape1[k] != shape2[k] || shape1[k] != shape[k]) {
                same_shapes = false;
                break;
            }
        }
        if (same_shapes && (nrows == 1 || (
            ncols*dp1 == rowstep1 &&
            ncols*dp2 == rowstep2 &&
            ncols*dp == rowstep))) {
            int_ total = nplanes*nrows*ncols;

            #pragma omp parallel for num_threads(ntasks)
            for (plane_idx = 0; plane_idx < ntasks; plane_idx++) {
                int_ pix0 = plane_idx*total/ntasks, pix1 = (plane_idx+1)*total/ntasks;
                processing_func(data1 + pix0*esz1, 0, dp1,
                        data2 + pix0*esz2, 0, dp2,
                        data + pix0*esz, 0, dp,
                        1, pix1 - pix0, &params);
            }
            return FX_OK;
        }
    }

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
    const int C1 = FX_QCONV_C;
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
    int_ NC, C, planesize = 1;
    int mask = inp_typ == FX_I8 ? 128 : 0;
    double avg_scale, inp_scale0, out_scale0, bias, inp_out_scale;
    int inp_zp0, out_zp0;

    if (channels_last)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (inp_typ != out_typ || (inp_typ != FX_I8 && inp_typ != FX_U8))
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (inp_ndims < 3 || inp_ndims != out_ndims+1)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (inp->shape.layout.tag != _FX_NN_Layout_NCXHWX ||
        out->shape.layout.tag != _FX_NN_Layout_NCHW)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (inp_shape[inp_ndims-1] != C1)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
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
    C = out_shape[1];
    for (int_ i = 0; i < inp_ndims-1; i++) {
        if (i < 2) {
            if (!((i == 0 && inp_shape[i] == out_shape[i]) ||
                  (i == 1 && inp_shape[i] == (out_shape[i] + C1 - 1)/C1)))
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
            const uint8_t* inptr = (const uint8_t*)inp_data->data + nc0*planesize*C1;
            uint8_t* outptr = (uint8_t*)out_data->data + nc0*C1;
            const int_ block_size = 1<<8;
            int64_t total[FX_QCONV_C];
            memset(total, 0, sizeof(total));
            /* (sum_j ((inp[j] - inp_zp)*inp_sc))/(out_sc*planesize) + out_zp =
               (inp_sc/(out_sc*planesize)*((sum_j inp[j]) - inp_zp*planesize) + out_zp =
               inp_sc/(out_sc*planesize)*(sum_j inp[j]) + out_zp - inp_zp*inp_sc/out_sc */
            for (int_ j = 0; j < planesize; ) {
                int_ block_end = j + block_size < planesize ? j + block_size : planesize;
                unsigned s[FX_QCONV_C] = {0, 0, 0, 0};
            #ifdef __ARM_NEON
                uint16x8_t vs = vdupq_n_u16(0);
                for (; j <= block_end - 2; j += 2)
                    vs = vaddq_u16(vs, vmovl_u8(vld1_u8(inptr + j*C1)));
                uint32x4_t vs0 = vmovl_u16(vget_low_u16(vs));
                uint32x4_t vs1 = vmovl_u16(vget_high_u16(vs));
                vst1q_u32(s, vaddq_u32(vs0, vs1));
            #endif
                for (; j < block_end; j++) {
                    s[0] += inptr[j*C1];
                    s[1] += inptr[j*C1+1];
                    s[2] += inptr[j*C1+2];
                    s[3] += inptr[j*C1+3];
                }
                for (int c = 0; c < C1; c++)
                    total[c] += s[c];
            }
            int C1_ = (int)(C - nc0*C1);
            C1_ = FX_MIN(C1_, C1);
            for (int c = 0; c < C1_; c++) {
                int iavg = (int)lrint(total[c]*avg_scale + bias);
                outptr[c] = FX_SATURATE(iavg, mask);
            }
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


fun NCXHWCtoNCHW(inp_t: Ast.nntensor_t)
{
    val (C0, C1) = match inp_t.shape.layout {
    | Ast.NN_Layout_NCXHWX(orig_C, c) => (orig_C, c)
    | _ => throw Ast.NNError(f"unexpected block layout; \
                the actual layout is {inp_t.shape.layout}")
    }
    val data = match inp_t.data {
    | Ast.NN_Data_U8 data_u8 => data_u8
    | _ => throw NotImplementedError
    }
    val inp_shape = inp_t.shape.shape
    val inp_ndims = inp_shape.size()
    val fold plane_size = 1 for i <- 2:inp_ndims-1 {plane_size*inp_shape[i]}
    val N = inp_shape[0]
    val inp_C = inp_shape[1]
    val out_shape = [N, inp_C, \inp_shape[2:inp_ndims-1]]
    assert(`inp_C == (C0 + C1 - 1)/C1`)
    val out_data =
        [@parallel for n <- 0:N for c <- 0:C0 for j <- 0:plane_size
        { val c_plane = c/C1, c_ofs = c - c_plane*C1
          data[((n*inp_C + c_plane)*plane_size + j)*C1 + c_ofs]}][:]
    Ast.nntensor_t {shape=Ast.nnshape_t {
        layout=Ast.NN_Layout_NCHW,
        shape=out_shape}, data=Ast.NN_Data_U8(out_data)}
}
