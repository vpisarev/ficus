/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// the header contains some common definitions that are used by
// the inline C code in NN module

#ifndef __FICUS_NN_COMMON_H__
#define __FICUS_NN_COMMON_H__

#include <limits.h>
#include <float.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _fx_nndata_t {
   int tag;
   union {
      fx_arr_t NN_Data_I8;
      fx_arr_t NN_Data_U8;
   } u;
} _fx_nndata_t;

enum {
    _FX_NN_Undefined=1, _FX_NN_I8, _FX_NN_U8, _FX_NN_I16, _FX_NN_U16,
    _FX_NN_I32, _FX_NN_U32, _FX_NN_I64, _FX_NN_U64, _FX_NN_FP16,
    _FX_NN_BF16, _FX_NN_FP32, _FX_NN_FP64, _FX_NN_Bool
};

enum {
    _FX_NN_Layout_Unknown=1,
    _FX_NN_Layout_NC,
    _FX_NN_Layout_NCHW,
    _FX_NN_Layout_NHWC,
    _FX_NN_Layout_NCHWxc
};

enum {
    _FX_NN_Inter_Nearest=1,
    _FX_NN_Inter_Linear=2,
    _FX_NN_Inter_Cubic=3
};

enum {
    _FX_NN_CT_HalfPixel=1,
    _FX_NN_CT_PyTorchHalfPixel=2,
    _FX_NN_CT_AlignCorners=3,
    _FX_NN_CT_Asymmetric=4,
    _FX_NN_CT_TFCropResize=5,
    _FX_NN_CT_OutHalfPixel=6
};

enum {
    _FX_NN_Nearest_RoundPreferFloor=1,
    _FX_NN_Nearest_RoundPreferCeil=2,
    _FX_NN_Nearest_Floor=3,
    _FX_NN_Nearest_Ceil=4
};

static int _fx_compute_resize_tab(int* tab, float* alphatab, int_ inpsz_, int_ outsz_,
                                  float scale, float roi_start, float roi_end,
                                  int mode, int coord_trans, int nearest_mode,
                                  int_ inp_step)
{
    int inpsz = (int)inpsz_, outsz = (int)outsz_;
    float a = 1.f/scale, b = 0.f;
    if (coord_trans == _FX_NN_CT_HalfPixel ||
        coord_trans == _FX_NN_CT_PyTorchHalfPixel) {
        b = 0.5f/scale - 0.5f;
        if (outsz == 1 && coord_trans == _FX_NN_CT_PyTorchHalfPixel) {
            a = 1.f; b = 0.f;
        }
    } else if (coord_trans == _FX_NN_CT_AlignCorners) {
        a = (float)(inpsz - 1)/(outsz - 1);
    } else if (coord_trans == _FX_NN_CT_TFCropResize) {
        if (outsz > 1) {
            a = (roi_end - roi_start)*(inpsz - 1)/(outsz - 1);
            b = roi_start * (inpsz - 1);
        } else {
            a = 1.f;
            b = 0.5 * (roi_start + roi_end) * (inpsz - 1);
        }
    } else if (coord_trans == _FX_NN_CT_Asymmetric)
        ;
    else
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    for (int out_x = 0; out_x < outsz; out_x++) {
        float inp_x = a*out_x + b;
        int int_x = (int)inp_x;
        if (mode == _FX_NN_Inter_Nearest) {
            if (int_x != inp_x) {
                int_x -= inp_x < int_x;
                int ceil_x = int_x + 1;
                if (nearest_mode == _FX_NN_Nearest_Floor)
                    ;
                else if (nearest_mode == _FX_NN_Nearest_Ceil)
                    int_x = ceil_x;
                else {
                    float a = inp_x - int_x;
                    float b = ceil_x - inp_x;
                    if (a > b || (a == b && nearest_mode == _FX_NN_Nearest_RoundPreferCeil))
                        int_x = ceil_x;
                }
            }
            int_x = int_x < 0 ? 0 : int_x >= inpsz ? inpsz - 1 : int_x;
            tab[out_x] = (int)(int_x*inp_step);
        } else if (mode == _FX_NN_Inter_Linear) {
            int_x -= inp_x < int_x;
            float a = inp_x - int_x;
            int int_x0 = int_x, int_x1 = int_x+1;
            if (int_x0 < 0) {
                int_x0 = int_x1 = 0;
                a = 0.f;
            } else if (int_x1 >= inpsz) {
                int_x0 = int_x1 = inpsz - 1;
                a = 0.f;
            }
            tab[out_x*2] = (int)(int_x0*inp_step);
            tab[out_x*2+1] = (int)(int_x1*inp_step);
            alphatab[out_x*2] = 1.f - a;
            alphatab[out_x*2+1] = a;
        } else {
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        }
    }
    return FX_OK;
}

#ifdef __ARM_NEON
#include <arm_neon.h>

#define _fx_c_exp_hi 88.3762626647949f
#define _fx_c_exp_lo -88.3762626647949f

#define _fx_c_cephes_LOG2EF 1.44269504088896341
#define _fx_c_cephes_exp_C1 0.693359375
#define _fx_c_cephes_exp_C2 -2.12194440e-4

#define _fx_c_cephes_exp_p0 1.9875691500E-4
#define _fx_c_cephes_exp_p1 1.3981999507E-3
#define _fx_c_cephes_exp_p2 8.3334519073E-3
#define _fx_c_cephes_exp_p3 4.1665795894E-2
#define _fx_c_cephes_exp_p4 1.6666665459E-1
#define _fx_c_cephes_exp_p5 5.0000001201E-1

/* exp() computed for 4 float at once */
static __inline float32x4_t _fx_vexpq_f32(float32x4_t x) {
    float32x4_t tmp, fx;

    float32x4_t one = vdupq_n_f32(1);
    x = vminq_f32(x, vdupq_n_f32(_fx_c_exp_hi));
    x = vmaxq_f32(x, vdupq_n_f32(_fx_c_exp_lo));

    /* express exp(x) as exp(g + n*log(2)) */
    fx = vmlaq_f32(vdupq_n_f32(0.5f), x, vdupq_n_f32(_fx_c_cephes_LOG2EF));

    /* perform a floorf */
    tmp = vcvtq_f32_s32(vcvtq_s32_f32(fx));

    /* if greater, substract 1 */
    uint32x4_t mask = vcgtq_f32(tmp, fx);
    mask = vandq_u32(mask, vreinterpretq_u32_f32(one));

    fx = vsubq_f32(tmp, vreinterpretq_f32_u32(mask));

    tmp = vmulq_f32(fx, vdupq_n_f32(_fx_c_cephes_exp_C1));
    float32x4_t z = vmulq_f32(fx, vdupq_n_f32(_fx_c_cephes_exp_C2));
    x = vsubq_f32(x, tmp);
    x = vsubq_f32(x, z);

    const float _fx_cephes_exp_p[6] = {
        _fx_c_cephes_exp_p0, _fx_c_cephes_exp_p1,
        _fx_c_cephes_exp_p2, _fx_c_cephes_exp_p3,
        _fx_c_cephes_exp_p4, _fx_c_cephes_exp_p5 };
    float32x4_t y = vld1q_dup_f32(_fx_cephes_exp_p+0);
    float32x4_t c1 = vld1q_dup_f32(_fx_cephes_exp_p+1);
    float32x4_t c2 = vld1q_dup_f32(_fx_cephes_exp_p+2);
    float32x4_t c3 = vld1q_dup_f32(_fx_cephes_exp_p+3);
    float32x4_t c4 = vld1q_dup_f32(_fx_cephes_exp_p+4);
    float32x4_t c5 = vld1q_dup_f32(_fx_cephes_exp_p+5);

    y = vmulq_f32(y, x);
    z = vmulq_f32(x, x);
    y = vaddq_f32(y, c1);
    y = vmulq_f32(y, x);
    y = vaddq_f32(y, c2);
    y = vmulq_f32(y, x);
    y = vaddq_f32(y, c3);
    y = vmulq_f32(y, x);
    y = vaddq_f32(y, c4);
    y = vmulq_f32(y, x);
    y = vaddq_f32(y, c5);

    y = vmulq_f32(y, z);
    y = vaddq_f32(y, x);
    y = vaddq_f32(y, one);

    /* build 2^n */
    int32x4_t mm = vcvtq_s32_f32(fx);
    mm = vaddq_s32(mm, vdupq_n_s32(0x7f));
    mm = vshlq_n_s32(mm, 23);
    float32x4_t pow2n = vreinterpretq_f32_s32(mm);

    y = vmulq_f32(y, pow2n);
    return y;
}
#endif

#ifdef __cplusplus
}
#endif

#endif
