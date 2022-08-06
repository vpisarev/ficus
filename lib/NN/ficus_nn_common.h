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

typedef struct _fx_nnlayout_t {
    int tag;
} _fx_nnlayout_t;

typedef struct _fx_nnshape_t {
    _fx_nnlayout_t layout;
    fx_arr_t shape;
} _fx_nnshape_t;

typedef struct _fx_nntensor_t {
   _fx_nnshape_t shape;
   _fx_nndata_t data;
} _fx_nntensor_t;

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

typedef void (*_fx_unary_func_t)(const void* inptr, void* outptr,
                                 int_ nelems, const float* param);

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

/* NEON implementation of sin, cos, exp and log

   Inspired by Intel Approximate Math library, and based on the
   corresponding algorithms of the cephes math library
*/

/* Copyright (C) 2011  Julien Pommier

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  (this is the zlib license)
*/

#ifdef __ARM_NEON
#define _FX_NN_ENABLE_FP16 1
#define _FX_FP16_CASE(x) x
#else
#define _FX_FP16_CASE(x)
#endif

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

#define _FX_VEXP_INIT() \
    float32x4_t _fx_vexp_lo = vdupq_n_f32(_fx_c_exp_lo); \
    float32x4_t _fx_vexp_hi = vdupq_n_f32(_fx_c_exp_hi); \
    float32x4_t _fx_vexp_half = vdupq_n_f32(0.5f); \
    float32x4_t _fx_vexp_one = vdupq_n_f32(1.f); \
    float32x4_t _fx_vexp_LOG2EF = vdupq_n_f32(_fx_c_cephes_LOG2EF); \
    float32x4_t _fx_vexp_C1 = vdupq_n_f32(-_fx_c_cephes_exp_C1); \
    float32x4_t _fx_vexp_C2 = vdupq_n_f32(-_fx_c_cephes_exp_C2); \
    float32x4_t _fx_vexp_p0 = vdupq_n_f32(_fx_c_cephes_exp_p0); \
    float32x4_t _fx_vexp_p1 = vdupq_n_f32(_fx_c_cephes_exp_p1); \
    float32x4_t _fx_vexp_p2 = vdupq_n_f32(_fx_c_cephes_exp_p2); \
    float32x4_t _fx_vexp_p3 = vdupq_n_f32(_fx_c_cephes_exp_p3); \
    float32x4_t _fx_vexp_p4 = vdupq_n_f32(_fx_c_cephes_exp_p4); \
    float32x4_t _fx_vexp_p5 = vdupq_n_f32(_fx_c_cephes_exp_p5)

#define _FX_VEXP_COMPUTE(x, y) { \
    float32x4_t _vexp_fx, _vexp_x, _vexp_y, _vexp_z; \
    _vexp_x = vminq_f32(x, _fx_vexp_hi); \
    _vexp_x = vmaxq_f32(_vexp_x, _fx_vexp_lo); \
    _vexp_fx = vfmaq_f32(_fx_vexp_half, _vexp_x, _fx_vexp_LOG2EF); \
    int32x4_t _vexp_mm = vcvtmq_s32_f32(_vexp_fx); \
    _vexp_fx = vcvtq_f32_s32(_vexp_mm); \
    _vexp_mm = vaddq_s32(_vexp_mm, vdupq_n_s32(0x7f)); \
    _vexp_mm = vshlq_n_s32(_vexp_mm, 23); \
    _vexp_x = vfmaq_f32(_vexp_x, _vexp_fx, _fx_vexp_C1); \
    _vexp_x = vfmaq_f32(_vexp_x, _vexp_fx, _fx_vexp_C2); \
    _vexp_z = vmulq_f32(_vexp_x, _vexp_x); \
    _vexp_y = vfmaq_f32(_fx_vexp_p1, _vexp_x, _fx_vexp_p0); \
    _vexp_y = vfmaq_f32(_fx_vexp_p2, _vexp_y, _vexp_x); \
    _vexp_y = vfmaq_f32(_fx_vexp_p3, _vexp_y, _vexp_x); \
    _vexp_y = vfmaq_f32(_fx_vexp_p4, _vexp_y, _vexp_x); \
    _vexp_y = vfmaq_f32(_fx_vexp_p5, _vexp_y, _vexp_x); \
    _vexp_y = vfmaq_f32(_vexp_x, _vexp_y, _vexp_z); \
    _vexp_y = vaddq_f32(_vexp_y, _fx_vexp_one); \
    y = vmulq_f32(_vexp_y, vreinterpretq_f32_s32(_vexp_mm)); \
    }

/* exp() computed for 4 float at once */
static __inline float32x4_t _fx_vexpq_f32(float32x4_t x) {
    _FX_VEXP_INIT();
    float32x4_t y;
    _FX_VEXP_COMPUTE(x, y);
    return y;
}
#endif

void _fx_nn_elemwise_clip_f32(const void* inptr_, void* outptr_,
                              int_ len, const float* param);
void _fx_nn_elemwise_leaky_relu_f32(const void* inptr_, void* outptr_,
                                    int_ len, const float* param);
void _fx_nn_elemwise_mish_f32(const void* inptr_, void* outptr_,
                              int_ len, const float* param);
void _fx_nn_elemwise_sigmoid_f32(const void* inptr_, void* outptr_,
                                 int_ len, const float* param);
void _fx_nn_elemwise_tanh_f32(const void* inptr_, void* outptr_,
                              int_ len, const float* param);
#if _FX_NN_ENABLE_FP16
void _fx_nn_elemwise_clip_f16(const void* inptr_, void* outptr_,
                              int_ len, const float* param);
void _fx_nn_elemwise_leaky_relu_f16(const void* inptr_, void* outptr_,
                                    int_ len, const float* param);
void _fx_nn_elemwise_mish_f16(const void* inptr_, void* outptr_,
                              int_ len, const float* param);
void _fx_nn_elemwise_sigmoid_f16(const void* inptr_, void* outptr_,
                                 int_ len, const float* param);
void _fx_nn_elemwise_tanh_f16(const void* inptr_, void* outptr_,
                              int_ len, const float* param);
#endif

#ifdef __ARM_NEON
#define FX_CONV_MR 4
#define FX_CONV_NR 28
enum { FX_VEC_NLANES=4, FX_VEC_F16_NLANES=8 };
#elif defined __AVX__
#define FX_CONV_MR 4
#define FX_CONV_NR 24
enum { FX_VEC_NLANES=8, FX_VEC_F16_NLANES=16 };
#else
enum { FX_VEC_NLANES=1, FX_VEC_F16_NLANES=1 };
#endif

#ifdef __ARM_NEON
#define FX_CONV_MR_FP16 8
#define FX_CONV_NR_FP16 24
#else
#define FX_CONV_MR_FP16 FX_CONV_MR
#define FX_CONV_NR_FP16 FX_CONV_NR
#endif

enum { _FX_ACTIV_NONE=1, _FX_ACTIV_RELU=2, _FX_ACTIV_CLIP=3, _FX_ACTIV_LRELU=4,
       _FX_ACTIV_SIGMOID=6, _FX_ACTIV_TANH=7, _FX_ACTIV_MISH=8 };
typedef _fx_unary_func_t _fx_activ_func_t;

typedef struct _fx_conv2d_t
{
    int layout, ngroups;
    int K, C, Hk, Wk;
    int stride_y, stride_x;
    int dilation_y, dilation_x;
    int pad_top, pad_bottom, pad_left, pad_right;
    int conv_type;
    float* weights;
    fx_f16* wf16;
    float* bias;
    int activ;
    _fx_activ_func_t activ_func;
    _fx_activ_func_t activ_func_f16;
    int nactiv_params;
    float* activ_params;
    float minval, maxval, alpha;
} _fx_conv2d_t;

#ifdef __cplusplus
}
#endif

#endif
