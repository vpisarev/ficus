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

#undef FX_SATURATE
#define FX_SATURATE(x, mask) \
    (uint8_t)((((x) & ~255) == 0 ? (x) : (x) < 0 ? 0 : 255) ^ (mask))

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

// !!! keep it consistent with NN.Ast.nnelwise_t !!!
enum {
    // binary op's
    _FX_NN_Add=1,
    _FX_NN_And,
    _FX_NN_Div,
    _FX_NN_Equal,
    _FX_NN_Greater,
    _FX_NN_GreaterOrEqual,
    _FX_NN_Less,
    _FX_NN_LessOrEqual,
    _FX_NN_Mod,
    _FX_NN_Mul,
    _FX_NN_Pow,
    _FX_NN_Or,
    _FX_NN_Sub,
    _FX_NN_Xor,

    // n-ary op's
    _FX_NN_Min,
    _FX_NN_Max,
    _FX_NN_Mean,

    // unary op's
    _FX_NN_Abs,
    _FX_NN_Acos,
    _FX_NN_Acosh,
    _FX_NN_Asin,
    _FX_NN_Asinh,
    _FX_NN_Atan,
    _FX_NN_Atanh,
    _FX_NN_Ceil,
    _FX_NN_Cos,
    _FX_NN_Cosh,
    _FX_NN_Erf,
    _FX_NN_Exp,
    _FX_NN_Floor,
    _FX_NN_IsInf,
    _FX_NN_IsNaN,
    _FX_NN_Log,
    _FX_NN_Mish,
    _FX_NN_Neg,
    _FX_NN_Not,
    _FX_NN_Relu,
    _FX_NN_Round,
    _FX_NN_Sigmoid,
    _FX_NN_Sign,
    _FX_NN_Sin,
    _FX_NN_Sinh,
    _FX_NN_Softplus,
    _FX_NN_Softsign,
    _FX_NN_Sqrt,
    _FX_NN_Tan,
    _FX_NN_Tanh
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
#define FX_CONV_MR_F16 8
#define FX_CONV_NR_F16 24
#define FX_VEC_NLANES_F32 4
#else
#ifdef __AVX__
#define FX_CONV_MR 4
#define FX_CONV_NR 24
#define FX_VEC_NLANES_F32 8
#else
#define FX_CONV_MR 4
#define FX_CONV_MR 12
#define FX_VEC_NLANES_F32 4
#endif
#define FX_CONV_MR_F16 FX_CONV_MR
#define FX_CONV_NR_F16 FX_CONV_NR
#endif

#define FX_VEC_NLANES_F16 (FX_VEC_NLANES_F32*2)
#define FX_VEC_NLANES_U8 (FX_VEC_NLANES_F32*4)

#define FX_QCONV_C 4
#define FX_QCONV_MR FX_CONV_MR
#define FX_QCONV_NR FX_CONV_NR

enum { _FX_ACTIV_NONE=1, _FX_ACTIV_RELU=2, _FX_ACTIV_CLIP=3, _FX_ACTIV_LRELU=4,
       _FX_ACTIV_SIGMOID=6, _FX_ACTIV_TANH=7, _FX_ACTIV_MISH=8 };
typedef _fx_unary_func_t _fx_activ_func_t;

enum { FX_CONV_TYPE_GENERIC=0, FX_CONV_TYPE_DEPTHWISE=1, FX_CONV_TYPE_WINOGRAD3X3=2 };

typedef struct _fx_conv2d_t
{
    int layout, ngroups;
    int K, C, Hk, Wk;
    int stride_y, stride_x;
    int dilation_y, dilation_x;
    int pad_top, pad_left, pad_bottom, pad_right;
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

typedef struct _fx_qconv2d_t
{
    int layout, ngroups;
    int K, C, Hk, Wk;
    int padded_ksize;
    int stride_y, stride_x;
    int dilation_y, dilation_x;
    int pad_top, pad_left, pad_bottom, pad_right;
    int conv_type;
    int w_typ;
    int16_t* depthwise_weights;
    uint8_t* weights;
    float* w_scale;
    int32_t* w_sum;
    int32_t* w_zp;
    int32_t* bias;
} _fx_qconv2d_t;

/*
    Prepare the strides for the efficient BROADCAST_MAX_DIMS-dimensional operation.
    The algorithm includes the following steps:

    1. Make both input array and the output array BROADCAST_MAX_DIMS-dimensional.
       If necessary, prepend the shapes with 1's and compute the corresponding strides.
       This makes the actual operation much more straight-forward,
       we just have to deal with fixed dimensionality using fixed number of nested loops.
    2. For some i one of the inputs may have i-th dimension == 1,
       whereas the other input may have the same dimension > 1.
       We need to handle it by zero'ing the corresponding i-th step.

       E.g. shape1[2] == 1, shape2[2] == 100 (and so shape[2] == 100).

       When we will iterate through this dimension within a nested loop and access elements

       for (int i0 = 0; i0 < shape[0]; i0++)
          for (int i1 = 0; i1 < shape[1]; i++) {
             for (int i2 = 0; i2 < shape[2]; i2++) {
                ...
                input1.ptr<float>[i0*step1[0] + i1*step1[1] + i2*step1[2] + ...]
             }

       we need to take into account that shape1[2] == 1 and set step1[2]=0.
    3. Often the inputs are contiguous (the output is assumed to be contiguous),
       so we can try to flatten/reshape inputs in order to increase the length of inner loops and
       correspondingly shorten the outer loops so that the loop overhead is reduced.
       We do flattening within in a loop with descending j, starting from j=BROADCAST_MAX_DIMS-2:
       3a. we check that for some stepi[j] = stepi[j+1]*shapei[j+1] for all i: i=0,1,2 (i=0 means the output tensor)
       3b. we also check that for each tensor we stay in scalar mode or stay in non-scalar mode
       3c. we also check if shapei[j] == 1 for all i.
       3d. if yes for (3a && (3b || 3c)), we do shapei[j+1] *= shapei[j] and eliminate j-th dimension.
       3e. otherwise, we leave everything as is, decrease j and proceed.
       3f. in the end of the loop we append the proper number of 1's
           to the final shape to keep it BROADCAST_MAX_DIMS-dimensional.
*/
static bool _fx_prepare_for_broadcast_op(
    int narrays, int max_ndims,
    const int* ndims, const int_** shape_,
    int_** shape, size_t** step)
{
    int i, j, k;

    // step 1.
    // * make all inputs and the output max_ndims-dimensional.
    // * compute proper step's
    for (i = max_ndims-1; i >= 0; i-- ) {
        for (k = 0; k < narrays; k++) {
            j = ndims[k] - (max_ndims - i);
            int sz_i = j >= 0 ? shape_[k][j] : 1;
            size_t st_i = i == max_ndims-1 ? 1 : step[k][i+1]*shape[k][i+1];
            shape[k][i] = sz_i;
            step[k][i] = st_i;
            if (shape[k][i] == 0)
                return false;
        }
    }

    // step 3. Let's do the flattening first,
    // since we'd need proper values of steps to check continuity.
    // this loop is probably the most tricky part
    // in the whole implementation of broadcasting.
    j = max_ndims-1;
    for (i = j - 1; i >= 0; i--) {
        bool all_contiguous = true, all_scalars = true, all_consistent = true;
        for(k = 0; k < narrays; k++) {
            size_t st = step[k][j]*shape[k][j];
            bool prev_scalar = shape[k][j] == 1;
            bool scalar = shape[k][i] == 1;
            all_contiguous = all_contiguous && (st == step[k][i]);
            all_scalars = all_scalars && scalar;
            all_consistent = all_consistent && (scalar == prev_scalar);
        }
        if (all_contiguous && (all_consistent || all_scalars)) {
            for(k = 0; k < narrays; k++)
                shape[k][j] *= shape[k][i];
        } else {
            j--;
            if (i < j) {
                for(k = 0; k < narrays; k++) {
                    shape[k][j] = shape[k][i];
                    step[k][j] = step[k][i];
                }
            }
        }
    }

    // step 2. Set some step's to 0's.
    for (i = max_ndims-1; i >= j; i--) {
        for (k = 0; k < narrays; k++)
            step[k][i] = shape[k][i] == 1 ? 0 : step[k][i];
    }
    for (; i >= 0; i--) {
        for (k = 0; k < narrays; k++) {
            step[k][i] = 0;
            shape[k][i] = 1;
        }
    }
    return true;
}

typedef struct _fx_depthwise2d_t
{
    int N, Hi, Wi, H0, W0;
    int inner_ytop, inner_xleft, inner_ybottom, inner_xright;
    const int* ofstab;
    const int* yxtab;
} _fx_depthwise2d_t;

static void _fx_calc_ofstab2d(int Wi, int Hk, int Wk, int dilation_y,
                              int dilation_x, int* yxtab, int* ofstab)
{
    for (int y = 0; y < Hk; y++)
        for (int x = 0; x < Wk; x++) {
            int k = y*Wk + x;
            int dy = y*dilation_y, dx = x*dilation_x;
            yxtab[k*2] = dy; yxtab[k*2+1] = dx;
            ofstab[k] = dy*Wi + dx;
        }
}

static void _fx_init_depthwise2d(int N, int Hi, int Wi, int H0, int W0,
                          int Hk, int Wk, int stride_y, int stride_x,
                          int dilation_y, int dilation_x,
                          int pad_top, int pad_left, int pad_bottom, int pad_right,
                          int* yxtab, int* ofstab,
                          _fx_depthwise2d_t* data)
{
    data->N = N;
    data->Hi = Hi; data->Wi = Wi;
    data->H0 = H0; data->W0 = W0;
    data->yxtab = yxtab; data->ofstab = ofstab;

    _fx_calc_ofstab2d(Wi, Hk, Wk, dilation_y, dilation_x, yxtab, ofstab);

    int inner_ytop = (pad_top + stride_y-1)/stride_y, inner_ybottom;
    int inner_xleft = (pad_left + stride_x-1)/stride_x, inner_xright;

    inner_xright = (Wi - (Wk - 1)*dilation_x + pad_left)/stride_x;
    inner_xright += inner_xright*stride_x - pad_left + (Wk-1)*dilation_x < Wi;
    inner_ybottom = (Hi - (Hk - 1)*dilation_y + pad_top)/stride_y;
    inner_ybottom += inner_ybottom*stride_y - pad_top + (Hk-1)*dilation_y < Hi;
    inner_xright = inner_xright < W0 ? inner_xright : W0;
    inner_ybottom = inner_ybottom < H0 ? inner_ybottom : H0;
    if (inner_xleft >= inner_xright || inner_ytop >= inner_ybottom) {
        inner_xleft = W0;
        inner_ytop = H0;
    }
    data->inner_ytop = inner_ytop;
    data->inner_xleft = inner_xleft;
    data->inner_ybottom = inner_ybottom;
    data->inner_xright = inner_xright;
}

#ifdef __cplusplus
}
#endif

#endif
