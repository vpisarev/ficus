/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include <alloca.h>
#include <float.h>
#include "ficus_nn_common.h"

#ifndef FLT16_MAX
#define FLT16_MAX 65504.f
#endif

typedef struct _fx_pooling2d_t
{
    _fx_depthwise2d_t dw_ctx;
    int Hk, Wk;
    int stride_y, stride_x;
    int dilation_y, dilation_x;
    int pad_top, pad_bottom, pad_left, pad_right;
    bool count_include_pad;
} _fx_pooling2d_t;

static void _fx_avgpool_2d_f32(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    int Hi = pool->dw_ctx.Hi, Wi = pool->dw_ctx.Wi;
    int H0 = pool->dw_ctx.H0, W0 = pool->dw_ctx.W0;
    int inner_y0 = pool->dw_ctx.inner_ytop;
    int inner_y1 = pool->dw_ctx.inner_ybottom;
    int inner_x0 = pool->dw_ctx.inner_xleft;
    int inner_x1 = pool->dw_ctx.inner_xright;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->dw_ctx.yxtab;
    const int* ofstab = pool->dw_ctx.ofstab;
    bool count_include_pad = pool->count_include_pad;
    float avg_scale = 1.f/(pool->Hk*pool->Wk);
#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_F32;
    bool useSIMD = stride_x == 1 && inner_x0 < W0;
    bool is3x3 = pool->Hk == 3 && pool->Wk == 3;
    float32x4_t vscale = vdupq_n_f32(avg_scale);
#endif

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    int count = 0;
                    s = 0;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        float v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s += v;
                        count++;
                    }
                    outptr[x0] = count_include_pad ? s*avg_scale : s/count;
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x1)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            float32x4_t s1 = vld1q_f32(inptr_xi + ofstab[1]);
                            float32x4_t s2 = vld1q_f32(inptr_xi + ofstab[2]);

                            s0 = vaddq_f32(s0, vld1q_f32(inptr_xi + ofstab[3]));
                            s1 = vaddq_f32(s1, vld1q_f32(inptr_xi + ofstab[4]));
                            s2 = vaddq_f32(s2, vld1q_f32(inptr_xi + ofstab[5]));

                            s0 = vaddq_f32(s0, vld1q_f32(inptr_xi + ofstab[6]));
                            s1 = vaddq_f32(s1, vld1q_f32(inptr_xi + ofstab[7]));
                            s2 = vaddq_f32(s2, vld1q_f32(inptr_xi + ofstab[8]));

                            s0 = vaddq_f32(vaddq_f32(s0, s1), s2);
                            vst1q_f32(outptr + x0, vmulq_f32(s0, vscale));
                        }
                    } else {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x1)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vaddq_f32(s0, vld1q_f32(inptr_xi + ofstab[k]));
                            vst1q_f32(outptr + x0, vmulq_f32(s0, vscale));
                        }
                    }
                }
            #endif
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const float* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        float v = inptr_xi[ofstab[k]];
                        s += v;
                    }
                    outptr[x0] = s*avg_scale;
                }
                x1 = W0;
            }
        }
    }
}

#ifdef _FX_NN_ENABLE_FP16
static void _fx_avgpool_2d_f16(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool)
{
    const fx_f16* inptr = (const fx_f16*)inptr_;
    fx_f16* outptr = (fx_f16*)outptr_;
    int Hi = pool->dw_ctx.Hi, Wi = pool->dw_ctx.Wi;
    int H0 = pool->dw_ctx.H0, W0 = pool->dw_ctx.W0;
    int inner_y0 = pool->dw_ctx.inner_ytop;
    int inner_y1 = pool->dw_ctx.inner_ybottom;
    int inner_x0 = pool->dw_ctx.inner_xleft;
    int inner_x1 = pool->dw_ctx.inner_xright;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->dw_ctx.yxtab;
    const int* ofstab = pool->dw_ctx.ofstab;
    bool count_include_pad = pool->count_include_pad;
    float avg_scale = 1.f/(pool->Hk*pool->Wk);
#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_F16;
    bool useSIMD = stride_x == 1 && inner_x0 < W0;
    bool is3x3 = pool->Hk == 3 && pool->Wk == 3;
    float16x8_t vscale = vdupq_n_f16(avg_scale);
#endif

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    int count = 0;
                    s = 0;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        float v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = FX_FLOAT(inptr[yi*Wi + xi]);
                        s += v;
                        count++;
                    }
                    outptr[x0] = FX_FLOAT16(count_include_pad ? s*avg_scale : s/count);
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x1)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left;
                            const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vld1q_f16(inptr_xi + ofstab[0]);
                            float16x8_t s1 = vld1q_f16(inptr_xi + ofstab[1]);
                            float16x8_t s2 = vld1q_f16(inptr_xi + ofstab[2]);

                            s0 = vaddq_f16(s0, vld1q_f16(inptr_xi + ofstab[3]));
                            s1 = vaddq_f16(s1, vld1q_f16(inptr_xi + ofstab[4]));
                            s2 = vaddq_f16(s2, vld1q_f16(inptr_xi + ofstab[5]));

                            s0 = vaddq_f16(s0, vld1q_f16(inptr_xi + ofstab[6]));
                            s1 = vaddq_f16(s1, vld1q_f16(inptr_xi + ofstab[7]));
                            s2 = vaddq_f16(s2, vld1q_f16(inptr_xi + ofstab[8]));

                            s0 = vaddq_f16(vaddq_f16(s0, s1), s2);
                            vst1q_f16(outptr + x0, vmulq_f16(s0, vscale));
                        }
                    } else {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x1)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vld1q_f16(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vaddq_f32(s0, vld1q_f16(inptr_xi + ofstab[k]));
                            vst1q_f16(outptr + x0, vmulq_f16(s0, vscale));
                        }
                    }
                }
            #endif
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = FX_FLOAT(inptr_xi[ofstab[0]]);
                    for (int k = 1; k < ksize; k++) {
                        float v = FX_FLOAT(inptr_xi[ofstab[k]]);
                        s += v;
                    }
                    outptr[x0] = FX_FLOAT16(s*avg_scale);
                }
                x1 = W0;
            }
        }
    }
}
#endif

static void _fx_maxpool_2d_f32(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    int Hi = pool->dw_ctx.Hi, Wi = pool->dw_ctx.Wi;
    int H0 = pool->dw_ctx.H0, W0 = pool->dw_ctx.W0;
    int inner_y0 = pool->dw_ctx.inner_ytop;
    int inner_y1 = pool->dw_ctx.inner_ybottom;
    int inner_x0 = pool->dw_ctx.inner_xleft;
    int inner_x1 = pool->dw_ctx.inner_xright;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->dw_ctx.yxtab;
    const int* ofstab = pool->dw_ctx.ofstab;
#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_F32;
    bool useSIMD = stride_x == 1 && inner_x0 < W0;
    bool is3x3 = pool->Hk == 3 && pool->Wk == 3;
#endif

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s = -FLT_MAX;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        float v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            float32x4_t s1 = vld1q_f32(inptr_xi + ofstab[1]);
                            float32x4_t s2 = vld1q_f32(inptr_xi + ofstab[2]);

                            s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[3]));
                            s1 = vmaxq_f32(s1, vld1q_f32(inptr_xi + ofstab[4]));
                            s2 = vmaxq_f32(s2, vld1q_f32(inptr_xi + ofstab[5]));

                            s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[6]));
                            s1 = vmaxq_f32(s1, vld1q_f32(inptr_xi + ofstab[7]));
                            s2 = vmaxq_f32(s2, vld1q_f32(inptr_xi + ofstab[8]));

                            s0 = vmaxq_f32(vmaxq_f32(s0, s1), s2);
                            vst1q_f32(outptr + x0, s0);
                        }
                    } else {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[k]));
                            vst1q_f32(outptr + x0, s0);
                        }
                    }
                }
            #endif
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const float* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        float v = inptr_xi[ofstab[k]];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                x1 = W0;
            }
        }
    }
}

#ifdef __ARM_NEON
static void _fx_maxpool_2d_f16(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool)
{
    const __fp16* inptr = (const __fp16*)inptr_;
    __fp16* outptr = (__fp16*)outptr_;
    int Hi = pool->dw_ctx.Hi, Wi = pool->dw_ctx.Wi;
    int H0 = pool->dw_ctx.H0, W0 = pool->dw_ctx.W0;
    int inner_y0 = pool->dw_ctx.inner_ytop;
    int inner_y1 = pool->dw_ctx.inner_ybottom;
    int inner_x0 = pool->dw_ctx.inner_xleft;
    int inner_x1 = pool->dw_ctx.inner_xright;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->dw_ctx.yxtab;
    const int* ofstab = pool->dw_ctx.ofstab;

    const int vec_nlanes = FX_VEC_NLANES_F16;
    bool useSIMD = stride_x == 1 && inner_x0 < W0;
    bool is3x3 = pool->Hk == 3 && pool->Wk == 3;

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                __fp16 s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s = -FLT16_MAX;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        __fp16 v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left;
                            const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vld1q_f16(inptr_xi + ofstab[0]);
                            float16x8_t s1 = vld1q_f16(inptr_xi + ofstab[1]);
                            float16x8_t s2 = vld1q_f16(inptr_xi + ofstab[2]);

                            s0 = vmaxq_f16(s0, vld1q_f16(inptr_xi + ofstab[3]));
                            s1 = vmaxq_f16(s1, vld1q_f16(inptr_xi + ofstab[4]));
                            s2 = vmaxq_f16(s2, vld1q_f16(inptr_xi + ofstab[5]));

                            s0 = vmaxq_f16(s0, vld1q_f16(inptr_xi + ofstab[6]));
                            s1 = vmaxq_f16(s1, vld1q_f16(inptr_xi + ofstab[7]));
                            s2 = vmaxq_f16(s2, vld1q_f16(inptr_xi + ofstab[8]));

                            s0 = vmaxq_f16(vmaxq_f16(s0, s1), s2);
                            vst1q_f16(outptr + x0, s0);
                        }
                    } else {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vld1q_f16(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vmaxq_f16(s0, vld1q_f16(inptr_xi + ofstab[k]));
                            vst1q_f16(outptr + x0, s0);
                        }
                    }
                }
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        __fp16 v = inptr_xi[ofstab[k]];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                x1 = W0;
            }
        }
    }
}
#endif

static void _fx_maxpool_2d_u8(int nc, const char* inptr_, char* outptr_,
                              const _fx_pooling2d_t* pool)
{
    const uint8_t* inptr = (const uint8_t*)inptr_;
    uint8_t* outptr = (uint8_t*)outptr_;
    int Hi = pool->dw_ctx.Hi, Wi = pool->dw_ctx.Wi;
    int H0 = pool->dw_ctx.H0, W0 = pool->dw_ctx.W0;
    int inner_y0 = pool->dw_ctx.inner_ytop;
    int inner_y1 = pool->dw_ctx.inner_ybottom;
    int inner_x0 = pool->dw_ctx.inner_xleft;
    int inner_x1 = pool->dw_ctx.inner_xright;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->dw_ctx.yxtab;
    const int* ofstab = pool->dw_ctx.ofstab;
#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_U8;
    bool useSIMD = stride_x == 1 && inner_x0 < W0;
    bool is3x3 = pool->Hk == 3 && pool->Wk == 3;
#endif

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                int s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s = 0;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        int v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = (uint8_t)s;
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left;
                            const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                            uint8x16_t s0 = vld1q_u8(inptr_xi + ofstab[0]);
                            uint8x16_t s1 = vld1q_u8(inptr_xi + ofstab[1]);
                            uint8x16_t s2 = vld1q_u8(inptr_xi + ofstab[2]);

                            s0 = vmaxq_u8(s0, vld1q_u8(inptr_xi + ofstab[3]));
                            s1 = vmaxq_u8(s1, vld1q_u8(inptr_xi + ofstab[4]));
                            s2 = vmaxq_u8(s2, vld1q_u8(inptr_xi + ofstab[5]));

                            s0 = vmaxq_u8(s0, vld1q_u8(inptr_xi + ofstab[6]));
                            s1 = vmaxq_u8(s1, vld1q_u8(inptr_xi + ofstab[7]));
                            s2 = vmaxq_u8(s2, vld1q_u8(inptr_xi + ofstab[8]));

                            s0 = vmaxq_u8(vmaxq_u8(s0, s1), s2);
                            vst1q_u8(outptr + x0, s0);
                        }
                    } else {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                            uint8x16_t s0 = vld1q_u8(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vmaxq_u8(s0, vld1q_u8(inptr_xi + ofstab[k]));
                            vst1q_u8(outptr + x0, s0);
                        }
                    }
                }
            #endif
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        int v = inptr_xi[ofstab[k]];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = (uint8_t)s;
                }
                x1 = W0;
            }
        }
    }
}

typedef void (*_fx_pool_func_t)(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool);

}

fun run_pooling(pool_typ: char, inp: Ast.nntensor_t, out: Ast.nntensor_t,
                kernel_shape_: int [], stride_: int [], dilation_: int [],
                padding_: int [], count_include_pad: bool,
                ntasks: int): void
@ccode {
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    int_ ndims = inp->shape.shape.dim[0].size;
    int_ k_ndims = kernel_shape_->dim[0].size;
    const int_* kernel_shape = (const int_*)(kernel_shape_->data);
    const int_* stride = (const int_*)(stride_->data);
    const int_* dilation = (const int_*)(dilation_->data);
    const int_* padding = (const int_*)(padding_->data);
    int inp_typ = inp->data.tag;
    size_t esz = inp_data->dim[0].step;
    const int_* inp_shape = (const int_*)inp->shape.shape.data;
    const int_* out_shape = (const int_*)out->shape.shape.data;
    int_ NC;
    size_t inp_planesize, out_planesize;
    int_ inner_y0, inner_y1;
    int_ inner_x0, inner_x1;
    _fx_pool_func_t func;
    int ksize = (int)(kernel_shape[0]*kernel_shape[1]);
    int* ofstab = (int*)alloca(ksize*3*sizeof(ofstab[0]));
    int* yxtab = ofstab + ksize;
    _fx_pooling2d_t pool;

    if (inp_typ != out->data.tag)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    if (inp->shape.layout.tag != _FX_NN_Layout_NCHW || k_ndims != 2)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims != 2 + k_ndims || ndims != out->shape.shape.dim[0].size ||
        inp_shape[0] != out_shape[0] || inp_shape[1] != out_shape[1])
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    memset(&pool, 0, sizeof(pool));
    NC = inp_shape[0]*inp_shape[1];
    pool.Hk = (int)kernel_shape[0];
    pool.Wk = (int)kernel_shape[1];
    pool.stride_y = (int)stride[0];
    pool.stride_x = (int)stride[1];
    pool.dilation_y = (int)dilation[0];
    pool.dilation_x = (int)dilation[1];
    pool.pad_top = (int)padding[0];
    pool.pad_left = (int)padding[1];
    pool.pad_bottom = (int)padding[2];
    pool.pad_right = (int)padding[3];
    pool.count_include_pad = count_include_pad;

    _fx_init_depthwise2d((int)inp_shape[0], (int)inp_shape[2], (int)inp_shape[3],
                         (int)out_shape[2], (int)out_shape[3],
                         pool.Hk, pool.Wk, pool.stride_y, pool.stride_x,
                         pool.dilation_y, pool.dilation_x,
                         pool.pad_top, pool.pad_left,
                         pool.pad_bottom, pool.pad_right,
                         yxtab, ofstab, &pool.dw_ctx);

    inp_planesize = (size_t)pool.dw_ctx.Hi*pool.dw_ctx.Wi;
    out_planesize = (size_t)pool.dw_ctx.H0*pool.dw_ctx.W0;

    if ((pool.Hk|pool.Wk) == 1 &&
        (pool.pad_left | pool.pad_right | pool.pad_top | pool.pad_bottom) != 0)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    /*printf("inpsize: %d x %d x %d x %d, outsize: %d x %d x %d x %d; kernel_size: %d x %d, stride: %d x %d, dilation: %d x %d; pad_y: (%d, %d), pad_x: (%d, %d), inner: y=%d - %d, x=%d - %d\n",
        pool.dw_ctx.N, (int)inp_shape[1], pool.dw_ctx.Hi, pool.dw_ctx.Wi,
        pool.dw_ctx.N, (int)out_shape[1], pool.dw_ctx.H0, pool.dw_ctx.W0,
        pool.Hk, pool.Wk, pool.stride_y, pool.stride_x, pool.dilation_y, pool.dilation_x,
        pool.pad_top, pool.pad_bottom, pool.pad_left, pool.pad_right,
        pool.dw_ctx.inner_ytop, pool.dw_ctx.inner_ybottom,
        pool.dw_ctx.inner_xleft, pool.dw_ctx.inner_xright);
    printf("ofstab: ");
    for(int k = 0; k < ksize; k++)
        printf("%d ", ofstab[k]);
    printf("\n");*/

    func =
        inp_typ == FX_U8 ?
            (pool_typ == 'M' ? _fx_maxpool_2d_u8 : 0) :
        inp_typ == FX_F32 ?
            (pool_typ == 'M' ? _fx_maxpool_2d_f32 :
            pool_typ == 'A' ? _fx_avgpool_2d_f32 : 0) :
    #if _FX_NN_ENABLE_FP16
        inp_typ == FX_F16 ?
            (pool_typ == 'M' ? _fx_maxpool_2d_f16 :
            pool_typ == 'A' ? _fx_avgpool_2d_f16 : 0) :
    #endif
        0;

    if (!func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (NC*out_planesize < 100000)
        ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ nc0 = task_id*NC/ntasks, nc1 = (task_id+1)*NC/ntasks;
        func((int)(nc1 - nc0),
            inp_data->data + inp_planesize*nc0*esz,
            out_data->data + out_planesize*nc0*esz,
            &pool);
    }

    return FX_OK;
}

fun run_maxpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_MaxPool {ceil_mode, dilations, kernel_shape, pads,
                  strides, storage_order, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_pooling('M', inp, out, kernel_shape, strides, dilations, pads, true, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_avgpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_AvgPool {ceil_mode, dilations, kernel_shape, pads,
    strides, count_include_pad, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_pooling('A', inp, out, kernel_shape, strides, dilations, pads,
                count_include_pad, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_global_avgpool(inp: Ast.nntensor_t, out: Ast.nntensor_t, ntasks: int): void
@ccode {
    int inp_typ = inp->data.tag, out_typ = out->data.tag;
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    const int_* inp_shape = (const int_*)inp->shape.shape.data;
    const int_* out_shape = (const int_*)out->shape.shape.data;
    int_ inp_ndims = inp->shape.shape.dim[0].size;
    int_ out_ndims = out->shape.shape.dim[0].size;
    int_ NC, planesize = 1;
    double scale;

    if (inp_typ != out_typ)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (inp_typ != FX_F32
        _FX_FP16_CASE(&& inp_typ != FX_F16)
        )
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (inp_ndims < 3 || inp_ndims != out_ndims)
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
    scale = planesize != 0 ? 1./planesize : 0.;
    /*printf("inp_typ=%d, planesize=%d, NC=%d, inp_shape=[%d, %d, %d, %d]\n",
        inp_typ, (int)planesize, (int)NC,
        (int)inp_shape[0], (int)inp_shape[1], (int)inp_shape[2], (int)inp_shape[3]);*/

    if (NC*planesize < 100000)
        ntasks = 1;
    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ nc0 = task_id*NC/ntasks, nc1 = (task_id + 1)*NC/ntasks;
        for (; nc0 < nc1; nc0++) {
            if (inp_typ == FX_F32) {
                const float* inptr = (const float*)inp_data->data + nc0*planesize;
                float* outptr = (float*)out_data->data + nc0;
                const int_ block_size = 64;
                double total = 0;
                for (int_ j = 0; j < planesize; ) {
                    int_ block_end = j + block_size < planesize ? j + block_size : planesize;
                    float s = 0;
                    for (; j < block_end; j++)
                        s += inptr[j];
                    total += s;
                }
                *outptr = (float)(total*scale);
            }
        #if _FX_NN_ENABLE_FP16
            else if (inp_typ == FX_F16) {
                const fx_f16* inptr = (const fx_f16*)inp_data->data + nc0*planesize;
                fx_f16* outptr = (fx_f16*)out_data->data + nc0;
                const int_ block_size = 256;
                double total = 0;
                for (int_ j = 0; j < planesize; ) {
                    int_ block_end = j + block_size < planesize ? j + block_size : planesize;
                    float s = 0;
                    for (; j < block_end; j++)
                        s += FX_FLOAT(inptr[j]);
                    total += s;
                }
                *outptr = FX_FLOAT16((float)(total*scale));
            }
        #endif
        }
    }
    return FX_OK;
}

fun run_global_avgpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_GlobalAvgPool {t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.NN_Layout_NCHW`)
    run_global_avgpool(inp, out, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
