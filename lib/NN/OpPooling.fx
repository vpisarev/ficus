/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include <alloca.h>
#include <float.h>
#include "ficus_nn_common.h"

typedef struct _fx_pooling2d_t
{
    _fx_depthwise2d_t dw_ctx;
    int Hk, Wk;
    int stride_y, stride_x;
    int dilation_y, dilation_x;
    int pad_top, pad_bottom, pad_left, pad_right;
    bool count_include_pad;
    float inp_scale, out_scale;
    int inp_zp, out_zp;
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

#if _FX_NN_ENABLE_FP16
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
                                s0 = vaddq_f16(s0, vld1q_f16(inptr_xi + ofstab[k]));
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

static void _fx_avgpool_2d_u8(int nc, const char* inptr_, char* outptr_,
                              const _fx_pooling2d_t* pool)
{
    const int C1 = FX_QCONV_C;
    const uint8_t* inptr = (const uint8_t*)inptr_;
    uint8_t* outptr = (uint8_t*)outptr_;
    int Hi = pool->dw_ctx.Hi, Wi = pool->dw_ctx.Wi;
    int H0 = pool->dw_ctx.H0, W0 = pool->dw_ctx.W0;
    int Hk = pool->Hk, Wk = pool->Wk;
    int inner_y0 = pool->dw_ctx.inner_ytop;
    int inner_y1 = pool->dw_ctx.inner_ybottom;
    int inner_x0 = pool->dw_ctx.inner_xleft;
    int inner_x1 = pool->dw_ctx.inner_xright;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->dw_ctx.yxtab;
    const int* ofstab_ = pool->dw_ctx.ofstab;
    int* ofstab = (int*)alloca(ksize*sizeof(ofstab[0]));
    float inp_scale = pool->inp_scale;
    float out_scale = pool->out_scale;
    int inp_zp = pool->inp_zp;
    int out_zp = pool->out_zp;
    bool count_include_pad = pool->count_include_pad;
    float scale_ = inp_scale/out_scale;
    float bias = out_zp - inp_zp*scale_;
    float scale = scale_/(Hk*Wk);
#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_U8;
    bool useSIMD = (stride_x == 1 || stride_x == 2) && inner_x0 < W0;
    bool is3x3 = stride_x == 1 && Hk == 3 && Wk == 3;
#endif
    for (int j = 0; j < ksize; j++)
        ofstab[j] = ofstab_[j]*C1;

    for (int c = 0; c < nc; c++, inptr += Hi*Wi*C1) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0*C1) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    int count = 0;
                    int s0 = 0, s1 = 0, s2 = 0, s3 = 0;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        int ofs;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        ofs = (yi*Wi + xi)*C1;
                        s0 += inptr[ofs];
                        s1 += inptr[ofs+1];
                        s2 += inptr[ofs+2];
                        s3 += inptr[ofs+3];
                        count++;
                    }
                    if (count_include_pad) {
                        s0 = s0*scale + bias;
                        s1 = s1*scale + bias;
                        s2 = s2*scale + bias;
                        s3 = s3*scale + bias;
                    } else {
                        float curr_scale = scale_/count;
                        s0 = s0*curr_scale + bias;
                        s1 = s1*curr_scale + bias;
                        s2 = s2*curr_scale + bias;
                        s3 = s3*curr_scale + bias;
                    }
                    int isum0 = (int)lrintf(s0);
                    int isum1 = (int)lrintf(s1);
                    int isum2 = (int)lrintf(s2);
                    int isum3 = (int)lrintf(s3);
                    outptr[x0*C1] = FX_SATURATE(isum0, 0);
                    outptr[x0*C1+1] = FX_SATURATE(isum1, 0);
                    outptr[x0*C1+2] = FX_SATURATE(isum2, 0);
                    outptr[x0*C1+3] = FX_SATURATE(isum3, 0);
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                    int s0 = 0, s1 = 0, s2 = 0, s3 = 0;
                    for (int k = 0; k < ksize; k++) {
                        int ofs = ofstab[k];
                        s0 += inptr[ofs];
                        s1 += inptr[ofs+1];
                        s2 += inptr[ofs+2];
                        s3 += inptr[ofs+3];
                    }
                    int isum0 = (int)lrintf(s0*scale + bias);
                    int isum1 = (int)lrintf(s1*scale + bias);
                    int isum2 = (int)lrintf(s2*scale + bias);
                    int isum3 = (int)lrintf(s3*scale + bias);
                    outptr[x0*C1] = FX_SATURATE(isum0, 0);
                    outptr[x0*C1+1] = FX_SATURATE(isum1, 0);
                    outptr[x0*C1+2] = FX_SATURATE(isum2, 0);
                    outptr[x0*C1+3] = FX_SATURATE(isum3, 0);
                }
                x1 = W0;
            }
        }
    }
}

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
                float s0;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s0 = -FLT_MAX;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        float v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s0 = FX_MAX(s0, v);
                    }
                    outptr[x0] = s0;
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
                    s0 = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        float v = inptr_xi[ofstab[k]];
                        s0 = FX_MAX(s0, v);
                    }
                    outptr[x0] = s0;
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
    int Hk = pool->Hk, Wk = pool->Wk;
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
    bool useSIMD = (stride_x == 1 || stride_x == 2) && inner_x0 < W0;
    bool is3x3 = stride_x == 1 && Hk == 3 && Wk == 3;

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                __fp16 s0;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s0 = -FLT16_MAX;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        __fp16 v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s0 = FX_MAX(s0, v);
                    }
                    outptr[x0] = s0;
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
                    } else if (stride_x == 1) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left;
                            const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vld1q_f16(inptr_xi + ofstab[0]);
                            for (int k = 1; k < ksize; k++)
                                s0 = vmaxq_f16(s0, vld1q_f16(inptr_xi + ofstab[k]));
                            vst1q_f16(outptr + x0, s0);
                        }
                    } else if (yi_ + (Hk-1)*pool->dilation_y < Hi-1) {
                        assert(stride_x == 2);
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left;
                            const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vld2q_f16(inptr_xi + ofstab[0]).val[0];
                            for (int k = 1; k < ksize; k++)
                                s0 = vmaxq_f16(s0, vld2q_f16(inptr_xi + ofstab[k]).val[0]);
                            vst1q_f16(outptr + x0, s0);
                        }
                    }
                }
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                    s0 = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        __fp16 v = inptr_xi[ofstab[k]];
                        s0 = FX_MAX(s0, v);
                    }
                    outptr[x0] = s0;
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
    const int C1 = FX_QCONV_C;
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
    const int* ofstab_ = pool->dw_ctx.ofstab;
    int* ofstab = (int*)alloca(ksize*sizeof(ofstab[0]));
#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_U8/8;
    bool useSIMD = (stride_x == 1 || stride_x == 2) && inner_x0 < W0;
    bool is3x3 = stride_x == 1 && pool->Hk == 3 && pool->Wk == 3;
#endif
    for (int j = 0; j < ksize; j++)
        ofstab[j] = ofstab_[j]*C1;

    for (int c = 0; c < nc; c++, inptr += Hi*Wi*C1) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0*C1) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    int s0 = 0, s1 = 0, s2 = 0, s3 = 0;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        int v0, v1, v2, v3, ofs;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        ofs = (yi*Wi + xi)*C1;
                        v0 = inptr[ofs];
                        v1 = inptr[ofs+1];
                        v2 = inptr[ofs+2];
                        v3 = inptr[ofs+3];
                        s0 = FX_MAX(s0, v0);
                        s1 = FX_MAX(s1, v1);
                        s2 = FX_MAX(s2, v2);
                        s3 = FX_MAX(s3, v3);
                    }
                    outptr[x0*C1] = (uint8_t)s0;
                    outptr[x0*C1+1] = (uint8_t)s1;
                    outptr[x0*C1+2] = (uint8_t)s2;
                    outptr[x0*C1+3] = (uint8_t)s3;
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
                            const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
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
                            vst1q_u8(outptr + x0*C1, s0);
                        }
                    } else if (stride_x == 1) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                            uint8x16_t s0 = vld1q_u8(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vmaxq_u8(s0, vld1q_u8(inptr_xi + ofstab[k]));
                            vst1q_u8(outptr + x0*C1, s0);
                        }
                    } else if (yi_ + (pool->Hk-1)*pool->dilation_y < Hi-1) {
                        assert(stride_x == 2);
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_x0)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                            uint8x16_t s0 = vdupq_n_u8(0);
                            for (k = 0; k < ksize; k++) {
                                uint32x4_t v0 = vld2q_u32((uint32_t*)(inptr_xi + ofstab[k])).val[0];
                                s0 = vmaxq_u8(s0, vreinterpretq_u32_u8(v0));
                            }
                            vst1q_u8(outptr + x0*C1, s0);
                        }
                    }
                }
            #endif
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                    int ofs = ofstab[0];
                    int s0 = inptr_xi[ofs], s1 = inptr_xi[ofs+1];
                    int s2 = inptr_xi[ofs+2], s3 = inptr_xi[ofs+3];
                    for (int k = 1; k < ksize; k++) {
                        ofs = ofstab[k];
                        int v0 = inptr_xi[ofs];
                        int v1 = inptr_xi[ofs+1];
                        int v2 = inptr_xi[ofs+2];
                        int v3 = inptr_xi[ofs+3];
                        s0 = FX_MAX(s0, v0);
                        s1 = FX_MAX(s1, v1);
                        s2 = FX_MAX(s2, v2);
                        s3 = FX_MAX(s3, v3);
                    }
                    outptr[x0*C1] = (uint8_t)s0;
                    outptr[x0*C1+1] = (uint8_t)s1;
                    outptr[x0*C1+2] = (uint8_t)s2;
                    outptr[x0*C1+3] = (uint8_t)s3;
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
                inp_out_scale_zp: (float, int, float, int)?, ntasks: int): void
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

    if (k_ndims != 2)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (ndims < 3 || ndims != out->shape.shape.dim[0].size ||
        inp_shape[0] != out_shape[0] || inp_shape[1] != out_shape[1])
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    if (inp_typ == FX_F16 || inp_typ == FX_F32) {
        if (inp->shape.layout.tag != _FX_NN_Layout_NCHW)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        if (ndims != 2 + k_ndims)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    } else if (inp_typ == FX_U8) {
        if (inp->shape.layout.tag != _FX_NN_Layout_NCXHWX)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        if (ndims != 3 + k_ndims || inp_shape[ndims-1] != out_shape[ndims-1])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (inp_shape[ndims-1] != 4)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        esz *= inp_shape[ndims-1];
    }

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

    if (inp_out_scale_zp->tag > 1) {
        pool.inp_scale = inp_out_scale_zp->u.Some.t0;
        pool.inp_zp = (int)inp_out_scale_zp->u.Some.t1;
        pool.out_scale = inp_out_scale_zp->u.Some.t2;
        pool.out_zp = (int)inp_out_scale_zp->u.Some.t3;
    }

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
            (pool_typ == 'M' ? _fx_maxpool_2d_u8 :
             pool_typ == 'A' ? _fx_avgpool_2d_u8 : 0) :
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

    if ((size_t)NC*out_planesize*pool.Hk*pool.Wk < 100000)
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
| Ast.NN_MaxPool {ceil_mode, attr, storage_order, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_pooling('M', inp, out, attr.kernel_shape, attr.strides, attr.dilations, attr.pads, true, None, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_avgpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_AvgPool {ceil_mode, attr, count_include_pad, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_pooling('A', inp, out, attr.kernel_shape, attr.strides, attr.dilations, attr.pads,
                count_include_pad, None, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_qavgpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_QLinearAvgPool {ceil_mode, attr, count_include_pad, t_inp,
    t_inp_scale, t_inp_zp, t_out_scale, t_out_zp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val inp_scale = model.get_tensor(t_inp_scale).data.float_scalar_or(1.f)
    val inp_zp = model.get_tensor(t_inp_zp).data.int_scalar_or(0)
    val out_scale = model.get_tensor(t_inp_scale).data.float_scalar_or(1.f)
    val out_zp = model.get_tensor(t_out_zp).data.int_scalar_or(0)
    val out = model.get_tensor(t_out)
    run_pooling('A', inp, out, attr.kernel_shape, attr.strides, attr.dilations, attr.pads,
                count_include_pad, Some((inp_scale, inp_zp, out_scale, out_zp)), *model.ntasks)
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
