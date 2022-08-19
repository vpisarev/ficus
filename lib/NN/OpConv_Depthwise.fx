/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// depthwise fp32 & fp16 convolution.
@ccode {
#include "ficus_nn_common.h"

int _fx_depthwise_conv2d_f32(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_conv2d_t* conv,
                             const char* inptr0, char* outptr0,
                             int ntasks)
{
    int Hi = dw_ctx->Hi, Wi = dw_ctx->Wi, H0 = dw_ctx->H0, W0 = dw_ctx->W0;
    int Hk = conv->Hk, Wk = conv->Wk;
    int stride_x = conv->stride_x, stride_y = conv->stride_y;
    int pad_top = conv->pad_top, pad_left = conv->pad_left;
    int_ NC = dw_ctx->N*conv->C;
    size_t inp_planesize = Hi*Wi, out_planesize = H0*W0;
    int ksize = Hk*Wk;
    int padded_ksize = (ksize + FX_VEC_NLANES_F32-1) & -FX_VEC_NLANES_F32;
    const int *ofstab = dw_ctx->ofstab, *yxtab = dw_ctx->yxtab;
    int inner_ytop = dw_ctx->inner_ytop;
    int inner_ybottom = dw_ctx->inner_ybottom;
    int inner_xleft = dw_ctx->inner_xleft;
    int inner_xright = dw_ctx->inner_xright;
    float minval = conv->minval, maxval = conv->maxval;
    float alpha = conv->alpha;
    bool fast_activ = conv->activ == _FX_ACTIV_RELU ||
                      (conv->activ == _FX_ACTIV_CLIP && minval == 0.f) ||
                      conv->activ == _FX_ACTIV_LRELU;
    _fx_activ_func_t activ_func = fast_activ ||
        conv->activ == _FX_ACTIV_NONE ? 0 : conv->activ_func;

#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_F32;
    float32x4_t valpha = vdupq_n_f32(alpha), vmaxval = vdupq_n_f32(maxval);
    float32x4_t z = vdupq_n_f32(0.f), one = vdupq_n_f32(1.f);
    bool useSIMD = (stride_x == 1 || stride_x == 2) && inner_xleft < W0;
    bool is3x3 = stride_x == 1 && Hk == 3 && Wk == 3;
    bool is3x3_r3 = is3x3 && conv->stride_y == 1 &&
        conv->dilation_y == 1 && conv->dilation_x == 1;
#endif

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int_ nc = 0; nc < NC; nc++) {
        int c = (int)(nc % conv->C), dy0 = 1;
        const float* inptr = (const float*)inptr0 + inp_planesize*nc;
        float* outptr = (float*)outptr0 + out_planesize*nc;
        float biasval = conv->bias[c];
        const float* weights = conv->weights + c*padded_ksize;
#ifdef __ARM_NEON
        float32x4_t w0=vdupq_n_f32(0.f), w1=w0, w2=w0, w3=w0, w4=w0, w5=w0, w6=w0, w7=w0, w8=w0, vbias = w0;
        if (useSIMD) {
            vbias = vdupq_n_f32(biasval);
            if (is3x3) {
                w0 = vdupq_n_f32(weights[0]);
                w1 = vdupq_n_f32(weights[1]);
                w2 = vdupq_n_f32(weights[2]);
                w3 = vdupq_n_f32(weights[3]);
                w4 = vdupq_n_f32(weights[4]);
                w5 = vdupq_n_f32(weights[5]);
                w6 = vdupq_n_f32(weights[6]);
                w7 = vdupq_n_f32(weights[7]);
                w8 = vdupq_n_f32(weights[8]);
            }
        }
#endif
        for (int y0 = 0; y0 < H0; y0 += dy0, outptr += W0*dy0) {
        #ifdef __ARM_NEON
            dy0 = inner_ytop <= y0 && y0+3 < inner_ybottom && is3x3_r3 ? 3 : 1;
        #endif
            int x0 = 0, x1 = y0 >= inner_ytop && y0 < inner_ybottom ? inner_xleft : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s_0, s_1, s_2;
                if (dy0 == 3) {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        s_0 = s_1 = s_2 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            int yi = yi_ + yxtab[k*2];
                            int xi = xi_ + yxtab[k*2+1];
                            float w = weights[k];
                            if ((unsigned)xi < (unsigned)Wi) {
                                s_0 += inptr[yi*Wi + xi]*w;
                                s_1 += inptr[(yi+1)*Wi + xi]*w;
                                s_2 += inptr[(yi+2)*Wi + xi]*w;
                            }
                        }
                        s_0 = s_0 <= maxval ? s_0 : maxval;
                        s_0 *= (s_0 < 0.f ? alpha : 1.f);
                        s_1 = s_1 <= maxval ? s_1 : maxval;
                        s_1 *= (s_1 < 0.f ? alpha : 1.f);
                        s_2 = s_2 <= maxval ? s_2 : maxval;
                        s_2 *= (s_2 < 0.f ? alpha : 1.f);
                        outptr[x0] = s_0;
                        outptr[x0 + W0] = s_1;
                        outptr[x0 + W0*2] = s_2;
                    }
                } else {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        s_0 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            int yi = yi_ + yxtab[k*2];
                            int xi = xi_ + yxtab[k*2+1];
                            float w = weights[k];
                            if (((unsigned)yi < (unsigned)Hi) & ((unsigned)xi < (unsigned)Wi))
                                s_0 += inptr[yi*Wi + xi]*w;
                        }
                        s_0 = s_0 <= maxval ? s_0 : maxval;
                        s_0 *= (s_0 < 0.f ? alpha : 1.f);
                        outptr[x0] = s_0;
                    }
                }
                if (x0 == W0)
                    break;
                x1 = inner_xright;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        if (dy0 == 3) {
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 <= inner_xleft)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                }
                                int xi_ = x0*stride_x - pad_left;
                                const float* inptr_xi = inptr + Wi*yi_ + xi_;
                                float32x4_t s0, s1, s2;
                                float32x4_t x00 = vld1q_f32(inptr_xi);
                                float32x4_t x01 = vld1q_f32(inptr_xi + 1);
                                float32x4_t x02 = vld1q_f32(inptr_xi + 2);

                                float32x4_t x10 = vld1q_f32(inptr_xi + Wi);
                                float32x4_t x11 = vld1q_f32(inptr_xi + Wi + 1);
                                float32x4_t x12 = vld1q_f32(inptr_xi + Wi + 2);

                                float32x4_t x20 = vld1q_f32(inptr_xi + Wi*2);
                                float32x4_t x21 = vld1q_f32(inptr_xi + Wi*2 + 1);
                                float32x4_t x22 = vld1q_f32(inptr_xi + Wi*2 + 2);

                                float32x4_t x30 = vld1q_f32(inptr_xi + Wi*3);
                                float32x4_t x31 = vld1q_f32(inptr_xi + Wi*3 + 1);
                                float32x4_t x32 = vld1q_f32(inptr_xi + Wi*3 + 2);

                                float32x4_t x40 = vld1q_f32(inptr_xi + Wi*4);
                                float32x4_t x41 = vld1q_f32(inptr_xi + Wi*4 + 1);
                                float32x4_t x42 = vld1q_f32(inptr_xi + Wi*4 + 2);

                                s0 = vfmaq_f32(vbias, x00, w0);
                                s1 = vfmaq_f32(vbias, x10, w0);
                                s2 = vfmaq_f32(vbias, x20, w0);

                                s0 = vfmaq_f32(s0, x01, w1);
                                s1 = vfmaq_f32(s1, x11, w1);
                                s2 = vfmaq_f32(s2, x21, w1);

                                s0 = vfmaq_f32(s0, x02, w2);
                                s1 = vfmaq_f32(s1, x12, w2);
                                s2 = vfmaq_f32(s2, x22, w2);

                                s0 = vfmaq_f32(s0, x10, w3);
                                s1 = vfmaq_f32(s1, x20, w3);
                                s2 = vfmaq_f32(s2, x30, w3);

                                s0 = vfmaq_f32(s0, x11, w4);
                                s1 = vfmaq_f32(s1, x21, w4);
                                s2 = vfmaq_f32(s2, x31, w4);

                                s0 = vfmaq_f32(s0, x12, w5);
                                s1 = vfmaq_f32(s1, x22, w5);
                                s2 = vfmaq_f32(s2, x32, w5);

                                s0 = vfmaq_f32(s0, x20, w6);
                                s1 = vfmaq_f32(s1, x30, w6);
                                s2 = vfmaq_f32(s2, x40, w6);

                                s0 = vfmaq_f32(s0, x21, w7);
                                s1 = vfmaq_f32(s1, x31, w7);
                                s2 = vfmaq_f32(s2, x41, w7);

                                s0 = vfmaq_f32(s0, x22, w8);
                                s1 = vfmaq_f32(s1, x32, w8);
                                s2 = vfmaq_f32(s2, x42, w8);

                                s0 = vmulq_f32(vminq_f32(s0, vmaxval),
                                               vbslq_f32(vcltq_f32(s0, z), valpha, one));
                                s1 = vmulq_f32(vminq_f32(s1, vmaxval),
                                               vbslq_f32(vcltq_f32(s1, z), valpha, one));
                                s2 = vmulq_f32(vminq_f32(s2, vmaxval),
                                               vbslq_f32(vcltq_f32(s2, z), valpha, one));
                                vst1q_f32(outptr + x0, s0);
                                vst1q_f32(outptr + W0 + x0, s1);
                                vst1q_f32(outptr + W0*2 + x0, s2);
                            }
                        } else {
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 <= inner_xleft)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                }
                                int xi_ = x0*stride_x - pad_left;
                                const float* inptr_xi = inptr + Wi*yi_ + xi_;
                                float32x4_t s0 = vfmaq_f32(vbias, vld1q_f32(inptr_xi + ofstab[0]), w0);
                                float32x4_t s1 = vmulq_f32(vld1q_f32(inptr_xi + ofstab[1]), w1);
                                float32x4_t s2 = vmulq_f32(vld1q_f32(inptr_xi + ofstab[2]), w2);

                                s0 = vfmaq_f32(s0, vld1q_f32(inptr_xi + ofstab[3]), w3);
                                s1 = vfmaq_f32(s1, vld1q_f32(inptr_xi + ofstab[4]), w4);
                                s2 = vfmaq_f32(s2, vld1q_f32(inptr_xi + ofstab[5]), w5);

                                s0 = vfmaq_f32(s0, vld1q_f32(inptr_xi + ofstab[6]), w6);
                                s1 = vfmaq_f32(s1, vld1q_f32(inptr_xi + ofstab[7]), w7);
                                s2 = vfmaq_f32(s2, vld1q_f32(inptr_xi + ofstab[8]), w8);

                                s0 = vaddq_f32(vaddq_f32(s0, s1), s2);
                                s0 = vmulq_f32(vminq_f32(s0, vmaxval),
                                               vbslq_f32(vcltq_f32(s0, z), valpha, one));
                                vst1q_f32(outptr + x0, s0);
                            }
                        }
                    } else if (stride_x == 1) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_xleft)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vbias;
                            for (; k <= ksize - 4; k += 4) {
                                float32x4_t v0 = vld1q_f32(inptr_xi + ofstab[k]);
                                float32x4_t v1 = vld1q_f32(inptr_xi + ofstab[k+1]);
                                float32x4_t v2 = vld1q_f32(inptr_xi + ofstab[k+2]);
                                float32x4_t v3 = vld1q_f32(inptr_xi + ofstab[k+3]);
                                float32x4_t ww = vld1q_f32(weights + k);
                                s0 = vfmaq_laneq_f32(s0, v0, ww, 0);
                                s0 = vfmaq_laneq_f32(s0, v1, ww, 1);
                                s0 = vfmaq_laneq_f32(s0, v2, ww, 2);
                                s0 = vfmaq_laneq_f32(s0, v3, ww, 3);
                            }
                            for (; k < ksize; k++)
                                s0 = vfmaq_f32(s0, vld1q_f32(inptr_xi + ofstab[k]),
                                               vdupq_n_f32(weights[k]));
                            s0 = vmulq_f32(vminq_f32(s0, vmaxval),
                                           vbslq_f32(vcltq_f32(s0, z), valpha, one));
                            vst1q_f32(outptr + x0, s0);
                        }
                    } else if (yi_ + (Hk-1)*conv->dilation_y < Hi-1) {
                        assert(stride_x == 2);
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_xleft)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vbias;
                            for (; k <= ksize - 4; k += 4) {
                                float32x4_t v0 = vld2q_f32(inptr_xi + ofstab[k]).val[0];
                                float32x4_t v1 = vld2q_f32(inptr_xi + ofstab[k+1]).val[0];
                                float32x4_t v2 = vld2q_f32(inptr_xi + ofstab[k+2]).val[0];
                                float32x4_t v3 = vld2q_f32(inptr_xi + ofstab[k+3]).val[0];
                                float32x4_t ww = vld1q_f32(weights + k);
                                s0 = vfmaq_laneq_f32(s0, v0, ww, 0);
                                s0 = vfmaq_laneq_f32(s0, v1, ww, 1);
                                s0 = vfmaq_laneq_f32(s0, v2, ww, 2);
                                s0 = vfmaq_laneq_f32(s0, v3, ww, 3);
                            }
                            for (; k < ksize; k++)
                                s0 = vfmaq_f32(s0, vld2q_f32(inptr_xi + ofstab[k]).val[0],
                                               vdupq_n_f32(weights[k]));
                            s0 = vmulq_f32(vminq_f32(s0, vmaxval),
                                           vbslq_f32(vcltq_f32(s0, z), valpha, one));
                            vst1q_f32(outptr + x0, s0);
                        }
                    }
                }
            #endif
                if (dy0 == 3) {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        const float* inptr_xi = inptr + Wi*yi_ + xi_;
                        s_0 = s_1 = s_2 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            int inp_ofs = ofstab[k];
                            float w = weights[k];
                            s_0 += inptr_xi[inp_ofs]*w;
                            s_1 += inptr_xi[inp_ofs + Wi]*w;
                            s_2 += inptr_xi[inp_ofs + Wi*2]*w;
                        }
                        s_0 = s_0 <= maxval ? s_0 : maxval;
                        s_0 *= (s_0 < 0.f ? alpha : 1.f);
                        s_1 = s_1 <= maxval ? s_1 : maxval;
                        s_1 *= (s_1 < 0.f ? alpha : 1.f);
                        s_2 = s_2 <= maxval ? s_2 : maxval;
                        s_2 *= (s_2 < 0.f ? alpha : 1.f);
                        outptr[x0] = s_0;
                        outptr[x0 + W0] = s_1;
                        outptr[x0 + W0*2] = s_2;
                    }
                } else {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        const float* inptr_xi = inptr + Wi*yi_ + xi_;
                        s_0 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            s_0 += inptr_xi[ofstab[k]]*weights[k];
                        }
                        s_0 = s_0 <= maxval ? s_0 : maxval;
                        s_0 *= (s_0 < 0.f ? alpha : 1.f);
                        outptr[x0] = s_0;
                    }
                }
                x1 = W0;
            }
        }
        if (activ_func)
            activ_func(outptr, outptr, (int_)out_planesize, conv->activ_params);
    }
    return FX_OK;
}

#if _FX_NN_ENABLE_FP16
int _fx_depthwise_conv2d_f16(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_conv2d_t* conv,
                             const char* inptr0, char* outptr0,
                             int ntasks)
{
    int Hi = dw_ctx->Hi, Wi = dw_ctx->Wi, H0 = dw_ctx->H0, W0 = dw_ctx->W0;
    int Hk = conv->Hk, Wk = conv->Wk;
    int stride_x = conv->stride_x, stride_y = conv->stride_y;
    int pad_top = conv->pad_top, pad_left = conv->pad_left;
    int_ NC = dw_ctx->N*conv->C;
    size_t inp_planesize = Hi*Wi, out_planesize = H0*W0;
    int ksize = Hk*Wk;
    int padded_ksize_f32 = (ksize + FX_VEC_NLANES_F32-1) & -FX_VEC_NLANES_F32;
    int padded_ksize = (ksize + FX_VEC_NLANES_F16-1) & -FX_VEC_NLANES_F16;
    const int *ofstab = dw_ctx->ofstab, *yxtab = dw_ctx->yxtab;
    int inner_ytop = dw_ctx->inner_ytop;
    int inner_ybottom = dw_ctx->inner_ybottom;
    int inner_xleft = dw_ctx->inner_xleft;
    int inner_xright = dw_ctx->inner_xright;
    float minval = conv->minval, maxval = conv->maxval;
    float alpha = conv->alpha;
    bool fast_activ = conv->activ == _FX_ACTIV_RELU ||
                      (conv->activ == _FX_ACTIV_CLIP && minval == 0.f) ||
                      conv->activ == _FX_ACTIV_LRELU;
    _fx_activ_func_t activ_func = fast_activ ||
        conv->activ == _FX_ACTIV_NONE ? 0 : conv->activ_func_f16;

#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_F16;
    float16x8_t valpha = vdupq_n_f16(alpha), vmaxval = vdupq_n_f16(maxval);
    float16x8_t z = vdupq_n_f16(0.f), one = vdupq_n_f16(1.f);
    bool useSIMD = (stride_x == 1 || stride_x == 2) && inner_xleft < W0;
    bool is3x3 = stride_x == 1 && Hk == 3 && Wk == 3;
    bool is3x3_r3 = is3x3 && conv->stride_y == 1 &&
        conv->dilation_y == 1 && conv->dilation_x == 1;
#endif

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int nc = 0; nc < NC; nc++) {
        int c = nc % conv->C, dy0 = 1;
        const fx_f16* inptr = (const fx_f16*)inptr0 + inp_planesize*nc;
        fx_f16* outptr = (fx_f16*)outptr0 + out_planesize*nc;
        float biasval = conv->bias[c];
        const float* w_f32 = conv->weights + c*padded_ksize_f32;
        const fx_f16* weights = conv->wf16 + c*padded_ksize;
#ifdef __ARM_NEON
        float16x8_t w0=vdupq_n_f16(0.f), w1=w0, w2=w0, w3=w0, w4=w0, w5=w0, w6=w0, w7=w0, w8=w0, vbias = w0;
        if (useSIMD) {
            vbias = vdupq_n_f16(biasval);
            if (is3x3) {
                w0 = vdupq_n_f16(weights[0]);
                w1 = vdupq_n_f16(weights[1]);
                w2 = vdupq_n_f16(weights[2]);
                w3 = vdupq_n_f16(weights[3]);
                w4 = vdupq_n_f16(weights[4]);
                w5 = vdupq_n_f16(weights[5]);
                w6 = vdupq_n_f16(weights[6]);
                w7 = vdupq_n_f16(weights[7]);
                w8 = vdupq_n_f16(weights[8]);
            }
        }
#endif
        for (int y0 = 0; y0 < H0; y0 += dy0, outptr += W0*dy0) {
        #ifdef __ARM_NEON
            dy0 = inner_ytop <= y0 && y0+3 < inner_ybottom && is3x3_r3 ? 3 : 1;
        #endif
            int x0 = 0, x1 = y0 >= inner_ytop && y0 < inner_ybottom ? inner_xleft : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s_0, s_1, s_2;
                if (dy0 == 3) {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        s_0 = s_1 = s_2 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            int yi = yi_ + yxtab[k*2];
                            int xi = xi_ + yxtab[k*2+1];
                            float w = w_f32[k];
                            if ((unsigned)xi < (unsigned)Wi) {
                                s_0 += FX_FLOAT(inptr[yi*Wi + xi])*w;
                                s_1 += FX_FLOAT(inptr[(yi+1)*Wi + xi])*w;
                                s_2 += FX_FLOAT(inptr[(yi+2)*Wi + xi])*w;
                            }
                        }
                        s_0 = s_0 <= maxval ? s_0 : maxval;
                        s_0 *= (s_0 < 0.f ? alpha : 1.f);
                        s_1 = s_1 <= maxval ? s_1 : maxval;
                        s_1 *= (s_1 < 0.f ? alpha : 1.f);
                        s_2 = s_2 <= maxval ? s_2 : maxval;
                        s_2 *= (s_2 < 0.f ? alpha : 1.f);
                        outptr[x0] = FX_FLOAT16(s_0);
                        outptr[x0 + W0] = FX_FLOAT16(s_1);
                        outptr[x0 + W0*2] = FX_FLOAT16(s_2);
                    }
                } else {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        s_0 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            int yi = yi_ + yxtab[k*2];
                            int xi = xi_ + yxtab[k*2+1];
                            float w = w_f32[k];
                            if (((unsigned)yi < (unsigned)Hi) & ((unsigned)xi < (unsigned)Wi))
                                s_0 += FX_FLOAT(inptr[yi*Wi + xi])*w;
                        }
                        s_0 = s_0 <= maxval ? s_0 : maxval;
                        s_0 *= (s_0 < 0.f ? alpha : 1.f);
                        outptr[x0] = FX_FLOAT16(s_0);
                    }
                }
                if (x0 == W0)
                    break;
                x1 = inner_xright;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        if (dy0 == 3) {
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 <= inner_xleft)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                }
                                int xi_ = x0 - pad_left;
                                const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                                float16x8_t s0, s1, s2;
                                float16x8_t x00 = vld1q_f16(inptr_xi);
                                float16x8_t x01 = vld1q_f16(inptr_xi + 1);
                                float16x8_t x02 = vld1q_f16(inptr_xi + 2);

                                float16x8_t x10 = vld1q_f16(inptr_xi + Wi);
                                float16x8_t x11 = vld1q_f16(inptr_xi + Wi + 1);
                                float16x8_t x12 = vld1q_f16(inptr_xi + Wi + 2);

                                float16x8_t x20 = vld1q_f16(inptr_xi + Wi*2);
                                float16x8_t x21 = vld1q_f16(inptr_xi + Wi*2 + 1);
                                float16x8_t x22 = vld1q_f16(inptr_xi + Wi*2 + 2);

                                float16x8_t x30 = vld1q_f16(inptr_xi + Wi*3);
                                float16x8_t x31 = vld1q_f16(inptr_xi + Wi*3 + 1);
                                float16x8_t x32 = vld1q_f16(inptr_xi + Wi*3 + 2);

                                float16x8_t x40 = vld1q_f16(inptr_xi + Wi*4);
                                float16x8_t x41 = vld1q_f16(inptr_xi + Wi*4 + 1);
                                float16x8_t x42 = vld1q_f16(inptr_xi + Wi*4 + 2);

                                s0 = vfmaq_f16(vbias, x00, w0);
                                s1 = vfmaq_f16(vbias, x10, w0);
                                s2 = vfmaq_f16(vbias, x20, w0);

                                s0 = vfmaq_f16(s0, x01, w1);
                                s1 = vfmaq_f16(s1, x11, w1);
                                s2 = vfmaq_f16(s2, x21, w1);

                                s0 = vfmaq_f16(s0, x02, w2);
                                s1 = vfmaq_f16(s1, x12, w2);
                                s2 = vfmaq_f16(s2, x22, w2);

                                s0 = vfmaq_f16(s0, x10, w3);
                                s1 = vfmaq_f16(s1, x20, w3);
                                s2 = vfmaq_f16(s2, x30, w3);

                                s0 = vfmaq_f16(s0, x11, w4);
                                s1 = vfmaq_f16(s1, x21, w4);
                                s2 = vfmaq_f16(s2, x31, w4);

                                s0 = vfmaq_f16(s0, x12, w5);
                                s1 = vfmaq_f16(s1, x22, w5);
                                s2 = vfmaq_f16(s2, x32, w5);

                                s0 = vfmaq_f16(s0, x20, w6);
                                s1 = vfmaq_f16(s1, x30, w6);
                                s2 = vfmaq_f16(s2, x40, w6);

                                s0 = vfmaq_f16(s0, x21, w7);
                                s1 = vfmaq_f16(s1, x31, w7);
                                s2 = vfmaq_f16(s2, x41, w7);

                                s0 = vfmaq_f16(s0, x22, w8);
                                s1 = vfmaq_f16(s1, x32, w8);
                                s2 = vfmaq_f16(s2, x42, w8);

                                s0 = vmulq_f16(vminq_f16(s0, vmaxval), vbslq_f16(vcltq_f16(s0, z), valpha, one));
                                s1 = vmulq_f16(vminq_f16(s1, vmaxval), vbslq_f16(vcltq_f16(s1, z), valpha, one));
                                s2 = vmulq_f16(vminq_f16(s2, vmaxval), vbslq_f16(vcltq_f16(s2, z), valpha, one));
                                vst1q_f16(outptr + x0, s0);
                                vst1q_f16(outptr + W0 + x0, s1);
                                vst1q_f16(outptr + W0*2 + x0, s2);
                            }
                        } else {
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 <= inner_xleft)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                }
                                int xi_ = x0*stride_x - pad_left;
                                const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                                float16x8_t s0 = vfmaq_f16(vbias, vld1q_f16(inptr_xi + ofstab[0]), w0);
                                float16x8_t s1 = vmulq_f16(vld1q_f16(inptr_xi + ofstab[1]), w1);
                                float16x8_t s2 = vmulq_f16(vld1q_f16(inptr_xi + ofstab[2]), w2);

                                s0 = vfmaq_f16(s0, vld1q_f16(inptr_xi + ofstab[3]), w3);
                                s1 = vfmaq_f16(s1, vld1q_f16(inptr_xi + ofstab[4]), w4);
                                s2 = vfmaq_f16(s2, vld1q_f16(inptr_xi + ofstab[5]), w5);

                                s0 = vfmaq_f16(s0, vld1q_f16(inptr_xi + ofstab[6]), w6);
                                s1 = vfmaq_f16(s1, vld1q_f16(inptr_xi + ofstab[7]), w7);
                                s2 = vfmaq_f16(s2, vld1q_f16(inptr_xi + ofstab[8]), w8);

                                s0 = vaddq_f16(vaddq_f16(s0, s1), s2);
                                s0 = vmulq_f16(vminq_f16(s0, vmaxval), vbslq_f16(vcltq_f16(s0, z), valpha, one));
                                vst1q_f16(outptr + x0, s0);
                            }
                        }
                    } else if (stride_x == 1) {
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_xleft)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vbias;
                            for (; k <= ksize - 4; k += 4) {
                                float16x8_t v0 = vld1q_f16(inptr_xi + ofstab[k]);
                                float16x8_t v1 = vld1q_f16(inptr_xi + ofstab[k+1]);
                                float16x8_t v2 = vld1q_f16(inptr_xi + ofstab[k+2]);
                                float16x8_t v3 = vld1q_f16(inptr_xi + ofstab[k+3]);
                                float16x4_t ww = vld1_f16(weights + k);
                                s0 = vfmaq_lane_f16(s0, v0, ww, 0);
                                s0 = vfmaq_lane_f16(s0, v1, ww, 1);
                                s0 = vfmaq_lane_f16(s0, v2, ww, 2);
                                s0 = vfmaq_lane_f16(s0, v3, ww, 3);
                            }
                            for (; k < ksize; k++)
                                s0 = vfmaq_f16(s0, vld1q_f16(inptr_xi + ofstab[k]), vdupq_n_f16(weights[k]));
                            s0 = vmulq_f16(vminq_f16(s0, vmaxval), vbslq_f16(vcltq_f16(s0, z), valpha, one));
                            vst1q_f16(outptr + x0, s0);
                        }
                    } else if (yi_ + (Hk-1)*conv->dilation_y < Hi-1) {
                        assert(stride_x == 2);
                        for (; x0 < x1; x0 += vec_nlanes) {
                            if (x0 + vec_nlanes > x1) {
                                if (x0 <= inner_xleft)
                                    break;
                                x0 = x1 - vec_nlanes;
                            }
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vbias;
                            for (; k <= ksize - 4; k += 4) {
                                float16x8_t v0 = vld2q_f16(inptr_xi + ofstab[k]).val[0];
                                float16x8_t v1 = vld2q_f16(inptr_xi + ofstab[k+1]).val[0];
                                float16x8_t v2 = vld2q_f16(inptr_xi + ofstab[k+2]).val[0];
                                float16x8_t v3 = vld2q_f16(inptr_xi + ofstab[k+3]).val[0];
                                float16x4_t ww = vld1_f16(weights + k);
                                s0 = vfmaq_lane_f16(s0, v0, ww, 0);
                                s0 = vfmaq_lane_f16(s0, v1, ww, 1);
                                s0 = vfmaq_lane_f16(s0, v2, ww, 2);
                                s0 = vfmaq_lane_f16(s0, v3, ww, 3);
                            }
                            for (; k < ksize; k++)
                                s0 = vfmaq_f16(s0, vld2q_f16(inptr_xi + ofstab[k]).val[0],
                                               vdupq_n_f16(weights[k]));
                            s0 = vmulq_f16(vminq_f16(s0, vmaxval),
                                           vbslq_f16(vcltq_f16(s0, z), valpha, one));
                            vst1q_f16(outptr + x0, s0);
                        }
                    }
                }
            #endif
                if (dy0 == 3) {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                        s_0 = s_1 = s_2 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            int inp_ofs = ofstab[k];
                            float w = w_f32[k];
                            s_0 += FX_FLOAT(inptr_xi[inp_ofs])*w;
                            s_1 += FX_FLOAT(inptr_xi[inp_ofs + Wi])*w;
                            s_2 += FX_FLOAT(inptr_xi[inp_ofs + Wi*2])*w;
                        }
                        s_0 = s_0 <= maxval ? s_0 : maxval;
                        s_0 *= (s_0 < 0.f ? alpha : 1.f);
                        s_1 = s_1 <= maxval ? s_1 : maxval;
                        s_1 *= (s_1 < 0.f ? alpha : 1.f);
                        s_2 = s_2 <= maxval ? s_2 : maxval;
                        s_2 *= (s_2 < 0.f ? alpha : 1.f);
                        outptr[x0] = FX_FLOAT16(s_0);
                        outptr[x0 + W0] = FX_FLOAT16(s_1);
                        outptr[x0 + W0*2] = FX_FLOAT16(s_2);
                    }
                } else {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                        s_0 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            s_0 += FX_FLOAT(inptr_xi[ofstab[k]])*weights[k];
                        }
                        s_0 = s_0 <= maxval ? s_0 : maxval;
                        s_0 *= (s_0 < 0.f ? alpha : 1.f);
                        outptr[x0] = FX_FLOAT16(s_0);
                    }
                }
                x1 = W0;
            }
        }
        if (activ_func)
            activ_func(outptr, outptr, (int_)out_planesize, conv->activ_params);
    }
    return FX_OK;
}
#endif

////////////////////////// Quantized depthwise convolution ////////////////////////

int _fx_depthwise_qconv2d_u8(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_qconv2d_t* qconv,
                             int inp_typ, const uint8_t* inptr0,
                             float inp_scale0, int inp_zp0,
                             int out_typ, uint8_t* outptr0,
                             float out_scale0, int out_zp0,
                             int ntasks)
{
    int N = dw_ctx->N, C = qconv->C;
    int Hi = dw_ctx->Hi, Wi = dw_ctx->Wi;
    int H0 = dw_ctx->H0, W0 = dw_ctx->W0;
    int Hk = qconv->Hk, Wk = qconv->Wk;
    size_t inp_planesize = Hi*Wi, out_planesize = H0*W0;
    int stride_x = qconv->stride_x, stride_y = qconv->stride_y;
    int pad_top = qconv->pad_top, pad_left = qconv->pad_left;
    int ksize = Hk*Wk;
    int padded_ksize = qconv->padded_ksize;
    int inner_ytop = dw_ctx->inner_ytop;
    int inner_ybottom = dw_ctx->inner_ybottom;
    int inner_xleft = dw_ctx->inner_xleft;
    int inner_xright = dw_ctx->inner_xright;
    int inp_mask = inp_typ == FX_I8 ? 128 : 0;
    int out_mask = out_typ == FX_I8 ? 128 : 0;
    float inv_out_scale0 = out_scale0 == 0.f ? 0.f : 1.f/out_scale0;
    const int* yxtab = dw_ctx->yxtab;
    const int* ofstab = dw_ctx->ofstab;
#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_U8/2;
    bool useSIMD = (stride_x == 1 || stride_x == 2) & inner_xleft < W0;
    bool is3x3 = stride_x == 1 && Hk == 3 && Wk == 3;
    bool is3x3_r3 = is3x3 && qconv->stride_y == 1 &&
        qconv->dilation_y == 1 && qconv->dilation_x == 1;
#endif

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        int nc0 = task_id*(N*C)/ntasks, nc1 = (task_id+1)*(N*C)/ntasks;
        for (; nc0 < nc1; nc0++) {
            int c = nc0 % C, dy0 = 1;
            const uint8_t* inptr = (const uint8_t*)inptr0 + inp_planesize*nc0;
            uint8_t* outptr = (uint8_t*)outptr0 + out_planesize*nc0;
            float scale = (inp_scale0*inv_out_scale0)*qconv->w_scale[c];
            float biasval = (qconv->bias[c] - inp_zp0*qconv->w_sum[c])*scale + out_zp0;
            const int16_t* weights = qconv->depthwise_weights + c*padded_ksize;
    #ifdef __ARM_NEON
            uint8x8_t vinp_mask = vdup_n_u8(inp_mask), vout_mask = vdup_n_u8(out_mask);
            int16x4_t w03 = vdup_n_s16(0), w47 = w03, w8 = w03;
            float32x4_t vscale = vdupq_n_f32(scale), vbias = vdupq_n_f32(biasval);
            if (useSIMD && is3x3) {
                w03 = vld1_s16(weights);
                w47 = vld1_s16(weights+4);
                w8 = vdup_n_s16(weights[8]);
            }
    #endif
            for (int y0 = 0; y0 < H0; y0 += dy0, outptr += W0*dy0) {
            #ifdef __ARM_NEON
                dy0 = inner_ytop <= y0 && y0+3 < inner_ybottom && is3x3_r3 ? 3 : 1;
            #endif
                int x0 = 0, x1 = y0 >= inner_ytop && y0 < inner_ybottom ? inner_xleft : W0;
                int yi_ = y0*stride_y - pad_top;
                for(;;) {
                    int s_0, s_1, s_2;
                    if (dy0 == 3) {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            s_0 = s_1 = s_2 = 0;
                            for (int k = 0; k < ksize; k++) {
                                int yi = yi_ + yxtab[k*2];
                                int xi = xi_ + yxtab[k*2+1];
                                int w = weights[k];
                                if ((unsigned)xi < (unsigned)Wi) {
                                    s_0 += (inptr[yi*Wi + xi] ^ inp_mask)*w;
                                    s_1 += (inptr[(yi+1)*Wi + xi] ^ inp_mask)*w;
                                    s_2 += (inptr[(yi+2)*Wi + xi] ^ inp_mask)*w;
                                } else {
                                    s_0 += inp_mask*w;
                                    s_1 += inp_mask*w;
                                    s_2 += inp_mask*w;
                                }
                            }
                            s_0 = (int)lrintf(s_0*scale + biasval);
                            s_1 = (int)lrintf(s_1*scale + biasval);
                            s_2 = (int)lrintf(s_2*scale + biasval);
                            outptr[x0] = FX_SATURATE(s_0, out_mask);
                            outptr[x0 + W0] = FX_SATURATE(s_1, out_mask);
                            outptr[x0 + W0*2] = FX_SATURATE(s_2, out_mask);
                        }
                    } else {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            s_0 = 0;
                            for (int k = 0; k < ksize; k++) {
                                int yi = yi_ + yxtab[k*2];
                                int xi = xi_ + yxtab[k*2+1];
                                int w = weights[k];
                                if (((unsigned)yi < (unsigned)Hi) & ((unsigned)xi < (unsigned)Wi))
                                    s_0 += (inptr[yi*Wi + xi] ^ inp_mask)*w;
                                else
                                    s_0 += inp_mask*w;
                            }
                            s_0 = (int)lrintf(s_0*scale + biasval);
                            outptr[x0] = FX_SATURATE(s_0, out_mask);
                        }
                    }
                    if (x0 == W0)
                        break;
                    x1 = inner_xright;
                #ifdef __ARM_NEON
                    if (useSIMD) {
                        if (is3x3) {
                            if (dy0 == 3) {
                                for (; x0 < x1; x0 += vec_nlanes) {
                                    if (x0 + vec_nlanes > x1) {
                                        if (x0 <= inner_xleft)
                                            break;
                                        x0 = x1 - vec_nlanes;
                                    }
                                    int xi_ = x0*stride_x - pad_left;
                                    const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                                    uint8x8_t b0, b1, b2;
                                    int32x4_t s0l = vdupq_n_s32(0), s0h=s0l, s1l=s0l, s1h=s0l, s2l=s0l, s2h=s0l;

                                    #undef _FX_QDEPTHWISE_LOAD3
                                    #define _FX_QDEPTHWISE_LOAD3(row) \
                                        b0 = veor_u8(vld1_u8(inptr_xi+Wi*row+0), vinp_mask); \
                                        b1 = veor_u8(vld1_u8(inptr_xi+Wi*row+1), vinp_mask); \
                                        b2 = veor_u8(vld1_u8(inptr_xi+Wi*row+2), vinp_mask); \
                                        int16x8_t x##row##0 = vreinterpretq_s16_u16(vmovl_u8(b0)); \
                                        int16x8_t x##row##1 = vreinterpretq_s16_u16(vmovl_u8(b1)); \
                                        int16x8_t x##row##2 = vreinterpretq_s16_u16(vmovl_u8(b2))

                                    #define _FX_QDEPTHWISE_ACC3(row0, row1, row2, col, w, lane) \
                                        s0l = vmlal_lane_s16(s0l, vget_low_s16(x##row0##col), w, lane); \
                                        s0h = vmlal_high_lane_s16(s0h, x##row0##col, w, lane); \
                                        s1l = vmlal_lane_s16(s1l, vget_low_s16(x##row1##col), w, lane); \
                                        s1h = vmlal_high_lane_s16(s1h, x##row1##col, w, lane); \
                                        s2l = vmlal_lane_s16(s2l, vget_low_s16(x##row2##col), w, lane); \
                                        s2h = vmlal_high_lane_s16(s2h, x##row2##col, w, lane)

                                    _FX_QDEPTHWISE_LOAD3(0);
                                    _FX_QDEPTHWISE_LOAD3(1);
                                    _FX_QDEPTHWISE_LOAD3(2);
                                    _FX_QDEPTHWISE_ACC3(0, 1, 2, 0, w03, 0);
                                    _FX_QDEPTHWISE_ACC3(0, 1, 2, 1, w03, 1);
                                    _FX_QDEPTHWISE_ACC3(0, 1, 2, 2, w03, 2);

                                    _FX_QDEPTHWISE_LOAD3(3);
                                    _FX_QDEPTHWISE_ACC3(1, 2, 3, 0, w03, 3);
                                    _FX_QDEPTHWISE_ACC3(1, 2, 3, 1, w47, 0);
                                    _FX_QDEPTHWISE_ACC3(1, 2, 3, 2, w47, 1);

                                    _FX_QDEPTHWISE_LOAD3(4);
                                    _FX_QDEPTHWISE_ACC3(2, 3, 4, 0, w47, 2);
                                    _FX_QDEPTHWISE_ACC3(2, 3, 4, 1, w47, 3);
                                    _FX_QDEPTHWISE_ACC3(2, 3, 4, 2, w8, 0);

                                    s0l = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0l), vscale));
                                    s0h = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0h), vscale));
                                    s1l = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s1l), vscale));
                                    s1h = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s1h), vscale));
                                    s2l = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s2l), vscale));
                                    s2h = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s2h), vscale));

                                    uint16x8_t t0 = vcombine_u16(vqmovun_s32(s0l), vqmovun_s32(s0h));
                                    uint16x8_t t1 = vcombine_u16(vqmovun_s32(s1l), vqmovun_s32(s1h));
                                    uint16x8_t t2 = vcombine_u16(vqmovun_s32(s2l), vqmovun_s32(s2h));

                                    b0 = veor_u8(vqmovn_u16(t0), vout_mask);
                                    b1 = veor_u8(vqmovn_u16(t1), vout_mask);
                                    b2 = veor_u8(vqmovn_u16(t2), vout_mask);

                                    vst1_u8(outptr + x0, b0);
                                    vst1_u8(outptr + W0 + x0, b1);
                                    vst1_u8(outptr + W0*2 + x0, b2);
                                }
                            } else {
                                for (; x0 < x1; x0 += vec_nlanes) {
                                    if (x0 + vec_nlanes > x1) {
                                        if (x0 <= inner_xleft)
                                            break;
                                        x0 = x1 - vec_nlanes;
                                    }
                                    int xi_ = x0*stride_x - pad_left;
                                    const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                                    uint8x8_t b0, b1, b2;
                                    int32x4_t s0l = vdupq_n_s32(0), s0h=s0l;

                                    #undef _FX_QDEPTHWISE_LOAD3
                                    #define _FX_QDEPTHWISE_LOAD3(row) \
                                        b0 = veor_u8(vld1_u8(inptr_xi+ofstab[row*3+0]), vinp_mask); \
                                        b1 = veor_u8(vld1_u8(inptr_xi+ofstab[row*3+1]), vinp_mask); \
                                        b2 = veor_u8(vld1_u8(inptr_xi+ofstab[row*3+2]), vinp_mask); \
                                        int16x8_t x##row##0 = vreinterpretq_s16_u16(vmovl_u8(b0)); \
                                        int16x8_t x##row##1 = vreinterpretq_s16_u16(vmovl_u8(b1)); \
                                        int16x8_t x##row##2 = vreinterpretq_s16_u16(vmovl_u8(b2))

                                    _FX_QDEPTHWISE_LOAD3(0);
                                    _FX_QDEPTHWISE_LOAD3(1);
                                    _FX_QDEPTHWISE_LOAD3(2);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x00), w03, 0);
                                    s0h = vmlal_high_lane_s16(s0h, x00, w03, 0);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x01), w03, 1);
                                    s0h = vmlal_high_lane_s16(s0h, x01, w03, 1);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x02), w03, 2);
                                    s0h = vmlal_high_lane_s16(s0h, x02, w03, 2);

                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x10), w03, 3);
                                    s0h = vmlal_high_lane_s16(s0h, x10, w03, 3);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x11), w47, 0);
                                    s0h = vmlal_high_lane_s16(s0h, x11, w47, 0);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x12), w47, 1);
                                    s0h = vmlal_high_lane_s16(s0h, x12, w47, 1);

                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x20), w47, 2);
                                    s0h = vmlal_high_lane_s16(s0h, x20, w47, 2);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x21), w47, 3);
                                    s0h = vmlal_high_lane_s16(s0h, x21, w47, 3);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(x22), w8, 0);
                                    s0h = vmlal_high_lane_s16(s0h, x22, w8, 0);

                                    s0l = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0l), vscale));
                                    s0h = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0h), vscale));
                                    uint16x8_t t0 = vcombine_u16(vqmovun_s32(s0l), vqmovun_s32(s0h));

                                    b0 = veor_u8(vqmovn_u16(t0), vout_mask);
                                    vst1_u8(outptr + x0, b0);
                                }
                            }
                        } else if (stride_x == 1) {
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 <= inner_xleft)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                }
                                int xi_ = x0*stride_x - pad_left;
                                const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                                int32x4_t s0l = vdupq_n_s32(0), s0h = s0l;
                                int k = 0;

                                for (; k <= ksize - 4; k += 4) {
                                    uint8x8_t b0 = veor_u8(vld1_u8(inptr_xi + ofstab[k]), vinp_mask);
                                    uint8x8_t b1 = veor_u8(vld1_u8(inptr_xi + ofstab[k+1]), vinp_mask);
                                    uint8x8_t b2 = veor_u8(vld1_u8(inptr_xi + ofstab[k+2]), vinp_mask);
                                    uint8x8_t b3 = veor_u8(vld1_u8(inptr_xi + ofstab[k+3]), vinp_mask);
                                    int16x4_t w = vld1_s16(weights + k);

                                    int16x8_t t0 = vreinterpretq_s16_u16(vmovl_u8(b0));
                                    int16x8_t t1 = vreinterpretq_s16_u16(vmovl_u8(b1));
                                    int16x8_t t2 = vreinterpretq_s16_u16(vmovl_u8(b2));
                                    int16x8_t t3 = vreinterpretq_s16_u16(vmovl_u8(b3));

                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(t0), w, 0);
                                    s0h = vmlal_high_lane_s16(s0h, t0, w, 0);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(t1), w, 1);
                                    s0h = vmlal_high_lane_s16(s0h, t1, w, 1);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(t2), w, 2);
                                    s0h = vmlal_high_lane_s16(s0h, t2, w, 2);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(t3), w, 3);
                                    s0h = vmlal_high_lane_s16(s0h, t3, w, 3);
                                }

                                for (; k < ksize; k++) {
                                    uint8x8_t b0 = veor_u8(vld1_u8(inptr_xi + ofstab[k]), vinp_mask);
                                    int16x8_t t0 = vreinterpretq_s16_u16(vmovl_u8(b0));
                                    int16x4_t w = vdup_n_s16(weights[k]);
                                    s0l = vmlal_s16(s0l, vget_low_s16(t0), w);
                                    s0h = vmlal_s16(s0h, vget_high_s16(t0), w);
                                }

                                {
                                s0l = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0l), vscale));
                                s0h = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0h), vscale));
                                uint16x8_t t0 = vcombine_u16(vqmovun_s32(s0l), vqmovun_s32(s0h));
                                vst1_u8(outptr + x0, veor_u8(vqmovn_u16(t0), vout_mask));
                                }
                            }
                        } else if (yi_ + (Hk-1)*qconv->dilation_y < Hi-1) {
                            assert(stride_x == 2);
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 <= inner_xleft)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                }
                                int xi_ = x0*stride_x - pad_left;
                                const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                                int32x4_t s0l = vdupq_n_s32(0), s0h = s0l;
                                int k = 0;

                                for (; k <= ksize - 4; k += 4) {
                                    uint8x8_t b0 = veor_u8(vld2_u8(inptr_xi + ofstab[k]).val[0], vinp_mask);
                                    uint8x8_t b1 = veor_u8(vld2_u8(inptr_xi + ofstab[k+1]).val[0], vinp_mask);
                                    uint8x8_t b2 = veor_u8(vld2_u8(inptr_xi + ofstab[k+2]).val[0], vinp_mask);
                                    uint8x8_t b3 = veor_u8(vld2_u8(inptr_xi + ofstab[k+3]).val[0], vinp_mask);
                                    int16x4_t w = vld1_s16(weights + k);

                                    int16x8_t t0 = vreinterpretq_s16_u16(vmovl_u8(b0));
                                    int16x8_t t1 = vreinterpretq_s16_u16(vmovl_u8(b1));
                                    int16x8_t t2 = vreinterpretq_s16_u16(vmovl_u8(b2));
                                    int16x8_t t3 = vreinterpretq_s16_u16(vmovl_u8(b3));

                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(t0), w, 0);
                                    s0h = vmlal_high_lane_s16(s0h, t0, w, 0);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(t1), w, 1);
                                    s0h = vmlal_high_lane_s16(s0h, t1, w, 1);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(t2), w, 2);
                                    s0h = vmlal_high_lane_s16(s0h, t2, w, 2);
                                    s0l = vmlal_lane_s16(s0l, vget_low_s16(t3), w, 3);
                                    s0h = vmlal_high_lane_s16(s0h, t3, w, 3);
                                }

                                for (; k < ksize; k++) {
                                    uint8x8_t b0 = veor_u8(vld2_u8(inptr_xi + ofstab[k]).val[0], vinp_mask);
                                    int16x8_t t0 = vreinterpretq_s16_u16(vmovl_u8(b0));
                                    int16x4_t w = vdup_n_s16(weights[k]);
                                    s0l = vmlal_s16(s0l, vget_low_s16(t0), w);
                                    s0h = vmlal_s16(s0h, vget_high_s16(t0), w);
                                }

                                {
                                s0l = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0l), vscale));
                                s0h = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0h), vscale));
                                uint16x8_t t0 = vcombine_u16(vqmovun_s32(s0l), vqmovun_s32(s0h));
                                vst1_u8(outptr + x0, veor_u8(vqmovn_u16(t0), vout_mask));
                                }
                            }
                        }
                    }
                #endif
                    if (dy0 == 3) {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                            s_0 = s_1 = s_2 = 0;
                            for (int k = 0; k < ksize; k++) {
                                int inp_ofs = ofstab[k];
                                int w = weights[k];
                                s_0 += (inptr_xi[inp_ofs] ^ inp_mask)*w;
                                s_1 += (inptr_xi[inp_ofs + Wi] ^ inp_mask)*w;
                                s_2 += (inptr_xi[inp_ofs + Wi*2] ^ inp_mask)*w;
                            }
                            s_0 = (int)lrintf(s_0*scale + biasval);
                            s_1 = (int)lrintf(s_1*scale + biasval);
                            s_2 = (int)lrintf(s_2*scale + biasval);
                            outptr[x0] = FX_SATURATE(s_0, out_mask);
                            outptr[x0 + W0] = FX_SATURATE(s_1, out_mask);
                            outptr[x0 + W0*2] = FX_SATURATE(s_2, out_mask);
                        }
                    } else {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            const uint8_t* inptr_xi = inptr + Wi*yi_ + xi_;
                            s_0 = 0;
                            for (int k = 0; k < ksize; k++) {
                                s_0 += (inptr_xi[ofstab[k]] ^ inp_mask)*weights[k];
                            }
                            s_0 = (int)lrintf(s_0*scale + biasval);
                            outptr[x0] = FX_SATURATE(s_0, out_mask);
                        }
                    }
                    x1 = W0;
                }
            }
        }
    }
    return FX_OK;
}

}
