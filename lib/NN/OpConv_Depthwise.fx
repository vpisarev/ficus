/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// depthwise fp32 & fp16 convolution.
@ccode {
#include "ficus_nn_common.h"

#if defined __ARM_NEON || defined __AVX2__
#define _FX_DEPTHWISE_USE_SIMD 1
#else
#define _FX_DEPTHWISE_USE_SIMD 0
#endif

int _fx_depthwise_conv2d_f32(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_conv2d_t* conv,
                             const char* inptr0, char* outptr0,
                             int ntasks)
{
    int Hi = dw_ctx->Hi, Wi = dw_ctx->Wi, H0 = dw_ctx->H0, W0 = dw_ctx->W0;
    int Hk = conv->Hk, Wk = conv->Wk;
    int stride_x = conv->stride_x, stride_y = conv->stride_y;
    int dilation_x = conv->dilation_x, dilation_y = conv->dilation_y;
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

#if _FX_DEPTHWISE_USE_SIMD
    const int vec_nlanes = FX_VEC_NLANES_F32;
    bool useSIMD = (stride_x == 1 || stride_x == 2) && inner_xleft < W0;
    int Wi_simd = Wi - (vec_nlanes - 1)*stride_x - (Wk - 1)*dilation_x;
    Wi_simd = Wi_simd < 0 ? 0 : Wi_simd;
    bool is3x3 = stride_x == 1 && Hk == 3 && Wk == 3;
    bool is3x3_r3 = is3x3 && stride_y == 1 &&
        dilation_y == 1 && dilation_x == 1;
    uint32_t xofsbuf[FX_VEC_NLANES_F32];
    uint32_t* xofstab = (uint32_t*)alloca(vec_nlanes*ksize*sizeof(xofstab[0]));
    for (int j = 0; j < vec_nlanes; j++)
        xofsbuf[j] = j*stride_x;
    for (int k = 0; k < ksize; k++) {
        int xi = yxtab[k*2+1];
        for (int j = 0; j < FX_VEC_NLANES_F32; j++)
            xofstab[k*vec_nlanes + j] = xi + xofsbuf[j];
    }
#ifdef __ARM_NEON
    float32x4_t valpha = vdupq_n_f32(alpha), vmaxval = vdupq_n_f32(maxval);
    float32x4_t z = vdupq_n_f32(0.f), one = vdupq_n_f32(1.f);
    uint32x4_t v_Wi = vdupq_n_u32((uint32_t)Wi);
    uint32x4_t vxofs0 = vld1q_u32(xofsbuf);
#elif defined __AVX2__
    __m256 valpha = _mm256_set1_ps(alpha);
    __m256 vmaxval = _mm256_set1_ps(maxval);
    __m256 z = _mm256_setzero_ps(), one = _mm256_set1_ps(1.f);
    __m256i v_Wi = _mm256_set1_epi32(Wi);
    __m256i vxofs0 = _mm256_loadu_si256((const __m256i*)xofsbuf);
#endif
#else
    const int vec_nlanes = 1;
#endif

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        int nc0 = task_id*NC/ntasks, nc1 = (task_id+1)*NC/ntasks;
        for (int nc = nc0; nc < nc1; nc++) {
            int c = nc % conv->C, dy0 = 1;
            const float* inptr = (const float*)inptr0 + inp_planesize*nc;
            float* outptr = (float*)outptr0 + out_planesize*nc;
            float biasval = conv->bias[c];
            const float* weights = conv->weights + c*padded_ksize;
            int safe_y0 = nc > nc0 ? 0 : 1;
            int safe_y1 = nc < nc1-1 ? H0 : H0-1;
        #if _FX_DEPTHWISE_USE_SIMD
            #ifdef __ARM_NEON
                float32x4_t w0=vdupq_n_f32(0.f), w1=w0, w2=w0, w3=w0, w4=w0, w5=w0, w6=w0, w7=w0, w8=w0;
                float32x4_t vbias = vdupq_n_f32(biasval);
                if (useSIMD && is3x3) {
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
            #elif defined __AVX2__
                const int even_mask[] = {0, 2, 4, 6, 1, 3, 5, 7};
                __m256 vbias = _mm256_set1_ps(biasval);
                __m256i even = _mm256_loadu_si256((const __m256i*)even_mask);
                #define _mm256_loadeven_ps(ptr) \
                    _mm256_permute2f128_ps( \
                        _mm256_permutevar8x32_ps(_mm256_loadu_ps(ptr), even), \
                        _mm256_permutevar8x32_ps(_mm256_loadu_ps((ptr) + 8), even), \
                        0 + 2*16)
            #endif
        #endif
            for (int y0 = 0; y0 < H0; y0 += dy0, outptr += W0*dy0) {
            #if _FX_DEPTHWISE_USE_SIMD && (defined __ARM_NEON)
                dy0 = inner_ytop <= y0 && y0+3 < inner_ybottom && is3x3_r3 ? 3 : 1;
            #endif
                int k0 = 0, k1 = ksize;
                bool inner_y = y0 >= inner_ytop && y0 < inner_ybottom;
                int x0 = 0, x1 = inner_xleft, x0_start;
                int yi_ = y0*stride_y - pad_top;
                if (!inner_y) {
                    for (; k0 < ksize; k0++)
                        if ((unsigned)(yi_ + yxtab[k0*2]) < (unsigned)Hi)
                            break;
                    for (; k1 > 0; k1--)
                        if ((unsigned)(yi_ + yxtab[k1*2-2]) < (unsigned)Hi)
                            break;
                }
            #if _FX_DEPTHWISE_USE_SIMD
                if (dy0 == 1 && !is3x3 && safe_y0 <= y0 && y0 < safe_y1 && W0 > vec_nlanes/2)
                    x1 = 0;
            #endif
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
                            for (int k = k0; k < k1; k++) {
                                int yi = yi_ + yxtab[k*2];
                                int xi = xi_ + yxtab[k*2+1];
                                float w = weights[k];
                                if ((unsigned)xi < (unsigned)Wi)
                                    s_0 += inptr[yi*Wi + xi]*w;
                            }
                            s_0 = s_0 <= maxval ? s_0 : maxval;
                            s_0 *= (s_0 < 0.f ? alpha : 1.f);
                            outptr[x0] = s_0;
                        }
                    }
                    if (x0 >= W0)
                        break;
                    x0_start = x0;
                    x1 = inner_xright;
                #if _FX_DEPTHWISE_USE_SIMD && (defined __ARM_NEON)
                    if (useSIMD) {
                        if (is3x3 && inner_y) {
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
                            if (y0 < safe_y1 && x0 + vec_nlanes/2 <= W0)
                                x1 = W0;
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 == x0_start && x0_start > 0)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                    if (x0 < 0) x0 = 0;
                                }
                                int xi_ = x0*stride_x - pad_left, k = k0;
                                const float* inptr_xi = inptr + Wi*yi_ + xi_;
                                float32x4_t s0 = vbias;
                                // (x0 + vec_nlanes - 1)*stride_x - pad_left + (Wk - 1)*dilation_x < Wi ~
                                // xi_ < Wi - (vec_nlanes - 1)*stride_x - (Wk - 1)*dilation_x = Wi_simd
                                if ((unsigned)xi_ < (unsigned)Wi_simd) {
                                    for (; k <= k1 - 4; k += 4) {
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
                                    for (; k < k1; k++)
                                        s0 = vfmaq_f32(s0, vld1q_f32(inptr_xi + ofstab[k]), vdupq_n_f32(weights[k]));
                                } else {
                                    uint32x4_t vxofs = vdupq_n_u32((uint32_t)xi_);
                                    for (; k <= k1 - 4; k += 4) {
                                    #define _FX_LOAD_APPLY_MASK_F32(i) \
                                        uint32x4_t m##i = vaddq_u32(vld1q_u32(xofstab + (k+i)*vec_nlanes), vxofs); \
                                        m##i = vcltq_u32(m##i, v_Wi); \
                                        m##i = vandq_u32(m##i, vld1q_u32( \
                                            (const uint32_t*)inptr_xi + ofstab[k+i])); \
                                        float32x4_t v##i = vreinterpretq_f32_u32(m##i)

                                        _FX_LOAD_APPLY_MASK_F32(0);
                                        _FX_LOAD_APPLY_MASK_F32(1);
                                        _FX_LOAD_APPLY_MASK_F32(2);
                                        _FX_LOAD_APPLY_MASK_F32(3);
                                        float32x4_t ww = vld1q_f32(weights + k);
                                        s0 = vfmaq_laneq_f32(s0, v0, ww, 0);
                                        s0 = vfmaq_laneq_f32(s0, v1, ww, 1);
                                        s0 = vfmaq_laneq_f32(s0, v2, ww, 2);
                                        s0 = vfmaq_laneq_f32(s0, v3, ww, 3);
                                    }
                                    for (; k < k1; k++) {
                                        _FX_LOAD_APPLY_MASK_F32(0);
                                        float32x4_t ww = vdupq_n_f32(weights[k]);
                                        s0 = vfmaq_f32(s0, v0, ww);
                                    }
                                }
                                s0 = vmulq_f32(vminq_f32(s0, vmaxval),
                                            vbslq_f32(vcltq_f32(s0, z), valpha, one));
                                vst1q_f32(outptr + x0, s0);
                            }
                        } else if(yi_ + (Hk-1)*dilation_y < Hi-1 || x1 < W0) {
                            assert(stride_x == 2);
                            if (y0 < safe_y1 && x0 + vec_nlanes/2 <= W0)
                                x1 = W0;
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 == x0_start && x0_start > 0)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                    if (x0 < 0) x0 = 0;
                                }
                                int xi_ = x0*stride_x - pad_left, k = k0;
                                const float* inptr_xi = inptr + Wi*yi_ + xi_;
                                float32x4_t s0 = vbias;
                                if ((unsigned)xi_ < (unsigned)Wi_simd) {
                                    for (; k <= k1 - 4; k += 4) {
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
                                    for (; k < k1; k++)
                                        s0 = vfmaq_f32(s0, vld2q_f32(inptr_xi + ofstab[k]).val[0], vdupq_n_f32(weights[k]));
                                } else {
                                    uint32x4_t vxofs = vdupq_n_u32((uint32_t)xi_);
                                    for (; k <= k1 - 4; k += 4) {
                                    #undef _FX_LOAD_APPLY_MASK_F32
                                    #define _FX_LOAD_APPLY_MASK_F32(i) \
                                        uint32x4_t m##i = vaddq_u32(vld1q_u32(xofstab + (k+i)*vec_nlanes), vxofs); \
                                        m##i = vcltq_u32(m##i, v_Wi); \
                                        m##i = vandq_u32(m##i, vld2q_u32( \
                                            (const uint32_t*)inptr_xi + ofstab[k+i]).val[0]); \
                                        float32x4_t v##i = vreinterpretq_f32_u32(m##i)

                                        _FX_LOAD_APPLY_MASK_F32(0);
                                        _FX_LOAD_APPLY_MASK_F32(1);
                                        _FX_LOAD_APPLY_MASK_F32(2);
                                        _FX_LOAD_APPLY_MASK_F32(3);
                                        float32x4_t ww = vld1q_f32(weights + k);
                                        s0 = vfmaq_laneq_f32(s0, v0, ww, 0);
                                        s0 = vfmaq_laneq_f32(s0, v1, ww, 1);
                                        s0 = vfmaq_laneq_f32(s0, v2, ww, 2);
                                        s0 = vfmaq_laneq_f32(s0, v3, ww, 3);
                                    }
                                    for (; k < k1; k++) {
                                        _FX_LOAD_APPLY_MASK_F32(0);
                                        float32x4_t ww = vdupq_n_f32(weights[k]);
                                        s0 = vfmaq_f32(s0, v0, ww);
                                    }
                                }
                                s0 = vmulq_f32(vminq_f32(s0, vmaxval),
                                            vbslq_f32(vcltq_f32(s0, z), valpha, one));
                                vst1q_f32(outptr + x0, s0);
                            }
                        }
                    }
                #elif _FX_DEPTHWISE_USE_SIMD && (defined __AVX2__)
                    if (useSIMD) {
                        if (stride_x == 1) {
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 <= inner_xleft)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                }
                                int xi_ = x0*stride_x - pad_left, k = 0;
                                const float* inptr_xi = inptr + Wi*yi_ + xi_;
                                __m256 s0 = vbias;
                                for (; k <= ksize - 4; k += 4) {
                                    __m256 v0 = _mm256_loadu_ps(inptr_xi + ofstab[k]);
                                    __m256 v1 = _mm256_loadu_ps(inptr_xi + ofstab[k+1]);
                                    __m256 v2 = _mm256_loadu_ps(inptr_xi + ofstab[k+2]);
                                    __m256 v3 = _mm256_loadu_ps(inptr_xi + ofstab[k+3]);
                                    __m256 w = _mm256_broadcast_ss(weights + k);
                                    s0 = _mm256_fmadd_ps(v0, w, s0);
                                    w = _mm256_broadcast_ss(weights + k + 1);
                                    s0 = _mm256_fmadd_ps(v1, w, s0);
                                    w = _mm256_broadcast_ss(weights + k + 2);
                                    s0 = _mm256_fmadd_ps(v2, w, s0);
                                    w = _mm256_broadcast_ss(weights + k + 3);
                                    s0 = _mm256_fmadd_ps(v3, w, s0);
                                }
                                for (; k < ksize; k++) {
                                    __m256 v0 = _mm256_loadu_ps(inptr_xi + ofstab[k]);
                                    __m256 w = _mm256_broadcast_ss(weights + k);
                                    s0 = _mm256_fmadd_ps(v0, w, s0);
                                }
                                s0 = _mm256_mul_ps(_mm256_min_ps(s0, vmaxval),
                                        _mm256_blendv_ps(one, valpha,
                                            _mm256_cmp_ps(s0, z, _CMP_LT_OQ)));
                                _mm256_storeu_ps(outptr + x0, s0);
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
                                __m256 s0 = vbias;
                                for (; k <= ksize - 4; k += 4) {
                                    __m256 v0 = _mm256_loadeven_ps(inptr_xi + ofstab[k]);
                                    __m256 v1 = _mm256_loadeven_ps(inptr_xi + ofstab[k+1]);
                                    __m256 v2 = _mm256_loadeven_ps(inptr_xi + ofstab[k+2]);
                                    __m256 v3 = _mm256_loadeven_ps(inptr_xi + ofstab[k+3]);
                                    __m256 w = _mm256_broadcast_ss(weights + k);
                                    s0 = _mm256_fmadd_ps(v0, w, s0);
                                    w = _mm256_broadcast_ss(weights + k + 1);
                                    s0 = _mm256_fmadd_ps(v1, w, s0);
                                    w = _mm256_broadcast_ss(weights + k + 2);
                                    s0 = _mm256_fmadd_ps(v2, w, s0);
                                    w = _mm256_broadcast_ss(weights + k + 3);
                                    s0 = _mm256_fmadd_ps(v3, w, s0);
                                }
                                for (; k < ksize; k++) {
                                    __m256 v0 = _mm256_loadeven_ps(inptr_xi + ofstab[k]);
                                    __m256 w = _mm256_broadcast_ss(weights + k);
                                    s0 = _mm256_fmadd_ps(v0, w, s0);
                                }
                                s0 = _mm256_mul_ps(_mm256_min_ps(s0, vmaxval),
                                        _mm256_blendv_ps(one, valpha,
                                        _mm256_cmp_ps(s0, z, _CMP_LT_OQ)));
                                _mm256_storeu_ps(outptr + x0, s0);
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
                            for (int k = k0; k < k1; k++) {
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
        }
        if (activ_func) {
            float* outptr_nc = (float*)outptr0 + out_planesize*nc0;
            activ_func(outptr_nc, outptr_nc,
                (int_)out_planesize*(nc1 - nc0),
                conv->activ_params);
        }
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
    int dilation_x = conv->dilation_x, dilation_y = conv->dilation_y;
    int pad_top = conv->pad_top, pad_left = conv->pad_left;
    int NC = (int)(dw_ctx->N*conv->C);
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
    //const int nc0 = -1;
#ifdef __ARM_NEON
    const int vec_nlanes = FX_VEC_NLANES_F16;
    int Wi_simd = Wi - (vec_nlanes - 1)*stride_x - (Wk - 1)*dilation_x;
    Wi_simd = Wi_simd < 0 ? 0 : Wi_simd;
    uint16x8_t v_Wi = vdupq_n_u16((uint16_t)Wi);
    float16x8_t valpha = vdupq_n_f16(alpha), vmaxval = vdupq_n_f16(maxval);
    float16x8_t z = vdupq_n_f16(0.f), one = vdupq_n_f16(1.f);
    bool useSIMD = (stride_x == 1 || stride_x == 2) && inner_xleft < W0;
    bool is3x3 = stride_x == 1 && Hk == 3 && Wk == 3;
    bool is3x3_r3 = is3x3 && stride_y == 1 &&
        dilation_y == 1 && dilation_x == 1;
    uint16_t xofsbuf[FX_VEC_NLANES_F16];
    for (int j = 0; j < vec_nlanes; j++)
        xofsbuf[j] = j*stride_x;
    uint16x8_t vxofs0 = vld1q_u16(xofsbuf);
    uint16_t* xofstab = (uint16_t*)alloca(vec_nlanes*ksize*sizeof(xofstab[0]));
    for (int k = 0; k < ksize; k++) {
        int xi = yxtab[k*2+1];
        uint16x8_t vxofs = vaddq_u16(vdupq_n_u16((uint16_t)xi), vxofs0);
        vst1q_u16(xofstab + k*vec_nlanes, vxofs);
    }
#endif

    //printf("depthwise: NC=%d, H0=%d, W0=%d, Hk=%d, Wk=%d, stride_y=%d, stride_x=%d, dilation_y=%d, dilation_x=%d, inner_xleft=%d, inner_xright=%d, inner_ytop=%d, inner_ybottom=%d\n",
    //    (int)NC, (int)H0, (int)W0, (int)Hk, (int)Wk, stride_y, stride_x, conv->dilation_y, conv->dilation_x, (int)inner_xleft, (int)inner_xright, (int)inner_ytop, (int)inner_ybottom);

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        int nc0 = task_id*NC/ntasks, nc1 = (task_id+1)*NC/ntasks;
        for (int nc = nc0; nc < nc1; nc++) {
            int c = nc % conv->C, dy0 = 1;
            const fx_f16* inptr = (const fx_f16*)inptr0 + inp_planesize*nc;
            fx_f16* outptr = (fx_f16*)outptr0 + out_planesize*nc;
            float biasval = conv->bias[c];
            const float* w_f32 = conv->weights + c*padded_ksize_f32;
            const fx_f16* weights = conv->wf16 + c*padded_ksize;
            int safe_y0 = nc > nc0 ? 0 : 1;
            int safe_y1 = nc < nc1-1 ? H0 : H0-1;

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
                int k0 = 0, k1 = ksize;
                bool inner_y = y0 >= inner_ytop && y0 < inner_ybottom;
                int x0 = 0, x1 = inner_xleft, x0_start;
                int yi_ = y0*stride_y - pad_top;
                if (!inner_y) {
                    for (; k0 < ksize; k0++)
                        if ((unsigned)(yi_ + yxtab[k0*2]) < (unsigned)Hi)
                            break;
                    for (; k1 > 0; k1--)
                        if ((unsigned)(yi_ + yxtab[k1*2-2]) < (unsigned)Hi)
                            break;
                }
                if (dy0 == 1 && !is3x3 && safe_y0 <= y0 && y0 < safe_y1 && W0 > vec_nlanes/2)
                    x1 = 0;
                for(;;) {
                    float s_0, s_1, s_2;
                    if (dy0 == 3) {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            s_0 = s_1 = s_2 = biasval;
                            // this branch is only executed when yi is within 0..Hi-1,
                            // therefore k0 == 0 and k1 == ksize
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
                            for (int k = k0; k < k1; k++) {
                                int yi = yi_ + yxtab[k*2];
                                int xi = xi_ + yxtab[k*2+1];
                                float w = w_f32[k];
                                if ((unsigned)xi < (unsigned)Wi)
                                    s_0 += FX_FLOAT(inptr[yi*Wi + xi])*w;
                            }
                            s_0 = s_0 <= maxval ? s_0 : maxval;
                            s_0 *= (s_0 < 0.f ? alpha : 1.f);
                            outptr[x0] = FX_FLOAT16(s_0);
                        }
                    }
                    if (x0 >= W0)
                        break;
                    x0_start = x0;
                    x1 = inner_xright;
                #ifdef __ARM_NEON
                    if (useSIMD) {
                        if (is3x3 && inner_y) {
                            if (dy0 == 3) {
                                for (; x0 < x1; x0 += vec_nlanes) {
                                    if (x0 + vec_nlanes > x1) {
                                        if (x0 <= x0_start)
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
                                        if (x0 <= x0_start)
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
                            if (y0 < safe_y1 && x0 + vec_nlanes/2 <= W0)
                                x1 = W0;
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 == x0_start && x0_start > 0)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                    if (x0 < 0) x0 = 0;
                                }
                                int xi_ = x0*stride_x - pad_left, k = k0;
                                const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                                float16x8_t s0 = vbias;
                                // (x0 + vec_nlanes - 1)*stride_x - pad_left + (Wk - 1)*dilation_x < Wi ~
                                // xi_ < Wi - (vec_nlanes - 1)*stride_x - (Wk - 1)*dilation_x = Wi_simd
                                if ((unsigned)xi_ < (unsigned)Wi_simd) {
                                    for (; k <= k1 - 4; k += 4) {
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
                                    for (; k < k1; k++)
                                        s0 = vfmaq_f16(s0, vld1q_f16(inptr_xi + ofstab[k]), vdupq_n_f16(weights[k]));
                                } else {
                                    uint16x8_t vxofs = vdupq_n_u16((uint16_t)xi_);
                                    for (; k <= k1 - 4; k += 4) {
                                    #define _FX_LOAD_APPLY_MASK_F16(i) \
                                        uint16x8_t m##i = vaddq_u16(vld1q_u16(xofstab + (k+i)*vec_nlanes), vxofs); \
                                        m##i = vcltq_u16(m##i, v_Wi); \
                                        m##i = vandq_u16(m##i, vld1q_u16( \
                                            (const uint16_t*)inptr_xi + ofstab[k+i])); \
                                        float16x8_t v##i = vreinterpretq_f16_u16(m##i)

                                        _FX_LOAD_APPLY_MASK_F16(0);
                                        _FX_LOAD_APPLY_MASK_F16(1);
                                        _FX_LOAD_APPLY_MASK_F16(2);
                                        _FX_LOAD_APPLY_MASK_F16(3);
                                        float16x4_t ww = vld1_f16(weights + k);
                                        s0 = vfmaq_lane_f16(s0, v0, ww, 0);
                                        s0 = vfmaq_lane_f16(s0, v1, ww, 1);
                                        s0 = vfmaq_lane_f16(s0, v2, ww, 2);
                                        s0 = vfmaq_lane_f16(s0, v3, ww, 3);
                                    }
                                    for (; k < k1; k++) {
                                        _FX_LOAD_APPLY_MASK_F16(0);
                                        float16x8_t ww = vdupq_n_f16(weights[k]);
                                        s0 = vfmaq_f16(s0, v0, ww);
                                    }
                                }
                                s0 = vmulq_f16(vminq_f16(s0, vmaxval), vbslq_f16(vcltq_f16(s0, z), valpha, one));
                                vst1q_f16(outptr + x0, s0);
                            }
                        } else if (yi_ + (Hk-1)*dilation_y < Hi-1 || x1 < W0) {
                            assert(stride_x == 2);
                            if (y0 < safe_y1 && x0 + vec_nlanes/2 <= W0)
                                x1 = W0;
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 == x0_start && x0_start > 0)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                    if (x0 < 0) x0 = 0;
                                }
                                int xi_ = x0*stride_x - pad_left, k = k0;
                                const fx_f16* inptr_xi = inptr + Wi*yi_ + xi_;
                                float16x8_t s0 = vbias;
                                if ((unsigned)xi_ < (unsigned)Wi_simd) {
                                    for (; k <= k1 - 4; k += 4) {
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
                                    for (; k < k1; k++)
                                        s0 = vfmaq_f16(s0, vld2q_f16(inptr_xi + ofstab[k]).val[0],
                                                    vdupq_n_f16(weights[k]));
                                } else {
                                    uint16x8_t vxofs = vdupq_n_u16((uint16_t)xi_);
                                    for (; k <= k1 - 4; k += 4) {
                                    #undef _FX_LOAD_APPLY_MASK_F16
                                    #define _FX_LOAD_APPLY_MASK_F16(i) \
                                        uint16x8_t m##i = vaddq_u16(vld1q_u16(xofstab + (k+i)*vec_nlanes), vxofs); \
                                        m##i = vcltq_u16(m##i, v_Wi); \
                                        m##i = vandq_u16(m##i, vld2q_u16( \
                                            (const uint16_t*)inptr_xi + ofstab[k+i]).val[0]); \
                                        float16x8_t v##i = vreinterpretq_f16_u16(m##i)

                                        _FX_LOAD_APPLY_MASK_F16(0);
                                        _FX_LOAD_APPLY_MASK_F16(1);
                                        _FX_LOAD_APPLY_MASK_F16(2);
                                        _FX_LOAD_APPLY_MASK_F16(3);
                                        float16x4_t ww = vld1_f16(weights + k);
                                        s0 = vfmaq_lane_f16(s0, v0, ww, 0);
                                        s0 = vfmaq_lane_f16(s0, v1, ww, 1);
                                        s0 = vfmaq_lane_f16(s0, v2, ww, 2);
                                        s0 = vfmaq_lane_f16(s0, v3, ww, 3);
                                    }
                                    for (; k < k1; k++) {
                                        _FX_LOAD_APPLY_MASK_F16(0);
                                        float16x8_t ww = vdupq_n_f16(weights[k]);
                                        s0 = vfmaq_f16(s0, v0, ww);
                                    }
                                }
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
                            for (int k = k0; k < k1; k++) {
                                s_0 += FX_FLOAT(inptr_xi[ofstab[k]])*w_f32[k];
                            }
                            s_0 = s_0 <= maxval ? s_0 : maxval;
                            s_0 *= (s_0 < 0.f ? alpha : 1.f);
                            outptr[x0] = FX_FLOAT16(s_0);
                        }
                    }
                    x1 = W0;
                }
            }
        }
        if (activ_func) {
            fx_f16* outptr_nc = (fx_f16*)outptr0 + out_planesize*nc0;
            activ_func(outptr_nc, outptr_nc,
                (int_)out_planesize*(nc1 - nc0),
                conv->activ_params);
        }
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
    const int C1 = FX_QCONV_C;
    int N = dw_ctx->N, C0 = (qconv->C + C1 - 1)/C1;
    int Hi = dw_ctx->Hi, Wi = dw_ctx->Wi;
    int H0 = dw_ctx->H0, W0 = dw_ctx->W0;
    int Hk = qconv->Hk, Wk = qconv->Wk;
    size_t inp_planesize = Hi*Wi, out_planesize = H0*W0;
    int stride_x = qconv->stride_x, stride_y = qconv->stride_y;
    int dilation_x = qconv->dilation_x, dilation_y = qconv->dilation_y;
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
    const int* ofstab_ = dw_ctx->ofstab;
    const int vec_nlanes = FX_VEC_NLANES_U8 >= 8 ? FX_VEC_NLANES_U8/8 : 1;
    const int vec_nlanes_c = vec_nlanes*C1;
    int* ofstab = (int*)alloca((padded_ksize + vec_nlanes_c*ksize)*sizeof(int));
#if _FX_DEPTHWISE_USE_SIMD
    bool useSIMD = (stride_x == 1 || stride_x == 2) && inner_xleft < W0;
    int Wi_simd = Wi - (vec_nlanes - 1)*stride_x - (Wk - 1)*dilation_x;
    Wi_simd = Wi_simd < 0 ? 0 : Wi_simd;
    bool is3x3 = stride_x == 1 && Hk == 3 && Wk == 3;
    bool is3x3_r3 = is3x3 && stride_y == 1 && dilation_y == 1 && dilation_x == 1;
    uint16_t xofsbuf[FX_VEC_NLANES_U8/8*FX_QCONV_C];
    uint16_t* xofstab = (uint16_t*)(ofstab + padded_ksize);
    for (int x = 0; x < vec_nlanes; x++)
        for (int j = 0; j < C1; j++)
            xofsbuf[x*C1 + j] = (uint16_t)(x*stride_x);
    for (int k = 0; k < ksize; k++) {
        int xi = yxtab[k*2+1];
        for (int j = 0; j < vec_nlanes_c; j++)
            xofstab[k*vec_nlanes_c + j] = (uint16_t)(xi + xofsbuf[j]);
    }
#ifdef __ARM_NEON
    uint16x8_t vinp_mask = vdupq_n_u16(inp_mask);
    uint8x8_t vout_mask = vdup_n_u8(out_mask);
    uint16x8_t v_Wi = vdupq_n_u16((uint16_t)Wi);
    uint16x8_t vxofs0 = vld1q_u16(xofsbuf);
#elif defined __AVX2__
    __m256i vinp_mask = _mm256_set1_epi8((int8_t)inp_mask);
    __m256i vout_mask = _mm256_set1_epi8((int8_t)out_mask);
    __m256i v_Wi = _mm256_set1_epi16((int16_t)Wi);
    __m256i vxofs0 = _mm256_loadu_si256((const __m256i*)xofsbuf);
#endif
#else
    const int vec_nlanes = 1;
#endif
    memset(ofstab, 0, padded_ksize*sizeof(int));
    for(int i = 0; i < ksize; i++)
        ofstab[i] = ofstab_[i]*C1;

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        int nc0 = task_id*(N*C0)/ntasks, nc1 = (task_id+1)*(N*C0)/ntasks;
        for (int nc = nc0; nc < nc1; nc++) {
            int c0 = nc % C0, dy0 = 1;
            c0 *= C1;
            const uint8_t* inptr = (const uint8_t*)inptr0 + inp_planesize*nc*C1;
            uint8_t* outptr = (uint8_t*)outptr0 + out_planesize*nc*C1;
            const int16_t* weights = qconv->depthwise_weights + c0*padded_ksize;
            float scale[FX_QCONV_C], biasval[FX_QCONV_C];
            int safe_y0 = nc > nc0 ? 0 : 1;
            int safe_y1 = nc < nc1-1 ? H0 : H0-1;
            for (int j = 0; j < C1; j++) {
                scale[j] = (inp_scale0*inv_out_scale0)*qconv->w_scale[c0+j];
                biasval[j] = (qconv->bias[c0+j] -
                              inp_zp0*qconv->w_sum[c0+j])*scale[j] + out_zp0;
            }

    #ifdef __ARM_NEON
            float32x4_t vscale = vld1q_f32(scale), vbias = vld1q_f32(biasval);
            int16x4_t w0 = vdup_n_s16(0), w1 = w0, w2 = w0,
                      w3 = w0, w4 = w0, w5 = w0, w6 = w0, w7 = w0, w8 = w0;
            if (useSIMD && is3x3) {
                w0 = vld1_s16(weights);
                w1 = vld1_s16(weights+4);
                w2 = vld1_s16(weights+8);
                w3 = vld1_s16(weights+12);
                w4 = vld1_s16(weights+16);
                w5 = vld1_s16(weights+20);
                w6 = vld1_s16(weights+24);
                w7 = vld1_s16(weights+28);
                w8 = vld1_s16(weights+32);
            }
    #endif
            for (int y0 = 0; y0 < H0; y0 += dy0, outptr += W0*C1*dy0) {
            #ifdef __ARM_NEON
                dy0 = inner_ytop <= y0 && y0+3 < inner_ybottom && is3x3_r3 ? 3 : 1;
            #endif
                int x0 = 0, x0_start, x1 = inner_xleft;
                int yi_ = y0*stride_y - pad_top;
                int k0 = 0, k1 = ksize;
                int s0_[FX_QCONV_C]={0,0,0,0};
                bool inner_y = y0 >= inner_ytop && y0 < inner_ybottom;
                if (!inner_y) {
                    for (; k0 < ksize; k0++) {
                        if ((unsigned)(yi_ + yxtab[k0*2]) < (unsigned)Hi)
                            break;
                        if (inp_mask != 0) {
                            s0_[0] += weights[k0*4];
                            s0_[1] += weights[k0*4+1];
                            s0_[2] += weights[k0*4+2];
                            s0_[3] += weights[k0*4+3];
                        }
                    }
                    for (; k1 > 0; k1--) {
                        if ((unsigned)(yi_ + yxtab[k1*2-2]) < (unsigned)Hi)
                            break;
                        if (inp_mask != 0) {
                            s0_[0] += weights[k1*4-4];
                            s0_[1] += weights[k1*4-3];
                            s0_[2] += weights[k1*4-2];
                            s0_[3] += weights[k1*4-1];
                        }
                    }
                    s0_[0] *= inp_mask;
                    s0_[1] *= inp_mask;
                    s0_[2] *= inp_mask;
                    s0_[3] *= inp_mask;
                }
            #ifdef __ARM_NEON
                int32x4_t vs0 = vld1q_s32(s0_);
            #endif
                if (dy0 == 1 && !is3x3 && safe_y0 <= y0 && y0 < safe_y1 && W0 > vec_nlanes)
                    x1 = 0;
                for(;;) {
                    int s_0, s_1, s_2, s_3;
                    if (dy0 == 3) {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            int s_00 = 0, s_01 = 0, s_02 = 0, s_03 = 0;
                            int s_10 = 0, s_11 = 0, s_12 = 0, s_13 = 0;
                            int s_20 = 0, s_21 = 0, s_22 = 0, s_23 = 0;
                            for (int k = 0; k < ksize; k++) {
                                int yi = yi_ + yxtab[k*2];
                                int xi = xi_ + yxtab[k*2+1];
                                int w0 = weights[k*C1];
                                int w1 = weights[k*C1+1];
                                int w2 = weights[k*C1+2];
                                int w3 = weights[k*C1+3];
                                if ((unsigned)xi < (unsigned)Wi) {
                                    int ofs = (yi*Wi + xi)*C1;
                                    s_00 += (inptr[ofs] ^ inp_mask)*w0;
                                    s_01 += (inptr[ofs+1] ^ inp_mask)*w1;
                                    s_02 += (inptr[ofs+2] ^ inp_mask)*w2;
                                    s_03 += (inptr[ofs+3] ^ inp_mask)*w3;
                                    s_10 += (inptr[ofs+Wi*C1] ^ inp_mask)*w0;
                                    s_11 += (inptr[ofs+Wi*C1+1] ^ inp_mask)*w1;
                                    s_12 += (inptr[ofs+Wi*C1+2] ^ inp_mask)*w2;
                                    s_13 += (inptr[ofs+Wi*C1+3] ^ inp_mask)*w3;
                                    s_20 += (inptr[ofs+Wi*C1*2] ^ inp_mask)*w0;
                                    s_21 += (inptr[ofs+Wi*C1*2+1] ^ inp_mask)*w1;
                                    s_22 += (inptr[ofs+Wi*C1*2+2] ^ inp_mask)*w2;
                                    s_23 += (inptr[ofs+Wi*C1*2+3] ^ inp_mask)*w3;
                                } else {
                                    s_00 += inp_mask*w0;
                                    s_01 += inp_mask*w1;
                                    s_02 += inp_mask*w2;
                                    s_03 += inp_mask*w3;
                                    s_10 += inp_mask*w0;
                                    s_11 += inp_mask*w1;
                                    s_12 += inp_mask*w2;
                                    s_13 += inp_mask*w3;
                                    s_20 += inp_mask*w0;
                                    s_21 += inp_mask*w1;
                                    s_22 += inp_mask*w2;
                                    s_23 += inp_mask*w3;
                                }
                            }
                            s_00 = (int)lrintf(s_00*scale[0] + biasval[0]);
                            s_01 = (int)lrintf(s_01*scale[1] + biasval[1]);
                            s_02 = (int)lrintf(s_02*scale[2] + biasval[2]);
                            s_03 = (int)lrintf(s_03*scale[3] + biasval[3]);
                            s_10 = (int)lrintf(s_10*scale[0] + biasval[0]);
                            s_11 = (int)lrintf(s_11*scale[1] + biasval[1]);
                            s_12 = (int)lrintf(s_12*scale[2] + biasval[2]);
                            s_13 = (int)lrintf(s_13*scale[3] + biasval[3]);
                            s_20 = (int)lrintf(s_20*scale[0] + biasval[0]);
                            s_21 = (int)lrintf(s_21*scale[1] + biasval[1]);
                            s_22 = (int)lrintf(s_22*scale[2] + biasval[2]);
                            s_23 = (int)lrintf(s_23*scale[3] + biasval[3]);
                            outptr[x0*C1] = FX_SATURATE(s_00, out_mask);
                            outptr[x0*C1+1] = FX_SATURATE(s_01, out_mask);
                            outptr[x0*C1+2] = FX_SATURATE(s_02, out_mask);
                            outptr[x0*C1+3] = FX_SATURATE(s_03, out_mask);
                            outptr[x0*C1+W0*C1] = FX_SATURATE(s_10, out_mask);
                            outptr[x0*C1+W0*C1+1] = FX_SATURATE(s_11, out_mask);
                            outptr[x0*C1+W0*C1+2] = FX_SATURATE(s_12, out_mask);
                            outptr[x0*C1+W0*C1+3] = FX_SATURATE(s_13, out_mask);
                            outptr[x0*C1+W0*C1*2] = FX_SATURATE(s_20, out_mask);
                            outptr[x0*C1+W0*C1*2+1] = FX_SATURATE(s_21, out_mask);
                            outptr[x0*C1+W0*C1*2+2] = FX_SATURATE(s_22, out_mask);
                            outptr[x0*C1+W0*C1*2+3] = FX_SATURATE(s_23, out_mask);
                        }
                    } else {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            s_0 = s0_[0]; s_1 = s0_[1];
                            s_2 = s0_[2]; s_3 = s0_[3];
                            for (int k = k0; k < k1; k++) {
                                int yi = yi_ + yxtab[k*2];
                                int xi = xi_ + yxtab[k*2+1];
                                int w0 = weights[k*C1];
                                int w1 = weights[k*C1+1];
                                int w2 = weights[k*C1+2];
                                int w3 = weights[k*C1+3];
                                if ((unsigned)xi < (unsigned)Wi) {
                                    int ofs = (yi*Wi + xi)*C1;
                                    s_0 += (inptr[ofs] ^ inp_mask)*w0;
                                    s_1 += (inptr[ofs+1] ^ inp_mask)*w1;
                                    s_2 += (inptr[ofs+2] ^ inp_mask)*w2;
                                    s_3 += (inptr[ofs+3] ^ inp_mask)*w3;
                                }
                                else {
                                    s_0 += inp_mask*w0;
                                    s_1 += inp_mask*w1;
                                    s_2 += inp_mask*w2;
                                    s_3 += inp_mask*w3;
                                }
                            }
                            s_0 = (int)lrintf(s_0*scale[0] + biasval[0]);
                            s_1 = (int)lrintf(s_1*scale[1] + biasval[1]);
                            s_2 = (int)lrintf(s_2*scale[2] + biasval[2]);
                            s_3 = (int)lrintf(s_3*scale[3] + biasval[3]);
                            outptr[x0*C1] = FX_SATURATE(s_0, out_mask);
                            outptr[x0*C1+1] = FX_SATURATE(s_1, out_mask);
                            outptr[x0*C1+2] = FX_SATURATE(s_2, out_mask);
                            outptr[x0*C1+3] = FX_SATURATE(s_3, out_mask);
                        }
                    }
                    if (x0 == W0)
                        break;
                    x0_start = x0;
                    x1 = inner_xright;
                #ifdef __ARM_NEON
                    if (useSIMD) {
                        if (is3x3 && inner_y) {
                            if (dy0 == 3) {
                                for (; x0 < x1; x0 += vec_nlanes) {
                                    if (x0 + vec_nlanes > x1) {
                                        if (x0 <= x0_start)
                                            break;
                                        x0 = x1 - vec_nlanes;
                                    }
                                    int xi_ = x0*stride_x - pad_left;
                                    const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                                    uint8x16_t u0, u1, u2;
                                    int32x4_t s00 = vdupq_n_s32(0), s01 = s00,
                                              s10 = s00, s11 = s00, s20 = s00, s21 = s00;

                                    #undef _FX_QDEPTHWISE_LOAD3
                                    #define _FX_QDEPTHWISE_LOAD3(row) \
                                        u0 = vmovl_u8(vld1_u8(inptr_xi+Wi*C1*row)); \
                                        u1 = vmovl_u8(vld1_u8(inptr_xi+Wi*C1*row+4)); \
                                        u2 = vmovl_u8(vld1_u8(inptr_xi+Wi*C1*row+8)); \
                                        int16x8_t v##row##0 = vreinterpretq_s16_u16( \
                                                veorq_u16(u0, vinp_mask)); \
                                        int16x8_t v##row##1 = vreinterpretq_s16_u16( \
                                                veorq_u16(u1, vinp_mask)); \
                                        int16x8_t v##row##2 = vreinterpretq_s16_u16( \
                                                veorq_u16(u2, vinp_mask))

                                    _FX_QDEPTHWISE_LOAD3(0);
                                    _FX_QDEPTHWISE_LOAD3(1);
                                    _FX_QDEPTHWISE_LOAD3(2);
                                    _FX_QDEPTHWISE_LOAD3(3);
                                    _FX_QDEPTHWISE_LOAD3(4);

                                    #undef _FX_QDEPTHWISE_ACC3
                                    #define _FX_QDEPTHWISE_ACC3(srow, vrow, w0_, w1_, w2_) \
                                        s##srow##0 = vmlal_s16(s##srow##0, \
                                                               vget_low_s16(v##vrow##0), w0_); \
                                        s##srow##1 = vmlal_s16(s##srow##1, \
                                                               vget_high_s16(v##vrow##0), w0_); \
                                        s##srow##0 = vmlal_s16(s##srow##0, \
                                                               vget_low_s16(v##vrow##1), w1_); \
                                        s##srow##1 = vmlal_s16(s##srow##1, \
                                                               vget_high_s16(v##vrow##1), w1_); \
                                        s##srow##0 = vmlal_s16(s##srow##0, \
                                                               vget_low_s16(v##vrow##2), w2_); \
                                        s##srow##1 = vmlal_s16(s##srow##1, \
                                                               vget_high_s16(v##vrow##2), w2_)

                                    _FX_QDEPTHWISE_ACC3(0, 0, w0, w1, w2);
                                    _FX_QDEPTHWISE_ACC3(1, 1, w0, w1, w2);
                                    _FX_QDEPTHWISE_ACC3(2, 2, w0, w1, w2);
                                    _FX_QDEPTHWISE_ACC3(0, 1, w3, w4, w5);
                                    _FX_QDEPTHWISE_ACC3(1, 2, w3, w4, w5);
                                    _FX_QDEPTHWISE_ACC3(2, 3, w3, w4, w5);
                                    _FX_QDEPTHWISE_ACC3(0, 2, w6, w7, w8);
                                    _FX_QDEPTHWISE_ACC3(1, 3, w6, w7, w8);
                                    _FX_QDEPTHWISE_ACC3(2, 4, w6, w7, w8);

                                    s00 = vcvtnq_s32_f32(vfmaq_f32(vbias,
                                                         vcvtq_f32_s32(s00), vscale));
                                    s01 = vcvtnq_s32_f32(vfmaq_f32(vbias,
                                                         vcvtq_f32_s32(s01), vscale));
                                    s10 = vcvtnq_s32_f32(vfmaq_f32(vbias,
                                                         vcvtq_f32_s32(s10), vscale));
                                    s11 = vcvtnq_s32_f32(vfmaq_f32(vbias,
                                                         vcvtq_f32_s32(s11), vscale));
                                    s20 = vcvtnq_s32_f32(vfmaq_f32(vbias,
                                                         vcvtq_f32_s32(s20), vscale));
                                    s21 = vcvtnq_s32_f32(vfmaq_f32(vbias,
                                                         vcvtq_f32_s32(s21), vscale));
                                    u0 = vcombine_u16(vqmovun_s32(s00), vqmovun_s32(s01));
                                    u1 = vcombine_u16(vqmovun_s32(s10), vqmovun_s32(s11));
                                    u2 = vcombine_u16(vqmovun_s32(s20), vqmovun_s32(s21));
                                    vst1_u8(outptr+x0*C1,
                                            veor_u8(vqmovn_u16(u0), vout_mask));
                                    vst1_u8(outptr+x0*C1+W0*C1,
                                            veor_u8(vqmovn_u16(u1), vout_mask));
                                    vst1_u8(outptr+x0*C1+W0*C1*2,
                                            veor_u8(vqmovn_u16(u2), vout_mask));
                                }
                            } else {
                                for (; x0 < x1; x0 += vec_nlanes) {
                                    if (x0 + vec_nlanes > x1) {
                                        if (x0 <= x0_start)
                                            break;
                                        x0 = x1 - vec_nlanes;
                                    }
                                    int xi_ = x0*stride_x - pad_left;
                                    const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                                    uint16x8_t u0, u1, u2;
                                    int32x4_t s0 = vdupq_n_s32(0), s1 = s0;

                                    #undef _FX_QDEPTHWISE_LOAD3
                                    #define _FX_QDEPTHWISE_LOAD3(row) \
                                        u0 = vmovl_u8(vld1_u8(inptr_xi+ofstab[row*3+0])); \
                                        u1 = vmovl_u8(vld1_u8(inptr_xi+ofstab[row*3+1])); \
                                        u2 = vmovl_u8(vld1_u8(inptr_xi+ofstab[row*3+2])); \
                                        int16x8_t v##row##0 = vreinterpretq_s16_u16( \
                                                veorq_u16(u0, vinp_mask)); \
                                        int16x8_t v##row##1 = vreinterpretq_s16_u16( \
                                                veorq_u16(u1, vinp_mask)); \
                                        int16x8_t v##row##2 = vreinterpretq_s16_u16( \
                                                veorq_u16(u2, vinp_mask))

                                    _FX_QDEPTHWISE_LOAD3(0);
                                    _FX_QDEPTHWISE_LOAD3(1);
                                    _FX_QDEPTHWISE_LOAD3(2);
                                    s0 = vmlal_s16(s0, vget_low_s16(v00), w0);
                                    s1 = vmlal_s16(s1, vget_high_s16(v00), w0);
                                    s0 = vmlal_s16(s0, vget_low_s16(v01), w1);
                                    s1 = vmlal_s16(s1, vget_high_s16(v01), w1);
                                    s0 = vmlal_s16(s0, vget_low_s16(v02), w2);
                                    s1 = vmlal_s16(s1, vget_high_s16(v02), w2);

                                    s0 = vmlal_s16(s0, vget_low_s16(v10), w3);
                                    s1 = vmlal_s16(s1, vget_high_s16(v10), w3);
                                    s0 = vmlal_s16(s0, vget_low_s16(v11), w4);
                                    s1 = vmlal_s16(s1, vget_high_s16(v11), w4);
                                    s0 = vmlal_s16(s0, vget_low_s16(v12), w5);
                                    s1 = vmlal_s16(s1, vget_high_s16(v12), w5);

                                    s0 = vmlal_s16(s0, vget_low_s16(v20), w6);
                                    s1 = vmlal_s16(s1, vget_high_s16(v20), w6);
                                    s0 = vmlal_s16(s0, vget_low_s16(v21), w7);
                                    s1 = vmlal_s16(s1, vget_high_s16(v21), w7);
                                    s0 = vmlal_s16(s0, vget_low_s16(v22), w8);
                                    s1 = vmlal_s16(s1, vget_high_s16(v22), w8);

                                    s0 = vcvtnq_s32_f32(vfmaq_f32(vbias,
                                                        vcvtq_f32_s32(s0), vscale));
                                    s1 = vcvtnq_s32_f32(vfmaq_f32(vbias,
                                                        vcvtq_f32_s32(s1), vscale));
                                    u0 = vcombine_u16(vqmovun_s32(s0), vqmovun_s32(s1));
                                    vst1_u8(outptr + x0*C1, veor_u8(vqmovn_u16(u0), vout_mask));
                                }
                            }
                        } else if (stride_x == 1) {
                            if (y0 < safe_y1 && x0 + vec_nlanes/2 <= W0)
                                x1 = W0;
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 == x0_start && x0_start > 0)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                    if (x0 < 0) x0 = 0;
                                }
                                int xi_ = x0*stride_x - pad_left;
                                const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                                int32x4_t s0 = vs0, s1 = vs0;
                                int k = k0;

                                if ((unsigned)xi_ < (unsigned)Wi_simd) {
                                    for (; k <= k1 - 3; k += 3) {
                                        uint16x8_t u0 = vmovl_u8(vld1_u8(inptr_xi+ofstab[k]));
                                        uint16x8_t u1 = vmovl_u8(vld1_u8(inptr_xi+ofstab[k+1]));
                                        uint16x8_t u2 = vmovl_u8(vld1_u8(inptr_xi+ofstab[k+2]));
                                        int16x4_t w0 = vld1_s16(weights + k*4);
                                        int16x4_t w1 = vld1_s16(weights + k*4 + 4);
                                        int16x4_t w2 = vld1_s16(weights + k*4 + 8);

                                        int16x8_t v0 = vreinterpretq_s16_u16(veorq_u16(u0, vinp_mask));
                                        int16x8_t v1 = vreinterpretq_s16_u16(veorq_u16(u1, vinp_mask));
                                        int16x8_t v2 = vreinterpretq_s16_u16(veorq_u16(u2, vinp_mask));

                                        s0 = vmlal_s16(s0, vget_low_s16(v0), w0);
                                        s1 = vmlal_s16(s1, vget_high_s16(v0), w0);
                                        s0 = vmlal_s16(s0, vget_low_s16(v1), w1);
                                        s1 = vmlal_s16(s1, vget_high_s16(v1), w1);
                                        s0 = vmlal_s16(s0, vget_low_s16(v2), w2);
                                        s1 = vmlal_s16(s1, vget_high_s16(v2), w2);
                                    }

                                    for (; k < k1; k++) {
                                        uint16x8_t u0 = vmovl_u8(vld1_u8(inptr_xi + ofstab[k]));
                                        int16x4_t w0 = vld1_s16(weights + k*4);
                                        int16x8_t t0 = vreinterpretq_s16_u16(veorq_u16(u0, vinp_mask));
                                        s0 = vmlal_s16(s0, vget_low_s16(t0), w0);
                                        s1 = vmlal_s16(s1, vget_high_s16(t0), w0);
                                    }
                                } else {
                                    uint16x8_t vxofs = vdupq_n_u16((uint16_t)xi_);
                                    for (; k <= k1 - 3; k += 3) {
                                        uint16x8_t u0 = vmovl_u8(vld1_u8(inptr_xi + ofstab[k]));
                                        uint16x8_t u1 = vmovl_u8(vld1_u8(inptr_xi + ofstab[k+1]));
                                        uint16x8_t u2 = vmovl_u8(vld1_u8(inptr_xi + ofstab[k+2]));
                                        uint16x8_t mask;
                                    #define _FX_APPLY_MASK_U16(i) \
                                        mask = vaddq_u16(vld1q_u16(xofstab + \
                                                (k+i)*vec_nlanes_c), vxofs); \
                                        mask = vcltq_u16(mask, v_Wi); \
                                        mask = vandq_u16(mask, u##i); \
                                        mask = veorq_u16(mask, vinp_mask); \
                                        int16x8_t v##i = vreinterpretq_f16_u16(mask)

                                        _FX_APPLY_MASK_U16(0);
                                        _FX_APPLY_MASK_U16(1);
                                        _FX_APPLY_MASK_U16(2);
                                        int16x4_t w0 = vld1_s16(weights + k*4);
                                        int16x4_t w1 = vld1_s16(weights + k*4 + 4);
                                        int16x4_t w2 = vld1_s16(weights + k*4 + 8);

                                        s0 = vmlal_s16(s0, vget_low_s16(v0), w0);
                                        s1 = vmlal_s16(s1, vget_high_s16(v0), w0);
                                        s0 = vmlal_s16(s0, vget_low_s16(v1), w1);
                                        s1 = vmlal_s16(s1, vget_high_s16(v1), w1);
                                        s0 = vmlal_s16(s0, vget_low_s16(v2), w2);
                                        s1 = vmlal_s16(s1, vget_high_s16(v2), w2);
                                    }

                                    for (; k < k1; k++) {
                                        uint16x8_t u0 = vmovl_u8(vld1_u8(inptr_xi + ofstab[k]));
                                        uint16x8_t mask;
                                        _FX_APPLY_MASK_U16(0);
                                        int16x4_t w0 = vld1_s16(weights + k*4);

                                        s0 = vmlal_s16(s0, vget_low_s16(v0), w0);
                                        s1 = vmlal_s16(s1, vget_high_s16(v0), w0);
                                    }
                                }

                                {
                                s0 = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0), vscale));
                                s1 = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s1), vscale));
                                uint16x8_t r0 = vcombine_u16(vqmovun_s32(s0),
                                                             vqmovun_s32(s1));
                                vst1_u8(outptr + x0*C1, veor_u8(vqmovn_u16(r0), vout_mask));
                                }
                            }
                        } else if (yi_ + (Hk-1)*dilation_y < Hi-1 || x1 < W0) {
                            assert(stride_x == 2);
                            if (y0 < safe_y1 && x0 + vec_nlanes/2 <= W0)
                                x1 = W0;
                            for (; x0 < x1; x0 += vec_nlanes) {
                                if (x0 + vec_nlanes > x1) {
                                    if (x0 <= inner_xleft)
                                        break;
                                    x0 = x1 - vec_nlanes;
                                }
                                int xi_ = x0*stride_x - pad_left;
                                const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                                int32x4_t s0 = vs0, s1 = vs0;
                                int k = k0;

                                #undef _FX_QCONV_LOAD_EVEN_С4
                                #define _FX_QCONV_LOAD_EVEN_C4(addr) \
                                    vmovl_u8(vreinterpret_u8_u32(vld2_u32((const uint32_t*)(addr)).val[0]))

                                if ((unsigned)xi_ < (unsigned)Wi_simd) {
                                    for (; k <= k1 - 3; k += 3) {
                                        uint16x8_t u0 = _FX_QCONV_LOAD_EVEN_C4(inptr_xi+ofstab[k]);
                                        uint16x8_t u1 = _FX_QCONV_LOAD_EVEN_C4(inptr_xi+ofstab[k+1]);
                                        uint16x8_t u2 = _FX_QCONV_LOAD_EVEN_C4(inptr_xi+ofstab[k+2]);
                                        int16x4_t w0 = vld1_s16(weights + k*4);
                                        int16x4_t w1 = vld1_s16(weights + k*4 + 4);
                                        int16x4_t w2 = vld1_s16(weights + k*4 + 8);

                                        int16x8_t v0 = vreinterpretq_s16_u16(veorq_u16(u0, vinp_mask));
                                        int16x8_t v1 = vreinterpretq_s16_u16(veorq_u16(u1, vinp_mask));
                                        int16x8_t v2 = vreinterpretq_s16_u16(veorq_u16(u2, vinp_mask));

                                        s0 = vmlal_s16(s0, vget_low_s16(v0), w0);
                                        s1 = vmlal_s16(s1, vget_high_s16(v0), w0);
                                        s0 = vmlal_s16(s0, vget_low_s16(v1), w1);
                                        s1 = vmlal_s16(s1, vget_high_s16(v1), w1);
                                        s0 = vmlal_s16(s0, vget_low_s16(v2), w2);
                                        s1 = vmlal_s16(s1, vget_high_s16(v2), w2);
                                    }

                                    for (; k < k1; k++) {
                                        uint16x8_t u0 = _FX_QCONV_LOAD_EVEN_C4(inptr_xi + ofstab[k]);
                                        int16x4_t w0 = vld1_s16(weights + k*4);
                                        int16x8_t t0 = vreinterpretq_s16_u16(veorq_u16(u0, vinp_mask));
                                        s0 = vmlal_s16(s0, vget_low_s16(t0), w0);
                                        s1 = vmlal_s16(s1, vget_high_s16(t0), w0);
                                    }
                                } else {
                                    uint16x8_t vxofs = vdupq_n_u16((uint16_t)xi_);
                                    for (; k <= k1 - 3; k += 3) {
                                        uint16x8_t u0 = _FX_QCONV_LOAD_EVEN_C4(inptr_xi+ofstab[k]);
                                        uint16x8_t u1 = _FX_QCONV_LOAD_EVEN_C4(inptr_xi+ofstab[k+1]);
                                        uint16x8_t u2 = _FX_QCONV_LOAD_EVEN_C4(inptr_xi+ofstab[k+2]);
                                        uint16x8_t mask;
                                        _FX_APPLY_MASK_U16(0);
                                        _FX_APPLY_MASK_U16(1);
                                        _FX_APPLY_MASK_U16(2);
                                        int16x4_t w0 = vld1_s16(weights + k*4);
                                        int16x4_t w1 = vld1_s16(weights + k*4 + 4);
                                        int16x4_t w2 = vld1_s16(weights + k*4 + 8);

                                        s0 = vmlal_s16(s0, vget_low_s16(v0), w0);
                                        s1 = vmlal_s16(s1, vget_high_s16(v0), w0);
                                        s0 = vmlal_s16(s0, vget_low_s16(v1), w1);
                                        s1 = vmlal_s16(s1, vget_high_s16(v1), w1);
                                        s0 = vmlal_s16(s0, vget_low_s16(v2), w2);
                                        s1 = vmlal_s16(s1, vget_high_s16(v2), w2);
                                    }

                                    for (; k < k1; k++) {
                                        uint16x8_t u0 = _FX_QCONV_LOAD_EVEN_C4(inptr_xi+ofstab[k]);
                                        uint16x8_t mask;
                                        _FX_APPLY_MASK_U16(0);
                                        int16x4_t w0 = vld1_s16(weights + k*4);

                                        s0 = vmlal_s16(s0, vget_low_s16(v0), w0);
                                        s1 = vmlal_s16(s1, vget_high_s16(v0), w0);
                                    }
                                }

                                {
                                s0 = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s0), vscale));
                                s1 = vcvtnq_s32_f32(vfmaq_f32(vbias, vcvtq_f32_s32(s1), vscale));
                                uint16x8_t t0 = vcombine_u16(vqmovun_s32(s0),
                                                             vqmovun_s32(s1));
                                vst1_u8(outptr + x0*C1, veor_u8(vqmovn_u16(t0), vout_mask));
                                }
                            }
                        }
                    }
                #endif
                    if (dy0 == 3) {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            int s_00 = 0, s_01 = 0, s_02 = 0, s_03 = 0;
                            int s_10 = 0, s_11 = 0, s_12 = 0, s_13 = 0;
                            int s_20 = 0, s_21 = 0, s_22 = 0, s_23 = 0;
                            const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                            for (int k = 0; k < ksize; k++) {
                                int ofs = ofstab[k];
                                int w0 = weights[k*C1];
                                int w1 = weights[k*C1+1];
                                int w2 = weights[k*C1+2];
                                int w3 = weights[k*C1+3];
                                s_00 += (inptr_xi[ofs] ^ inp_mask)*w0;
                                s_01 += (inptr_xi[ofs+1] ^ inp_mask)*w1;
                                s_02 += (inptr_xi[ofs+2] ^ inp_mask)*w2;
                                s_03 += (inptr_xi[ofs+3] ^ inp_mask)*w3;
                                s_10 += (inptr_xi[ofs+Wi*C1] ^ inp_mask)*w0;
                                s_11 += (inptr_xi[ofs+Wi*C1+1] ^ inp_mask)*w1;
                                s_12 += (inptr_xi[ofs+Wi*C1+2] ^ inp_mask)*w2;
                                s_13 += (inptr_xi[ofs+Wi*C1+3] ^ inp_mask)*w3;
                                s_20 += (inptr_xi[ofs+Wi*C1*2] ^ inp_mask)*w0;
                                s_21 += (inptr_xi[ofs+Wi*C1*2+1] ^ inp_mask)*w1;
                                s_22 += (inptr_xi[ofs+Wi*C1*2+2] ^ inp_mask)*w2;
                                s_23 += (inptr_xi[ofs+Wi*C1*2+3] ^ inp_mask)*w3;
                            }
                            s_00 = (int)lrintf(s_00*scale[0] + biasval[0]);
                            s_01 = (int)lrintf(s_01*scale[1] + biasval[1]);
                            s_02 = (int)lrintf(s_02*scale[2] + biasval[2]);
                            s_03 = (int)lrintf(s_03*scale[3] + biasval[3]);
                            s_10 = (int)lrintf(s_10*scale[0] + biasval[0]);
                            s_11 = (int)lrintf(s_11*scale[1] + biasval[1]);
                            s_12 = (int)lrintf(s_12*scale[2] + biasval[2]);
                            s_13 = (int)lrintf(s_13*scale[3] + biasval[3]);
                            s_20 = (int)lrintf(s_20*scale[0] + biasval[0]);
                            s_21 = (int)lrintf(s_21*scale[1] + biasval[1]);
                            s_22 = (int)lrintf(s_22*scale[2] + biasval[2]);
                            s_23 = (int)lrintf(s_23*scale[3] + biasval[3]);
                            outptr[x0*C1] = FX_SATURATE(s_00, out_mask);
                            outptr[x0*C1+1] = FX_SATURATE(s_01, out_mask);
                            outptr[x0*C1+2] = FX_SATURATE(s_02, out_mask);
                            outptr[x0*C1+3] = FX_SATURATE(s_03, out_mask);
                            outptr[x0*C1+W0*C1] = FX_SATURATE(s_10, out_mask);
                            outptr[x0*C1+W0*C1+1] = FX_SATURATE(s_11, out_mask);
                            outptr[x0*C1+W0*C1+2] = FX_SATURATE(s_12, out_mask);
                            outptr[x0*C1+W0*C1+3] = FX_SATURATE(s_13, out_mask);
                            outptr[x0*C1+W0*C1*2] = FX_SATURATE(s_20, out_mask);
                            outptr[x0*C1+W0*C1*2+1] = FX_SATURATE(s_21, out_mask);
                            outptr[x0*C1+W0*C1*2+2] = FX_SATURATE(s_22, out_mask);
                            outptr[x0*C1+W0*C1*2+3] = FX_SATURATE(s_23, out_mask);
                        }
                    } else {
                        for (; x0 < x1; x0++) {
                            int xi_ = x0*stride_x - pad_left;
                            const uint8_t* inptr_xi = inptr + (Wi*yi_ + xi_)*C1;
                            s_0 = s0_[0]; s_1 = s0_[1];
                            s_2 = s0_[2]; s_3 = s0_[3];
                            for (int k = k0; k < k1; k++) {
                                int ofs = ofstab[k];
                                int w0 = weights[k*C1];
                                int w1 = weights[k*C1+1];
                                int w2 = weights[k*C1+2];
                                int w3 = weights[k*C1+3];
                                s_0 += (inptr_xi[ofs] ^ inp_mask)*w0;
                                s_1 += (inptr_xi[ofs+1] ^ inp_mask)*w1;
                                s_2 += (inptr_xi[ofs+2] ^ inp_mask)*w2;
                                s_3 += (inptr_xi[ofs+3] ^ inp_mask)*w3;
                            }
                            s_0 = (int)lrintf(s_0*scale[0] + biasval[0]);
                            s_1 = (int)lrintf(s_1*scale[1] + biasval[1]);
                            s_2 = (int)lrintf(s_2*scale[2] + biasval[2]);
                            s_3 = (int)lrintf(s_3*scale[3] + biasval[3]);
                            outptr[x0*C1] = FX_SATURATE(s_0, out_mask);
                            outptr[x0*C1+1] = FX_SATURATE(s_1, out_mask);
                            outptr[x0*C1+2] = FX_SATURATE(s_2, out_mask);
                            outptr[x0*C1+3] = FX_SATURATE(s_3, out_mask);
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
