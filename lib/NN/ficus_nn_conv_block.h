/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// fp16/fp32 convolution kernels for im2col-based convolution.
// The header is not intented to be used alone.
// It is assumed to be included into OpConv.fx
static void _fx_conv_block_f32( int k, const void* a_, const void* b_,
                                void* c_, int ldc, const void* pb_, int ldp,
                                const float* bias, float alpha, float maxval, bool activ)
{
    const float* a = (const float*)a_;
    const float* b = (const float*)b_;
    float* c = (float*)c_;
    const float* pb = (const float*)pb_;

#ifdef __ARM_NEON
#if FX_CONV_MR == 4 && FX_CONV_NR == 28
    float32x4_t c00 = vdupq_n_f32(bias[0]), c01 = c00, c02 = c00, c03 = c00, c04 = c00, c05 = c00, c06 = c00;
    float32x4_t c10 = vdupq_n_f32(bias[1]), c11 = c10, c12 = c10, c13 = c10, c14 = c10, c15 = c10, c16 = c10;
    float32x4_t c20 = vdupq_n_f32(bias[2]), c21 = c20, c22 = c20, c23 = c20, c24 = c20, c25 = c20, c26 = c20;
    float32x4_t c30 = vdupq_n_f32(bias[3]), c31 = c30, c32 = c30, c33 = c30, c34 = c30, c35 = c30, c36 = c30;

    for( int p = 0; p < k; p++, a += FX_CONV_MR, b += FX_CONV_NR )
    {
        float32x4_t a0 = vld1q_f32(a), b0, b1, b2;
        b0 = vld1q_f32(b); b1 = vld1q_f32(b + 4); b2 = vld1q_f32(b + 8);

        c00 = vfmaq_laneq_f32(c00, b0, a0, 0);
        c01 = vfmaq_laneq_f32(c01, b1, a0, 0);
        c02 = vfmaq_laneq_f32(c02, b2, a0, 0);
        c10 = vfmaq_laneq_f32(c10, b0, a0, 1);
        c11 = vfmaq_laneq_f32(c11, b1, a0, 1);
        c12 = vfmaq_laneq_f32(c12, b2, a0, 1);
        c20 = vfmaq_laneq_f32(c20, b0, a0, 2);
        c21 = vfmaq_laneq_f32(c21, b1, a0, 2);
        c22 = vfmaq_laneq_f32(c22, b2, a0, 2);
        c30 = vfmaq_laneq_f32(c30, b0, a0, 3);
        c31 = vfmaq_laneq_f32(c31, b1, a0, 3);
        c32 = vfmaq_laneq_f32(c32, b2, a0, 3);

        b0 = vld1q_f32(b + 12); b1 = vld1q_f32(b + 16); b2 = vld1q_f32(b + 20);

        c03 = vfmaq_laneq_f32(c03, b0, a0, 0);
        c04 = vfmaq_laneq_f32(c04, b1, a0, 0);
        c05 = vfmaq_laneq_f32(c05, b2, a0, 0);
        c13 = vfmaq_laneq_f32(c13, b0, a0, 1);
        c14 = vfmaq_laneq_f32(c14, b1, a0, 1);
        c15 = vfmaq_laneq_f32(c15, b2, a0, 1);
        c23 = vfmaq_laneq_f32(c23, b0, a0, 2);
        c24 = vfmaq_laneq_f32(c24, b1, a0, 2);
        c25 = vfmaq_laneq_f32(c25, b2, a0, 2);
        c33 = vfmaq_laneq_f32(c33, b0, a0, 3);
        c34 = vfmaq_laneq_f32(c34, b1, a0, 3);
        c35 = vfmaq_laneq_f32(c35, b2, a0, 3);

        b0 = vld1q_f32(b + 24);
        c06 = vfmaq_laneq_f32(c06, b0, a0, 0);
        c16 = vfmaq_laneq_f32(c16, b0, a0, 1);
        c26 = vfmaq_laneq_f32(c26, b0, a0, 2);
        c36 = vfmaq_laneq_f32(c36, b0, a0, 3);
    }

    if (pb) {
        c00 = vaddq_f32(c00, vld1q_f32(pb));
        c01 = vaddq_f32(c01, vld1q_f32(pb + 4));
        c02 = vaddq_f32(c02, vld1q_f32(pb + 8));
        c03 = vaddq_f32(c03, vld1q_f32(pb + 12));
        c04 = vaddq_f32(c04, vld1q_f32(pb + 16));
        c05 = vaddq_f32(c05, vld1q_f32(pb + 20));
        c06 = vaddq_f32(c06, vld1q_f32(pb + 24));

        c10 = vaddq_f32(c10, vld1q_f32(pb + ldp));
        c11 = vaddq_f32(c11, vld1q_f32(pb + ldp + 4));
        c12 = vaddq_f32(c12, vld1q_f32(pb + ldp + 8));
        c13 = vaddq_f32(c13, vld1q_f32(pb + ldp + 12));
        c14 = vaddq_f32(c14, vld1q_f32(pb + ldp + 16));
        c15 = vaddq_f32(c15, vld1q_f32(pb + ldp + 20));
        c16 = vaddq_f32(c16, vld1q_f32(pb + ldp + 24));

        c20 = vaddq_f32(c20, vld1q_f32(pb + ldp*2));
        c21 = vaddq_f32(c21, vld1q_f32(pb + ldp*2 + 4));
        c22 = vaddq_f32(c22, vld1q_f32(pb + ldp*2 + 8));
        c23 = vaddq_f32(c23, vld1q_f32(pb + ldp*2 + 12));
        c24 = vaddq_f32(c24, vld1q_f32(pb + ldp*2 + 16));
        c25 = vaddq_f32(c25, vld1q_f32(pb + ldp*2 + 20));
        c26 = vaddq_f32(c26, vld1q_f32(pb + ldp*2 + 24));

        c30 = vaddq_f32(c30, vld1q_f32(pb + ldp*3));
        c31 = vaddq_f32(c31, vld1q_f32(pb + ldp*3 + 4));
        c32 = vaddq_f32(c32, vld1q_f32(pb + ldp*3 + 8));
        c33 = vaddq_f32(c33, vld1q_f32(pb + ldp*3 + 12));
        c34 = vaddq_f32(c34, vld1q_f32(pb + ldp*3 + 16));
        c35 = vaddq_f32(c35, vld1q_f32(pb + ldp*3 + 20));
        c36 = vaddq_f32(c36, vld1q_f32(pb + ldp*3 + 24));
    }

    if (activ) {
        float32x4_t valpha = vdupq_n_f32(alpha), vmax = vdupq_n_f32(maxval);
        float32x4_t z = vdupq_n_f32(0.f), one = vdupq_n_f32(1.f);
        c00 = vmulq_f32(vminq_f32(c00, vmax), vbslq_f32(vcltq_f32(c00, z), valpha, one));
        c01 = vmulq_f32(vminq_f32(c01, vmax), vbslq_f32(vcltq_f32(c01, z), valpha, one));
        c02 = vmulq_f32(vminq_f32(c02, vmax), vbslq_f32(vcltq_f32(c02, z), valpha, one));
        c03 = vmulq_f32(vminq_f32(c03, vmax), vbslq_f32(vcltq_f32(c03, z), valpha, one));
        c04 = vmulq_f32(vminq_f32(c04, vmax), vbslq_f32(vcltq_f32(c04, z), valpha, one));
        c05 = vmulq_f32(vminq_f32(c05, vmax), vbslq_f32(vcltq_f32(c05, z), valpha, one));
        c06 = vmulq_f32(vminq_f32(c06, vmax), vbslq_f32(vcltq_f32(c06, z), valpha, one));

        c10 = vmulq_f32(vminq_f32(c10, vmax), vbslq_f32(vcltq_f32(c10, z), valpha, one));
        c11 = vmulq_f32(vminq_f32(c11, vmax), vbslq_f32(vcltq_f32(c11, z), valpha, one));
        c12 = vmulq_f32(vminq_f32(c12, vmax), vbslq_f32(vcltq_f32(c12, z), valpha, one));
        c13 = vmulq_f32(vminq_f32(c13, vmax), vbslq_f32(vcltq_f32(c13, z), valpha, one));
        c14 = vmulq_f32(vminq_f32(c14, vmax), vbslq_f32(vcltq_f32(c14, z), valpha, one));
        c15 = vmulq_f32(vminq_f32(c15, vmax), vbslq_f32(vcltq_f32(c15, z), valpha, one));
        c16 = vmulq_f32(vminq_f32(c16, vmax), vbslq_f32(vcltq_f32(c16, z), valpha, one));

        c20 = vmulq_f32(vminq_f32(c20, vmax), vbslq_f32(vcltq_f32(c20, z), valpha, one));
        c21 = vmulq_f32(vminq_f32(c21, vmax), vbslq_f32(vcltq_f32(c21, z), valpha, one));
        c22 = vmulq_f32(vminq_f32(c22, vmax), vbslq_f32(vcltq_f32(c22, z), valpha, one));
        c23 = vmulq_f32(vminq_f32(c23, vmax), vbslq_f32(vcltq_f32(c23, z), valpha, one));
        c24 = vmulq_f32(vminq_f32(c24, vmax), vbslq_f32(vcltq_f32(c24, z), valpha, one));
        c25 = vmulq_f32(vminq_f32(c25, vmax), vbslq_f32(vcltq_f32(c25, z), valpha, one));
        c26 = vmulq_f32(vminq_f32(c26, vmax), vbslq_f32(vcltq_f32(c26, z), valpha, one));

        c30 = vmulq_f32(vminq_f32(c30, vmax), vbslq_f32(vcltq_f32(c30, z), valpha, one));
        c31 = vmulq_f32(vminq_f32(c31, vmax), vbslq_f32(vcltq_f32(c31, z), valpha, one));
        c32 = vmulq_f32(vminq_f32(c32, vmax), vbslq_f32(vcltq_f32(c32, z), valpha, one));
        c33 = vmulq_f32(vminq_f32(c33, vmax), vbslq_f32(vcltq_f32(c33, z), valpha, one));
        c34 = vmulq_f32(vminq_f32(c34, vmax), vbslq_f32(vcltq_f32(c34, z), valpha, one));
        c35 = vmulq_f32(vminq_f32(c35, vmax), vbslq_f32(vcltq_f32(c35, z), valpha, one));
        c36 = vmulq_f32(vminq_f32(c36, vmax), vbslq_f32(vcltq_f32(c36, z), valpha, one));
    }
    vst1q_f32(c, c00); vst1q_f32(c+4, c01);
    vst1q_f32(c+8, c02); vst1q_f32(c+12, c03);
    vst1q_f32(c+16, c04); vst1q_f32(c+20, c05);
    vst1q_f32(c+24, c06);

    vst1q_f32(c+ldc, c10); vst1q_f32(c+ldc+4, c11);
    vst1q_f32(c+ldc+8, c12); vst1q_f32(c+ldc+12, c13);
    vst1q_f32(c+ldc+16, c14); vst1q_f32(c+ldc+20, c15);
    vst1q_f32(c+ldc+24, c16);

    vst1q_f32(c+ldc*2, c20); vst1q_f32(c+ldc*2+4, c21);
    vst1q_f32(c+ldc*2+8, c22); vst1q_f32(c+ldc*2+12, c23);
    vst1q_f32(c+ldc*2+16, c24); vst1q_f32(c+ldc*2+20, c25);
    vst1q_f32(c+ldc*2+24, c26);

    vst1q_f32(c+ldc*3, c30); vst1q_f32(c+ldc*3+4, c31);
    vst1q_f32(c+ldc*3+8, c32); vst1q_f32(c+ldc*3+12, c33);
    vst1q_f32(c+ldc*3+16, c34); vst1q_f32(c+ldc*3+20, c35);
    vst1q_f32(c+ldc*3+24, c36);
#else
#error "unsupported FX_CONV_MR and FX_CONV_NR"
#endif
#else
    float cbuf[FX_CONV_MR*FX_CONV_NR];
    for( int i = 0; i < FX_CONV_MR; i++ )
    {
        float beta = bias[i];
        for( int j = 0; j < FX_CONV_NR; j++ )
            cbuf[i*FX_CONV_NR + j] = beta;
        if (pb) {
            for( int j = 0; j < FX_CONV_NR; j++ )
                cbuf[i*FX_CONV_NR + j] += pb[i*ldc + j];
        }
    }
    for( int p = 0; p < k; p++ )
    {
        for( int i = 0; i < FX_CONV_MR; i++ )
        {
            float ai = a[FX_CONV_MR*p + i];
            for( int j = 0; j < FX_CONV_NR; j++ )
                cbuf[i*FX_CONV_NR+j] += b[FX_CONV_NR*p + j]*ai;
        }
    }
    if (activ) {
        for( int i = 0; i < FX_CONV_MR*FX_CONV_NR; i++ )
        {
            float v = cbuf[i];
            v = v <= maxval ? v : maxval;
            v *= (v < 0.f ? alpha : 1.f);
            cbuf[i] = v;
        }
    }
    for(int i = 0; i < FX_CONV_MR; i++) {
        for(int j = 0; j < FX_CONV_NR; j++)
            c[i*ldc + j] = cbuf[i*FX_CONV_NR + j];
    }
#endif
}

#ifdef _FX_NN_ENABLE_FP16
static void _fx_conv_block_f16( int k, const void *a_, const void *b_,
                                void *c_, int ldc, const void* pb_, int ldp,
                                const float* bias, float alpha,
                                float maxval, bool activ )
{
    const fx_f16* a = (const fx_f16*)a_;
    const fx_f16* b = (const fx_f16*)b_;
    fx_f16* c = (fx_f16*)c_;
    const fx_f16* pb = (const fx_f16*)pb_;
#if FX_CONV_NR_FP16 == 24 && FX_CONV_MR_FP16 == 8
    float cbuf[FX_CONV_MR_FP16*FX_CONV_NR_FP16];

#undef _FX_SET_BIAS
#define _FX_SET_BIAS(row) \
    bv = vdupq_n_f32(bias[row]); \
    vst1q_f32(cbuf + row*FX_CONV_NR_FP16, bv); \
    vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 4, bv); \
    vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 8, bv); \
    vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 12, bv); \
    vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 16, bv); \
    vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 20, bv)

    float32x4_t bv;
    _FX_SET_BIAS(0);
    _FX_SET_BIAS(1);
    _FX_SET_BIAS(2);
    _FX_SET_BIAS(3);
    _FX_SET_BIAS(4);
    _FX_SET_BIAS(5);
    _FX_SET_BIAS(6);
    _FX_SET_BIAS(7);

    const int BLOCK_SZ = 64;
    for( int k0 = 0; k0 < k; ) {
        float16x8_t c00 = vdupq_n_f16((fx_f16)0.f), c01 = c00, c02 = c00;
        float16x8_t c10 = vdupq_n_f16((fx_f16)0.f), c11 = c10, c12 = c10;
        float16x8_t c20 = vdupq_n_f16((fx_f16)0.f), c21 = c20, c22 = c20;
        float16x8_t c30 = vdupq_n_f16((fx_f16)0.f), c31 = c30, c32 = c30;
        float16x8_t c40 = vdupq_n_f16((fx_f16)0.f), c41 = c40, c42 = c40;
        float16x8_t c50 = vdupq_n_f16((fx_f16)0.f), c51 = c50, c52 = c50;
        float16x8_t c60 = vdupq_n_f16((fx_f16)0.f), c61 = c60, c62 = c60;
        float16x8_t c70 = vdupq_n_f16((fx_f16)0.f), c71 = c70, c72 = c70;
        int k1 = k0 + BLOCK_SZ <= k ? k0 + BLOCK_SZ : k;

        for( ; k0 < k1; k0++, a += FX_CONV_MR_FP16, b += FX_CONV_NR_FP16 )
        {
            float16x8_t a0 = vld1q_f16(a);
            float16x8_t b0 = vld1q_f16(b), b1 = vld1q_f16(b + 8), b2 = vld1q_f16(b + 16);

            c00 = vfmaq_laneq_f16(c00, b0, a0, 0);
            c01 = vfmaq_laneq_f16(c01, b1, a0, 0);
            c02 = vfmaq_laneq_f16(c02, b2, a0, 0);

            c10 = vfmaq_laneq_f16(c10, b0, a0, 1);
            c11 = vfmaq_laneq_f16(c11, b1, a0, 1);
            c12 = vfmaq_laneq_f16(c12, b2, a0, 1);

            c20 = vfmaq_laneq_f16(c20, b0, a0, 2);
            c21 = vfmaq_laneq_f16(c21, b1, a0, 2);
            c22 = vfmaq_laneq_f16(c22, b2, a0, 2);

            c30 = vfmaq_laneq_f16(c30, b0, a0, 3);
            c31 = vfmaq_laneq_f16(c31, b1, a0, 3);
            c32 = vfmaq_laneq_f16(c32, b2, a0, 3);

            c40 = vfmaq_laneq_f16(c40, b0, a0, 4);
            c41 = vfmaq_laneq_f16(c41, b1, a0, 4);
            c42 = vfmaq_laneq_f16(c42, b2, a0, 4);

            c50 = vfmaq_laneq_f16(c50, b0, a0, 5);
            c51 = vfmaq_laneq_f16(c51, b1, a0, 5);
            c52 = vfmaq_laneq_f16(c52, b2, a0, 5);

            c60 = vfmaq_laneq_f16(c60, b0, a0, 6);
            c61 = vfmaq_laneq_f16(c61, b1, a0, 6);
            c62 = vfmaq_laneq_f16(c62, b2, a0, 6);

            c70 = vfmaq_laneq_f16(c70, b0, a0, 7);
            c71 = vfmaq_laneq_f16(c71, b1, a0, 7);
            c72 = vfmaq_laneq_f16(c72, b2, a0, 7);
        }

        float32x4_t t0, t1, t2, t3, t4, t5;

    #undef _FX_UPDATE_CBUF_ROW
    #define _FX_UPDATE_CBUF_ROW(row) \
        t0 = vcvt_f32_f16(vget_low_f16(c##row##0)); \
        t1 = vcvt_f32_f16(vget_high_f16(c##row##0)); \
        t2 = vcvt_f32_f16(vget_low_f16(c##row##1)); \
        t3 = vcvt_f32_f16(vget_high_f16(c##row##1)); \
        t4 = vcvt_f32_f16(vget_low_f16(c##row##2)); \
        t5 = vcvt_f32_f16(vget_high_f16(c##row##2)); \
        t0 = vaddq_f32(t0, vld1q_f32(cbuf + row*FX_CONV_NR_FP16)); \
        t1 = vaddq_f32(t1, vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 4)); \
        t2 = vaddq_f32(t2, vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 8)); \
        t3 = vaddq_f32(t3, vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 12)); \
        t4 = vaddq_f32(t4, vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 16)); \
        t5 = vaddq_f32(t5, vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 20)); \
        vst1q_f32(cbuf + row*FX_CONV_NR_FP16, t0); \
        vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 4, t1); \
        vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 8, t2); \
        vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 12, t3); \
        vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 16, t4); \
        vst1q_f32(cbuf + row*FX_CONV_NR_FP16 + 20, t5)

        _FX_UPDATE_CBUF_ROW(0);
        _FX_UPDATE_CBUF_ROW(1);
        _FX_UPDATE_CBUF_ROW(2);
        _FX_UPDATE_CBUF_ROW(3);
        _FX_UPDATE_CBUF_ROW(4);
        _FX_UPDATE_CBUF_ROW(5);
        _FX_UPDATE_CBUF_ROW(6);
        _FX_UPDATE_CBUF_ROW(7);
    }

    float16x8_t valpha = vdupq_n_f16((__fp16)alpha);
    float16x8_t vmax = vdupq_n_f16((__fp16)maxval);
    float16x8_t z = vdupq_n_f16(0.f), one = vdupq_n_f16(1.f);
    float16x8_t c0, c1, c2;
    float16x4_t f0, f1;
#undef _FX_FINIT_ROW
#define _FX_FINIT_ROW(row) \
    f0 = vcvt_f16_f32(vld1q_f32(cbuf + row*FX_CONV_NR_FP16)); \
    f1 = vcvt_f16_f32(vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 4)); \
    c0 = vcombine_f16(f0, f1); \
    f0 = vcvt_f16_f32(vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 8)); \
    f1 = vcvt_f16_f32(vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 12)); \
    c1 = vcombine_f16(f0, f1); \
    f0 = vcvt_f16_f32(vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 16)); \
    f1 = vcvt_f16_f32(vld1q_f32(cbuf + row*FX_CONV_NR_FP16 + 20)); \
    c2 = vcombine_f16(f0, f1); \
    if (pb) { \
        c0 = vaddq_f16(c0, vld1q_f16(pb + row*ldp)); \
        c1 = vaddq_f16(c1, vld1q_f16(pb + row*ldp + 8)); \
        c2 = vaddq_f16(c2, vld1q_f16(pb + row*ldp + 16)); \
    } \
    if (activ) { \
        c0 = vmulq_f16(vminq_f16(c0, vmax), vbslq_f16(vcltq_f16(c0, z), valpha, one)); \
        c1 = vmulq_f16(vminq_f16(c1, vmax), vbslq_f16(vcltq_f16(c1, z), valpha, one)); \
        c2 = vmulq_f16(vminq_f16(c2, vmax), vbslq_f16(vcltq_f16(c2, z), valpha, one)); \
    } \
    vst1q_f16(c + row*ldc, c0); \
    vst1q_f16(c + row*ldc + 8, c1); \
    vst1q_f16(c + row*ldc + 16, c2)

    _FX_FINIT_ROW(0);
    _FX_FINIT_ROW(1);
    _FX_FINIT_ROW(2);
    _FX_FINIT_ROW(3);
    _FX_FINIT_ROW(4);
    _FX_FINIT_ROW(5);
    _FX_FINIT_ROW(6);
    _FX_FINIT_ROW(7);
#else
    //#error "unsupported FX_CONV_NR_FP16 and/or FX_CONV_MR_FP16"
    float cbuf[FX_CONV_MR_FP16*FX_CONV_NR_FP16];
    for( int i = 0; i < FX_CONV_MR_FP16; i++ )
    {
        float beta = bias[i];
        if (pb) {
            for( int j = 0; j < FX_CONV_NR_FP16; j++ )
                cbuf[i*FX_CONV_NR_FP16 + j] = beta + pb[i*ldp + j];
        } else {
            for( int j = 0; j < FX_CONV_NR_FP16; j++ )
                cbuf[i*FX_CONV_NR_FP16 + j] = beta;
        }
    }
    for( int p = 0; p < k; p++ )
    {
        for( int i = 0; i < FX_CONV_MR_FP16; i++ )
        {
            float ai = a[FX_CONV_MR_FP16*p + i];
            for( int j = 0; j < FX_CONV_NR_FP16; j++ )
                cbuf[i*FX_CONV_NR_FP16+j] += b[FX_CONV_NR_FP16*p + j]*ai;
        }
    }
    if (activ) {
        for( int i = 0; i < FX_CONV_MR_FP16*FX_CONV_NR_FP16; i++ )
        {
            float v = cbuf[i];
            v = v <= maxval ? v : maxval;
            v *= (v < 0.f ? alpha : 1.f);
            cbuf[i] = v;
        }
    }
    for(int i = 0; i < FX_CONV_MR_FP16; i++) {
        for(int j = 0; j < FX_CONV_NR_FP16; j++)
            c[i*ldc + j] = FX_FLOAT16(cbuf[i*FX_CONV_NR_FP16 + j]);
    }
#endif
}
#endif
