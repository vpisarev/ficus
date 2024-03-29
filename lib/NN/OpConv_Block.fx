/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

@ccode {
#include "ficus_nn_common.h"

void _fx_conv_block_f32( int k, const void* a_, const void* b_,
                         void* c_, int ldc, const void* bp_, int ldp,
                         const float* bias, float alpha, float maxval, bool activ)
{
    const float* a = (const float*)a_;
    const float* b = (const float*)b_;
    float* c = (float*)c_;
    const float* bp = (const float*)bp_;

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

    if (bp) {
        c00 = vaddq_f32(c00, vld1q_f32(bp));
        c01 = vaddq_f32(c01, vld1q_f32(bp + 4));
        c02 = vaddq_f32(c02, vld1q_f32(bp + 8));
        c03 = vaddq_f32(c03, vld1q_f32(bp + 12));
        c04 = vaddq_f32(c04, vld1q_f32(bp + 16));
        c05 = vaddq_f32(c05, vld1q_f32(bp + 20));
        c06 = vaddq_f32(c06, vld1q_f32(bp + 24));

        c10 = vaddq_f32(c10, vld1q_f32(bp + ldp));
        c11 = vaddq_f32(c11, vld1q_f32(bp + ldp + 4));
        c12 = vaddq_f32(c12, vld1q_f32(bp + ldp + 8));
        c13 = vaddq_f32(c13, vld1q_f32(bp + ldp + 12));
        c14 = vaddq_f32(c14, vld1q_f32(bp + ldp + 16));
        c15 = vaddq_f32(c15, vld1q_f32(bp + ldp + 20));
        c16 = vaddq_f32(c16, vld1q_f32(bp + ldp + 24));

        c20 = vaddq_f32(c20, vld1q_f32(bp + ldp*2));
        c21 = vaddq_f32(c21, vld1q_f32(bp + ldp*2 + 4));
        c22 = vaddq_f32(c22, vld1q_f32(bp + ldp*2 + 8));
        c23 = vaddq_f32(c23, vld1q_f32(bp + ldp*2 + 12));
        c24 = vaddq_f32(c24, vld1q_f32(bp + ldp*2 + 16));
        c25 = vaddq_f32(c25, vld1q_f32(bp + ldp*2 + 20));
        c26 = vaddq_f32(c26, vld1q_f32(bp + ldp*2 + 24));

        c30 = vaddq_f32(c30, vld1q_f32(bp + ldp*3));
        c31 = vaddq_f32(c31, vld1q_f32(bp + ldp*3 + 4));
        c32 = vaddq_f32(c32, vld1q_f32(bp + ldp*3 + 8));
        c33 = vaddq_f32(c33, vld1q_f32(bp + ldp*3 + 12));
        c34 = vaddq_f32(c34, vld1q_f32(bp + ldp*3 + 16));
        c35 = vaddq_f32(c35, vld1q_f32(bp + ldp*3 + 20));
        c36 = vaddq_f32(c36, vld1q_f32(bp + ldp*3 + 24));
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
        if (bp) {
            for( int j = 0; j < FX_CONV_NR; j++ )
                cbuf[i*FX_CONV_NR + j] += bp[i*ldc + j];
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

void _fx_conv_block_f16( int k, const void *a_, const void *b_,
                        void *c_, int ldc, const void* bp_, int ldp,
                        const float* bias, float alpha,
                        float maxval, bool activ )
{
    const fx_f16* a = (const fx_f16*)a_;
    const fx_f16* b = (const fx_f16*)b_;
    fx_f16* c = (fx_f16*)c_;
    const fx_f16* bp = (const fx_f16*)bp_;
#if FX_CONV_NR_F16 == 24 && FX_CONV_MR_F16 == 8
    float16x8_t c00 = vdupq_n_f16((fx_f16)0.f), c01 = c00, c02 = c00;
    float16x8_t c10 = vdupq_n_f16((fx_f16)0.f), c11 = c10, c12 = c10;
    float16x8_t c20 = vdupq_n_f16((fx_f16)0.f), c21 = c20, c22 = c20;
    float16x8_t c30 = vdupq_n_f16((fx_f16)0.f), c31 = c30, c32 = c30;
    float16x8_t c40 = vdupq_n_f16((fx_f16)0.f), c41 = c40, c42 = c40;
    float16x8_t c50 = vdupq_n_f16((fx_f16)0.f), c51 = c50, c52 = c50;
    float16x8_t c60 = vdupq_n_f16((fx_f16)0.f), c61 = c60, c62 = c60;
    float16x8_t c70 = vdupq_n_f16((fx_f16)0.f), c71 = c70, c72 = c70;

    for( int p = 0; p < k; p++, a += FX_CONV_MR_F16, b += FX_CONV_NR_F16 )
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

    float16x8_t vbias;
    #define _FX_ADD_BIAS_F16(row) \
        vbias = vdupq_n_f16(bias[row]); \
        c##row##0 = vaddq_f16(c##row##0, vbias); \
        c##row##1 = vaddq_f16(c##row##1, vbias); \
        c##row##2 = vaddq_f16(c##row##2, vbias)
    _FX_ADD_BIAS_F16(0);
    _FX_ADD_BIAS_F16(1);
    _FX_ADD_BIAS_F16(2);
    _FX_ADD_BIAS_F16(3);
    _FX_ADD_BIAS_F16(4);
    _FX_ADD_BIAS_F16(5);
    _FX_ADD_BIAS_F16(6);
    _FX_ADD_BIAS_F16(7);

    if (bp) {
        #define _FX_ADD_BYPASS_F16(row) \
            c##row##0 = vaddq_f16(c##row##0, vld1q_f16(bp + ldp*row)); \
            c##row##1 = vaddq_f16(c##row##1, vld1q_f16(bp + ldp*row + 8)); \
            c##row##2 = vaddq_f16(c##row##2, vld1q_f16(bp + ldp*row + 16))
        _FX_ADD_BYPASS_F16(0);
        _FX_ADD_BYPASS_F16(1);
        _FX_ADD_BYPASS_F16(2);
        _FX_ADD_BYPASS_F16(3);
        _FX_ADD_BYPASS_F16(4);
        _FX_ADD_BYPASS_F16(5);
        _FX_ADD_BYPASS_F16(6);
        _FX_ADD_BYPASS_F16(7);
    }

    if (activ) {
        float16x8_t valpha = vdupq_n_f16(alpha);
        float16x8_t vmax = vdupq_n_f16(maxval);
        float16x8_t z = vdupq_n_f16(0.f), one = vdupq_n_f16(1.f);

        #define _FX_APPLY_ACTIV_F16(row) \
            c##row##0 = vmulq_f16(vminq_f16(c##row##0, vmax), vbslq_f16(vcltq_f16(c##row##0, z), valpha, one)); \
            c##row##1 = vmulq_f16(vminq_f16(c##row##1, vmax), vbslq_f16(vcltq_f16(c##row##1, z), valpha, one)); \
            c##row##2 = vmulq_f16(vminq_f16(c##row##2, vmax), vbslq_f16(vcltq_f16(c##row##2, z), valpha, one))

        _FX_APPLY_ACTIV_F16(0);
        _FX_APPLY_ACTIV_F16(1);
        _FX_APPLY_ACTIV_F16(2);
        _FX_APPLY_ACTIV_F16(3);
        _FX_APPLY_ACTIV_F16(4);
        _FX_APPLY_ACTIV_F16(5);
        _FX_APPLY_ACTIV_F16(6);
        _FX_APPLY_ACTIV_F16(7);
    }

    #define _FX_STORE_F16(row) \
        vst1q_f16(c + ldc*row, c##row##0); \
        vst1q_f16(c + ldc*row + 8, c##row##1); \
        vst1q_f16(c + ldc*row + 16, c##row##2)

    _FX_STORE_F16(0);
    _FX_STORE_F16(1);
    _FX_STORE_F16(2);
    _FX_STORE_F16(3);
    _FX_STORE_F16(4);
    _FX_STORE_F16(5);
    _FX_STORE_F16(6);
    _FX_STORE_F16(7);
#else
    //#error "unsupported FX_CONV_NR_F16 and/or FX_CONV_MR_F16"
    float cbuf[FX_CONV_MR_F16*FX_CONV_NR_F16];
    for( int i = 0; i < FX_CONV_MR_F16; i++ )
    {
        float beta = bias[i];
        if (pb) {
            for( int j = 0; j < FX_CONV_NR_F16; j++ )
                cbuf[i*FX_CONV_NR_F16 + j] = beta + pb[i*ldp + j];
        } else {
            for( int j = 0; j < FX_CONV_NR_F16; j++ )
                cbuf[i*FX_CONV_NR_F16 + j] = beta;
        }
    }
    for( int p = 0; p < k; p++ )
    {
        for( int i = 0; i < FX_CONV_MR_F16; i++ )
        {
            float ai = a[FX_CONV_MR_F16*p + i];
            for( int j = 0; j < FX_CONV_NR_F16; j++ )
                cbuf[i*FX_CONV_NR_F16+j] += b[FX_CONV_NR_F16*p + j]*ai;
        }
    }
    if (activ) {
        for( int i = 0; i < FX_CONV_MR_F16*FX_CONV_NR_F16; i++ )
        {
            float v = cbuf[i];
            v = v <= maxval ? v : maxval;
            v *= (v < 0.f ? alpha : 1.f);
            cbuf[i] = v;
        }
    }
    for(int i = 0; i < FX_CONV_MR_F16; i++) {
        for(int j = 0; j < FX_CONV_NR_F16; j++)
            c[i*ldc + j] = FX_FLOAT16(cbuf[i*FX_CONV_NR_F16 + j]);
    }
#endif
}

void _fx_conv_update_block_f32( int np, int width, const void* a_, const void* b_, void* c_, int ldc, bool init_c )
{
    const float* a = (const float*)a_;
    const float* b = (const float*)b_;
    float* c = (float*)c_;

#ifdef __ARM_NEON
#if FX_CONV_MR == 4 && FX_CONV_NR == 28
    float32x4_t c00 = vdupq_n_f32(0.f), c01 = c00, c02 = c00, c03 = c00, c04 = c00, c05 = c00, c06 = c00;
    float32x4_t c10 = vdupq_n_f32(0.f), c11 = c10, c12 = c10, c13 = c10, c14 = c10, c15 = c10, c16 = c10;
    float32x4_t c20 = vdupq_n_f32(0.f), c21 = c20, c22 = c20, c23 = c20, c24 = c20, c25 = c20, c26 = c20;
    float32x4_t c30 = vdupq_n_f32(0.f), c31 = c30, c32 = c30, c33 = c30, c34 = c30, c35 = c30, c36 = c30;

    if (width > 12) {
        for( int p = 0; p < np; p++, a += FX_CONV_MR, b += FX_CONV_NR )
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
    } else {
        for( int p = 0; p < np; p++, a += FX_CONV_MR, b += FX_CONV_NR )
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
        }
    }

    if (!init_c) {
        c00 = vaddq_f32(c00, vld1q_f32(c));
        c01 = vaddq_f32(c01, vld1q_f32(c + 4));
        c02 = vaddq_f32(c02, vld1q_f32(c + 8));
        c03 = vaddq_f32(c03, vld1q_f32(c + 12));
        c04 = vaddq_f32(c04, vld1q_f32(c + 16));
        c05 = vaddq_f32(c05, vld1q_f32(c + 20));
        c06 = vaddq_f32(c06, vld1q_f32(c + 24));

        c10 = vaddq_f32(c10, vld1q_f32(c + ldc));
        c11 = vaddq_f32(c11, vld1q_f32(c + ldc + 4));
        c12 = vaddq_f32(c12, vld1q_f32(c + ldc + 8));
        c13 = vaddq_f32(c13, vld1q_f32(c + ldc + 12));
        c14 = vaddq_f32(c14, vld1q_f32(c + ldc + 16));
        c15 = vaddq_f32(c15, vld1q_f32(c + ldc + 20));
        c16 = vaddq_f32(c16, vld1q_f32(c + ldc + 24));

        c20 = vaddq_f32(c20, vld1q_f32(c + ldc*2));
        c21 = vaddq_f32(c21, vld1q_f32(c + ldc*2 + 4));
        c22 = vaddq_f32(c22, vld1q_f32(c + ldc*2 + 8));
        c23 = vaddq_f32(c23, vld1q_f32(c + ldc*2 + 12));
        c24 = vaddq_f32(c24, vld1q_f32(c + ldc*2 + 16));
        c25 = vaddq_f32(c25, vld1q_f32(c + ldc*2 + 20));
        c26 = vaddq_f32(c26, vld1q_f32(c + ldc*2 + 24));

        c30 = vaddq_f32(c30, vld1q_f32(c + ldc*3));
        c31 = vaddq_f32(c31, vld1q_f32(c + ldc*3 + 4));
        c32 = vaddq_f32(c32, vld1q_f32(c + ldc*3 + 8));
        c33 = vaddq_f32(c33, vld1q_f32(c + ldc*3 + 12));
        c34 = vaddq_f32(c34, vld1q_f32(c + ldc*3 + 16));
        c35 = vaddq_f32(c35, vld1q_f32(c + ldc*3 + 20));
        c36 = vaddq_f32(c36, vld1q_f32(c + ldc*3 + 24));
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
#elif defined __AVX2__ && FX_CONV_MR == 4 && FX_CONV_NR == 24
    __m256 c00 = _mm256_setzero_ps(), c01 = c00, c02 = c00;
    __m256 c10 = _mm256_setzero_ps(), c11 = c10, c12 = c10;
    __m256 c20 = _mm256_setzero_ps(), c21 = c20, c22 = c20;
    __m256 c30 = _mm256_setzero_ps(), c31 = c30, c32 = c30;

    for( int p = 0; p < np; p++, a += FX_CONV_MR, b += FX_CONV_NR )
    {
        __m256 b0 = _mm256_loadu_ps(b);
        __m256 b1 = _mm256_loadu_ps(b + 8);
        __m256 b2 = _mm256_loadu_ps(b + 16);
        __m256 a0 = _mm256_broadcast_ss(a);

        c00 = _mm256_fmadd_ps(a0, b0, c00);
        c01 = _mm256_fmadd_ps(a0, b1, c01);
        c02 = _mm256_fmadd_ps(a0, b2, c02);
        a0 = _mm256_broadcast_ss(a + 1);
        c10 = _mm256_fmadd_ps(a0, b0, c10);
        c11 = _mm256_fmadd_ps(a0, b1, c11);
        c12 = _mm256_fmadd_ps(a0, b2, c12);
        a0 = _mm256_broadcast_ss(a + 2);
        c20 = _mm256_fmadd_ps(a0, b0, c20);
        c21 = _mm256_fmadd_ps(a0, b1, c21);
        c22 = _mm256_fmadd_ps(a0, b2, c22);
        a0 = _mm256_broadcast_ss(a + 3);
        c30 = _mm256_fmadd_ps(a0, b0, c30);
        c31 = _mm256_fmadd_ps(a0, b1, c31);
        c32 = _mm256_fmadd_ps(a0, b2, c32);
    }

    if (!init_c) {
        c00 = _mm256_add_ps(c00, _mm256_loadu_ps(c));
        c01 = _mm256_add_ps(c01, _mm256_loadu_ps(c + 8));
        c02 = _mm256_add_ps(c02, _mm256_loadu_ps(c + 16));

        c10 = _mm256_add_ps(c10, _mm256_loadu_ps(c + ldc));
        c11 = _mm256_add_ps(c11, _mm256_loadu_ps(c + ldc + 8));
        c12 = _mm256_add_ps(c12, _mm256_loadu_ps(c + ldc + 16));

        c20 = _mm256_add_ps(c20, _mm256_loadu_ps(c + ldc*2));
        c21 = _mm256_add_ps(c21, _mm256_loadu_ps(c + ldc*2 + 8));
        c22 = _mm256_add_ps(c22, _mm256_loadu_ps(c + ldc*2 + 16));

        c30 = _mm256_add_ps(c30, _mm256_loadu_ps(c + ldc*3));
        c31 = _mm256_add_ps(c31, _mm256_loadu_ps(c + ldc*3 + 8));
        c32 = _mm256_add_ps(c32, _mm256_loadu_ps(c + ldc*3 + 16));
    }

    _mm256_storeu_ps(c, c00);
    _mm256_storeu_ps(c + 8, c01);
    _mm256_storeu_ps(c + 16, c02);

    _mm256_storeu_ps(c + ldc, c10);
    _mm256_storeu_ps(c + ldc + 8, c11);
    _mm256_storeu_ps(c + ldc + 16, c12);

    _mm256_storeu_ps(c + ldc*2, c20);
    _mm256_storeu_ps(c + ldc*2 + 8, c21);
    _mm256_storeu_ps(c + ldc*2 + 16, c22);

    _mm256_storeu_ps(c + ldc*3, c30);
    _mm256_storeu_ps(c + ldc*3 + 8, c31);
    _mm256_storeu_ps(c + ldc*3 + 16, c32);
#else
    float cbuf[FX_CONV_MR*FX_CONV_NR];
    memset(cbuf, 0, sizeof(cbuf));
    for( int p = 0; p < np; p++ )
    {
        for( int i = 0; i < FX_CONV_MR; i++ )
        {
            float ai = a[FX_CONV_MR*p + i];
            for( int j = 0; j < FX_CONV_NR; j++ )
                cbuf[i*FX_CONV_NR+j] += b[FX_CONV_NR*p + j]*ai;
        }
    }
    if (!init_c) {
        for(int i = 0; i < FX_CONV_MR; i++) {
            for(int j = 0; j < FX_CONV_NR; j++)
                c[i*ldc + j] += cbuf[i*FX_CONV_NR + j];
        }
    } else {
        for(int i = 0; i < FX_CONV_MR; i++) {
            for(int j = 0; j < FX_CONV_NR; j++)
                c[i*ldc + j] = cbuf[i*FX_CONV_NR + j];
        }
    }
#endif
}

#if _FX_NN_ENABLE_FP16
void _fx_conv_update_block_f16( int np, int width, const void* a_, const void* b_, void* c_, int ldc, bool init_c )
{
    const fx_f16* a = (const fx_f16*)a_;
    const fx_f16* b = (const fx_f16*)b_;
    fx_f16* c = (fx_f16*)c_;
#if FX_CONV_NR_F16 == 24 && FX_CONV_MR_F16 == 8
    float16x8_t c00 = vdupq_n_f16((fx_f16)0.f), c01 = c00, c02 = c00;
    float16x8_t c10 = vdupq_n_f16((fx_f16)0.f), c11 = c10, c12 = c10;
    float16x8_t c20 = vdupq_n_f16((fx_f16)0.f), c21 = c20, c22 = c20;
    float16x8_t c30 = vdupq_n_f16((fx_f16)0.f), c31 = c30, c32 = c30;
    float16x8_t c40 = vdupq_n_f16((fx_f16)0.f), c41 = c40, c42 = c40;
    float16x8_t c50 = vdupq_n_f16((fx_f16)0.f), c51 = c50, c52 = c50;
    float16x8_t c60 = vdupq_n_f16((fx_f16)0.f), c61 = c60, c62 = c60;
    float16x8_t c70 = vdupq_n_f16((fx_f16)0.f), c71 = c70, c72 = c70;

    if (width > 16) {
        for( int p = 0; p < np; p++, a += FX_CONV_MR_F16, b += FX_CONV_NR_F16 )
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
    } else if (width > 8) {
        for( int p = 0; p < np; p++, a += FX_CONV_MR_F16, b += FX_CONV_NR_F16 )
        {
            float16x8_t a0 = vld1q_f16(a);
            float16x8_t b0 = vld1q_f16(b), b1 = vld1q_f16(b + 8);

            c00 = vfmaq_laneq_f16(c00, b0, a0, 0);
            c01 = vfmaq_laneq_f16(c01, b1, a0, 0);

            c10 = vfmaq_laneq_f16(c10, b0, a0, 1);
            c11 = vfmaq_laneq_f16(c11, b1, a0, 1);

            c20 = vfmaq_laneq_f16(c20, b0, a0, 2);
            c21 = vfmaq_laneq_f16(c21, b1, a0, 2);

            c30 = vfmaq_laneq_f16(c30, b0, a0, 3);
            c31 = vfmaq_laneq_f16(c31, b1, a0, 3);

            c40 = vfmaq_laneq_f16(c40, b0, a0, 4);
            c41 = vfmaq_laneq_f16(c41, b1, a0, 4);

            c50 = vfmaq_laneq_f16(c50, b0, a0, 5);
            c51 = vfmaq_laneq_f16(c51, b1, a0, 5);

            c60 = vfmaq_laneq_f16(c60, b0, a0, 6);
            c61 = vfmaq_laneq_f16(c61, b1, a0, 6);

            c70 = vfmaq_laneq_f16(c70, b0, a0, 7);
            c71 = vfmaq_laneq_f16(c71, b1, a0, 7);
        }
    } else {
        for( int p = 0; p < np; p++, a += FX_CONV_MR_F16, b += FX_CONV_NR_F16 )
        {
            float16x8_t a0 = vld1q_f16(a);
            float16x8_t b0 = vld1q_f16(b);

            c00 = vfmaq_laneq_f16(c00, b0, a0, 0);
            c10 = vfmaq_laneq_f16(c10, b0, a0, 1);
            c20 = vfmaq_laneq_f16(c20, b0, a0, 2);
            c30 = vfmaq_laneq_f16(c30, b0, a0, 3);
            c40 = vfmaq_laneq_f16(c40, b0, a0, 4);
            c50 = vfmaq_laneq_f16(c50, b0, a0, 5);
            c60 = vfmaq_laneq_f16(c60, b0, a0, 6);
            c70 = vfmaq_laneq_f16(c70, b0, a0, 7);
        }
    }

    if (!init_c) {
    #undef _FX_UPDATE_CBUF_ROW
    #define _FX_UPDATE_CBUF_ROW(row) \
        c##row##0 = vaddq_f16(c##row##0, vld1q_f16(c + row*ldc)); \
        c##row##1 = vaddq_f16(c##row##1, vld1q_f16(c + row*ldc + 8)); \
        c##row##2 = vaddq_f16(c##row##2, vld1q_f16(c + row*ldc + 16))

        _FX_UPDATE_CBUF_ROW(0);
        _FX_UPDATE_CBUF_ROW(1);
        _FX_UPDATE_CBUF_ROW(2);
        _FX_UPDATE_CBUF_ROW(3);
        _FX_UPDATE_CBUF_ROW(4);
        _FX_UPDATE_CBUF_ROW(5);
        _FX_UPDATE_CBUF_ROW(6);
        _FX_UPDATE_CBUF_ROW(7);
    }

    #undef _FX_STORE_CBUF_ROW
    #define _FX_STORE_CBUF_ROW(row) \
        vst1q_f16(c + row*ldc, c##row##0); \
        vst1q_f16(c + row*ldc + 8, c##row##1); \
        vst1q_f16(c + row*ldc + 16, c##row##2)

    _FX_STORE_CBUF_ROW(0);
    _FX_STORE_CBUF_ROW(1);
    _FX_STORE_CBUF_ROW(2);
    _FX_STORE_CBUF_ROW(3);
    _FX_STORE_CBUF_ROW(4);
    _FX_STORE_CBUF_ROW(5);
    _FX_STORE_CBUF_ROW(6);
    _FX_STORE_CBUF_ROW(7);
#else
    float cbuf[FX_CONV_MR_F16*FX_CONV_NR_F16];
    memset(cbuf, 0, sizeof(cbuf));
    for( int p = 0; p < np; p++ )
    {
        for( int i = 0; i < FX_CONV_MR_F16; i++ )
        {
            float ai = FX_FLOAT(a[FX_CONV_MR_F16*p + i]);
            for( int j = 0; j < FX_CONV_NR_F16; j++ )
                cbuf[i*FX_CONV_NR_F16+j] += FX_FLOAT(b[FX_CONV_NR_F16*p + j])*ai;
        }
    }
    if (!init_c) {
        for(int i = 0; i < FX_CONV_MR_F16; i++) {
            for(int j = 0; j < FX_CONV_NR_F16; j++)
                c[i*ldc + j] = FX_FLOAT16(FX_FLOAT(c[i*ldc + j]) + cbuf[i*FX_CONV_NR_F16 + j]);
        }
    } else {
        for(int i = 0; i < FX_CONV_MR_F16; i++) {
            for(int j = 0; j < FX_CONV_NR_F16; j++)
                c[i*ldc + j] = FX_FLOAT16(cbuf[i*FX_CONV_NR_F16 + j]);
        }
    }
#endif
}
#endif

void _fx_conv_update_block_u8( int np, const void* a_, const void* b_,
                               void* c_, int ldc, bool init_c )
{
    const uint8_t* a = (const uint8_t*)a_;
    const uint8_t* b = (const uint8_t*)b_;
    uint32_t* c = (uint32_t*)c_;

#if FX_QCONV_C != 4
    #error "unsupported FX_QCONV_C (must be 4)"
#endif

#ifdef __ARM_NEON
#if FX_QCONV_MR == 4 && FX_QCONV_NR == 28
    uint32x4_t c00 = vdupq_n_u32(0), c01 = c00, c02 = c00, c03 = c00, c04 = c00, c05 = c00, c06 = c00;
    uint32x4_t c10 = vdupq_n_u32(0), c11 = c10, c12 = c10, c13 = c10, c14 = c10, c15 = c10, c16 = c10;
    uint32x4_t c20 = vdupq_n_u32(0), c21 = c20, c22 = c20, c23 = c20, c24 = c20, c25 = c20, c26 = c20;
    uint32x4_t c30 = vdupq_n_u32(0), c31 = c30, c32 = c30, c33 = c30, c34 = c30, c35 = c30, c36 = c30;

    for( int p = 0; p < np; p += FX_QCONV_C,
                            a += FX_QCONV_C*FX_QCONV_C,
                            b += FX_QCONV_NR*FX_QCONV_C )
    {
        uint8x16_t a0 = vld1q_u8(a), b0, b1, b2;
        b0 = vld1q_u8(b); b1 = vld1q_u8(b + 16); b2 = vld1q_u8(b + 32);

        c00 = vdotq_laneq_u32(c00, b0, a0, 0);
        c01 = vdotq_laneq_u32(c01, b1, a0, 0);
        c02 = vdotq_laneq_u32(c02, b2, a0, 0);
        c10 = vdotq_laneq_u32(c10, b0, a0, 1);
        c11 = vdotq_laneq_u32(c11, b1, a0, 1);
        c12 = vdotq_laneq_u32(c12, b2, a0, 1);
        c20 = vdotq_laneq_u32(c20, b0, a0, 2);
        c21 = vdotq_laneq_u32(c21, b1, a0, 2);
        c22 = vdotq_laneq_u32(c22, b2, a0, 2);
        c30 = vdotq_laneq_u32(c30, b0, a0, 3);
        c31 = vdotq_laneq_u32(c31, b1, a0, 3);
        c32 = vdotq_laneq_u32(c32, b2, a0, 3);

        b0 = vld1q_u8(b + 48); b1 = vld1q_u8(b + 64); b2 = vld1q_u8(b + 80);

        c03 = vdotq_laneq_u32(c03, b0, a0, 0);
        c04 = vdotq_laneq_u32(c04, b1, a0, 0);
        c05 = vdotq_laneq_u32(c05, b2, a0, 0);
        c13 = vdotq_laneq_u32(c13, b0, a0, 1);
        c14 = vdotq_laneq_u32(c14, b1, a0, 1);
        c15 = vdotq_laneq_u32(c15, b2, a0, 1);
        c23 = vdotq_laneq_u32(c23, b0, a0, 2);
        c24 = vdotq_laneq_u32(c24, b1, a0, 2);
        c25 = vdotq_laneq_u32(c25, b2, a0, 2);
        c33 = vdotq_laneq_u32(c33, b0, a0, 3);
        c34 = vdotq_laneq_u32(c34, b1, a0, 3);
        c35 = vdotq_laneq_u32(c35, b2, a0, 3);

        b0 = vld1q_u8(b + 96);
        c06 = vdotq_laneq_u32(c06, b0, a0, 0);
        c16 = vdotq_laneq_u32(c16, b0, a0, 1);
        c26 = vdotq_laneq_u32(c26, b0, a0, 2);
        c36 = vdotq_laneq_u32(c36, b0, a0, 3);
    }

    if (!init_c) {
    #undef _FX_UPDATE_QCONV_BLOCK
    #define _FX_UPDATE_QCONV_BLOCK(i) \
        c##i##0 = vaddq_u32(c##i##0, vld1q_u32(c+i*ldc)); \
        c##i##1 = vaddq_u32(c##i##1, vld1q_u32(c+i*ldc+4)); \
        c##i##2 = vaddq_u32(c##i##2, vld1q_u32(c+i*ldc+8)); \
        c##i##3 = vaddq_u32(c##i##3, vld1q_u32(c+i*ldc+12)); \
        c##i##4 = vaddq_u32(c##i##4, vld1q_u32(c+i*ldc+16)); \
        c##i##5 = vaddq_u32(c##i##5, vld1q_u32(c+i*ldc+20)); \
        c##i##6 = vaddq_u32(c##i##6, vld1q_u32(c+i*ldc+24))

        _FX_UPDATE_QCONV_BLOCK(0);
        _FX_UPDATE_QCONV_BLOCK(1);
        _FX_UPDATE_QCONV_BLOCK(2);
        _FX_UPDATE_QCONV_BLOCK(3);
    }

    #undef _FX_STORE_QCONV_BLOCK
    #define _FX_STORE_QCONV_BLOCK(i) \
        vst1q_u32(c+i*ldc, c##i##0); \
        vst1q_u32(c+i*ldc+4, c##i##1); \
        vst1q_u32(c+i*ldc+8, c##i##2); \
        vst1q_u32(c+i*ldc+12, c##i##3); \
        vst1q_u32(c+i*ldc+16, c##i##4); \
        vst1q_u32(c+i*ldc+20, c##i##5); \
        vst1q_u32(c+i*ldc+24, c##i##6)


    _FX_STORE_QCONV_BLOCK(0);
    _FX_STORE_QCONV_BLOCK(1);
    _FX_STORE_QCONV_BLOCK(2);
    _FX_STORE_QCONV_BLOCK(3);
#else
#error "unsupported FX_QCONV_MR and/or FX_QCONV_NR"
#endif
#else
    uint32_t cbuf[FX_QCONV_C*FX_QCONV_NR];
    memset(cbuf, 0, sizeof(cbuf));
    for( int p = 0; p < np; p += FX_QCONV_C,
            a += FX_QCONV_C*FX_QCONV_C,
            b += FX_QCONV_C*FX_QCONV_NR )
    {
        for( int i = 0; i < FX_QCONV_C; i++ )
        {
            uint8_t ai0 = a[i*FX_QCONV_C];
            uint8_t ai1 = a[i*FX_QCONV_C + 1];
            uint8_t ai2 = a[i*FX_QCONV_C + 2];
            uint8_t ai3 = a[i*FX_QCONV_C + 3];
            for( int j = 0; j < FX_QCONV_NR; j++ )
                cbuf[i*FX_QCONV_NR + j] += b[j*FX_QCONV_C]*ai0 +
                                           b[j*FX_QCONV_C + 1]*ai1 +
                                           b[j*FX_QCONV_C + 2]*ai2 +
                                           b[j*FX_QCONV_C + 3]*ai3;
        }
    }
    if (!init_c) {
        for(int i = 0; i < FX_QCONV_C; i++) {
            for(int j = 0; j < FX_QCONV_NR; j++)
                c[i*ldc + j] += cbuf[i*FX_QCONV_NR + j];
        }
    } else {
        for(int i = 0; i < FX_QCONV_C; i++) {
            for(int j = 0; j < FX_QCONV_NR; j++)
                c[i*ldc + j] = cbuf[i*FX_QCONV_NR + j];
        }
    }
#endif
}
}
