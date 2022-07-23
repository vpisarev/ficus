/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Winograd-based convolution.
// The header is not intented to be used alone.
// It is assumed to be included into OpConv.fx

#ifdef __ARM_NEON
static void _fx_winograd_accum(const float* inp, const float* ww,
                               float* out, int Cg, int Kg)
{
#ifdef __ARM_NEON
    for (int k = 0; k < Kg; k++) {
        float32x4_t s0 = vdupq_n_f32(0.f), s1 = s0, s2 = s0, s3 = s0;
        float32x4_t s4 = s0, s5 = s0, s6 = s0, s7 = s0;
        float32x4_t s8 = s0, s9 = s0, s10 = s0, s11 = s0;
        float32x4_t s12 = s0, s13 = s0, s14 = s0, s15 = s0;
        for(int c = 0; c < Cg; c++) {
            const float* inp_c = inp + c*_FX_WINO_AREA;
            const float* ww_kc = ww + (k*Cg + c)*_FX_WINO_AREA;
            float32x4_t x0, x1, x2, x3, w0, w1, w2, w3;
            x0 = vld1q_f32(inp_c); x1 = vld1q_f32(inp_c + 4);
            x2 = vld1q_f32(inp_c + 8); x3 = vld1q_f32(inp_c + 12);
            w0 = vld1q_f32(ww_kc); w1 = vld1q_f32(ww_kc + 4);
            w2 = vld1q_f32(ww_kc + 8); w3 = vld1q_f32(ww_kc + 12);
            s0 = vfmaq_f32(s0, x0, w0); s1 = vfmaq_f32(s1, x1, w1);
            s2 = vfmaq_f32(s2, x2, w2); s3 = vfmaq_f32(s3, x3, w3);

            x0 = vld1q_f32(inp_c + 16); x1 = vld1q_f32(inp_c + 20);
            x2 = vld1q_f32(inp_c + 24); x3 = vld1q_f32(inp_c + 28);
            w0 = vld1q_f32(ww_kc + 16); w1 = vld1q_f32(ww_kc + 20);
            w2 = vld1q_f32(ww_kc + 24); w3 = vld1q_f32(ww_kc + 28);
            s4 = vfmaq_f32(s4, x0, w0); s5 = vfmaq_f32(s5, x1, w1);
            s6 = vfmaq_f32(s6, x2, w2); s7 = vfmaq_f32(s7, x3, w3);

            x0 = vld1q_f32(inp_c + 32); x1 = vld1q_f32(inp_c + 36);
            x2 = vld1q_f32(inp_c + 40); x3 = vld1q_f32(inp_c + 44);
            w0 = vld1q_f32(ww_kc + 32); w1 = vld1q_f32(ww_kc + 36);
            w2 = vld1q_f32(ww_kc + 40); w3 = vld1q_f32(ww_kc + 44);
            s8 = vfmaq_f32(s8, x0, w0); s9 = vfmaq_f32(s9, x1, w1);
            s10 = vfmaq_f32(s10, x2, w2); s11 = vfmaq_f32(s11, x3, w3);

            x0 = vld1q_f32(inp_c + 48); x1 = vld1q_f32(inp_c + 52);
            x2 = vld1q_f32(inp_c + 56); x3 = vld1q_f32(inp_c + 60);
            w0 = vld1q_f32(ww_kc + 48); w1 = vld1q_f32(ww_kc + 52);
            w2 = vld1q_f32(ww_kc + 56); w3 = vld1q_f32(ww_kc + 60);
            s12 = vfmaq_f32(s12, x0, w0); s13 = vfmaq_f32(s13, x1, w1);
            s14 = vfmaq_f32(s14, x2, w2); s15 = vfmaq_f32(s15, x3, w3);
        }
        float* out_k = out + k*_FX_WINO_AREA;
        vst1q_f32(out_k, s0); vst1q_f32(out_k+4, s1);
        vst1q_f32(out_k+8, s2); vst1q_f32(out_k+12, s3);
        vst1q_f32(out_k+16, s4); vst1q_f32(out_k+20, s5);
        vst1q_f32(out_k+24, s6); vst1q_f32(out_k+28, s7);
        vst1q_f32(out_k+32, s8); vst1q_f32(out_k+36, s9);
        vst1q_f32(out_k+40, s10); vst1q_f32(out_k+44, s11);
        vst1q_f32(out_k+48, s12); vst1q_f32(out_k+52, s13);
        vst1q_f32(out_k+56, s14); vst1q_f32(out_k+60, s15);
    }
#elif 0
    float tout[_FX_WINO_AREA];
    for (int k = 0; k < Kg; k++) {
        memset(tout, 0, sizeof(tout));
        for(int c = 0; c < Cg; c++) {
            const float* inp_c = inp + c*_FX_WINO_AREA;
            const float* ww_kc = ww + (k*Cg + c)*_FX_WINO_AREA;
            for(int j = 0; j < _FX_WINO_AREA; j++)
                tout[j] += inp_c[j]*ww_kc[j];
        }
        memcpy(out + k*_FX_WINO_AREA, tout, sizeof(tout));
    }
#else
    float tout[_FX_WINO_AREA];
    for (int k = 0; k < Kg; k++) {
        memset(tout, 0, sizeof(tout));
        for(int c = 0; c < 16; c++) {
            const float* inp_c = inp + c*_FX_WINO_AREA;
            const float* ww_kc = ww + (k*Cg + c)*_FX_WINO_AREA;
            for(int j = 0; j < _FX_WINO_AREA; j++)
                tout[j] += inp_c[j]*ww_kc[j];
        }
        memcpy(out + k*_FX_WINO_AREA, tout, sizeof(tout));
    }
#endif
}

#undef _FX_CONV_T4x4
#define _FX_CONV_T4x4(a, b, c, d, tr0, tr1) \
    tr0 = vtrnq_f32(a, b); \
    tr1 = vtrnq_f32(c, d); \
    a = vcombine_f32(vget_low_f32(tr0.val[0]), vget_low_f32(tr1.val[0])); \
    b = vcombine_f32(vget_low_f32(tr0.val[1]), vget_low_f32(tr1.val[1])); \
    c = vcombine_f32(vget_high_f32(tr0.val[0]), vget_high_f32(tr1.val[0])); \
    d = vcombine_f32(vget_high_f32(tr0.val[1]), vget_high_f32(tr1.val[1]))

static void _fx_conv_winograd_BtXB_8x8(const float* inptr, int inpstep,
                                       float* outptr, int outstep)
{
    // out = Bt*inp*B
    /*const float Bt[] = {
        1.f, 0.f, -5.25f, 0.f, 5.25f, 0.f, -1.f, 0.f,
        0.f, 1.f, 1.f, -4.25f, -4.25f, 1.f, 1.f, 0.f,
        0.f, -1.f, 1.f, 4.25f, -4.25f, -1.f, 1.f, 0.f,
        0.f, 0.5f, 0.25f, -2.5f, -1.25f, 2.f, 1.f, 0.f,
        0.f, -0.5f, 0.25f, 2.5f, -1.25f, -2.f, 1.f, 0.f,
        0.f, 2.f, 4.f, -2.5f, -5.f, 0.5f, 1.f, 0.f,
        0.f, -2.f, 4.f, 2.5f, -5.f, -0.5f, 1.f, 0.f,
        0.f, -1.f, 0.f, 5.25f, 0.f, -5.25f, 0.f, 1.f
    };*/
    float32x4_t x00 = vld1q_f32(inptr), x01 = vld1q_f32(inptr + 4);
    float32x4_t x10 = vld1q_f32(inptr + inpstep), x11 = vld1q_f32(inptr + inpstep + 4);
    float32x4_t x20 = vld1q_f32(inptr + inpstep*2), x21 = vld1q_f32(inptr + inpstep*2 + 4);
    float32x4_t x30 = vld1q_f32(inptr + inpstep*3), x31 = vld1q_f32(inptr + inpstep*3 + 4);
    float32x4_t x40 = vld1q_f32(inptr + inpstep*4), x41 = vld1q_f32(inptr + inpstep*4 + 4);
    float32x4_t x50 = vld1q_f32(inptr + inpstep*5), x51 = vld1q_f32(inptr + inpstep*5 + 4);
    float32x4_t x60 = vld1q_f32(inptr + inpstep*6), x61 = vld1q_f32(inptr + inpstep*6 + 4);
    float32x4_t x70 = vld1q_f32(inptr + inpstep*7), x71 = vld1q_f32(inptr + inpstep*7 + 4);

    // Y[0] = [1.f, 0.f, -5.25f, 0.f, 5.25f, 0.f, -1.f, 0.f]*X
    // Y[7] = [0.f, -1.f, 0.f, 5.25f, 0.f, -5.25f, 0.f, 1.f]*X
    float32x4_t q5_25 = vdupq_n_f32(5.25f), t00, t01, t10, t11;
    t00 = vsubq_f32(x40, x20);
    t01 = vsubq_f32(x41, x21);
    t10 = vsubq_f32(x30, x50);
    t11 = vsubq_f32(x31, x51);
    float32x4_t y00 = vfmaq_f32(vsubq_f32(x00, x60), t00, q5_25);
    float32x4_t y01 = vfmaq_f32(vsubq_f32(x01, x61), t01, q5_25);
    float32x4_t y70 = vfmaq_f32(vsubq_f32(x70, x10), t10, q5_25);
    float32x4_t y71 = vfmaq_f32(vsubq_f32(x71, x11), t11, q5_25);

    // Y[1] = [0.f, 1.f, 1.f, -4.25f, -4.25f, 1.f, 1.f, 0.f]*X
    // Y[2] = [0.f, -1.f, 1.f, 4.25f, -4.25f, -1.f, 1.f, 0.f]*X
    float32x4_t qm4_25 = vdupq_n_f32(-4.25f);
    t00 = vfmaq_f32(vaddq_f32(x10, x50), x30, qm4_25);
    t01 = vfmaq_f32(vaddq_f32(x11, x51), x31, qm4_25);
    t10 = vfmaq_f32(vaddq_f32(x20, x60), x40, qm4_25);
    t11 = vfmaq_f32(vaddq_f32(x21, x61), x41, qm4_25);

    float32x4_t y10 = vaddq_f32(t00, t10), y11 = vaddq_f32(t01, t11);
    float32x4_t y20 = vsubq_f32(t10, t00), y21 = vsubq_f32(t11, t01);

    // Y[3] = [0.f, 0.5f, 0.25f, -2.5f, -1.25f, 2.f, 1.f, 0.f]*X
    // Y[4] = [0.f, -0.5f, 0.25f, 2.5f, -1.25f, -2.f, 1.f, 0.f]*X
    float32x4_t q0_5 = vdupq_n_f32(0.5f), q0_25 = vdupq_n_f32(0.25f);
    float32x4_t qm2_5 = vdupq_n_f32(-2.5f), qm1_25 = vdupq_n_f32(-1.25f);
    t00 = vfmaq_f32(vaddq_f32(x50, x50), x10, q0_5);
    t01 = vfmaq_f32(vaddq_f32(x51, x51), x11, q0_5);
    t10 = vfmaq_f32(x60, x20, q0_25);
    t11 = vfmaq_f32(x61, x21, q0_25);
    t00 = vfmaq_f32(t00, x30, qm2_5);
    t01 = vfmaq_f32(t01, x31, qm2_5);
    t10 = vfmaq_f32(t10, x40, qm1_25);
    t11 = vfmaq_f32(t11, x41, qm1_25);

    float32x4_t y30 = vaddq_f32(t00, t10), y31 = vaddq_f32(t01, t11);
    float32x4_t y40 = vsubq_f32(t10, t00), y41 = vsubq_f32(t11, t01);

    // Y[5] = [0.f, 2.f, 4.f, -2.5f, -5.f, 0.5f, 1.f, 0.f]*X
    // Y[6] = [0.f, -2.f, 4.f, 2.5f, -5.f, -0.5f, 1.f, 0.f]*X
    float32x4_t q4 = vdupq_n_f32(4.f), qm5 = vdupq_n_f32(-5.f);
    t00 = vfmaq_f32(vaddq_f32(x10, x10), x50, q0_5);
    t01 = vfmaq_f32(vaddq_f32(x11, x11), x51, q0_5);
    t10 = vfmaq_f32(x60, x20, q4);
    t11 = vfmaq_f32(x61, x21, q4);
    t00 = vfmaq_f32(t00, x30, qm2_5);
    t01 = vfmaq_f32(t01, x31, qm2_5);
    t10 = vfmaq_f32(t10, x40, qm5);
    t11 = vfmaq_f32(t11, x41, qm5);

    float32x4_t y50 = vaddq_f32(t00, t10), y51 = vaddq_f32(t01, t11);
    float32x4_t y60 = vsubq_f32(t10, t00), y61 = vsubq_f32(t11, t01);

    /* transpose 8x8 matrix in-place with some renumeration of the elements:
       Y:
            y00 y01
            y10 y11
            ...
            y70 y71
       Y':
            y00 y40
            y10 y50
            y20 y60
            y30 y70
            y01 y41
            y11 y51
            y21 y61
            y31 y71
        in other words, y40 <-> y01, y50 <-> y11, y60 <-> y21, y70 <-> y31
    */
    float32x4x2_t tr0, tr1;

    _FX_CONV_T4x4(y00, y10, y20, y30, tr0, tr1);
    _FX_CONV_T4x4(y01, y11, y21, y31, tr0, tr1);
    _FX_CONV_T4x4(y40, y50, y60, y70, tr0, tr1);
    _FX_CONV_T4x4(y41, y51, y61, y71, tr0, tr1);

    // Z[0] = [1.f, 0.f, -5.25f, 0.f, 5.25f, 0.f, -1.f, 0.f]*Y
    // Z[7] = [0.f, -1.f, 0.f, 5.25f, 0.f, -5.25f, 0.f, 1.f]*Y
    t00 = vsubq_f32(y01, y20);
    t01 = vsubq_f32(y41, y60);
    t10 = vsubq_f32(y30, y11);
    t11 = vsubq_f32(y70, y51);
    float32x4_t z00 = vfmaq_f32(vsubq_f32(y00, y21), t00, q5_25);
    float32x4_t z01 = vfmaq_f32(vsubq_f32(y40, y61), t01, q5_25);
    float32x4_t z70 = vfmaq_f32(vsubq_f32(y31, y10), t10, q5_25);
    float32x4_t z71 = vfmaq_f32(vsubq_f32(y71, y50), t11, q5_25);

    // Z[1] = [0.f, 1.f, 1.f, -4.25f, -4.25f, 1.f, 1.f, 0.f]*Y
    // Z[2] = [0.f, -1.f, 1.f, 4.25f, -4.25f, -1.f, 1.f, 0.f]*Y
    t00 = vfmaq_f32(vaddq_f32(y10, y11), y30, qm4_25);
    t01 = vfmaq_f32(vaddq_f32(y50, y51), y70, qm4_25);
    t10 = vfmaq_f32(vaddq_f32(y20, y21), y01, qm4_25);
    t11 = vfmaq_f32(vaddq_f32(y60, y61), y41, qm4_25);

    float32x4_t z10 = vaddq_f32(t00, t10), z11 = vaddq_f32(t01, t11);
    float32x4_t z20 = vsubq_f32(t10, t00), z21 = vsubq_f32(t11, t01);

    // Z[3] = [0.f, 0.5f, 0.25f, -2.5f, -1.25f, 2.f, 1.f, 0.f]*Y
    // Z[4] = [0.f, -0.5f, 0.25f, 2.5f, -1.25f, -2.f, 1.f, 0.f]*Y
    t00 = vfmaq_f32(vaddq_f32(y11, y11), y10, q0_5);
    t01 = vfmaq_f32(vaddq_f32(y51, y51), y50, q0_5);
    t10 = vfmaq_f32(y21, y20, q0_25);
    t11 = vfmaq_f32(y61, y60, q0_25);
    t00 = vfmaq_f32(t00, y30, qm2_5);
    t01 = vfmaq_f32(t01, y70, qm2_5);
    t10 = vfmaq_f32(t10, y01, qm1_25);
    t11 = vfmaq_f32(t11, y41, qm1_25);

    float32x4_t z30 = vaddq_f32(t00, t10), z31 = vaddq_f32(t01, t11);
    float32x4_t z40 = vsubq_f32(t10, t00), z41 = vsubq_f32(t11, t01);

    // Z[5] = [0.f, 2.f, 4.f, -2.5f, -5.f, 0.5f, 1.f, 0.f]*Y
    // Z[6] = [0.f, -2.f, 4.f, 2.5f, -5.f, -0.5f, 1.f, 0.f]*Y
    t00 = vfmaq_f32(vaddq_f32(y10, y10), y11, q0_5);
    t01 = vfmaq_f32(vaddq_f32(y50, y50), y51, q0_5);
    t10 = vfmaq_f32(y21, y20, q4);
    t11 = vfmaq_f32(y61, y60, q4);
    t00 = vfmaq_f32(t00, y30, qm2_5);
    t01 = vfmaq_f32(t01, y70, qm2_5);
    t10 = vfmaq_f32(t10, y01, qm5);
    t11 = vfmaq_f32(t11, y41, qm5);

    float32x4_t z50 = vaddq_f32(t00, t10), z51 = vaddq_f32(t01, t11);
    float32x4_t z60 = vsubq_f32(t10, t00), z61 = vsubq_f32(t11, t01);

    vst1q_f32(outptr, z00);
    vst1q_f32(outptr + 4, z01);
    vst1q_f32(outptr + outstep, z10);
    vst1q_f32(outptr + outstep + 4, z11);
    vst1q_f32(outptr + outstep*2, z20);
    vst1q_f32(outptr + outstep*2 + 4, z21);
    vst1q_f32(outptr + outstep*3, z30);
    vst1q_f32(outptr + outstep*3 + 4, z31);
    vst1q_f32(outptr + outstep*4, z40);
    vst1q_f32(outptr + outstep*4 + 4, z41);
    vst1q_f32(outptr + outstep*5, z50);
    vst1q_f32(outptr + outstep*5 + 4, z51);
    vst1q_f32(outptr + outstep*6, z60);
    vst1q_f32(outptr + outstep*6 + 4, z61);
    vst1q_f32(outptr + outstep*7, z70);
    vst1q_f32(outptr + outstep*7 + 4, z71);
}

static void _fx_conv_winograd_AtXA_8x8(const float* inptr, int inpstep,
                                       float* outptr, int outstep, const float* pbptr,
                                       float bias, bool activ, float minval, float maxval)
{
    /*const float At[] = {
     1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 0.f,
     0.f, 1.f, -1.f, 2.f, -2.f, 0.5f, -0.5f, 0.f,
     0.f, 1.f, 1.f, 4.f, 4.f, 0.25f, 0.25f, 0.f,
     0.f, 1.f, -1.f, 8.f, -8.f, 0.125f, -0.125f, 0.f,
     0.f, 1.f, 1.f, 16.f, 16.f, 1.f/16, 1.f/16, 0.f,
     0.f, 1.f, -1.f, 32.f, -32.f, 1.f/32, -1.f/32, 1.f
    };*/
    float32x4_t x00 = vld1q_f32(inptr), x01 = vld1q_f32(inptr + 4);
    float32x4_t x10 = vld1q_f32(inptr + inpstep), x11 = vld1q_f32(inptr + inpstep + 4);
    float32x4_t x20 = vld1q_f32(inptr + inpstep*2), x21 = vld1q_f32(inptr + inpstep*2 + 4);
    float32x4_t x30 = vld1q_f32(inptr + inpstep*3), x31 = vld1q_f32(inptr + inpstep*3 + 4);
    float32x4_t x40 = vld1q_f32(inptr + inpstep*4), x41 = vld1q_f32(inptr + inpstep*4 + 4);
    float32x4_t x50 = vld1q_f32(inptr + inpstep*5), x51 = vld1q_f32(inptr + inpstep*5 + 4);
    float32x4_t x60 = vld1q_f32(inptr + inpstep*6), x61 = vld1q_f32(inptr + inpstep*6 + 4);
    float32x4_t x70 = vld1q_f32(inptr + inpstep*7), x71 = vld1q_f32(inptr + inpstep*7 + 4);

    float32x4_t s12_0, s12_1, s34_0, s34_1, s56_0, s56_1;
    s12_0 = vaddq_f32(x10, x20); s12_1 = vaddq_f32(x11, x21);
    s34_0 = vaddq_f32(x30, x40); s34_1 = vaddq_f32(x31, x41);
    s56_0 = vaddq_f32(x50, x60); s56_1 = vaddq_f32(x51, x61);

    float32x4_t y00 = vaddq_f32(vaddq_f32(vaddq_f32(x00, s12_0), s34_0), s56_0);
    float32x4_t y01 = vaddq_f32(vaddq_f32(vaddq_f32(x01, s12_1), s34_1), s56_1);
    float32x4_t y20 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 4.0f), s56_0, 0.25f);
    float32x4_t y21 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 4.0f), s56_1, 0.25f);
    float32x4_t y40 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 16.0f), s56_0, 1.f/16);
    float32x4_t y41 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 16.0f), s56_1, 1.f/16);

    s12_0 = vsubq_f32(x10, x20); s12_1 = vsubq_f32(x11, x21);
    s34_0 = vsubq_f32(x30, x40); s34_1 = vsubq_f32(x31, x41);
    s56_0 = vsubq_f32(x50, x60); s56_1 = vsubq_f32(x51, x61);

    float32x4_t y50 = vfmaq_n_f32(vfmaq_n_f32(vaddq_f32(x70, s12_0),
                                  s34_0, 32.f), s56_0, 1.f/32);
    float32x4_t y51 = vfmaq_n_f32(vfmaq_n_f32(vaddq_f32(x71, s12_1),
                                  s34_1, 32.f), s56_1, 1.f/32);
    float32x4_t y10 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 2.0f), s56_0, 0.5f);
    float32x4_t y11 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 2.0f), s56_1, 0.5f);
    float32x4_t y30 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 8.0f), s56_0, 0.125f);
    float32x4_t y31 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 8.0f), s56_1, 0.125f);
    float32x4_t y60 = vdupq_n_f32(0.f), y61 = y60, y70 = y60, y71 = y60;

    /* transpose 8x8 matrix in-place with some renumeration of the elements:
       Y:
            y00 y01
            y10 y11
            ...
            y50 y51
            0   0
            0   0
       Y':
            y00 y40
            y10 y50
            y20 y60
            y30 y70
            y01 y41
            y11 y51
            y21 y61
            y31 y71
        in other words, y40 <-> y01, y50 <-> y11, y60 <-> y21, y70 <-> y31
    */
    float32x4x2_t tr0, tr1;

    _FX_CONV_T4x4(y00, y10, y20, y30, tr0, tr1);
    _FX_CONV_T4x4(y01, y11, y21, y31, tr0, tr1);
    _FX_CONV_T4x4(y40, y50, y60, y70, tr0, tr1);
    _FX_CONV_T4x4(y41, y51, y61, y71, tr0, tr1);

    s12_0 = vaddq_f32(y10, y20); s12_1 = vaddq_f32(y50, y60);
    s34_0 = vaddq_f32(y30, y01); s34_1 = vaddq_f32(y70, y41);
    s56_0 = vaddq_f32(y11, y21); s56_1 = vaddq_f32(y51, y61);

    float32x4_t z00 = vaddq_f32(vaddq_f32(vaddq_f32(y00, s12_0), s34_0), s56_0);
    float32x4_t z01 = vaddq_f32(vaddq_f32(vaddq_f32(y40, s12_1), s34_1), s56_1);
    float32x4_t z20 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 4.0f), s56_0, 0.25f);
    float32x4_t z21 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 4.0f), s56_1, 0.25f);
    float32x4_t z40 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 16.0f), s56_0, 1.f/16);
    float32x4_t z41 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 16.0f), s56_1, 1.f/16);

    s12_0 = vsubq_f32(y10, y20); s12_1 = vsubq_f32(y50, y60);
    s34_0 = vsubq_f32(y30, y01); s34_1 = vsubq_f32(y70, y41);
    s56_0 = vsubq_f32(y11, y21); s56_1 = vsubq_f32(y51, y61);

    float32x4_t z50 = vfmaq_n_f32(vfmaq_n_f32(vaddq_f32(y31, s12_0),
                                  s34_0, 32.f), s56_0, 1.f/32);
    float32x4_t z51 = vfmaq_n_f32(vfmaq_n_f32(vaddq_f32(y71, s12_1),
                                  s34_1, 32.f), s56_1, 1.f/32);
    float32x4_t z10 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 2.0f), s56_0, 0.5f);
    float32x4_t z11 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 2.0f), s56_1, 0.5f);
    float32x4_t z30 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 8.0f), s56_0, 0.125f);
    float32x4_t z31 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 8.0f), s56_1, 0.125f);
    float32x4_t vbias = vdupq_n_f32(bias);

    z00 = vaddq_f32(z00, vbias); z01 = vaddq_f32(z01, vbias);
    z10 = vaddq_f32(z10, vbias); z11 = vaddq_f32(z11, vbias);
    z20 = vaddq_f32(z20, vbias); z21 = vaddq_f32(z21, vbias);
    z30 = vaddq_f32(z30, vbias); z31 = vaddq_f32(z31, vbias);
    z40 = vaddq_f32(z40, vbias); z41 = vaddq_f32(z41, vbias);
    z50 = vaddq_f32(z50, vbias); z51 = vaddq_f32(z51, vbias);

    if (pbptr) {
        z00 = vaddq_f32(z00, vld1q_f32(pbptr));
        z01 = vaddq_f32(z01, vld1q_f32(pbptr + 4));
        z10 = vaddq_f32(z10, vld1q_f32(pbptr + outstep));
        z11 = vaddq_f32(z11, vld1q_f32(pbptr + outstep + 4));
        z20 = vaddq_f32(z20, vld1q_f32(pbptr + outstep*2));
        z21 = vaddq_f32(z21, vld1q_f32(pbptr + outstep*2 + 4));
        z30 = vaddq_f32(z30, vld1q_f32(pbptr + outstep*3));
        z31 = vaddq_f32(z31, vld1q_f32(pbptr + outstep*3 + 4));
        z40 = vaddq_f32(z40, vld1q_f32(pbptr + outstep*4));
        z41 = vaddq_f32(z41, vld1q_f32(pbptr + outstep*4 + 4));
        z50 = vaddq_f32(z50, vld1q_f32(pbptr + outstep*5));
        // make sure we don't step beyond the passby tensor boundary
        z51 = vaddq_f32(z51, vcombine_f32(vld1_f32(pbptr + outstep*5 + 4), vdup_n_f32(0.f)));
    }

    if (activ) {
        float32x4_t vminv = vdupq_n_f32(minval), vmaxv = vdupq_n_f32(maxval);
        z00 = vminq_f32(vmaxq_f32(z00, vminv), vmaxv);
        z01 = vminq_f32(vmaxq_f32(z01, vminv), vmaxv);
        z10 = vminq_f32(vmaxq_f32(z10, vminv), vmaxv);
        z11 = vminq_f32(vmaxq_f32(z11, vminv), vmaxv);
        z20 = vminq_f32(vmaxq_f32(z20, vminv), vmaxv);
        z21 = vminq_f32(vmaxq_f32(z21, vminv), vmaxv);
        z30 = vminq_f32(vmaxq_f32(z30, vminv), vmaxv);
        z31 = vminq_f32(vmaxq_f32(z31, vminv), vmaxv);
        z40 = vminq_f32(vmaxq_f32(z40, vminv), vmaxv);
        z41 = vminq_f32(vmaxq_f32(z41, vminv), vmaxv);
        z50 = vminq_f32(vmaxq_f32(z50, vminv), vmaxv);
        z51 = vminq_f32(vmaxq_f32(z51, vminv), vmaxv);
    }

    vst1q_f32(outptr, z00);
    vst1_f32(outptr + 4, vget_low_f32(z01));
    vst1q_f32(outptr + outstep, z10);
    vst1_f32(outptr + outstep + 4, vget_low_f32(z11));
    vst1q_f32(outptr + outstep*2, z20);
    vst1_f32(outptr + outstep*2 + 4, vget_low_f32(z21));
    vst1q_f32(outptr + outstep*3, z30);
    vst1_f32(outptr + outstep*3 + 4, vget_low_f32(z31));
    vst1q_f32(outptr + outstep*4, z40);
    vst1_f32(outptr + outstep*4 + 4, vget_low_f32(z41));
    vst1q_f32(outptr + outstep*5, z50);
    vst1_f32(outptr + outstep*5 + 4, vget_low_f32(z51));
}

static int _fx_winograd_conv2d(int ndims, const int_* inpsize, const float* inp,
                                const int_* outsize, float* out, const float* passby,
                                const struct _fx_conv2d_t* conv, int ntasks)
{
    assert(ndims == 4 && inpsize[0] == outsize[0] && outsize[1] == conv->K && inpsize[1] == conv->C);
    int N = (int)inpsize[0], C = (int)inpsize[1], Hi = (int)inpsize[2], Wi = (int)inpsize[3];
    int K = conv->K, Hk = conv->Hk, Wk = conv->Wk;
    int H0 = (int)outsize[2], W0 = (int)outsize[3], ngroups = conv->ngroups;
    int Cg = C/ngroups, Kg = K/ngroups;
    const size_t inp_planesize = (size_t)Hi*Wi;
    const size_t out_planesize = (size_t)H0*W0;
    float minval = conv->minval, maxval = conv->maxval;
    bool fast_activ = conv->activ != _FX_ACTIV_NONE && conv->activ_func == 0;
    _fx_activ_func_t activ_func = !fast_activ ? conv->activ_func : 0;
    const float* activ_params = conv->activ_params;
    int blocks_per_row = (W0+_FX_WINO_STEP-1)/_FX_WINO_STEP;
    int blocks_per_plane = ((H0+_FX_WINO_STEP-1)/_FX_WINO_STEP)*blocks_per_row;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    size_t taskbufsize = _FX_WINO_SIZE*_FX_WINO_SIZE*(Cg+Kg);
    int64_t ts = fx_tick_count();
    float* wbuf_all = (float*)fx_malloc(ntasks*taskbufsize*sizeof(wbuf_all[0]));
    int ofstab[9], yxtab[9*2];
    for (int y = 0; y < Hk; y++)
        for( int x = 0; x < Wk; x++) {
            int k = y*Wk + x;
            yxtab[k*2] = y; yxtab[k*2+1] = x;
            ofstab[k] = y*Wi + x;
        }
    //printf("blocks_per_plane=%d\n", blocks_per_plane);
    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        float* wbuf_task = &wbuf_all[taskbufsize*task_id];
        int ngb0 = (N*ngroups*blocks_per_plane)*task_id/ntasks;
        int ngb1 = (N*ngroups*blocks_per_plane)*(task_id+1)/ntasks;
        for(int ngb = ngb0; ngb < ngb1; ngb++) {
            int n = ngb/(ngroups*blocks_per_plane), gb = ngb - n*(ngroups*blocks_per_plane);
            int g = gb/blocks_per_plane, yx0 = gb - g*blocks_per_plane;
            int y0 = yx0/blocks_per_row;
            int x0 = yx0 - y0*blocks_per_row;
            y0 = y0*_FX_WINO_STEP - pad_top;
            x0 = x0*_FX_WINO_STEP - pad_left;
            bool partial = y0 < 0 || y0 + _FX_WINO_SIZE > Hi ||
                           x0 < 0 || x0 + _FX_WINO_SIZE > Wi;
            int dx1 = 0, dx2 = _FX_WINO_SIZE, dy1 = 0, dy2 = _FX_WINO_SIZE;
            float inpbuf[_FX_WINO_AREA], Bt_inp[_FX_WINO_AREA], *At_out = Bt_inp, outbuf[_FX_WINO_AREA], pbbuf[_FX_WINO_AREA];
            int inpstep = Wi;
            if (partial) {
                memset(inpbuf, 0, sizeof(inpbuf));
                dy1 = -y0 > 0 ? -y0 : 0;
                dy2 = Hi - y0 < _FX_WINO_SIZE ? Hi - y0 : _FX_WINO_SIZE;
                if (dy2 < dy1) {dy2 = dy1 = 0;}
                dx1 = -x0 > 0 ? -x0 : 0;
                dx2 = Wi - x0 < _FX_WINO_SIZE ? Wi - x0 : _FX_WINO_SIZE;
                if (dx2 < dx1) {dx2 = dx1 = 0;}
                inpstep = _FX_WINO_SIZE;
            }

            // 1. compute forward Winograd transform of input blocks
            //    {y0 <= y < y0+_FX_WINO_SIZE, x0 <= x < x0+_FX_WINO_SIZE}
            //    for each channel
            for(int c = 0; c < Cg; c++) {
                const float* inptr0 = inp + ((n*ngroups + g)*Cg + c)*inp_planesize + y0*Wi + x0;
                const float* inptr = inptr0;
                if (partial) {
                    inptr0 -= y0*Wi + x0;
                    if (dx1 < dx2 && dy1 < dy2) {
                        for(int dy = dy1; dy < dy2; dy++)
                            memcpy(&inpbuf[dy*_FX_WINO_SIZE + dx1],
                                inptr0 + (y0+dy)*Wi + (x0+dx1), (dx2-dx1)*sizeof(inpbuf[0]));
                    }
                    inptr = inpbuf;
                }
                _fx_conv_winograd_BtXB_8x8(inptr, inpstep, wbuf_task + c*_FX_WINO_AREA, _FX_WINO_SIZE);
            }

            // 2. accumulate element-wise products wbuf_out[k,yx] += wbuf_inp[c,yx].*ww[k,c,yx],
            //    where ww is Winograd-tranformed weights
            _fx_winograd_accum(wbuf_task, conv->weights,
                               wbuf_task + Cg*_FX_WINO_AREA, Cg, Kg);

            y0 += pad_top;
            x0 += pad_left;
            dy1 = H0 - y0;
            if (dy1 > _FX_WINO_STEP) dy1 = _FX_WINO_STEP;
            dx1 = W0 - x0;
            if (dx1 > _FX_WINO_STEP) dx1 = _FX_WINO_STEP;
            partial = dy1 < _FX_WINO_STEP || dx1 < _FX_WINO_STEP;
            if (passby && partial) {
                memset(outbuf, 0, sizeof(outbuf));
                memset(pbbuf, 0, sizeof(pbbuf));
            }

            // 3. do inverse winograd transformation, then bias shift + activation
            for(int k = 0; k < Kg; k++) {
                size_t outofs = ((n*ngroups + g)*Kg + k)*out_planesize + y0*W0 + x0;
                float* outptr0 = out + outofs;
                const float* pbptr0 = passby ? passby + outofs : 0;
                float* outptr = outptr0;
                const float* pbptr = pbptr0;
                int outstep0 = W0, outstep = outstep0;
                float biasv = conv->bias[g*Kg + k];
                if (partial) {
                    outptr = outbuf;
                    outstep = _FX_WINO_SIZE;
                    if (pbptr) {
                        for (int y = 0; y < dy1; y++)
                            memcpy(&pbbuf[y*_FX_WINO_SIZE], pbptr + y*outstep,
                                   dx1*sizeof(pbptr[0]));
                        pbptr = pbbuf;
                    }
                }
                _fx_conv_winograd_AtXA_8x8(wbuf_task + (Cg+k)*_FX_WINO_AREA, _FX_WINO_SIZE,
                                           outptr, outstep, pbptr, biasv, fast_activ, minval, maxval);
                if (activ_func)
                    activ_func(outptr, outstep, _FX_WINO_STEP, _FX_WINO_STEP, activ_params);
                if (partial) {
                    for (int y = 0; y < dy1; y++)
                        memcpy(outptr0 + y*outstep0, outbuf + y*outstep,
                               dx1*sizeof(outptr[0]));
                }
            }
        }
    }

    fx_free(wbuf_all);
    {
    ts = fx_tick_count() - ts;
    total_time += ts;
    //printf("Winograd: N=%d, K=%d, C=%d, Hi=%d, Wi=%d: time=%.1f\n", N, K, C, Hi, Wi, ts*1000./fx_tick_frequency());
    }
    return FX_OK;
}
#endif
