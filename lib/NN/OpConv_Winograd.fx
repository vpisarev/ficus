/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Winograd-based convolution.
// The header is not intented to be used alone.
// It is assumed to be included into OpConv.fx

@ccode {
#include "ficus_nn_common.h"

static void
_fx_winograd_accum_f32(const float* inwptr, const float* wptr,
                       float* outbuf, int Cg, int iblock)
{
#ifdef __ARM_NEON
    FX_STATIC_ASSERT(_FX_WINO_IBLOCK == 6 && _FX_WINO_KBLOCK == 4);
    if (iblock > 3) {
        for (int atom_id = 0; atom_id < _FX_WINO_NATOMS_F32; atom_id++,
                                            outbuf += _FX_WINO_ATOM_F32)
        {
            float32x4_t s00 = vdupq_n_f32(0.f), s01 = s00, s02 = s00, s03 = s00, s04 = s00, s05 = s00;
            float32x4_t s10 = vdupq_n_f32(0.f), s11 = s00, s12 = s00, s13 = s00, s14 = s00, s15 = s00;
            float32x4_t s20 = vdupq_n_f32(0.f), s21 = s00, s22 = s00, s23 = s00, s24 = s00, s25 = s00;
            float32x4_t s30 = vdupq_n_f32(0.f), s31 = s00, s32 = s00, s33 = s00, s34 = s00, s35 = s00;
            for (int c = 0; c < Cg; c++, inwptr += _FX_WINO_IBLOCK*_FX_WINO_ATOM_F32,
                                        wptr += _FX_WINO_KBLOCK*_FX_WINO_ATOM_F32) {
                float32x4_t w0 = vld1q_f32(wptr), w1 = vld1q_f32(wptr + 4);
                float32x4_t w2 = vld1q_f32(wptr + 8), w3 = vld1q_f32(wptr + 12);
                float32x4_t x0, x1;
                x0 = vld1q_f32(inwptr);
                x1 = vld1q_f32(inwptr + 4);
                s00 = vfmaq_f32(s00, w0, x0);
                s01 = vfmaq_f32(s01, w0, x1);
                s10 = vfmaq_f32(s10, w1, x0);
                s11 = vfmaq_f32(s11, w1, x1);
                s20 = vfmaq_f32(s20, w2, x0);
                s21 = vfmaq_f32(s21, w2, x1);
                s30 = vfmaq_f32(s30, w3, x0);
                s31 = vfmaq_f32(s31, w3, x1);
                x0 = vld1q_f32(inwptr + 8);
                x1 = vld1q_f32(inwptr + 12);
                s02 = vfmaq_f32(s02, w0, x0);
                s03 = vfmaq_f32(s03, w0, x1);
                s12 = vfmaq_f32(s12, w1, x0);
                s13 = vfmaq_f32(s13, w1, x1);
                s22 = vfmaq_f32(s22, w2, x0);
                s23 = vfmaq_f32(s23, w2, x1);
                s32 = vfmaq_f32(s32, w3, x0);
                s33 = vfmaq_f32(s33, w3, x1);
                x0 = vld1q_f32(inwptr + 16);
                x1 = vld1q_f32(inwptr + 20);
                s04 = vfmaq_f32(s04, w0, x0);
                s05 = vfmaq_f32(s05, w0, x1);
                s14 = vfmaq_f32(s14, w1, x0);
                s15 = vfmaq_f32(s15, w1, x1);
                s24 = vfmaq_f32(s24, w2, x0);
                s25 = vfmaq_f32(s25, w2, x1);
                s34 = vfmaq_f32(s34, w3, x0);
                s35 = vfmaq_f32(s35, w3, x1);
            }

            vst1q_f32(outbuf, s00);
            vst1q_f32(outbuf + 1*64, s01);
            vst1q_f32(outbuf + 2*64, s02);
            vst1q_f32(outbuf + 3*64, s03);
            vst1q_f32(outbuf + 4*64, s04);
            vst1q_f32(outbuf + 5*64, s05);

            vst1q_f32(outbuf + 6*64, s10);
            vst1q_f32(outbuf + 7*64, s11);
            vst1q_f32(outbuf + 8*64, s12);
            vst1q_f32(outbuf + 9*64, s13);
            vst1q_f32(outbuf + 10*64, s14);
            vst1q_f32(outbuf + 11*64, s15);

            vst1q_f32(outbuf + 12*64, s20);
            vst1q_f32(outbuf + 13*64, s21);
            vst1q_f32(outbuf + 14*64, s22);
            vst1q_f32(outbuf + 15*64, s23);
            vst1q_f32(outbuf + 16*64, s24);
            vst1q_f32(outbuf + 17*64, s25);

            vst1q_f32(outbuf + 18*64, s30);
            vst1q_f32(outbuf + 19*64, s31);
            vst1q_f32(outbuf + 20*64, s32);
            vst1q_f32(outbuf + 21*64, s33);
            vst1q_f32(outbuf + 22*64, s34);
            vst1q_f32(outbuf + 23*64, s35);
        }
    } else {
        for (int atom_id = 0; atom_id < _FX_WINO_NATOMS_F32; atom_id++,
                                            outbuf += _FX_WINO_ATOM_F32)
        {
            float32x4_t s00 = vdupq_n_f32(0.f), s01 = s00, s02 = s00;
            float32x4_t s10 = vdupq_n_f32(0.f), s11 = s00, s12 = s00;
            float32x4_t s20 = vdupq_n_f32(0.f), s21 = s00, s22 = s00;
            float32x4_t s30 = vdupq_n_f32(0.f), s31 = s00, s32 = s00;
            for (int c = 0; c < Cg; c++, inwptr += _FX_WINO_IBLOCK*_FX_WINO_ATOM_F32,
                                         wptr += _FX_WINO_KBLOCK*_FX_WINO_ATOM_F32) {
                float32x4_t w0 = vld1q_f32(wptr), w1 = vld1q_f32(wptr + 4);
                float32x4_t w2 = vld1q_f32(wptr + 8), w3 = vld1q_f32(wptr + 12);
                float32x4_t x0, x1, x2;
                x0 = vld1q_f32(inwptr);
                x1 = vld1q_f32(inwptr + 4);
                x2 = vld1q_f32(inwptr + 8);
                s00 = vfmaq_f32(s00, w0, x0);
                s01 = vfmaq_f32(s01, w0, x1);
                s02 = vfmaq_f32(s02, w0, x2);
                s10 = vfmaq_f32(s10, w1, x0);
                s11 = vfmaq_f32(s11, w1, x1);
                s12 = vfmaq_f32(s12, w1, x2);
                s20 = vfmaq_f32(s20, w2, x0);
                s21 = vfmaq_f32(s21, w2, x1);
                s22 = vfmaq_f32(s22, w2, x2);
                s30 = vfmaq_f32(s30, w3, x0);
                s31 = vfmaq_f32(s31, w3, x1);
                s32 = vfmaq_f32(s32, w3, x2);
            }

            vst1q_f32(outbuf, s00);
            vst1q_f32(outbuf + 1*64, s01);
            vst1q_f32(outbuf + 2*64, s02);
            vst1q_f32(outbuf + 6*64, s10);
            vst1q_f32(outbuf + 7*64, s11);
            vst1q_f32(outbuf + 8*64, s12);
            vst1q_f32(outbuf + 12*64, s20);
            vst1q_f32(outbuf + 13*64, s21);
            vst1q_f32(outbuf + 14*64, s22);
            vst1q_f32(outbuf + 18*64, s30);
            vst1q_f32(outbuf + 19*64, s31);
            vst1q_f32(outbuf + 20*64, s32);
        }
    }
#else
    for (int atom_id = 0; atom_id < _FX_WINO_NATOMS_F32;
                            atom_id++, outbuf += _FX_WINO_ATOM_F32)
    {
        float sumbuf[_FX_WINO_IBLOCK*_FX_WINO_KBLOCK*_FX_WINO_ATOM_F32];
        memset(sumbuf, 0, sizeof(sumbuf));
        for (int c = 0; c < Cg; c++, inwptr += _FX_WINO_IBLOCK*_FX_WINO_ATOM_F32,
                                     wptr += _FX_WINO_KBLOCK*_FX_WINO_ATOM_F32) {
            for (int i = 0; i < _FX_WINO_KBLOCK; i++) {
                for (int j = 0; j < _FX_WINO_IBLOCK; j++) {
                    int i_ = i*_FX_WINO_ATOM_F32;
                    int j_ = j*_FX_WINO_ATOM_F32;
                    int ij_ = i_*_FX_WINO_IBLOCK + j_;
                    float s0 = inwptr[j_ + 0]*wptr[i_ + 0];
                    float s1 = inwptr[j_ + 1]*wptr[i_ + 1];
                    float s2 = inwptr[j_ + 2]*wptr[i_ + 2];
                    float s3 = inwptr[j_ + 3]*wptr[i_ + 3];
                    sumbuf[ij_ + 0] += s0;
                    sumbuf[ij_ + 1] += s1;
                    sumbuf[ij_ + 2] += s2;
                    sumbuf[ij_ + 3] += s3;
                }
            }
        }
        for (int ij = 0; ij < _FX_WINO_KBLOCK*_FX_WINO_IBLOCK; ij++) {
            int ij_ = ij*_FX_WINO_ATOM_F32;
            int ij_out = ij*_FX_WINO_AREA;
            outbuf[ij_out + 0] = sumbuf[ij_ + 0];
            outbuf[ij_out + 1] = sumbuf[ij_ + 1];
            outbuf[ij_out + 2] = sumbuf[ij_ + 2];
            outbuf[ij_out + 3] = sumbuf[ij_ + 3];
        }
    }
#endif
}

#if _FX_NN_ENABLE_FP16
static void
_fx_winograd_accum_f16(const fx_f16* inwptr, const fx_f16* wptr,
                       fx_f16* outbuf, int Cg, int iblock)
{
#ifdef __ARM_NEON
    FX_STATIC_ASSERT(_FX_WINO_IBLOCK == 6 && _FX_WINO_KBLOCK == 4);
    if (iblock > 3) {
        for (int atom_id = 0; atom_id < _FX_WINO_NATOMS_F16; atom_id++,
                                            outbuf += _FX_WINO_ATOM_F16)
        {
            float16x8_t s00 = vdupq_n_f16(0.f), s01 = s00, s02 = s00, s03 = s00, s04 = s00, s05 = s00;
            float16x8_t s10 = vdupq_n_f16(0.f), s11 = s00, s12 = s00, s13 = s00, s14 = s00, s15 = s00;
            float16x8_t s20 = vdupq_n_f16(0.f), s21 = s00, s22 = s00, s23 = s00, s24 = s00, s25 = s00;
            float16x8_t s30 = vdupq_n_f16(0.f), s31 = s00, s32 = s00, s33 = s00, s34 = s00, s35 = s00;
            for (int c = 0; c < Cg; c++, inwptr += _FX_WINO_IBLOCK*_FX_WINO_ATOM_F16,
                                        wptr += _FX_WINO_KBLOCK*_FX_WINO_ATOM_F16) {
                float16x8_t w0 = vld1q_f16(wptr), w1 = vld1q_f16(wptr + 8);
                float16x8_t w2 = vld1q_f16(wptr + 16), w3 = vld1q_f16(wptr + 24);
                float16x8_t x0, x1, x2;
                x0 = vld1q_f16(inwptr);
                x1 = vld1q_f16(inwptr + 8);
                x2 = vld1q_f16(inwptr + 16);

                s00 = vfmaq_f16(s00, w0, x0);
                s01 = vfmaq_f16(s01, w0, x1);
                s02 = vfmaq_f16(s02, w0, x2);

                s10 = vfmaq_f16(s10, w1, x0);
                s11 = vfmaq_f16(s11, w1, x1);
                s12 = vfmaq_f16(s12, w1, x2);

                s20 = vfmaq_f16(s20, w2, x0);
                s21 = vfmaq_f16(s21, w2, x1);
                s22 = vfmaq_f16(s22, w2, x2);

                s30 = vfmaq_f16(s30, w3, x0);
                s31 = vfmaq_f16(s31, w3, x1);
                s32 = vfmaq_f16(s32, w3, x2);

                x0 = vld1q_f16(inwptr + 24);
                x1 = vld1q_f16(inwptr + 32);
                x2 = vld1q_f16(inwptr + 40);

                s03 = vfmaq_f16(s03, w0, x0);
                s04 = vfmaq_f16(s04, w0, x1);
                s05 = vfmaq_f16(s05, w0, x2);

                s13 = vfmaq_f16(s13, w1, x0);
                s14 = vfmaq_f16(s14, w1, x1);
                s15 = vfmaq_f16(s15, w1, x2);

                s23 = vfmaq_f16(s23, w2, x0);
                s24 = vfmaq_f16(s24, w2, x1);
                s25 = vfmaq_f16(s25, w2, x2);

                s33 = vfmaq_f16(s33, w3, x0);
                s34 = vfmaq_f16(s34, w3, x1);
                s35 = vfmaq_f16(s35, w3, x2);
            }

            vst1q_f16(outbuf, s00);
            vst1q_f16(outbuf + 1*64, s01);
            vst1q_f16(outbuf + 2*64, s02);
            vst1q_f16(outbuf + 3*64, s03);
            vst1q_f16(outbuf + 4*64, s04);
            vst1q_f16(outbuf + 5*64, s05);

            vst1q_f16(outbuf + 6*64, s10);
            vst1q_f16(outbuf + 7*64, s11);
            vst1q_f16(outbuf + 8*64, s12);
            vst1q_f16(outbuf + 9*64, s13);
            vst1q_f16(outbuf + 10*64, s14);
            vst1q_f16(outbuf + 11*64, s15);

            vst1q_f16(outbuf + 12*64, s20);
            vst1q_f16(outbuf + 13*64, s21);
            vst1q_f16(outbuf + 14*64, s22);
            vst1q_f16(outbuf + 15*64, s23);
            vst1q_f16(outbuf + 16*64, s24);
            vst1q_f16(outbuf + 17*64, s25);

            vst1q_f16(outbuf + 18*64, s30);
            vst1q_f16(outbuf + 19*64, s31);
            vst1q_f16(outbuf + 20*64, s32);
            vst1q_f16(outbuf + 21*64, s33);
            vst1q_f16(outbuf + 22*64, s34);
            vst1q_f16(outbuf + 23*64, s35);
        }
    } else {
        for (int atom_id = 0; atom_id < _FX_WINO_NATOMS_F16; atom_id++,
                                            outbuf += _FX_WINO_ATOM_F16)
        {
            float16x8_t s00 = vdupq_n_f16(0.f), s01 = s00, s02 = s00;
            float16x8_t s10 = vdupq_n_f16(0.f), s11 = s00, s12 = s00;
            float16x8_t s20 = vdupq_n_f16(0.f), s21 = s00, s22 = s00;
            float16x8_t s30 = vdupq_n_f16(0.f), s31 = s00, s32 = s00;
            for (int c = 0; c < Cg; c++, inwptr += _FX_WINO_IBLOCK*_FX_WINO_ATOM_F16,
                                         wptr += _FX_WINO_KBLOCK*_FX_WINO_ATOM_F16) {
                float32x4_t w0 = vld1q_f16(wptr), w1 = vld1q_f16(wptr + 8);
                float32x4_t w2 = vld1q_f16(wptr + 16), w3 = vld1q_f16(wptr + 24);
                float32x4_t x0, x1, x2;
                x0 = vld1q_f16(inwptr);
                x1 = vld1q_f16(inwptr + 8);
                x2 = vld1q_f16(inwptr + 16);
                s00 = vfmaq_f16(s00, w0, x0);
                s01 = vfmaq_f16(s01, w0, x1);
                s02 = vfmaq_f16(s02, w0, x2);
                s10 = vfmaq_f16(s10, w1, x0);
                s11 = vfmaq_f16(s11, w1, x1);
                s12 = vfmaq_f16(s12, w1, x2);
                s20 = vfmaq_f16(s20, w2, x0);
                s21 = vfmaq_f16(s21, w2, x1);
                s22 = vfmaq_f16(s22, w2, x2);
                s30 = vfmaq_f16(s30, w3, x0);
                s31 = vfmaq_f16(s31, w3, x1);
                s32 = vfmaq_f16(s32, w3, x2);
            }

            vst1q_f16(outbuf, s00);
            vst1q_f16(outbuf + 1*64, s01);
            vst1q_f16(outbuf + 2*64, s02);
            vst1q_f16(outbuf + 6*64, s10);
            vst1q_f16(outbuf + 7*64, s11);
            vst1q_f16(outbuf + 8*64, s12);
            vst1q_f16(outbuf + 12*64, s20);
            vst1q_f16(outbuf + 13*64, s21);
            vst1q_f16(outbuf + 14*64, s22);
            vst1q_f16(outbuf + 18*64, s30);
            vst1q_f16(outbuf + 19*64, s31);
            vst1q_f16(outbuf + 20*64, s32);
        }
    }
#else
    for (int atom_id = 0; atom_id < _FX_WINO_NATOMS_F16;
                            atom_id++, outbuf += _FX_WINO_ATOM_F16)
    {
        float sumbuf[_FX_WINO_IBLOCK*_FX_WINO_KBLOCK*_FX_WINO_ATOM_F16];
        memset(sumbuf, 0, sizeof(sumbuf));
        for (int c = 0; c < Cg; c++, inwptr += _FX_WINO_IBLOCK*_FX_WINO_ATOM_F16,
                                     wptr += _FX_WINO_KBLOCK*_FX_WINO_ATOM_F16) {
            for (int i = 0; i < _FX_WINO_KBLOCK; i++) {
                int i_ = i*_FX_WINO_ATOM_F16;
                float w0 = FX_FLOAT(wptr[i_ + 0]), w1 = FX_FLOAT(wptr[i_ + 1]);
                float w2 = FX_FLOAT(wptr[i_ + 2]), w3 = FX_FLOAT(wptr[i_ + 3]);
                float w4 = FX_FLOAT(wptr[i_ + 4]), w5 = FX_FLOAT(wptr[i_ + 5]);
                float w6 = FX_FLOAT(wptr[i_ + 6]), w7 = FX_FLOAT(wptr[i_ + 7]);
                for (int j = 0; j < _FX_WINO_IBLOCK; j++) {
                    int j_ = j*_FX_WINO_ATOM_F16;
                    int ij_ = i_*_FX_WINO_IBLOCK + j_;
                    float s0 = FX_FLOAT(inwptr[j_ + 0])*w0;
                    float s1 = FX_FLOAT(inwptr[j_ + 1])*w1;
                    float s2 = FX_FLOAT(inwptr[j_ + 2])*w2;
                    float s3 = FX_FLOAT(inwptr[j_ + 3])*w3;
                    float s4 = FX_FLOAT(inwptr[j_ + 4])*w4;
                    float s5 = FX_FLOAT(inwptr[j_ + 5])*w5;
                    float s6 = FX_FLOAT(inwptr[j_ + 6])*w6;
                    float s7 = FX_FLOAT(inwptr[j_ + 7])*w7;
                    sumbuf[ij_ + 0] += s0;
                    sumbuf[ij_ + 1] += s1;
                    sumbuf[ij_ + 2] += s2;
                    sumbuf[ij_ + 3] += s3;
                    sumbuf[ij_ + 4] += s4;
                    sumbuf[ij_ + 5] += s5;
                    sumbuf[ij_ + 6] += s6;
                    sumbuf[ij_ + 7] += s7;
                }
            }
        }
        for (int ij = 0; ij < _FX_WINO_KBLOCK*_FX_WINO_IBLOCK; ij++) {
            int ij_ = ij*_FX_WINO_ATOM_F16;
            int ij_out = ij*_FX_WINO_AREA;
            outbuf[ij_out + 0] = FX_FLOAT16(sumbuf[ij_ + 0]);
            outbuf[ij_out + 1] = FX_FLOAT16(sumbuf[ij_ + 1]);
            outbuf[ij_out + 2] = FX_FLOAT16(sumbuf[ij_ + 2]);
            outbuf[ij_out + 3] = FX_FLOAT16(sumbuf[ij_ + 3]);
            outbuf[ij_out + 4] = FX_FLOAT16(sumbuf[ij_ + 4]);
            outbuf[ij_out + 5] = FX_FLOAT16(sumbuf[ij_ + 5]);
            outbuf[ij_out + 6] = FX_FLOAT16(sumbuf[ij_ + 6]);
            outbuf[ij_out + 7] = FX_FLOAT16(sumbuf[ij_ + 7]);
        }
    }
#endif
}
#endif

#ifdef __ARM_NEON
#define T4x4(a, b, c, d, tr0, tr1) \
    tr0 = vtrnq_f32(a, b); \
    tr1 = vtrnq_f32(c, d); \
    a = vcombine_f32(vget_low_f32(tr0.val[0]), vget_low_f32(tr1.val[0])); \
    b = vcombine_f32(vget_low_f32(tr0.val[1]), vget_low_f32(tr1.val[1])); \
    c = vcombine_f32(vget_high_f32(tr0.val[0]), vget_high_f32(tr1.val[0])); \
    d = vcombine_f32(vget_high_f32(tr0.val[1]), vget_high_f32(tr1.val[1]))

/*  Forward Winograd 8x8 transform:
    out = (B'*inp*B)', where
    inp is input 8x8 FP32 matrix,
    B' is
    [1.f, 0.f, -5.25f, 0.f, 5.25f, 0.f, -1.f, 0.f,
    0.f, 1.f, 1.f, -4.25f, -4.25f, 1.f, 1.f, 0.f,
    0.f, -1.f, 1.f, 4.25f, -4.25f, -1.f, 1.f, 0.f,
    0.f, 0.5f, 0.25f, -2.5f, -1.25f, 2.f, 1.f, 0.f,
    0.f, -0.5f, 0.25f, 2.5f, -1.25f, -2.f, 1.f, 0.f,
    0.f, 2.f, 4.f, -2.5f, -5.f, 0.5f, 1.f, 0.f,
    0.f, -2.f, 4.f, 2.5f, -5.f, -0.5f, 1.f, 0.f,
    0.f, -1.f, 0.f, 5.25f, 0.f, -5.25f, 0.f, 1.f]

    inp is pre-loaded into xij registers,
    out will be stored in zij, where 0<=i<=7, 0<=j<=1.
*/
#define _FX_WINOGRAD_FWD_8x8() \
    { \
    /* Y[0] = [1.f, 0.f, -5.25f, 0.f, 5.25f, 0.f, -1.f, 0.f]*X */ \
    /* Y[7] = [0.f, -1.f, 0.f, 5.25f, 0.f, -5.25f, 0.f, 1.f]*X */ \
    float32x4_t q5_25 = vdupq_n_f32(5.25f), t00, t01, t10, t11; \
    t00 = vsubq_f32(x40, x20); \
    t01 = vsubq_f32(x41, x21); \
    t10 = vsubq_f32(x30, x50); \
    t11 = vsubq_f32(x31, x51); \
    float32x4_t y00 = vfmaq_f32(vsubq_f32(x00, x60), t00, q5_25); \
    float32x4_t y01 = vfmaq_f32(vsubq_f32(x01, x61), t01, q5_25); \
    float32x4_t y70 = vfmaq_f32(vsubq_f32(x70, x10), t10, q5_25); \
    float32x4_t y71 = vfmaq_f32(vsubq_f32(x71, x11), t11, q5_25); \
    \
    /* Y[1] = [0.f, 1.f, 1.f, -4.25f, -4.25f, 1.f, 1.f, 0.f]*X */ \
    /* Y[2] = [0.f, -1.f, 1.f, 4.25f, -4.25f, -1.f, 1.f, 0.f]*X */ \
    float32x4_t qm4_25 = vdupq_n_f32(-4.25f); \
    t00 = vfmaq_f32(vaddq_f32(x10, x50), x30, qm4_25); \
    t01 = vfmaq_f32(vaddq_f32(x11, x51), x31, qm4_25); \
    t10 = vfmaq_f32(vaddq_f32(x20, x60), x40, qm4_25); \
    t11 = vfmaq_f32(vaddq_f32(x21, x61), x41, qm4_25); \
    \
    float32x4_t y10 = vaddq_f32(t00, t10), y11 = vaddq_f32(t01, t11); \
    float32x4_t y20 = vsubq_f32(t10, t00), y21 = vsubq_f32(t11, t01); \
    \
    /* Y[3] = [0.f, 0.5f, 0.25f, -2.5f, -1.25f, 2.f, 1.f, 0.f]*X */ \
    /* Y[4] = [0.f, -0.5f, 0.25f, 2.5f, -1.25f, -2.f, 1.f, 0.f]*X */ \
    float32x4_t q0_5 = vdupq_n_f32(0.5f), q0_25 = vdupq_n_f32(0.25f); \
    float32x4_t qm2_5 = vdupq_n_f32(-2.5f), qm1_25 = vdupq_n_f32(-1.25f); \
    t00 = vfmaq_f32(vaddq_f32(x50, x50), x10, q0_5); \
    t01 = vfmaq_f32(vaddq_f32(x51, x51), x11, q0_5); \
    t10 = vfmaq_f32(x60, x20, q0_25); \
    t11 = vfmaq_f32(x61, x21, q0_25); \
    t00 = vfmaq_f32(t00, x30, qm2_5); \
    t01 = vfmaq_f32(t01, x31, qm2_5); \
    t10 = vfmaq_f32(t10, x40, qm1_25); \
    t11 = vfmaq_f32(t11, x41, qm1_25); \
    \
    float32x4_t y30 = vaddq_f32(t00, t10), y31 = vaddq_f32(t01, t11); \
    float32x4_t y40 = vsubq_f32(t10, t00), y41 = vsubq_f32(t11, t01); \
    \
    /* Y[5] = [0.f, 2.f, 4.f, -2.5f, -5.f, 0.5f, 1.f, 0.f]*X */ \
    /* Y[6] = [0.f, -2.f, 4.f, 2.5f, -5.f, -0.5f, 1.f, 0.f]*X */ \
    float32x4_t q4 = vdupq_n_f32(4.f), qm5 = vdupq_n_f32(-5.f); \
    t00 = vfmaq_f32(vaddq_f32(x10, x10), x50, q0_5); \
    t01 = vfmaq_f32(vaddq_f32(x11, x11), x51, q0_5); \
    t10 = vfmaq_f32(x60, x20, q4); \
    t11 = vfmaq_f32(x61, x21, q4); \
    t00 = vfmaq_f32(t00, x30, qm2_5); \
    t01 = vfmaq_f32(t01, x31, qm2_5); \
    t10 = vfmaq_f32(t10, x40, qm5); \
    t11 = vfmaq_f32(t11, x41, qm5); \
    \
    float32x4_t y50 = vaddq_f32(t00, t10), y51 = vaddq_f32(t01, t11); \
    float32x4_t y60 = vsubq_f32(t10, t00), y61 = vsubq_f32(t11, t01); \
    \
    /* transpose 8x8 matrix in-place with some renumeration of the elements: */ \
    /* Y:              */ \
    /*        y00 y01  */ \
    /*        y10 y11  */ \
    /*        ...      */ \
    /*        y70 y71  */ \
    /*   Y':           */ \
    /*        y00 y40  */ \
    /*        y10 y50  */ \
    /*        y20 y60  */ \
    /*        y30 y70  */ \
    /*        y01 y41  */ \
    /*        y11 y51  */ \
    /*        y21 y61  */ \
    /*        y31 y71  */ \
    /*    in other words, y40 <-> y01, y50 <-> y11, y60 <-> y21, y70 <-> y31 */ \
    float32x4x2_t tr0, tr1; \
    \
    T4x4(y00, y10, y20, y30, tr0, tr1); \
    T4x4(y01, y11, y21, y31, tr0, tr1); \
    T4x4(y40, y50, y60, y70, tr0, tr1); \
    T4x4(y41, y51, y61, y71, tr0, tr1); \
    \
    /* Z[0] = [1.f, 0.f, -5.25f, 0.f, 5.25f, 0.f, -1.f, 0.f]*Y */ \
    /* Z[7] = [0.f, -1.f, 0.f, 5.25f, 0.f, -5.25f, 0.f, 1.f]*Y */ \
    t00 = vsubq_f32(y01, y20); \
    t01 = vsubq_f32(y41, y60); \
    t10 = vsubq_f32(y30, y11); \
    t11 = vsubq_f32(y70, y51); \
    z00 = vfmaq_f32(vsubq_f32(y00, y21), t00, q5_25); \
    z01 = vfmaq_f32(vsubq_f32(y40, y61), t01, q5_25); \
    z70 = vfmaq_f32(vsubq_f32(y31, y10), t10, q5_25); \
    z71 = vfmaq_f32(vsubq_f32(y71, y50), t11, q5_25); \
    \
    /* Z[1] = [0.f, 1.f, 1.f, -4.25f, -4.25f, 1.f, 1.f, 0.f]*Y */ \
    /* Z[2] = [0.f, -1.f, 1.f, 4.25f, -4.25f, -1.f, 1.f, 0.f]*Y */ \
    t00 = vfmaq_f32(vaddq_f32(y10, y11), y30, qm4_25); \
    t01 = vfmaq_f32(vaddq_f32(y50, y51), y70, qm4_25); \
    t10 = vfmaq_f32(vaddq_f32(y20, y21), y01, qm4_25); \
    t11 = vfmaq_f32(vaddq_f32(y60, y61), y41, qm4_25); \
    \
    z10 = vaddq_f32(t00, t10); z11 = vaddq_f32(t01, t11); \
    z20 = vsubq_f32(t10, t00); z21 = vsubq_f32(t11, t01); \
    \
    /* Z[3] = [0.f, 0.5f, 0.25f, -2.5f, -1.25f, 2.f, 1.f, 0.f]*Y */ \
    /* Z[4] = [0.f, -0.5f, 0.25f, 2.5f, -1.25f, -2.f, 1.f, 0.f]*Y */ \
    t00 = vfmaq_f32(vaddq_f32(y11, y11), y10, q0_5); \
    t01 = vfmaq_f32(vaddq_f32(y51, y51), y50, q0_5); \
    t10 = vfmaq_f32(y21, y20, q0_25); \
    t11 = vfmaq_f32(y61, y60, q0_25); \
    t00 = vfmaq_f32(t00, y30, qm2_5); \
    t01 = vfmaq_f32(t01, y70, qm2_5); \
    t10 = vfmaq_f32(t10, y01, qm1_25); \
    t11 = vfmaq_f32(t11, y41, qm1_25); \
    \
    z30 = vaddq_f32(t00, t10); z31 = vaddq_f32(t01, t11); \
    z40 = vsubq_f32(t10, t00); z41 = vsubq_f32(t11, t01); \
    \
    /* Z[5] = [0.f, 2.f, 4.f, -2.5f, -5.f, 0.5f, 1.f, 0.f]*Y */ \
    /* Z[6] = [0.f, -2.f, 4.f, 2.5f, -5.f, -0.5f, 1.f, 0.f]*Y */ \
    t00 = vfmaq_f32(vaddq_f32(y10, y10), y11, q0_5); \
    t01 = vfmaq_f32(vaddq_f32(y50, y50), y51, q0_5); \
    t10 = vfmaq_f32(y21, y20, q4); \
    t11 = vfmaq_f32(y61, y60, q4); \
    t00 = vfmaq_f32(t00, y30, qm2_5); \
    t01 = vfmaq_f32(t01, y70, qm2_5); \
    t10 = vfmaq_f32(t10, y01, qm5); \
    t11 = vfmaq_f32(t11, y41, qm5); \
    \
    z50 = vaddq_f32(t00, t10); z51 = vaddq_f32(t01, t11); \
    z60 = vsubq_f32(t10, t00); z61 = vsubq_f32(t11, t01); \
    }

static void
_fx_winograd_BtXB_8x8_f32(const float* inptr, int inpstep,
                          float* outptr, int Cg)
{
    float32x4_t x00 = vld1q_f32(inptr), x01 = vld1q_f32(inptr + 4);
    float32x4_t x10 = vld1q_f32(inptr + inpstep), x11 = vld1q_f32(inptr + inpstep + 4);
    float32x4_t x20 = vld1q_f32(inptr + inpstep*2), x21 = vld1q_f32(inptr + inpstep*2 + 4);
    float32x4_t x30 = vld1q_f32(inptr + inpstep*3), x31 = vld1q_f32(inptr + inpstep*3 + 4);
    float32x4_t x40 = vld1q_f32(inptr + inpstep*4), x41 = vld1q_f32(inptr + inpstep*4 + 4);
    float32x4_t x50 = vld1q_f32(inptr + inpstep*5), x51 = vld1q_f32(inptr + inpstep*5 + 4);
    float32x4_t x60 = vld1q_f32(inptr + inpstep*6), x61 = vld1q_f32(inptr + inpstep*6 + 4);
    float32x4_t x70 = vld1q_f32(inptr + inpstep*7), x71 = vld1q_f32(inptr + inpstep*7 + 4);

    float32x4_t z00, z01, z10, z11, z20, z21, z30, z31, z40, z41, z50, z51, z60, z61, z70, z71;

    _FX_WINOGRAD_FWD_8x8()

    const int outstep = _FX_WINO_IBLOCK*_FX_WINO_ATOM_F32*Cg;
    vst1q_f32(outptr, z00);
    vst1q_f32(outptr + outstep, z01);
    vst1q_f32(outptr + outstep*2, z10);
    vst1q_f32(outptr + outstep*3, z11);
    vst1q_f32(outptr + outstep*4, z20);
    vst1q_f32(outptr + outstep*5, z21);
    vst1q_f32(outptr + outstep*6, z30);
    vst1q_f32(outptr + outstep*7, z31);
    vst1q_f32(outptr + outstep*8, z40);
    vst1q_f32(outptr + outstep*9, z41);
    vst1q_f32(outptr + outstep*10, z50);
    vst1q_f32(outptr + outstep*11, z51);
    vst1q_f32(outptr + outstep*12, z60);
    vst1q_f32(outptr + outstep*13, z61);
    vst1q_f32(outptr + outstep*14, z70);
    vst1q_f32(outptr + outstep*15, z71);
}

/*  Inverse Winograd 8x8 transform:
    out = (A'*inp*A)', where
    inp is input 8x8 FP32 matrix,
    A' is
    [1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 0.f,
     0.f, 1.f, -1.f, 2.f, -2.f, 0.5f, -0.5f, 0.f,
     0.f, 1.f, 1.f, 4.f, 4.f, 0.25f, 0.25f, 0.f,
     0.f, 1.f, -1.f, 8.f, -8.f, 0.125f, -0.125f, 0.f,
     0.f, 1.f, 1.f, 16.f, 16.f, 1.f/16, 1.f/16, 0.f,
     0.f, 1.f, -1.f, 32.f, -32.f, 1.f/32, -1.f/32, 1.f]

    inp is pre-loaded into xij registers,
    out will be stored in zij, where (0<=i<=7 for x, 0<=i<=5 for z), 0<=j<=1.

    After the inverse transform is done, we add bias,
    optionally add results from the earlier tensors (by-pass),
    optionally apply activation function and then
    store the final results.

    Note that both _FX_WINOGRAD_FWD_8x8() and
    _FX_WINOGRAD_INV_8x8() produce tranposed output.
    That is, after both forward and then inverse transformation,
    we get non-transposed result.
    Of course, for the correct work of Winograd-based convolution,
    the Winograd-transformed weights should also be transposed.
    init_conv() (see OpConv.fx) takes care of that.
*/
#define _FX_WINOGRAD_INV_8x8() \
    { \
    float32x4_t s12_0, s12_1, s34_0, s34_1, s56_0, s56_1; \
    s12_0 = vaddq_f32(x10, x20); s12_1 = vaddq_f32(x11, x21); \
    s34_0 = vaddq_f32(x30, x40); s34_1 = vaddq_f32(x31, x41); \
    s56_0 = vaddq_f32(x50, x60); s56_1 = vaddq_f32(x51, x61); \
    \
    float32x4_t y00 = vaddq_f32(vaddq_f32(vaddq_f32(x00, s12_0), s34_0), s56_0); \
    float32x4_t y01 = vaddq_f32(vaddq_f32(vaddq_f32(x01, s12_1), s34_1), s56_1); \
    float32x4_t y20 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 4.0f), s56_0, 0.25f); \
    float32x4_t y21 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 4.0f), s56_1, 0.25f); \
    float32x4_t y40 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 16.0f), s56_0, 1.f/16); \
    float32x4_t y41 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 16.0f), s56_1, 1.f/16); \
    \
    s12_0 = vsubq_f32(x10, x20); s12_1 = vsubq_f32(x11, x21); \
    s34_0 = vsubq_f32(x30, x40); s34_1 = vsubq_f32(x31, x41); \
    s56_0 = vsubq_f32(x50, x60); s56_1 = vsubq_f32(x51, x61); \
    \
    float32x4_t y50 = vfmaq_n_f32(vfmaq_n_f32(vaddq_f32(x70, s12_0), \
                                  s34_0, 32.f), s56_0, 1.f/32); \
    float32x4_t y51 = vfmaq_n_f32(vfmaq_n_f32(vaddq_f32(x71, s12_1), \
                                  s34_1, 32.f), s56_1, 1.f/32); \
    float32x4_t y10 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 2.0f), s56_0, 0.5f); \
    float32x4_t y11 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 2.0f), s56_1, 0.5f); \
    float32x4_t y30 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 8.0f), s56_0, 0.125f); \
    float32x4_t y31 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 8.0f), s56_1, 0.125f); \
    float32x4_t y60 = vdupq_n_f32(0.f), y61 = y60, y70 = y60, y71 = y60; \
    \
    /* transpose 8x8 matrix in-place with some renumeration of the elements: */ \
    /*  Y: */ \
    /*        y00 y01 */ \
    /*        y10 y11 */ \
    /*        ... */ \
    /*        y50 y51 */ \
    /*        0   0 */ \
    /*        0   0 */ \
    /*   Y': */ \
    /*        y00 y40 */ \
    /*        y10 y50 */ \
    /*        y20 y60 */ \
    /*        y30 y70 */ \
    /*        y01 y41 */ \
    /*        y11 y51 */ \
    /*        y21 y61 */ \
    /*        y31 y71 */ \
    /*    in other words, y40 <-> y01, y50 <-> y11, y60 <-> y21, y70 <-> y31 */ \
    float32x4x2_t tr0, tr1; \
    \
    T4x4(y00, y10, y20, y30, tr0, tr1); \
    T4x4(y01, y11, y21, y31, tr0, tr1); \
    T4x4(y40, y50, y60, y70, tr0, tr1); \
    T4x4(y41, y51, y61, y71, tr0, tr1); \
    \
    s12_0 = vaddq_f32(y10, y20); s12_1 = vaddq_f32(y50, y60); \
    s34_0 = vaddq_f32(y30, y01); s34_1 = vaddq_f32(y70, y41); \
    s56_0 = vaddq_f32(y11, y21); s56_1 = vaddq_f32(y51, y61); \
    \
    z00 = vaddq_f32(vaddq_f32(vaddq_f32(y00, s12_0), s34_0), s56_0); \
    z01 = vaddq_f32(vaddq_f32(vaddq_f32(y40, s12_1), s34_1), s56_1); \
    z20 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 4.0f), s56_0, 0.25f); \
    z21 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 4.0f), s56_1, 0.25f); \
    z40 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 16.0f), s56_0, 1.f/16); \
    z41 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 16.0f), s56_1, 1.f/16); \
    \
    s12_0 = vsubq_f32(y10, y20); s12_1 = vsubq_f32(y50, y60); \
    s34_0 = vsubq_f32(y30, y01); s34_1 = vsubq_f32(y70, y41); \
    s56_0 = vsubq_f32(y11, y21); s56_1 = vsubq_f32(y51, y61); \
    \
    z50 = vfmaq_n_f32(vfmaq_n_f32(vaddq_f32(y31, s12_0), \
                      s34_0, 32.f), s56_0, 1.f/32); \
    z51 = vfmaq_n_f32(vfmaq_n_f32(vaddq_f32(y71, s12_1), \
                      s34_1, 32.f), s56_1, 1.f/32); \
    z10 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 2.0f), s56_0, 0.5f); \
    z11 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 2.0f), s56_1, 0.5f); \
    z30 = vfmaq_n_f32(vfmaq_n_f32(s12_0, s34_0, 8.0f), s56_0, 0.125f); \
    z31 = vfmaq_n_f32(vfmaq_n_f32(s12_1, s34_1, 8.0f), s56_1, 0.125f); \
    float32x4_t vbias = vdupq_n_f32(bias); \
    \
    z00 = vaddq_f32(z00, vbias); \
    z01 = vaddq_f32(z01, vbias); \
    z10 = vaddq_f32(z10, vbias); \
    z11 = vaddq_f32(z11, vbias); \
    z20 = vaddq_f32(z20, vbias); \
    z21 = vaddq_f32(z21, vbias); \
    z30 = vaddq_f32(z30, vbias); \
    z31 = vaddq_f32(z31, vbias); \
    z40 = vaddq_f32(z40, vbias); \
    z41 = vaddq_f32(z41, vbias); \
    z50 = vaddq_f32(z50, vbias); \
    z51 = vaddq_f32(z51, vbias); \
    }

static void
_fx_winograd_AtXA_8x8_f32(const float* inptr, int inpstep,
                          float* bpptr, int bpstep, float* outptr, int outstep,
                          float bias, float maxval, float alpha,
                          bool fast_activ)
{
    float32x4_t x00 = vld1q_f32(inptr), x01 = vld1q_f32(inptr + 4);
    float32x4_t x10 = vld1q_f32(inptr + inpstep), x11 = vld1q_f32(inptr + inpstep + 4);
    float32x4_t x20 = vld1q_f32(inptr + inpstep*2), x21 = vld1q_f32(inptr + inpstep*2 + 4);
    float32x4_t x30 = vld1q_f32(inptr + inpstep*3), x31 = vld1q_f32(inptr + inpstep*3 + 4);
    float32x4_t x40 = vld1q_f32(inptr + inpstep*4), x41 = vld1q_f32(inptr + inpstep*4 + 4);
    float32x4_t x50 = vld1q_f32(inptr + inpstep*5), x51 = vld1q_f32(inptr + inpstep*5 + 4);
    float32x4_t x60 = vld1q_f32(inptr + inpstep*6), x61 = vld1q_f32(inptr + inpstep*6 + 4);
    float32x4_t x70 = vld1q_f32(inptr + inpstep*7), x71 = vld1q_f32(inptr + inpstep*7 + 4);
    float32x4_t z00, z01, z10, z11, z20, z21, z30, z31, z40, z41, z50, z51;

    _FX_WINOGRAD_INV_8x8()

    if (bpptr) {
        float32x2_t zhalf = vdup_n_f32(0.f);
        z00 = vaddq_f32(z00, vld1q_f32(bpptr));
        z01 = vaddq_f32(z01, vcombine_f32(vld1_f32(bpptr + 4), zhalf));
        z10 = vaddq_f32(z10, vld1q_f32(bpptr + bpstep));
        z11 = vaddq_f32(z11, vcombine_f32(vld1_f32(bpptr + bpstep + 4), zhalf));
        z20 = vaddq_f32(z20, vld1q_f32(bpptr + bpstep*2));
        z21 = vaddq_f32(z21, vcombine_f32(vld1_f32(bpptr + bpstep*2 + 4), zhalf));
        z30 = vaddq_f32(z30, vld1q_f32(bpptr + bpstep*3));
        z31 = vaddq_f32(z31, vcombine_f32(vld1_f32(bpptr + bpstep*3 + 4), zhalf));
        z40 = vaddq_f32(z40, vld1q_f32(bpptr + bpstep*4));
        z41 = vaddq_f32(z41, vcombine_f32(vld1_f32(bpptr + bpstep*4 + 4), zhalf));
        z50 = vaddq_f32(z50, vld1q_f32(bpptr + bpstep*5));
        z51 = vaddq_f32(z51, vcombine_f32(vld1_f32(bpptr + bpstep*5 + 4), zhalf));
    }

    if (fast_activ) {
        float32x4_t vmax = vdupq_n_f32(maxval);
        float32x4_t valpha = vdupq_n_f32(alpha);
        float32x4_t z = vdupq_n_f32(0.f);
        float32x4_t one = vdupq_n_f32(1.f);

        z00 = vmulq_f32(vminq_f32(z00, vmax), vbslq_f32(vcltq_f32(z00, z), valpha, one));
        z01 = vmulq_f32(vminq_f32(z01, vmax), vbslq_f32(vcltq_f32(z01, z), valpha, one));
        z10 = vmulq_f32(vminq_f32(z10, vmax), vbslq_f32(vcltq_f32(z10, z), valpha, one));
        z11 = vmulq_f32(vminq_f32(z11, vmax), vbslq_f32(vcltq_f32(z11, z), valpha, one));
        z20 = vmulq_f32(vminq_f32(z20, vmax), vbslq_f32(vcltq_f32(z20, z), valpha, one));
        z21 = vmulq_f32(vminq_f32(z21, vmax), vbslq_f32(vcltq_f32(z21, z), valpha, one));
        z30 = vmulq_f32(vminq_f32(z30, vmax), vbslq_f32(vcltq_f32(z30, z), valpha, one));
        z31 = vmulq_f32(vminq_f32(z31, vmax), vbslq_f32(vcltq_f32(z31, z), valpha, one));
        z40 = vmulq_f32(vminq_f32(z40, vmax), vbslq_f32(vcltq_f32(z40, z), valpha, one));
        z41 = vmulq_f32(vminq_f32(z41, vmax), vbslq_f32(vcltq_f32(z41, z), valpha, one));
        z50 = vmulq_f32(vminq_f32(z50, vmax), vbslq_f32(vcltq_f32(z50, z), valpha, one));
        z51 = vmulq_f32(vminq_f32(z51, vmax), vbslq_f32(vcltq_f32(z51, z), valpha, one));
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

#if _FX_NN_ENABLE_FP16
static void
_fx_winograd_BtXB_8x8_f16(const fx_f16* inptr, int inpstep,
                          fx_f16* outptr, int Cg)
{
    float32x4_t x00 = vcvt_f32_f16(vld1_f16(inptr)), x01 = vcvt_f32_f16(vld1_f16(inptr + 4));
    float32x4_t x10 = vcvt_f32_f16(vld1_f16(inptr + inpstep)), x11 = vcvt_f32_f16(vld1_f16(inptr + inpstep + 4));
    float32x4_t x20 = vcvt_f32_f16(vld1_f16(inptr + inpstep*2)), x21 = vcvt_f32_f16(vld1_f16(inptr + inpstep*2 + 4));
    float32x4_t x30 = vcvt_f32_f16(vld1_f16(inptr + inpstep*3)), x31 = vcvt_f32_f16(vld1_f16(inptr + inpstep*3 + 4));
    float32x4_t x40 = vcvt_f32_f16(vld1_f16(inptr + inpstep*4)), x41 = vcvt_f32_f16(vld1_f16(inptr + inpstep*4 + 4));
    float32x4_t x50 = vcvt_f32_f16(vld1_f16(inptr + inpstep*5)), x51 = vcvt_f32_f16(vld1_f16(inptr + inpstep*5 + 4));
    float32x4_t x60 = vcvt_f32_f16(vld1_f16(inptr + inpstep*6)), x61 = vcvt_f32_f16(vld1_f16(inptr + inpstep*6 + 4));
    float32x4_t x70 = vcvt_f32_f16(vld1_f16(inptr + inpstep*7)), x71 = vcvt_f32_f16(vld1_f16(inptr + inpstep*7 + 4));

    float32x4_t z00, z01, z10, z11, z20, z21, z30, z31, z40, z41, z50, z51, z60, z61, z70, z71;

    _FX_WINOGRAD_FWD_8x8()

    const int outstep = _FX_WINO_IBLOCK*_FX_WINO_ATOM_F16*Cg;
    vst1_f16(outptr, vcvt_f16_f32(z00));
    vst1_f16(outptr + 4, vcvt_f16_f32(z01));
    vst1_f16(outptr + outstep, vcvt_f16_f32(z10));
    vst1_f16(outptr + outstep + 4, vcvt_f16_f32(z11));
    vst1_f16(outptr + outstep*2, vcvt_f16_f32(z20));
    vst1_f16(outptr + outstep*2 + 4, vcvt_f16_f32(z21));
    vst1_f16(outptr + outstep*3, vcvt_f16_f32(z30));
    vst1_f16(outptr + outstep*3 + 4, vcvt_f16_f32(z31));
    vst1_f16(outptr + outstep*4, vcvt_f16_f32(z40));
    vst1_f16(outptr + outstep*4 + 4, vcvt_f16_f32(z41));
    vst1_f16(outptr + outstep*5, vcvt_f16_f32(z50));
    vst1_f16(outptr + outstep*5 + 4, vcvt_f16_f32(z51));
    vst1_f16(outptr + outstep*6, vcvt_f16_f32(z60));
    vst1_f16(outptr + outstep*6 + 4, vcvt_f16_f32(z61));
    vst1_f16(outptr + outstep*7, vcvt_f16_f32(z70));
    vst1_f16(outptr + outstep*7 + 4, vcvt_f16_f32(z71));
}

static void
_fx_winograd_AtXA_8x8_f16(const fx_f16* inptr, int inpstep,
                          fx_f16* bpptr, int bpstep, fx_f16* outptr, int outstep,
                          float bias, float maxval, float alpha,
                          bool fast_activ)
{
    float32x4_t x00 = vcvt_f32_f16(vld1_f16(inptr)), x01 = vcvt_f32_f16(vld1_f16(inptr + 4));
    float32x4_t x10 = vcvt_f32_f16(vld1_f16(inptr + inpstep)), x11 = vcvt_f32_f16(vld1_f16(inptr + inpstep + 4));
    float32x4_t x20 = vcvt_f32_f16(vld1_f16(inptr + inpstep*2)), x21 = vcvt_f32_f16(vld1_f16(inptr + inpstep*2 + 4));
    float32x4_t x30 = vcvt_f32_f16(vld1_f16(inptr + inpstep*3)), x31 = vcvt_f32_f16(vld1_f16(inptr + inpstep*3 + 4));
    float32x4_t x40 = vcvt_f32_f16(vld1_f16(inptr + inpstep*4)), x41 = vcvt_f32_f16(vld1_f16(inptr + inpstep*4 + 4));
    float32x4_t x50 = vcvt_f32_f16(vld1_f16(inptr + inpstep*5)), x51 = vcvt_f32_f16(vld1_f16(inptr + inpstep*5 + 4));
    float32x4_t x60 = vcvt_f32_f16(vld1_f16(inptr + inpstep*6)), x61 = vcvt_f32_f16(vld1_f16(inptr + inpstep*6 + 4));
    float32x4_t x70 = vcvt_f32_f16(vld1_f16(inptr + inpstep*7)), x71 = vcvt_f32_f16(vld1_f16(inptr + inpstep*7 + 4));
    float32x4_t z00, z01, z10, z11, z20, z21, z30, z31, z40, z41, z50, z51;

    _FX_WINOGRAD_INV_8x8()

    float16x8_t z0 = vcombine_f16(vcvt_f16_f32(z00), vcvt_f16_f32(z01));
    float16x8_t z1 = vcombine_f16(vcvt_f16_f32(z10), vcvt_f16_f32(z11));
    float16x8_t z2 = vcombine_f16(vcvt_f16_f32(z20), vcvt_f16_f32(z21));
    float16x8_t z3 = vcombine_f16(vcvt_f16_f32(z30), vcvt_f16_f32(z31));
    float16x8_t z4 = vcombine_f16(vcvt_f16_f32(z40), vcvt_f16_f32(z41));
    float16x8_t z5 = vcombine_f16(vcvt_f16_f32(z50), vcvt_f16_f32(z51));

    if (bpptr) {
        z0 = vaddq_f16(z0, vld1q_f16(bpptr));
        z1 = vaddq_f16(z1, vld1q_f16(bpptr + bpstep));
        z2 = vaddq_f16(z2, vld1q_f16(bpptr + bpstep*2));
        z3 = vaddq_f16(z3, vld1q_f16(bpptr + bpstep*3));
        z4 = vaddq_f16(z4, vld1q_f16(bpptr + bpstep*4));
        // try not to read data beyond memory buffer
        float16x8_t bp5 = vcombine_f16(vld1_f16(bpptr + bpstep*5), vdup_n_f16(0.f));
        bp5 = vsetq_lane_f16(bpptr[bpstep*5 + 4], bp5, 4);
        bp5 = vsetq_lane_f16(bpptr[bpstep*5 + 5], bp5, 5);
        z5 = vaddq_f16(z5, bp5);
    }

    if (fast_activ) {
        float16x8_t vmax = vdupq_n_f16(maxval);
        float16x8_t valpha = vdupq_n_f16(alpha);
        float16x8_t z = vdupq_n_f16(0.f);
        float16x8_t one = vdupq_n_f16(1.f);

        z0 = vmulq_f16(vminq_f16(z0, vmax), vbslq_f16(vcltq_f16(z0, z), valpha, one));
        z1 = vmulq_f16(vminq_f16(z1, vmax), vbslq_f16(vcltq_f16(z1, z), valpha, one));
        z2 = vmulq_f16(vminq_f16(z2, vmax), vbslq_f16(vcltq_f16(z2, z), valpha, one));
        z3 = vmulq_f16(vminq_f16(z3, vmax), vbslq_f16(vcltq_f16(z3, z), valpha, one));
        z4 = vmulq_f16(vminq_f16(z4, vmax), vbslq_f16(vcltq_f16(z4, z), valpha, one));
        z5 = vmulq_f16(vminq_f16(z5, vmax), vbslq_f16(vcltq_f16(z5, z), valpha, one));
    }

    vst1_f16(outptr, vget_low_f16(z0));
    outptr[4] = vgetq_lane_f16(z0, 4);
    outptr[5] = vgetq_lane_f16(z0, 5);

    vst1_f16(outptr + outstep, vget_low_f16(z1));
    outptr[outstep + 4] = vgetq_lane_f16(z1, 4);
    outptr[outstep + 5] = vgetq_lane_f16(z1, 5);

    vst1_f16(outptr + outstep*2, vget_low_f16(z2));
    outptr[outstep*2 + 4] = vgetq_lane_f16(z2, 4);
    outptr[outstep*2 + 5] = vgetq_lane_f16(z2, 5);

    vst1_f16(outptr + outstep*3, vget_low_f16(z3));
    outptr[outstep*3 + 4] = vgetq_lane_f16(z3, 4);
    outptr[outstep*3 + 5] = vgetq_lane_f16(z3, 5);

    vst1_f16(outptr + outstep*4, vget_low_f16(z4));
    outptr[outstep*4 + 4] = vgetq_lane_f16(z4, 4);
    outptr[outstep*4 + 5] = vgetq_lane_f16(z4, 5);

    vst1_f16(outptr + outstep*5, vget_low_f16(z5));
    outptr[outstep*5 + 4] = vgetq_lane_f16(z5, 4);
    outptr[outstep*5 + 5] = vgetq_lane_f16(z5, 5);
}
#endif

int _fx_winograd_conv2d(int typ, int ndims, const int_* inpshape, const char* inp,
                        const char* bypass, const int_* outshape, char* out,
                        const struct _fx_conv2d_t* conv, int ntasks,
                        fx_arr_t* curr_scratch_buf, fx_arr_t* new_scratch_buf)
{
    assert(ndims == 4 && inpshape[0] == outshape[0] &&
           outshape[1] == conv->K && inpshape[1] == conv->C);
    size_t esz = fx_elemsize(typ);
    int N = (int)inpshape[0], C = (int)inpshape[1];
    int Hi = (int)inpshape[2], Wi = (int)inpshape[3];
    int K = conv->K, Hk = conv->Hk, Wk = conv->Wk;
    int H0 = (int)outshape[2], W0 = (int)outshape[3];
    int ngroups = conv->ngroups, Cg = C/ngroups, Kg = K/ngroups;
    int Kg_nblocks = (Kg + _FX_WINO_KBLOCK - 1)/_FX_WINO_KBLOCK;
    const size_t inp_planesize = (size_t)Hi*Wi;
    const size_t out_planesize = (size_t)H0*W0;
    float minval = conv->minval, maxval = conv->maxval, alpha = conv->alpha;
    bool fast_activ = conv->activ == _FX_ACTIV_RELU ||
                      conv->activ == _FX_ACTIV_LRELU ||
                      (conv->activ == _FX_ACTIV_CLIP && minval == 0);
    _fx_activ_func_t activ_func = fast_activ || conv->activ == _FX_ACTIV_NONE ? 0 :
                    typ == FX_F32 ? conv->activ_func :
                    typ == FX_F16 ? conv->activ_func_f16 : 0;
    const float* activ_params = conv->activ_params;
    int blocks_per_row = (W0+_FX_WINO_STEP-1)/_FX_WINO_STEP;
    int blocks_per_plane = ((H0+_FX_WINO_STEP-1)/_FX_WINO_STEP)*blocks_per_row;
    int blocks_per_plane_aligned = ((blocks_per_plane +
                _FX_WINO_IBLOCK-1)/_FX_WINO_IBLOCK)*_FX_WINO_IBLOCK;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    int ATOM_SIZE = typ == FX_F16 ? _FX_WINO_ATOM_F16 : _FX_WINO_ATOM_F32;
    char* wbuf_all = 0;
    size_t totalbufsize = (size_t)N*C*blocks_per_plane_aligned*_FX_WINO_AREA*esz;
    int ofstab[9], yxtab[9*2];
    for (int y = 0; y < Hk; y++)
        for( int x = 0; x < Wk; x++) {
            int k = y*Wk + x;
            yxtab[k*2] = y; yxtab[k*2+1] = x;
            ofstab[k] = y*Wi + x;
        }
    if (typ != FX_F32 && typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (typ == FX_F16) {
        maxval = maxval > FLT16_MAX ? FLT16_MAX : maxval;
    }

    if (totalbufsize > curr_scratch_buf->dim[0].step*
                       curr_scratch_buf->dim[0].size) {
        int_ totalbufsz = (int_)totalbufsize;
        int status = fx_make_arr(1, &totalbufsz, 1, 0, 0, 0, new_scratch_buf);
        if (status < 0)
            return status;
    } else {
        fx_copy_arr(curr_scratch_buf, new_scratch_buf);
    }
    wbuf_all = new_scratch_buf->data;

    //printf("ficus nn: fast_activ=%d, activ=%d, maxval=%.2f, alpha=%.2f\n",
    //        (int)fast_activ, conv->activ, maxval, alpha);

    // Phase 1. compute forward Winograd transforms for all input blocks,
    // all input planes, all samples in the batch.
    // [TODO]: maybe, if there are too many input channels, it makes sense to
    // transform only part of input channels at once and then compute the partial
    // accumulated sums (i.e. update the output buffers several times,
    // rather than compute them in one pass).
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        int nc0 = (N*C)*task_id/ntasks;
        int nc1 = (N*C)*(task_id+1)/ntasks;
        for(; nc0 < nc1; nc0++) {
            int n = nc0 / C;
            int c = nc0 - n*C;
            int g = c / Cg;
            c -= g*Cg;
            for (int block_id = 0; block_id < blocks_per_plane; block_id += _FX_WINO_IBLOCK) {
                for (int db = 0; db < _FX_WINO_IBLOCK; db++) {
                    size_t inwofs = ((n*ngroups + g)*blocks_per_plane_aligned +
                        block_id)*Cg*_FX_WINO_AREA +
                        (c*_FX_WINO_IBLOCK + db)*ATOM_SIZE;
                    if (block_id + db < blocks_per_plane) {
                        int y0 = (block_id + db)/ blocks_per_row;
                        int x0 = (block_id + db) - y0 * blocks_per_row;
                        y0 = y0*_FX_WINO_STEP - pad_top;
                        x0 = x0*_FX_WINO_STEP - pad_left;
                        bool partial = y0 < 0 || y0 + _FX_WINO_SIZE > Hi ||
                                       x0 < 0 || x0 + _FX_WINO_SIZE > Wi;
                        int dx1 = 0, dx2 = _FX_WINO_SIZE, dy1 = 0, dy2 = _FX_WINO_SIZE;
                        int inpstep = Wi;
                    #if _FX_NN_ENABLE_FP16
                        if (typ == FX_F16) {
                            fx_f16* inptr0 = (fx_f16*)inp + nc0*inp_planesize + y0*Wi + x0;
                            fx_f16* inptr = inptr0;
                            fx_f16 inpbuf[_FX_WINO_AREA];

                            if (partial) {
                                memset(inpbuf, 0, sizeof(inpbuf));
                                dy1 = -y0 > 0 ? -y0 : 0;
                                dy2 = Hi - y0 < _FX_WINO_SIZE ? Hi - y0 : _FX_WINO_SIZE;
                                if (dy2 < dy1) {dy2 = dy1 = 0;}
                                dx1 = -x0 > 0 ? -x0 : 0;
                                dx2 = Wi - x0 < _FX_WINO_SIZE ? Wi - x0 : _FX_WINO_SIZE;
                                if (dx2 < dx1) {dx2 = dx1 = 0;}
                                inptr0 -= y0*Wi + x0;
                                if (dx1 < dx2 && dy1 < dy2) {
                                    for(int dy = dy1; dy < dy2; dy++)
                                        memcpy(&inpbuf[dy*_FX_WINO_SIZE + dx1],
                                            inptr0 + (y0+dy)*Wi + (x0+dx1),
                                            (dx2-dx1)*sizeof(inpbuf[0]));
                                }
                                inptr = inpbuf;
                                inpstep = _FX_WINO_SIZE;
                            }
                            fx_f16* inwptr = (fx_f16*)wbuf_all + inwofs;
                            _fx_winograd_BtXB_8x8_f16(inptr, inpstep, inwptr, Cg);
                        } else
                    #endif
                        {
                            float inpbuf[_FX_WINO_AREA];
                            float* inptr0 = (float*)inp + nc0*inp_planesize + y0*Wi + x0;
                            float* inptr = inptr0;

                            if (partial) {
                                memset(inpbuf, 0, sizeof(inpbuf));
                                dy1 = -y0 > 0 ? -y0 : 0;
                                dy2 = Hi - y0 < _FX_WINO_SIZE ? Hi - y0 : _FX_WINO_SIZE;
                                if (dy2 < dy1) {dy2 = dy1 = 0;}
                                dx1 = -x0 > 0 ? -x0 : 0;
                                dx2 = Wi - x0 < _FX_WINO_SIZE ? Wi - x0 : _FX_WINO_SIZE;
                                if (dx2 < dx1) {dx2 = dx1 = 0;}
                                inptr0 -= y0*Wi + x0;
                                if (dx1 < dx2 && dy1 < dy2) {
                                    for(int dy = dy1; dy < dy2; dy++)
                                        memcpy(&inpbuf[dy*_FX_WINO_SIZE + dx1],
                                            inptr0 + (y0+dy)*Wi + (x0+dx1),
                                            (dx2-dx1)*sizeof(inpbuf[0]));
                                }
                                inptr = inpbuf;
                                inpstep = _FX_WINO_SIZE;
                            }
                            float* inwptr = (float*)wbuf_all + inwofs;
                            _fx_winograd_BtXB_8x8_f32(inptr, inpstep, inwptr, Cg);
                        }
                    } else {
                    #if _FX_NN_ENABLE_FP16
                        if (typ == FX_F16) {
                            uint16_t* inwptr = (uint16_t*)wbuf_all + inwofs;
                            for (int i = 0; i < _FX_WINO_NATOMS_F16; i++,
                                inwptr += _FX_WINO_IBLOCK*_FX_WINO_ATOM_F16) {
                                inwptr[0] = inwptr[1] = inwptr[2] = inwptr[3] = inwptr[4] = inwptr[5] = inwptr[6] = inwptr[7] = 0;
                            }
                        } else
                    #endif
                        {
                            float* inwptr = (float*)wbuf_all + inwofs;
                            for (int i = 0; i < _FX_WINO_NATOMS_F32; i++,
                                inwptr += _FX_WINO_IBLOCK*_FX_WINO_ATOM_F32) {
                                inwptr[0] = inwptr[1] = inwptr[2] = inwptr[3] = 0.f;
                            }
                        }
                    }
                }
            }
        }
    }

    // Phase 2. compute elemwise-weighted sums of transformed blocks,
    // apply inverse Winograd transforms to the sums,
    // add bias, apply activation function if any and store the results.
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        float out_wbuf[_FX_WINO_AREA*_FX_WINO_KBLOCK*_FX_WINO_IBLOCK], outbuf[_FX_WINO_AREA];
    #if _FX_NN_ENABLE_FP16
        fx_f16* out_wbuf_f16 = (fx_f16*)out_wbuf;
        fx_f16* outbuf_f16 = (fx_f16*)outbuf;
    #endif
        int ngk0 = (int)(((int64_t)N*Kg_nblocks*ngroups)*task_id/ntasks);
        int ngk1 = (int)(((int64_t)N*Kg_nblocks*ngroups)*(task_id+1)/ntasks);
        memset(out_wbuf, 0, sizeof(out_wbuf));
        memset(outbuf, 0, sizeof(outbuf));
        for(; ngk0 < ngk1; ngk0++) {
            int n = ngk0 / (Kg_nblocks*ngroups);
            int gk0 = ngk0 % (Kg_nblocks*ngroups);
            int g = gk0 / Kg_nblocks;
            int k0 = (gk0 % Kg_nblocks)*_FX_WINO_KBLOCK;
            int k1 = k0 + _FX_WINO_KBLOCK <= Kg ? k0 + _FX_WINO_KBLOCK : Kg;

            for (int block_id0 = 0; block_id0 < blocks_per_plane; block_id0 += _FX_WINO_IBLOCK) {
                int block_id1 = block_id0 + _FX_WINO_IBLOCK;
                block_id1 = block_id1 < blocks_per_plane ? block_id1 : blocks_per_plane;
                size_t inwofs = ((n*ngroups + g)*blocks_per_plane_aligned + block_id0)*Cg*_FX_WINO_AREA;
                size_t wofs = k0*Cg*_FX_WINO_AREA;
                #if _FX_NN_ENABLE_FP16
                if (typ == FX_F16) {
                    fx_f16* inwptr = (fx_f16*)wbuf_all + inwofs;
                    const fx_f16* wptr = conv->wf16 + wofs;
                    _fx_winograd_accum_f16(inwptr, wptr, out_wbuf_f16, Cg, block_id1 - block_id0);
                } else
                #endif
                {
                    float* inwptr = (float*)wbuf_all + inwofs;
                    const float* wptr = conv->weights + wofs;
                    _fx_winograd_accum_f32(inwptr, wptr, out_wbuf, Cg, block_id1 - block_id0);
                }

                for (int k = k0; k < k1; k++) {
                    float biasv = conv->bias[g*Kg + k];
                    for (int block_id = block_id0; block_id < block_id1; block_id++) {
                        int y0 = block_id / blocks_per_row;
                        int x0 = block_id - y0 * blocks_per_row;
                        y0 = y0*_FX_WINO_STEP;
                        x0 = x0*_FX_WINO_STEP;
                        int dy1 = H0 - y0;
                        if (dy1 > _FX_WINO_STEP) dy1 = _FX_WINO_STEP;
                        int dx1 = W0 - x0;
                        if (dx1 > _FX_WINO_STEP) dx1 = _FX_WINO_STEP;
                        assert(dx1 > 0 && dy1 > 0);
                        bool partial = activ_func || dy1 < _FX_WINO_STEP || dx1 < _FX_WINO_STEP;
                        size_t outofs = (n*K + g*Kg + k)*out_planesize + y0*W0 + x0;
                        int outstep = W0;

                    #if _FX_NN_ENABLE_FP16
                        if (typ == FX_F16) {
                            fx_f16* outptr0 = (fx_f16*)out + outofs;
                            fx_f16* pbptr0 = bypass ? (fx_f16*)bypass + outofs : 0;
                            fx_f16 *outptr = outptr0, *bpptr = pbptr0;

                            if (partial) {
                                outptr = outbuf_f16;
                                outstep = _FX_WINO_SIZE;
                                if (pbptr0) {
                                    bpptr = outbuf_f16;
                                    for (int y = 0; y < dy1; y++)
                                        memcpy(outbuf_f16 + y*_FX_WINO_SIZE, pbptr0 + y*W0,
                                               dx1*sizeof(pbptr0[0]));
                                }
                            }
                            _fx_winograd_AtXA_8x8_f16(out_wbuf_f16 + ((k - k0)*_FX_WINO_IBLOCK +
                                    (block_id - block_id0))*_FX_WINO_AREA, _FX_WINO_SIZE,
                                    bpptr, outstep, outptr, outstep, biasv, maxval, alpha, fast_activ);
                            if (partial) {
                                if (activ_func)
                                    activ_func(outptr, outptr, _FX_WINO_SIZE*_FX_WINO_STEP, activ_params);
                                for (int y = 0; y < dy1; y++)
                                    memcpy(outptr0 + y*W0, outptr + y*_FX_WINO_SIZE,
                                           dx1*sizeof(outptr0[0]));
                            }
                        } else
                    #endif
                        {
                            float* outptr0 = (float*)out + outofs;
                            float* pbptr0 = bypass ? (float*)bypass + outofs : 0;
                            float *outptr = outptr0, *bpptr = pbptr0;

                            if (partial) {
                                outptr = outbuf;
                                outstep = _FX_WINO_SIZE;
                                if (pbptr0) {
                                    bpptr = outbuf;
                                    for (int y = 0; y < dy1; y++)
                                        memcpy(outbuf + y*_FX_WINO_SIZE, pbptr0 + y*W0,
                                               dx1*sizeof(pbptr0[0]));
                                }
                            }
                            _fx_winograd_AtXA_8x8_f32(out_wbuf + ((k - k0)*_FX_WINO_IBLOCK +
                                    (block_id - block_id0))*_FX_WINO_AREA, _FX_WINO_SIZE,
                                    bpptr, outstep, outptr, outstep, biasv, maxval, alpha, fast_activ);
                            if (partial) {
                                if (activ_func)
                                    activ_func(outptr, outptr, _FX_WINO_SIZE*_FX_WINO_STEP, activ_params);
                                for (int y = 0; y < dy1; y++)
                                    memcpy(outptr0 + y*W0, outptr + y*_FX_WINO_SIZE,
                                           dx1*sizeof(outptr0[0]));
                            }
                        }
                    }
                }
            }
        }
    }

    return FX_OK;
}
#endif
}
