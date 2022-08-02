/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// The implementation is derived (but then heavily rewritten)
// from BLISLAB code (https://github.com/flame/blislab).
// Below is the original copyright and the license

/*
 * --------------------------------------------------------------------------
 * BLISLAB
 * --------------------------------------------------------------------------
 * Copyright (C) 2016, The University of Texas at Austin
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *  - Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *  - Neither the name of The University of Texas nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *
 * bl_sgemm.c
 * */

#define _FX_GEMM_STORAGE (1<<20)
#define _FX_GEMM_MAX_STACKBUF (1 << 14)

#define _FX_SGEMM_MC 64
#define _FX_SGEMM_NC 240
#define _FX_SGEMM_VOL (1<<18)
#define _FX_SGEMM_MR 8
#define _FX_SGEMM_NR 12

#define _FX_DGEMM_MC 48
#define _FX_DGEMM_NC 120
#define _FX_DGEMM_VOL (1<<17)
#define _FX_DGEMM_MR 8
#define _FX_DGEMM_NR 6

#define _FX_GEMM_IMPLEMENT_PACK(N, suffix, styp, dtyp) \
static void _fx_gemm_pack##N##suffix( int_ m, int_ k, const void* A_, \
                                      int_ lda0, int_ lda1, void* packA_ ) \
{ \
    const styp* A = (const styp*)A_; \
    dtyp* packA = (dtyp*)packA_; \
    for( int_ i = 0; i < m; i += N ) { \
        if (i + N-1 < m) { \
            const styp* a_ptr = A + lda0*i; \
            for( int_ j = 0; j < k*lda1; packA += N, j += lda1 ) \
            { \
                _FX_GEMM_LOAD_TO_BUF_##N(styp); \
                _FX_GEMM_PACK##suffix##_##N(buf, packA); \
            } \
        } else { \
            const styp* a_ptr[N]; \
            for (int k = 0; k < N; k++) a_ptr[k] = A + lda0*(i+k < m ? i+k : i); \
            for( int_ j = 0; j < k*lda1; packA += N, j += lda1 ) \
            { \
                _FX_GEMM_LOAD_TO_BUF_BORDERS_##N(styp); \
                _FX_GEMM_PACK##suffix##_##N(buf, packA); \
            } \
        } \
    } \
}

#define _FX_GEMM_LOAD_TO_BUF_6(styp) \
    styp buf[] = { \
        a_ptr[j], a_ptr[j+lda0], a_ptr[j+lda0*2], a_ptr[j+lda0*3], \
        a_ptr[j+lda0*4], a_ptr[j+lda0*5] }

#define _FX_GEMM_LOAD_TO_BUF_BORDERS_6(styp) \
    styp buf[] = { \
        a_ptr[0][j], a_ptr[1][j], a_ptr[2][j], a_ptr[3][j], \
        a_ptr[4][j], a_ptr[5][j] }

#define _FX_GEMM_LOAD_TO_BUF_8(styp) \
    styp buf[] = { \
        a_ptr[j], a_ptr[j+lda0], a_ptr[j+lda0*2], a_ptr[j+lda0*3], \
        a_ptr[j+lda0*4], a_ptr[j+lda0*5], a_ptr[j+lda0*6], a_ptr[j+lda0*7] }

#define _FX_GEMM_LOAD_TO_BUF_BORDERS_8(styp) \
    styp buf[] = { \
        a_ptr[0][j], a_ptr[1][j], a_ptr[2][j], a_ptr[3][j], \
        a_ptr[4][j], a_ptr[5][j], a_ptr[6][j], a_ptr[7][j] }

#define _FX_GEMM_LOAD_TO_BUF_12(styp) \
    styp buf[] = { \
        a_ptr[j], a_ptr[j+lda0], a_ptr[j+lda0*2], a_ptr[j+lda0*3], \
        a_ptr[j+lda0*4], a_ptr[j+lda0*5], a_ptr[j+lda0*6], a_ptr[j+lda0*7], \
        a_ptr[j+lda0*8], a_ptr[j+lda0*9], a_ptr[j+lda0*10], a_ptr[j+lda0*11] }

#define _FX_GEMM_LOAD_TO_BUF_BORDERS_12(styp) \
    styp buf[] = { \
        a_ptr[0][j], a_ptr[1][j], a_ptr[2][j], a_ptr[3][j], \
        a_ptr[4][j], a_ptr[5][j], a_ptr[6][j], a_ptr[7][j], \
        a_ptr[8][j], a_ptr[9][j], a_ptr[10][j], a_ptr[11][j] }

#define _FX_GEMM_PACK_COPY(src, dst, N) \
    memcpy((dst), (src), N*sizeof(src[0]))
#define _FX_GEMM_PACK_f64_6(src, dst) _FX_GEMM_PACK_COPY((src), (dst), 6)
#define _FX_GEMM_PACK_f32_8(src, dst) _FX_GEMM_PACK_COPY((src), (dst), 8)
#define _FX_GEMM_PACK_f64_8(src, dst) _FX_GEMM_PACK_COPY((src), (dst), 8)
#define _FX_GEMM_PACK_f32_12(src, dst) _FX_GEMM_PACK_COPY((src), (dst), 12)

#define _FX_GEMM_PACK_TO_DOUBLE(src, dst, N) \
    for (int k = 0; k < N; k++) (dst)[k] = (double)((src)[k])
#define _FX_GEMM_PACK_f32f64_6(src, dst) _FX_GEMM_PACK_TO_DOUBLE(src, dst, 6)
#define _FX_GEMM_PACK_f32f64_8(src, dst) _FX_GEMM_PACK_TO_DOUBLE(src, dst, 8)

#if defined FX_SIMD_NEON && FX_SIMD_NEON

#define _FX_GEMM_PACK_f16f32_8(src, dst) \
    float16x8_t x0 = vld1q_f16((src)); \
    float32x4_t y0 = vcvt_f32_f16(vget_low_f16(x0)); \
    float32x4_t y1 = vcvt_f32_f16(vget_high_f16(x0)); \
    vst1q_f32((dst), y0); vst1q_f32((dst) + 4, y1)

#define _FX_GEMM_PACK_f16f32_12(src, dst) \
    float16x8_t x0 = vld1q_f16((src)); \
    float16x4_t x1 = vld1_f16((src) + 8); \
    float32x4_t y0 = vcvt_f32_f16(vget_low_f16(x0)); \
    float32x4_t y1 = vcvt_f32_f16(vget_high_f16(x0)); \
    float32x4_t y2 = vcvt_f32_f16(x1); \
    vst1q_f32((dst), y0); \
    vst1q_f32((dst) + 4, y1); \
    vst1q_f32((dst) + 8, y2)

#define _FX_GEMM_PACK_f32f16_8(src, dst) \
    float32x4_t x0 = vld1q_f32((src)); \
    float32x4_t x1 = vld1q_f32((src) + 4); \
    float16x8_t y0 = vcombine_f16(vcvt_f16_f32(x0), vcvt_f16_f32(x1)); \
    vst1q_f16((dst), y0)

#elif defined FX_SIMD_AVX2 && FX_SIMD_AVX2

#define _FX_GEMM_PACK_f16f32_8(src, dst) \
    __m128i x0 = _mm_loadu_si128((const __m128i*)(src)); \
    __m128 y0 = _mm_cvtph_ps(x0); \
    __m128 y1 = _mm_cvtph_ps(_mm_unpackhi_epi64(x0, x0)); \
    _mm_storeu_ps((dst), y0); \
    _mm_storeu_ps((dst) + 4, y1)

#define _FX_GEMM_PACK_f16f32_12(src, dst) \
    __m128i x0 = _mm_loadu_si128((const __m128i*)(src)); \
    __m128i x1 = _mm_loadl_epi64((const __m128i*)((src) + 8)); \
    __m128 y0 = _mm_cvtph_ps(x0); \
    __m128 y1 = _mm_cvtph_ps(_mm_unpackhi_epi64(x0, x0)); \
    __m128 y2 = _mm_cvtph_ps(x1); \
    _mm_storeu_ps((dst), y0); \
    _mm_storeu_ps((dst) + 4, y1); \
    _mm_storeu_ps((dst) + 8, y2)

#define _FX_GEMM_PACK_f32f16_8(src, dst) \
    __m128i x0 = _mm_loadu_ps((src)); \
    __m128i x1 = _mm_loadu_ps((src) + 4); \
    __m128i y0 = _mm_cvtps_ph(x0, 0); \
    __m128i y1 = _mm_cvtps_ph(x1, 0); \
    _mm_storeu_si128((const __m128i*)(dst), _mm_unpacklo_epi64(y0, y1))

#else

#define _FX_GEMM_PACK_TO_FLOAT(src, dst, N) \
    for (int k = 0; k < N; k++) (dst)[k] = FX_FLOAT((src)[k])

#define _FX_GEMM_PACK_TO_FLOAT16(src, dst, N) \
    for (int k = 0; k < N; k++) (dst)[k] = FX_FLOAT16((src)[k])

#define _FX_GEMM_PACK_f16f32_8(src, dst) _FX_GEMM_PACK_TO_FLOAT((src), (dst), 8)
#define _FX_GEMM_PACK_f16f32_12(src, dst) _FX_GEMM_PACK_TO_FLOAT((src), (dst), 12)
#define _FX_GEMM_PACK_f32f16_8(src, dst) _FX_GEMM_PACK_TO_FLOAT16((src), (dst), 8)

#endif

_FX_GEMM_IMPLEMENT_PACK(6, _f64, double, double)
_FX_GEMM_IMPLEMENT_PACK(8, _f32, float, float)
_FX_GEMM_IMPLEMENT_PACK(8, _f64, double, double)
_FX_GEMM_IMPLEMENT_PACK(12, _f32, float, float)

_FX_GEMM_IMPLEMENT_PACK(8, _f16f32, fx_f16, float)
_FX_GEMM_IMPLEMENT_PACK(12, _f16f32, fx_f16, float)

_FX_GEMM_IMPLEMENT_PACK(6, _f32f64, float, double)
_FX_GEMM_IMPLEMENT_PACK(8, _f32f64, float, double)

typedef void (*_fx_gemm_packer_t)(int_ m, int_ k, const void* A_,
                                  int_ lda0, int_ lda1, void* packA_);

static void fx_gemm8x12_f32(int k, const char *a_, const char *b_,
                            char *c_, int ldc, const void* palpha)
{
    const float* a = (const float*)a_;
    const float* b = (const float*)b_;
    float* c = (float*)c_;
    float alpha = *(const float*)palpha;

#if defined FX_SIMD_NEON && FX_SIMD_NEON
    float32x4_t s00 = vdupq_n_f32(0.f), s01 = s00, s02 = s00;
    float32x4_t s10 = s00, s11 = s00, s12 = s00;
    float32x4_t s20 = s00, s21 = s00, s22 = s00;
    float32x4_t s30 = s00, s31 = s00, s32 = s00;
    float32x4_t s40 = s00, s41 = s00, s42 = s00;
    float32x4_t s50 = s00, s51 = s00, s52 = s00;
    float32x4_t s60 = s00, s61 = s00, s62 = s00;
    float32x4_t s70 = s00, s71 = s00, s72 = s00;

    for( int p = 0; p < k; p++, a += _FX_SGEMM_MR, b += _FX_SGEMM_NR )
    {
        float32x4_t a0 = vld1q_f32(a);
        float32x4_t b0 = vld1q_f32(b), b1 = vld1q_f32(b + 4), b2 = vld1q_f32(b + 8);

        s00 = vfmaq_laneq_f32(s00, b0, a0, 0);
        s01 = vfmaq_laneq_f32(s01, b1, a0, 0);
        s02 = vfmaq_laneq_f32(s02, b2, a0, 0);
        s10 = vfmaq_laneq_f32(s10, b0, a0, 1);
        s11 = vfmaq_laneq_f32(s11, b1, a0, 1);
        s12 = vfmaq_laneq_f32(s12, b2, a0, 1);

        s20 = vfmaq_laneq_f32(s20, b0, a0, 2);
        s21 = vfmaq_laneq_f32(s21, b1, a0, 2);
        s22 = vfmaq_laneq_f32(s22, b2, a0, 2);
        s30 = vfmaq_laneq_f32(s30, b0, a0, 3);
        s31 = vfmaq_laneq_f32(s31, b1, a0, 3);
        s32 = vfmaq_laneq_f32(s32, b2, a0, 3);

        a0 = vld1q_f32(a + 4);

        s40 = vfmaq_laneq_f32(s40, b0, a0, 0);
        s41 = vfmaq_laneq_f32(s41, b1, a0, 0);
        s42 = vfmaq_laneq_f32(s42, b2, a0, 0);
        s50 = vfmaq_laneq_f32(s50, b0, a0, 1);
        s51 = vfmaq_laneq_f32(s51, b1, a0, 1);
        s52 = vfmaq_laneq_f32(s52, b2, a0, 1);

        s60 = vfmaq_laneq_f32(s60, b0, a0, 2);
        s61 = vfmaq_laneq_f32(s61, b1, a0, 2);
        s62 = vfmaq_laneq_f32(s62, b2, a0, 2);
        s70 = vfmaq_laneq_f32(s70, b0, a0, 3);
        s71 = vfmaq_laneq_f32(s71, b1, a0, 3);
        s72 = vfmaq_laneq_f32(s72, b2, a0, 3);
    }

    float32x4_t c0, c1, c2, c3, c4, c5, valpha = vdupq_n_f32(alpha);
    #define _FX_SGEMM_FINIT(row0, row1) \
        c0 = vld1q_f32(c + row0*ldc); \
        c1 = vld1q_f32(c + row0*ldc + 4); \
        c2 = vld1q_f32(c + row0*ldc + 8); \
        c3 = vld1q_f32(c + row1*ldc); \
        c4 = vld1q_f32(c + row1*ldc + 4); \
        c5 = vld1q_f32(c + row1*ldc + 8); \
        c0 = vfmaq_f32(c0, s##row0##0, valpha); \
        c1 = vfmaq_f32(c1, s##row0##1, valpha); \
        c2 = vfmaq_f32(c2, s##row0##2, valpha); \
        c3 = vfmaq_f32(c3, s##row1##0, valpha); \
        c4 = vfmaq_f32(c4, s##row1##1, valpha); \
        c5 = vfmaq_f32(c5, s##row1##2, valpha); \
        vst1q_f32(c + row0*ldc, c0); \
        vst1q_f32(c + row0*ldc + 4, c1); \
        vst1q_f32(c + row0*ldc + 8, c2); \
        vst1q_f32(c + row1*ldc, c3); \
        vst1q_f32(c + row1*ldc + 4, c4); \
        vst1q_f32(c + row1*ldc + 8, c5)

    _FX_SGEMM_FINIT(0, 1);
    _FX_SGEMM_FINIT(2, 3);
    _FX_SGEMM_FINIT(4, 5);
    _FX_SGEMM_FINIT(6, 7);
#else
    // we give a chance to compiler to vectorize the loop by storing
    // intermediate sums in local buffer with compile-time-constant size.
    float sbuf[_FX_SGEMM_MR*_FX_SGEMM_NR];
    memset(sbuf, 0, sizeof(sbuf));
    for( int p = 0; p < k; p++ ) {
        for( int i = 0; i < _FX_SGEMM_MR; i++ ) {
            float ai = a[_FX_SGEMM_MR*p + i];
            for( int j = 0; j < _FX_SGEMM_NR; j++ )
                sbuf[i*_FX_SGEMM_NR+j] += b[_FX_SGEMM_NR*p + j]*ai;
        }
    }
    for (int i = 0; i < _FX_SGEMM_MR; i++) {
        for (int j = 0; j < _FX_SGEMM_NR; j++)
            c[i*ldc + j] += alpha*sbuf[i*_FX_SGEMM_NR+j];
    }
#endif
}

static void fx_gemm8x6_f64(int_ k, const char *a_, const char *b_,
                           char *c_, int_ ldc, const void* palpha)
{
    const double* a = (const double*)a_;
    const double* b = (const double*)b_;
    double* c = (double*)c_;
    double alpha = *(const double*)palpha;

#if defined FX_SIMD_NEON && FX_SIMD_NEON
    float64x2_t s00 = vdupq_n_f64(0.), s01 = s00, s02 = s00;
    float64x2_t s10 = s00, s11 = s00, s12 = s00;
    float64x2_t s20 = s00, s21 = s00, s22 = s00;
    float64x2_t s30 = s00, s31 = s00, s32 = s00;
    float64x2_t s40 = s00, s41 = s00, s42 = s00;
    float64x2_t s50 = s00, s51 = s00, s52 = s00;
    float64x2_t s60 = s00, s61 = s00, s62 = s00;
    float64x2_t s70 = s00, s71 = s00, s72 = s00;

    for( int_ p = 0; p < k; p++, a += _FX_DGEMM_MR, b += _FX_DGEMM_NR )
    {
        float64x2_t a0 = vld1q_f64(a);
        float64x2_t b0 = vld1q_f64(b), b1 = vld1q_f64(b + 2), b2 = vld1q_f64(b + 4);

        s00 = vfmaq_laneq_f64(s00, b0, a0, 0);
        s01 = vfmaq_laneq_f64(s01, b1, a0, 0);
        s02 = vfmaq_laneq_f64(s02, b2, a0, 0);
        s10 = vfmaq_laneq_f64(s10, b0, a0, 1);
        s11 = vfmaq_laneq_f64(s11, b1, a0, 1);
        s12 = vfmaq_laneq_f64(s12, b2, a0, 1);

        a0 = vld1q_f64(a + 2);

        s20 = vfmaq_laneq_f64(s20, b0, a0, 0);
        s21 = vfmaq_laneq_f64(s21, b1, a0, 0);
        s22 = vfmaq_laneq_f64(s22, b2, a0, 0);
        s30 = vfmaq_laneq_f64(s30, b0, a0, 1);
        s31 = vfmaq_laneq_f64(s31, b1, a0, 1);
        s32 = vfmaq_laneq_f64(s32, b2, a0, 1);

        a0 = vld1q_f64(a + 4);

        s40 = vfmaq_laneq_f64(s40, b0, a0, 0);
        s41 = vfmaq_laneq_f64(s41, b1, a0, 0);
        s42 = vfmaq_laneq_f64(s42, b2, a0, 0);
        s50 = vfmaq_laneq_f64(s50, b0, a0, 1);
        s51 = vfmaq_laneq_f64(s51, b1, a0, 1);
        s52 = vfmaq_laneq_f64(s52, b2, a0, 1);

        a0 = vld1q_f64(a + 6);

        s60 = vfmaq_laneq_f64(s60, b0, a0, 0);
        s61 = vfmaq_laneq_f64(s61, b1, a0, 0);
        s62 = vfmaq_laneq_f64(s62, b2, a0, 0);
        s70 = vfmaq_laneq_f64(s70, b0, a0, 1);
        s71 = vfmaq_laneq_f64(s71, b1, a0, 1);
        s72 = vfmaq_laneq_f64(s72, b2, a0, 1);
    }

    float64x2_t c0, c1, c2, c3, c4, c5, valpha = vdupq_n_f64(alpha);
    #define _FX_DGEMM_FINIT(row0, row1) \
        c0 = vld1q_f64(c + row0*ldc); \
        c1 = vld1q_f64(c + row0*ldc + 2); \
        c2 = vld1q_f64(c + row0*ldc + 4); \
        c3 = vld1q_f64(c + row1*ldc); \
        c4 = vld1q_f64(c + row1*ldc + 2); \
        c5 = vld1q_f64(c + row1*ldc + 4); \
        c0 = vfmaq_f64(c0, s##row0##0, valpha); \
        c1 = vfmaq_f64(c1, s##row0##1, valpha); \
        c2 = vfmaq_f64(c2, s##row0##2, valpha); \
        c3 = vfmaq_f64(c3, s##row1##0, valpha); \
        c4 = vfmaq_f64(c4, s##row1##1, valpha); \
        c5 = vfmaq_f64(c5, s##row1##2, valpha); \
        vst1q_f64(c + row0*ldc, c0); \
        vst1q_f64(c + row0*ldc + 2, c1); \
        vst1q_f64(c + row0*ldc + 4, c2); \
        vst1q_f64(c + row1*ldc, c3); \
        vst1q_f64(c + row1*ldc + 2, c4); \
        vst1q_f64(c + row1*ldc + 4, c5)

    _FX_DGEMM_FINIT(0, 1);
    _FX_DGEMM_FINIT(2, 3);
    _FX_DGEMM_FINIT(4, 5);
    _FX_DGEMM_FINIT(6, 7);
#else
    // we give a chance to compiler to vectorize the loop by storing
    // intermediate sums in local buffer with compile-time-constant size.
    double sbuf[_FX_DGEMM_MR*_FX_DGEMM_NR];
    memset(sbuf, 0, sizeof(sbuf));
    for( int_ p = 0; p < k; p++ ) {
        for( int_ i = 0; i < _FX_DGEMM_MR; i++ ) {
            double ai = a[_FX_DGEMM_MR*p + i];
            for( int_ j = 0; j < _FX_DGEMM_NR; j++ )
                sbuf[i*_FX_DGEMM_NR+j] += b[_FX_DGEMM_NR*p + j]*ai;
        }
    }
    for (int_ i = 0; i < _FX_DGEMM_MR; i++) {
        for (int_ j = 0; j < _FX_DGEMM_NR; j++)
            c[i*ldc + j] += alpha*sbuf[i*_FX_DGEMM_NR+j];
    }
#endif
}

static int fx_gemm_thin(double alpha, double beta, int_ M, int_ N, int_ K,
                        int a_typ, const char* a_, int_ lda0, int_ lda1,
                        int b_typ, const char* b_, int_ ldb,
                        int c_typ, char* c_, int_ ldc, int num_threads)
{
    float alphaf = (float)alpha, betaf = (float)beta;
    int_ nsubtasks = 1, Nblocks = 1;
    if (num_threads > 1 && (uint64_t)M*N*K >= 100000) {
        if (M < num_threads)
            Nblocks = num_threads/M;
    } else {
        num_threads = 1;
    }
    nsubtasks = M*Nblocks;
    if (a_typ == FX_F32 && b_typ == FX_F32 && c_typ == FX_F32) {
        const float* a = (const float*)a_;
        #pragma omp parallel for num_threads(num_threads)
        for (int_ task_id = 0; task_id < num_threads; task_id++)
        {
            int_ tile0 = task_id*nsubtasks/num_threads, tile1 = (task_id + 1)*nsubtasks/num_threads;
            for( ; tile0 < tile1; tile0++ ) {
                int_ i = tile0/Nblocks;
                int_ nb = tile0 - i*Nblocks;
                int_ j0 = nb*N/Nblocks, j1 = (nb+1)*N/Nblocks;
                int_ j, k;
                float* c_i = (float*)c_ + i*ldc;
                if (betaf == 0.f)
                    for( j = j0; j < j1; j++ ) c_i[j] = 0.f;
                else if (betaf != 1.f)
                    for( j = j0; j < j1; j++ ) c_i[j] *= betaf;
                for( k = 0; k < K; k++ ) {
                    const float* b_k = (const float*)b_ + k*ldb;
                    float aval = alphaf*a[i*lda0 + k*lda1];
                    for( j = j0; j < j1; j++ )
                        c_i[j] += aval*b_k[j];
                }
            }
        }
    } else if (c_typ == FX_F16 || c_typ == FX_F32) {
        if ((a_typ != FX_F32 && a_typ != FX_F16) ||
            (b_typ != FX_F32 && b_typ != FX_F16))
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        #pragma omp parallel for num_threads(num_threads)
        for (int_ task_id = 0; task_id < num_threads; task_id++) {
            int_ tile0 = task_id*nsubtasks/num_threads, tile1 = (task_id + 1)*nsubtasks/num_threads;
            for( ; tile0 < tile1; tile0++ ) {
                int_ i = tile0/Nblocks;
                int_ nb = tile0 - i*Nblocks;
                int_ j0 = nb*N/Nblocks, j1 = (nb+1)*N/Nblocks, Nt = j1 - j0;
                int_ j, k;
                float* cbuf = (float*)alloca(Nt*sizeof(cbuf[0]));
                fx_f16* cf16_i = (fx_f16*)c_ + i*ldc + j0;
                float* c_i = c_typ == FX_F16 ? cbuf : (float*)c_ + i*ldc + j0;
                if (betaf == 0.f)
                    for( j = 0; j < Nt; j++ ) c_i[j] = 0.f;
                else if (c_i == cbuf) {
                    j = 0;
                    for( ; j <= Nt - 8; j += 8 ) {
                        float buf[8];
                        _FX_GEMM_PACK_f16f32_8(cf16_i + j, buf);
                        for (k = 0; k < 8; k++)
                            c_i[j + k] = buf[k]*betaf;
                    }
                    for (; j < Nt; j++)
                        c_i[j] = FX_FLOAT(cf16_i[j])*betaf;
                } else if (betaf != 1.f) {
                    for( j = 0; j < Nt; j++ ) c_i[j] *= betaf;
                }
                for( k = 0; k < K; k++ ) {
                    float aval = alphaf*(a_typ == FX_F32 ? *((const float*)a_ + i*lda0 + k*lda1) :
                                                 FX_FLOAT(*((const fx_f16*)a_ + i*lda0 + k*lda1)));
                    if (b_typ == FX_F32) {
                        const float* b_k = (const float*)b_ + k*ldb + j0;
                        for( j = 0; j < Nt; j++ )
                            c_i[j] += aval*b_k[j];
                    } else {
                        const fx_f16* b_k = (const fx_f16*)b_ + k*ldb + j0;
                        j = 0;
                    #if defined FX_SIMD_NEON && FX_SIMD_NEON
                        float32x4_t va = vdupq_n_f32(aval);
                        for (; j <= Nt - 8; j += 8) {
                            float16x8_t bval = vld1q_f16(b_k + j);
                            float32x4_t b0 = vcvt_f32_f16(vget_low_f16(bval));
                            float32x4_t b1 = vcvt_f32_f16(vget_high_f16(bval));
                            float32x4_t c0 = vld1q_f32(c_i + j);
                            float32x4_t c1 = vld1q_f32(c_i + j + 4);
                            c0 = vfmaq_f32(c0, b0, va);
                            c1 = vfmaq_f32(c1, b1, va);
                            vst1q_f32(c_i + j, c0);
                            vst1q_f32(c_i + j + 4, c1);
                        }
                    #elif defined FX_SIMD_AVX2 && FX_SIMD_AVX2
                        __m128 va = _mm_set1_ps(aval);
                        for (; j <= Nt - 8; j += 8) {
                            __m128i bval = _mm_loadu_si128((const __m128i*)(b_k + j));
                            __m128 b0 = _mm_cvtph_ps(bval);
                            __m128 b1 = _mm_cvtph_ps(_mm_unpackhi_epi64(bval, bval));
                            __m128 c0 = _mm_loadu_ps(c_i + j);
                            __m128 c1 = _mm_loadu_ps(c_i + j + 4);
                            c0 = _mm_fmadd_ps(b0, va, c0);
                            c1 = _mm_fmadd_ps(b1, va, c1);
                            _mm_storeu_ps(c_i + j, c0);
                            _mm_storeu_ps(c_i + j + 4, c1);
                        }
                    #endif
                        for (; j < Nt; j++) {
                            float bval = FX_FLOAT(b_k[j]);
                            c_i[j] += aval*bval;
                        }
                    }
                }
                if (c_i == cbuf) {
                    int j = 0;
                    for( ; j <= Nt - 8; j += 8 ) {
                        _FX_GEMM_PACK_f32f16_8(c_i + j, cf16_i + j);
                    }
                    for (; j < Nt; j++)
                        cf16_i[j] = FX_FLOAT16(c_i[j]);
                }
            }
        }
    } else if (c_typ == FX_F64) {
        if ((a_typ != FX_F32 && a_typ != FX_F64) ||
            (b_typ != FX_F32 && b_typ != FX_F64))
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        #pragma omp parallel for num_threads(num_threads)
        for (int_ task_id = 0; task_id < num_threads; task_id++) {
            int_ tile0 = task_id*nsubtasks/num_threads, tile1 = (task_id + 1)*nsubtasks/num_threads;
            for( ; tile0 < tile1; tile0++ ) {
                int_ i = tile0/Nblocks;
                int_ nb = tile0 - i*Nblocks;
                int_ j0 = nb*N/Nblocks, j1 = (nb+1)*N/Nblocks;
                int_ j, k;
                double* c_i = (double*)c_ + i*ldc;
                if (beta == 0.f)
                    for( j = j0; j < j1; j++ ) c_i[j] = 0.;
                else if (beta != 1.f)
                    for( j = j0; j < j1; j++ ) c_i[j] *= beta;
                for( k = 0; k < K; k++ ) {
                    double aval = alpha*(a_typ == FX_F32 ? (double)*((const float*)a_ + i*lda0 + k*lda1) :
                                                                  *((const double*)a_ + i*lda0 + k*lda1));
                    if (b_typ == FX_F32) {
                        const float* b_k = (const float*)b_ + k*ldb;
                        for( j = j0; j < j1; j++ )
                            c_i[j] += aval*b_k[j];
                    } else {
                        const double* b_k = (const double*)b_ + k*ldb;
                        for( j = j0; j < j1; j++ )
                            c_i[j] += aval*b_k[j];
                    }
                }
            }
        }
    } else {
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }
    return FX_OK;
}

static void fx_gemm_macro_kernel( int typ, int_ m, int_ n, int_ k,
                                  const char *packA, const char *packB,
                                  const void* palpha, char *c, int_ ldc0,
                                  int_ MR, int_ NR )
{
    int_ esz = fx_elemsize(typ);
    int_ ldc0_esz = ldc0*esz;
    assert(typ == FX_F32 || typ == FX_F64);

    double tempC[_FX_SGEMM_MR*_FX_SGEMM_NR]; // make sure the buffer is big enough
    for( int_ i = 0; i < m; i += MR ) {
        for( int_ j = 0; j < n; j += NR ) {
            char* cptr0 = &c[i * ldc0_esz + j * esz];
            char* cptr = cptr0;
            int_ ldc = ldc0;
            int_ mr = m - i < MR ? m - i : MR;
            int_ nr = n - j < NR ? n - j : NR;
            int_ nr_esz = nr*esz;
            bool partial = (bool)((mr < MR) | (nr < NR));
            if (partial) {
                memset(tempC, 0, sizeof(tempC));
                cptr = (char*)tempC;
                ldc = NR;
                for(int_ p = 0; p < mr; p++)
                    memcpy(cptr + p*(ldc*esz), cptr0 + p*ldc0_esz, nr_esz);
            }

            if (typ == FX_F32)
                fx_gemm8x12_f32(k, packA + i * k * esz, packB + j * k * esz, cptr, ldc, palpha);
            else
                fx_gemm8x6_f64(k, packA + i * k * esz, packB + j * k * esz, cptr, ldc, palpha);
            if (partial) {
                for(int_ p = 0; p < mr; p++)
                    memcpy(cptr0 + p*ldc0_esz, cptr + p*(ldc*esz), nr_esz);
            }
        }
    }
}


int fx_mpgemm( bool tA, bool tB, double alpha, double beta,
               int_ ma, int_ na, int a_typ, const void *a_, int_ lda0, int_ lda1,
               int_ mb, int_ nb, int b_typ, const void *b_, int_ ldb0, int_ ldb1,
               int c_typ, void *c_, int_ ldc, int num_threads )
{
    const char* a = (const char*)a_;
    const char* b = (const char*)b_;
    char* c = (char*)c_;

    num_threads = num_threads <= 0 ? fx_get_max_threads() : num_threads;
    num_threads = num_threads <= 0 ? 1 : num_threads;
    int_ t, M = tA ? na : ma, N = tB ? mb : nb, K = tA ? ma : na;

    if (tA) FX_SWAP(lda0, lda1, t);
    if (tB) FX_SWAP(ldb0, ldb1, t);

    if (!(ma > 0 && na > 0 && mb > 0 && nb > 0 && a && b && c && ldc >= N))
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);

    if (!tB && ldb1 == 1 && (M <= 4 || (uint64_t)M*N*K <= 10000))
        return fx_gemm_thin(alpha, beta, M, N, K, a_typ, a, lda0, lda1,
                            b_typ, b, ldb0, c_typ, c, ldc, num_threads);

    {
    float alphaf = (float)alpha, betaf = (float)beta;
    _fx_gemm_packer_t a_packer, b_packer;
    int w_typ = c_typ != FX_F16 ? c_typ : FX_F32;
    const void* palpha = w_typ == FX_F64 ?
        (const void*)&alpha : (const void*)&alphaf;
    int_ a_esz = fx_elemsize(a_typ), b_esz = fx_elemsize(b_typ);
    int_ c_esz = fx_elemsize(c_typ), w_esz = fx_elemsize(w_typ);
    int_ GEMM_MC, GEMM_NC, GEMM_VOL, GEMM_MR, GEMM_NR;
    if (w_typ == FX_F64) {
        GEMM_MC = _FX_DGEMM_MC;
        GEMM_NC = _FX_DGEMM_NC;
        GEMM_VOL = _FX_DGEMM_VOL;
        GEMM_MR = _FX_DGEMM_MR;
        GEMM_NR = _FX_DGEMM_NR;
    } else {
        GEMM_MC = _FX_SGEMM_MC;
        GEMM_NC = _FX_SGEMM_NC;
        GEMM_VOL = _FX_SGEMM_VOL;
        GEMM_MR = _FX_SGEMM_MR;
        GEMM_NR = _FX_SGEMM_NR;
    }

    int_ MC = (((GEMM_MC < M ? GEMM_MC : M) + GEMM_MR-1) / GEMM_MR) * GEMM_MR;
    int_ NC = (((GEMM_NC < N ? GEMM_NC : N) + GEMM_NR-1) / GEMM_NR) * GEMM_NR;
    int_ KC = _FX_GEMM_STORAGE/((MC+NC)*w_esz);
    KC = KC > 8 ? KC : 8;
    KC = KC < K ? KC : K;

    size_t bufsize = (KC*(MC+NC) + MC*NC*(c_typ == FX_F16))*w_esz;
    bool use_stackbuf = bufsize <= _FX_GEMM_MAX_STACKBUF;
    int_ m_tiles = (M + MC - 1)/MC;
    int_ n_tiles = (N + NC - 1)/NC;
    int_ total_tiles = m_tiles * n_tiles;
    int_ ntasks = (int_)num_threads < total_tiles ?
            (int_)num_threads : total_tiles;
    if ((uint64_t)total_tiles*K < 10000)
        ntasks = 1;

    a_packer =
        a_typ == FX_F32 ? (c_typ == FX_F32 ? _fx_gemm_pack8_f32 :
                           c_typ == FX_F64 ? _fx_gemm_pack8_f32f64 : 0) :
        a_typ == FX_F64 ? (c_typ == FX_F64 ? _fx_gemm_pack8_f64 : 0) :
        a_typ == FX_F16 ? (c_typ == FX_F32 || c_typ == FX_F16 ? _fx_gemm_pack8_f16f32 : 0) : 0;

    b_packer =
        b_typ == FX_F32 ? (c_typ == FX_F32 ? _fx_gemm_pack12_f32 :
                           c_typ == FX_F64 ? _fx_gemm_pack6_f32f64 : 0) :
        b_typ == FX_F64 ? (c_typ == FX_F64 ? _fx_gemm_pack6_f64 : 0) :
        b_typ == FX_F16 ? (c_typ == FX_F32 || c_typ == FX_F16 ? _fx_gemm_pack12_f16f32 : 0) : 0;

    if (!a_packer || !b_packer)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    #pragma omp parallel for num_threads(ntasks)
    for( int tid = 0; tid < ntasks; tid++ )
    {
        char* packA = (char*)(use_stackbuf ? alloca(bufsize) : fx_malloc(bufsize));
        char* packB = packA + KC*MC*w_esz;
        char* cbuf = c_typ == FX_F16 ? packB + KC*NC*w_esz : 0;
        int_ start_tile = total_tiles*tid/ntasks;
        int_ end_tile = total_tiles*(tid+1)/ntasks;

        for( int_ tile_idx = start_tile; tile_idx < end_tile; tile_idx++ )
        {
            int_ i0 = (tile_idx / n_tiles)*MC;
            int_ j0 = (tile_idx % n_tiles)*NC;
            int_ mc = M - i0 < MC ? M - i0 : MC;
            int_ nc = N - j0 < NC ? N - j0 : NC;
            int_ ldc_block = ldc;
            char* c_block = c + (i0 * ldc + j0)*c_esz;

            if (cbuf) {
                c_block = cbuf;
                ldc_block = nc;
                for(int_ i = 0; i < mc; i++)
                    memset(c_block + i*ldc_block*w_esz, 0, nc*w_esz);
            } else if (beta == 0.) {
                for(int_ i = 0; i < mc; i++)
                    memset(c_block + i*ldc_block*w_esz, 0, nc*w_esz);
            } else if (beta != 1.) {
                if (c_typ == FX_F32) {
                    for(int_ i = 0; i < mc; i++) {
                        float* c_i = (float*)c_block + i*ldc_block;
                        for(int_ j = 0; j < nc; j++)
                            c_i[j] *= beta;
                    }
                } else {
                    for(int_ i = 0; i < mc; i++) {
                        double* c_i = (double*)c_block + i*ldc_block;
                        for(int_ j = 0; j < nc; j++)
                            c_i[j] *= beta;
                    }
                }
            }

            for( int_ k0 = 0; k0 < K; k0 += KC )
            {
                int_ kc = K - k0 < KC ? K - k0 : KC;
                a_packer(mc, kc, a + (i0*lda0 + k0*lda1)*a_esz, lda0, lda1, packA);
                b_packer(nc, kc, b + (k0*ldb0 + j0*ldb1)*b_esz, ldb1, ldb0, packB);
                fx_gemm_macro_kernel(w_typ, mc, nc, kc, packA, packB, palpha,
                                     c_block, ldc_block, GEMM_MR, GEMM_NR);
            }

            if (cbuf) {
                for(int_ i = 0; i < mc; i++) {
                    const float* c_i = (const float*)cbuf + i*ldc_block;
                    fx_f16* cf16_i = (fx_f16*)c + (i0 + i)*ldc + j0;
                    int_ j = 0;
                #if defined FX_SIMD_NEON && FX_SIMD_NEON
                    float32x4_t va = vdupq_n_f32(alphaf), vb = vdupq_n_f32(betaf);
                    uint16x8_t mask = vreinterpretq_u16_u32(vcagtq_f32(va, vdupq_n_f32(0.f)));

                    for (; j <= nc - 8; j += 8) {
                        float32x4_t c0 = vld1q_f32(c_i + j);
                        float32x4_t c1 = vld1q_f32(c_i + j + 4);
                        uint16x8_t c_ = vld1q_u16((uint16_t*)(cf16_i + j));
                        float16x8_t cf = vreinterpretq_f16_u16(vandq_u16(c_, mask));
                        c0 = vfmaq_f32(c0, vcvt_f32_f16(vget_low_f16(cf)), vb);
                        c1 = vfmaq_f32(c1, vcvt_f32_f16(vget_high_f16(cf)), vb);
                        cf = vcombine_f16(vcvt_f16_f32(c0), vcvt_f16_f32(c1));
                        vst1q_f16(cf16_i + j, cf);
                    }
                #elif defined FX_SIMD_AVX2 && FX_SIMD_AVX2
                    __m128 va = _mm_set1_ps(alphaf), vb = _mm_set1_ps(betaf);
                    __m128i mask = _mm_castps_si128(_mm_cmpneq_ps(vb, _mm_setzero_ps()));

                    for (; j <= nc - 8; j += 8) {
                        __m128 c0 = _mm_loadu_ps(c_i + j);
                        __m128 c1 = _mm_loadu_ps(c_i + j + 4);
                        __m128i c_ = _mm_loadu_si128((const __m128i*)(cf16_i + j));
                        c_ = _mm_and_si128(c_, mask);
                        c0 = _mm_fmadd_ps(_mm_cvtph_ps(c_), vb, c0);
                        c1 = _mm_fmadd_ps(_mm_cvtph_ps(_mm_unpackhi_epi64(c_, c_), vb, c1);
                        __m128i cf16_0 = _mm_cvtps_ph(c0, 0);
                        __m128i cf16_1 = _mm_cvtps_ph(c1, 0);
                        _mm_storeu_ps((const __m128i*)(cf16_i + j), _mm_unpacklo_epi64(cf16_0, cf16_1));
                    }
                #endif
                    if (betaf == 0.f) {
                        for (; j < nc; j++)
                            cf16_i[j] = FX_FLOAT16(c_i[j]);
                    } else {
                        for (; j < nc; j++) {
                            float cval = FX_FLOAT(cf16_i[j])*betaf + c_i[j];
                            cf16_i[j] = FX_FLOAT16(cval);
                        }
                    }
                }
            }
        }
        if (!use_stackbuf)
            fx_free(packA);
    }
    }

    return 0;
}

// rs, re, rd is for "row start", "row end", "row delta"
// cs, ce, cd is for "column start", "column end", "column delta"
// If re1, ce1, re2 or ce2 are equal to -1, we consider all matrix in corresponding
// dimension.
int fx_gemm(fx_arr_t* m1, bool t1, int_ rs1, int_ re1, int_ rd1, int_ cs1, int_ ce1, int_ cd1,
            fx_arr_t* m2, bool t2, int_ rs2, int_ re2, int_ rd2, int_ cs2, int_ ce2, int_ cd2, fx_arr_t* result)
{
    int fx_status = FX_OK;
    if (m1->ndims != 2 || m2->ndims != 2)
        return FX_SET_EXN_FAST(FX_EXN_DimError);
    size_t m1_esz = m1->dim[1].step;
    size_t m2_esz = m2->dim[1].step;
    int m1_typ = m1_esz == 2 ? FX_F16 : m1_esz == 4 ? FX_F32 : m1_esz == 8 ? FX_F64 : 0;
    int m2_typ = m2_esz == 2 ? FX_F16 : m2_esz == 4 ? FX_F32 : m2_esz == 8 ? FX_F64 : 0;
    int r_esz = m1_esz >= m2_esz ? m1_esz : m2_esz;
    int r_typ = r_esz == 2 ? FX_F16 : r_esz == 4 ? FX_F32 : r_esz == 8 ? FX_F64 : 0;
    if (m1_typ == 0 || m2_typ == 0)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    re1 = (re1 == -1) ? m1->dim[0].size : re1;
    ce1 = (ce1 == -1) ? m1->dim[1].size : ce1;
    re2 = (re2 == -1) ? m2->dim[0].size : re2;
    ce2 = (ce2 == -1) ? m2->dim[1].size : ce2;

    /*printf("elemsize=%d, t1=%d, rs1=%d, re1=%d, rd1=%d, cs1=%d, ce1=%d, cd1=%d, t2=%d, rs2=%d, re2=%d, rd2=%d, cs2=%d, ce2=%d, cd2=%d\n",
        (int)elemsize, (int)t1, (int)rs1, (int)re1, (int)rd1, (int)cs1, (int)ce1, (int)cd1,
        (int)t2, (int)rs2, (int)re2, (int)rd2, (int)cs2, (int)ce2, (int)cd2);*/

    if (rs1<0 || rs1 > m1->dim[0].size || re1<0 || re1 > m1->dim[0].size || rd1<0 || rs1>=re1 ||
        cs1<0 || cs1 > m1->dim[1].size || ce1<0 || ce1 > m1->dim[1].size || cd1<0 || cs1>=ce1 ||
        rs2<0 || rs2 > m2->dim[0].size || re2<0 || re2 > m2->dim[0].size || rd2<0 || rs2>=re2 ||
        cs2<0 || cs2 > m2->dim[1].size || ce2<0 || ce2 > m2->dim[1].size || cd2<0 || cs2>=ce2)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    // Virtual sizes of matrixes after subarraying, but before transposition.
    int_ rows1 = (re1 - rs1 - 1)/rd1 + 1;
    int_ cols1 = (ce1 - cs1 - 1)/cd1 + 1;
    int_ rows2 = (re2 - rs2 - 1)/rd2 + 1;
    int_ cols2 = (ce2 - cs2 - 1)/cd2 + 1;
    const char* m1data = m1->data + m1->dim[0].step*rs1 + m1_esz*cs1;
    const char* m2data = m2->data + m2->dim[0].step*rs2 + m2_esz*cs2;

    int_ ldm1 = (int_)((m1->dim[0].step / m1_esz)*rd1);
    int_ ldm2 = (int_)((m2->dim[0].step / m2_esz)*rd2);
    int_ ldresult;
    int_ ressize[FX_MAX_DIMS];

    int_ inner = t1 ? rows1 : cols1;
    if(inner != (t2 ? cols2 : rows2))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    int_ rrows = t1 ? cols1 : rows1;
    int_ rcols = t2 ? rows2 : cols2;

    ressize[0] = rrows;
    ressize[1] = rcols;
    fx_status = fx_make_arr(2, ressize, r_esz, 0, 0, 0, result);
    if(fx_status<0)
        return fx_status;

    assert(m1->dim[0].step % m1_esz == 0 &&
           m2->dim[0].step % m2_esz == 0 &&
           result->dim[0].step % r_esz == 0);
    ldresult = result->dim[0].step / r_esz;
    /*printf("m1_esz=%d, m2_esz=%d, r_esz=%d, t1=%d, t2=%d, rows1=%d, cols1=%d, rows2=%d, cols2=%d, ldm1=%d, cd1=%d, ldm2=%d, cd2=%d, ldresult=%d\n",
        (int)m1_esz, (int)m2_esz, (int)r_esz, (int)t1, (int)t2,
        (int)rows1, (int)cols1, (int)rows2, (int)cols2,
        (int)ldm1, (int)cd1, (int)ldm2, (int)cd2, (int)ldresult);*/

    return fx_mpgemm(t1, t2, 1., 0.,
        rows1, cols1, m1_typ, m1data, ldm1, cd1,
        rows2, cols2, m2_typ, m2data, ldm2, cd2,
        r_typ, result->data, ldresult, 0);
}
