/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// the file is almost completely rewritten, but still derived from
// BLISLAB code (https://github.com/flame/blislab).
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

#define FX_GEMM_MAX_STACKBUF (1 << 14)
#define FX_SGEMM_MC 64
#define FX_SGEMM_NC 240
#define FX_SGEMM_STORAGE (1<<20)
#define FX_SGEMM_VOL (1<<18)
#define FX_SGEMM_MR 8
#define FX_SGEMM_NR 12

static void
fx_sgemm_pack8( int m, int k, float alpha,
                const float* A, int lda0, int lda1,
                float* packA )
{
    for( int i = 0; i < m; i += 8 ) {
        if (i + 7 < m) {
            const float* a_ptr = A + lda0*i;
            if (alpha == 1.f) {
                for( int j = 0; j < k*lda1; packA += 8, j += lda1 )
                {
                    packA[0] = a_ptr[j];
                    packA[1] = a_ptr[j+lda0];
                    packA[2] = a_ptr[j+lda0*2];
                    packA[3] = a_ptr[j+lda0*3];
                    packA[4] = a_ptr[j+lda0*4];
                    packA[5] = a_ptr[j+lda0*5];
                    packA[6] = a_ptr[j+lda0*6];
                    packA[7] = a_ptr[j+lda0*7];
                }
            } else {
                for( int j = 0; j < k*lda1; packA += 8, j += lda1 )
                {
                    packA[0] = a_ptr[j]*alpha;
                    packA[1] = a_ptr[j+lda0]*alpha;
                    packA[2] = a_ptr[j+lda0*2]*alpha;
                    packA[3] = a_ptr[j+lda0*3]*alpha;
                    packA[4] = a_ptr[j+lda0*4]*alpha;
                    packA[5] = a_ptr[j+lda0*5]*alpha;
                    packA[6] = a_ptr[j+lda0*6]*alpha;
                    packA[7] = a_ptr[j+lda0*7]*alpha;
                }
            }
        } else {
            const float* a_ptr0 = A + lda0*i;
            const float* a_ptr1 = A + lda0*(i+1 < m ? i+1 : i);
            const float* a_ptr2 = A + lda0*(i+2 < m ? i+2 : i);
            const float* a_ptr3 = A + lda0*(i+3 < m ? i+3 : i);
            const float* a_ptr4 = A + lda0*(i+4 < m ? i+4 : i);
            const float* a_ptr5 = A + lda0*(i+5 < m ? i+5 : i);
            const float* a_ptr6 = A + lda0*(i+6 < m ? i+6 : i);
            const float* a_ptr7 = A + lda0*(i+7 < m ? i+7 : i);

            if (alpha == 1.f) {
                for( int j = 0; j < k*lda1; packA += 8, j += lda1 )
                {
                    packA[0] = a_ptr0[j];
                    packA[1] = a_ptr1[j];
                    packA[2] = a_ptr2[j];
                    packA[3] = a_ptr3[j];
                    packA[4] = a_ptr4[j];
                    packA[5] = a_ptr5[j];
                    packA[6] = a_ptr6[j];
                    packA[7] = a_ptr7[j];
                }
            } else {
                for( int j = 0; j < k*lda1; packA += 8, j += lda1 )
                {
                    packA[0] = a_ptr0[j]*alpha;
                    packA[1] = a_ptr1[j]*alpha;
                    packA[2] = a_ptr2[j]*alpha;
                    packA[3] = a_ptr3[j]*alpha;
                    packA[4] = a_ptr4[j]*alpha;
                    packA[5] = a_ptr5[j]*alpha;
                    packA[6] = a_ptr6[j]*alpha;
                    packA[7] = a_ptr7[j]*alpha;
                }
            }
        }
    }
}

static void
fx_sgemm_pack12( int m, int k, float alpha,
                 const float* A, int lda0, int lda1,
                 float* packA )
{
    for( int i = 0; i < m; i += 12 ) {
        if (i + 11 < m) {
            const float* a_ptr = A + lda0*i;
            if (alpha == 1.f) {
                for( int j = 0; j < k*lda1; packA += 12, j += lda1 )
                {
                    packA[0] = a_ptr[j];
                    packA[1] = a_ptr[j+lda0];
                    packA[2] = a_ptr[j+lda0*2];
                    packA[3] = a_ptr[j+lda0*3];
                    packA[4] = a_ptr[j+lda0*4];
                    packA[5] = a_ptr[j+lda0*5];
                    packA[6] = a_ptr[j+lda0*6];
                    packA[7] = a_ptr[j+lda0*7];
                    packA[8] = a_ptr[j+lda0*8];
                    packA[9] = a_ptr[j+lda0*9];
                    packA[10] = a_ptr[j+lda0*10];
                    packA[11] = a_ptr[j+lda0*11];
                }
            } else {
                for( int j = 0; j < k*lda1; packA += 12, j += lda1 )
                {
                    packA[0] = a_ptr[j]*alpha;
                    packA[1] = a_ptr[j+lda0]*alpha;
                    packA[2] = a_ptr[j+lda0*2]*alpha;
                    packA[3] = a_ptr[j+lda0*3]*alpha;
                    packA[4] = a_ptr[j+lda0*4]*alpha;
                    packA[5] = a_ptr[j+lda0*5]*alpha;
                    packA[6] = a_ptr[j+lda0*6]*alpha;
                    packA[7] = a_ptr[j+lda0*7]*alpha;
                    packA[8] = a_ptr[j+lda0*8]*alpha;
                    packA[9] = a_ptr[j+lda0*9]*alpha;
                    packA[10] = a_ptr[j+lda0*10]*alpha;
                    packA[11] = a_ptr[j+lda0*11]*alpha;
                }
            }
        } else {
            const float* a_ptr0 = A + lda0*i;
            const float* a_ptr1 = A + lda0*(i+1 < m ? i+1 : i);
            const float* a_ptr2 = A + lda0*(i+2 < m ? i+2 : i);
            const float* a_ptr3 = A + lda0*(i+3 < m ? i+3 : i);
            const float* a_ptr4 = A + lda0*(i+4 < m ? i+4 : i);
            const float* a_ptr5 = A + lda0*(i+5 < m ? i+5 : i);
            const float* a_ptr6 = A + lda0*(i+6 < m ? i+6 : i);
            const float* a_ptr7 = A + lda0*(i+7 < m ? i+7 : i);
            const float* a_ptr8 = A + lda0*(i+8 < m ? i+8 : i);
            const float* a_ptr9 = A + lda0*(i+9 < m ? i+9 : i);
            const float* a_ptr10 = A + lda0*(i+10 < m ? i+10 : i);
            const float* a_ptr11 = A + lda0*(i+11 < m ? i+11 : i);

            if (alpha == 1.f) {
                for( int j = 0; j < k*lda1; packA += 12, j += lda1 )
                {
                    packA[0] = a_ptr0[j];
                    packA[1] = a_ptr1[j];
                    packA[2] = a_ptr2[j];
                    packA[3] = a_ptr3[j];
                    packA[4] = a_ptr4[j];
                    packA[5] = a_ptr5[j];
                    packA[6] = a_ptr6[j];
                    packA[7] = a_ptr7[j];
                    packA[8] = a_ptr8[j];
                    packA[9] = a_ptr9[j];
                    packA[10] = a_ptr10[j];
                    packA[11] = a_ptr11[j];
                }
            } else {
                for( int j = 0; j < k*lda1; packA += 12, j += lda1 )
                {
                    packA[0] = a_ptr0[j]*alpha;
                    packA[1] = a_ptr1[j]*alpha;
                    packA[2] = a_ptr2[j]*alpha;
                    packA[3] = a_ptr3[j]*alpha;
                    packA[4] = a_ptr4[j]*alpha;
                    packA[5] = a_ptr5[j]*alpha;
                    packA[6] = a_ptr6[j]*alpha;
                    packA[7] = a_ptr7[j]*alpha;
                    packA[8] = a_ptr8[j]*alpha;
                    packA[9] = a_ptr9[j]*alpha;
                    packA[10] = a_ptr10[j]*alpha;
                    packA[11] = a_ptr11[j]*alpha;
                }
            }
        }
    }
}


static void fx_sgemm8x12( int k, const float *a, const float *b, float *c, int ldc )
{
#ifdef __ARM_NEON
    float32x4_t c0 = vld1q_f32(c), c1 = vld1q_f32(c+4), c2 = vld1q_f32(c+8);
    float32x4_t c3 = vld1q_f32(c+ldc), c4 = vld1q_f32(c+ldc+4), c5 = vld1q_f32(c+ldc+8);
    float32x4_t c6 = vld1q_f32(c+ldc*2), c7 = vld1q_f32(c+ldc*2+4), c8 = vld1q_f32(c+ldc*2+8);
    float32x4_t c9 = vld1q_f32(c+ldc*3), c10 = vld1q_f32(c+ldc*3+4), c11 = vld1q_f32(c+ldc*3+8);
    float32x4_t c12 = vld1q_f32(c+ldc*4), c13 = vld1q_f32(c+ldc*4+4), c14 = vld1q_f32(c+ldc*4+8);
    float32x4_t c15 = vld1q_f32(c+ldc*5), c16 = vld1q_f32(c+ldc*5+4), c17 = vld1q_f32(c+ldc*5+8);
    float32x4_t c18 = vld1q_f32(c+ldc*6), c19 = vld1q_f32(c+ldc*6+4), c20 = vld1q_f32(c+ldc*6+8);
    float32x4_t c21 = vld1q_f32(c+ldc*7), c22 = vld1q_f32(c+ldc*7+4), c23 = vld1q_f32(c+ldc*7+8);

    for( int p = 0; p < k; p++, a += FX_SGEMM_MR, b += FX_SGEMM_NR )
    {
        float32x4_t a0 = vld1q_f32(a);
        float32x4_t b0 = vld1q_f32(b), b1 = vld1q_f32(b + 4), b2 = vld1q_f32(b + 8);

        c0 = vfmaq_laneq_f32(c0, b0, a0, 0);
        c1 = vfmaq_laneq_f32(c1, b1, a0, 0);
        c2 = vfmaq_laneq_f32(c2, b2, a0, 0);
        c3 = vfmaq_laneq_f32(c3, b0, a0, 1);
        c4 = vfmaq_laneq_f32(c4, b1, a0, 1);
        c5 = vfmaq_laneq_f32(c5, b2, a0, 1);

        c6 = vfmaq_laneq_f32(c6, b0, a0, 2);
        c7 = vfmaq_laneq_f32(c7, b1, a0, 2);
        c8 = vfmaq_laneq_f32(c8, b2, a0, 2);
        c9 = vfmaq_laneq_f32(c9, b0, a0, 3);
        c10 = vfmaq_laneq_f32(c10, b1, a0, 3);
        c11 = vfmaq_laneq_f32(c11, b2, a0, 3);

        a0 = vld1q_f32(a + 4);

        c12 = vfmaq_laneq_f32(c12, b0, a0, 0);
        c13 = vfmaq_laneq_f32(c13, b1, a0, 0);
        c14 = vfmaq_laneq_f32(c14, b2, a0, 0);
        c15 = vfmaq_laneq_f32(c15, b0, a0, 1);
        c16 = vfmaq_laneq_f32(c16, b1, a0, 1);
        c17 = vfmaq_laneq_f32(c17, b2, a0, 1);

        c18 = vfmaq_laneq_f32(c18, b0, a0, 2);
        c19 = vfmaq_laneq_f32(c19, b1, a0, 2);
        c20 = vfmaq_laneq_f32(c20, b2, a0, 2);
        c21 = vfmaq_laneq_f32(c21, b0, a0, 3);
        c22 = vfmaq_laneq_f32(c22, b1, a0, 3);
        c23 = vfmaq_laneq_f32(c23, b2, a0, 3);
    }
    vst1q_f32(c, c0); vst1q_f32(c+4, c1); vst1q_f32(c+8, c2);
    vst1q_f32(c + ldc, c3); vst1q_f32(c + ldc + 4, c4); vst1q_f32(c + ldc + 8, c5);
    vst1q_f32(c + ldc*2, c6); vst1q_f32(c + ldc*2 + 4, c7); vst1q_f32(c + ldc*2 + 8, c8);
    vst1q_f32(c + ldc*3, c9); vst1q_f32(c + ldc*3 + 4, c10); vst1q_f32(c + ldc*3 + 8, c11);
    vst1q_f32(c + ldc*4, c12); vst1q_f32(c + ldc*4 + 4, c13); vst1q_f32(c + ldc*4 + 8, c14);
    vst1q_f32(c + ldc*5, c15); vst1q_f32(c + ldc*5 + 4, c16); vst1q_f32(c + ldc*5 + 8, c17);
    vst1q_f32(c + ldc*6, c18); vst1q_f32(c + ldc*6 + 4, c19); vst1q_f32(c + ldc*6 + 8, c20);
    vst1q_f32(c + ldc*7, c21); vst1q_f32(c + ldc*7 + 4, c22); vst1q_f32(c + ldc*7 + 8, c23);
#else
    for( int p = 0; p < k; p++ )
    {
        for( int i = 0; i < FX_SGEMM_MR; i++ )
        {
            float alpha = a[FX_SGEMM_MR*p + i];
            for( int j = 0; j < FX_SGEMM_NR; j++ )
            {
                c[i*ldc+j] += b[FX_SGEMM_NR*p + j]*alpha;
            }
        }
    }
#endif
}

static void fx_sgemm_macro_kernel( int m, int n, int k,
                                   const float *packA, const float *packB,
                                   float *c, int ldc0 )
{
    float tempC[FX_SGEMM_MR*FX_SGEMM_NR];
    for( int i = 0; i < m; i += FX_SGEMM_MR ) {
        for( int j = 0; j < n; j += FX_SGEMM_NR ) {
            float* cptr0 = &c[i * ldc0 + j];
            float* cptr = cptr0;
            int ldc = ldc0;
            int mr = fx_mini(m - i, FX_SGEMM_MR), nr = fx_mini(n - j, FX_SGEMM_NR);
            bool partial = mr < FX_SGEMM_MR || nr < FX_SGEMM_NR;
            if( partial ) {
                memset(tempC, 0, sizeof(tempC));
                cptr = tempC;
                ldc = FX_SGEMM_NR;
                for(int p = 0; p < mr; p++)
                    memcpy(cptr + p*ldc, cptr0 + p*ldc0, nr*sizeof(c[0]));
            }

            fx_sgemm8x12(k, &packA[i * k], &packB[j * k], cptr, ldc);
            if( partial ) {
                for(int p = 0; p < mr; p++)
                    memcpy(cptr0 + p*ldc0, cptr + p*ldc, nr*sizeof(c[0]));
            }
        }
    }
}

static void fx_sgemm_thin(bool tA, float alpha, float beta,
                          int M, int N, int K,
                          const float* a, int lda0, int lda1,
                          const float* b, int ldb,
                          float* c, int ldc, int num_threads)
{
    #pragma omp parallel for if(M*N*K >= 100000)
    for( int i = 0; i < M; i++ ) {
        if (beta == 0.f)
            for( int j = 0; j < N; j++ ) c[i*ldc + j] = 0.f;
        else if (beta != 1.f)
            for( int j = 0; j < N; j++ ) c[i*ldc + j] *= beta;
        for( int k = 0; k < K; k++ ) {
            float aval = alpha*a[i*lda0 + k*lda1];
            for( int j = 0; j < N; j++ )
                c[i*ldc + j] += aval*b[ldb*k + j];
        }
    }
}

int fx_sgemm( bool tA, bool tB, float alpha, float beta,
               int ma, int na, const float *a, int lda0, int lda1,
               int mb, int nb, const float *b, int ldb0, int ldb1,
               float *c, int ldc, int num_threads )
{
    assert(FX_SGEMM_MR == 8 && FX_SGEMM_NR == 12);
    num_threads = num_threads <= 0 ? 4 : fx_maxi(num_threads, 1);
    int M = tA ? na : ma, N = tB ? mb : nb, K = tA ? ma : na;

    if (tA) { int t = lda0; lda0 = lda1; lda1 = t; }
    if (tB) { int t = ldb0; ldb0 = ldb1; ldb1 = t; }

    assert( ma > 0 && na > 0 && mb > 0 && nb > 0 &&
            a && b && c && ldc >= N );

    if(!tB && ldb1 == 1 && (M <= 4 || (double)M*N*K <= 10000.)) {
        fx_sgemm_thin(tA, alpha, beta, M, N, K, a, lda0, lda1,
                      b, ldb0, c, ldc, num_threads);
        return 0;
    }

    {
    int MC = ((fx_mini(FX_SGEMM_MC, M) + FX_SGEMM_MR-1) / FX_SGEMM_MR) * FX_SGEMM_MR;
    int NC = ((fx_mini(FX_SGEMM_NC, N) + FX_SGEMM_NR-1) / FX_SGEMM_NR) * FX_SGEMM_NR;
    int KC = FX_SGEMM_STORAGE/((MC+NC)*sizeof(c[0]));
    KC = fx_mini(KC, K);
    KC = fx_maxi(KC, 1);
    uint64_t ntasks_ = ((uint64_t)MC*(uint64_t)NC*(uint64_t)KC+
                            FX_SGEMM_VOL-1)/FX_SGEMM_VOL;
    int ntasks = (int)(ntasks_ < (uint64_t)num_threads ? ntasks_ :
                            (uint64_t)num_threads);
    size_t bufsize = KC*(MC+NC)*sizeof(c[0]);
    bool use_stackbuf = bufsize <= FX_GEMM_MAX_STACKBUF;
    int m_tiles = (M + MC - 1)/MC;
    int n_tiles = (N + NC - 1)/NC;
    int total_tiles = m_tiles * n_tiles;

    #pragma omp parallel for if (ntasks > 1) num_threads(ntasks)
    for( int tid = 0; tid < ntasks; tid++ )
    {
        float* packA = (float*)(use_stackbuf ? alloca(bufsize) : fx_malloc(bufsize));
        float* packB = packA + KC*MC;
        int start_tile = total_tiles*tid/ntasks;
        int end_tile = total_tiles*(tid+1)/ntasks;
        if (M == 1)
            memset(packA, 0, KC*MC*sizeof(packA[0]));

        for( int tile_idx = start_tile; tile_idx < end_tile; tile_idx++ )
        {
            int i0 = (tile_idx / n_tiles)*MC;
            int j0 = (tile_idx % n_tiles)*NC;
            int mc = fx_mini(M - i0, MC);
            int nc = fx_mini(N - j0, NC);

            if (beta == 0.f) {
                for(int i = 0; i < mc; i++)
                    memset(c + (i+i0)*ldc + j0, 0, nc*sizeof(c[0]));
            } else if (beta != 1.f) {
                for(int i = 0; i < mc; i++)
                    for(int j = 0; j < nc; j++)
                        c[(i+i0)*ldc + j+j0] *= beta;
            }

            for( int k0 = 0; k0 < K; k0 += KC )
            {
                int kc = fx_mini(K - k0, KC);
                fx_sgemm_pack8(mc, kc, alpha, &a[i0*lda0 + k0*lda1], lda0, lda1, packA);
                fx_sgemm_pack12(nc, kc, 1.f, &b[k0*ldb0 + j0*ldb1], ldb1, ldb0, packB);
                fx_sgemm_macro_kernel(mc, nc, kc, packA, packB, &c[i0 * ldc + j0], ldc);
            }
        }
        if (!use_stackbuf)
            fx_free(packA);
    }
    }

    return 0;
}

#define FX_DGEMM_MC 48
#define FX_DGEMM_NC 120
#define FX_DGEMM_STORAGE (1<<20)
#define FX_DGEMM_VOL (1<<17)
#define FX_DGEMM_MR 8
#define FX_DGEMM_NR 6

static void
fx_dgemm_pack8( int m, int k, double alpha,
                const double* A, int lda0, int lda1,
                double* packA )
{
    for( int i = 0; i < m; i += 8 ) {
        if (i + 7 < m) {
            const double* a_ptr = A + lda0*i;
            if (alpha == 1.f) {
                for( int j = 0; j < k*lda1; packA += 8, j += lda1 )
                {
                    packA[0] = a_ptr[j];
                    packA[1] = a_ptr[j+lda0];
                    packA[2] = a_ptr[j+lda0*2];
                    packA[3] = a_ptr[j+lda0*3];
                    packA[4] = a_ptr[j+lda0*4];
                    packA[5] = a_ptr[j+lda0*5];
                    packA[6] = a_ptr[j+lda0*6];
                    packA[7] = a_ptr[j+lda0*7];
                }
            } else {
                for( int j = 0; j < k*lda1; packA += 8, j += lda1 )
                {
                    packA[0] = a_ptr[j]*alpha;
                    packA[1] = a_ptr[j+lda0]*alpha;
                    packA[2] = a_ptr[j+lda0*2]*alpha;
                    packA[3] = a_ptr[j+lda0*3]*alpha;
                    packA[4] = a_ptr[j+lda0*4]*alpha;
                    packA[5] = a_ptr[j+lda0*5]*alpha;
                    packA[6] = a_ptr[j+lda0*6]*alpha;
                    packA[7] = a_ptr[j+lda0*7]*alpha;
                }
            }
        } else {
            const double* a_ptr0 = A + lda0*i;
            const double* a_ptr1 = A + lda0*(i+1 < m ? i+1 : i);
            const double* a_ptr2 = A + lda0*(i+2 < m ? i+2 : i);
            const double* a_ptr3 = A + lda0*(i+3 < m ? i+3 : i);
            const double* a_ptr4 = A + lda0*(i+4 < m ? i+4 : i);
            const double* a_ptr5 = A + lda0*(i+5 < m ? i+5 : i);
            const double* a_ptr6 = A + lda0*(i+6 < m ? i+6 : i);
            const double* a_ptr7 = A + lda0*(i+7 < m ? i+7 : i);

            if (alpha == 1.f) {
                for( int j = 0; j < k*lda1; packA += 8, j += lda1 )
                {
                    packA[0] = a_ptr0[j];
                    packA[1] = a_ptr1[j];
                    packA[2] = a_ptr2[j];
                    packA[3] = a_ptr3[j];
                    packA[4] = a_ptr4[j];
                    packA[5] = a_ptr5[j];
                    packA[6] = a_ptr6[j];
                    packA[7] = a_ptr7[j];
                }
            } else {
                for( int j = 0; j < k*lda1; packA += 8, j += lda1 )
                {
                    packA[0] = a_ptr0[j]*alpha;
                    packA[1] = a_ptr1[j]*alpha;
                    packA[2] = a_ptr2[j]*alpha;
                    packA[3] = a_ptr3[j]*alpha;
                    packA[4] = a_ptr4[j]*alpha;
                    packA[5] = a_ptr5[j]*alpha;
                    packA[6] = a_ptr6[j]*alpha;
                    packA[7] = a_ptr7[j]*alpha;
                }
            }
        }
    }
}

static void
fx_dgemm_pack6( int m, int k, double alpha,
                const double* A, int lda0, int lda1,
                double* packA )
{
    for( int i = 0; i < m; i += 6 ) {
        if (i + 5 < m) {
            const double* a_ptr = A + lda0*i;
            if (alpha == 1.f) {
                for( int j = 0; j < k*lda1; packA += 6, j += lda1 )
                {
                    packA[0] = a_ptr[j];
                    packA[1] = a_ptr[j+lda0];
                    packA[2] = a_ptr[j+lda0*2];
                    packA[3] = a_ptr[j+lda0*3];
                    packA[4] = a_ptr[j+lda0*4];
                    packA[5] = a_ptr[j+lda0*5];
                }
            } else {
                for( int j = 0; j < k*lda1; packA += 6, j += lda1 )
                {
                    packA[0] = a_ptr[j]*alpha;
                    packA[1] = a_ptr[j+lda0]*alpha;
                    packA[2] = a_ptr[j+lda0*2]*alpha;
                    packA[3] = a_ptr[j+lda0*3]*alpha;
                    packA[4] = a_ptr[j+lda0*4]*alpha;
                    packA[5] = a_ptr[j+lda0*5]*alpha;
                }
            }
        } else {
            const double* a_ptr0 = A + lda0*i;
            const double* a_ptr1 = A + lda0*(i+1 < m ? i+1 : i);
            const double* a_ptr2 = A + lda0*(i+2 < m ? i+2 : i);
            const double* a_ptr3 = A + lda0*(i+3 < m ? i+3 : i);
            const double* a_ptr4 = A + lda0*(i+4 < m ? i+4 : i);
            const double* a_ptr5 = A + lda0*(i+5 < m ? i+5 : i);

            if (alpha == 1.f) {
                for( int j = 0; j < k*lda1; packA += 6, j += lda1 )
                {
                    packA[0] = a_ptr0[j];
                    packA[1] = a_ptr1[j];
                    packA[2] = a_ptr2[j];
                    packA[3] = a_ptr3[j];
                    packA[4] = a_ptr4[j];
                    packA[5] = a_ptr5[j];
                }
            } else {
                for( int j = 0; j < k*lda1; packA += 6, j += lda1 )
                {
                    packA[0] = a_ptr0[j]*alpha;
                    packA[1] = a_ptr1[j]*alpha;
                    packA[2] = a_ptr2[j]*alpha;
                    packA[3] = a_ptr3[j]*alpha;
                    packA[4] = a_ptr4[j]*alpha;
                    packA[5] = a_ptr5[j]*alpha;
                }
            }
        }
    }
}

static void fx_dgemm8x6( int k, const double *a, const double *b, double *c, int ldc )
{
#ifdef __ARM_NEON
    float64x2_t c0 = vld1q_f64(c), c1 = vld1q_f64(c+2), c2 = vld1q_f64(c+4);
    float64x2_t c3 = vld1q_f64(c+ldc), c4 = vld1q_f64(c+ldc+2), c5 = vld1q_f64(c+ldc+4);
    float64x2_t c6 = vld1q_f64(c+ldc*2), c7 = vld1q_f64(c+ldc*2+2), c8 = vld1q_f64(c+ldc*2+4);
    float64x2_t c9 = vld1q_f64(c+ldc*3), c10 = vld1q_f64(c+ldc*3+2), c11 = vld1q_f64(c+ldc*3+4);
    float64x2_t c12 = vld1q_f64(c+ldc*4), c13 = vld1q_f64(c+ldc*4+2), c14 = vld1q_f64(c+ldc*4+4);
    float64x2_t c15 = vld1q_f64(c+ldc*5), c16 = vld1q_f64(c+ldc*5+2), c17 = vld1q_f64(c+ldc*5+4);
    float64x2_t c18 = vld1q_f64(c+ldc*6), c19 = vld1q_f64(c+ldc*6+2), c20 = vld1q_f64(c+ldc*6+4);
    float64x2_t c21 = vld1q_f64(c+ldc*7), c22 = vld1q_f64(c+ldc*7+2), c23 = vld1q_f64(c+ldc*7+4);

    for( int p = 0; p < k; p++, a += FX_DGEMM_MR, b += FX_DGEMM_NR )
    {
        float64x2_t aval = vld1q_f64(a);
        float64x2_t b0 = vld1q_f64(b), b1 = vld1q_f64(b + 2), b2 = vld1q_f64(b + 4);

        c0 = vfmaq_laneq_f64(c0, b0, aval, 0);
        c1 = vfmaq_laneq_f64(c1, b1, aval, 0);
        c2 = vfmaq_laneq_f64(c2, b2, aval, 0);
        c3 = vfmaq_laneq_f64(c3, b0, aval, 1);
        c4 = vfmaq_laneq_f64(c4, b1, aval, 1);
        c5 = vfmaq_laneq_f64(c5, b2, aval, 1);

        aval = vld1q_f64(a+2);

        c6 = vfmaq_laneq_f64(c6, b0, aval, 0);
        c7 = vfmaq_laneq_f64(c7, b1, aval, 0);
        c8 = vfmaq_laneq_f64(c8, b2, aval, 0);
        c9 = vfmaq_laneq_f64(c9, b0, aval, 1);
        c10 = vfmaq_laneq_f64(c10, b1, aval, 1);
        c11 = vfmaq_laneq_f64(c11, b2, aval, 1);

        aval = vld1q_f64(a+4);

        c12 = vfmaq_laneq_f64(c12, b0, aval, 0);
        c13 = vfmaq_laneq_f64(c13, b1, aval, 0);
        c14 = vfmaq_laneq_f64(c14, b2, aval, 0);
        c15 = vfmaq_laneq_f64(c15, b0, aval, 1);
        c16 = vfmaq_laneq_f64(c16, b1, aval, 1);
        c17 = vfmaq_laneq_f64(c17, b2, aval, 1);

        aval = vld1q_f64(a+6);

        c18 = vfmaq_laneq_f64(c18, b0, aval, 0);
        c19 = vfmaq_laneq_f64(c19, b1, aval, 0);
        c20 = vfmaq_laneq_f64(c20, b2, aval, 0);
        c21 = vfmaq_laneq_f64(c21, b0, aval, 1);
        c22 = vfmaq_laneq_f64(c22, b1, aval, 1);
        c23 = vfmaq_laneq_f64(c23, b2, aval, 1);
    }
    vst1q_f64(c, c0); vst1q_f64(c+2, c1); vst1q_f64(c+4, c2);
    vst1q_f64(c + ldc, c3); vst1q_f64(c + ldc + 2, c4); vst1q_f64(c + ldc + 4, c5);
    vst1q_f64(c + ldc*2, c6); vst1q_f64(c + ldc*2 + 2, c7); vst1q_f64(c + ldc*2 + 4, c8);
    vst1q_f64(c + ldc*3, c9); vst1q_f64(c + ldc*3 + 2, c10); vst1q_f64(c + ldc*3 + 4, c11);
    vst1q_f64(c + ldc*4, c12); vst1q_f64(c + ldc*4 + 2, c13); vst1q_f64(c + ldc*4 + 4, c14);
    vst1q_f64(c + ldc*5, c15); vst1q_f64(c + ldc*5 + 2, c16); vst1q_f64(c + ldc*5 + 4, c17);
    vst1q_f64(c + ldc*6, c18); vst1q_f64(c + ldc*6 + 2, c19); vst1q_f64(c + ldc*6 + 4, c20);
    vst1q_f64(c + ldc*7, c21); vst1q_f64(c + ldc*7 + 2, c22); vst1q_f64(c + ldc*7 + 4, c23);
#else
    for( int p = 0; p < k; p++ )
    {
        for( int i = 0; i < FX_DGEMM_MR; i++ )
        {
            float alpha = a[FX_DGEMM_MR*p + i];
            for( int j = 0; j < FX_DGEMM_NR; j++ )
            {
                c[i*ldc+j] += b[FX_DGEMM_NR*p + j]*alpha;
            }
        }
    }
#endif
}

static void fx_dgemm_macro_kernel( int m, int n, int k,
                                   const double *packA, const double *packB,
                                   double *c, int ldc0 )
{
    double tempC[FX_DGEMM_MR*FX_DGEMM_NR];
    for( int i = 0; i < m; i += FX_DGEMM_MR ) {
        for( int j = 0; j < n; j += FX_DGEMM_NR ) {
            double* cptr0 = &c[i * ldc0 + j];
            double* cptr = cptr0;
            int ldc = ldc0;
            int mr = fx_mini(m - i, FX_DGEMM_MR), nr = fx_mini(n - j, FX_DGEMM_NR);
            bool partial = mr < FX_DGEMM_MR || nr < FX_DGEMM_NR;
            if( partial ) {
                memset(tempC, 0, sizeof(tempC));
                cptr = tempC;
                ldc = FX_DGEMM_NR;
                for(int p = 0; p < mr; p++)
                    memcpy(cptr + p*ldc, cptr0 + p*ldc0, nr*sizeof(c[0]));
            }

            fx_dgemm8x6(k, &packA[i * k], &packB[j * k], cptr, ldc);
            if( partial ) {
                for(int p = 0; p < mr; p++)
                    memcpy(cptr0 + p*ldc0, cptr + p*ldc, nr*sizeof(c[0]));
            }
        }
    }
}

static void fx_dgemm_thin(bool tA, double alpha, double beta,
                          int M, int N, int K,
                          const double* a, int lda0, int lda1,
                          const double* b, int ldb,
                          double* c, int ldc, int num_threads)
{
    #pragma omp parallel for if(M*N*K >= 100000)
    for( int i = 0; i < M; i++ ) {
        if (beta == 0.f)
            for( int j = 0; j < N; j++ ) c[i*ldc + j] = 0.f;
        else if (beta != 1.f)
            for( int j = 0; j < N; j++ ) c[i*ldc + j] *= beta;
        for( int k = 0; k < K; k++ ) {
            float aval = alpha*a[i*lda0 + k*lda1];
            for( int j = 0; j < N; j++ )
                c[i*ldc + j] += aval*b[ldb*k + j];
        }
    }
}

int fx_dgemm( bool tA, bool tB, double alpha, double beta,
               int ma, int na, const double *a, int lda0, int lda1,
               int mb, int nb, const double *b, int ldb0, int ldb1,
               double *c, int ldc, int num_threads )
{
    assert(FX_DGEMM_MR == 8 && FX_DGEMM_NR == 6);
    num_threads = num_threads <= 0 ? 4 : fx_maxi(num_threads, 1);

    if (tA) { int t = lda0; lda0 = lda1; lda1 = t; }
    if (tB) { int t = ldb0; ldb0 = ldb1; ldb1 = t; }

    int M = tA ? na : ma, N = tB ? mb : nb, K = tA ? ma : na;
    assert( ma > 0 && na > 0 && mb > 0 && nb > 0 &&
            a && b && c && ldc >= N );

    if(!tB && ldb1 == 1 && (M <= 4 || (double)M*N*K <= 10000)) {
        fx_dgemm_thin(tA, alpha, beta, M, N, K, a, lda0, lda1,
                      b, ldb0, c, ldc, num_threads);
        return 0;
    }

    {
    int MC = ((fx_mini(FX_DGEMM_MC, M) + FX_DGEMM_MR-1) / FX_DGEMM_MR) * FX_DGEMM_MR;
    int NC = ((fx_mini(FX_DGEMM_NC, N) + FX_DGEMM_NR-1) / FX_DGEMM_NR) * FX_DGEMM_NR;
    int KC = FX_DGEMM_STORAGE/((MC+NC)*sizeof(c[0]));
    KC = fx_mini(KC, K);
    KC = fx_maxi(KC, 1);
    uint64_t ntasks_ = ((uint64_t)MC*(uint64_t)NC*(uint64_t)KC+
                            FX_DGEMM_VOL-1)/FX_DGEMM_VOL;
    int ntasks = (int)(ntasks_ < (uint64_t)num_threads ? ntasks_ :
                            (uint64_t)num_threads);
    size_t bufsize = KC*(MC+NC)*sizeof(c[0]);
    bool use_stackbuf = bufsize <= FX_GEMM_MAX_STACKBUF;
    int m_tiles = (M + MC - 1)/MC;
    int n_tiles = (N + NC - 1)/NC;
    int total_tiles = m_tiles * n_tiles;

    #pragma omp parallel for if(ntasks > 1) num_threads(ntasks)
    for( int tid = 0; tid < ntasks; tid++ )
    {
        double* packA = (double*)(use_stackbuf ? alloca(bufsize) : fx_malloc(bufsize));
        double* packB = packA + KC*MC;
        int start_tile = total_tiles*tid/ntasks;
        int end_tile = total_tiles*(tid+1)/ntasks;
        if (M == 1)
            memset(packA, 0, KC*MC*sizeof(packA[0]));

        for( int tile_idx = start_tile; tile_idx < end_tile; tile_idx++ )
        {
            int i0 = (tile_idx / n_tiles)*MC;
            int j0 = (tile_idx % n_tiles)*NC;
            int mc = fx_mini(M - i0, MC);
            int nc = fx_mini(N - j0, NC);

            if (beta == 0.f) {
                for(int i = 0; i < mc; i++)
                    memset(c + (i+i0)*ldc + j0, 0, nc*sizeof(c[0]));
            } else if (beta != 1.f) {
                for(int i = 0; i < mc; i++)
                    for(int j = 0; j < nc; j++)
                        c[(i+i0)*ldc + j+j0] *= beta;
            }

            for( int k0 = 0; k0 < K; k0 += KC )
            {
                int kc = fx_mini(K - k0, KC);
                fx_dgemm_pack8(mc, kc, alpha, &a[i0*lda0 + k0*lda1], lda0, lda1, packA);
                fx_dgemm_pack6(nc, kc, 1.f, &b[k0*ldb0 + j0*ldb1], ldb1, ldb0, packB);
                fx_dgemm_macro_kernel(mc, nc, kc, packA, packB, &c[i0 * ldc + j0], ldc);
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
        FX_FAST_THROW_RET(FX_EXN_DimError);
    size_t elemsize = m1->dim[1].step;
    if (elemsize != m2->dim[1].step || !(elemsize == sizeof(float) || elemsize == sizeof(double)))
        FX_FAST_THROW_RET(FX_EXN_TypeMismatchError);

    re1 = (re1 == -1) ? m1->dim[0].size : re1;
    ce1 = (ce1 == -1) ? m1->dim[1].size : ce1;
    re2 = (re2 == -1) ? m2->dim[0].size : re2;
    ce2 = (ce2 == -1) ? m2->dim[1].size : ce2;

    if (rs1<0 || rs1 > m1->dim[0].size || re1<0 || re1 > m1->dim[0].size || rd1<0 || rs1>=re1 ||
        cs1<0 || cs1 > m1->dim[1].size || ce1<0 || ce1 > m1->dim[1].size || cd1<0 || cs1>=ce1 ||
        rs2<0 || rs2 > m2->dim[0].size || re2<0 || re2 > m2->dim[0].size || rd2<0 || rs2>=re2 ||
        cs2<0 || cs2 > m2->dim[1].size || ce2<0 || ce2 > m2->dim[1].size || cd2<0 || cs2>=ce2)
        FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);

    // Virtual sizes of matrixes after subarraying, but before transposition.
    int_ rows1 = (re1 - rs1 - 1)/rd1 + 1;
    int_ cols1 = (ce1 - cs1 - 1)/cd1 + 1;
    int_ rows2 = (re2 - rs2 - 1)/rd2 + 1;
    int_ cols2 = (ce2 - cs2 - 1)/cd2 + 1;
    const char* m1data = m1->data + m1->dim[0].step*rs1 + elemsize*cs1;
    const char* m2data = m2->data + m2->dim[0].step*rs2 + elemsize*cs2;

    int ldm1 = (int)((m1->dim[0].step / elemsize)*rd1);
    int ldm2 = (int)((m2->dim[0].step / elemsize)*rd2);
    int ldresult;
    int_ ressize[FX_MAX_DIMS];

    int_ inner = t1 ? rows1 : cols1;
    if(inner != (t2 ? cols2 : rows2))
        FX_FAST_THROW_RET(FX_EXN_SizeMismatchError);

    int_ rrows = t1 ? cols1 : rows1;
    int_ rcols = t2 ? rows2 : cols2;

    //TODO: Is it possible to consider case when we don't need memory allocation?
    ressize[0] = rrows;
    ressize[1] = rcols;
    fx_status = fx_make_arr(2, ressize, elemsize,
        m1->free_elem, m1->copy_elem, 0, result);
    if(fx_status<0)
        FX_FAST_THROW_RET(fx_status);

    assert(m1->dim[0].step % elemsize == 0 &&
           m2->dim[0].step % elemsize == 0 &&
           result->dim[0].step % elemsize == 0);
    ldresult = result->dim[0].step / elemsize;

    if (elemsize == sizeof(float)) {
        fx_sgemm(t1, t2, 1.f, 0.f,
            (int)rows1, (int)cols1, (const float*)m1data, ldm1, cd1,
            (int)rows2, (int)cols2, (const float*)m2data, ldm2, cd2,
            (float*)(result->data), ldresult, 0);
    } else {
        fx_dgemm(t1, t2, 1.f, 0.f,
            (int)rows1, (int)cols1, (const double*)m1data, ldm1, cd1,
            (int)rows2, (int)cols2, (const double*)m2data, ldm2, cd2,
            (double*)(result->data), ldresult, 0);
    }

    return fx_status;
}
