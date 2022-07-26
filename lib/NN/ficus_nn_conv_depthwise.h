/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// (ARM-only)
// depthwise fp32 & fp16 convolution.
// The header is not intented to be used alone.
// It is assumed to be included into OpConv.fx

static int _fx_depthwise_conv2d(int ndims, const int_* inp_shape, const fx_arr_t* inp_data,
                                const int_* out_shape, fx_arr_t* out_data,
                                const _fx_conv2d_t* conv, int ntasks)
{
    assert(ndims == 4 && inp_shape[0] == out_shape[0] && out_shape[1] == conv->K && inp_shape[1] == conv->C);
    assert(conv->ngroups == conv->K && conv->K == conv->C);
    int N = (int)inp_shape[0], C = (int)inp_shape[1];
    int Hi = (int)inp_shape[2], Wi = (int)inp_shape[3];
    int Hk = conv->Hk, Wk = conv->Wk;
    int H0 = out_shape[2], W0 = out_shape[3];
    size_t esz = inp_data->dim[0].step;
    const size_t inp_planesize = (size_t)Hi*Wi;
    const size_t out_planesize = (size_t)H0*W0;
    float minval = conv->minval, maxval = conv->maxval;
    bool fast_activ = conv->activ == _FX_ACTIV_RELU ||
                      conv->activ == _FX_ACTIV_CLIP;
    _fx_activ_func_t activ_func = !fast_activ ? conv->activ_func : 0;
    const float* activ_params = conv->activ_params;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    int ksize = Hk*Wk, padded_ksize = ((ksize + FX_VEC_NLANES-1)/FX_VEC_NLANES)*FX_VEC_NLANES;

    int* ofstab = (int*)alloca(3*padded_ksize*sizeof(ofstab[0]));
    int* yxtab = ofstab + padded_ksize;
    const float* weights0 = conv->weights, *bias = conv->bias;
    int inner_ytop = (pad_bottom + stride_y-1)/stride_y, inner_ybottom;
    int inner_xleft = (pad_left + stride_x-1)/stride_x, inner_xright;
    for (int k = 0; k < padded_ksize; k++) {
        int y = k < ksize ? k / Wk : 0;
        int x = k < ksize ? k % Wk : 0;
        int dy = y*dilation_y, dx = x*dilation_x;
        yxtab[k*2] = dy; yxtab[k*2+1] = dx;
        ofstab[k] = dy*Wi + dx;
    }
    assert(ksize > 1 || (pad_left == 0 && pad_right == 0 && pad_top == 0 && pad_bottom == 0));

    inner_xright = (Wi - (Wk - 1)*dilation_x + pad_left)/stride_x;
    inner_xright += inner_xright*stride_x - pad_left + (Wk-1)*dilation_x < Wi;
    inner_ybottom = (Hi - (Hk - 1)*dilation_y + pad_top)/stride_y;
    inner_ybottom += inner_ybottom*stride_y - pad_top + (Hk-1)*dilation_y < Hi;
    if (inner_xleft >= inner_xright || inner_ytop >= inner_ybottom) {
        inner_xleft = W0;
        inner_ytop = H0;
    }
    inner_ybottom = inner_ybottom < H0 ? inner_ybottom : H0;
    //printf("Wi=%d, W0=%d, sx=%d, dx=%d, left=%d, right=%d, pad_left=%d, pad_right=%d\n", Wi, W0, stride_x, dilation_x, inner_xleft, inner_xright, pad_left, pad_right);

#ifdef __ARM_NEON
    float32x4_t vminval = vdupq_n_f32(minval), vmaxval = vdupq_n_f32(maxval);
    bool useSIMD = stride_x == 1 && inner_xleft < W0;
    bool is3x3 = Hk == 3 && Wk == 3;
#endif

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int nc = 0; nc < N*C; nc++) {
        int c = nc % C, dy0 = 1;
        const float* inptr = (const float*)inp_data->data + inp_planesize*nc;
        float* outptr = (float*)out_data->data + out_planesize*nc;
        float biasval = bias[c];
        const float* weights = weights0 + c*padded_ksize;
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
            dy0 = inner_ytop <= y0 && y0+3 < inner_ybottom && is3x3 && stride_y == 1 && dilation_y == 1 ? 3 : 1;
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
                            int dy = yxtab[k*2];
                            int yi = yi_ + dy;
                            int xi = xi_ + yxtab[k*2+1];
                            float w = weights[k];
                            if ((unsigned)xi < (unsigned)Wi) {
                                s_0 += inptr[yi*Wi + xi]*w;
                                s_1 += inptr[(yi+1)*Wi + xi]*w;
                                s_2 += inptr[(yi+2)*Wi + xi]*w;
                            }
                        }
                        s_0 = fx_minf(fx_maxf(s_0, minval), maxval);
                        s_1 = fx_minf(fx_maxf(s_1, minval), maxval);
                        s_2 = fx_minf(fx_maxf(s_2, minval), maxval);
                        outptr[x0] = s_0;
                        outptr[x0 + W0] = s_1;
                        outptr[x0 + W0*2] = s_2;
                    }
                } else {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        s_0 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            int dy = yxtab[k*2];
                            int yi = yi_ + dy;
                            int xi = xi_ + yxtab[k*2+1];
                            float w = weights[k];
                            if (((unsigned)yi < (unsigned)Hi) & ((unsigned)xi < (unsigned)Wi))
                                s_0 += inptr[yi*Wi + xi]*w;
                        }
                        s_0 = fx_minf(fx_maxf(s_0, minval), maxval);
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
                            for (; x0 <= x1 - FX_VEC_NLANES; x0 += FX_VEC_NLANES) {
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

                                s0 = vminq_f32(vmaxq_f32(s0, vminval), vmaxval);
                                s1 = vminq_f32(vmaxq_f32(s1, vminval), vmaxval);
                                s2 = vminq_f32(vmaxq_f32(s2, vminval), vmaxval);
                                vst1q_f32(outptr + x0, s0);
                                vst1q_f32(outptr + W0 + x0, s1);
                                vst1q_f32(outptr + W0*2 + x0, s2);
                            }
                        } else {
                            for (; x0 <= x1 - FX_VEC_NLANES; x0 += FX_VEC_NLANES) {
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
                                s0 = vminq_f32(vmaxq_f32(s0, vminval), vmaxval);
                                vst1q_f32(outptr + x0, s0);
                            }
                        }
                    } else {
                        for (; x0 <= x1 - FX_VEC_NLANES; x0 += FX_VEC_NLANES) {
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
                                s0 = vfmaq_f32(s0, vld1q_f32(inptr_xi + ofstab[k]), vdupq_n_f32(weights[k]));
                            s0 = vminq_f32(vmaxq_f32(s0, vminval), vmaxval);
                            vst1q_f32(outptr + x0, s0);
                        }
                    }
                }
            #endif
                if (dy0 == 3) {
                    for (; x0 < x1; x0++) {
                        int xi_ = x0*stride_x - pad_left;
                        const float* inptr_xi = inptr + W0*yi_ + xi_;
                        s_0 = s_1 = s_2 = biasval;
                        for (int k = 0; k < ksize; k++) {
                            int inp_ofs = ofstab[k];
                            float w = weights[k];
                            s_0 += inptr_xi[inp_ofs]*w;
                            s_1 += inptr_xi[inp_ofs + Wi]*w;
                            s_2 += inptr_xi[inp_ofs + Wi*2]*w;
                        }
                        s_0 = fx_minf(fx_maxf(s_0, minval), maxval);
                        s_1 = fx_minf(fx_maxf(s_1, minval), maxval);
                        s_2 = fx_minf(fx_maxf(s_2, minval), maxval);
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
                        s_0 = fx_minf(fx_maxf(s_0, minval), maxval);
                        outptr[x0] = s_0;
                    }
                }
                x1 = W0;
            }
        }
        if (activ_func)
            activ_func(outptr, outptr, (int_)out_planesize, activ_params);
    }
    return FX_OK;
}
