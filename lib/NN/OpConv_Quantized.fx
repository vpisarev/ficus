/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast, OpConv_Block, OpConv_Depthwise

@ccode {
#include <assert.h>
#include <float.h>
#include <math.h>
#include "ficus_nn_common.h"

void _fx_conv_update_block_u8( int np, const void* a_, const void* b_,
                               void* c_, int ldc, bool init_c );
int _fx_depthwise_qconv2d_u8(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_qconv2d_t* qconv,
                             int inp_typ, const uint8_t* inptr0,
                             float inp_scale0, int inp_zp0,
                             int out_typ, uint8_t* outptr0,
                             float out_scale0, int out_zp0,
                             int ntasks);

static void _fx_free_qconv2d(void* conv_ptr)
{
    _fx_qconv2d_t* conv = (_fx_qconv2d_t*)conv_ptr;
    if(conv) {
        fx_free(conv->weights);
        fx_free(conv->depthwise_weights);
        fx_free(conv->w_scale);
        fx_free(conv);
    }
}

static int _fx_init_qconv2d(
    int layout_orig, int layout, int ngroups,
    int K, int C, int Hk, int Wk,
    int stride_y, int stride_x,
    int dilation_y, int dilation_x,
    int pad_top, int pad_left,
    int pad_bottom, int pad_right,
    int w_scale_size, int w_typ,
    const char* weights,
    const float* w_scale,
    const char* w_zp,
    const int32_t* bias,
    fx_cptr_t* fx_result)
{
    _fx_qconv2d_t* conv = (_fx_qconv2d_t*)fx_malloc(sizeof(*conv));
    int k, nbias = K + 32;
    int fx_status;
    size_t biasbufsize;

    memset(conv, 0, sizeof(*conv));
    //assert(layout_orig == _FX_NN_Layout_NCHW && layout == _FX_NN_Layout_NCHW);
    assert(ngroups > 0 && K > 0 && C > 0 && K % ngroups == 0 && C % ngroups == 0);
    assert(Hk > 0 && Wk > 0);
    assert(stride_y > 0 && stride_x > 0);
    assert(dilation_y > 0 && dilation_x > 0);
    assert(pad_top >= 0 && pad_bottom >= 0 && pad_left >= 0 && pad_right >= 0);
    assert(FX_QCONV_MR == FX_QCONV_C);
    conv->layout = layout;
    conv->K = K; conv->C = C; conv->Hk = Hk; conv->Wk = Wk;
    conv->padded_ksize = (Hk*Wk + FX_VEC_NLANES_F16-1) & -FX_VEC_NLANES_F16;
    conv->stride_y = stride_y;
    conv->stride_x = stride_x;
    conv->dilation_y = dilation_y;
    conv->dilation_x = dilation_x;
    conv->pad_top = pad_top; conv->pad_left = pad_left;
    conv->pad_bottom = pad_bottom; conv->pad_right = pad_right;
    conv->ngroups = ngroups;
    conv->conv_type =
        ngroups == K && ngroups == C ?
                _FX_CONV_TYPE_DEPTHWISE :
                _FX_CONV_TYPE_GENERIC;
    conv->w_typ = w_typ;
    int w_mask = w_typ == FX_I8 ? 128 : 0;

    biasbufsize = nbias*(sizeof(conv->w_scale[0])*4);
    conv->w_scale = (float*)fx_malloc(biasbufsize);
    if (conv->w_scale) {
        memset(conv->w_scale, 0, biasbufsize);
        conv->w_zp = (int32_t*)(conv->w_scale + nbias);
        conv->w_sum = conv->w_zp + nbias;
        conv->bias = conv->w_sum + nbias;

        if (conv->w_scale) {
            for(k = 0; k < K; k++) {
                int bias_k = bias ? bias[k] : 0;
                conv->bias[k] = bias_k;
                int w_idx = w_scale_size == K ? k : 0;
                conv->w_scale[k] = w_scale[w_idx];
                conv->w_zp[k] = (((const uint8_t*)w_zp)[w_idx] ^ w_mask);
            }
        }
    }

    if (conv->conv_type == _FX_CONV_TYPE_DEPTHWISE) {
        // for depth-wise convolutions on NCHW data we just preserve the weights in KCHW layout,
        // but add some padding to make the weights array layout more SIMD-friendly
        int ksize = Hk*Wk;
        int Cx = (C + FX_QCONV_C - 1)/FX_QCONV_C;
        int padded_ksize = conv->padded_ksize;
        size_t dwbufsize = Cx*FX_QCONV_C*padded_ksize*sizeof(conv->depthwise_weights[0]);
        conv->depthwise_weights = (int16_t*)fx_malloc(dwbufsize);
        if (conv->depthwise_weights) {
            memset(conv->depthwise_weights, 0, dwbufsize);
            for(int c0 = 0; c0 < C; c0 += FX_QCONV_C) {
                int w_zp_c[FX_QCONV_C], w_sum_c[FX_QCONV_C];
                for (int j = 0; j < FX_QCONV_C; j++) {
                    w_zp_c[j] = conv->w_zp[c0 + j];
                    w_sum_c[j] = 0;
                }
                for (int yx = 0; yx < ksize; yx++) {
                    for (int j = 0; j < FX_QCONV_C; j++) {
                        int w = c0 + j < C ? ((uint8_t*)weights)[(c0 + j)*ksize + yx] : 0;
                        w = (w ^ w_mask) - w_zp_c[j];
                        conv->depthwise_weights[c0*padded_ksize + yx*FX_QCONV_C + j] = (int16_t)w;
                        w_sum_c[j] += w;
                    }
                }
                for (int j = 0; j < FX_QCONV_C; j++)
                    conv->w_sum[c0 + j] = w_sum_c[j];
            }
        }
    } else {
        int Kg = K/ngroups, Cg = C/ngroups;
        // the weights are packed as
        // ngroups x (ceil((K/ngroups)/FX_QCONV_MR)*FX_QCONV_MR) x (Cg*Hk*Wk) x FX_QCONV_MR tensor
        int Kg_aligned = ((Kg + FX_QCONV_MR - 1)/FX_QCONV_MR)*FX_QCONV_MR;
        int Cg_aligned = ((Cg + FX_QCONV_C - 1)/FX_QCONV_C)*FX_QCONV_C;
        int HkWkCg_aligned = Hk*Wk*Cg_aligned;
        size_t nweights = (size_t)ngroups*Kg_aligned*HkWkCg_aligned;
        conv->weights = (uint8_t*)fx_malloc(nweights*sizeof(conv->weights[0]));
        if (conv->weights) {
            memset(conv->weights, 0, nweights*sizeof(conv->weights[0]));
            uint8_t* packed_wptr = conv->weights;
            for(int g = 0; g < ngroups; g++) {
                for(int k0 = 0; k0 < Kg_aligned; k0 += FX_QCONV_MR) {
                    int dk = Kg - k0 < FX_QCONV_MR ? Kg - k0 : FX_QCONV_MR;
                    int k_idx0 = g*Kg + k0;
                    for(int yx = 0; yx < Hk*Wk; yx++) {
                        for(int c0 = 0; c0 < Cg; c0 += FX_QCONV_C,
                                                 packed_wptr += FX_QCONV_MR*FX_QCONV_C)
                        {
                            const uint8_t* wptr = (const uint8_t*)weights +
                                    (size_t)(k_idx0*Cg + c0)*Hk*Wk + yx;
                            int32_t* w_sum = conv->w_sum + k_idx0;
                            int k = 0;
                            for(; k < dk; k++, wptr += Cg*Hk*Wk) {
                                for (int c = 0; c < FX_QCONV_C; c++) {
                                    int w = (c0 + c < Cg ? wptr[Hk*Wk*c] : 0) ^ w_mask;
                                    packed_wptr[k*FX_QCONV_C + c] = (uint8_t)w;
                                    w_sum[k] += (uint8_t)w;
                                }
                            }
                            for(; k < FX_QCONV_MR; k++) {
                                for (int c = 0; c < FX_QCONV_C; c++) {
                                    packed_wptr[k*FX_QCONV_C + c] = w_mask;
                                    w_sum[k] += w_mask;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if (conv && conv->w_scale && (conv->weights || conv->depthwise_weights))
        fx_status = fx_make_cptr(conv, _fx_free_qconv2d, fx_result);
    else
        fx_status = FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
    if (fx_status < 0)
        _fx_free_qconv2d(conv);
    return fx_status;
}

static int _fx_qconv2d( const _fx_nntensor_t* inp, float inp_scale0, int inp_zp0_,
                       _fx_nntensor_t* out, float out_scale0, int out_zp0_,
                       _fx_qconv2d_t* conv, int_ ntasks,
                       fx_arr_t* curr_scratch_buf, fx_arr_t* new_scratch_buf )
{
    int inp_typ = inp->data.tag;
    int out_typ = out->data.tag;
    int w_typ = conv->w_typ;

    // [TODO] sometimes we deal with small kernels and few input channels,
    // then C_BLOCK_SIZE=256 is probably greater than HkWkCg and then we can,
    // given constant L1 cache size, increase K_BLOCK_SIZE.
    // it can also be that the number of output channels is small.
    // then we can increase the amount of pixels (yx_limit - yx0) that we process at once.
    // That is, we should make K_BLOCK_SIZE and C_BLOCK_SIZE adaptive.
    int MAX_STRIPES = (56 + FX_QCONV_NR - 1)/FX_QCONV_NR;
    int K_BLOCK_SIZE = (32 + FX_QCONV_C*FX_QCONV_C - 1)/(FX_QCONV_C*FX_QCONV_C);
    int C_BLOCK_SIZE = 1024;

    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    const int_* inp_shape = (const int_*)(inp_shape_->data);
    const int_* out_shape = (const int_*)(out_shape_->data);
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    size_t c_esz = sizeof(int);
    int ndims = inp_shape_->dim[0].size;
    int out_ndims = out_shape_->dim[0].size;

    int xc = inp_shape[4];
    int N = inp_shape[0], C = conv->C, K = conv->K;
    int Hi = ndims >= 5 ? inp_shape[2] : 0, Wi = ndims >= 5 ? inp_shape[3] : 0;
    int H0 = out_ndims >= 5 ? out_shape[2] : 0, W0 = out_ndims >= 5 ? out_shape[3] : 0;
    int inp_planesize = Hi*Wi, out_planesize = H0*W0;
    int Hk = conv->Hk, Wk = conv->Wk, ksize = Hk*Wk, ngroups = conv->ngroups;
    int Cg = C/ngroups, Kg = K/ngroups;
    int Cg_nblocks = (Cg + FX_QCONV_C - 1)/FX_QCONV_C;
    int Cg_aligned = Cg_nblocks*FX_QCONV_C;
    int Kg_nblocks = (Kg + FX_QCONV_MR-1)/FX_QCONV_MR;
    int Kg_aligned = Kg_nblocks*FX_QCONV_MR;
    int stripes_per_plane = (out_planesize + FX_QCONV_NR - 1)/FX_QCONV_NR;
    bool parallel_by_K = stripes_per_plane < ntasks*4;
    int Kstripes = (parallel_by_K ? Kg_nblocks : 1)*
                   (parallel_by_K ? 1 : stripes_per_plane);
    int nsubtasks = N*ngroups*Kstripes;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    int pad_x = pad_left + pad_right;
    bool fast_1x1 = stride_x == 1 && stride_y == 1 && ksize == 1;
    int HkWkCg_aligned = ksize*Cg_aligned;
    size_t stripesize = FX_QCONV_NR*HkWkCg_aligned;
    size_t taskbufsize = (stripesize + FX_QCONV_NR*FX_QCONV_C*(K_BLOCK_SIZE+1)*c_esz)*MAX_STRIPES;
    size_t totalbufsize = taskbufsize*ntasks;
    char* inpbuf_all = 0;
    int* localbuf = (int*)alloca(ksize*3*sizeof(localbuf[0]) + FX_QCONV_NR);
    int* ofstab = localbuf;
    int* yxtab = ofstab + ksize;
    int inp_mask = inp_typ == FX_I8 ? 128 : 0;
    int out_mask = out_typ == FX_I8 ? 128 : 0;
    int inp_zp0 = inp_zp0_ + inp_mask;
    uint8_t inp_zp0_b = (uint8_t)inp_zp0;
    const uint32_t inp_zp0_d = inp_zp0_b | (inp_zp0_b << 8) | (inp_zp0_b << 16) | (inp_zp0_b << 24);
    int out_zp0 = out_zp0_ + out_mask;
    int status = 0;
    float inv_out_scale0 = out_scale0 == 0.f ? 0.f : 1.f/out_scale0;

    if (xc != 4)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (ndims != 5 || inp_shape[0] != out_shape[0] ||
        inp_shape[1] != (conv->C + xc-1)/xc || out_shape[1] != (conv->K+xc-1)/xc)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if ((inp_typ != FX_I8 && inp_typ != FX_U8) ||
        (w_typ != FX_I8 && w_typ != FX_U8) ||
        (out_typ != FX_I8 && out_typ != FX_U8))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ksize == 1) {
        if((pad_left | pad_right | pad_top | pad_bottom) != 0)
            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        if (stride_x == 1 && stride_y == 1 && (H0 != Hi || W0 != Wi))
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    }

    _fx_calc_ofstab2d(Wi, Hk, Wk, dilation_y, dilation_x, yxtab, ofstab);
    if (conv->conv_type == _FX_CONV_TYPE_DEPTHWISE) {
        _fx_depthwise2d_t dw_ctx;
        _fx_init_depthwise2d(N, Hi, Wi, H0, W0, Hk, Wk,
                            stride_y, stride_x, dilation_y, dilation_x,
                            pad_top, pad_left, pad_bottom, pad_right,
                            yxtab, ofstab, &dw_ctx);

        int status = _fx_depthwise_qconv2d_u8(&dw_ctx, conv,
            inp_typ, (const uint8_t*)inp_data->data, inp_scale0, inp_zp0,
            out_typ, (uint8_t*)out_data->data, out_scale0, out_zp0,
            (int)ntasks);

        if (status >= 0)
            fx_copy_arr(curr_scratch_buf, new_scratch_buf);
        return status;
    }

    if (totalbufsize > curr_scratch_buf->dim[0].step*curr_scratch_buf->dim[0].size) {
        int_ totalbufsz = (int_)totalbufsize;
        int status = fx_make_arr(1, &totalbufsz, 1, 0, 0, 0, new_scratch_buf);
        if (status < 0)
            return status;
    } else {
        fx_copy_arr(curr_scratch_buf, new_scratch_buf);
    }
    inpbuf_all = new_scratch_buf->data;

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int32_t* cbuf_task = (int32_t*)&inpbuf_all[taskbufsize*task_id];
        int32_t* isum_task = cbuf_task + FX_QCONV_NR*MAX_STRIPES*K_BLOCK_SIZE*FX_QCONV_C;
        uint32_t* inpbuf_task = (uint32_t*)(isum_task + FX_QCONV_NR*MAX_STRIPES);
        int ngs0 = (int)((size_t)nsubtasks*task_id/ntasks);
        int ngs1 = (int)((size_t)nsubtasks*(task_id+1)/ntasks);
        for (int subtask = ngs0; subtask < ngs1; ) {
            int ng = subtask / Kstripes;
            int kyx0 = subtask - ng*Kstripes;
            int kyx1 = kyx0 + (ngs1 - subtask);
            int n = ng/ngroups, g = ng - n*ngroups;
            size_t inp_plane_ofs = (size_t)(n*ngroups + g)*Cg_nblocks*inp_planesize;
            kyx1 = kyx1 <= Kstripes ? kyx1 : Kstripes;
            subtask += kyx1 - kyx0;
            int k0, k1;
            int yx0, yx_limit, yx_block_limit = 0;
            if (parallel_by_K) {
                k0 = kyx0;
                k1 = kyx1;
                k1 = k1 <= Kg_nblocks ? k1 : Kg_nblocks;
                yx0 = 0;
                yx_limit = out_planesize;
            } else {
                k0 = 0;
                k1 = Kg_nblocks;
                yx0 = kyx0*FX_QCONV_NR;
                yx_limit = kyx1*FX_QCONV_NR;
                yx_limit = yx_limit < out_planesize ? yx_limit : out_planesize;
            }

            for (; yx0 < yx_limit; yx0 = yx_block_limit) {
                // step 1. extract part of input tensor and represent it in zigzag form
                yx_block_limit = yx0 + FX_QCONV_NR*MAX_STRIPES;
                yx_block_limit = yx_block_limit < yx_limit ? yx_block_limit : yx_limit;
                int nstripes = (yx_block_limit - yx0 + FX_QCONV_NR - 1)/FX_QCONV_NR;
                int yx0_saved = yx0;
                assert(nstripes <= MAX_STRIPES);
                for (int stripe = 0; yx0 < yx_block_limit; stripe++, yx0 += FX_QCONV_NR) {
                    uint32_t* inpbuf = inpbuf_task + stripe*(stripesize/FX_QCONV_C);
                    const uint32_t* inptr = (const uint32_t*)inp_data->data + inp_plane_ofs;
                    //
                    //   1. pack the data. Copy the HkxWk FX_QCONV_NR-wide slices from
                    //     each feature plane of the input tensor to the input buffer.
                    //
                    if (fast_1x1) {
                        int slice_len = yx_block_limit - yx0;
                        bool partial = slice_len < FX_QCONV_NR;
                        inptr += yx0;
                        if (!partial) {
                            // Make special branch where memcpy() is called with a constant buffer size.
                            // Compilers will likely unroll this loop properly.
                            for (int c = 0; c < Cg_nblocks; c++,
                                                    inptr += inp_planesize,
                                                    inpbuf += FX_QCONV_NR)
                                memcpy(inpbuf, inptr, FX_QCONV_NR*xc);
                        } else {
                            for (int c = 0; c < Cg_nblocks; c++,
                                                    inptr += inp_planesize,
                                                    inpbuf += FX_QCONV_NR) {
                                memcpy(inpbuf, inptr, slice_len*xc);
                                memset(inpbuf + slice_len, 0,
                                       (FX_QCONV_NR - slice_len)*xc);
                            }
                        }
                    } else {
                        int y0_ = yx0/W0, x0_ = yx0 - y0_*W0;
                        for (int k = 0; k < ksize; k++) {
                            int dy = yxtab[k*2], dx = yxtab[k*2+1];
                            int i = 0, y0 = y0_, x0 = x0_;
                            for (; i < FX_QCONV_NR;) {
                                uint32_t* inpbuf_ki = inpbuf + k*(FX_QCONV_NR*Cg_nblocks) + i;
                                int yi = y0*stride_y + dy - pad_top;
                                int xi = x0*stride_x + dx - pad_left;

                                if ((unsigned)yi < (unsigned)Hi &&
                                    (unsigned)xi < (unsigned)Wi) {
                                    const uint32_t* inptr_ki = inptr + yi*Wi + xi;
                                    if (i + 6 <= FX_QCONV_NR && x0 + 6 <= W0 &&
                                        xi + stride_x*6 <= Wi) {
                                        if (stride_x == 1) {
                                            for (int c = 0; c < Cg_nblocks; c++,
                                                    inpbuf_ki += FX_QCONV_NR,
                                                    inptr_ki += inp_planesize) {
                                                uint32_t t0 = inptr_ki[0], t1 = inptr_ki[1];
                                                uint32_t t2 = inptr_ki[2], t3 = inptr_ki[3];
                                                uint32_t t4 = inptr_ki[4], t5 = inptr_ki[5];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                                inpbuf_ki[4] = t4; inpbuf_ki[5] = t5;
                                            }
                                        } else {
                                            for (int c = 0; c < Cg_nblocks; c++,
                                                    inpbuf_ki += FX_QCONV_NR,
                                                    inptr_ki += inp_planesize) {
                                                uint32_t t0 = inptr_ki[0];
                                                uint32_t t1 = inptr_ki[stride_x];
                                                uint32_t t2 = inptr_ki[stride_x*2];
                                                uint32_t t3 = inptr_ki[stride_x*3];
                                                uint32_t t4 = inptr_ki[stride_x*4];
                                                uint32_t t5 = inptr_ki[stride_x*5];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                                inpbuf_ki[4] = t4; inpbuf_ki[5] = t5;
                                            }
                                        }
                                        i += 6;
                                        x0 += 6;
                                    } else {
                                        for (int c = 0; c < Cg_nblocks; c++,
                                                inpbuf_ki += FX_QCONV_NR,
                                                inptr_ki += inp_planesize)
                                            *inpbuf_ki = *inptr_ki;
                                        i++;
                                        x0++;
                                    }
                                } else {
                                    for (int c = 0; c < Cg_nblocks; c++,
                                                inpbuf_ki += FX_QCONV_NR)
                                        *inpbuf_ki = inp_zp0_d;
                                    i++;
                                    x0++;
                                }
                                int mask = x0 >= W0;
                                y0 += mask;
                                x0 &= mask-1;
                            }
                        }
                    }
                }
                yx0 = yx0_saved;

                for (int stripe = 0; stripe < nstripes; stripe++) {
                    int32_t* inpsumptr = isum_task + stripe*FX_QCONV_NR;
                #ifdef __ARM_NEON
                    #if FX_QCONV_NR != 28
                    #error "unsupported FX_QCONV_NR (must be 28)"
                    #endif
                    if (inp_mask == 0) {
                        uint32x4_t s0 = vdupq_n_u32(0), s1 = s0, s2 = s0,
                                s3 = s0, s4 = s0, s5 = s0, s6 = s0;
                        uint8x16_t _1s_ = vdupq_n_u8(1);
                        for (int p = 0; p < HkWkCg_aligned; p += FX_QCONV_C) {
                            uint8_t* inptr = (uint8_t*)inpbuf_task +
                                                stripe*stripesize + p*FX_QCONV_NR;
                            uint8x16_t t0 = vld1q_u8(inptr);
                            uint8x16_t t1 = vld1q_u8(inptr + 16);
                            uint8x16_t t2 = vld1q_u8(inptr + 16*2);
                            uint8x16_t t3 = vld1q_u8(inptr + 16*3);
                            uint8x16_t t4 = vld1q_u8(inptr + 16*4);
                            uint8x16_t t5 = vld1q_u8(inptr + 16*5);
                            uint8x16_t t6 = vld1q_u8(inptr + 16*6);
                            s0 = vdotq_u32(s0, t0, _1s_);
                            s1 = vdotq_u32(s1, t1, _1s_);
                            s2 = vdotq_u32(s2, t2, _1s_);
                            s3 = vdotq_u32(s3, t3, _1s_);
                            s4 = vdotq_u32(s4, t4, _1s_);
                            s5 = vdotq_u32(s5, t5, _1s_);
                            s6 = vdotq_u32(s6, t6, _1s_);
                        }
                        vst1q_u32((uint32_t*)inpsumptr, s0);
                        vst1q_u32((uint32_t*)inpsumptr + 4, s1);
                        vst1q_u32((uint32_t*)inpsumptr + 8, s2);
                        vst1q_u32((uint32_t*)inpsumptr + 12, s3);
                        vst1q_u32((uint32_t*)inpsumptr + 16, s4);
                        vst1q_u32((uint32_t*)inpsumptr + 20, s5);
                        vst1q_u32((uint32_t*)inpsumptr + 24, s6);
                    } else
                #endif
                    {
                        for (int j = 0; j < FX_QCONV_NR; j++)
                            inpsumptr[j] = 0;
                        for (int p = 0; p < HkWkCg_aligned; p += FX_QCONV_C) {
                            uint8_t* inptr = (uint8_t*)inpbuf_task +
                            stripe*stripesize + p*FX_QCONV_NR;
                            if (inp_mask == 0) {
                                for (int j = 0; j < FX_QCONV_NR; j++) {
                                    inpsumptr[j] += inptr[j*FX_QCONV_C] + inptr[j*FX_QCONV_C+1] +
                                    inptr[j*FX_QCONV_C+2] + inptr[j*FX_QCONV_C+3];
                                }
                            } else {
                                for (int j = 0; j < FX_QCONV_NR; j++) {
                                    int t0 = inptr[j*FX_QCONV_C] ^ inp_mask;
                                    int t1 = inptr[j*FX_QCONV_C+1] ^ inp_mask;
                                    int t2 = inptr[j*FX_QCONV_C+2] ^ inp_mask;
                                    int t3 = inptr[j*FX_QCONV_C+3] ^ inp_mask;
                                    inpsumptr[j] += t0 + t1 + t2 + t3;
                                    inptr[j*FX_QCONV_C] = (uint8_t)t0;
                                    inptr[j*FX_QCONV_C+1] = (uint8_t)t1;
                                    inptr[j*FX_QCONV_C+2] = (uint8_t)t2;
                                    inptr[j*FX_QCONV_C+3] = (uint8_t)t3;
                                }
                            }
                        }
                    }
                }

                const uint32_t* weights = (const uint32_t*)conv->weights + g*Kg_nblocks*HkWkCg_aligned;
                const int32_t* biasptr = conv->bias + Kg*g;
                const int32_t* w_sum = conv->w_sum + Kg*g;
                const float* w_scale = conv->w_scale + Kg*g;
                const int32_t* w_zp = conv->w_zp + Kg*g;
                int ldc = nstripes*FX_QCONV_NR;
                // 2. do convolution, compute Kg x (yx_block_limit - yx0) part of the output tensor
                for (int k0_block = k0; k0_block < k1; k0_block += K_BLOCK_SIZE) {
                    int k1_block = k0_block + K_BLOCK_SIZE < k1 ? k0_block + K_BLOCK_SIZE : k1;
                    for (int c0 = 0; c0 < HkWkCg_aligned; c0 += C_BLOCK_SIZE) {
                        int c1 = c0 + C_BLOCK_SIZE < HkWkCg_aligned ? c0 + C_BLOCK_SIZE : HkWkCg_aligned;
                        for (int stripe = 0; stripe < nstripes; stripe++) {
                            const uint32_t* wptr = (const uint32_t*)weights + k0_block*HkWkCg_aligned + c0;
                            const uint32_t* inptr = inpbuf_task + stripe*(stripesize/FX_QCONV_C) + (c0/FX_QCONV_C)*FX_QCONV_NR;
                            int32_t* cptr = cbuf_task + stripe*FX_QCONV_NR;
                            for (int k = k0_block; k < k1_block; k++,
                                    wptr += HkWkCg_aligned,
                                    cptr += FX_QCONV_C*ldc) {
                                _fx_conv_update_block_u8(c1 - c0, wptr, inptr, cptr, ldc, c0 == 0);
                            }
                        }
                    }

                    size_t outofs = (((n*ngroups + g)*Kg_nblocks + k0_block)*out_planesize + yx0)*FX_QCONV_C;
                    int out_width = yx_block_limit - yx0;
                    const int32_t* cptr = (const int32_t*)cbuf_task;
                    uint8_t* outptr = (uint8_t*)out_data->data + outofs;

                    for (int k = k0_block; k < k1_block; k++,
                        cptr += ldc*FX_QCONV_C, outptr += out_planesize*FX_QCONV_C)
                    {
                        float scale[4], biasval[4];
                        int w_zp_k[4], j;
                        for (j = 0; j < 4; j++) {
                            float sc = inp_scale0*inv_out_scale0*w_scale[k*FX_QCONV_C+j];
                            scale[j] = sc;
                            w_zp_k[j] = -w_zp[k*FX_QCONV_C+j];
                            biasval[j] = biasptr[k*FX_QCONV_C+j]*sc + out_zp0 -
                                sc*inp_zp0*(HkWkCg_aligned*w_zp_k[j] + w_sum[k*FX_QCONV_C+j]);
                        }
                        j = 0;
                    #ifdef __ARM_NEON
                        int32x4_t vw_zp = vld1q_s32(w_zp_k);
                        float32x4_t vscale = vld1q_f32(scale);
                        float32x4_t vbias = vld1q_f32(biasval);
                        uint8x16_t vout_mask = vdupq_n_u8((uint8_t)out_mask);
                        for (; j < out_width; j += 4) {
                            if (j + 4 > out_width) {
                                if (j == 0)
                                    break;
                                j = out_width - 4;
                            }
                            int32x4_t c0 = vld1q_s32(cptr + j);
                            int32x4_t c1 = vld1q_s32(cptr + j + ldc);
                            int32x4_t c2 = vld1q_s32(cptr + j + ldc*2);
                            int32x4_t c3 = vld1q_s32(cptr + j + ldc*3);
                            int32x4_t isum0 = vld1q_s32(isum_task + j);
                            int32x4x2_t tr0, tr1;
                            tr0 = vtrnq_s32(c0, c1);
                            tr1 = vtrnq_s32(c2, c3);
                            c0 = vcombine_s32(vget_low_s32(tr0.val[0]), vget_low_s32(tr1.val[0]));
                            c1 = vcombine_s32(vget_low_s32(tr0.val[1]), vget_low_s32(tr1.val[1]));
                            c2 = vcombine_s32(vget_high_s32(tr0.val[0]), vget_high_s32(tr1.val[0]));
                            c3 = vcombine_s32(vget_high_s32(tr0.val[1]), vget_high_s32(tr1.val[1]));
                            c0 = vmlaq_laneq_s32(c0, vw_zp, isum0, 0);
                            c1 = vmlaq_laneq_s32(c1, vw_zp, isum0, 1);
                            c2 = vmlaq_laneq_s32(c2, vw_zp, isum0, 2);
                            c3 = vmlaq_laneq_s32(c3, vw_zp, isum0, 3);
                            float32x4_t v0 = vfmaq_f32(vbias, vcvtq_f32_s32(c0), vscale);
                            float32x4_t v1 = vfmaq_f32(vbias, vcvtq_f32_s32(c1), vscale);
                            float32x4_t v2 = vfmaq_f32(vbias, vcvtq_f32_s32(c2), vscale);
                            float32x4_t v3 = vfmaq_f32(vbias, vcvtq_f32_s32(c3), vscale);
                            c0 = vcvtnq_s32_f32(v0);
                            c1 = vcvtnq_s32_f32(v1);
                            c2 = vcvtnq_s32_f32(v2);
                            c3 = vcvtnq_s32_f32(v3);
                            uint8x8_t b0 = vqmovn_u16(vcombine_u16(vqmovun_s32(c0), vqmovun_s32(c1)));
                            uint8x8_t b1 = vqmovn_u16(vcombine_u16(vqmovun_s32(c2), vqmovun_s32(c3)));
                            vst1q_u8(outptr + j*FX_QCONV_C,
                                     veorq_u8(vcombine_u8(b0, b1), vout_mask));
                        }
                    #endif
                        for (; j < out_width; j++) {
                            int isum_j = isum_task[j];
                            int delta_jk0 = w_zp_k[0]*isum_j;
                            int delta_jk1 = w_zp_k[1]*isum_j;
                            int delta_jk2 = w_zp_k[2]*isum_j;
                            int delta_jk3 = w_zp_k[3]*isum_j;
                            int v0, v1, v2, v3;
                            v0 = (int)lrintf((cptr[j] + delta_jk0)*scale[0] + biasval[0]);
                            v1 = (int)lrintf((cptr[j+ldc] + delta_jk1)*scale[1] + biasval[1]);
                            v2 = (int)lrintf((cptr[j+ldc*2] + delta_jk2)*scale[2] + biasval[2]);
                            v3 = (int)lrintf((cptr[j+ldc*3] + delta_jk3)*scale[3] + biasval[3]);
                            outptr[j*FX_QCONV_C] = FX_SATURATE(v0, out_mask);
                            outptr[j*FX_QCONV_C+1] = FX_SATURATE(v1, out_mask);
                            outptr[j*FX_QCONV_C+2] = FX_SATURATE(v2, out_mask);
                            outptr[j*FX_QCONV_C+3] = FX_SATURATE(v3, out_mask);
                        }
                    }
                }
            }
        }
    }
    return FX_OK;
}
}

fun init_qconv( kernel_shape: int [], strides: int [],
                dilations: int [], pads: int [], group: int,
                weights: Ast.nntensor_t, w_scale: Ast.nntensor_t,
                w_zp: Ast.nntensor_t, bias: Ast.nntensor_t ): cptr
@ccode
{
    int w_typ = weights->data.tag, b_typ = bias->data.tag;
    int w_scale_typ = w_scale->data.tag, w_zp_typ = w_zp->data.tag;
    const fx_arr_t* w_data = &weights->data.u.NN_Data_I8;
    const fx_arr_t* w_shape = &weights->shape.shape;
    const fx_arr_t* w_scale_data = &w_scale->data.u.NN_Data_I8;
    const fx_arr_t* w_zp_data = &w_zp->data.u.NN_Data_I8;
    const fx_arr_t* bias_data = &bias->data.u.NN_Data_I8;
    const int_* w_shape_ = (const int_*)w_shape->data;
    const int_* strides_ = (const int_*)strides->data;
    const int_* dilations_ = (const int_*)dilations->data;
    const int_* pads_ = (const int_*)pads->data;
    int_ K = w_shape_ ? w_shape_[0] : 0;
    int_ w_scale_size = w_scale_typ <= 1 ? 0 : w_scale_data->dim[0].size;
    const float* bn_data_[4] = {0, 0, 0, 0};
    if (w_shape->ndims != 1 || w_shape->dim[0].size != 4 ||
        strides->ndims != 1 || strides->dim[0].size != 2 ||
        dilations->ndims != 1 || dilations->dim[0].size != 2 ||
        pads->ndims != 1 || pads->dim[0].size != 4)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);

    if ((w_typ != FX_I8 && w_typ != FX_U8) ||
        w_scale_typ != FX_F32 || w_zp_typ != w_typ ||
        (b_typ > 1 && b_typ != FX_I32))
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    if ((w_scale_size != 1 && w_scale_size != K) ||
        w_zp_data->dim[0].size != w_scale_size ||
        (b_typ > 1 && bias_data->dim[0].size != K))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    return _fx_init_qconv2d(_FX_NN_Layout_NCHW, _FX_NN_Layout_NCHW, (int)group,
        (int)w_shape_[0], (int)w_shape_[1]*group, (int)w_shape_[2], (int)w_shape_[3],
        (int)strides_[0], (int)strides_[1], (int)dilations_[0], (int)dilations_[1],
        (int)pads_[0], (int)pads_[1], (int)pads_[2], (int)pads_[3],
        (int)w_scale_size, w_typ, w_data->data, (const float*)w_scale_data->data,
        w_zp_data->data, b_typ == FX_I32 ? (const int32_t*)bias_data->data : 0,
        fx_result);
}

fun run_qconv(inp: Ast.nntensor_t, inp_scale0: float, inp_zp0: int,
              out: Ast.nntensor_t, out_scale0: float, out_zp0: int,
              qconv_data: cptr, ntasks: int, scratch_buf: Ast.nnbuf_t): Ast.nnbuf_t
@ccode {
    _fx_qconv2d_t* qconv = qconv_data && qconv_data->ptr ?
        (_fx_qconv2d_t*)qconv_data->ptr : 0;
    if (!qconv)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
    return _fx_qconv2d((const _fx_nntensor_t*)inp, inp_scale0, (int)inp_zp0,
                       (_fx_nntensor_t*)out, out_scale0, (int)out_zp0,
                       qconv, ntasks, scratch_buf, fx_result);
}

fun run_qconv(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_QLinearConv {attr={kernel_shape, pads, strides, dilations, group},
        qconv_data, t_inp, t_inp_scale, t_inp_zp, t_weights, t_w_scale, t_w_zp,
        t_bias, t_out, t_out_scale, t_out_zp} =>
    val inp = model.get_tensor(t_inp)
    val inp_scale = model.get_tensor(t_inp_scale)
    val inp_zp = model.get_tensor(t_inp_zp)
    val out = model.get_tensor(t_out)
    val out_scale = model.get_tensor(t_out_scale)
    val out_zp = model.get_tensor(t_out_zp)
    assert(`kernel_shape.size() == 2 && inp.shape.shape.size() == 5`)
    if *qconv_data == null || !model.isconst(t_weights) || !model.isconst(t_w_scale) ||
        !model.isconst(t_w_zp) || !model.isconst(t_bias) {
        val weights = model.get_tensor(t_weights)
        val w_scale = model.get_tensor(t_w_scale)
        val w_zp = model.get_tensor(t_w_zp)
        val bias = model.get_tensor(t_bias)

        *qconv_data = null // first of all, release the previous data, if any
                          // this way we can immediately re-use the same chunk of memory
                          // for the updated convolution structure
        *qconv_data = init_qconv(kernel_shape, strides, dilations, pads, group,
                                weights, w_scale, w_zp, bias)
    }
    val inp_scale0 = inp_scale.data.float_scalar_or(1.f)
    val inp_zp0 = inp_zp.data.int_scalar_or(0)
    val out_scale0 = out_scale.data.float_scalar_or(1.f)
    val out_zp0 = out_zp.data.int_scalar_or(0)
    *model.scratch_buf = run_qconv(inp, inp_scale0, inp_zp0,
            out, out_scale0, out_zp0, *qconv_data,
            *model.ntasks, *model.scratch_buf)
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

/*fun run_conv_transposed(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_ConvTranspose {kernel_shape, pads, strides, dilations, group,
        out_shape, out_padding, t_inp, t_weights, t_bias, t_out} =>
    val out = model.get_tensor(t_out)
    match out.data {
    | Ast.NN_Data_FP32 out_data => for _@idx <- out_data {out_data[idx] = 0.f}
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}*/
