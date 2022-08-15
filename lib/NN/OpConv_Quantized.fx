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
    conv->conv_type = ngroups == K && ngroups == C ? FX_CONV_TYPE_DEPTHWISE :
                      Hk == 3 && Wk == 3 && dilation_y == 1 && dilation_x == 1 &&
                      stride_y == 1 && stride_x == 1 ? FX_CONV_TYPE_WINOGRAD3X3 : FX_CONV_TYPE_GENERIC;
    // so far we only have ARM implementation of Winograd-based 3x3 convolution
#if 1 //ndef __ARM_NEON
    if (conv->conv_type != FX_CONV_TYPE_DEPTHWISE)
        conv->conv_type = FX_CONV_TYPE_GENERIC;
#endif
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

    if (conv->conv_type == FX_CONV_TYPE_DEPTHWISE) {
        // for depth-wise convolutions on NCHW data we just preserve the weights in KCHW layout,
        // but add some padding to make the weights array layout more SIMD-friendly
        int ksize = Hk*Wk;
        int padded_ksize = conv->padded_ksize;
        size_t dwbufsize = C*padded_ksize*sizeof(conv->depthwise_weights[0]);
        conv->depthwise_weights = (int16_t*)fx_malloc(dwbufsize);
        if (conv->depthwise_weights) {
            memset(conv->depthwise_weights, 0, dwbufsize);
            for(int c = 0; c < C; c++) {
                int w_zp_c = conv->w_zp[c], w_sum_c = 0;
                for (int k = 0; k < ksize; k++) {
                    int w = (((const uint8_t*)weights)[c*ksize + k] ^ w_mask) - w_zp_c;
                    conv->depthwise_weights[c*padded_ksize + k] = (int16_t)w;
                    w_sum_c += w;
                }
                conv->w_sum[c] = w_sum_c;
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
                                    w_sum[k] += w;
                                }
                            }
                            for(; k < FX_QCONV_MR; k++) {
                                for (int c = 0; c < FX_QCONV_C; c++)
                                    packed_wptr[k*FX_QCONV_C + c] = w_mask;
                                w_sum[k] += w_mask*FX_QCONV_C;
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
    int K_BLOCK_SIZE = ((32*(inp_typ == FX_F16 ? 2 : 1) + FX_QCONV_MR - 1)/FX_QCONV_MR)*FX_QCONV_MR;
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

    int N = inp_shape[0], C = conv->C, K = conv->K;
    int Hi = ndims >= 4 ? inp_shape[2] : 0, Wi = ndims >= 4 ? inp_shape[3] : 0;
    int H0 = out_ndims >= 4 ? out_shape[2] : 0, W0 = out_ndims >= 4 ? out_shape[3] : 0;
    int inp_planesize = Hi*Wi, out_planesize = H0*W0;
    int Hk = conv->Hk, Wk = conv->Wk, ksize = Hk*Wk, ngroups = conv->ngroups;
    int Cg = C/ngroups, Kg = K/ngroups;
    int Cg_aligned = ((Cg + FX_QCONV_C - 1)/FX_QCONV_C)*FX_QCONV_C;
    int Kg_nblocks = (Kg + FX_QCONV_MR-1)/FX_QCONV_MR;
    int Kg_aligned = Kg_nblocks*FX_QCONV_MR;
    int stripes_per_sample = (out_planesize + FX_QCONV_NR - 1)/FX_QCONV_NR;
    if (stripes_per_sample < ntasks*4)
        stripes_per_sample = 1;
    else
        Kg_nblocks = 1;
    int Kstripes = Kg_nblocks*stripes_per_sample;
    int nsubtasks = N*ngroups*Kstripes;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    int pad_x = pad_left + pad_right;
    bool fast_1x1 = stride_x == 1 && stride_y == 1 && ksize == 1;
    bool s1d1 = stride_x == 1 && stride_y == 1 && dilation_x == 1 && dilation_y == 1;
    int HkWkCg_aligned = ksize*Cg_aligned;
    size_t stripesize = FX_QCONV_NR*HkWkCg_aligned;
    size_t taskbufsize = (stripesize + FX_QCONV_NR*(K_BLOCK_SIZE+1)*c_esz)*MAX_STRIPES;
    size_t totalbufsize = taskbufsize*ntasks;
    char* inpbuf_all = 0;
    int* localbuf = (int*)alloca(ksize*3*sizeof(localbuf[0]) + FX_QCONV_NR);
    int* ofstab = localbuf;
    int* yxtab = ofstab + ksize;
    uint8_t* zbuf = (uint8_t*)(yxtab + ksize*2);
    int inp_mask = inp_typ == FX_I8 ? 128 : 0;
    int out_mask = out_typ == FX_I8 ? 128 : 0;
    int inp_zp0 = inp_zp0_ + inp_mask;
    uint8_t inp_zp0_b = (uint8_t)inp_zp0;
    int out_zp0 = out_zp0_ + out_mask;
    int status = 0;
    float inv_out_scale0 = out_scale0 == 0.f ? 0.f : 1.f/out_scale0;

    if (ndims != 4 || inp_shape[0] != out_shape[0] ||
        inp_shape[1] != conv->C || out_shape[1] != conv->K)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if ((inp_typ != FX_I8 && inp_typ != FX_U8) ||
        (w_typ != FX_I8 && w_typ != FX_U8) ||
        (out_typ != FX_I8 && out_typ != FX_U8))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (s1d1 && (pad_right > conv->pad_right || pad_right < 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (ksize == 1) {
        if((pad_left | pad_right | pad_top | pad_bottom) != 0)
            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        if (stride_x == 1 && stride_y == 1 && (H0 != Hi || W0 != Wi))
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    }

    if (conv->conv_type == FX_CONV_TYPE_DEPTHWISE) {
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

    _fx_calc_ofstab2d(Wi, Hk, Wk, dilation_y, dilation_x, yxtab, ofstab);
    memset(zbuf, inp_zp0_b, FX_QCONV_NR);

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int32_t* cbuf_task = (int32_t*)&inpbuf_all[taskbufsize*task_id];
        int32_t* isum_task = cbuf_task + FX_QCONV_NR*MAX_STRIPES*K_BLOCK_SIZE;
        uint8_t* inpbuf_task = (uint8_t*)(isum_task + FX_QCONV_NR*MAX_STRIPES);
        int ngs0 = (int)((size_t)nsubtasks*task_id/ntasks);
        int ngs1 = (int)((size_t)nsubtasks*(task_id+1)/ntasks);
        for (int subtask = ngs0; subtask < ngs1; ) {
            int ng = subtask / Kstripes;
            int kyx0 = subtask - ng*Kstripes;
            int kyx1 = kyx0 + (ngs1 - subtask);
            int n = ng/ngroups, g = ng - n*ngroups;
            size_t inp_plane_ofs = (size_t)(n*ngroups + g)*Cg*inp_planesize;
            kyx1 = kyx1 <= Kstripes ? kyx1 : Kstripes;
            subtask += kyx1 - kyx0;
            int k0, k1;
            int yx0, yx_limit, yx_block_limit = 0;
            if (stripes_per_sample == 1) {
                k0 = kyx0 * FX_QCONV_MR;
                k1 = kyx1 * FX_QCONV_MR;
                k1 = k1 <= Kg ? k1 : Kg;
                yx0 = 0;
                yx_limit = out_planesize;
            } else {
                k0 = 0;
                k1 = Kg;
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
                    uint8_t* inpbuf = inpbuf_task + stripe*stripesize;
                    const uint8_t* inptr = (const uint8_t*)inp_data->data + inp_plane_ofs;
                    //
                    //   1. pack the data. Copy the HkxWk FX_QCONV_NR-wide slices from
                    //     each feature plane of the input tensor to the input buffer.
                    //
                    if (fast_1x1) {
                        int slice_len = yx_block_limit - yx0;
                        bool partial = slice_len < FX_QCONV_NR;
                        // super-fast branch for 1x1 convolutions with sy=sx=1.
                        // in this case each feature plane can be safely treated
                        // as 1D array and we just extract next portion
                        // of FX_QCONV_NR elements from each feature plane and
                        // put it together.
                        inptr += yx0;
                        int c = 0;
                        for (; c <= Cg - FX_QCONV_C; c += FX_QCONV_C,
                                                     inptr += inp_planesize*FX_QCONV_C,
                                                     inpbuf += FX_QCONV_NR*FX_QCONV_C) {
                            int j = 0;
                        #ifdef __ARM_NEON
                            for (; j < slice_len; j += 16) {
                                if (j + 16 > slice_len) {
                                    if (j == 0)
                                        break;
                                    j = slice_len - 16;
                                }
                                uint8x16_t t0 = vld1q_u8(inptr + j);
                                uint8x16_t t1 = vld1q_u8(inptr + j + inp_planesize);
                                uint8x16_t t2 = vld1q_u8(inptr + j + inp_planesize*2);
                                uint8x16_t t3 = vld1q_u8(inptr + j + inp_planesize*3);
                                uint8x16_t t02_0 = vzip1q_u8(t0, t2);
                                uint8x16_t t02_1 = vzip2q_u8(t0, t2);
                                uint8x16_t t13_0 = vzip1q_u8(t1, t3);
                                uint8x16_t t13_1 = vzip2q_u8(t1, t3);
                                uint8x16_t t0123_0 = vzip1q_u8(t02_0, t13_0);
                                uint8x16_t t0123_1 = vzip2q_u8(t02_0, t13_0);
                                uint8x16_t t0123_2 = vzip1q_u8(t02_1, t13_1);
                                uint8x16_t t0123_3 = vzip2q_u8(t02_1, t13_1);
                                vst1q_u8(inpbuf + j*FX_QCONV_C, t0123_0);
                                vst1q_u8(inpbuf + j*FX_QCONV_C + 16, t0123_1);
                                vst1q_u8(inpbuf + j*FX_QCONV_C + 32, t0123_2);
                                vst1q_u8(inpbuf + j*FX_QCONV_C + 48, t0123_3);
                            }
                        #endif
                            for (; j < slice_len; j++) {
                                uint8_t t0 = inptr[j];
                                uint8_t t1 = inptr[j + inp_planesize];
                                uint8_t t2 = inptr[j + inp_planesize*2];
                                uint8_t t3 = inptr[j + inp_planesize*3];
                                inpbuf[j*FX_QCONV_C] = t0;
                                inpbuf[j*FX_QCONV_C+1] = t1;
                                inpbuf[j*FX_QCONV_C+2] = t2;
                                inpbuf[j*FX_QCONV_C+3] = t3;
                            }
                            for (; j < FX_QCONV_NR; j++) {
                                inpbuf[j*FX_QCONV_C] = inpbuf[j*FX_QCONV_C+1] =
                                    inpbuf[j*FX_QCONV_C+2] = inpbuf[j*FX_QCONV_C+3] = inp_zp0_b;
                            }
                        }
                        if (c < Cg) {
                            const uint8_t* inptr0 = inptr;
                            const uint8_t* inptr1 = c+1 < Cg ? inptr + inp_planesize : zbuf;
                            const uint8_t* inptr2 = c+2 < Cg ? inptr + inp_planesize*2 : zbuf;
                            const uint8_t* inptr3 = c+3 < Cg ? inptr + inp_planesize*3 : zbuf;
                            int j = 0;
                            for (; j < slice_len; j++) {
                                uint8_t t0 = inptr0[j], t1 = inptr1[j];
                                uint8_t t2 = inptr2[j], t3 = inptr3[j];
                                inpbuf[j*FX_QCONV_C] = t0;
                                inpbuf[j*FX_QCONV_C+1] = t1;
                                inpbuf[j*FX_QCONV_C+2] = t2;
                                inpbuf[j*FX_QCONV_C+3] = t3;
                            }
                            for (; j < FX_QCONV_NR; j++) {
                                inpbuf[j*FX_QCONV_C] = inpbuf[j*FX_QCONV_C+1] =
                                    inpbuf[j*FX_QCONV_C+2] = inpbuf[j*FX_QCONV_C+3] = inp_zp0_b;
                            }
                        }
                    } else {
                        int y0_ = yx0/W0, x0_ = yx0 - y0_*W0;
                        for (int k = 0; k < ksize; k++) {
                            int dy = yxtab[k*2], dx = yxtab[k*2+1];
                            int i = 0, y0 = y0_, x0 = x0_;
                            for (; i < FX_QCONV_NR;) {
                                uint8_t* inpbuf_ki = inpbuf +
                                    k*(FX_QCONV_NR*Cg_aligned) + i*FX_QCONV_C;
                                int yi = y0*stride_y + dy - pad_top;
                                int xi = x0*stride_x + dx - pad_left;

                                #define _FX_PACK_U8_LOAD8(c, stride, ptr)   \
                                    uint8_t t0##c = *((ptr) + stride*0);    \
                                    uint8_t t1##c = *((ptr) + stride*1);    \
                                    uint8_t t2##c = *((ptr) + stride*2);    \
                                    uint8_t t3##c = *((ptr) + stride*3);    \
                                    uint8_t t4##c = *((ptr) + stride*4);    \
                                    uint8_t t5##c = *((ptr) + stride*5);    \
                                    uint8_t t6##c = *((ptr) + stride*6);    \
                                    uint8_t t7##c = *((ptr) + stride*7)

                                #define _FX_PACK_U8_STORE4(group)               \
                                    *(inpbuf_ki + group*4 + 0) = t##group##0;   \
                                    *(inpbuf_ki + group*4 + 1) = t##group##1;   \
                                    *(inpbuf_ki + group*4 + 2) = t##group##2;   \
                                    *(inpbuf_ki + group*4 + 3) = t##group##3

                                if ((unsigned)yi < (unsigned)Hi &&
                                    (unsigned)xi < (unsigned)Wi) {
                                    const uint8_t* inptr_ki = inptr + yi*Wi + xi;
                                    if (i + 8 <= FX_QCONV_NR && x0 + 8 <= W0 &&
                                        xi + stride_x*8 <= Wi) {
                                        int c = 0;
                                        if (stride_x == 1) {
                                            for (; c <= Cg - FX_QCONV_C; c += FX_QCONV_C,
                                                        inpbuf_ki += FX_QCONV_NR*FX_QCONV_C,
                                                        inptr_ki += inp_planesize*FX_QCONV_C) {
                                            #ifdef __ARM_NEON
                                                uint8x8_t t0 = vld1_u8(inptr_ki);
                                                uint8x8_t t1 = vld1_u8(inptr_ki + inp_planesize);
                                                uint8x8_t t2 = vld1_u8(inptr_ki + inp_planesize*2);
                                                uint8x8_t t3 = vld1_u8(inptr_ki + inp_planesize*3);
                                                uint8x8_t t02_0 = vzip1_u8(t0, t2);
                                                uint8x8_t t02_1 = vzip2_u8(t0, t2);
                                                uint8x8_t t13_0 = vzip1_u8(t1, t3);
                                                uint8x8_t t13_1 = vzip2_u8(t1, t3);
                                                uint8x8_t t0123_0 = vzip1_u8(t02_0, t13_0);
                                                uint8x8_t t0123_1 = vzip2_u8(t02_0, t13_0);
                                                uint8x8_t t0123_2 = vzip1_u8(t02_1, t13_1);
                                                uint8x8_t t0123_3 = vzip2_u8(t02_1, t13_1);
                                                vst1_u8(inpbuf_ki, t0123_0);
                                                vst1_u8(inpbuf_ki + 8, t0123_1);
                                                vst1_u8(inpbuf_ki + 16, t0123_2);
                                                vst1_u8(inpbuf_ki + 24, t0123_3);
                                            #else
                                                _FX_PACK_U8_LOAD8(0, 1, inptr_ki);
                                                _FX_PACK_U8_LOAD8(1, 1, inptr_ki + inp_planesize);
                                                _FX_PACK_U8_LOAD8(2, 1, inptr_ki + inp_planesize*2);
                                                _FX_PACK_U8_LOAD8(3, 1, inptr_ki + inp_planesize*3);
                                                _FX_PACK_U8_STORE4(0);
                                                _FX_PACK_U8_STORE4(1);
                                                _FX_PACK_U8_STORE4(2);
                                                _FX_PACK_U8_STORE4(3);
                                                _FX_PACK_U8_STORE4(4);
                                                _FX_PACK_U8_STORE4(5);
                                                _FX_PACK_U8_STORE4(6);
                                                _FX_PACK_U8_STORE4(7);
                                            #endif
                                            }
                                        } else {
                                            for (; c <= Cg - FX_QCONV_C; c += FX_QCONV_C,
                                                        inpbuf_ki += FX_QCONV_NR*FX_QCONV_C,
                                                        inptr_ki += inp_planesize*FX_QCONV_C) {
                                                _FX_PACK_U8_LOAD8(0, stride_x, inptr_ki);
                                                _FX_PACK_U8_LOAD8(1, stride_x, inptr_ki + inp_planesize);
                                                _FX_PACK_U8_LOAD8(2, stride_x, inptr_ki + inp_planesize*2);
                                                _FX_PACK_U8_LOAD8(3, stride_x, inptr_ki + inp_planesize*3);
                                                _FX_PACK_U8_STORE4(0);
                                                _FX_PACK_U8_STORE4(1);
                                                _FX_PACK_U8_STORE4(2);
                                                _FX_PACK_U8_STORE4(3);
                                                _FX_PACK_U8_STORE4(4);
                                                _FX_PACK_U8_STORE4(5);
                                                _FX_PACK_U8_STORE4(6);
                                                _FX_PACK_U8_STORE4(7);
                                            }
                                        }
                                        if (c < Cg) {
                                            const uint8_t* inptr0 = inptr_ki;
                                            const uint8_t* inptr1 = c+1 < Cg ?
                                                inptr_ki + inp_planesize : zbuf;
                                            const uint8_t* inptr2 = c+2 < Cg ?
                                                inptr_ki + inp_planesize*2 : zbuf;
                                            const uint8_t* inptr3 = c+3 < Cg ?
                                                inptr_ki + inp_planesize*3 : zbuf;
                                            _FX_PACK_U8_LOAD8(0, stride_x, inptr0);
                                            _FX_PACK_U8_LOAD8(1, stride_x, inptr1);
                                            _FX_PACK_U8_LOAD8(2, stride_x, inptr2);
                                            _FX_PACK_U8_LOAD8(3, stride_x, inptr3);
                                            _FX_PACK_U8_STORE4(0);
                                            _FX_PACK_U8_STORE4(1);
                                            _FX_PACK_U8_STORE4(2);
                                            _FX_PACK_U8_STORE4(3);
                                            _FX_PACK_U8_STORE4(4);
                                            _FX_PACK_U8_STORE4(5);
                                            _FX_PACK_U8_STORE4(6);
                                            _FX_PACK_U8_STORE4(7);
                                        }
                                        i += 8;
                                        x0 += 8;
                                    } else {
                                        int c = 0;
                                        for (; c <= Cg - FX_QCONV_C; c += FX_QCONV_C,
                                             inpbuf_ki += FX_QCONV_NR*FX_QCONV_C,
                                             inptr_ki += inp_planesize*FX_QCONV_C) {
                                            uint8_t t0 = inptr_ki[0];
                                            uint8_t t1 = inptr_ki[inp_planesize];
                                            uint8_t t2 = inptr_ki[inp_planesize*2];
                                            uint8_t t3 = inptr_ki[inp_planesize*3];
                                            inpbuf_ki[0] = t0;
                                            inpbuf_ki[1] = t1;
                                            inpbuf_ki[2] = t2;
                                            inpbuf_ki[3] = t3;
                                        }
                                        if (c < Cg) {
                                            uint8_t t0 = inptr_ki[0];
                                            uint8_t t1 = c + 1 < Cg ?
                                                inptr_ki[inp_planesize] : inp_zp0_b;
                                            uint8_t t2 = c + 2 < Cg ?
                                                inptr_ki[inp_planesize*2] : inp_zp0_b;
                                            uint8_t t3 = c + 3 < Cg ?
                                                inptr_ki[inp_planesize*3] : inp_zp0_b;
                                            inpbuf_ki[0] = t0;
                                            inpbuf_ki[1] = t1;
                                            inpbuf_ki[2] = t2;
                                            inpbuf_ki[3] = t3;
                                        }
                                        i++;
                                        x0++;
                                    }
                                } else {
                                    for (int c = 0; c < Cg; c += FX_QCONV_C,
                                            inpbuf_ki += FX_QCONV_NR*FX_QCONV_C) {
                                        inpbuf_ki[0] = inpbuf_ki[1] =
                                            inpbuf_ki[2] = inpbuf_ki[3] = inp_zp0_b;
                                    }
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

                memset(isum_task, 0, FX_QCONV_NR*nstripes*c_esz);
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
                            uint8_t* inptr = inpbuf_task + stripe*stripesize + p*FX_QCONV_NR;
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
                    for (int p = 0; p < HkWkCg_aligned; p += FX_QCONV_C) {
                        uint8_t* inptr = inpbuf_task + stripe*stripesize + p*FX_QCONV_NR;
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

                yx0 = yx0_saved;
                const uint8_t* weights = conv->weights + g*Kg_aligned*HkWkCg_aligned;
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
                            const uint8_t* wptr = weights + k0_block*HkWkCg_aligned + c0*FX_QCONV_MR;
                            const uint8_t* inptr = inpbuf_task + stripe*stripesize + c0*FX_QCONV_NR;
                            int32_t* cptr = cbuf_task + stripe*FX_QCONV_NR;
                            for (int k = k0_block; k < k1_block; k += FX_QCONV_MR,
                                    wptr += HkWkCg_aligned*FX_QCONV_MR,
                                    cptr += FX_QCONV_MR*ldc) {
                                _fx_conv_update_block_u8(c1 - c0, wptr, inptr, cptr, ldc, c0 == 0);
                            }
                        }
                    }

                    size_t outofs = ((n*ngroups + g)*Kg + k0_block)*out_planesize + yx0;
                    int out_width = yx_block_limit - yx0;
                    const int32_t* cptr = (const int32_t*)cbuf_task;
                    uint8_t* outptr = (uint8_t*)(out_data->data + outofs);

                    for (int k = k0_block; k < k1_block; k++,
                            cptr += ldc, outptr += out_planesize)
                    {
                        float scale = inp_scale0*inv_out_scale0*w_scale[k];
                        int w_zp_k = w_zp[k];
                        float biasval = biasptr[k]*scale + out_zp0 +
                            scale*inp_zp0*(HkWkCg_aligned*w_zp_k - w_sum[k]);
                        int j = 0;
                    #ifdef __ARM_NEON
                        int32x4_t vw_zp = vdupq_n_s32(-w_zp_k);
                        float32x4_t vscale = vdupq_n_f32(scale);
                        float32x4_t vbias = vdupq_n_f32(biasval);
                        uint8x8_t vout_mask = vdup_n_u8((uint8_t)out_mask);
                        for (; j < out_width; j += 8) {
                            if (j + 8 > out_width) {
                                if (j == 0)
                                    break;
                                j = out_width - 8;
                            }
                            int32x4_t c0 = vld1q_s32(cptr + j);
                            int32x4_t c1 = vld1q_s32(cptr + j + 4);
                            int32x4_t isum0 = vld1q_s32(isum_task + j);
                            int32x4_t isum1 = vld1q_s32(isum_task + j + 4);
                            c0 = vmlaq_s32(c0, isum0, vw_zp);
                            c1 = vmlaq_s32(c1, isum1, vw_zp);
                            float32x4_t v0 = vfmaq_f32(vbias, vcvtq_f32_s32(c0), vscale);
                            float32x4_t v1 = vfmaq_f32(vbias, vcvtq_f32_s32(c1), vscale);
                            c0 = vcvtnq_s32_f32(v0);
                            c1 = vcvtnq_s32_f32(v1);
                            uint16x4_t w0 = vqmovun_s32(c0);
                            uint16x4_t w1 = vqmovun_s32(c1);
                            uint8x8_t b = vqmovn_u16(vcombine_u16(w0, w1));
                            vst1_u8(outptr + j, veor_u8(b, vout_mask));
                        }
                    #endif
                        for (; j < out_width; j++) {
                            int delta_jk = w_zp_k*isum_task[j];
                            int v = (int)lrintf((cptr[j] - delta_jk)*scale + biasval);
                            outptr[j] = FX_SATURATE(v, out_mask);
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
    assert(`kernel_shape.size() == 2 && inp.shape.shape.size() == 4`)
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
