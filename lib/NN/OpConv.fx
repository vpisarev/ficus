/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast, OpConv_Block, OpConv_Depthwise

type activ_func_t =
    | ACTIV_NONE | ACTIV_RELU | ACTIV_CLIP | ACTIV_LRELU
    | ACTIV_SIGMOID | ACTIV_TANH | ACTIV_MISH

@ccode {
#include <assert.h>
#include <float.h>
#include <math.h>
#include "ficus_nn_common.h"

enum {
    _FX_WINO_STEP=6,
    _FX_WINO_KSIZE=3,
    _FX_WINO_SIZE=_FX_WINO_STEP+_FX_WINO_KSIZE-1,
    _FX_WINO_AREA=_FX_WINO_SIZE*_FX_WINO_SIZE
};

enum { FX_CONV_TYPE_GENERIC=0, FX_CONV_TYPE_DEPTHWISE=1, FX_CONV_TYPE_WINOGRAD3X3=2 };

void _fx_conv_update_block_f32( int np, const void* a_, const void* b_,
                                void* c_, int ldc, bool init_c );
void _fx_conv_update_block_f16( int np, const void* a_, const void* b_,
                                void* c_, int ldc, bool init_c );
int _fx_depthwise_conv2d(int ndims, int inp_typ,
                         const int_* inp_shape, const fx_arr_t* inp_data,
                         const int_* out_shape, fx_arr_t* out_data,
                         const _fx_conv2d_t* conv, int ntasks);

static void _fx_free_conv2d(void* conv_ptr)
{
    _fx_conv2d_t* conv = (_fx_conv2d_t*)conv_ptr;
    if(conv) {
        fx_free(conv->weights);
        fx_free(conv->wf16);
        fx_free(conv->bias);
        fx_free(conv->activ_params);
        fx_free(conv);
    }
}

static int _fx_init_conv2d(
    int layout_orig, int layout, int ngroups,
    int K, int C, int Hk, int Wk,
    int stride_y, int stride_x,
    int dilation_y, int dilation_x,
    int pad_top, int pad_left,
    int pad_bottom, int pad_right,
    const float* weights,
    const float* bias,
    const float* bn_mean,
    const float* bn_var,
    const float* bn_scale,
    const float* bn_shift,
    float bn_eps,
    int activ,
    const float* activ_params,
    int nactiv_params,
    fx_cptr_t* fx_result)
{
    _fx_conv2d_t* conv = (_fx_conv2d_t*)fx_malloc(sizeof(*conv));
    float* bn_ab = bn_mean || bn_var || bn_scale || bn_shift ?
        (float*)fx_malloc(K*2*sizeof(bn_ab[0])) : 0;
    int k, nbias = K + 32;
    int fx_status;

    memset(conv, 0, sizeof(*conv));
    assert(layout_orig == _FX_NN_Layout_NCHW && layout == _FX_NN_Layout_NCHW);
    assert(ngroups > 0 && K > 0 && C > 0 && K % ngroups == 0 && C % ngroups == 0);
    assert(Hk > 0 && Wk > 0);
    assert(stride_y > 0 && stride_x > 0);
    assert(dilation_y > 0 && dilation_x > 0);
    assert(pad_top >= 0 && pad_bottom >= 0 && pad_left >= 0 && pad_right >= 0);
    conv->layout = layout;
    conv->K = K; conv->C = C; conv->Hk = Hk; conv->Wk = Wk;
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
    conv->activ = activ;
    conv->minval = -FLT_MAX;
    conv->maxval = FLT_MAX;
    conv->alpha = 1.f;
    if (activ == _FX_ACTIV_NONE) {
        ;
    } else if (activ == _FX_ACTIV_RELU) {
        conv->alpha = 0.f;
        conv->minval = 0.f;
    } else if (activ == _FX_ACTIV_LRELU) {
        assert(nactiv_params == 1);
        conv->alpha = activ_params[0];
    } else if (activ == _FX_ACTIV_CLIP) {
        assert(nactiv_params == 2);
        conv->minval = activ_params[0];
        conv->maxval = activ_params[1];
        conv->alpha = conv->minval == 0.f ? 0.f : 1.f;
        conv->activ_func = _fx_nn_elemwise_clip_f32;
        conv->activ_func_f16 = _fx_nn_elemwise_clip_f16;
    } else if (activ == _FX_ACTIV_MISH) {
        conv->activ_func = _fx_nn_elemwise_mish_f32;
        conv->activ_func_f16 = _fx_nn_elemwise_mish_f16;
    } else if (activ == _FX_ACTIV_SIGMOID) {
        conv->activ_func = _fx_nn_elemwise_sigmoid_f32;
        conv->activ_func_f16 = _fx_nn_elemwise_sigmoid_f16;
    } else if (activ == _FX_ACTIV_TANH) {
        conv->activ_func = _fx_nn_elemwise_tanh_f32;
        conv->activ_func_f16 = _fx_nn_elemwise_tanh_f16;
    } else {
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }
    conv->nactiv_params = nactiv_params;
    if (nactiv_params > 0) {
        size_t param_sz = nactiv_params*sizeof(conv->activ_params[0]);
        conv->activ_params = (float*)fx_malloc(param_sz);
        memcpy(conv->activ_params, activ_params, param_sz);
    }

    if (bn_ab) {
        for(k = 0; k < K; k++) {
            float mean = bn_mean ? bn_mean[k] : 0.f;
            float vr = bn_var ? bn_var[k] : 1.f;
            float a = (bn_scale ? bn_scale[k] : 1.f)/sqrt(vr + bn_eps);
            float b = (bn_shift ? bn_shift[k] : 0.f) - a*mean;
            bn_ab[k] = a;
            bn_ab[k + K] = b;
        }
    }

    // store bias; append some zero's to make sure that
    // we can always read FX_CONV_MR elements starting from any valid index
    conv->bias = (float*)fx_malloc(nbias*sizeof(conv->bias[0]));
    if (conv->bias) {
        for(k = 0; k < K; k++) {
            float bias_k = bias ? bias[k] : 0.f;
            conv->bias[k] = bn_ab ? bn_ab[k + K] + bn_ab[k]*bias_k : bias_k;
        }

        for(; k < nbias; k++)
            conv->bias[k] = 0.f;
    }
    conv->wf16 = 0;

    if (conv->conv_type == FX_CONV_TYPE_DEPTHWISE) {
        // for depth-wise convolutions on NCHW data we just preserve the weights in KCHW layout,
        // but add some padding to make the weights array layout more SIMD-friendly
        int ksize = Hk*Wk;
        int padded_ksize = ((ksize + FX_VEC_NLANES-1)/FX_VEC_NLANES)*FX_VEC_NLANES;
        int nweights = C*padded_ksize;
        conv->weights = (float*)fx_malloc(nweights*sizeof(conv->weights[0]));
        if (conv->weights) {
            memset(conv->weights, 0, nweights*sizeof(conv->weights[0]));
            for(int c = 0; c < C; c++) {
                float scale = bn_ab ? bn_ab[c] : 1.f;
                for (int k = 0; k < ksize; k++)
                    conv->weights[c*padded_ksize + k] = weights[c*ksize + k]*scale;
            }
        }
    #if _FX_NN_ENABLE_FP16
        padded_ksize = ((ksize + FX_VEC_F16_NLANES-1)/FX_VEC_F16_NLANES)*FX_VEC_F16_NLANES;
        nweights = C*padded_ksize;
        conv->wf16 = (fx_f16*)fx_malloc(nweights*sizeof(conv->wf16[0]));
        if (conv->wf16) {
            memset(conv->wf16, 0, nweights*sizeof(conv->wf16[0]));
            for(int c = 0; c < C; c++) {
                float scale = bn_ab ? bn_ab[c] : 1.f;
                for (int k = 0; k < ksize; k++)
                    conv->wf16[c*padded_ksize + k] = FX_FLOAT16(weights[c*ksize + k]*scale);
            }
        }
    #endif
    } else if (conv->conv_type == FX_CONV_TYPE_WINOGRAD3X3) {
        const float G[] = {
            1.f, 0.f, 0.f,
            -2.f/9, -2.f/9, -2.f/9,
            -2.f/9, 2.f/9, -2.f/9,
            1.f/90, 1.f/45, 2.f/45,
            1.f/90, -1.f/45, 2.f/45,
            32.f/45, 16.f/45, 8.f/45,
            32.f/45, -16.f/45, 8.f/45,
            0.f, 0.f, 1.f
        };
        // the weights are packed as K * (C/ngroups) * W * W, where W is
        // the both height and width of the Winograd-transformed kernel
        int Cg = C/ngroups;
        size_t nweights = K*Cg*_FX_WINO_AREA;
        conv->weights = (float*)fx_malloc(nweights*sizeof(conv->weights[0]));
        memset(conv->weights, 0, nweights*sizeof(conv->weights[0]));
        for(int k = 0; k < K; k++) {
            float scale = bn_ab ? bn_ab[k] : 1.f;
            for(int c = 0; c < Cg; c++) {
                float GW[_FX_WINO_SIZE*3];
                fx_mpgemm(false, true, scale, 0.f,
                        _FX_WINO_SIZE, 3, FX_F32, G, 3, 1,
                        3, 3, FX_F32, weights + (k*Cg + c)*Hk*Wk, 3, 1,
                        FX_F32, GW, 3, 1);
                fx_mpgemm(false, true, 1.f, 0.f,
                        _FX_WINO_SIZE, 3, FX_F32, GW, 3, 1,
                        _FX_WINO_SIZE, 3, FX_F32, G, 3, 1,
                        FX_F32, conv->weights + (k*Cg + c)*_FX_WINO_AREA,
                        _FX_WINO_SIZE, 1);
            }
        }
    } else {
        int Kg = K/ngroups, Cg = C/ngroups;
#if _FX_NN_ENABLE_FP16
        {
        // the weights are packed as
        // ngroups x (ceil((K/ngroups)/FX_CONV_MR)*FX_CONV_MR) x (Cg*Hk*Wk) x FX_CONV_MR tensor
        int Kg_aligned = ((Kg + FX_CONV_MR_FP16 - 1)/FX_CONV_MR_FP16)*FX_CONV_MR_FP16;
        size_t nweights = (size_t)ngroups*Kg_aligned*Cg*Hk*Wk;
        conv->wf16 = (fx_f16*)fx_malloc(nweights*sizeof(conv->weights[0]));
        if (conv->wf16) {
            memset(conv->wf16, 0, nweights*sizeof(conv->wf16[0]));
            fx_f16* packed_wptr = conv->wf16;
            for(int g = 0; g < ngroups; g++) {
                for(int k0 = 0; k0 < Kg_aligned; k0 += FX_CONV_MR_FP16) {
                    int dk = Kg - k0 < FX_CONV_MR_FP16 ? Kg - k0 : FX_CONV_MR_FP16;
                    int k_idx = g*Kg + k0;
                    for(int yx = 0; yx < Hk*Wk; yx++) {
                        for(int c = 0; c < Cg; c++, packed_wptr += FX_CONV_MR_FP16) {
                            const float* wptr = weights + (size_t)(k_idx*Cg + c)*Hk*Wk + yx;
                            int k = 0;
                            for(; k < dk; k++, wptr += Cg*Hk*Wk) {
                                float scale = bn_ab ? bn_ab[k_idx+k] : 1.f;
                                packed_wptr[k] = (fx_f16)(*wptr*scale);
                            }
                            for(; k < FX_CONV_MR_FP16; k++)
                                packed_wptr[k] = (fx_f16)0.f;
                        }
                    }
                }
            }
        }
        }
#endif
        {
        // the weights are packed as
        // ngroups x (ceil((K/ngroups)/FX_CONV_MR)*FX_CONV_MR) x (Cg*Hk*Wk) x FX_CONV_MR tensor
        int Kg_aligned = ((Kg + FX_CONV_MR - 1)/FX_CONV_MR)*FX_CONV_MR;
        size_t nweights = (size_t)ngroups*Kg_aligned*Cg*Hk*Wk;
        conv->weights = (float*)fx_malloc(nweights*sizeof(conv->weights[0]));
        if (conv->weights) {
            memset(conv->weights, 0, nweights*sizeof(conv->weights[0]));
            float* packed_wptr = conv->weights;
            for(int g = 0; g < ngroups; g++) {
                for(int k0 = 0; k0 < Kg_aligned; k0 += FX_CONV_MR) {
                    int dk = Kg - k0 < FX_CONV_MR ? Kg - k0 : FX_CONV_MR;
                    int k_idx = g*Kg + k0;
                    for(int yx = 0; yx < Hk*Wk; yx++) {
                        for(int c = 0; c < Cg; c++, packed_wptr += FX_CONV_MR) {
                            const float* wptr = weights + (size_t)(k_idx*Cg + c)*Hk*Wk + yx;
                            int k = 0;
                            for(; k < dk; k++, wptr += Cg*Hk*Wk) {
                                float scale = bn_ab ? bn_ab[k_idx+k] : 1.f;
                                packed_wptr[k] = *wptr*scale;
                            }
                            for(; k < FX_CONV_MR; k++)
                                packed_wptr[k] = 0.f;
                        }
                    }
                }
            }
        }
        }
    }

    fx_free(bn_ab);
    if (conv && conv->bias && conv->weights)
        fx_status = fx_make_cptr(conv, _fx_free_conv2d, fx_result);
    else
        fx_status = FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
    if (fx_status < 0)
        _fx_free_conv2d(conv);
    return fx_status;
}

int64_t depthwise_time = 0, _1x1_time = 0;
int depthwise_calls = 0, _1x1_calls = 0;

int _fx_conv2d(const _fx_nntensor_t* inp, _fx_nntensor_t* out,
               const _fx_nntensor_t* passby,
               _fx_conv2d_t* conv, int_ ntasks,
               fx_arr_t* curr_scratch_buf, fx_arr_t* new_scratch_buf)
{
    int inp_typ = inp->data.tag;
    int out_typ = out->data.tag;
    int pb_typ = passby ? passby->data.tag : FX_Notype;

    int CONV_MR = inp_typ == FX_F16 ? FX_CONV_MR_FP16 : FX_CONV_MR;
    int CONV_NR = inp_typ == FX_F16 ? FX_CONV_NR_FP16 : FX_CONV_NR;

    // [TODO] sometimes we deal with small kernels and few input channels,
    // then C_BLOCK_SIZE=256 is probably greater than HkWkCg and then we can,
    // given constant L1 cache size, increase K_BLOCK_SIZE.
    // it can also be that the number of output channels is small.
    // then we can increase the amount of pixels (yx_limit - yx0) that we process at once.
    // That is, we should make K_BLOCK_SIZE and C_BLOCK_SIZE adaptive.
    int MAX_STRIPES = (56 + CONV_NR - 1)/CONV_NR;
    int K_BLOCK_SIZE = ((32*(inp_typ == FX_F16 ? 2 : 1) + CONV_MR - 1)/CONV_MR)*CONV_MR;
    int C_BLOCK_SIZE = 256;

    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    const int_* inp_shape = (const int_*)(inp_shape_->data);
    const int_* out_shape = (const int_*)(out_shape_->data);
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    const fx_arr_t* pb_data = passby ? &passby->data.u.NN_Data_I8 : 0;
    size_t esz = inp_data->dim[0].step;
    size_t c_esz = sizeof(float);
    int ndims = inp_shape_->dim[0].size;
    int out_ndims = out_shape_->dim[0].size;
    float minval = conv->minval, maxval = conv->maxval, alpha = conv->alpha;
    bool fast_activ = conv->activ == _FX_ACTIV_RELU ||
                      (conv->activ == _FX_ACTIV_CLIP && minval == 0.f) ||
                      conv->activ == _FX_ACTIV_LRELU;
    _fx_activ_func_t activ_func = fast_activ || conv->activ == _FX_ACTIV_NONE ? 0 :
        inp_typ == FX_F32 ? conv->activ_func :
        inp_typ == FX_F16 ? conv->activ_func_f16 : 0;
    const float* activ_params = conv->activ_params;

    int N = inp_shape[0], C = conv->C, K = conv->K;
    int Hi = ndims >= 4 ? inp_shape[2] : 0, Wi = ndims >= 4 ? inp_shape[3] : 0;
    int H0 = out_ndims >= 4 ? out_shape[2] : 0, W0 = out_ndims >= 4 ? out_shape[3] : 0;
    int inp_planesize = Hi*Wi, out_planesize = H0*W0;
    int Hk = conv->Hk, Wk = conv->Wk, ksize = Hk*Wk, ngroups = conv->ngroups;
    int Cg = C/ngroups, Kg = K/ngroups;
    int Kg_nblocks = (Kg + CONV_MR-1)/CONV_MR, Kg_aligned = Kg_nblocks*CONV_MR;
    int stripes_per_sample = (out_planesize + CONV_NR - 1)/CONV_NR;
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
    int HkWkCg = ksize*Cg, HkWkC = HkWkCg*ngroups;
    size_t stripesize = CONV_NR*ksize*Cg;
    size_t taskbufsize = (stripesize + CONV_NR*K_BLOCK_SIZE*(c_esz/esz))*MAX_STRIPES;
    size_t totalbufsize = taskbufsize*ntasks*esz;
    char* inpbuf_all = 0;
    int* interior_ofstab = (int*)alloca(ksize*3*sizeof(interior_ofstab[0]));
    int* yxtab = interior_ofstab + ksize;
    int status = 0;
    //int64_t t_ = fx_tick_count();

    if (ndims != 4 || inp_shape[0] != out_shape[0] ||
        inp_shape[1] != conv->C || out_shape[1] != conv->K)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (inp_typ != FX_F32 && inp_typ != FX_F16)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (inp_typ != out_typ || (pb_typ > 1 && pb_typ != inp_typ))
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (s1d1 && (pad_right > conv->pad_right || pad_right < 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (ksize == 1) {
        if((pad_left | pad_right | pad_top | pad_bottom) != 0)
            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        if (stride_x == 1 && stride_y == 1 && (H0 != Hi || W0 != Wi))
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    }

    //printf("HAVE PASSBY: %d, fast_activ=%d, activ=%d, minval=%.5g, maxval=%.5g, alpha=%.5g\n",
    //    (int)(pb_data->data != 0), (int)fast_activ, (int)conv->activ, minval, maxval, alpha);

    if (conv->conv_type == FX_CONV_TYPE_DEPTHWISE) {
        fx_copy_arr(curr_scratch_buf, new_scratch_buf);
        //int64_t t = fx_tick_count();
        int status = _fx_depthwise_conv2d(ndims, inp_typ, inp_shape, inp_data,
                                    out_shape, out_data, conv, ntasks);
        //t = fx_tick_count() - t;
        //depthwise_time += t;
        //depthwise_calls++;
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
    //printf("taskbufsize=%d, stripes_per_sample=%d, weights_size=%d\n", (int)(taskbufsize*esz), (int)stripes_per_sample, (int)(K*Cg*ksize*esz));
    //inpbuf_all = (char*)fx_malloc(totalbufsize);

    for (int y = 0; y < Hk; y++)
        for( int x = 0; x < Wk; x++) {
            int k = y*Wk + x;
            int dy = y*dilation_y, dx = x*dilation_x;
            yxtab[k*2] = dy; yxtab[k*2+1] = dx;
            interior_ofstab[k] = dy*Wi + dx;
        }

    //memset(out_data->data, 0, out_planesize*N*K*esz);

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        //printf("task #%d\n", task_id);
        char* inpbuf_task = &inpbuf_all[taskbufsize*task_id*esz];
        float* cbuf_task = (float*)(inpbuf_task + stripesize*MAX_STRIPES*esz);
        //float inpbuf_task_gold[Cg*ksize*CONV_NR];
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
                k0 = kyx0 * CONV_MR;
                k1 = kyx1 * CONV_MR;
                k1 = k1 <= Kg ? k1 : Kg;
                yx0 = 0;
                yx_limit = out_planesize;
            } else {
                k0 = 0;
                k1 = Kg;
                yx0 = kyx0*CONV_NR;
                yx_limit = kyx1*CONV_NR;
                yx_limit = yx_limit < out_planesize ? yx_limit : out_planesize;
            }

            for (; yx0 < yx_limit; yx0 = yx_block_limit) {
                /*
                ....................... MACROBLOCKS PROCESSING .....................

                Processing each macroblock means complete computation of a certain
                region inside 4D output tensor:
                {sample_index_in_batch=n, group=g, k0 <= k0_block k < k1_block <= k1},
                0 <= {yx0 <= yx < yx_block_limit} <= yx_limit <= W0*H0,
                where yx is a flattened index: yx = y*W0 + x. Such a scheme is also
                friendly for the further extension of this kernel to
                1D and 3D convolution, we just need to modify the repacking part
                (the loop below) to handle 1D and 3D spatial geometry
                (maybe move it to a dedicated function(s)).
                That is, if we consider YX (or, more generally, all the spatial
                dimensions flattened, X or ZYX ...) as 1D continous space,
                the macroblock is 2D rectangular block of size:

                (k1_block - k0_block) x (yx_block_limit - yx0).

                Each macroblock consists of several microblocks, which size
                (CONV_MR x CONV_NR) depends on the used accuracy mode and architecture:
                ARM 64-bit:
                    FP32: 4 (CONV_MR) x 28 (CONV_NR)
                    FP16: 8 (CONV_MR) x 24 (CONV_NR)
                We allocate a temporary buffer for macroblock that fits the integer
                number of microblocks and after computing the macroblock we copy
                the proper part of temporary buffer to the output tensor.

                Procedure consists of 3 steps:
                1. Repacking part of input tensor in zigzag form, which is optimal
                   for microblock processing. It can be viewed as im2col applied to
                   a part of input tensor combined with zigzag reshaping, all in one loop.

                   Basically, we extract Hk*Wk*Cg x CONV_NR*nstripes im2col-ed block,
                   where Hk x Wk is the spatial kernel size (e.g. 1x1 or 3x3),
                   Cg is the number of input channels in group, CONV_NR is microblock width
                   and 1 <= nstripes < MAX_STRIPES. The extracted input tensor
                   data is stored stripe by stripe, where each stripe is
                   Hk*Wk*Cg x CONV_NR, i.e. it's perfectly suitable for microblock processing.

                   Note that convolution weights are repacked before the actual inference;
                   They are also stored stripe by stripe, where each stripe is
                   Hk*Wk*Cg x CONV_MR and CONV_MR is the microblock height.

                2,3. The actual convolution (step 2), which essentially computes
                   A'*B matrix product, where A is Hk*Wk*Cg x (k1-k0)
                   (a part of 'weights' tensor) and B is Hk*Wk*Cg x CONV_NR*nstripes.
                   The product is computed in 4 nested loops.
                   After each macroblock is computed, it's postprocessed (step 3):
                   1. bias is added.
                   2. pre-activation-function 'pass-by' activations are optionally added
                   3. activation function is applied
                   4. post-activation-function 'pass-by' activations are optionally added.
                   5. the result is stored in the output tensor

                   Here is the scheme:

                   for (k0_block = k0; k0_block < k1; k0_block += K_BLOCK_SIZE) {
                       k1_block = min(k0_block + K_BLOCK_SIZE, k1)
                       // set 'C_buffer' of size (k1_block - k0_block) x (CONV_NR*nstripes) to 0's.
                       memset(C_buffer, 0, ...);
                       for (c0 = 0; c0 < Hk*Wk*Cg; c0 += C_BLOCK_SIZE) {
                           c1 = min(c0 + C_BLOCK_SIZE, Hk*Wk*Cg)
                           // compute C_buffer += A[k0_block:k1_block,c0:c1]'*B[c0:c1,:]
                           for (stripe = 0; stripe < nstripes; stripe++) {
                               for (k = k0_block; k < k1_block; k += CONV_MR) {
                                   // compute rectangular part of the result and accumulate it
                                   // using microblock-level kernel
                                   C_buffer[k:k+CONV_MR, stripe*CONV_NR:(stripe+1)*CONV_NR] +=
                                       A[c0:c1, k:k+CONV_MR]'*
                                       B[c0:c1, stripe*CONV_NR:(stripe+1)*CONV_NR]
                               }
                           }
                       }
                       // post-process C_buffer (1, 2, 3, 4, 5 ...)
                       ...
                   }

                   Such scheme, with properly selected K_BLOCK_SIZE and C_BLOCK_SIZE, let
                   us to achieve much better L1 and L2 cache utilization.
                */

                // step 1. extract part of input tensor and represent it in zigzag form
                yx_block_limit = yx0 + CONV_NR*MAX_STRIPES;
                yx_block_limit = yx_block_limit < yx_limit ? yx_block_limit : yx_limit;
                int nstripes = (yx_block_limit - yx0 + CONV_NR - 1)/CONV_NR;
                int yx0_saved = yx0, yx1;
                assert(nstripes <= MAX_STRIPES);
                for (int stripe = 0; yx0 < yx_block_limit; stripe++, yx0 += CONV_NR) {
                    char* inpbuf = inpbuf_task + stripe*stripesize*esz;
                    const char* inptr = inp_data->data + inp_plane_ofs*esz;
                    /*
                        1. pack the data. Copy the HkxWk CONV_NR-wide slices from
                           each feature plane of the input tensor to the input buffer.
                    */
                    if (fast_1x1) {
                        int slice_len = yx1 - yx0;
                        bool partial = slice_len < CONV_NR;
                        // super-fast branch for 1x1 convolutions with sy=sx=1.
                        // in this case each feature plane can be safely treated
                        // as 1D array and we just extract next portion
                        // of CONV_NR elements from each feature plane and
                        // put it together.
                        inptr += yx0*esz;
                        if (!partial) {
                            // Make special branch where memcpy() is called with a constant buffer size.
                            // Compilers will likely unroll this loop properly.
                            if (esz == sizeof(float)) {
                                for (int c = 0; c < Cg; c++, inptr += inp_planesize*sizeof(float),
                                                             inpbuf += FX_CONV_NR*sizeof(float))
                                    memcpy(inpbuf, inptr, FX_CONV_NR*sizeof(float));
                            } else {
                                assert(esz == sizeof(fx_f16));
                                for (int c = 0; c < Cg; c++, inptr += inp_planesize*sizeof(fx_f16),
                                                             inpbuf += FX_CONV_NR_FP16*sizeof(fx_f16))
                                    memcpy(inpbuf, inptr, FX_CONV_NR_FP16*sizeof(fx_f16));
                            }
                        } else {
                            for (int c = 0; c < Cg; c++, inptr += inp_planesize*esz, inpbuf += CONV_NR*esz) {
                                memcpy(inpbuf, inptr, slice_len*esz);
                                memset(inpbuf + slice_len*esz, 0, (CONV_NR - slice_len)*esz);
                            }
                        }
                    } else {
                        //memset(inpbuf, 0, HkWkCg*CONV_NR*esz);
                        int y0_ = yx0/W0, x0_ = yx0 - y0_*W0;
                        for (int k = 0; k < ksize; k++) {
                            int dy = yxtab[k*2], dx = yxtab[k*2+1];
                            if (esz == 4) {
                                int i = 0, y0 = y0_, x0 = x0_;
                                for (; i < FX_CONV_NR;) {
                                    float* inpbuf_ki = (float*)inpbuf + k*(FX_CONV_NR*Cg) + i;
                                    int yi = y0*stride_y + dy - pad_top;
                                    int xi = x0*stride_x + dx - pad_left;

                                    if ((unsigned)yi < (unsigned)Hi &&
                                        (unsigned)xi < (unsigned)Wi) {
                                        const float* inptr_ki = (float*)inptr + yi*Wi + xi;
                                        if (i + 6 <= FX_CONV_NR && x0 + 6 <= W0 && xi + stride_x*6 <= Wi) {
                                            if (stride_x == 1) {
                                                for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR, inptr_ki += inp_planesize) {
                                                    float t0 = inptr_ki[0], t1 = inptr_ki[1];
                                                    float t2 = inptr_ki[2], t3 = inptr_ki[3];
                                                    float t4 = inptr_ki[4], t5 = inptr_ki[5];
                                                    inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                    inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                                    inpbuf_ki[4] = t4; inpbuf_ki[5] = t5;
                                                }
                                            } else {
                                                for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR, inptr_ki += inp_planesize) {
                                                    float t0 = inptr_ki[0], t1 = inptr_ki[stride_x];
                                                    float t2 = inptr_ki[stride_x*2], t3 = inptr_ki[stride_x*3];
                                                    float t4 = inptr_ki[stride_x*4], t5 = inptr_ki[stride_x*5];
                                                    inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                    inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                                    inpbuf_ki[4] = t4; inpbuf_ki[5] = t5;
                                                }
                                            }
                                            i += 6;
                                            x0 += 6;
                                        } else {
                                            for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR, inptr_ki += inp_planesize)
                                                *inpbuf_ki = *inptr_ki;
                                            i++;
                                            x0++;
                                        }
                                    } else {
                                        for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR)
                                            *inpbuf_ki = 0.f;
                                        i++;
                                        x0++;
                                    }
                                    int mask = x0 >= W0;
                                    y0 += mask;
                                    x0 &= mask-1;
                                }
                            } else {
                                assert(esz == 2);
                                int i = 0, y0 = y0_, x0 = x0_;
                                for (; i < FX_CONV_NR_FP16;) {
                                    int16_t* inpbuf_ki = (int16_t*)inpbuf + k*(FX_CONV_NR_FP16*Cg) + i;
                                    int yi = y0*stride_y + dy - pad_top;
                                    int xi = x0*stride_x + dx - pad_left;

                                    if ((unsigned)yi < (unsigned)Hi &&
                                        (unsigned)xi < (unsigned)Wi) {
                                        const int16_t* inptr_ki = (int16_t*)inptr + yi*Wi + xi;
                                        if (i + 4 <= FX_CONV_NR_FP16 && x0 + 4 <= W0 && xi + stride_x*4 <= Wi) {
                                            if (stride_x == 1) {
                                                for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR_FP16, inptr_ki += inp_planesize) {
                                                    int16_t t0 = inptr_ki[0], t1 = inptr_ki[1];
                                                    int16_t t2 = inptr_ki[2], t3 = inptr_ki[3];
                                                    inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                    inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                                }
                                            } else {
                                                for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR_FP16, inptr_ki += inp_planesize) {
                                                    int16_t t0 = inptr_ki[0], t1 = inptr_ki[stride_x];
                                                    int16_t t2 = inptr_ki[stride_x*2], t3 = inptr_ki[stride_x*3];
                                                    inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                    inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                                }
                                            }
                                            i += 4;
                                            x0 += 4;
                                        } else {
                                            for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR_FP16, inptr_ki += inp_planesize)
                                                *inpbuf_ki = *inptr_ki;
                                            i++;
                                            x0++;
                                        }
                                    } else {
                                        for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR_FP16)
                                            *inpbuf_ki = 0;
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
                }

                yx0 = yx0_saved;
                const char* weights = (inp_typ == FX_F32 ?
                    (const char*)conv->weights : (const char*)conv->wf16) + g*Kg_aligned*HkWkCg*esz;
                const float* biasptr = conv->bias + Kg*g;
                int ldc = nstripes*CONV_NR;
                // 2. do convolution, compute Kg x (yx_block_limit - yx0) part of the output tensor
                for (int k0_block = k0; k0_block < k1; k0_block += K_BLOCK_SIZE) {
                    int k1_block = k0_block + K_BLOCK_SIZE < k1 ? k0_block + K_BLOCK_SIZE : k1;
                    for (int c0 = 0; c0 < HkWkCg; c0 += C_BLOCK_SIZE) {
                        int c1 = c0 + C_BLOCK_SIZE < HkWkCg ? c0 + C_BLOCK_SIZE : HkWkCg;
                        for (int stripe = 0; stripe < nstripes; stripe++) {
                            const char* wptr = weights + (k0_block*HkWkCg + c0*CONV_MR)*esz;
                            const char* inptr = inpbuf_task + (stripe*stripesize + c0*CONV_NR)*esz;
                            float* cptr = cbuf_task + stripe*CONV_NR;
                            for (int k = k0_block; k < k1_block; k += CONV_MR,
                                    wptr += HkWkCg*CONV_MR*esz,
                                    cptr += CONV_MR*ldc
                                    ) {
                                if (inp_typ == FX_F32) {
                                    _fx_conv_update_block_f32(c1 - c0,
                                        wptr, inptr, cptr, ldc, c0 == 0);
                                } else {
                                    _fx_conv_update_block_f16(c1 - c0,
                                        wptr, inptr, cptr, ldc, c0 == 0);
                                }
                            }
                        }
                    }

                    size_t outofs = (((n*ngroups + g)*Kg + k0_block)*out_planesize + yx0)*esz;
                    int out_width = yx_block_limit - yx0;
                    const float* cptr = (const float*)cbuf_task;
                    if (inp_typ == FX_F32) {
                        float* outptr = (float*)(out_data->data + outofs);
                        const float* pbptr = pb_data && pb_data->data ? (float*)(pb_data->data + outofs) : 0;

                        for (int k = k0_block; k < k1_block; k++,
                                cptr += ldc, outptr += out_planesize,
                                pbptr += (pbptr ? out_planesize : 0)) {
                            float biasval = biasptr[k];
                            int j = 0;

                        #ifdef __ARM_NEON
                            float32x4_t vbias = vdupq_n_f32(biasval), valpha = vdupq_n_f32(alpha), vmax = vdupq_n_f32(maxval);
                            float32x4_t z = vdupq_n_f32(0.f), one = vdupq_n_f32(1.f);
                            if (pbptr) {
                                for (; j < out_width; j += 8) {
                                    if (j + 8 > out_width) {
                                        if (j == 0)
                                            break;
                                        j = out_width - 8;
                                    }
                                    float32x4_t v0 = vaddq_f32(vld1q_f32(cptr + j), vbias);
                                    float32x4_t v1 = vaddq_f32(vld1q_f32(cptr + j + 4), vbias);
                                    v0 = vaddq_f32(v0, vld1q_f32(pbptr + j));
                                    v1 = vaddq_f32(v1, vld1q_f32(pbptr + j + 4));
                                    v0 = vmulq_f32(vminq_f32(v0, vmax), vbslq_f32(vcltq_f32(v0, z), valpha, one));
                                    v1 = vmulq_f32(vminq_f32(v1, vmax), vbslq_f32(vcltq_f32(v1, z), valpha, one));
                                    vst1q_f32(outptr + j, v0);
                                    vst1q_f32(outptr + j + 4, v1);
                                }
                            } else {
                                for (; j < out_width; j += 8) {
                                    if (j + 8 > out_width) {
                                        if (j == 0)
                                            break;
                                        j = out_width - 8;
                                    }
                                    float32x4_t v0 = vaddq_f32(vld1q_f32(cptr + j), vbias);
                                    float32x4_t v1 = vaddq_f32(vld1q_f32(cptr + j + 4), vbias);
                                    v0 = vmulq_f32(vminq_f32(v0, vmax), vbslq_f32(vcltq_f32(v0, z), valpha, one));
                                    v1 = vmulq_f32(vminq_f32(v1, vmax), vbslq_f32(vcltq_f32(v1, z), valpha, one));
                                    vst1q_f32(outptr + j, v0);
                                    vst1q_f32(outptr + j + 4, v1);
                                }
                            }
                        #endif

                            if (pbptr) {
                                for (; j < out_width; j++) {
                                    float v = cptr[j] + biasval;
                                    v += pbptr[j];
                                    v = v <= maxval ? v : maxval;
                                    v *= (v < 0.f ? alpha : 1.f);
                                    outptr[j] = v;
                                }
                            } else {
                                for (; j < out_width; j++) {
                                    float v = cptr[j] + biasval;
                                    v = v <= maxval ? v : maxval;
                                    v *= (v < 0.f ? alpha : 1.f);
                                    outptr[j] = v;
                                }
                            }
                        }
                    } else {
                        fx_f16* outptr = (fx_f16*)(out_data->data + outofs);
                        const fx_f16* pbptr = pb_data && pb_data->data ? (fx_f16*)(pb_data->data + outofs) : 0;

                        for (int k = k0_block; k < k1_block; k++,
                                cptr += ldc, outptr += out_planesize,
                                pbptr += (pbptr ? out_planesize : 0)) {
                            float biasval = biasptr[k];
                            int j = 0;

                        #ifdef __ARM_NEON
                            float32x4_t vbias = vdupq_n_f32(biasval);
                            float16x8_t valpha = vdupq_n_f16(alpha);
                            float16x8_t vmax = vdupq_n_f16(maxval < 65504.f ? maxval : 65504.f);
                            float16x8_t z = vdupq_n_f16(0.f), one = vdupq_n_f16(1.f);
                            if (pbptr) {
                                for (; j < out_width; j += 8) {
                                    if (j + 8 > out_width) {
                                        if (j == 0)
                                            break;
                                        j = out_width - 8;
                                    }
                                    float32x4_t v0 = vaddq_f32(vld1q_f32(cptr + j), vbias);
                                    float32x4_t v1 = vaddq_f32(vld1q_f32(cptr + j + 4), vbias);
                                    float16x8_t v = vcombine_f16(vcvt_f16_f32(v0), vcvt_f16_f32(v1));
                                    v = vaddq_f16(v, vld1q_f16(pbptr + j));
                                    v = vmulq_f16(vminq_f16(v, vmax), vbslq_f16(vcltq_f16(v, z), valpha, one));
                                    vst1q_f16(outptr + j, v);
                                }
                            } else {
                                for (; j < out_width; j += 8) {
                                    if (j + 8 > out_width) {
                                        if (j == 0)
                                            break;
                                        j = out_width - 8;
                                    }
                                    float32x4_t v0 = vaddq_f32(vld1q_f32(cptr + j), vbias);
                                    float32x4_t v1 = vaddq_f32(vld1q_f32(cptr + j + 4), vbias);
                                    float16x8_t v = vcombine_f16(vcvt_f16_f32(v0), vcvt_f16_f32(v1));
                                    v = vmulq_f16(vminq_f16(v, vmax), vbslq_f16(vcltq_f16(v, z), valpha, one));
                                    vst1q_f16(outptr + j, v);
                                }
                            }
                        #endif

                            if (pbptr) {
                                for (; j < out_width; j++) {
                                    float v = cptr[j] + biasval;
                                    v += FX_FLOAT(pbptr[j]);
                                    v = v <= maxval ? v : maxval;
                                    v *= (v < 0.f ? alpha : 1.f);
                                    outptr[j] = FX_FLOAT16(v);
                                }
                            } else {
                                for (; j < out_width; j++) {
                                    float v = cptr[j] + biasval;
                                    v = v <= maxval ? v : maxval;
                                    v *= (v < 0.f ? alpha : 1.f);
                                    outptr[j] = FX_FLOAT16(v);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /*if (Wk == 1 && Hk == 1)
    {
        t_ = fx_tick_count() - t_;
        _1x1_time += t_;
        _1x1_calls++;
    }*/

    return FX_OK;
}
}

fun init_conv(kernel_shape: int [], strides: int [],
              dilations: int [], pads: int [], group: int,
              w_shape: Ast.nnshape_t, w_data: float [],
              bias_shape: Ast.nnshape_t, bias_data: float [],
              bn_data: float [][], bn_eps: float,
              activ_func: activ_func_t, activ_params: float []): cptr
@ccode
{
    const int_* w_shape_ = (const int_*)w_shape->shape.data;
    const int_* strides_ = (const int_*)strides->data;
    const int_* dilations_ = (const int_*)dilations->data;
    const int_* pads_ = (const int_*)pads->data;
    int_ n_bn_data = bn_data->dim[0].size;
    int_ n_activ_params = activ_params->dim[0].size;
    int_ K = w_shape_ ? w_shape_[0] : 0;
    const float* bn_data_[4] = {0, 0, 0, 0};
    if (w_shape->shape.ndims != 1 || w_shape->shape.dim[0].size != 4 ||
        strides->ndims != 1 || strides->dim[0].size != 2 ||
        dilations->ndims != 1 || dilations->dim[0].size != 2 ||
        pads->ndims != 1 || pads->dim[0].size != 4 ||
        (n_bn_data != 4 && n_bn_data != 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeError);

    if (n_bn_data > 0) {
        for(int_ i = 0; i < n_bn_data; i++) {
            fx_arr_t* bn_data_i = ((fx_arr_t*)bn_data->data) + i;
            int_ n_bn_data_i = bn_data_i->dim[0].size;
            if (n_bn_data_i != 0 && n_bn_data_i != K)
                return FX_SET_EXN_FAST(FX_EXN_SizeError);
            bn_data_[i] = (const float*)bn_data_i->data;
        }
    }

    return _fx_init_conv2d(_FX_NN_Layout_NCHW, _FX_NN_Layout_NCHW, (int)group,
        (int)w_shape_[0], (int)w_shape_[1]*group, (int)w_shape_[2], (int)w_shape_[3],
        (int)strides_[0], (int)strides_[1], (int)dilations_[0], (int)dilations_[1],
        (int)pads_[0], (int)pads_[1], (int)pads_[2], (int)pads_[3],
        (const float*)w_data->data, (const float*)bias_data->data,
        bn_data_[0], bn_data_[1], bn_data_[2], bn_data_[3], bn_eps,
        (int)activ_func->tag, (const float*)activ_params->data,
        (int)n_activ_params, fx_result);
}

fun run_conv(inp: Ast.nntensor_t, out: Ast.nntensor_t, passby: Ast.nntensor_t,
             conv_data: cptr, ntasks: int, scratch_buf: Ast.nnbuf_t): Ast.nnbuf_t
@ccode {
    _fx_conv2d_t* conv = conv_data && conv_data->ptr ? (_fx_conv2d_t*)conv_data->ptr : 0;
    if (!conv)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
    return _fx_conv2d((const _fx_nntensor_t*)inp, (_fx_nntensor_t*)out,
                      (const _fx_nntensor_t*)passby, conv, ntasks, scratch_buf, fx_result);
}

fun run_conv(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Conv {attr={kernel_shape, pads, strides, dilations, group},
        conv_data, fused_batch_norm, non_const_batch_norm,
        fused_activ, non_const_activ, t_inp, t_weights, t_bias, t_out, t_passby} =>
    val inp = model.get_tensor(t_inp)
    val weights = model.get_tensor(t_weights)
    val bias = model.get_tensor(t_bias)
    val out = model.get_tensor(t_out)
    val pb = model.get_tensor(t_passby)
    assert(`kernel_shape.size() == 2 && inp.shape.shape.size() == 4`)
    if *conv_data == null || !model.isconst(t_weights) || !model.isconst(t_bias) ||
        non_const_batch_norm || non_const_activ {
        //println(f"Conv: weights.data: {weights.data.elemtype()}, bias.data: {bias.data.elemtype()}")
        val empty: float [] = []
        val (bn_data, bn_eps) =
            match fused_batch_norm {
            | Some (Ast.NN_BatchNorm {epsilon, t_mean, t_var, t_scale, t_B}) =>
                    val bn_mean = model.get_tensor(t_mean)
                    val bn_var = model.get_tensor(t_var)
                    val bn_scale = model.get_tensor(t_scale)
                    val bn_bias = model.get_tensor(t_B)
                    ([for bn <- [bn_mean, bn_var, bn_scale, bn_bias] {
                        match bn.data {
                        | Ast.NN_Data_FP32 bn_data => bn_data
                        | Ast.NN_Data_Empty => empty
                        | _ => throw Ast.NNError(f"unsupported type '{bn.data.elemtype()}' of batch norm data; should be fp32")
                        }
                    }], epsilon)
            | _ => (([]: float [][]), 0.f)
            }
        val (activ_func, (activ_params : float [])) = match fused_activ {
            | Some (Ast.NN_Elemwise {el_op=Ast.NN_Relu}) => (ACTIV_RELU, [])
            | Some (Ast.NN_Elemwise {el_op=Ast.NN_Sigmoid}) => (ACTIV_SIGMOID, [])
            | Some (Ast.NN_Elemwise {el_op=Ast.NN_Tanh}) => (ACTIV_TANH, [])
            | Some (Ast.NN_Elemwise {el_op=Ast.NN_Mish}) => (ACTIV_MISH, [])
            | Some (Ast.NN_Clip {t_min, t_max}) =>
                val minval = model.get_tensor(t_min)
                val maxval = model.get_tensor(t_max)
                val minval = minval.data.float_scalar_or(-FLT_MAX)
                val maxval = maxval.data.float_scalar_or(FLT_MAX)
                (ACTIV_CLIP, [minval, maxval])
            | Some (Ast.NN_LeakyRelu {alpha}) => (ACTIV_LRELU, [alpha])
            | Some op =>
                throw Ast.NNError(f"unexpected activation {op.name()}")
            | _ => (ACTIV_NONE, [])
            }
        match (weights.data, bias.data) {
        | (Ast.NN_Data_FP32 w_data, (Ast.NN_Data_FP32 _ | Ast.NN_Data_Empty)) =>
            val bias_data = match bias.data { Ast.NN_Data_FP32 bias_data => bias_data | _ => [] }
            *conv_data = null // first of all, release the previous data, if any
                              // this way we can immediately re-use the same chunk of memory
                              // for the updated convolution structure
            *conv_data = init_conv(kernel_shape, strides, dilations, pads, group,
                                   weights.shape, w_data, bias.shape, bias_data,
                                   bn_data, bn_eps, activ_func, activ_params)
        | _ => throw NotImplementedError
        }
    }
    *model.scratch_buf = run_conv(inp, out, pb, *conv_data, *model.ntasks, *model.scratch_buf)
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
