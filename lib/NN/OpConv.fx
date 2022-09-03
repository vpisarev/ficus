/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast, OpConv_Block, OpConv_Depthwise, OpConv_Winograd

@ccode {
#include <assert.h>
#include <float.h>
#include <math.h>
#include "ficus_nn_common.h"

void _fx_conv_block_f32( int k, const void* a_, const void* b_,
                         void* c_, int ldc, const void* bp_, int ldp,
                         const float* bias, float alpha, float maxval, bool activ);
void _fx_conv_block_f16( int k, const void* a_, const void* b_,
                         void* c_, int ldc, const void* bp_, int ldp,
                         const float* bias, float alpha, float maxval, bool activ);
void _fx_conv_update_block_f32( int np, const void* a_, const void* b_,
                                void* c_, int ldc, bool init_c );
void _fx_conv_update_block_f16( int np, const void* a_, const void* b_,
                                void* c_, int ldc, bool init_c );
int _fx_depthwise_conv2d_f32(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_conv2d_t* conv,
                             const char* inptr0, char* outptr0,
                             int ntasks);
int _fx_depthwise_conv2d_f16(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_conv2d_t* conv,
                             const char* inptr0, char* outptr0,
                             int ntasks);
int _fx_winograd_conv2d(int typ, int ndims, const int_* inpshape, const char* inp,
                        const char* bypass, const int_* outshape, char* out,
                        const struct _fx_conv2d_t* conv, int ntasks,
                        fx_arr_t* curr_scratch_buf, fx_arr_t* new_scratch_buf);

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
    int ntasks,
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
    conv->conv_type =
        ngroups == K && ngroups == C ? _FX_CONV_TYPE_DEPTHWISE :
    // so far we only have ARM implementation of Winograd-based 3x3 convolution
    #ifdef __ARM_NEON
        Hk == 3 && Wk == 3 && dilation_y == 1 && dilation_x == 1 &&
        stride_y == 1 && stride_x == 1 ? _FX_CONV_TYPE_WINOGRAD3X3 :
    #endif
        _FX_CONV_TYPE_GENERIC;
    // looks like there is a bug in Winograd code when ngroups > 1
    if (conv->conv_type != _FX_CONV_TYPE_DEPTHWISE && conv->ngroups != 1)
        conv->conv_type = _FX_CONV_TYPE_GENERIC;
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
        _FX_FP16_CASE(conv->activ_func_f16 = _fx_nn_elemwise_clip_f16);
    } else if (activ == _FX_ACTIV_MISH) {
        conv->activ_func = _fx_nn_elemwise_mish_f32;
        _FX_FP16_CASE(conv->activ_func_f16 = _fx_nn_elemwise_mish_f16);
    } else if (activ == _FX_ACTIV_SIGMOID) {
        conv->activ_func = _fx_nn_elemwise_sigmoid_f32;
        _FX_FP16_CASE(conv->activ_func_f16 = _fx_nn_elemwise_sigmoid_f16);
    } else if (activ == _FX_ACTIV_TANH) {
        conv->activ_func = _fx_nn_elemwise_tanh_f32;
        _FX_FP16_CASE(conv->activ_func_f16 = _fx_nn_elemwise_tanh_f16);
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

    if (conv->conv_type == _FX_CONV_TYPE_DEPTHWISE) {
        // for depth-wise convolutions on NCHW data we just preserve the weights in KCHW layout,
        // but add some padding to make the weights array layout more SIMD-friendly
        int ksize = Hk*Wk;
        int padded_ksize = ((ksize + FX_VEC_NLANES_F32-1)/FX_VEC_NLANES_F32)*FX_VEC_NLANES_F32;
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
        padded_ksize = ((ksize + FX_VEC_NLANES_F16-1)/FX_VEC_NLANES_F16)*FX_VEC_NLANES_F16;
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
    } else if (conv->conv_type == _FX_CONV_TYPE_WINOGRAD3X3) {
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
        // the weights are packed as 6-dim tensor:
        // ngroups * ceil((K/ngroups)/KBLOCK) * (W*W/ATOM_SIZE) * (C/ngroups) * KBLOCK * ATOM_SIZE,
        // where W is the size of Winograd-transformed kernel (8x8),
        // ATOM_SIZE is number of lanes in SIMD register (4 for NEON and FP32),
        // KBLOCK is some platform-dependent constant dependent on the number of SIMD registers.
        int Cg = C/ngroups;
        int Kg = K/ngroups;
        int Kg_nblocks = (Kg + _FX_WINO_KBLOCK - 1)/_FX_WINO_KBLOCK;
        size_t nweights = ngroups*Kg_nblocks*Cg*_FX_WINO_KBLOCK*_FX_WINO_AREA;
        conv->weights = (float*)fx_malloc(nweights*sizeof(conv->weights[0]));
        memset(conv->weights, 0, nweights*sizeof(conv->weights[0]));
    #if _FX_NN_ENABLE_FP16
        conv->wf16 = (fx_f16*)fx_malloc(nweights*sizeof(conv->weights[0]));
        memset(conv->wf16, 0, nweights*sizeof(conv->wf16[0]));
    #endif
        #pragma omp parallel for num_threads(ntasks)
        for(int k = 0; k < K; k++) {
            float scale = bn_ab ? bn_ab[k] : 1.f;
            int g = k / Kg;
            int k_ = k - g*Kg;
            int k0 = k_ / _FX_WINO_KBLOCK;
            int dk = k_ - k0*_FX_WINO_KBLOCK;
            for(int c = 0; c < Cg; c++) {
                float GW[_FX_WINO_SIZE*3], GWG[_FX_WINO_AREA];
                fx_mpgemm(false, true, scale, 0.f,
                        _FX_WINO_SIZE, 3, FX_F32, G, 3, 1,
                        3, 3, FX_F32, weights + (k*Cg + c)*Hk*Wk, 3, 1,
                        FX_F32, GW, 3, 1);
                fx_mpgemm(false, true, 1.f, 0.f,
                        _FX_WINO_SIZE, 3, FX_F32, GW, 3, 1,
                        _FX_WINO_SIZE, 3, FX_F32, G, 3, 1,
                        FX_F32, GWG, _FX_WINO_SIZE, 1);
                float* wptr = conv->weights +
                            (g*Kg_nblocks + k0)*Cg*_FX_WINO_KBLOCK*_FX_WINO_AREA +
                            (c*_FX_WINO_KBLOCK + dk)*_FX_WINO_ATOM_F32;
                for (int i = 0; i < _FX_WINO_NATOMS_F32; i++,
                    wptr += Cg*_FX_WINO_KBLOCK*_FX_WINO_ATOM_F32) {
                    assert(conv->weights <= wptr && wptr + _FX_WINO_ATOM_F32 <= conv->weights + nweights);
                    for (int j = 0; j < _FX_WINO_ATOM_F32; j++)
                        wptr[j] = GWG[i*_FX_WINO_ATOM_F32 + j];
                }
            #if _FX_NN_ENABLE_FP16
                fx_f16* wptr_f16 = conv->wf16 +
                            (g*Kg_nblocks + k0)*Cg*_FX_WINO_KBLOCK*_FX_WINO_AREA +
                            (c*_FX_WINO_KBLOCK + dk)*_FX_WINO_ATOM_F16;
                for (int i = 0; i < _FX_WINO_NATOMS_F16; i++,
                    wptr_f16 += Cg*_FX_WINO_KBLOCK*_FX_WINO_ATOM_F16) {
                    for (int j = 0; j < _FX_WINO_ATOM_F16; j++)
                        wptr_f16[j] = FX_FLOAT16(GWG[i*_FX_WINO_ATOM_F16 + j]);
                }
            #endif
            }
        }
    } else {
        int Kg = K/ngroups, Cg = C/ngroups;
#if _FX_NN_ENABLE_FP16
        {
        // the weights are packed as
        // ngroups x (ceil((K/ngroups)/FX_CONV_MR)*FX_CONV_MR) x (Cg*Hk*Wk) x FX_CONV_MR tensor
        int Kg_aligned = ((Kg + FX_CONV_MR_F16 - 1)/FX_CONV_MR_F16)*FX_CONV_MR_F16;
        size_t nweights = (size_t)ngroups*Kg_aligned*Cg*Hk*Wk;
        conv->wf16 = (fx_f16*)fx_malloc(nweights*sizeof(conv->weights[0]));
        if (conv->wf16) {
            memset(conv->wf16, 0, nweights*sizeof(conv->wf16[0]));
            fx_f16* packed_wptr = conv->wf16;
            for(int g = 0; g < ngroups; g++) {
                for(int k0 = 0; k0 < Kg_aligned; k0 += FX_CONV_MR_F16) {
                    int dk = Kg - k0 < FX_CONV_MR_F16 ? Kg - k0 : FX_CONV_MR_F16;
                    int k_idx = g*Kg + k0;
                    for(int yx = 0; yx < Hk*Wk; yx++) {
                        for(int c = 0; c < Cg; c++, packed_wptr += FX_CONV_MR_F16) {
                            const float* wptr = weights + (size_t)(k_idx*Cg + c)*Hk*Wk + yx;
                            int k = 0;
                            for(; k < dk; k++, wptr += Cg*Hk*Wk) {
                                float scale = bn_ab ? bn_ab[k_idx+k] : 1.f;
                                packed_wptr[k] = (fx_f16)(*wptr*scale);
                            }
                            for(; k < FX_CONV_MR_F16; k++)
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

int _fx_conv2d(const _fx_nntensor_t* inp, _fx_nntensor_t* out,
               const _fx_nntensor_t* bypass, _fx_conv2d_t* conv, int_ ntasks,
               fx_arr_t* curr_scratch_buf, fx_arr_t* new_scratch_buf)
{
    int inp_typ = inp->data.tag;
    int out_typ = out->data.tag;
    int pb_typ = bypass ? bypass->data.tag : FX_Notype;

    int CONV_MR = inp_typ == FX_F16 ? FX_CONV_MR_F16 : FX_CONV_MR;
    int CONV_NR = inp_typ == FX_F16 ? FX_CONV_NR_F16 : FX_CONV_NR;
    int K_BLOCK = 24;

    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    const int_* inp_shape = (const int_*)(inp_shape_->data);
    const int_* out_shape = (const int_*)(out_shape_->data);
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    const fx_arr_t* bp_data = bypass ? &bypass->data.u.NN_Data_I8 : 0;
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

    int N = (int)inp_shape[0], C = conv->C, K = conv->K;
    int Hi = (int)(ndims >= 4 ? inp_shape[2] : 0), Wi = (int)(ndims >= 4 ? inp_shape[3] : 0);
    int H0 = (int)(out_ndims >= 4 ? out_shape[2] : 0), W0 = (int)(out_ndims >= 4 ? out_shape[3] : 0);
    int inp_planesize = Hi*Wi, out_planesize = H0*W0;
    int Hk = conv->Hk, Wk = conv->Wk, ksize = Hk*Wk, ngroups = conv->ngroups;
    int Cg = C/ngroups, Kg = K/ngroups;
    int Kg_nblocks = (Kg + CONV_MR-1)/CONV_MR, Kg_aligned = Kg_nblocks*CONV_MR;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    int pad_x = pad_left + pad_right;
    bool fast_1x1 = stride_x == 1 && stride_y == 1 && ksize == 1;
    bool s1d1 = stride_x == 1 && stride_y == 1 && dilation_x == 1 && dilation_y == 1;
    int HkWkCg = ksize*Cg, HkWkC = HkWkCg*ngroups;
    int stripes_per_plane = (out_planesize + CONV_NR - 1)/CONV_NR;
    size_t stripe_size = (size_t)HkWkCg*CONV_NR;
    size_t totalbufsize = N*ngroups*stripes_per_plane*stripe_size*esz;
    char* inpbuf_all = 0;
    int* ofstab = (int*)alloca(ksize*3*sizeof(ofstab[0]));
    int* yxtab = ofstab + ksize;
    int status = 0;

    if (ndims != 4 || inp_shape[0] != out_shape[0] ||
        inp_shape[1] != conv->C || out_shape[1] != conv->K)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (inp_typ != FX_F32
        _FX_FP16_CASE(&& inp_typ != FX_F16)
        )
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

    if (conv->conv_type == _FX_CONV_TYPE_DEPTHWISE) {
        _fx_depthwise2d_t dw_ctx;
        _fx_init_depthwise2d(N, Hi, Wi, H0, W0, Hk, Wk,
                            stride_y, stride_x, dilation_y, dilation_x,
                            pad_top, pad_left, pad_bottom, pad_right,
                            yxtab, ofstab, &dw_ctx);

        // bypass != 0 is not supported for depthwise convolutions
        // (graph fusion module should prevent such fusion)
        if (bp_data && bp_data->data)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

        int status =
            inp_typ == FX_F32 ?
                _fx_depthwise_conv2d_f32(&dw_ctx, conv, inp_data->data,
                                         out_data->data, (int)ntasks) :
        #if _FX_NN_ENABLE_FP16
            inp_typ == FX_F16 ?
                _fx_depthwise_conv2d_f16(&dw_ctx, conv, inp_data->data,
                                         out_data->data, (int)ntasks) :
        #endif
            FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

        if (status >= 0)
            fx_copy_arr(curr_scratch_buf, new_scratch_buf);
        return status;
    } else if (conv->conv_type == _FX_CONV_TYPE_WINOGRAD3X3) {
        if (inp_typ != out_typ)
            return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
        return _fx_winograd_conv2d(inp_typ, 4, inp_shape, inp_data->data,
                        bp_data ? bp_data->data : 0, out_shape, out_data->data,
                        conv, (int)ntasks, curr_scratch_buf, new_scratch_buf);
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

    // phase 1. im2row
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        int nc0 = task_id*N*C/ntasks, nc1 = (task_id+1)*N*C/ntasks, dc = 0;
        for (; nc0 < nc1; nc0 += dc) {
            int n = nc0/C, c0 = nc0 - n*C;
            int g = c0 / Cg;
            c0 -= g*Cg;
            dc = Cg - c0 <= nc1 - nc0 ? Cg - c0 : nc1 - nc0;
            const char* inptr_ = inp_data->data + (size_t)nc0*inp_planesize*esz;
            char* inpbuf_ = inpbuf_all + ((n*ngroups + g)*stripes_per_plane*stripe_size + c0*CONV_NR)*esz;
            int yx0 = 0;
            for (int i = 0; i < stripes_per_plane; i++, yx0 += CONV_NR, inpbuf_ += stripe_size*esz) {
                char* inpbuf = inpbuf_;
                int yx1 = yx0 + CONV_NR;
                yx1 = yx1 <= out_planesize ? yx1 : out_planesize;
                int slice_len = yx1 - yx0;
                bool partial = slice_len < CONV_NR;
                if (fast_1x1) {
                    const char* inptr = inptr_ + yx0*esz;
                    if (!partial) {
                        // Make special branch where memcpy() is called with a constant buffer size.
                        // Compilers will likely unroll this loop properly.
                        if (esz == sizeof(float)) {
                            for (int c = 0; c < dc; c++, inptr += inp_planesize*esz, inpbuf += FX_CONV_NR*sizeof(float))
                                memcpy(inpbuf, inptr, FX_CONV_NR*sizeof(float));
                        } else {
                            for (int c = 0; c < dc; c++, inptr += inp_planesize*esz, inpbuf += FX_CONV_NR_F16*sizeof(fx_f16))
                                memcpy(inpbuf, inptr, FX_CONV_NR_F16*sizeof(fx_f16));
                        }
                    } else {
                        for (int c = 0; c < dc; c++, inptr += inp_planesize*esz, inpbuf += CONV_NR*esz) {
                            memcpy(inpbuf, inptr, slice_len*esz);
                            memset(inpbuf + slice_len*esz, 0, (CONV_NR - slice_len)*esz);
                        }
                    }
                } else {
                    int y0_ = yx0/W0, x0_ = yx0 - y0_*W0;
                    for (int k = 0; k < ksize; k++) {
                        int dy = yxtab[k*2], dx = yxtab[k*2+1];
                        if (esz == 4) {
                            int i = 0, y0 = y0_, x0 = x0_;
                            for (; i < FX_CONV_NR;) {
                                int* inpbuf_ki = (int*)inpbuf + k*(FX_CONV_NR*Cg) + i;
                                int yi = y0*stride_y + dy - pad_top;
                                int xi = x0*stride_x + dx - pad_left;

                                if ((unsigned)yi < (unsigned)Hi &&
                                    (unsigned)xi < (unsigned)Wi) {
                                    const int* inptr_ki = (int*)inptr_ + yi*Wi + xi;
                                    if (i + 6 <= FX_CONV_NR && x0 + 6 <= W0 && xi + stride_x*6 <= Wi) {
                                        if (stride_x == 1) {
                                            for (int c = 0; c < dc; c++, inpbuf_ki += FX_CONV_NR, inptr_ki += inp_planesize) {
                                                int t0 = inptr_ki[0], t1 = inptr_ki[1];
                                                int t2 = inptr_ki[2], t3 = inptr_ki[3];
                                                int t4 = inptr_ki[4], t5 = inptr_ki[5];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                                inpbuf_ki[4] = t4; inpbuf_ki[5] = t5;
                                            }
                                        } else {
                                            for (int c = 0; c < dc; c++, inpbuf_ki += FX_CONV_NR, inptr_ki += inp_planesize) {
                                                int t0 = inptr_ki[0], t1 = inptr_ki[stride_x];
                                                int t2 = inptr_ki[stride_x*2], t3 = inptr_ki[stride_x*3];
                                                int t4 = inptr_ki[stride_x*4], t5 = inptr_ki[stride_x*5];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                                inpbuf_ki[4] = t4; inpbuf_ki[5] = t5;
                                            }
                                        }
                                        i += 6;
                                        x0 += 6;
                                    } else {
                                        for (int c = 0; c < dc; c++, inpbuf_ki += FX_CONV_NR, inptr_ki += inp_planesize)
                                            *inpbuf_ki = *inptr_ki;
                                        i++;
                                        x0++;
                                    }
                                } else {
                                    for (int c = 0; c < dc; c++, inpbuf_ki += FX_CONV_NR)
                                        *inpbuf_ki = 0;
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
                            for (; i < FX_CONV_NR_F16;) {
                                int16_t* inpbuf_ki = (int16_t*)inpbuf + k*(FX_CONV_NR_F16*Cg) + i;
                                int yi = y0*stride_y + dy - pad_top;
                                int xi = x0*stride_x + dx - pad_left;

                                if ((unsigned)yi < (unsigned)Hi &&
                                    (unsigned)xi < (unsigned)Wi) {
                                    const int16_t* inptr_ki = (int16_t*)inptr_ + yi*Wi + xi;
                                    if (i + 4 <= FX_CONV_NR_F16 && x0 + 4 <= W0 && xi + stride_x*4 <= Wi) {
                                        if (stride_x == 1) {
                                            for (int c = 0; c < dc; c++, inpbuf_ki += FX_CONV_NR_F16, inptr_ki += inp_planesize) {
                                                int16_t t0 = inptr_ki[0], t1 = inptr_ki[1];
                                                int16_t t2 = inptr_ki[2], t3 = inptr_ki[3];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                            }
                                        } else {
                                            for (int c = 0; c < dc; c++, inpbuf_ki += FX_CONV_NR_F16, inptr_ki += inp_planesize) {
                                                int16_t t0 = inptr_ki[0], t1 = inptr_ki[stride_x];
                                                int16_t t2 = inptr_ki[stride_x*2], t3 = inptr_ki[stride_x*3];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                            }
                                        }
                                        i += 4;
                                        x0 += 4;
                                    } else {
                                        for (int c = 0; c < dc; c++, inpbuf_ki += FX_CONV_NR_F16, inptr_ki += inp_planesize)
                                            *inpbuf_ki = *inptr_ki;
                                        i++;
                                        x0++;
                                    }
                                } else {
                                    for (int c = 0; c < dc; c++, inpbuf_ki += FX_CONV_NR_F16)
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
        }
    }

    // phase 2. do the actual convolution
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        int ngk0 = task_id*N*ngroups*Kg_nblocks/ntasks;
        int ngk1 = (task_id + 1)*N*ngroups*Kg_nblocks/ntasks;
        int dngk = 0;
        for ( ; ngk0 < ngk1; ngk0 += dngk ) {
            int n = ngk0 / (Kg_nblocks*ngroups);
            int gk0 = ngk0 - n*ngroups*Kg_nblocks;
            int g = gk0 / Kg_nblocks;
            int k0 = gk0 - g * Kg_nblocks;
            dngk = Kg_nblocks - k0;
            if (dngk > ngk1 - ngk0) dngk = ngk1 - ngk0;
            if (dngk > K_BLOCK/CONV_MR) dngk = K_BLOCK/CONV_MR;
            k0 *= CONV_MR;
            int dk0 = dngk * CONV_MR, k1;
            if (k0 + dk0 > Kg) dk0 = Kg - k0;
            k1 = k0 + dk0;
            size_t bufofs = (n*ngroups + g)*stripes_per_plane*stripe_size;
            size_t outofs = (n*ngroups + g)*Kg*out_planesize;

            if (inp_typ == FX_F32) {
                float outbuf[FX_CONV_MR*FX_CONV_NR];
                memset(outbuf, 0, CONV_MR*CONV_NR*sizeof(outbuf[0]));
                const float* inpbuf = (const float*)inpbuf_all + bufofs;
                int yx0 = 0;
                float* outptr0 = (float*)out_data->data + outofs;
                float* bpptr0 = bp_data && bp_data->data ? (float*)bp_data->data + outofs : 0;
                for (; yx0 < out_planesize; yx0 += CONV_NR, outptr0 += CONV_NR, inpbuf += stripe_size) {
                    int dyx = CONV_NR;
                    if (yx0 + dyx > out_planesize) dyx = out_planesize - yx0;
                    bool partial0 = activ_func || dyx < CONV_NR;
                    for (int k = k0; k < k1; k += CONV_MR) {
                        size_t wofs = (g*Kg_aligned + k)*HkWkCg;
                        const float* weights = conv->weights + wofs;
                        const float* biasptr = conv->bias + Kg*g + k;
                        int dk = CONV_MR;
                        if (dk > k1 - k) dk = k1 - k;
                        bool partial = partial0 || dk < CONV_MR;
                        float* outptr = outptr0 + k*out_planesize;
                        int outstep = out_planesize;
                        const float* bpptr = bpptr0 ? bpptr0 + k*out_planesize + yx0 : 0;

                        if (partial) {
                            if (bpptr) {
                                for (int k_ = 0; k_ < dk; k_++)
                                    memcpy(outbuf + k_*CONV_NR, bpptr + k_*outstep, dyx*sizeof(bpptr[0]));
                                bpptr = outbuf;
                            }
                            outptr = outbuf;
                            outstep = CONV_NR;
                        }
                        _fx_conv_block_f32(HkWkCg, weights, inpbuf, outptr, outstep, bpptr, outstep,
                                            biasptr, alpha, maxval, fast_activ);
                        if (partial) {
                            if (activ_func)
                                activ_func(outbuf, outbuf, CONV_NR*dk, activ_params);
                            for (int k_ = 0; k_ < dk; k_++)
                                memcpy(outptr0 + (k + k_)*out_planesize, outptr + k_*CONV_NR, dyx*sizeof(outptr[0]));
                        }
                    }
                }
            }
        #if _FX_NN_ENABLE_FP16
            else {
                fx_f16 outbuf[FX_CONV_MR_F16*FX_CONV_NR_F16];
                memset(outbuf, 0, CONV_MR*CONV_NR*sizeof(outbuf[0]));
                const fx_f16* inpbuf = (const fx_f16*)inpbuf_all + bufofs;
                int yx0 = 0;
                fx_f16* outptr0 = (fx_f16*)out_data->data + outofs;
                fx_f16* bpptr0 = bp_data && bp_data->data ? (fx_f16*)bp_data->data + outofs : 0;
                for (; yx0 < out_planesize; yx0 += CONV_NR, outptr0 += CONV_NR, inpbuf += stripe_size) {
                    int dyx = CONV_NR;
                    if (yx0 + dyx > out_planesize) dyx = out_planesize - yx0;
                    bool partial0 = activ_func || dyx < CONV_NR;
                    for (int k = k0; k < k1; k += CONV_MR) {
                        size_t wofs = (g*Kg_aligned + k)*HkWkCg;
                        const fx_f16* weights = conv->wf16 + wofs;
                        const float* biasptr = conv->bias + Kg*g + k;
                        int dk = CONV_MR;
                        if (dk > k1 - k) dk = k1 - k;
                        bool partial = partial0 || dk < CONV_MR;
                        fx_f16* outptr = outptr0 + k*out_planesize;
                        int outstep = out_planesize;
                        const fx_f16* bpptr = bpptr0 ? bpptr0 + k*out_planesize + yx0 : 0;

                        if (partial) {
                            if (bpptr) {
                                for (int k_ = 0; k_ < dk; k_++)
                                    memcpy(outbuf + k_*CONV_NR, bpptr + k_*outstep, dyx*sizeof(bpptr[0]));
                                bpptr = outbuf;
                            }
                            outptr = outbuf;
                            outstep = CONV_NR;
                        }
                        _fx_conv_block_f16(HkWkCg, weights, inpbuf, outptr, outstep, bpptr, outstep,
                                            biasptr, alpha, maxval, fast_activ);
                        if (partial) {
                            if (activ_func)
                                activ_func(outbuf, outbuf, CONV_NR*dk, activ_params);
                            for (int k_ = 0; k_ < dk; k_++)
                                memcpy(outptr0 + (k + k_)*out_planesize, outptr + k_*CONV_NR, dyx*sizeof(outptr[0]));
                        }
                    }
                }
            }
        #endif
        }
    }

    return FX_OK;
}
}

fun init_conv(kernel_shape: int [], strides: int [],
              dilations: int [], pads: int [], group: int,
              weights: Ast.nntensor_t,
              bias: Ast.nntensor_t,
              bn_data: Ast.nntensor_t [], bn_eps: float,
              activ_func: Ast.nnactiv_t, activ_params: float [],
              ntasks: int): cptr
@ccode
{
    int w_typ = weights->data.tag, b_typ = bias->data.tag;
    const fx_arr_t* w_data = &weights->data.u.NN_Data_I8;
    const fx_arr_t* bias_data = b_typ > 1 ? &bias->data.u.NN_Data_I8 : 0;
    const fx_arr_t* w_shape = &weights->shape.shape;
    const int_* w_shape_ = (const int_*)w_shape->data;
    const int_* strides_ = (const int_*)strides->data;
    const int_* dilations_ = (const int_*)dilations->data;
    const int_* pads_ = (const int_*)pads->data;
    int_ n_bn_data = bn_data->dim[0].size;
    int_ n_activ_params = activ_params->dim[0].size;
    int_ K = w_shape_ ? w_shape_[0] : 0;
    const float* bn_data_[4] = {0, 0, 0, 0};

    if (w_typ != FX_F32 || (b_typ > 1 && b_typ != FX_F32))
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    if (w_shape->ndims != 1 || w_shape->dim[0].size != 4 ||
        strides->ndims != 1 || strides->dim[0].size != 2 ||
        dilations->ndims != 1 || dilations->dim[0].size != 2 ||
        pads->ndims != 1 || pads->dim[0].size != 4 ||
        (n_bn_data != 4 && n_bn_data != 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeError);

    if (n_bn_data > 0) {
        if (n_bn_data != 4)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        for(int_ i = 0; i < n_bn_data; i++) {
            const _fx_nntensor_t* bn_data_i_ = ((const _fx_nntensor_t*)bn_data->data) + i;
            int bn_typ_i = bn_data_i_->data.tag;
            if (bn_typ_i != FX_F32)
                return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
            if (bn_data_i_->shape.shape.dim[0].size > 1)
                return FX_SET_EXN_FAST(FX_EXN_DimError);
            const fx_arr_t* bn_data_i = &bn_data_i_->data.u.NN_Data_I8;
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
        (const float*)w_data->data, bias_data ? (const float*)bias_data->data : 0,
        bn_data_[0], bn_data_[1], bn_data_[2], bn_data_[3], bn_eps,
        (int)activ_func->tag, (const float*)activ_params->data,
        (int)n_activ_params, (int)ntasks, fx_result);
}

fun run_conv(inp: Ast.nntensor_t, out: Ast.nntensor_t, bypass: Ast.nntensor_t,
             conv_data: cptr, ntasks: int, scratch_buf: Ast.nnbuf_t): Ast.nnbuf_t
@ccode {
    _fx_conv2d_t* conv = conv_data && conv_data->ptr ? (_fx_conv2d_t*)conv_data->ptr : 0;
    if (!conv)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
    return _fx_conv2d((const _fx_nntensor_t*)inp, (_fx_nntensor_t*)out,
                      (const _fx_nntensor_t*)bypass, conv, ntasks, scratch_buf, fx_result);
}

fun run_conv(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Conv {attr={kernel_shape, pads, strides, dilations, group},
        conv_data, fused_batch_norm, non_const_batch_norm,
        fused_activ, non_const_activ, t_inp, t_weights, t_bias, t_out, t_passby} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val pb = model.get_tensor(t_passby)
    assert(`kernel_shape.size() == 2 && inp.shape.shape.size() == 4`)
    if *conv_data == null || !model.isconst(t_weights) || !model.isconst(t_bias) ||
        non_const_batch_norm || non_const_activ {
        //println(f"Conv: weights.data: {weights.data.elemtype()}, bias.data: {bias.data.elemtype()}")
        val (bn_data, bn_eps) =
            match fused_batch_norm {
            | Some (Ast.NN_BatchNorm {epsilon, t_mean, t_var, t_scale, t_B}) =>
                    val bn_mean = model.get_tensor(t_mean)
                    val bn_var = model.get_tensor(t_var)
                    val bn_scale = model.get_tensor(t_scale)
                    val bn_bias = model.get_tensor(t_B)
                    ([bn_mean, bn_var, bn_scale, bn_bias], epsilon)
            | _ => (([]: Ast.nntensor_t []), 0.f)
            }
        val (activ_id, (activ_params : float [])) = match fused_activ {
            | Some (Ast.NN_Elemwise {el_op=Ast.NN_Relu}) => (Ast.NN_ACTIV_RELU, [])
            | Some (Ast.NN_Elemwise {el_op=Ast.NN_Sigmoid}) => (Ast.NN_ACTIV_SIGMOID, [])
            | Some (Ast.NN_Elemwise {el_op=Ast.NN_Tanh}) => (Ast.NN_ACTIV_TANH, [])
            | Some (Ast.NN_Elemwise {el_op=Ast.NN_Mish}) => (Ast.NN_ACTIV_MISH, [])
            | Some (Ast.NN_Clip {t_min, t_max}) =>
                val minval = model.get_tensor(t_min)
                val maxval = model.get_tensor(t_max)
                val minval = minval.data.float_scalar_or(-FLT_MAX)
                val maxval = maxval.data.float_scalar_or(FLT_MAX)
                (Ast.NN_ACTIV_CLIP, [minval, maxval])
            | Some (Ast.NN_LeakyRelu {alpha}) => (Ast.NN_ACTIV_LRELU, [alpha])
            | Some op =>
                throw Ast.NNError(f"unexpected activation {op.name()}")
            | _ => (Ast.NN_ACTIV_NONE, [])
            }
        val weights = model.get_tensor(t_weights)
        val bias = model.get_tensor(t_bias)
        *conv_data = null // first of all, release the previous data, if any
                          // this way we can immediately re-use the same chunk of memory
                          // for the updated convolution structure
        *conv_data = init_conv(kernel_shape, strides, dilations, pads, group,
                                weights, bias, bn_data, bn_eps,
                                activ_id, activ_params, *model.ntasks)
    }
    *model.scratch_buf = run_conv(inp, out, pb, *conv_data, *model.ntasks, *model.scratch_buf)
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}
