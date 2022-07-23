/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast

type activ_func_t =
    | ACTIV_NONE | ACTIV_RELU | ACTIV_CLIP | ACTIV_LRELU
    | ACTIV_PRELU | ACTIV_SIGMOID | ACTIV_TANH | ACTIV_MISH

@ccode {
#include <assert.h>
#include <float.h>
#include <math.h>
#include "ficus_nn_common.h"

enum { _FX_LAYOUT_UNKNOWN=0, _FX_LAYOUT_NCHW=1, _FX_LAYOUT_NHWC=2 };
enum { _FX_CONV_TYPE_GENERIC=0, _FX_CONV_TYPE_DEPTHWISE=1, _FX_CONV_TYPE_WINOGRAD3X3=2 };
enum {
    _FX_WINO_STEP=6,
    _FX_WINO_KSIZE=3,
    _FX_WINO_SIZE=_FX_WINO_STEP+_FX_WINO_KSIZE-1,
    _FX_WINO_AREA=_FX_WINO_SIZE*_FX_WINO_SIZE
};

enum { _FX_ACTIV_CUSTOM=0, _FX_ACTIV_NONE=1, _FX_ACTIV_RELU=2, _FX_ACTIV_CLIP=3, _FX_ACTIV_LRELU=4,
       _FX_ACTIV_PRELU=5, _FX_ACTIV_SIGMOID=6, _FX_ACTIV_TANH=7, _FX_ACTIV_MISH=8 };

typedef void (*_fx_conv_block_t)(int k, const void *a, const void *b,
                                 void *c, int ldc, const void* pb, int ldp,
                                 const void* bias, float alpha,
                                 float maxval, bool activ );
typedef void (*_fx_activ_func_t)(float* data, size_t step, int size0,
                                 int size1, const float* params);

#ifdef __ARM_NEON
#define FX_CONV_MR 4
#define FX_CONV_NR 28
enum { FX_VEC_NLANES=4 };
#elif defined __AVX__
#define FX_CONV_MR 4
#define FX_CONV_NR 24
enum { FX_VEC_NLANES=8 };
#else
enum { FX_VEC_NLANES=1 };
#endif

#ifdef __ARM_NEON
#define FX_CONV_MR_FP16 8
#define FX_CONV_NR_FP16 24
typedef __fp16 flt16_t;
#else
#define FX_CONV_MR_FP16 FX_CONV_MR
#define FX_CONV_NR_FP16 FX_CONV_NR
typedef int16_t flt16_t;
#endif

static void _fx_activ_clip(float* data, size_t step, int size0,
                            int size1, const float* params)
{
    float a = params[0], b = params[1];
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j++) {
            float x = data[j];
            x = x >= a ? x : a;
            x = x <= b ? x : b;
            data[j] = x;
        }
    }
}

static void _fx_activ_lrelu(float* data, size_t step, int size0,
                            int size1, const float* params)
{
    float alpha = params[0];
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j++) {
            float x = data[j];
            float a = x >= 0.f ? 1 : alpha;
            data[j] = x*a;
        }
    }
}

static void _fx_activ_prelu(float* data, size_t step, int size0,
                            int size1, const float* params)
{
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j++) {
            float x = data[j];
            float alpha = params[j];
            float a = x >= 0.f ? 1 : alpha;
            data[j] = x*a;
        }
    }
}

static void _fx_activ_sigmoid(float* data, size_t step, int size0,
                              int size1, const float* params)
{
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j++) {
            float x = data[j];
            float e_x = expf(-fabsf(x));
            float denom = 1.f/(1 + e_x);
            data[j] = (x >= 0.f ? 1.f : e_x)*denom;
        }
    }
}

static void _fx_activ_tanh(float* data, size_t step, int size0,
                           int size1, const float* params)
{
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j++) {
            data[j] = tanh(data[j]);
        }
    }
}

static void _fx_activ_mish(float* data, size_t step, int size0,
                           int size1, const float* params)
{
#ifdef __ARM_NEON
    assert(size1 % 4 == 0);
    float32x4_t thr = vdupq_n_f32(-36.73f), one = vdupq_n_f32(1.f), z = vdupq_n_f32(0.f);
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j += 4) {
            float32x4_t x = vld1q_f32(data + j);
            x = vbslq_f32(vcleq_f32(x, thr), z, x);
            float32x4_t y = _fx_vexpq_f32(vsubq_f32(z, x));
            float32x4_t y2 = vaddq_f32(y, y);
            float32x4_t y2_1 = vaddq_f32(y2, one);
            x = vdivq_f32(
                vmulq_f32(x, y2_1),
                vaddq_f32(y2_1, vmulq_f32(y2, y)));
            vst1q_f32(data + j, x);
        }
    }
#else
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j += 4) {
            float x = data[j];
            x *= (x > -36.73f ? 1.f : 0.f);
            float y = expf(-x);
            data[j] = x*(1 + 2*y)/(1 + 2*y + 2*y*y);
        }
    }
#endif
}

enum { FX_CONV_TYPE_GENERIC=0, FX_CONV_TYPE_DEPTHWISE=1, FX_CONV_TYPE_WINOGRAD3X3=2 };

typedef struct _fx_conv2d_t
{
    int layout, ngroups;
    int K, C, Hk, Wk;
    int stride_y, stride_x;
    int dilation_y, dilation_x;
    int pad_top, pad_bottom, pad_left, pad_right;
    int conv_type;
    float* weights;
    flt16_t* wf16;
    float* bias;
    int activ;
    float* activ_params;
    int nactive_params;
    _fx_activ_func_t activ_func;
    float minval, maxval;
} _fx_conv2d_t;

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
    _fx_activ_func_t activ_func,
    const float* activ_params,
    int activ_nparams,
    fx_cptr_t* fx_result)
{
    _fx_conv2d_t* conv = (_fx_conv2d_t*)fx_malloc(sizeof(*conv));
    float* bn_ab = bn_mean || bn_var || bn_scale || bn_shift ?
        (float*)fx_malloc(K*2*sizeof(bn_ab[0])) : 0;
    int k, nbias = K + 32;
    int fx_status;

    memset(conv, 0, sizeof(*conv));
    assert(layout_orig == _FX_LAYOUT_NCHW && layout == _FX_LAYOUT_NCHW);
    assert(ngroups > 0 && K > 0 && C > 0 && K % ngroups == 0 && C % ngroups == 0);
    assert(Hk > 0 && Wk > 0);
    assert(stride_y > 0 && stride_x > 0);
    assert(dilation_y > 0 && dilation_x > 0);
    assert(pad_top >= 0 && pad_bottom >= 0 && pad_left >= 0 && pad_right >= 0);
    assert((activ_func == 0) ^ (activ == _FX_ACTIV_CUSTOM));
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
    conv->activ_func =
        activ == _FX_ACTIV_CUSTOM ? activ_func :
        activ == _FX_ACTIV_RELU ? 0 :
        activ == _FX_ACTIV_CLIP ? _fx_activ_clip :
        activ == _FX_ACTIV_LRELU ? _fx_activ_lrelu :
        activ == _FX_ACTIV_PRELU ? _fx_activ_prelu :
        activ == _FX_ACTIV_MISH ? _fx_activ_mish :
        activ == _FX_ACTIV_SIGMOID ? _fx_activ_sigmoid :
        activ == _FX_ACTIV_TANH ? _fx_activ_tanh : 0;
    conv->minval = -FLT_MAX;
    conv->maxval = FLT_MAX;
    conv->activ_params = 0;
    if (activ == _FX_ACTIV_RELU) {
        conv->minval = 0.f;
    } else {
        if (activ == _FX_ACTIV_CLIP) {
            assert(activ_params && activ_nparams == 2);
            conv->minval = activ_params[0];
            conv->maxval = activ_params[1];
        }
        if (activ_params) {
            assert(activ_nparams > 0);
            conv->activ_params = (float*)fx_malloc(activ_nparams*sizeof(activ_params[0]));
            if (!conv->activ_params)
                return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
            memcpy(conv->activ_params, activ_params, activ_nparams*sizeof(activ_params[0]));
        }
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
                fx_sgemm(false, true, scale, 0.f,
                        _FX_WINO_SIZE, 3, G, 3, 1,
                        3, 3, weights + (k*Cg + c)*Hk*Wk, 3, 1,
                        GW, 3, 1);
                fx_sgemm(false, true, 1.f, 0.f,
                        _FX_WINO_SIZE, 3, GW, 3, 1,
                        _FX_WINO_SIZE, 3, G, 3, 1,
                        conv->weights + (k*Cg + c)*_FX_WINO_AREA, _FX_WINO_SIZE, 1);
            }
        }
    } else {
        int Kg = K/ngroups, Cg = C/ngroups;
#ifdef __ARM_NEON
        {
        // the weights are packed as
        // ngroups x (ceil((K/ngroups)/FX_CONV_MR)*FX_CONV_MR) x (Cg*Hk*Wk) x FX_CONV_MR tensor
        int Kg_aligned = ((Kg + FX_CONV_MR_FP16 - 1)/FX_CONV_MR_FP16)*FX_CONV_MR_FP16;
        size_t nweights = (size_t)ngroups*Kg_aligned*Cg*Hk*Wk;
        conv->wf16 = (flt16_t*)fx_malloc(nweights*sizeof(conv->weights[0]));
        if (conv->wf16) {
            memset(conv->wf16, 0, nweights*sizeof(conv->wf16[0]));
            flt16_t* packed_wptr = conv->wf16;
            for(int g = 0; g < ngroups; g++) {
                for(int k0 = 0; k0 < Kg_aligned; k0 += FX_CONV_MR_FP16) {
                    int dk = Kg - k0 < FX_CONV_MR_FP16 ? Kg - k0 : FX_CONV_MR_FP16;
                    int k_idx = g*Kg + k0;
                    for(int c = 0; c < Cg; c++) {
                        for(int yx = 0; yx < Hk*Wk; yx++, packed_wptr += FX_CONV_MR_FP16) {
                            const float* wptr = weights + (size_t)(k_idx*Cg + c)*Hk*Wk + yx;
                            int k = 0;
                            for(; k < dk; k++, wptr += Cg*Hk*Wk) {
                                float scale = bn_ab ? bn_ab[k_idx+k] : 1.f;
                                packed_wptr[k] = (flt16_t)(*wptr*scale);
                            }
                            for(; k < FX_CONV_MR_FP16; k++)
                                packed_wptr[k] = (flt16_t)0.f;
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
                    for(int c = 0; c < Cg; c++) {
                        for(int yx = 0; yx < Hk*Wk; yx++, packed_wptr += FX_CONV_MR) {
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

#include "ficus_nn_conv_block.h"
#include "ficus_nn_conv_depthwise.h"

#if 0 //def __ARM_NEON

static int _fx_conv2d_fp16(int ndims, const int_* inpsize, const float* inp,
                          const int_* outsize, float* out,
                          const float* passby, _fx_conv2d_t* conv,
                          int ntasks)
{
    assert(ndims == 4 &&
           inpsize[0] == outsize[0] &&
           outsize[1] == conv->K &&
           inpsize[1] == conv->C);
    int N = (int)inpsize[0], C = (int)inpsize[1], Hi = (int)inpsize[2], Wi = (int)inpsize[3];
    int K = conv->K, Hk = conv->Hk, Wk = conv->Wk, ksize = Hk*Wk, ngroups = conv->ngroups;
    int H0 = (int)outsize[2], W0 = (int)outsize[3];
    int Cg = C/ngroups, Kg = K/ngroups;
    int Kg_nblocks = (Kg + FX_CONV_MR_FP16-1)/FX_CONV_MR_FP16, Kg_aligned = Kg_nblocks*FX_CONV_MR_FP16;
    int inp_planesize = Hi*Wi;
    int out_planesize = H0*W0;
    float minval = conv->minval, maxval = conv->maxval;
    float alpha =
        conv->activ == _FX_ACTIV_RELU ? 0.f :
        conv->activ == _FX_ACTIV_LRELU ? conv->activ_params[0] :
        conv->activ == _FX_ACTIV_CLIP && minval == 0.f ? 0.f : 1.f;
    bool fast_activ = conv->activ == _FX_ACTIV_RELU ||
                      conv->activ == _FX_ACTIV_LRELU ||
                      (conv->activ == _FX_ACTIV_CLIP && minval == 0);
    _fx_activ_func_t activ_func = !fast_activ ? conv->activ_func : 0;
    //printf("activ=%d, minval=%f, maxval=%f, alpha=%f, fast_activ=%d, activ_func=%p\n",
    //    conv->activ, minval, maxval, alpha, (int)fast_activ, activ_func);
    const float* activ_params = conv->activ_params;
    int stripes_per_sample = (out_planesize + FX_CONV_NR_FP16 - 1)/FX_CONV_NR_FP16;
    if (stripes_per_sample < ntasks*4)
        stripes_per_sample = 1;
    else
        Kg_nblocks = 1;
    int Kstripes = Kg_nblocks*stripes_per_sample;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int nsubtasks = N*ngroups*Kstripes;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left;
    int pad_right = W0 - pad_left - 1 + Wk - Wi, pad_x = pad_left + pad_right;
    bool fast_1x1 = stride_x == 1 && stride_y == 1 && ksize == 1;
    bool s1d1 = stride_x == 1 && stride_y == 1 && dilation_x == 1 && dilation_y == 1;
    int HkWkCg = ksize*Cg, HkWkC = HkWkCg*ngroups;
    size_t taskbufsize = FX_CONV_NR_FP16*ksize*Cg;
    flt16_t* inpbuf_all = (flt16_t*)fx_malloc(ntasks*taskbufsize*sizeof(inpbuf_all[0]) + ksize*3*sizeof(int));
    int* interior_ofstab = (int*)(inpbuf_all + ntasks*taskbufsize);
    int* yxtab = interior_ofstab + Hk*Wk;
    int64_t ts = fx_tick_count();
    assert(!s1d1 || (pad_right <= conv->pad_right && pad_right >= 0));

    for (int y = 0; y < Hk; y++)
        for( int x = 0; x < Wk; x++) {
            int k = y*Wk + x;
            int dy = y*dilation_y, dx = x*dilation_x;
            yxtab[k*2] = dy; yxtab[k*2+1] = dx;
            interior_ofstab[k] = dy*Wi + dx;
        }
    if (ksize == 1) {
        assert(pad_left == 0 && pad_right == 0 && pad_top == 0 && pad_bottom == 0);
        assert(stride_x != 1 || stride_y != 1 || (H0 == Hi && W0 == Wi));
    }

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        flt16_t* inpbuf_task = &inpbuf_all[taskbufsize*task_id];
        //float inpbuf_task_gold[Cg*ksize*FX_CONV_NR_FP16];
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
            int yx0, yx_limit;
            if (stripes_per_sample == 1) {
                k0 = kyx0 * FX_CONV_MR_FP16;
                k1 = kyx1 * FX_CONV_MR_FP16;
                k1 = k1 <= Kg ? k1 : Kg;
                yx0 = 0;
                yx_limit = out_planesize;
            } else {
                k0 = 0;
                k1 = Kg;
                yx0 = kyx0*FX_CONV_NR_FP16;
                yx_limit = kyx1*FX_CONV_NR_FP16;
                yx_limit = yx_limit < out_planesize ? yx_limit : out_planesize;
            }

            for (; yx0 < yx_limit; yx0 += FX_CONV_NR_FP16) {
                flt16_t* inpbuf = inpbuf_task;
                const float* inptr = inp + inp_plane_ofs;
                int yx1 = yx0 + FX_CONV_NR_FP16;
                yx1 = yx1 <= yx_limit ? yx1 : yx_limit;
                int slice_len = yx1 - yx0;
                bool partial0 = slice_len < FX_CONV_NR_FP16;
                //if (partial0)
                //    putchar('.');
                /*
                    1. pack the data. Copy the HkxWk FX_CONV_NR_FP16-wide slices from
                       each feature plane of the input tensor to the input buffer.
                */
                if (fast_1x1) {
                    /*
                       super-fast branch for 1x1 convolutions with sy=sx=1.
                       in this case each feature plane can be safely treated
                       as 1D array and we just extract next portion
                       of FX_CONV_NR_FP16 elements from each feature plane and
                       put it together.
                    */
                    inptr += yx0;
                    if (!partial0) {
                        // Make special branch where memcpy() is called with a constant buffer size.
                        // Compilers will likely unroll this loop properly.
                        for (int c = 0; c < Cg; c++, inptr += inp_planesize, inpbuf += FX_CONV_NR_FP16) {
                            for (int j = 0; j < FX_CONV_NR_FP16; j++)
                                inpbuf[j] = (flt16_t)inptr[j];
                        }
                    } else {
                        for (int c = 0; c < Cg; c++, inptr += inp_planesize, inpbuf += FX_CONV_NR_FP16) {
                            for (int j = 0; j < slice_len; j++)
                                inpbuf[j] = (flt16_t)inptr[j];
                            memset(inpbuf + slice_len, 0, (FX_CONV_NR_FP16 - slice_len)*sizeof(inpbuf[0]));
                        }
                    }
                } else if (s1d1) {
                    /*
                     slower, but still fast branch for sy=sx=1, dy=dx=1.
                     in this case we copy data from input tensors by chunks and
                     interleave the data in inpbuf with 0's
                     (that correspond to the padding elements) when necessary
                     */
                    int y0 = yx0/W0, x0 = yx0 - y0*W0;
                    for (int k = 0; k < ksize; k++) {
                        int dx_k = yxtab[k*2+1] - pad_left;
                        int yi = y0 - pad_top + yxtab[k*2], xi = x0 + dx_k;
                        // xi_0 is the x-coordinate that we set when we move to the next row.
                        // Wi_part, correspondingly, is how many elements at
                        // max we can grab from the input tensor row as long as we set xi=xi_0.
                        int xi_0 = dx_k <= 0 ? 0 : dx_k;
                        int Wi_part = W0 + (dx_k <= 0 ? dx_k : 0);
                        Wi_part = (Wi_part < Wi - xi_0) ? Wi_part : (Wi - xi_0);
                        // di_z0 is how many zero elements we put to inpbuf between rows.
                        int di_z0 = dx_k <= 0 ? -dx_k : W0 + dx_k - Wi;
                        flt16_t* inpbuf_k = inpbuf_task + k*FX_CONV_NR_FP16;
                        int i = 0, di = W0 - x0;
                        // if we are initially outside of the input tensor, we first put some 0's
                        // into inpbuf and then start the main loop
                        if (((unsigned)xi >= (unsigned)Wi) | ((unsigned)yi >= (unsigned)Hi)) {
                            di = xi >= Wi ? di : xi >= 0 ? 0 : -xi < di ? -xi : di;
                            if ((unsigned)yi < (unsigned)(Hi-1))
                                yi += (xi >= Wi);
                            else if (yi < 0) {
                                di = (-yi-1)*W0 + (W0 - x0) + (dx_k < 0 ? -dx_k : 0);
                                yi = 0;
                            } else if ((yi >= Hi) | (xi >= 0))
                                di = FX_CONV_NR_FP16;
                            di = di < FX_CONV_NR_FP16 ? di : FX_CONV_NR_FP16;
                            assert(di > 0);
                            for (int c = 0; c < Cg; c++)
                                memset(inpbuf_k + c*(FX_CONV_NR_FP16*ksize), 0, di*sizeof(inpbuf_k[0]));
                            i = di;
                            xi = xi_0;
                            di = Wi_part;
                        }
                        di = di < Wi - xi ? di : Wi - xi;
                        for (; i < FX_CONV_NR_FP16;) {
                            di = (di < FX_CONV_NR_FP16 - i) ? di : (FX_CONV_NR_FP16 - i);
                            const float* inptr_k = inptr + yi*Wi + xi;
                            int di_z = FX_CONV_NR_FP16 - (i + di);
                            if (di_z > 0) {
                                // we handle the end of the feature plane gracefully,
                                // in this case we just add as many 0's as necessary
                                // to complete each contiguous slice
                                // in inpbuf to FX_CONV_NR_FP16 elements.
                                di_z = ((yi == Hi-1) | (di_z < di_z0)) ? di_z : di_z0;
                                for (int c = 0; c < Cg; c++) {
                                    for (int j = 0; j < di; j++)
                                        inpbuf_k[i + c*(FX_CONV_NR_FP16*ksize) + j] = inptr_k[c*inp_planesize + j];
                                    memset(inpbuf_k + i + di + c*(FX_CONV_NR_FP16*ksize),
                                           0, di_z*sizeof(inpbuf_k[0]));
                                }
                            } else {
                                for (int c = 0; c < Cg; c++) {
                                    for (int j = 0; j < di; j++)
                                        inpbuf_k[i + c*(FX_CONV_NR_FP16*ksize) + j] = inptr_k[c*inp_planesize + j];
                                }
                            }
                            i += di + di_z;
                            di = Wi_part;
                            xi = xi_0;
                            yi++;
                        }
                    }
                } else {
                    int y0_ = yx0/W0, x0_ = yx0 - y0_*W0;
                    for (int k = 0; k < ksize; k++) {
                        int dy = yxtab[k*2], dx = yxtab[k*2+1];
                        int i = 0, y0 = y0_, x0 = x0_;
                        for (; i < FX_CONV_NR_FP16;) {
                            flt16_t* inpbuf_ki = inpbuf_task + k*FX_CONV_NR_FP16 + i;
                            int yi = y0*stride_y + dy - pad_top;
                            int xi = x0*stride_x + dx - pad_left;

                            if ((unsigned)yi < (unsigned)Hi &&
                                (unsigned)xi < (unsigned)Wi) {
                                const float* inptr_ki = inptr + yi*Wi + xi;
                                if (i + 4 <= FX_CONV_NR_FP16 && x0 + 4 <= W0 && xi + stride_x*4 <= Wi) {
                                    if (stride_x == 2) {
                                        for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR_FP16*ksize, inptr_ki += inp_planesize) {
                                            flt16_t t0 = (flt16_t)inptr_ki[0], t1 = (flt16_t)inptr_ki[2];
                                            flt16_t t2 = (flt16_t)inptr_ki[4], t3 = (flt16_t)inptr_ki[6];
                                            inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                            inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                        }
                                    } else {
                                        for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR_FP16*ksize, inptr_ki += inp_planesize) {
                                            flt16_t t0 = (flt16_t)inptr_ki[0], t1 = (flt16_t)inptr_ki[stride_x];
                                            flt16_t t2 = (flt16_t)inptr_ki[stride_x*2], t3 = (flt16_t)inptr_ki[stride_x*3];
                                            inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                            inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                        }
                                    }
                                    i += 4;
                                    x0 += 4;
                                } else {
                                    for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR_FP16*ksize, inptr_ki += inp_planesize)
                                        *inpbuf_ki = (flt16_t)*inptr_ki;
                                    i++;
                                    x0++;
                                }
                            } else {
                                for (int c = 0; c < Cg; c++, inpbuf_ki += FX_CONV_NR_FP16*ksize)
                                    inpbuf_ki[0] = (flt16_t)0.f;
                                i++;
                                x0++;
                            }
                            int mask = x0 >= W0;
                            y0 += mask;
                            x0 &= mask-1;
                        }
                    }
                }

                // 2. do convolution, compute Kg x (yx1 - yx0) part of the output tensor
                {
                float cbuf[FX_CONV_MR_FP16*FX_CONV_NR_FP16], pbbuf[FX_CONV_MR_FP16*FX_CONV_NR_FP16];
                int outstep0 = out_planesize;
                int pbstep0 = passby ? out_planesize : FX_CONV_NR_FP16;
                size_t outofs = ((n*ngroups + g)*Kg + k0)*outstep0 + yx0;
                float* outptr0 = out + outofs;
                const float* pbptr0 = passby ? passby + outofs : pbbuf;
                memset(pbbuf, 0, sizeof(pbbuf));
                for(int k = k0; k < k1; k += FX_CONV_MR_FP16, outptr0 += outstep0*FX_CONV_MR_FP16,
                                    pbptr0 += (passby ? outstep0*FX_CONV_MR_FP16 : 0)) {
                    int dk = Kg - k < FX_CONV_MR_FP16 ? Kg - k : FX_CONV_MR_FP16;
                    bool partial = partial0 || dk < FX_CONV_MR_FP16 || activ_func;
                    float* outptr = outptr0;
                    float* pbptr = (float*)pbptr0;
                    int outstep = outstep0, pbstep = pbstep0;
                    if (partial) {
                        outptr = cbuf;
                        outstep = FX_CONV_NR_FP16;
                        if (passby) {
                            pbptr = pbbuf;
                            pbstep = outstep;
                            for (int k1 = 0; k1 < dk; k1++)
                                memcpy(&pbbuf[k1*FX_CONV_NR_FP16], pbptr0 + k1*outstep0,
                                    slice_len*sizeof(pbbuf[0]));
                        }
                    }
                    _fx_conv_block_fp16(HkWkCg, conv->wf16+(g*Kg_aligned + k)*HkWkCg,
                                        inpbuf_task, outptr, outstep, pbptr, pbstep,
                                        conv->bias + Kg*g + k,
                                        alpha, maxval, fast_activ);
                    if (partial) {
                        if (activ_func)
                            activ_func(outptr, outstep, 1, dk*FX_CONV_NR_FP16, activ_params);
                        for (int i = 0; i < dk; i++)
                            memcpy(outptr0 + i*outstep0, &cbuf[i*FX_CONV_NR_FP16],
                                   (yx1 - yx0)*sizeof(cbuf[0]));
                    }
                }
                }
            }
        }
    }

    fx_free(inpbuf_all);
    return FX_OK;
}
#endif

static int _fx_conv2d(const _fx_nntensor_t* inp, _fx_nntensor_t* out,
                      const _fx_nntensor_t* passby,
                      _fx_conv2d_t* conv, int_ ntasks,
                      fx_arr_t* scratch_buf, fx_arr_t* fx_result)
{
    int inp_typ = inp->data.tag;
    int out_typ = out->data.tag;
    int pb_typ = passby->data.tag;

    int CONV_MR = inp_typ == _FX_NN_FP16 ? FX_CONV_MR_FP16 : FX_CONV_MR;
    int CONV_NR = inp_typ == _FX_NN_FP16 ? FX_CONV_NR_FP16 : FX_CONV_NR;

    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    const fx_arr_t* pb_shape_ = &passby->shape.shape;
    const int_* inp_shape = (const int_*)(inp_shape_->data);
    const int_* out_shape = (const int_*)(out_shape_->data);
    const int_* pb_shape = (const int_*)(pb_shape_->data);
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    const fx_arr_t* pb_data = &passby->data.u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;
    int ndims = inp_shape_->dim[0].size;
    int out_ndims = out_shape_->dim[0].size;
    _fx_conv_block_t conv_block_func;
    float minval = conv->minval, maxval = conv->maxval;
    bool fast_activ = conv->activ == _FX_ACTIV_RELU || conv->activ == _FX_ACTIV_CLIP;
    _fx_activ_func_t activ_func = !fast_activ ? conv->activ_func : 0;
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
    size_t taskbufsize = CONV_NR*ksize*Cg, totalbufsize = taskbufsize*ntasks*esz;
    char* inpbuf_all = 0;
    int* interior_ofstab = (int*)alloca(ksize*3*sizeof(interior_ofstab[0]));
    int* yxtab = interior_ofstab + ksize;

    if (ndims != 4 || inp_shape[0] != out_shape[0] ||
        inp_shape[1] != conv->C || out_shape[1] != conv->K)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (inp_typ != out_typ)
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
        fx_copy_arr(scratch_buf, fx_result);
        return _fx_depthwise_conv2d(ndims, inp_shape, inp_data, out_shape, out_data, conv, ntasks);
    }

    conv_block_func = inp_typ == _FX_NN_FP32 ? _fx_conv_block_f32 : 0;
    if (!conv_block_func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (totalbufsize > scratch_buf->dim[0].step*scratch_buf->dim[0].size) {
        int_ totalbufsz = (int_)totalbufsize;
        int status = fx_make_arr(1, &totalbufsz, 1, 0, 0, 0, fx_result);
        if (status < 0)
            return status;
    } else {
        fx_copy_arr(scratch_buf, fx_result);
    }
    inpbuf_all = fx_result->data;

    for (int y = 0; y < Hk; y++)
        for( int x = 0; x < Wk; x++) {
            int k = y*Wk + x;
            int dy = y*dilation_y, dx = x*dilation_x;
            yxtab[k*2] = dy; yxtab[k*2+1] = dx;
            interior_ofstab[k] = dy*Wi + dx;
        }

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        char* inpbuf_task = &inpbuf_all[taskbufsize*task_id*esz];
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
            int yx0, yx_limit;
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

            for (; yx0 < yx_limit; yx0 += CONV_NR) {
                char* inpbuf = inpbuf_task;
                const char* inptr = inp_data->data + inp_plane_ofs*esz;
                int yx1 = yx0 + CONV_NR;
                yx1 = yx1 <= yx_limit ? yx1 : yx_limit;
                int slice_len = yx1 - yx0;
                bool partial0 = slice_len < CONV_NR;
                //if (partial0)
                //    putchar('.');
                /*
                    1. pack the data. Copy the HkxWk CONV_NR-wide slices from
                       each feature plane of the input tensor to the input buffer.
                */
                if (fast_1x1) {
                    // super-fast branch for 1x1 convolutions with sy=sx=1.
                    // in this case each feature plane can be safely treated
                    // as 1D array and we just extract next portion
                    // of CONV_NR elements from each feature plane and
                    // put it together.
                    inptr += yx0*esz;
                    if (!partial0) {
                        // Make special branch where memcpy() is called with a constant buffer size.
                        // Compilers will likely unroll this loop properly.
                        if (esz == sizeof(float)) {
                            for (int c = 0; c < Cg; c++, inptr += inp_planesize*esz, inpbuf += CONV_NR*esz)
                                memcpy(inpbuf, inptr, CONV_NR*sizeof(float));
                        } else if (esz == sizeof(flt16_t)) {
                            for (int c = 0; c < Cg; c++, inptr += inp_planesize*esz, inpbuf += CONV_NR*esz)
                                memcpy(inpbuf, inptr, CONV_NR*sizeof(flt16_t));
                        } else {
                            for (int c = 0; c < Cg; c++, inptr += inp_planesize*esz, inpbuf += CONV_NR*esz)
                                memcpy(inpbuf, inptr, CONV_NR*esz);
                        }
                    } else {
                        for (int c = 0; c < Cg; c++, inptr += inp_planesize*esz, inpbuf += CONV_NR*esz) {
                            memcpy(inpbuf, inptr, slice_len*esz);
                            memset(inpbuf + slice_len*esz, 0, (CONV_NR - slice_len)*esz);
                        }
                    }
                } else if (s1d1) {
                    // slower, but still fast branch for sy=sx=1, dy=dx=1.
                    // in this case we copy data from input tensors by chunks and
                    // interleave the data in inpbuf with 0's
                    // (that correspond to the padding elements) when necessary
                    int y0 = yx0/W0, x0 = yx0 - y0*W0;
                    for (int k = 0; k < ksize; k++) {
                        int dx_k = yxtab[k*2+1] - pad_left;
                        int yi = y0 - pad_top + yxtab[k*2], xi = x0 + dx_k;
                        // xi_0 is the x-coordinate that we set when we move to the next row.
                        // Wi_part, correspondingly, is how many elements at
                        // max we can grab from the input tensor row as long as we set xi=xi_0.
                        int xi_0 = dx_k <= 0 ? 0 : dx_k;
                        int Wi_part = W0 + (dx_k <= 0 ? dx_k : 0);
                        Wi_part = (Wi_part < Wi - xi_0) ? Wi_part : (Wi - xi_0);
                        // di_z0 is how many zero elements we put to inpbuf between rows.
                        int di_z0 = dx_k <= 0 ? -dx_k : W0 + dx_k - Wi;
                        char* inpbuf_k = inpbuf_task + k*CONV_NR*esz;
                        int i = 0, di = W0 - x0;
                        // if we are initially outside of the input tensor, we first put some 0's
                        // into inpbuf and then start the main loop
                        if (((unsigned)xi >= (unsigned)Wi) | ((unsigned)yi >= (unsigned)Hi)) {
                            di = xi >= Wi ? di : xi >= 0 ? 0 : -xi < di ? -xi : di;
                            if ((unsigned)yi < (unsigned)(Hi-1))
                                yi += (xi >= Wi);
                            else if (yi < 0) {
                                di = (-yi-1)*W0 + (W0 - x0) + (dx_k < 0 ? -dx_k : 0);
                                yi = 0;
                            } else if ((yi >= Hi) | (xi >= 0))
                                di = CONV_NR;
                            di = di < CONV_NR ? di : CONV_NR;
                            assert(di > 0);
                            for (int c = 0; c < Cg; c++)
                                memset(inpbuf_k + c*((CONV_NR*ksize)*esz), 0, di*esz);
                            i = di;
                            xi = xi_0;
                            di = Wi_part;
                        }
                        di = di < Wi - xi ? di : Wi - xi;
                        for (; i < CONV_NR;) {
                            di = (di < CONV_NR - i) ? di : (CONV_NR - i);
                            const char* inptr_k = inptr + (yi*Wi + xi)*esz;
                            int di_z = CONV_NR - (i + di);
                            if (di_z > 0) {
                                // we handle the end of the feature plane gracefully,
                                // in this case we just add as many 0's as necessary
                                // to complete each contiguous slice
                                // in inpbuf to CONV_NR elements.
                                di_z = ((yi == Hi-1) | (di_z < di_z0)) ? di_z : di_z0;
                                for (int c = 0; c < Cg; c++) {
                                    memcpy(inpbuf_k + (i + c*(CONV_NR*ksize))*esz,
                                           inptr_k + c*(inp_planesize*esz),
                                           di*esz);
                                    memset(inpbuf_k + (i + di + c*(CONV_NR*ksize))*esz,
                                           0, di_z*esz);
                                }
                            } else {
                                for (int c = 0; c < Cg; c++) {
                                    memcpy(inpbuf_k + (i + c*(CONV_NR*ksize))*esz,
                                           inptr_k + c*(inp_planesize*esz),
                                           di*esz);
                                }
                            }
                            i += di + di_z;
                            di = Wi_part;
                            xi = xi_0;
                            yi++;
                        }
                    }
                } else {
                    int y0_ = yx0/W0, x0_ = yx0 - y0_*W0;
                    for (int k = 0; k < ksize; k++) {
                        int dy = yxtab[k*2], dx = yxtab[k*2+1];
                        if (esz == 4) {
                            int i = 0, y0 = y0_, x0 = x0_;
                            for (; i < CONV_NR;) {
                                float* inpbuf_ki = (float*)inpbuf_task + k*CONV_NR + i;
                                int yi = y0*stride_y + dy - pad_top;
                                int xi = x0*stride_x + dx - pad_left;

                                if ((unsigned)yi < (unsigned)Hi &&
                                    (unsigned)xi < (unsigned)Wi) {
                                    const float* inptr_ki = (float*)inptr + yi*Wi + xi;
                                    if (i + 4 <= CONV_NR && x0 + 4 <= W0 && xi + stride_x*4 <= Wi) {
                                        if (stride_x == 2) {
                                            for (int c = 0; c < Cg; c++, inpbuf_ki += CONV_NR*ksize, inptr_ki += inp_planesize) {
                                                float t0 = inptr_ki[0], t1 = inptr_ki[2];
                                                float t2 = inptr_ki[4], t3 = inptr_ki[6];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                            }
                                        } else {
                                            for (int c = 0; c < Cg; c++, inpbuf_ki += CONV_NR*ksize, inptr_ki += inp_planesize) {
                                                float t0 = inptr_ki[0], t1 = inptr_ki[stride_x];
                                                float t2 = inptr_ki[stride_x*2], t3 = inptr_ki[stride_x*3];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                            }
                                        }
                                        i += 4;
                                        x0 += 4;
                                    } else {
                                        for (int c = 0; c < Cg; c++, inpbuf_ki += CONV_NR*ksize, inptr_ki += inp_planesize)
                                            *inpbuf_ki = *inptr_ki;
                                        i++;
                                        x0++;
                                    }
                                } else {
                                    for (int c = 0; c < Cg; c++, inpbuf_ki += CONV_NR*ksize)
                                        inpbuf_ki[0] = 0.f;
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
                            for (; i < CONV_NR;) {
                                int16_t* inpbuf_ki = (int16_t*)inpbuf_task + k*CONV_NR + i;
                                int yi = y0*stride_y + dy - pad_top;
                                int xi = x0*stride_x + dx - pad_left;

                                if ((unsigned)yi < (unsigned)Hi &&
                                    (unsigned)xi < (unsigned)Wi) {
                                    const int16_t* inptr_ki = (int16_t*)inptr + yi*Wi + xi;
                                    if (i + 4 <= CONV_NR && x0 + 4 <= W0 && xi + stride_x*4 <= Wi) {
                                        if (stride_x == 2) {
                                            for (int c = 0; c < Cg; c++, inpbuf_ki += CONV_NR*ksize, inptr_ki += inp_planesize) {
                                                int16_t t0 = inptr_ki[0], t1 = inptr_ki[2];
                                                int16_t t2 = inptr_ki[4], t3 = inptr_ki[6];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                            }
                                        } else {
                                            for (int c = 0; c < Cg; c++, inpbuf_ki += CONV_NR*ksize, inptr_ki += inp_planesize) {
                                                int16_t t0 = inptr_ki[0], t1 = inptr_ki[stride_x];
                                                int16_t t2 = inptr_ki[stride_x*2], t3 = inptr_ki[stride_x*3];
                                                inpbuf_ki[0] = t0; inpbuf_ki[1] = t1;
                                                inpbuf_ki[2] = t2; inpbuf_ki[3] = t3;
                                            }
                                        }
                                        i += 4;
                                        x0 += 4;
                                    } else {
                                        for (int c = 0; c < Cg; c++, inpbuf_ki += CONV_NR*ksize, inptr_ki += inp_planesize)
                                            *inpbuf_ki = *inptr_ki;
                                        i++;
                                        x0++;
                                    }
                                } else {
                                    for (int c = 0; c < Cg; c++, inpbuf_ki += CONV_NR*ksize)
                                        inpbuf_ki[0] = 0;
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

                // 2. do convolution, compute Kg x (yx1 - yx0) part of the output tensor
                {
                int outstep0 = out_planesize;
                size_t outofs = ((n*ngroups + g)*Kg + k0)*outstep0 + yx0;
                float* outptr0 = (float*)out_data->data + outofs;
                const float* pbptr0 = pb_data->data ? (const float*)pb_data->data + outofs : 0;
                float cbuf[CONV_MR*CONV_NR], pbbuf[CONV_MR*CONV_NR];
                memset(pbbuf, 0, sizeof(pbbuf));
                for(int k = k0; k < k1; k += CONV_MR, outptr0 += outstep0*CONV_MR,
                                    pbptr0 += (pbptr0 ? outstep0*CONV_MR : 0)) {
                    int dk = Kg - k < CONV_MR ? Kg - k : CONV_MR;
                    bool partial = partial0 || dk < CONV_MR || activ_func;
                    float* outptr = outptr0;
                    float* pbptr = (float*)pbptr0;
                    int outstep = outstep0;
                    if (partial) {
                        outptr = cbuf;
                        outstep = CONV_NR;
                        if (pbptr) {
                            pbptr = pbbuf;
                            for (int k1 = 0; k1 < dk; k1++)
                                memcpy(&pbbuf[k1*CONV_NR], pbptr0 + k1*outstep0,
                                    slice_len*sizeof(pbbuf[0]));
                        }
                    }
                    conv_block_func(HkWkCg, conv->weights+(g*Kg_aligned + k)*HkWkCg,
                                    inpbuf_task, outptr, outstep, pbptr, outstep,
                                    conv->bias + Kg*g + k, minval, maxval, fast_activ);
                    if (partial) {
                        if (activ_func)
                            activ_func(outptr, outstep, 1, dk*CONV_NR, activ_params);
                        for (int i = 0; i < dk; i++)
                            memcpy(outptr0 + i*outstep0, &cbuf[i*CONV_NR],
                                   (yx1 - yx0)*sizeof(cbuf[0]));
                    }
                }
                }
            }
        }
    }

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

    return _fx_init_conv2d(_FX_LAYOUT_NCHW, _FX_LAYOUT_NCHW, (int)group,
        (int)w_shape_[0], (int)w_shape_[1]*group, (int)w_shape_[2], (int)w_shape_[3],
        (int)strides_[0], (int)strides_[1], (int)dilations_[0], (int)dilations_[1],
        (int)pads_[0], (int)pads_[1], (int)pads_[2], (int)pads_[3],
        (const float*)w_data->data, (const float*)bias_data->data,
        bn_data_[0], bn_data_[1], bn_data_[2], bn_data_[3], bn_eps,
        (int)activ_func->tag, 0, (const float*)activ_params->data,
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
