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

#ifdef __ARM_NEON
#include <arm_neon.h>
#endif

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

typedef void (*_fx_activ_func_t)(float* data, size_t step, int size0,
                                 int size1, const float* params);

#ifdef __ARM_NEON
#if 0
#define FX_CONV_MR 8
#define FX_CONV_NR 12
#else
#define FX_CONV_MR 4
#define FX_CONV_NR 28
#endif
enum { FX_VEC_NLANES=4 };
#elif defined __AVX__
#define FX_CONV_MR 4
#define FX_CONV_NR 24
enum { FX_VEC_NLANES=8 };
#else
enum { FX_VEC_NLANES=1 };
#endif

static void _fx_activ_lrelu(float* data, size_t step, int size0,
                            int size1, const float* params)
{
    float alpha = params[0];
    step /= sizeof(data[0]);
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
    step /= sizeof(data[0]);
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
    step /= sizeof(data[0]);
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
    step /= sizeof(data[0]);
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j++) {
            data[j] = tanh(data[j]);
        }
    }
}

static void _fx_activ_mish(float* data, size_t step, int size0,
                           int size1, const float* params)
{
    step /= sizeof(data[0]);
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j++) {
            float x = data[j];
            x = x > -36.73f ? x : -0.f;
            float y = expf(-x);
            data[j] = x*(1 + 2*y)/(1 + 2*y + 2*y*y);
        }
    }
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
    float* bias;
    int activ;
    float* activ_params;
    int nactive_params;
    _fx_activ_func_t activ_func;
    float minval, maxval;
    int prev_Hi, prev_Wi, prev_H0, prev_W0;
    int* ofstab;

} _fx_conv2d_t;

static void _fx_free_conv2d(void* conv_ptr)
{
    _fx_conv2d_t* conv = (_fx_conv2d_t*)conv_ptr;
    if(conv) {
        fx_free(conv->weights);
        fx_free(conv->bias);
        fx_free(conv->activ_params);
        fx_free(conv->ofstab);
        fx_free(conv);
    }
}

static void _fx_update_ofstab(_fx_conv2d_t* conv, int Hi, int Wi, int H0, int W0)
{
    int k, Hk = conv->Hk, Wk = conv->Wk, ksize = Hk*Wk;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int pad_top = conv->pad_top, pad_left = conv->pad_left;
    int noutliers = 0;
    int* yxtab = (int*)alloca(ksize*2*sizeof(yxtab[0]));
    int outlier_ofs = H0*W0;
    int* ofstab;

    if (Hi == conv->prev_Hi && Wi == conv->prev_Wi &&
        H0 == conv->prev_H0 && W0 == conv->prev_W0)
        return;

    fx_free(conv->ofstab);

    for (int dy = 0; dy < Hk; dy++)
        for(int dx = 0; dx < Wk; dx++, k++) {
            yxtab[k*2] = dy*dilation_y;
            yxtab[k*2+1] = dx*dilation_x;
        }

    for (int y0 = 0; y0 < H0; y0++) {
        int yi_ = y0*stride_y - pad_top;
        for (int x0 = 0; x0 < W0; x0++) {
            int xi_ = x0*stride_x - pad_left;
            noutliers += (yi_ < 0) | (yi_ + (Hk-1)*dilation_y >= Hi) |
                         (xi_ < 0) | (xi_ + (Wk-1)*dilation_x >= Wi);
        }
    }
    conv->ofstab = (int*)fx_malloc((H0*W0 + noutliers*ksize)*sizeof(conv->ofstab[0]));
    ofstab = conv->ofstab;

    for (int y0 = 0; y0 < H0; y0++) {
        int yi_ = y0*stride_y - pad_top;
        for (int x0 = 0; x0 < W0; x0++) {
            int xi_ = x0*stride_x - pad_left;
            if ((yi_ < 0) | (yi_ + (Hk-1)*dilation_y >= Hi) |
                (xi_ < 0) | (xi_ + (Wk-1)*dilation_x >= Wi)) {
                for (k = 0; k < ksize; k++) {
                    int yi = yi_ + yxtab[k*2], xi = xi_ + yxtab[k*2+1];
                    int ofs = yi*Wi + xi;
                    ofstab[outlier_ofs + k] = (((unsigned)yi < (unsigned)Hi) &
                                               ((unsigned)xi < (unsigned)Wi)) ? ofs : -1;
                }
                ofstab[y0*W0 + x0] = -outlier_ofs;
                outlier_ofs += ksize;
            } else {
                ofstab[y0*W0 + x0] = yi_*Wi + xi_;
            }
        }
    }
    conv->prev_H0 = H0;
    conv->prev_W0 = W0;
    conv->prev_Hi = Hi;
    conv->prev_Wi = Wi;
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
    int k, nbias = K + FX_CONV_MR-1;
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
        activ == _FX_ACTIV_RELU || activ == _FX_ACTIV_CLIP ? 0 :
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
    } else if (activ == _FX_ACTIV_CLIP) {
        assert(activ_params && activ_nparams == 2);
        conv->minval = activ_params[0];
        conv->maxval = activ_params[1];
    } else if (activ_params) {
        assert(activ_nparams > 0);
        conv->activ_params = (float*)fx_malloc(activ_nparams*sizeof(activ_params[0]));
        if (!conv->activ_params)
            return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
        memcpy(conv->activ_params, activ_params, activ_nparams*sizeof(activ_params[0]));
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
        // the weights are packed as
        // ngroups x (ceil((K/ngroups)/FX_CONV_MR)*FX_CONV_MR) x (Cg*Hk*Wk) x FX_CONV_MR tensor
        int Kg = K/ngroups, Cg = C/ngroups;
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
            assert(packed_wptr - conv->weights <= nweights);
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

static void _fx_conv_block( int k, const float *a, const float *b,
                            float *c, int ldc, const float* pb, const float* bias,
                            float minval, float maxval, bool activ)
{
#ifdef __ARM_NEON
#if FX_CONV_MR == 8 && FX_CONV_NR == 12
    float32x4_t c0 = vdupq_n_f32(bias[0]), c1 = c0, c2 = c0;
    float32x4_t c3 = vdupq_n_f32(bias[1]), c4 = c3, c5 = c3;
    float32x4_t c6 = vdupq_n_f32(bias[2]), c7 = c6, c8 = c6;
    float32x4_t c9 = vdupq_n_f32(bias[3]), c10 = c9, c11 = c9;
    float32x4_t c12 = vdupq_n_f32(bias[4]), c13 = c12, c14 = c12;
    float32x4_t c15 = vdupq_n_f32(bias[5]), c16 = c15, c17 = c15;
    float32x4_t c18 = vdupq_n_f32(bias[6]), c19 = c18, c20 = c18;
    float32x4_t c21 = vdupq_n_f32(bias[7]), c22 = c21, c23 = c21;

    if (pb) {
        c0 = vaddq_f32(c0, vld1q_f32(pb)); c1 = vaddq_f32(c1, vld1q_f32(pb + 4)); c2 = vaddq_f32(c2, vld1q_f32(pb + 8));
        c3 = vaddq_f32(c3, vld1q_f32(pb + ldc)); c4 = vaddq_f32(c4, vld1q_f32(pb + ldc + 4)); c5 = vaddq_f32(c5, vld1q_f32(pb + ldc + 8));
        c6 = vaddq_f32(c6, vld1q_f32(pb + ldc*2)); c7 = vaddq_f32(c7, vld1q_f32(pb + ldc*2 + 4)); c8 = vaddq_f32(c8, vld1q_f32(pb + ldc*2 + 8));
        c9 = vaddq_f32(c9, vld1q_f32(pb + ldc*3)); c10 = vaddq_f32(c10, vld1q_f32(pb + ldc*3 + 4)); c11 = vaddq_f32(c11, vld1q_f32(pb + ldc*3 + 8));
        c12 = vaddq_f32(c12, vld1q_f32(pb + ldc*4)); c13 = vaddq_f32(c13, vld1q_f32(pb + ldc*4 + 4)); c14 = vaddq_f32(c14, vld1q_f32(pb + ldc*4 + 8));
        c15 = vaddq_f32(c15, vld1q_f32(pb + ldc*5)); c16 = vaddq_f32(c16, vld1q_f32(pb + ldc*5 + 4)); c17 = vaddq_f32(c17, vld1q_f32(pb + ldc*5 + 8));
        c18 = vaddq_f32(c18, vld1q_f32(pb + ldc*6)); c19 = vaddq_f32(c19, vld1q_f32(pb + ldc*6 + 4)); c20 = vaddq_f32(c20, vld1q_f32(pb + ldc*6 + 8));
        c21 = vaddq_f32(c21, vld1q_f32(pb + ldc*7)); c22 = vaddq_f32(c22, vld1q_f32(pb + ldc*7 + 4)); c23 = vaddq_f32(c23, vld1q_f32(pb + ldc*7 + 8));
    }

    for( int p = 0; p < k; p++, a += FX_CONV_MR, b += FX_CONV_NR )
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
    if (activ) {
        float32x4_t vmin = vdupq_n_f32(minval), vmax = vdupq_n_f32(maxval);
        c0 = vminq_f32(vmaxq_f32(c0, vmin), vmax);
        c1 = vminq_f32(vmaxq_f32(c1, vmin), vmax);
        c2 = vminq_f32(vmaxq_f32(c2, vmin), vmax);
        c3 = vminq_f32(vmaxq_f32(c3, vmin), vmax);
        c4 = vminq_f32(vmaxq_f32(c4, vmin), vmax);
        c5 = vminq_f32(vmaxq_f32(c5, vmin), vmax);
        c6 = vminq_f32(vmaxq_f32(c6, vmin), vmax);
        c7 = vminq_f32(vmaxq_f32(c7, vmin), vmax);
        c8 = vminq_f32(vmaxq_f32(c8, vmin), vmax);
        c9 = vminq_f32(vmaxq_f32(c9, vmin), vmax);
        c10 = vminq_f32(vmaxq_f32(c10, vmin), vmax);
        c11 = vminq_f32(vmaxq_f32(c11, vmin), vmax);
        c12 = vminq_f32(vmaxq_f32(c12, vmin), vmax);
        c13 = vminq_f32(vmaxq_f32(c13, vmin), vmax);
        c14 = vminq_f32(vmaxq_f32(c14, vmin), vmax);
        c15 = vminq_f32(vmaxq_f32(c15, vmin), vmax);
        c16 = vminq_f32(vmaxq_f32(c16, vmin), vmax);
        c17 = vminq_f32(vmaxq_f32(c17, vmin), vmax);
        c18 = vminq_f32(vmaxq_f32(c18, vmin), vmax);
        c19 = vminq_f32(vmaxq_f32(c19, vmin), vmax);
        c20 = vminq_f32(vmaxq_f32(c20, vmin), vmax);
        c21 = vminq_f32(vmaxq_f32(c21, vmin), vmax);
        c22 = vminq_f32(vmaxq_f32(c22, vmin), vmax);
        c23 = vminq_f32(vmaxq_f32(c23, vmin), vmax);
    }
    vst1q_f32(c, c0); vst1q_f32(c+4, c1); vst1q_f32(c+8, c2);
    vst1q_f32(c + ldc, c3); vst1q_f32(c + ldc + 4, c4); vst1q_f32(c + ldc + 8, c5);
    vst1q_f32(c + ldc*2, c6); vst1q_f32(c + ldc*2 + 4, c7); vst1q_f32(c + ldc*2 + 8, c8);
    vst1q_f32(c + ldc*3, c9); vst1q_f32(c + ldc*3 + 4, c10); vst1q_f32(c + ldc*3 + 8, c11);
    vst1q_f32(c + ldc*4, c12); vst1q_f32(c + ldc*4 + 4, c13); vst1q_f32(c + ldc*4 + 8, c14);
    vst1q_f32(c + ldc*5, c15); vst1q_f32(c + ldc*5 + 4, c16); vst1q_f32(c + ldc*5 + 8, c17);
    vst1q_f32(c + ldc*6, c18); vst1q_f32(c + ldc*6 + 4, c19); vst1q_f32(c + ldc*6 + 8, c20);
    vst1q_f32(c + ldc*7, c21); vst1q_f32(c + ldc*7 + 4, c22); vst1q_f32(c + ldc*7 + 8, c23);
#elif FX_CONV_MR == 4 && FX_CONV_NR == 28
    float32x4_t c00 = vdupq_n_f32(bias[0]), c01 = c00, c02 = c00, c03 = c00, c04 = c00, c05 = c00, c06 = c00;
    float32x4_t c10 = vdupq_n_f32(bias[1]), c11 = c10, c12 = c10, c13 = c10, c14 = c10, c15 = c10, c16 = c10;
    float32x4_t c20 = vdupq_n_f32(bias[2]), c21 = c20, c22 = c20, c23 = c20, c24 = c20, c25 = c20, c26 = c20;
    float32x4_t c30 = vdupq_n_f32(bias[3]), c31 = c30, c32 = c30, c33 = c30, c34 = c30, c35 = c30, c36 = c30;

    if (pb) {
        c00 = vaddq_f32(c00, vld1q_f32(pb));
        c01 = vaddq_f32(c01, vld1q_f32(pb + 4));
        c02 = vaddq_f32(c02, vld1q_f32(pb + 8));
        c03 = vaddq_f32(c03, vld1q_f32(pb + 12));
        c04 = vaddq_f32(c04, vld1q_f32(pb + 16));
        c05 = vaddq_f32(c05, vld1q_f32(pb + 20));
        c06 = vaddq_f32(c06, vld1q_f32(pb + 24));

        c10 = vaddq_f32(c10, vld1q_f32(pb + ldc));
        c11 = vaddq_f32(c11, vld1q_f32(pb + ldc + 4));
        c12 = vaddq_f32(c12, vld1q_f32(pb + ldc + 8));
        c13 = vaddq_f32(c13, vld1q_f32(pb + ldc + 12));
        c14 = vaddq_f32(c14, vld1q_f32(pb + ldc + 16));
        c15 = vaddq_f32(c15, vld1q_f32(pb + ldc + 20));
        c16 = vaddq_f32(c16, vld1q_f32(pb + ldc + 24));

        c20 = vaddq_f32(c20, vld1q_f32(pb + ldc*2));
        c21 = vaddq_f32(c21, vld1q_f32(pb + ldc*2 + 4));
        c22 = vaddq_f32(c22, vld1q_f32(pb + ldc*2 + 8));
        c23 = vaddq_f32(c23, vld1q_f32(pb + ldc*2 + 12));
        c24 = vaddq_f32(c24, vld1q_f32(pb + ldc*2 + 16));
        c25 = vaddq_f32(c25, vld1q_f32(pb + ldc*2 + 20));
        c26 = vaddq_f32(c26, vld1q_f32(pb + ldc*2 + 24));

        c30 = vaddq_f32(c30, vld1q_f32(pb + ldc*3));
        c31 = vaddq_f32(c31, vld1q_f32(pb + ldc*3 + 4));
        c32 = vaddq_f32(c32, vld1q_f32(pb + ldc*3 + 8));
        c33 = vaddq_f32(c33, vld1q_f32(pb + ldc*3 + 12));
        c34 = vaddq_f32(c34, vld1q_f32(pb + ldc*3 + 16));
        c35 = vaddq_f32(c35, vld1q_f32(pb + ldc*3 + 20));
        c36 = vaddq_f32(c36, vld1q_f32(pb + ldc*3 + 24));
    }

    for( int p = 0; p < k; p++, a += FX_CONV_MR, b += FX_CONV_NR )
    {
        float32x4_t a0 = vld1q_f32(a), b0, b1, b2;
        b0 = vld1q_f32(b); b1 = vld1q_f32(b + 4); b2 = vld1q_f32(b + 8);

        c00 = vfmaq_laneq_f32(c00, b0, a0, 0);
        c01 = vfmaq_laneq_f32(c01, b1, a0, 0);
        c02 = vfmaq_laneq_f32(c02, b2, a0, 0);
        c10 = vfmaq_laneq_f32(c10, b0, a0, 1);
        c11 = vfmaq_laneq_f32(c11, b1, a0, 1);
        c12 = vfmaq_laneq_f32(c12, b2, a0, 1);
        c20 = vfmaq_laneq_f32(c20, b0, a0, 2);
        c21 = vfmaq_laneq_f32(c21, b1, a0, 2);
        c22 = vfmaq_laneq_f32(c22, b2, a0, 2);
        c30 = vfmaq_laneq_f32(c30, b0, a0, 3);
        c31 = vfmaq_laneq_f32(c31, b1, a0, 3);
        c32 = vfmaq_laneq_f32(c32, b2, a0, 3);

        b0 = vld1q_f32(b + 12); b1 = vld1q_f32(b + 16); b2 = vld1q_f32(b + 20);

        c03 = vfmaq_laneq_f32(c03, b0, a0, 0);
        c04 = vfmaq_laneq_f32(c04, b1, a0, 0);
        c05 = vfmaq_laneq_f32(c05, b2, a0, 0);
        c13 = vfmaq_laneq_f32(c13, b0, a0, 1);
        c14 = vfmaq_laneq_f32(c14, b1, a0, 1);
        c15 = vfmaq_laneq_f32(c15, b2, a0, 1);
        c23 = vfmaq_laneq_f32(c23, b0, a0, 2);
        c24 = vfmaq_laneq_f32(c24, b1, a0, 2);
        c25 = vfmaq_laneq_f32(c25, b2, a0, 2);
        c33 = vfmaq_laneq_f32(c33, b0, a0, 3);
        c34 = vfmaq_laneq_f32(c34, b1, a0, 3);
        c35 = vfmaq_laneq_f32(c35, b2, a0, 3);

        b0 = vld1q_f32(b + 24);
        c06 = vfmaq_laneq_f32(c06, b0, a0, 0);
        c16 = vfmaq_laneq_f32(c16, b0, a0, 1);
        c26 = vfmaq_laneq_f32(c26, b0, a0, 2);
        c36 = vfmaq_laneq_f32(c36, b0, a0, 3);
    }
    if (activ) {
        float32x4_t vmin = vdupq_n_f32(minval), vmax = vdupq_n_f32(maxval);
        c00 = vminq_f32(vmaxq_f32(c00, vmin), vmax);
        c01 = vminq_f32(vmaxq_f32(c01, vmin), vmax);
        c02 = vminq_f32(vmaxq_f32(c02, vmin), vmax);
        c03 = vminq_f32(vmaxq_f32(c03, vmin), vmax);
        c04 = vminq_f32(vmaxq_f32(c04, vmin), vmax);
        c05 = vminq_f32(vmaxq_f32(c05, vmin), vmax);
        c06 = vminq_f32(vmaxq_f32(c06, vmin), vmax);

        c10 = vminq_f32(vmaxq_f32(c10, vmin), vmax);
        c11 = vminq_f32(vmaxq_f32(c11, vmin), vmax);
        c12 = vminq_f32(vmaxq_f32(c12, vmin), vmax);
        c13 = vminq_f32(vmaxq_f32(c13, vmin), vmax);
        c14 = vminq_f32(vmaxq_f32(c14, vmin), vmax);
        c15 = vminq_f32(vmaxq_f32(c15, vmin), vmax);
        c16 = vminq_f32(vmaxq_f32(c16, vmin), vmax);

        c20 = vminq_f32(vmaxq_f32(c20, vmin), vmax);
        c21 = vminq_f32(vmaxq_f32(c21, vmin), vmax);
        c22 = vminq_f32(vmaxq_f32(c22, vmin), vmax);
        c23 = vminq_f32(vmaxq_f32(c23, vmin), vmax);
        c24 = vminq_f32(vmaxq_f32(c24, vmin), vmax);
        c25 = vminq_f32(vmaxq_f32(c25, vmin), vmax);
        c26 = vminq_f32(vmaxq_f32(c26, vmin), vmax);

        c30 = vminq_f32(vmaxq_f32(c30, vmin), vmax);
        c31 = vminq_f32(vmaxq_f32(c31, vmin), vmax);
        c32 = vminq_f32(vmaxq_f32(c32, vmin), vmax);
        c33 = vminq_f32(vmaxq_f32(c33, vmin), vmax);
        c34 = vminq_f32(vmaxq_f32(c34, vmin), vmax);
        c35 = vminq_f32(vmaxq_f32(c35, vmin), vmax);
        c36 = vminq_f32(vmaxq_f32(c36, vmin), vmax);
    }
    vst1q_f32(c, c00); vst1q_f32(c+4, c01);
    vst1q_f32(c+8, c02); vst1q_f32(c+12, c03);
    vst1q_f32(c+16, c04); vst1q_f32(c+20, c05);
    vst1q_f32(c+24, c06);

    vst1q_f32(c+ldc, c10); vst1q_f32(c+ldc+4, c11);
    vst1q_f32(c+ldc+8, c12); vst1q_f32(c+ldc+12, c13);
    vst1q_f32(c+ldc+16, c14); vst1q_f32(c+ldc+20, c15);
    vst1q_f32(c+ldc+24, c16);

    vst1q_f32(c+ldc*2, c20); vst1q_f32(c+ldc*2+4, c21);
    vst1q_f32(c+ldc*2+8, c22); vst1q_f32(c+ldc*2+12, c23);
    vst1q_f32(c+ldc*2+16, c24); vst1q_f32(c+ldc*2+20, c25);
    vst1q_f32(c+ldc*2+24, c26);

    vst1q_f32(c+ldc*3, c30); vst1q_f32(c+ldc*3+4, c31);
    vst1q_f32(c+ldc*3+8, c32); vst1q_f32(c+ldc*3+12, c33);
    vst1q_f32(c+ldc*3+16, c34); vst1q_f32(c+ldc*3+20, c35);
    vst1q_f32(c+ldc*3+24, c36);
#else
#error "unsupported FX_CONV_MR and FX_CONV_NR"
#endif
#else
    for( int i = 0; i < FX_CONV_MR; i++ )
    {
        float beta = bias[i];
        for( int j = 0; j < FX_CONV_NR; j++ )
            c[i*ldc + j] = beta;
    }
    for( int p = 0; p < k; p++ )
    {
        int ch = p/9, yx = p - ch*9;
        int ky = yx/3, kx = yx % 3;

        for( int i = 0; i < FX_CONV_MR; i++ )
        {
            float alpha = a[FX_CONV_MR*p + i];
            for( int j = 0; j < FX_CONV_NR; j++ )
            {
                //if(call == 1 && kx == 1 && ky == 1 && i == 0 && j == 0)
                //    printf("c == %d: inpval = %.2f, w = %.2f\n", ch, b[FX_CONV_NR*p+j], alpha);
                c[i*ldc+j] += b[FX_CONV_NR*p + j]*alpha;
            }
        }
    }
    if (activ) {
        for( int i = 0; i < FX_CONV_MR; i++ )
        {
            for( int j = 0; j < FX_CONV_NR; j++ ) {
                float v = c[i*ldc + j];
                v = _fx_minf(_fx_maxf(v, minval), maxval);
                c[i*ldc + j] = v;
            }
        }
    }
#endif
}

static int _fx_depthwise_conv2d(int ndims, const int_* inpsize, const float* inp,
                                 const int_* outsize, float* out,
                                 const _fx_conv2d_t* conv)
{
    assert(ndims == 4 && inpsize[0] == outsize[0] && outsize[1] == conv->K && inpsize[1] == conv->C);
    assert(conv->ngroups == conv->K && conv->K == conv->C);
    int N = (int)inpsize[0], C = (int)inpsize[1], Hi = (int)inpsize[2], Wi = (int)inpsize[3];
    int Hk = conv->Hk, Wk = conv->Wk;
    int H0 = (int)outsize[2], W0 = (int)outsize[3];
    const size_t inp_planesize = (size_t)Hi*Wi;
    const size_t out_planesize = (size_t)H0*W0;
    float minval = conv->minval, maxval = conv->maxval;
    bool fast_activ = conv->activ != _FX_ACTIV_NONE && conv->activ_func == 0;
    _fx_activ_func_t activ_func = !fast_activ ? conv->activ_func : 0;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    int ksize = Hk*Wk, padded_ksize = ((ksize + FX_VEC_NLANES-1)/FX_VEC_NLANES)*FX_VEC_NLANES;

    int* ofstab = (int*)fx_malloc(3*padded_ksize*sizeof(ofstab[0]));
    int* yxtab = ofstab + padded_ksize;
    const float* weights0 = conv->weights, *bias = conv->bias;
    int inner_ytop = (pad_top + stride_y-1)/stride_y, inner_ybottom;
    int inner_xleft = (pad_left + stride_x-1)/stride_x, inner_xright;
    for (int k = 0; k < padded_ksize; k++) {
        int y = k < ksize ? k / Wk : 0;
        int x = k < ksize ? k % Wk : 0;
        int dy = y*dilation_y, dx = x*dilation_x;
        yxtab[k*2] = dy; yxtab[k*2+1] = dx;
        ofstab[k] = dy*Wi + dx;
    }
    if ((Hk|Wk) == 1) {
        assert(pad_left == 0 && pad_right == 0 && pad_top == 0 && pad_bottom == 0);
    }
    inner_xright = (Wi - (Wk - 1)*dilation_x + pad_left)/stride_x;
    inner_xright += inner_xright*stride_x - pad_left + (Wk-1)*dilation_x < Wi;
    inner_ybottom = (Hi - (Hk - 1)*dilation_y + pad_top)/stride_y;
    inner_ybottom += inner_ybottom*stride_y - pad_top + (Hk-1)*dilation_y < Hi;
    if (inner_xleft >= inner_xright || inner_ytop >= inner_ybottom) {
        inner_xleft = W0;
        inner_ytop = H0;
    }

#ifdef __ARM_NEON
    float32x4_t vminval = vdupq_n_f32(minval), vmaxval = vdupq_n_f32(maxval);
    bool useSIMD = stride_x == 1 && inner_xleft < W0;
    bool is3x3 = Hk == 3 && Wk == 3;
#endif

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for
    for (int nc = 0; nc < N*C; nc++) {
        int c = nc % C;
        const float* inptr = inp + inp_planesize*nc;
        float* outptr = out + out_planesize*nc;
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

        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_ytop && y0 < inner_ybottom ? inner_xleft : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s = biasval;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        s += inptr[yi*Wi + xi]*weights[k];
                    }
                    s = fx_minf(fx_maxf(s, minval), maxval);
                    outptr[x0] = s;
                }
                if (x0 == W0)
                    break;
                x1 = inner_xright;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
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
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const float* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = biasval;
                    for (int k = 0; k < ksize; k++)
                        s += inptr_xi[ofstab[k]]*weights[k];
                    s = fx_minf(fx_maxf(s, minval), maxval);
                    outptr[x0] = s;
                }
                x1 = W0;
            }
        }
        if (activ_func) {
            activ_func(outptr, 0, 1, W0*H0, conv->activ_params);
        }
    }

    fx_free(ofstab);
    return FX_OK;
}

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
    printf("Winograd: N=%d, K=%d, C=%d, Hi=%d, Wi=%d: time=%.1f\n", N, K, C, Hi, Wi, ts*1000./fx_tick_frequency());
    }
    return FX_OK;
}
#endif

#if 0
static int _fx_conv2d(int ndims, const int_* inpsize, const float* inp,
                       const int_* outsize, float* out,
                       const float* passby, const _fx_conv2d_t* conv, int ntasks)
{
    assert(ndims == 4 &&
           inpsize[0] == outsize[0] &&
           outsize[1] == conv->K &&
           inpsize[1] == conv->C);
    if (conv->conv_type == _FX_CONV_TYPE_DEPTHWISE) {
        return _fx_depthwise_conv2d(ndims, inpsize, inp, outsize, out, conv);
    }
#ifdef __ARM_NEON
    else if (conv->conv_type == _FX_CONV_TYPE_WINOGRAD3X3) {
        return _fx_winograd_conv2d(ndims, inpsize, inp, outsize, out, passby, conv, ntasks);
    }
#endif
    int N = (int)inpsize[0], C = (int)inpsize[1], Hi = (int)inpsize[2], Wi = (int)inpsize[3];
    int K = conv->K, Hk = conv->Hk, Wk = conv->Wk, ngroups = conv->ngroups;
    int H0 = (int)outsize[2], W0 = (int)outsize[3];
    int Cg = C/ngroups, Kg = K/ngroups, Kg_nblocks = (Kg + FX_CONV_MR-1)/FX_CONV_MR;
    int inp_planesize = Hi*Wi;
    int out_planesize = H0*W0;
    float minval = conv->minval, maxval = conv->maxval;
    bool fast_activ = conv->activ != _FX_ACTIV_NONE && conv->activ_func == 0;
    _fx_activ_func_t activ_func = !fast_activ ? conv->activ_func : 0;
    const float* activ_params = conv->activ_params;
    int stripes_per_sample = (H0*W0+FX_CONV_NR-1)/FX_CONV_NR;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int nsubtasks = N*ngroups*Kg_nblocks;
    //bool is_winograd3x3 = Hk == 3 && Wk == 3 && dilation_y == 1 && dilation_x == 1 && stride_y == 1 && stride_x == 1;
    int64_t ts = fx_tick_count();
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    bool fast_1x1 = stride_x == 1 && stride_y == 1 && (Wk|Hk) == 1;
    /*if (H0 == Hi && W0 == Wi && stride_y == 1 && stride_x == 1 && dilation_y == 1 && dilation_x == 1) {
        Hk = Wk = 1;
        pad_top = pad_bottom = pad_left = pad_right = 0;
    }*/
    int HkWkCg = Hk*Wk*Cg, HkWkC = HkWkCg*ngroups;
    size_t taskbufsize = FX_CONV_NR*Hk*Wk*Cg;
    float* inpbuf_all = (float*)fx_malloc(ntasks*taskbufsize*sizeof(inpbuf_all[0]) + Hk*Wk*3*sizeof(int));
    int* ofstab = (int*)(inpbuf_all + ntasks*taskbufsize);
    int* yxtab = ofstab + Hk*Wk;
    for (int y = 0; y < Hk; y++)
        for( int x = 0; x < Wk; x++) {
            int k = y*Wk + x;
            int dy = y*dilation_y, dx = x*dilation_x;
            yxtab[k*2] = dy; yxtab[k*2+1] = dx;
            ofstab[k] = dy*Wi + dx;
        }
    if ((Hk|Wk) == 1) {
        assert(pad_left == 0 && pad_right == 0 && pad_top == 0 && pad_bottom == 0);
        assert(stride_x != 1 || stride_y != 1 || (H0 == Hi && W0 == Wi));
    }

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        float* inpbuf_task = &inpbuf_all[taskbufsize*task_id];
        int ngs0 = (int)((size_t)nsubtasks*task_id/ntasks), ngs1 = (int)((size_t)nsubtasks*(task_id+1)/ntasks);
        //printf("task id=%d: ngs0=%d, ngs1 = %d\n", task_id, ngs0, ngs1);
        for(int ngs = ngs0; ngs < ngs1; ngs++) {
            int n = ngs/(ngroups*stripes_per_sample), gs = ngs - n*(ngroups*stripes_per_sample);
            int g = gs/stripes_per_sample, yx0 = (gs - g*stripes_per_sample)*FX_CONV_NR;
            int yx1 = yx0 + FX_CONV_NR; yx1 = yx1 < H0*W0 ? yx1 : H0*W0;
            int y0 = yx0/W0, x0 = yx0 - y0*W0;
            size_t inp_plane_ofs = (n*ngroups + g)*Cg*Hi*Wi;
            int yi_ = y0*stride_y - pad_top;
            int xi_ = x0*stride_x - pad_left;
            if (yx1 < yx0 + FX_CONV_NR)
                memset(inpbuf_task, 0, taskbufsize*sizeof(inpbuf_task[0]));
            // 1. pack input data
            if (yx1 == yx0 + FX_CONV_NR &&
                (fast_1x1 ||
                (stride_x == 1 &&
                0 <= yi_ && yi_ + (Hk-1)*dilation_y < Hi &&
                0 <= xi_ && xi_ + FX_CONV_NR-1 + (Wk-1)*dilation_x < Wi))) {
                // A. almost general case (stride_x == 1) when the whole slice
                //    (yx0 <= yx < yx0 + FX_CONV_NR) is inside one row of the input tensor, i.e.
                //    (n=n, c=c, y=y0, x0 <= x < x0+FX_CONV_NR).
                //    in this case we pack data for FX_CONV_NR output elements at once
                const float* inptr = inp + inp_plane_ofs + yi_*Wi + xi_;
                float* inpbuf = inpbuf_task;
                if ((Hk|Wk) == 1) {
                    // A1. special optimization for 1x1 kernel
                    for (int c = 0; c < Cg; c++, inptr += Hi*Wi, inpbuf += FX_CONV_NR) {
                        memcpy(inpbuf, inptr, FX_CONV_NR*sizeof(inpbuf[0]));
                    }
                } else {
                    for (int c = 0; c < Cg; c++, inptr += Hi*Wi) {
                        for (int k = 0; k < Hk*Wk; k++, inpbuf += FX_CONV_NR) {
                            const float* inptr_k = inptr + ofstab[k];
                            memcpy(inpbuf, inptr_k, FX_CONV_NR*sizeof(inpbuf[0]));
                        }
                    }
                }
            } else if ((Hk|Wk) == 1) {
                // B. 1x1 case, if it's not classified as A.
                //    in this case the input slice is always inside input tensor,
                //    but it may cross an input tensor row.
                for (int yx = yx0; yx < yx1; yx++) {
                    float* inpbuf = inpbuf_task + (yx - yx0);
                    yi_ = y0*stride_y;
                    xi_ = x0*stride_x;
                    const float* inptr = inp + inp_plane_ofs + yi_*Wi + xi_;
                    for (int c = 0; c < Cg; c++, inptr += Hi*Wi, inpbuf += FX_CONV_NR)
                        *inpbuf = *inptr;
                    if (++x0 >= W0) {
                        x0 = 0;
                        ++y0;
                    }
                }
            } else {
                for (int yx = yx0; yx < yx1; yx++) {
                    float* inpbuf = inpbuf_task + (yx - yx0);
                    yi_ = y0*stride_y - pad_top;
                    xi_ = x0*stride_x - pad_left;
                    if (0 <= yi_ && yi_ + (Hk-1)*dilation_y < Hi &&
                        0 <= xi_ && xi_ + (Wk-1)*dilation_x < Wi) {
                        // C. the (Hk x Wk) patch is inside input plane, do offset-based packing
                        const float* inptr = inp + inp_plane_ofs + yi_*Wi + xi_;
                        for (int c = 0; c < Cg; c++, inptr += Hi*Wi) {
                            for (int k = 0; k < Hk*Wk; k++, inpbuf += FX_CONV_NR)
                                *inpbuf = inptr[ofstab[k]];
                        }
                    } else {
                        // D. the slowest path where we need to check each element in the (Hk x Wk) patch
                        for (int k = 0; k < Hk*Wk; k++, inpbuf += FX_CONV_NR) {
                            int yi = yi_ + yxtab[k*2];
                            int xi = xi_ + yxtab[k*2+1];
                            if ((unsigned)yi < (unsigned)Hi &&
                                (unsigned)xi < (unsigned)Wi) {
                                const float* inptr = inp + inp_plane_ofs + yi*Wi + xi;
                                for (int c = 0; c < Cg; c++, inptr += Hi*Wi) {
                                    //if (y0 == 0 && x0 == 0 && yi == 0 && xi == 0)
                                    //    printf("c == %d: inpval = %.2f\n", c, *inptr);
                                    inpbuf[c*(Hk*Wk*FX_CONV_NR)] = *inptr;
                                }
                            } else {
                                for (int c = 0; c < Cg; c++)
                                    inpbuf[c*(Hk*Wk*FX_CONV_NR)] = 0.f;
                            }
                        }
                    }
                    if (++x0 >= W0) {
                        x0 = 0;
                        ++y0;
                    }
                }
            }

            // 2. do convolution, compute Kg x (yx1 - yx0) part of the output tensor
            {
            int outstep0 = H0*W0;
            size_t outofs = (n*ngroups + g)*Kg*outstep0 + yx0;
            float* outptr0 = out + outofs;
            const float* pbptr0 = passby ? passby + outofs : 0;
            float cbuf[FX_CONV_MR*FX_CONV_NR], pbbuf[FX_CONV_MR*FX_CONV_NR];
            bool partial0 = yx1 - yx0 < FX_CONV_NR;
            memset(pbbuf, 0, sizeof(pbbuf));
            for(int k = 0; k < Kg; k += FX_CONV_MR, outptr0 += outstep0*FX_CONV_MR,
                                   pbptr0 += (pbptr0 ? outstep0*FX_CONV_MR : 0))
            {
                int dk = Kg - k < FX_CONV_MR ? Kg - k : FX_CONV_MR;
                bool partial = partial0 || dk < FX_CONV_MR;
                float* outptr = outptr0;
                float* pbptr = (float*)pbptr0;
                int outstep = outstep0;
                if (partial) {
                    outptr = cbuf;
                    outstep = FX_CONV_NR;
                    if (pbptr) {
                        pbptr = pbbuf;
                        for (int k1 = 0; k1 < dk; k1++)
                            memcpy(&pbbuf[k1*FX_CONV_NR], pbptr0 + k1*outstep0,
                                (yx1 - yx0)*sizeof(pbbuf[0]));
                    }
                }
                _fx_conv_block(HkWkCg, conv->weights+(g*(Kg_nblocks*FX_CONV_MR) + k)*HkWkCg,
                           inpbuf_task, outptr, outstep, pbptr, conv->bias + Kg*g + k,
                           minval, maxval, fast_activ);
                if (activ_func)
                    activ_func(outptr, outstep, FX_CONV_MR, FX_CONV_NR, activ_params);
                if (partial) {
                    for (int k1 = 0; k1 < dk; k1++)
                        memcpy(outptr0 + k1*outstep0, &cbuf[k1*FX_CONV_NR],
                               (yx1 - yx0)*sizeof(cbuf[0]));
                }
            }
            }
        }
    }

    fx_free(inpbuf_all);
    /*{
    ts = fx_tick_count() - ts;
    printf("Conv 2D (%dx%ds%d): (%d x %d x %d x %d) => (%d x %d x %d x %d): time=%.1f\n",
        Hk, Wk, stride_x, N, C, Hi, Wi, N, K, H0, W0, ts*1000./fx_tick_frequency());
    }*/
    return FX_OK;
}
#else
static double total_time_1x1 = 0;
static double min_total_time_1x1 = 0;
static int _fx_conv2d(int ndims, const int_* inpsize, const float* inp,
                       const int_* outsize, float* out,
                       const float* passby, _fx_conv2d_t* conv, int ntasks)
{
    assert(ndims == 4 &&
           inpsize[0] == outsize[0] &&
           outsize[1] == conv->K &&
           inpsize[1] == conv->C);
    if (conv->conv_type == _FX_CONV_TYPE_DEPTHWISE) {
        return _fx_depthwise_conv2d(ndims, inpsize, inp, outsize, out, conv);
    }
#ifdef __ARM_NEON
    else if (conv->conv_type == _FX_CONV_TYPE_WINOGRAD3X3) {
        return _fx_winograd_conv2d(ndims, inpsize, inp, outsize, out, passby, conv, ntasks);
    }
#endif
    int N = (int)inpsize[0], C = (int)inpsize[1], Hi = (int)inpsize[2], Wi = (int)inpsize[3];
    int K = conv->K, Hk = conv->Hk, Wk = conv->Wk, ksize = Hk*Wk, ngroups = conv->ngroups;
    int H0 = (int)outsize[2], W0 = (int)outsize[3];
    int Cg = C/ngroups, Kg = K/ngroups;
    int Kg_nblocks = (Kg + FX_CONV_MR-1)/FX_CONV_MR, Kg_aligned = Kg_nblocks*FX_CONV_MR;
    int inp_planesize = Hi*Wi;
    int out_planesize = H0*W0;
    float minval = conv->minval, maxval = conv->maxval;
    bool fast_activ = conv->activ != _FX_ACTIV_NONE && conv->activ_func == 0;
    _fx_activ_func_t activ_func = !fast_activ ? conv->activ_func : 0;
    const float* activ_params = conv->activ_params;
    int stripes_per_sample = (out_planesize + FX_CONV_NR - 1)/FX_CONV_NR;
    if (stripes_per_sample < ntasks*4)
        stripes_per_sample = 1;
    else
        Kg_nblocks = 1;
    int Kstripes = Kg_nblocks*stripes_per_sample;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    //int stripes_per_sample = (out_planesize+FX_CONV_NR-1)/FX_CONV_NR;
    int nsubtasks = N*ngroups*Kstripes;
    //bool is_winograd3x3 = Hk == 3 && Wk == 3 && dilation_y == 1 && dilation_x == 1 && stride_y == 1 && stride_x == 1;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left;
    int pad_right = W0 - pad_left - 1 + Wk - Wi, pad_x = pad_left + pad_right;
    bool fast_1x1 = stride_x == 1 && stride_y == 1 && ksize == 1;
    bool s1d1 = stride_x == 1 && stride_y == 1 && dilation_x == 1 && dilation_y == 1;
    int HkWkCg = ksize*Cg, HkWkC = HkWkCg*ngroups;
    size_t taskbufsize = FX_CONV_NR*ksize*Cg;
    float* inpbuf_all = (float*)fx_malloc(ntasks*taskbufsize*sizeof(inpbuf_all[0]) + ksize*3*sizeof(int));
    int* interior_ofstab = (int*)(inpbuf_all + ntasks*taskbufsize);
    int* yxtab = interior_ofstab + Hk*Wk;
    if (!fast_1x1 && !s1d1)
        _fx_update_ofstab(conv, Hi, Wi, H0, W0);
    const int* ofstab = conv->ofstab;
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
        float* inpbuf_task = &inpbuf_all[taskbufsize*task_id];
        //float inpbuf_task_gold[Cg*ksize*FX_CONV_NR];
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
                k0 = kyx0 * FX_CONV_MR;
                k1 = kyx1 * FX_CONV_MR;
                k1 = k1 <= Kg ? k1 : Kg;
                yx0 = 0;
                yx_limit = out_planesize;
            } else {
                k0 = 0;
                k1 = Kg;
                yx0 = kyx0*FX_CONV_NR;
                yx_limit = kyx1*FX_CONV_NR;
                yx_limit = yx_limit < out_planesize ? yx_limit : out_planesize;
            }

            for (; yx0 < yx_limit; yx0 += FX_CONV_NR) {
                float* inpbuf = inpbuf_task;
                const float* inptr = inp + inp_plane_ofs;
                int yx1 = yx0 + FX_CONV_NR;
                yx1 = yx1 <= yx_limit ? yx1 : yx_limit;
                int slice_len = yx1 - yx0;
                bool partial0 = slice_len < FX_CONV_NR;
                //if (partial0)
                //    putchar('.');
                /*
                    1. pack the data. Copy the HkxWk FX_CONV_NR-wide slices from
                       each feature plane of the input tensor to the input buffer.
                */
                if (fast_1x1) {
                    /*
                       super-fast branch for 1x1 convolutions with sy=sx=1.
                       in this case each feature plane can be safely treated
                       as 1D array and we just extract next portion
                       of FX_CONV_NR elements from each feature plane and
                       put it together.
                    */
                    inptr += yx0;
                    if (!partial0) {
                        // Make special branch where memcpy() is called with a constant buffer size.
                        // Compilers will likely unroll this loop properly.
                        for (int c = 0; c < Cg; c++, inptr += inp_planesize, inpbuf += FX_CONV_NR)
                            memcpy(inpbuf, inptr, FX_CONV_NR*sizeof(inpbuf[0]));
                    } else {
                        for (int c = 0; c < Cg; c++, inptr += inp_planesize, inpbuf += FX_CONV_NR) {
                            memcpy(inpbuf, inptr, slice_len*sizeof(inpbuf[0]));
                            memset(inpbuf + slice_len, 0, (FX_CONV_NR - slice_len)*sizeof(inpbuf[0]));
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
                        float* inpbuf_k = inpbuf_task + k*FX_CONV_NR;
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
                                di = FX_CONV_NR;
                            di = di < FX_CONV_NR ? di : FX_CONV_NR;
                            assert(di > 0);
                            for (int c = 0; c < Cg; c++)
                                memset(inpbuf_k + c*(FX_CONV_NR*ksize), 0, di*sizeof(inpbuf_k[0]));
                            i = di;
                            xi = xi_0;
                            di = Wi_part;
                        }
                        di = di < Wi - xi ? di : Wi - xi;
                        for (; i < FX_CONV_NR;) {
                            di = (di < FX_CONV_NR - i) ? di : (FX_CONV_NR - i);
                            const float* inptr_k = inptr + yi*Wi + xi;
                            int di_z = FX_CONV_NR - (i + di);
                            if (di_z > 0) {
                                // we handle the end of the feature plane gracefully,
                                // in this case we just add as many 0's as necessary
                                // to complete each contiguous slice
                                // in inpbuf to FX_CONV_NR elements.
                                di_z = ((yi == Hi-1) | (di_z < di_z0)) ? di_z : di_z0;
                                for (int c = 0; c < Cg; c++) {
                                    memcpy(inpbuf_k + i + c*(FX_CONV_NR*ksize),
                                           inptr_k + c*inp_planesize,
                                           di*sizeof(inpbuf_k[0]));
                                    memset(inpbuf_k + i + di + c*(FX_CONV_NR*ksize),
                                           0, di_z*sizeof(inpbuf_k[0]));
                                }
                            } else {
                                for (int c = 0; c < Cg; c++) {
                                    memcpy(inpbuf_k + i + c*(FX_CONV_NR*ksize),
                                           inptr_k + c*inp_planesize,
                                           di*sizeof(inpbuf_k[0]));
                                }
                            }
                            i += di + di_z;
                            di = Wi_part;
                            xi = xi_0;
                            yi++;
                        }
                    }
                } else {
                    // [TODO] looks like the offset table does not accelerate things at all.
                    // need to get rid of it.
                    for(int i = 0; i < slice_len; i++) {
                        int ofs0 = ofstab[yx0 + i];
                        if (ofs0 >= 0) {
                            for(int k = 0; k < ksize; k++) {
                                const float* inptr_k = inptr + ofs0 + interior_ofstab[k];
                                float* inpbuf_k = inpbuf_task + k*FX_CONV_NR + i;
                                for(int c = 0; c < Cg; c++, inptr_k += inp_planesize, inpbuf_k += ksize*FX_CONV_NR)
                                    *inpbuf_k = *inptr_k;
                            }
                        } else {
                            const int* border_ofstab = ofstab - ofs0;
                            for(int k = 0; k < ksize; k++) {
                                float* inpbuf_k = inpbuf_task + k*FX_CONV_NR + i;
                                int ofs_k = border_ofstab[k];
                                if (ofs_k >= 0) {
                                    const float* inptr_k = inptr + ofs_k;
                                    for(int c = 0; c < Cg; c++, inptr_k += inp_planesize, inpbuf_k += ksize*FX_CONV_NR)
                                        *inpbuf_k = *inptr_k;
                                } else {
                                    for(int c = 0; c < Cg; c++, inpbuf_k += ksize*FX_CONV_NR)
                                        *inpbuf_k = 0.f;
                                }
                            }
                        }
                    }
                    if (partial0) {
                        float* inpbuf_k = inpbuf_task + slice_len;
                        size_t padding_size = (FX_CONV_NR - slice_len)*sizeof(inpbuf_k[0]);
                        for(int c = 0; c < Cg; c++) {
                            for(int k = 0; k < ksize; k++, inpbuf_k += FX_CONV_NR)
                                memset(inpbuf_k, 0, padding_size);
                        }
                    }
                }

                // 2. do convolution, compute Kg x (yx1 - yx0) part of the output tensor
                {
                int outstep0 = out_planesize;
                size_t outofs = ((n*ngroups + g)*Kg + k0)*outstep0 + yx0;
                float* outptr0 = out + outofs;
                const float* pbptr0 = passby ? passby + outofs : 0;
                float cbuf[FX_CONV_MR*FX_CONV_NR], pbbuf[FX_CONV_MR*FX_CONV_NR];
                memset(pbbuf, 0, sizeof(pbbuf));
                for(int k = k0; k < k1; k += FX_CONV_MR, outptr0 += outstep0*FX_CONV_MR,
                                    pbptr0 += (pbptr0 ? outstep0*FX_CONV_MR : 0)) {
                    int dk = Kg - k < FX_CONV_MR ? Kg - k : FX_CONV_MR;
                    bool partial = partial0 || dk < FX_CONV_MR;
                    float* outptr = outptr0;
                    float* pbptr = (float*)pbptr0;
                    int outstep = outstep0;
                    if (partial) {
                        outptr = cbuf;
                        outstep = FX_CONV_NR;
                        if (pbptr) {
                            pbptr = pbbuf;
                            for (int k1 = 0; k1 < dk; k1++)
                                memcpy(&pbbuf[k1*FX_CONV_NR], pbptr0 + k1*outstep0,
                                    (yx1 - yx0)*sizeof(pbbuf[0]));
                        }
                    }
                    _fx_conv_block(HkWkCg, conv->weights+(g*Kg_aligned + k)*HkWkCg,
                                   inpbuf_task, outptr, outstep, pbptr, conv->bias + Kg*g + k,
                                   minval, maxval, fast_activ);
                    if (partial) {
                        for (int i = 0; i < dk; i++)
                            memcpy(outptr0 + i*outstep0, &cbuf[i*FX_CONV_NR],
                                   (yx1 - yx0)*sizeof(cbuf[0]));
                    }
                }
                }
            }
        }
    }

    fx_free(inpbuf_all);
    if (Hk == 1 && Wk == 1)
    {
    ts = fx_tick_count() - ts;
    total_time_1x1 += ts;
    //printf("Conv 2D (%dx%ds%dd%d): (%d x %d x %d x %d) => (%d x %d x %d x %d): time=%.1f\n",
    //    Hk, Wk, stride_x, dilation_x, N, C, Hi, Wi, N, K, H0, W0, ts*1000./fx_tick_frequency());
    }
    return FX_OK;
}
#endif
}

@nothrow fun get_total_time_1x1(): double = @ccode { return min_total_time_1x1; }
@nothrow fun reset_min_total_time_1x1(): void = @ccode { min_total_time_1x1 = 0.; }
@nothrow fun reset_total_time_1x1(): void = @ccode { total_time_1x1 = 0.; }
@nothrow fun update_total_time_1x1(): void = @ccode {
    min_total_time_1x1 = min_total_time_1x1 == 0 || min_total_time_1x1 > total_time_1x1 ?
        total_time_1x1 : min_total_time_1x1;
}

fun init_conv(kernel_shape: int [], strides: int [], dilations: int [], pads: int [], group: int,
              w_shape: Ast.nnshape_t, w_data: float [], bias_shape: Ast.nnshape_t, bias_data: float [],
              bn_data: float [][], bn_eps: float, activ_func: activ_func_t, activ_params: float []): cptr
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

fun run_conv(inp_shape: Ast.nnshape_t, inp_data: float [],
             out_shape: Ast.nnshape_t, out_data: float [],
             bp_data: float [], conv_data: cptr): void
@ccode {
    const int ntasks = 4;
    _fx_conv2d_t* conv = conv_data && conv_data->ptr ? (_fx_conv2d_t*)conv_data->ptr : 0;
    int_ ndims = inp_shape->shape.dim[0].size;
    if (!conv)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
    if (ndims != 4 || ndims != out_shape->shape.dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    return _fx_conv2d((int)ndims,
                      (const int_*)inp_shape->shape.data,
                      (const float*)inp_data->data,
                      (const int_*)out_shape->shape.data,
                      (float*)out_data->data,
                      (float*)bp_data->data,
                      conv, ntasks);
}

fun run_conv(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Conv {attr={kernel_shape, pads, strides, dilations, group},
        conv_data, fused_batch_norm, non_const_batch_norm,
        fused_activ, non_const_activ, t_inp, t_weights, t_bias, t_out, t_passby} =>
    assert(`kernel_shape.size() == 2`)
    val inp = net.get_tensor(t_inp)
    val weights = net.get_tensor(t_weights)
    val bias = net.get_tensor(t_bias)
    val out = net.get_tensor(t_out)
    val pb = net.get_tensor(t_passby)
    if *conv_data == null || !net.isconst(t_weights) || !net.isconst(t_bias) ||
        non_const_batch_norm || non_const_activ {
        //println(f"Conv: weights.data: {weights.data.elemtype()}, bias.data: {bias.data.elemtype()}")
        val empty: float [] = []
        val (bn_data, bn_eps) =
            match fused_batch_norm {
            | Some (Ast.NN_BatchNorm {epsilon, t_mean, t_var, t_scale, t_B}) =>
                    val bn_mean = net.get_tensor(t_mean)
                    val bn_var = net.get_tensor(t_var)
                    val bn_scale = net.get_tensor(t_scale)
                    val bn_bias = net.get_tensor(t_B)
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
            | Some (Ast.NN_Clip {t_min, t_max}) =>
                val minval = net.get_tensor(t_min)
                val maxval = net.get_tensor(t_max)
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
    match (inp.data, out.data, pb.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data, (Ast.NN_Data_FP32 _ | Ast.NN_Data_Empty)) =>
        val pb_data: float [] = match pb.data {|Ast.NN_Data_FP32 pb_data => pb_data | _ => []}
        run_conv(inp.shape, inp_data, out.shape, out_data, pb_data, *conv_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_conv_transposed(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_ConvTranspose {kernel_shape, pads, strides, dilations, group,
        out_shape, out_padding, t_inp, t_weights, t_bias, t_out} =>
    val out = net.get_tensor(t_out)
    match out.data {
    | Ast.NN_Data_FP32 out_data => for _@idx <- out_data {out_data[idx] = 0.f}
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}
