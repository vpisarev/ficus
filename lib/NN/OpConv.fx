/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast

@ccode {
#include <assert.h>
#include <float.h>
#include <math.h>

#ifdef __ARM_NEON
#include <arm_neon.h>
#endif

enum { _FX_LAYOUT_UNKNOWN=0, _FX_LAYOUT_NCHW=1, _FX_LAYOUT_NHWC=2 };
enum { _FX_CONV_GENERIC=0, _FX_CONV_DEPTHWISE=1, _FX_CONV_WINOGRAD3x3=2 };
enum { _FX_ACTIV_CUSTOM=-1, _FX_ACTIV_NONE=0, _FX_ACTIV_RELU=1, _FX_ACTIV_CLIP=2, _FX_ACTIV_LRELU=3,
       _FX_ACTIV_PRELU=4, _FX_ACTIV_RELU6=6, _FX_ACTIV_SIGMOID=7, _FX_ACTIV_TANH=8, _FX_ACTIV_MISH=9 };

typedef void (*_fx_activ_func_t)(float* data, size_t step, int size0, int size1, const float* params);

#ifdef __ARM_NEON
enum { FX_CONV_MR=8, FX_CONV_NR=12, FX_VEC_NLANES=4 };
#elif defined __AVX__
enum { FX_CONV_MR=6, FX_CONV_NR=8, FX_VEC_NLANES=8 };
#else
enum { FX_CONV_MR=6, FX_CONV_NR=4, FX_VEC_NLANES=1 };
#endif

static void _fx_activ_lrelu(float* data, size_t step, int size0, int size1, const float* params)
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

static void _fx_activ_prelu(float* data, size_t step, int size0, int size1, const float* params)
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

static void _fx_activ_sigmoid(float* data, size_t step, int size0, int size1, const float* params)
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

static void _fx_activ_tanh(float* data, size_t step, int size0, int size1, const float* params)
{
    step /= sizeof(data[0]);
    for (int i = 0; i < size0; i++, data += step) {
        for (int j = 0; j < size1; j++) {
            data[j] = tanh(data[j]);
        }
    }
}

static void _fx_activ_mish(float* data, size_t step, int size0, int size1, const float* params)
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

typedef struct _fx_conv2d_t
{
    int layout, ngroups;
    int K, C, Hk, Wk;
    int stride_y, stride_x;
    int dilation_y, dilation_x;
    int pad_top, pad_bottom, pad_left, pad_right;
    float* weights;
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
    int k = 0, nbias = K + FX_CONV_MR-1;
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
    conv->activ = activ;
    conv->activ_func =
        activ == _FX_ACTIV_CUSTOM ? activ_func :
        activ == _FX_ACTIV_RELU || activ == _FX_ACTIV_RELU6 || activ == _FX_ACTIV_CLIP ? 0 :
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
    } else if (activ == _FX_ACTIV_RELU6) {
        conv->minval = 0.f;
        conv->maxval = 6.f;
    } else if (activ_params) {
        assert(activ_nparams > 0);
        conv->activ_params = (float*)fx_malloc(activ_nparams*sizeof(activ_params[0]));
        if (!conv->activ_params)
            return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
        memcpy(conv->activ_params, activ_params, activ_nparams*sizeof(activ_params[0]));
    }

    // store bias; append some zero's to make sure that
    // we can always read FX_CONV_MR elements starting from any valid index
    conv->bias = (float*)fx_malloc(nbias*sizeof(conv->bias[0]));
    if (conv->bias) {
        for(; k < K; k++)
            conv->bias[k] = bias ? bias[k] : 0.f;
        for(; k < nbias; k++)
            conv->bias[k] = 0.f;
    }

    if (ngroups == K && ngroups == C) {
        // for depth-wise convolutions on NCHW data we just preserve the weights in KCHW layout,
        // but add some padding to make the weights array layout more SIMD-friendly
        int ksize = Hk*Wk;
        int padded_ksize = ((ksize + FX_VEC_NLANES-1)/FX_VEC_NLANES)*FX_VEC_NLANES;
        int nweights = C*padded_ksize;
        conv->weights = (float*)fx_malloc(nweights*sizeof(conv->weights[0]));
        if (conv->weights) {
            memset(conv->weights, 0, nweights*sizeof(conv->weights[0]));
            for(int c = 0; c < C; c++) {
                for (int k = 0; k < ksize; k++)
                    conv->weights[c*padded_ksize + k] = weights[c*ksize + k];
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
                    for(int c = 0; c < Cg; c++) {
                        for(int yx = 0; yx < Hk*Wk; yx++, packed_wptr += FX_CONV_MR) {
                            const float* wptr = weights + (size_t)((g*Kg + k0)*Cg + c)*Hk*Wk + yx;
                            int k = 0;
                            for(; k < dk; k++, wptr += Cg*Hk*Wk)
                                packed_wptr[k] = *wptr;
                            for(; k < FX_CONV_MR; k++)
                                packed_wptr[k] = 0.f;
                        }
                    }
                }
            }
        }
    }

    if (conv && conv->bias && conv->weights)
        fx_status = fx_make_cptr(conv, _fx_free_conv2d, fx_result);
    else
        fx_status = FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
    if (fx_status < 0)
        _fx_free_conv2d(conv);
    return fx_status;
}

static void _fx_conv_block( int k, const float *a, const float *b,
                            float *c, int ldc, const float* bias,
                            float minval, float maxval, bool activ )
{
#ifdef __ARM_NEON
    float32x4_t c0 = vdupq_n_f32(bias[0]), c1 = c0, c2 = c0;
    float32x4_t c3 = vdupq_n_f32(bias[1]), c4 = c3, c5 = c3;
    float32x4_t c6 = vdupq_n_f32(bias[2]), c7 = c6, c8 = c6;
    float32x4_t c9 = vdupq_n_f32(bias[3]), c10 = c9, c11 = c9;
    float32x4_t c12 = vdupq_n_f32(bias[4]), c13 = c12, c14 = c12;
    float32x4_t c15 = vdupq_n_f32(bias[5]), c16 = c15, c17 = c15;
    float32x4_t c18 = vdupq_n_f32(bias[6]), c19 = c18, c20 = c18;
    float32x4_t c21 = vdupq_n_f32(bias[7]), c22 = c21, c23 = c21;

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

static void _fx_depthwise_conv2d(int ndims, const int_* inpsize, const float* inp,
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
    bool slow_activ = conv->activ_func != 0;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    int ksize = Hk*Wk, padded_ksize = ((ksize + FX_VEC_NLANES-1)/FX_VEC_NLANES)*FX_VEC_NLANES;

    int* ofstab = (int*)fx_malloc(3*padded_ksize*sizeof(ofstab[0]));
    int* xytab = ofstab + padded_ksize;
    const float* weights0 = conv->weights, *bias = conv->bias;
    int inner_ytop = (pad_top + stride_y-1)/stride_y, inner_ybottom;
    int inner_xleft = (pad_left + stride_x-1)/stride_x, inner_xright;
    for (int k = 0; k < padded_ksize; k++) {
        int y = k < ksize ? k / Wk : 0;
        int x = k < ksize ? k % Wk : 0;
        int dy = y*dilation_y, dx = x*dilation_x;
        xytab[k*2] = dy; xytab[k*2+1] = dx;
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
                        int yi = yi_ + xytab[k*2];
                        int xi = xi_ + xytab[k*2+1];
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
        if (slow_activ) {
            conv->activ_func(outptr, 0, 1, W0*H0, conv->activ_params);
        }
    }

    fx_free(ofstab);
}

static int _fx_conv2d(int ndims, const int_* inpsize, const float* inp,
                       const int_* outsize, float* out,
                       const _fx_conv2d_t* conv, int ntasks)
{
    assert(ndims == 4 &&
           inpsize[0] == outsize[0] &&
           outsize[1] == conv->K &&
           inpsize[1] == conv->C);
    if (conv->ngroups == conv->K && conv->ngroups == conv->C) {
        _fx_depthwise_conv2d(ndims, inpsize, inp, outsize, out, conv);
        return FX_OK;
    }
    int N = (int)inpsize[0], C = (int)inpsize[1], Hi = (int)inpsize[2], Wi = (int)inpsize[3];
    int K = conv->K, Hk = conv->Hk, Wk = conv->Wk, ngroups = conv->ngroups;
    int H0 = (int)outsize[2], W0 = (int)outsize[3];
    int Cg = C/ngroups, Kg = K/ngroups, Kg_aligned = ((Kg + FX_CONV_MR-1)/FX_CONV_MR)*FX_CONV_MR;
    int HkWkCg = Hk*Wk*Cg, HkWkC = HkWkCg*ngroups;
    const size_t inp_planesize = (size_t)Hi*Wi;
    const size_t out_planesize = (size_t)H0*W0;
    float minval = conv->minval, maxval = conv->maxval;
    bool fast_activ = conv->activ != _FX_ACTIV_NONE && conv->activ_func == 0;
    _fx_activ_func_t activ_func = conv->activ_func;
    const float* activ_params = conv->activ_params;
    int stripes_per_sample = (H0*W0+FX_CONV_NR-1)/FX_CONV_NR;
    int stride_y = conv->stride_y, stride_x = conv->stride_x;
    int dilation_y = conv->dilation_y, dilation_x = conv->dilation_x;
    int pad_top = conv->pad_top, pad_bottom = conv->pad_bottom;
    int pad_left = conv->pad_left, pad_right = conv->pad_right;
    size_t taskbufsize = FX_CONV_NR*Hk*Wk*Cg;
    float* inpbuf_all = (float*)fx_malloc(ntasks*taskbufsize*sizeof(inpbuf_all[0]) + Hk*Wk*3*sizeof(int));
    int* ofstab = (int*)(inpbuf_all + ntasks*taskbufsize);
    int* xytab = ofstab + Hk*Wk;
    for (int y = 0; y < Hk; y++)
        for( int x = 0; x < Wk; x++) {
            int k = y*Wk + x;
            int dy = y*dilation_y, dx = x*dilation_x;
            xytab[k*2] = dy; xytab[k*2+1] = dx;
            ofstab[k] = dy*Wi + dx;
        }
    if ((Hk|Wk) == 1) {
        assert(pad_left == 0 && pad_right == 0 && pad_top == 0 && pad_bottom == 0);
    }

    // (K x Cg*Hk*Wk) * (Cg*Hk*Wk x H0*W0)
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++) {
        float* inpbuf_task = &inpbuf_all[taskbufsize*task_id];
        int ngs0 = (N*ngroups*stripes_per_sample)*task_id/ntasks, ngs1 = (N*ngroups*stripes_per_sample)*(task_id+1)/ntasks;
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
            if (stride_x == 1 && yx1 == yx0 + FX_CONV_NR &&
                0 <= yi_ && yi_ + (Hk-1)*dilation_y < Hi &&
                0 <= xi_ && xi_ + FX_CONV_NR-1 + (Wk-1)*dilation_x < Wi) {
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
                            int yi = yi_ + xytab[k*2];
                            int xi = xi_ + xytab[k*2+1];
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
            float* outptr0 = out + (n*ngroups + g)*Kg*outstep0 + yx0;
            float cbuf[FX_CONV_MR*FX_CONV_NR];
            bool partial0 = yx1 - yx0 < FX_CONV_NR;
            for(int k = 0; k < Kg; k += FX_CONV_MR, outptr0 += outstep0*FX_CONV_MR) {
                int dk = Kg - k < FX_CONV_MR ? Kg - k : FX_CONV_MR;
                bool partial = partial0 || dk < FX_CONV_MR;
                float* outptr = outptr0;
                int outstep = outstep0;
                if (partial) {
                    outptr = cbuf;
                    outstep = FX_CONV_NR;
                }
                _fx_conv_block(HkWkCg, conv->weights+(g*Kg_aligned + k)*HkWkCg,
                           inpbuf_task, outptr, outstep, conv->bias + Kg*g + k,
                           minval, maxval, fast_activ);
                if (activ_func) {
                    activ_func(outptr, outstep, FX_CONV_MR, FX_CONV_NR, activ_params);
                }
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
    return FX_OK;
}
}

fun init_conv(kernel_shape: int [], strides: int [], dilations: int [], pads: int [], group: int,
              w_shape: Ast.nnshape_t, w_data: float [], bias_shape: Ast.nnshape_t, bias_data: float []): cptr
@ccode
{
    const int_* w_shape_ = (const int_*)w_shape->shape.data;
    const int_* strides_ = (const int_*)strides->data;
    const int_* dilations_ = (const int_*)dilations->data;
    const int_* pads_ = (const int_*)pads->data;
    if (w_shape->shape.ndims != 1 || w_shape->shape.dim[0].size != 4 ||
        strides->ndims != 1 || strides->dim[0].size != 2 ||
        dilations->ndims != 1 || dilations->dim[0].size != 2 ||
        pads->ndims != 1 || pads->dim[0].size != 4)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    return _fx_init_conv2d(_FX_LAYOUT_NCHW, _FX_LAYOUT_NCHW, (int)group,
        (int)w_shape_[0], (int)w_shape_[1]*group, (int)w_shape_[2], (int)w_shape_[3],
        (int)strides_[0], (int)strides_[1], (int)dilations_[0], (int)dilations_[1],
        (int)pads_[0], (int)pads_[1], (int)pads_[2], (int)pads_[3],
        (const float*)w_data->data, (const float*)bias_data->data,
        0, 0, 0, 0, 0, _FX_ACTIV_NONE, 0, 0, 0, fx_result);
}

fun run_conv(inp_shape: Ast.nnshape_t, inp_data: float [],
             out_shape: Ast.nnshape_t, out_data: float [],
             conv_data: cptr): void
@ccode
{
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
                      conv, ntasks);
}

fun run_conv(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Conv {kernel_shape, pads, strides, dilations, group, conv_data, t_inp, t_weights, t_bias, t_out} =>
    assert(`kernel_shape.size() == 2`)
    val inp = net.get_tensor(t_inp)
    val weights = net.get_tensor(t_weights)
    val bias = net.get_tensor(t_bias)
    val out = net.get_tensor(t_out)
    if *conv_data == null || !net.isconst(t_weights) || !net.isconst(t_bias) {
        match (weights.data, bias.data) {
        | (Ast.NN_Data_FP32 w_data, Ast.NN_Data_FP32 bias_data) =>
            *conv_data = null // first of all, release the previous data, if any
                              // this way we can immediately re-use the same chunk of memory
                              // for the updated convolution structure
            *conv_data = init_conv(kernel_shape, strides, dilations, pads, group,
                                   weights.shape, w_data, bias.shape, bias_data)
        | _ => throw NotImplementedError
        }
    }
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_conv(inp.shape, inp_data, out.shape, out_data, *conv_data)
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
