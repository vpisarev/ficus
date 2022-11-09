@ifdef HAVE_JIT
pragma "clib:loopslayers", "clib:loops", "clib:stdc++"
@ccode
{
#include "loopslayers/loopslayers.h"
#include "ficus_nn_common.h"
#define HAVE_JIT

int _fx_depthwise_conv2d_f32_jit(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_conv2d_t* conv,
                             const char* inptr0, char* outptr0,
                             int ntasks)
{
    int Hi = dw_ctx->Hi, Wi = dw_ctx->Wi, H0 = dw_ctx->H0, W0 = dw_ctx->W0;
    int Hk = conv->Hk, Wk = conv->Wk;
    int stride_x = conv->stride_x, stride_y = conv->stride_y;
    int dilation_x = conv->dilation_x, dilation_y = conv->dilation_y;
    int pad_top = conv->pad_top, pad_left = conv->pad_left;
    int pad_bottom = conv->pad_bottom, pad_right = conv->pad_right;
    int C = conv->C;
    int NC = dw_ctx->N*C;
    dwconv_f32_t jit_func = (dwconv_f32_t)conv->jit_func_f32;
    float* bias = conv->bias;

    float* kernel = conv->weights;

    int NCtask_ = NC / ntasks;
    int tail_task_num = NC % ntasks;
    struct dwc_algs_limits algs_limits_;
    struct dwc_algs_limits algs_limits_tail;
    calc_dwc_algs_limits_f32(&algs_limits_, (tail_task_num ? NCtask_ + 1 : NCtask_), Hi, Wi, Hk, Wk, H0, W0, pad_top, pad_left, pad_bottom, pad_right, stride_x, stride_y, dilation_x, dilation_y);
    if(tail_task_num)
        calc_dwc_algs_limits_f32(&algs_limits_tail, NCtask_, Hi, Wi, Hk, Wk, H0, W0, pad_top, pad_left, pad_bottom, pad_right, stride_x, stride_y, dilation_x, dilation_y);
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++)
    {
        int NC0 = task_id * NCtask_ + (task_id < tail_task_num ? task_id : tail_task_num);
        int kCS = NC0%C;
        int NCtask = (task_id < tail_task_num ? NCtask_ + 1 : NCtask_);
        struct dwc_algs_limits* algs_limits = ((tail_task_num == 0) || (task_id < tail_task_num)? &algs_limits_ : &algs_limits_tail);
        float* data = (((float*)inptr0) + NC0 * Hi * Wi);
        float* result = (((float*)outptr0) + NC0 * H0 * W0);
        jit_func(data, kernel, bias, Hi, Wi, C, NCtask, kCS, result, H0, W0, algs_limits);
    }
    return FX_OK;
}
#if _FX_NN_ENABLE_FP16
int _fx_depthwise_conv2d_f16_jit(const _fx_depthwise2d_t* dw_ctx,
                             const _fx_conv2d_t* conv,
                             const char* inptr0, char* outptr0,
                             int ntasks)
{
    int Hi = dw_ctx->Hi, Wi = dw_ctx->Wi, H0 = dw_ctx->H0, W0 = dw_ctx->W0;
    int Hk = conv->Hk, Wk = conv->Wk;
    int stride_x = conv->stride_x, stride_y = conv->stride_y;
    int dilation_x = conv->dilation_x, dilation_y = conv->dilation_y;
    int pad_top = conv->pad_top, pad_left = conv->pad_left;
    int pad_bottom = conv->pad_bottom, pad_right = conv->pad_right;
    int C = conv->C;
    int NC = dw_ctx->N*C;
    dwconv_f16_t jit_func = (dwconv_f16_t)conv->jit_func_f16;
    fx_f16* bias = (fx_f16*)alloca(C*sizeof(fx_f16));
    for(int bnum = 0; bnum < C; bnum++) 
        bias[bnum] = conv->bias[bnum]; //TODO(ch): Why it's not an fx_f16_t
    fx_f16* kernel = conv->wf16;
    int NCtask_ = NC / ntasks;
    int tail_task_num = NC % ntasks;
    struct dwc_algs_limits algs_limits_;
    struct dwc_algs_limits algs_limits_tail;
    calc_dwc_algs_limits_f16(&algs_limits_, (tail_task_num ? NCtask_ + 1 : NCtask_), Hi, Wi, Hk, Wk, H0, W0, pad_top, pad_left, pad_bottom, pad_right, stride_x, stride_y, dilation_x, dilation_y);
    if(tail_task_num)
        calc_dwc_algs_limits_f16(&algs_limits_tail, NCtask_, Hi, Wi, Hk, Wk, H0, W0, pad_top, pad_left, pad_bottom, pad_right, stride_x, stride_y, dilation_x, dilation_y);
    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++)
    {
        int NC0 = task_id * NCtask_ + (task_id < tail_task_num ? task_id : tail_task_num);
        int kCS = NC0%C;
        int NCtask = (task_id < tail_task_num ? NCtask_ + 1 : NCtask_);
        struct dwc_algs_limits* algs_limits = ((tail_task_num == 0) || (task_id < tail_task_num)? &algs_limits_ : &algs_limits_tail);
        fx_f16* data = (((fx_f16*)inptr0) + NC0 * Hi * Wi);
        fx_f16* result = (((fx_f16*)outptr0) + NC0 * H0 * W0);
        jit_func(data, kernel, bias, Hi, Wi, C, NCtask, kCS, result, H0, W0, algs_limits);
    }
    
    return FX_OK;
}
#endif //_FX_NN_ENABLE_FP16

void generate_dwc_jits(_fx_conv2d_t* conv)
{
    conv->jit_func_f32 = conv->jit_func_f16 = NULL;
#ifdef __ARM_NEON
    int kh = conv->Hk;
    int kw = conv->Wk;
    int padding_top = conv->pad_top;
    int padding_left = conv->pad_left;
    int padding_bottom = conv->pad_bottom;
    int padding_right = conv->pad_right;
    int stride_x = conv->stride_x, stride_y = conv->stride_y;
    int dilation_x = conv->dilation_x, dilation_y = conv->dilation_y;
    float alpha = conv->alpha;
    int not_an_act = ACT_LRELU * 3;

    int activation_type = conv->activ == _FX_ACTIV_RELU ? ACT_NONE :
                          conv->activ == _FX_ACTIV_RELU ? ACT_RELU :
                          (conv->activ == _FX_ACTIV_CLIP && conv->minval == 0.f && conv->maxval == 6.f) ? ACT_RELU6 :
                          conv->activ == _FX_ACTIV_RELU ? ACT_LRELU : not_an_act;
    if(stride_x != 1 || stride_y != 1 || dilation_x != 1 || dilation_y != 1 || activation_type == not_an_act)
        return;
    conv->jit_func_f32 = (void*)generate_dwc_f32(kh, kw, padding_top, padding_left, padding_bottom, padding_right, stride_x, stride_y, dilation_x, dilation_y, activation_type, alpha);

#if _FX_NN_ENABLE_FP16
    conv->jit_func_f16 = (void*)generate_dwc_f16(kh, kw, padding_top, padding_left, padding_bottom, padding_right, stride_x, stride_y, dilation_x, dilation_y, activation_type, alpha);
#endif //_FX_NN_ENABLE_FP16
#endif
}
}
@endif