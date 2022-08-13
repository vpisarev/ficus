/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// various element-wise operations
import Ast

@ccode {
#include "ficus_nn_common.h"

#undef _FX_IMPLEMENT_UNARY_OP
#define _FX_IMPLEMENT_UNARY_OP(suffix, _Tp1, _Tp2, op) \
void _fx_nn_elemwise_##suffix(const void* inptr_, void* outptr_, \
                              int_ len, const float* param) \
{ \
    const _Tp1* inptr = (const _Tp1*)inptr_; \
    _Tp2* outptr = (_Tp2*)outptr_; \
    for (int_ j = 0; j < len; j++) { \
        _Tp1 x = inptr[j]; \
        outptr[j] = (_Tp2)op(x); \
    } \
}

#define _FX_NOP(x)      (x)
#define _FX_RELU(x)     (x >= 0.f ? x : 0.f)
#define _FX_SIGMOID(x)  (1.f / (1.f + expf(-(x))))
#define _FX_SIGN(x)     ((x >= 0.f) - (x <= 0.f))
#define _FX_SOFTPLUS(x) logf(1 + expf(x))
#define _FX_SOFTSIGN(x) ((x)/(1 + fabsf(x)))
#define _FX_SAT_U8(x)   ((x) & ~255 ? (x) : (x) < 0 ? 0 : 255)
#define _FX_SAT_I8(x)   (((x)+128) & ~255 ? (x) : (x) < -128 ? 0 : 127)
#define _FX_NONZERO(x)  ((x) != 0)

static __inline int8_t _fx_cast_f32i8(float x) { int y = (int)lrintf(x); return _FX_SAT_I8(y); }
static __inline uint8_t _fx_cast_f32u8(float x) { int y = (int)lrintf(x); return _FX_SAT_U8(y); }

static _FX_IMPLEMENT_UNARY_OP(abs_f32, float, float, fabsf)
static _FX_IMPLEMENT_UNARY_OP(acos_f32, float, float, acosf)
static _FX_IMPLEMENT_UNARY_OP(acosh_f32, float, float, acoshf)
static _FX_IMPLEMENT_UNARY_OP(asin_f32, float, float, asinf)
static _FX_IMPLEMENT_UNARY_OP(asinh_f32, float, float, asinhf)
static _FX_IMPLEMENT_UNARY_OP(atan_f32, float, float, atanf)
static _FX_IMPLEMENT_UNARY_OP(atanh_f32, float, float, atanhf)
static _FX_IMPLEMENT_UNARY_OP(ceil_f32, float, float, ceilf)
static _FX_IMPLEMENT_UNARY_OP(cos_f32, float, float, cosf)
static _FX_IMPLEMENT_UNARY_OP(cosh_f32, float, float, coshf)
static _FX_IMPLEMENT_UNARY_OP(exp_f32, float, float, expf)
static _FX_IMPLEMENT_UNARY_OP(floor_f32, float, float, floorf)
static _FX_IMPLEMENT_UNARY_OP(log_f32, float, float, logf)
_FX_IMPLEMENT_UNARY_OP(relu_f32, float, float, _FX_RELU)
static _FX_IMPLEMENT_UNARY_OP(round_f32, float, float, roundf)
static _FX_IMPLEMENT_UNARY_OP(sign_f32, float, float, _FX_SIGN)
static _FX_IMPLEMENT_UNARY_OP(sin_f32, float, float, sinf)
static _FX_IMPLEMENT_UNARY_OP(sinh_f32, float, float, sinhf)
static _FX_IMPLEMENT_UNARY_OP(softplus_f32, float, float, _FX_SOFTPLUS)
static _FX_IMPLEMENT_UNARY_OP(softsign_f32, float, float, _FX_SOFTSIGN)
static _FX_IMPLEMENT_UNARY_OP(sqrt_f32, float, float, sqrtf)
static _FX_IMPLEMENT_UNARY_OP(tan_f32, float, float, tanf)

_FX_IMPLEMENT_UNARY_OP(tanh_f32, float, float, tanhf)

static _FX_IMPLEMENT_UNARY_OP(cast_i8i32, int8_t, int32_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_i8i64, int8_t, int64_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_i8f32, int8_t, float, _FX_NOP)

static _FX_IMPLEMENT_UNARY_OP(cast_u8i32, uint8_t, int32_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_u8i64, uint8_t, int64_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_u8f32, uint8_t, float, _FX_NOP)

static _FX_IMPLEMENT_UNARY_OP(cast_i32i8, int32_t, int8_t, _FX_SAT_I8)
static _FX_IMPLEMENT_UNARY_OP(cast_i32u8, int32_t, uint8_t, _FX_SAT_U8)
static _FX_IMPLEMENT_UNARY_OP(cast_i32i64, int32_t, int64_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_i32f32, int32_t, float, _FX_NOP)

static _FX_IMPLEMENT_UNARY_OP(cast_i64i8, int64_t, int8_t, _FX_SAT_I8)
static _FX_IMPLEMENT_UNARY_OP(cast_i64u8, int64_t, uint8_t, _FX_SAT_U8)
static _FX_IMPLEMENT_UNARY_OP(cast_i64i32, int64_t, int32_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_i64f32, int64_t, float, _FX_NOP)

static _FX_IMPLEMENT_UNARY_OP(cast_f32i8, float, int8_t, _fx_cast_f32i8)
static _FX_IMPLEMENT_UNARY_OP(cast_f32u8, float, uint8_t, _fx_cast_f32u8)
static _FX_IMPLEMENT_UNARY_OP(cast_f32i32, float, int32_t, lrintf)
static _FX_IMPLEMENT_UNARY_OP(cast_f32i64, float, int64_t, lrintf)

static _FX_IMPLEMENT_UNARY_OP(cast_i8b, int8_t,  bool, _FX_NONZERO)
static _FX_IMPLEMENT_UNARY_OP(cast_i16b, int16_t, bool, _FX_NONZERO)
static _FX_IMPLEMENT_UNARY_OP(cast_i32b, int32_t, bool, _FX_NONZERO)
static _FX_IMPLEMENT_UNARY_OP(cast_i64b, int64_t, bool, _FX_NONZERO)
static _FX_IMPLEMENT_UNARY_OP(cast_f32b, float,  bool, _FX_NONZERO)

static _FX_IMPLEMENT_UNARY_OP(cast_bi8, bool, int8_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_bi16, bool, int16_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_bi32, bool, int32_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_bi64, bool, int64_t, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_bf32, bool, float, _FX_NOP)

void _fx_nn_elemwise_sigmoid_f32(const void* inptr_, void* outptr_,
                                 int_ len, const float* param)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    for (int_ i = 0; i < len; i++) {
        float x = inptr[i];
        float e_x = expf(-fabsf(x));
        outptr[i] = (x >= 0.f ? 1.f : e_x)/(1 + e_x);
    }
}

void _fx_nn_elemwise_mish_f32(const void* inptr_, void* outptr_,
                              int_ len, const float* param)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    int_ i = 0;
#ifdef __ARM_NEON
    float32x4_t thr = vdupq_n_f32(-36.73f), one = vdupq_n_f32(1.f), z = vdupq_n_f32(0.f);
    _FX_VEXP_INIT();
    for (; i <= len - 4; i += 4) {
        float32x4_t x = vld1q_f32(inptr + i), y;
        x = vbslq_f32(vcleq_f32(x, thr), z, x);
        _FX_VEXP_COMPUTE(vsubq_f32(z, x), y);
        float32x4_t _2y = vaddq_f32(y, y);
        float32x4_t _2y_1 = vaddq_f32(_2y, one);
        x = vdivq_f32(
            vmulq_f32(x, _2y_1),
            vaddq_f32(_2y_1, vmulq_f32(_2y, y)));
        vst1q_f32(outptr + i, x);
    }
#endif
    for (; i < len; i++) {
        float x = inptr[i];
        x *= (x > -36.73f ? 1.f : 0.f);
        float y = expf(-x);
        outptr[i] = x*(1 + 2*y)/(1 + 2*y + 2*y*y);
    }
}

void _fx_nn_elemwise_leaky_relu_f32(const void* inptr_, void* outptr_,
                                    int_ len, const float* param)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    float alpha = *param;
    for (int_ i = 0; i < len; i++) {
        float x = inptr[i];
        outptr[i] = x*(x >= 0 ? 1.f : alpha);
    }
}

void _fx_nn_elemwise_clip_f32(const void* inptr_, void* outptr_,
                              int_ len, const float* param)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    float minval = param[0], maxval = param[1];
    for (int_ i = 0; i < len; i++) {
        float x = inptr[i];
        x = x >= minval ? x : minval;
        x = x <= maxval ? x : maxval;
        outptr[i] = x;
    }
}

void _fx_nn_elemwise_scale_f32(const void* inptr_, void* outptr_,
                               int_ len, const float* param)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    float scale = *param;
    for (int_ i = 0; i < len; i++)
        outptr[i] = inptr[i]*scale;
}

#if _FX_NN_ENABLE_FP16

static _FX_IMPLEMENT_UNARY_OP(abs_f16, fx_f16, fx_f16, fabsf)
static _FX_IMPLEMENT_UNARY_OP(ceil_f16, fx_f16, fx_f16, ceilf)
static _FX_IMPLEMENT_UNARY_OP(floor_f16, fx_f16, fx_f16, floorf)
static _FX_IMPLEMENT_UNARY_OP(round_f16, fx_f16, fx_f16, roundf)
static _FX_IMPLEMENT_UNARY_OP(sqrt_f16, fx_f16, fx_f16, sqrtf)
_FX_IMPLEMENT_UNARY_OP(relu_f16, fx_f16, fx_f16, _FX_RELU)
_FX_IMPLEMENT_UNARY_OP(tanh_f16, fx_f16, fx_f16, tanhf)

static _FX_IMPLEMENT_UNARY_OP(cast_f32f16, float, fx_f16, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_f16f32, fx_f16, float, _FX_NOP)

static _FX_IMPLEMENT_UNARY_OP(cast_i32f16, int32_t, fx_f16, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_i64f16, int64_t, fx_f16, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_f16i32, fx_f16, int32_t, lrintf)
static _FX_IMPLEMENT_UNARY_OP(cast_f16i64, fx_f16, int64_t, lrintf)

static _FX_IMPLEMENT_UNARY_OP(cast_i8f16, int8_t, fx_f16, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_u8f16, uint8_t, fx_f16, _FX_NOP)
static _FX_IMPLEMENT_UNARY_OP(cast_f16i8, fx_f16, int8_t, _fx_cast_f32i8)
static _FX_IMPLEMENT_UNARY_OP(cast_f16u8, fx_f16, uint8_t, _fx_cast_f32u8)
static _FX_IMPLEMENT_UNARY_OP(cast_f16b, fx_f16, bool, _FX_NONZERO)
static _FX_IMPLEMENT_UNARY_OP(cast_bf16, bool, fx_f16, _FX_NOP)

void _fx_nn_elemwise_sigmoid_f16(const void* inptr_, void* outptr_,
                              int_ len, const float* param)
{
    const fx_f16* inptr = (const fx_f16*)inptr_;
    fx_f16* outptr = (fx_f16*)outptr_;
    for (int_ i = 0; i < len; i++) {
        float x = FX_FLOAT(inptr[i]);
        float e_x = expf(-fabsf(x));
        outptr[i] = FX_FLOAT16((x >= 0.f ? 1.f : e_x)/(1 + e_x));
    }
}

void _fx_nn_elemwise_mish_f16(const void* inptr_, void* outptr_,
                              int_ len, const float* param)
{
    const fx_f16* inptr = (const fx_f16*)inptr_;
    fx_f16* outptr = (fx_f16*)outptr_;
    int_ i = 0;
#ifdef __ARM_NEON
    float16x8_t thr = vdupq_n_f16(-36.73f), one = vdupq_n_f16(1.f), z = vdupq_n_f16(0.f);
    _FX_VEXP_INIT();
    for (; i <= len - 4; i += 4) {
        float32x4_t x = vcvt_f32_f16(vld1_f16(inptr + i)), y;
        x = vbslq_f32(vcleq_f32(x, thr), z, x);
        _FX_VEXP_COMPUTE(vsubq_f32(z, x), y);
        float32x4_t _2y = vaddq_f32(y, y);
        float32x4_t _2y_1 = vaddq_f32(_2y, one);
        x = vdivq_f32(
            vmulq_f32(x, _2y_1),
            vaddq_f32(_2y_1, vmulq_f32(_2y, y)));
        vst1_f16(outptr + i, vcvt_f16_f32(x));
    }
#endif
    for (; i < len; i++) {
        float x = FX_FLOAT(inptr[i]);
        x *= (x > -36.73f ? 1.f : 0.f);
        float y = expf(-x);
        outptr[i] = FX_FLOAT16(x*(1 + 2*y)/(1 + 2*y + 2*y*y));
    }
}

void _fx_nn_elemwise_clip_f16(const void* inptr_, void* outptr_,
                              int_ len, const float* param)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    float minval = param[0], maxval = param[1];
    for (int_ i = 0; i < len; i++) {
        float x = inptr[i];
        x = x >= minval ? x : minval;
        x = x <= maxval ? x : maxval;
        outptr[i] = x;
    }
}

void _fx_nn_elemwise_leaky_relu_f16(const void* inptr_, void* outptr_,
                                    int_ len, const float* param)
{
    const fx_f16* inptr = (const fx_f16*)inptr_;
    fx_f16* outptr = (fx_f16*)outptr_;
    float alpha = *param;
    for (int_ j = 0; j < len; j++) {
        float x = FX_FLOAT(inptr[j]);
        outptr[j] = FX_FLOAT16(x*(x >= 0 ? 1.f : alpha));
    }
}

void _fx_nn_elemwise_scale_f16(const void* inptr_, void* outptr_,
                               int_ len, const float* param)
{
    const fx_f16* inptr = (const fx_f16*)inptr_;
    fx_f16* outptr = (fx_f16*)outptr_;
    float scale = *param;
    for (int_ i = 0; i < len; i++)
        outptr[i] = FX_FLOAT16(inptr[i]*scale);
}
#endif

static _fx_unary_func_t _fx_get_cast_func(int inp_typ, int out_typ)
{
    return
        inp_typ == FX_I8 ?
            (out_typ == FX_I32 ? _fx_nn_elemwise_cast_i8i32 :
            out_typ == FX_I64 ? _fx_nn_elemwise_cast_i8i64 :
            out_typ == FX_Bool ? _fx_nn_elemwise_cast_i8b :
            _FX_FP16_CASE(out_typ == FX_F16 ? _fx_nn_elemwise_cast_i8f16 :)
            out_typ == FX_F32 ? _fx_nn_elemwise_cast_i8f32 : 0) :
        inp_typ == FX_U8 ?
            (out_typ == FX_I32 ? _fx_nn_elemwise_cast_u8i32 :
            out_typ == FX_I64 ? _fx_nn_elemwise_cast_u8i64 :
            out_typ == FX_Bool ? _fx_nn_elemwise_cast_i8b :
            _FX_FP16_CASE(out_typ == FX_F16 ? _fx_nn_elemwise_cast_u8f16 :)
            out_typ == FX_F32 ? _fx_nn_elemwise_cast_u8f32 : 0) :
        inp_typ == FX_I32 ?
            (out_typ == FX_I8 ? _fx_nn_elemwise_cast_i32i8 :
            out_typ == FX_U8 ? _fx_nn_elemwise_cast_i32u8 :
            out_typ == FX_I64 ? _fx_nn_elemwise_cast_i32i64 :
            out_typ == FX_Bool ? _fx_nn_elemwise_cast_i32b :
            _FX_FP16_CASE(out_typ == FX_F16 ? _fx_nn_elemwise_cast_i32f16 :)
            out_typ == FX_F32 ? _fx_nn_elemwise_cast_i32f32 : 0) :
        inp_typ == FX_I64 ?
            (out_typ == FX_I8 ? _fx_nn_elemwise_cast_i64i8 :
            out_typ == FX_U8 ? _fx_nn_elemwise_cast_i64u8 :
            out_typ == FX_I32 ? _fx_nn_elemwise_cast_i64i32 :
            out_typ == FX_Bool ? _fx_nn_elemwise_cast_i64b :
            _FX_FP16_CASE(out_typ == FX_F16 ? _fx_nn_elemwise_cast_i64f16 :)
            out_typ == FX_F32 ? _fx_nn_elemwise_cast_i64f32 : 0) :
        inp_typ == FX_F32 ?
            (out_typ == FX_I8 ? _fx_nn_elemwise_cast_f32i8 :
            out_typ == FX_U8 ? _fx_nn_elemwise_cast_f32u8 :
            out_typ == FX_I32 ? _fx_nn_elemwise_cast_f32i32 :
            out_typ == FX_I64 ? _fx_nn_elemwise_cast_f32i64 :
            out_typ == FX_Bool ? _fx_nn_elemwise_cast_f32b :
            _FX_FP16_CASE(out_typ == FX_F16 ? _fx_nn_elemwise_cast_f32f16 :)
            0) :
        inp_typ == FX_Bool ?
            (out_typ == FX_I8 ? _fx_nn_elemwise_cast_bi8 :
            out_typ == FX_U8 ? _fx_nn_elemwise_cast_bi8 :
            out_typ == FX_I32 ? _fx_nn_elemwise_cast_bi32 :
            out_typ == FX_I64 ? _fx_nn_elemwise_cast_bi64 :
            out_typ == FX_F32 ? _fx_nn_elemwise_cast_bf32 :
            _FX_FP16_CASE(out_typ == FX_F16 ? _fx_nn_elemwise_cast_bf16 :)
            0) :
    #ifdef __ARM_NEON
        inp_typ == FX_F16 ?
            (out_typ == FX_I8 ? _fx_nn_elemwise_cast_f16i8 :
            out_typ == FX_U8 ? _fx_nn_elemwise_cast_f16u8 :
            out_typ == FX_I32 ? _fx_nn_elemwise_cast_f16i32 :
            out_typ == FX_I64 ? _fx_nn_elemwise_cast_f16i64 :
            out_typ == FX_F32 ? _fx_nn_elemwise_cast_i64f32 : 0) :
    #endif
        0;
}

static _fx_unary_func_t _fx_get_unary_func(int op, int inp_typ)
{
    return
        op == _FX_NN_Abs ? (inp_typ == FX_F32 ? _fx_nn_elemwise_abs_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_abs_f16 :) 0) :
        op == _FX_NN_Acos ? (inp_typ == FX_F32 ? _fx_nn_elemwise_acos_f32 : 0) :
        op == _FX_NN_Acosh ? (inp_typ == FX_F32 ? _fx_nn_elemwise_acosh_f32 : 0) :
        op == _FX_NN_Asin ? (inp_typ == FX_F32 ? _fx_nn_elemwise_asin_f32 : 0) :
        op == _FX_NN_Asinh ? (inp_typ == FX_F32 ? _fx_nn_elemwise_asinh_f32 : 0) :
        op == _FX_NN_Atan ? (inp_typ == FX_F32 ? _fx_nn_elemwise_atan_f32 : 0) :
        op == _FX_NN_Atanh ? (inp_typ == FX_F32 ? _fx_nn_elemwise_atanh_f32 : 0) :
        op == _FX_NN_Ceil ? (inp_typ == FX_F32 ? _fx_nn_elemwise_ceil_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_ceil_f16 :) 0) :
        op == _FX_NN_Cos ? (inp_typ == FX_F32 ? _fx_nn_elemwise_cos_f32 : 0) :
        op == _FX_NN_Cosh ? (inp_typ == FX_F32 ? _fx_nn_elemwise_cosh_f32 : 0) :
        op == _FX_NN_Exp ? (inp_typ == FX_F32 ? _fx_nn_elemwise_exp_f32 : 0) :
        op == _FX_NN_Floor ? (inp_typ == FX_F32 ? _fx_nn_elemwise_floor_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_floor_f16 :) 0) :
        op == _FX_NN_Log ? (inp_typ == FX_F32 ? _fx_nn_elemwise_log_f32 : 0) :
        op == _FX_NN_Mish ? (inp_typ == FX_F32 ? _fx_nn_elemwise_mish_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_mish_f16 :) 0) :
        op == _FX_NN_Relu ? (inp_typ == FX_F32 ? _fx_nn_elemwise_relu_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_relu_f16 :) 0) :
        op == _FX_NN_Round ? (inp_typ == FX_F32 ? _fx_nn_elemwise_round_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_round_f16 :) 0) :
        op == _FX_NN_Sigmoid ? (inp_typ == FX_F32 ? _fx_nn_elemwise_sigmoid_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_sigmoid_f16 :) 0) :
        op == _FX_NN_Sign ? (inp_typ == FX_F32 ? _fx_nn_elemwise_sign_f32 : 0) :
        op == _FX_NN_Sin ? (inp_typ == FX_F32 ? _fx_nn_elemwise_sin_f32 : 0) :
        op == _FX_NN_Sinh ? (inp_typ == FX_F32 ? _fx_nn_elemwise_sinh_f32 : 0) :
        op == _FX_NN_Softplus ? (inp_typ == FX_F32 ? _fx_nn_elemwise_softplus_f32 : 0) :
        op == _FX_NN_Softsign ? (inp_typ == FX_F32 ? _fx_nn_elemwise_softsign_f32 : 0) :
        op == _FX_NN_Sqrt ? (inp_typ == FX_F32 ? _fx_nn_elemwise_sqrt_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_sqrt_f16 :) 0) :
        op == _FX_NN_Tan ? (inp_typ == FX_F32 ? _fx_nn_elemwise_tan_f32 : 0) :
        op == _FX_NN_Tanh ? (inp_typ == FX_F32 ? _fx_nn_elemwise_tanh_f32 :
                _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_tanh_f16 :) 0) :
        0;
}

static int _fx_run_any_unary(_fx_nntensor_t* inp, _fx_nntensor_t* out,
                             _fx_unary_func_t func, const float* param, int_ ntasks)
{
    fx_arr_t* inp_shape_ = &inp->shape.shape;
    fx_arr_t* out_shape_ = &out->shape.shape;
    int_ i, ndims = inp_shape_->dim[0].size;
    int_ total_size = 1;
    fx_arr_t* inp_data_ = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data_ = &out->data.u.NN_Data_I8;
    size_t inp_esz = inp_data_->dim[0].step;
    size_t out_esz = out_data_->dim[0].step;

    if (!func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims != out_shape_->dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    for (int_ i = 0; i < ndims; i++) {
        int_ sz_i = ((int_*)(inp_shape_->data))[i];
        if (sz_i != ((int_*)(out_shape_->data))[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        total_size *= sz_i;
    }

    if (total_size < 100000) ntasks = 1;
    else ntasks = ntasks > 0 ? ntasks : 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ pix0 = task_id*total_size/ntasks;
        int_ pix1 = (task_id+1)*total_size/ntasks;
        const char* inptr = inp_data_->data + inp_esz*pix0;
        char* outptr = out_data_->data + out_esz*pix0;
        int_ len = pix1 - pix0;
        func(inptr, outptr, len, param);
    }
    return FX_OK;
}

#define _FX_OP_ADD(x, y) ((x) + (y))
#define _FX_OP_SUB(x, y) ((x) - (y))
#define _FX_OP_MUL(x, y) ((x) * (y))
#define _FX_OP_DIV(x, y) ((x) / (y))
#define _FX_OP_MOD(x, y) ((x) % (y))
#define _FX_OP_MIN(x, y) ((x) <= (y) ? (x) : (y))
#define _FX_OP_MAX(x, y) ((x) >= (y) ? (x) : (y))
#define _FX_OP_MEAN(x, y) (((x) + (y))*param)
#define _FX_OP_AND(x, y) ((x) & (y))
#define _FX_OP_OR(x, y) ((x) | (y))
#define _FX_OP_XOR(x, y) ((x) ^ (y))
#define _FX_OP_CMP_GT(x, y) ((x) > (y))
#define _FX_OP_CMP_GE(x, y) ((x) >= (y))
#define _FX_OP_CMP_EQ(x, y) ((x) == (y))

#undef _FX_IMPLEMENT_BINARY_OP
#define _FX_IMPLEMENT_BINARY_OP(suffix, _Tp1, _Tp2, _Tp, do_op) \
static void _fx_nn_elemwise_##suffix(const char* data1, size_t rowstep1, size_t dp1, \
                                const char* data2, size_t rowstep2, size_t dp2, \
                                char* data, size_t rowstep, size_t dp, \
                                int_ nrows, int_ ncols, float param) \
{ \
    for (int_ i = 0; i < nrows; i++) { \
        const _Tp1* ptr1 = (const _Tp1*)data1 + rowstep1*i; \
        const _Tp2* ptr2 = (const _Tp2*)data2 + rowstep2*i; \
        _Tp* ptr = (_Tp*)data + rowstep*i; \
        if (dp1 == 1 && dp2 == 1 && dp == 1) { \
            for(int_ j = 0; j < ncols; j++) { \
                _Tp1 x1 = ptr1[j]; _Tp2 x2 = ptr2[j]; \
                ptr[j] = (_Tp)do_op(x1, x2); \
            } \
        } else if (dp1 == 1 && dp2 == 0 && dp == 1) { \
            _Tp2 x2 = *ptr2; \
            for(int_ j = 0; j < ncols; j++) { \
                _Tp1 x1 = ptr1[j]; \
                ptr[j] = (_Tp)do_op(x1, x2); \
            } \
        } else if (dp1 == 0 && dp2 == 1 && dp == 1) { \
            _Tp1 x1 = *ptr1; \
            for(int_ j = 0; j < ncols; j++) { \
                _Tp2 x2 = ptr2[j]; \
                ptr[j] = (_Tp)do_op(x1, x2); \
            } \
        } else { \
            for(int_ j = 0; j < ncols; j++, ptr1 += dp1, ptr2 += dp2, ptr += dp) { \
                _Tp1 x1 = *ptr1; _Tp2 x2 = *ptr2; \
                *ptr = (_Tp)do_op(x1, x2); \
            } \
        } \
    } \
}

typedef void (*_fx_nn_elemwise_binary_func_t)(
    const char* data1, size_t rowstep1, size_t dp1,
    const char* data2, size_t rowstep2, size_t dp2,
    char* data, size_t rowstep, size_t dp,
    int_ nrows, int_ ncols, float param);

_FX_IMPLEMENT_BINARY_OP(add_f32, float, float, float, _FX_OP_ADD)
_FX_IMPLEMENT_BINARY_OP(add_i32, int, int, int, _FX_OP_ADD)
_FX_IMPLEMENT_BINARY_OP(add_i64, int64_t, int64_t, int64_t, _FX_OP_ADD)

_FX_IMPLEMENT_BINARY_OP(sub_f32, float, float, float, _FX_OP_SUB)
_FX_IMPLEMENT_BINARY_OP(sub_i32, int, int, int, _FX_OP_SUB)
_FX_IMPLEMENT_BINARY_OP(sub_i64, int64_t, int64_t, int64_t, _FX_OP_SUB)

_FX_IMPLEMENT_BINARY_OP(mul_f32, float, float, float, _FX_OP_MUL)
_FX_IMPLEMENT_BINARY_OP(mul_i32, int, int, int, _FX_OP_MUL)
_FX_IMPLEMENT_BINARY_OP(mul_i64, int64_t, int64_t, int64_t, _FX_OP_MUL)

_FX_IMPLEMENT_BINARY_OP(div_f32, float, float, float, _FX_OP_DIV)
_FX_IMPLEMENT_BINARY_OP(div_i32, int, int, int, _FX_OP_DIV)
_FX_IMPLEMENT_BINARY_OP(div_i64, int64_t, int64_t, int64_t, _FX_OP_DIV)

_FX_IMPLEMENT_BINARY_OP(mod_f32, float, float, float, fmodf)
_FX_IMPLEMENT_BINARY_OP(mod_i32, int, int, int, _FX_OP_MOD)
_FX_IMPLEMENT_BINARY_OP(mod_i64, int64_t, int64_t, int64_t, _FX_OP_MOD)

_FX_IMPLEMENT_BINARY_OP(pow_f32, float, float, float, powf)
_FX_IMPLEMENT_BINARY_OP(pow_i32, int, int, int, pow)
_FX_IMPLEMENT_BINARY_OP(pow_i64, int64_t, int64_t, int64_t, pow)

_FX_IMPLEMENT_BINARY_OP(min_f32, float, float, float, _FX_OP_MIN)
_FX_IMPLEMENT_BINARY_OP(min_i32, int, int, int, _FX_OP_MIN)
_FX_IMPLEMENT_BINARY_OP(min_i64, int64_t, int64_t, int64_t, _FX_OP_MIN)

_FX_IMPLEMENT_BINARY_OP(max_f32, float, float, float, _FX_OP_MAX)
_FX_IMPLEMENT_BINARY_OP(max_i32, int, int, int, _FX_OP_MAX)
_FX_IMPLEMENT_BINARY_OP(max_i64, int64_t, int64_t, int64_t, _FX_OP_MAX)

_FX_IMPLEMENT_BINARY_OP(mean_f32, float, float, float, _FX_OP_MEAN)
_FX_IMPLEMENT_BINARY_OP(mean_i32, int, int, int, _FX_OP_MEAN)
_FX_IMPLEMENT_BINARY_OP(mean_i64, int64_t, int64_t, int64_t, _FX_OP_MEAN)

_FX_IMPLEMENT_BINARY_OP(cmp_eq_f32, float, float, bool, _FX_OP_CMP_EQ)
_FX_IMPLEMENT_BINARY_OP(cmp_eq_i32, int, int, bool, _FX_OP_CMP_EQ)
_FX_IMPLEMENT_BINARY_OP(cmp_eq_i64, int64_t, int64_t, bool, _FX_OP_CMP_EQ)

_FX_IMPLEMENT_BINARY_OP(cmp_gt_f32, float, float, bool, _FX_OP_CMP_GT)
_FX_IMPLEMENT_BINARY_OP(cmp_gt_i32, int, int, bool, _FX_OP_CMP_GT)
_FX_IMPLEMENT_BINARY_OP(cmp_gt_i64, int64_t, int64_t, bool, _FX_OP_CMP_GT)

_FX_IMPLEMENT_BINARY_OP(cmp_ge_f32, float, float, bool, _FX_OP_CMP_GE)
_FX_IMPLEMENT_BINARY_OP(cmp_ge_i32, int, int, bool, _FX_OP_CMP_GE)
_FX_IMPLEMENT_BINARY_OP(cmp_ge_i64, int64_t, int64_t, bool, _FX_OP_CMP_GE)

_FX_IMPLEMENT_BINARY_OP(and_bool, bool, bool, bool, _FX_OP_AND)
_FX_IMPLEMENT_BINARY_OP(or_bool, bool, bool, bool, _FX_OP_OR)
_FX_IMPLEMENT_BINARY_OP(xor_bool, bool, bool, bool, _FX_OP_XOR)

#if _FX_NN_ENABLE_FP16
_FX_IMPLEMENT_BINARY_OP(add_f16_f32f16, float, fx_f16, fx_f16, _FX_OP_ADD)
_FX_IMPLEMENT_BINARY_OP(mul_f16_f32f16, float, fx_f16, fx_f16, _FX_OP_MUL)
_FX_IMPLEMENT_BINARY_OP(min_f16_f32f16, float, fx_f16, fx_f16, _FX_OP_MIN)
_FX_IMPLEMENT_BINARY_OP(max_f16_f32f16, float, fx_f16, fx_f16, _FX_OP_MAX)
_FX_IMPLEMENT_BINARY_OP(mean_f16_f32f16, float, fx_f16, fx_f16, _FX_OP_MEAN)
#endif

static _fx_nn_elemwise_binary_func_t
_fx_get_elemwise_binary_func(int el_op, int typ, int typ2)
{
    if (typ != typ2)
        return
    #if _FX_NN_ENABLE_FP16
            el_op == _FX_NN_Add ?
            (typ == FX_F32 && typ2 == FX_F16 ? _fx_nn_elemwise_add_f16_f32f16 : 0) :
            el_op == _FX_NN_Mul ?
            (typ == FX_F32 && typ2 == FX_F16 ? _fx_nn_elemwise_mul_f16_f32f16 : 0) :
            el_op == _FX_NN_Min ?
            (typ == FX_F32 && typ2 == FX_F16 ? _fx_nn_elemwise_min_f16_f32f16 : 0) :
            el_op == _FX_NN_Max ?
            (typ == FX_F32 && typ2 == FX_F16 ? _fx_nn_elemwise_max_f16_f32f16 : 0) :
            el_op == _FX_NN_Mean ?
            (typ == FX_F32 && typ2 == FX_F16 ? _fx_nn_elemwise_mean_f16_f32f16 : 0) :
    #endif
            0;
    return
        el_op == _FX_NN_Add ?
            (typ == FX_F32 ? _fx_nn_elemwise_add_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_add_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_add_i64 : 0) :
        el_op == _FX_NN_Sub ?
            (typ == FX_F32 ? _fx_nn_elemwise_sub_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_sub_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_sub_i64 : 0) :
        el_op == _FX_NN_Mul ?
            (typ == FX_F32 ? _fx_nn_elemwise_mul_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_mul_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_mul_i64 : 0) :
        el_op == _FX_NN_Div ?
            (typ == FX_F32 ? _fx_nn_elemwise_div_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_div_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_div_i64 : 0) :
        el_op == _FX_NN_Mod ?
            (typ == FX_F32 ? _fx_nn_elemwise_mod_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_mod_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_mod_i64 : 0) :
        el_op == _FX_NN_Pow ?
            (typ == FX_F32 ? _fx_nn_elemwise_pow_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_pow_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_pow_i64 : 0) :
        el_op == _FX_NN_Min ?
            (typ == FX_F32 ? _fx_nn_elemwise_min_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_min_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_min_i64 : 0) :
        el_op == _FX_NN_Max ?
            (typ == FX_F32 ? _fx_nn_elemwise_max_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_max_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_max_i64 : 0) :
        el_op == _FX_NN_Mean ?
            (typ == FX_F32 ? _fx_nn_elemwise_mean_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_mean_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_mean_i64 : 0) :
        el_op == _FX_NN_Equal ?
            (typ == FX_F32 ? _fx_nn_elemwise_cmp_eq_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_cmp_eq_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_cmp_eq_i64 : 0) :
        el_op == _FX_NN_Greater ?
            (typ == FX_F32 ? _fx_nn_elemwise_cmp_gt_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_cmp_gt_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_cmp_gt_i64 : 0) :
        el_op == _FX_NN_GreaterOrEqual ?
            (typ == FX_F32 ? _fx_nn_elemwise_cmp_ge_f32 :
             typ == FX_I32 ? _fx_nn_elemwise_cmp_ge_i32 :
             typ == FX_I64 ? _fx_nn_elemwise_cmp_ge_i64 : 0) :
        el_op == _FX_NN_And ?
            (typ == FX_Bool ? _fx_nn_elemwise_and_bool : 0) :
        el_op == _FX_NN_Or ?
            (typ == FX_Bool ? _fx_nn_elemwise_or_bool : 0) :
        el_op == _FX_NN_Xor ?
            (typ == FX_Bool ? _fx_nn_elemwise_and_bool : 0) :
        0;
}
}

fun run_cast(inp: Ast.nntensor_t, out: Ast.nntensor_t, ntasks: int): void
@ccode {
    _fx_unary_func_t func = _fx_get_cast_func(inp->data.tag, out->data.tag);
    return _fx_run_any_unary((_fx_nntensor_t*)inp, (_fx_nntensor_t*)out, func, 0, ntasks);
}

fun run_cast(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Cast {t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_cast(inp, out, *model.ntasks)
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_clip(inp: Ast.nntensor_t, out: Ast.nntensor_t, minval: float, maxval: float, ntasks: int): void
@ccode {
    float param[] = {minval, maxval};
    int inp_typ = inp->data.tag;
    _fx_unary_func_t func = inp_typ == FX_F32 ? _fx_nn_elemwise_clip_f32 :
        _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_clip_f16 :) 0;

    return _fx_run_any_unary((_fx_nntensor_t*)inp, (_fx_nntensor_t*)out, func, param, ntasks);
}

fun run_clip(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Clip {t_inp, t_min, t_max, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val minval = model.get_tensor(t_min)
    val maxval = model.get_tensor(t_max)
    val minval = minval.data.float_scalar_or(-FLT_MAX)
    val maxval = maxval.data.float_scalar_or(FLT_MAX)
    run_clip(inp, out, minval, maxval, *model.ntasks)
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

@private fun run_constantOfShape(v: 't, out: 't []) = for _@idx <- out {out[idx] = v}

fun run_constantOfShape(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_ConstantOfShape {value, t_out} =>
    val out = model.get_tensor(t_out)
    match (value.data, out.data) {
    | (Ast.NN_Data_U8 v_data, Ast.NN_Data_U8 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.NN_Data_I8 v_data, Ast.NN_Data_I8 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.NN_Data_I32 v_data, Ast.NN_Data_I32 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.NN_Data_FP32 v_data, Ast.NN_Data_FP32 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_unary(op: Ast.nnelwise_t, inp: Ast.nntensor_t, out: Ast.nntensor_t, ntasks: int): void
@ccode {
    _fx_unary_func_t func = _fx_get_unary_func(op->tag, inp->data.tag);
    return _fx_run_any_unary((_fx_nntensor_t*)inp, (_fx_nntensor_t*)out, func, 0, ntasks);
}

fun run_binary(el_op_: Ast.nnelwise_t, inp1: Ast.nntensor_t,
               inp2: Ast.nntensor_t, out: Ast.nntensor_t,
               param: float, ntasks: int): void
@ccode {
    enum {_FX_ELEMWISE_MAX_DIMS=5};
    int_ shape1[_FX_ELEMWISE_MAX_DIMS], shape2[_FX_ELEMWISE_MAX_DIMS], shape[_FX_ELEMWISE_MAX_DIMS];
    size_t step1[_FX_ELEMWISE_MAX_DIMS], step2[_FX_ELEMWISE_MAX_DIMS], step[_FX_ELEMWISE_MAX_DIMS];
    fx_arr_t* inp1_shape_ = &inp1->shape.shape;
    fx_arr_t* inp2_shape_ = &inp2->shape.shape;
    fx_arr_t* out_shape_ = &out->shape.shape;
    int el_op = el_op_->tag, inp_typ = inp1->data.tag;
    int inp2_typ = inp2->data.tag, out_typ = out->data.tag;
    fx_arr_t* inp1_data = &inp1->data.u.NN_Data_I8;
    fx_arr_t* inp2_data = &inp2->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;

    if (el_op == _FX_NN_Less || el_op == _FX_NN_LessOrEqual ||
        ((inp_typ == FX_F16 && inp2_typ == FX_F32) &&
        (el_op == _FX_NN_Add || el_op == _FX_NN_Mul ||
         el_op == _FX_NN_Min || el_op == _FX_NN_Max))) {
        el_op = el_op == _FX_NN_Less ? _FX_NN_Greater : el_op == _FX_NN_LessOrEqual ?
            _FX_NN_GreaterOrEqual : el_op;
        fx_arr_t* t;
        FX_SWAP(inp1_shape_, inp2_shape_, t);
        FX_SWAP(inp1_data, inp2_data, t);
        int t2;
        FX_SWAP(inp_typ, inp2_typ, t2);
    }

    int all_ndims[] = {
        (int)inp1_shape_->dim[0].size,
        (int)inp2_shape_->dim[0].size,
        (int)out_shape_->dim[0].size
    };
    const int_* orig_shapes[] = {
        (int_*)inp1_shape_->data,
        (int_*)inp2_shape_->data,
        (int_*)out_shape_->data
    };
    int_* shapes[] = {shape1, shape2, shape};
    size_t* steps[] = {step1, step2, step};

    for (int i = 0; i < 3; i++) {
        if (all_ndims[i] > _FX_ELEMWISE_MAX_DIMS)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }

    // some of inputs are empty => result is empty
    if (!_fx_prepare_for_broadcast_op(3, _FX_ELEMWISE_MAX_DIMS,
                                  all_ndims, orig_shapes,
                                  shapes, steps))
        return FX_OK;

    {
    size_t dp1 = step1[_FX_ELEMWISE_MAX_DIMS-1];
    size_t dp2 = step2[_FX_ELEMWISE_MAX_DIMS-1];
    size_t dp = step[_FX_ELEMWISE_MAX_DIMS-1];
    size_t rowstep1 = step1[_FX_ELEMWISE_MAX_DIMS-2];
    size_t rowstep2 = step2[_FX_ELEMWISE_MAX_DIMS-2];
    size_t rowstep = step[_FX_ELEMWISE_MAX_DIMS-2];
    char* data1 = inp1_data->data;
    char* data2 = inp2_data->data;
    char* data = out_data->data;
    size_t esz1 = inp1_data->dim[0].step;
    size_t esz2 = inp2_data->dim[0].step;
    size_t esz = out_data->dim[0].step;
    int_ nrows = shape[_FX_ELEMWISE_MAX_DIMS-2];
    int_ ncols = shape[_FX_ELEMWISE_MAX_DIMS-1];
    size_t plane_idx, nplanes = 1;
    _fx_nn_elemwise_binary_func_t processing_func =
        _fx_get_elemwise_binary_func(el_op, inp_typ, inp2_typ);

    if (!processing_func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    for (int k = 0; k < _FX_ELEMWISE_MAX_DIMS-2; k++) nplanes *= shape[k];

    for (plane_idx = 0; plane_idx < nplanes; plane_idx++) {
        size_t ofs1 = 0, ofs2 = 0, ofs = 0;
        size_t idx = plane_idx;
        for (int k = _FX_ELEMWISE_MAX_DIMS-3; k >= 0; k--) {
            size_t prev_idx = idx/shape[k];
            size_t i_k = idx - prev_idx*shape[k];
            ofs1 += i_k*step1[k];
            ofs2 += i_k*step2[k];
            ofs += i_k*step[k];
            idx = prev_idx;
        }

        processing_func(data1 + ofs1*esz1, rowstep1, dp1,
                        data2 + ofs2*esz2, rowstep2, dp2,
                        data + ofs*esz, rowstep, dp,
                        nrows, ncols, param);
    }
    }

    return FX_OK;
}

fun run_elemwise(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Elemwise {el_op, t_inp, t_out} =>
    val ntasks = *model.ntasks
    val ninputs = t_inp.size()
    val inp0 = model.get_tensor(t_inp[0])
    val out = model.get_tensor(t_out)
    if ninputs == 1 {
        run_unary(el_op, inp0, out, ntasks)
    } else if ninputs == 2 {
        val inp1 = model.get_tensor(t_inp[1])
        val param = 0.5f
        run_binary(el_op, inp0, inp1, out, param, ntasks)
    } else {
        val el_op_0 = match el_op {Ast.NN_Mean => Ast.NN_Add | _ => el_op}
        val param = 1.f/ninputs
        run_binary(el_op_0, inp0, model.get_tensor(t_inp[1]), out, param, ntasks)
        for j <- 2:ninputs {
            val el_op_j = if j+1 == ninputs {el_op} else {el_op_0}
            run_binary(el_op_j, model.get_tensor(t_inp[j]), out, out, param, ntasks)
        }
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_leaky_relu(inp: Ast.nntensor_t, out: Ast.nntensor_t, alpha: float, ntasks: int): void
@ccode {
    int inp_typ = inp->data.tag;
    _fx_unary_func_t func = inp_typ == FX_F32 ? _fx_nn_elemwise_leaky_relu_f32 :
        _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_leaky_relu_f16 :) 0;
    return _fx_run_any_unary((_fx_nntensor_t*)inp, (_fx_nntensor_t*)out, func, &alpha, ntasks);
}

fun run_leaky_relu(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_LeakyRelu {alpha, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_leaky_relu(inp, out, alpha, *model.ntasks)
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_dropout(inp: Ast.nntensor_t, out: Ast.nntensor_t, ratio: float, ntasks: int): void
@ccode {
    float scale = 1.f/(1 - ratio);
    int inp_typ = inp->data.tag;
    _fx_unary_func_t func = inp_typ == FX_F32 ? _fx_nn_elemwise_scale_f32 :
        _FX_FP16_CASE(inp_typ == FX_F16 ? _fx_nn_elemwise_scale_f16 :) 0;
    return _fx_run_any_unary((_fx_nntensor_t*)inp, (_fx_nntensor_t*)out, func, &scale, ntasks);
}

fun run_dropout(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Dropout {t_inp, t_ratio, t_training_mode, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val training_mode =
        if t_training_mode == 0 {
            false
        } else {
            val t = model.get_tensor(t_training_mode)
            match t.data {
            | Ast.NN_Data_Bool f => f[0]
            | Ast.NN_Data_FP32 f => f[0] != 0.f
            | Ast.NN_Data_I32 f => f[0] != 0i32
            | Ast.NN_Data_U8 f => f[0] != 0u8
            | Ast.NN_Data_I8 f => f[0] != 0i8
            | tdata => int(tdata)[0] != 0
            }
        }

    val ratio =
        if !training_mode {0.f}
        else if t_ratio == 0 {0.5f} else {
            match model.get_tensor(t_ratio).data {
            | Ast.NN_Data_FP32 ratio_data => ratio_data[0]
            | _ => throw NotImplementedError
            }
        }
    if ratio == 0.f {
        model.copy_tensor_data(t_inp, t_out)
    } else {
        run_dropout(inp, out, ratio, *model.ntasks)
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_expand(inp: Ast.nntensor_t, out: Ast.nntensor_t): void
@ccode
{
    enum {_FX_EXPAND_MAX_DIMS = 5};
    int_ inp_shape[_FX_EXPAND_MAX_DIMS], shape[_FX_EXPAND_MAX_DIMS];
    size_t inp_step[_FX_EXPAND_MAX_DIMS], out_step[_FX_EXPAND_MAX_DIMS];
    fx_arr_t* inp_shape_ = &inp->shape.shape;
    fx_arr_t* out_shape_ = &out->shape.shape;
    fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;

    int all_ndims[] = {
        (int)inp_shape_->dim[0].size,
        (int)out_shape_->dim[0].size
    };
    const int_* orig_shapes[] = {
        (int_*)inp_shape_->data,
        (int_*)out_shape_->data
    };
    int_* shapes[] = {inp_shape, shape};
    size_t* steps[] = {inp_step, out_step};

    if (all_ndims[0] > _FX_EXPAND_MAX_DIMS || all_ndims[1] > _FX_EXPAND_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (inp->data.tag != out->data.tag)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    // some of inputs are empty => result is empty
    if (!_fx_prepare_for_broadcast_op(2, _FX_EXPAND_MAX_DIMS, all_ndims, orig_shapes, shapes, steps))
        return FX_OK;

    {
    size_t inp_dp = inp_step[_FX_EXPAND_MAX_DIMS-1];
    size_t out_dp = out_step[_FX_EXPAND_MAX_DIMS-1];
    assert(out_dp == 1);
    size_t inp_rowstep = inp_step[_FX_EXPAND_MAX_DIMS-2];
    size_t out_rowstep = out_step[_FX_EXPAND_MAX_DIMS-2];
    char* inptr0 = inp_data->data;
    char* outptr0 = out_data->data;
    size_t esz = out_data->dim[0].step;
    int_ nrows = shape[_FX_EXPAND_MAX_DIMS-2];
    int_ ncols = shape[_FX_EXPAND_MAX_DIMS-1];
    size_t plane_idx, nplanes = 1;

    if (esz != 1 && esz != 2 && esz != 4 && esz != 8)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    for (int k = 0; k < _FX_EXPAND_MAX_DIMS-2; k++) nplanes *= shape[k];

    for (plane_idx = 0; plane_idx < nplanes; plane_idx++) {
        size_t inp_ofs = 0, out_ofs = 0;
        size_t idx = plane_idx;
        for (int k = _FX_EXPAND_MAX_DIMS-3; k >= 0; k--) {
            size_t prev_idx = idx/shape[k];
            size_t i_k = idx - prev_idx*shape[k];
            inp_ofs += i_k*inp_step[k];
            out_ofs += i_k*out_step[k];
            idx = prev_idx;
        }

        #undef _FX_IMPLEMENT_EXPAND
        #define _FX_IMPLEMENT_EXPAND(_Tp) \
            for (int_ i = 0; i < nrows; i++) { \
                const _Tp* inptr = (const _Tp*)inptr0 + inp_ofs + inp_rowstep*i; \
                _Tp* outptr = (_Tp*)outptr0 + out_ofs + out_rowstep*i; \
                if (inp_dp == 1) { \
                    for(int_ j = 0; j < ncols; j++) { \
                        outptr[j] = inptr[j]; \
                    } \
                } else { \
                    _Tp x = *inptr; \
                    for(int_ j = 0; j < ncols; j++) { \
                        outptr[j] = x; \
                    } \
                } \
            }

        if (esz == 1) {
            _FX_IMPLEMENT_EXPAND(int8_t)
        } else if (esz == 2) {
            _FX_IMPLEMENT_EXPAND(int16_t)
        } else if (esz == 4) {
            _FX_IMPLEMENT_EXPAND(int32_t)
        } else {
            _FX_IMPLEMENT_EXPAND(int64_t)
        }
    }
    }

    return FX_OK;
}

fun run_expand(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Expand {t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_expand(inp, out)
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}
