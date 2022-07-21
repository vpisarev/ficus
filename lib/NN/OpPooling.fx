/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include <alloca.h>
#include <float.h>
#include "ficus_nn_common.h"

#ifdef __ARM_NEON
#include <arm_neon.h>
#endif

#ifdef __ARM_NEON
enum { FX_VEC_NLANES=4 };
#elif defined __AVX__
enum { FX_VEC_NLANES=8 };
#else
enum { FX_VEC_NLANES=1 };
#endif

#ifndef FLT16_MAX
#define FLT16_MAX 6.5504e+4
#endif

typedef struct _fx_pooling2d_t
{
    int Hi, Wi, H0, W0;
    int Hk, Wk;
    int stride_y, stride_x;
    int pad_top, pad_bottom, pad_left, pad_right;
    int inner_y0, inner_y1;
    int inner_x0, inner_x1;
    bool count_include_pad;
    const int* ofstab;
    const int* yxtab;
} _fx_pooling2d_t;

static void _fx_avgpool_2d_f32(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    int Hi = pool->Hi, Wi = pool->Wi, H0 = pool->H0, W0 = pool->W0;
    int inner_y0 = pool->inner_y0, inner_y1 = pool->inner_y1;
    int inner_x0 = pool->inner_x0, inner_x1 = pool->inner_x1;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->yxtab;
    const int* ofstab = pool->ofstab;
    bool count_include_pad = pool->count_include_pad;
    float avg_scale = 1.f/(pool->Hk*pool->Wk);
#ifdef __ARM_NEON
    bool useSIMD = stride_x == 1 && inner_x0 < W0;
    bool is3x3 = pool->Hk == 3 && pool->Wk == 3;
    float32x4_t vscale = vdupq_n_f32(avg_scale);
    const int vec_nlanes = FX_VEC_NLANES;
#endif

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s = -FLT_MAX;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        float v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 <= x1 - vec_nlanes; x0 += vec_nlanes) {
                            int xi_ = x0*stride_x - pad_left;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            float32x4_t s1 = vld1q_f32(inptr_xi + ofstab[1]);
                            float32x4_t s2 = vld1q_f32(inptr_xi + ofstab[2]);

                            s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[3]));
                            s1 = vmaxq_f32(s1, vld1q_f32(inptr_xi + ofstab[4]));
                            s2 = vmaxq_f32(s2, vld1q_f32(inptr_xi + ofstab[5]));

                            s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[6]));
                            s1 = vmaxq_f32(s1, vld1q_f32(inptr_xi + ofstab[7]));
                            s2 = vmaxq_f32(s2, vld1q_f32(inptr_xi + ofstab[8]));

                            s0 = vmaxq_f32(vmaxq_f32(s0, s1), s2);
                            vst1q_f32(outptr + x0, s0);
                        }
                    } else {
                        for (; x0 <= x1 - vec_nlanes; x0 += vec_nlanes) {
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[k]));
                            vst1q_f32(outptr + x0, s0);
                        }
                    }
                }
            #endif
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const float* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        float v = inptr_xi[ofstab[k]];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                x1 = W0;
            }
        }
    }
}

static void _fx_maxpool_2d_f32(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool)
{
    const float* inptr = (const float*)inptr_;
    float* outptr = (float*)outptr_;
    int Hi = pool->Hi, Wi = pool->Wi, H0 = pool->H0, W0 = pool->W0;
    int inner_y0 = pool->inner_y0, inner_y1 = pool->inner_y1;
    int inner_x0 = pool->inner_x0, inner_x1 = pool->inner_x1;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->yxtab;
    const int* ofstab = pool->ofstab;
#ifdef __ARM_NEON
    bool useSIMD = stride_x == 1 && inner_x0 < W0;
    bool is3x3 = pool->Hk == 3 && pool->Wk == 3;
    const int vec_nlanes = FX_VEC_NLANES;
#endif

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s = -FLT_MAX;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        float v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 <= x1 - vec_nlanes; x0 += vec_nlanes) {
                            int xi_ = x0*stride_x - pad_left;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            float32x4_t s1 = vld1q_f32(inptr_xi + ofstab[1]);
                            float32x4_t s2 = vld1q_f32(inptr_xi + ofstab[2]);

                            s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[3]));
                            s1 = vmaxq_f32(s1, vld1q_f32(inptr_xi + ofstab[4]));
                            s2 = vmaxq_f32(s2, vld1q_f32(inptr_xi + ofstab[5]));

                            s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[6]));
                            s1 = vmaxq_f32(s1, vld1q_f32(inptr_xi + ofstab[7]));
                            s2 = vmaxq_f32(s2, vld1q_f32(inptr_xi + ofstab[8]));

                            s0 = vmaxq_f32(vmaxq_f32(s0, s1), s2);
                            vst1q_f32(outptr + x0, s0);
                        }
                    } else {
                        for (; x0 <= x1 - vec_nlanes; x0 += vec_nlanes) {
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const float* inptr_xi = inptr + Wi*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vmaxq_f32(s0, vld1q_f32(inptr_xi + ofstab[k]));
                            vst1q_f32(outptr + x0, s0);
                        }
                    }
                }
            #endif
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const float* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        float v = inptr_xi[ofstab[k]];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                x1 = W0;
            }
        }
    }
}

#ifdef __ARM_NEON
static void _fx_maxpool_2d_f16(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool)
{
    const __fp16* inptr = (const __fp16*)inptr_;
    __fp16* outptr = (__fp16*)outptr_;
    int Hi = pool->Hi, Wi = pool->Wi, H0 = pool->H0, W0 = pool->W0;
    int inner_y0 = pool->inner_y0, inner_y1 = pool->inner_y1;
    int inner_x0 = pool->inner_x0, inner_x1 = pool->inner_x1;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int ksize = pool->Hk*pool->Wk;
    int pad_top = pool->pad_top, pad_left = pool->pad_left;
    const int* yxtab = pool->yxtab;
    const int* ofstab = pool->ofstab;
    const int vec_nlanes = FX_VEC_NLANES*2;

    bool useSIMD = stride_x == 1 && inner_x0 < W0;
    bool is3x3 = pool->Hk == 3 && pool->Wk == 3;

    for (int c = 0; c < nc; c++, inptr += Hi*Wi) {
        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_y0 && y0 < inner_y1 ? inner_x0 : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                __fp16 s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s = -FLT16_MAX;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + yxtab[k*2];
                        int xi = xi_ + yxtab[k*2+1];
                        __fp16 v;
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        v = inptr[yi*Wi + xi];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                if (x0 == W0)
                    break;
                x1 = inner_x1;
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 <= x1 - vec_nlanes; x0 += vec_nlanes) {
                            int xi_ = x0*stride_x - pad_left;
                            const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vld1q_f16(inptr_xi + ofstab[0]);
                            float16x8_t s1 = vld1q_f16(inptr_xi + ofstab[1]);
                            float16x8_t s2 = vld1q_f16(inptr_xi + ofstab[2]);

                            s0 = vmaxq_f16(s0, vld1q_f16(inptr_xi + ofstab[3]));
                            s1 = vmaxq_f16(s1, vld1q_f16(inptr_xi + ofstab[4]));
                            s2 = vmaxq_f16(s2, vld1q_f16(inptr_xi + ofstab[5]));

                            s0 = vmaxq_f16(s0, vld1q_f16(inptr_xi + ofstab[6]));
                            s1 = vmaxq_f16(s1, vld1q_f16(inptr_xi + ofstab[7]));
                            s2 = vmaxq_f16(s2, vld1q_f16(inptr_xi + ofstab[8]));

                            s0 = vmaxq_f16(vmaxq_f16(s0, s1), s2);
                            vst1q_f16(outptr + x0, s0);
                        }
                    } else {
                        for (; x0 <= x1 - vec_nlanes; x0 += vec_nlanes) {
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                            float16x8_t s0 = vld1q_f16(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vmaxq_f16(s0, vld1q_f16(inptr_xi + ofstab[k]));
                            vst1q_f16(outptr + x0, s0);
                        }
                    }
                }
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const __fp16* inptr_xi = inptr + Wi*yi_ + xi_;
                    s = inptr_xi[ofstab[0]];
                    for (int k = 1; k < ksize; k++) {
                        __fp16 v = inptr_xi[ofstab[k]];
                        s = s >= v ? s : v;
                    }
                    outptr[x0] = s;
                }
                x1 = W0;
            }
        }
    }
}
#endif

typedef void (*_fx_pool_func_t)(int nc, const char* inptr_, char* outptr_,
                               const _fx_pooling2d_t* pool);

}

fun run_pooling(pool_typ: char, inp: Ast.nntensor_t, out: Ast.nntensor_t,
                kernel_shape_: int [], stride_: int [], dilation_: int [],
                padding_: int [], count_include_pad: bool,
                ntasks: int): void
@ccode {
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    _fx_pooling2d_t pool;
    int_ ndims = inp->shape.shape.dim[0].size;
    int_ k_ndims = kernel_shape_->dim[0].size;
    const int_* kernel_shape = (const int_*)(kernel_shape_->data);
    const int_* stride = (const int_*)(stride_->data);
    const int_* dilation = (const int_*)(dilation_->data);
    const int_* padding = (const int_*)(padding_->data);
    int inp_typ = inp->data.tag;
    size_t esz = inp_data->dim[0].step;
    const int_* inpsize = (const int_*)inp->shape.shape.data;
    const int_* outsize = (const int_*)out->shape.shape.data;
    int_ NC, ksize;
    size_t inp_planesize, out_planesize;
    int_ inner_y0, inner_y1;
    int_ inner_x0, inner_x1;
    _fx_pool_func_t func;
    int *ofstab, *yxtab;

    if (inp_typ != out->data.tag)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    if (inp->shape.layout.tag != _FX_NN_Layout_NCHW || k_ndims != 2)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims != 2 + k_ndims || ndims != out->shape.shape.dim[0].size ||
        inpsize[0] != outsize[0] || inpsize[1] != outsize[1])
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    memset(&pool, 0, sizeof(pool));

    NC = inpsize[0]*inpsize[1];
    pool.Hi = inpsize[2];
    pool.Wi = inpsize[3];
    pool.H0 = outsize[2];
    pool.W0 = outsize[3];
    pool.Hk = (int)kernel_shape[0];
    pool.Wk = (int)kernel_shape[1];
    pool.stride_y = (int)stride[0];
    pool.stride_x = (int)stride[1];
    pool.pad_top = (int)padding[0];
    pool.pad_left = (int)padding[1];
    pool.pad_bottom = (int)padding[2];
    pool.pad_right = (int)padding[3];
    pool.count_include_pad = count_include_pad;
    inp_planesize = (size_t)pool.Hi*pool.Wi;
    out_planesize = (size_t)pool.H0*pool.W0;

    if ((pool.Hk|pool.Wk) == 1 &&
        (pool.pad_left | pool.pad_right | pool.pad_top | pool.pad_bottom) != 0)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    ksize = pool.Hk*pool.Wk;
    ofstab = (int*)alloca(3*ksize*sizeof(ofstab[0]));
    yxtab = ofstab + ksize;

    for (int_ k = 0; k < ksize; k++) {
        int_ y = k / pool.Wk;
        int_ x = k - y * pool.Wk;
        int_ dy = y*dilation[0], dx = x*dilation[1];
        yxtab[k*2] = (int)dy; yxtab[k*2+1] = (int)dx;
        ofstab[k] = (int)(dy*pool.Wi + dx);
    }

    /*printf("inpsize: %d x %d x %d x %d, outsize: %d x %d x %d x %d; kernel_size: %d x %d, stride: %d x %d, dilation: %d x %d; pad_y: (%d, %d), pad_x: (%d, %d), inner: y=%d - %d, x=%d - %d\n",
        (int)inpsize[0], (int)inpsize[1], (int)inpsize[2], (int)inpsize[3],
        (int)outsize[0], (int)outsize[1], (int)outsize[2], (int)outsize[3],
        Hk, Wk, stride_y, stride_x, dilation_y, dilation_x,
        pad_top, pad_bottom, pad_left, pad_right,
        inner_ytop, inner_ybottom, inner_xleft, inner_xright);
    printf("ofstab: ");
    for(int k = 0; k < ksize; k++)
        printf("%d ", ofstab[k]);
    printf("\n");*/

    func =
        inp_typ == _FX_NN_FP32 ?
            (pool_typ == 'M' ? _fx_maxpool_2d_f32 :
            pool_typ == 'A' ? _fx_avgpool_2d_f32 : 0) :
    #ifdef __ARM_NEON
        inp_typ == _FX_NN_FP16 ?
            (pool_typ == 'M' ? _fx_maxpool_2d_f16 : 0) :
    #endif
        0;

    if (!func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    inner_y0 = (pool.pad_top + pool.stride_y-1)/pool.stride_y;
    inner_y1 = (pool.Hi - (pool.Hk - 1)*dilation[0] + pool.pad_top)/pool.stride_y;
    inner_y1 += inner_y1*pool.stride_y - pool.pad_top + (pool.Hk-1)*dilation[0] < pool.Hi;

    inner_x0 = (pool.pad_left + pool.stride_x-1)/pool.stride_x;
    inner_x1 = (pool.Wi - (pool.Wk - 1)*dilation[1] + pool.pad_left)/pool.stride_x;
    inner_x1 += inner_x1*pool.stride_x - pool.pad_left + (pool.Wk-1)*dilation[1] < pool.Wi;

    if (inner_x0 >= inner_x1 || inner_y0 >= inner_y1) {
        inner_x0 = pool.W0;
        inner_y0 = pool.H0;
    }

    pool.inner_y0 = (int)inner_y0;
    pool.inner_y1 = (int)inner_y1;
    pool.inner_x0 = (int)inner_x0;
    pool.inner_x1 = (int)inner_x1;
    pool.ofstab = ofstab;
    pool.yxtab = yxtab;

    if (NC*out_planesize < 100000)
        ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ nc0 = task_id*NC/ntasks, nc1 = (task_id+1)*NC/ntasks;
        func((int)(nc1 - nc0),
            inp_data->data + inp_planesize*nc0*esz,
            out_data->data + out_planesize*nc0*esz,
            &pool);
    }

    return FX_OK;
}

fun run_maxpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_MaxPool {ceil_mode, dilations, kernel_shape, pads,
                  strides, storage_order, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_pooling('M', inp, out, kernel_shape, strides, dilations, pads, true, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_avgpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_AvgPool {ceil_mode, dilations, kernel_shape, pads,
    strides, count_include_pad, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_pooling('A', inp, out, kernel_shape, strides, dilations, pads,
                count_include_pad, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_global_avgpool_2d(inp: 't [], inp_shape: Ast.nnshape_t,
                          out: float [], out_shape: Ast.nnshape_t, zero: 'w)
{
    assert(`inp_shape.shape.size() == 4 && out_shape.shape.size() == 4`)
    val (N, C, H, W) = (inp_shape.shape[0], inp_shape.shape[1],
                        inp_shape.shape[2], inp_shape.shape[3])
    assert(`out_shape.shape[0] == N`)
    assert(`out_shape.shape[1] == C`)
    assert(`out_shape.shape[2] == 1 && out_shape.shape[3] == 1`)
    val planesize = H*W
    val scale = 1.f/planesize

    @parallel for nc <- 0:N*C {
        val i0 = nc*planesize
        val i1 = i0 + planesize
        out[nc] = (fold s = zero for i <- i0:i1 {s + inp[i]})*scale
    }
}

fun run_global_avgpool(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_GlobalAvgPool {t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.NN_Layout_NCHW`)
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_global_avgpool_2d(inp_data, inp.shape, out_data, out.shape, 0.f)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
