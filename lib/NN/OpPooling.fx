/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include <alloca.h>
#include <float.h>

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

typedef struct _fx_pooling2d_t
{
    int C, Hk, Wk;
    int stride_y, stride_x;
    int dilation_y, dilation_x;
    int pad_top, pad_bottom, pad_left, pad_right;
    bool count_include_pad;
} _fx_pooling2d_t;

static int _fx_maxpool2d(int ndims, const int_* inpsize, const float* inp,
                         const int_* outsize, float* out,
                         const _fx_pooling2d_t* pool)
{
    int N = (int)inpsize[0], C = (int)inpsize[1];
    int Hi = (int)inpsize[2], Wi = (int)inpsize[3];
    int Hk = pool->Hk, Wk = pool->Wk;
    int H0 = (int)outsize[2], W0 = (int)outsize[3];
    const size_t inp_planesize = (size_t)Hi*Wi;
    const size_t out_planesize = (size_t)H0*W0;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int dilation_y = pool->dilation_y, dilation_x = pool->dilation_x;
    int pad_top = pool->pad_top, pad_bottom = pool->pad_bottom;
    int pad_left = pool->pad_left, pad_right = pool->pad_right;
    int ksize = Hk*Wk, padded_ksize = ((ksize + FX_VEC_NLANES-1)/FX_VEC_NLANES)*FX_VEC_NLANES;

    int* ofstab = (int*)alloca(3*padded_ksize*sizeof(ofstab[0]));
    int* xytab = ofstab + padded_ksize;
    int inner_ytop = (pad_top + stride_y-1)/stride_y, inner_ybottom;
    int inner_xleft = (pad_left + stride_x-1)/stride_x, inner_xright;
    for (int k = 0; k < padded_ksize; k++) {
        int y = k < ksize ? k / Wk : 0;
        int x = k < ksize ? k % Wk : 0;
        int dy = y*dilation_y, dx = x*dilation_x;
        xytab[k*2] = dy; xytab[k*2+1] = dx;
        ofstab[k] = dy*Wi + dx;
    }
    inner_xright = (Wi - (Wk - 1)*dilation_x + pad_left)/stride_x;
    inner_xright += inner_xright*stride_x - pad_left + (Wk-1)*dilation_x < Wi;
    inner_ybottom = (Hi - (Hk - 1)*dilation_y + pad_top)/stride_y;
    inner_ybottom += inner_ybottom*stride_y - pad_top + (Hk-1)*dilation_y < Hi;
    if (inner_xleft >= inner_xright || inner_ytop >= inner_ybottom) {
        inner_xleft = W0;
        inner_ytop = H0;
    }
    /*printf("inpsize: %d x %d x %d x %d, outsize: %d x %d x %d x %d; kernel_size: %d x %d, stride: %d x %d, dilation: %d x %d; inner: y=%d - %d, x=%d - %d\n",
        (int)inpsize[0], (int)inpsize[1], (int)inpsize[2], (int)inpsize[3],
        (int)outsize[0], (int)outsize[1], (int)outsize[2], (int)outsize[3],
        Hk, Wk, stride_y, stride_x, dilation_y, dilation_x,
        inner_ytop, inner_ybottom, inner_xleft, inner_xright);
    printf("ofstab: ");
    for(int k = 0; k < ksize; k++)
        printf("%d ", ofstab[k]);
    printf("\n");*/

#ifdef __ARM_NEON
    bool useSIMD = stride_x == 1 && inner_xleft < W0;
    bool is3x3 = Hk == 3 && Wk == 3;
#endif

    #pragma omp parallel for
    for (int nc = 0; nc < N*C; nc++) {
        int c = nc % C;
        const float* inptr = inp + inp_planesize*nc;
        float* outptr = out + out_planesize*nc;

        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_ytop && y0 < inner_ybottom ? inner_xleft : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    s = -FLT_MAX;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + xytab[k*2];
                        int xi = xi_ + xytab[k*2+1];
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        s = fx_maxf(s, inptr[yi*Wi + xi]);
                    }
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
                        for (; x0 <= x1 - FX_VEC_NLANES; x0 += FX_VEC_NLANES) {
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const float* inptr_xi = inptr + W0*yi_ + xi_;
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
                    for (int k = 1; k < ksize; k++)
                        s = fx_maxf(s, inptr_xi[ofstab[k]]);
                    outptr[x0] = s;
                }
                x1 = W0;
            }
        }
    }

    return FX_OK;
}

static int _fx_avgpool2d(int ndims, const int_* inpsize, const float* inp,
                         const int_* outsize, float* out,
                         const _fx_pooling2d_t* pool)
{
    int N = (int)inpsize[0], C = (int)inpsize[1];
    int Hi = (int)inpsize[2], Wi = (int)inpsize[3];
    int Hk = pool->Hk, Wk = pool->Wk;
    int H0 = (int)outsize[2], W0 = (int)outsize[3];
    const size_t inp_planesize = (size_t)Hi*Wi;
    const size_t out_planesize = (size_t)H0*W0;
    int stride_y = pool->stride_y, stride_x = pool->stride_x;
    int dilation_y = pool->dilation_y, dilation_x = pool->dilation_x;
    int pad_top = pool->pad_top, pad_bottom = pool->pad_bottom;
    int pad_left = pool->pad_left, pad_right = pool->pad_right;
    int ksize = Hk*Wk, padded_ksize = ((ksize + FX_VEC_NLANES-1)/FX_VEC_NLANES)*FX_VEC_NLANES;
    bool count_include_pad = pool->count_include_pad;
    float avg_scale = 1.f/(Hk*Wk);

    int* ofstab = (int*)alloca(3*padded_ksize*sizeof(ofstab[0]));
    int* xytab = ofstab + padded_ksize;
    int inner_ytop = (pad_top + stride_y-1)/stride_y, inner_ybottom;
    int inner_xleft = (pad_left + stride_x-1)/stride_x, inner_xright;
    for (int k = 0; k < padded_ksize; k++) {
        int y = k < ksize ? k / Wk : 0;
        int x = k < ksize ? k % Wk : 0;
        int dy = y*dilation_y, dx = x*dilation_x;
        xytab[k*2] = dy; xytab[k*2+1] = dx;
        ofstab[k] = dy*Wi + dx;
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
    bool useSIMD = stride_x == 1 && inner_xleft < W0;
    bool is3x3 = Hk == 3 && Wk == 3;
    float32x4_t vscale = vdupq_n_f32(avg_scale);
#endif

    #pragma omp parallel for
    for (int nc = 0; nc < N*C; nc++) {
        int c = nc % C;
        const float* inptr = inp + inp_planesize*nc;
        float* outptr = out + out_planesize*nc;

        for (int y0 = 0; y0 < H0; y0++, outptr += W0) {
            int x0 = 0, x1 = y0 >= inner_ytop && y0 < inner_ybottom ? inner_xleft : W0;
            int yi_ = y0*stride_y - pad_top;
            for(;;) {
                float s;
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    int counter = 0;
                    s = 0.f;
                    for (int k = 0; k < ksize; k++) {
                        int yi = yi_ + xytab[k*2];
                        int xi = xi_ + xytab[k*2+1];
                        if ((unsigned)yi >= (unsigned)Hi || (unsigned)xi >= (unsigned)Wi)
                            continue;
                        s += inptr[yi*Wi + xi];
                        counter++;
                    }
                    outptr[x0] = count_include_pad ? s*avg_scale : s/(counter > 0 ? counter : 1);
                }
                if (x0 == W0)
                    break;
                x1 = inner_xright;
            #ifdef __ARM_NEON
                if (useSIMD) {
                    if (is3x3) {
                        for (; x0 <= x1 - FX_VEC_NLANES; x0 += FX_VEC_NLANES) {
                            int xi_ = x0*stride_x - pad_left;
                            const float* inptr_xi = inptr + W0*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            float32x4_t s1 = vld1q_f32(inptr_xi + ofstab[1]);
                            float32x4_t s2 = vld1q_f32(inptr_xi + ofstab[2]);

                            s0 = vaddq_f32(s0, vld1q_f32(inptr_xi + ofstab[3]));
                            s1 = vaddq_f32(s1, vld1q_f32(inptr_xi + ofstab[4]));
                            s2 = vaddq_f32(s2, vld1q_f32(inptr_xi + ofstab[5]));

                            s0 = vaddq_f32(s0, vld1q_f32(inptr_xi + ofstab[6]));
                            s1 = vaddq_f32(s1, vld1q_f32(inptr_xi + ofstab[7]));
                            s2 = vaddq_f32(s2, vld1q_f32(inptr_xi + ofstab[8]));

                            s0 = vaddq_f32(vaddq_f32(s0, s1), s2);
                            vst1q_f32(outptr + x0, vmulq_f32(s0, vscale));
                        }
                    } else {
                        for (; x0 <= x1 - FX_VEC_NLANES; x0 += FX_VEC_NLANES) {
                            int xi_ = x0*stride_x - pad_left, k = 0;
                            const float* inptr_xi = inptr + W0*yi_ + xi_;
                            float32x4_t s0 = vld1q_f32(inptr_xi + ofstab[0]);
                            for (k = 1; k < ksize; k++)
                                s0 = vaddq_f32(s0, vld1q_f32(inptr_xi + ofstab[k]));
                            vst1q_f32(outptr + x0, vmulq_f32(s0, vscale));
                        }
                    }
                }
            #endif
                for (; x0 < x1; x0++) {
                    int xi_ = x0*stride_x - pad_left;
                    const float* inptr_xi = inptr + W0*yi_ + xi_;
                    s = inptr_xi[0];
                    for (int k = 1; k < ksize; k++)
                        s += inptr_xi[ofstab[k]];
                    outptr[x0] = s*avg_scale;
                }
                x1 = W0;
            }
        }
    }

    return FX_OK;
}

}

fun run_maxpool_2d(inp: 't [], inp_shape: Ast.nnshape_t,
                   out: 't [], out_shape: Ast.nnshape_t,
                   Hk: int, Wk: int, sy: int, sx: int,
                   dy: int, dx: int, pad_top: int, pad_left: int,
                   pad_bottom: int, pad_right: int): void
@ccode {
    _fx_pooling2d_t pool;
    int_ ndims = inp_shape->shape.dim[0].size;
    const int_* inpsize = (const int_*)inp_shape->shape.data;
    const int_* outsize = (const int_*)out_shape->shape.data;

    if (ndims != 4 || ndims != out_shape->shape.dim[0].size ||
        inpsize[0] != outsize[0] || inpsize[1] != outsize[1])
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if ((Hk|Wk) == 1 && (pad_left != 0 || pad_right != 0 || pad_top != 0 || pad_bottom != 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    pool.C = (int)inpsize[1];
    pool.Hk = (int)Hk; pool.Wk = (int)Wk;
    pool.stride_y = (int)sy; pool.stride_x = (int)sx;
    pool.dilation_y = (int)dy; pool.dilation_x = (int)dx;
    pool.pad_top = (int)pad_top; pool.pad_left = (int)pad_left;
    pool.pad_bottom = (int)pad_bottom; pool.pad_right = (int)pad_right;
    return _fx_maxpool2d((int)ndims, inpsize, (const float*)inp->data,
                         outsize, (float*)out->data, &pool);
}

fun run_maxpool(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_MaxPool {ceil_mode, dilations, kernel_shape, pads, strides, storage_order, t_inp, t_out}
    when kernel_shape.size() == 2 =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.NN_Layout_NCHW`)
    match (inp.data, out.data) {
    | (Ast.NN_Data_U8 inp_data, Ast.NN_Data_U8 out_data) =>
        run_maxpool_2d(inp_data, inp.shape, out_data, out.shape,
            kernel_shape[0], kernel_shape[0],
            strides[0], strides[1], dilations[0], dilations[1],
            pads[0], pads[1], pads[2], pads[3])
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_maxpool_2d(inp_data, inp.shape, out_data, out.shape, kernel_shape[0], kernel_shape[0],
            strides[0], strides[1], dilations[0], dilations[1],
            pads[0], pads[1], pads[2], pads[3])
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation {op.name()}")
}

fun run_avgpool_2d(inp: 't [], inp_shape: Ast.nnshape_t,
                   out: 't [], out_shape: Ast.nnshape_t,
                   Hk: int, Wk: int, sy: int, sx: int,
                   dy: int, dx: int, pad_top: int, pad_left: int,
                   pad_bottom: int, pad_right: int,
                   count_include_pad: bool): void
@ccode {
    _fx_pooling2d_t pool;
    int_ ndims = inp_shape->shape.dim[0].size;
    const int_* inpsize = (const int_*)inp_shape->shape.data;
    const int_* outsize = (const int_*)out_shape->shape.data;

    if (ndims != 4 || ndims != out_shape->shape.dim[0].size ||
        inpsize[0] != outsize[0] || inpsize[1] != outsize[1])
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if ((Hk|Wk) == 1 && (pad_left != 0 || pad_right != 0 || pad_top != 0 || pad_bottom != 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    pool.C = (int)inpsize[1];
    pool.Hk = (int)Hk; pool.Wk = (int)Wk;
    pool.stride_y = (int)sy; pool.stride_x = (int)sx;
    pool.dilation_y = (int)dy; pool.dilation_x = (int)dx;
    pool.pad_top = (int)pad_top; pool.pad_left = (int)pad_left;
    pool.pad_bottom = (int)pad_bottom; pool.pad_right = (int)pad_right;
    pool.count_include_pad = count_include_pad;
    return _fx_avgpool2d((int)ndims, inpsize, (const float*)inp->data,
                         outsize, (float*)out->data, &pool);
}

fun run_avgpool(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_AvgPool {ceil_mode, dilations, kernel_shape, pads,
    strides, count_include_pad, t_inp, t_out}
    when kernel_shape.size() == 2 =>

    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.NN_Layout_NCHW`)
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_avgpool_2d(inp_data, inp.shape, out_data, out.shape,
            kernel_shape[0], kernel_shape[0],
            strides[0], strides[1], dilations[0], dilations[1],
            pads[0], pads[1], pads[2], pads[3], count_include_pad)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation {op.name()}")
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

fun run_global_avgpool(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_GlobalAvgPool {t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.NN_Layout_NCHW`)
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_global_avgpool_2d(inp_data, inp.shape, out_data, out.shape, 0.f)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation {op.name()}")
}
