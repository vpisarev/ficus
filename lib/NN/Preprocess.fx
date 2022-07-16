/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Various preprocessing functions

import Ast

@ccode {
#include "ficus_nn_common.h"
}

type resize_mode_t = ResizeStretch | ResizeFit | ResizeCropCenter
type image_preprocess_params_t =
{
    input_size: (int*2)
    resize_mode: resize_mode_t
    mean: (float*3)
    scale: (float*3)
    swaprb: bool
    layout: Ast.nnlayout_t
    elemtype: Ast.nntyp_t
    ntasks: int
}

@private fun resize_normalize_image(
    image: uint8x3 [,], input_size: (int*2),
    yxscale: (float*2), yxdelta: (float*2),
    mean: (float*3), scale: (float*3),
    swaprb: bool, layout_: Ast.nnlayout_t,
    elemtype_: Ast.nntyp_t, ntasks: int): Ast.nndata_t
@ccode {
    int_ image_height = image->dim[0].size;
    int_ image_width = image->dim[1].size;
    int_ N = 1, C = 3, H = input_size->t0, W = input_size->t1;
    int_ nhwc_shape[] = {1, H, W, C};
    int_ nchw_shape[] = {1, C, H, W};
    int layout = layout_->tag;
    int_* shape = layout == _FX_NN_Layout_NHWC ? nhwc_shape : nchw_shape;
    int elemtype = elemtype_->tag;
    size_t esz = elemtype == _FX_NN_U8 || elemtype == _FX_NN_I8 ?
        1 : elemtype == _FX_NN_FP32 ? 4 : 2;
    fx_arr_t data;
    int status = FX_OK;
    float scale_y = yxscale->t0, scale_x = yscale->t1;
    float dy = yxdelta->t0, dx = yxdelta->t1;
    int_ Cstep = layout == _FX_NN_Layout_NHWC ? 1 : H*W;
    int_ ystep = layout == _FX_NN_Layout_NHWC ? C*W : W;
    int_ pixsize = layout == _FX_NN_Layout_NHWC ? C : 1;
    size_t total = C*H*W;
    int* buf = (int*)alloca(W*C*esz + W*(sizeof(int) + sizeof(float)));
    int* xbuf = buf;
    float* alphabuf = (float*)(xbuf + W);
    char* borderbuf = (char*)(alphabuf + W);
    float m0 = mean->t0, m1 = mean->t1, m2 = mean->t2;
    float s0 = scale->t0, s1 = scale->t1, s2 = scale->t2;

    if (elemtype != _FX_NN_U8 && elemtype != _FX_NN_FP32)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if ((elemtype == _FX_NN_U8) && (s0 != 1.f || s1 != 1.f || s2 != 1.f))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    status = fx_make_arr(4, shape, esz, 0, 0, 0, &data);
    if (status < 0)
        return status;

    if (elemtype == _FX_NN_U8) {
        uint8_t r = fx_sat_f2u8(mean->t0);
        uint8_t g = fx_sat_f2u8(mean->t1);
        uint8_t b = fx_sat_f2u8(mean->t2);
        if (swaprb) {
            uint8_t t;
            FX_SWAP(r, b, t);
        }
        uint8_t* bufptr = (uint8_t*)borderbuf;
        for (int_ x = 0; x < W; x++, bufptr += pixsize) {
            bufptr[0] = r;
            bufptr[Cstep] = g;
            bufptr[Cstep*2] = b;
        }
    } else {
        memset(borderbuf, 0, W*C*esz);
    }

    for (int_ x = 0; x < W; x++) {
        float sx = x*scale_x + dx;
        if (sx < 0 || sx >= image_width) {
            xbuf[x] = -1;
            alphabuf[x] = 0.f;
        } else {
            int ix = (int)sx;
            xbuf[x] = ix;
            sx -= ix;
            alphabuf[x] = sx;
        }
    }

    if (total < 100000) ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ y0 = task_id*H/ntasks, y1 = (task_id+1)*H/ntasks;
        int bofs = swaprb ? 0 : 2, rofs = 2 - bofs;
        for (; y0 < y1; y0++) {
            float sy = y0*scale_y + dy;
            char* outptr_ = data->data + ystep*y0;
            const uint8_t *inptr0, *inptr1;
            int iy = (int)sy;
            if (sy < 0.f || sy >= image_height) {
                if (layout == _FX_NN_Layout_NHWC)
                    memcpy(outptr_y_, borderbuf, W*C*esz);
                else {
                    memcpy(outptr_y_, borderbuf, W*esz);
                    memcpy(outptr_y_ + Cstep*esz, borderbuf + W*esz, W*esz);
                    memcpy(outptr_y_ + Cstep*2*esz, borderbuf + W*2*esz, W*esz);
                }
                continue;
            }
            sy -= iy;
            inptr0 = (uint8_t*)image->data + image_width*3*(iy >= image_height ? image_height - 1 : iy);
            inptr1 = (uint8_t*)image->data + image_width*3*(iy+1 >= image_height ? image_height - 1 : iy+1);

            if (elemtype == _FX_NN_U8) {
                const uint8_t* bbuf = (const uint8_t*)borderbuf;
                uint8_t r_def = bbuf[0], g_def = bbuf[1], b_def = bbuf[2];
                uint8_t* outptr = (uint8_t*)outptr_;

                for (int_ x = 0; x < W; x++, ouptr += pixsize) {
                    int ix = xbuf[x];
                    float sx = alphabuf[x];
                    uint8_t r, g, b;
                    if ((unsigned)ix < (unsigned)(image_width-1)) {
                        int r0 = inptr0[ix*3 + rofs];
                        int g0 = inptr0[ix*3 + 1];
                        int b0 = inptr0[ix*3 + bofs];
                        int r1 = inptr0[ix*3 + rofs + 3];
                        int g1 = inptr0[ix*3 + 4];
                        int b1 = inptr0[ix*3 + bofs + 3];
                        int r2 = inptr1[ix*3 + rofs];
                        int g2 = inptr1[ix*3 + 1];
                        int b2 = inptr1[ix*3 + bofs];
                        int r3 = inptr1[ix*3 + rofs + 3];
                        int g3 = inptr1[ix*3 + 4];
                        int b3 = inptr1[ix*3 + bofs + 3];
                        float r0f = r0 + (r1 - r0)*sx;
                        float g0f = g0 + (g1 - g0)*sx;
                        float b0f = b0 + (b1 - b0)*sx;
                        float r1f = r2 + (r3 - r2)*sx;
                        float g1f = g2 + (g3 - g2)*sx;
                        float b1f = b2 + (b3 - b2)*sx;
                        r = fx_sat_f2u8(r0f + (r1f - r0f)*sy);
                        g = fx_sat_f2u8(g0f + (g1f - g0f)*sy);
                        b = fx_sat_f2u8(b0f + (b1f - b0f)*sy);
                    } else if (ix < 0) {
                        r = r_def;
                        g = g_def;
                        b = b_def;
                    } else {
                        int r0 = inptr0[(image_width-1)*3 + rofs];
                        int g0 = inptr0[(image_width-1)*3 + 1];
                        int b0 = inptr0[(image_width-1)*3 + bofs];
                        int r2 = inptr1[(image_width-1)*3 + rofs];
                        int g2 = inptr1[(image_width-1)*3 + 1];
                        int b2 = inptr1[(image_width-1)*3 + bofs];
                        r = fx_sat_f2u8(r0 + (r2 - r0)*sy);
                        g = fx_sat_f2u8(g0 + (g2 - g0)*sy);
                        b = fx_sat_f2u8(b0 + (b2 - b0)*sy);
                    }
                    r = fx_sat_f2u8(rf);
                    g = fx_sat_f2u8(gf);
                    b = fx_sat_f2u8(bf);
                    outptr[0] = r;
                    outptr[Cstep] = g;
                    outptr[Cstep*2] = b;
                }
            } else if (elemtype == _FX_NN_FP32) {
                const float* bbuf = (const float*)borderbuf;
                float r_def = bbuf[0], g_def = bbuf[1], b_def = bbuf[2];
                float* outptr = (float*)outptr_;

                for (int_ x = 0; x < W; x++, ouptr += pixsize) {
                    int ix = xbuf[x];
                    float sx = alphabuf[x];
                    float r, g, b;
                    if ((unsigned)ix < (unsigned)(image_width-1)) {
                        int r0 = inptr0[ix*3 + rofs];
                        int g0 = inptr0[ix*3 + 1];
                        int b0 = inptr0[ix*3 + bofs];
                        int r1 = inptr0[ix*3 + rofs + 3];
                        int g1 = inptr0[ix*3 + 4];
                        int b1 = inptr0[ix*3 + bofs + 3];
                        int r2 = inptr1[ix*3 + rofs];
                        int g2 = inptr1[ix*3 + 1];
                        int b2 = inptr1[ix*3 + bofs];
                        int r3 = inptr1[ix*3 + rofs + 3];
                        int g3 = inptr1[ix*3 + 4];
                        int b3 = inptr1[ix*3 + bofs + 3];
                        float r0f = r0 + (r1 - r0)*sx;
                        float g0f = g0 + (g1 - g0)*sx;
                        float b0f = b0 + (b1 - b0)*sx;
                        float r1f = r2 + (r3 - r2)*sx;
                        float g1f = g2 + (g3 - g2)*sx;
                        float b1f = b2 + (b3 - b2)*sx;
                        r = ((r0f + (r1f - r0f)*sy) - m0)*s0;
                        g = ((g0f + (g1f - g0f)*sy) - m1)*s1;
                        b = ((b0f + (b1f - b0f)*sy) - m2)*s2;
                    } else if (ix < 0) {
                        r = r_def;
                        g = g_def;
                        b = b_def;
                    } else {
                        int r0 = inptr0[(image_width-1)*3 + rofs];
                        int g0 = inptr0[(image_width-1)*3 + 1];
                        int b0 = inptr0[(image_width-1)*3 + bofs];
                        int r2 = inptr1[(image_width-1)*3 + rofs];
                        int g2 = inptr1[(image_width-1)*3 + 1];
                        int b2 = inptr1[(image_width-1)*3 + bofs];
                        r = ((r0 + (r2 - r0)*sy) - m0)*s0;
                        g = ((g0 + (g2 - g0)*sy) - m1)*s1;
                        b = ((b0 + (b2 - b0)*sy) - m2)*s2;
                    }
                    outptr[0] = r;
                    outptr[Cstep] = g;
                    outptr[Cstep*2] = b;
                }
            }
        }
    }
    fx_result->u.NN_Data_I8 = data;
    fx_result->tag = elemtype;
    return FX_OK;
}

fun image_to_tensor(image: uint8x3 [,], params: image_preprocess_params_t): Ast.nntensor_t
{
    assert(`layout == Ast.NN_Layout_NCHW || layout == Ast.NN_Layout_NHWC`)
    val image_size = image.size()
    val ratio_y = float(params.input_size.0)/image_size.0
    val ratio_x = float(params.input_size.1)/image_size.1
    val (scale_y, scale_x, dy, dx) =
        match params.resize_mode {
        | ResizeStretch =>
            (params.input_size, 1.f/ratio_y, 1.f/ratio_x, 0, 0)
        | ResizeFit =>
            if ratio_y < ratio_x {
                val ratio = ratio_y
                val scale = 1.f/ratio
                val resized_size_x = round(image_size.1*ratio)
                val dx = (input_size.1 - resized_size_x)/2;
                (scale, scale, 0.f, -dx*scale)
            } else {
                val ratio = ratio_x
                val scale = 1.f/ratio
                val resized_size_y = round(image_size.0*ratio)
                val dy = (input_size.0 - resized_size_y)/2;
                (scale, scale, -dy*scale, 0.f)
            }
        | ResizeCropCenter =>
            val (scale, dy, dx) =
                if ratio_y > ratio_x {
                    val ratio = ratio_y
                    val scale = 1.f/ratio
                    (scale, 0.f, (image_size.1 - params.input_size.1*scale)*0.5f)
                } else {
                    val ratio = ratio_x
                    val scale = 1.f/ratio
                    (scale, (image_size.0 - params.input_size.0*scale)*0.5f, 0.f)
                }
            (scale, scale, dy, dx)
        }
    val data = resize_normalize_image(image, params.input_size,
                                      (scale_y, scale_x), (dy, dx),
                                      params.mean, params.scale,
                                      params.swaprb, params.layout,
                                      params.elemtype, params.ntasks)
    val shape = match params.layout {
        | Ast.NN_Layout_NCHW => [N, 3, params.input_size.0, params.input_size.1]
        | Ast.NN_Layout_NHWC => [N, params.input_size.0, params.input_size.1, 3]
        | _ => throw Ast.NNError(f"unxpected layout '{params.layout}'")
    }
    Ast.nntensor_t {data=data, shape=Ast.nnshape_t {shape=shape, layout=params.layout}}
}
