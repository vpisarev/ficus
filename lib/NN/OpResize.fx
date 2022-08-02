/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
   Resize operation
*/

import Ast

@ccode {

#include "ficus_nn_common.h"

enum { _FX_RESIZE_MAX_DIMS = 4 };

static int _fx_prepare_for_resize(
    const fx_arr_t* inp_shape_, const fx_arr_t* inp_data,
    const fx_arr_t* out_shape_, fx_arr_t* out_data,
    const fx_arr_t* scales_, const fx_arr_t* sizes_,
    const fx_arr_t* roi_, int interp_mode,
    int coord_trans, int nearest_mode,
    int_* inp_shape, int_* out_shape,
    int_* inp_step, int_* out_step,
    int** alltabs, int** tab, float** alpha)
{
    int_ i, ndims = inp_shape_->dim[0].size;
    int_ out_ndims = out_shape_->dim[0].size;
    size_t esz = inp_data->dim[0].step;
    size_t out_esz = out_data->dim[0].step;
    int_ total_tab_size = 0;
    int_ dim_delta = _FX_RESIZE_MAX_DIMS - ndims;
    int tab_sc = interp_mode == _FX_NN_Inter_Nearest ? 1 :
                 interp_mode == _FX_NN_Inter_Linear ? 4 : 8;
    int status = FX_OK;

    if (ndims != out_ndims)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (esz != out_esz)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (sizes_ != 0 && sizes_->data != 0 && sizes_->dim[0].size != ndims)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (scales_ != 0 && scales_->data != 0 && scales_->dim[0].size != ndims)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (roi_ != 0 && roi_->data != 0 && roi_->dim[0].size != ndims*2)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (((scales_ == 0 || scales_->data == 0) && (sizes_ == 0 || sizes_->data == 0)) ||
        (scales_ != 0 && scales_->data != 0 && sizes_ != 0 && sizes_->data != 0))
        return FX_SET_EXN_FAST(FX_EXN_BadArgError);

    for (i = ndims-1; i >= 0; i--) {
        int_ inpsz_i = ((int_*)inp_shape_->data)[i];
        int_ outsz_i = ((int_*)out_shape_->data)[i];
        if (sizes_ != 0 && sizes_->data && ((int_*)sizes_->data)[i] != outsz_i)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        inp_shape[dim_delta + i] = inpsz_i;
        out_shape[dim_delta + i] = outsz_i;
        inp_step[dim_delta + i] = i == ndims-1 ? 1 :
            inp_step[dim_delta+i+1]*inp_shape[dim_delta+i+1];
        out_step[dim_delta + i] = i == ndims-1 ? 1 :
            out_step[dim_delta+i+1]*out_shape[dim_delta+i+1];
    }
    for (i = 0; i < dim_delta; i++) {
        inp_shape[i] = out_shape[i] = 1;
        inp_step[i] = 0;
        out_step[i] = 0;
    }

    for (i = 0; i < _FX_RESIZE_MAX_DIMS; i++)
        total_tab_size += out_shape[i]*tab_sc;
    *alltabs = (int*)fx_malloc(total_tab_size*sizeof(alltabs[0]));
    if (!*alltabs)
        return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);

    total_tab_size = 0;
    for (i = 0; i < _FX_RESIZE_MAX_DIMS; i++) {
        int_ inpsz_i = inp_shape[i];
        int_ outsz_i = out_shape[i];
        tab[i] = *alltabs + total_tab_size;
        alpha[i] = interp_mode == _FX_NN_Inter_Nearest ? 0 : (float*)(tab[i] + outsz_i*(tab_sc/2));
        total_tab_size += outsz_i*tab_sc;
        float scale = i < dim_delta ? 1.f :
            scales_ != 0 && scales_->data != 0 ? ((float*)(scales_->data))[i - dim_delta] :
            (float)outsz_i/inpsz_i;
        float roi_start = 0.f, roi_end = 1.f;
        if (roi_ != 0 && roi_->data && i >= dim_delta) {
            roi_start = ((float*)(roi_->data))[i - dim_delta];
            roi_end = ((float*)(roi_->data))[i - dim_delta + ndims];
        }
        status = _fx_compute_resize_tab(tab[i], alpha[i],
                        inpsz_i, outsz_i, scale, roi_start, roi_end,
                        interp_mode, coord_trans, nearest_mode, inp_step[i]);
        if (status < 0)
            break;
    }
    if (status < 0) {
        fx_free(*alltabs);
        *alltabs = 0;
    }
    return status;
}
}

@private fun run_resize_nearest(inp_shape_: int [], inp_data_: Ast.nndata_t,
                                out_shape_: int [], out_data_: Ast.nndata_t,
                                scales_: float [], sizes_: int [], roi_: float [],
                                coord_trans: Ast.nncoord_trans_t,
                                nearest_mode: Ast.nnearest_mode_t,
                                ntasks: int): void
@ccode {
    int_ inp_shape[_FX_RESIZE_MAX_DIMS];
    int_ out_shape[_FX_RESIZE_MAX_DIMS];
    int_ inp_step[_FX_RESIZE_MAX_DIMS];
    int_ out_step[_FX_RESIZE_MAX_DIMS];
    fx_arr_t* inp_data = &inp_data_->u.NN_Data_I8;
    fx_arr_t* out_data = &out_data_->u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;
    int* alltabs = 0;
    int* tab[_FX_RESIZE_MAX_DIMS];
    float* alpha[_FX_RESIZE_MAX_DIMS];
    int status = _fx_prepare_for_resize(inp_shape_, inp_data, out_shape_, out_data,
                                        scales_, sizes_, roi_, _FX_NN_Inter_Nearest,
                                        coord_trans->tag, nearest_mode->tag,
                                        inp_shape, out_shape, inp_step, out_step,
                                        &alltabs, tab, alpha);
    int_ nsubtasks = out_shape[0]*out_shape[1]*out_shape[2];
    size_t inp_planesize = (size_t)inp_shape[2]*inp_shape[3];
    size_t out_planesize = (size_t)out_shape[2]*out_shape[3];

    if (status < 0)
        return status;
    if (esz != 1 && esz != 2 && esz != 4 && esz != 8)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    #pragma omp parallel for num_threads((int)ntasks)
    for(int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ ncy0 = task_id*nsubtasks/ntasks;
        int_ ncy1 = (task_id+1)*nsubtasks/ntasks;
        int_ outsz_0 = out_shape[0];
        int_ outsz_1 = out_shape[1];
        int_ outsz_2 = out_shape[2];
        int_ outsz_3 = out_shape[3];
        int_ H0 = out_shape[2];
        int_ nc0 = ncy0/outsz_2;
        int_ j2 = ncy0 - nc0*outsz_2;
        int_ j0 = nc0/outsz_1;
        int_ j1 = nc0 - j0*outsz_1;
        int_ out_ofs = j0*out_step[0] + j1*out_step[1] + j2*out_step[2];
        const int* tab3 = tab[3];

    #undef _FX_IMPLEMENT_RESIZE_NEAREST
    #define _FX_IMPLEMENT_RESIZE_NEAREST(typ) \
        typ* outptr = (typ*)(out_data->data) + out_ofs; \
        for(int_ ncy = ncy0; ncy < ncy1; ncy++) { \
            const typ* inptr = (const typ*)(inp_data->data) + tab[0][j0] + tab[1][j1] + tab[2][j2]; \
            int_ j3 = 0; \
            for (; j3 <= outsz_3 - 4; j3 += 4, outptr += 4) { \
                typ x0 = inptr[tab3[j3]], x1 = inptr[tab3[j3+1]]; \
                typ x2 = inptr[tab3[j3+2]], x3 = inptr[tab3[j3+3]]; \
                outptr[0] = x0; outptr[1] = x1; \
                outptr[2] = x2; outptr[3] = x3; \
            } \
            for (; j3 < outsz_3; j3++, outptr++) \
                *outptr = inptr[tab3[j3]]; \
            /* move to the next row, and then possibly to the next feature plane, next sample in the batch ... */ \
            if (++j2 >= outsz_2) { \
                j2 = 0; \
                if (++j1 >= outsz_1) { \
                    j1 = 0; \
                    if (++j0 >= outsz_0) \
                        break; \
                } \
            } \
        }
        if (esz == 4) {
            _FX_IMPLEMENT_RESIZE_NEAREST(int)
        } else if (esz == 1) {
            _FX_IMPLEMENT_RESIZE_NEAREST(int8_t)
        } else if (esz == 2) {
            _FX_IMPLEMENT_RESIZE_NEAREST(int16_t)
        } else {
            _FX_IMPLEMENT_RESIZE_NEAREST(int64_t)
        }
    }
    fx_free(alltabs);
    return FX_OK;
}

@private fun run_resize_linear(inp_shape_: int [], inp_data_: Ast.nndata_t,
                               out_shape_: int [], out_data_: Ast.nndata_t,
                               scales_: float [], sizes_: int [], roi_: float [],
                               coord_trans: Ast.nncoord_trans_t, ntasks: int): void
@ccode {
    int_ inp_shape[_FX_RESIZE_MAX_DIMS];
    int_ out_shape[_FX_RESIZE_MAX_DIMS];
    int_ inp_step[_FX_RESIZE_MAX_DIMS];
    int_ out_step[_FX_RESIZE_MAX_DIMS];
    fx_arr_t* inp_data = &inp_data_->u.NN_Data_I8;
    fx_arr_t* out_data = &out_data_->u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;
    int* alltabs = 0;
    int* tab[_FX_RESIZE_MAX_DIMS];
    float* alpha[_FX_RESIZE_MAX_DIMS];
    int typ = inp_data_->tag;
    int_ nsubtasks;
    size_t inp_planesize, out_planesize;
    int status;
    volatile int parallel_status = FX_OK;

    if (typ != FX_F32 && typ != FX_U8)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    status = _fx_prepare_for_resize(inp_shape_, inp_data, out_shape_, out_data,
                                    scales_, sizes_, roi_, _FX_NN_Inter_Linear,
                                    coord_trans->tag, _FX_NN_Nearest_Floor,
                                    inp_shape, out_shape, inp_step, out_step,
                                    &alltabs, tab, alpha);
    if (status < 0)
        return status;

    // in the case of linear interpolation we process all channels of each "pixel" at once,
    // so in total we have N*H subtasks (N is the number of samples in batch, H is the image height):
    // each subtask computes a certain row the output image, all its channnels.
    nsubtasks = out_shape[0]*out_shape[2];
    inp_planesize = (size_t)inp_shape[2]*inp_shape[3];
    out_planesize = (size_t)out_shape[2]*out_shape[3];

    /*printf("Resize linear (typ=%s): ", typ == FX_F32 ? "FP32" : "U8");
    for (int i = 0; i < _FX_RESIZE_MAX_DIMS; i++) {
        printf("%s%d -> %d (%s)", (i == 0 ? "" : ", "), (int)inp_shape[i], (int)out_shape[i],
            (scale_dims[i] ? "true" : "false"));
    }
    printf("\n");*/

    if (inp_shape[0] != out_shape[0] || inp_shape[1] != out_shape[1]) {
        status = FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    } else if (inp_shape[2] == out_shape[2] && inp_shape[3] == out_shape[3]) {
        memcpy(out_data->data, inp_data->data, inp_data->dim[0].size*esz);
    } else {
        #pragma omp parallel for num_threads((int)ntasks)
        for(int_ task_id = 0; task_id < ntasks; task_id++) {
            int_ ny0 = task_id*nsubtasks/ntasks;
            int_ ny1 = (task_id+1)*nsubtasks/ntasks;
            int_ nchannels = out_shape[1];
            int_ height = out_shape[2];
            int_ width = out_shape[3];
            const int* ytab = tab[2];
            const int* xtab = tab[3];
            const float* yalpha = alpha[2];
            const float* xalpha = alpha[3];
            for (; ny0 < ny1; ny0++) {
                int_ sample_id = ny0/height;
                int_ y = ny0 - height*sample_id;
                float ya0 = yalpha[y*2], ya1 = yalpha[y*2+1];
                int_ y0 = ytab[y*2], y1 = ytab[y*2+1];

                #undef _FX_IMPLEMENT_RESIZE_LINEAR
                #define _FX_IMPLEMENT_RESIZE_LINEAR(typ, round_delta) \
                    typ* outptr = (typ*)out_data->data + sample_id*(nchannels*out_planesize) + y*width; \
                    /* ytab already contains input tensor y's multiplied by input width */ \
                    typ* inptr0 = (typ*)inp_data->data + sample_id*(nchannels*inp_planesize) + y0; \
                    typ* inptr1 = (typ*)inp_data->data + sample_id*(nchannels*inp_planesize) + y1; \
                    if (nchannels == 1) { \
                        for (int_ x = 0; x < width; x++, outptr++) { \
                            int x0 = xtab[x*2], x1 = xtab[x*2+1]; \
                            float xa0 = xalpha[x*2], xa1 = xalpha[x*2+1]; \
                            float out = (inptr0[x0]*xa0 + inptr0[x1]*xa1)*ya0 + \
                                        (inptr1[x0]*xa0 + inptr1[x1]*xa1)*ya1; \
                            outptr[0] = (typ)(out + round_delta); \
                        } \
                    } else if (nchannels == 3) { \
                        for (int_ x = 0; x < width; x++, outptr++) { \
                            int x0 = xtab[x*2], x1 = xtab[x*2+1]; \
                            float xa0 = xalpha[x*2], xa1 = xalpha[x*2+1]; \
                            float out0 = (inptr0[x0]*xa0 + inptr0[x1]*xa1)*ya0 + \
                                        (inptr1[x0]*xa0 + inptr1[x1]*xa1)*ya1; \
                            float out1 = (inptr0[x0+inp_planesize]*xa0 + inptr0[x1+inp_planesize]*xa1)*ya0 + \
                                        (inptr1[x0+inp_planesize]*xa0 + inptr1[x1+inp_planesize]*xa1)*ya1; \
                            float out2 = (inptr0[x0+inp_planesize*2]*xa0 + inptr0[x1+inp_planesize*2]*xa1)*ya0 + \
                                        (inptr1[x0+inp_planesize*2]*xa0 + inptr1[x1+inp_planesize*2]*xa1)*ya1; \
                            outptr[0] = (typ)(out0 + round_delta); \
                            outptr[out_planesize] = (typ)(out1 + round_delta); \
                            outptr[out_planesize*2] = (typ)(out2 + round_delta); \
                        } \
                    } else { \
                        for (int_ x = 0; x < width; x++, outptr++) { \
                            int x0 = xtab[x*2], x1 = xtab[x*2+1]; \
                            float xa0 = xalpha[x*2], xa1 = xalpha[x*2+1]; \
                            for (int_ c = 0; c < nchannels; c++) { \
                                float out = (inptr0[x0+inp_planesize*c]*xa0 + inptr0[x1+inp_planesize*c]*xa1)*ya0 + \
                                            (inptr1[x0+inp_planesize*c]*xa0 + inptr1[x1+inp_planesize*c]*xa1)*ya1; \
                                outptr[out_planesize*c] = (typ)(out + round_delta); \
                            } \
                        } \
                    }
                if (typ == FX_F32) {
                    _FX_IMPLEMENT_RESIZE_LINEAR(float, 0.f)
                } else if (typ == FX_U8) {
                    _FX_IMPLEMENT_RESIZE_LINEAR(uint8_t, 0.5f)
                } else {
                    parallel_status = FX_EXN_NotImplementedError;
                    break;
                }
            }
        }
    }
    if (parallel_status < 0)
        status = FX_SET_EXN_FAST(parallel_status);
    fx_free(alltabs);
    return status;
}

fun run_resize(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Resize {coord_trans, cubic_coeff_a, exclude_outside,
        extrapolation_value, mode, nearest_mode, t_inp, t_scales, t_sizes, t_roi, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val inp_shape = inp.shape.shape
    val out_shape = out.shape.shape
    val scales = float(model.get_tensor(t_scales))
    val sizes = int(model.get_tensor(t_sizes))
    val roi = float(model.get_tensor(t_roi))
    match mode {
    | Ast.NN_Inter_Nearest =>
        run_resize_nearest(inp_shape, inp.data, out_shape, out.data, scales, sizes, roi,
                           coord_trans, nearest_mode, *model.ntasks)
    | Ast.NN_Inter_Linear =>
        val inp_typ = inp.elemtype()
        val out_typ = out.elemtype()
        assert(`inp_typ == out_typ`)
        run_resize_linear(inp_shape, inp.data, out_shape, out.data,
                          scales, sizes, roi, coord_trans, *model.ntasks)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
