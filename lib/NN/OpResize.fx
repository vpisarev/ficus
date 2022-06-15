/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
   Resize operation
*/

import Ast

@ccode {

typedef struct _fx_nndata_t {
   int tag;
   union {
      fx_arr_t NN_Data_I8;
      fx_arr_t NN_Data_U8;
   } u;
} _fx_nndata_t;

enum {
    _FX_NN_Inter_Nearest=1,
    _FX_NN_Inter_Linear=2,
    _FX_NN_Inter_Cubic=3
};

enum {
    _FX_NN_CT_HalfPixel=1,
    _FX_NN_CT_PyTorchHalfPixel=2,
    _FX_NN_CT_AlignCorners=3,
    _FX_NN_CT_Asymmetric=4,
    _FX_NN_CT_TFCropResize=5,
    _FX_NN_CT_OutHalfPixel=6
};

enum {
    _FX_NN_Nearest_RoundPreferFloor=1,
    _FX_NN_Nearest_RoundPreferCeil=2,
    _FX_NN_Nearest_Floor=3,
    _FX_NN_Nearest_Ceil=4
};

enum { _FX_RESIZE_MAX_DIMS = 4 };

static int _fx_compute_tab(int* tab, float* alphatab, int_ inpsz_, int_ outsz_,
                           float scale, float roi_start, float roi_end,
                           int mode, int coord_trans, int nearest_mode,
                           int_ inp_step)
{
    int inpsz = (int)inpsz_, outsz = (int)outsz_;
    float a = 1.f/scale, b = 0.f;
    if (coord_trans == _FX_NN_CT_HalfPixel ||
        coord_trans == _FX_NN_CT_PyTorchHalfPixel) {
        b = 0.5f/scale - 0.5f;
        if (outsz == 1 && coord_trans == _FX_NN_CT_PyTorchHalfPixel) {
            a = 1.f; b = 0.f;
        }
    } else if (coord_trans == _FX_NN_CT_AlignCorners) {
        a = (float)(inpsz - 1)/(outsz - 1);
    } else if (coord_trans == _FX_NN_CT_TFCropResize) {
        if (outsz > 1) {
            a = (roi_end - roi_start)*(inpsz - 1)/(outsz - 1);
            b = roi_start * (inpsz - 1);
        } else {
            a = 1.f;
            b = 0.5 * (roi_start + roi_end) * (inpsz - 1);
        }
    } else if (coord_trans == _FX_NN_CT_Asymmetric)
        ;
    else
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    for (int out_x = 0; out_x < outsz; out_x++) {
        float inp_x = a*out_x + b;
        int int_x = (int)inp_x;
        if (mode == _FX_NN_Inter_Nearest) {
            if (int_x != inp_x) {
                int_x -= inp_x < int_x;
                int ceil_x = int_x + 1;
                if (nearest_mode == _FX_NN_Nearest_Floor)
                    ;
                else if (nearest_mode == _FX_NN_Nearest_Ceil)
                    int_x = ceil_x;
                else {
                    float a = inp_x - int_x;
                    float b = ceil_x - inp_x;
                    if (a > b || (a == b && nearest_mode == _FX_NN_Nearest_RoundPreferCeil))
                        int_x = ceil_x;
                }
            }
            int_x = int_x < 0 ? 0 : int_x >= inpsz ? inpsz - 1 : int_x;
            tab[out_x] = (int)(int_x*inp_step);
        } else if (mode == _FX_NN_Inter_Linear) {
            int_x -= inp_x < int_x;
            float a = inp_x - int_x;
            float a0 = (unsigned)int_x < (unsigned)inpsz ? a : 0.f;
            float a1 = (unsigned)(int_x+1) < (unsigned)inpsz ? a : 0.f;
            int int_x0 = (unsigned)int_x < (unsigned)inpsz ? a : 0.f;
            int int_x1 = int_x+1 < 0 ? 0 : int_x1 >= inpsz ? inpsz - 1 : int_x1;
            tab[out_x*2] = (int)(int_x*inp_step);
            tab[out_x*2+1] = (int)(int_x1*inp_step);
            alphatab[out_x*2] = a;
            alphatab[out_x*2+1] = 1.f - a;
        } else {
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        }
    }
    return FX_OK;
}

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
        status = _fx_compute_tab(tab[i], alpha[i],
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
    fx_arr_t* inp_data = &((_fx_nndata_t*)inp_data_)->u.NN_Data_I8;
    fx_arr_t* out_data = &((_fx_nndata_t*)out_data_)->u.NN_Data_I8;
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
    for(int task_id = 0; task_id < ntasks; task_id++) {
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

fun run_resize(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Resize {coord_trans, cubic_coeff_a, exclude_outside,
        extrapolation_value, mode, nearest_mode, t_inp, t_scales, t_sizes, t_roi, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    val inp_shape = inp.shape.shape
    val out_shape = out.shape.shape
    val scales = float(net.get_tensor(t_scales))
    val sizes = int(net.get_tensor(t_sizes))
    val roi = float(net.get_tensor(t_roi))
    match mode {
    | Ast.NN_Inter_Nearest =>
        run_resize_nearest(inp_shape, inp.data, out_shape, out.data, scales, sizes, roi,
                           coord_trans, nearest_mode, *net.ntasks)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
