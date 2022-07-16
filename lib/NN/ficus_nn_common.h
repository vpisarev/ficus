/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// the header contains some common definitions that are used by
// the inline C code in NN module

#ifndef __FICUS_NN_COMMON_H__
#define __FICUS_NN_COMMON_H__

#include <limits.h>
#include <float.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _fx_nndata_t {
   int tag;
   union {
      fx_arr_t NN_Data_I8;
      fx_arr_t NN_Data_U8;
   } u;
} _fx_nndata_t;

enum {
    _FX_NN_Undefined=1, _FX_NN_I8, _FX_NN_U8, _FX_NN_I16, _FX_NN_U16,
    _FX_NN_I32, _FX_NN_U32, _FX_NN_I64, _FX_NN_U64, _FX_NN_FP16,
    _FX_NN_BF16, _FX_NN_FP32, _FX_NN_FP64, _FX_NN_Bool
};

enum {
    _FX_NN_Layout_Unknown,
    _FX_NN_Layout_NC,
    _FX_NN_Layout_NCHW,
    _FX_NN_Layout_NHWC
    _FX_NN_Layout_NCHWxc
};

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

static int _fx_compute_resize_tab(int* tab, float* alphatab, int_ inpsz_, int_ outsz_,
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
            int int_x0 = int_x, int_x1 = int_x+1;
            if (int_x0 < 0) {
                int_x0 = int_x1 = 0;
                a = 0.f;
            } else if (int_x1 >= inpsz) {
                int_x0 = int_x1 = inpsz - 1;
                a = 0.f;
            }
            tab[out_x*2] = (int)(int_x0*inp_step);
            tab[out_x*2+1] = (int)(int_x1*inp_step);
            alphatab[out_x*2] = 1.f - a;
            alphatab[out_x*2+1] = a;
        } else {
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        }
    }
    return FX_OK;
}

#ifdef __cplusplus
}
#endif

#endif
