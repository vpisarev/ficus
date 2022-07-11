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

#ifdef __cplusplus
}
#endif

#endif
