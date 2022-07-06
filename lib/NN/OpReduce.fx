/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include "ficus_nn_common.h"
typedef int64_t _fx_coord_t;
}

fun run_nonzero(inp_shape_: int [], inp_data_: Ast.nndata_t,
                         out_buf0: Ast.nnbuf_t, ntasks: int):
    (int, int64 [], Ast.nnbuf_t)
@ccode {
    int_ ndims = inp_shape_->dim[0].size;
    const int_* inp_shape = (const int_*)inp_shape_->data;
    int_* cbuf = (int_*)alloca((ntasks*2 + 1)*sizeof(cbuf[0]));
    int_* cofs = cbuf + ntasks;
    int inp_typ = inp_data_->tag, out_typ = _FX_NN_I64;
    size_t esz = sizeof(_fx_coord_t);
    fx_arr_t* inp_data = &inp_data_->u.NN_Data_I8;
    fx_arr_t out_buf, out_data;
    int_ inp_total = inp_data->dim[0].size;
    int_ out_total_bytes = out_buf0->dim[0].size*out_buf0->dim[0].step;
    int_ new_out_total_bytes, out_total;
    int status = FX_OK;
    if (inp_total < 1000000) {
        ntasks = 1;
    }

    if (inp_typ != _FX_NN_I8 && inp_typ != _FX_NN_U8 && inp_typ != _FX_NN_Bool &&
        inp_typ != _FX_NN_I32 && inp_typ != _FX_NN_U32 &&
        inp_typ != _FX_NN_I64 && inp_typ != _FX_NN_U64 &&
        inp_typ != _FX_NN_FP32)
    {
        printf("run_nonzero: unsupported inp_typ=%d\n", inp_typ);
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ i0 = task_id*inp_total/ntasks, i1 = (task_id+1)*inp_total/ntasks;
        int_ nz = 0;
        #undef _FX_IMPLEMENT_NONZERO_COUNT
        #define _FX_IMPLEMENT_NONZERO_COUNT(typ) \
        { \
            const typ* inptr = (const typ*)(inp_data->data); \
            for (; i0 < i1; i0++) \
                nz += inptr[i0] != 0; \
        }
        if (inp_typ == _FX_NN_U8 || inp_typ == _FX_NN_I8 || inp_typ == _FX_NN_Bool) {
            _FX_IMPLEMENT_NONZERO_COUNT(int8_t)
        } else if (inp_typ == _FX_NN_I32 || inp_typ == _FX_NN_U32) {
            _FX_IMPLEMENT_NONZERO_COUNT(int32_t)
        } else if (inp_typ == _FX_NN_I64 || inp_typ == _FX_NN_U64) {
            _FX_IMPLEMENT_NONZERO_COUNT(int64_t)
        } else {
            assert(inp_typ == _FX_NN_FP32);
            _FX_IMPLEMENT_NONZERO_COUNT(float)
        }
        cbuf[task_id] = nz;
    }

    cofs[0] = 0;
    for (int_ i = 0; i < ntasks; i++)
        cofs[i+1] = cofs[i] + cbuf[i];
    out_total = cofs[ntasks];
    new_out_total_bytes = out_total*ndims*esz;
    if (out_total_bytes < new_out_total_bytes) {
        status = fx_make_arr(1, &new_out_total_bytes, 1, 0, 0, 0, &out_buf);
        if (status < 0)
            return status;
    } else {
        fx_copy_arr(out_buf0, &out_buf);
    }

    fx_copy_arr(&out_buf, &out_data);
    out_data.dim[0].size = out_total*ndims;
    out_data.dim[0].step = esz;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ i0 = task_id*inp_total/ntasks, i1 = (task_id+1)*inp_total/ntasks;
        int64_t* outptr = (int64_t*)out_data.data + cofs[task_id];

        #undef _FX_IMPLEMENT_NONZERO_STORE
        #define _FX_IMPLEMENT_NONZERO_STORE(typ) \
        const typ* inptr = (const typ*)(inp_data->data); \
        for (; i0 < i1; i0++) \
            if (inptr[i0] != 0) { \
                int_ idx = i0; \
                for (int_ j = ndims-1; j >= 0; j--) { \
                    int_ sub_idx = idx / inp_shape[j]; \
                    int_ k = idx - sub_idx*inp_shape[j]; \
                    outptr[j*out_total] = k; \
                    idx = sub_idx; \
                } \
                outptr++; \
            }

        if (inp_typ == _FX_NN_U8 || inp_typ == _FX_NN_I8 || inp_typ == _FX_NN_Bool) {
            _FX_IMPLEMENT_NONZERO_STORE(int8_t)
        } else if (inp_typ == _FX_NN_I32 || inp_typ == _FX_NN_U32) {
            _FX_IMPLEMENT_NONZERO_STORE(int32_t)
        } else if (inp_typ == _FX_NN_I64 || inp_typ == _FX_NN_U64) {
            _FX_IMPLEMENT_NONZERO_STORE(int64_t)
        } else {
            assert(inp_typ == _FX_NN_FP32);
            _FX_IMPLEMENT_NONZERO_STORE(float)
        }
    }

    fx_result->t0 = out_total;
    // incref's are already called inside fx_copy_arr,
    // so here we just copy the headers as plain structures
    fx_result->t1 = out_data;
    fx_result->t2 = out_buf;

    return status;
}

fun run_nonzero(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_NonZero {t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val inp_shape = inp.shape.shape
    val ndims = inp_shape.size()
    val out_bufidx = model.bufidxs[t_out]
    val out_buf = model.buffers[out_bufidx]
    val (out_total, out_data, out_buf) = run_nonzero(inp_shape, inp.data, out_buf, *model.ntasks)
    model.buffers[out_bufidx] = out_buf
    model.tensors[t_out] = Ast.nntensor_t {
        shape=Ast.nnshape_t {shape=[ndims, out_total],
        layout=Ast.NN_Layout_NC}, data=Ast.NN_Data_I64(out_data)
        }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
