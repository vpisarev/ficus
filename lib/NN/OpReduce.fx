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

@ccode {
static bool _fx_cmp_lt_i8(const void* a, const void* b, void* userdata) {
    const int8_t* arr = (const int8_t*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] < arr[ib];
}

static bool _fx_cmp_gt_i8(const void* a, const void* b, void* userdata) {
    const int8_t* arr = (const int8_t*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] > arr[ib];
}

static bool _fx_cmp_lt_u8(const void* a, const void* b, void* userdata) {
    const uint8_t* arr = (const uint8_t*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] < arr[ib];
}

static bool _fx_cmp_gt_u8(const void* a, const void* b, void* userdata) {
    const uint8_t* arr = (const uint8_t*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] > arr[ib];
}

static bool _fx_cmp_lt_i32(const void* a, const void* b, void* userdata) {
    const int32_t* arr = (const int32_t*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] < arr[ib];
}

static bool _fx_cmp_gt_i32(const void* a, const void* b, void* userdata) {
    const int32_t* arr = (const int32_t*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] > arr[ib];
}

static bool _fx_cmp_lt_i64(const void* a, const void* b, void* userdata) {
    const int64_t* arr = (const int64_t*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] < arr[ib];
}

static bool _fx_cmp_gt_i64(const void* a, const void* b, void* userdata) {
    const int64_t* arr = (const int64_t*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] > arr[ib];
}

static bool _fx_cmp_lt_f32(const void* a, const void* b, void* userdata) {
    const float* arr = (const float*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] < arr[ib];
}

static bool _fx_cmp_gt_f32(const void* a, const void* b, void* userdata) {
    const float* arr = (const float*)userdata;
    int ia = *(const int*)a, ib = *(const int*)b;
    return arr[ia] > arr[ib];
}
}

fun run_top_k(inp_shape_: int [], inp_data_: Ast.nndata_t,
              out_shape_: int [], out_data_: Ast.nndata_t,
              out_ind_data_: Ast.nndata_t, axis: int,
              largest: bool, sorted: bool,
              K: int, ntasks: int): void
@ccode {
    enum {_FX_TOPK_MAX_DIMS=5};
    int_ ndims = inp_shape_->dim[0].size;
    int inp_typ = inp_data_->tag, out_typ = out_data_->tag;
    const int_* inp_shape = (const int_*)inp_shape_->data;
    const int_* out_shape = (const int_*)out_shape_->data;
    int_ inp_step[_FX_TOPK_MAX_DIMS], out_step[_FX_TOPK_MAX_DIMS];
    fx_arr_t* inp_data = &inp_data_->u.NN_Data_I8;
    fx_arr_t* out_data = &out_data_->u.NN_Data_I8;
    fx_arr_t* out_ind_data = &out_ind_data_->u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;
    int_ nslices = 1;
    int_ len = inp_shape[axis];
    int_ len_aligned = (len + 15) & -16;
    char* all_sort_bufs;
    volatile int status = FX_OK;
    fx_less_t cmpfunc =
        inp_typ == _FX_NN_I8 ? (largest ? _fx_cmp_gt_i8 : _fx_cmp_lt_i8) :
        inp_typ == _FX_NN_U8 ? (largest ? _fx_cmp_gt_u8 : _fx_cmp_lt_u8) :
        inp_typ == _FX_NN_I32 ? (largest ? _fx_cmp_gt_i32 : _fx_cmp_lt_i32) :
        inp_typ == _FX_NN_I64 ? (largest ? _fx_cmp_gt_i64 : _fx_cmp_lt_i64) :
        inp_typ == _FX_NN_FP32 ? (largest ? _fx_cmp_gt_f32 : _fx_cmp_lt_f32) : 0;

    if (!cmpfunc)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (inp_typ != out_typ)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (inp_typ != _FX_NN_FP32 &&
        inp_typ != _FX_NN_I32 &&
        inp_typ != _FX_NN_I64 &&
        inp_typ != _FX_NN_I8 &&
        inp_typ != _FX_NN_U8 &&
        out_ind_data_->tag != _FX_NN_I64)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (ndims > _FX_TOPK_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims != out_shape_->dim[0].size)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (K > inp_shape[axis])
        K = inp_shape[axis];

    for (int_ i = ndims - 1; i >= 0; i--) {
        inp_step[i] = i == ndims - 1 ? 1 : inp_step[i+1]*inp_shape[i+1];
        out_step[i] = i == ndims - 1 ? 1 : out_step[i+1]*out_shape[i+1];
        if (i != axis) {
            if (inp_shape[i] != out_shape[i])
                return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
            nslices *= inp_shape[i];
        } else if (out_shape[i] != K) {
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        }
    }
    if (nslices*len <= 100000)
        ntasks = 1;
    all_sort_bufs = (char*)fx_malloc(len_aligned*(esz + sizeof(int))*ntasks);
    if (!all_sort_bufs)
        return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);

    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++)
    {
        char* sort_buf0 = all_sort_bufs + task_id*len_aligned*(esz + sizeof(int));
        int* sort_buf_idx = (int*)(sort_buf0 + len_aligned*esz);
        int_ slice0 = task_id*nslices/ntasks, slice1 = (task_id + 1)*nslices/ntasks;
        int_ inp_step_a = inp_step[axis], out_step_a = out_step[axis];
        for (;slice0 < slice1 && status >= 0; slice0++) {
            int_ idx = slice0;
            int_ i, inp_ofs = 0, out_ofs = 0;
            int64_t* out_ind_ptr;

            for (i = ndims-1; i >= 0; i--)
                if (i != axis) {
                    int_ prev_idx = idx / inp_shape[i];
                    idx -= prev_idx*inp_shape[i];
                    inp_ofs += inp_step[i]*idx;
                    out_ofs += out_step[i]*idx;
                    idx = prev_idx;
                }
            #undef _FX_NN_TOPK_INIT_SORTBUF
            #define _FX_NN_TOPK_INIT_SORTBUF(typ) \
                typ* inptr = (typ*)inp_data->data + inp_ofs; \
                typ* sort_buf = (typ*)sort_buf0; \
                for (i = 0; i < len; i++) { \
                    sort_buf[i] = inptr[i*inp_step_a]; \
                    sort_buf_idx[i] = (int)i; \
                }
            if (esz == 1) {
                _FX_NN_TOPK_INIT_SORTBUF(int8_t)
            } else if (esz == 2) {
                _FX_NN_TOPK_INIT_SORTBUF(int16_t)
            } else if (esz == 4) {
                _FX_NN_TOPK_INIT_SORTBUF(int32_t)
            } else {
                assert(esz == 8);
                _FX_NN_TOPK_INIT_SORTBUF(int64_t)
            }

            status = fx_qsort(sort_buf_idx, len, esz, cmpfunc, sort_buf0, K);
            if (status < 0)
                break;
            out_ind_ptr = (int64_t*)out_ind_data->data + out_ofs;

            #undef _FX_NN_TOPK_STORE
            #define _FX_NN_TOPK_STORE(typ) \
                typ* outptr = (typ*)out_data->data + out_ofs; \
                typ* sort_buf = (typ*)sort_buf0; \
                for (i = 0; i < K; i++) { \
                    int j = sort_buf_idx[i]; \
                    out_ind_ptr[i] = j; \
                    outptr[i] = sort_buf[j]; \
                }
            if (esz == 1) {
                _FX_NN_TOPK_STORE(int8_t)
            } else if (esz == 2) {
                _FX_NN_TOPK_STORE(int16_t)
            } else if (esz == 4) {
                _FX_NN_TOPK_STORE(int32_t)
            } else {
                assert(esz == 8);
                _FX_NN_TOPK_STORE(int64_t)
            }

        }
    }
    if (status < 0)
        FX_SET_EXN_FAST(status);
    fx_free(all_sort_bufs);
    return status;
}

fun run_top_k(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_TopK {axis, largest, sorted, t_inp, t_K, t_out, t_out_ind} =>
    val inp = model.get_tensor(t_inp)
    val inp_shape = inp.shape.shape
    val out = model.get_tensor(t_out)
    val out_ind = model.get_tensor(t_out_ind)
    val tK = model.get_tensor(t_K).data
    assert(`tK.total() == 1`)
    val K = match tK {
        | Ast.NN_Data_I64 tK_data => int(tK_data[0])
        | _ => throw Ast.NNError("incorrect type of K tensor in topK: INT64 is expected\n")
        }
    assert(`K >= 0`)
    val ndims = inp_shape.size()
    val axis = Ast.normalize_axis(axis, ndims)
    assert(`out.shape.shape == out_ind.shape.shape`)
    run_top_k(inp_shape, inp.data, out.shape.shape, out.data, out_ind.data,
              axis, largest, sorted, K, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
