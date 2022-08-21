/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

@ccode {
#include "ficus_nn_common.h"
typedef int64_t _fx_coord_t;
}

fun run_nonzero(inp: Ast.nntensor_t, out_buf0: Ast.nnbuf_t, ntasks: int):
    (int, int64 [], Ast.nnbuf_t)
@ccode {
    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    int_ ndims = inp_shape_->dim[0].size;
    const int_* inp_shape = (const int_*)inp_shape_->data;
    int_* cbuf = (int_*)alloca((ntasks*2 + 1)*sizeof(cbuf[0]));
    int_* cofs = cbuf + ntasks;
    int inp_typ = inp->data.tag, out_typ = FX_I64;
    size_t esz = sizeof(_fx_coord_t);
    fx_arr_t out_buf, out_data;
    int_ inp_total = inp_data->dim[0].size;
    int_ out_total_bytes = out_buf0->dim[0].size*out_buf0->dim[0].step;
    int_ new_out_total_bytes, out_total;
    int status = FX_OK;
    if (inp_total < 1000000) {
        ntasks = 1;
    }

    if (inp_typ != FX_I8 && inp_typ != FX_U8 && inp_typ != FX_Bool &&
        inp_typ != FX_I32 && inp_typ != FX_U32 &&
        inp_typ != FX_I64 && inp_typ != FX_U64 &&
        inp_typ != FX_F32)
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
        if (inp_typ == FX_U8 || inp_typ == FX_I8 || inp_typ == FX_Bool) {
            _FX_IMPLEMENT_NONZERO_COUNT(int8_t)
        } else if (inp_typ == FX_I32 || inp_typ == FX_U32) {
            _FX_IMPLEMENT_NONZERO_COUNT(int32_t)
        } else if (inp_typ == FX_I64 || inp_typ == FX_U64) {
            _FX_IMPLEMENT_NONZERO_COUNT(int64_t)
        } else {
            assert(inp_typ == FX_F32);
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

        if (inp_typ == FX_U8 || inp_typ == FX_I8 || inp_typ == FX_Bool) {
            _FX_IMPLEMENT_NONZERO_STORE(int8_t)
        } else if (inp_typ == FX_I32 || inp_typ == FX_U32) {
            _FX_IMPLEMENT_NONZERO_STORE(int32_t)
        } else if (inp_typ == FX_I64 || inp_typ == FX_U64) {
            _FX_IMPLEMENT_NONZERO_STORE(int64_t)
        } else {
            assert(inp_typ == FX_F32);
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
    val ndims = inp.shape.shape.size()
    val out_bufidx = model.bufidxs[t_out]
    val out_buf = model.buffers[out_bufidx]
    val (out_total, out_data, out_buf) = run_nonzero(inp, out_buf, *model.ntasks)
    model.buffers[out_bufidx] = out_buf
    model.tensors[t_out] = Ast.nntensor_t {
        shape=Ast.nnshape_t {shape=[ndims, out_total],
        layout=Ast.NN_Layout_ND}, data=Ast.NN_Data_I64(out_data)
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

fun run_top_k(inp: Ast.nntensor_t, out: Ast.nntensor_t,
              out_ind: Ast.nntensor_t, axis: int,
              largest: bool, sorted: bool,
              K: int, ntasks: int): void
@ccode {
    enum {_FX_TOPK_MAX_DIMS=5};
    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    fx_arr_t* out_ind_data = &out_ind->data.u.NN_Data_I8;
    int_ ndims = inp_shape_->dim[0].size;
    int inp_typ = inp->data.tag, out_typ = out->data.tag;
    const int_* inp_shape = (const int_*)inp->shape.shape.data;
    const int_* out_shape = (const int_*)out->shape.shape.data;
    int_ inp_step[_FX_TOPK_MAX_DIMS], out_step[_FX_TOPK_MAX_DIMS];
    size_t esz = inp_data->dim[0].step;
    int_ nslices = 1;
    int_ len = inp_shape[axis];
    int_ len_aligned = (len + 15) & -16;
    char* all_sort_bufs;
    volatile int status = FX_OK;
    fx_less_t cmpfunc =
        inp_typ == FX_I8 ? (largest ? _fx_cmp_gt_i8 : _fx_cmp_lt_i8) :
        inp_typ == FX_U8 ? (largest ? _fx_cmp_gt_u8 : _fx_cmp_lt_u8) :
        inp_typ == FX_I32 ? (largest ? _fx_cmp_gt_i32 : _fx_cmp_lt_i32) :
        inp_typ == FX_I64 ? (largest ? _fx_cmp_gt_i64 : _fx_cmp_lt_i64) :
        inp_typ == FX_F32 ? (largest ? _fx_cmp_gt_f32 : _fx_cmp_lt_f32) : 0;

    if (!cmpfunc)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (inp_typ != out_typ)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (inp_typ != FX_F32 &&
        inp_typ != FX_I32 &&
        inp_typ != FX_I64 &&
        inp_typ != FX_I8 &&
        inp_typ != FX_U8 &&
        out_ind->data.tag != FX_I64)
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
    val out = model.get_tensor(t_out)
    val out_ind = model.get_tensor(t_out_ind)
    val tK = model.get_tensor(t_K).data
    assert(`tK.total() == 1`)
    val K = match tK {
        | Ast.NN_Data_I64 tK_data => int(tK_data[0])
        | _ => throw Ast.NNError("incorrect type of K tensor in topK: INT64 is expected\n")
        }
    assert(`K >= 0`)
    val ndims = inp.shape.shape.size()
    val axis = Ast.normalize_axis(axis, ndims)
    assert(`out.shape.shape == out_ind.shape.shape`)
    run_top_k(inp, out, out_ind, axis, largest, sorted, K, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

@ccode {

enum {
    _FX_NN_REDUCE_L1=1,
    _FX_NN_REDUCE_L2,
    _FX_NN_REDUCE_LOG_SUM,
    _FX_NN_REDUCE_LOG_SUM_EXP,
    _FX_NN_REDUCE_MAX,
    _FX_NN_REDUCE_MEAN,
    _FX_NN_REDUCE_MIN,
    _FX_NN_REDUCE_PROD,
    _FX_NN_REDUCE_SUM,
    _FX_NN_REDUCE_SUM_SQUARE
};

#define _FX_REDUCE_OP_MIN(a, b) ((a) <= (b) ? (a) : (b))
#define _FX_REDUCE_OP_MAX(a, b) ((a) >= (b) ? (a) : (b))
#define _FX_REDUCE_OP_SUM(a, b) ((a) + (b))
#define _FX_REDUCE_OP_SUM_ABSF(a, b) ((a) + fabs(b))
#define _FX_REDUCE_OP_SUM_ABS(a, b) ((a) + ((b) >= 0 ? (b) : -(b)))
#define _FX_REDUCE_OP_SUM_SQR(a, b) ((a) + (b)*(b))
#define _FX_REDUCE_OP_SUM_EXP(a, b) ((a) + exp(b))
#define _FX_REDUCE_OP_PROD(a, b) ((a) * (b))

#define _FX_REDUCE_IMPL(typ, acctyp, suffix, op, val0) \
static void _fx_reduce_##suffix(const char* inptr0, int_ ystep, int_ xstep, \
                                int_ nrows, int_ ncols, char* accptr, bool init) { \
    acctyp acc = init ? (acctyp)val0 : *(acctyp*)accptr; \
    if (xstep == 1) { \
        for (int_ i = 0; i < nrows; i++) { \
            const typ* inptr = (const typ*)inptr0 + ystep*i; \
            for (int_ j = 0; j < ncols; j++) { \
                acctyp x = (acctyp)inptr[j]; \
                acc = op(acc, x); \
            } \
        } \
    } else { \
        for (int_ i = 0; i < nrows; i++) { \
            const typ* inptr = (const typ*)inptr0 + ystep*i; \
            for (int_ j = 0; j < ncols; j++) { \
                acctyp x = (acctyp)inptr[j*xstep]; \
                acc = op(acc, x); \
            } \
        } \
    } \
    *(acctyp*)accptr = acc; \
}

_FX_REDUCE_IMPL(int8_t, int32_t, max_i8, _FX_REDUCE_OP_MAX, -128)
_FX_REDUCE_IMPL(int8_t, int32_t, min_i8, _FX_REDUCE_OP_MIN, 127)
_FX_REDUCE_IMPL(uint8_t, uint32_t, max_u8, _FX_REDUCE_OP_MAX, 0)
_FX_REDUCE_IMPL(uint8_t, uint32_t, min_u8, _FX_REDUCE_OP_MIN, 255)
_FX_REDUCE_IMPL(int32_t, int32_t, max_i32, _FX_REDUCE_OP_MAX, INT_MIN)
_FX_REDUCE_IMPL(int32_t, int32_t, min_i32, _FX_REDUCE_OP_MIN, INT_MAX)
_FX_REDUCE_IMPL(uint32_t, uint32_t, max_u32, _FX_REDUCE_OP_MAX, 0)
_FX_REDUCE_IMPL(uint32_t, uint32_t, min_u32, _FX_REDUCE_OP_MIN, UINT_MAX)
_FX_REDUCE_IMPL(int64_t, int64_t, max_i64, _FX_REDUCE_OP_MAX, 0x8000000000000000LL)
_FX_REDUCE_IMPL(int64_t, int64_t, min_i64, _FX_REDUCE_OP_MIN, 0x7fffffffffffffffLL)
_FX_REDUCE_IMPL(uint64_t, uint64_t, max_u64, _FX_REDUCE_OP_MAX, 0)
_FX_REDUCE_IMPL(uint64_t, uint64_t, min_u64, _FX_REDUCE_OP_MIN, 0xffffffffffffffffUL)
_FX_REDUCE_IMPL(float, float, max_f32, _FX_REDUCE_OP_MAX, -FLT_MAX)
_FX_REDUCE_IMPL(float, float, min_f32, _FX_REDUCE_OP_MIN, FLT_MAX)

_FX_REDUCE_IMPL(int32_t, int64_t, sum_i32, _FX_REDUCE_OP_SUM, 0)
_FX_REDUCE_IMPL(uint32_t, uint64_t, sum_u32, _FX_REDUCE_OP_SUM, 0)
_FX_REDUCE_IMPL(int64_t, int64_t, sum_i64, _FX_REDUCE_OP_SUM, 0)
_FX_REDUCE_IMPL(float, double, sum_f32, _FX_REDUCE_OP_SUM, 0.f)

_FX_REDUCE_IMPL(int32_t, int64_t, sum_abs_i32, _FX_REDUCE_OP_SUM_ABS, 0)
_FX_REDUCE_IMPL(int64_t, int64_t, sum_abs_i64, _FX_REDUCE_OP_SUM_ABS, 0)
_FX_REDUCE_IMPL(float, double, sum_abs_f32, _FX_REDUCE_OP_SUM_ABSF, 0.f)

_FX_REDUCE_IMPL(int32_t, uint64_t, sum_sqr_i32, _FX_REDUCE_OP_SUM_SQR, 0)
_FX_REDUCE_IMPL(uint32_t, uint64_t, sum_sqr_u32, _FX_REDUCE_OP_SUM_SQR, 0)
_FX_REDUCE_IMPL(int64_t, uint64_t, sum_sqr_i64, _FX_REDUCE_OP_SUM_SQR, 0)
_FX_REDUCE_IMPL(uint64_t, uint64_t, sum_sqr_u64, _FX_REDUCE_OP_SUM_SQR, 0)
_FX_REDUCE_IMPL(float, double, sum_sqr_f32, _FX_REDUCE_OP_SUM_SQR, 0.)

_FX_REDUCE_IMPL(float, double, sum_exp_f32, _FX_REDUCE_OP_SUM_EXP, 0.)

_FX_REDUCE_IMPL(int32_t, int64_t, prod_i32, _FX_REDUCE_OP_PROD, 1)
_FX_REDUCE_IMPL(uint32_t, uint64_t, prod_u32, _FX_REDUCE_OP_PROD, 1)
_FX_REDUCE_IMPL(int64_t, int64_t, prod_i64, _FX_REDUCE_OP_PROD, 1)
_FX_REDUCE_IMPL(uint64_t, uint64_t, prod_u64, _FX_REDUCE_OP_PROD, 1)
_FX_REDUCE_IMPL(float, double, prod_f32, _FX_REDUCE_OP_PROD, 1.f)

static void _fx_finit_copy_i8(const char* inptr, char* outptr, double param)
{ *outptr = *inptr; }

static void _fx_finit_copy_i32(const char* inptr, char* outptr, double param)
{ *(int*)outptr = *(int*)inptr; }

static void _fx_finit_copy_i64(const char* inptr, char* outptr, double param)
{ *(int64_t*)outptr = *(int64_t*)inptr; }

static void _fx_finit_cast_f64f32(const char* inptr, char* outptr, double param)
{ *(float*)outptr = (float)*(double*)inptr; }

static void _fx_finit_cast_i64i32(const char* inptr, char* outptr, double param)
{ *(int32_t*)outptr = (int32_t)(*(int64_t*)inptr); }

// [TODO] check for overflow on Windows where 'long int' is 32-bit integer.
static void _fx_finit_sqrt_u64u32(const char* inptr, char* outptr, double param)
{ *(uint32_t*)outptr = (uint32_t)lrint(sqrt((double)*(uint64_t*)inptr)); }

static void _fx_finit_sqrt_u64(const char* inptr, char* outptr, double param)
{ *(uint64_t*)outptr = (uint64_t)lrint(sqrt((double)*(uint64_t*)inptr)); }

static void _fx_finit_sqrt_f64f32(const char* inptr, char* outptr, double param)
{ *(float*)outptr = (float)sqrt(*(double*)inptr); }

static void _fx_finit_log_f64f32(const char* inptr, char* outptr, double param)
{ *(float*)outptr = (float)log(*(double*)inptr); }

static void _fx_finit_scale_f64f32(const char* inptr, char* outptr, double param)
{ *(float*)outptr = (float)(*(double*)inptr*param); }

static void _fx_finit_scale_i64i32(const char* inptr, char* outptr, double param)
{ *(int32_t*)outptr = (int32_t)lrint(*(int64_t*)inptr*param); }

// [TODO] check for overflow on Windows where 'long int' is 32-bit integer.
static void _fx_finit_scale_u64u32(const char* inptr, char* outptr, double param)
{ *(uint32_t*)outptr = (uint32_t)lrint(*(uint64_t*)inptr*param); }

static void _fx_finit_scale_i64(const char* inptr, char* outptr, double param)
{ *(int64_t*)outptr = (int64_t)lrint(*(int64_t*)inptr*param); }

static void _fx_finit_scale_u64(const char* inptr, char* outptr, double param)
{ *(uint64_t*)outptr = (uint64_t)lrint(*(uint64_t*)inptr*param); }

#ifdef _FX_NN_ENABLE_FP16
_FX_REDUCE_IMPL(fx_f16, double, sum_f16, _FX_REDUCE_OP_SUM, 0.f)
_FX_REDUCE_IMPL(fx_f16, double, prod_f16, _FX_REDUCE_OP_PROD, 1.f)

static void _fx_finit_cast_f64f16(const char* inptr, char* outptr, double param)
{ *(fx_f16*)outptr = FX_FLOAT16((float)*(double*)inptr); }
static void _fx_finit_scale_f64f16(const char* inptr, char* outptr, double param)
{ *(fx_f16*)outptr = FX_FLOAT16((float)(*(double*)inptr*param)); }
#endif

typedef void (*_fx_reduce_func_t)(const char* inptr, int_ ystep, int_ xstep,
                                int_ nrows, int_ ncols, char* outptr, bool init);
typedef void (*_fx_reduce_finit_func_t)(const char* acc, char* outptr, double param);
}

fun run_reduce(inp: Ast.nntensor_t, out: Ast.nntensor_t,
               axes_: int [], keepdims: bool,
               reduce_op_: Ast.nnreduce_t, ntasks: int): void
@ccode {
    enum {_FX_REDUCE_MAX_DIMS=5};
    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    int_ inp_ndims = inp_shape_->dim[0].size;
    int_ out_ndims = out_shape_->dim[0].size;
    int_ naxes = axes_->dim[0].size;
    if (naxes == 0)
        naxes = inp_ndims;
    const int_* axes = (const int_*)(axes_->data);
    int inp_typ = inp->data.tag, out_typ = out->data.tag;
    const int_* inp_shape = (const int_*)inp_shape_->data;
    const int_* out_shape = (const int_*)out_shape_->data;
    int_ inp_step[_FX_REDUCE_MAX_DIMS], out_step[_FX_REDUCE_MAX_DIMS];
    int_ dim_map[_FX_REDUCE_MAX_DIMS] = {0, 1, 2, 3, 4};
    int_ reduce_shape[_FX_REDUCE_MAX_DIMS], reduce_step[_FX_REDUCE_MAX_DIMS];
    bool axes_mask[_FX_REDUCE_MAX_DIMS]={false, false, false, false, false};
    size_t esz = inp_data->dim[0].step;
    int_ out_total = 1, reduce_total = 1;
    int reduce_op = reduce_op_->tag;
    _fx_reduce_finit_func_t finit_func = 0;
    _fx_reduce_func_t reduce_func = 0;
    double param = 1.;

    if (inp_typ != out_typ)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (inp_ndims > _FX_REDUCE_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if ((keepdims && inp_ndims != out_ndims) ||
        (!keepdims && inp_ndims != out_ndims + naxes))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    for (int_ i = inp_ndims - 1; i >= 0; i--) {
        inp_step[i] = i == inp_ndims - 1 ? 1 : inp_step[i+1]*inp_shape[i+1];
    }
    for (int_ i = out_ndims - 1; i >= 0; i--) {
        out_step[i] = i == out_ndims - 1 ? 1 : out_step[i+1]*out_shape[i+1];
        out_total *= out_shape[i];
    }
    for (int_ i = 0; i < naxes; i++) {
        int_ a = axes ? axes[i] : i;
        if (a < 0) a += inp_ndims;
        if (a < 0 || a >= inp_ndims)
            return FX_SET_EXN_FAST(FX_EXN_OutOfRangeError);
        if (axes_mask[a])
            return FX_SET_EXN_FAST(FX_EXN_BadArgError);
        axes_mask[a] = true;
        reduce_total *= inp_shape[a];
    }

    for (int_ i = 0; i < out_ndims; i++)
        dim_map[i] = i;

    {
    int_ j = 0, k = 0;
    for (int_ i = 0; i < inp_ndims; i++) {
        if (!axes_mask[i]) {
            if (!keepdims) {
                dim_map[k] = i;
                k++;
            }
            continue;
        }
        reduce_shape[j] = inp_shape[i];
        reduce_step[j] = inp_step[i];
        j++;
    }
    }

    if (reduce_op == _FX_NN_REDUCE_MIN || reduce_op == _FX_NN_REDUCE_MAX) {
        if (inp_typ == FX_I8) {
            reduce_func = reduce_op == _FX_NN_REDUCE_MIN ?
                _fx_reduce_min_i8 : _fx_reduce_max_i8;
            finit_func = _fx_finit_copy_i8;
        } else if (inp_typ == FX_U8) {
            reduce_func = reduce_op == _FX_NN_REDUCE_MIN ?
                _fx_reduce_min_u8 : _fx_reduce_max_u8;
            finit_func = _fx_finit_copy_i8; // copy_i* === copy_u*
        } else if (inp_typ == FX_I32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_MIN ?
                _fx_reduce_min_i32 : _fx_reduce_max_i32;
            finit_func = _fx_finit_copy_i32;
        } else if (inp_typ == FX_U32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_MIN ?
                _fx_reduce_min_u32 : _fx_reduce_max_u32;
            finit_func = _fx_finit_copy_i32;
        } else if (inp_typ == FX_I64) {
            reduce_func = reduce_op == _FX_NN_REDUCE_MIN ?
                _fx_reduce_min_i64 : _fx_reduce_max_i64;
            finit_func = _fx_finit_copy_i64;
        } else if (inp_typ == FX_U64) {
            reduce_func = reduce_op == _FX_NN_REDUCE_MIN ?
                _fx_reduce_min_u64 : _fx_reduce_max_u64;
            finit_func = _fx_finit_copy_i64;
        } else if (inp_typ == FX_F32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_MIN ?
                _fx_reduce_min_f32 : _fx_reduce_max_f32;
            finit_func = _fx_finit_copy_i32;
        }
    } else if (reduce_op == _FX_NN_REDUCE_SUM ||
               reduce_op == _FX_NN_REDUCE_PROD ||
               reduce_op == _FX_NN_REDUCE_MEAN) {
        if (inp_typ == FX_I32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_PROD ?
                _fx_reduce_prod_i32 : _fx_reduce_sum_i32;
            finit_func = reduce_op == _FX_NN_REDUCE_MEAN ?
                _fx_finit_scale_i64i32 : _fx_finit_cast_i64i32;
        } else if (inp_typ == FX_U32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_PROD ?
                _fx_reduce_prod_u32 : _fx_reduce_sum_u32;
            finit_func = reduce_op == _FX_NN_REDUCE_MEAN ?
                _fx_finit_scale_u64u32 : _fx_finit_cast_i64i32 ;
        } else if (inp_typ == FX_I64) {
            reduce_func = reduce_op == _FX_NN_REDUCE_PROD ?
                _fx_reduce_prod_i64 : _fx_reduce_sum_i64;
            finit_func = reduce_op == _FX_NN_REDUCE_MEAN ?
                _fx_finit_scale_i64 : _fx_finit_copy_i64;
        } else if (inp_typ == FX_U64) {
            reduce_func = reduce_op == _FX_NN_REDUCE_PROD ?
                _fx_reduce_prod_u64 : _fx_reduce_sum_i64; // sum_i64 === sum_u64
            finit_func = reduce_op == _FX_NN_REDUCE_MEAN ?
                _fx_finit_scale_u64 : _fx_finit_copy_i64;
        } else if (inp_typ == FX_F32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_PROD ?
                _fx_reduce_prod_f32 : _fx_reduce_sum_f32;
            finit_func = reduce_op == _FX_NN_REDUCE_MEAN ?
                _fx_finit_scale_f64f32 : _fx_finit_cast_f64f32;
        }
    #ifdef _FX_NN_ENABLE_FP16
        else if (inp_typ == FX_F16) {
            reduce_func = reduce_op == _FX_NN_REDUCE_PROD ?
                _fx_reduce_prod_f16 : _fx_reduce_sum_f16;
            finit_func = reduce_op == _FX_NN_REDUCE_MEAN ?
                _fx_finit_scale_f64f16 : _fx_finit_cast_f64f16;
        }
    #endif
        if (reduce_op == _FX_NN_REDUCE_MEAN)
            param = reduce_total == 0 ? 0. : 1./reduce_total;
    } else if (reduce_op == _FX_NN_REDUCE_L1 ||
               reduce_op == _FX_NN_REDUCE_L2 ||
               reduce_op == _FX_NN_REDUCE_SUM_SQUARE) {
        if (inp_typ == FX_I32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_L1 ?
                _fx_reduce_sum_abs_i32 : _fx_reduce_sum_sqr_i32;
            finit_func = reduce_op == _FX_NN_REDUCE_L2 ?
                _fx_finit_sqrt_u64u32 : _fx_finit_cast_i64i32;
        } else if (inp_typ == FX_U32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_L1 ?
                _fx_reduce_sum_u32 : _fx_reduce_sum_sqr_u32;
            finit_func = reduce_op == _FX_NN_REDUCE_L2 ?
                _fx_finit_sqrt_u64u32 : _fx_finit_cast_i64i32;
        } else if (inp_typ == FX_I64) {
            reduce_func = reduce_op == _FX_NN_REDUCE_L1 ?
                _fx_reduce_sum_abs_i64 : _fx_reduce_sum_sqr_i64;
            finit_func = reduce_op == _FX_NN_REDUCE_L2 ?
                _fx_finit_sqrt_u64 : _fx_finit_copy_i64;
        } else if (inp_typ == FX_U64) {
            reduce_func = reduce_op == _FX_NN_REDUCE_L1 ?
                _fx_reduce_sum_i64 : _fx_reduce_sum_sqr_u64;
            finit_func = reduce_op == _FX_NN_REDUCE_L2 ?
                _fx_finit_sqrt_u64 : _fx_finit_copy_i64;
        } else if (inp_typ == FX_F32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_L1 ?
                _fx_reduce_sum_abs_f32 : _fx_reduce_sum_sqr_f32;
            finit_func = reduce_op == _FX_NN_REDUCE_L2 ?
                _fx_finit_sqrt_f64f32 : _fx_finit_cast_f64f32;
        }
    } else if (reduce_op == _FX_NN_REDUCE_LOG_SUM ||
               reduce_op == _FX_NN_REDUCE_LOG_SUM_EXP) {
        if (inp_typ == FX_F32) {
            reduce_func = reduce_op == _FX_NN_REDUCE_LOG_SUM_EXP ?
                _fx_reduce_sum_exp_f32 : _fx_reduce_sum_f32;
            finit_func = _fx_finit_log_f64f32;
        }
    }

    if (!reduce_func || !finit_func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (out_total == 1 || out_total*reduce_total < 100000)
        ntasks = 1;

    /*printf("esz=%d, reduce_func=%p, sum_f32=%p, reduce_total=%d, out_total=%d\n",
        (int)esz, reduce_func, _fx_reduce_sum_f32,
        (int)reduce_total, (int)out_total);*/

    #pragma omp parallel for num_threads(ntasks)
    for (int task_id = 0; task_id < ntasks; task_id++)
    {
        char acc[8];
        int_ out_idx0 = task_id*out_total/ntasks, out_idx1 = (task_id + 1)*out_total/ntasks;
        int_ ystep = naxes >= 2 ? reduce_step[naxes - 2] : 0;
        int_ xstep = naxes >= 1 ? reduce_step[naxes - 1] : 0;
        int_ nrows = naxes >= 2 ? reduce_shape[naxes - 2] : 1;
        int_ ncols = naxes >= 1 ? reduce_shape[naxes - 1] : 1;

        if (nrows > 1 && ystep == ncols*xstep)
        {
            ncols *= nrows;
            nrows = 1;
        }
        for (;out_idx0 < out_idx1; out_idx0++) {
            int_ idx = out_idx0;
            int_ i, j, inp_ofs = 0, out_ofs = 0;
            const char* inptr = inp_data->data;
            char* outptr = out_data->data;

            for (i = out_ndims-1; i >= 0; i--) {
                int_ prev_idx = idx / out_shape[i];
                idx -= prev_idx*out_shape[i];
                out_ofs += out_step[i]*idx;
                inp_ofs += inp_step[dim_map[i]]*idx;
                idx = prev_idx;
            }
            assert(out_idx0 == out_ofs);
            inptr += inp_ofs*esz;
            outptr += out_ofs*esz;

            //printf("out_ofs=%d, out_idx0=%d, inp_ofs=%d, ystep=%d, xstep=%d, nrows=%d, ncols=%d\n",
            //    (int)out_ofs, (int)out_idx0, (int)inp_ofs, (int)ystep, (int)xstep, (int)nrows, (int)ncols);

            for (j = 0; j < reduce_total; j += nrows*ncols) {
                int_ y, x;
                inp_ofs = 0;
                idx = j;
                for (i = naxes-1; i >= 0; i--) {
                    int_ prev_idx = idx / reduce_shape[i];
                    idx -= prev_idx*reduce_shape[i];
                    inp_ofs += reduce_step[i]*idx;
                    idx = prev_idx;
                }
                reduce_func(inptr + inp_ofs, ystep, xstep, nrows, ncols, acc, j == 0);
            }
            finit_func(acc, outptr, param);
            /*printf("acc=%.2f, param=%.2f, *outptr=%.2f\n",
                (reduce_op == _FX_NN_REDUCE_MAX || reduce_op == _FX_NN_REDUCE_MIN ?
                (double)*(float*)acc : *(double*)acc), param, *(float*)outptr);*/
        }
    }
    return FX_OK;
}

fun run_reduce(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Reduce {reduce_op, axes, keepdims, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_reduce(inp, out, axes, keepdims, reduce_op, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
