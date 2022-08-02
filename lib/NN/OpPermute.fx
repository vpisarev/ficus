/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast

@ccode {
#include "ficus_nn_common.h"
}

@private fun run_concat_(axis: int, inp_shape_: int [][], inp_data_: Ast.nndata_t [],
                         out_shape_: int [], out_data_: Ast.nndata_t, ntasks: int): void
@ccode {
    int_ ndims = out_shape_->dim[0].size, nslices = 1;
    const int_* out_shape = (const int_*)(out_shape_->data);
    size_t esz = out_data_->u.NN_Data_I8.dim[0].step;
    size_t slicesize = esz;
    size_t outstride = 0, totalsize = 0;
    int_ i, ninputs = inp_data_->dim[0].size;
    int_ input_size_axis = 0;
    for (i = ndims-1; i > axis; i--)
        slicesize *= out_shape[i];
    outstride = slicesize*out_shape[axis];
    for (i = 0; i < axis; i++)
        nslices *= out_shape[i];

    for (i = 0; i < ninputs; i++) {
        fx_arr_t* inp_shape_i_ = (fx_arr_t*)inp_shape_->data + i;
        const int_* inp_shape_i = (const int_*)(inp_shape_i_->data);
        size_t esz_i = ((_fx_nndata_t*)inp_data_->data + i)->u.NN_Data_I8.dim[0].step;
        size_t total_i = esz_i;
        if (esz_i != esz)
            return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
        if (inp_shape_i_->ndims != 1 || inp_shape_i_->dim[0].size != ndims)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        for (int_ j = 0; j < ndims; j++) {
            int_ sz_ij = inp_shape_i[j];
            if (j != axis) {
                if (out_shape[j] != sz_ij)
                    return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
            } else {
                input_size_axis += sz_ij;
            }
            total_i *= sz_ij;
        }
        totalsize += total_i;
    }

    #pragma omp parallel for num_threads(ntasks) if (totalsize > 10000000)
    for (i = 0; i < ninputs; i++) {
        char* outptr = out_data_->u.NN_Data_I8.data;
        fx_arr_t* inp_shape_i_ = (fx_arr_t*)inp_shape_->data + i;
        const int_* inp_shape_i = (const int_*)(inp_shape_i_->data);
        const char* inptr_i = ((_fx_nndata_t*)inp_data_->data + i)->u.NN_Data_I8.data;
        int_ sz_a;
        for (int_ j = 0; j < i; j++) {
            fx_arr_t* inp_shape_j_ = (fx_arr_t*)inp_shape_->data + j;
            const int_* inp_shape_j = (const int_*)(inp_shape_j_->data);
            sz_a = inp_shape_j[axis];
            outptr += slicesize*sz_a;
        }
        sz_a = inp_shape_i[axis];
        size_t slicesize_i = slicesize*sz_a;
        for (int_ j = 0; j < nslices; j++)
            memcpy(outptr + j*outstride, inptr_i + j*slicesize_i, slicesize_i);
    }
    return FX_OK;
}

fun run_concat(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Concat {axis, t_inp, t_out} =>
    val out = model.get_tensor(t_out)
    val out_shape = out.shape.shape
    val ndims = out_shape.size()
    var etyp_0 = out.data.elemtype()
    val (inp_data, inp_shape) = [@unzip for idx@i <- t_inp {
            val t = model.get_tensor(idx)
            val etyp_i = t.data.elemtype()
            assert(`etyp_0 == etyp_i`)
            (t.data, t.shape.shape)
        }]

    val axis = Ast.normalize_axis(axis, ndims)
    run_concat_(axis, inp_shape, inp_data, out_shape, out.data, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_gather(axis: int, inp: Ast.nntensor_t, ind: Ast.nntensor_t,
               out: Ast.nntensor_t, ntasks: int): void
@ccode {
    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* ind_shape_ = &ind->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    const int_* inp_shape = (const int_*)(inp_shape_->data);
    const int_* ind_shape = (const int_*)(ind_shape_->data);
    const int_* out_shape = (const int_*)(out_shape_->data);
    int_ r = inp_shape_->dim[0].size;
    int_ q = ind_shape_->dim[0].size;
    int_ ndims = out_shape_->dim[0].size;
    int_ pq = 1, nslices = 1;
    volatile int fx_status = FX_OK;
    size_t elemsize = inp->data.u.NN_Data_I8.dim[0].step;
    size_t slicesize = elemsize;
    size_t inp_stride, out_stride;
    const char* inptr0 = inp->data.u.NN_Data_I8.data;
    char* outptr0 = out->data.u.NN_Data_I8.data;
    const int* ind32 = 0;
    const int64_t* ind64 = 0;
    int_ s = inp_shape[axis];

    if (inp->data.tag <= 1)
        return FX_SET_EXN_FAST(FX_EXN_NullPtrError);

    if (inp->data.tag != out->data.tag)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    if (ind->data.tag == FX_I32) {
        ind32 = (const int*)ind->data.u.NN_Data_I8.data;
    }
    else if (ind->data.tag == FX_I64) {
        ind64 = (const int64_t*)ind->data.u.NN_Data_I8.data;
    } else {
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }

    if (ndims != q + r - 1)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    for(int_ i = 0; i < q; i++)
        pq *= ind_shape[i];

    for(int_ j = 0; j < r; j++) {
        int_ szj = inp_shape[j];
        if (j < axis)
            nslices *= szj;
        else if (j > axis)
            slicesize *= szj;
    }
    inp_stride = slicesize * s;
    out_stride = slicesize * pq;
    if(pq == 1 || slicesize*nslices*pq < 1000000)
        ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++) {
        int_ j0 = task_id*pq/ntasks, j1 = (task_id+1)*pq/ntasks;
        for (; j0 < j1; j0++) {
            int_ k = ind32 ? (int_)ind32[j0] : (int_)ind64[j0];
            char* outptr = outptr0 + j0*slicesize;
            const char* inptr = inptr0;
            for (int_ i = 0; i < nslices; i++, inptr += inp_stride, outptr += out_stride) {
                k += k < 0 ? s : 0;
                if (k < 0 || k >= s) {
                    fx_status = FX_EXN_OutOfRangeError; continue;
                }
                memcpy(outptr, inptr + k*slicesize, slicesize);
            }
        }
    }
    return fx_status >= 0 ? fx_status : FX_SET_EXN_FAST(fx_status);
}

fun run_gather(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Gather {axis, t_inp, t_ind, t_out} =>
    val inp = model.get_tensor(t_inp)
    val r = inp.shape.shape.size()
    val axis = Ast.normalize_axis(axis, r)
    val ind = model.get_tensor(t_ind)
    val out = model.get_tensor(t_out)
    run_gather(axis, inp, ind, out, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_shape(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Shape {start, end, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val inp_shape = inp.shape.shape
    val out = model.get_tensor(t_out)
    val ndims = inp_shape.size()
    val start = Ast.normalize_axis(start, ndims)
    val end = if end >= ndims {ndims} else {Ast.normalize_axis(end, ndims)}
    match out.data {
    | Ast.NN_Data_I64 out_data =>
        val out_size = out_data.size()
        assert(`out_size == end - start`)
        for i <- 0:out_size {
            out_data[i] = (inp_shape[i + start] :> int64)
        }
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_transpose(inp: Ast.nntensor_t, perm_: int [], out: Ast.nntensor_t): void
@ccode {
    enum {TRANSPOSE_MAX_DIMS=5};
    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    int_ i, ndims = inp_shape_->dim[0].size;
    fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;
    size_t out_esz = out_data->dim[0].step;
    int_ out_ndims = out_shape_->dim[0].size;
    int_ perm_ndims = perm_->dim[0].size;
    int_ perm[TRANSPOSE_MAX_DIMS] = {0, 1, 2, 3, 4};
    int_ inp_shape[TRANSPOSE_MAX_DIMS] = {1, 1, 1, 1, 1};
    int_ out_shape[TRANSPOSE_MAX_DIMS] = {1, 1, 1, 1, 1};
    size_t inp_step[TRANSPOSE_MAX_DIMS] = {0, 0, 0, 0, 1};
    int_ delta = TRANSPOSE_MAX_DIMS - ndims;

    if (ndims > TRANSPOSE_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims != out_ndims || (ndims != perm_ndims && perm_ndims != 0))
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (esz != out_esz)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    /*for(i = 0; i < ndims; i++) {
        printf("i=%d. inp_shape_i=%d, out_shape_i=%d, perm_i=%d\n", (int)i,
            (int)((int_*)inp_shape_->data)[i], (int)((int_*)out_shape_->data)[i], (int)((int_*)perm_->data)[i]);
    }*/

    for(i = ndims - 1; i >= 0; i--) {
        int_ j = perm_->data ? ((int_*)perm_->data)[i] : ndims - i - 1;
        int_ inp_dim, out_dim;
        if (j < 0) j = ndims - j;
        if (j < 0 || j >= ndims)
            return FX_SET_EXN_FAST(FX_EXN_OutOfRangeError);
        perm[delta + i] = j + delta;
        inp_dim = ((int_*)inp_shape_->data)[j];
        out_dim = ((int_*)out_shape_->data)[i];
        if (inp_dim != out_dim)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        inp_shape[delta + i] = ((int_*)inp_shape_->data)[i];
        out_shape[delta + i] = ((int_*)out_shape_->data)[i];
    }

    for (i = TRANSPOSE_MAX_DIMS-2; i >= 0; i--)
        inp_step[i] = inp_step[i+1]*inp_shape[i+1];

    int_ sz4 = out_shape[0], sz3 = out_shape[1];
    int_ sz2 = out_shape[2], sz1 = out_shape[3], sz0 = out_shape[4];
    size_t p4 = inp_step[perm[0]], p3 = inp_step[perm[1]];
    size_t p2 = inp_step[perm[2]], p1 = inp_step[perm[3]], p0 = inp_step[perm[4]];

    /*printf("esz=%d, out_esz=%d\n", (int)esz, (int)out_esz);
    for(i = 0; i < TRANSPOSE_MAX_DIMS; i++) {
        printf("i=%d. inp_shape_i=%d, out_shape_i=%d, perm_i=%d, inp_step_i=%d\n", (int)i,
            (int)inp_shape[i], (int)out_shape[i], (int)perm[i], (int)inp_step[i]);
    }
    printf("p0=%d, p1=%d, p2=%d, p3=%d\n", (int)p0, (int)p1, (int)p2, (int)p3);*/

    #undef _FX_IMPLEMENT_TRANSPOSE
    #define _FX_IMPLEMENT_TRANSPOSE(typ) \
        const typ* inptr0 = (const typ*)(inp_data->data); \
        typ* outptr = (typ*)(out_data->data); \
        for (int_ i4 = 0; i4 < sz4; i4++) { \
            for (int_ i3 = 0; i3 < sz3; i3++) { \
                for (int_ i2 = 0; i2 < sz2; i2++) { \
                    for (int_ i1 = 0; i1 < sz1; i1++, outptr += sz0) { \
                        int_ i0 = 0; \
                        const typ* inptr = inptr0 + i4*p4 + i3*p3 + i2*p2 + i1*p1; \
                        for (; i0 <= sz0 - 3; i0 += 3) { \
                            int_ ip0 = i0*p0; \
                            typ t0 = inptr[ip0]; \
                            typ t1 = inptr[ip0+p0]; \
                            typ t2 = inptr[ip0+p0*2]; \
                            outptr[i0] = t0; \
                            outptr[i0+1] = t1; \
                            outptr[i0+2] = t2; \
                        } \
                        for (; i0 < sz0; i0++) \
                            outptr[i0] = inptr[i0*p0]; \
                    } \
                } \
            } \
        }

    if (esz == 4) {
        _FX_IMPLEMENT_TRANSPOSE(int)
    } else if (esz == 2) {
        _FX_IMPLEMENT_TRANSPOSE(int16_t)
    } else if (esz == 1) {
        _FX_IMPLEMENT_TRANSPOSE(int8_t)
    } else if (esz == 8) {
        _FX_IMPLEMENT_TRANSPOSE(int64_t)
    } else {
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }
    return FX_OK;
}

fun run_transpose(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Transpose {perm, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_transpose(inp, perm, out)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_slice(inp: Ast.nntensor_t, out: Ast.nntensor_t,
              axes_: Ast.nntensor_t, starts_: Ast.nntensor_t,
              ends_: Ast.nntensor_t, steps_: Ast.nntensor_t): void
@ccode {
    enum {SLICE_MAX_DIMS=5};
    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    int_ i, ndims = inp_shape_->dim[0].size;
    int_ out_ndims = out_shape_->dim[0].size;
    const fx_arr_t* inp_data = &inp->data.u.NN_Data_I8;
    fx_arr_t* out_data = &out->data.u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;
    size_t out_esz = out_data->dim[0].step;
    int_ starts[SLICE_MAX_DIMS] = {0, 0, 0, 0, 0};
    int_ ends[SLICE_MAX_DIMS] = {INT_MAX, INT_MAX, INT_MAX, INT_MAX, INT_MAX};
    int_ steps[SLICE_MAX_DIMS] = {1, 1, 1, 1, 1};
    int_ inp_shape[SLICE_MAX_DIMS] = {1, 1, 1, 1, 1};
    int_ out_shape[SLICE_MAX_DIMS] = {1, 1, 1, 1, 1};
    int_ inp_step[SLICE_MAX_DIMS] = {0, 0, 0, 0, 1};
    int_ delta = SLICE_MAX_DIMS - ndims;
    int_ naxes = axes_->data.tag > 1 ? axes_->data.u.NN_Data_I8.dim[0].size : ndims;
    bool empty_out = false;
    const char* inptr0 = inp_data->data;
    char* axes_data = axes_->data.u.NN_Data_I8.data;
    char* starts_data = starts_->data.u.NN_Data_I8.data;
    char* ends_data = ends_->data.u.NN_Data_I8.data;
    char* steps_data = steps_->data.u.NN_Data_I8.data;

    if (ndims > SLICE_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims != out_ndims || naxes > ndims)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (esz != out_esz)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    for (int k = 0; k < 4; k++) {
        const _fx_nntensor_t* t = (const _fx_nntensor_t*)(
                k == 0 ? axes_ : k == 1 ? starts_ :
                k == 2 ? ends_ : steps_);
        int typ = t->data.tag;
        if (typ > 1) {
            const fx_arr_t* shape = &t->shape.shape;
            const fx_arr_t* data = &t->data.u.NN_Data_I8;
            if (typ != FX_I32 && typ != FX_I64)
                return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
            if (shape->dim[0].size != 1 || *(int_*)(shape->data) != naxes ||
                data->ndims != 1 || data->dim[0].size != naxes)
                return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        } else if (k == 1 || k == 2) {
            // starts_ and ends_ must be specified
            return FX_SET_EXN_FAST(FX_EXN_NullPtrError);
        }
    }

    for (i = ndims - 1; i >= 0; i--) {
        inp_shape[delta + i] = ((int_*)inp_shape_->data)[i];
        out_shape[delta + i] = ((int_*)out_shape_->data)[i];
    }
    for (i = SLICE_MAX_DIMS-1; i >= 0; i--)
        inp_step[i] = i == SLICE_MAX_DIMS-1 ? 1 : inp_step[i+1]*inp_shape[i+1];
    for (i = 0; i < naxes; i++) {
        int_ j = axes_->data.tag == FX_I32 ? (int_)((int32_t*)axes_data)[i] :
                 axes_->data.tag == FX_I64 ? (int_)((int64_t*)axes_data)[i] : i;
        int_ start = starts_->data.tag == FX_I32 ? (int_)((int32_t*)starts_data)[i] :
                     starts_->data.tag == FX_I64 ? (int_)((int64_t*)starts_data)[i] : 0;
        int_ end = ends_->data.tag == FX_I32 ? (int_)((int32_t*)ends_data)[i] :
                   ends_->data.tag == FX_I64 ? (int_)((int64_t*)ends_data)[i] : INT_MAX;
        int_ step = steps_->data.tag == FX_I32 ? (int_)((int32_t*)steps_data)[i] :
                   steps_->data.tag == FX_I64 ? (int_)((int64_t*)steps_data)[i] : 1;
        int_ sz_j, out_sz_j;

        if (j < 0) j += ndims;
        if (j < 0 || j >= ndims)
            return FX_SET_EXN_FAST(FX_EXN_RangeError);
        j += delta;
        sz_j = inp_shape[j];
        if (step == 0)
            return FX_SET_EXN_FAST(FX_EXN_ZeroStepError);
        if (start < 0) start += sz_j;
        if (start < 0) start = 0;
        if (start > sz_j - (step < 0)) start = sz_j - (step < 0);
        if (end < 0) end += sz_j;
        if (end < -(step < 0)) end = -(step < 0);
        if (end > sz_j - (step < 0)) end = sz_j - (step < 0);

        out_sz_j = step > 0 ? (end - start + step-1)/step : (start - end - step-1)/(-step);
        if (out_sz_j < 0)
            return FX_SET_EXN_FAST(FX_EXN_RangeError);
        if (out_sz_j != out_shape[j])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (out_sz_j == 0)
            empty_out = true;
        starts[j] = start;
        ends[j] = end;
        steps[j] = step;
        //printf("slice: i=%d. j=%d. sz_j=%d, starts_j=%d, ends_j=%d, steps_j=%d\n",
        //       (int)i, (int)(j-delta), (int)sz_j, (int)starts[j], (int)ends[j], (int)steps[j]);
    }

    for (i = 0; i < SLICE_MAX_DIMS; i++) {
        inptr0 += starts[i]*inp_step[i]*esz;
        inp_step[i] *= steps[i];
    }

    int_ sz0 = out_shape[4], sz1 = out_shape[3];
    int_ sz2 = out_shape[2], sz3 = out_shape[1], sz4 = out_shape[0];
    int_ p0 = inp_step[4], p1 = inp_step[3];
    int_ p2 = inp_step[2], p3 = inp_step[1], p4 = inp_step[0];

#undef _FX_IMPLEMENT_SLICE
#define _FX_IMPLEMENT_SLICE(typ) \
    typ* outptr = (typ*)(out_data->data); \
    for(int_ i4 = 0; i4 < sz4; i4++) { \
        for(int_ i3 = 0; i3 < sz3; i3++) { \
            for(int_ i2 = 0; i2 < sz2; i2++) { \
                for(int_ i1 = 0; i1 < sz1; i1++, outptr += sz0) { \
                    const typ* inptr = (const typ*)inptr0 + i4*p4 + i3*p3 + i2*p2 + i1*p1; \
                    int_ i0 = 0; \
                    if (p0 == 1) { \
                        memcpy(outptr, inptr, sz0*esz); \
                        continue; \
                    } \
                    for (; i0 <= sz0 - 4; i0 += 4) { \
                        int_ ip0 = i0*p0; \
                        typ t0 = inptr[ip0], t1 = inptr[ip0 + p0]; \
                        typ t2 = inptr[ip0 + p0*2], t3 = inptr[ip0 + p0*3]; \
                        outptr[i0] = t0; outptr[i0+1] = t1; \
                        outptr[i0+2] = t2; outptr[i0+3] = t3; \
                    } \
                    for (; i0 < sz0; i0++) \
                        outptr[i0] = inptr[i0*p0]; \
                } \
            } \
        } \
    }

    if (empty_out) return FX_OK;
    if (esz == 4) {
        _FX_IMPLEMENT_SLICE(int)
    } else if (esz == 2) {
        _FX_IMPLEMENT_SLICE(int16_t)
    } else if (esz == 1) {
        _FX_IMPLEMENT_SLICE(int8_t)
    } else if (esz == 8) {
        _FX_IMPLEMENT_SLICE(int64_t)
    } else {
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }
    return FX_OK;
}

fun run_slice(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val axes = model.get_tensor(t_axes)
    val starts = model.get_tensor(t_starts)
    val ends = model.get_tensor(t_ends)
    val steps = model.get_tensor(t_steps)
    run_slice(inp, out, axes, starts, ends, steps)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

@private fun run_split_(axis: int, inp_shape_: int [], inp_data_: Ast.nndata_t,
                        out_shape_: int [][], out_data_: Ast.nndata_t [], ntasks: int): void
@ccode {
    int_ ndims = inp_shape_->dim[0].size, nslices = 1;
    const int_* inp_shape = (const int_*)(inp_shape_->data);
    size_t esz = inp_data_->u.NN_Data_I8.dim[0].step;
    size_t slicesize = esz;
    size_t inpstride = 0, totalsize = 0;
    int_ i, noutputs = out_data_->dim[0].size;
    int_ output_size_axis = 0;
    for (i = ndims-1; i > axis; i--)
        slicesize *= inp_shape[i];
    inpstride = slicesize*inp_shape[axis];
    for (i = 0; i < axis; i++)
        nslices *= inp_shape[i];

    for (i = 0; i < noutputs; i++) {
        fx_arr_t* out_shape_i_ = (fx_arr_t*)out_shape_->data + i;
        const int_* out_shape_i = (const int_*)(out_shape_i_->data);
        size_t esz_i = ((_fx_nndata_t*)out_data_->data + i)->u.NN_Data_I8.dim[0].step;
        size_t total_i = esz_i;
        if (esz_i != esz)
            return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
        if (out_shape_i_->ndims != 1 || out_shape_i_->dim[0].size != ndims)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        for (int_ j = 0; j < ndims; j++) {
            int_ sz_ij = out_shape_i[j];
            if (j != axis) {
                if (inp_shape[j] != sz_ij)
                    return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
            } else {
                output_size_axis += sz_ij;
            }
            total_i *= sz_ij;
        }
        totalsize += total_i;
    }

    #pragma omp parallel for num_threads(ntasks) if (totalsize > 10000000)
    for (i = 0; i < noutputs; i++) {
        const char* inptr = inp_data_->u.NN_Data_I8.data;
        fx_arr_t* out_shape_i_ = (fx_arr_t*)out_shape_->data + i;
        const int_* out_shape_i = (const int_*)(out_shape_i_->data);
        char* outptr_i = ((_fx_nndata_t*)out_data_->data + i)->u.NN_Data_I8.data;
        int_ sz_a;
        for (int_ j = 0; j < i; j++) {
            fx_arr_t* out_shape_j_ = (fx_arr_t*)out_shape_->data + j;
            const int_* out_shape_j = (const int_*)(out_shape_j_->data);
            sz_a = out_shape_j[axis];
            inptr += slicesize*sz_a;
        }
        sz_a = out_shape_i[axis];
        size_t slicesize_i = slicesize*sz_a;
        for (int_ j = 0; j < nslices; j++)
            memcpy(outptr_i + j*slicesize_i, inptr + j*inpstride, slicesize_i);
    }
    return FX_OK;
}

fun run_split(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Split {axis, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val inp_shape = inp.shape.shape
    val ndims = inp_shape.size()
    var etyp_0 = inp.data.elemtype()
    val (out_data, out_shape) = [@unzip for idx@i <- t_out {
            val t = model.get_tensor(idx)
            val etyp_i = t.data.elemtype()
            assert(`etyp_0 == etyp_i`)
            (t.data, t.shape.shape)
        }]
    val axis = Ast.normalize_axis(axis, ndims)
    run_split_(axis, inp_shape, inp.data, out_shape, out_data, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_tile(inp: Ast.nntensor_t, repeats_: Ast.nntensor_t,
             out: Ast.nntensor_t, ntasks: int): void
@ccode {
    enum {TILE_MAX_DIMS=4};
    const fx_arr_t* inp_shape_ = &inp->shape.shape;
    const fx_arr_t* repeats_shape_ = &repeats_->shape.shape;
    const fx_arr_t* out_shape_ = &out->shape.shape;
    const fx_arr_t* repeats_data = &repeats_->data.u.NN_Data_I8;
    int_ ndims = inp_shape_->dim[0].size;
    const int_* inp_shape0 = (const int_*)(inp_shape_->data);
    const int_* out_shape0 = (const int_*)(out_shape_->data);
    const int64_t* repeats0 = (const int64_t*)(repeats_data->data);
    size_t esz = inp->data.u.NN_Data_I8.dim[0].step;
    size_t out_esz = out->data.u.NN_Data_I8.dim[0].step;
    int_ total_size = 1, total_repeats = 1;
    int_ inp_shape[TILE_MAX_DIMS] = {1, 1, 1, 1};
    int_ out_shape[TILE_MAX_DIMS] = {1, 1, 1, 1};
    int_ repeats[TILE_MAX_DIMS] = {1, 1, 1, 1};
    size_t inp_step[TILE_MAX_DIMS], out_step[TILE_MAX_DIMS];
    int_ delta = TILE_MAX_DIMS - ndims;
    char* inp_data0 = inp->data.u.NN_Data_I8.data;

    if (ndims > TILE_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (repeats_->data.tag != FX_I64)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    if (esz != out_esz)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    if (ndims != out_shape_->dim[0].size ||
        repeats_shape_->dim[0].size != 1 ||
        ((int_*)repeats_shape_->data)[0] != ndims ||
        ndims != repeats_data->dim[0].size )
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    for (int i = 0; i < ndims; i++) {
        inp_shape[i + delta] = inp_shape0[i];
        out_shape[i + delta] = out_shape0[i];
        repeats[i + delta] = (int_)repeats0[i];
        if (out_shape0[i] != inp_shape0[i]*repeats0[i])
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        total_repeats *= repeats0[i];
        total_size *= out_shape0[i];
    }

    for (int i = TILE_MAX_DIMS-1; i >= 0; i--) {
        if (i == TILE_MAX_DIMS-1)
            inp_step[i] = out_step[i] = 1;
        else {
            inp_step[i] = inp_step[i+1]*inp_shape[i+1];
            out_step[i] = out_step[i+1]*out_shape[i+1];
        }
        //printf("Tile: i=%d. inp_step_i=%d, out_step_i=%d, inp_shape_i=%d, out_shape_i=%d, repeats_i=%d\n",
        //    i, (int)inp_step[i], (int)out_step[i], (int)inp_shape[i], (int)out_shape[i], (int)repeats[i]);
    }

    if (ntasks > total_repeats)
        ntasks = total_repeats;
    if (total_size < 1000000)
        ntasks = 1;
    // [TODO] compress some inner dimensions 'i' iff repeats[i] == 1
    // ...
    #pragma omp parallel for num_threads(ntasks)
    for (int_ i = 0; i < ntasks; i++) {
        int_ j0 = i*total_repeats/ntasks, j1 = (i+1)*total_repeats/ntasks;
        int_ ofs[TILE_MAX_DIMS];
        int_ sz0 = inp_shape[0], sz1 = inp_shape[1], sz2 = inp_shape[2], sz3 = inp_shape[3];

        // this is a partial case of compression to reduce the innermost loop overhead
        int_ out_step_prelast = out_step[TILE_MAX_DIMS-2];
        /*if (repeats[TILE_MAX_DIMS-1] == 1) {
            sz3 *= sz2;
            out_step_prelast *= sz2;
            sz2 = 1;
        }*/

        for (int_ j = j0; j < j1; j++)
        {
            // convert raw tile index into n-dim tile index.
            // but we don't need this nd-index itself, we just need the
            // offset of the tile in the output tensor
            int_ j_ = j, raw_ofs = 0;
            for (int_ k = TILE_MAX_DIMS-1; k >= 0; k--) {
                int_ r = repeats[k];
                int_ q = j_ / r;
                raw_ofs += (j_ - q*r)*inp_shape[k]*out_step[k];
                j_ = q;
            }
            //printf("j=%d, j0=%d, j1=%d, raw_ofs=%d, esz=%d\n", (int)j, (int)j0, (int)j1, (int)raw_ofs, (int)esz);

            #undef _FX_IMPLEMENT_TILE
            #define _FX_IMPLEMENT_TILE(typ) \
            typ* inp_data = (typ*)inp_data0; \
            typ* out_data0 = (typ*)(out->data.u.NN_Data_I8.data) + raw_ofs; \
            for (int_ i0 = 0; i0 < sz0; i0++) { \
                for (int_ i1 = 0; i1 < sz1; i1++) { \
                    typ* out_data = out_data0 + i0*out_step[0] + i1*out_step[1]; \
                    for (int_ i2 = 0; i2 < sz2; i2++, out_data += out_step_prelast, inp_data += sz3) { \
                        for (int_ i3 = 0; i3 < sz3; i3++) { \
                            out_data[i3] = inp_data[i3]; \
                        } \
                    } \
                } \
            }

            if (esz == 1) {
                _FX_IMPLEMENT_TILE(int8_t)
            } else if (esz == 2) {
                _FX_IMPLEMENT_TILE(int16_t)
            } else if (esz == 4) {
                _FX_IMPLEMENT_TILE(int32_t)
            } else {
                assert(esz == 8);
                _FX_IMPLEMENT_TILE(int64_t)
            }
        }
    }
    return FX_OK;
}

fun run_tile(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Tile {t_inp, t_repeats, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val repeats = model.get_tensor(t_repeats)
    run_tile(inp, repeats, out, *model.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
