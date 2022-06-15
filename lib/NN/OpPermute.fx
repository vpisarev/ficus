/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast

@ccode {
#include <limits.h>

typedef struct _fx_nndata_t {
   int tag;
   union {
      fx_arr_t NN_Data_I8;
      fx_arr_t NN_Data_U8;
   } u;
} _fx_nndata_t;
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

fun run_concat(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Concat {axis, t_inp, t_out} =>
    val out = net.get_tensor(t_out)
    val out_shape = out.shape.shape
    val ndims = out_shape.size()
    var etyp_0 = out.data.elemtype()
    val (inp_data, inp_shape) = [@unzip for idx@i <- t_inp {
            val t = net.get_tensor(idx)
            val etyp_i = t.data.elemtype()
            assert(`etyp_0 == etyp_i`)
            (t.data, t.shape.shape)
        }]

    val axis = Ast.normalize_axis(axis, ndims)
    run_concat_(axis, inp_shape, inp_data, out_shape, out.data, *net.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

@private fun run_gather_(axis: int, inp_shape: int [], inp_data: Ast.nndata_t,
                         ind_shape: int [], ind_data: Ast.nndata_t,
                         out_shape: int [], out_data: Ast.nndata_t, ntasks: int): void
@ccode {
    int_ r = inp_shape->dim[0].size;
    int_ q = ind_shape->dim[0].size;
    int_ ndims = out_shape->dim[0].size;
    int_ pq = 1, nslices = 1;
    int fx_status = FX_OK;
    size_t elemsize = inp_data->u.NN_Data_I8.dim[0].step;
    size_t slicesize = elemsize;
    size_t inp_stride, out_stride;
    const char* inptr0 = inp_data->u.NN_Data_I8.data;
    char* outptr0 = out_data->u.NN_Data_I8.data;
    const fx_arr_t* ind_arr = &(ind_data->u.NN_Data_I32);
    const int* ind32 = 0;
    const int64_t* ind64 = 0;
    int_ s = ((const int_*)inp_shape->data)[axis];

    if (ind_arr->dim[0].step == sizeof(int)) {
        ind32 = (const int*)ind_arr->data;
    }
    else {
        assert(ind_arr->dim[0].step == sizeof(int64_t));
        ind64 = (const int64_t*)ind_arr->data;
    }

    for(int_ i = 0; i < q; i++)
        pq *= ((int_*)ind_shape->data)[i];

    for(int_ j = 0; j < r; j++) {
        int_ szj = ((int_*)inp_shape->data)[j];
        if (j < axis)
            nslices *= szj;
        else if (j > axis)
            slicesize *= szj;
    }
    inp_stride = slicesize * ((int_*)inp_shape->data)[axis];
    out_stride = slicesize * pq;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ i = 0; i < pq; i++) {
        int_ k = ind32 ? (int_)ind32[i] : (int_)ind64[i];
        k += k < 0 ? s : 0;
        const char* inptr = inptr0 + k*slicesize;
        char* outptr = outptr0 + i*slicesize;
        if (k >= s) {
            fx_status = FX_EXN_OutOfRangeError; continue;
        }
        for(int_ j = 0; j < nslices; j++)
            memcpy(outptr + j*out_stride, inptr + j*inp_stride, slicesize);
    }

    return fx_status >= 0 ? fx_status : FX_SET_EXN_FAST(fx_status);
}

fun run_gather(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Gather {axis, t_inp, t_ind, t_out} =>
    val inp = net.get_tensor(t_inp)
    val inp_shape = inp.shape.shape
    val r = inp_shape.size()
    val axis = Ast.normalize_axis(axis, r)
    val ind = net.get_tensor(t_ind)
    val ind_shape = ind.shape.shape
    val q = ind_shape.size()
    val out = net.get_tensor(t_out)
    val out_shape = out.shape.shape
    val ndims = out_shape.size()
    assert(`ndims == q + r - 1`)
    match ind.data {
    | Ast.NN_Data_I32 _ | Ast.NN_Data_I64 _ => {}
    | _ => throw Ast.NNError(f"Gather: unsupported index type '{ind.data.elemtype()}'")
    }
    match (inp.data, out.data) {
    | (Ast.NN_Data_Empty, _) => throw Ast.NNError(f"Gather: input data cannot be empty")
    | (_, Ast.NN_Data_Empty) => throw Ast.NNError(f"Gather: output data cannot be empty")
    | _ => {}
    }
    run_gather_(axis, inp_shape, inp.data, ind_shape, ind.data, out_shape, out.data, *net.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_shape(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Shape {start, end, t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val inp_shape = inp.shape.shape
    val out = net.get_tensor(t_out)
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

@private fun run_transpose_(inp_shape_: int [], inp_data_: Ast.nndata_t, perm_: int [],
                            out_shape_: int [], out_data_: Ast.nndata_t): void
@ccode {
    enum {TRANSPOSE_MAX_DIMS=4};
    int_ i, ndims = inp_shape_->dim[0].size;
    fx_arr_t* inp_data = &((_fx_nndata_t*)inp_data_)->u.NN_Data_I8;
    fx_arr_t* out_data = &((_fx_nndata_t*)out_data_)->u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;
    size_t out_esz = out_data->dim[0].step;
    int_ out_ndims = out_shape_->dim[0].size;
    int_ perm_ndims = perm_->dim[0].size;
    int_ perm[TRANSPOSE_MAX_DIMS] = {0, 1, 2, 3};
    int_ inp_shape[TRANSPOSE_MAX_DIMS] = {1, 1, 1, 1};
    int_ out_shape[TRANSPOSE_MAX_DIMS] = {1, 1, 1, 1};
    size_t inp_step[TRANSPOSE_MAX_DIMS] = {0, 0, 0, 1};
    int_ delta = TRANSPOSE_MAX_DIMS - ndims;

    if (ndims > TRANSPOSE_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims != out_ndims || ndims != perm_ndims)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (esz != out_esz)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    /*for(i = 0; i < ndims; i++) {
        printf("i=%d. inp_shape_i=%d, out_shape_i=%d, perm_i=%d\n", (int)i,
            (int)((int_*)inp_shape_->data)[i], (int)((int_*)out_shape_->data)[i], (int)((int_*)perm_->data)[i]);
    }*/

    for(i = 0; i < ndims; i++) {
        int_ j = ((int_*)perm_->data)[i];
        int_ inp_dim, out_dim;
        if (j < 0) j = ndims - j;
        if (j < 0 || j >= ndims)
            return FX_SET_EXN_FAST(FX_EXN_OutOfRangeError);
        inp_dim = ((int_*)inp_shape_->data)[j];
        out_dim = ((int_*)out_shape_->data)[i];
        if (inp_dim != out_dim)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    }

    for (i = ndims - 1; i >= 0; i--) {
        perm[delta + i] = ((int_*)perm_->data)[i] + delta;
        inp_shape[delta + i] = ((int_*)inp_shape_->data)[i];
        out_shape[delta + i] = ((int_*)out_shape_->data)[i];
    }
    for (i = TRANSPOSE_MAX_DIMS-2; i >= 0; i--)
        inp_step[i] = inp_step[i+1]*inp_shape[i+1];

    int_ sz0 = out_shape[0], sz1 = out_shape[1];
    int_ sz2 = out_shape[2], sz3 = out_shape[3];
    size_t p0 = inp_step[perm[0]], p1 = inp_step[perm[1]];
    size_t p2 = inp_step[perm[2]], p3 = inp_step[perm[3]];

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
        for (int_ i0 = 0; i0 < sz0; i0++) { \
            for (int_ i1 = 0; i1 < sz1; i1++) { \
                for (int_ i2 = 0; i2 < sz2; i2++, outptr += sz3) { \
                    int_ i3 = 0; \
                    const typ* inptr = inptr0 + i0*p0 + i1*p1 + i2*p2; \
                    for (; i3 <= sz3 - 3; i3 += 3) { \
                        int_ ip3 = i3*p3; \
                        typ t0 = inptr[ip3], t1 = inptr[ip3+p3], t2 = inptr[ip3+p3*2]; \
                        outptr[i3] = t0; \
                        outptr[i3+1] = t1; \
                        outptr[i3+2] = t2; \
                    } \
                    for (; i3 < sz3; i3++) \
                        outptr[i3] = inptr[i3*p3]; \
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

fun run_transpose(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Transpose {perm, t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    run_transpose_(inp.shape.shape, inp.data, perm, out.shape.shape, out.data)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

@private fun run_slice_(inp_shape_: int [], inp_data_: Ast.nndata_t,
                        out_shape_: int [], out_data_: Ast.nndata_t,
                        axes_: int [], starts_: int [], ends_: int [], steps_: int []): void
@ccode {
    enum {SLICE_MAX_DIMS=4};
    int_ i, ndims = inp_shape_->dim[0].size;
    fx_arr_t* inp_data = &((_fx_nndata_t*)inp_data_)->u.NN_Data_I8;
    fx_arr_t* out_data = &((_fx_nndata_t*)out_data_)->u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;
    size_t out_esz = out_data->dim[0].step;
    int_ out_ndims = out_shape_->dim[0].size;
    int_ starts[SLICE_MAX_DIMS] = {0, 0, 0, 0};
    int_ ends[SLICE_MAX_DIMS] = {INT_MAX, INT_MAX, INT_MAX, INT_MAX};
    int_ steps[SLICE_MAX_DIMS] = {1, 1, 1, 1};
    int_ inp_shape[SLICE_MAX_DIMS] = {1, 1, 1, 1};
    int_ out_shape[SLICE_MAX_DIMS] = {1, 1, 1, 1};
    int_ inp_step[SLICE_MAX_DIMS] = {0, 0, 0, 1};
    int_ delta = SLICE_MAX_DIMS - ndims;
    int_ naxes = axes_->dim[0].size;
    bool empty_out = false;
    const char* inptr0 = inp_data->data;

    if (ndims > SLICE_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    if (ndims != out_ndims)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (esz != out_esz)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (naxes > ndims ||
        starts_->dim[0].size != naxes ||
        ends_->dim[0].size != naxes ||
        steps_->dim[0].size != naxes)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    /*for(i = 0; i < ndims; i++) {
        printf("i=%d. inp_shape_i=%d, out_shape_i=%d, perm_i=%d\n", (int)i,
            (int)((int_*)inp_shape_->data)[i], (int)((int_*)out_shape_->data)[i], (int)((int_*)perm_->data)[i]);
    }*/

    for (i = ndims - 1; i >= 0; i--) {
        inp_shape[delta + i] = ((int_*)inp_shape_->data)[i];
        out_shape[delta + i] = ((int_*)out_shape_->data)[i];
    }
    for (i = SLICE_MAX_DIMS-2; i >= 0; i--)
        inp_step[i] = inp_step[i+1]*inp_shape[i+1];
    for (i = 0; i < naxes; i++) {
        int_ j = ((int_*)axes_->data)[i];
        int_ start = ((int_*)starts_->data)[i];
        int_ end = ((int_*)ends_->data)[i];
        int_ step = ((int_*)steps_->data)[i];
        int_ sz_j, out_sz_j;

        if (j < 0) j += ndims;
        if (j < 0 || j >= ndims)
            return FX_SET_EXN_FAST(FX_EXN_RangeError);
        j += delta;
        sz_j = inp_shape[j];
        if (start < 0) start += sz_j;
        if (start < 0)
            return FX_SET_EXN_FAST(FX_EXN_RangeError);
        if (start > sz_j) start = sz_j;
        if (end < 0) end += sz_j;
        if (end < 0)
            return FX_SET_EXN_FAST(FX_EXN_RangeError);
        if (end > sz_j) end = sz_j;
        if (step == 0)
            return FX_SET_EXN_FAST(FX_EXN_RangeError);
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
    }

    for (i = 0; i < SLICE_MAX_DIMS; i++) {
        if (ends[i] > inp_shape[i]) ends[i] = inp_shape[i];
        if (steps[i] > 0)
            inptr0 += starts[i]*inp_step[i]*esz;
        else {
            inptr0 += (ends[i]-1)*inp_step[i]*esz;
            inp_step[i] *= -1;
        }
    }

    int_ sz0 = out_shape[0], sz1 = out_shape[1];
    int_ sz2 = out_shape[2], sz3 = out_shape[3];
    int_ p0 = inp_step[0], p1 = inp_step[1];
    int_ p2 = inp_step[2], p3 = inp_step[3];

#undef _FX_IMPLEMENT_SLICE
#define _FX_IMPLEMENT_SLICE(typ) \
    typ* outptr = (typ*)(out_data->data); \
    for(int_ i0 = 0; i0 < sz0; i0++) { \
        for(int_ i1 = 0; i1 < sz1; i1++) { \
            for(int_ i2 = 0; i2 < sz2; i2++, outptr += sz3) { \
                const typ* inptr = (const typ*)inptr0 + i0*p0 + i1*p1 + i2*p2; \
                int_ i3 = 0; \
                if (p3 == 1) { \
                    memcpy(outptr, inptr, sz3*esz); \
                    continue; \
                } \
                for (; i3 <= sz3 - 4; i3 += 4) { \
                    int_ ip3 = i3*p3; \
                    typ t0 = inptr[ip3], t1 = inptr[ip3 + p3]; \
                    typ t2 = inptr[ip3 + p3*2], t3 = inptr[ip3 + p3*3]; \
                    outptr[i3] = t0; outptr[i3+1] = t1; \
                    outptr[i3+2] = t2; outptr[i3+3] = t3; \
                } \
                for (; i3 < sz3; i3++) \
                    outptr[i3] = inptr[i3*p3]; \
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

fun run_slice(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    val inp_shape = inp.shape.shape
    val out_shape = out.shape.shape
    val ndims = inp_shape.size()
    val axes = if t_axes != 0 {int(net.get_tensor(t_axes))} else {mkrange(ndims)}
    val naxes = axes.size()
    val starts = int(net.get_tensor(t_starts))
    val ends = int(net.get_tensor(t_ends))
    val steps = if t_steps != 0 {int(net.get_tensor(t_steps))} else {array(naxes, 1)}
    run_slice_(inp_shape, inp.data, out_shape, out.data, axes, starts, ends, steps)
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

fun run_split(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Split {axis, t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val inp_shape = inp.shape.shape
    val ndims = inp_shape.size()
    var etyp_0 = inp.data.elemtype()
    val (out_data, out_shape) = [@unzip for idx@i <- t_out {
            val t = net.get_tensor(idx)
            val etyp_i = t.data.elemtype()
            assert(`etyp_0 == etyp_i`)
            (t.data, t.shape.shape)
        }]
    val axis = Ast.normalize_axis(axis, ndims)
    run_split_(axis, inp_shape, inp.data, out_shape, out_data, *net.ntasks)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
