/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
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
}

@private fun run_concat_(axis: int, inp_shape: int [][], inp_data: Ast.nndata_t [],
                         out_shape: int [], out_data: Ast.nndata_t): void
@ccode {
    int_ ndims = out_shape->dim[0].size, nslices = 1;
    size_t elemsize = out_data->u.NN_Data_I8.dim[0].step;
    size_t slicesize = elemsize;
    size_t outstride = out_shape->dim[axis].size;
    int_ i, ninputs = inp_shape->dim[0].size;
    for (i = ndims-2; i > axis; i--)
        slicesize *= (size_t)out_shape->dim[i].size;
    for (i = 0; i < axis; i++)
        nslices *= out_shape->dim[i].size;
    outstride *= slicesize;

    #pragma omp parallel for
    for (i = 0; i < ninputs; i++) {
        char* outptr = out_data->u.NN_Data_I8.data;
        fx_arr_t* inp_shape_i = (fx_arr_t*)inp_shape->data + i;
        const char* inptr_i = ((_fx_nndata_t*)inp_data->data + i)->u.NN_Data_I8.data;
        int_ sz_a;
        for (int_ j = 0; j < i; j++) {
            fx_arr_t* inp_shape_j = (fx_arr_t*)inp_shape->data + j;
            sz_a = inp_shape_j->dim[axis].size;
            outptr += slicesize*sz_a;
        }
        sz_a = inp_shape_i->dim[axis].size;
        size_t slicesize_i = slicesize*sz_a;
        for (int_ j = 0; j < nslices; j++)
            memcpy(outptr + j*outstride, inptr_i + j*slicesize_i, slicesize_i);
    }
    return FX_OK;
}

fun run_concat(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Concat {axis, t_inp, t_out} =>
    var etyp_0 = Ast.NN_FP32
    val (inp_data, inp_shape) = [@unzip for idx@i <- t_inp {
            val t = net.get_tensor(idx)
            val etyp_i = t.data.elemtype()
            if i > 0 {
                assert(`etyp_0 == etyp_i`)
            } else {
                etyp_0 = etyp_i
            }
            (t.data, t.shape.shape)
        }]
    val out = net.get_tensor(t_out)
    val out_shape = out.shape.shape
    val ndims = out_shape.size()
    val axis = Ast.normalize_axis(axis, ndims)
    run_concat_(axis, inp_shape, inp_data, out_shape, out.data)
| _ => throw Ast.NNError(f"unsupported operation {op.name()}")
}

@private fun run_gather_(axis: int, inp_shape: int [], inp_data: Ast.nndata_t,
                         ind_shape: int [], ind_data: Ast.nndata_t,
                         out_shape: int [], out_data: Ast.nndata_t): void
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

    #pragma omp parallel for
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
    run_gather_(axis, inp_shape, inp.data, ind_shape, ind.data, out_shape, out.data)
| _ => throw Ast.NNError(f"unsupported operation {op.name()}")
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
| _ => throw Ast.NNError(f"unsupported operation {op.name()}")
}
