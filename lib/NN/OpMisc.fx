/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// various element-wise operations
import Ast

fun run_lrn_2d(inp: float [], inp_shape: Ast.nnshape_t, out: float [],
               size: int, alpha: float, beta: float, bias: float)
{
    val N = inp_shape.shape[0], C = inp_shape.shape[1]
    val H = inp_shape.shape[2], W = inp_shape.shape[3]
    val plane_size = H*W, npixels = N*plane_size
    val dc0 = floor((size-1)/2.), dc1 = ceil((size-1)/2.)+1
    val scale = alpha/size
    @parallel for i <- 0:npixels {
        val plane_idx = i/plane_size
        val plane_ofs = i - plane_idx*plane_size
        val ofs0 = plane_idx*plane_size*C + plane_ofs
        for c <- 0:C {
            val ofs = ofs0 + c*plane_size
            val c0 = max(c-dc0, 0)
            val c1 = min(c+dc1, C)
            val fold sqsum = 0.f for j <- c0:c1 {
                val x = inp[j*plane_size + ofs0]
                sqsum + x*x
            }
            out[ofs] = inp[ofs]*pow(bias + scale*sqsum, -beta)
        }
    }
}

fun run_lrn(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_LRN {size, alpha, beta, bias, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.NN_Layout_NCHW`)
    assert(`inp.shape.shape.size() == 4`)
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_lrn_2d(inp_data, inp.shape, out_data, size, alpha, beta, bias)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}

fun run_range(out_shape_: int [], out_data: Ast.nndata_t,
              start: double, limit: double, delta: double): void
{
    val nelems = max(ceil((limit - start)/delta), 0)
    assert(`out_data.total() == nelems && out_shape_.size() == 1`)
    match out_data {
    | Ast.NN_Data_FP32 data =>
        for i <- 0:nelems {data[i] = float(start + i*delta)}
    | Ast.NN_Data_I32 data =>
        val start = int(start), delta = int(delta)
        for i <- 0:nelems {data[i] = int32(start + i*delta)}
    | Ast.NN_Data_I64 data =>
        val start = int64(start), delta = int64(delta)
        for i <- 0:nelems {data[i] = int32(start + i*delta)}
    | _ => throw Ast.NNError("run_range: unsupported type `out_data.elemtype()`")
    }
}

fun run_range(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Range {t_start, t_limit, t_delta, t_out} =>
    val start = model.get_tensor(t_start)
    val start = start.data
    val limit = model.get_tensor(t_limit).data
    val delta = model.get_tensor(t_delta).data
    val out = model.get_tensor(t_out)
    assert(`start.total() == 1`)
    assert(`limit.total() == 1`)
    assert(`delta.total() == 1`)
    val start = start.double_scalar_or(0.)
    val limit = limit.double_scalar_or(0.)
    val delta = delta.double_scalar_or(1.)
    run_range(out.shape.shape, out.data, start, limit, delta)
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
