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

fun run_lrn(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_LRN {size, alpha, beta, bias, t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.NN_Layout_NCHW`)
    assert(`inp.shape.shape.size() == 4`)
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_lrn_2d(inp_data, inp.shape, out_data, size, alpha, beta, bias)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation {op.name()}")
}

fun run_softmax(inp: 't [], inp_shape: Ast.nnshape_t, out: 't [])
{
    val N = inp_shape.shape[0], C = inp_shape.shape[1]
    @parallel for i <- 0:N {
        val ofs = i*C
        val fold maxval = inp[ofs] for j <- 1:C {max(maxval, inp[ofs+j])}
        var s = 0.
        val tab = [for j <- 0:C {val t = exp(inp[ofs+j] - maxval); s += t; t}]
        val s = (1/s :> 't)
        for j <- 0:C {out[ofs+j] = tab[j]*s}
    }
}

fun run_softmax(net: Ast.nnet_t, op: Ast.nnop_t) =
match op {
| Ast.NN_SoftMax {axis, t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    assert(`inp.shape.shape.size() == 2`)
    assert(`axis == 1 || axis == -1`)
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_softmax(inp_data, inp.shape, out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation {op.name()}")
}