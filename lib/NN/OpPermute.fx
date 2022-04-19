/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast

fun run_shape(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_Shape {start, end, t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val inp_shape = inp.shape.shape
    val out = net.get_tensor(t_out)
    val ndims = inp_shape.size()
    val start = Ast.normalize_axis(start, ndims)
    val end = if end >= ndims {ndims} else {Ast.normalize_axis(end, ndims)}
    match out.data {
    | Ast.DL_Data_I64 out_data =>
        val out_size = out_data.size()
        println(f"Shape: out_size={out_size}, start={start}, end={end}")
        assert(`out_size == end - start`)
        for i <- 0:out_size {
            out_data[i] = (inp_shape[i + start] :> int64)
        }
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unsupported operation {op.name()}")
}
