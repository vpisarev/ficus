/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/
import Ast

fun run_conv(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_Conv {kernel_shape, pads, strides, dilations, group, t_inp, t_weights, t_bias, t_out} =>
    val out = net.get_tensor(t_out)
    match out.data {
    | Ast.DL_Data_FP32 out_data => for _@idx <- out_data {out_data[idx] = 0.f}
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

fun run_conv_transposed(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_ConvTranspose {kernel_shape, pads, strides, dilations, group,
        out_shape, out_padding, t_inp, t_weights, t_bias, t_out} =>
    val out = net.get_tensor(t_out)
    match out.data {
    | Ast.DL_Data_FP32 out_data => for _@idx <- out_data {out_data[idx] = 0.f}
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}
