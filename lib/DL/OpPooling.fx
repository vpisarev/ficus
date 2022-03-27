/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Ast

fun run_maxpool_2d(inp: 't [], inp_shape: Ast.dlshape_t,
                   out: 't [], out_shape: Ast.dlshape_t,
                   kh: int, kw: int, sy: int, sx: int, dy: int, dx: int,
                   pad_top: int, pad_left: int, pad_bottom: int, pad_right: int)
{
    for _@idx <- out {out[idx] = (0 :> 't)}
}

fun run_maxpool(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_MaxPool {ceil_mode, dilations, kernel_shape, pads, strides, storage_order, t_inp, t_out}
    when kernel_shape.size() == 2 =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.DL_Layout_NCHW`)
    match (inp.data, out.data) {
    | (Ast.DL_Data_U8 inp_data, Ast.DL_Data_U8 out_data) =>
        run_maxpool_2d(inp_data, inp.shape, out_data, out.shape,
            kernel_shape[0], kernel_shape[0],
            strides[0], strides[1], dilations[0], dilations[1],
            pads[0], pads[1], pads[2], pads[3])
    | (Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_maxpool_2d(inp_data, inp.shape, out_data, out.shape, kernel_shape[0], kernel_shape[0],
            strides[0], strides[1], dilations[0], dilations[1],
            pads[0], pads[1], pads[2], pads[3])
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unsupported operation {op.name()}")
}

fun run_avgpool_2d(inp: 't [], inp_shape: Ast.dlshape_t,
                   out: 't [], out_shape: Ast.dlshape_t,
                   kh: int, kw: int, sy: int, sx: int, dy: int, dx: int,
                   pad_top: int, pad_left: int, pad_bottom: int, pad_right: int,
                   count_include_pad: bool)
{
    for _@idx <- out {out[idx] = (0 :> 't)}
}

fun run_avgpool(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_AvgPool {ceil_mode, dilations, kernel_shape, pads,
    strides, count_include_pad, t_inp, t_out}
    when kernel_shape.size() == 2 =>

    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    assert(`inp.shape.layout == Ast.DL_Layout_NCHW`)
    match (inp.data, out.data) {
    | (Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_avgpool_2d(inp_data, inp.shape, out_data, out.shape,
            kernel_shape[0], kernel_shape[0],
            strides[0], strides[1], dilations[0], dilations[1],
            pads[0], pads[1], pads[2], pads[3], count_include_pad)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unsupported operation {op.name()}")
}
