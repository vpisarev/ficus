/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// various element-wise operations
import Ast

@private fun run_cast(inp: 'inpT [], out: 'outT []) = for x@idx <- inp {out[idx] = (x :> 'outT)}
@private fun run_cast(inp: float [], out: uint8 []) = for x@idx <- inp {out[idx] = sat_uint8(x)}
@private fun run_cast(inp: float [], out: int8 []) = for x@idx <- inp {out[idx] = sat_int8(x)}
@private fun run_cast(inp: float [], out: int16 []) = for x@idx <- inp {out[idx] = sat_int16(x)}
@private fun run_cast(inp: float [], out: int32 []) = for x@idx <- inp {out[idx] = int32(round(x))}

fun run_cast(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_Cast {t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    match (inp.data, out.data) {
    | (Ast.DL_Data_Empty, _) => {}
    | (_, Ast.DL_Data_Empty) => {}
    | (Ast.DL_Data_I8 inp_data, Ast.DL_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I16 inp_data, Ast.DL_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I32 inp_data, Ast.DL_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I64 inp_data, Ast.DL_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_FP32 inp_data, Ast.DL_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_Bool inp_data, Ast.DL_Data_U8 out_data) => run_cast(inp_data, out_data)

    | (Ast.DL_Data_U8 inp_data, Ast.DL_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I16 inp_data, Ast.DL_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I32 inp_data, Ast.DL_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I64 inp_data, Ast.DL_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_FP32 inp_data, Ast.DL_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_Bool inp_data, Ast.DL_Data_I8 out_data) => run_cast(inp_data, out_data)

    | (Ast.DL_Data_I8 inp_data, Ast.DL_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_U8 inp_data, Ast.DL_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I16 inp_data, Ast.DL_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I64 inp_data, Ast.DL_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_FP32 inp_data, Ast.DL_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_Bool inp_data, Ast.DL_Data_I32 out_data) => run_cast(inp_data, out_data)

    | (Ast.DL_Data_I8 inp_data, Ast.DL_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_U8 inp_data, Ast.DL_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I16 inp_data, Ast.DL_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I32 inp_data, Ast.DL_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I64 inp_data, Ast.DL_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_Bool inp_data, Ast.DL_Data_FP32 out_data) => run_cast(inp_data, out_data)

    | (Ast.DL_Data_I8 inp_data, Ast.DL_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_U8 inp_data, Ast.DL_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I16 inp_data, Ast.DL_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I32 inp_data, Ast.DL_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_I64 inp_data, Ast.DL_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.DL_Data_FP32 inp_data, Ast.DL_Data_Bool out_data) => run_cast(inp_data, out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

@private fun run_clip(inp: 't [], minval: 't, maxval: 't, out: 't []) =
    for x@idx <- inp {out[idx] = min(max(x, minval), maxval)}

fun run_clip(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op
{
| Ast.DL_Clip {t_inp, t_min, t_max, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    val minval = net.get_tensor(t_min)
    val maxval = net.get_tensor(t_max)
    match (inp.data, minval.data, maxval.data, out.data) {
    | (Ast.DL_Data_U8 inp_data, Ast.DL_Data_U8 min_data, Ast.DL_Data_U8 max_data, Ast.DL_Data_U8 out_data) =>
        run_clip(inp_data, min_data[0], max_data[0], out_data)
    | (Ast.DL_Data_I8 inp_data, Ast.DL_Data_I8 min_data, Ast.DL_Data_I8 max_data, Ast.DL_Data_I8 out_data) =>
        run_clip(inp_data, min_data[0], max_data[0], out_data)
    | (Ast.DL_Data_I32 inp_data, Ast.DL_Data_I32 min_data, Ast.DL_Data_I32 max_data, Ast.DL_Data_I32 out_data) =>
        run_clip(inp_data, min_data[0], max_data[0], out_data)
    | (Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 min_data, Ast.DL_Data_FP32 max_data, Ast.DL_Data_FP32 out_data) =>
        run_clip(inp_data, min_data[0], max_data[0], out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

@private fun run_constantOfShape(v: 't, out: 't []) = for _@idx <- out {out[idx] = v}

fun run_constantOfShape(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_ConstantOfShape {value, t_out} =>
    val out = net.get_tensor(t_out)
    match (value.data, out.data) {
    | (Ast.DL_Data_U8 v_data, Ast.DL_Data_U8 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.DL_Data_I8 v_data, Ast.DL_Data_I8 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.DL_Data_I32 v_data, Ast.DL_Data_I32 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.DL_Data_FP32 v_data, Ast.DL_Data_FP32 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

@private fun run_abs(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = abs(x)}
@private fun run_cos(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = cos(x)}
@private fun run_exp(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = exp(x)}
@private fun run_log(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = log(x)}
@private fun run_relu(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = max(x, (0:>'t))}
@private fun run_sigmoid(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = 1 / (1 + exp(-x))}
@private fun run_sign(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = (sign(x) :> 't)}
@private fun run_sin(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = sin(x)}
@private fun run_softplus(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = log(1 + exp(x))}
@private fun run_softsign(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = x/(1 + abs(x))}
@private fun run_sqrt(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = sqrt(x)}

fun run_unary(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op
{
| Ast.DL_Elemwise {el_op, t_inp, t_out} when t_inp.size() == 1 =>
    val inp = net.get_tensor(t_inp[0])
    val out = net.get_tensor(t_out)
    match (el_op, inp.data, out.data) {
    | (Ast.DL_Abs, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_abs(inp_data, out_data)
    | (Ast.DL_Cos, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_cos(inp_data, out_data)
    | (Ast.DL_Exp, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_exp(inp_data, out_data)
    | (Ast.DL_Log, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_log(inp_data, out_data)
    | (Ast.DL_Relu, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_relu(inp_data, out_data)
    | (Ast.DL_Sigmoid, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_sigmoid(inp_data, out_data)
    | (Ast.DL_Sign, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_sign(inp_data, out_data)
    | (Ast.DL_Sin, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_sin(inp_data, out_data)
    | (Ast.DL_Softplus, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_softplus(inp_data, out_data)
    | (Ast.DL_Softsign, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_softsign(inp_data, out_data)
    | (Ast.DL_Sqrt, Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_sqrt(inp_data, out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

// [TODO] add broadcast support
@private fun run_cmp_gt(inp0: 't [], inp1: 't [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x > y}
@private fun run_cmp_ge(inp0: 't [], inp1: 't [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x >= y}
@private fun run_cmp_lt(inp0: 't [], inp1: 't [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x < y}
@private fun run_cmp_le(inp0: 't [], inp1: 't [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x <= y}
@private fun run_cmp_eq(inp0: 't [], inp1: 't [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x == y}
@private fun run_cmp_ne(inp0: 't [], inp1: 't [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x != y}
@private fun run_add(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x + y}
@private fun run_sub(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x - y}
@private fun run_mul(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x * y}
@private fun run_div(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x / y}
@private fun run_mod(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x - floor(x / y)*y}
@private fun run_pow(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x ** y}
@private fun run_max(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = max(x, y)}
@private fun run_mean(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = (x + y)/2}
@private fun run_mean(inp0: float [], inp1: float [], out: float []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = (x + y)*0.5f}
@private fun run_min(inp0: 't [], inp1: 't [], out: 't []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = min(x, y)}
@private fun run_and(inp0: bool [], inp1: bool [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x & y}
@private fun run_or(inp0: bool [], inp1: bool [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x | y}
@private fun run_xor(inp0: bool [], inp1: bool [], out: bool []) =
    for x@idx <- inp0, y <- inp1 {out[idx] = x ^ y}

fun run_binary(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op
{
| Ast.DL_Elemwise {el_op, t_inp, t_out} when t_inp.size() == 2 =>
    val inp0 = net.get_tensor(t_inp[0])
    val inp1 = net.get_tensor(t_inp[1])
    val out = net.get_tensor(t_out)
    match (el_op, inp0.data, inp1.data, out.data) {
    | (Ast.DL_Add, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_add(inp0_data, inp1_data, out_data)
    | (Ast.DL_And, Ast.DL_Data_Bool inp0_data, Ast.DL_Data_Bool inp1_data, Ast.DL_Data_Bool out_data) =>
        run_and(inp0_data, inp1_data, out_data)
    | (Ast.DL_Div, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_div(inp0_data, inp1_data, out_data)
    | (Ast.DL_Equal, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_Bool out_data) =>
        run_cmp_eq(inp0_data, inp1_data, out_data)
    | (Ast.DL_Greater, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_Bool out_data) =>
        run_cmp_gt(inp0_data, inp1_data, out_data)
    | (Ast.DL_GreaterOrEqual, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_Bool out_data) =>
        run_cmp_ge(inp0_data, inp1_data, out_data)
    | (Ast.DL_Less, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_Bool out_data) =>
        run_cmp_lt(inp0_data, inp1_data, out_data)
    | (Ast.DL_LessOrEqual, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_Bool out_data) =>
        run_cmp_le(inp0_data, inp1_data, out_data)
    | (Ast.DL_Max, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_max(inp0_data, inp1_data, out_data)
    | (Ast.DL_Mean, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_mean(inp0_data, inp1_data, out_data)
    | (Ast.DL_Min, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_min(inp0_data, inp1_data, out_data)
    | (Ast.DL_Mod, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_mod(inp0_data, inp1_data, out_data)
    | (Ast.DL_Mul, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_mul(inp0_data, inp1_data, out_data)
    | (Ast.DL_Or, Ast.DL_Data_Bool inp0_data, Ast.DL_Data_Bool inp1_data, Ast.DL_Data_Bool out_data) =>
        run_or(inp0_data, inp1_data, out_data)
    | (Ast.DL_Pow, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_pow(inp0_data, inp1_data, out_data)
    | (Ast.DL_Sub, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_sub(inp0_data, inp1_data, out_data)
    | (Ast.DL_Xor, Ast.DL_Data_Bool inp0_data, Ast.DL_Data_Bool inp1_data, Ast.DL_Data_Bool out_data) =>
        run_xor(inp0_data, inp1_data, out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

fun run_nary(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op
{
| Ast.DL_Elemwise {el_op, t_inp, t_out} when t_inp.size() >= 2 =>
    val ninputs = t_inp.size()
    val inp = [| for i <- t_inp {net.get_tensor(i)} |]
    val out = net.get_tensor(t_out)
    match (el_op, inp[0].data, inp[1].data, out.data) {
    | (Ast.DL_Max, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_max(inp0_data, inp1_data, out_data)
        for j <- 2:ninputs {
            match inp[j].data {
            | Ast.DL_Data_FP32 inparrj => run_min(inparrj, out_data, out_data)
            | _ => throw TypeMismatchError
            }
        }
    | (Ast.DL_Mean, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_add(inp0_data, inp1_data, out_data)
        for j <- 2:ninputs {
            match inp[j].data {
            | Ast.DL_Data_FP32 inparrj => run_add(inparrj, out_data, out_data)
            | _ => throw TypeMismatchError
            }
        }
        val scale = 1.f/ninputs
        for x@idx <- out_data {out_data[idx] = x*scale}
    | (Ast.DL_Min, Ast.DL_Data_FP32 inp0_data, Ast.DL_Data_FP32 inp1_data, Ast.DL_Data_FP32 out_data) =>
        run_min(inp0_data, inp1_data, out_data)
        for j <- 2:ninputs {
            match inp[j].data {
            | Ast.DL_Data_FP32 inparrj => run_min(inparrj, out_data, out_data)
            | _ => throw TypeMismatchError
            }
        }
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

fun run_dropout(inp: 't [], out: 't [], ratio: float)
{
    val scale = 1.f/(1 - ratio);
    for x@idx <- inp {out[idx] = (x*scale :> 't)}
}

fun run_dropout(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op
{
| Ast.DL_Dropout {t_inp, t_ratio, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    val ratio = if t_ratio == 0 {0.5f} else {
        match net.get_tensor(t_ratio).data {
        | Ast.DL_Data_FP32 ratio_data => ratio_data[0]
        | _ => throw NotImplementedError
        }
    }
    match (inp.data, out.data) {
    | (Ast.DL_Data_FP32 inp_data, Ast.DL_Data_FP32 out_data) =>
        run_dropout(inp_data, out_data, ratio)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}