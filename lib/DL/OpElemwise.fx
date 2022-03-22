/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// various element-wise operations
import Ast

@private fun run_cast(inp: 'inpT [], out: 'outT []) = for e@idx <- inp {out[idx] = (e :> 'outT)}
@private fun run_cast(inp: float [], out: uint8 []) = for e@idx <- inp {out[idx] = sat_uint8(e)}
@private fun run_cast(inp: float [], out: int8 []) = for e@idx <- inp {out[idx] = sat_int8(e)}
@private fun run_cast(inp: float [], out: int16 []) = for e@idx <- inp {out[idx] = sat_int16(e)}
@private fun run_cast(inp: float [], out: int32 []) = for e@idx <- inp {out[idx] = int32(round(e))}

fun run_cast(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_Cast {t_inp, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    match (inp.data, out.data) {
    | (Ast.DL_Data_Empty, _) => {}
    | (_, Ast.DL_Data_Empty) => {}
    | (Ast.DL_Data_I8 inparr, Ast.DL_Data_U8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I16 inparr, Ast.DL_Data_U8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I32 inparr, Ast.DL_Data_U8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I64 inparr, Ast.DL_Data_U8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_FP32 inparr, Ast.DL_Data_U8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_Bool inparr, Ast.DL_Data_U8 outarr) => run_cast(inparr, outarr)

    | (Ast.DL_Data_U8 inparr, Ast.DL_Data_I8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I16 inparr, Ast.DL_Data_I8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I32 inparr, Ast.DL_Data_I8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I64 inparr, Ast.DL_Data_I8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_FP32 inparr, Ast.DL_Data_I8 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_Bool inparr, Ast.DL_Data_I8 outarr) => run_cast(inparr, outarr)

    | (Ast.DL_Data_I8 inparr, Ast.DL_Data_I32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_U8 inparr, Ast.DL_Data_I32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I16 inparr, Ast.DL_Data_I32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I64 inparr, Ast.DL_Data_I32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_FP32 inparr, Ast.DL_Data_I32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_Bool inparr, Ast.DL_Data_I32 outarr) => run_cast(inparr, outarr)

    | (Ast.DL_Data_I8 inparr, Ast.DL_Data_FP32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_U8 inparr, Ast.DL_Data_FP32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I16 inparr, Ast.DL_Data_FP32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I32 inparr, Ast.DL_Data_FP32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I64 inparr, Ast.DL_Data_FP32 outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_Bool inparr, Ast.DL_Data_FP32 outarr) => run_cast(inparr, outarr)

    | (Ast.DL_Data_I8 inparr, Ast.DL_Data_Bool outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_U8 inparr, Ast.DL_Data_Bool outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I16 inparr, Ast.DL_Data_Bool outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I32 inparr, Ast.DL_Data_Bool outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_I64 inparr, Ast.DL_Data_Bool outarr) => run_cast(inparr, outarr)
    | (Ast.DL_Data_FP32 inparr, Ast.DL_Data_Bool outarr) => run_cast(inparr, outarr)
    | _ => throw Ast.DLError(f"unsupported combination of formats in {op.name()}")
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

@private fun run_clip(inp: 't [], minval: 't, maxval: 't, out: 't []) =
    for e@idx <- inp {out[idx] = min(max(e, minval), maxval)}

fun run_clip(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op
{
| Ast.DL_Clip {t_inp, t_min, t_max, t_out} =>
    val inp = net.get_tensor(t_inp)
    val out = net.get_tensor(t_out)
    val minval = net.get_tensor(t_min)
    val maxval = net.get_tensor(t_max)
    match (inp.data, minval.data, maxval.data, out.data) {
    | (Ast.DL_Data_U8 inparr, Ast.DL_Data_U8 minarr, Ast.DL_Data_U8 maxarr, Ast.DL_Data_U8 outarr) =>
        run_clip(inparr, minarr[0], maxarr[0], outarr)
    | (Ast.DL_Data_I8 inparr, Ast.DL_Data_I8 minarr, Ast.DL_Data_I8 maxarr, Ast.DL_Data_I8 outarr) =>
        run_clip(inparr, minarr[0], maxarr[0], outarr)
    | (Ast.DL_Data_I32 inparr, Ast.DL_Data_I32 minarr, Ast.DL_Data_I32 maxarr, Ast.DL_Data_I32 outarr) =>
        run_clip(inparr, minarr[0], maxarr[0], outarr)
    | (Ast.DL_Data_FP32 inparr, Ast.DL_Data_FP32 minarr, Ast.DL_Data_FP32 maxarr, Ast.DL_Data_FP32 outarr) =>
        run_clip(inparr, minarr[0], maxarr[0], outarr)
    | _ => throw Ast.DLError(f"unsupported combination of formats in {op.name()}")
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}

@private fun run_constantOfShape(v: 't, out: 't []) = for _@idx <- out {out[idx] = v}

fun run_constantOfShape(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op {
| Ast.DL_ConstantOfShape {value, t_out} =>
    val out = net.get_tensor(t_out)
    match (value.data, out.data) {
    | (Ast.DL_Data_U8 varr, Ast.DL_Data_U8 outarr) =>
        OpElemwise.run_constantOfShape(varr[0], outarr)
    | (Ast.DL_Data_I8 varr, Ast.DL_Data_I8 outarr) =>
        OpElemwise.run_constantOfShape(varr[0], outarr)
    | (Ast.DL_Data_I32 varr, Ast.DL_Data_I32 outarr) =>
        OpElemwise.run_constantOfShape(varr[0], outarr)
    | (Ast.DL_Data_FP32 varr, Ast.DL_Data_FP32 outarr) =>
        OpElemwise.run_constantOfShape(varr[0], outarr)
    | _ => throw Ast.DLError(f"unsupported combination of formats in {op.name()}")
    }
| _ => throw Ast.DLError(f"unexpected op {op.name()}")
}
