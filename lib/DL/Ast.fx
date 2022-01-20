/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Deep Models description

import Hashmap

type dltyp_t =
    | DL_Undefined | DL_I8 | DL_U8 | DL_I16 | DL_U16 | DL_I32 | DL_U32
    | DL_I64 | DL_U64 | DL_FP16 | DL_BF16 | DL_FP32 | DL_FP64

type dldata_t =
    | DL_Data_Empty
    | DL_Data_I8: int8 []
    | DL_Data_U8: uint8 []
    | DL_Data_I32: int32 []
    | DL_Data_I64: int64 []
    | DL_Data_FP32: float []

type dlargkind_t =
    | DL_Arg_Const
    | DL_Arg_Output
    | DL_Arg_State
    | DL_Arg_Buffer

type dllayout_t =
    | DL_Layout_Unknown
    | DL_Layout_NC
    | DL_Layout_NCHW
    | DL_Layout_NHWC

type dlpadding_t =
    | DL_Pad_None
    | DL_Pad_SameUpper
    | DL_Pad_SameLower
    | DL_Pad_Valid

type dlshape_t
{
    layout: dllayout_t
    shape: int []
}

type dltensor_t =
{
    shape: dlshape_t
    data: dldata_t
}

type dlarg_t =
{
    name: string
    argkind: dlargkind_t
    shape: dlshape_t
    typ: dltyp_t
    idx: int
}

type dlelwise1_t =
    | DL_Abs | DL_Acos | DL_Acosh | DL_Asin | DL_Asinh | DL_Atan | DL_Atanh
    | DL_Ceil | DL_Cos | DL_Cosh | DL_Erf | DL_Exp | DL_Floor | DL_IsInf | DL_IsNaN | DL_Log
    | DL_Neg | DL_Not | DL_Relu | DL_Round | DL_Sigmoid | DL_Sign | DL_Sin | DL_Sinh
    | DL_Softplus | DL_Softsign | DL_Sqrt | DL_Tan | DL_Tanh

type dlelwise2_t =
    | DL_Add | DL_And | DL_Div | DL_Equal | DL_Greater | DL_Less
    | DL_Mod | DL_Mul | DL_Or | DL_Sub | DL_Xor

type dlorder_t =
    | DL_RowMajor
    | DL_ColumnMajor

type dlop_t =
    | DL_AvgPool: {
        ceil_mode: bool
        dilation: (int, int) = (1, 1)
        kernel_shape: (int, int)
        pads: (int, int, int, int) = (0, 0, 0, 0)
        strides: (int, int)
        storage_order: dlorder_t
        count_include_pad: bool
        t_inp: int; t_out: int }
    | DL_BatchNorm: {
        epsilon: float
        momentum: float
        training_mode: bool
        t_inp: int; t_scale: int; t_B: int
        t_mean: int; t_var: int; t_out: int }
    | DL_Clip: {
        t_inp: int; t_min: int; t_max: int; t_out: int }
    | DL_Concat: {
        axis: int; t_inp: int []; t_out: int }
    | DL_Conv2D: {
        kernel_shape: (int, int)
        pads: (int, int, int, int) = (0, 0, 0, 0)
        strides: (int, int) = (1, 1)
        dilations: (int, int) = (1, 1)
        group: int = 1
        t_inp: int; t_weights: int; t_bias: int; t_out: int }
    | DL_Dropout: {
        seed: int = 0
        t_inp: int; t_ratio: int; t_training_mode: int; t_out: int }
    | DL_EWise1: {
        op: dlelwise1_t; t_inp: int; t_out: int}
    | DL_EWise2: {
        op: dlelwise2_t; t_inp1: int; t_inp2: int; t_out: int}
    | DL_Flatten: {
        axis: int; t_inp: int; t_out: int }
    | DL_Gather: {
        axis: int; t_inp: int; t_ind: int; t_out: int }
    | DL_Gemm: {
        alpha: float = 1.f
        beta: float = 1.f
        transA: bool = false
        transB: bool = false
        t_inp: int; t_weights: int; t_bias: int; t_out: int }
    | DL_GlobalAvgPool: {
        t_inp: int; t_out: int }
    | DL_LRN: {
        size: int
        alpha: float
        beta: float
        bias: float
        t_inp: int; t_out: int }
    | DL_MaxPool: {
        ceil_mode: bool
        dilation: (int, int) = (1, 1)
        kernel_shape: (int, int)
        pads: (int, int, int, int) = (0, 0, 0, 0)
        strides: (int, int)
        storage_order: dlorder_t = DL_RowMajor
        t_inp: int; t_out: int }
    | DL_Reshape: {
        allowzero: bool=false; t_inp: int; t_shape: int; t_out: int }
    | DL_Shape: {
        start: int; end: int; t_inp: int; t_out: int }
    | DL_Slice: {
        t_inp: int; t_starts: int; t_ends: int; t_axes: int; t_steps: int; t_out: int }
    | DL_SoftMax: {
        axis: int=-1; t_inp: int; t_out: int }
    | DL_Transpose: {
        perm: int []; t_inp: int; t_out: int }
    | DL_Unsqueeze: {
        t_inp: int; t_axes: int; t_out: int}

type dlnames_t = (string, int) Hashmap.t

type dlonnx_t =
{
    ir_version: int64
    producer: string
    domain: string
    doc_string: string
    opsets: (int64, string) list
}

type dlnet_info_t =
    | DL_Net_Generic
    | DL_Net_Onnx : dlonnx_t

type dlnet_t =
{
    info: dlnet_info_t
    inpargs: int []
    outargs: int []
    argnames: dlnames_t
    dimnames: dlnames_t
    dimnames_: string []
    args: dlarg_t []
    prog: dlop_t []
    consts: dltensor_t []
    outputs: dltensor_t []
    states: dltensor_t []
    buffers: dltensor_t []
}

fun empty_net() = dlnet_t {
    info = DL_Net_Generic,
    inpargs = [],
    outargs = [],
    argnames = Hashmap.empty(8, "", 0),
    dimnames = Hashmap.empty(8, "", 0),
    dimnames_ = [],
    args = [],
    prog = [],
    consts = [],
    outputs = [],
    states = [],
    buffers = []
}

fun string(order: dlorder_t)
{
    | DL_RowMajor => "RowMajor"
    | DL_ColumnMajor => "ColumnMajor"
}

fun string(layout: dllayout_t)
{
    | DL_Layout_Unknown => "Unknown"
    | DL_Layout_NC => "NC"
    | DL_Layout_NCHW => "NCHW"
    | DL_Layout_NHWC => "NHWC"
}

fun string(p: dlargkind_t) {
    | DL_Arg_Const => "const"
    | DL_Arg_Output => "out"
    | DL_Arg_State => "state"
    | DL_Arg_Buffer => "buffer"
}

fun string(ew: dlelwise1_t)
{
    | DL_Abs => "Abs"
    | DL_Acos => "Acos"
    | DL_Acosh => "Acosh"
    | DL_Asin => "Asin"
    | DL_Asinh => "Asinh"
    | DL_Atan => "Atan"
    | DL_Atanh => "Atanh"
    | DL_Ceil => "Ceil"
    | DL_Cos => "Cos"
    | DL_Cosh => "Cosh"
    | DL_Erf => "Erf"
    | DL_Exp => "Exp"
    | DL_Floor => "Floor"
    | DL_IsInf => "IsInf"
    | DL_IsNaN => "IsNaN"
    | DL_Log => "Log"
    | DL_Neg => "Neg"
    | DL_Not => "Not"
    | DL_Relu => "Relu"
    | DL_Round => "Round"
    | DL_Sigmoid => "Sigmoid"
    | DL_Sign => "Sign"
    | DL_Sin => "Sin"
    | DL_Sinh => "Sinh"
    | DL_Softplus => "Softplus"
    | DL_Softsign => "Softsign"
    | DL_Sqrt => "Sqrt"
    | DL_Tan => "Tan"
    | DL_Tanh => "Tanh"
}

fun string(ew: dlelwise2_t)
{
    | DL_Add => "Add"
    | DL_And => "And"
    | DL_Div => "Div"
    | DL_Equal => "Equal"
    | DL_Greater => "Greater"
    | DL_Less => "Less"
    | DL_Mod => "Mod"
    | DL_Mul => "Mul"
    | DL_Or => "Or"
    | DL_Sub => "Sub"
    | DL_Xor => "Xor"
}

fun string(p: dlpadding_t) {
    | DL_Pad_None => "NoPadding"
    | DL_Pad_SameUpper => "Pad_SameUpper"
    | DL_Pad_SameLower => "Pad_SameLower"
    | DL_Pad_Valid => "Pad_Valid"
}

fun total(d: dldata_t)
{
    | DL_Data_Empty => 0
    | DL_Data_I8(elems) => size(elems)
    | DL_Data_U8(elems) => size(elems)
    | DL_Data_I32(elems) => size(elems)
    | DL_Data_I64(elems) => size(elems)
    | DL_Data_FP32(elems) => size(elems)
}

fun float(d: dldata_t)
{
    | DL_Data_Empty => ([] : float [])
    | DL_Data_I8(elems) => float(elems)
    | DL_Data_U8(elems) => float(elems)
    | DL_Data_I32(elems) => float(elems)
    | DL_Data_I64(elems) => float(elems)
    | DL_Data_FP32(elems) => elems
}

fun tdata2str(d: dldata_t) =
match d {
    | DL_Data_Empty => "[]"
    | DL_Data_I8(elems) => string(elems)
    | DL_Data_U8(elems) => string(elems)
    | DL_Data_I32(elems) => string(elems)
    | DL_Data_I64(elems) => string(elems)
    | DL_Data_FP32(elems) => string(elems)
}

fun string(typ: dltyp_t)
{
    | DL_Undefined => "Undefined"
    | DL_I8 => "I8"
    | DL_U8 => "U8"
    | DL_I16 => "I16"
    | DL_U16 => "U16"
    | DL_I32 => "I32"
    | DL_U32 => "U32"
    | DL_I64 => "I64"
    | DL_U64 => "U64"
    | DL_FP16 => "FP16"
    | DL_BF16 => "BF16"
    | DL_FP32 => "FP32"
    | DL_FP64 => "FP64"
}

fun dim2str(net: dlnet_t, d: int) = if d > 0 {string(d)} else if d == 0 {"?"} else {net.dimnames_[-d-1]}

fun shape2str(net: dlnet_t, s: dlshape_t)
{
    val shape_str = "x".join([|for d <- s.shape {dim2str(net, d)}|])
    (match s.layout {
    | DL_Layout_Unknown => ""
    | _ => f"{s.layout} "
    }) + f"<{shape_str}>"
}

fun gettype(t: dldata_t) {
    | DL_Data_Empty => DL_Undefined
    | DL_Data_I8 _ => DL_I8
    | DL_Data_U8 _ => DL_U8
    | DL_Data_I32 _ => DL_I32
    | DL_Data_I64 _ => DL_I64
    | DL_Data_FP32 _ => DL_FP32
}

fun tensor2str(net: dlnet_t, t: dltensor_t, show_small: bool) =
match t.data {
    | DL_Data_Empty => "[]"
    | _ =>
        val sp = shape2str(net, t.shape)
        val tprefix = string(gettype(t.data))
        val nelems = total(t.data)
        val tdata_str = if nelems <= 10 && show_small {tdata2str(t.data)} else {"[...]"}
        sp + " " + tprefix + " " + tdata_str
}

fun arg2str(net: dlnet_t, t: dlarg_t)
{
    val sp = shape2str(net, t.shape)
    val cprefix = match t.argkind { DL_Arg_Buffer => "" | _ => string(t.argkind) + " " }
    val tdatastr = match t {
        | {argkind=DL_Arg_Const, shape={shape}, idx}
            when idx > 0 && size(shape) == 1 && shape[0] < 10 =>
            ": " + tdata2str(net.consts[idx].data)
        | _ => ""
    }
    cprefix + sp + " " + string(t.typ) + tdatastr
}

fun parse_params(params: string): string list
{
    var paren_stack: char list = []
    fun issep(c: char) = match (c, paren_stack) {
        | ('(', _) | ('[', _) | ('{', _) => paren_stack = c :: paren_stack; false
        | (')', '(' :: rest) => paren_stack = rest; false
        | (']', '[' :: rest) => paren_stack = rest; false
        | ('}', '{' :: rest) => paren_stack = rest; false
        | (')', _) | (']', _) | ('}', _) =>
            throw Fail("unexpected closing ')', ']' or '}' in the parameters")
        | (',', []) => true
        | _ => false
    }
    params.tokens(issep).map(String.strip)
}

fun op2str(opname: string, params: string, tensors: string list)
{
    val indent = "   "
    val pl = parse_params(params)
    val pprefix = if pl == [] {""} else {f"\n{indent}"}
    join_embrace(f"{opname} {{\n{indent}",
        join_embrace(pprefix, "\n}", f",\n{indent}", pl),
        f"\n{indent}", tensors)
}

fun gettensor(net: dlnet_t, arg: dlarg_t) =
match arg.argkind {
    | DL_Arg_Const => net.consts[arg.idx]
    | DL_Arg_Output => net.outputs[arg.idx]
    | DL_Arg_State => net.states[arg.idx]
    | DL_Arg_Buffer => net.buffers[arg.idx]
}

fun t2str(net: dlnet_t, tensors: (string, int) list) =
    [for (name, tidx) <- tensors {
        val targ = net.args[tidx]
        f"{name}=\"{targ.name}\", // {arg2str(net, targ)}"
    }]

fun op2str(net: dlnet_t, op: dlop_t) =
match op {
    | DL_AvgPool { ceil_mode, dilation, kernel_shape, pads,
        strides, storage_order, count_include_pad, t_inp, t_out} =>
        op2str("AvgPool", f"ceil_mode={ceil_mode},\ndilation={dilation},\nkernel_shape={kernel_shape},\n\
            pads={pads},\nstrides={strides},\nstorage_order={storage_order},\ncount_include_pad={count_include_pad}",
            t2str(net, [("t_inp", t_inp), ("t_out", t_out)]))
    | DL_BatchNorm {epsilon, momentum, training_mode, t_inp, t_scale, t_B, t_mean, t_var, t_out} =>
        op2str("BatchNorm", f"epsilon={epsilon},\nmomentum={momentum},\ntraining_mode={training_mode}",
            t2str(net, [("t_inp", t_inp), ("t_scale", t_scale), ("t_B", t_B),
            ("t_mean", t_mean), ("t_var", t_var), ("t_out", t_out)]))
    | DL_Concat {axis, t_inp, t_out} =>
        op2str("Concat", f"{{axis={axis}}}", t2str(net, [for t_inp_i@i <- t_inp {(f"t_inp{i}", t_inp_i)}] + [("t_out", t_out)]))
    | DL_Clip {t_inp, t_min, t_max, t_out} =>
        op2str("Clip", "", t2str(net, [("t_inp", t_inp), ("t_min", t_min), ("t_max", t_max), ("t_out", t_out)]))
    | DL_Conv2D {kernel_shape, pads=pads, strides, dilations, group, t_inp, t_weights, t_bias, t_out} =>
        op2str("Conv2D", f"kernel_shape={kernel_shape},\n\
            pads={pads},\nstrides={strides},\ndilations={dilations},\ngroup={group}",
            t2str(net, [("t_inp", t_inp), ("t_weights", t_weights), ("t_bias", t_bias), ("t_out", t_out)]))
    | DL_Dropout {seed, t_inp, t_ratio, t_training_mode, t_out} =>
        op2str("Dropout", f"seed={seed}", t2str(net,
            [("t_inp", t_inp), ("t_ratio", t_ratio), ("t_training_mode", t_training_mode), ("t_out", t_out)]))
    | DL_EWise1 {op, t_inp, t_out} =>
        op2str(string(op), "", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]))
    | DL_EWise2 {op, t_inp1, t_inp2, t_out} =>
        op2str(string(op), "", t2str(net, [("inp1", t_inp1), ("inp2", t_inp2), ("t_out", t_out)]))
    | DL_Flatten {axis, t_inp, t_out} =>
        op2str("Flatten", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]))
    | DL_Gather {axis, t_inp, t_ind, t_out} =>
        op2str("Gather", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_ind", t_ind), ("t_out", t_out)]))
    | DL_Gemm {alpha, beta, transA, transB, t_inp, t_weights, t_bias, t_out} =>
        op2str("Gemm", f"alpha={alpha},\nbeta={beta},\ntransA={transA},\ntransB={transB}",
        t2str(net, [("t_inp", t_inp), ("t_weights", t_weights), ("t_bias", t_bias), ("t_out", t_out)]))
    | DL_GlobalAvgPool {t_inp, t_out} =>
        op2str("GlobalAvgPool", "", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]))
    | DL_LRN {size, alpha, beta, bias, t_inp, t_out} =>
        op2str("LRN", f"size={size},\nalpha={alpha},\nbeta={beta},\nbias={bias}",
        t2str(net, [("t_inp", t_inp), ("t_out", t_out)]))
    | DL_MaxPool { ceil_mode, dilation, kernel_shape, pads,
        strides, storage_order, t_inp, t_out } =>
        op2str("MaxPool", f"ceil_mode={ceil_mode}, dilation={dilation}, kernel_shape={kernel_shape}, \
            pads={pads}, strides={strides}, storage_order={storage_order}",
            t2str(net, [("t_inp", t_inp), ("t_out", t_out)]))
    | DL_Reshape {allowzero, t_inp, t_shape, t_out} =>
        op2str("Reshape", f"allowzero={allowzero}", t2str(net, [("t_inp", t_inp), ("t_shape", t_shape), ("t_out", t_out)]))
    | DL_Shape {start, end, t_inp, t_out} =>
        op2str("Shape", f"start={start}, end={end}", t2str(net, [("t_data", t_inp), ("t_shape", t_out)]))
    | DL_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
        op2str("Slice", "", t2str(net, [("t_inp", t_inp), ("t_starts", t_starts),
            ("t_ends", t_ends), ("t_axes", t_axes), ("t_steps", t_steps), ("t_out", t_out)]))
    | DL_SoftMax {axis, t_inp, t_out} =>
        op2str("SoftMax", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]))
    | DL_Transpose {perm, t_inp, t_out} =>
        op2str("Tranpose", f"perm={perm}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]))
    | DL_Unsqueeze {t_inp, t_axes, t_out} =>
        op2str("Unsqueeze", "", t2str(net, [("t_inp", t_inp), ("t_axes", t_axes), ("t_out", t_out)]))
}

fun string(info: dlnet_info_t) {
    | DL_Net_Generic => "Generic_Net {}"
    | DL_Net_Onnx(onnx) =>
        val opsets=
            if onnx.opsets == [] {"[]"}
            else {
                join_embrace("[\n       ", "\n    ]", ",\n        ",
                    [for (ver, domain) <- onnx.opsets {f"\"{domain} v{ver}\""}])
            }
        f"Imported_from_Onnx {{
    ir_version={onnx.ir_version},
    producer=\"{onnx.producer}\",
    domain=\"{onnx.domain}\",
    doc_string=\"{onnx.doc_string}\",
    opsets={opsets}
}}"
}

fun print(net: dlnet_t)
{
    match net.info {
    | DL_Net_Generic => {}
    | _ => println(string(net.info))
    }
    for (n, ts) <- [("inputs", net.inpargs), ("outputs", net.outargs)] {
        val ts = [for tidx <- ts {
            val targ = net.args[tidx]
            f"\"{targ.name}\", // {arg2str(net, targ)}"
        }]
        println(join_embrace(f"{n}=[\n", "\n]", "\n    ", ts))
    }

    for p <- net.prog {
        println(op2str(net, p))
    }
}
