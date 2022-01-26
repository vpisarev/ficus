/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Deep Models description

import Hashmap

type dltyp_t =
    | DL_Undefined | DL_I8 | DL_U8 | DL_I16 | DL_U16 | DL_I32 | DL_U32
    | DL_I64 | DL_U64 | DL_FP16 | DL_BF16 | DL_FP32 | DL_FP64 | DL_BOOL

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

type dlpooling_t =
    | DL_Pool_Avg
    | DL_Pool_Max

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

type dlelwise_t =
    | DL_Abs | DL_Acos | DL_Acosh | DL_Asin | DL_Asinh | DL_Atan | DL_Atanh
    | DL_Ceil | DL_Cos | DL_Cosh | DL_Erf | DL_Exp | DL_Floor | DL_IsInf | DL_IsNaN | DL_Log
    | DL_Neg | DL_Not | DL_Relu | DL_Round | DL_Sigmoid | DL_Sign | DL_Sin | DL_Sinh
    | DL_Softplus | DL_Softsign | DL_Sqrt | DL_Tan | DL_Tanh
    | DL_Add | DL_And | DL_Div | DL_Equal | DL_Greater | DL_Less
    | DL_Mod | DL_Mul | DL_Or | DL_Sub | DL_Xor
    | DL_Min | DL_Max | DL_Mean

type dlreduce_t =
    | DL_ReduceMin | DL_ReduceMax | DL_ReduceMean
    | DL_ReduceL1 | DL_ReduceL2
    | DL_ReduceLogSum | DL_ReduceLogSumExp
    | DL_ReduceProd | DL_ReduceSum | DL_ReduceSumSquare

type dlorder_t =
    | DL_RowMajor
    | DL_ColumnMajor

type dlcoord_trans_t =
    | DL_CT_HalfPixel
    | DL_CT_PyTorchHalfPixel
    | DL_CT_AlignCorners
    | DL_CT_Asymmetric
    | DL_CT_TFCropResize
    | DL_CT_OutHalfPixel

type dlinterpolation_t =
    | DL_Inter_Nearest
    | DL_Inter_Linear
    | DL_Inter_Cubic

type dlnearest_mode_t =
    | DL_Nearest_RoundPreferFloor
    | DL_Nearest_RoundPreferCeil
    | DL_Nearest_Floor
    | DL_Nearest_Ceil

type dlop_t =
    | DL_AvgPool: {
        name: string
        ceil_mode: bool
        dilations: int []
        kernel_shape: int []
        pads: int []
        strides: int []
        storage_order: dlorder_t
        count_include_pad: bool
        t_inp: int; t_out: int }
    | DL_BatchNorm: {
        name: string
        epsilon: float
        momentum: float
        training_mode: bool
        t_inp: int; t_scale: int; t_B: int
        t_mean: int; t_var: int; t_out: int }
    | DL_Cast: {
        name: string; to: dltyp_t; t_inp: int; t_out: int }
    | DL_Clip: {
        name: string; t_inp: int; t_min: int; t_max: int; t_out: int }
    | DL_Concat: {
        name: string; axis: int; t_inp: int []; t_out: int }
    | DL_ConstantOfShape: {
        name: string; value: dltensor_t; t_shape: int; t_out: int }
    | DL_Conv: {
        name: string
        kernel_shape: int []
        pads: int []
        strides: int []
        dilations: int []
        group: int
        t_inp: int; t_weights: int; t_bias: int; t_out: int }
    | DL_ConvTranspose: {
        name: string
        kernel_shape: int []
        pads: int []
        strides: int []
        dilations: int []
        out_shape: int []
        out_padding: int []
        group: int
        t_inp: int; t_weights: int; t_bias: int; t_out: int }
    | DL_Dropout: {
        name: string; seed: int = 0
        t_inp: int; t_ratio: int; t_training_mode: int; t_out: int }
    | DL_Elemwise: {
        name: string; op: dlelwise_t; t_inp: int []; t_out: int }
    | DL_Expand: {
        name: string; t_inp: int; t_shape: int; t_out: int }
    | DL_Flatten: {
        name: string; axis: int; t_inp: int; t_out: int }
    | DL_Gather: {
        name: string; axis: int; t_inp: int; t_ind: int; t_out: int }
    | DL_Gemm: {
        name: string
        alpha: float = 1.f
        beta: float = 1.f
        transA: bool = false
        transB: bool = false
        t_inp: int; t_weights: int; t_bias: int; t_out: int }
    | DL_GlobalAvgPool: {
        name: string; t_inp: int; t_out: int }
    | DL_Identity: {
        name: string; t_inp: int; t_out: int }
    | DL_If: {
        name: string; then_branch: dlgraph_t; else_branch: dlgraph_t; t_inp: int; t_out: int [] }
    | DL_LeakyRelu: {
        name: string; alpha: float; t_inp: int; t_out: int }
    | DL_Loop: {
        name: string; body: dlgraph_t; t_trip_count: int;
        t_cond_in: int; t_v_in: int [];
        t_cond_out: int; t_v_out: int [] }
    | DL_LRN: {
        name: string; size: int; alpha: float
        beta: float; bias: float
        t_inp: int; t_out: int }
    | DL_MaxPool: {
        name: string
        ceil_mode: bool
        dilations: int []
        kernel_shape: int []
        pads: int []
        strides: int []
        storage_order: dlorder_t = DL_RowMajor
        t_inp: int; t_out: int }
    | DL_NonMaxSuppression: {
        name: string
        center_point_box: bool;
        t_boxes: int;
        t_scores: int;
        t_max_output_boxes_per_class: int;
        t_iou_threshold: int;
        t_score_threshold: int;
        t_out: int }
    | DL_NonZero: {
        name: string; t_inp: int; t_out: int }
    | DL_Range: {
        name: string; t_start: int; t_limit: int; t_delta: int; t_out: int }
    | DL_Reduce: {
        name: string; reduce_op: dlreduce_t; axes: int []; keepdims: bool; t_inp: int; t_out: int }
    | DL_Reshape: {
        name: string; allowzero: bool=false; t_inp: int; t_shape: int; t_out: int }
    | DL_Resize: {
        name: string
        coord_trans: dlcoord_trans_t
        cubic_coeff_a: float
        exclude_outside: bool
        extrapolation_value: float
        mode: dlinterpolation_t
        nearest_mode: dlnearest_mode_t
        t_inp: int; t_scales: int; t_sizes: int
        t_roi: int; t_out: int }
    | DL_RoiAlign: {
        name: string
        coord_trans: dlcoord_trans_t;
        mode: dlpooling_t;
        output_height: int; output_width: int;
        sampling_ratio: int; spatial_scale: float;
        t_inp: int; t_rois: int; t_batch_ind: int; t_out: int }
    | DL_Scatter: {
        name: string; axis: int; t_data: int; t_updates: int; t_indices: int; t_out: int }
    | DL_Shape: {
        name: string; start: int; end: int; t_inp: int; t_out: int }
    | DL_Slice: {
        name: string; t_inp: int; t_starts: int; t_ends: int; t_axes: int; t_steps: int; t_out: int }
    | DL_SoftMax: {
        name: string; axis: int=-1; t_inp: int; t_out: int }
    | DL_Split: {
        name: string; axis: int; t_inp: int; t_split: int; t_out: int [] }
    | DL_Squeeze: {
        name: string; t_inp: int; t_axes: int; t_out: int }
    | DL_Tile: {
        name: string; t_inp: int; t_repeats: int; t_out: int }
    | DL_TopK: {
        name: string; axis: int; largest: bool; sorted: bool; t_inp: int; t_K: int; t_out: int; t_out_ind: int }
    | DL_Transpose: {
        name: string; perm: int []; t_inp: int; t_out: int }
    | DL_Unsqueeze: {
        name: string; t_inp: int; t_axes: int; t_out: int }

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

type dlgraph_t =
    DL_Graph: {
        inpargs: int []
        outargs: int []
        prog: dlop_t [] }

type dlnet_t =
{
    info: dlnet_info_t
    argnames: dlnames_t
    dimnames: dlnames_t
    dimnames_: string []
    args: dlarg_t []
    consts: dltensor_t []
    outputs: dltensor_t []
    buffers: dltensor_t []
    graph: dlgraph_t
}

fun empty_net() = dlnet_t {
    info = DL_Net_Generic,
    argnames = Hashmap.empty(8, "", 0),
    dimnames = Hashmap.empty(8, "", 0),
    dimnames_ = [],
    args = [],
    consts = [],
    outputs = [],
    buffers = [],
    graph = DL_Graph {inpargs = [], outargs = [], prog=[]}
}

fun empty_graph() = DL_Graph {
    inpargs = [],
    outargs = [],
    prog = []
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
    | DL_Arg_Buffer => "buffer"
}

fun string(ew: dlelwise_t)
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

    | DL_Min => "Min"
    | DL_Max => "Max"
    | DL_Mean => "Mean"
}

fun string(r: dlreduce_t)
{
    | DL_ReduceMin => "ReduceMin"
    | DL_ReduceMax => "ReduceMax"
    | DL_ReduceMean => "ReduceMean"
    | DL_ReduceL1 => "ReduceL1"
    | DL_ReduceL2 => "ReduceL2"
    | DL_ReduceLogSum => "ReduceLogSum"
    | DL_ReduceLogSumExp => "ReduceLogSumExp"
    | DL_ReduceProd => "ReduceProd"
    | DL_ReduceSum => "ReduceSum"
    | DL_ReduceSumSquare => "ReduceSumSquare"
}

fun string(p: dlpadding_t) {
    | DL_Pad_None => "NoPadding"
    | DL_Pad_SameUpper => "Pad_SameUpper"
    | DL_Pad_SameLower => "Pad_SameLower"
    | DL_Pad_Valid => "Pad_Valid"
}

fun string(interpolation: dlinterpolation_t)
{
    | DL_Inter_Nearest => "Nearest"
    | DL_Inter_Linear => "Linear"
    | DL_Inter_Cubic => "Cubic"
}

fun string(coord_trans: dlcoord_trans_t)
{
    | DL_CT_HalfPixel => "HalfPixel"
    | DL_CT_PyTorchHalfPixel => "PyTorchHalfPixel"
    | DL_CT_AlignCorners => "AlignCorners"
    | DL_CT_Asymmetric => "Asymmetric"
    | DL_CT_TFCropResize => "TFCropResize"
    | DL_CT_OutHalfPixel => "OutputHalfPixel"
}

fun string(nearest_round: dlnearest_mode_t)
{
    | DL_Nearest_RoundPreferFloor => "Nearest_RoundPreferFloor"
    | DL_Nearest_RoundPreferCeil => "Nearest_RoundPreferCeil"
    | DL_Nearest_Ceil => "Nearest_Ceil"
    | DL_Nearest_Floor => "Nearest_Floor"
}

fun string(mode: dlpooling_t)
{
    | DL_Pool_Max => "MaxPooling"
    | DL_Pool_Avg => "AveragePooling"
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

fun arr2str(elems: 't []) = join_embrace("[", "]", ",", elems.map(repr))

fun tdata2str(d: dldata_t) {
    match d {
        | DL_Data_Empty => "[]"
        | DL_Data_I8(elems) => arr2str(elems)
        | DL_Data_U8(elems) => arr2str(elems)
        | DL_Data_I32(elems) => arr2str(elems)
        | DL_Data_I64(elems) => arr2str(elems)
        | DL_Data_FP32(elems) => arr2str(elems)
    }
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
    | DL_BOOL => "BOOL"
}

fun dim2str(net: dlnet_t, d: int) = if d > 0 {string(d)} else if d == 0 {"?"} else {net.dimnames_[-d-1]}

fun shape2str(net: dlnet_t, s: dlshape_t)
{
    val shape_str = " x ".join([|for d <- s.shape {dim2str(net, d)}|])
    (match (s.layout, size(s.shape)) {
    | (DL_Layout_Unknown, _) => ""
    | (_, dims) when dims > 1 => f"{s.layout} "
    | _ => ""
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

fun op2str(name: string, opname: string, params: string, tensors: string list, indent0: string)
{
    val indent = indent0 + "   "
    val pl = parse_params(params)
    val pprefix = if pl == [] {""} else {f"\n{indent}"}
    join_embrace(f"{opname} {{\n{indent}name=\"{name}\",\n{indent}",
        join_embrace(pprefix, f"\n{indent0}}}", f",\n{indent}", pl),
        f"\n{indent}", tensors)
}

fun gettensor(net: dlnet_t, arg: dlarg_t) =
match arg.argkind {
    | DL_Arg_Const => net.consts[arg.idx]
    | DL_Arg_Output => net.outputs[arg.idx]
    | DL_Arg_Buffer => net.buffers[arg.idx]
}

fun t2str(net: dlnet_t, tensors: (string, int) list) =
    [for (name, tidx) <- tensors {
        val targ = net.args[tidx]
        f"{name}=\"{targ.name}\", // {arg2str(net, targ)}"
    }]

fun graph2str(net: dlnet_t, graph: dlgraph_t, indent: string)
{
    val {inpargs, outargs, prog} = graph
    val new_indent = indent + "  "
    val prog_indent = new_indent + "  "
    val inpstrs = [| for a <- inpargs {net.args[a].name} |]
    val outstrs = [| for a <- outargs {net.args[a].name} |]
    val prog = [| for op <- prog {op2str(net, op, prog_indent)} |]
    join_embrace(f"graph {{\n{new_indent}inputs={inpstrs},\n\
        {new_indent}outputs={outstrs},\n{new_indent}prog={{\n{prog_indent}",
        f"\n{new_indent}}}\n{indent}}}",
        f",\n{prog_indent}", prog)
}

fun get_opname(op: dlop_t) {
    | DL_AvgPool {name} => (name, "AvgPool")
    | DL_BatchNorm {name} => (name, "BatchNorm")
    | DL_Cast {name} => (name, "Cast")
    | DL_Clip {name} => (name, "Clip")
    | DL_Concat {name} => (name, "Concat")
    | DL_ConstantOfShape {name} => (name, "ConstantOfShape")
    | DL_Conv {name} => (name, "Conv")
    | DL_ConvTranspose {name} => (name, "ConvTranspose")
    | DL_Dropout {name} => (name, "Dropout")
    | DL_Elemwise {name, op} => (name, f"Elemwise{op}")
    | DL_Expand {name} => (name, "Expand")
    | DL_Flatten {name} => (name, "Flatten")
    | DL_Gather {name} => (name, "Gather")
    | DL_Gemm {name} => (name, "Gemm")
    | DL_GlobalAvgPool {name} => (name, "GlobalAvgPool")
    | DL_Identity {name} => (name, "Identity")
    | DL_If {name} => (name, "If")
    | DL_LeakyRelu {name} => (name, "LeakyRelu")
    | DL_Loop {name} => (name, "Loop")
    | DL_LRN {name} => (name, "LRN")
    | DL_MaxPool {name} => (name, "MaxPool")
    | DL_NonMaxSuppression {name} => (name, "NonMaxSuppression")
    | DL_NonZero {name} => (name, "NonZero")
    | DL_Range {name} => (name, "Range")
    | DL_Reduce {name, reduce_op} => (name, string(reduce_op))
    | DL_Resize {name} => (name, "Resize")
    | DL_Reshape {name} => (name, "Reshape")
    | DL_RoiAlign {name} => (name, "RoiAlign")
    | DL_Scatter {name} => (name, "Scatter")
    | DL_Shape {name} => (name, "Shape")
    | DL_Slice {name} => (name, "Slice")
    | DL_SoftMax {name} => (name, "SoftMax")
    | DL_Split {name} => (name, "Split")
    | DL_Squeeze {name} => (name, "Squeeze")
    | DL_Tile {name} => (name, "Tile")
    | DL_TopK {name} => (name, "TopK")
    | DL_Transpose {name} => (name, "Transpose")
    | DL_Unsqueeze {name} => (name, "Unsqueeze")
}

fun targs2pairs(prefix: string, args: int []) = [for a@i <- args {(f"{prefix}{i}", a)}]

fun op2str(net: dlnet_t, op: dlop_t, indent: string)
{
    val sub_indent = indent + "  "
    //println(f"dumping op={get_opname(op)}")
    match op {
    | DL_AvgPool { name, ceil_mode, dilations, kernel_shape, pads,
        strides, storage_order, count_include_pad, t_inp, t_out} =>
        op2str(name, "AvgPool", f"ceil_mode={ceil_mode},\ndilations={dilations},\nkernel_shape={kernel_shape},\n\
            pads={pads},\nstrides={strides},\nstorage_order={storage_order},\ncount_include_pad={count_include_pad}",
            t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_BatchNorm {name, epsilon, momentum, training_mode, t_inp, t_scale, t_B, t_mean, t_var, t_out} =>
        op2str(name, "BatchNorm", f"epsilon={epsilon},\nmomentum={momentum},\ntraining_mode={training_mode}",
            t2str(net, [("t_inp", t_inp), ("t_scale", t_scale), ("t_B", t_B),
            ("t_mean", t_mean), ("t_var", t_var), ("t_out", t_out)]), indent)
    | DL_Cast {name, to, t_inp, t_out} =>
        op2str(name, "Cast", f"to={to}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Clip {name, t_inp, t_min, t_max, t_out} =>
        op2str(name, "Clip", "", t2str(net, [("t_inp", t_inp), ("t_min", t_min), ("t_max", t_max), ("t_out", t_out)]), indent)
    | DL_Concat {name, axis, t_inp, t_out} =>
        op2str(name, "Concat", f"axis={axis}", t2str(net, targs2pairs("t_inp", t_inp) + [("t_out", t_out)]), indent)
    | DL_ConstantOfShape {name, value, t_shape, t_out} =>
        op2str(name, "ConstantOfShape", f"value={tensor2str(net, value, true)}",
            t2str(net, [("t_shape", t_shape), ("t_out", t_out)]), indent)
    | DL_Conv {name, kernel_shape, pads, strides, dilations, group, t_inp, t_weights, t_bias, t_out} =>
        op2str(name, "Conv", f"kernel_shape={kernel_shape}, \
            pads={pads}, strides={strides}, dilations={dilations}, group={group}",
            t2str(net, [("t_inp", t_inp), ("t_weights", t_weights), ("t_bias", t_bias), ("t_out", t_out)]), indent)
    | DL_ConvTranspose {name, kernel_shape, pads, strides, dilations, group,
        out_shape, out_padding, t_inp, t_weights, t_bias, t_out} =>
        op2str(name, "Conv", f"kernel_shape={kernel_shape}, \
            pads={pads}, strides={strides}, dilations={dilations}, group={group}, out_padding={out_padding}, out_shape={out_shape}",
            t2str(net, [("t_inp", t_inp), ("t_weights", t_weights), ("t_bias", t_bias), ("t_out", t_out)]), indent)
    | DL_Dropout {name, seed, t_inp, t_ratio, t_training_mode, t_out} =>
        op2str(name, "Dropout", f"seed={seed}", t2str(net,
            [("t_inp", t_inp), ("t_ratio", t_ratio), ("t_training_mode", t_training_mode), ("t_out", t_out)]), indent)
    | DL_Elemwise {name, op, t_inp, t_out} =>
        op2str(name, string(op), "", t2str(net, targs2pairs("t_inp", t_inp) + [("t_out", t_out)]), indent)
    | DL_Expand {name, t_inp, t_shape, t_out} =>
        op2str(name, "Expand", "", t2str(net, [("t_inp", t_inp), ("t_shape", t_shape), ("t_out", t_out)]), indent)
    | DL_Flatten {name, axis, t_inp, t_out} =>
        op2str(name, "Flatten", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Gather {name, axis, t_inp, t_ind, t_out} =>
        op2str(name, "Gather", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_ind", t_ind), ("t_out", t_out)]), indent)
    | DL_Gemm {name, alpha, beta, transA, transB, t_inp, t_weights, t_bias, t_out} =>
        op2str(name, "Gemm", f"alpha={alpha},\nbeta={beta},\ntransA={transA},\ntransB={transB}",
        t2str(net, [("t_inp", t_inp), ("t_weights", t_weights), ("t_bias", t_bias), ("t_out", t_out)]), indent)
    | DL_GlobalAvgPool {name, t_inp, t_out} =>
        op2str(name, "GlobalAvgPool", "", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Identity {name, t_inp, t_out} =>
        op2str(name, "Identity", "", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_If {name, then_branch, else_branch, t_inp, t_out} =>
        val then_branch_str = graph2str(net, then_branch, sub_indent)
        val else_branch_str = graph2str(net, else_branch, sub_indent)
        op2str(name, "If", f"then={then_branch_str}, else={else_branch_str}",
            t2str(net, [("t_inp", t_inp)] + targs2pairs("t_out", t_out)), indent)
    | DL_LeakyRelu {name, alpha, t_inp, t_out} =>
        op2str(name, "LeakyRelu", f"alpha={alpha}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Loop {name, body, t_trip_count, t_cond_in, t_v_in, t_cond_out, t_v_out} =>
        val body_str = graph2str(net, body, sub_indent)
        op2str(name, "Loop", f"body={body_str}",
            t2str(net, [("t_trip_count", t_trip_count), ("t_cond_in", t_cond_in)] +
                targs2pairs("t_v_in", t_v_in) + [("t_cond_out", t_cond_out)] +
                targs2pairs("t_v_out", t_v_out)), indent)
    | DL_LRN {name, size, alpha, beta, bias, t_inp, t_out} =>
        op2str(name, "LRN", f"size={size},\nalpha={alpha},\nbeta={beta},\nbias={bias}",
                t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_MaxPool { name, ceil_mode, dilations, kernel_shape, pads,
        strides, storage_order, t_inp, t_out } =>
        op2str(name, "MaxPool", f"ceil_mode={ceil_mode}, dilations={dilations}, kernel_shape={kernel_shape}, \
            pads={pads}, strides={strides}, storage_order={storage_order}",
            t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_NonMaxSuppression {
        name, center_point_box, t_boxes, t_scores,
        t_max_output_boxes_per_class, t_iou_threshold,
        t_score_threshold, t_out } =>
        op2str(name, "NonMaxSuppression", f"center_point_box={center_point_box}",
            t2str(net, [("t_boxes", t_boxes), ("t_scores", t_scores), ("t_max_output_boxes_per_class", t_max_output_boxes_per_class),
            ("t_iou_threshold", t_iou_threshold), ("t_score_threshold", t_score_threshold), ("t_out", t_out)]), indent)
    | DL_NonZero { name, t_inp, t_out } =>
        op2str(name, "NonZero", "", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Range {name, t_start, t_limit, t_delta, t_out} =>
        op2str(name, "Range", "", t2str(net, [("t_start", t_start), ("t_limit", t_limit),
            ("t_delta", t_delta), ("t_out", t_out)]), indent)
    | DL_Reduce {name, reduce_op, axes, keepdims, t_inp, t_out} =>
        op2str(name, string(reduce_op), f"axes={axes}, keepdims={keepdims}",
            t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Resize { name, coord_trans, cubic_coeff_a, exclude_outside, extrapolation_value,
        mode, nearest_mode, t_inp, t_scales, t_sizes, t_roi, t_out } =>
        val nearest_mode_str = if mode == DL_Inter_Nearest {f", nearest_mode={nearest_mode}"} else {""}
        val tensors = [("t_out", t_out)]
        val tensors = if coord_trans == DL_CT_TFCropResize {("t_roi", t_roi) :: tensors} else {tensors}
        val tensors = if t_scales != 0 {("t_scales", t_scales) :: tensors} else {("t_sizes", t_sizes) :: tensors}
        op2str(name, "Resize", f"coord_trans={coord_trans}, cubic_coeff_a={cubic_coeff_a},\
            exclude_outside={exclude_outside}, extrapolation_value={extrapolation_value},\
            mode={mode}{nearest_mode_str}",
            t2str(net, ("t_inp", t_inp) :: tensors), indent)
    | DL_Reshape {name, allowzero, t_inp, t_shape, t_out} =>
        op2str(name, "Reshape", f"allowzero={allowzero}",
            t2str(net, [("t_inp", t_inp), ("t_shape", t_shape), ("t_out", t_out)]), indent)
    | DL_RoiAlign {name, coord_trans, mode, output_height, output_width,
        sampling_ratio, spatial_scale, t_inp, t_rois, t_batch_ind, t_out} =>
        op2str(name, "RoiAlign", f"coord_trans={coord_trans}, pooling_mode={mode},\
            output_height={output_height}, output_width={output_width},\
            sampling_ratio={sampling_ratio}, sampling_ratio={sampling_ratio}",
            t2str(net, [("t_inp", t_inp), ("t_rois", t_rois), ("t_batch_ind", t_batch_ind), ("t_out", t_out)]), indent)
    | DL_Scatter {name, axis, t_data, t_updates, t_indices, t_out} =>
        op2str(name, "Scatter", f"axis={axis}",
            t2str(net, [("t_data", t_data), ("t_updates", t_updates), ("t_indices", t_indices), ("t_out", t_out)]), indent)
    | DL_Shape {name, start, end, t_inp, t_out} =>
        op2str(name, "Shape", f"start={start}, end={end}",
            t2str(net, [("t_data", t_inp), ("t_shape", t_out)]), indent)
    | DL_Slice {name, t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
        op2str(name, "Slice", "", t2str(net, [("t_inp", t_inp), ("t_starts", t_starts),
            ("t_ends", t_ends), ("t_axes", t_axes), ("t_steps", t_steps), ("t_out", t_out)]), indent)
    | DL_SoftMax {name, axis, t_inp, t_out } =>
        op2str(name, "SoftMax", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Split {name, axis, t_inp, t_split, t_out} =>
        op2str(name, "Split", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_split", t_split)] + targs2pairs("t_out", t_out)), indent)
    | DL_Squeeze {name, t_inp, t_axes, t_out} =>
        op2str(name, "Squeeze", "", t2str(net, [("t_inp", t_inp), ("t_axes", t_axes), ("t_out", t_out)]), indent)
    | DL_Tile {name, t_inp, t_repeats, t_out} =>
        op2str(name, "Tile", "", t2str(net, [("t_inp", t_inp), ("t_repeats", t_repeats), ("t_out", t_out)]), indent)
    | DL_TopK {name, axis, largest, sorted, t_inp, t_K, t_out, t_out_ind} =>
        op2str(name, "TopK", f"axis={axis}, largest={largest}, sorted={sorted}",
            t2str(net, [("t_inp", t_inp), ("t_K", t_K), ("t_out", t_out), ("t_out_ind", t_out_ind)]), indent)
    | DL_Transpose {name, perm, t_inp, t_out} =>
        op2str(name, "Tranpose", f"perm={perm}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Unsqueeze {name, t_inp, t_axes, t_out} =>
        op2str(name, "Unsqueeze", "", t2str(net, [("t_inp", t_inp), ("t_axes", t_axes), ("t_out", t_out)]), indent)
    }
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
    println(graph2str(net, net.graph, ""))
}
