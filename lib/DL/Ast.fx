/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Deep Models description

import Hashmap

exception DLError: string

type dltyp_t =
    | DL_Undefined | DL_I8 | DL_U8 | DL_I16 | DL_U16 | DL_I32 | DL_U32
    | DL_I64 | DL_U64 | DL_FP16 | DL_BF16 | DL_FP32 | DL_FP64 | DL_Bool

// please, keep the set and the order of tags here equivalent to dltyp_t,
// because some C code may assume dltyp_t::tag == dldata_t::tag.
class dldata_t =
    | DL_Data_Empty
    | DL_Data_I8: int8 []
    | DL_Data_U8: uint8 []
    | DL_Data_I16: int16 []
    | DL_Data_U16: uint16 []
    | DL_Data_I32: int32 []
    | DL_Data_U32: uint32 []
    | DL_Data_I64: int64 []
    | DL_Data_U64: uint64 []
    | DL_Data_Stub_FP16
    | DL_Data_Stub_BF16
    | DL_Data_FP32: float []
    | DL_Data_FP64: double []
    | DL_Data_Bool: bool []

type dlargkind_t =
    | DL_Arg_Const
    | DL_Arg_Input
    | DL_Arg_Output
    | DL_Arg_Temp

type dllayout_t =
    | DL_Layout_Unknown
    | DL_Layout_NC
    | DL_Layout_NCHW
    | DL_Layout_NHWC
    | DL_Layout_NCHWxc

type dlpadding_t =
    | DL_Pad_None
    | DL_Pad_SameUpper
    | DL_Pad_SameLower
    | DL_Pad_Valid

type dlpooling_t =
    | DL_Pool_Avg
    | DL_Pool_Max

type dlbuf_t = uint8 []

class dlshape_t
{
    layout: dllayout_t
    shape: int []
}

class dltensor_t
{
    shape: dlshape_t
    data: dldata_t
}

class dlarg_t
{
    name: string
    argkind: dlargkind_t
    shape: dlshape_t
    typ: dltyp_t
}

type dlelwise_t =
    | DL_Abs | DL_Acos | DL_Acosh | DL_Asin | DL_Asinh | DL_Atan | DL_Atanh
    | DL_Ceil | DL_Cos | DL_Cosh | DL_Erf | DL_Exp | DL_Floor | DL_IsInf | DL_IsNaN | DL_Log
    | DL_Neg | DL_Not | DL_Relu | DL_Round | DL_Sigmoid | DL_Sign | DL_Sin | DL_Sinh
    | DL_Softplus | DL_Softsign | DL_Sqrt | DL_Tan | DL_Tanh
    | DL_Add | DL_And | DL_Div | DL_Equal | DL_Greater | DL_Less
    | DL_Mod | DL_Mul | DL_Pow | DL_Or | DL_Sub | DL_Xor
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

class dlop_t =
    | DL_AvgPool: {
        name: string
        ceil_mode: bool
        dilations: int []
        kernel_shape: int []
        pads: int []
        strides: int []
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
        name: string; el_op: dlelwise_t; t_inp: int []; t_out: int }
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
        t_A: int; t_B: int; t_bias: int; t_out: int }
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

class dlgraph_t =
DL_Graph: {
    inpargs: int []
    outargs: int []
    prog: dlop_t []
}

class dlnet_t
{
    info: dlnet_info_t
    argnames: dlnames_t
    dimnames: dlnames_t
    dimnames_: string []
    args: dlarg_t []
    tensors: dltensor_t []
    bufidxs: int []
    buffers: dlbuf_t []
    graph: dlgraph_t
    preferred_layout: dllayout_t
}

type op_callback_t = (dlnet_t, dlop_t) -> void

fun empty_net() = dlnet_t {
    info = DL_Net_Generic,
    argnames = Hashmap.empty(8, "", 0),
    dimnames = Hashmap.empty(8, "", 0),
    dimnames_ = [],
    args = [],
    tensors = [],
    bufidxs = [],
    buffers = [],
    graph = DL_Graph {inpargs = [], outargs = [], prog=[]},
    preferred_layout = DL_Layout_NCHW
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
    | DL_Layout_NCHWxc => "NCHWxc"
}

fun string(p: dlargkind_t) {
    | DL_Arg_Const => "const"
    | DL_Arg_Input => "inp"
    | DL_Arg_Output => "out"
    | DL_Arg_Temp => "temp"
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
    | DL_Pow  => "Pow"
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

fun dldata_t.total() =
match self {
    | DL_Data_Empty
    | DL_Data_Stub_FP16 | DL_Data_Stub_BF16 => 0
    | DL_Data_I8(elems) => size(elems)
    | DL_Data_U8(elems) => size(elems)
    | DL_Data_I16(elems) => size(elems)
    | DL_Data_U16(elems) => size(elems)
    | DL_Data_I32(elems) => size(elems)
    | DL_Data_U32(elems) => size(elems)
    | DL_Data_I64(elems) => size(elems)
    | DL_Data_U64(elems) => size(elems)
    | DL_Data_FP32(elems) => size(elems)
    | DL_Data_FP64(elems) => size(elems)
    | DL_Data_Bool(elems) => size(elems)
}

fun dltensor_t.total() =
match self.data {
    | DL_Data_Empty
    | DL_Data_Stub_FP16 | DL_Data_Stub_BF16 => 0
    | DL_Data_I8(elems) => size(elems)
    | DL_Data_U8(elems) => size(elems)
    | DL_Data_I16(elems) => size(elems)
    | DL_Data_U16(elems) => size(elems)
    | DL_Data_I32(elems) => size(elems)
    | DL_Data_U32(elems) => size(elems)
    | DL_Data_I64(elems) => size(elems)
    | DL_Data_U64(elems) => size(elems)
    | DL_Data_FP32(elems) => size(elems)
    | DL_Data_FP64(elems) => size(elems)
    | DL_Data_Bool(elems) => size(elems)
}

fun float(d: dldata_t)
{
    | DL_Data_Empty
    | DL_Data_Stub_FP16 | DL_Data_Stub_BF16 => ([] : float [])
    | DL_Data_I8(elems) => float(elems)
    | DL_Data_U8(elems) => float(elems)
    | DL_Data_I16(elems) => float(elems)
    | DL_Data_U16(elems) => float(elems)
    | DL_Data_I32(elems) => float(elems)
    | DL_Data_U32(elems) => float(elems)
    | DL_Data_I64(elems) => float(elems)
    | DL_Data_U64(elems) => float(elems)
    | DL_Data_FP32(elems) => elems
    | DL_Data_FP64(elems) => float(elems)
    | DL_Data_Bool(elems) => float(elems)
}

fun double(d: dldata_t)
{
    | DL_Data_Empty
    | DL_Data_Stub_FP16 | DL_Data_Stub_BF16 => ([] : double [])
    | DL_Data_I8(elems) => double(elems)
    | DL_Data_U8(elems) => double(elems)
    | DL_Data_I16(elems) => double(elems)
    | DL_Data_U16(elems) => double(elems)
    | DL_Data_I32(elems) => double(elems)
    | DL_Data_U32(elems) => double(elems)
    | DL_Data_I64(elems) => double(elems)
    | DL_Data_U64(elems) => double(elems)
    | DL_Data_FP32(elems) => double(elems)
    | DL_Data_FP64(elems) => elems
    | DL_Data_Bool(elems) => double(elems)
}

fun int(d: dldata_t)
{
    | DL_Data_Empty
    | DL_Data_Stub_FP16 | DL_Data_Stub_BF16 => ([] : int [])
    | DL_Data_I8(elems) => int(elems)
    | DL_Data_U8(elems) => int(elems)
    | DL_Data_I16(elems) => int(elems)
    | DL_Data_U16(elems) => int(elems)
    | DL_Data_I32(elems) => int(elems)
    | DL_Data_U32(elems) => int(elems)
    | DL_Data_I64(elems) => int(elems)
    | DL_Data_U64(elems) => int(elems)
    | DL_Data_FP32(elems) => int(elems)
    | DL_Data_FP64(elems) => int(elems)
    | DL_Data_Bool(elems) => int(elems)
}

fun float(t: dltensor_t) = float(t.data)
fun double(t: dltensor_t) = double(t.data)
fun int(t: dltensor_t) = int(t.data)

fun arr2str(elems: 't []) = join_embrace("[", "]", ",", elems.map(repr))

fun tdata2str(d: dldata_t)
{
    | DL_Data_Empty
    | DL_Data_Stub_FP16 | DL_Data_Stub_BF16 => "[]"
    | DL_Data_I8(elems) => arr2str(elems)
    | DL_Data_U8(elems) => arr2str(elems)
    | DL_Data_I16(elems) => arr2str(elems)
    | DL_Data_U16(elems) => arr2str(elems)
    | DL_Data_I32(elems) => arr2str(elems)
    | DL_Data_U32(elems) => arr2str(elems)
    | DL_Data_I64(elems) => arr2str(elems)
    | DL_Data_U64(elems) => arr2str(elems)
    | DL_Data_FP32(elems) => arr2str(elems)
    | DL_Data_FP64(elems) => arr2str(elems)
    | DL_Data_Bool(elems) => arr2str(elems)
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
    | DL_Bool => "Bool"
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

fun dldata_t.elemtype() =
match self {
    | DL_Data_Empty => DL_Undefined
    | DL_Data_I8 _ => DL_I8
    | DL_Data_U8 _ => DL_U8
    | DL_Data_I16 _ => DL_I16
    | DL_Data_U16 _ => DL_U16
    | DL_Data_I32 _ => DL_I32
    | DL_Data_U32 _ => DL_U32
    | DL_Data_I64 _ => DL_I64
    | DL_Data_U64 _ => DL_U64
    | DL_Data_Stub_FP16 => DL_FP16
    | DL_Data_Stub_BF16 => DL_BF16
    | DL_Data_FP32 _ => DL_FP32
    | DL_Data_FP64 _ => DL_FP64
    | DL_Data_Bool _ => DL_Bool
}

fun dltensor_t.elemtype() =
match self.data {
    | DL_Data_Empty => DL_Undefined
    | DL_Data_I8 _ => DL_I8
    | DL_Data_U8 _ => DL_U8
    | DL_Data_I16 _ => DL_I16
    | DL_Data_U16 _ => DL_U16
    | DL_Data_I32 _ => DL_I32
    | DL_Data_U32 _ => DL_U32
    | DL_Data_I64 _ => DL_I64
    | DL_Data_U64 _ => DL_U64
    | DL_Data_Stub_FP16 => DL_FP16
    | DL_Data_Stub_BF16 => DL_BF16
    | DL_Data_FP32 _ => DL_FP32
    | DL_Data_FP64 _ => DL_FP64
    | DL_Data_Bool _ => DL_Bool
}

fun tensor2str(net: dlnet_t, t: dltensor_t, show_small: bool) =
match t.data {
    | DL_Data_Empty => "[]"
    | _ =>
        val sp = shape2str(net, t.shape)
        val tprefix = string(t.elemtype())
        val nelems = t.total()
        val tdata_str = if nelems <= 10 && show_small {tdata2str(t.data)} else {"[...]"}
        sp + " " + tprefix + " " + tdata_str
}

fun arg2str(net: dlnet_t, argidx: int)
{
    val targ = net.args[argidx]
    val sp = shape2str(net, targ.shape)
    val cprefix = match targ.argkind { DL_Arg_Temp => "" | _ => string(targ.argkind) + " " }
    val (tdatastr, bufstr) = match targ {
        | {argkind=DL_Arg_Const, shape={shape}}
            when argidx > 0 && size(shape) == 1 && shape[0] < 10 =>
            (": " + tdata2str(net.tensors[argidx].data), "")
        | {argkind=DL_Arg_Temp} => ("", f" (buf #{net.bufidxs[argidx]})")
        | _ => ("", "")
    }
    cprefix + sp + " " + string(targ.typ) + tdatastr + bufstr
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

fun dlop_t.name() = match self
{
    | DL_AvgPool {name} => (name, "AvgPool")
    | DL_BatchNorm {name} => (name, "BatchNorm")
    | DL_Cast {name} => (name, "Cast")
    | DL_Clip {name} => (name, "Clip")
    | DL_Concat {name} => (name, "Concat")
    | DL_ConstantOfShape {name} => (name, "ConstantOfShape")
    | DL_Conv {name} => (name, "Conv")
    | DL_ConvTranspose {name} => (name, "ConvTranspose")
    | DL_Dropout {name} => (name, "Dropout")
    | DL_Elemwise {name, el_op} => (name, f"Elemwise{el_op}")
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

fun t2str(net: dlnet_t, tensors: (string, int) list) =
    [for (name, tidx) <- tensors {
        val targ = net.args[tidx]
        f"{name}=\"{targ.name}\", // {arg2str(net, tidx)}"
    }]

fun dlop_t.get_inputs_outputs(): (int [], int []) = match self
{
    | DL_AvgPool {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_BatchNorm {t_inp, t_scale, t_B, t_mean, t_var, t_out} => ([|t_inp, t_scale, t_B, t_mean, t_var|], [|t_out|])
    | DL_Cast {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Clip {t_inp, t_min, t_max, t_out} => ([|t_inp, t_min, t_max|], [|t_out|])
    | DL_Concat {t_inp, t_out} => (t_inp, [|t_out|])
    | DL_ConstantOfShape {t_shape, t_out} => ([|t_shape|], [|t_out|])
    | DL_Conv {t_inp, t_weights, t_bias, t_out} => ([|t_inp, t_weights, t_bias|], [|t_out|])
    | DL_ConvTranspose {t_inp, t_weights, t_bias, t_out} => ([|t_inp, t_weights, t_bias|], [|t_out|])
    | DL_Dropout {t_inp, t_ratio, t_training_mode, t_out} => ([|t_inp, t_ratio, t_training_mode|], [|t_out|])
    | DL_Elemwise {t_inp, t_out} => (t_inp, [|t_out|])
    | DL_Expand {t_inp, t_shape, t_out} => ([|t_inp, t_shape|], [|t_out|])
    | DL_Flatten {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Gather {t_inp, t_ind, t_out} => ([|t_inp, t_ind|], [|t_out|])
    | DL_Gemm {t_A, t_B, t_bias, t_out} => ([|t_A, t_B, t_bias|], [|t_out|])
    | DL_GlobalAvgPool {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Identity {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_If {t_inp, t_out} => ([|t_inp|], t_out)
    | DL_LeakyRelu {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Loop {t_trip_count, t_cond_in, t_v_in, t_cond_out, t_v_out} =>
        ([|t_trip_count, t_cond_in, \t_v_in|], [|t_cond_out, \t_v_out|])
    | DL_LRN {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_MaxPool {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_NonMaxSuppression {t_boxes, t_scores, t_max_output_boxes_per_class,
        t_iou_threshold, t_score_threshold, t_out} =>
            ([|t_boxes, t_scores, t_max_output_boxes_per_class, t_iou_threshold, t_score_threshold|], [|t_out|])
    | DL_NonZero {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Range {t_start, t_limit, t_delta, t_out} => ([|t_start, t_limit, t_delta|], [|t_out|])
    | DL_Reduce {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Reshape {t_inp, t_shape, t_out} => ([|t_inp|], [|t_out|])
    | DL_Resize {t_inp, t_scales, t_sizes, t_roi, t_out} => ([|t_inp, t_scales, t_sizes, t_roi|], [|t_out|])
    | DL_RoiAlign {t_inp, t_rois, t_batch_ind, t_out} => ([|t_inp, t_rois, t_batch_ind|], [|t_out|])
    | DL_Scatter {t_data, t_updates, t_indices, t_out} => ([|t_data, t_updates, t_indices|], [|t_out|])
    | DL_Shape {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} => ([|t_inp, t_starts, t_ends, t_axes, t_steps|], [|t_out|])
    | DL_SoftMax {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Split {t_inp, t_split, t_out} => ([|t_inp, t_split|], t_out)
    | DL_Squeeze {t_inp, t_axes, t_out} => ([|t_inp, t_axes|], [|t_out|])
    | DL_Tile {t_inp, t_repeats, t_out} => ([|t_inp, t_repeats|], [|t_out|])
    | DL_TopK {t_inp, t_K, t_out, t_out_ind} =>  ([|t_inp, t_K|], [|t_out, t_out_ind|])
    | DL_Transpose {t_inp, t_out} => ([|t_inp|], [|t_out|])
    | DL_Unsqueeze {t_inp, t_axes, t_out} => ([|t_inp, t_axes|], [|t_out|])
}

fun op2str(net: dlnet_t, op: dlop_t, indent: string)
{
    val sub_indent = indent + "  "
    //println(f"dumping op={get_opname(op)}")
    match op {
    | DL_AvgPool {name, ceil_mode, dilations, kernel_shape, pads,
        strides, count_include_pad, t_inp, t_out} =>
        op2str(name, "AvgPool", f"ceil_mode={ceil_mode},\ndilations={dilations},\nkernel_shape={kernel_shape},\n\
            pads={pads},\nstrides={strides},\ncount_include_pad={count_include_pad}",
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
    | DL_Elemwise {name, el_op, t_inp, t_out} =>
        op2str(name, string(el_op), "", t2str(net, targs2pairs("t_inp", t_inp) + [("t_out", t_out)]), indent)
    | DL_Expand {name, t_inp, t_shape, t_out} =>
        op2str(name, "Expand", "", t2str(net, [("t_inp", t_inp), ("t_shape", t_shape), ("t_out", t_out)]), indent)
    | DL_Flatten {name, axis, t_inp, t_out} =>
        op2str(name, "Flatten", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | DL_Gather {name, axis, t_inp, t_ind, t_out} =>
        op2str(name, "Gather", f"axis={axis}", t2str(net, [("t_inp", t_inp), ("t_ind", t_ind), ("t_out", t_out)]), indent)
    | DL_Gemm {name, alpha, beta, transA, transB, t_A, t_B, t_bias, t_out} =>
        op2str(name, "Gemm", f"alpha={alpha},\nbeta={beta},\ntransA={transA},\ntransB={transB}",
        t2str(net, [("t_A", t_A), ("t_B", t_B), ("t_bias", t_bias), ("t_out", t_out)]), indent)
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
    | DL_MaxPool {name, ceil_mode, dilations, kernel_shape, pads,
        strides, storage_order, t_inp, t_out} =>
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

fun elemsize(t: dltyp_t)
{
    | DL_Undefined => -1
    | DL_I8 | DL_U8 | DL_Bool => 1
    | DL_I16 | DL_U16 | DL_FP16 | DL_BF16 => 2
    | DL_I32 | DL_U32 | DL_FP32 => 4
    | DL_I64 | DL_U64 | DL_FP64 => 8
}

fun dldata_t.copy() =
match self {
    | DL_Data_Empty | DL_Data_Stub_FP16 | DL_Data_Stub_BF16 => DL_Data_Empty
    | DL_Data_I8(arr) => DL_Data_I8(copy(arr))
    | DL_Data_U8(arr) => DL_Data_U8(copy(arr))
    | DL_Data_I16(arr) => DL_Data_I16(copy(arr))
    | DL_Data_U16(arr) => DL_Data_U16(copy(arr))
    | DL_Data_I32(arr) => DL_Data_I32(copy(arr))
    | DL_Data_U32(arr) => DL_Data_U32(copy(arr))
    | DL_Data_I64(arr) => DL_Data_I64(copy(arr))
    | DL_Data_U64(arr) => DL_Data_U64(copy(arr))
    | DL_Data_FP32(arr) => DL_Data_FP32(copy(arr))
    | DL_Data_FP64(arr) => DL_Data_FP64(copy(arr))
    | DL_Data_Bool(arr) => DL_Data_Bool(copy(arr))
}

fun empty_shape() = dlshape_t {
    layout = DL_Layout_Unknown,
    shape = []
}

fun empty_tensor() = dltensor_t {
    shape = empty_shape(),
    data = DL_Data_Empty
}

fun empty_arg() = dlarg_t {
    name = "",
    argkind = DL_Arg_Temp,
    shape = empty_shape(),
    typ = DL_Undefined
}

fun dlshape_t.total() = fold p=1 for sz <- self.shape {p*sz}

fun dlshape_t.copy() = dlshape_t {
    layout = self.layout,
    shape = self.shape.copy()
}

fun dlshape_t.get_num_channels()
{
    val ndims = self.shape.size()
    match (self.layout, ndims) {
    | (_, 1) => self.shape[0]
    | (DL_Layout_NC, _) => self.shape[1]
    | (DL_Layout_NCHW, _) => self.shape[1]
    | (DL_Layout_NHWC, _) => self.shape[ndims-1]
    | (DL_Layout_NCHWxc, _) => self.shape[1]*self.shape[ndims-1]
    | _ => -1
    }
}

fun dlshape_t.get_spatial_channel_range()
{
    val ndims = self.shape.size()
    match self.layout {
    | DL_Layout_NCHW => (2, ndims)
    | DL_Layout_NHWC => (1, ndims-1)
    | DL_Layout_NCHWxc => (2, ndims-1)
    | _ => throw DLError(f"the shape layout {self.layout} is not supported in get_spatial_channel_range()")
    }
}

fun coerce_layouts(a: dllayout_t, b: dllayout_t) =
match (a, b) {
    | (DL_Layout_Unknown, _) => b
    | (_, DL_Layout_Unknown) => a
    | (DL_Layout_NC, _) => b
    | (_, DL_Layout_NC) => a
    | (_, _) =>
        if a != b {
            throw DLError(f"two layouts, {a} and {b}, cannot be used together")
        }
        a
}

// see https://github.com/onnx/onnx/blob/main/docs/Broadcasting.md
// for the description of multi-way broadcasting
fun dlshape_t.broadcast(another: dlshape_t)
{
    val layout = coerce_layouts(self.layout, another.layout)
    if self.shape == another.shape {
        self.{layout = layout}
    } else {
        val ndims0 = self.shape.size()
        val ndims1 = another.shape.size()
        val ndims = max(ndims0, ndims1)
        val d0 = ndims - ndims0
        val d1 = ndims - ndims1
        val sh = [| for i <- 0:ndims {
            val a = if i >= d0 {self.shape[i-d0]} else {1}
            val b = if i >= d1 {another.shape[i-d1]} else {1}
            if a == b {a} else if a == 1 {b} else if b == 1 {a}
            else {
                throw DLError("the two shapes are not compatible for the mutual broadcasting")
            }
        } |]
        dlshape_t {shape=sh, layout=layout}
    }
}

fun make_tensor(shape: dlshape_t, typ: dltyp_t)
{
    val total = shape.total()
    val data = match typ {
        | DL_I8 => DL_Data_I8(array(total, 0i8))
        | DL_U8 => DL_Data_U8(array(total, 0u8))
        | DL_I16 => DL_Data_I16(array(total, 0i16))
        | DL_U16 => DL_Data_U16(array(total, 0u16))
        | DL_I32 => DL_Data_I32(array(total, 0i32))
        | DL_U32 => DL_Data_U32(array(total, 0u32))
        | DL_I64 => DL_Data_I64(array(total, 0i64))
        | DL_U64 => DL_Data_U64(array(total, 0u64))
        | DL_FP32 => DL_Data_FP32(array(total, 0.f))
        | DL_FP64 => DL_Data_FP64(array(total, 0.))
        | DL_Bool => DL_Data_Bool(array(total, false))
        | _ => throw DLError(f"unsupported tensor type {typ}")
    }
    dltensor_t {shape=shape, data=data}
}

fun dltensor_t.copy() =
    dltensor_t { shape=self.shape.copy(), data=self.data.copy() }
fun dltensor_t.isscalar() = self.shape.total() == 1

fun dlarg_t.isconst() = self.argkind == DL_Arg_Const

fun dlarg_t.copy() = dlarg_t {
    name = self.name,
    argkind = self.argkind,
    shape = self.shape.copy(),
    typ = self.typ
}

fun dlnet_t.get_tensor(argidx: int) = self.tensors[argidx]

fun dlnet_t.isconst(argidx: int) = self.args[argidx].argkind == DL_Arg_Const
fun dlnet_t.istemp(argidx: int) = self.args[argidx].argkind == DL_Arg_Temp
fun dlnet_t.isscalar(argidx: int) = self.tensors[argidx].isscalar()
fun dlnet_t.get_input_names(): string [] =
    [| for i <- self.graph.inpargs {
        self.args[i].name
    } |]
fun dlnet_t.get_output_names(): string [] =
    [| for i <- self.graph.outargs {
        self.args[i].name
    } |]

fun fit(shape: dlshape_t, typ: dltyp_t, data: dldata_t, buf: dlbuf_t): (dldata_t, dlbuf_t)
{
    val bufpadding = 128
    val new_total = shape.total()
    val elemsz = elemsize(typ)
    val typ0 = data.elemtype()
    val total0 = data.total()
    val reallocate = typ != typ0 || total0 != new_total

    fun fit_(total: int, elemsz: int, typ: dltyp_t,
        bufpadding: int, buf: dlbuf_t): (dldata_t, dlbuf_t)
    @ccode {
        int_ total_ = total*elemsz + bufpadding;
        fx_arr_t* data_arr = &fx_result->t0.u.DL_Data_U8;

        // if buffer has enough space to fit the new data, we re-use it
        if (total_ > buf->dim[0].size*(int_)buf->dim[0].step) {
            int fx_status = fx_make_arr(1, &total_, 1, 0, 0, 0, &fx_result->t1);
            if (fx_status < 0) return fx_status;
        } else {
            // copy the header
            fx_copy_arr(buf, &fx_result->t1);
        }

        // copy the header: data and buf will share data pointer and refcounter,
        // but will have different element type and number of elements.
        fx_copy_arr(&fx_result->t1, data_arr);
        data_arr->dim[0].size = total;
        data_arr->dim[0].step = (size_t)elemsz;
        fx_result->t0.tag = typ->tag;
        return FX_OK;
    }

    if reallocate || buf.size() < new_total*elemsz + bufpadding {
        fit_(new_total, elemsz, typ, bufpadding, buf)
    } else {
        (data, buf)
    }
}

fun dlnet_t.copy_tensor_data(t_inp: int, t_out: int)
{
    fun copy_(inp: dldata_t, out: dldata_t): void
    @ccode {
        fx_arr_t* inp_arr, *out_arr;
        if (inp->tag != out->tag)
            return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
        if (inp->tag == 1)
            return FX_OK;
        inp_arr = &inp->u.DL_Data_U8;
        out_arr = &out->u.DL_Data_U8;
        if (inp_arr->dims != out_arr->dims || inp_arr->dims != 1 || inp_arr->dim[0].size != out_arr->dim[0].size)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (inp_arr->data != out_arr->data)
            memcpy(out_arr->data, inp_arr->data, inp_arr->dim[0].size*inp_arr->dim[0].step);
        return FX_OK;
    }

    val inp = self.get_tensor(t_inp)
    val out = self.get_tensor(t_out)
    copy_(inp.data, out.data)
}

fun normalize_axis(axis: int, ndims: int) {
    val axis = if axis < 0 {axis + ndims} else {axis}
    assert(`0 <= axis <= ndims`)
    axis
}
