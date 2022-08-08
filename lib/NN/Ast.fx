/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Deep Models description

import Hashmap

exception NNError: string

// please, keep the set and the order of tags here equivalent to scalar_t,
// because some C code may assume scalar_t::tag == nndata_t::tag.
class nndata_t =
    | NN_Data_Empty
    | NN_Data_Int: int []
    | NN_Data_I8: int8 []
    | NN_Data_U8: uint8 []
    | NN_Data_I16: int16 []
    | NN_Data_U16: uint16 []
    | NN_Data_I32: int32 []
    | NN_Data_U32: uint32 []
    | NN_Data_I64: int64 []
    | NN_Data_U64: uint64 []
    | NN_Data_FP16: half []
    | NN_Data_Stub_BF16
    | NN_Data_FP32: float []
    | NN_Data_FP64: double []
    | NN_Data_Bool: bool []

type nnargkind_t =
    | NN_Arg_Const
    | NN_Arg_Input
    | NN_Arg_Output
    | NN_Arg_Temp

type nnlayout_t =
    | NN_Layout_Unknown
    | NN_Layout_ND
    | NN_Layout_NCHW
    | NN_Layout_NHWC
    | NN_Layout_NCHWxc

type nnpadding_t =
    | NN_Pad_None
    | NN_Pad_SameUpper
    | NN_Pad_SameLower
    | NN_Pad_Valid

type nnpooling_t =
    | NN_Pool_Avg
    | NN_Pool_Max

type nnbuf_t = uint8 []

class nnshape_t
{
    layout: nnlayout_t
    shape: int []
}

class nntensor_t
{
    shape: nnshape_t
    data: nndata_t
}

class nnarg_t
{
    name: string
    argkind: nnargkind_t
    shape: nnshape_t
    typ: scalar_t
}

type nnelwise_t =
    | NN_Add | NN_And | NN_Div | NN_Equal | NN_Greater | NN_GreaterOrEqual
    | NN_Less | NN_LessOrEqual | NN_Mod | NN_Mul | NN_Pow | NN_Or | NN_Sub | NN_Xor
    | NN_Min | NN_Max | NN_Mean
    | NN_Abs | NN_Acos | NN_Acosh | NN_Asin | NN_Asinh | NN_Atan | NN_Atanh
    | NN_Ceil | NN_Cos | NN_Cosh | NN_Erf | NN_Exp | NN_Floor | NN_IsInf | NN_IsNaN
    | NN_Log | NN_Mish | NN_Neg | NN_Not | NN_Relu | NN_Round | NN_Sigmoid | NN_Sign
    | NN_Sin | NN_Sinh | NN_Softplus | NN_Softsign | NN_Sqrt | NN_Tan | NN_Tanh | NN_Elemwise_ZZ

type nnreduce_t =
    | NN_ReduceL1 | NN_ReduceL2
    | NN_ReduceLogSum | NN_ReduceLogSumExp
    | NN_ReduceMax | NN_ReduceMean| NN_ReduceMin
    | NN_ReduceProd | NN_ReduceSum | NN_ReduceSumSquare

type nnorder_t =
    | NN_RowMajor
    | NN_ColumnMajor

type nncoord_trans_t =
    | NN_CT_HalfPixel
    | NN_CT_PyTorchHalfPixel
    | NN_CT_AlignCorners
    | NN_CT_Asymmetric
    | NN_CT_TFCropResize
    | NN_CT_OutHalfPixel

type nninterpolation_t =
    | NN_Inter_Nearest
    | NN_Inter_Linear
    | NN_Inter_Cubic

type nnearest_mode_t =
    | NN_Nearest_RoundPreferFloor
    | NN_Nearest_RoundPreferCeil
    | NN_Nearest_Floor
    | NN_Nearest_Ceil

type nnconv_attr_t
{
    kernel_shape: int []
    pads: int []
    strides: int []
    dilations: int []
    group: int
}

class nnop_t =
    | NN_AvgPool: {
        name: string
        ceil_mode: bool
        dilations: int []
        kernel_shape: int []
        pads: int []
        strides: int []
        count_include_pad: bool
        t_inp: int; t_out: int }
    | NN_BatchNorm: {
        name: string
        epsilon: float
        momentum: float
        training_mode: bool
        t_inp: int; t_scale: int; t_B: int
        t_mean: int; t_var: int; t_out: int }
    | NN_Cast: {
        name: string; to: scalar_t; t_inp: int; t_out: int }
    | NN_Clip: {
        name: string; t_inp: int; t_min: int; t_max: int; t_out: int }
    | NN_Concat: {
        name: string; axis: int; t_inp: int []; t_out: int }
    | NN_ConstantOfShape: {
        name: string; value: nntensor_t; t_shape: int; t_out: int }
    | NN_Conv: {
        name: string
        attr: nnconv_attr_t
        conv_data: cptr ref
        fused_batch_norm: nnop_t?
        non_const_batch_norm: bool
        fused_activ: nnop_t?
        non_const_activ: bool
        t_inp: int; t_weights: int
        t_bias: int; t_out: int
        t_passby: int }
    | NN_ConvTranspose: {
        name: string
        attr: nnconv_attr_t
        out_shape: int []
        out_padding: int []
        t_inp: int; t_weights: int; t_bias: int; t_out: int }
    | NN_DequantizeLinear: {
        name: string
        axis: int
        t_inp: int
        t_scale: int
        t_zp: int
        t_out: int }
    | NN_Dropout: {
        name: string; seed: int = 0
        t_inp: int; t_ratio: int; t_training_mode: int; t_out: int }
    | NN_Elemwise: {
        name: string; el_op: nnelwise_t; t_inp: int []; t_out: int }
    | NN_Expand: {
        name: string; t_inp: int; t_shape: int; t_out: int }
    | NN_Flatten: {
        name: string; axis: int; t_inp: int; t_out: int }
    | NN_Gather: {
        name: string; axis: int; t_inp: int; t_ind: int; t_out: int }
    | NN_Gemm: {
        name: string
        alpha: float = 1.f
        beta: float = 1.f
        transA: bool = false
        transB: bool = false
        t_A: int; t_B: int; t_bias: int; t_out: int }
    | NN_GlobalAvgPool: {
        name: string; t_inp: int; t_out: int }
    | NN_Identity: {
        name: string; t_inp: int; t_out: int }
    | NN_If: {
        name: string; then_branch: nngraph_t; else_branch: nngraph_t; t_inp: int; t_out: int [] }
    | NN_LeakyRelu: {
        name: string; alpha: float; t_inp: int; t_out: int }
    | NN_Loop: {
        name: string; body: nngraph_t; t_trip_count: int;
        t_cond_in: int; t_v_in: int [];
        t_v_out: int [] }
    | NN_LRN: {
        name: string; size: int; alpha: float
        beta: float; bias: float
        t_inp: int; t_out: int }
    | NN_MaxPool: {
        name: string
        ceil_mode: bool
        dilations: int []
        kernel_shape: int []
        pads: int []
        strides: int []
        storage_order: nnorder_t = NN_RowMajor
        t_inp: int; t_out: int }
    | NN_NonMaxSuppression: {
        name: string
        center_point_box: bool;
        t_boxes: int;
        t_scores: int;
        t_max_output_boxes_per_class: int;
        t_iou_threshold: int;
        t_score_threshold: int;
        t_out: int }
    | NN_NonZero: {
        name: string; t_inp: int; t_out: int }
    | NN_QLinearConv: {
        name: string
        attr: nnconv_attr_t
        qconv_data: cptr ref
        t_inp: int; t_weights: int
        t_bias: int; t_out: int;
        t_inp_scale: int; t_inp_zp: int;
        t_w_scale: int; t_w_zp: int;
        t_out_scale: int; t_out_zp: int }
    | NN_QLinearAdd: {
        name: string
        t_A: int; t_B: int; t_out: int
        t_A_scale: int; t_A_zp: int
        t_B_scale: int; t_B_zp: int
        t_out_scale: int; t_out_zp: int }
    | NN_QLinearGlobalAvgPool: {
        name: string
        channels_last: bool
        t_inp: int; t_out: int
        t_inp_scale: int; t_inp_zp: int
        t_out_scale: int; t_out_zp: int }
    | NN_QLinearMatMul: {
        name: string
        t_A: int; t_B: int; t_out: int
        t_A_scale: int; t_A_zp: int
        t_B_scale: int; t_B_zp: int
        t_out_scale: int; t_out_zp: int }
    | NN_QuantizeLinear: {
        name: string
        axis: int
        t_inp: int
        t_scale: int
        t_zp: int
        t_out: int }
    | NN_Range: {
        name: string; t_start: int; t_limit: int; t_delta: int; t_out: int }
    | NN_Reduce: {
        name: string; reduce_op: nnreduce_t; axes: int []; keepdims: bool; t_inp: int; t_out: int }
    | NN_Reshape: {
        name: string; allowzero: bool=false; t_inp: int; t_shape: int; t_out: int }
    | NN_Resize: {
        name: string
        coord_trans: nncoord_trans_t
        cubic_coeff_a: float
        exclude_outside: bool
        extrapolation_value: float
        mode: nninterpolation_t
        nearest_mode: nnearest_mode_t
        t_inp: int; t_scales: int; t_sizes: int
        t_roi: int; t_out: int }
    | NN_RoiAlign: {
        name: string
        coord_trans: nncoord_trans_t;
        mode: nnpooling_t;
        output_height: int; output_width: int;
        sampling_ratio: int; spatial_scale: float;
        t_inp: int; t_rois: int; t_batch_ind: int; t_out: int }
    | NN_Scatter: {
        name: string; axis: int; t_data: int; t_updates: int; t_indices: int; t_out: int }
    | NN_Shape: {
        name: string; start: int; end: int; t_inp: int; t_out: int }
    | NN_Slice: {
        name: string; t_inp: int; t_starts: int; t_ends: int; t_axes: int; t_steps: int; t_out: int }
    | NN_SoftMax: {
        name: string; axis: int=-1; t_inp: int; t_out: int }
    | NN_Split: {
        name: string; axis: int; t_inp: int; t_split: int; t_out: int [] }
    | NN_Squeeze: {
        name: string; t_inp: int; t_axes: int; t_out: int }
    | NN_Tile: {
        name: string; t_inp: int; t_repeats: int; t_out: int }
    | NN_TopK: {
        name: string; axis: int; largest: bool; sorted: bool; t_inp: int; t_K: int; t_out: int; t_out_ind: int }
    | NN_Transpose: {
        name: string; perm: int []; t_inp: int; t_out: int }
    | NN_Unsqueeze: {
        name: string; t_inp: int; t_axes: int; t_out: int }
    | NN_Nop // shall always be the last operation in the list

val nn_operations = NN_Nop.__tag__
val nn_elemwise_operations = NN_Elemwise_ZZ.__tag__ - 1
val nn_total_operations = nn_operations + nn_elemwise_operations

type nnnames_t = (string, int) Hashmap.t

type nnonnx_t =
{
    ir_version: int64
    producer: string
    domain: string
    doc_string: string
    opsets: (int64, string) list
}

type dlnet_info_t =
    | NN_Net_Generic
    | NN_Net_Onnx : nnonnx_t

class nngraph_t =
NN_Graph: {
    name: string
    inpargs: int []
    outargs: int []
    prog: nnop_t []
}

class nnmodel_t
{
    info: dlnet_info_t
    argnames: nnnames_t
    dimnames: nnnames_t
    dimnames_: string []
    args: nnarg_t []
    tensors: nntensor_t []
    bufidxs: int []
    buffers: nnbuf_t []
    graph: nngraph_t
    preferred_layout: nnlayout_t
    ntasks: int ref
    use_fp16: bool ref
    trace: bool ref
    profile: bool ref
    detailed_profile: bool ref
    scratch_buf: nnbuf_t ref
    perf_profile_time: int64 []
    perf_profile_count: int []
}

type op_callback_t = (nnmodel_t, nnop_t) -> void

fun empty_net() = nnmodel_t {
    info = NN_Net_Generic,
    argnames = Hashmap.empty(8, "", 0),
    dimnames = Hashmap.empty(8, "", 0),
    dimnames_ = [],
    args = [],
    tensors = [],
    bufidxs = [],
    buffers = [],
    graph = NN_Graph {name = "main", inpargs = [], outargs = [], prog=[]},
    preferred_layout = NN_Layout_NCHW,
    ntasks = ref 4,
    use_fp16 = ref false,
    trace = ref false,
    profile = ref false,
    detailed_profile = ref false,
    scratch_buf = ref ([] : nnbuf_t),
    perf_profile_time = array(nn_total_operations, 0L),
    perf_profile_count = array(nn_total_operations, 0)
}

fun empty_graph() = NN_Graph {
    name = "",
    inpargs = [],
    outargs = [],
    prog = []
}

fun string(order: nnorder_t)
{
    | NN_RowMajor => "RowMajor"
    | NN_ColumnMajor => "ColumnMajor"
}

fun string(layout: nnlayout_t)
{
    | NN_Layout_Unknown => "Unknown"
    | NN_Layout_ND => "ND"
    | NN_Layout_NCHW => "NCHW"
    | NN_Layout_NHWC => "NHWC"
    | NN_Layout_NCHWxc => "NCHWxc"
}

fun string(p: nnargkind_t) {
    | NN_Arg_Const => "const"
    | NN_Arg_Input => "inp"
    | NN_Arg_Output => "out"
    | NN_Arg_Temp => "temp"
}

fun string(ew: nnelwise_t)
{
    | NN_Abs => "Abs"
    | NN_Acos => "Acos"
    | NN_Acosh => "Acosh"
    | NN_Asin => "Asin"
    | NN_Asinh => "Asinh"
    | NN_Atan => "Atan"
    | NN_Atanh => "Atanh"
    | NN_Ceil => "Ceil"
    | NN_Cos => "Cos"
    | NN_Cosh => "Cosh"
    | NN_Erf => "Erf"
    | NN_Exp => "Exp"
    | NN_Floor => "Floor"
    | NN_IsInf => "IsInf"
    | NN_IsNaN => "IsNaN"
    | NN_Log => "Log"
    | NN_Neg => "Neg"
    | NN_Not => "Not"
    | NN_Mish => "Mish"
    | NN_Relu => "Relu"
    | NN_Round => "Round"
    | NN_Sigmoid => "Sigmoid"
    | NN_Sign => "Sign"
    | NN_Sin => "Sin"
    | NN_Sinh => "Sinh"
    | NN_Softplus => "Softplus"
    | NN_Softsign => "Softsign"
    | NN_Sqrt => "Sqrt"
    | NN_Tan => "Tan"
    | NN_Tanh => "Tanh"

    | NN_Add => "Add"
    | NN_And => "And"
    | NN_Div => "Div"
    | NN_Equal => "Equal"
    | NN_Greater => "Greater"
    | NN_GreaterOrEqual => "GreaterOrEqual"
    | NN_Less => "Less"
    | NN_LessOrEqual => "LessOrEqual"
    | NN_Mod => "Mod"
    | NN_Mul => "Mul"
    | NN_Or => "Or"
    | NN_Pow  => "Pow"
    | NN_Sub => "Sub"
    | NN_Xor => "Xor"

    | NN_Min => "Min"
    | NN_Max => "Max"
    | NN_Mean => "Mean"

    | NN_Elemwise_ZZ => "???"
}

fun string(r: nnreduce_t)
{
    | NN_ReduceMin => "ReduceMin"
    | NN_ReduceMax => "ReduceMax"
    | NN_ReduceMean => "ReduceMean"
    | NN_ReduceL1 => "ReduceL1"
    | NN_ReduceL2 => "ReduceL2"
    | NN_ReduceLogSum => "ReduceLogSum"
    | NN_ReduceLogSumExp => "ReduceLogSumExp"
    | NN_ReduceProd => "ReduceProd"
    | NN_ReduceSum => "ReduceSum"
    | NN_ReduceSumSquare => "ReduceSumSquare"
}

fun string(p: nnpadding_t) {
    | NN_Pad_None => "NoPadding"
    | NN_Pad_SameUpper => "Pad_SameUpper"
    | NN_Pad_SameLower => "Pad_SameLower"
    | NN_Pad_Valid => "Pad_Valid"
}

fun string(interpolation: nninterpolation_t)
{
    | NN_Inter_Nearest => "Nearest"
    | NN_Inter_Linear => "Linear"
    | NN_Inter_Cubic => "Cubic"
}

fun string(coord_trans: nncoord_trans_t)
{
    | NN_CT_HalfPixel => "HalfPixel"
    | NN_CT_PyTorchHalfPixel => "PyTorchHalfPixel"
    | NN_CT_AlignCorners => "AlignCorners"
    | NN_CT_Asymmetric => "Asymmetric"
    | NN_CT_TFCropResize => "TFCropResize"
    | NN_CT_OutHalfPixel => "OutputHalfPixel"
}

fun string(nearest_round: nnearest_mode_t)
{
    | NN_Nearest_RoundPreferFloor => "Nearest_RoundPreferFloor"
    | NN_Nearest_RoundPreferCeil => "Nearest_RoundPreferCeil"
    | NN_Nearest_Ceil => "Nearest_Ceil"
    | NN_Nearest_Floor => "Nearest_Floor"
}

fun string(mode: nnpooling_t)
{
    | NN_Pool_Max => "MaxPooling"
    | NN_Pool_Avg => "AveragePooling"
}

fun nndata_t.total() =
match self {
    | NN_Data_Empty
    | NN_Data_Stub_BF16 => 0
    | NN_Data_Int(elems) => size(elems)
    | NN_Data_I8(elems) => size(elems)
    | NN_Data_U8(elems) => size(elems)
    | NN_Data_I16(elems) => size(elems)
    | NN_Data_U16(elems) => size(elems)
    | NN_Data_I32(elems) => size(elems)
    | NN_Data_U32(elems) => size(elems)
    | NN_Data_I64(elems) => size(elems)
    | NN_Data_U64(elems) => size(elems)
    | NN_Data_FP32(elems) => size(elems)
    | NN_Data_FP64(elems) => size(elems)
    | NN_Data_FP16(elems) => size(elems)
    | NN_Data_Bool(elems) => size(elems)
}

fun nntensor_t.total() =
match self.data {
    | NN_Data_Empty
    | NN_Data_Stub_BF16 => 0
    | NN_Data_Int(elems) => size(elems)
    | NN_Data_I8(elems) => size(elems)
    | NN_Data_U8(elems) => size(elems)
    | NN_Data_I16(elems) => size(elems)
    | NN_Data_U16(elems) => size(elems)
    | NN_Data_I32(elems) => size(elems)
    | NN_Data_U32(elems) => size(elems)
    | NN_Data_I64(elems) => size(elems)
    | NN_Data_U64(elems) => size(elems)
    | NN_Data_FP32(elems) => size(elems)
    | NN_Data_FP64(elems) => size(elems)
    | NN_Data_FP16(elems) => size(elems)
    | NN_Data_Bool(elems) => size(elems)
}

fun float(d: nndata_t)
{
    | NN_Data_Empty
    | NN_Data_Stub_BF16 => ([] : float [])
    | NN_Data_Int(elems) => float(elems)
    | NN_Data_I8(elems) => float(elems)
    | NN_Data_U8(elems) => float(elems)
    | NN_Data_I16(elems) => float(elems)
    | NN_Data_U16(elems) => float(elems)
    | NN_Data_I32(elems) => float(elems)
    | NN_Data_U32(elems) => float(elems)
    | NN_Data_I64(elems) => float(elems)
    | NN_Data_U64(elems) => float(elems)
    | NN_Data_FP32(elems) => elems
    | NN_Data_FP64(elems) => float(elems)
    | NN_Data_FP16(elems) => float(elems)
    | NN_Data_Bool(elems) => float(elems)
}

fun double_scalar_or(d: nndata_t, defval: double): double =
match d {
    | NN_Data_Empty => defval
    | NN_Data_Stub_BF16 _ => throw Fail("FP16 is not supported yet")
    | NN_Data_Int(elems) => double(elems[0])
    | NN_Data_I8(elems) => double(elems[0])
    | NN_Data_U8(elems) => double(elems[0])
    | NN_Data_I16(elems) => double(elems[0])
    | NN_Data_U16(elems) => double(elems[0])
    | NN_Data_I32(elems) => double(elems[0])
    | NN_Data_U32(elems) => double(elems[0])
    | NN_Data_I64(elems) => double(elems[0])
    | NN_Data_U64(elems) => double(elems[0])
    | NN_Data_FP16(elems) => double(elems[0])
    | NN_Data_FP32(elems) => double(elems[0])
    | NN_Data_FP64(elems) => elems[0]
    | NN_Data_Bool(elems) => double(elems[0])
}

fun float_scalar_or(d: nndata_t, defval: float): float =
match d {
    | NN_Data_Empty => defval
    | NN_Data_Stub_BF16 => throw Fail("FP16 is not supported yet")
    | NN_Data_Int(elems) => float(elems[0])
    | NN_Data_I8(elems) => float(elems[0])
    | NN_Data_U8(elems) => float(elems[0])
    | NN_Data_I16(elems) => float(elems[0])
    | NN_Data_U16(elems) => float(elems[0])
    | NN_Data_I32(elems) => float(elems[0])
    | NN_Data_U32(elems) => float(elems[0])
    | NN_Data_I64(elems) => float(elems[0])
    | NN_Data_U64(elems) => float(elems[0])
    | NN_Data_FP32(elems) => elems[0]
    | NN_Data_FP64(elems) => float(elems[0])
    | NN_Data_FP16(elems) => float(elems[0])
    | NN_Data_Bool(elems) => float(elems[0])
}

fun double(d: nndata_t)
{
    | NN_Data_Empty
    | NN_Data_Stub_BF16 => ([] : double [])
    | NN_Data_Int(elems) => double(elems)
    | NN_Data_I8(elems) => double(elems)
    | NN_Data_U8(elems) => double(elems)
    | NN_Data_I16(elems) => double(elems)
    | NN_Data_U16(elems) => double(elems)
    | NN_Data_I32(elems) => double(elems)
    | NN_Data_U32(elems) => double(elems)
    | NN_Data_I64(elems) => double(elems)
    | NN_Data_U64(elems) => double(elems)
    | NN_Data_FP32(elems) => double(elems)
    | NN_Data_FP64(elems) => elems
    | NN_Data_FP16(elems) => double(elems)
    | NN_Data_Bool(elems) => double(elems)
}

fun int(d: nndata_t)
{
    | NN_Data_Empty
    | NN_Data_Stub_BF16 => ([] : int [])
    | NN_Data_Int(elems) => elems
    | NN_Data_I8(elems) => int(elems)
    | NN_Data_U8(elems) => int(elems)
    | NN_Data_I16(elems) => int(elems)
    | NN_Data_U16(elems) => int(elems)
    | NN_Data_I32(elems) => int(elems)
    | NN_Data_U32(elems) => int(elems)
    | NN_Data_I64(elems) => int(elems)
    | NN_Data_U64(elems) => int(elems)
    | NN_Data_FP32(elems) => int(elems)
    | NN_Data_FP64(elems) => int(elems)
    | NN_Data_FP16(elems) => int(elems)
    | NN_Data_Bool(elems) => int(elems)
}

fun float(t: nntensor_t) = float(t.data)
fun double(t: nntensor_t) = double(t.data)
fun int(t: nntensor_t) = int(t.data)

fun arr2str(elems: 't []) = join_embrace("[", "]", ",", elems.map(repr))

fun tdata2str(d: nndata_t)
{
    | NN_Data_Empty
    | NN_Data_Stub_BF16 => "[]"
    | NN_Data_Int(elems) => arr2str(elems)
    | NN_Data_I8(elems) => arr2str(elems)
    | NN_Data_U8(elems) => arr2str(elems)
    | NN_Data_I16(elems) => arr2str(elems)
    | NN_Data_U16(elems) => arr2str(elems)
    | NN_Data_I32(elems) => arr2str(elems)
    | NN_Data_U32(elems) => arr2str(elems)
    | NN_Data_I64(elems) => arr2str(elems)
    | NN_Data_U64(elems) => arr2str(elems)
    | NN_Data_FP32(elems) => arr2str(elems)
    | NN_Data_FP64(elems) => arr2str(elems)
    | NN_Data_FP16(elems) => arr2str(elems)
    | NN_Data_Bool(elems) => arr2str(elems)
}

fun dim2str(model: nnmodel_t, d: int) = if d >= 0 {string(d)} else {model.dimnames_[-d-1]}

fun shape2str(model: nnmodel_t, s: nnshape_t)
{
    val shape_str = " x ".join([for d <- s.shape {dim2str(model, d)}])
    (match (s.layout, size(s.shape)) {
    | (NN_Layout_Unknown, _) => ""
    | (_, dims) when dims > 1 => f"{s.layout} "
    | _ => ""
    }) + f"<{shape_str}>"
}

fun nndata_t.elemtype(): scalar_t =
match self {
    | NN_Data_Empty => Notype
    | NN_Data_Int _ => Type_Int
    | NN_Data_I8 _ => Type_I8
    | NN_Data_U8 _ => Type_U8
    | NN_Data_I16 _ => Type_I16
    | NN_Data_U16 _ => Type_U16
    | NN_Data_I32 _ => Type_I32
    | NN_Data_U32 _ => Type_U32
    | NN_Data_I64 _ => Type_I64
    | NN_Data_U64 _ => Type_U64
    | NN_Data_Stub_BF16 => Type_BF16
    | NN_Data_FP32 _ => Type_F32
    | NN_Data_FP64 _ => Type_F64
    | NN_Data_FP16 _ => Type_F16
    | NN_Data_Bool _ => Type_Bool
}

fun elemtype(t: nntensor_t): scalar_t = t.data.elemtype()

fun tensor2str(model: nnmodel_t, t: nntensor_t, show_small: bool) =
match t.data {
    | NN_Data_Empty => "[]"
    | _ =>
        val sp = shape2str(model, t.shape)
        val tprefix = string(t.elemtype())
        val nelems = t.total()
        val tdata_str = if nelems <= 10 && show_small {tdata2str(t.data)} else {"[...]"}
        sp + " " + tprefix + " " + tdata_str
}

fun string(t: nntensor_t, ~border: int=3, ~braces: bool=true)
{
    fun row2str(data: 't [], n: int, ofs: int)
    {
        val ndump = min(n, border*2+1)
        val elems = [for i <- 0:ndump {
                val j = if n == ndump || i < border {i} else if i == border {-1}
                        else {n-border*2-1+i}
                if j >= 0 {string(data[ofs + j])} else {"... "}
            }]
        ", ".join(elems)
    }

    match t.data {
    | NN_Data_Empty => "no data"
    | _ when exists(for i <- t.shape.shape {i == 0}) => "no data"
    | _ =>
        val shape = t.shape.shape
        val ndims = shape.size()
        var rows: string list = []
        val step = array(ndims, 1)
        for i <- ndims-2:-1:-1 { step[i] *= step[i+1]*shape[i+1] }
        fun slice2str(d: int, ofs: int) {
            val n = if d >= ndims {1} else {shape[d]}
            if d >= ndims - 1 {
                match t.data {
                | NN_Data_Empty => {}
                | NN_Data_Int data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_I8 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_U8 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_I16 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_U16 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_I32 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_U32 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_I64 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_U64 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_FP32 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_FP64 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_FP16 data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_Bool data => rows = row2str(data, n, ofs) :: rows
                | NN_Data_Stub_BF16 =>
                    throw NotImplementedError
                }
            } else {
                val ndump = min(n, border*2+1)
                var dots = false
                for i <- 0:ndump {
                    if i > 0 && !dots {
                        val nempty_lines = ndims - 2 - d
                        for k <- 0:nempty_lines {rows = "" :: rows}
                    }
                    val j = if n == ndump || i < border {i}
                            else if i == border {-1}
                            else {n - border*2 - 1 + i}
                    dots = j < 0
                    if !dots {
                        slice2str(d+1, ofs + j*step[d])
                    } else {
                        rows = "..." :: rows
                    }
                }
            }
        }
        slice2str(0, 0)
        val (begin, end) = if ndims > 0 && braces {("[", "]")} else {("", "")}
        join_embrace(begin, end, "\n", rows.rev())
    }
}

fun arg2str(model: nnmodel_t, argidx: int)
{
    val targ = model.args[argidx]
    val sp = shape2str(model, targ.shape)
    val cprefix = match targ.argkind { NN_Arg_Temp => "" | _ => string(targ.argkind) + " " }
    val (tdatastr, bufstr) = match targ {
        | {argkind=NN_Arg_Const, shape={shape}}
            when argidx > 0 && size(shape) == 1 && shape[0] < 10 =>
            (": " + tdata2str(model.tensors[argidx].data), "")
        | {argkind=NN_Arg_Temp} => ("", f" (buf #{model.bufidxs[argidx]})")
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

fun op2str(name: string, opname: string, params: string, tensors: string [], indent0: string)
{
    val indent = indent0 + "   "
    val pl = parse_params(params)
    val pprefix = if pl == [] {""} else {f"\n{indent}"}
    join_embrace(f"{opname} {{\n{indent}name=\"{name}\",\n{indent}",
        join_embrace(pprefix, f"\n{indent0}}}", f",\n{indent}", pl),
        f"\n{indent}", tensors)
}

fun graph2str(model: nnmodel_t, graph: nngraph_t, indent: string)
{
    fun grapharg2str(model: nnmodel_t, idx: int, indent: string) {
        val targ = model.args[idx]
        f"{indent}\"{targ.name}\", // {arg2str(model, idx)}\n"
    }
    val {inpargs, outargs, prog} = graph
    val new_indent = indent + "  "
    val prog_indent = new_indent + "  "
    val inpstrs = "".join([for a <- inpargs {grapharg2str(model, a, prog_indent)}])
    val outstrs = "".join([for a <- outargs {grapharg2str(model, a, prog_indent)}])
    val prog = [for op@i <- prog {
        f"{indent}// op #{i}\n{prog_indent}" + op2str(model, op, prog_indent)}]
    join_embrace(f"graph {{\n{new_indent}inputs={{\n{inpstrs}{new_indent}}},\n\
        {new_indent}outputs={{\n{outstrs}{new_indent}}},\n{new_indent}prog={{\n{prog_indent}",
        f"\n{new_indent}}}\n{indent}}}",
        f",\n{prog_indent}", prog)
}

fun nnop_t.name(): (string, string) = match self
{
    | NN_AvgPool {name} => (name, "AvgPool")
    | NN_BatchNorm {name} => (name, "BatchNorm")
    | NN_Cast {name} => (name, "Cast")
    | NN_Clip {name} => (name, "Clip")
    | NN_Concat {name} => (name, "Concat")
    | NN_ConstantOfShape {name} => (name, "ConstantOfShape")
    | NN_Conv {name} => (name, "Conv")
    | NN_ConvTranspose {name} => (name, "ConvTranspose")
    | NN_DequantizeLinear {name} => (name, "DequantizeLinear")
    | NN_Dropout {name} => (name, "Dropout")
    | NN_Elemwise {name, el_op} => (name, string(el_op))
    | NN_Expand {name} => (name, "Expand")
    | NN_Flatten {name} => (name, "Flatten")
    | NN_Gather {name} => (name, "Gather")
    | NN_Gemm {name} => (name, "Gemm")
    | NN_GlobalAvgPool {name} => (name, "GlobalAvgPool")
    | NN_Identity {name} => (name, "Identity")
    | NN_If {name} => (name, "If")
    | NN_LeakyRelu {name} => (name, "LeakyRelu")
    | NN_Loop {name} => (name, "Loop")
    | NN_LRN {name} => (name, "LRN")
    | NN_MaxPool {name} => (name, "MaxPool")
    | NN_NonMaxSuppression {name} => (name, "NonMaxSuppression")
    | NN_NonZero {name} => (name, "NonZero")
    | NN_QLinearAdd {name} => (name, "QLinearAdd")
    | NN_QLinearGlobalAvgPool {name} => (name, "QLinearGlobalAvgPool")
    | NN_QLinearMatMul {name} => (name, "QLinearMatMul")
    | NN_QLinearConv {name} => (name, "QLinearConv")
    | NN_QuantizeLinear {name} => (name, "QuantizeLinear")
    | NN_Range {name} => (name, "Range")
    | NN_Reduce {name, reduce_op} => (name, string(reduce_op))
    | NN_Resize {name} => (name, "Resize")
    | NN_Reshape {name} => (name, "Reshape")
    | NN_RoiAlign {name} => (name, "RoiAlign")
    | NN_Scatter {name} => (name, "Scatter")
    | NN_Shape {name} => (name, "Shape")
    | NN_Slice {name} => (name, "Slice")
    | NN_SoftMax {name} => (name, "SoftMax")
    | NN_Split {name} => (name, "Split")
    | NN_Squeeze {name} => (name, "Squeeze")
    | NN_Tile {name} => (name, "Tile")
    | NN_TopK {name} => (name, "TopK")
    | NN_Transpose {name} => (name, "Transpose")
    | NN_Unsqueeze {name} => (name, "Unsqueeze")
    | NN_Nop => ("<nop>", "Nop")
}

fun nnop_t.perf_profile_index(): int = match self {
    | NN_Elemwise {el_op} => nn_operations + el_op.__tag__ - 1
    | _ => self.__tag__ - 1
}

fun targs2pairs(prefix: string, args: int []) =
    if args.size() == 1 {[(prefix, args[0])]}
    else {[for a@i <- args {(f"{prefix}{i}", a)}]}

fun t2str(model: nnmodel_t, tensors: (string, int) []) =
    [for (name, tidx) <- tensors {
        val targ = if tidx >= 0 {model.args[tidx]} else {empty_arg()}
        f"{name}=\"{targ.name}\", // {arg2str(model, tidx)}"
    }]

fun nnop_t.get_inputs_outputs(): (int [], int []) = match self
{
    | NN_Nop => ([], [])
    | NN_AvgPool {t_inp, t_out} => ([t_inp], [t_out])
    | NN_BatchNorm {t_inp, t_scale, t_B, t_mean, t_var, t_out} => ([t_inp, t_scale, t_B, t_mean, t_var], [t_out])
    | NN_Cast {t_inp, t_out} => ([t_inp], [t_out])
    | NN_Clip {t_inp, t_min, t_max, t_out} => ([t_inp, t_min, t_max], [t_out])
    | NN_Concat {t_inp, t_out} => (t_inp, [t_out])
    | NN_ConstantOfShape {t_shape, t_out} => ([t_shape], [t_out])
    | NN_Conv {t_inp, t_weights, t_bias, t_out, t_passby} => ([t_inp, t_weights, t_bias, t_passby], [t_out])
    | NN_ConvTranspose {t_inp, t_weights, t_bias, t_out} => ([t_inp, t_weights, t_bias], [t_out])
    | NN_DequantizeLinear {t_inp, t_scale, t_zp, t_out} => ([t_inp, t_scale, t_zp], [t_out])
    | NN_Dropout {t_inp, t_ratio, t_training_mode, t_out} => ([t_inp, t_ratio, t_training_mode], [t_out])
    | NN_Elemwise {t_inp, t_out} => (t_inp, [t_out])
    | NN_Expand {t_inp, t_shape, t_out} => ([t_inp, t_shape], [t_out])
    | NN_Flatten {t_inp, t_out} => ([t_inp], [t_out])
    | NN_Gather {t_inp, t_ind, t_out} => ([t_inp, t_ind], [t_out])
    | NN_Gemm {t_A, t_B, t_bias, t_out} => ([t_A, t_B, t_bias], [t_out])
    | NN_GlobalAvgPool {t_inp, t_out} => ([t_inp], [t_out])
    | NN_Identity {t_inp, t_out} => ([t_inp], [t_out])
    | NN_If {t_inp, t_out} => ([t_inp], t_out)
    | NN_LeakyRelu {t_inp, t_out} => ([t_inp], [t_out])
    | NN_Loop {t_trip_count, t_cond_in, t_v_in, t_v_out} =>
        ([t_trip_count, t_cond_in, \t_v_in], t_v_out)
    | NN_LRN {t_inp, t_out} => ([t_inp], [t_out])
    | NN_MaxPool {t_inp, t_out} => ([t_inp], [t_out])
    | NN_NonMaxSuppression {t_boxes, t_scores, t_max_output_boxes_per_class,
        t_iou_threshold, t_score_threshold, t_out} =>
            ([t_boxes, t_scores, t_max_output_boxes_per_class, t_iou_threshold, t_score_threshold], [t_out])
    | NN_NonZero {t_inp, t_out} => ([t_inp], [t_out])
    | NN_QLinearAdd {t_A, t_A_scale, t_A_zp, t_B, t_B_scale, t_B_zp,
                    t_out_scale, t_out_zp, t_out} =>
        ([t_A, t_A_scale, t_A_zp, t_B, t_B_scale, t_B_zp, t_out_scale, t_out_zp], [t_out])
    | NN_QLinearConv {t_inp, t_weights, t_bias, t_out, t_inp_scale, t_inp_zp,
                      t_w_scale, t_w_zp, t_out_scale, t_out_zp} =>
        ([t_inp, t_weights, t_bias, t_inp_scale, t_inp_zp,
          t_w_scale, t_w_zp, t_out_scale, t_out_zp], [t_out])
    | NN_QLinearGlobalAvgPool {t_inp, t_out, t_inp_scale, t_inp_zp, t_out_scale, t_out_zp} =>
        ([t_inp, t_inp_scale, t_inp_zp, t_out_scale, t_out_zp], [t_out])
    | NN_QLinearMatMul {t_A, t_A_scale, t_A_zp, t_B, t_B_scale, t_B_zp,
                    t_out_scale, t_out_zp, t_out} =>
        ([t_A, t_A_scale, t_A_zp, t_B, t_B_scale, t_B_zp, t_out_scale, t_out_zp], [t_out])
    | NN_QuantizeLinear {t_inp, t_scale, t_zp, t_out} => ([t_inp, t_scale, t_zp], [t_out])
    | NN_Range {t_start, t_limit, t_delta, t_out} => ([t_start, t_limit, t_delta], [t_out])
    | NN_Reduce {t_inp, t_out} => ([t_inp], [t_out])
    | NN_Reshape {t_inp, t_shape, t_out} => ([t_inp, t_shape], [t_out])
    | NN_Resize {t_inp, t_scales, t_sizes, t_roi, t_out} => ([t_inp, t_scales, t_sizes, t_roi], [t_out])
    | NN_RoiAlign {t_inp, t_rois, t_batch_ind, t_out} => ([t_inp, t_rois, t_batch_ind], [t_out])
    | NN_Scatter {t_data, t_updates, t_indices, t_out} => ([t_data, t_updates, t_indices], [t_out])
    | NN_Shape {t_inp, t_out} => ([t_inp], [t_out])
    | NN_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} => ([t_inp, t_starts, t_ends, t_axes, t_steps], [t_out])
    | NN_SoftMax {t_inp, t_out} => ([t_inp], [t_out])
    | NN_Split {t_inp, t_split, t_out} => ([t_inp, t_split], t_out)
    | NN_Squeeze {t_inp, t_axes, t_out} => ([t_inp, t_axes], [t_out])
    | NN_Tile {t_inp, t_repeats, t_out} => ([t_inp, t_repeats], [t_out])
    | NN_TopK {t_inp, t_K, t_out, t_out_ind} =>  ([t_inp, t_K], [t_out, t_out_ind])
    | NN_Transpose {t_inp, t_out} => ([t_inp], [t_out])
    | NN_Unsqueeze {t_inp, t_axes, t_out} => ([t_inp, t_axes], [t_out])
}

fun op2str(model: nnmodel_t, op: nnop_t, indent: string): string
{
    fun conv_attr2str(attr: nnconv_attr_t) =
        f"kernel_shape={attr.kernel_shape}, \
        pads={attr.pads}, strides={attr.strides}, \
        dilations={attr.dilations}, group={attr.group}"

    val sub_indent = indent + "  "
    //println(f"dumping op={op.name()}")
    match op {
    | NN_Nop => "Nop"
    | NN_AvgPool {name, ceil_mode, dilations, kernel_shape, pads,
        strides, count_include_pad, t_inp, t_out} =>
        op2str(name, "AvgPool", f"ceil_mode={ceil_mode},\ndilations={dilations},\nkernel_shape={kernel_shape},\n\
            pads={pads},\nstrides={strides},\ncount_include_pad={count_include_pad}",
            t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_BatchNorm {name, epsilon, momentum, training_mode, t_inp, t_scale, t_B, t_mean, t_var, t_out} =>
        op2str(name, "BatchNorm", f"epsilon={epsilon},\nmomentum={momentum},\ntraining_mode={training_mode}",
            t2str(model, [("t_inp", t_inp), ("t_scale", t_scale), ("t_B", t_B),
            ("t_mean", t_mean), ("t_var", t_var), ("t_out", t_out)]), indent)
    | NN_Cast {name, to, t_inp, t_out} =>
        op2str(name, "Cast", f"to={to}", t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_Clip {name, t_inp, t_min, t_max, t_out} =>
        op2str(name, "Clip", "", t2str(model, [("t_inp", t_inp), ("t_min", t_min), ("t_max", t_max), ("t_out", t_out)]), indent)
    | NN_Concat {name, axis, t_inp, t_out} =>
        op2str(name, "Concat", f"axis={axis}", t2str(model, [\targs2pairs("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_ConstantOfShape {name, value, t_shape, t_out} =>
        op2str(name, "ConstantOfShape", f"value={tensor2str(model, value, true)}",
            t2str(model, [("t_shape", t_shape), ("t_out", t_out)]), indent)
    | NN_Conv {name=convname, attr,
        fused_batch_norm, fused_activ, t_inp, t_weights, t_bias, t_out, t_passby} =>
        val bnorm_name = match fused_batch_norm {
            | Some(NN_BatchNorm _) => " + BatchNorm"
            | _ => ""}
        val activ_name = match fused_activ {
            | Some(activ) =>
                val (_, opname): (string, string) = activ.name()
                " + " + opname
            | _ => ""}
        val (passby_name, passby_attr) =
            if t_passby > 0 {
                (" + Add", f", passby=\"{model.args[t_passby].name}\"")
            } else {("", "")}
        op2str(convname, "Conv" + bnorm_name + passby_name + activ_name,
            conv_attr2str(attr) + f"{passby_attr}",
            t2str(model, [("t_inp", t_inp), ("t_weights", t_weights),
                ("t_bias", t_bias), ("t_out", t_out)]), indent)
    | NN_ConvTranspose {name, attr, out_shape, out_padding,
                        t_inp, t_weights, t_bias, t_out} =>
        op2str(name, "Conv", conv_attr2str(attr) + f"out_padding={out_padding}, out_shape={out_shape}",
            t2str(model, [("t_inp", t_inp), ("t_weights", t_weights), ("t_bias", t_bias), ("t_out", t_out)]), indent)
    | NN_DequantizeLinear {name, axis, t_inp, t_scale, t_zp, t_out} =>
        op2str(name, "DequantizeLinear", f"axis={axis}",
            t2str(model, [("t_inp", t_inp), ("t_scale", t_scale),
                   ("t_zp", t_zp), ("t_out", t_out)]), indent)
    | NN_Dropout {name, seed, t_inp, t_ratio, t_training_mode, t_out} =>
        op2str(name, "Dropout", f"seed={seed}", t2str(model,
            [("t_inp", t_inp), ("t_ratio", t_ratio), ("t_training_mode", t_training_mode), ("t_out", t_out)]), indent)
    | NN_Elemwise {name, el_op, t_inp, t_out} =>
        val targs = [\targs2pairs("t_inp", t_inp), ("t_out", t_out)]
        op2str(name, string(el_op), "", t2str(model, targs), indent)
    | NN_Expand {name, t_inp, t_shape, t_out} =>
        op2str(name, "Expand", "", t2str(model, [("t_inp", t_inp), ("t_shape", t_shape), ("t_out", t_out)]), indent)
    | NN_Flatten {name, axis, t_inp, t_out} =>
        op2str(name, "Flatten", f"axis={axis}", t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_Gather {name, axis, t_inp, t_ind, t_out} =>
        op2str(name, "Gather", f"axis={axis}", t2str(model, [("t_inp", t_inp), ("t_ind", t_ind), ("t_out", t_out)]), indent)
    | NN_Gemm {name, alpha, beta, transA, transB, t_A, t_B, t_bias, t_out} =>
        op2str(name, "Gemm", f"alpha={alpha},\nbeta={beta},\ntransA={transA},\ntransB={transB}",
        t2str(model, [("t_A", t_A), ("t_B", t_B), ("t_bias", t_bias), ("t_out", t_out)]), indent)
    | NN_GlobalAvgPool {name, t_inp, t_out} =>
        op2str(name, "GlobalAvgPool", "", t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_Identity {name, t_inp, t_out} =>
        op2str(name, "Identity", "", t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_If {name, then_branch, else_branch, t_inp, t_out} =>
        val then_branch_str = graph2str(model, then_branch, sub_indent)
        val else_branch_str = graph2str(model, else_branch, sub_indent)
        op2str(name, "If", f"then={then_branch_str}, else={else_branch_str}",
            t2str(model, [("t_inp", t_inp), \targs2pairs("t_out", t_out)]), indent)
    | NN_LeakyRelu {name, alpha, t_inp, t_out} =>
        op2str(name, "LeakyRelu", f"alpha={alpha}", t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_Loop {name, body, t_trip_count, t_cond_in, t_v_in, t_v_out} =>
        val body_str = graph2str(model, body, sub_indent)
        op2str(name, "Loop", f"body={body_str}",
            t2str(model, [("t_trip_count", t_trip_count), ("t_cond_in", t_cond_in),
                \targs2pairs("t_v_in", t_v_in), \targs2pairs("t_v_out", t_v_out)]), indent)
    | NN_LRN {name, size, alpha, beta, bias, t_inp, t_out} =>
        op2str(name, "LRN", f"size={size},\nalpha={alpha},\nbeta={beta},\nbias={bias}",
                t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_MaxPool {name, ceil_mode, dilations, kernel_shape, pads,
        strides, storage_order, t_inp, t_out} =>
        op2str(name, "MaxPool", f"ceil_mode={ceil_mode}, dilations={dilations}, kernel_shape={kernel_shape}, \
            pads={pads}, strides={strides}, storage_order={storage_order}",
            t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_NonMaxSuppression {
        name, center_point_box, t_boxes, t_scores,
        t_max_output_boxes_per_class, t_iou_threshold,
        t_score_threshold, t_out } =>
        op2str(name, "NonMaxSuppression", f"center_point_box={center_point_box}",
            t2str(model, [("t_boxes", t_boxes), ("t_scores", t_scores), ("t_max_output_boxes_per_class", t_max_output_boxes_per_class),
            ("t_iou_threshold", t_iou_threshold), ("t_score_threshold", t_score_threshold), ("t_out", t_out)]), indent)
    | NN_NonZero { name, t_inp, t_out } =>
        op2str(name, "NonZero", "", t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_QLinearAdd {name, t_A, t_A_scale, t_A_zp, t_B, t_B_scale, t_B_zp,
                    t_out_scale, t_out_zp, t_out} =>
        op2str(name, "QLinearAdd", "",
            t2str(model, [("t_A", t_A), ("t_B", t_B),
                ("t_A_scale", t_A_scale), ("t_A_zp", t_A_zp),
                ("t_B_scale", t_B_scale), ("t_B_zp", t_B_zp),
                ("t_out", t_out)]), indent)
    | NN_QLinearConv {name, attr, t_inp, t_weights, t_bias, t_out, t_inp_scale, t_inp_zp,
                      t_w_scale, t_w_zp, t_out_scale, t_out_zp} =>
        op2str(name, "QLinearConv", conv_attr2str(attr),
            t2str(model, [("t_inp", t_inp), ("t_weights", t_weights), ("t_bias", t_bias),
                ("t_inp_scale", t_inp_scale), ("t_inp_zp", t_inp_zp),
                ("t_w_scale", t_w_scale), ("t_w_zp", t_w_zp),
                ("t_out_scale", t_out_scale), ("t_out_zp", t_out_zp),
                ("t_out", t_out)]), indent)
    | NN_QLinearGlobalAvgPool {name, channels_last, t_inp, t_out, t_inp_scale,
                               t_inp_zp, t_out_scale, t_out_zp} =>
        op2str(name, "QLinearGlobalAvgPool", f"channels_last={channels_last}",
            t2str(model, [("t_inp", t_inp),
                ("t_inp_scale", t_inp_scale), ("t_inp_zp", t_inp_zp),
                ("t_out_scale", t_out_scale), ("t_out_zp", t_out_zp),
                ("t_out", t_out)]), indent)
    | NN_QLinearMatMul {name, t_A, t_A_scale, t_A_zp, t_B, t_B_scale, t_B_zp,
                    t_out_scale, t_out_zp, t_out} =>
        op2str(name, "QLinearMatMul", "",
            t2str(model, [("t_A", t_A), ("t_B", t_B),
                ("t_A_scale", t_A_scale), ("t_A_zp", t_A_zp),
                ("t_B_scale", t_B_scale), ("t_B_zp", t_B_zp),
                ("t_out", t_out)]), indent)
    | NN_QuantizeLinear {name, axis, t_inp, t_scale, t_zp, t_out} =>
        op2str(name, "QuantizeLinear", f"axis={axis}",
            t2str(model, [("t_inp", t_inp), ("t_scale", t_scale),
                   ("t_zp", t_zp), ("t_out", t_out)]), indent)
    | NN_Range {name, t_start, t_limit, t_delta, t_out} =>
        op2str(name, "Range", "", t2str(model, [("t_start", t_start), ("t_limit", t_limit),
            ("t_delta", t_delta), ("t_out", t_out)]), indent)
    | NN_Reduce {name, reduce_op, axes, keepdims, t_inp, t_out} =>
        op2str(name, string(reduce_op), f"axes={axes}, keepdims={keepdims}",
            t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_Resize { name, coord_trans, cubic_coeff_a, exclude_outside, extrapolation_value,
        mode, nearest_mode, t_inp, t_scales, t_sizes, t_roi, t_out } =>
        val nearest_mode_str =
            if mode == NN_Inter_Nearest {f", nearest_mode={nearest_mode}"} else {""}
        val tensors = ("t_out", t_out) :: []
        val tensors = if coord_trans == NN_CT_TFCropResize {("t_roi", t_roi) :: tensors} else {tensors}
        val tensors = ("t_scales", t_scales) :: ("t_sizes", t_sizes) :: tensors
        op2str(name, "Resize", f"coord_trans={coord_trans}, cubic_coeff_a={cubic_coeff_a},\
            exclude_outside={exclude_outside}, extrapolation_value={extrapolation_value},\
            mode={mode}{nearest_mode_str}",
            t2str(model, array(("t_inp", t_inp) :: tensors)), indent)
    | NN_Reshape {name, allowzero, t_inp, t_shape, t_out} =>
        op2str(name, "Reshape", f"allowzero={allowzero}",
            t2str(model, [("t_inp", t_inp), ("t_shape", t_shape), ("t_out", t_out)]), indent)
    | NN_RoiAlign {name, coord_trans, mode, output_height, output_width,
        sampling_ratio, spatial_scale, t_inp, t_rois, t_batch_ind, t_out} =>
        op2str(name, "RoiAlign", f"coord_trans={coord_trans}, pooling_mode={mode},\
            output_height={output_height}, output_width={output_width},\
            sampling_ratio={sampling_ratio}, sampling_ratio={sampling_ratio}",
            t2str(model, [("t_inp", t_inp), ("t_rois", t_rois), ("t_batch_ind", t_batch_ind), ("t_out", t_out)]), indent)
    | NN_Scatter {name, axis, t_data, t_updates, t_indices, t_out} =>
        op2str(name, "Scatter", f"axis={axis}",
            t2str(model, [("t_data", t_data), ("t_updates", t_updates), ("t_indices", t_indices), ("t_out", t_out)]), indent)
    | NN_Shape {name, start, end, t_inp, t_out} =>
        op2str(name, "Shape", f"start={start}, end={end}",
            t2str(model, [("t_data", t_inp), ("t_shape", t_out)]), indent)
    | NN_Slice {name, t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
        op2str(name, "Slice", "", t2str(model, [("t_inp", t_inp), ("t_starts", t_starts),
            ("t_ends", t_ends), ("t_axes", t_axes), ("t_steps", t_steps), ("t_out", t_out)]), indent)
    | NN_SoftMax {name, axis, t_inp, t_out } =>
        op2str(name, "SoftMax", f"axis={axis}", t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_Split {name, axis, t_inp, t_split, t_out} =>
        op2str(name, "Split", f"axis={axis}", t2str(model,
            [("t_inp", t_inp), ("t_split", t_split), \targs2pairs("t_out", t_out)]), indent)
    | NN_Squeeze {name, t_inp, t_axes, t_out} =>
        op2str(name, "Squeeze", "", t2str(model, [("t_inp", t_inp), ("t_axes", t_axes), ("t_out", t_out)]), indent)
    | NN_Tile {name, t_inp, t_repeats, t_out} =>
        op2str(name, "Tile", "", t2str(model, [("t_inp", t_inp), ("t_repeats", t_repeats), ("t_out", t_out)]), indent)
    | NN_TopK {name, axis, largest, sorted, t_inp, t_K, t_out, t_out_ind} =>
        op2str(name, "TopK", f"axis={axis}, largest={largest}, sorted={sorted}",
            t2str(model, [("t_inp", t_inp), ("t_K", t_K), ("t_out", t_out), ("t_out_ind", t_out_ind)]), indent)
    | NN_Transpose {name, perm, t_inp, t_out} =>
        op2str(name, "Transpose", f"perm={perm}", t2str(model, [("t_inp", t_inp), ("t_out", t_out)]), indent)
    | NN_Unsqueeze {name, t_inp, t_axes, t_out} =>
        op2str(name, "Unsqueeze", "", t2str(model, [("t_inp", t_inp), ("t_axes", t_axes), ("t_out", t_out)]), indent)
    }
}

fun string(info: dlnet_info_t) {
    | NN_Net_Generic => "Generic_Net {}"
    | NN_Net_Onnx(onnx) =>
        val opsets=
            if onnx.opsets == [] {"[]"}
            else {
                join_embrace("[\n       ", "\n    ]", ",\n        ",
                    [:: for (ver, domain) <- onnx.opsets {f"\"{domain} v{ver}\""}])
            }
        f"Imported_from_Onnx {{
    ir_version={onnx.ir_version},
    producer=\"{onnx.producer}\",
    domain=\"{onnx.domain}\",
    doc_string=\"{onnx.doc_string}\",
    opsets={opsets}
}}"
}

fun print(model: nnmodel_t)
{
    match model.info {
    | NN_Net_Generic => {}
    | _ => println(string(model.info))
    }
    println(graph2str(model, model.graph, ""))
}

fun nndata_t.copy() =
match self {
    | NN_Data_Empty | NN_Data_Stub_BF16 => NN_Data_Empty
    | NN_Data_Int(arr) => NN_Data_Int(copy(arr))
    | NN_Data_I8(arr) => NN_Data_I8(copy(arr))
    | NN_Data_U8(arr) => NN_Data_U8(copy(arr))
    | NN_Data_I16(arr) => NN_Data_I16(copy(arr))
    | NN_Data_U16(arr) => NN_Data_U16(copy(arr))
    | NN_Data_I32(arr) => NN_Data_I32(copy(arr))
    | NN_Data_U32(arr) => NN_Data_U32(copy(arr))
    | NN_Data_I64(arr) => NN_Data_I64(copy(arr))
    | NN_Data_U64(arr) => NN_Data_U64(copy(arr))
    | NN_Data_FP32(arr) => NN_Data_FP32(copy(arr))
    | NN_Data_FP64(arr) => NN_Data_FP64(copy(arr))
    | NN_Data_FP16(arr) => NN_Data_FP16(copy(arr))
    | NN_Data_Bool(arr) => NN_Data_Bool(copy(arr))
}

fun empty_shape() = nnshape_t {
    layout = NN_Layout_Unknown,
    shape = []
}

fun empty_tensor() = nntensor_t {
    shape = empty_shape(),
    data = NN_Data_Empty
}

fun empty_arg() = nnarg_t {
    name = "",
    argkind = NN_Arg_Temp,
    shape = empty_shape(),
    typ = Notype
}

fun total(shape: nnshape_t) = fold p=1 for sz <- shape.shape {p*sz}

fun nnshape_t.copy() = nnshape_t {
    layout = self.layout,
    shape = self.shape.copy()
}

fun nnshape_t.get_num_channels()
{
    val ndims = self.shape.size()
    match (self.layout, ndims) {
    | (_, 1) => self.shape[0]
    | (NN_Layout_ND, _) => self.shape[1]
    | (NN_Layout_NCHW, _) => self.shape[1]
    | (NN_Layout_NHWC, _) => self.shape[ndims-1]
    | (NN_Layout_NCHWxc, _) => self.shape[1]*self.shape[ndims-1]
    | _ => -1
    }
}

fun nnshape_t.get_spatial_channel_range()
{
    val ndims = self.shape.size()
    match self.layout {
    | NN_Layout_NCHW => (2, ndims)
    | NN_Layout_NHWC => (1, ndims-1)
    | NN_Layout_NCHWxc => (2, ndims-1)
    | _ => throw NNError(f"the shape layout {self.layout} is not supported in get_spatial_channel_range()")
    }
}

fun coerce_layouts(a: nnlayout_t, b: nnlayout_t) =
match (a, b) {
    | (NN_Layout_Unknown, _) => b
    | (_, NN_Layout_Unknown) => a
    | (NN_Layout_ND, _) => b
    | (_, NN_Layout_ND) => a
    | (_, _) =>
        if a != b {
            throw NNError(f"two layouts, {a} and {b}, cannot be used together")
        }
        a
}

// see https://github.com/onnx/onnx/blob/main/docs/Broadcasting.md
// for the description of multi-way broadcasting
fun nnshape_t.broadcast(another: nnshape_t)
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
        val sh = [for i <- 0:ndims {
            val a = if i >= d0 {self.shape[i-d0]} else {1}
            val b = if i >= d1 {another.shape[i-d1]} else {1}
            if a == b {a} else if a == 1 {b} else if b == 1 {a}
            else {
                throw NNError("the two shapes are not compatible for the mutual broadcasting")
            }
        }]
        nnshape_t {shape=sh, layout=layout}
    }
}

fun mktensor(shape: nnshape_t, typ: scalar_t)
{
    val total = shape.total()
    val data = match typ {
        | Type_Int => NN_Data_Int(array(total, 0))
        | Type_I8 => NN_Data_I8(array(total, 0i8))
        | Type_U8 => NN_Data_U8(array(total, 0u8))
        | Type_I16 => NN_Data_I16(array(total, 0i16))
        | Type_U16 => NN_Data_U16(array(total, 0u16))
        | Type_I32 => NN_Data_I32(array(total, 0i32))
        | Type_U32 => NN_Data_U32(array(total, 0u32))
        | Type_I64 => NN_Data_I64(array(total, 0i64))
        | Type_U64 => NN_Data_U64(array(total, 0u64))
        | Type_F32 => NN_Data_FP32(array(total, 0.f))
        | Type_F64 => NN_Data_FP64(array(total, 0.))
        | Type_Bool => NN_Data_Bool(array(total, false))
        | _ => throw NNError(f"unsupported tensor type {typ}")
    }
    nntensor_t {shape=shape, data=data}
}

@private fun mktensor_(arr: uint8 [], shape: int [], typ: scalar_t, ~layout: nnlayout_t=NN_Layout_Unknown)
{
    fun make_data_(arr: uint8 [], typ: scalar_t): nndata_t
    @ccode {
        fx_result->tag = typ->tag;
        fx_copy_arr(arr, &fx_result->u.NN_Data_U8);
        return FX_OK;
    }
    val data = make_data_(arr, typ)
    nntensor_t {shape=nnshape_t {shape=shape, layout=layout}, data=data}
}

fun mktensor(arr: 't [+], ~layout: nnlayout_t=NN_Layout_Unknown)
{
    val sz = arr.size()
    val shape = [for sz_i <- sz {sz_i}]
    val typ = scalar_type(0 :> 't)
    mktensor_(reinterpret(arr[:]) : uint8 [], shape, typ, layout=layout)
}

fun mktensor(arr: 't [], ~layout: nnlayout_t=NN_Layout_Unknown)
{
    val sz = arr.size()
    val shape = [sz]
    val typ = scalar_type(0 :> 't)
    mktensor_(reinterpret(arr[:]) : uint8 [], shape, typ, layout=layout)
}

fun nntensor_t.copy() =
    nntensor_t { shape=self.shape.copy(), data=self.data.copy() }
fun nntensor_t.isscalar() = self.shape.total() == 1
fun nntensor_t.isfloatscalar() =
    self.shape.total() == 1 &&
    (match self.data {NN_Data_FP32 _ => true | _ => false})

fun nnarg_t.isconst() = self.argkind == NN_Arg_Const

fun nnarg_t.copy() = nnarg_t {
    name = self.name,
    argkind = self.argkind,
    shape = self.shape.copy(),
    typ = self.typ
}

fun nnmodel_t.get_tensor(argidx: int) = if argidx > 0 {self.tensors[argidx]} else {empty_tensor()}

fun nnmodel_t.isconst(argidx: int) = argidx == 0 || self.args[argidx].argkind == NN_Arg_Const
fun nnmodel_t.isinput(argidx: int) = argidx > 0 && self.args[argidx].argkind == NN_Arg_Input
fun nnmodel_t.isoutput(argidx: int) = argidx > 0 && self.args[argidx].argkind == NN_Arg_Output
fun nnmodel_t.istemp(argidx: int) = argidx > 0 && self.args[argidx].argkind == NN_Arg_Temp
fun nnmodel_t.isscalar(argidx: int) = argidx > 0 && self.tensors[argidx].isscalar()
fun nnmodel_t.isfloatscalar(argidx: int) = argidx > 0 && self.tensors[argidx].isfloatscalar()
fun nnmodel_t.get_input_names(): string [] =
    [for i <- self.graph.inpargs {
        self.args[i].name
    }]
fun nnmodel_t.get_output_names(): string [] =
    [for i <- self.graph.outargs {
        self.args[i].name
    }]

fun fit(shape: nnshape_t, typ: scalar_t, data: nndata_t,
        buf: nnbuf_t, ~x2: bool=false): (nndata_t, nnbuf_t)
{
    val bufpadding = 128
    val new_total = shape.total()
    val elemsz = elemsize(typ)
    val typ0 = data.elemtype()
    val total0 = data.total()
    val reallocate = typ != typ0 || total0 != new_total

    fun fit_(total: int, elemsz: int, typ: scalar_t,
        bufpadding: int, buf: nnbuf_t, x2: bool): (nndata_t, nnbuf_t)
    @ccode {
        int_ total_ = total*elemsz + bufpadding;
        fx_arr_t* data_arr = &fx_result->t0.u.NN_Data_U8;

        // if buffer has enough space to fit the new data, we re-use it
        if (total_ > buf->dim[0].size*(int_)buf->dim[0].step) {
            if (x2) {
                int_ curr_x2 = buf->dim[0].size*(int_)buf->dim[0].step*2;
                total_ = total_ >= curr_x2 ? total_ : curr_x2;
            }
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
        fit_(new_total, elemsz, typ, bufpadding, buf, x2)
    } else {
        (data, buf)
    }
}

fun fit(model: nnmodel_t, argidx: int, shape: nnshape_t, typ: scalar_t, ~x2: bool=false)
{
    val shape0 = model.tensors[argidx].shape
    val typ0 = model.tensors[argidx].elemtype()
    if shape != shape0 || typ != typ0 {
        model.args[argidx].shape.layout = shape.layout
        val bufidx = model.bufidxs[argidx]
        //println(f"   fit into buf #{bufidx} with shape={shape}, typ={typ}, data of {model.tensors[argidx].data.total()} elems, buf of {model.buffers[bufidx].size()} bytes")
        val (data, buf) = fit(shape, typ, model.tensors[argidx].data, model.buffers[bufidx], x2=x2)
        model.tensors[argidx].data = data
        model.tensors[argidx].shape = shape
        model.buffers[bufidx] = buf
    }
}

fun copy_tensor_data(inp: nndata_t, out: nndata_t): void
@ccode {
    fx_arr_t* inp_arr, *out_arr;
    if (inp->tag != out->tag)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
    if (inp->tag == 1)
        return FX_OK;
    inp_arr = &inp->u.NN_Data_U8;
    out_arr = &out->u.NN_Data_U8;
    if (inp_arr->ndims != out_arr->ndims ||
        inp_arr->ndims != 1 ||
        inp_arr->dim[0].size*inp_arr->dim[0].step !=
        out_arr->dim[0].size*out_arr->dim[0].step)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
    if (inp_arr->data != out_arr->data)
        memcpy(out_arr->data, inp_arr->data,
                inp_arr->dim[0].size*inp_arr->dim[0].step);
    return FX_OK;
}

fun nnmodel_t.copy_tensor_data(t_inp: int, t_out: int)
{
    val inp = self.get_tensor(t_inp)
    val out = self.get_tensor(t_out)
    copy_tensor_data(inp.data, out.data)
}

fun nnmodel_t.concat_inplace(t_inp: int, t_out: int)
{
    fun do_concat_(inp_data_: nndata_t, orig_out_data_: nndata_t,
                   new_out_data_: nndata_t): void
    @ccode {
        fx_arr_t* inp_data = &inp_data_->u.NN_Data_U8;
        fx_arr_t* orig_out_data = &orig_out_data_->u.NN_Data_U8;
        fx_arr_t* new_out_data = &new_out_data_->u.NN_Data_U8;
        size_t esz = inp_data->dim[0].step;
        int_ delta = inp_data->dim[0].size;
        int_ sz0 = orig_out_data->dim[0].size;
        int_ sz1 = new_out_data->dim[0].size;
        if (orig_out_data->dim[0].step != esz || new_out_data->dim[0].step != esz)
            return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
        if (sz1 != sz0 + delta)
            return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);
        if (new_out_data->data != orig_out_data->data)
            memcpy(new_out_data->data, orig_out_data->data, sz0*esz);
        memcpy(new_out_data->data + sz0*esz, inp_data->data, delta*esz);
        return FX_OK;
    }
    val inp = self.get_tensor(t_inp)
    val out = self.get_tensor(t_out)
    val inp_shape = inp.shape.shape
    val out_shape = out.shape.shape
    val inp_ndims = inp_shape.size()
    val out_ndims = out_shape.size()
    val inp_typ = inp.elemtype()
    val out_typ = out.elemtype()
    assert(`inp_ndims+1 == out_ndims`)
    assert(`inp_typ == out_typ`)
    val new_shape = [for i <- 0:out_ndims {
        if i == 0 {out_shape[0] + 1}
        else {
            assert(`inp_shape[i-1] == out_shape[i]`)
            out_shape[i]
        }}]
    val orig_out_data = out.data
    self.fit(t_out, nnshape_t {shape=new_shape, layout=inp.shape.layout}, inp_typ, x2=true)
    do_concat_(inp.data, orig_out_data, self.tensors[t_out].data)
}

fun nnmodel_t.use_counts(): int []
{
    val nargs = self.args.size()
    val usecounts = array(nargs, 0)

    fun update_counts(graph: nngraph_t)
    {
        val {outargs} = graph
        /* 1. when we have the main graph, we gonna use its outputs in one way or another,
              so we increment the use count.
           2. when we have a subgraph, we need to copy (which could possibly done in-place, i.e.
              without actual copying) its formal outputs to the actual outputs,
              specified in If, Loop, Scan etc. To reflect it, we increment the use counts as well.
           So, whether it's the main graph or a subgraph, we increment 'usage counter' of each
           its output
        */
        for outarg <- outargs {usecounts[outarg] += 1}
        for op <- graph.prog {
            val (inps, _) = op.get_inputs_outputs()
            for i <- inps {usecounts[i] += 1}
            match op {
            | NN_If {then_branch, else_branch} =>
                update_counts(then_branch)
                update_counts(else_branch)
            | NN_Loop {body} =>
                update_counts(body)
            | _ => {}
            }
        }
    }

    update_counts(self.graph)
    usecounts
}

fun normalize_axis(axis: int, ndims: int) {
    val axis = if axis < 0 {axis + ndims} else {axis}
    assert(`0 <= axis <= ndims`)
    axis
}

always_use((NN_Data_I32([0i32, 1i32]), NN_ReduceL1, NN_Add))
