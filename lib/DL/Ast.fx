/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Deep Nets topology description

import Hashmap

type dltyp_t =
    | DL_UNDEFINED | DL_I8 | DL_U8 | DL_I16 | DL_U16 | DL_I32 | DL_U32
    | DL_I64 | DL_U64 | DL_FP16 | DL_BF16 | DL_FP32 | DL_FP64

type dldata_t =
    | DL_TN_EMPTY
    | DL_TN_I8: int8 []
    | DL_TN_U8: uint8 []
    | DL_TN_I32: int32 []
    | DL_TN_I64: int64 []
    | DL_TN_FP32: float []

type dlshape_t
{
    shape: int []
    typ: dltyp_t
}

type dltensor_t =
{
    isconst: bool
    shape: int []
    data: dldata_t
}

type dlactiv_t =
    | DL_Relu
    | DL_PRelu: float []

type dlelwise1_t =
    | DL_Abs
    | DL_Sign
    | DL_Exp
    | DL_Log
    | DL_Sin
    | DL_Cos

type dlelwise2_t =
    | DL_Add
    | DL_Sub
    | DL_Less
    | DL_Greater

type dlpadding_t =
    | DL_PadNone
    | DL_PadSameUpper
    | DL_PadSameLower
    | DL_PadValid

type dlorder_t =
    | DL_RowMajor
    | DL_ColumnMajor

type dlop_t =
    | DL_Conv2D: (dlconv2d_t, int, int, int, int)
    | DL_Gemm: (dlgemm_t, int, int, int, int)
    | DL_Activ: (dlactiv_t, int, int)
    | DL_EWise1: (dlelwise1_t, int, int)
    | DL_EWise2: (dlelwise2_t, int, int, int)
    | DL_BatchNorm: (dlbatchnorm_t, int, int, int, int)
    | DL_MaxPool: (dlpool2d_t, int, int)
    | DL_AvgPool: (dlpool2d_t, int, int)
    | DL_Reshape: (int list, int, int)
    | DL_SoftMax: (int, int)
    | DL_Concat: (int, int list, int)
    | DL_Dropout: (float, int, int)
    | DL_LRN: (dllrn_t, int, int)

type dlconv2d_t =
{
    dilation: (int, int) = (1, 1)
    kernel_shape: (int, int)
    pads: (int, int) = (0, 0)
    strides: (int, int) = (1, 1)
    ngroups: int = 1
}

type dlgemm_t =
{
    alpha: float = 1.f
    beta: float = 1.f
    transA: bool = false
    transB: bool = false
}

type dlpool2d_t =
{
    auto_pad: dlpadding_t = DL_PadNone
    ceil_mode: bool = false
    dilation: (int, int) = (1, 1)
    kernel_shape: (int, int)
    pads: (int, int) = (0, 0)
    strides: (int, int) = (1, 1)
    storage_order: dlorder_t = DL_RowMajor
    count_include_pad: bool = false
}

type dllrn_t =
{
    size: int
    alpha: float = 0.0001f
    beta: float = 0.75f
    bias: float = 1.f
}

type dlbatchnorm_t =
{
    epsilon: float = 1e-5f
    momentum: float = 0.9f
    training_mode: bool = false
}

type dlnames_t = (string, int) Hashmap.t

type dltinfo_t =
{
    name: string
    shape: dlshape_t
    constidx: int
    bufidx: int
}

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
    inputs: int []
    outputs: int []
    tensor_names: dlnames_t
    constants: dltensor_t []
    bufs: dltensor_t []
    tensors: dltinfo_t []
    prog: dlop_t []
}

fun empty_dlnet() = dlnet_t {
    info = DL_Net_Generic,
    inputs = [],
    outputs = [],
    tensor_names = Hashmap.empty(8, "", 0),
    constants = [],
    bufs = [],
    tensors = [],
    prog = []
}

fun string(order: dlorder_t)
{
    | DL_RowMajor => "RowMajor"
    | DL_ColumnMajor => "ColumnMajor"
}

fun string(activ: dlactiv_t)
{
    | DL_Relu => "ReLU"
    | DL_PRelu(p) => f"PReLU {{p={p}}}"
}

fun string(ew: dlelwise1_t)
{
    | DL_Abs => "Abs"
    | DL_Sign => "Sign"
    | DL_Exp => "Exp"
    | DL_Log => "Log"
    | DL_Sin => "Sin"
    | DL_Cos => "Cos"
}

fun string(ew: dlelwise2_t)
{
    | DL_Add => "Add"
    | DL_Sub => "Sub"
    | DL_Less => "Less"
    | DL_Greater => "Greater"
}

fun string(p: dlpadding_t) {
    | DL_PadNone => "NoPadding"
    | DL_PadSameUpper => "Padding_SameUpper"
    | DL_PadSameLower => "Padding_SameLower"
    | DL_PadValid => "Padding_Valid"
}

fun total(d: dldata_t)
{
    | DL_TN_EMPTY => 0
    | DL_TN_I8(elems) => elems.total()
    | DL_TN_U8(elems) => elems.total()
    | DL_TN_I32(elems) => elems.total()
    | DL_TN_I64(elems) => elems.total()
    | DL_TN_FP32(elems) => elems.total()
}

fun float(d: dldata_t)
{
    | DL_TN_EMPTY => ([] : float [])
    | DL_TN_I8(elems) => float(elems)
    | DL_TN_U8(elems) => float(elems)
    | DL_TN_I32(elems) => float(elems)
    | DL_TN_I64(elems) => float(elems)
    | DL_TN_FP32(elems) => elems
}

fun tdata2str(d: dldata_t) =
match d {
    | DL_TN_EMPTY => "[]"
    | DL_TN_I8(elems) => string(elems)
    | DL_TN_U8(elems) => string(elems)
    | DL_TN_I32(elems) => string(elems)
    | DL_TN_I64(elems) => string(elems)
    | DL_TN_FP32(elems) => string(elems)
}

fun string(typ: dltyp_t)
{
    | DL_UNDEFINED => "UNDEFINED"
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
}

fun string(s: dlshape_t)
{
    val shape_str = "x".join(s.shape.map(string))
    f"{s.typ} <{shape_str}>"
}

fun gettype(t: dltensor_t) = match t.data {
    | DL_TN_EMPTY => DL_UNDEFINED
    | DL_TN_I8 _ => DL_I8
    | DL_TN_U8 _ => DL_U8
    | DL_TN_I32 _ => DL_I32
    | DL_TN_I64 _ => DL_I64
    | DL_TN_FP32 _ => DL_FP32
}

fun shape(t: dltensor_t) =
    dlshape_t {shape=t.shape, typ=gettype(t)}

fun string(t: dltensor_t) = match t.data {
    | DL_TN_EMPTY => "[]"
    | _ =>
        val nelems = total(t.data)
        val sp = string(shape(t))
        val cprefix = if t.isconst {"const "} else {""}
        cprefix + sp + " " + (if nelems > 10 || !t.isconst {"[...]"} else {tdata2str(t.data)})
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
        | (',', ('{' :: [] | [])) => true
        | _ => false
    }
    params.tokens(issep).map(String.strip)
}

fun op2str(opname: string, params: string, tensors: string list)
{
    val indent = "   "
    val pl = parse_params(params)
    join_embrace(f"{opname} {{\n{indent}",
        join_embrace("", "\n}", f",\n{indent}", pl),
        "\n{indent}", tensors)
}

fun t2str(net: dlnet_t, tensors: (string, int) list) =
    [for (name, tidx) <- tensors {
        val tinfo = net.tensors[tidx]
        val t = if tinfo.constidx >= 0 {net.constants[tinfo.constidx]}
                else {net.bufs[tinfo.bufidx]}
        f"{name}=\"{tinfo.name}\", // {t}"
    }]

fun op2str(net: dlnet_t, op: dlop_t) =
match op {
    | DL_Conv2D(conv2d, inp, out, w, bias) =>
        op2str("Conv2D", string(conv2d), t2str(net, [("inp", inp), ("out", out), ("weights", w), ("bias", bias)]))
    | DL_Gemm(gemm, inp, out, w, bias) =>
        op2str("Gemm", string(gemm), t2str(net, [("inp", inp), ("out", out), ("weights", w), ("bias", bias)]))
    | DL_Activ(activ, inp, out) =>
        val astr = string(activ)
        val pos = astr.find('{')
        val (name, params) =
            if pos > 0 { (astr[:pos-1], astr[pos:]) }
            else {(astr, "")}
        op2str(name, params, t2str(net, [("inp", inp), ("out", out)]))
    | DL_EWise1(ew1, inp, out) =>
        op2str(string(ew1), "", t2str(net, [("inp", inp), ("out", out)]))
    | DL_EWise2(ew2, inp1, inp2, out) =>
        op2str(string(ew2), "", t2str(net, [("inp1", inp1), ("inp2", inp2), ("out", out)]))
    | DL_BatchNorm(bnorm, inp, out, scale, b) =>
        op2str("BatchNorm", string(bnorm), t2str(net, [("inp", inp), ("out", out), ("scale", scale), ("B", b)]))
    | DL_MaxPool(pool, inp, out) =>
        op2str("MaxPool", string(pool), t2str(net, [("inp", inp), ("out", out)]))
    | DL_AvgPool(pool, inp, out) =>
        op2str("AvgPool", string(pool), t2str(net, [("inp", inp), ("out", out)]))
    | DL_Reshape(shape, inp, out) =>
        op2str("Reshape", f"{{shape={shape}}}", t2str(net, [("inp", inp), ("out", out)]))
    | DL_SoftMax(inp, out) =>
        op2str("SoftMax", "", t2str(net, [("inp", inp), ("out", out)]))
    | DL_Concat(dim, inplist, out) =>
        op2str("Concat", f"{{dim={dim}}}", t2str(net, [for inp@i <- inplist {(f"inp{i}", inp)}] + [("out", out)]))
    | DL_Dropout(scale, inp, out) =>
        op2str("Dropout", f"{{scale={scale}}}", t2str(net, [("inp", inp), ("out", out)]))
    | DL_LRN(lrn, inp, out) =>
        op2str("LRN", string(lrn), t2str(net, [("inp", inp), ("out", out)]))
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
    for (n, ts) <- [("inputs", net.inputs), ("outputs", net.outputs)] {
        val ts = [for tidx <- ts {
            val tinfo = net.tensors[tidx]
            val t = if tinfo.constidx >= 0 {net.constants[tinfo.constidx]}
                else {net.bufs[tinfo.bufidx]}
            f"\"{tinfo.name}\", // {t}"
        }]
        println(join_embrace(f"{n}=[\n", "\n]", "\n    ", ts))
    }

    for p <- net.prog {
        println(op2str(net, p))
    }
}
