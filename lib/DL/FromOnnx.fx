/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Converts Onnx model to DL.Ast
import Ast, Onnx.Ast as OAst, Onnx.Parser
import Hashmap

exception OnnxConvertError: string

type param_hash_t = (string, int) Hashmap.t
type dim_t = DimParam: string | DimValue: int64

fun onnx2typ(t: OAst.tensor_t) = match t.data {
    | OAst.T_INT8 _ => Ast.DL_I8
    | OAst.T_INT64 _ => Ast.DL_I64
    | OAst.T_FLOAT _ => Ast.DL_FP32
}

fun onnx2typ(dtyp: OAst.datatype_t) {
    | OAst.DTYP_UNDEFINED => Ast.DL_UNDEFINED
    | OAst.DTYP_FLOAT => Ast.DL_FP32
    | OAst.DTYP_UINT8 => Ast.DL_U8
    | OAst.DTYP_INT8 => Ast.DL_I8
    | OAst.DTYP_UINT16 => Ast.DL_U16
    | OAst.DTYP_INT16 => Ast.DL_I16
    | OAst.DTYP_INT32 => Ast.DL_I32
    | OAst.DTYP_UINT32 => Ast.DL_U32
    | OAst.DTYP_INT64 => Ast.DL_I64
    | OAst.DTYP_UINT64 => Ast.DL_U64
    | OAst.DTYP_FLOAT16 => Ast.DL_FP16
    | OAst.DTYP_BFLOAT16 => Ast.DL_BF16
    | OAst.DTYP_BOOL => Ast.DL_I8
    | OAst.DTYP_DOUBLE => Ast.DL_FP64
    | OAst.DTYP_STRING | OAst.DTYP_COMPLEX64 | OAst.DTYP_COMPLEX128 =>
        throw OnnxConvertError("unsupported datatype")
}

fun onnx2tensor(t: OAst.tensor_t, isconst: bool) =
Ast.dltensor_t {
    isconst = isconst,
    shape = t.shape,
    data = (match t.data {
    | OAst.T_INT8(w) => Ast.DL_I8(w)
    | OAst.T_INT64(w) _ => Ast.DL_I64(w)
    | OAst.T_FLOAT(w) => Ast.DL_FP32(w)
    })
}

fun onnx2shape(params: vi: OAst.valueinfo_t) =
match vi.typ {
    | OAst.TYPINFO_TENSOR(dtyp, dims) =>
        dlshape_t { shape=[| for d <- dims {
            match d {
            | OAst.
            }
         }]}

}

fun 

fun onnx2tinfo(vi: OAst.valueinfo_t) =
Ast.dltinfo_t {
    name = vi.name,
    shape = dlshape_t {
        shape =
    }: dlshape_t
    constidx: int
    bufidx: int
    isconst = false,
    shape = vi.
    match t.data {
    | T_INT8 _ => DL_I8
    | T_INT64 _ => DL_I64
    | T_FLOAT _ => DL_FP32
}

fun onnx2type valueinfo_t =
{
    name: tensor_id_t
    denotation: string
    typ: typeinfo_t
}



fun convert(model: OAst.model_t): Ast.dlnet_t
{
    val all_names = Hashmap.empty(1024, "", )
    var nbufs = 0

    val info = Ast.DL_Net_Onnx (Ast.dlonnx_t {
        ir_version = model.ir_version,
        producer = model.producer,
        domain = model.domain,
        doc_string = model.doc_string,
        opsets = [for ops <- model.import_opsets {(ops.version, ops.domain)}]
    })

    val (cbufs, consts) = [|
        @unzip for c@i <- model.graph.initialiazers {
            all_names.add(c.name, -i-1)
            (onnx2tensor(c, true),
            Ast.dltinfo_t {
                name=c.name,
                shape=Ast.dlshape_t {
                    shape=c.shape,
                    typ=onnx2typ(c)
                },
                constidx=i,
                bufidx=-1
            })
        } |]
    val values = [|
        for v@i <- model.graph.values {


        } |]

    Ast.empty_dlnet().{
        info = info,
        cbufs = cbufs,
        tensors = consts
    }

    type node_t =
{
    name: string
    op: string
    inputs: tensor_id_t []
    outputs: tensor_id_t []
    attrs: attr_t []
}

type graph_t =
{
    name: string
    inputs: valueinfo_t []
    outputs: valueinfo_t []
    values: valueinfo_t []
    initializers: tensor_t []
    nodes: node_t []
}
}

fun convert_int(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrInt(i) => int(i)
    | OAst.AttrFloat(f) => int(f)
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to int")
    }

fun read(fname: string) = convert(Onnx.Parser.parse(fname))
