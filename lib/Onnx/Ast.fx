/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// hierarchical representation of Onnx format
import Hashmap

type datatype_t =
    | DTYP_UNDEFINED | DTYP_FLOAT | DTYP_UINT8 | DTYP_INT8 | DTYP_UINT16 | DTYP_INT16
    | DTYP_INT32 | DTYP_INT64 | DTYP_STRING | DTYP_BOOL | DTYP_FLOAT16 | DTYP_DOUBLE
    | DTYP_UINT32 | DTYP_UINT64 | DTYP_COMPLEX64 | DTYP_COMPLEX128 | DTYP_BFLOAT16

type tdata_t =
    | T_FLOAT: float []
    | T_INT8: int8 []
    | T_INT32: int32 []
    | T_INT64: int64 []

type tensor_t =
{
    name: string
    shape: int []
    data: tdata_t
}

type opset_t =
{
    version: int64
    domain: string
}

type dim_t = DimParam: string | DimValue: int64
type typeinfo_t = TYPINFO_TENSOR: (datatype_t, dim_t [])

type valueinfo_t =
{
    name: string
    denotation: string
    typeinfo: typeinfo_t
}

type attrval_t =
    | AttrInt: int64 | AttrFloat: float
    | AttrString: string | AttrTensor: tensor_t
    | AttrFloats: float [] | AttrInts: int [] | AttrStrings: string []
    | AttrGraph: graph_t

type attr_t = Attr: {
    name: string
    v: attrval_t
}

type node_t = Node: {
    name: string
    op: string
    inputs: string []
    outputs: string []
    attrs: attr_t []
}

type graph_t = Graph: {
    name: string
    inputs: valueinfo_t []
    outputs: valueinfo_t []
    values: valueinfo_t []
    initializers: tensor_t []
    nodes: node_t []
}

type model_t =
{
    ir_version: int64
    producer: string
    domain: string
    doc_string: string
    import_opsets: opset_t []
    metadata: (string, string) []
    graph: graph_t
}

fun total(t: tensor_t) = product(t.shape, 1)

fun string(d: datatype_t)
{
    | DTYP_UNDEFINED => "undefined"
    | DTYP_FLOAT => "float"
    | DTYP_UINT8 => "uint8"
    | DTYP_INT8 => "int8"
    | DTYP_UINT16 => "uint16"
    | DTYP_INT16 => "int16"
    | DTYP_INT32 => "int32"
    | DTYP_INT64 => "int64"
    | DTYP_STRING => "string"
    | DTYP_BOOL => "bool"
    | DTYP_FLOAT16 => "float16"
    | DTYP_DOUBLE => "double"
    | DTYP_UINT32 => "uint32"
    | DTYP_UINT64 => "uint64"
    | DTYP_COMPLEX64 => "complex64"
    | DTYP_COMPLEX128 => "complex128"
    | DTYP_BFLOAT16 => "bfloat16"
}

fun string(ops: opset_t) = f"{ops.domain} v{ops.version}"
fun string(dim: dim_t) {
    | DimParam(n) => n
    | DimValue(v) => string(v)
}

fun tensor_data_prefix(t: tdata_t)
{
    | T_FLOAT _ => "float32"
    | T_INT8 _ => "int8"
    | T_INT32 _ => "int32"
    | T_INT64 _ => "int64"
}

fun tensor_data_to_floats(t: tdata_t)
{
    | T_FLOAT(w) => w
    | T_INT8(w) => float(w)
    | T_INT32(w) => float(w)
    | T_INT64(w) => float(w)
}

fun tensor_data_to_ints(t: tdata_t)
{
    | T_FLOAT(w) => int(w)
    | T_INT8(w) => int(w)
    | T_INT32(w) => int(w)
    | T_INT64(w) => int(w)
}

fun print_tensor_data(t: tdata_t)
{
    | T_FLOAT(data) => print(data)
    | T_INT8(data) => print(data)
    | T_INT32(data) => print(data)
    | T_INT64(data) => print(data)
}

fun print(t: tensor_t)
{
    val shape = "x".join(t.shape.map(string))
    print(f"tensor {{name='{t.name}', shape={shape}, data={tensor_data_prefix(t.data)} ")
    if total(t) > 10 {print("[...]")} else {print_tensor_data(t.data)}
    print("}")
}

fun print(a: attr_t)
{
    print(f"{a.name}: ")
    match a.v {
    | AttrInt(i) => print(i)
    | AttrFloat(f) => print(f)
    | AttrString(s) => print(repr(s))
    | AttrTensor(t) => print(t)
    | AttrInts(ints) => print(ints)
    | AttrFloats(floats) => print(floats)
    | AttrStrings(strings) => print(strings)
    | AttrGraph(gr) => print(gr)
    }
}

fun print(n: node_t)
{
    val indent0 = " "*4
    val indent1 = indent0 + indent0
    val indent2 = indent1 + indent0
    val indent3 = indent2 + indent0
    println(f"{{")
    println(f"{indent2}name: '{n.name}'")
    println(f"{indent2}op: {n.op}")
    println(f"{indent2}inputs: {n.inputs}")
    println(f"{indent2}outputs: {n.outputs}")
    print(f"{indent2}attributes: {{")
    if n.attrs.empty() {print(f"}}\n{indent1}}}")}
    else {
        println()
        for a <- n.attrs {
            print(indent3)
            println(a)
        }
        print(f"{indent2}}}\n{indent1}}}")
    }
}

fun print(vi: valueinfo_t)
{
    print(f"{vi.name}")
    if vi.denotation != "" {
        print(f" ({vi.denotation})")
    }
    print(": ")
    match vi.typeinfo {
    | TYPINFO_TENSOR (dt, diminfo) =>
        val diminfo = " x ".join(diminfo.map(string))
        print(f"{dt}, {diminfo}")
    }
}

fun print(graph: graph_t)
{
    val indent0 = " "*4, indent1 = indent0 + indent0
    println(f"graph {{")
    println(f"{indent0}name: {graph.name},")
    println(f"{indent0}inputs: {{")
    for x <- graph.inputs { print(indent1); print(x); println(",") }
    println(f"{indent0}}},\n{indent0}outputs: {{")
    for x <- graph.outputs { print(indent1); print(x); println(",") }
    println(f"{indent0}}},\n{indent0}values: {{")
    for x <- graph.values { print(indent1); print(x); println(",") }
    println(f"{indent0}}},\n{indent0}initializers: {{")
    for x <- graph.initializers { print(indent1); print(x); println(",") }
    println(f"{indent0}}},\n{indent0}nodes: {{")
    for x <- graph.nodes { print(indent1); print(x); println(",") }
    print(f"{indent0}}}\n}")
}

fun print(model: model_t)
{
    println(f"ir_version: {model.ir_version}")
    println(f"producer: {model.producer}")
    println(f"domain: {model.domain}")
    println(f"doc_string: '{model.doc_string}'")
    for opset <- model.import_opsets {
        println(f"import opset: {opset}")
    }
    for (k, v) <- model.metadata {
        println(f"property '{k}': '{v}'")
    }
    println(model.graph)
    println("// end of model")
}

always_use([::AttrInt(5L), AttrString("abc")])
