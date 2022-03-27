import Ast, File
import Protobuf.Impl

@ccode {
#include "ficus-onnx.pb-c.c"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

static int64_t unpack_int64(uint8_t* p)
{
    uint64_t x = p[0] | ((uint64_t)p[1]<<8) | ((uint64_t)p[2]<<16) | ((uint64_t)p[3]<<24) |
        ((uint64_t)p[4]<<32) | ((uint64_t)p[5]<<40) | ((uint64_t)p[6]<<48) | ((uint64_t)p[7]<<56);
    return (int64_t)(x);
}

typedef struct onnx_dim_t
{
    int tag;
    union
    {
        fx_str_t param;
        int64_t value;
    } u;
} onnx_dim_t;

typedef struct onnx_type_info_t
{
    int datatype;
    fx_arr_t shapeinfo;
} onnx_type_info_t;

typedef struct onnx_value_info_t
{
    fx_str_t name;
    fx_str_t denotation;
    onnx_type_info_t typeinfo;
} onnx_value_info_t;

typedef struct onnx_tensor_t
{
    fx_str_t name;
    fx_arr_t shape;
    struct {
        int tag;
        fx_arr_t arr;
    } data;
} onnx_tensor_t;

struct onnx_graph_t;

typedef struct onnx_attr_t
{
    int_ rc;
    fx_str_t name;
    struct {
        int_ rc;
        int tag;
        union {
            int64_t attrInt;
            float attrFloat;
            fx_str_t attrStr;
            onnx_tensor_t attrTensor;
            fx_arr_t attrArr;
            struct onnx_graph_t* attrGraph;
        } u;
    }* v;
} onnx_attr_t;

typedef struct onnx_node_t
{
    int_ rc;
    fx_str_t name;
    fx_str_t op;
    fx_arr_t inputs;
    fx_arr_t outputs;
    fx_arr_t attrs;
} onnx_node_t;

typedef struct onnx_graph_t
{
    int_ rc;
    fx_str_t name;
    fx_arr_t inputs;
    fx_arr_t outputs;
    fx_arr_t values;
    fx_arr_t initializers;
    fx_arr_t nodes;
} onnx_graph_t;

typedef struct onnx_opset_import_t
{
    int64_t version;
    fx_str_t domain;
} onnx_opset_import_t;

typedef struct onnx_metadata_entry_t
{
    fx_str_t key;
    fx_str_t value;
} onnx_metadata_entry_t;

typedef struct onnx_model_t
{
    int64_t ir_version;
    fx_str_t producer;
    fx_str_t domain;
    fx_str_t doc_string;
    fx_arr_t opset_import;
    fx_arr_t metadata;
    onnx_graph_t* graph;
} onnx_model_t;

enum {
    REF_GRAPH = 0,
    REF_NODE = 1,
    REF_TENSOR = 2,
    REF_ATTR = 3,
    REF_VALINFO = 4,
    REF_DIM = 5,
    REF_STR = 6,
    REF_FLOAT = 7,
    REF_INT64 = 8,
    REF_OPSET_IMPORT = 9,
    REF_METADATA_ENTRY = 10,
    REF_MAX = 11
};

typedef int (*onnx_parse_elem_t)(const void* arrelem, const fx_arr_t* refarrs, char* result);

static int onnx_parse_array(const void* arr_, size_t nelems_, const fx_arr_t* refarrs, int idx,
                            onnx_parse_elem_t parse_elem, fx_arr_t* result)
{
    assert(0 <= idx && idx < REF_MAX);
    size_t elemsize = refarrs[idx].dim[0].step;
    const void** arr = (const void**)arr_;
    int_ nelems = (int_)nelems_;
    int fx_status = fx_make_arr(1, &nelems, elemsize, refarrs[idx].free_elem, refarrs[idx].copy_elem, 0, result);
    if (fx_status < 0) return fx_status;
    if (parse_elem) {
        for(int_ i = 0; i < nelems; i++) {
            fx_status = parse_elem(arr[i], refarrs, result->data + i*elemsize);
            if (fx_status < 0)
                break;
        }
    } else {
        memcpy(result->data, arr_, nelems*elemsize);
    }
    return fx_status;
}

static int onnx_parse_dim(FicusOnnx__TensorShapeProto__Dimension* dim,
                          const fx_arr_t* refarrs, onnx_dim_t* result)
{
    int fx_status = FX_OK;
    bool is_value = dim->value_case == FICUS_ONNX__TENSOR_SHAPE_PROTO__DIMENSION__VALUE_DIM_VALUE;
    result->tag = 1 + (int)is_value;
    if (is_value) {
        result->u.value = dim->dim_value;
    } else {
        fx_status = fx_cstr2str(dim->dim_param, -1, &result->u.param);
    }
    return fx_status;
}

static int onnx_parse_value_info(FicusOnnx__ValueInfoProto* vi, const fx_arr_t* refarrs,
                                 onnx_value_info_t* result)
{
    int fx_status = fx_cstr2str(vi->name, -1, &result->name);
    if (fx_status >= 0)
        fx_status = fx_cstr2str(vi->type->denotation, -1, &result->denotation);
    if (fx_status >= 0) {
        if (vi->type->value_case == FICUS_ONNX__TYPE_PROTO__VALUE_TENSOR_TYPE) {
            result->typeinfo.datatype = vi->type->tensor_type->elem_type + 1;
            const FicusOnnx__TensorShapeProto* shape = vi->type->tensor_type->shape;
            fx_status = onnx_parse_array(shape->dim, shape->n_dim, refarrs, REF_DIM,
                                     (onnx_parse_elem_t)onnx_parse_dim, &result->typeinfo.shapeinfo);
        } else {
            printf("error: unsupported value_case: vi->type->value_case=%d\n", (int)vi->type->value_case);
            FX_FAST_THROW_RET(FX_EXN_NotImplementedError);
        }
    }
    return fx_status;
}

static int onnx_parse_tensor(FicusOnnx__TensorProto* tensor, const fx_arr_t* refarrs,
                            onnx_tensor_t* result)
{
    int fx_status = fx_cstr2str(tensor->name, -1, &result->name);
    int_ n_dims = (int_)tensor->n_dims;
    int_ temp_dims = n_dims > 0 ? n_dims : 1;
    int_ total = 1;
    if (fx_status >= 0) {
        fx_status = fx_make_arr(1, &temp_dims, sizeof(int_), 0, 0, 0, &result->shape);
        if (fx_status >= 0) {
            int_* shape = (int_*)result->shape.data;
            shape[0] = 1;
            for(int_ i = 0; i < n_dims; i++) {
                shape[i] = (int_)tensor->dims[i];
                total *= shape[i];
            }
        }
    }
    if (fx_status >= 0) {
        int tag = tensor->data_type == FICUS_ONNX__TENSOR_PROTO__DATA_TYPE__INT8 ? 2 :
                  tensor->data_type == FICUS_ONNX__TENSOR_PROTO__DATA_TYPE__INT32 ? 3 :
                  tensor->data_type == FICUS_ONNX__TENSOR_PROTO__DATA_TYPE__INT64 ? 4 :
                  tensor->data_type == FICUS_ONNX__TENSOR_PROTO__DATA_TYPE__FLOAT ? 1 : -1;
        if (tag < 0)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
        size_t elemsize = tag == 1 || tag == 3 ? sizeof(float) : tag == 2 ? 1 : 8;
        fx_status = fx_make_arr(1, &total, elemsize, 0, 0, 0, &result->data.arr);
        if (fx_status >= 0) {
            result->data.tag = tag;
            if (elemsize == 1 && tensor->raw_data.len == total) {
                memcpy(result->data.arr.data, tensor->raw_data.data, total*elemsize);
            } else if (elemsize == 4 && tensor->n_float_data == total) {
                memcpy(result->data.arr.data, tensor->float_data, total*elemsize);
            } else if (elemsize == 4 && tensor->raw_data.len == total*4) {
                uint32_t* dst = (uint32_t*)result->data.arr.data;
                for(int_ i = 0; i < total; i++) {
                    uint8_t* p = tensor->raw_data.data + i*4;
                    dst[i] = (uint32_t)p[0] | ((uint32_t)p[1] << 8) |
                        ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
                }
            } else if (elemsize == 8 && tensor->raw_data.len == total*8) {
                int64_t* dst = (int64_t*)result->data.arr.data;
                for(int_ i = 0; i < total; i++) {
                    uint8_t* p = tensor->raw_data.data + i*8;
                    dst[i] = unpack_int64(p);
                }
            } else {
                memset(result->data.arr.data, 0, elemsize*total);
            }
        }
    }
    return fx_status;
}

static int onnx_parse_str(const char* str, const fx_arr_t* refarrs, fx_str_t* result)
{
    return fx_cstr2str(str, -1, result);
}

static int onnx_parse_graph(const FicusOnnx__GraphProto* graph,
                            const fx_arr_t* refarrs,
                            onnx_graph_t** result_);

static int onnx_parse_attr(const FicusOnnx__AttributeProto* attr,
                           const fx_arr_t* refarrs, onnx_attr_t** result_)
{
    onnx_attr_t* result = (onnx_attr_t*)fx_malloc(sizeof(*result));
    int fx_status, t;
    if (!result) {
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }
    *result_ = result;
    memset(result, 0, sizeof(*result));
    result->rc = 1;
    result->v = fx_malloc(sizeof(*result->v));
    if (!result) {
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }
    memset(result->v, 0, sizeof(*result->v));
    result->v->rc = 1;
    fx_status = fx_cstr2str(attr->name, -1, &result->name);
    t = attr->type;
    if (fx_status < 0)
        return fx_status;
    if (t == FICUS_ONNX__ATTRIBUTE_PROTO__ATTRIBUTE_TYPE__INT) {
        result->v->tag = 1;
        result->v->u.attrInt = attr->i;
    } else if (t == FICUS_ONNX__ATTRIBUTE_PROTO__ATTRIBUTE_TYPE__FLOAT) {
        result->v->tag = 2;
        result->v->u.attrFloat = attr->f;
    } else if (t == FICUS_ONNX__ATTRIBUTE_PROTO__ATTRIBUTE_TYPE__STRING) {
        result->v->tag = 3;
        fx_status = fx_cstr2str((char*)attr->s.data, attr->s.len, &result->v->u.attrStr);
    } else if (t == FICUS_ONNX__ATTRIBUTE_PROTO__ATTRIBUTE_TYPE__TENSOR) {
        result->v->tag = 4;
        fx_status = onnx_parse_tensor(attr->t, refarrs, &result->v->u.attrTensor);
    } else if (t == FICUS_ONNX__ATTRIBUTE_PROTO__ATTRIBUTE_TYPE__FLOATS) {
        result->v->tag = 5;
        fx_status = onnx_parse_array(attr->floats, attr->n_floats, refarrs, REF_FLOAT,
                                     0, &result->v->u.attrArr);
    } else if (t == FICUS_ONNX__ATTRIBUTE_PROTO__ATTRIBUTE_TYPE__INTS) {
        result->v->tag = 6;
        fx_status = onnx_parse_array(attr->ints, attr->n_ints, refarrs, REF_INT64,
                                     0, &result->v->u.attrArr);
    } else if (t == FICUS_ONNX__ATTRIBUTE_PROTO__ATTRIBUTE_TYPE__STRINGS) {
        result->v->tag = 7;
        fx_status = onnx_parse_array(attr->strings, attr->n_strings, refarrs, REF_STR,
                                     (onnx_parse_elem_t)onnx_parse_str, &result->v->u.attrArr);
    } else if (t == FICUS_ONNX__ATTRIBUTE_PROTO__ATTRIBUTE_TYPE__GRAPH) {
        result->v->tag = 8;
        fx_status = onnx_parse_graph(attr->g, refarrs, &result->v->u.attrGraph);
    } else {
        printf("error: unsupported attribute: attr->type=%d\n", t);
        FX_FAST_THROW_RET(FX_EXN_NotImplementedError);
    }
    return fx_status;
}

static int onnx_parse_node(const FicusOnnx__NodeProto* node,
                           const fx_arr_t* refarrs, onnx_node_t** result_)
{
    onnx_node_t* result = (onnx_node_t*)fx_malloc(sizeof(*result));
    int fx_status;
    if (!result) {
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }
    *result_ = result;
    memset(result, 0, sizeof(*result));
    result->rc = 1;
    fx_status = fx_cstr2str(node->name, -1, &result->name);
    if (fx_status >= 0)
        fx_status = fx_cstr2str(node->op_type, -1, &result->op);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(node->input, node->n_input, refarrs, REF_STR,
                                     (onnx_parse_elem_t)onnx_parse_str, &result->inputs);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(node->output, node->n_output, refarrs, REF_STR,
                                     (onnx_parse_elem_t)onnx_parse_str, &result->outputs);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(node->attribute, node->n_attribute, refarrs, REF_ATTR,
                                     (onnx_parse_elem_t)onnx_parse_attr, &result->attrs);
    return fx_status;
}

static int onnx_parse_graph(const FicusOnnx__GraphProto* graph,
                            const fx_arr_t* refarrs,
                            onnx_graph_t** result_)
{
    onnx_graph_t* result = (onnx_graph_t*)fx_malloc(sizeof(*result));
    int fx_status, t;
    if (!result) {
        FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
    }
    *result_ = result;
    memset(result, 0, sizeof(*result));
    result->rc = 1;
    fx_status = fx_cstr2str(graph->name, -1, &result->name);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(graph->input, graph->n_input, refarrs, REF_VALINFO,
                                     (onnx_parse_elem_t)onnx_parse_value_info, &result->inputs);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(graph->output, graph->n_output, refarrs, REF_VALINFO,
                                     (onnx_parse_elem_t)onnx_parse_value_info, &result->outputs);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(graph->value_info, graph->n_value_info, refarrs, REF_VALINFO,
                                     (onnx_parse_elem_t)onnx_parse_value_info, &result->values);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(graph->initializer, graph->n_initializer, refarrs, REF_TENSOR,
                                     (onnx_parse_elem_t)onnx_parse_tensor, &result->initializers);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(graph->node, graph->n_node, refarrs, REF_NODE,
                                     (onnx_parse_elem_t)onnx_parse_node, &result->nodes);
    return fx_status;
}

static int onnx_parse_opset(const FicusOnnx__OperatorSetIdProto* opset_import,
                            const fx_arr_t* refarrs, onnx_opset_import_t* result)
{
    int fx_status = fx_cstr2str(opset_import->domain, -1, &result->domain);
    result->version = opset_import->version;
    return fx_status;
}

static int onnx_parse_metadata(const FicusOnnx__StringStringEntryProto* metadata_entry,
                               const fx_arr_t* refarrs, onnx_metadata_entry_t* result)
{
    int fx_status = fx_cstr2str(metadata_entry->key, -1, &result->key);
    if (fx_status >= 0)
        fx_status = fx_cstr2str(metadata_entry->value, -1, &result->value);
    return fx_status;
}

static int onnx_parse_model(const fx_arr_t* arr, const fx_arr_t* refarrs,
                            onnx_model_t* result)
{
    const uint8_t* data = (const uint8_t*)arr->data;
    size_t datasize = (size_t)arr->dim[0].size;
    FicusOnnx__ModelProto* model;
    int fx_status = FX_OK;

    assert(refarrs[REF_GRAPH].dim[0].step == sizeof(onnx_graph_t*));
    assert(refarrs[REF_NODE].dim[0].step == sizeof(onnx_node_t*));
    assert(refarrs[REF_ATTR].dim[0].step == sizeof(onnx_attr_t*));
    assert(refarrs[REF_VALINFO].dim[0].step == sizeof(onnx_value_info_t));
    assert(refarrs[REF_TENSOR].dim[0].step == sizeof(onnx_tensor_t));
    assert(refarrs[REF_DIM].dim[0].step == sizeof(onnx_dim_t));
    assert(refarrs[REF_STR].dim[0].step == sizeof(fx_str_t));
    assert(refarrs[REF_FLOAT].dim[0].step == sizeof(float));
    assert(refarrs[REF_INT64].dim[0].step == sizeof(int64_t));
    assert(refarrs[REF_OPSET_IMPORT].dim[0].step == sizeof(onnx_opset_import_t));
    assert(refarrs[REF_METADATA_ENTRY].dim[0].step == sizeof(onnx_metadata_entry_t));

    if( !data || datasize == 0 || arr->ndims != 1 || arr->dim[0].step != 1 ||
        arr->copy_elem != 0 && arr->free_elem != 0 ) {
        FX_FAST_THROW_RET(FX_EXN_BadArgError);
    }

    model = ficus_onnx__model_proto__unpack(0, datasize, data);
    if(!model)
        FX_FAST_THROW_RET(FX_EXN_BadArgError);

    result->ir_version = model->ir_version;
    fx_status = fx_cstr2str(model->producer_name, -1, &result->producer);
    if (fx_status >= 0)
        fx_status = fx_cstr2str(model->domain, -1, &result->domain);
    if (fx_status >= 0)
        fx_status = fx_cstr2str(model->doc_string, -1, &result->doc_string);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(model->opset_import, model->n_opset_import, refarrs, REF_OPSET_IMPORT,
                                     (onnx_parse_elem_t)onnx_parse_opset, &result->opset_import);
    if (fx_status >= 0)
        fx_status = onnx_parse_array(model->metadata_props, model->n_metadata_props, refarrs, REF_METADATA_ENTRY,
                                     (onnx_parse_elem_t)onnx_parse_metadata, &result->metadata);
    if (fx_status >= 0)
        fx_status = onnx_parse_graph(model->graph, refarrs, &result->graph);
    ficus_onnx__model_proto__free_unpacked(model, 0);
    return fx_status;
}
}

type refarrs_t = (
    Ast.graph_t [],
    Ast.node_t [],
    Ast.tensor_t [],
    Ast.attr_t [],
    Ast.valueinfo_t [],
    Ast.dim_t [],
    string [],
    float [],
    int64 [],
    Ast.opset_t [],
    (string, string) [])

fun get_refarrs(): refarrs_t
{
    val some_attrs = [| Ast.Attr {name="size", v=Ast.AttrInt(1L)}, Ast.Attr {name="url", v=Ast.AttrString("http://a.b.c")} |]
    val some_node = Ast.Node {name="n", op="conv", inputs=[|"0"|], outputs=[|"1"|], attrs=some_attrs}
    val some_dim = Ast.DimValue(1L)
    val some_vi = Ast.valueinfo_t {name="x", denotation="", typeinfo=Ast.TYPINFO_TENSOR(Ast.DTYP_FLOAT, [|some_dim|])}
    val some_tensor = Ast.tensor_t {name="t", shape=[|1|], data=Ast.T_FLOAT([|1.f|])}
    val some_graph = Ast.Graph {name="", inputs=[], outputs=[], values=[], initializers=[], nodes=[|some_node|]}
    val some_opset = Ast.opset_t {version=1L, domain=""}

    ([|some_graph|], [|some_node|], [|some_tensor|],
    some_attrs, [|some_vi|], [|some_dim|], [|"abc"|], [|0.f|], [|0L|], [|some_opset|], [|("key", "value")|])
}

val refarrs = get_refarrs()

fun parse(data: uint8 [])
{
    fun parse_(data: uint8 [], refarrs: refarrs_t): Ast.model_t
    @ccode {
        return onnx_parse_model(data, &refarrs->t0, (onnx_model_t*)fx_result);
    }
    parse_(data, refarrs)
}

fun parse(fname: string)
{
    val data = File.read_binary_u8(fname)
    parse(data)
}
