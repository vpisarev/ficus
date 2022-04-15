/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Converts Onnx model to DL.Ast
import Ast, Onnx.Ast as OAst, Onnx.Parser
import Hashmap, Dynvec

exception OnnxConvertError: string

@private fun onnx2typ(t: OAst.tensor_t) = match t.data {
    | OAst.T_INT8 _ => Ast.DL_I8
    | OAst.T_INT32 _ => Ast.DL_I32
    | OAst.T_INT64 _ => Ast.DL_I64
    | OAst.T_FLOAT _ => Ast.DL_FP32
}

@private fun onnx2typ(dtyp: OAst.datatype_t) {
    | OAst.DTYP_UNDEFINED => Ast.DL_Undefined
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
    | OAst.DTYP_BOOL => Ast.DL_Bool
    | OAst.DTYP_DOUBLE => Ast.DL_FP64
    | OAst.DTYP_STRING | OAst.DTYP_COMPLEX64 | OAst.DTYP_COMPLEX128 =>
        throw OnnxConvertError("unsupported datatype")
}

@private fun onnx2tensor(t: OAst.tensor_t) =
Ast.dltensor_t {
    shape = Ast.dlshape_t {layout=Ast.DL_Layout_NCHW, shape=t.shape},
    data = (match t.data {
    | OAst.T_INT8(w) => Ast.DL_Data_I8(w)
    | OAst.T_INT32(w) => Ast.DL_Data_I32(w)
    | OAst.T_INT64(w) => Ast.DL_Data_I64(w)
    | OAst.T_FLOAT(w) => Ast.DL_Data_FP32(w)
    })
}

@private fun onnx2shape(dimnames: Ast.dlnames_t, ti: OAst.typeinfo_t) =
match ti {
| OAst.TYPINFO_TENSOR(dtyp, dims) =>
    val shape = [for d <- dims {
        | OAst.DimParam(name) =>
            val nelems = dimnames.size()
            val idx = dimnames.find_idx_or_insert(name)
            val v = dimnames.table[idx].data
            if v < 0 {v} else {
                val v = -nelems - 1
                dimnames.table[idx].data = v
                v
            }
        | OAst.DimValue(v) => int(v)
        }]
    (Ast.dlshape_t {
        layout = Ast.DL_Layout_Unknown,
        shape = shape
    }, onnx2typ(dtyp))
}

@private fun onnx2arg(dimnames: Ast.dlnames_t,
             vi: OAst.valueinfo_t,
             argkind: Ast.dlargkind_t)
{
    val (shape, typ) = onnx2shape(dimnames, vi.typeinfo)
    Ast.dlarg_t {
        name = vi.name,
        argkind = argkind,
        shape = shape,
        typ = typ
    }
}

@private fun attr2string(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrString(s) => s
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to string")
    }

@private fun attr2int(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrInt(i) => int(i)
    | OAst.AttrFloat(f) => int(f)
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to int")
    }

@private fun attr2float(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrInt(i) => float(i)
    | OAst.AttrFloat(f) => f
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to float")
    }

@private fun attr2ints(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrInt(i) => [int(i)]
    | OAst.AttrInts(ints) => ints
    | OAst.AttrTensor (OAst.tensor_t {shape, data}) =>
        assert(size(shape) == 1)
        OAst.tensor_data_to_ints(data)
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to 1D int array")
    }

@private fun attr2floats(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrFloat(f) => [f]
    | OAst.AttrFloats(fs) => fs
    | OAst.AttrTensor (OAst.tensor_t {shape, data}) =>
        assert(size(shape) == 1)
        OAst.tensor_data_to_floats(data)
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to 1D float array")
    }

@private val scalar_shape = Ast.dlshape_t {shape=[], layout=Ast.DL_Layout_Unknown}

@private fun attr2tensor(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrFloat(f) => Ast.dltensor_t {shape=scalar_shape, data=Ast.DL_Data_FP32([f])}
    | OAst.AttrFloats(fs) => Ast.dltensor_t {shape=scalar_shape, data=Ast.DL_Data_FP32(fs)}
    | OAst.AttrInt(i) => Ast.dltensor_t {shape=scalar_shape, data=Ast.DL_Data_I32([int32(i)])}
    | OAst.AttrInts(ints) => Ast.dltensor_t {shape=scalar_shape, data=Ast.DL_Data_I32(int32(ints))}
    | OAst.AttrTensor(t) => onnx2tensor(t)
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to tensor")
    }

@private fun attr2autopad(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrString(s) =>
        match s.toupper() {
        | "SAME_LOWER" => Ast.DL_Pad_SameLower
        | "SAME_UPPER" => Ast.DL_Pad_SameUpper
        | "VALID" => Ast.DL_Pad_Valid
        | _ => Ast.DL_Pad_None
        }
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to padding type")
    }

@private fun autopad2pads(p: Ast.dlpadding_t, kernel_shape: int [], pads0: int [])
{
    val dims = size(kernel_shape)
    [for i <- 0:dims*2 {
        val i_ = i%dims
        val delta_i = (kernel_shape[i_] + 1) % 2
        val half_i = kernel_shape[i_] / 2
        match (p, i >= dims) {
        | (Ast.DL_Pad_SameLower, false) | (Ast.DL_Pad_SameUpper, true) => half_i + delta_i
        | (Ast.DL_Pad_SameLower, _) | (Ast.DL_Pad_SameUpper, _) => half_i
        | (Ast.DL_Pad_Valid, _) => 0
        | _ => pads0[i]
        }
    }]
}

fun convert(model: OAst.model_t): Ast.dlnet_t
{
    val dimnames = Hashmap.empty(1024, "", 0)
    val argnames = Hashmap.empty(1024, "", 0)
    val empty_arg = Ast.empty_arg().{argkind = Ast.DL_Arg_Const}
    val empty_tensor = Ast.empty_tensor()

    val info = Ast.DL_Net_Onnx (Ast.dlonnx_t {
        ir_version = model.ir_version,
        producer = model.producer,
        domain = model.domain,
        doc_string = model.doc_string,
        opsets = [:: for ops <- model.import_opsets {(ops.version, ops.domain)}]
    })

    val args = [ empty_arg ]
    val vargs = Dynvec.create(args, empty_arg)
    val vtensors = Dynvec.create([ empty_tensor ], empty_tensor)
    dimnames.add("?", -1)

    fun get_const_tensor_arg(name: string, data: Ast.dldata_t) =
        match argnames.find_opt(name) {
        | Some(idx) => idx
        | _ =>
            val sz = Ast.total(data)
            val shape = Ast.dlshape_t { shape=[sz], layout=Ast.DL_Layout_Unknown }
            val arg = Ast.dlarg_t {
                name = name,
                argkind = Ast.DL_Arg_Const,
                shape = shape,
                typ = data.elemtype()
            }
            val t = Ast.dltensor_t {
                shape = shape,
                data = data
            }
            val idx = vargs.push()
            val _ = vtensors.push()
            vargs.data[idx] = arg
            vtensors.data[idx] = t
            argnames.add(name, idx)
            idx
        }
    //fun get_const_tensor_arg(i: int) = get_const_tensor_arg(f"const:{i}", Ast.DL_Data_I32([int32(i)]))
    fun get_const_tensor_arg(f: float) =
        get_const_tensor_arg(f"const:{f}", Ast.DL_Data_FP32([f]))
    fun get_const_tensor_arg(i: int) =
        get_const_tensor_arg(f"const:{i}", Ast.DL_Data_I32([int32(i)]))
    fun get_const_tensor_arg(iarr: int []) =
        get_const_tensor_arg(f"const:{Ast.arr2str(iarr)}", Ast.DL_Data_I32(int32(iarr)))

    val net = Ast.empty_net().{
        info = info,
        argnames = argnames,
        args = args
    }

    fun find_inp_arg(nspace: string, argname: string) =
        match argnames.find_opt((if nspace == "" {nspace} else {nspace + "::"}) + argname) {
        | Some(idx) => idx
        | _ =>
            if nspace == "" {
                throw OnnxConvertError(f"cannot find input '{argname}'")
            }
            val pos = nspace.rfind("::")
            find_inp_arg((if pos >= 0 {nspace[:pos]} else {""}), argname)
        }

    fun convert_graph(onnx_graph: OAst.graph_t, nspace: string, nested: bool) {
        val nspace_ = if nspace == "" {nspace} else {nspace + "::"}
        for c <- onnx_graph.initializers {
            val argidx = vargs.push()
            val _ = vtensors.push()
            val t = onnx2tensor(c)
            val arg = Ast.dlarg_t {
                name = nspace_ + c.name,
                argkind = Ast.DL_Arg_Const,
                shape = t.shape,
                typ = t.elemtype()
            }
            argnames.add(arg.name, argidx)
            vargs.data[argidx] = arg
            vtensors.data[argidx] = t
        }
        fun convert_targ_group(group: OAst.valueinfo_t [], argkind: Ast.dlargkind_t) {
            for vi@i <- group {
                val argidx = vargs.push()
                val _ = vtensors.push()
                argnames.add(nspace_ + vi.name, argidx)
                val arg = onnx2arg(dimnames, vi, argkind)
                vargs.data[argidx] = arg
                vtensors.data[argidx] = Ast.dltensor_t {
                        shape = arg.shape,
                        data=Ast.DL_Data_Empty
                    }
            }
        }
        val inputs_start = vargs.count
        convert_targ_group(onnx_graph.inputs, if nested {Ast.DL_Arg_Temp} else {Ast.DL_Arg_Input})
        val outputs_start = vargs.count
        convert_targ_group(onnx_graph.outputs, if nested {Ast.DL_Arg_Temp} else {Ast.DL_Arg_Output})
        val values_start = vargs.count
        convert_targ_group(onnx_graph.values, Ast.DL_Arg_Temp)
        val inpargs = mkrange(inputs_start, outputs_start)
        val outargs = mkrange(outputs_start, values_start)

        val fold prog = [] for node <- onnx_graph.nodes {
            //println(f"parsing operation '{node.name}' (Op={node.op}); #args = {vargs.count}")
            val name = node.name
            val inputs = [for argname <- node.inputs { find_inp_arg(nspace, argname) }]
            val outputs = [for argname <- node.outputs {
                // unlike operation inputs, we do not search in the outer scopes for the outputs.
                // instead, we create a new local value
                val argname = nspace_ + argname
                match argnames.find_opt(argname) {
                | Some(idx) => idx
                | _ =>
                    val idx = vargs.push()
                    val _ = vtensors.push()
                    val arg = Ast.dlarg_t {
                        name=argname,
                        argkind=Ast.DL_Arg_Temp, // later on we can convert it to output
                        shape=Ast.dlshape_t {
                            shape=[],
                            layout=Ast.DL_Layout_Unknown
                        },
                        typ=Ast.DL_Undefined }
                    argnames.add(argname, idx)
                    vargs.data[idx] = arg
                    val t = Ast.dltensor_t { shape=arg.shape, data=Ast.DL_Data_Empty }
                    vtensors.data[idx] = t
                    idx
                }}]
            val ninputs = size(inputs), noutputs = size(outputs)

            val rev_more_ops = match node.op {
            | "Add" | "And" | "Div" | "Equal" | "Greater" | "Less"
            | "Mod" | "Mul" | "Or" | "Sub" | "Xor" =>
                assert(`ninputs == 2`)
                assert(`noutputs == 1`)
                val el_op = match node.op {
                    | "Add" => Ast.DL_Add
                    | "And" => Ast.DL_And
                    | "Div" => Ast.DL_Div
                    | "Equal" => Ast.DL_Equal
                    | "Greater" => Ast.DL_Greater
                    | "GreaterOrEqual" => Ast.DL_GreaterOrEqual
                    | "Less" => Ast.DL_Less
                    | "LessOrEqual" => Ast.DL_LessOrEqual
                    | "Mod" => Ast.DL_Mod
                    | "Mul" => Ast.DL_Mul
                    | "Or" => Ast.DL_Or
                    | "Sub" => Ast.DL_Sub
                    | "Xor" => Ast.DL_Xor
                    | _ => throw OnnxConvertError(f"unsupported unary operation {node.op}")
                }
                [:: Ast.DL_Elemwise {name=name, el_op=el_op, t_inp=inputs, t_out=outputs[0]}]
            | "Abs" |  "Relu" | "Abs" | "Acos" | "Acosh" | "Asin" | "Asinh" | "Atan" | "Atanh"
            | "Ceil" | "Cos" | "Cosh" | "Erf" | "Exp" | "Floor" | "IsInf" | "IsNaN" | "Log"
            | "Neg" | "Not" | "Relu" | "Round" | "Sigmoid" | "Sign" | "Sin" | "Sinh"
            | "Softplus" | "Softsign" | "Sqrt" | "Tan" | "Tanh" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                val el_op = match node.op {
                    | "Abs" => Ast.DL_Abs | "Acos" => Ast.DL_Acos | "Acosh" => Ast.DL_Acosh
                    | "Asin" => Ast.DL_Asin | "Asinh" => Ast.DL_Asinh | "Atan" => Ast.DL_Atan
                    | "Atanh" => Ast.DL_Atanh | "Ceil" => Ast.DL_Ceil | "Cos" => Ast.DL_Cos
                    | "Cosh" => Ast.DL_Cosh | "Erf" => Ast.DL_Erf | "Exp" => Ast.DL_Exp
                    | "Floor" => Ast.DL_Floor | "IsInf" => Ast.DL_IsInf | "IsNaN" => Ast.DL_IsNaN
                    | "Log" => Ast.DL_Log | "Neg" => Ast.DL_Neg | "Not" => Ast.DL_Not
                    | "Relu" => Ast.DL_Relu | "Round" => Ast.DL_Round | "Sigmoid" => Ast.DL_Sigmoid
                    | "Sign" => Ast.DL_Sign | "Sin" => Ast.DL_Sin | "Sinh" => Ast.DL_Sinh
                    | "Softplus" => Ast.DL_Softplus | "Softsign" => Ast.DL_Softsign
                    | "Sqrt" => Ast.DL_Sqrt | "Tan" => Ast.DL_Tan | "Tanh" => Ast.DL_Tanh
                    | _ => throw OnnxConvertError(f"unsupported binary operation {node.op}")
                }
                [:: Ast.DL_Elemwise {name=name, el_op=el_op, t_inp=inputs, t_out=outputs[0]}]
            | "Min" | "Max" | "Mean" =>
                assert(`ninputs > 1`)
                assert(`noutputs == 1`)
                val el_op = match node.op {
                    | "Min" => Ast.DL_Min | "Max" => Ast.DL_Max | "Mean" => Ast.DL_Mean
                    | _ => throw OnnxConvertError(f"unsupported element-wise operation {node.op}")
                }
                [:: Ast.DL_Elemwise {name=name, el_op=el_op, t_inp=inputs, t_out=outputs[0]}]
            | "ReduceMin" | "ReduceMax" | "ReduceMean" | "ReduceProd" | "ReduceSum" | "ReduceSumSquare"
            | "ReduceL1" | "ReduceL2" | "ReduceLogSum" | "ReduceLogSumExp" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var axes = [], keepdims = 1
                for a <- node.attrs {
                    | {name="axes"} => axes = attr2ints(a)
                    | {name="keepdims"} => keepdims = attr2int(a)
                    | _ => {}
                }
                val reduce_op = match node.op {
                    | "ReduceMin" => Ast.DL_ReduceMin
                    | "ReduceMax" => Ast.DL_ReduceMax
                    | "ReduceMean" => Ast.DL_ReduceMean
                    | "ReduceProd" => Ast.DL_ReduceProd
                    | "ReduceSum" => Ast.DL_ReduceSum
                    | "ReduceSumSquare" => Ast.DL_ReduceSumSquare
                    | "ReduceL1" => Ast.DL_ReduceL1
                    | "ReduceL2" => Ast.DL_ReduceL2
                    | "ReduceLogSum" => Ast.DL_ReduceLogSum
                    | "ReduceLogSumExp" => Ast.DL_ReduceLogSumExp
                    | _ => throw OnnxConvertError(f"unsupported reduce operation {node.op}")
                }
                [:: Ast.DL_Reduce {name=name, reduce_op=reduce_op, axes=axes, keepdims=keepdims!=0, t_inp=inputs[0], t_out=outputs[0]}]
            | "AveragePool" | "MaxPool" =>
                val ismax = node.op == "MaxPool"
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var kernel_shape = [], strides = [], dilations = []
                var pads = [], auto_pad = Ast.DL_Pad_None, count_include_pad = 0
                var ceil_mode = 0, storage_order = 0
                for a <- node.attrs {
                    | {name="kernel_shape"} => kernel_shape = attr2ints(a)
                    | {name="pads"} => pads = attr2ints(a)
                    | {name="strides"} => strides = attr2ints(a)
                    | {name="dilations"} => dilations = attr2ints(a)
                    | {name="auto_pad"} => auto_pad = attr2autopad(a)
                    | {name="ceil_mode"} => ceil_mode = attr2int(a)
                    | {name="storage_order"} => storage_order = attr2int(a)
                    | {name="count_include_pad"} => count_include_pad = attr2int(a)
                    | _ => {}
                }
                val dims = size(kernel_shape)
                if dims == 0 {
                    throw OnnxConvertError(f"{node.name} (op={node.op}): kernel_size is not specified")
                }
                if pads == [] {pads = array(dims, 0)}
                if strides == [] {strides = array(dims, 1)}
                if dilations == [] {dilations = array(dims, 1)}
                val pads = autopad2pads(auto_pad, kernel_shape, pads)
                val storage_order = if storage_order == 0 {Ast.DL_RowMajor} else {Ast.DL_ColumnMajor}
                [:: if ismax {
                    Ast.DL_MaxPool {
                        name=name, kernel_shape=kernel_shape, pads=pads,
                        strides=strides, dilations=dilations,
                        ceil_mode = ceil_mode != 0,
                        storage_order=storage_order,
                        t_inp=inputs[0], t_out=outputs[0] }
                } else {
                    Ast.DL_AvgPool {
                        name=name, kernel_shape=kernel_shape, pads=pads,
                        strides=strides, dilations=dilations,
                        ceil_mode = ceil_mode != 0,
                        count_include_pad=count_include_pad != 0,
                        t_inp=inputs[0], t_out=outputs[0] }
                }]
            | "BatchNormalization" =>
                assert(ninputs == 5)
                assert(1 <= noutputs <= 3)
                var epsilon = 1e-5f, momentum=0.9f, training_mode=0
                for a <- node.attrs {
                    | {name="epsilon"} => epsilon = attr2float(a)
                    | {name="momentum"} => momentum = attr2float(a)
                    | {name="training_mode"} => training_mode = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_BatchNorm {
                    name=name, epsilon=epsilon,
                    momentum=momentum, training_mode=training_mode!=0,
                    t_inp=inputs[0], t_scale=inputs[1], t_B=inputs[2],
                    t_mean=inputs[3], t_var=inputs[4], t_out=outputs[0] }]
            | "Cast" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var to = 0
                for a <- node.attrs {
                    | {name="to"} => to = attr2int(a)
                    | _ => {}
                }
                val to = match to {
                    | 1 => Ast.DL_FP32 | 2 => Ast.DL_U8 | 3 => Ast.DL_I8 | 4 => Ast.DL_U16
                    | 5 => Ast.DL_I16 | 6 => Ast.DL_I32 | 7 => Ast.DL_I64 | 9 => Ast.DL_Bool | 10 => Ast.DL_FP16
                    | 11 => Ast.DL_FP64 | 12 => Ast.DL_U32 | 13 => Ast.DL_U64 | 16 => Ast.DL_BF16
                    | _ => throw OnnxConvertError(f"{node.name} (op=Cast): unknown/unsupported target type {to}")
                }
                [:: Ast.DL_Cast { name=name, to=to, t_inp=inputs[0], t_out=outputs[0] }]
            | "Clip" =>
                assert(`ninputs == 1 || ninputs == 3`)
                assert(`noutputs == 1`)
                var maxv = 3.402823e+38f, minv = -maxv
                for a <- node.attrs {
                    | {name="min"} => minv = attr2float(a)
                    | {name="max"} => maxv = attr2float(a)
                    | _ => {}
                }
                val t_min = if ninputs == 3 {inputs[1]} else {get_const_tensor_arg(minv)}
                val t_max = if ninputs == 3 {inputs[2]} else {get_const_tensor_arg(maxv)}
                [:: Ast.DL_Clip {name=name, t_inp=inputs[0], t_min=t_min, t_max=t_max, t_out=outputs[0]}]
            | "Constant" =>
                assert(`ninputs == 0`)
                assert(`noutputs == 1`)
                var t = empty_tensor, count = 0
                for a <- node.attrs {
                    | {name=("value"|"value_float"|"value_int"|"value_floats"|"value_ints")} =>
                        if count != 0 {
                            throw OnnxConvertError(f"{node.name} (op=Constant): more than one 'value_*' is specified")
                        }
                        count += 1
                        t = attr2tensor(a)
                    | _ =>
                        throw OnnxConvertError(f"{node.name} (op=Constant): unsupported attribute '{a.name}'")
                }
                if count != 1 {
                    throw OnnxConvertError(f"{node.name} (op=Constant): missing value")
                }
                vargs.data[outputs[0]] = vargs.data[outputs[0]].{
                    argkind=Ast.DL_Arg_Const, shape=t.shape,
                    typ=t.elemtype()}
                vtensors.data[outputs[0]] = t
                [] // there is no actual output operation
            | "ConstantOfShape" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var value = empty_tensor
                for a <- node.attrs {
                    | {name="value"} => value = attr2tensor(a)
                    | _ => {}
                }
                [:: Ast.DL_ConstantOfShape { name=name, value=value, t_shape=inputs[0], t_out=outputs[0] }]
            | "Concat" =>
                assert(`noutputs == 1`)
                var axis = 0
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_Concat {name=name, axis=axis, t_inp=copy(inputs), t_out=outputs[0]}]
            | "Conv" | "ConvTranspose" =>
                assert(`ninputs == 2 || ninputs == 3`)
                assert(`noutputs == 1`)
                var kernel_shape = [], strides = [], dilations = []
                var pads = [], auto_pad = Ast.DL_Pad_None, group = 1
                var out_padding = [], out_shape = []
                for a <- node.attrs {
                    | {name="kernel_shape"} => kernel_shape = attr2ints(a)
                    | {name="pads"} => pads = attr2ints(a)
                    | {name="strides"} => strides = attr2ints(a)
                    | {name="dilations"} => dilations = attr2ints(a)
                    | {name="group"} => group = attr2int(a)
                    | {name="auto_pad"} => auto_pad = attr2autopad(a)
                    | {name="out_padding"} => out_padding = attr2ints(a)
                    | {name="out_shape"} => out_shape = attr2ints(a)
                    | _ => {}
                }
                val dims = size(kernel_shape)
                if dims == 0 {
                    throw OnnxConvertError(f"{node.name} (op=Conv): missing kernel shape")
                }
                if pads == [] {pads = array(dims, 0)}
                if strides == [] {strides = array(dims, 1)}
                if dilations == [] {dilations = array(dims, 1)}
                val pads = autopad2pads(auto_pad, kernel_shape, pads)
                if node.op == "Conv" {
                    [:: Ast.DL_Conv {
                        name=name, kernel_shape=kernel_shape, pads=pads,
                        strides=strides, dilations=dilations, group = group,
                        conv_data=ref null,
                        t_inp=inputs[0], t_weights=inputs[1],
                        t_bias=(if ninputs == 3 {inputs[2]} else {0}),
                        t_out=outputs[0]}]
                } else {
                    [:: Ast.DL_ConvTranspose {
                        name=name, kernel_shape=kernel_shape, pads=pads,
                        strides=strides, dilations=dilations, group = group,
                        out_padding = out_padding, out_shape = out_shape,
                        t_inp=inputs[0], t_weights=inputs[1],
                        t_bias=(if ninputs == 3 {inputs[2]} else {0}),
                        t_out=outputs[0]}]
                }
            | "Dropout" =>
                assert(`1 <= ninputs <= 3`)
                assert(`noutputs == 1 || noutputs == 2`)
                var seed=0
                for a <- node.attrs {
                    | {name="seed"} => seed = attr2int(a)
                    | _ => {}
                }
                val t_training_mode =
                    if ninputs < 3 {
                        0
                    } else {
                        val t_training_mode = inputs[2]
                        assert(`vargs.data[t_training_mode].argkind == Ast.DL_Arg_Const`)
                        assert(`vtensors.data[t_training_mode].data.total() == 1`)
                        assert(`int(vtensors.data[t_training_mode].data)[0] == 0`)
                        t_training_mode
                    }
                [:: Ast.DL_Dropout {
                    name=name, seed=seed,
                    t_inp=inputs[0],
                    t_ratio=(if ninputs >= 2 {inputs[1]} else {get_const_tensor_arg(0.5f)}),
                    t_training_mode=t_training_mode,
                    t_out=outputs[0] }] // ignore the second output
            | "Expand" =>
                assert(`ninputs == 2`)
                assert(`noutputs == 1`)
                [:: Ast.DL_Expand { name=name, t_inp=inputs[0], t_shape=inputs[1], t_out=outputs[0] }]
            | "Flatten" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var axis=1
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_Flatten {name=name, axis=axis, t_inp=inputs[0], t_out=outputs[0]}]
            | "Gather" =>
                assert(`ninputs == 2`)
                assert(`noutputs == 1`)
                var axis=0
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_Gather {
                    name=name, axis=axis,
                    t_inp=inputs[0], t_ind=inputs[1],
                    t_out=outputs[0]}]
            | "Gemm" =>
                assert(`ninputs == 2 || ninputs == 3`)
                assert(`noutputs == 1`)
                var alpha=1.f, beta=1.f, transA=0, transB=0
                for a <- node.attrs {
                    | {name="alpha"} => alpha = attr2float(a)
                    | {name="beta"} => beta = attr2float(a)
                    | {name="transA"} => transA = attr2int(a)
                    | {name="transB"} => transB = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_Gemm {
                    name=name, alpha=alpha, beta=beta,
                    transA=transA!=0, transB=transB!=0,
                    t_A=inputs[0], t_B=inputs[1],
                    t_bias=(if ninputs == 3 {inputs[2]} else {0}),
                    t_out=outputs[0]}]
            | "GlobalAveragePool" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                [:: Ast.DL_GlobalAvgPool {name=name, t_inp=inputs[0], t_out=outputs[0]}]
            | "Identity" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                [:: Ast.DL_Identity {name=name, t_inp=inputs[0], t_out=outputs[0]}]
            | "If" =>
                assert(`ninputs == 1`)
                assert(`noutputs >= 1`)
                var then_br = Ast.empty_graph(), else_br = Ast.empty_graph()
                for a <- node.attrs {
                    | {name="then_branch", v=OAst.AttrGraph(g)} => then_br = convert_graph(g, nspace_+g.name, true)
                    | {name="else_branch", v=OAst.AttrGraph(g)} => else_br = convert_graph(g, nspace_+g.name, true)
                    | _ => {}
                }
                if then_br.prog == [] {throw OnnxConvertError("f{node.name} (op=If): 'then' branch is missing")}
                if else_br.prog == [] {throw OnnxConvertError("f{node.name} (op=If): 'else' branch is missing")}
                [:: Ast.DL_If {
                    name=name, then_branch=then_br, else_branch=else_br,
                    t_inp=inputs[0], t_out=outputs}]
            | "LeakyRelu" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var alpha = 0.01f
                for a <- node.attrs {
                    | {name="alpha"} => alpha = attr2float(a)
                    | _ => {}
                }
                [:: Ast.DL_LeakyRelu {name=name, alpha=alpha, t_inp=inputs[0], t_out=outputs[0]}]
            | "Loop" =>
                assert(`ninputs >= 2`)
                assert(`noutputs >= 1`)
                var body = Ast.empty_graph()
                for a <- node.attrs {
                    | {name="body", v=OAst.AttrGraph(g)} => body = convert_graph(g, nspace_+g.name, true)
                    | _ => {}
                }
                if body.prog == [] {throw OnnxConvertError("f{node.name} (op=Loop): 'body' graph is missing")}
                [:: Ast.DL_Loop {
                    name=name, body=body, t_trip_count=inputs[0],
                    t_cond_in=inputs[1], t_v_in=inputs[2:],
                    t_cond_out=outputs[0], t_v_out=outputs[1:]}]
            | "LRN" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var size = -1, alpha = 0.0001f, beta = 0.75f, bias = 1.0f
                for a <- node.attrs {
                    | {name="alpha"} => alpha = attr2float(a)
                    | {name="beta"} => beta = attr2float(a)
                    | {name="bias"} => bias = attr2float(a)
                    | {name="size"} => size = attr2int(a)
                    | _ => {}
                }
                assert(`size > 0`)
                [:: Ast.DL_LRN {
                    name=name, size=size, alpha=alpha,
                    beta=beta, bias=bias,
                    t_inp=inputs[0], t_out=outputs[0] }]
            | "NonMaxSuppression" =>
                assert(`2 <= ninputs <= 5`)
                assert(noutputs == 1)
                var center_point_box=0
                for a <- node.attrs {
                    | {name="center_point_box"} => center_point_box = attr2int(a)
                    | _ => {}
                }
                val t_max_output_boxes_per_class = if ninputs > 2 {inputs[2]} else {get_const_tensor_arg(0)}
                val t_iou_threshold = if ninputs > 3 {inputs[3]} else {get_const_tensor_arg(0.f)}
                val t_score_threshold = if ninputs > 3 {inputs[3]} else {get_const_tensor_arg(0.f)}
                [:: Ast.DL_NonMaxSuppression {
                    name=name,
                    center_point_box=center_point_box!=0,
                    t_boxes=inputs[0],
                    t_scores=inputs[1],
                    t_max_output_boxes_per_class=t_max_output_boxes_per_class,
                    t_iou_threshold=t_iou_threshold,
                    t_score_threshold=t_score_threshold,
                    t_out=outputs[0] }]
            | "NonZero" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                [:: Ast.DL_NonZero { name=name, t_inp=inputs[0], t_out=outputs[0] }]
            | "Range" =>
                assert(`ninputs == 3`)
                assert(`noutputs == 1`)
                [:: Ast.DL_Range { name=name, t_start=inputs[0], t_limit=inputs[1], t_delta=inputs[2], t_out=outputs[0] }]
            | "Reshape" =>
                assert(`ninputs == 1 || ninputs == 2`)
                assert(`noutputs == 1`)
                var allowzero = 0, cshape = []
                for a <- node.attrs {
                    | {name="allowzero"} => allowzero = attr2int(a)
                    | {name="shape"} => cshape = attr2ints(a)
                    | _ => {}
                }
                val t_shape = if ninputs == 2 {inputs[1]} else {get_const_tensor_arg(cshape)}
                [:: Ast.DL_Reshape {
                    name=name, allowzero=allowzero != 0,
                    t_inp=inputs[0], t_shape=t_shape, t_out=outputs[0]}]
            | "Resize" =>
                assert(`1 <= ninputs <= 4`)
                assert(`noutputs == 1`)
                var coord_trans = "half_pixel", cubic_coeff_a = -0.75f,
                    exclude_outside = 0, extrapolation_value = 0.f,
                    mode = "nearest", nearest_mode = "round_prefer_floor"
                for a <- node.attrs {
                    | {name="coordinate_transformation_mode"} => coord_trans = attr2string(a)
                    | {name="cubic_coeff_a"} => cubic_coeff_a = attr2float(a)
                    | {name="exclude_outside"} => exclude_outside = attr2int(a)
                    | {name="mode"} => mode = attr2string(a)
                    | {name="nearest_mode"} => nearest_mode = attr2string(a)
                    | _ => {}
                }
                val coord_trans = match coord_trans {
                    | "pytorch_half_pixel" => Ast.DL_CT_PyTorchHalfPixel
                    | "align_corners" => Ast.DL_CT_AlignCorners
                    | "asymmetric" => Ast.DL_CT_Asymmetric
                    | "tf_crop_and_resize" => Ast.DL_CT_TFCropResize
                    | _ => Ast.DL_CT_HalfPixel // [TODO] issue warning
                }
                val mode = match mode {
                    | "nearest" => Ast.DL_Inter_Nearest
                    | "cubic" => Ast.DL_Inter_Cubic
                    | _ => Ast.DL_Inter_Linear // [TODO] issue warning
                }
                val nearest_mode = match nearest_mode {
                    | "floor" => Ast.DL_Nearest_Floor
                    | "ceil" => Ast.DL_Nearest_Ceil
                    | "round_prefer_ceil" => Ast.DL_Nearest_RoundPreferCeil
                    | _ => Ast.DL_Nearest_RoundPreferFloor
                }
                [:: Ast.DL_Resize {
                    name=name,
                    coord_trans=coord_trans, cubic_coeff_a=cubic_coeff_a,
                    exclude_outside=exclude_outside != 0,
                    extrapolation_value=extrapolation_value,
                    mode=mode, nearest_mode=nearest_mode,
                    t_inp=inputs[0],
                    t_roi=if ninputs > 1 {inputs[1]} else {0},
                    t_scales=if ninputs > 2 {inputs[2]} else {0},
                    t_sizes=if ninputs > 3 {inputs[3]} else {0},
                    t_out=outputs[0]}]
            | "RoiAlign" =>
                assert(`ninputs == 3`)
                assert(`noutputs == 1`)
                var coord_trans = "half_pixel", mode="avg",
                    output_height=1, output_width=1, sampling_ratio=0, spatial_scale=1.f
                for a <- node.attrs {
                    | {name="coordinate_transformation_mode"} => coord_trans = attr2string(a)
                    | {name="mode"} => mode = attr2string(a)
                    | {name="output_height"} => output_height = attr2int(a)
                    | {name="output_width"} => output_width = attr2int(a)
                    | {name="sampling_ratio"} => sampling_ratio = attr2int(a)
                    | {name="spatial_scale"} => spatial_scale = attr2float(a)
                    | _ => {}
                }
                val coord_trans = match coord_trans {
                    | "output_half_pixel" => Ast.DL_CT_OutHalfPixel
                    | _ => Ast.DL_CT_HalfPixel // [TODO] issue warning
                }
                val mode = match mode {
                    | "max" => Ast.DL_Pool_Max
                    | _ => Ast.DL_Pool_Avg // [TODO] issue warning
                }
                [:: Ast.DL_RoiAlign {
                    name=name, coord_trans=coord_trans, mode=mode,
                    output_height=output_height, output_width=output_width,
                    sampling_ratio=sampling_ratio, spatial_scale=spatial_scale,
                    t_inp=inputs[0], t_rois=inputs[1], t_batch_ind=inputs[2], t_out=outputs[0]}]
            | "Scatter" =>
                assert(`ninputs == 3`)
                assert(`noutputs == 1`)
                var axis = 0
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_Scatter {name=name, axis=axis, t_data=inputs[0],
                    t_updates=inputs[1], t_indices=inputs[2], t_out=outputs[0]}]
            | "Shape" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var start = 0, end = 2147483647
                for a <- node.attrs {
                    | {name="start"} => start = attr2int(a)
                    | {name="end"} => end = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_Shape {name=name, start=start, end=end, t_inp=inputs[0], t_out=outputs[0]}]
            | "Slice" =>
                assert(`ninputs == 1 || (3 <= ninputs <= 5)`)
                assert(`noutputs == 1`)
                var starts = [], ends = [], axes = [], steps = []
                for a <- node.attrs {
                    | {name="starts"} => starts = attr2ints(a)
                    | {name="ends"} => ends = attr2ints(a)
                    | {name="axes"} => axes = attr2ints(a)
                    | {name="steps"} => steps = attr2ints(a)
                    | _ => {}
                }
                val t_starts = if ninputs >= 3 {inputs[1]} else {get_const_tensor_arg(starts)}
                val t_ends = if ninputs >= 3 {inputs[2]} else {get_const_tensor_arg(ends)}
                val t_axes = if ninputs >= 4 {inputs[3]} else if axes != [] {get_const_tensor_arg(axes)} else {0}
                val t_steps = if ninputs >= 5 {inputs[4]} else if steps != [] {get_const_tensor_arg(steps)} else {0}
                [:: Ast.DL_Slice {name=name, t_inp=inputs[0], t_starts=t_starts, t_ends=t_ends,
                    t_axes=t_axes, t_steps=t_steps, t_out=outputs[0]}]
            | "Softmax" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var axis = -1
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_SoftMax {name=name, axis=axis, t_inp=inputs[0], t_out=outputs[0]}]
            | "Split" =>
                assert(`ninputs == 1 || ninputs == 2`)
                var axis = 0, split = []
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | {name="split"} =>
                        split = attr2ints(a)
                    | _ => {}
                }
                val t_split = if ninputs == 2 {inputs[1]} else if split != [] {get_const_tensor_arg(split)} else {0}
                [:: Ast.DL_Split {
                    name=name, axis=axis, t_inp=inputs[0], t_split=t_split, t_out=outputs }]
            | "Squeeze" =>
                assert(`ninputs == 1 || ninputs == 2`)
                assert(`noutputs == 1`)
                var axes = []
                for a <- node.attrs {
                    | {name="axes"} => axes = attr2ints(a)
                    | _ => {}
                }
                val t_axes = if ninputs == 2 {inputs[1]} else {get_const_tensor_arg(axes)}
                if ninputs == 1 && axes == [] {
                    throw OnnxConvertError(f"{node.name} (op=Squeeze): 'axes' is missing")
                }
                [:: Ast.DL_Squeeze {
                    name=name, t_inp=inputs[0], t_axes=t_axes, t_out=outputs[0] }]
            | "Tile" =>
                assert(`ninputs == 2`)
                assert(`noutputs == 1`)
                [:: Ast.DL_Tile {name=name, t_inp=inputs[0], t_repeats=inputs[1], t_out=outputs[0]}]
            | "TopK" =>
                assert(`ninputs == 2`)
                assert(`noutputs == 2`)
                var axis = -1, largest = 1, sorted = 1
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | {name="largest"} => largest = attr2int(a)
                    | {name="sorted"} => sorted = attr2int(a)
                    | _ => {}
                }
                [:: Ast.DL_TopK {
                    name=name, axis=axis, largest=largest!=0, sorted=sorted!=0,
                    t_inp=inputs[0], t_K=inputs[1], t_out=outputs[0], t_out_ind=outputs[1]}]
            | "Transpose" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var perm = []
                for a <- node.attrs {
                    | {name="perm"} => perm = attr2ints(a)
                    | _ => {}
                }
                [:: Ast.DL_Transpose {name=name, perm=perm, t_inp=inputs[0], t_out=outputs[0]}]
            | "Unsqueeze" =>
                assert(`ninputs == 1 || ninputs == 2`)
                assert(`noutputs == 1`)
                var axes = []
                for a <- node.attrs {
                    | {name="axes"} => axes = attr2ints(a)
                    | _ => {}
                }
                val t_axes = if ninputs == 2 {inputs[1]} else {get_const_tensor_arg(axes)}
                if ninputs == 1 && axes == [] {
                    throw OnnxConvertError(f"{node.name} (op=Unsqueeze): 'axes' is missing")
                }
                [:: Ast.DL_Unsqueeze {
                    name=name, t_inp=inputs[0], t_axes=t_axes, t_out=outputs[0] }]
            | _ => throw OnnxConvertError(f"unsupported operation '{node.op}'")
            }
            rev_more_ops + prog
        }
        Ast.DL_Graph {
            inpargs = inpargs,
            outargs = outargs,
            prog = array(prog.rev()),
        }
    }

    val graph = convert_graph(model.graph, "", false)
    val ndimnames = dimnames.size()
    val dimnames_ = array(ndimnames, "")
    dimnames.app(fun (name, v) {dimnames_[-v-1] = name})
    net.{
        graph = graph,
        argnames = argnames,
        dimnames = dimnames,
        dimnames_ = dimnames_,
        args = vargs.data[:vargs.count],
        tensors = vtensors.data[:vtensors.count],
        bufidxs = array(vargs.count, -1)
    }
}

always_use(OnnxConvertError("abc"))
fun read(fname: string) = convert(Onnx.Parser.parse(fname))
