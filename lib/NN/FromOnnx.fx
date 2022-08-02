/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Converts Onnx onnx_model to NN.Ast
import Ast, Onnx.Ast as OAst, Onnx.Parser
import Hashmap, Hashset, Dynvec

exception OnnxConvertError: string

@private fun onnx2typ(t: OAst.tensor_t) = match t.data {
    | OAst.T_INT8 _ => Type_I8
    | OAst.T_INT32 _ => Type_I32
    | OAst.T_INT64 _ => Type_I64
    | OAst.T_FLOAT _ => Type_F32
    | OAst.T_BOOL _ => Type_Bool
}

@private fun onnx2typ(dtyp: OAst.datatype_t) {
    | OAst.DTYP_UNDEFINED => Notype
    | OAst.DTYP_FLOAT => Type_F32
    | OAst.DTYP_UINT8 => Type_U8
    | OAst.DTYP_INT8 => Type_I8
    | OAst.DTYP_UINT16 => Type_U16
    | OAst.DTYP_INT16 => Type_I16
    | OAst.DTYP_INT32 => Type_I32
    | OAst.DTYP_UINT32 => Type_U32
    | OAst.DTYP_INT64 => Type_I64
    | OAst.DTYP_UINT64 => Type_U64
    | OAst.DTYP_FLOAT16 => Type_F16
    | OAst.DTYP_BFLOAT16 => Type_BF16
    | OAst.DTYP_BOOL => Type_Bool
    | OAst.DTYP_DOUBLE => Type_F64
    | OAst.DTYP_STRING | OAst.DTYP_COMPLEX64 | OAst.DTYP_COMPLEX128 =>
        throw OnnxConvertError("unsupported datatype")
}

@private fun onnx2tensor(t: OAst.tensor_t) =
Ast.nntensor_t {
    shape = Ast.nnshape_t {layout=Ast.NN_Layout_Unknown, shape=t.shape},
    data = (match t.data {
    | OAst.T_INT8(w) => Ast.NN_Data_I8(w)
    | OAst.T_INT32(w) => Ast.NN_Data_I32(w)
    | OAst.T_INT64(w) => Ast.NN_Data_I64(w)
    | OAst.T_FLOAT(w) => Ast.NN_Data_FP32(w)
    | OAst.T_BOOL(w) => Ast.NN_Data_Bool(w)
    | _ => throw Ast.NNError(f"unsupported tensor type {OAst.tensor_data_prefix(t.data)}")
    })
}

@private fun onnx2shape(dimnames: Ast.nnnames_t, ti: OAst.typeinfo_t) =
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
    (Ast.nnshape_t {
        layout = Ast.NN_Layout_Unknown,
        shape = shape
    }, onnx2typ(dtyp))
}

@private fun onnx2arg(dimnames: Ast.nnnames_t,
             vi: OAst.valueinfo_t,
             argkind: Ast.nnargkind_t)
{
    val (shape, typ) = onnx2shape(dimnames, vi.typeinfo)
    Ast.nnarg_t {
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
    | OAst.AttrInts(ints) => int(ints)
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

@private val scalar_shape = Ast.nnshape_t {shape=[], layout=Ast.NN_Layout_Unknown}

@private fun attr2tensor(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrFloat(f) => Ast.nntensor_t {shape=scalar_shape, data=Ast.NN_Data_FP32([f])}
    | OAst.AttrFloats(fs) => Ast.nntensor_t {shape=scalar_shape, data=Ast.NN_Data_FP32(fs)}
    | OAst.AttrInt(i) => Ast.nntensor_t {shape=scalar_shape, data=Ast.NN_Data_I64([i])}
    | OAst.AttrInts(ints) => Ast.nntensor_t {shape=scalar_shape, data=Ast.NN_Data_I64(ints)}
    | OAst.AttrTensor(t) => onnx2tensor(t)
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to tensor")
    }

@private fun attr2autopad(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrString(s) =>
        match s.toupper() {
        | "SAME_LOWER" => Ast.NN_Pad_SameLower
        | "SAME_UPPER" => Ast.NN_Pad_SameUpper
        | "VALID" => Ast.NN_Pad_Valid
        | _ => Ast.NN_Pad_None
        }
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to padding type")
    }

@private fun autopad2pads(p: Ast.nnpadding_t, kernel_shape: int [], pads0: int [])
{
    val dims = size(kernel_shape)
    [for i <- 0:dims*2 {
        val i_ = i%dims
        val delta_i = (kernel_shape[i_] + 1) % 2
        val half_i = kernel_shape[i_] / 2
        //println(f"i={i}. i_={i_}, p={p}, kernel_shape={kernel_shape}, pads0={pads0}")
        match (p, i >= dims) {
        | (Ast.NN_Pad_SameLower, false) | (Ast.NN_Pad_SameUpper, true) => half_i
        | (Ast.NN_Pad_SameLower, _) | (Ast.NN_Pad_SameUpper, _) => half_i - delta_i
        | (Ast.NN_Pad_Valid, _) => 0
        | _ => pads0[i]
        }
    }]
}

fun convert(onnx_model: OAst.model_t): Ast.nnmodel_t
{
    val unsupported_ops = Hashset.empty(1024, "")
    val dimnames = Hashmap.empty(1024, "", 0)
    val argnames = Hashmap.empty(1024, "", 0)
    val empty_arg = Ast.empty_arg().{argkind = Ast.NN_Arg_Const}
    val empty_tensor = Ast.empty_tensor()

    val info = Ast.NN_Net_Onnx (Ast.nnonnx_t {
        ir_version = onnx_model.ir_version,
        producer = onnx_model.producer,
        domain = onnx_model.domain,
        doc_string = onnx_model.doc_string,
        opsets = [:: for ops <- onnx_model.import_opsets {(ops.version, ops.domain)}]
    })

    val args = [ empty_arg ]
    val vargs = Dynvec.create(args, empty_arg)
    val vtensors = Dynvec.create([ empty_tensor ], empty_tensor)
    dimnames.add("?", -1)

    fun get_const_tensor_arg(name: string, data: Ast.nndata_t) =
        match argnames.find_opt(name) {
        | Some(idx) => idx
        | _ =>
            val sz = Ast.total(data)
            val shape = Ast.nnshape_t { shape=[sz], layout=Ast.NN_Layout_Unknown }
            val arg = Ast.nnarg_t {
                name = name,
                argkind = Ast.NN_Arg_Const,
                shape = shape,
                typ = data.elemtype()
            }
            val t = Ast.nntensor_t {
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
    //fun get_const_tensor_arg(i: int) = get_const_tensor_arg(f"const:{i}", Ast.NN_Data_I32([int32(i)]))
    fun get_const_tensor_arg(f: float) =
        get_const_tensor_arg(f"const:{f}", Ast.NN_Data_FP32([f]))
    fun get_const_tensor_arg(i: int) =
        get_const_tensor_arg(f"const:{i}", Ast.NN_Data_I32([int32(i)]))
    fun get_const_tensor_arg(iarr: int []) =
        get_const_tensor_arg(f"const:{Ast.arr2str(iarr)}", Ast.NN_Data_I32(int32(iarr)))

    val model = Ast.empty_net().{
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
            val arg = Ast.nnarg_t {
                name = nspace_ + c.name,
                argkind = Ast.NN_Arg_Const,
                shape = t.shape,
                typ = t.elemtype()
            }
            argnames.add(arg.name, argidx)
            vargs.data[argidx] = arg
            vtensors.data[argidx] = t
        }
        fun convert_targ_group(group: OAst.valueinfo_t [], argkind: Ast.nnargkind_t) {
            for vi@i <- group {
                if vi.name == "" || !argnames.mem(vi.name) {
                    val argidx = vargs.push()
                    val _ = vtensors.push()
                    argnames.add(nspace_ + vi.name, argidx)
                    val arg = onnx2arg(dimnames, vi, argkind)
                    vargs.data[argidx] = arg
                    vtensors.data[argidx] = Ast.nntensor_t {
                            shape = arg.shape,
                            data=Ast.NN_Data_Empty
                        }
                }
            }
        }
        val inputs_start = vargs.count
        convert_targ_group(onnx_graph.inputs, if nested {Ast.NN_Arg_Temp} else {Ast.NN_Arg_Input})
        val outputs_start = vargs.count
        convert_targ_group(onnx_graph.outputs, if nested {Ast.NN_Arg_Temp} else {Ast.NN_Arg_Output})
        val values_start = vargs.count
        convert_targ_group(onnx_graph.values, Ast.NN_Arg_Temp)
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
                | Some(idx) =>
                    //println(f"found '{argname}' as {idx}")
                    idx
                | _ =>
                    val idx = vargs.push()
                    val _ = vtensors.push()
                    //println(f"did not found '{argname}'; adding arg #{idx}")
                    val arg = Ast.nnarg_t {
                        name=argname,
                        argkind=Ast.NN_Arg_Temp, // later on we can convert it to output
                        shape=Ast.nnshape_t {
                            shape=[],
                            layout=Ast.NN_Layout_Unknown
                        },
                        typ=Notype }
                    argnames.add(argname, idx)
                    vargs.data[idx] = arg
                    val t = Ast.nntensor_t { shape=arg.shape, data=Ast.NN_Data_Empty }
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
                    | "Add" => Ast.NN_Add
                    | "And" => Ast.NN_And
                    | "Div" => Ast.NN_Div
                    | "Equal" => Ast.NN_Equal
                    | "Greater" => Ast.NN_Greater
                    | "GreaterOrEqual" => Ast.NN_GreaterOrEqual
                    | "Less" => Ast.NN_Less
                    | "LessOrEqual" => Ast.NN_LessOrEqual
                    | "Mod" => Ast.NN_Mod
                    | "Mul" => Ast.NN_Mul
                    | "Or" => Ast.NN_Or
                    | "Sub" => Ast.NN_Sub
                    | "Xor" => Ast.NN_Xor
                    | _ => throw OnnxConvertError(f"unsupported unary operation {node.op}")
                }
                [:: Ast.NN_Elemwise {name=name, el_op=el_op, t_inp=inputs, t_out=outputs[0]}]
            | "Abs" |  "Relu" | "Abs" | "Acos" | "Acosh" | "Asin" | "Asinh" | "Atan" | "Atanh"
            | "Ceil" | "Cos" | "Cosh" | "Erf" | "Exp" | "Floor" | "IsInf" | "IsNaN" | "Log"
            | "Neg" | "Not" | "Relu" | "Round" | "Sigmoid" | "Sign" | "Sin" | "Sinh"
            | "Softplus" | "Softsign" | "Sqrt" | "Tan" | "Tanh" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                val el_op = match node.op {
                    | "Abs" => Ast.NN_Abs | "Acos" => Ast.NN_Acos | "Acosh" => Ast.NN_Acosh
                    | "Asin" => Ast.NN_Asin | "Asinh" => Ast.NN_Asinh | "Atan" => Ast.NN_Atan
                    | "Atanh" => Ast.NN_Atanh | "Ceil" => Ast.NN_Ceil | "Cos" => Ast.NN_Cos
                    | "Cosh" => Ast.NN_Cosh | "Erf" => Ast.NN_Erf | "Exp" => Ast.NN_Exp
                    | "Floor" => Ast.NN_Floor | "IsInf" => Ast.NN_IsInf | "IsNaN" => Ast.NN_IsNaN
                    | "Log" => Ast.NN_Log | "Neg" => Ast.NN_Neg | "Not" => Ast.NN_Not
                    | "Relu" => Ast.NN_Relu | "Round" => Ast.NN_Round | "Sigmoid" => Ast.NN_Sigmoid
                    | "Sign" => Ast.NN_Sign | "Sin" => Ast.NN_Sin | "Sinh" => Ast.NN_Sinh
                    | "Softplus" => Ast.NN_Softplus | "Softsign" => Ast.NN_Softsign
                    | "Sqrt" => Ast.NN_Sqrt | "Tan" => Ast.NN_Tan | "Tanh" => Ast.NN_Tanh
                    | _ => throw OnnxConvertError(f"unsupported binary operation {node.op}")
                }
                [:: Ast.NN_Elemwise {name=name, el_op=el_op, t_inp=inputs, t_out=outputs[0]}]
            | "Min" | "Max" | "Mean" =>
                assert(`ninputs > 1`)
                assert(`noutputs == 1`)
                val el_op = match node.op {
                    | "Min" => Ast.NN_Min | "Max" => Ast.NN_Max | "Mean" => Ast.NN_Mean
                    | _ => throw OnnxConvertError(f"unsupported element-wise operation {node.op}")
                }
                [:: Ast.NN_Elemwise {name=name, el_op=el_op, t_inp=inputs, t_out=outputs[0]}]
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
                    | "ReduceMin" => Ast.NN_ReduceMin
                    | "ReduceMax" => Ast.NN_ReduceMax
                    | "ReduceMean" => Ast.NN_ReduceMean
                    | "ReduceProd" => Ast.NN_ReduceProd
                    | "ReduceSum" => Ast.NN_ReduceSum
                    | "ReduceSumSquare" => Ast.NN_ReduceSumSquare
                    | "ReduceL1" => Ast.NN_ReduceL1
                    | "ReduceL2" => Ast.NN_ReduceL2
                    | "ReduceLogSum" => Ast.NN_ReduceLogSum
                    | "ReduceLogSumExp" => Ast.NN_ReduceLogSumExp
                    | _ => throw OnnxConvertError(f"unsupported reduce operation {node.op}")
                }
                [:: Ast.NN_Reduce {name=name, reduce_op=reduce_op, axes=axes, keepdims=keepdims!=0, t_inp=inputs[0], t_out=outputs[0]}]
            | "AveragePool" | "MaxPool" =>
                val ismax = node.op == "MaxPool"
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var kernel_shape = [], strides = [], dilations = []
                var pads = [], auto_pad = Ast.NN_Pad_None, count_include_pad = 0
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
                if pads == [] {pads = array(dims*2, 0)}
                if strides == [] {strides = array(dims, 1)}
                if dilations == [] {dilations = array(dims, 1)}
                val pads = autopad2pads(auto_pad, kernel_shape, pads)
                val storage_order = if storage_order == 0 {Ast.NN_RowMajor} else {Ast.NN_ColumnMajor}
                assert(`strides.size() == dims && dilations.size() == dims && pads.size() == dims*2`)
                [:: if ismax {
                    Ast.NN_MaxPool {
                        name=name, kernel_shape=kernel_shape, pads=pads,
                        strides=strides, dilations=dilations,
                        ceil_mode = ceil_mode != 0,
                        storage_order=storage_order,
                        t_inp=inputs[0], t_out=outputs[0] }
                } else {
                    Ast.NN_AvgPool {
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
                [:: Ast.NN_BatchNorm {
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
                    | 1 => Type_F32 | 2 => Type_U8 | 3 => Type_I8 | 4 => Type_U16
                    | 5 => Type_I16 | 6 => Type_I32 | 7 => Type_I64 | 9 => Type_Bool | 10 => Type_F16
                    | 11 => Type_F64 | 12 => Type_U32 | 13 => Type_U64 | 16 => Type_BF16
                    | _ => throw OnnxConvertError(f"{node.name} (op=Cast): unknown/unsupported target type {to}")
                }
                [:: Ast.NN_Cast { name=name, to=to, t_inp=inputs[0], t_out=outputs[0] }]
            | "Clip" =>
                assert(`ninputs == 1 || ninputs == 3`)
                assert(`noutputs == 1`)
                var maxv = 3.402823e+38f, minv = -maxv
                for a <- node.attrs {
                    | {name="min"} => minv = attr2float(a)
                    | {name="max"} => maxv = attr2float(a)
                    | _ => {}
                }
                val t_min = if ninputs > 1 {inputs[1]} else {get_const_tensor_arg(minv)}
                val t_max = if ninputs == 3 {inputs[2]} else {get_const_tensor_arg(maxv)}
                [:: Ast.NN_Clip {name=name, t_inp=inputs[0], t_min=t_min, t_max=t_max, t_out=outputs[0]}]
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
                    argkind=Ast.NN_Arg_Const, shape=t.shape,
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
                [:: Ast.NN_ConstantOfShape { name=name, value=value, t_shape=inputs[0], t_out=outputs[0] }]
            | "Concat" =>
                assert(`noutputs == 1`)
                var axis = 0
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.NN_Concat {name=name, axis=axis, t_inp=copy(inputs), t_out=outputs[0]}]
            | "Conv" | "ConvTranspose" =>
                assert(`ninputs == 2 || ninputs == 3`)
                assert(`noutputs == 1`)
                var kernel_shape = [], strides = [], dilations = []
                var pads = [], auto_pad = Ast.NN_Pad_None, group = 1
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
                    [:: Ast.NN_Conv {
                        name=name,
                        attr=Ast.nnconv_attr_t {
                            kernel_shape=kernel_shape, pads=pads,
                            strides=strides, dilations=dilations, group = group },
                        conv_data=ref null,
                        fused_batch_norm=None,
                        non_const_batch_norm=false,
                        fused_activ=None,
                        non_const_activ=false,
                        t_inp=inputs[0], t_weights=inputs[1],
                        t_bias=(if ninputs == 3 {inputs[2]} else {0}),
                        t_out=outputs[0], t_passby=0}]
                } else {
                    [:: Ast.NN_ConvTranspose {
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
                        assert(`vargs.data[t_training_mode].argkind == Ast.NN_Arg_Const`)
                        assert(`vtensors.data[t_training_mode].data.total() == 1`)
                        t_training_mode
                    }
                [:: Ast.NN_Dropout {
                    name=name, seed=seed,
                    t_inp=inputs[0],
                    t_ratio=(if ninputs >= 2 {inputs[1]} else {get_const_tensor_arg(0.5f)}),
                    t_training_mode=t_training_mode,
                    t_out=outputs[0] }] // ignore the second output
            | "Expand" =>
                assert(`ninputs == 2`)
                assert(`noutputs == 1`)
                [:: Ast.NN_Expand { name=name, t_inp=inputs[0], t_shape=inputs[1], t_out=outputs[0] }]
            | "Flatten" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var axis=1
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.NN_Flatten {name=name, axis=axis, t_inp=inputs[0], t_out=outputs[0]}]
            | "Gather" =>
                assert(`ninputs == 2`)
                assert(`noutputs == 1`)
                var axis=0
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.NN_Gather {
                    name=name, axis=axis,
                    t_inp=inputs[0], t_ind=inputs[1],
                    t_out=outputs[0]}]
            | "Gemm" | "MatMul" =>
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
                if node.op == "MatMul" {
                    assert(`ninputs == 2 && alpha == 1.f && transA == 0 && transB == 0`)
                }
                [:: Ast.NN_Gemm {
                    name=name, alpha=alpha, beta=beta,
                    transA=transA!=0, transB=transB!=0,
                    t_A=inputs[0], t_B=inputs[1],
                    t_bias=(if ninputs == 3 {inputs[2]} else {0}),
                    t_out=outputs[0]}]
            | "GlobalAveragePool" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                [:: Ast.NN_GlobalAvgPool {name=name, t_inp=inputs[0], t_out=outputs[0]}]
            | "Identity" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                [:: Ast.NN_Identity {name=name, t_inp=inputs[0], t_out=outputs[0]}]
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
                [:: Ast.NN_If {
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
                [:: Ast.NN_LeakyRelu {name=name, alpha=alpha, t_inp=inputs[0], t_out=outputs[0]}]
            | "Loop" =>
                assert(`ninputs >= 2`)
                assert(`noutputs >= 1`)
                var body = Ast.empty_graph()
                for a <- node.attrs {
                    | {name="body", v=OAst.AttrGraph(g)} => body = convert_graph(g, nspace_+g.name, true)
                    | _ => {}
                }
                if body.prog == [] {throw OnnxConvertError("f{node.name} (op=Loop): 'body' graph is missing")}
                [:: Ast.NN_Loop {
                    name=name, body=body, t_trip_count=inputs[0],
                    t_cond_in=inputs[1], t_v_in=inputs[2:],
                    t_v_out=outputs}]
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
                [:: Ast.NN_LRN {
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
                [:: Ast.NN_NonMaxSuppression {
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
                [:: Ast.NN_NonZero { name=name, t_inp=inputs[0], t_out=outputs[0] }]
            | "Range" =>
                assert(`ninputs == 3`)
                assert(`noutputs == 1`)
                [:: Ast.NN_Range { name=name, t_start=inputs[0], t_limit=inputs[1], t_delta=inputs[2], t_out=outputs[0] }]
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
                [:: Ast.NN_Reshape {
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
                var have_roi = 0
                val coord_trans = match coord_trans {
                    | "pytorch_half_pixel" => Ast.NN_CT_PyTorchHalfPixel
                    | "align_corners" => Ast.NN_CT_AlignCorners
                    | "asymmetric" => Ast.NN_CT_Asymmetric
                    | "tf_crop_and_resize" =>
                        have_roi = if ninputs > 1 {1} else {0}
                        Ast.NN_CT_TFCropResize
                    | _ => Ast.NN_CT_HalfPixel // [TODO] issue warning
                }
                val mode = match mode {
                    | "nearest" => Ast.NN_Inter_Nearest
                    | "cubic" => Ast.NN_Inter_Cubic
                    | _ => Ast.NN_Inter_Linear // [TODO] issue warning
                }
                val nearest_mode = match nearest_mode {
                    | "floor" => Ast.NN_Nearest_Floor
                    | "ceil" => Ast.NN_Nearest_Ceil
                    | "round_prefer_ceil" => Ast.NN_Nearest_RoundPreferCeil
                    | _ => Ast.NN_Nearest_RoundPreferFloor
                }
                //println(f"parsing Resize: inputs={inputs}")
                [:: Ast.NN_Resize {
                    name=name,
                    coord_trans=coord_trans, cubic_coeff_a=cubic_coeff_a,
                    exclude_outside=exclude_outside != 0,
                    extrapolation_value=extrapolation_value,
                    mode=mode, nearest_mode=nearest_mode,
                    t_inp=inputs[0],
                    t_roi=if have_roi > 0 {inputs[1]} else {0},
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
                    | "output_half_pixel" => Ast.NN_CT_OutHalfPixel
                    | _ => Ast.NN_CT_HalfPixel // [TODO] issue warning
                }
                val mode = match mode {
                    | "max" => Ast.NN_Pool_Max
                    | _ => Ast.NN_Pool_Avg // [TODO] issue warning
                }
                [:: Ast.NN_RoiAlign {
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
                [:: Ast.NN_Scatter {name=name, axis=axis, t_data=inputs[0],
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
                [:: Ast.NN_Shape {name=name, start=start, end=end, t_inp=inputs[0], t_out=outputs[0]}]
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
                [:: Ast.NN_Slice {name=name, t_inp=inputs[0], t_starts=t_starts, t_ends=t_ends,
                    t_axes=t_axes, t_steps=t_steps, t_out=outputs[0]}]
            | "Softmax" =>
                assert(`ninputs == 1`)
                assert(`noutputs == 1`)
                var axis = -1
                for a <- node.attrs {
                    | {name="axis"} => axis = attr2int(a)
                    | _ => {}
                }
                [:: Ast.NN_SoftMax {name=name, axis=axis, t_inp=inputs[0], t_out=outputs[0]}]
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
                [:: Ast.NN_Split {
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
                [:: Ast.NN_Squeeze {
                    name=name, t_inp=inputs[0], t_axes=t_axes, t_out=outputs[0] }]
            | "Tile" =>
                assert(`ninputs == 2`)
                assert(`noutputs == 1`)
                [:: Ast.NN_Tile {name=name, t_inp=inputs[0], t_repeats=inputs[1], t_out=outputs[0]}]
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
                [:: Ast.NN_TopK {
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
                [:: Ast.NN_Transpose {name=name, perm=perm, t_inp=inputs[0], t_out=outputs[0]}]
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
                [:: Ast.NN_Unsqueeze {
                    name=name, t_inp=inputs[0], t_axes=t_axes, t_out=outputs[0] }]
            | _ =>
                unsupported_ops.add(node.op)
                []
            }
            rev_more_ops + prog
        }
        Ast.NN_Graph {
            name = onnx_graph.name,
            inpargs = inpargs,
            outargs = outargs,
            prog = array(prog.rev()),
        }
    }

    val graph = convert_graph(onnx_model.graph, "", false)
    val ndimnames = dimnames.size()
    val dimnames_ = array(ndimnames, "")
    dimnames.app(fun (name, v) {dimnames_[-v-1] = name})

    if !unsupported_ops.empty() {
        val ops = unsupported_ops.array()
        sort(ops, (<))
        val msg = if ops.size() == 1 {f"unsupported op: {ops[0]}"}
                  else {join_embrace("unsupported op's: ", "", ", ", ops)}
        throw OnnxConvertError(msg)
    }

    model.{
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
