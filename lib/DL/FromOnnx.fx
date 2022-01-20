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
    | OAst.DTYP_BOOL => Ast.DL_I8
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
    val shape = [| for d <- dims {
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
        } |]
    (Ast.dlshape_t {
        layout = Ast.DL_Layout_Unknown,
        shape = shape
    }, onnx2typ(dtyp))
}

@private fun onnx2arg(dimnames: Ast.dlnames_t,
             vi: OAst.valueinfo_t,
             argkind: Ast.dlargkind_t, idx: int)
{
    val (shape, typ) = onnx2shape(dimnames, vi.typeinfo)
    Ast.dlarg_t {
        name = vi.name,
        argkind = argkind,
        shape = shape,
        typ = typ,
        idx = idx
    }
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
    | OAst.AttrInt(i) => [|int(i)|]
    | OAst.AttrInts(ints) => ints
    | OAst.AttrTensor (OAst.tensor_t {shape, data}) =>
        assert(size(shape) == 1)
        OAst.tensor_data_to_ints(data)
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to 1D int array")
    }

@private fun attr2floats(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrFloat(f) => [|f|]
    | OAst.AttrFloats(fs) => fs
    | OAst.AttrTensor (OAst.tensor_t {shape, data}) =>
        assert(size(shape) == 1)
        OAst.tensor_data_to_floats(data)
    | _ => throw OnnxConvertError(f"error when converting attribute {a.name} to 1D float array")
    }

@private val scalar_shape = Ast.dlshape_t {shape=[], layout=Ast.DL_Layout_Unknown}

@private fun attr2tensor(a: OAst.attr_t) =
    match a.v {
    | OAst.AttrFloat(f) => Ast.dltensor_t {shape=scalar_shape, data=Ast.DL_Data_FP32([|f|])}
    | OAst.AttrFloats(fs) => Ast.dltensor_t {shape=scalar_shape, data=Ast.DL_Data_FP32(fs)}
    | OAst.AttrInt(i) => Ast.dltensor_t {shape=scalar_shape, data=Ast.DL_Data_I32([|int32(i)|])}
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

@private fun autopad2pads2d(p: Ast.dlpadding_t, kernel_shape: int [], pads0: int [])
{
    val delta_0 = (kernel_shape[0] + 1) % 2
    val delta_1 = (kernel_shape[1] + 1) % 2
    val half_0 = kernel_shape[0]/2
    val half_1 = kernel_shape[1]/2
    match p {
        | Ast.DL_Pad_SameLower =>
            (half_0 + delta_0, half_1 + delta_1, half_0, half_1)
        | Ast.DL_Pad_SameUpper =>
            (half_0, half_1, half_0 + delta_0, half_1 + delta_1)
        | Ast.DL_Pad_Valid => (0, 0, 0, 0)
        | _ => (pads0[0], pads0[1], pads0[2], pads0[3])
    }
}

fun convert(model: OAst.model_t): Ast.dlnet_t
{
    val empty_name = ""
    val dimnames = Hashmap.empty(1024, "", 0)
    val argnames = Hashmap.empty(1024, "", 0)
    val empty_shape = Ast.dlshape_t {shape=[], layout=Ast.DL_Layout_Unknown}
    val empty_arg = Ast.dlarg_t {
        name = empty_name,
        argkind = Ast.DL_Arg_Const,
        shape = empty_shape,
        typ = Ast.DL_Undefined,
        idx = 0
    }
    val empty_tensor = Ast.dltensor_t {
        shape = empty_shape,
        data = Ast.DL_Data_Empty
    }

    val info = Ast.DL_Net_Onnx (Ast.dlonnx_t {
        ir_version = model.ir_version,
        producer = model.producer,
        domain = model.domain,
        doc_string = model.doc_string,
        opsets = [for ops <- model.import_opsets {(ops.version, ops.domain)}]
    })

    val (cargs, consts) = [|
        @unzip for c@i <- model.graph.initializers {
            argnames.add(c.name, i+1)
            val t = onnx2tensor(c)
            val arg = Ast.dlarg_t {
                name = c.name,
                argkind = Ast.DL_Arg_Const,
                shape = t.shape,
                typ = Ast.gettype(t.data),
                idx = i+1
            }
            (arg, t)
        } |]

    var argidx0 = size(cargs)+1

    val arg_groups = [| for group@k <- (model.graph.inputs, model.graph.outputs, model.graph.values) {
        val arg_group = [| for vi@i <- group {
            // constants have been already converted;
            // "states" are not recognized yet; probably, it can only be done in a separate compile step
            val (argkind, idx) = if k == 1 {(Ast.DL_Arg_Output, i)} else {(Ast.DL_Arg_Buffer, -1)}
            argnames.add(vi.name, argidx0 + i)
            onnx2arg(dimnames, vi, argkind, idx)
        } |]
        val saved_argidx0 = argidx0
        argidx0 += size(arg_group)
        (arg_group, saved_argidx0)
    } |]

    val args = [| empty_arg, \cargs, \arg_groups[0].0, \arg_groups[1].0, \arg_groups[2].0 |]
    val inpargs = mkrange(arg_groups[0].1, arg_groups[1].1)
    val outargs = mkrange(arg_groups[1].1, arg_groups[2].1)
    val ndimnames = dimnames.size()
    val dimnames_ = array(ndimnames, "")
    val vargs = Dynvec.create(args, empty_arg)
    val consts = [| empty_tensor, \consts |]
    val vconsts = Dynvec.create(consts, empty_tensor)
    dimnames.app(fun (name, v) {dimnames_[-v-1] = name})

    fun get_const_tensor_arg(name: string, data: Ast.dldata_t) =
        match argnames.find_opt(name) {
        | Some(idx) => idx
        | _ =>
            val sz = Ast.total(data)
            val shape = Ast.dlshape_t { shape=[|sz|], layout=Ast.DL_Layout_Unknown }
            val t = Ast.dltensor_t {
                shape = shape,
                data = data
            }
            val c_idx = vconsts.push()
            vconsts.data[c_idx] = t
            val arg = Ast.dlarg_t {
                name = name,
                argkind = Ast.DL_Arg_Const,
                shape = shape,
                typ = Ast.gettype(data),
                idx = c_idx
            }
            val idx = vargs.push()
            vargs.data[idx] = arg
            argnames.add(name, idx)
            idx
        }
    //fun get_const_tensor_arg(i: int) = get_const_tensor_arg(f"const:{i}", Ast.DL_Data_I32([|int32(i)|]))
    fun get_const_tensor_arg(f: float) =
        get_const_tensor_arg(f"const:{f}", Ast.DL_Data_FP32([|f|]))
    fun get_const_tensor_arg(iarr: int []) =
        get_const_tensor_arg(f"const:{iarr}", Ast.DL_Data_I32(int32(iarr)))

    val net = Ast.empty_net().{
        info = info,
        argnames = argnames,
        dimnames = dimnames,
        dimnames_ = dimnames_,
        inpargs = inpargs,
        outargs = outargs,
        args = args,
        consts = consts,
    }

    val fold prog = [] for node <- model.graph.nodes {
        println(f"parsing operation '{node.op}'")
        val inputs = [| for argname <- node.inputs {
            match argnames.find_opt(argname) {
            | Some(idx) => idx
            | _ => throw OnnxConvertError(f"cannot find input '{argname}'")
            }} |]
        val outputs = [| for argname <- node.outputs {
            match argnames.find_opt(argname) {
            | Some(idx) => idx
            | _ =>
                val idx = vargs.push()
                vargs.data[idx] = Ast.dlarg_t {
                    name=argname,
                    argkind=Ast.DL_Arg_Buffer, // later on we can convert it to output
                    shape=Ast.dlshape_t {
                        shape=[],
                        layout=Ast.DL_Layout_Unknown
                    },
                    typ=Ast.DL_Undefined,
                    idx=-1 }
                argnames.add(argname, idx)
                idx
            }} |]
        val ninputs = size(inputs), noutputs = size(outputs)

        val new_ops = match node.op {
        | "Add" | "And" | "Div" | "Equal" | "Greater" | "Less" | "Mod" | "Mul" | "Or" | "Sub" | "Xor" =>
            assert(`ninputs == 2`)
            assert(`noutputs == 1`)
            val op = match node.op {
                | "Add" => Ast.DL_Add
                | "And" => Ast.DL_And
                | "Div" => Ast.DL_Div
                | "Equal" => Ast.DL_Equal
                | "Greater" => Ast.DL_Greater
                | "Less" => Ast.DL_Less
                | "Mod" => Ast.DL_Mod
                | "Mul" => Ast.DL_Mul
                | "Or" => Ast.DL_Or
                | "Sub" => Ast.DL_Sub
                | "Xor" => Ast.DL_Xor
            }
            [Ast.DL_EWise2 {op=op, t_inp1=inputs[0], t_inp2=inputs[1], t_out=outputs[0]}]
        | "Abs" |  "Relu" =>
            assert(`ninputs == 1`)
            assert(`noutputs == 1`)
            [Ast.DL_EWise1 {op=Ast.DL_Relu, t_inp=inputs[0], t_out=outputs[0]}]
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
            }
            if size(kernel_shape) != 2 {
                throw OnnxConvertError(f"{node.name} (op={node.op}): not a 2D pooling operation!")
            }
            if pads == [] {pads = [|0,0,0,0|]} else {assert(size(pads) == 4)}
            if strides == [] {strides = [|1,1|]} else {assert(size(strides) == 2)}
            if dilations == [] {dilations = [|1,1|]} else {assert(size(dilations) == 2)}
            val storage_order = if storage_order == 0 {Ast.DL_RowMajor} else {Ast.DL_ColumnMajor}
            [if ismax {
                Ast.DL_MaxPool {
                    kernel_shape=(kernel_shape[0], kernel_shape[1]),
                    pads=autopad2pads2d(auto_pad, kernel_shape, pads),
                    strides=(strides[0], strides[1]),
                    dilations=(dilations[0], dilations[1]),
                    ceil_mode = ceil_mode != 0,
                    storage_order=storage_order,
                    t_inp=inputs[0], t_out=outputs[0] }
            } else {
                Ast.DL_AvgPool {
                    kernel_shape=(kernel_shape[0], kernel_shape[1]),
                    pads=autopad2pads2d(auto_pad, kernel_shape, pads),
                    strides=(strides[0], strides[1]),
                    dilations=(dilations[0], dilations[1]),
                    ceil_mode = ceil_mode != 0,
                    count_include_pad=count_include_pad != 0,
                    storage_order=storage_order,
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
            }
            [Ast.DL_BatchNorm {
                epsilon=epsilon, momentum=momentum, training_mode=training_mode!=0,
                t_inp=inputs[0], t_scale=inputs[1], t_B=inputs[2],
                t_mean=inputs[3], t_var=inputs[4], t_out=outputs[0] }]
        | "Clip" =>
            assert(`ninputs == 1 || ninputs == 3`)
            assert(`noutputs == 1`)
            var maxv = 3.402823e+38f, minv = -maxv
            for a <- node.attrs {
                | {name="min"} => minv = attr2float(a)
                | {name="max"} => maxv = attr2float(a)
            }
            val t_min = if ninputs == 3 {inputs[1]} else {get_const_tensor_arg(minv)}
            val t_max = if ninputs == 3 {inputs[2]} else {get_const_tensor_arg(maxv)}
            [Ast.DL_Clip {t_inp=inputs[0], t_min=t_min, t_max=t_max, t_out=outputs[0]}]
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
            val c_idx = vconsts.push()
            vconsts.data[c_idx] = t
            vargs.data[outputs[0]] = vargs.data[outputs[0]].{argkind=Ast.DL_Arg_Const, shape=t.shape, typ=Ast.gettype(t.data), idx=c_idx}
            [] // there is no actual output operation
        | "Concat" =>
            assert(`noutputs == 1`)
            var axis = 0
            for a <- node.attrs {
                | {name="axis"} => axis = attr2int(a)
            }
            [Ast.DL_Concat {axis=axis, t_inp=copy(inputs), t_out=outputs[0]}]
        | "Conv" =>
            assert(`ninputs == 2 || ninputs == 3`)
            assert(`noutputs == 1`)
            var kernel_shape = [], strides = [], dilations = []
            var pads = [], auto_pad = Ast.DL_Pad_None, group = 1
            for a <- node.attrs {
                | {name="kernel_shape"} => kernel_shape = attr2ints(a)
                | {name="pads"} => pads = attr2ints(a)
                | {name="strides"} => strides = attr2ints(a)
                | {name="dilations"} => dilations = attr2ints(a)
                | {name="group"} => group = attr2int(a)
                | {name="auto_pad"} => auto_pad = attr2autopad(a)
            }
            if size(kernel_shape) != 2 {
                throw OnnxConvertError(f"{node.name} (op=Conv): not a 2D convolution!")
            }
            if pads == [] {pads = [|0,0,0,0|]} else {assert(size(pads) == 4)}
            if strides == [] {strides = [|1,1|]} else {assert(size(strides) == 2)}
            if dilations == [] {dilations = [|1,1|]} else {assert(size(dilations) == 2)}
            [Ast.DL_Conv2D {
                kernel_shape=(kernel_shape[0], kernel_shape[1]),
                pads=autopad2pads2d(auto_pad, kernel_shape, pads),
                strides=(strides[0], strides[1]),
                dilations=(dilations[0], dilations[1]),
                group = group,
                t_inp=inputs[0], t_weights=inputs[1],
                t_bias=(if ninputs == 3 {inputs[2]} else {0}),
                t_out=outputs[0]}]
        | "Dropout" =>
            assert(`1 <= ninputs <= 3`)
            assert(`noutputs == 1 || noutputs == 2`)
            var seed=0
            for a <- node.attrs {
                | {name="seed"} => seed = attr2int(a)
            }
            [Ast.DL_Dropout {
                seed=seed,
                t_inp=inputs[0],
                t_ratio=(if ninputs >= 2 {inputs[1]} else {get_const_tensor_arg(0.5f)}),
                t_training_mode=(if ninputs >= 3 {inputs[2]} else {0}),
                t_out=outputs[0] }] // ignore the second output
        | "Flatten" =>
            assert(`ninputs == 1`)
            assert(`noutputs == 1`)
            var axis=1
            for a <- node.attrs {
                | {name="axis"} => axis = attr2int(a)
            }
            [Ast.DL_Flatten {axis=axis, t_inp=inputs[0], t_out=outputs[0]}]
        | "Gather" =>
            assert(`ninputs == 2`)
            assert(`noutputs == 1`)
            var axis=0
            for a <- node.attrs {
                | {name="axis"} => axis = attr2int(a)
            }
            [Ast.DL_Gather {
                axis=axis,
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
            }
            [Ast.DL_Gemm {
                alpha=alpha, beta=beta,
                transA=transA!=0, transB=transB!=0,
                t_inp=inputs[0], t_weights=inputs[1],
                t_bias=(if ninputs == 3 {inputs[2]} else {0}),
                t_out=outputs[0]}]
        | "GlobalAveragePool" =>
            assert(`ninputs == 1`)
            assert(`noutputs == 1`)
            [Ast.DL_GlobalAvgPool {t_inp=inputs[0], t_out=outputs[0]}]
        | "LRN" =>
            assert(ninputs == 1)
            assert(noutputs == 1)
            var size = -1, alpha = 0.0001f, beta = 0.75f, bias = 1.0f
            for a <- node.attrs {
                | {name="alpha"} => alpha = attr2float(a)
                | {name="beta"} => beta = attr2float(a)
                | {name="bias"} => bias = attr2float(a)
                | {name="size"} => size = attr2int(a)
            }
            assert(`size > 0`)
            [Ast.DL_LRN {
                size=size, alpha=alpha, beta=beta, bias=bias,
                t_inp=inputs[0], t_out=outputs[0] }]
        | "Reshape" =>
            assert(`ninputs == 1 || ninputs == 2`)
            assert(`noutputs == 1`)
            var allowzero = 0, cshape = []
            for a <- node.attrs {
                | {name="allowzero"} => allowzero = attr2int(a)
                | {name="shape"} => cshape = attr2ints(a)
            }
            val t_shape = if ninputs == 2 {inputs[1]} else {get_const_tensor_arg(cshape)}
            [Ast.DL_Reshape {
                allowzero=allowzero != 0,
                t_inp=inputs[0], t_shape=t_shape, t_out=outputs[0]}]
        | "Shape" =>
            assert(`ninputs == 1`)
            assert(`noutputs == 1`)
            var start = 0, end = 2147483647
            for a <- node.attrs {
                | {name="start"} => start = attr2int(a)
                | {name="end"} => end = attr2int(a)
            }
            [Ast.DL_Shape {start=start, end=end, t_inp=inputs[0], t_out=outputs[0]}]
        | "Slice" =>
            assert(`ninputs == 1 || (3 <= ninputs <= 5)`)
            assert(`noutputs == 1`)
            var starts = [], ends = [], axes = [], steps = []
            for a <- node.attrs {
                | {name="starts"} => starts = attr2ints(a)
                | {name="ends"} => ends = attr2ints(a)
                | {name="axes"} => axes = attr2ints(a)
                | {name="steps"} => steps = attr2ints(a)
            }
            val t_starts = if ninputs >= 3 {inputs[1]} else {get_const_tensor_arg(starts)}
            val t_ends = if ninputs >= 3 {inputs[2]} else {get_const_tensor_arg(ends)}
            val t_axes = if ninputs >= 4 {inputs[3]} else if axes != [] {get_const_tensor_arg(axes)} else {0}
            val t_steps = if ninputs >= 5 {inputs[4]} else if steps != [] {get_const_tensor_arg(steps)} else {0}
            [Ast.DL_Slice {t_inp=inputs[0], t_starts=t_starts, t_ends=t_ends,
                t_axes=t_axes, t_steps=t_steps, t_out=outputs[0]}]
        | "Softmax" =>
            assert(`ninputs == 1`)
            assert(`noutputs == 1`)
            var axis = -1
            for a <- node.attrs {
                | {name="axis"} => axis = attr2int(a)
            }
            [Ast.DL_SoftMax {axis=axis, t_inp=inputs[0], t_out=outputs[0]}]
        | "Transpose" =>
            assert(`ninputs == 1`)
            assert(`noutputs == 1`)
            var perm = []
            for a <- node.attrs {
                | {name="perm"} => perm = attr2ints(a)
            }
            [Ast.DL_Transpose {perm=perm, t_inp=inputs[0], t_out=outputs[0]}]
        | "Unsqueeze" =>
            assert(`ninputs == 1 || ninputs == 2`)
            assert(`noutputs == 1`)
            var axes = []
            for a <- node.attrs {
                | {name="axes"} => axes = attr2ints(a)
            }
            val t_axes = if ninputs == 2 {inputs[1]} else {get_const_tensor_arg(axes)}
            if ninputs == 1 && axes == [] {
                throw OnnxConvertError(f"{node.name} (op=Unsqueeze): 'axes' is missing")
            }
            [Ast.DL_Unsqueeze {
                t_inp=inputs[0], t_axes=t_axes, t_out=outputs[0] }]
        | _ => throw OnnxConvertError(f"unsupported operation '{node.op}'")
        }
        new_ops + prog
    }

    net.{
        prog = array(prog.rev()),
        argnames = argnames,
        args = vargs.data[:vargs.count],
        consts = vconsts.data[:vconsts.count]
    }
}

always_use(OnnxConvertError("abc"))
fun read(fname: string) = convert(Onnx.Parser.parse(fname))
