/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Computes shapes of all intermediate tensors
import Hashmap
import Ast, InferShapes
import OpConv, OpElemwise, OpGemm, OpMisc, OpPermute, OpPooling, OpReduce

fun run(net: Ast.nnet_t, inputs: (string, Ast.nntensor_t) []/*,
            cb_before: Ast.op_callback_t?, cb_after: Ast.op_callback_t?*/,
        ~outputs: (string, Ast.nntensor_t) [] = []):
    (string, Ast.nntensor_t) []
{
    var empty_names = true

    // assign input tensors
    for (inpname, t)@i <- inputs {
        // check that either all input names are empty or none of them
        if i == 0 {empty_names = inpname == ""}
        else {assert(empty_names == (inpname == ""))}
        val argidx =
            if inpname == "" {net.graph.inpargs[i]}
            else {
                val argidx = net.argnames.find_opt(inpname).value_or(-1)
                if argidx < 0 {
                    throw Ast.NNError(f"cannot find input '{inpname}'; available inputs are: {net.get_input_names()}")
                }
                argidx
            }
        println(f"assigned input #{i} to {net.args[argidx].name}")
        val arg = net.args[argidx]
        println(f"input #{i}: {arg}")
        assert(arg.argkind == Ast.NN_Arg_Input)
        assert(arg.typ == t.elemtype())
        assert(arg.shape.layout == t.shape.layout || arg.shape.layout == Ast.NN_Layout_Unknown)
        net.tensors[argidx] = t
    }

    println("running main graph")
    run_graph(net, net.graph, outputs)

    // collect outputs
    [for argidx <- net.graph.outargs {
        val arg = net.args[argidx]
        assert(arg.argkind == Ast.NN_Arg_Output)
        (arg.name, net.tensors[argidx])
    }]
}

fun run_op(net: Ast.nnet_t, op: Ast.nnop_t) =
match op
{
    | Ast.NN_AvgPool _ =>
        OpPooling.run_avgpool(net, op)
    | Ast.NN_BatchNorm {epsilon, momentum, training_mode,
        t_inp, t_scale, t_B, t_mean, t_var, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Cast _ =>
        OpElemwise.run_cast(net, op)
    | Ast.NN_Clip _ =>
        OpElemwise.run_clip(net, op)
    | Ast.NN_Concat _ =>
        OpPermute.run_concat(net, op)
    | Ast.NN_ConstantOfShape _ =>
        OpElemwise.run_constantOfShape(net, op)
    | Ast.NN_Conv _ =>
        OpConv.run_conv(net, op)
    | Ast.NN_ConvTranspose _ =>
        OpConv.run_conv_transposed(net, op)
    | Ast.NN_Dropout _ =>
        OpElemwise.run_dropout(net, op)
    | Ast.NN_Elemwise {t_inp} =>
        val ninputs = t_inp.size()
        if ninputs == 1 {
            OpElemwise.run_unary(net, op)
        } else if ninputs == 2 {
            OpElemwise.run_binary(net, op)
        } else {
            OpElemwise.run_nary(net, op)
        }
    | Ast.NN_Expand {t_inp, t_shape, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Flatten {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Gather _ =>
        OpPermute.run_gather(net, op)
    | Ast.NN_Gemm _ =>
        OpGemm.run_gemm(net, op)
    | Ast.NN_GlobalAvgPool {t_inp, t_out} =>
        OpPooling.run_global_avgpool(net, op)
    | Ast.NN_Identity {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.NN_If {then_branch, else_branch, t_inp, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_LeakyRelu {alpha, t_inp, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Loop {body, t_trip_count, t_cond_in, t_v_in, t_cond_out, t_v_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_LRN _ =>
        OpMisc.run_lrn(net, op)
    | Ast.NN_MaxPool _ =>
        OpPooling.run_maxpool(net, op)
    | Ast.NN_NonMaxSuppression {center_point_box, t_boxes, t_scores,
        t_max_output_boxes_per_class, t_iou_threshold, t_score_threshold, t_out } =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_NonZero {t_inp, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Range {t_start, t_limit, t_delta, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Reduce {reduce_op, axes, keepdims, t_inp, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Reshape {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Resize {coord_trans, cubic_coeff_a, exclude_outside,
        extrapolation_value, mode, nearest_mode, t_inp, t_scales, t_sizes, t_roi, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_RoiAlign {coord_trans, mode, output_height, output_width,
        sampling_ratio, spatial_scale, t_inp, t_rois, t_batch_ind, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Scatter {axis, t_data, t_updates, t_indices, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Shape _ =>
        OpPermute.run_shape(net, op)
    | Ast.NN_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_SoftMax _ =>
        OpMisc.run_softmax(net, op)
    | Ast.NN_Split {axis, t_inp, t_split, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Squeeze {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Tile {t_inp, t_repeats, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_TopK {axis, largest, sorted, t_inp, t_K, t_out, t_out_ind} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Transpose {perm, t_inp, t_out} =>
        throw Ast.NNError(f"unsupported operation {op.name()}")
    | Ast.NN_Unsqueeze {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
}

fun run_graph(net: Ast.nnet_t, graph: Ast.nngraph_t, outputs: (string, Ast.nntensor_t) [])
{
    for op <- graph.prog {
        println(f"preparing to run op {op.name()}")
        val oinfo = InferShapes.infer(net, op)
        for oi@outidx <- oinfo {
            val {idx=argidx, shape, typ} = oi
            println(f"   output #{outidx} ('{net.args[argidx].name}'): {oi}")
            val shape0 = net.tensors[argidx].shape
            val typ0 = net.tensors[argidx].elemtype()
            if shape != shape0 || typ != typ0 {
                val arg = net.args[argidx]
                net.args[argidx].shape.layout = shape.layout
                if arg.argkind == Ast.NN_Arg_Output {
                    net.tensors[argidx] = Ast.make_tensor(shape, typ)
                } else if arg.argkind == Ast.NN_Arg_Temp {
                    val bufidx = net.bufidxs[argidx]
                    println(f"   fit into buf #{bufidx} with shape={shape}, typ={typ}, data of {net.tensors[argidx].data.total()} elems, buf of {net.buffers[bufidx].size()} bytes")
                    val (data, buf) = Ast.fit(shape, typ, net.tensors[argidx].data, net.buffers[bufidx])
                    net.tensors[argidx].data = data
                    net.tensors[argidx].shape = shape
                    net.buffers[bufidx] = buf
                } else {
                    throw Ast.NNError(f"unexpected argkind={arg.argkind} of the output {outidx} of {op.name()}")
                }
            }
        }
        println(f"running op {op.name()}")
        run_op(net, op)
        if outputs != [] {
            for oi@outidx <- oinfo {
                val {idx=argidx} = oi
                val name = net.args[argidx].name
                match find_opt(for (n,_)@i <- outputs {n == name}) {
                | Some((_, i)) =>
                    outputs[i].1 = net.tensors[argidx].copy()
                | _ => {}
                }
            }
        }
    }
}

fun top_k(t: Ast.nntensor_t, k: int)
{
    val shape = t.shape.shape
    val ndims = shape.size()
    assert(k > 0)
    assert(ndims == 1 || ndims == 2)
    val (nrows, ncols) = if ndims == 1 {(1, shape[0])} else {(shape[0], shape[1])}
    val k = min(k, ncols)
    val data = float(t)
    assert(data.size() == nrows*ncols)
    val temp = array(ncols, (0.f, 0))
    val result = array((nrows, k), (0, 0.f))
    for i <- 0:nrows {
        val data_i = data[i*ncols:(i+1)*ncols]
        if k == 1 {
            val (prob, label) = maxindex(data_i)
            result[i, 0] = (label, prob)
        } else {
            for j <- 0:ncols {temp[j] = (-data_i[j], j)}
            sort(temp, (<), prefix=k)
            for j <- 0:k {
                val t = temp[j]
                result[i, j] = (t.1, -t.0)
            }
        }
    }
    result
}
