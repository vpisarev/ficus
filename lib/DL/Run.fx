/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Computes shapes of all intermediate tensors
import Hashmap
import Ast, InferShapes, OpElemwise, OpConv, OpPooling, OpReduce, OpPermute

fun inference(net: Ast.dlnet_t, inputs: (string, Ast.dltensor_t) [],
            cb_before: Ast.op_callback_t?, cb_after: Ast.op_callback_t?):
    (string, Ast.dltensor_t) []
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
                    throw Ast.DLError(f"cannot find input '{inpname}'; available inputs are: {net.get_input_names()}")
                }
                argidx
            }
        val arg = net.args[argidx]
        assert(arg.argkind == Ast.DL_Arg_Input)
        assert(arg.typ == t.elemtype())
        assert(arg.shape.layout == t.shape.layout)
        net.tensors[argidx] = t
    }

    run_graph(net, net.graph)

    // collect outputs
    [| for argidx <- net.graph.outargs {
        val arg = net.args[argidx]
        assert(arg.argkind == Ast.DL_Arg_Output)
        (arg.name, net.tensors[argidx])
    } |]
}

fun run_op(net: Ast.dlnet_t, op: Ast.dlop_t) =
match op
{
    | Ast.DL_AvgPool _ =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_BatchNorm {epsilon, momentum, training_mode,
        t_inp, t_scale, t_B, t_mean, t_var, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Cast _ =>
        OpElemwise.run_cast(net, op)
    | Ast.DL_Clip _ =>
        OpElemwise.run_clip(net, op)
    | Ast.DL_Concat _ =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_ConstantOfShape _ =>
        OpElemwise.run_constantOfShape(net, op)
    | Ast.DL_Conv {kernel_shape, pads, strides, dilations, group, t_inp, t_weights, t_bias, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_ConvTranspose {kernel_shape, pads, strides, dilations, out_shape,
                            out_padding, group, t_inp, t_weights, t_bias, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Dropout {seed, t_inp, t_ratio, t_training_mode, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Elemwise {el_op, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Expand {t_inp, t_shape, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Flatten {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.DL_Gather {axis, t_inp, t_ind, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Gemm {alpha, beta, transA, transB, t_A, t_B, t_bias, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_GlobalAvgPool {t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Identity {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.DL_If {then_branch, else_branch, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_LeakyRelu {alpha, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Loop {body, t_trip_count, t_cond_in, t_v_in, t_cond_out, t_v_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_LRN {size, alpha, beta, bias, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_MaxPool {ceil_mode, dilations, kernel_shape, pads, strides, storage_order, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_NonMaxSuppression {center_point_box, t_boxes, t_scores,
        t_max_output_boxes_per_class, t_iou_threshold, t_score_threshold, t_out } =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_NonZero {t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Range {t_start, t_limit, t_delta, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Reduce {reduce_op, axes, keepdims, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Reshape {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.DL_Resize {coord_trans, cubic_coeff_a, exclude_outside,
        extrapolation_value, mode, nearest_mode, t_inp, t_scales, t_sizes, t_roi, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_RoiAlign {coord_trans, mode, output_height, output_width,
        sampling_ratio, spatial_scale, t_inp, t_rois, t_batch_ind, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Scatter {axis, t_data, t_updates, t_indices, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Shape {start, end, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_SoftMax {axis, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Split {axis, t_inp, t_split, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Squeeze {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.DL_Tile {t_inp, t_repeats, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_TopK {axis, largest, sorted, t_inp, t_K, t_out, t_out_ind} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Transpose {perm, t_inp, t_out} =>
        throw Ast.DLError(f"unsupported operation {op.name()}")
    | Ast.DL_Unsqueeze {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
}

fun run_graph(net: Ast.dlnet_t, graph: Ast.dlgraph_t)
{
    for op <- graph.prog {
        val oinfo = InferShapes.infer(net, op)
        for oi@outidx <- oinfo {
            val {idx=argidx, shape, typ} = oi
            val shape0 = net.tensors[argidx].shape
            val typ0 = net.tensors[argidx].elemtype()
            if shape != shape0 || typ != typ0 {
                val arg = net.args[argidx]
                if arg.argkind == Ast.DL_Arg_Output {
                    net.tensors[argidx] = Ast.make_tensor(shape, typ)
                } else if arg.argkind == Ast.DL_Arg_Temp {
                    val bufidx = net.bufidxs[argidx]
                    val (data, buf) = Ast.fit(shape, typ, net.tensors[argidx].data, net.buffers[bufidx])
                    net.tensors[argidx].data = data
                    net.buffers[bufidx] = buf
                } else {
                    throw Ast.DLError(f"unexpected argkind={arg.argkind} of the output {outidx} of {op.name()}")
                }
            }
        }
        run_op(net, op)
    }
}
