/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Computes shapes of all intermediate tensors
import Ast

fun get_shape_typ(net: dlnet_t, _: dlop_t, argidx: int)
{
    val arg = net.args[argidx]
    (arg.shape, arg.typ)
}

fun get_shape(net: dlnet_t, op: dlop_t, argidx: int) = get_shape_typ(net, op, argidx).0

fun set_shape_typ(net: dlnet_t, _: dlop_t, argidx: int, (shape: Ast.dlshape_t, typ: Ast.dltyp_t))
{
    val arg = net.args[argidx]
    net.args[argidx] = arg.{shape=shape, typ=typ}
}

fun copy_shape_typ(net: dlnet_t, op: dlop_t, t_inp: int, t_out: int) =
    set_shape_typ(net, op, t_out, get_shape_typ(net, op, t_inp))

fun get_channel_dim(net: dlnet_t, _: dlop_t, argidx: int) {
    val arg = net.args[argidx]
    val dims = size(arg.shape.shape)
    match (arg.shape.layout, dims) {
    | (_, 1) => 0
    | (Ast.DL_Layout_NCHW, _) => 1
    | (Ast.DL_Layout_NHWC, _) => dims-1
    | _ => -1
    }
}

fun new_var_dim(net: dlnet_t, _: dlop_t, name_prefix: string) {
    var name = ""
    for i <- 0:1000000 {
        var temp_name =
    }
}

fun infer(net: dlnet_t, op: dlop_t)
{
    val (name, opname) = Ast.get_opname(op)
    match op {
    | DL_AvgPool: {
        ceil_mode: bool
        dilations: int []
        kernel_shape: int []
        pads: int []
        strides: int []
        storage_order: dlorder_t
        count_include_pad: bool
        t_inp: int; t_out: int }
    | DL_BatchNorm: { t_inp, t_scale, t_B, t_mean, t_var, t_out }

    | DL_Cast {to, t_inp, t_out} =>
        set_shape(net, op, t_out, (get_shape(net, op, t_inp), to))
    | DL_Clip {t_inp, t_out} =>
        copy_shape_typ(net, op, t_inp, t_out)
    | DL_Concat { axis, t_inp, t_out } =>
        val (shape0, typ0) = get_shape_typ(net, op, t_inp[0])
        val dims = size(shape0.shape)
        val axis = if axis >= 0 {axis} else {dims + axis}
        val temp_shape = copy(shape0.shape)
        assert(`0 <= axis < dims`)
        val fold out_shape_a = 0 for t_inp_i@i <- t_inp {
            val (shape_i, typ_i) = get_shape_typ(net, op, t_inp_i)
            assert(`size(shape_i.shape) == size(shape0.shape)`)
            assert(`shape_i.layout == shape0.layout`)
            assert(`typ_i == typ0`)
            temp_shape[axis] = shape_i.shape[axis]
            assert(`temp_shape == shape_i.shape`)
            out_shape_a + shape_i.shape[axis]
        }
        temp_shape[axis] = out_shape_a
        set_shape_typ(net, op, t_out, (shape0.{shape=temp_shape}, typ0))
    | DL_ConstantOfShape {
        value: dltensor_t; t_shape: int; t_out: int }
    | DL_Conv: {
        kernel_shape: int []
        pads: int []
        strides: int []
        dilations: int []
        group: int
        t_inp: int; t_weights: int; t_bias: int; t_out: int }
    | DL_ConvTranspose: {
        kernel_shape: int []
        pads: int []
        strides: int []
        dilations: int []
        out_shape: int []
        out_padding: int []
        group: int
        t_inp: int; t_weights: int; t_bias: int; t_out: int }
    | DL_Dropout {t_inp, t_ratio, t_training_mode, t_out } =>
    | DL_Elemwise {t_inp, t_out} =>

    | DL_Expand {t_inp, t_shape, t_out} =>

    | DL_Flatten {axis, t_inp, t_out} =>
    | DL_Gather {axis, t_inp, t_out} =>
    | DL_Gemm: {transA, transB, t_inp, t_weights, t_out} =>

    | DL_GlobalAvgPool {t_inp, t_out} =>
        val (shape, typ) = get_shape_typ(net, op, t_inp)
        val channel_dim = get_channel_dim(net, op, t_inp)
        val outshape = [| for sz@d <- shape.shape {if d == 0 || d == channel_dim {sz} else {1}} |]
        set_shape_typ(net, op, (Ast.dlshape_t {shape=outshape, layout=shape.layout}, typ))
    | DL_Identity {t_inp, t_out} =>
        copy_shape_typ(net, op, t_inp, t_out)
    | DL_If: {
        then_branch: dlgraph_t; else_branch: dlgraph_t; t_inp: int; t_out: int [] }
    | DL_LeakyRelu: {t_inp, t_out} =>
        copy_shape_typ(net, op, t_inp, t_out)
        alpha: float; t_inp: int; t_out: int }
    | DL_Loop: {
        body: dlgraph_t; t_trip_count: int;
        t_cond_in: int; t_v_in: int [];
        t_cond_out: int; t_v_out: int [] }
    | DL_LRN {t_inp, t_out} =>
        copy_shape_typ(net, op, t_inp, t_out)
    | DL_MaxPool: {
        ceil_mode: bool
        dilations: int []
        kernel_shape: int []
        pads: int []
        strides: int []
        storage_order: dlorder_t = DL_RowMajor
        t_inp: int; t_out: int }
    | DL_NonMaxSuppression: {
        center_point_box: bool;
        t_boxes: int;
        t_scores: int;
        t_max_output_boxes_per_class: int;
        t_iou_threshold: int;
        t_score_threshold: int;
        t_out: int }
    | DL_NonZero {t_inp, t_out} =>

    | DL_Range {t_start, t_limit, t_delta, t_out} =>
        val
    | DL_Reduce: {
        reduce_op: dlreduce_t; axes: int []; keepdims: bool; t_inp: int; t_out: int }
    | DL_Reshape {allowzero, t_inp, t_shape, t_out}

    | DL_Resize: {
        coord_trans: dlcoord_trans_t
        cubic_coeff_a: float
        exclude_outside: bool
        extrapolation_value: float
        mode: dlinterpolation_t
        nearest_mode: dlnearest_mode_t
        t_inp: int; t_scales: int; t_sizes: int
        t_roi: int; t_out: int }
    | DL_RoiAlign: {
        coord_trans: dlcoord_trans_t;
        mode: dlpooling_t;
        output_height: int; output_width: int;
        sampling_ratio: int; spatial_scale: float;
        t_inp: int; t_rois: int; t_batch_ind: int; t_out: int }
    | DL_Scatter: {
        axis: int; t_data: int; t_updates: int; t_indices: int; t_out: int }
    | DL_Shape {start, end, t_out} =>
        val shape = get_shape(net, op, t_inp)
        val dims = size(shape.shape)
        val start = if start >= 0 {start} else {dims + start}
        val end = min(dims, (if end >= 0 {end} else {dims + end}))
        set_shape_typ(net, op, t_out, (dl_shape_t {shape=[|max(end-start, 0)|],
            layout=shape.layout}, Ast.DL_I64))
    | DL_Slice: {
        t_inp: int; t_starts: int; t_ends: int; t_axes: int; t_steps: int; t_out: int }
    | DL_SoftMax {t_inp, t_out} =>
        copy_shape_typ(net, op, t_inp, t_out)
    | DL_Split: {
        axis: int; t_inp: int; t_split: int; t_out: int [] }
    | DL_Squeeze: {t_inp, t_axes, t_out} =>
    | DL_Tile: {
        t_inp: int; t_repeats: int; t_out: int }
    | DL_TopK: {
        axis: int; largest: bool; sorted: bool; t_inp: int; t_K: int; t_out: int; t_out_ind: int }
    | DL_Transpose {perm, t_inp, t_out} =>
    | DL_Unsqueeze {t_inp, t_axes, t_out} =>
    }
}
