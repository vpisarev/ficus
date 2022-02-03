/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Computes shapes of all intermediate tensors
import Ast, Hashmap

type counters_t = (string, int) Hashmap.t

type argshapeinfo_t =
{
    idx: int, // index of argument
    shape: Ast.dlshape_t, // inferenced shape (may contain '?' or other vars)
    typ: Ast.dltyp_t, // type
    dynamic: bool   // whether the argument shape is dynamic
                    // (i.e. it depends on the actual content of inputs,
                    // not only on their shapes (see, e.g. ONNX op "NonZero"))
}

fun infer(net: dlnet_t, op: dlop_t): argshapeinfo_t []
{
    val (name, opname) = Ast.get_opname(op)

    fun get_shape_typ(argidx: int)
    {
        val t = net.gettensor(argidx)
        (t.shape, t.data.elemtype())
    }

    fun get_shape(argidx: int) = get_shape_typ(argidx).0

    fun copy_shape_typ(t_inp: int, t_out: int) {
        val (shape, typ) = get_shape_typ(t_inp)
        argshapeinfo_t { idx=t_out, shape=shape, typ=typ, dynamic=false }
    }

    // [TODO] NCHWxc is not handled properly yet
    try {
    match op {
    | DL_AvgPool {ceil_mode, dilations, kernel_shape, pads, strides, t_inp, t_out} =>
        [|pool_shape(ceil_mode, dilations, kernel_shape,
            pads, strides, storage, t_inp, t_out)|]
    | DL_BatchNorm {t_inp, t_scale, t_B, t_mean, t_var, t_out} =>

    | DL_Cast {to, t_inp, t_out} =>
        val shape = get_shape(t_inp)
        [|argshapeinfo_t {idx=t_out, shape=shape, typ=to, dynamic=false}|]
    | DL_Clip {t_inp, t_out} =>
        [|copy_shape_typ(t_inp, t_out)|]
    | DL_Concat { axis, t_inp, t_out } =>
        val (shape0, typ0) = get_shape_typ(t_inp[0])
        val ndims = size(shape0.shape)
        val axis = Ast.normalize_axis(axis, ndims)
        val temp_shape = copy(shape0.shape)
        val fold out_shape_a = 0 for t_inp_i@i <- t_inp {
            val (shape_i, typ_i) = get_shape_typ(t_inp_i)
            assert(`size(shape_i.shape) == size(shape0.shape)`)
            assert(`shape_i.layout == shape0.layout`)
            assert(`typ_i == typ0`)
            temp_shape[axis] = shape_i.shape[axis]
            assert(`temp_shape == shape_i.shape`)
            out_shape_a + shape_i.shape[axis]
        }
        temp_shape[axis] = out_shape_a
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=shape0.layout, temp_shape}, typ=typ0, dynamic=false}|]
    | DL_ConstantOfShape {value, t_shape, t_out} =>

    | DL_Conv {kernel_shape, pads, strides, dilations, group, t_inp, t_weights, t_out} =>

    | DL_ConvTranspose {kernel_shape, pads, strides, dilations,
        out_shape, out_padding, group, t_inp, t_weights, t_out} =>
    | DL_Dropout {t_inp, t_out} =>
        [|copy_shape_typ(t_inp, t_out)|]
    | DL_Elemwise {op, t_inp, t_out} =>
        val enable_broadcast = match op {
            | Ast.DL_Add | Ast.DL_And | Ast.DL_Div
            | Ast.DL_Equal | Ast.DL_Greater | Ast.DL_Less
            | Ast.DL_Max | Ast.DL_Mean | Ast.DL_Min
            | Ast.DL_Mul | Ast.DL_Or | Ast.DL_Pow
            | Ast.DL_Sub | Ast.DL_Xor => true
            | _ => false }
        val (shape0, typ0) = get_shape_typ(t_inp[0])
        val fold out_shape = shape0 for t_inp_i@i <- t_inp {
            val (shape_i, typ_i) = get_shape_typ(t_inp_i)
            assert(`typ_i == typ0`)
            if enable_broadcast {
                out_shape.broadcast(shape_i)
            } else {
                assert(`shape0.layout == shape_i.layout`)
                assert(`shape0.shape == shape_i.shape`)
                out_shape
            }
        }
        [|argshapeinfo_t {idx=t_out, shape=out_shape, typ=typ0, dynamic=false}|]
    | DL_Expand {t_inp, t_shape, t_out} =>
        val (shape0, typ0) = get_shape_typ(t_inp)
        val shape_arg = net.args[t_shape]
        val shape_arr = int(net.gettensor(shape_arg))
        assert(`size(shape_arr.shape.shape) == 1`)
        val constshape = shape_arg.isconst()
        val out_shape = shape0.broadcast(dlshape_t {layout=shape0.layout, shape=shape_arr})
        assert(`out_shape.shape == shape_arr`)
        [|argshapeinfo_t {idx=t_out, shape=out_shape, typ=typ0, dynamic=!constshape}|]
    | DL_Flatten {axis, t_inp, t_out} =>
        // [TODO] handle NCHWxc properly
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axis = Ast.normalize_axis(axis, ndims)
        val fold sz1 = 1, sz2 = 1 for sz@i <- shape.shape {
            if i < axis {(sz1*sz, sz2)} else {(sz1, sz2*sz)}
        }
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=Ast.DL_Layout_NC,
            shape=[|sz1, sz2|]}, typ=typ, dynamic=false}|]
    | DL_Gather {axis, t_inp, t_out} =>
        throw Ast.DLError(f"shape inference for op={opname} is not implemented")
    | DL_Gemm {transA, transB, t_A, t_B, t_bias, t_out} =>
        val (ashape, atyp) = get_shape_typ(t_A)
        val (bshape, btyp) = get_shape_typ(t_B)
        val (cshape, ctyp) = get_shape_typ(t_bias)
        val ndims_c = cshape.shape.size()
        assert(`ashape.shape.size() == 2`)
        assert(`bshape.shape.size() == 2`)
        assert(`ndims_c <= 2`)
        val arows = ashape.shape[0], acols = ashape.shape[1]
        val brows = bshape.shape[0], bcols = bshape.shape[1]
        val (crows, ccols) =
            if ndims_c == 2 {(cshape.shape[0], cshape.shape[1])}
            else if ndims_c == 1 {(1, cshape.shape[0])}
            else {(1, 1)}
        assert(`(if transA {arows} else {acols}) == (if transB {bcols} else {brows})`)
        val out_rows = if transA {acols} else {arows}
        val out_cols = if transB {brows} else {bcols}
        assert(`crows == 1 || crows == out_rows`)
        assert(`ccols == 1 || ccols == out_cols`)
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=bshape.layout,
            shape=[|out_rows, out_cols|]}, typ=typ, dynamic=false}|]
    | DL_GlobalAvgPool {t_inp, t_out} =>
        val (shape, typ) = get_shape_typ(net, op, t_inp)
        val ndims = shape.shape.size()
        val channel_dim = shape.get_channel_dim()
        val out_shape = [| for sz@d <- shape.shape {
                if d == 0 || d == channel_dim ||
                   (channel_dim == -ndims && (d == 1 || d == ndims-1)) {sz} else {1}
            } |]
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {shape=out_shape,
            layout=shape.layout}, typ=typ, dynamic=false}|]
    | DL_Identity {t_inp, t_out} =>
        [|copy_shape_typ(t_inp, t_out)|]
    | DL_If _ =>
        throw Ast.DLError(f"shape inference for op={opname} is not implemented")
    | DL_LeakyRelu {t_inp, t_out} =>
        [|copy_shape_typ(t_inp, t_out)|]
    | DL_Loop _ =>
        throw Ast.DLError(f"shape inference for op={opname} is not implemented")
    | DL_LRN {t_inp, t_out} =>
        [|copy_shape_typ(t_inp, t_out)|]
    | DL_MaxPool { ceil_mode, dilations, kernel_shape, pads,
        strides, storage_order, t_inp, t_out } =>
        [|pool_shape(ceil_mode, dilations, kernel_shape,
            pads, strides, storage, t_inp, t_out)|]
    | DL_NonMaxSuppression {
        center_point_box,
        t_boxes: int;
        t_scores: int;
        t_max_output_boxes_per_class: int;
        t_iou_threshold: int;
        t_score_threshold: int;
        t_out: int }
    | DL_NonZero {t_inp, t_out} =>
        val shape = get_shape(t_inp)
        val ndims = shape.shape.size()
        val out_shape = dlshape_t {layout=Ast.DL_Layout_NC, shape=[|shape, -1|]}
        [|argshapeinfo_t {idx=t_out, shape=out_shape, typ=typ, dynamic=true}|]
    | DL_Range {t_start, t_limit, t_delta, t_out} =>
        val t_start = net.gettensor(t_start)
        val typ = t_start.data.elemtype()
        val t_start = double(t_start)
        val t_limit = double(net.gettensor(t_limit))
        val t_delta = double(net.gettensor(t_delta))
        assert(`t_start.size() == 1`)
        assert(`t_limit.size() == 1`)
        assert(`t_delta.size() == 1`)
        val nelems = max(ceil((limit[0] - start[0])/delta[0]), 0)
        val allconsts = net.isconst(t_start) && net.isconst(t_limit) && net.isconst(t_delta)
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=Ast.DL_Shape_NC, shape=[|nelems|]},
            typ=typ, dynamic=!allconsts}|]
    | DL_Reduce {reduce_op, axes, keepdims, t_inp, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val dummy_val = -1000000
        val out_shape =
            if axes == [] {
                [| 1 |]
            } else {
                val out_shape = shape.shape.copy()
                for axis <- axes {
                    val axis = Ast.normalize_axis(axis, ndims)
                    out_shape[axis] = if keepdims {1} else {dummy_val}
                }
                if keep_dims {out_shape}
                else {
                    var out_ndims = 0
                    for sz@i <- out_shape {
                        if sz != dummy_val {
                            out_shape[out_ndims] = sz
                            out_ndims += 1
                        }
                    out_shape[:out_ndims]
                }
            }
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=shape.layout, shape=out_shape},
            typ=typ, dynamic=false}|]
    | DL_Reshape {allowzero, t_inp, t_shape, t_out} =>
        // [TODO] handle NCHWxc
        val (shape, typ) = get_shape_typ(t_inp)
        val (sshape, styp) = get_shape_typ(t_shape)
        assert(`sshape.shape.size() == 1`)
        val new_shape = int(net.gettensor(t_shape))
        val ndims = shape.shape.size()
        val new_ndims = new_shape.size()
        assert(`ndims == new_ndims`)
        val fold old_total = 1 for sz <- shape.shape {if sz != 0 {total*sz} else {total}}
        val fold new_total=1, havem1=false for sz@i <- new_shape {
            if sz == -1 {
                if havem1 {throw DLError(f"shape inference: {name} (op={opname}): the new shape contains more than -1")}
                (new_total, true)
            } else if sz == 0 {
                if allowzero {(new_total, havem1)}
                else {
                    val sz = shape.shape[i]
                    (new_total*sz, havem1)
                }
            }
        }
        val m1sz = if havem1 {
            if old_total % new_total != 0 {
                throw DLError(f"shape inference: {name} (op={opname}): the new shape contains -1, \
                    but the computed size ({old_total}/{new_total}) is not an integer")
            }
            old_total/new_total } else {1}
        val out_shape = [| for sz@i <- new_shape {
                if sz == -1 { m1sz }
                else if sz != 0 { sz }
                else if allowzero { 0 }
                else { shape.shape[i] }
            } |]
        val constshape = net.isconst(t_shape)
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=shape.layout, shape=out_shape},
            typ=typ, dynamic=!constshape}|]
    | DL_Resize { coord_trans, t_inp, t_scales, t_sizes, t_roi, t_out } =>
        // [TODO] handle NCHWxc properly
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val roi =
            if t_roi == 0 {
                ([] : float [])
            } else {
                val roi = float(net.gettensor(t_roi))
                assert(`coord_trans == Ast.DL_CT_TFCropResize`)
                assert(roi.size() == ndims*2)
                roi
            }
        val out_shape = if t_sizes != 0 {
            val sizes = int(net.gettensor(t_sizes))
            assert(`sizes.size() == ndims`)
        } else if t_scales != 0 {
            val scales = int(net.gettensor(t_scales))
            assert(`scales.size() == ndims`)
            [| for sz@i <- shape.shape0, scale <- scales {
                val sz =
                    if t_roi == 0 {float(sz)}
                    else {sz*(roi[i + ndims] - roi[i])}
                floor(sz*scale)
            } |]
        } else {
            throw DLError(f"shape inference: {name} (op={opname}): both scales and sizes are missing in Resize op")}
        }
        val constshape =
            (t_roi == 0 || net.isconst(t_roi)) &&
            (t_sizes == 0 || net.isconst(t_sizes)) &&
            (t_scales == 0 || net.isconst(t_scales))
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=shape.layout, shape=out_shape},
            typ=typ, dynamic=!constshape}|]
    | DL_RoiAlign {output_height, output_width, t_inp, t_rois, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val (roi_shape, roi_typ) = get_shape_typ(t_rois)
        assert(`ndims == 4 || ndims == 5`)
        assert(`roi_shape.shape.size() == 2`)
        assert(`roi_shape.shape[1] == 4`)
        assert(`typ == roi_typ`)
        val nrois = roi_shape.shape[0]
        val C = shape.get_num_channels()
        val out_shape = [| nrois, C, output_height, output_width |]
        // [TODO] probably, if we get tensor with NCHWxc layout on input,
        // we could keep NCHWxc layout in the output as well
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=Ast.DL_Layout_NCHW,
            shape=out_shape}, typ=typ,
            dynamic=false // t_rois is likely a variable-size tensor,
                          // but this operation itself produces fixed-size output
                          // given fixed-size inputs
            }|]
    | DL_Scatter {t_data, t_out} =>
        // [TODO] check t_updates and t_indices shape,
        // but this can also be done in the actual operation implementation
        [|copy_shape_typ(t_data, t_out)|]
    | DL_Shape {start, end, t_out} =>
        val shape = get_shape(net, op, t_inp)
        val dims = shape.shape.size()
        val start = if start >= 0 {start} else {dims + start}
        val end = min(dims, (if end >= 0 {end} else {dims + end}))
        val out_shape = [|max(end-start, 0)|]
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=Ast.DL_Layout_NC,
            shape=out_shape}, typ=Ast.DL_I64, dynamic=false}|]
    | DL_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axes = if t_axes != 0 {int(net.gettensor(t_axes)} else {mkrange(ndims)}
        val naxes = axes.size()
        val starts = int(net.gettensor(t_starts))
        val ends = int(net.gettensor(t_ends))
        val steps = if t_steps != 0 {int(net.gettensor(t_steps))} else {array(naxes, 1)}
        assert(`starts.size() == naxes`)
        assert(`ends.size() == naxes`)
        assert(`steps.size() == naxes`)
        val out_shape = shape.shape.copy()
        for axis <- axes, start <- starts, end <- ends, step <- steps {
            val axis = Ast.normalize_axis(axis, ndims)
            val sz_a = shape.shape[axis]
            val start = min((if start < 0 {start + sz_a} else {start}), sz_a)
            assert()
            val end = min((if end < 0 {end + sz_a} else {end}), sz_a)
            assert(`step != 0`)
            val nelems = if step > 0 {(end - start + step-1)/step}
                         else {(start - end - step-1)/(-step)}
            out_shape[axis] = nelems
        }
        val constshape =
            (t_starts == 0 || net.isconst(t_starts)) &&
            (t_ends == 0 || net.isconst(t_ends)) &&
            (t_axes == 0 || net.isconst(t_axes)) &&
            (t_steps == 0 || net.isconst(t_steps))
        [|argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {layout=Ast.DL_Layout_NCHW,
            shape=out_shape}, typ=typ, dynamic=!constshape}|]
    | DL_SoftMax {t_inp, t_out} =>
        [|copy_shape_typ(t_inp, t_out)|]
    | DL_Split {axis, t_inp, t_split, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axis = Ast.normalize_axis(axis, ndims)
        val sz_a = shape.shape[axis]
        val noutputs = t_out.size()
        if t_split != 0 {
            val splits = int(net.gettensor(t_split))
            val constshape = net.isconst(t_split)
            val total_sz = 0
            assert(`splits.size() == noutputs`)
            [| for t_out_i@i <- t_out {
                val shape_i = shape.shape.copy()
                val sz_i = splits[i]
                assert(`sz_i > 0`)
                shape_i[axis] = sz_i
                total_sz += sz_i
                assert(`(i < noutputs-1 && total_sz < sz_a) ||
                        (i == noutputs-1 && total_sz == sz_a)`)
                argshapeinfo_t {idx=t_out_i, shape=Ast.dlshape_t {
                    layout=shape.layout, shape=shape_i}, typ=typ,
                    dynamic=!constshape }
            } |]
        }
        else {
            assert(`sz_a % noutputs == 0`)
            val out_shape = shape.shape.copy()
            out_shape[axis] = sz_a / noutputs
            [| for t_out_i@i <- t_out {
                argshapeinfo_t {idx=t_out_i, shape=Ast.dlshape_t {
                    layout=shape.layout, shape=out_shape}, typ=typ,
                    dynamic=false }
            } |]
        }
    | DL_Squeeze {t_inp, t_axes, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axes = if t_axes != 0 {int(net.gettensor(t_axes))} else {mkrange(ndims)}
        val constshape = net.isconst(t_axes)
        val out_shape_0 = shape.shape.copy()
        val fold out_ndims = ndims for axis <- axes {
            val axis = Ast.normalize_axis(axis, ndims)
            if shape.shape[axis] == 1 {
                assert(`out_shape_0[axis] >= 0`)
                out_shape_0[axis] = -1
                out_ndims - 1
            } else {out_ndims}
        }
        var j = 0
        val out_shape = array(out_ndims, 0)
        for i <- 0:ndims {
            if out_shape_0[i] >= 0 {
                out_shape[j] = out_shape_0[i]
                j += 1
            }
        }
        [| argshapeinfo_t {idx=t_out, shape=Ast.dlshape_t {
            layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=!constshape } |]
    | DL_Tile {t_inp, t_repeats, t_out} =>

    | DL_TopK {axis, t_inp, t_K, t_out, t_out_ind} =>
    | DL_Transpose {perm, t_inp, t_out} =>
    | DL_Unsqueeze {t_inp, t_axes, t_out} =>
    }} catch {
    | AssertError =>
        throw DLError(f"shape inference: {name} (op={opname}): assertion failed")
    | Fail(msg) =>
        throw DLError(f"shape inference: {name} (op={opname}): {msg}")
    }
}
