/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Computes shapes of all intermediate tensors
import Ast, Hashmap

type argshapeinfo_t =
{
    idx: int // index of argument
    shape: Ast.nnshape_t // inferenced shape (may contain '?' or other vars)
    typ: Ast.nntyp_t // type
    dynamic: bool   // whether the argument shape is dynamic
                    // (i.e. it depends on the actual content of inputs,
                    // not only on their shapes (see, e.g. ONNX op "NonZero"))
}

fun infer(model: Ast.nnmodel_t, op: Ast.nnop_t): argshapeinfo_t []
{
    val (name, opname) = op.name()

    fun get_shape_typ(argidx: int)
    {
        val t = model.get_tensor(argidx)
        (t.shape, t.elemtype())
    }

    fun get_shape(argidx: int) = get_shape_typ(argidx).0

    fun copy_shape_typ(t_inp: int, t_out: int) {
        val (shape, typ) = get_shape_typ(t_inp)
        argshapeinfo_t { idx=t_out, shape=shape, typ=typ, dynamic=false }
    }

    fun infer_pooling_shape(ceil_mode: bool, dilations: int [], kernel_shape: int [],
                            pads: int [], strides: int [], t_inp: int, t_out: int)
    {
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size() // convolution may produce a tensor of different size than output,
                                    // but it will always have the same dimensionality, regardless of the layout
        assert(`shape.shape.size() >= 3`)
        val (c_start, c_end) = shape.get_spatial_channel_range()
        val nspatdims = c_end - c_start
        val out_shape = [for i <- 0:ndims {
                if i < c_start || i >= c_end {shape.shape[i]} // copy # of channels 1:1
                else { // one of spatial dimensionalities
                    val i1 = i - c_start
                    val inpsz = shape.shape[i]
                    val pad = pads[i1] + pads[i1+nspatdims]
                    val nelems = double(inpsz + pad - dilations[i1]*(kernel_shape[i1] - 1) - 1)/strides[i1] + 1
                    if ceil_mode {ceil(nelems)} else {floor(nelems)}
                }
            }]
        argshapeinfo_t {
            idx=t_out,
            shape=Ast.nnshape_t {layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=false
        }
    }

    //val (inp, _) = op.get_inputs_outputs()
    //println(f"inferring shape for {op.name()}: inp #0 shape={model.get_tensor(inp[0]).shape}")

    try {
    match op {
    | Ast.NN_Nop => []
    | Ast.NN_AvgPool {ceil_mode, dilations, kernel_shape, pads, strides, t_inp, t_out} =>
        [infer_pooling_shape(ceil_mode, dilations, kernel_shape,
            pads, strides, t_inp, t_out)]
    | Ast.NN_BatchNorm {t_inp, t_out} =>
        [copy_shape_typ(t_inp, t_out)]
    | Ast.NN_Cast {to, t_inp, t_out} =>
        val shape = get_shape(t_inp)
        [argshapeinfo_t {idx=t_out, shape=shape, typ=to, dynamic=false}]
    | Ast.NN_Clip {t_inp, t_min, t_max, t_out} =>
        assert(`model.isscalar(t_min)`)
        assert(`model.isscalar(t_max)`)
        [copy_shape_typ(t_inp, t_out)]
    | Ast.NN_Concat { axis, t_inp, t_out } =>
        val (shape0, typ0) = get_shape_typ(t_inp[0])
        val ndims = size(shape0.shape)
        val axis = Ast.normalize_axis(axis, ndims)
        val out_shape = shape0.shape.copy()
        val fold out_shape_a = 0 for t_inp_i@i <- t_inp {
            val (shape_i, typ_i) = get_shape_typ(t_inp_i)
            assert(`size(shape_i.shape) == size(shape0.shape)`)
            assert(`typ_i == typ0`)
            out_shape[axis] = shape_i.shape[axis]
            assert(`out_shape == shape_i.shape`)
            out_shape_a + shape_i.shape[axis]
        }
        out_shape[axis] = out_shape_a
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=shape0.layout,
            shape=out_shape}, typ=typ0, dynamic=false}]
    | Ast.NN_ConstantOfShape {value, t_shape, t_out} =>
        assert(`value.isscalar()`)
        val typ = value.elemtype()
        val value = float(value.data)
        val out_shape = int(model.get_tensor(t_shape))
        val const_shape = model.isconst(t_shape)
        assert(`value.size() == 1`)
        [argshapeinfo_t {idx=t_out,
            shape=Ast.nnshape_t {layout=Ast.NN_Layout_Unknown, shape=out_shape},
            typ=typ, dynamic=!const_shape}]
    | Ast.NN_Conv {attr={kernel_shape, pads, strides, dilations, group},
        t_inp, t_weights, t_out, t_passby} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size() // convolution may produce a tensor of different size than output,
                                    // but it will always have the same dimensionality, regardless of the layout
        val wshape = get_shape(t_weights)
        val wndims = wshape.shape.size()
        assert(`shape.shape.size() >= 3`)
        val N = shape.shape[0]
        val (c_start, c_end) = shape.get_spatial_channel_range()
        val nspatdims = c_end - c_start
        val out_shape = [for i <- 0:ndims {
                if i == 0 {N} // just copy N, the batch size
                else if i < c_start {wshape.shape[0]} // we have NCHW or NCHWxc: take K from from wshape
                else if i >= c_end { // we have either NHWC or NCHWxc: take K or k from wshape
                    wshape.shape[wndims-1]
                } else { // one of spatial dimensionalities
                    val i1 = i - c_start
                    val inpsz = shape.shape[i]
                    val pad = pads[i1] + pads[i1+nspatdims]
                    (inpsz + pad - dilations[i1]*(kernel_shape[i1] - 1) - 1)/strides[i1] + 1
                }
            }]
        if t_passby > 0 {
            val (pb_shape, pb_typ) = get_shape_typ(t_passby)
            assert(`pb_shape.shape == out_shape`)
            assert(`pb_typ == typ`)
        }
        [argshapeinfo_t {
            idx=t_out,
            shape=Ast.nnshape_t {layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=false // the shape of output tensor depends only on the shape of inputs,
                          // not on their content, even if the weights are computed dynamically,
                          // thus we set dynamic=false
        }]
    | Ast.NN_ConvTranspose {kernel_shape, pads, strides, dilations,
        out_shape, out_padding, group, t_inp, t_weights, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size() // convolution may produce a tensor of different size than output,
                                    // but it will always have the same dimensionality, regardless of the layout
        val wshape = get_shape(t_weights)
        val wndims = wshape.shape.size()
        assert(`shape.shape.size() >= 3`)
        val N = shape.shape[0]
        val (c_start, c_end) = shape.get_spatial_channel_range()
        val nspatdims = c_end - c_start
        val out_shape =
            if out_shape != [] {
                assert(`out_shape.size() == ndims`)
                out_shape
            } else {
                [for i <- 0:ndims {
                    if i == 0 {N} // just copy N, the batch size
                    else if i < c_start {wshape.shape[0]} // we have NCHW or NCHWxc: take K from from wshape
                    else if i >= c_end { // we have either NHWC or NCHWxc: take K or k from wshape
                        wshape.shape[wndims-1]
                    } else { // one of spatial dimensionalities
                        val i1 = i - c_start
                        val inpsz = shape.shape[i]
                        val pad = pads[i1] + pads[i1+nspatdims]
                        val extra_pad = if out_padding == [] {0} else {out_padding[i1]}
                        strides[i1] * (inpsz - 1) + extra_pad + ((kernel_shape[i1] - 1) * dilations[i1] + 1) - pad
                    }
                }]
            }
        [argshapeinfo_t {
            idx=t_out,
            shape=Ast.nnshape_t {layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=false // the shape of output tensor depends only on the shape of inputs,
                          // not on their content, even if the weights are computed dynamically,
                          // thus we set dynamic=false
        }]
    | Ast.NN_Dropout {t_inp, t_ratio, t_out} =>
        assert(`model.isfloatscalar(t_ratio)`)
        [copy_shape_typ(t_inp, t_out)]
    | Ast.NN_Elemwise {el_op, t_inp, t_out} =>
        val enable_broadcast = match el_op {
            | Ast.NN_Add | Ast.NN_And | Ast.NN_Div
            | Ast.NN_Equal | Ast.NN_Greater | Ast.NN_Less
            | Ast.NN_LessOrEqual | Ast.NN_GreaterOrEqual
            | Ast.NN_Max | Ast.NN_Mean | Ast.NN_Min
            | Ast.NN_Mul | Ast.NN_Or | Ast.NN_Pow
            | Ast.NN_Sub | Ast.NN_Xor => true
            | _ => false }
        val (shape0, typ0) = get_shape_typ(t_inp[0])
        val out_typ = match el_op {
            | Ast.NN_Equal | Ast.NN_Greater | Ast.NN_Less
            | Ast.NN_LessOrEqual | Ast.NN_GreaterOrEqual
            | Ast.NN_IsInf | Ast.NN_IsNaN => Ast.NN_Bool
            | _ => typ0
            }
        val fold out_shape = shape0 for t_inp_i@i <- t_inp {
            val (shape_i, typ_i) = get_shape_typ(t_inp_i)
            assert(`typ_i == typ0`)
            if enable_broadcast {
                out_shape.broadcast(shape_i)
            } else {
                assert(`shape0.shape == shape_i.shape`)
                out_shape.{layout = Ast.coerce_layouts(out_shape.layout, shape_i.layout)}
            }
        }
        [argshapeinfo_t {idx=t_out, shape=out_shape, typ=out_typ, dynamic=false}]
    | Ast.NN_Expand {t_inp, t_shape, t_out} =>
        val (shape0, typ0) = get_shape_typ(t_inp)
        val shape_arg = model.get_tensor(t_shape)
        val shape_arr = int(shape_arg.data)
        assert(`shape_arg.shape.shape.size() == 1`)
        val const_shape = model.isconst(t_shape)
        val out_shape = shape0.broadcast(Ast.nnshape_t {layout=shape0.layout, shape=shape_arr})
        assert(`out_shape.shape == shape_arr`)
        [argshapeinfo_t {idx=t_out, shape=out_shape, typ=typ0, dynamic=!const_shape}]
    | Ast.NN_Flatten {axis, t_inp, t_out} =>
        // [TODO] handle NCHWxc properly
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axis = Ast.normalize_axis(axis, ndims)
        val fold sz1 = 1, sz2 = 1 for sz@i <- shape.shape {
            if i < axis {(sz1*sz, sz2)} else {(sz1, sz2*sz)}
        }
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=Ast.NN_Layout_NC,
            shape=[sz1, sz2]}, typ=typ, dynamic=false}]
    | Ast.NN_Gather {axis, t_inp, t_ind, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val (ind_shape, ind_typ) = get_shape_typ(t_ind)
        val const_ind = model.isconst(t_ind)
        assert(`match ind_typ {Ast.NN_I32 | Ast.NN_I64 => true | _ => false}`)
        val r = shape.shape.size()
        val q = ind_shape.shape.size()
        val axis = Ast.normalize_axis(axis, r)
        val out_ndims = q + r - 1
        val out_shape = [for i <- 0:out_ndims {
            if i < q {
                ind_shape.shape[i]
            } else {
                val j = i - q
                if j < axis {shape.shape[j]}
                else {shape.shape[j+1]}
            }}]
        //println(f"Gather: axis={axis}, inpshape={shape.shape}, indshape={ind_shape.shape}, out_shape={out_shape}")
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=shape.layout,
            shape=out_shape}, typ=typ, dynamic=!const_ind}]
    | Ast.NN_Gemm {transA, transB, t_A, t_B, t_bias, t_out} =>
        val (ashape, atyp) = get_shape_typ(t_A)
        val (bshape, btyp) = get_shape_typ(t_B)
        val (cshape, ctyp) = get_shape_typ(t_bias)
        val ndims_c = cshape.shape.size()
        assert(`ashape.shape.size() == 2`)
        assert(`bshape.shape.size() == 2`)
        assert(`ndims_c <= 2`)
        assert(`atyp == btyp`)
        assert(`btyp == ctyp`)
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
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=bshape.layout,
            shape=[out_rows, out_cols]}, typ=atyp, dynamic=false}]
    | Ast.NN_GlobalAvgPool {t_inp, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val (c_start, c_end) = shape.get_spatial_channel_range()
        val out_shape = [for sz@d <- shape.shape {
                if c_start <= d < c_end {1} else {sz}
            }]
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {shape=out_shape,
            layout=shape.layout}, typ=typ, dynamic=false}]
    | Ast.NN_Identity {t_inp, t_out} =>
        [copy_shape_typ(t_inp, t_out)]
    | Ast.NN_If {t_inp} =>
        // we don't go into the if branches;
        // instead, we infere shapes inside one of the branch when we execute it
        val (shape, typ) = get_shape_typ(t_inp)
        println(f"If: t_inp's shape={shape.shape}, total={shape.total()}")
        assert(`shape.total() == 1`)
        assert(`typ == Ast.NN_Bool`)
        []
    | Ast.NN_LeakyRelu {t_inp, t_out} =>
        [copy_shape_typ(t_inp, t_out)]
    | Ast.NN_Loop {body, t_trip_count, t_cond_in, t_v_in, t_v_out} =>
        // we don't go recursively into the loop body;
        // instead, we infere shapes inside body while we execute the loop
        val {inpargs} = body
        val (tcount_shape, tcount_typ) = get_shape_typ(t_trip_count)
        val (tcond_shape, tcond_typ) = get_shape_typ(t_cond_in)
        val n_state_vars = t_v_in.size()
        val n_accums = t_v_out.size() - n_state_vars
        assert(`n_accums >= 0`)
        if t_trip_count > 0 {
            assert(`tcount_shape.total() == 1`)
            assert(`tcount_typ == Ast.NN_I32 || tcount_typ == Ast.NN_I64`)
        }
        if t_cond_in > 0 {
            assert(`tcond_shape.total() == 1`)
            assert(`tcond_typ == Ast.NN_Bool`)
        }
        // we don't calculate shapes for output tensors & accums (t_v_out);
        // accumulators are be created empty initially,
        // and are augmented after each iteration.
        [ for i <- -2:n_state_vars {
            val argidx = if i == -2 { t_trip_count }
                         else if i == -1 { t_cond_in }
                         else { t_v_in[i] }
            copy_shape_typ(argidx, inpargs[i+2])
        } ]
    | Ast.NN_LRN {t_inp, t_out} =>
        [copy_shape_typ(t_inp, t_out)]
    | Ast.NN_MaxPool { ceil_mode, dilations, kernel_shape, pads,
        strides, t_inp, t_out } =>
        [infer_pooling_shape(ceil_mode, dilations, kernel_shape,
            pads, strides, t_inp, t_out)]
    | Ast.NN_NonMaxSuppression {t_out} =>
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=Ast.NN_Layout_NC, shape=[1, 3]},
            typ=Ast.NN_I64, dynamic=true}]
    | Ast.NN_NonZero {t_inp, t_out} =>
        val shape = get_shape(t_inp)
        val ndims = shape.shape.size()
        val out_shape = Ast.nnshape_t {layout=Ast.NN_Layout_NC, shape=[ndims, 1]}
        [argshapeinfo_t {idx=t_out, shape=out_shape, typ=Ast.NN_I64, dynamic=true}]
    | Ast.NN_Range {t_start, t_limit, t_delta, t_out} =>
        val start = model.get_tensor(t_start)
        val typ = start.elemtype()
        val start = start.data
        val limit = model.get_tensor(t_limit).data
        val delta = model.get_tensor(t_delta).data
        assert(`start.total() == 1`)
        assert(`limit.total() == 1`)
        assert(`delta.total() == 1`)
        val start = start.double_scalar_or(0.)
        val limit = limit.double_scalar_or(0.)
        val delta = delta.double_scalar_or(1.)
        val nelems = max(ceil((limit - start)/delta), 0)
        val allconsts = model.isconst(t_start) && model.isconst(t_limit) && model.isconst(t_delta)
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=Ast.NN_Layout_NC, shape=[nelems]},
            typ=typ, dynamic=!allconsts}]
    | Ast.NN_Reduce {reduce_op, axes, keepdims, t_inp, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val dummy_val = -1000000
        val out_shape =
            if axes == [] {
                [ 1 ]
            } else {
                val out_shape = shape.shape.copy()
                for axis <- axes {
                    val axis = Ast.normalize_axis(axis, ndims)
                    out_shape[axis] = if keepdims {1} else {dummy_val}
                }
                if keepdims {out_shape}
                else {
                    var out_ndims = 0
                    for sz@i <- out_shape {
                        if sz != dummy_val {
                            out_shape[out_ndims] = sz
                            out_ndims += 1
                        }
                    }
                    out_shape[:out_ndims]
                }
            }
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=shape.layout, shape=out_shape},
            typ=typ, dynamic=false}]
    | Ast.NN_Reshape {allowzero, t_inp, t_shape, t_out} =>
        // [TODO] handle NCHWxc
        val (shape, typ) = get_shape_typ(t_inp)
        val (sshape, _) = get_shape_typ(t_shape)
        assert(`sshape.shape.size() == 1 || sshape.shape.size() == 0`)
        val new_shape = int(model.get_tensor(t_shape))
        val fold old_total = 1 for sz <- shape.shape {old_total*sz}
        val fold new_total=1, havem1=false for sz@i <- new_shape {
            if sz == -1 {
                if havem1 {throw Ast.NNError(f"shape inference: {name} (op={opname}): the new shape contains more than one -1")}
                (new_total, true)
            } else if sz == 0 {
                if allowzero {(new_total, havem1)}
                else {
                    val sz = shape.shape[i]
                    (new_total*sz, havem1)
                }
            } else {(new_total*sz, havem1)}
        }
        val m1sz = if havem1 {
            if old_total % new_total != 0 {
                throw Ast.NNError(f"shape inference: {name} (op={opname}): the new shape contains -1, \
                    but the computed size ({old_total}/{new_total}) is not an integer")
            }
            old_total/new_total } else {1}
        val out_shape = [for sz@i <- new_shape {
                if sz == -1 { m1sz }
                else if sz != 0 { sz }
                else if allowzero { 0 }
                else { shape.shape[i] }
            }]
        val const_shape = model.isconst(t_shape)
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=shape.layout, shape=out_shape},
            typ=typ, dynamic=!const_shape}]
    | Ast.NN_Resize { coord_trans, t_inp, t_scales, t_sizes, t_roi, t_out } =>
        // [TODO] handle NCHWxc properly
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        //println(f"Resize inp shape: {shape.shape}")
        val roi =
            if t_roi == 0 {
                ([] : float [])
            } else {
                val roi = float(model.get_tensor(t_roi))
                if roi == [] {
                    roi
                } else {
                    assert(`coord_trans == Ast.NN_CT_TFCropResize`)
                    assert(`roi.size() == ndims*2`)
                    roi
                }
            }
        val out_shape = if t_sizes != 0 {
            val sizes = int(model.get_tensor(t_sizes))
            //println(f"Resize sizes: {sizes}")
            assert(`sizes.size() == ndims`)
            sizes
        } else if t_scales != 0 {
            val scales = float(model.get_tensor(t_scales))
            assert(`scales.size() == ndims`)
            //println(f"Resize scales: {scales}")
            [for sz@i <- shape.shape, scale <- scales {
                val sz =
                    if t_roi == 0 {float(sz)}
                    else {sz*(roi[i + ndims] - roi[i])}
                floor(sz*scale)
            }]
        } else {
            throw Ast.NNError(f"shape inference: {name} (op={opname}): both scales and sizes are missing")
        }
        //println(f"Resize out shape: {out_shape}")
        // [TODO] when 't_sizes' is computed using 'Shape' operation and some operations
        // on the shape, it could be treated as a constant input of 'Resize' in some cases,
        // i.e. 'dynamic' could be set to false.
        val const_shape =
            (t_roi == 0 || model.isconst(t_roi)) &&
            (t_sizes == 0 || model.isconst(t_sizes)) &&
            (t_scales == 0 || model.isconst(t_scales))
        /*println(f"resize const_shape: roi: {t_roi == 0 || model.isconst(t_roi)}, \
                                      sizes: {t_sizes == 0 || model.isconst(t_sizes)} \
                                      scales: {t_scales == 0 || model.isconst(t_scales)}")*/
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=shape.layout, shape=out_shape},
            typ=typ, dynamic=!const_shape}]
    | Ast.NN_RoiAlign {output_height, output_width, t_inp, t_rois, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val (roi_shape, roi_typ) = get_shape_typ(t_rois)
        assert(`ndims == 4 || ndims == 5`)
        assert(`roi_shape.shape.size() == 2`)
        assert(`roi_shape.shape[1] == 4`)
        assert(`typ == roi_typ`)
        val nrois = roi_shape.shape[0]
        val C = shape.get_num_channels()
        val out_shape = [ nrois, C, output_height, output_width ]
        // [TODO] probably, if we get tensor with NCHWxc layout on input,
        // we could keep NCHWxc layout in the output as well
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=Ast.NN_Layout_NCHW,
            shape=out_shape}, typ=typ,
            dynamic=false // t_rois is likely a variable-size tensor,
                          // but this operation itself produces fixed-size output
                          // given fixed-size inputs
            }]
    | Ast.NN_Scatter {t_data, t_out} =>
        // [TODO] check t_updates and t_indices shape,
        // but this can also be done in the actual operation implementation
        [copy_shape_typ(t_data, t_out)]
    | Ast.NN_Shape {start, end, t_inp, t_out} =>
        val shape = get_shape(t_inp)
        val ndims = shape.shape.size()
        val start = Ast.normalize_axis(start, ndims)
        val end = if end >= ndims {ndims} else {Ast.normalize_axis(end, ndims)}
        val out_shape = [max(end-start, 0)]
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=Ast.NN_Layout_NC,
            shape=out_shape}, typ=Ast.NN_I64, dynamic=false}]
    | Ast.NN_Slice {t_inp, t_starts, t_ends, t_axes, t_steps, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axes = if t_axes != 0 {int(model.get_tensor(t_axes))} else {mkrange(ndims)}
        val naxes = axes.size()
        val starts = int(model.get_tensor(t_starts))
        val ends = int(model.get_tensor(t_ends))
        val steps = if t_steps != 0 {int(model.get_tensor(t_steps))} else {array(naxes, 1)}
        assert(`starts.size() == naxes`)
        assert(`ends.size() == naxes`)
        assert(`steps.size() == naxes`)
        val out_shape = shape.shape.copy()
        for axis <- axes, start <- starts, end <- ends, step <- steps {
            val axis = Ast.normalize_axis(axis, ndims)
            val sz_a = shape.shape[axis]
            val start = min((if start < 0 {start + sz_a} else {start}), sz_a)
            val end = min((if end < 0 {end + sz_a} else {end}), sz_a)
            assert(`step != 0`)
            val nelems = if step > 0 {(end - start + step-1)/step}
                         else {(start - end - step-1)/(-step)}
            assert(`nelems >= 0`)
            out_shape[axis] = nelems
        }
        val const_shape =
            (t_starts == 0 || model.isconst(t_starts)) &&
            (t_ends == 0 || model.isconst(t_ends)) &&
            (t_axes == 0 || model.isconst(t_axes)) &&
            (t_steps == 0 || model.isconst(t_steps))
        [argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {layout=Ast.NN_Layout_NCHW,
            shape=out_shape}, typ=typ, dynamic=!const_shape}]
    | Ast.NN_SoftMax {t_inp, t_out} =>
        [copy_shape_typ(t_inp, t_out)]
    | Ast.NN_Split {axis, t_inp, t_split, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axis = Ast.normalize_axis(axis, ndims)
        val sz_a = shape.shape[axis]
        val noutputs = t_out.size()
        if t_split != 0 {
            val splits = int(model.get_tensor(t_split))
            val const_shape = model.isconst(t_split)
            var total_sz = 0
            assert(`splits.size() == noutputs`)
            [for t_out_i@i <- t_out {
                val shape_i = shape.shape.copy()
                val sz_i = splits[i]
                assert(`sz_i > 0`)
                shape_i[axis] = sz_i
                total_sz += sz_i
                assert(`(i < noutputs-1 && total_sz < sz_a) ||
                        (i == noutputs-1 && total_sz == sz_a)`)
                argshapeinfo_t {idx=t_out_i, shape=Ast.nnshape_t {
                    layout=shape.layout, shape=shape_i}, typ=typ,
                    dynamic=!const_shape }
            }]
        }
        else {
            assert(`sz_a % noutputs == 0`)
            val out_shape = shape.shape.copy()
            out_shape[axis] = sz_a / noutputs
            [for t_out_i@i <- t_out {
                argshapeinfo_t {idx=t_out_i, shape=Ast.nnshape_t {
                    layout=shape.layout, shape=out_shape}, typ=typ,
                    dynamic=false }
            }]
        }
    | Ast.NN_Squeeze {t_inp, t_axes, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axes = if t_axes != 0 {int(model.get_tensor(t_axes))} else {mkrange(ndims)}
        val const_shape = model.isconst(t_axes)
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
        [ argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {
            layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=!const_shape }]
    | Ast.NN_Tile {t_inp, t_repeats, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val repeats = int(model.get_tensor(t_repeats))
        assert(`repeats.size() == ndims`)
        val out_shape = [for sz <- shape.shape, r <- repeats { assert(`r >= 0`); sz*r }]
        val const_shape = model.isconst(t_repeats)
        [ argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {
            layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=!const_shape }]
    | Ast.NN_TopK {axis, t_inp, t_K, t_out, t_out_ind} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val tK = model.get_tensor(t_K).data
        assert(`tK.total() == 1`)
        val K = match tK {
            | Ast.NN_Data_I64 tK_data => int(tK_data[0])
            | _ => throw Ast.NNError("incorrect type of K tensor in topK: INT64 is expected\n")
            }
        assert(`K >= 0`)
        val ndims = shape.shape.size()
        val axis = Ast.normalize_axis(axis, ndims)
        val out_shape = shape.shape.copy()
        out_shape[axis] = min(K, shape.shape[axis])
        val constK = model.isconst(t_K)
        [ argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {
            layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=!constK },
           argshapeinfo_t {idx=t_out_ind, shape=Ast.nnshape_t {
            layout=shape.layout, shape=out_shape}, typ=Ast.NN_I64,
            dynamic=!constK }]
    | Ast.NN_Transpose {perm, t_inp, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val perm = if perm != [] {int(perm)} else {mkrange(ndims-1, -1, -1)}
        assert(`perm.size() == ndims`)
        val out_shape = [for axis <- 0:ndims {
            val axis = Ast.normalize_axis(axis, ndims)
            shape.shape[perm[axis]]}]
        [ argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {
            layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=false}]
    | Ast.NN_Unsqueeze {t_inp, t_axes, t_out} =>
        val (shape, typ) = get_shape_typ(t_inp)
        val ndims = shape.shape.size()
        val axes = int(model.get_tensor(t_axes))
        val out_ndims = ndims + axes.size()
        val out_shape = array(out_ndims, 0)
        for axis <- axes {
            val axis = Ast.normalize_axis(axis, out_ndims)
            assert(`out_shape[axis] == 0`)
            out_shape[axis] = 1
        }
        var j = 0
        for i <- 0:out_ndims {
            if out_shape[i] == 0 {
                out_shape[i] = shape.shape[j]
                j += 1
            }
        }
        val constaxes = model.isconst(t_axes)
        [ argshapeinfo_t {idx=t_out, shape=Ast.nnshape_t {
            layout=shape.layout, shape=out_shape}, typ=typ,
            dynamic=!constaxes }]
    }} catch {
    | AssertError =>
        throw Ast.NNError(f"shape inference: {name} (op={opname}): assertion failed")
    | Fail(msg) =>
        throw Ast.NNError(f"shape inference: {name} (op={opname}): {msg}")
    }
}
