/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Runs the inference on user-provided data
import Hashmap, Sys
import Ast, InferShapes, OpConv, RunOp

fun run(net: Ast.nnet_t, inputs: (string, Ast.nntensor_t) []/*,
            cb_before: Ast.op_callback_t?, cb_after: Ast.op_callback_t?*/,
        ~outputs: (string, Ast.nntensor_t) [] = []):
    (string, Ast.nntensor_t) []
{
    var empty_names = true
    OpConv.reset_total_time()

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
        //println(f"assigned input #{i} to {net.args[argidx].name}")
        val arg = net.args[argidx]
        //println(f"input #{i}: {arg}")
        assert(arg.argkind == Ast.NN_Arg_Input)
        assert(arg.typ == t.elemtype())
        assert(arg.shape.layout == t.shape.layout || arg.shape.layout == Ast.NN_Layout_Unknown)
        net.tensors[argidx] = t
    }

    //println("running main graph")
    run_graph(net, net.graph, outputs)
    OpConv.update_total_time()

    // collect outputs
    [for argidx <- net.graph.outargs {
        val arg = net.args[argidx]
        assert(arg.argkind == Ast.NN_Arg_Output)
        (arg.name, net.tensors[argidx])
    }]
}

fun run_graph(net: Ast.nnet_t, graph: Ast.nngraph_t, outputs: (string, Ast.nntensor_t) [])
{
    for op <- graph.prog {
        //println(f"preparing to run op {op.name()}")
        val oinfo = InferShapes.infer(net, op)
        for oi@outidx <- oinfo {
            val {idx=argidx, shape, typ} = oi
            //println(f"   output #{outidx} ('{net.args[argidx].name}'): {oi}")
            val shape0 = net.tensors[argidx].shape
            val typ0 = net.tensors[argidx].elemtype()
            if shape != shape0 || typ != typ0 {
                val arg = net.args[argidx]
                net.args[argidx].shape.layout = shape.layout
                if arg.argkind == Ast.NN_Arg_Output {
                    net.tensors[argidx] = Ast.make_tensor(shape, typ)
                } else if arg.argkind == Ast.NN_Arg_Temp {
                    val bufidx = net.bufidxs[argidx]
                    //println(f"   fit into buf #{bufidx} with shape={shape}, typ={typ}, data of {net.tensors[argidx].data.total()} elems, buf of {net.buffers[bufidx].size()} bytes")
                    val (data, buf) = Ast.fit(shape, typ, net.tensors[argidx].data, net.buffers[bufidx])
                    net.tensors[argidx].data = data
                    net.tensors[argidx].shape = shape
                    net.buffers[bufidx] = buf
                } else {
                    throw Ast.NNError(f"unexpected argkind={arg.argkind} of the output {outidx} of {op.name()}")
                }
            }
        }
        //println(f"running op '{op.name()}'")
        //val t = Sys.tick_count()
        RunOp.run_op(net, op)
        //val t = Sys.tick_count() - t
        //println(f"{op.name()}: {round(t*1000./Sys.tick_frequency(), 2)}ms")
        /*for oi@outidx <- oinfo {
            val {idx=argidx} = oi
            match net.get_tensor(argidx).data {
            | Ast.NN_Data_FP32 data =>
                match find_opt(for x <- data {isnan(x)}) {
                | Some _ => throw Ast.NNError(f"result of '{op.name()}' has NaN's!")
                | _ => {}
                }
            | _ => {}
            }
        }*/

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
