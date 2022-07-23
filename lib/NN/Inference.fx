/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Runs the inference on user-provided data
import Hashmap, Json, LexerUtils as Lxu, Sys
import Ast, InferShapes, RunOp

fun run(model: Ast.nnmodel_t, inputs: (string, Ast.nntensor_t) []/*,
            cb_before: Ast.op_callback_t?, cb_after: Ast.op_callback_t?*/,
        ~outputs: (string, Ast.nntensor_t) [] = []):
    (string, Ast.nntensor_t) []
{
    var empty_names = true
    val ninputs = inputs.size()

    assert(`ninputs == model.graph.inpargs.size()`)

    // assign input tensors
    for (inpname, inp)@i <- inputs {
        // check that either all input names are empty or none of them
        if i == 0 {empty_names = inpname == ""}
        else {assert(`empty_names == (inpname == "")`)}
        val argidx =
            if inpname == "" {model.graph.inpargs[i]}
            else {
                val argidx = model.argnames.find_opt(inpname).value_or(-1)
                if argidx < 0 {
                    throw Ast.NNError(f"cannot find input '{inpname}'; available inputs are: {model.get_input_names()}")
                }
                argidx
            }
        //println(f"input #{i} is set to {model.args[argidx].name}")
        val arg = model.args[argidx]
        val inp_typ = inp.elemtype()
        //println(f"input #{i}: {arg}")
        assert(`arg.argkind == Ast.NN_Arg_Input`)
        assert(`arg.typ == inp_typ`)
        assert(`arg.shape.layout == inp.shape.layout || arg.shape.layout == Ast.NN_Layout_Unknown`)
        model.fit(argidx, inp.shape, inp_typ)
        Ast.copy_tensor_data(inp.data, model.tensors[argidx].data)
    }

    //println("running main graph")
    run_graph(model, model.graph, outputs)

    // collect outputs
    [for argidx <- model.graph.outargs {
        val arg = model.args[argidx]
        assert(`arg.argkind == Ast.NN_Arg_Output`)
        (arg.name, model.tensors[argidx])
    }]
}

fun dump_arg(model: Ast.nnmodel_t, prefix: string, idx: int, argidx: int, dumpdata: bool)
{
    val t = model.get_tensor(argidx)
    val name = model.args[argidx].name
    val etyp = t.elemtype()
    val sh = join_embrace("{", "}", ",", [for sz <- t.shape.shape {string(sz)}])
    println(f"{prefix} {idx} Name: {name}\n Buf: {model.bufidxs[argidx]}\n Type: {etyp}\n Shape: {sh}")
    if dumpdata {
        println(string(t))
    }
}

fun run_graph(model: Ast.nnmodel_t, graph: Ast.nngraph_t, outputs: (string, Ast.nntensor_t) [])
{
    val nops = graph.prog.size()
    var t = 0L
    for op@opidx <- graph.prog {
        if *model.profile {
            t = Sys.tick_count()
        }
        val (opname, opkind) = op.name()
        val (inps, outs) = op.get_inputs_outputs()
        if *model.trace {
            println("-----------")
            println(f"'{graph.name}'[{opidx}/{nops}]. {opkind} node: {opname}")
            for inp@i <- inps {
                dump_arg(model, "Input", i, inp, false)
            }
        }
        //val noindent=""
        //println(f"preparing to run op {model.op2str(op, noindent)}")
        /*match op {
        | Ast.NN_Transpose {name, t_inp, t_out, perm} when name=="where_op_added__679" =>
            println(f"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            println(f"TRANSPOSE: t_inp={model.tensor2str(model.get_tensor(t_inp), true)}, perm={perm}, t_out={model.tensor2str(model.get_tensor(t_out), true)}")
        | _ => {}
        }*/
        val oinfo = InferShapes.infer(model, op)
        for oi@outidx <- oinfo {
            val {idx=argidx, shape, typ} = oi
            //println(f"output #{outidx} ('{model.args[argidx].name}'): {oi}")
            if model.bufidxs[argidx] >= 0 {
                //println(f"fit buf #{model.bufidxs[argidx]} to fit arg #{argidx} with shape #{shape.shape}")
                model.fit(argidx, shape, typ)
            }
        }
        //println(f"[op #{opidx}/{nops} in graph '{graph.name}']. running op '{op.name()}'")
        match op {
        | Ast.NN_If {then_branch, else_branch, t_inp, t_out} =>
            val inp = model.get_tensor(t_inp)
            val f = match inp.data {
                | Ast.NN_Data_Bool inp_data => inp_data[0]
                | _ => throw Ast.NNError("Incorrect input of If operation, a boolean scalar is expected")
                }
            val branch = if f {then_branch} else {else_branch}
            run_graph(model, branch, outputs)
            val {outargs} = branch
            assert(`outargs.size() == t_out.size()`)
            for br_outidx <- outargs, outidx <- t_out {
                val br_out = model.tensors[br_outidx]
                model.fit(outidx, br_out.shape, br_out.elemtype())
                model.copy_tensor_data(br_outidx, outidx)
            }
        | Ast.NN_Loop { body, t_trip_count, t_cond_in, t_v_in, t_v_out } =>
            val {inpargs, outargs} = body
            val n_state_vars = t_v_in.size()
            val n_accums = t_v_out.size() - n_state_vars
            assert(`n_accums >= 0`)
            var trip_count =
                if t_trip_count > 0 {
                    val t_data = model.tensors[t_trip_count].data
                    assert(`t_data.total() == 1`)
                    match t_data {
                    | Ast.NN_Data_Empty => None
                    | Ast.NN_Data_I32 data => Some(int64(data[0]))
                    | Ast.NN_Data_I64 data => Some(int64(data[0]))
                    | _ => throw Ast.NNError("Loop's trip_count (if any) is expected to be I32/I64 scalar")
                    }
                } else {None}
            val trip_count0 = trip_count
            var loop_condition =
                if t_cond_in > 0 {
                    val t_data = model.tensors[t_cond_in].data
                    assert(`t_data.total() == 1`)
                    match t_data {
                    | Ast.NN_Data_Empty => true
                    | Ast.NN_Data_Bool data => data[0]
                    | _ => throw Ast.NNError("Loop's cond_in (if any) is expected to be Bool scalar")
                    }
                } else {true}
            // copy trip_count, t_cond_in & t_v_in tensors
            // to the respective graph inpargs
            for i <- -2:n_state_vars {
                val v_inp = if i == -2 { t_trip_count }
                            else if i == -1 { t_cond_in }
                            else { t_v_in[i] }
                val inparg = inpargs[i+2]
                if model.bufidxs[inparg] >= 0 {
                    model.copy_tensor_data(v_inp, inparg)
                }
            }

            var iter = 0
            while loop_condition && trip_count.value_or(1L) > 0L {
                if *model.trace {
                    println(f"================ LOOP ITERATION #{iter}/{trip_count0.value_or(-1L)} ================")
                }
                run_graph(model, body, outputs)
                val outarg_0 = outargs[0]
                trip_count = match trip_count {
                    | Some(i) => Some(i-1)
                    | _ => trip_count
                    }
                if outarg_0 > 0 {
                    val l_data = model.tensors[outarg_0].data
                    assert(`l_data.total() == 1`)
                    loop_condition = match l_data {
                        | Ast.NN_Data_Bool data => data[0]
                        | _ => throw Ast.NNError("Loop's cond_out (if any) is expected to be Bool scalar")
                        }
                }
                for i <- 0:n_state_vars+n_accums {
                    val isaccum = i >= n_state_vars
                    val outarg = outargs[i+1]
                    val v_out = t_v_out[i]
                    if model.bufidxs[v_out] >= 0 {
                        if !isaccum || iter == 0 {
                            val t = model.tensors[outarg]
                            val t_shape = t.shape
                            val shape1 = if isaccum {[1, \t_shape.shape]} else {t_shape.shape}
                            val new_shape = Ast.nnshape_t {layout=t_shape.layout, shape=shape1}
                            model.fit(v_out, new_shape, t.elemtype())
                            model.copy_tensor_data(outarg, v_out)
                        } else {
                            //println(f"accumulating '{model.args[outarg].name}'={model.tensors[outarg]} to '{model.args[v_out].name}'")
                            model.concat_inplace(outarg, v_out)
                        }
                    }
                }
                for i <- 0:n_state_vars {
                    val outarg = outargs[i+1]
                    val v_inp = inpargs[i+2]
                    if v_inp >= 0 && model.bufidxs[v_inp] >= 0 {
                        val t = model.tensors[outarg]
                        val t_shape = t.shape
                        model.fit(v_inp, t_shape, t.elemtype())
                        model.copy_tensor_data(outarg, v_inp)
                    }
                }
                iter += 1
            }
            if *model.trace {
                println(f"================ LOOP IS OVER ================")
            }
        | _ => RunOp.run_op(model, op)
        }
        if *model.profile {
            t = Sys.tick_count() - t
            val t_ms = t*1000./Sys.tick_frequency()
            println(f"TIME {op.name()}: {round(t_ms, 2)}ms")
        }
        /*for oi@outidx <- oinfo {
            val {idx=argidx} = oi
            match model.get_tensor(argidx).data {
            | Ast.NN_Data_FP32 data =>
                match find_opt(for x <- data {isnan(x)}) {
                | Some _ => throw Ast.NNError(f"result of '{op.name()}' has NaN's!")
                | _ => {}
                }
            | _ => {}
            }
        }*/
        /*match op {
        | Ast.NN_Clip {name, t_inp, t_out} when name == "FeatureExtractor/MobilenetV1/MobilenetV1/Conv2d_11_depthwise/Relu6" =>
            println(f"CLIP: t_inp={model.tensor2str(model.get_tensor(t_inp), true)}, t_out={model.tensor2str(model.get_tensor(t_out), true)}")
            throw Fail("reached 'Relu6'")
        | _ => {}
        }*/
        if *model.trace {
            println("-----------")
            for out@i <- outs {
                dump_arg(model, "Output", i, out, true)
            }
            println()
        }

        if outputs != [] {
            for oi@outidx <- oinfo {
                val {idx=argidx} = oi
                val name = model.args[argidx].name
                match find_opt(for (n,_)@i <- outputs {n == name}) {
                | Some((_, i)) =>
                    outputs[i].1 = model.tensors[argidx].copy()
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
    assert(`k > 0`)
    assert(`ndims == 1 || ndims == 2 || (ndims == 4 && shape[2] == 1 && shape[3] == 1)`)
    val (nrows, ncols) = if ndims == 1 {(1, shape[0])} else {(shape[0], shape[1])}
    val k = min(k, ncols)
    val data = float(t)
    assert(`data.size() == nrows*ncols`)
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

fun read_labels(fname: string) =
    try {
        match Json.parse_file(fname) {
        | Json.Map(entries) =>
            val fold maxk = 0 for (k, v) <- entries {
                val ik = k.to_int().value_or(-1)
                assert(ik >= 0)
                max(maxk, ik)
            }
            val labels_arr = array(maxk + 1, "unknown")
            for (k, v) <- entries {
                val ik = k.to_int().value_or(-1)
                val v = match v {
                    | Json.Str(s) => s
                    | _ => println(f"invalid label for {k}"); throw Fail("")
                    }
                labels_arr[ik] = v
            }
            labels_arr
        | _ => println("invalid labels"); throw Fail("")
        }
    } catch {
    | Lxu.LexerError(lloc, msg) => println(f"{fname}:{lloc.0}: error: {msg}"); throw Fail("")
    }
