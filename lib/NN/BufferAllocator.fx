/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Allocates buffers for all intermediate tensors of the graph/model

/*
The algorithm is quite simple, but there are some nuances in the attempt to re-use memory more efficiently:

All operations' arguments in graph and sub-graphs are classified into 4 categories:
a) inputs, b) outputs, c) constants and d) temporary values.

Except for the buffers, each other argument gets its own dedicated storage,
which makes things more clear and predictable.
So, this algorithm allocates memory only for the temporary tensors.

During the inference process, each temporary tensor is computed
by one of the operations and then used by zero or more subsequent operations (only as input).
The obvious example of a model where a tensor could be used more than once is Resnet.
Once during the inference process a tensor is used for the last time and
won't be used by any subsequent operations, the memory buffer could be re-used for
other arguments. We want to assign each temporary tensor to some temporary buffer.

We do it using 2-stage algorithm:

1. First, we calculate, how many times each argument is used and store the counters into 'usecounts'.
2. Second, we scan the operations in topologically sorted order
    2.0. Sanity check: We check that each input argument of the operation is either input or constant,
         or it's a temporary tensor with the buffer assigned to it.
    2.1. For in-place reshape operations, such as squeeze/unsqueeze/flatten etc, or element-wise operations,
         we check whether the input is a temporary value and is not used in any subsequent operations.
         If the checks pass, we assign output argument to the same buffer as input
         (or one of the inputs in the case of binary, ternary etc. element-wise operations)
    2.2. Otherwise, for each output argument of operation, which is not a network output argument.
         we assign the most recently-used free buffer (i.e. the top buffer in the stack of free buffers).
         If there is no free buffers, i.e. the stack is empty, we create a new buffer, and use it.
    2.3. For each input we decrement the corresponding element of 'usecounts'. If the counter reaches 0 and the input
         is not aliased with one of the outputs (see 2.1), we push the corresponding buffer index into the stack of free buffers.
    2.4. In the case of in-place operations and sometimes when using subgraphs (e.g. in If, Loop operations) we may
         re-use the same buffer for several arguments
         (which can be ouputs for some operations and inputs for some subsequent operations).
         In order to handle it all properly, during the buffer assignment algorithm we maintain use counter for each
         buffer, which should not be confused with use counters for arguments. A pool of free buffers contains zero or
         more "spare" buffers with 0 use counts. A buffer in use has the corresponding usage count > 0.
         When some argument is not needed anymore, and if it's not a constant, it decrements the usage counter of the buffer
         where it resides. When the counter reaches zero, we return the buffer into the pool of free buffers and then
         we can reuse the same buffer for another argument (or probably different shape and/or type, see below).
         In principle, we could 'protect' some buffers from the premature release and re-use by incrementing the use counts
         of the respective arguments that reside in those buffers, but that would make the bookkeeping much more complex.

Please, note that when we reuse buffers, we don't check any types, shape or a total size of the buffer needed.
We reallocate each buffer at runtime to fit each single argument that it's used for. For example, let's say the buffer #3
is used for arguments #5 (10x10x10 FP32), #10 (6x6x32 FP32) and #14 (300x1 UINT64). Then during the the first run of
the inference the buffer #3 will be reallocated from 0 bytes to 1000*4 bytes to fit arg #10,
then from 4000 to 6*6*32*4=4608 bytes to fit arg #10 and then it will fit arg #14 without reallocations.
During the second run of inference with the same resolution input the buffer will not be reallocated.

The reallocation is done using NN.Ast.fit() function.
*/

import Ast, Dynvec

fun assign_buffers(model: Ast.nnmodel_t)
{
    val nargs = model.args.size()
    val usecounts = model.use_counts()
    var nbufs = 0
    val freebufs = Dynvec.create(0, 0)
    val buf_usecounts = Dynvec.create(0, 0)
    val bufidxs = array(nargs, -1)

    /*
        Let's define 3 workhorse functions that would abstract the use and bookkeeping of buffers:
        1. get_free_buffer() takes the first spare buffer from the pool of free buffers. Since
           we don't necessarily know the shape/type of tensor type at this stage, this is quite
           reasonable behaviour - we cannot do anything more complex that that. On the positive side,
           since the pool of free buffers operates like a stack, the first free buffer is the most
           recently released buffer, so we improve cache locality using this pattern.
           When we don't have spare buffers in the pool, we "virtually" create a new buffer
           (by incrementing the number of buffers used) and return it.

           For the retrieved buffer we set its use count to 1.
        2. release_buffer(bufidx) decrements the buffer use count and returns it to the pool
           of free buffers as long as the use counter reaches 0.
        3. share_buffer(from_arg, to_arg) takes two argument indices.
           It makes argument 'to_arg' use the same buffer as for 'from_arg'.
           Use counter for the assigned to 'to_arg' buffer (if any) is decremented.
           Use counter for the 'from_arg' buffer is incremented, correpondingly.
    */
    fun get_free_buffer()
    {
        if freebufs.count == 0 {
            freebufs.do_push(nbufs)
            buf_usecounts.do_push(0)
            //println(f"added buf #{nbufs}: {freebufs.data[:freebufs.count]}")
            nbufs += 1
        }
        val outidx = freebufs.pop()
        buf_usecounts.data[outidx] = 1
        //println(f"use buf #{outidx}")
        outidx
    }

    fun release_buffer(bufidx: int)
    {
        if bufidx >= 0 {
            assert(`buf_usecounts.data[bufidx] > 0`)
            buf_usecounts.data[bufidx] -= 1
            if buf_usecounts.data[bufidx] == 0 {
                freebufs.do_push(bufidx)
            }
        }
    }

    fun share_buffer(from_arg: int, to_arg: int)
    {
        assert(`!model.isconst(from_arg) && !model.isconst(to_arg)`)
        val from_buf = bufidxs[from_arg]
        assert(`from_buf >= 0`)
        val to_buf = bufidxs[to_arg]
        bufidxs[to_arg] = from_buf
        buf_usecounts.data[from_buf] += 1
        if to_buf >= 0 {
            release_buffer(to_buf)
        }
    }

    /*println("=================== arg use counts =====================")
    for i <- 0:nargs {
        println(f"{model.args[i].name}: count={usecounts[i]}")
    }
    println("========================================================")*/

    fun assign_buffers_(graph: Ast.nngraph_t): void
    {
        val {prog} = graph
        for op <- prog {
            if op == Ast.NN_Nop {continue}
            val (inps, outs) = op.get_inputs_outputs()
            /*
                Determine if we can possibly re-use some of the input buffers for the output as well,
                in other words, whether we can run the operation in-place.
                Not only it saves memory, but it can also:
                   1. improve L2/L3 cache re-use
                   2. effectively convert some copy/re-shape operations
                      (Identity, Flatten, Reshape, Squeeze, Unsqueeze)
                      into Nop (no-operation).
            */
            val (inplace_op, reuse_idx) = match op {
                | Ast.NN_BatchNorm _ | Ast.NN_Clip _ | Ast.NN_Dropout _
                | Ast.NN_Flatten _ | Ast.NN_Identity _ | Ast.NN_LeakyRelu _
                | Ast.NN_Reshape _ | Ast.NN_Squeeze _ | Ast.NN_Unsqueeze _ =>
                    (usecounts[inps[0]] == 1 && model.istemp(inps[0]), inps[0])
                | Ast.NN_Scatter _ =>
                    (usecounts[inps[0]] == 1 && inps[1] != inps[0] && inps[2] != inps[0] && model.istemp(inps[0]), inps[0])
                // because of posssible broadcasting we cannot safely perform
                // element-wise operations in-place, unless there is just one input.
                | Ast.NN_Elemwise {el_op, t_inp} when t_inp.size() == 1 && (match el_op {
                    | Ast.NN_IsInf | Ast.NN_IsNaN => false | _ => true}) =>
                    match find_opt(for argidx <- inps {usecounts[argidx] == 1 && model.istemp(argidx)}) {
                    | Some(argidx) => (true, argidx)
                    | _ => (false, -1)
                    }
                | Ast.NN_Conv {t_passby} when
                    t_passby > 0 && usecounts[t_passby] == 1 &&
                    model.istemp(t_passby) =>
                    (true, t_passby)
                | _ => (false, -1)
                }
            /*
               Unless the operation is in-place, assign buffers for each output.
               We do it before we recursively process subgraphs inside If/Loop/Scan.
               this way we avoid any possible influence of buffer allocation inside a subgraph
               to the parent graphs.
            */
            if inplace_op && bufidxs[outs[0]] < 0 &&
               model.args[outs[0]].argkind != Ast.NN_Arg_Output {
                share_buffer(reuse_idx, outs[0])
            } else {
                for argidx <- outs {
                    if bufidxs[argidx] < 0 {
                        bufidxs[argidx] = get_free_buffer()
                    }
                }
            }
            /*fun gen_info(args: int []) =
                [for i <- args {
                    model.args[i].name +
                        (if i == 0 {""} else if model.isconst(i) {": const"} else
                        {
                            val bufidx = bufidxs[i]
                            assert(`bufidx >= 0 && buf_usecounts.data[bufidx] > 0`)
                            f": buf #{bufidx}"
                        })
                }]
            println(f"name={op.name()}, inplace={inplace_op}, inps={gen_info(inps)}, outs={gen_info(outs)}")
            println(f"\tbuf_usecounts: {buf_usecounts.data[:nbufs]}")*/

            match op {
            /*
                Pre-allocate buffers for the output nodes of then- and else- branches.
                We try to alias them with the corresponding t_out[i] elements, so
                that we save one copy operation.
                [TODO]
                It's not the most optimal buffer allocation.
                In the ideal case, e.g. when both then- and else- branches
                are just sequences of element-wise operations that can be executed in-place,
                we could simply use a single buffer for both then- and else- branches.
                Here we will use separate buffers, but let's assume we could
                optimize out such trivial branches at the graph fusion level
                (especially when we have JIT).
            */
            | Ast.NN_If {then_branch, else_branch, t_inp, t_out} =>
                val {outargs=then_outargs} = then_branch
                val {outargs=else_outargs} = else_branch
                val nouts = t_out.size()
                assert(`then_outargs.size() == nouts && else_outargs.size() == nouts`)
                for outarg <- t_out, then_outarg <- then_outargs, else_outarg <- else_outargs {
                    if !model.isconst(then_outarg) && usecounts[then_outarg] == 1 {
                        share_buffer(outarg, then_outarg)
                    }
                    if !model.isconst(else_outarg) && usecounts[else_outarg] == 1 {
                        share_buffer(outarg, else_outarg)
                    }
                }
                assign_buffers_(then_branch)
                assign_buffers_(else_branch)
                for outarg <- t_out, then_outarg <- then_outargs, else_outarg <- else_outargs {
                    release_buffer(bufidxs[then_outarg])
                    release_buffer(bufidxs[else_outarg])
                }
            /*
                In the case of loop we try to alias t_v_in[i] and t_v_out[i] so that
                we eliminate some copy operations after each loop iteration.
            */
            | Ast.NN_Loop {body, t_trip_count, t_cond_in, t_v_in, t_v_out} =>
                val {inpargs, outargs} = body
                val n_inpargs = inpargs.size()
                val n_outargs = outargs.size()
                val n_state_vars = t_v_in.size()
                val n_accums = t_v_out.size() - n_state_vars
                assert(`n_inpargs == 2 + n_state_vars`)
                assert(`n_accums >= 0 && n_accums == n_outargs - n_state_vars - 1`)
                val inparg_0 = inpargs[0]
                if inparg_0 > 0 && usecounts[inparg_0] > 0 {
                    assert(`!model.isconst(inparg_0)`)
                    if !model.isconst(t_trip_count) {
                        share_buffer(t_trip_count, inparg_0)
                    } else {
                        bufidxs[inparg_0] = get_free_buffer()
                    }
                }

                for i <- -1:n_state_vars {
                    val inparg = inpargs[i+2], outarg = outargs[i+1]
                    val (v_inp, v_out) = if i < 0 { (t_cond_in, 0) }
                                         else { (t_v_in[i], t_v_out[i]) }
                    if inparg > 0 && usecounts[inparg] > 0 {
                        assert(`!model.isconst(inparg)`)
                        if !model.isconst(v_inp) {
                            share_buffer(v_inp, inparg)
                        } else {
                            bufidxs[inparg] = get_free_buffer()
                        }
                    }
                    if !model.isconst(v_out) {
                        /*if !model.isconst(v_inp) {
                            share_buffer(v_inp, v_out)
                        }*/
                        if !model.isconst(outarg) && usecounts[outarg] == 1 {
                            share_buffer(v_out, outarg)
                        }
                    }
                    /*
                        println(f"{i}. v_inp='{model.args[v_inp].name}', bufidxs[v_inp]={bufidxs[v_inp]}, \
                              inparg='{model.args[inparg].name}', bufidxs[inparg]={bufidxs[inparg]}, \
                              v_out='{model.args[v_out].name}', bufidxs[v_out]={bufidxs[v_out]}, \
                              outarg='{model.args[outarg].name}', bufidxs[outarg]={bufidxs[outarg]}")
                    */
                }

                //for bufidx@i <- bufidxs {model.bufidxs[i] = bufidx}
                //val noindent=""; println(f"occured loop: {model.op2str(op, noindent)}")

                /*
                // we accumulate 'scan' outputs of Loop non-inplace
                for i <- 0:n_accums {
                    int outarg = outargs[i+1+n_state_vars]
                    int acc = t_v_out[i+n_state_vars]

                    assert(`!model.isconst(outarg) && !model.isconst(acc)`)
                    share_buffer(acc, outarg)
                }
                */

                assign_buffers_(body)
                /*for inparg <- inpargs {
                    release_buffer(bufidxs[inparg])
                }*/
                for outarg <- outargs {
                    release_buffer(bufidxs[outarg])
                }
            | _ => {}
            }

            for outarg <- outs {
                if usecounts[outarg] == 0 {
                    release_buffer(bufidxs[outarg])
                    //println(f"arg '{model.args[argidx].name}' (buf #{model.args[argidx].idx}) is not used anymore: {freebufs.data[:freebufs.count]}")
                }
            }
            // let's release inputs in the reverse order to keep the buffer allocation consistent across the network
            val ninps = inps.size()
            for i <- 0:ninps {
                val inparg = inps[ninps-i-1]
                if bufidxs[inparg] >= 0 {
                    usecounts[inparg] -= 1
                    if usecounts[inparg] == 0 {
                        release_buffer(bufidxs[inparg])
                    }
                }
            }
        }
    }

    for inparg <- model.graph.inpargs {
        if !model.isconst(inparg) {
            bufidxs[inparg] = get_free_buffer()
        }
    }
    assign_buffers_(model.graph)
    model.{bufidxs = bufidxs, buffers = array(nbufs, ([]: uint8 []))}
}
