/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Allocates buffers for all intermediate tensors of the graph/model

/*

The algorithm is quite simple:

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

Please, note that when we reuse buffers, we don't check any types, shape or a total size of the buffer needed.
We reallocate each buffer at runtime to fit each single argument that it's used for. For example, let's say the buffer #3
is used for arguments #5 (10x10x10 FP32), #10 (6x6x32 FP32) and #14 (300x1 UINT64). Then during the the first run of
the inference the buffer #3 will be reallocated from 0 bytes to 1000*4 bytes to fit arg #10,
then from 4000 to 6*6*32*4=4608 bytes to fit arg #10 and then it will fit arg #14 without reallocations.
During the second run of inference with the same resolution input the buffer will not be reallocated.

The reallocation is done using NN.Ast.fit() function.
*/

import Ast, Dynvec

fun assign_buffers(net: Ast.nnet_t)
{
    val nargs = net.args.size()
    val usecounts = net.use_counts()
    var nbufs = 0
    val freebufs = Dynvec.create(0, 0)
    val bufidxs = array(nargs, -1)

    /*println("=================== arg use counts =====================")
    for i <- 0:nargs {
        println(f"{net.args[i].name}: count={usecounts[i]}")
    }
    println("========================================================")*/

    fun assign_buffers_(graph: Ast.nngraph_t): void
    {
        for op <- graph.prog {
            // [TODO] this is the simplest but sub-optimal algorithm, e.g.
            // we could reuse the same buffers in 'then' and 'else' branches
            if op == Ast.NN_Nop {continue}
            match op {
            | Ast.NN_If {then_branch, else_branch} =>
                assign_buffers_(then_branch)
                assign_buffers_(else_branch)
            | Ast.NN_Loop {body} =>
                assign_buffers_(body)
            | _ => {}
            }
            val (inps, outs) = op.get_inputs_outputs()
            val (inplace_op, reuse_idx) = match op {
                | Ast.NN_BatchNorm _ | Ast.NN_Clip _ | Ast.NN_Dropout _
                | Ast.NN_Flatten _ | Ast.NN_Identity _ | Ast.NN_LeakyRelu _
                | Ast.NN_Reshape _ | Ast.NN_Squeeze _ | Ast.NN_Unsqueeze _ =>
                    (usecounts[inps[0]] == 1 && net.istemp(inps[0]), inps[0])
                | Ast.NN_Scatter _ =>
                    (usecounts[inps[0]] == 1 && inps[1] != inps[0] && inps[2] != inps[0] && net.istemp(inps[0]), inps[0])
                // because of posssible broadcasting we cannot safely perform
                // element-wise operations in-place, unless there is just one input.
                | Ast.NN_Elemwise {el_op, t_inp} when t_inp.size() == 1 && (match el_op {
                    | Ast.NN_IsInf | Ast.NN_IsNaN => false | _ => true}) =>
                    match find_opt(for argidx <- inps {usecounts[argidx] == 1 && net.istemp(argidx)}) {
                    | Some(argidx) => (true, argidx)
                    | _ => (false, -1)
                    }
                | Ast.NN_Conv {t_passby} when t_passby > 0 && usecounts[t_passby] == 1 =>
                    (true, t_passby)
                | _ => (false, -1)
                }
            //println(f"name={op.name()}, inplace={inplace_op}, inps={[::for i<-inps {net.args[i].name}]}, outs={[::for i<-outs {net.args[i].name}]}")
            if inplace_op && net.args[outs[0]].argkind != Ast.NN_Arg_Output {
                bufidxs[outs[0]] = bufidxs[reuse_idx]
            } else {
                for argidx <- outs {
                    if net.istemp(argidx) {
                        if freebufs.count == 0 {
                            freebufs.do_push(nbufs)
                            //println(f"added buf #{nbufs}: {freebufs.data[:freebufs.count]}")
                            nbufs += 1
                        }
                        val outidx = freebufs.top()
                        //println(f"use buf #{outidx}")
                        freebufs.pop()
                        bufidxs[argidx] = outidx
                    }
                }
            }
            for argidx <- outs {
                if net.istemp(argidx) && usecounts[argidx] == 0 {
                    freebufs.do_push(bufidxs[argidx])
                    //println(f"arg '{net.args[argidx].name}' (buf #{net.args[argidx].idx}) is not used anymore: {freebufs.data[:freebufs.count]}")
                }
            }
            val ninps = inps.size()
            for i <- 0:ninps {
                val argidx = inps[ninps-i-1]
                if net.istemp(argidx) && (!inplace_op || argidx != reuse_idx) {
                    assert(usecounts[argidx] > 0)
                    usecounts[argidx] -= 1
                    if usecounts[argidx] == 0 {
                        freebufs.do_push(bufidxs[argidx])
                        //println(f"arg '{net.args[argidx].name}' (buf #{net.args[argidx].idx}) is not used anymore: {freebufs.data[:freebufs.count]}")
                    }
                }
            }
        }
    }
    assign_buffers_(net.graph)
    net.{bufidxs = bufidxs, buffers = array(nbufs, ([]: uint8 []))}
}
