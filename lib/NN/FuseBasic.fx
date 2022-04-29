/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Performs some basic graph optimizations:
    1. Conv + Batchnorm
    2. Conv [+ Batchnorm] + ReLU/ReLU6/Clip
*/
import Ast, Dynvec, Hashmap

fun get_produced_by(net: Ast.nnet_t, graph: Ast.nngraph_t)
{
    val nargs = net.args.size()
    val produced_by = array(nargs, -1)

    for op@op_idx <- graph.prog {
        val (_, outs) = op.get_inputs_outputs()
        for i <- outs {
            if i > 0 {produced_by[i] = op_idx}
            // we don't look into If and Loop subgraphs,
            // because get_produced_by() assumes linear indices
            // inside a single flat graph
        }
    }
    produced_by
}

fun fuse_conv_elemwise(net: Ast.nnet_t, graph: Ast.nngraph_t, usecounts: int [])
{
    val produced_by = get_produced_by(net, graph)
    val prog = graph.prog
    val new_prog = Dynvec.create(0, Ast.NN_Nop)
    var modified = false
    fun get_op(op_idx: int) =
        if op_idx >= 0 {prog[op_idx]} else {Ast.NN_Nop}

    for op@i <- prog {
        val (fused_op_idx, fused_op, t_out_old, t_out_new) = match op {
        | Ast.NN_BatchNorm {t_inp, t_mean, t_var, t_scale, t_B, t_out}
            when usecounts[t_inp] == 1 =>
            val conv_op_idx = produced_by[t_inp]
            val conv_op = get_op(conv_op_idx)
            match conv_op {
            | Ast.NN_Conv {
                name, attr, conv_data,
                fused_batch_norm=None, fused_activ=None,
                t_inp=t_conv_inp, t_weights, t_bias, t_out=t_conv_out }
                when t_conv_out == t_inp =>
                // merge Conv + BatchNorm
                val non_const_batch_norm =
                    !net.isconst(t_mean) || !net.isconst(t_var) ||
                    !net.isconst(t_scale) || !net.isconst(t_B)
                *conv_data = null
                val new_conv_op = Ast.NN_Conv {
                    name=name, attr=attr, conv_data=conv_data,
                    fused_batch_norm=Some(op),
                    non_const_batch_norm=non_const_batch_norm,
                    fused_activ=None, non_const_activ=false,
                    t_inp=t_conv_inp, t_weights=t_weights,
                    t_bias=t_bias, t_out=t_out}
                (conv_op_idx, new_conv_op, t_conv_out, t_out)
            | _ => (-1, Ast.NN_Nop, -1, -1)
            }
        | Ast.NN_Elemwise {el_op=(Ast.NN_Relu | Ast.NN_Sigmoid | Ast.NN_Tanh)}
        | Ast.NN_Clip _
        | Ast.NN_LeakyRelu _ =>
            val (t_activ_inp, t_activ_out) = op.get_inputs_outputs()
            val conv_op_idx = produced_by[t_activ_inp[0]]
            val conv_op = get_op(conv_op_idx)
            match conv_op {
            | Ast.NN_Conv {
                name, attr, conv_data,
                fused_batch_norm, non_const_batch_norm, fused_activ=None,
                t_inp=t_conv_inp, t_weights, t_bias, t_out=t_conv_out }
                when t_conv_out == t_activ_inp[0] && usecounts[t_conv_out] == 1 =>
                // merge Conv + activation
                val non_const_activ = match op {
                    | Ast.NN_Clip {t_min, t_max} => !net.isconst(t_min) || !net.isconst(t_max)
                    | _ => false }
                *conv_data = null
                val new_conv_op = Ast.NN_Conv {
                    name=name, attr=attr, conv_data=conv_data,
                    fused_batch_norm=fused_batch_norm,
                    non_const_batch_norm=non_const_batch_norm,
                    fused_activ=Some(op), non_const_activ=non_const_activ,
                    t_inp=t_conv_inp, t_weights=t_weights,
                    t_bias=t_bias, t_out=t_activ_out[0]}
                (conv_op_idx, new_conv_op, t_conv_out, t_activ_out[0])
            | _ => (-1, Ast.NN_Nop, -1, -1)
            }
        | _ => (-1, Ast.NN_Nop, -1, -1)
        }
        if fused_op_idx >= 0 {
            modified = true
            prog[fused_op_idx] = fused_op
            usecounts[t_out_old] = 0
            produced_by[t_out_old] = -1
            produced_by[t_out_new] = fused_op_idx
        } else {
            new_prog.do_push(op)
        }
    }

    if modified {
        Ast.NN_Graph {inpargs=graph.inpargs, outargs=graph.outargs,
            prog=new_prog.data[0:new_prog.count].copy()}
    } else {
        graph
    }
}

fun fuse_basic(net: Ast.nnet_t)
{
    val usecounts = net.use_counts()
    val new_graph = net.graph
    val new_graph = fuse_conv_elemwise(net, new_graph, usecounts)
    net.{graph = new_graph}
}
