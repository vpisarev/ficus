/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Performs some basic graph optimizations:
    * Conv [+ Batchnorm] [+ Add] [+ ReLU/ReLU6/Clip/LeakyReLU/Sigmoid/Tanh/Mish]
    * Log (1 + Exp(x)) => Softplus(x)
    * x*Tanh(Softplus(x)) => Mish(x)
*/
import Ast, Dynvec, Hashmap

fun fuse_conv_elemwise(net: Ast.nnet_t, graph: Ast.nngraph_t, usecounts: int [])
{
    val nargs = net.args.size()
    val produced_by = array(nargs, -1)
    val prog = graph.prog
    val new_prog = Dynvec.create(0, Ast.NN_Nop)
    var modified = false
    fun get_op(op_idx: int) =
        if op_idx >= 0 {new_prog.data[op_idx]} else {Ast.NN_Nop}
    fun is_const_scalar_tensor(argidx: int, v: float?) =
        if net.isconst(argidx) {
            val t = net.get_tensor(argidx)
            if t.shape.total() == 1 {
                match (v, t.data) {
                | (None, _) => true
                | (Some v0, Ast.NN_Data_FP32 data) => v0 == data[0]
                | _ => false
                }
            } else {false}
        } else {false}
    fun is_elemwise(el_op: Ast.nnelwise_t, t_inp: int, check_used_once: bool): (int*2) =
        if t_inp < 0 {(-1, -1)}
        else {
            val op_idx = produced_by[t_inp]
            match get_op(op_idx) {
            | Ast.NN_Elemwise {el_op=el_op2, t_inp} when
                (el_op2 == el_op && (!check_used_once || usecounts[t_inp[0]] == 1)) =>
                (op_idx, t_inp[0])
            | _ => (-1, -1)
            }
        }

    for op@i <- prog {
        var imm_op_idx = -1, t_imm_arg = -1, mish_arg_idx = -1
        val (fused_op_idx, fused_op, t_out_new, (t_out_removed : int [])) = match op {
        // fuse convolution + batchnorm
        | Ast.NN_BatchNorm {t_inp, t_mean, t_var, t_scale, t_B, t_out}
            when usecounts[t_inp] == 1 =>
            val conv_op_idx = produced_by[t_inp]
            val conv_op = get_op(conv_op_idx)
            match conv_op {
            | Ast.NN_Conv {
                name, attr, conv_data,
                fused_batch_norm=None, fused_activ=None,
                t_inp=t_conv_inp, t_weights, t_bias, t_out=t_conv_out, t_passby=0 }
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
                    t_bias=t_bias, t_out=t_out, t_passby=0}
                (conv_op_idx, new_conv_op, t_out, [t_conv_out])
            | _ => (-1, Ast.NN_Nop, -1, [])
            }
        // fuse convolution [+ batchnorm] + activation
        | Ast.NN_Elemwise {el_op=(Ast.NN_Relu | Ast.NN_Sigmoid | Ast.NN_Tanh | Ast.NN_Mish)}
        | Ast.NN_Clip _
        | Ast.NN_LeakyRelu _ =>
            val (t_activ_inp, t_activ_out) = op.get_inputs_outputs()
            val conv_op_idx = produced_by[t_activ_inp[0]]
            val conv_op = get_op(conv_op_idx)
            match conv_op {
            | Ast.NN_Conv {
                name, attr, conv_data,
                fused_batch_norm, non_const_batch_norm, fused_activ=None,
                t_inp=t_conv_inp, t_weights, t_bias, t_out=t_conv_out, t_passby }
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
                    t_bias=t_bias, t_out=t_activ_out[0], t_passby=t_passby}
                (conv_op_idx, new_conv_op, t_activ_out[0], [t_conv_out])
            | _ => (-1, Ast.NN_Nop, -1, [])
            }
        // convert log(1 + exp(x)) to softplus(x)
        | Ast.NN_Elemwise {name, el_op=Ast.NN_Log, t_inp, t_out=t_out_log} when usecounts [t_inp[0]] == 1 =>
            val t_inp0 = t_inp[0]
            val add_op_idx = produced_by[t_inp0]
            var _1_arg = -1
            match get_op(add_op_idx) {
            | Ast.NN_Elemwise {el_op=Ast.NN_Add, t_inp=t_inp_add, t_out=t_out_add}
                when ({if is_const_scalar_tensor(t_inp_add[0], Some(1.f)) {_1_arg=0}
                       else if is_const_scalar_tensor(t_inp_add[1], Some(1.f)) {_1_arg=1};
                       (_1_arg >= 0 && usecounts[t_inp_add[1-_1_arg]] == 1)}) =>
                val t_inp_add0 = t_inp_add[1-_1_arg]
                val (exp_op_idx, t_inp_exp) = is_elemwise(Ast.NN_Exp, t_inp_add0, false)
                if t_inp_exp >= 0 {
                    new_prog.data[add_op_idx] = Ast.NN_Nop
                    val fused_op = Ast.NN_Elemwise {name=name,
                        el_op=Ast.NN_Softplus, t_inp=[t_inp_exp], t_out=t_out_log}
                    (exp_op_idx, fused_op, t_out_log, [t_out_add, t_inp_add0])
                } else {
                    (-1, Ast.NN_Nop, -1, [])
                }
            | _ => (-1, Ast.NN_Nop, -1, [])
            }
        // convert x*tanh(softplus(x)) to mish(x)
        | Ast.NN_Elemwise {name, el_op=Ast.NN_Mul, t_inp=t_inp_mul, t_out=t_out_mul} when
            ({
                for i <- 0:2 {
                    val (tanh_op_idx, t_tanh_arg) =
                        is_elemwise(Ast.NN_Tanh, t_inp_mul[i], true)
                    if t_tanh_arg >= 0 {
                        imm_op_idx = tanh_op_idx
                        t_imm_arg = t_tanh_arg
                        mish_arg_idx = 1-i
                        break
                    }
                };
                mish_arg_idx >= 0
            }) =>
            val (splus_op_idx, t_inp_splus) = is_elemwise(Ast.NN_Softplus, t_imm_arg, false)
            if t_inp_splus == t_inp_mul[mish_arg_idx] {
                new_prog.data[imm_op_idx] = Ast.NN_Nop
                usecounts[t_inp_splus] -= 1
                val fused_op = Ast.NN_Elemwise {name=name, el_op=Ast.NN_Mish,
                    t_inp=[t_inp_splus], t_out=t_out_mul}
                (splus_op_idx, fused_op, t_out_mul, [t_inp_mul[1-mish_arg_idx], t_imm_arg])
            } else {
                (-1, Ast.NN_Nop, -1, [])
            }
        // fuse convolution [+ batchnorm] + add
        | Ast.NN_Elemwise {el_op=Ast.NN_Add, t_inp, t_out} =>
            val t_inp0 = t_inp[0], t_inp1 = t_inp[1]
            val uc0 = usecounts[t_inp0], uc1 = usecounts[t_inp1]
            val (t_passby_idx, conv_op_idx) =
                if uc0 == 1 && uc1 == 1 {
                    val conv_op_idx0 = produced_by[t_inp0]
                    val conv_op_idx1 = produced_by[t_inp1]
                    if conv_op_idx0 > conv_op_idx1 {(t_inp1, conv_op_idx0)}
                    else {(t_inp0, conv_op_idx1)}
                } else if (uc0 == 1 && uc1 == 2) || (uc0 == 2 || uc1 == 1) {
                    val (t_inp0, t_inp1) = if uc0 == 2 {(t_inp0, t_inp1)} else {(t_inp1, t_inp0)}
                    fun is_passby(t_inp0: int, t_inp1: int, depth: int) {
                        val conv_op_idx = produced_by[t_inp1]
                        val conv_op = get_op(conv_op_idx)
                        match conv_op {
                        | Ast.NN_Conv {t_inp=t_conv_inp, t_out=t_conv_out, t_passby}
                            when t_passby == 0 || depth > 0 =>
                            if t_conv_inp == t_inp0 {true}
                            else {is_passby(t_inp0, t_conv_inp, depth+1)}
                        | _ => false
                        }
                    }
                    if is_passby(t_inp0, t_inp1, 0) {
                        (t_inp0, produced_by[t_inp1])
                    } else {(-1, -1)}
                } else {(-1, -1)}
            val conv_op = get_op(conv_op_idx)
            match conv_op {
            | Ast.NN_Conv {
                name, attr, conv_data,
                fused_batch_norm, non_const_batch_norm, fused_activ=None,
                t_inp=t_conv_inp, t_weights, t_bias, t_out=t_conv_out, t_passby=0 }
                when usecounts[t_conv_out] == 1 =>
                // merge Conv + Add
                val new_conv_op = Ast.NN_Conv {
                    name=name, attr=attr, conv_data=conv_data,
                    fused_batch_norm=fused_batch_norm,
                    non_const_batch_norm=non_const_batch_norm,
                    fused_activ=None, non_const_activ=false,
                    t_inp=t_conv_inp, t_weights=t_weights,
                    t_bias=t_bias, t_out=t_out, t_passby=t_passby_idx}
                (conv_op_idx, new_conv_op, t_out, [t_conv_out])
            | _ => (-1, Ast.NN_Nop, -1, [])
            }
        | _ => (-1, Ast.NN_Nop, -1, [])
        }
        if fused_op_idx >= 0 {
            modified = true
            new_prog.data[fused_op_idx] = fused_op
            produced_by[t_out_new] = fused_op_idx
            for t_out_old <- t_out_removed {
                usecounts[t_out_old] = 0
                produced_by[t_out_old] = -1
            }
        } else {
            val (_, t_outs) = op.get_inputs_outputs()
            for out <- t_outs {produced_by[out] = new_prog.count}
            new_prog.do_push(op)
        }
    }

    if modified {
        var j = 0
        for i <- 0:new_prog.count {
            match new_prog.data[i] {
            | Ast.NN_Nop => {}
            | op =>
                if j < i {new_prog.data[j] = op}
                j += 1
            }
        }
        Ast.NN_Graph {inpargs=graph.inpargs,
            outargs=graph.outargs, prog=new_prog.data[0:j].copy()}
    } else {
        graph
    }
}

fun fuse_basic(net: Ast.nnet_t)
{
    val usecounts = net.use_counts()
    var new_graph = net.graph
    for iter <- 0:3 {
        val nops_before = new_graph.prog.size()
        new_graph = fuse_conv_elemwise(net, new_graph, usecounts)
        val nops_after = new_graph.prog.size()
        if nops_after == nops_before {break}
    }
    net.{graph = new_graph}
}
