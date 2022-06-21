/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Constant folding: if an operation can be performed
// before running the graph on actual data (i.e. when the inputs are constants or
// expressions on constants), it's eliminated from the graph and
// the result of operation is represented as the constant.

import Dynvec
import Ast, InferShapes, RunOp

fun cfold(model: Ast.nnmodel_t)
{
    val graph = cfold_graph(model, model.graph)
    model.{graph = graph}
}

fun cfold_graph(model: Ast.nnmodel_t, graph: Ast.nngraph_t)
{
    val new_prog = Dynvec.create(0, Ast.NN_Nop)
    for op <- graph.prog {
        val opt_op = match op {
        | Ast.NN_If {name, then_branch, else_branch, t_inp, t_out} =>
            val then_branch = cfold_graph(model, then_branch)
            val else_branch = cfold_graph(model, else_branch)
            Some(Ast.NN_If {name=name, then_branch=then_branch,
                else_branch=else_branch, t_inp=t_inp, t_out=t_out})
        | Ast.NN_Loop {name, body, t_trip_count, t_cond_in, t_v_in,
                       t_cond_out, t_v_out} =>
            val body = cfold_graph(model, body)
            Some(Ast.NN_Loop {name=name, body=body,
                t_trip_count=t_trip_count,
                t_cond_in=t_cond_in, t_v_in=t_v_in,
                t_cond_out=t_cond_out, t_v_out=t_v_out})
        | _ =>
            val (inps, outs) = op.get_inputs_outputs()
            if !all(for t_inp <- inps {model.isconst(t_inp)}) ||
               exists(for t_out <- outs {model.isoutput(t_out)}) {
                Some(op)
            } else {
                val oinfo = InferShapes.infer(model, op)
                for oi@outidx <- oinfo {
                    val {idx=argidx, shape, typ} = oi
                    val arg = model.args[argidx]
                    if arg.argkind == Ast.NN_Arg_Temp {
                        model.args[argidx].argkind = Ast.NN_Arg_Const
                        model.tensors[argidx] = Ast.make_tensor(shape, typ)
                    } else {
                        throw Ast.NNError(f"unexpected argkind={arg.argkind} of the output {outidx} of {op.name()}")
                    }
                }
                RunOp.run_op(model, op)
                None
            }
        }
        match opt_op {
        | Some op => new_prog.do_push(op)
        | _ => {}
        }
    }
    if new_prog.count == graph.prog.size() {graph}
    else {
        val {inpargs, outargs} = graph
        Ast.NN_Graph {inpargs=inpargs, outargs=outargs,
            prog = new_prog.data[:new_prog.count]}
    }
}
