/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// dumps .onnx model

import Hashset, Onnx.Ast, Onnx.Parser, Sys

fun collect_opnames(model: Onnx.Ast.model_t)
{
    val all_ops = Hashset.empty(256, "")
    fun collect_opnames_(ops: string Hashset.t, g: Onnx.Ast.graph_t): void
    {
        for n <- g.nodes {
            ops.add(n.op)
            for attr <- n.attrs {
                match attr.v {
                | Onnx.Ast.AttrGraph(subgraph) => collect_opnames_(ops, subgraph)
                | _ => {}
                }
            }
        }
    }
    collect_opnames_(all_ops, model.graph)
    all_ops
}

val model_name = match Sys.arguments() {
    | [:: f] => f
    | _ => println("specify .onnx model name"); throw Fail("")
    }
val model = Onnx.Parser.parse(model_name)
println(model)
val ops = collect_opnames(model)
val ops = ops.list().sort((<))
println("operations: ")
for op <- ops {
    println(op)
}
