/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Performs some basic graph optimizations:
    1. Conv + Batchnorm
    2. Conv [+ Batchnorm] + ReLU/ReLU6/Clip
*/
import Ast

fun fuse_basic(net: Ast.nnet_t)
{
    val new_graph = net.graph
    val new_graph = fuse_conv_elemwise(net, new_graph)
    net.{graph = new_graph}
}

fun fuse_conv_elemwise(net: Ast.nnet_t, graph: Ast.nngraph_t)
{
    val prog = graph.prog
}