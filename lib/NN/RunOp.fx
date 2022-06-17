/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// simple dispatcher to run an operation;
// will likely be eliminated soon

import Ast
import OpConv, OpElemwise, OpNN, OpMisc, OpPermute, OpPooling, OpReduce, OpResize

fun run_op(net: Ast.nnet_t, op: Ast.nnop_t) =
match op
{
    | Ast.NN_Nop => {}
    | Ast.NN_AvgPool _ =>
        OpPooling.run_avgpool(net, op)
    | Ast.NN_BatchNorm _ =>
        OpNN.run_batchnorm(net, op)
    | Ast.NN_Cast _ =>
        OpElemwise.run_cast(net, op)
    | Ast.NN_Clip _ =>
        OpElemwise.run_clip(net, op)
    | Ast.NN_Concat _ =>
        OpPermute.run_concat(net, op)
    | Ast.NN_ConstantOfShape _ =>
        OpElemwise.run_constantOfShape(net, op)
    | Ast.NN_Conv _ =>
        OpConv.run_conv(net, op)
    | Ast.NN_ConvTranspose _ =>
        OpConv.run_conv_transposed(net, op)
    | Ast.NN_Dropout _ =>
        OpElemwise.run_dropout(net, op)
    | Ast.NN_Elemwise {t_inp} =>
        val ninputs = t_inp.size()
        if ninputs == 1 {
            OpElemwise.run_unary(net, op)
        } else if ninputs == 2 {
            OpElemwise.run_binary(net, op)
        } else {
            OpElemwise.run_nary(net, op)
        }
    | Ast.NN_Expand _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Flatten {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Gather _ =>
        OpPermute.run_gather(net, op)
    | Ast.NN_Gemm _ =>
        OpNN.run_gemm(net, op)
    | Ast.NN_GlobalAvgPool _ =>
        OpPooling.run_global_avgpool(net, op)
    | Ast.NN_Identity {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.NN_If _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_LeakyRelu _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Loop _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_LRN _ =>
        OpMisc.run_lrn(net, op)
    | Ast.NN_MaxPool _ =>
        OpPooling.run_maxpool(net, op)
    | Ast.NN_NonMaxSuppression _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_NonZero _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Range _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Reduce _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Reshape {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Resize _ =>
        OpResize.run_resize(net, op)
    | Ast.NN_RoiAlign _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Scatter _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Shape _ =>
        OpPermute.run_shape(net, op)
    | Ast.NN_Slice _ =>
        OpPermute.run_slice(net, op)
    | Ast.NN_SoftMax _ =>
        OpNN.run_softmax(net, op)
    | Ast.NN_Split _ =>
        OpPermute.run_split(net, op)
    | Ast.NN_Squeeze {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Tile _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_TopK _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Transpose _ =>
        OpPermute.run_transpose(net, op)
    | Ast.NN_Unsqueeze {t_inp, t_out} =>
        net.copy_tensor_data(t_inp, t_out)
}
