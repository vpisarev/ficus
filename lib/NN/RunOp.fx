/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// simple dispatcher to run an operation;
// will likely be eliminated soon

import Ast
import OpConv, OpElemwise, OpNN, OpMisc, OpPermute, OpPooling, OpReduce, OpResize

fun run_op(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
    | Ast.NN_Nop => {}
    | Ast.NN_AvgPool _ =>
        OpPooling.run_avgpool(model, op)
    | Ast.NN_BatchNorm _ =>
        OpNN.run_batchnorm(model, op)
    | Ast.NN_Cast _ =>
        OpElemwise.run_cast(model, op)
    | Ast.NN_Clip _ =>
        OpElemwise.run_clip(model, op)
    | Ast.NN_Concat _ =>
        OpPermute.run_concat(model, op)
    | Ast.NN_ConstantOfShape _ =>
        OpElemwise.run_constantOfShape(model, op)
    | Ast.NN_Conv _ =>
        OpConv.run_conv(model, op)
    | Ast.NN_ConvTranspose _ =>
        OpConv.run_conv_transposed(model, op)
    | Ast.NN_Dropout _ =>
        OpElemwise.run_dropout(model, op)
    | Ast.NN_Elemwise {t_inp} =>
        val ninputs = t_inp.size()
        if ninputs == 1 {
            OpElemwise.run_unary(model, op)
        } else if ninputs == 2 {
            OpElemwise.run_binary(model, op)
        } else {
            OpElemwise.run_nary(model, op)
        }
    | Ast.NN_Expand _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Flatten {t_inp, t_out} =>
        model.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Gather _ =>
        OpPermute.run_gather(model, op)
    | Ast.NN_Gemm _ =>
        OpNN.run_gemm(model, op)
    | Ast.NN_GlobalAvgPool _ =>
        OpPooling.run_global_avgpool(model, op)
    | Ast.NN_Identity {t_inp, t_out} =>
        model.copy_tensor_data(t_inp, t_out)
    | Ast.NN_If _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_LeakyRelu _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Loop _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_LRN _ =>
        OpMisc.run_lrn(model, op)
    | Ast.NN_MaxPool _ =>
        OpPooling.run_maxpool(model, op)
    | Ast.NN_NonMaxSuppression _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_NonZero _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Range _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Reduce _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Reshape {t_inp, t_out} =>
        model.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Resize _ =>
        OpResize.run_resize(model, op)
    | Ast.NN_RoiAlign _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Scatter _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Shape _ =>
        OpPermute.run_shape(model, op)
    | Ast.NN_Slice _ =>
        OpPermute.run_slice(model, op)
    | Ast.NN_SoftMax _ =>
        OpNN.run_softmax(model, op)
    | Ast.NN_Split _ =>
        OpPermute.run_split(model, op)
    | Ast.NN_Squeeze {t_inp, t_out} =>
        model.copy_tensor_data(t_inp, t_out)
    | Ast.NN_Tile _ =>
        OpPermute.run_tile(model, op)
    | Ast.NN_TopK _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_Transpose _ =>
        OpPermute.run_transpose(model, op)
    | Ast.NN_Unsqueeze {t_inp, t_out} =>
        model.copy_tensor_data(t_inp, t_out)
}
