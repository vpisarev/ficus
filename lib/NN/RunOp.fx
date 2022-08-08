/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// simple dispatcher to run an operation;
// will likely be eliminated soon

import Ast
import OpConv, OpElemwise, OpNN, OpMisc, OpNMS, OpPermute, OpPooling, OpQuantized, OpReduce, OpResize

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
        //OpConv.run_conv_transposed(model, op)
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_DequantizeLinear _ =>
        OpQuantized.run_dequantize(model, op)
    | Ast.NN_Dropout _ =>
        OpElemwise.run_dropout(model, op)
    | Ast.NN_Elemwise _ =>
        OpElemwise.run_elemwise(model, op)
    | Ast.NN_Expand _ =>
        OpElemwise.run_expand(model, op)
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
        // handled in Inference.fx
        throw Ast.NNError(f"unexpected operation '{op.name()}'")
    | Ast.NN_LeakyRelu _ =>
        OpElemwise.run_leaky_relu(model, op)
    | Ast.NN_Loop _ =>
        // handled in Inference.fx
        throw Ast.NNError(f"unexpected operation '{op.name()}'")
    | Ast.NN_LRN _ =>
        OpMisc.run_lrn(model, op)
    | Ast.NN_MaxPool _ =>
        OpPooling.run_maxpool(model, op)
    | Ast.NN_NonMaxSuppression _ =>
        OpNMS.run_nms(model, op)
    | Ast.NN_NonZero _ =>
        OpReduce.run_nonzero(model, op)
    | Ast.NN_QLinearAdd _ =>
        OpQuantized.run_qadd(model, op)
    | Ast.NN_QLinearConv _ =>
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_QLinearMatMul _ =>
        //OpQuantized.run_qgemm(model, op)
        throw Ast.NNError(f"unsupported operation '{op.name()}'")
    | Ast.NN_QLinearGlobalAvgPool _ =>
        OpPooling.run_qglobal_avgpool(model, op)
    | Ast.NN_QuantizeLinear _ =>
        OpQuantized.run_quantize(model, op)
    | Ast.NN_Range _ =>
        OpMisc.run_range(model, op)
    | Ast.NN_Reduce _ =>
        OpReduce.run_reduce(model, op)
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
        OpReduce.run_top_k(model, op)
    | Ast.NN_Transpose _ =>
        OpPermute.run_transpose(model, op)
    | Ast.NN_Unsqueeze {t_inp, t_out} =>
        model.copy_tensor_data(t_inp, t_out)
}
