/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// ONNX-compliant implementation of NonMaxSuppression

import Ast

@private run_nms()

fun run_nms(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_NonMaxSuppression {center_point_box, t_boxes, t_scores,
        t_max_output_boxes_per_class,
        t_iou_threshold, t_score_threshold, t_out} =>
    val boxes = model.get_tensor(t_boxes)
    val scores = model.get_tensor(t_scores)
    val max_boxes = model.get_tensor(t_max_output_boxes_per_class)
    val out_bufdix = model.bufidxs[t_out]
    val out_buf = model.buffers[out_bufidx]
    val iou_threshold = model.get_tensor(t_iou_threshold).data.float_scalar_or(0.f)
    val score_threshold = model.get_tensor(t_score_threshold).data.float_scalar_or(0.f)
    val max_boxes_data = match max_boses.data {
        | Ast.NN_Data_Empty => []
        | Ast.NN_Data_I64 max_boxes_data => max_boxes_data
        | _ => throw Ast.NNError(f"NonMaxSuppression: 'max_output_boxes_per_class' parameter, if any, must have I64 type")
    }
    match (boxes.data, scores.data) {
    | (Ast.NN_Data_FP32 boxes_data, Ast.NN_Data_FP32 scores_data) =>
        val (ndetections, out_data, out_buf) = run_nms(boxes.shape.shape, boxes_data, scores.shape.shape, scores_data,
                                    max_boxes.shape.shape, max_boxes_data, iou_threshold, score_threshold, *model.ntasks)
        model.buffers[out_bufidx] = out_buf
        model.tensors[t_out] = Ast.nntensor {data = Ast.NN_Data_I64 out_data,
            shape = Ast.nnshape_t {layout=Ast.NN_Layout_NC, shape=[ndetections, 3]}}
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
