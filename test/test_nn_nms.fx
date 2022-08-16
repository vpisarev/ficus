/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// some NN/ONNX ops tests, extracted from ONNX specification:
// https://github.com/onnx/onnx/blob/main/docs/Operators.md

from UTest import *
import NN.Ast as Ast, NN.OpNMS as OpNMS

TEST("NN.NonMaxSuppression.center_point", fun()
{
    val center_point_box=true
    val boxes = float([
        0.5, 0.5, 1.0, 1.0,
        0.5, 0.6, 1.0, 1.0,
        0.5, 0.4, 1.0, 1.0,
        0.5, 10.5, 1.0, 1.0,
        0.5, 10.6, 1.0, 1.0,
        0.5, 100.5, 1.0, 1.0])
    val scores = float([0.9, 0.75, 0.6, 0.95, 0.5, 0.3])
    val max_output_boxes_per_class = 3
    val iou_threshold = 0.5f
    val score_threshold = 0.f
    val boxes = Ast.mktensor(boxes.reshape(1, 6, 4))
    val scores = Ast.mktensor(scores.reshape(1, 1, 6))

    val selected_ref = [0L, 0L, 3L; 0L, 0L, 0L; 0L, 0L, 5L]
    val (n, selected, buf) =
        OpNMS.run_nms(boxes, scores, max_output_boxes_per_class,
                      center_point_box, iou_threshold, score_threshold, [], 4)

    EXPECT_EQ(`selected.reshape(n, 3)`, selected_ref)
    EXPECT_EQ(`buf.size()`, n*3*8)
})

TEST("NN.NonMaxSuppression.suppress_by_IOU_and_scores", fun()
{
    val center_point_box = false
    val boxes = float([
        0.0, 0.0, 1.0, 1.0,
        0.0, 0.1, 1.0, 1.1,
        0.0, -0.1, 1.0, 0.9,
        0.0, 10.0, 1.0, 11.0,
        0.0, 10.1, 1.0, 11.1,
        0.0, 100.0, 1.0, 101.0])
    val scores = float([0.9, 0.75, 0.6, 0.95, 0.5, 0.3])
    val max_output_boxes_per_class = 3
    val iou_threshold = 0.5f
    val score_threshold = 0.4f
    val selected_ref = [0L, 0L, 3L; 0L, 0L, 0L]
    val boxes = Ast.mktensor(boxes.reshape(1, 6, 4))
    val scores = Ast.mktensor(scores.reshape(1, 1, 6))
    val (n, selected, buf) =
        OpNMS.run_nms(boxes, scores, max_output_boxes_per_class,
                      center_point_box, iou_threshold, score_threshold, [], 4)

    EXPECT_EQ(`selected.reshape(n, 3)`, selected_ref)
    EXPECT_EQ(`buf.size()`, n*3*8)
})

TEST("NN.NonMaxSuppression.two_classes", fun()
{
    val center_point_box = false
    val boxes = float([
        0.0, 0.0, 1.0, 1.0,
        0.0, 0.1, 1.0, 1.1,
        0.0, -0.1, 1.0, 0.9,
        0.0, 10.0, 1.0, 11.0,
        0.0, 10.1, 1.0, 11.1,
        0.0, 100.0, 1.0, 101.0])
    val scores = float([0.9, 0.75, 0.6, 0.95, 0.5, 0.3,
                        0.9, 0.75, 0.6, 0.95, 0.5, 0.3])
    val max_output_boxes_per_class = 2
    val iou_threshold = 0.5f
    val score_threshold = 0.f
    val boxes = Ast.mktensor(boxes.reshape(1, 6, 4))
    val scores = Ast.mktensor(scores.reshape(1, 2, 6))

    val selected_ref = int64([0, 0, 3; 0, 0, 0; 0, 1, 3; 0, 1, 0])
    val (n, selected, buf) = OpNMS.run_nms(boxes, scores,
                                    max_output_boxes_per_class, center_point_box,
                                    iou_threshold, score_threshold, [], 4)
    EXPECT_EQ(`selected.reshape(n, 3)`, selected_ref)
    EXPECT_EQ(`buf.size()`, n*3*8)
})
