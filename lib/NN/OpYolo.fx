/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
   Yolo v4 post-processing steps;
   Adopted from https://github.com/onnx/models/tree/main/vision/object_detection_segmentation/yolov4
*/

import Ast

val yolov4_default_anchors =
[
    (12.f, 16.f), (19.f, 36.f), (40.f, 28.f);
    (36.f, 75.f), (76.f, 55.f), (72.f, 146.f);
    (142.f, 110.f), (192.f, 243.f), (459.f, 401.f)
]
val yolov4_default_strides = [8, 16, 32]
val yolov4_default_scale = [1.2f, 1.1f, 1.05f]
type yolo_detection_t = (float*6)

fun sigmoid(x: float) = 1.f/(1.f+exp(-x))

fun extract_boxes(yolo_outputs: Ast.nntensor_t [],
    anchors: (float*2) [,], strides: int [], xyscale: float [])
{
    var maxboxes = 0, n0 = 0
    val data = [for out@i <- yolo_outputs {
        val shape_i = out.shape.shape
        assert(shape_i.size() == 5)
        val N = shape_i[0], h = shape_i[1], w = shape_i[2], m = shape_i[3], n = shape_i[4]
        assert(N == 1 && h == w && m == 3)
        maxboxes += h*w*m
        if i == 0 {n0 = n} else {assert(n0 == n)}
        match out.data {
        | Ast.NN_Data_FP32 data => data.reshape(h*w*m, n)
        | _ => throw Ast.NNError("floating-point output in Yolo is expected")
        }
    }]
    val boxes = array((maxboxes, n0), 0.f)
    var k = 0

    // define anchor boxes
    for out@i <- yolo_outputs, data_i <- data, xyscale_i <- xyscale, stride_i <- strides {
        val conv_shape = out.shape.shape
        val output_size = conv_shape[1]
        val xyscale_i_delta = -0.5f*(xyscale_i - 1)
        for y0 <- 0:output_size {
            for x0 <- 0:output_size {
                for j <- 0:3 {
                    val idx = (y0*output_size + x0)*3 + j
                    val x = (sigmoid(data_i[idx, 0])*xyscale_i + xyscale_i_delta + x0)*stride_i
                    val y = (sigmoid(data_i[idx, 1])*xyscale_i + xyscale_i_delta + y0)*stride_i
                    val w = exp(data_i[idx, 2])*anchors[i, j].0
                    val h = exp(data_i[idx, 3])*anchors[i, j].1
                    boxes[k, 0] = x
                    boxes[k, 1] = y
                    boxes[k, 2] = w
                    boxes[k, 3] = h
                    boxes[k, 4] = data_i[idx, 4]
                    for kp <- 5:n0 {boxes[k, kp] = data_i[idx, kp]}
                    k += 1
                }
            }
        }
    }
    boxes
}

// remove boundary boxs with a low detection probability
fun postprocess_boxes( pred_bbox: float [,], orig_img_size: (int*2),
                       input_size: int, score_threshold: float)
{
    val (nboxes, box_data_size) = pred_bbox.size()
    val cls0 = 5, nclasses = box_data_size - cls0
    assert(nclasses > 0)
    val new_boxes = array(nboxes, (0.f, 0.f, 0.f, 0.f, 0.f, 0.f))
    val org_h = float(orig_img_size.0), org_w = float(orig_img_size.1)
    val resize_ratio = min(input_size/org_w, input_size/org_h)
    val dw = (input_size - resize_ratio*org_w)/2
    val dh = (input_size - resize_ratio*org_h)/2
    var j = 0

    for i <- 0:nboxes {
        val x = pred_bbox[i, 0], y = pred_bbox[i, 1], w = pred_bbox[i, 2], h = pred_bbox[i, 3]
        val obj_conf = pred_bbox[i, 4]
        val xmin = x - w*0.5f, ymin = y - h*0.5f, xmax = x + w*0.5f, ymax = y + h*0.5f
        if isnan(x) || isnan(y) || isnan(w) || isnan(h) || isinf(xmax) || isinf(ymax) {continue}

        val xmin = (xmin - dw)/resize_ratio, xmax = (xmax - dw)/resize_ratio
        val ymin = (ymin - dh)/resize_ratio, ymax = (ymax - dh)/resize_ratio
        val xmin = max(xmin, 0.f), xmax = min(xmax, org_w - 1.f)
        val ymin = max(ymin, 0.f), ymax = min(ymax, org_h - 1.f)
        if xmin >= xmax || ymin >= ymax {continue}

        val fold best_class=cls0, pmax = pred_bbox[i, cls0] for k <- 1:nclasses {
            val p = pred_bbox[i, k+cls0]
            if p > pmax {(k, p)} else {(best_class, pmax)}
        }
        val score = obj_conf * pmax
        if score <= score_threshold {continue}

        new_boxes[j] = (xmin, ymin, xmax, ymax, score, float(best_class))
        j += 1
    }
    new_boxes[:j]
}

// calculate the Intersection-Over-Union value
fun bboxes_iou(box1: yolo_detection_t, box2: yolo_detection_t): float
{
    val area1 = (box1.2 - box1.0)*(box1.3 - box1.1)
    val area2 = (box2.2 - box2.0)*(box2.3 - box2.1)

    val xmin = max(box1.0, box2.0)
    val ymin = max(box1.1, box2.1)
    val xmax = min(box2.2, box2.2)
    val ymax = min(box2.3, box2.3)
    val dx = max(xmax - xmin, 0.f)
    val dy = max(ymax - ymin, 0.f)
    val inter_area = dx*dy
    val union_area = area1 + area2 - inter_area

    inter_area/max(union_area, FLT_EPSILON)
}

type nms_method_t = NMS_DEFAULT | NMS_SOFT

/*
bboxes: (xmin, ymin, xmax, ymax, score, class)

Note: soft-nms, https://arxiv.org/pdf/1704.04503.pdf
        https://github.com/bharatsingh430/soft-nms
*/
fun nms(bboxes: yolo_detection_t [], iou_threshold: float,
        ~sigma: float=0.3f, ~method: nms_method_t=NMS_DEFAULT)
{
    val nboxes = bboxes.size()
    println(f"starting nms; nboxes={nboxes}")
    var nbest_boxes = 0
    val niters = 1000
    val sorted_boxes: yolo_detection_t [] = bboxes.copy()
    val zbox = (0.f, 0.f, 0.f, 0.f, 0.f, -1.f)
    val best_boxes = array(nboxes, zbox)
    sort(sorted_boxes, fun (a, b) {
        a.5 < b.5 || (a.5 == b.5 && (a.4 > b.4 ||
            (a.4 == b.4 && (a.0, a.1, a.2, a.3) < (b.0, b.1, b.2, b.3)))) })

    var i0 = 0, i1 = 0
    while i0 < nboxes {
        val cls = sorted_boxes[i0].5
        i1 = i0+1
        while i1 < nboxes && sorted_boxes[i1].5 == cls {i1 += 1}
        for k <- 0:niters {
            val fold ibest = -1, pmax = 0.f for i <- i0:i1 {
                val p = sorted_boxes[i].4
                if sorted_boxes[i].5 == cls && p > pmax {(i, p)} else {(ibest, pmax)}
            }
            ignore(pmax)
            if ibest < 0 {break}
            val best_box = sorted_boxes[ibest]
            best_boxes[nbest_boxes] = best_box
            nbest_boxes += 1
            if ibest > i0 {sorted_boxes[ibest]=sorted_boxes[i0]}
            var j = i1
            for i <- i1-1:i0:-1 {
                val iou = bboxes_iou(best_box, sorted_boxes[i])
                if method == NMS_DEFAULT {
                    if iou <= iou_threshold {
                        j -= 1
                        if i < j {sorted_boxes[j] = sorted_boxes[i]}
                    }
                } else {
                    val w = exp(-iou*iou/sigma)*sorted_boxes[i].4
                    if w > 0 {
                        j -= 1
                        if i < j {sorted_boxes[j] = sorted_boxes[i]}
                        sorted_boxes[j].4 = w
                    }
                }
            }
            i0 = j
        }
        i0 = i1
    }
    best_boxes[:nbest_boxes]
}

fun yolov4_postprocess(yolo_outputs: Ast.nntensor_t [],
    ~orig_image_size: (int*2), ~input_size: int,
    ~score_threshold: float, ~nms_threshold: float,
    ~anchors: (float*2) [,], ~strides: int [], ~xyscale: float [])
{
    val boxes = extract_boxes(yolo_outputs, anchors, strides, xyscale)
    val boxes = postprocess_boxes(boxes, orig_image_size, input_size, score_threshold)
    nms(boxes, nms_threshold)
}
