/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
   Yolo v4 post-processing steps;
   Adopted from https://github.com/onnx/models/tree/main/vision/object_detection_segmentation/yolov4
*/

import Ast, Dynvec

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

fun collect_boxes(yolo_outputs: Ast.nntensor_t [],
    anchors: (float*2) [,], strides: int [], xyscale: float [],
    orig_image_size: (int*2), input_size: int, score_threshold: float)
{
    val CLS0 = 5
    val boxes = Dynvec.create(0, (0.f, 0.f, 0.f, 0.f, 0.f, 0.f))
    val org_h = float(orig_image_size.0)
    val org_w = float(orig_image_size.1)
    val resize_ratio = min(input_size/org_w, input_size/org_h)
    val dw = (input_size - resize_ratio*org_w)/2
    val dh = (input_size - resize_ratio*org_h)/2

    // define anchor boxes
    for out@i <- yolo_outputs, xyscale_i <- xyscale, stride_i <- strides {
        val shape_i = out.shape.shape
        assert(shape_i.size() == 5)
        val N = shape_i[0], output_size = shape_i[1], output_size_w = shape_i[2], m = shape_i[3], n = shape_i[4]
        assert(N == 1 && output_size == output_size_w && m == 3 && n > CLS0)
        val xyscale_i_delta = -0.5f*(xyscale_i - 1)
        val nclasses = n - CLS0
        val data_i = match out.data {
            | Ast.NN_Data_FP32 out_data => out_data.reshape(N*output_size*output_size*m, n)
            | _ => throw Ast.NNError("floating-point tensors are expected as Yolo outputs")
            }

        assert(`n == yolo_outputs[0].shape.shape[4]`)
        for y0 <- 0:output_size {
            for x0 <- 0:output_size {
                for j <- 0:m {
                    val idx = (y0*output_size + x0)*m + j
                    val obj_conf = data_i[idx, 4]
                    if obj_conf <= score_threshold {continue}

                    val fold best_class=0, pmax=data_i[idx, CLS0] for k <- 1:nclasses {
                        val p = data_i[idx, k+CLS0]
                        if p > pmax {(k, p)} else {(best_class, pmax)}
                    }
                    val score = max(obj_conf, 0.f)*max(pmax, 0.f)
                    if score <= score_threshold {continue}

                    val x = (sigmoid(data_i[idx, 0])*xyscale_i + xyscale_i_delta + x0)*stride_i
                    val y = (sigmoid(data_i[idx, 1])*xyscale_i + xyscale_i_delta + y0)*stride_i
                    val w = exp(data_i[idx, 2])*anchors[i, j].0
                    val h = exp(data_i[idx, 3])*anchors[i, j].1
                    val xmin = x - w*0.5f, ymin = y - h*0.5f, xmax = x + w*0.5f, ymax = y + h*0.5f
                    if isnan(x) || isnan(y) || isnan(w) || isnan(h) || isinf(xmax) || isinf(ymax) {continue}

                    val xmin = (xmin - dw)/resize_ratio
                    val xmax = (xmax - dw)/resize_ratio
                    val ymin = (ymin - dh)/resize_ratio
                    val ymax = (ymax - dh)/resize_ratio

                    if xmin >= org_w || ymin >= org_h || xmax < 0.f || ymax < 0.f {continue}
                    //println(f"{boxes.count}: {(xmin, ymin, xmax, ymax, score, best_class)}")

                    boxes.do_push((xmin, ymin, xmax, ymax, score, float(best_class)))
                }
            }
        }
    }
    boxes.data[:boxes.count]
}

// calculate the Intersection-Over-Union value
fun bboxes_iou(box1: yolo_detection_t, box2: yolo_detection_t): float
{
    val area1 = (box1.2 - box1.0)*(box1.3 - box1.1)
    val area2 = (box2.2 - box2.0)*(box2.3 - box2.1)

    val xmin = max(box1.0, box2.0)
    val ymin = max(box1.1, box2.1)
    val xmax = min(box1.2, box2.2)
    val ymax = min(box1.3, box2.3)
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
    //println(f"starting nms; nboxes={nboxes}")
    val sorted_idx = mkrange(nboxes)
    sort(sorted_idx, fun (a_idx, b_idx) {
        val a_cls = bboxes[a_idx].5, b_cls = bboxes[b_idx].5
        a_cls < b_cls || (a_cls == b_cls && a_idx < b_idx) })
    val sorted_boxes = [for i <- 0:nboxes {bboxes[sorted_idx[i]]}]
    val niters = 1000
    var nbest_boxes = 0

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
            if nbest_boxes < ibest {
                sorted_boxes[nbest_boxes] = best_box
            }
            //println(f"{nbest_boxes}: {best_box}")
            nbest_boxes += 1
            if ibest > i0 {sorted_boxes[ibest]=sorted_boxes[i0]}
            var j = i1
            for i <- i1-1:i0:-1 {
                val iou = bboxes_iou(best_box, sorted_boxes[i])
                if iou <= iou_threshold {
                    j -= 1
                    if i < j {sorted_boxes[j] = sorted_boxes[i]}
                }
            }
            i0 = j
        }
        i0 = i1
    }
    sorted_boxes[:nbest_boxes]
}

fun yolov4_postprocess(yolo_outputs: Ast.nntensor_t [],
    ~orig_image_size: (int*2), ~input_size: int,
    ~score_threshold: float, ~nms_threshold: float,
    ~anchors: (float*2) [,], ~strides: int [], ~xyscale: float [])
{
    val boxes = collect_boxes(yolo_outputs, anchors, strides, xyscale,
                              orig_image_size, input_size, score_threshold)
    nms(boxes, nms_threshold)
}
