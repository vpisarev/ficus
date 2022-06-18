/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Json, Sys, LexerUtils as Lxu
import OpenCV as cv
import Color
//import Image.Decoder
import NN.Ast, NN.Inference, NN.FromOnnx, NN.ConstFold, NN.FuseBasic, NN.BufferAllocator, NN.OpConv, NN.OpYolo

val coco_class_names =
    ["person", "bicycle", "car", "motorbike", "aeroplane", "bus", "train", "truck", "boat", "traffic light",
    "fire hydrant", "stop sign", "parking meter", "bench", "bird", "cat", "dog", "horse", "sheep", "cow",
    "elephant", "bear", "zebra", "giraffe", "backpack", "umbrella", "handbag", "tie", "suitcase", "frisbee",
    "skis", "snowboard", "sports ball", "kite", "baseball bat", "baseball glove", "skateboard", "surfboard",
    "tennis racket", "bottle", "wine glass", "cup", "fork", "knife", "spoon", "bowl", "banana", "apple",
    "sandwich", "orange", "broccoli", "carrot", "hot dog", "pizza", "donut", "cake", "chair", "sofa",
    "potted plant", "bed", "dining table", "toilet", "tvmonitor", "laptop", "mouse", "remote", "keyboard",
    "cell phone", "microwave", "oven", "toaster", "sink", "refrigerator", "book", "clock", "vase", "scissors",
    "teddy bear", "hair drier", "toothbrush"]

fun draw_boxes(image: (uint8*3) [,], bboxes: (float*6) [], ~class_names: string [], ~show_labels:bool=true)
{
    val nclasses = class_names.size()
    val colors = [for i <- 0:nclasses {
        val rgb = Color.hsv2rgb(i*360.f/nclasses, 1.f, 1.f)
        (rgb.2*255., rgb.1*255., rgb.0*255., 255.)
    }]

    for (x0, y0, x1, y1, conf, clsid) <- bboxes {
        cv.rectangle(image, (round(x0), round(y0)),
            (round(x1), round(y1)), colors[int(clsid)], thickness=3)
    }
}

var mname = "", lname = ""
var images: string list = []
var ntasks = 0
var use_f16 = false
var temp_name = ""

fun parse_args(args: string list)
{
    | "-ntasks" :: ntasks_ :: rest =>
        ntasks = ntasks_.to_int_or(0); parse_args(rest)
    | "-f16" :: rest =>
        use_f16 = true; parse_args(rest)
    | "-labels" :: lname_ :: rest =>
        lname = lname_; parse_args(rest)
    | "-temp" :: tname :: rest =>
        temp_name = tname; parse_args(rest)
    | "-model" :: mname_ :: rest =>
        mname = mname_; parse_args(rest)
    | optname :: _ when optname.startswith('-') =>
        println(f"error: unknown option {optname}"); throw Fail("")
    | imgname :: rest =>
        images = imgname :: images; parse_args(rest)
    | _ => {}
}

parse_args(Sys.arguments())
images = images.rev()
//println(f"model='{mname}', lname='{lname}', images={images}")

val labels: string [] =
    if lname == "" {[]}
    else {
        try {
            match Json.parse_file(lname) {
            | Json.Map(entries) =>
                val fold maxk = 0 for (k, v) <- entries {
                    val ik = k.to_int().value_or(-1)
                    assert(ik >= 0)
                    max(maxk, ik)
                }
                val labels_arr = array(maxk + 1, "unknown")
                for (k, v) <- entries {
                    val ik = k.to_int().value_or(-1)
                    val v = match v {
                        | Json.Str(s) => s
                        | _ => println(f"invalid label for {k}"); throw Fail("")
                        }
                    labels_arr[ik] = v
                }
                labels_arr
            | _ => println("invalid labels"); throw Fail("")
            }
        } catch {
        | Lxu.LexerError(lloc, msg) => println(f"{lname}:{lloc.0}: error: {msg}"); throw Fail("")
        }
    }

/*val fface = cv.makeFontFace("sans")
val fontSize = 20

for imgname@i <- images {
    val img = cv.imread(imgname)
    val inp = cv.blobFromImage(img, size=(224, 224),
            mean=(104.00698793, 116.66876762, 122.67891434),
            swapRB=false, crop=false)
    val probs = net.forward(inp)
    val (_, _, _, n) = size(probs)
    val tprobs = [for i <- 0:n {(probs[0, 0, 0, i], i)}]
    sort(tprobs, (>))
    for j <- 0:5 {
        val lbl = if labels != [] {labels[tprobs[j].1]} else {f"class_{tprobs[j].1}"}
        val p = tprobs[j].0
        cv.putText(img, f"{lbl}: p={round(p, 3)}",
            (10, 30 + j*(fontSize+5)), cv.RGB(0, 255, 0), weight=400, fontFace=fface, size=fontSize)
    }
    cv.imshow(f"image #{i+1}", img)
}
cv.waitKey()
*/

val model =
    try NN.FromOnnx.read(mname)
    catch {
    | NN.FromOnnx.OnnxConvertError(msg) =>
        println(f"error: {msg}"); throw Fail("")
    | Fail(msg) =>
        println(f"error: {msg}"); throw Fail("")
    }
val model = NN.ConstFold.cfold(model)
val model = NN.FuseBasic.fuse_basic(model)
val model = NN.BufferAllocator.assign_buffers(model)
println(model)
val k = 5
val temp_outputs = if temp_name != "" {
    [for i <- [temp_name] {(i, NN.Ast.empty_tensor())}]
} else {[]}

type nn_output_t = (string, NN.Ast.nntensor_t)
if ntasks > 0 {
    *model.ntasks = ntasks
}
*model.use_f16 = use_f16
val size0 = 416

for imgname@i <- images {
    //println(f"starting reading model '{mname}'")
    val img = cv.imread(imgname)
    val (h, w) = img.size()
    val resized_img = if h == w {
            cv.resize(img, (size0, size0), interpolation=cv.INTER_LINEAR)
        } else {
            val (w, h) = if w > h {(size0, (h*size0+w/2)/w)} else {((w*size0+h/2)/h, size0)}
            val resized_img = cv.resize(img, (w, h), interpolation=cv.INTER_LINEAR)
            val canvas = array((size0, size0), (128u8, 128u8, 128u8))
            val y0 = (size0-h)/2
            val x0 = (size0-w)/2
            canvas[y0:y0+h,x0:x0+w] = resized_img
            canvas
        }
    val resized_img = cv.cvtColor(resized_img, cv.COLOR_BGR2RGB)
    //cv.imshow("original", img)
    //cv.imshow("test", resized_img)
    //ignore(cv.waitKey())

    val inp = reshape_multichan(resized_img, (1, size0, size0, 3)).*(1.f/255)
    println(inp.size())
    val inp_ = NN.Ast.make_tensor(inp)
    var outputs: nn_output_t [] = []
    NN.OpConv.reset_min_total_time_1x1()
    val niters = 10
    val (gmean, mintime) = Sys.timeit(
        fun () {
            outputs =
            try NN.Inference.run(model, [("", inp_)], outputs=temp_outputs) catch {
            | NN.Ast.NNError msg => println(f"exception NNError('{msg}') occured"); []
            | Fail msg => println(f"failure: '{msg}'"); []
            }
        }, iterations=niters, batch=1)
    val total_time = NN.OpConv.get_total_time_1x1()*1000/Sys.tick_frequency()
    println(f"execution time: gmean={gmean*1000.}, mintime={mintime*1000.}, 1x1 total={total_time} ms")
    //println(out[0][:][:20])
    for out@i <- [\outputs, \temp_outputs] {
        println(f"output #{i}: name='{out.0}', shape={out.1.shape}")
        /*val out_data = match out.1.data {
            | NN.Ast.NN_Data_FP32 out_data => out_data
            | _ => throw NN.Ast.NNError("floating-point data is expected")
        }
        val oshape = out.1.shape.shape
        val m0 = 5, n0 = 8

        println(f"========================= output {i} ======================")
        if oshape.size() == 5 {
            val _ = oshape[1], w = oshape[2], m = oshape[3], n = oshape[4]
            for y <- 0:3 {
                if y > 0 {
                    println(f"---------------------- y = {y} --------------------")
                }
                for x <- 0:m0 {
                    for j <- 0:m {
                        print(f"y = {y}, x = {x}, j = {j}: ")
                        for k <- 0:n0 {
                            val idx = ((y*w + x)*m + j)*n + k
                            print(f"{out_data[idx]}, ")
                        }
                        println()
                    }
                }
            }
        } else {
            val N = oshape[0], C = oshape[1], H = oshape[2], W = oshape[3]
            for c <- 0:min(n0, C) {
                if c > 0 {
                    println(f"---------------------- c = {c} --------------------")
                }
                for y <- 0:n0 {
                    print(f"y={y}: ")
                    for x <- 0:n0 {
                        val idx = (c*H + y)*W + x
                        print(f"{out_data[idx]}, ")
                    }
                    println()
                }
            }
        }*/
        /*val sorted_k = NN.Inference.top_k(out.1, k)
        for j <- 0:k {
            val (label, prob) = sorted_k[0, j]
            val label_str = if labels != [] {labels[label]} else {f"class_{label}"}
            println(f"\t{j+1}. label={label_str} ({label}), prob={prob}")
        }*/
    }
    val outputs = [for (_, out) <- outputs[:3] {out}]
    val boxes = NN.OpYolo.yolov4_postprocess(
        outputs, orig_image_size=(h, w), input_size=size0,
        score_threshold=0.25f, nms_threshold=0.22f,
        anchors=NN.OpYolo.yolov4_default_anchors,
        strides=NN.OpYolo.yolov4_default_strides,
        xyscale=NN.OpYolo.yolov4_default_scale)
    draw_boxes(img, boxes, class_names=coco_class_names)
    cv.imshow("detection", img)
    ignore(cv.waitKey())
}
