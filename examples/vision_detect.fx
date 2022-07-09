/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Filename, Json, Sys, LexerUtils as Lxu
import OpenCV as cv
import Color
//import Image.Decoder
import NN.Ast, NN.Inference, NN.FromOnnx, NN.ConstFold, NN.FuseBasic, NN.BufferAllocator, NN.OpConv, NN.OpDetect

type model_kind_t = DetectorSSD | DetectorYolo | DetectorTinyYolo | DetectorAuto

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

    for (y0, x0, y1, x1, conf, clsid) <- bboxes {
        cv.rectangle(image, (round(x0), round(y0)),
            (round(x1), round(y1)), colors[int(clsid)], thickness=3)
    }
}

var mname = "", lname = ""
var images: string list = []
var ntasks = 0
var use_fp16 = false
var temp_name = ""
var detector_kind = DetectorAuto

fun parse_args(args: string list)
{
    | "-ntasks" :: ntasks_ :: rest =>
        ntasks = ntasks_.to_int_or(0); parse_args(rest)
    | "-fp16" :: rest =>
        use_fp16 = true; parse_args(rest)
    | "-labels" :: lname_ :: rest =>
        lname = lname_; parse_args(rest)
    | "-temp" :: tname :: rest =>
        temp_name = tname; parse_args(rest)
    | "-ssd" :: rest =>
        detector_kind = DetectorSSD; parse_args(rest)
    | "-yolo" :: rest =>
        detector_kind = DetectorYolo; parse_args(rest)
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

var model =
    try NN.FromOnnx.read(mname)
    catch {
    | NN.FromOnnx.OnnxConvertError(msg) =>
        println(f"error: {msg}"); throw Fail("")
    | Fail(msg) =>
        println(f"error: {msg}"); throw Fail("")
    }

if detector_kind == DetectorAuto {
    val mname_upcase = Filename.basename(mname).toupper()
    if mname_upcase.contains("SSD") {
        detector_kind = DetectorSSD
    } else if mname_upcase.contains("YOLO") {
        if mname_upcase.contains("TINY") {
            detector_kind = DetectorTinyYolo
        } else {
            detector_kind = DetectorYolo
        }
    }
}

var ok =
try {
    model = NN.ConstFold.cfold(model)
    model = NN.FuseBasic.fuse_basic(model)
    model = NN.BufferAllocator.assign_buffers(model)
    true
} catch {
    | NN.Ast.NNError msg => println(f"exception NNError('{msg}') occured"); false
    | Fail msg => println(f"failure: '{msg}'"); false
}
if !ok {throw Fail("exiting")}
println(model)

var planar_input = true, ndims0 = 4, input_typ = NN.Ast.NN_FP32
for t_inp@i <- model.graph.inpargs {
    val inparg = model.args[t_inp]
    val shape = inparg.shape.shape
    val ndims = shape.size()
    if i == 0 {
        ndims0 = ndims
        input_typ = inparg.typ
        if (ndims == 3 || ndims == 4) && shape[ndims-1] == 3 {
            planar_input = false
        }
    }
    println(f"input #{i}, '{inparg.name}': {NN.Ast.shape2str(model, inparg.shape)}, typ={inparg.typ}")
}

val (input_size, planar_input) = match detector_kind {
    | DetectorYolo => (416, false)
    | DetectorTinyYolo => (416, true)
    | DetectorSSD => (300, false)
    | _ => (300, planar_input)
    }

val k = 5
val temp_outputs = if temp_name != "" {
    [for i <- [temp_name] {(i, NN.Ast.empty_tensor())}]
} else {[]}

type nn_output_t = (string, NN.Ast.nntensor_t)
if ntasks > 0 {
    *model.ntasks = ntasks
}
*model.use_fp16 = use_fp16

for imgname@i <- images {
    //println(f"starting reading model '{mname}'")
    val img = cv.imread(imgname)
    val (h, w) = img.size()
    val resized_img =
        if h == w {
            cv.resize(img, (input_size, input_size), interpolation=cv.INTER_LINEAR)
        } else {
            val (w, h) = if w > h {(input_size, (h*input_size+w/2)/w)} else {((w*input_size+h/2)/h, input_size)}
            val resized_img = cv.resize(img, (w, h), interpolation=cv.INTER_LINEAR)
            val canvas = array((input_size, input_size), (128u8, 128u8, 128u8))
            val y0 = (input_size-h)/2
            val x0 = (input_size-w)/2
            canvas[y0:y0+h,x0:x0+w] = resized_img
            canvas
        }
    val resized_img = cv.cvtColor(resized_img, cv.COLOR_BGR2RGB)
    val inp = reshape_multichan(resized_img, (1, input_size, input_size, 3))
    val inp_ =
        if input_typ == NN.Ast.NN_FP32 {
            NN.Ast.make_tensor(inp.*(1.f/255))
        } else {
            NN.Ast.make_tensor(inp)
        }
    var outputs: nn_output_t [] = []
    NN.OpConv.reset_min_total_time()
    val niters = 1
    val (gmean, mintime) = Sys.timeit(
        fun () {
            outputs =
            try NN.Inference.run(model, [("", inp_)], outputs=temp_outputs) catch {
            | NN.Ast.NNError msg => println(f"exception NNError('{msg}') occured"); []
            | Fail msg => println(f"failure: '{msg}'"); []
            }
        }, iterations=niters, batch=1)
    val total_time = NN.OpConv.get_total_time()*1000/Sys.tick_frequency()
    println(f"execution time: gmean={gmean*1000.}, mintime={mintime*1000.}, conv total={total_time} ms")
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
    }
    val outputs = [for (_, out) <- outputs {out}]

    val boxes = match detector_kind {
        | DetectorYolo => NN.OpDetect.yolov4_postprocess(
            outputs, orig_image_size=(h, w), input_size=input_size,
            score_threshold=0.25f, nms_threshold=0.22f,
            anchors=NN.OpDetect.yolov4_default_anchors,
            strides=NN.OpDetect.yolov4_default_strides,
            xyscale=NN.OpDetect.yolov4_default_scale)
        | _ =>
            println(f"num_detections: {int(outputs[3])[0]}")
            NN.OpDetect.ssd_postprocess(outputs, orig_image_size=(h, w), input_size=input_size)
        }
    draw_boxes(img, boxes, class_names=coco_class_names)
    cv.imshow("detection", img)
    ignore(cv.waitKey())
}
