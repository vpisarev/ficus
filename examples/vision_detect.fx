/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

import Filename, Json, Sys, LexerUtils as Lxu
import Color, Drawing.Shapes as draw, Image.Decoder, Image.Encoder
import NN.Ast as Ast, NN.Inference, NN.Labels, NN.OpDetect, NN.Preprocess as Preprocess

type detector_kind_t = DetectorSSD | DetectorYolo | DetectorTinyYolo | DetectorAuto

fun draw_boxes(image: (uint8*3) [,], bboxes: (float*6) [],
               ~class_names: string [], ~show_labels:bool=true)
{
    val nclasses = class_names.size()
    val colors = [for i <- 0:nclasses {
        val hue = ((i*10 + 37) % nclasses)*360.f/nclasses
        val rgb = Color.hsv2rgb(hue, 1.f, 1.f)
        sat_uint8((rgb.2*255., rgb.1*255., rgb.0*255.))
    }]

    val (h, w) = image.size()
    val thickness = if h*w > 300*300 {5} else {3}
    for (y0, x0, y1, x1, conf, clsid)@i <- bboxes {
        val x0_ = round(x0), y0_ = round(y0), x1_ = round(x1), y1_ = round(y1)
        val clsid = int(clsid)
        draw.rectangle(image, (x0_, y0_), (x1_, y1_), colors[clsid], thickness=thickness)
        println(f"object #{i} ({class_names[clsid]}, class_id={clsid}): {(x0, y0, x1-x0+1, y1-y0+1):.1f}, conf={conf:.2f}")
    }
}

var mname = "", lname = ""
var images: string list = []
var ntasks = 4
var use_fp16 = false
var trace = false
var detector_kind = DetectorAuto
var show_boxes = true
var dump = false
var profile = false
var niter = 15
var outname = "output.jpg"

fun parse_args(args: string list)
{
    | "-ntasks" :: ntasks_ :: rest =>
        ntasks = ntasks_.to_int_or(0); parse_args(rest)
    | "-fp16" :: rest =>
        use_fp16 = true; parse_args(rest)
    | "-trace" :: rest =>
        trace = true; parse_args(rest)
    | "-profile" :: rest =>
        profile = true; parse_args(rest)
    | "-niter" :: niter_ :: rest =>
        niter = match niter_.to_int() {
            | Some(n) => n
            | _ =>
                println(f"error: expected integer argument of '-niter' option")
                throw Fail("")
            }
        parse_args(rest)
    | "-ssd" :: rest =>
        detector_kind = DetectorSSD; parse_args(rest)
    | "-yolo" :: rest =>
        detector_kind = DetectorYolo; parse_args(rest)
    | "-tinyyolo" :: rest =>
        detector_kind = DetectorTinyYolo; parse_args(rest)
    | "-noshow" :: rest =>
        show_boxes = false; parse_args(rest)
    | "-o" :: outname_ :: rest =>
        outname = outname_; parse_args(rest)
    | "-no" :: rest =>
        outname = ""; parse_args(rest)
    | "-dump" :: rest =>
        dump = true; parse_args(rest)
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

val model =
try {
    NN.Inference.read_model(mname)
} catch {
| Fail(msg) => println(msg); throw Exit(1)
| e => println(e); throw Exit(1)
}

if dump {
    println(model)
}

var planar_input = true, ndims0 = 4, input_typ = Type_F32
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
    println(f"input #{i}, '{inparg.name}': {Ast.shape2str(model, inparg.shape)}, typ={inparg.typ}")
}

val (input_size, planar_input, labels) = match detector_kind {
    | DetectorYolo => (416, false, NN.Labels.COCO_2014)
    | DetectorTinyYolo => (416, true, NN.Labels.COCO_2014)
    | DetectorSSD => (300, false, NN.Labels.COCO_paper)
    | _ => (300, planar_input, NN.Labels.COCO_2014)
    }

type nn_output_t = (string, Ast.nntensor_t)
if ntasks > 0 {
    *model.ntasks = ntasks
}
*model.profile = profile
*model.trace = trace
if trace {
    *model.detailed_profile = true
}
*model.use_fp16 = use_fp16
if use_fp16 && input_typ == Type_F32 {
    input_typ = Type_F16
}

val preprocess_params = Preprocess.image_preprocess_params_t
{
    input_size = (input_size, input_size),
    resize_mode = Preprocess.ResizeFit,
    mean = (0.f, 0.f, 0.f),
    scale = if input_typ == Type_U8 {(1.f, 1.f, 1.f)}
            else {(1.f/255, 1.f/255, 1.f/255)},
    swaprb = false,
    layout = if planar_input {Ast.NN_Layout_NCHW} else {Ast.NN_Layout_NHWC},
    elemtype = input_typ
}

for imgname@i <- images {
    println(f"processing '{imgname}' ...")
    val img = Image.Decoder.imread_rgb(imgname)
    val (h, w) = img.size()
    val inp = Preprocess.image_to_tensor(img, preprocess_params, ntasks)
    val inputs = match detector_kind {
        | DetectorTinyYolo => [("", inp), ("", NN.Ast.mktensor([float(h), float(w)].reshape(1, 2)))]
        | _ => [("", inp)]
        }

    var outputs: nn_output_t [] = []
    var best_profile = model.perf_profile_time.copy()
    val (gmean, mintime) = Sys.timeit(
        fun () {
            outputs =
            try NN.Inference.run(model, inputs, outputs=[]) catch {
            | Ast.NNError msg => println(f"exception NNError('{msg}') occured"); []
            | Fail msg => println(f"failure: '{msg}'"); []
            }
        }, iterations=niter, batch=1, updated_min=Some(
        fun () {if profile {best_profile=model.perf_profile_time.copy()}}))
    if (profile) {
        model.perf_profile_time[:] = best_profile
        NN.Inference.print_total_profile(model)
    }
    println(f"execution time: gmean={gmean*1000.:.2f}ms, mintime={mintime*1000.:.2f}ms")
    for out@i <- outputs {
        println(f"output #{i}: name='{out.0}', shape={out.1.shape}")
    }
    val outputs = [for (_, out) <- outputs {out}]

    val boxes = match detector_kind {
        | DetectorYolo => NN.OpDetect.yolov4_postprocess(
            outputs, orig_image_size=(h, w), input_size=input_size,
            score_threshold=0.25f, nms_threshold=0.22f,
            anchors=NN.OpDetect.yolov4_default_anchors,
            strides=NN.OpDetect.yolov4_default_strides,
            xyscale=NN.OpDetect.yolov4_default_scale)
        | DetectorSSD =>
            NN.OpDetect.ssd_postprocess(outputs, orig_image_size=(h, w), input_size=input_size)
        | DetectorTinyYolo =>
            NN.OpDetect.tinyyolo_postprocess(outputs, orig_image_size=(h, w), input_size=input_size)
        | _ =>
            println("unrecognized detector type; specify it explicitly via command line options: '-ssd', '-yolo' or '-tinyyolo'")
            throw Fail("...")
        }
    println(f"{imgname}: {boxes.size()} object(s) detected")
    draw_boxes(img, boxes, class_names=labels)
    if outname != "" {
        Image.Encoder.imwrite(outname, img)
    }
    /*if show_boxes {
        cv.imshow("detection", img)
        ignore(cv.waitKey())
    }*/
}
