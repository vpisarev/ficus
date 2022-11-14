import Filename, Json, Sys, LexerUtils as Lxu
import Image.Decoder
import NN.Ast, NN.Inference, NN.Labels, NN.Preprocess, NN.Jit
//To build with jit, use next line:
//bin/ficus -rebuild examples/vision_classify.fx -verbose -D HAVE_JIT -cflags "-I $HOME/work/loops/include/" -clibs "-L $HOME/work/build/loops"
type classifier_kind =
    | ClassifierAuto
    | ClassifierAlexNet
    | ClassifierResnet
    | ClassifierEfficientNet

var mname = "", lname = ""
var images: string list = []
var labels: string [] = []
var niter = 15
var ntasks = 4
var use_fp16 = false
var trace = false
var profile = false
var detailed_profile = false
var use_jit = false
var jit_ctx = ref null
var use_jit = false
var dump = false
var classifier_kind = ClassifierAuto

val preprocess_params_alexnet = NN.Preprocess.image_preprocess_params_t {
    input_size = (224, 224),
    resize_mode = NN.Preprocess.ResizeCropCenter,
    mean = (123.68f, 116.779f, 103.939f),
    scale = (1.f, 1.f, 1.f),
    swaprb = true,
    layout = NN.Ast.NN_Layout_NCHW,
    elemtype = Type_F32
}

val preprocess_params_resnet = NN.Preprocess.image_preprocess_params_t {
    input_size = (224, 224),
    resize_mode = NN.Preprocess.ResizeCropCenter,
    mean = (123.68f, 116.779f, 103.939f),
    scale = (1.f/(255*0.229f), 1.f/(255*0.224f), 1.f/(255*0.225f)),
    swaprb = true, // ???
    layout = NN.Ast.NN_Layout_NCHW,
    elemtype = Type_F32
}

val preprocess_params_efficientnet = NN.Preprocess.image_preprocess_params_t {
    input_size = (224, 224),
    resize_mode = NN.Preprocess.ResizeCropCenter,
    mean = (127.f, 127.f, 127.f),
    scale = (1.f/128, 1.f/128, 1.f/128),
    swaprb = false,
    layout = NN.Ast.NN_Layout_NHWC,
    elemtype = Type_F32
}

fun parse_args(args: string list)
{
    | "-ntasks" :: ntasks_ :: rest =>
        ntasks = ntasks_.to_int_or(0); parse_args(rest)
    | "-fp16" :: rest =>
        use_fp16 = true; parse_args(rest)
    | "-fp32" :: rest =>
        use_fp16 = false; parse_args(rest)
    | "-trace" :: rest =>
        trace = true; parse_args(rest)
    | "-labels" :: lname_ :: rest =>
        lname = lname_; parse_args(rest)
    | "-dump" :: rest =>
        dump = true; parse_args(rest)
    | "-model" :: mname_ :: rest =>
        mname = mname_; parse_args(rest)
    | "-profile" :: rest =>
        profile = true; parse_args(rest)
    | "-detailed-profile" :: rest =>
        detailed_profile = true; parse_args(rest)
    | "-use_jit" :: rest =>
        use_jit = true; jit_ctx = ref NN.Jit.create_context(); parse_args(rest)
    | "-niter" :: niter_ :: rest =>
        niter = match niter_.to_int() {
            | Some(n) => n
            | _ =>
                println(f"error: expected integer argument of '-niter' option")
                throw Fail("")
            }
        parse_args(rest)
    | optname :: _ when optname.startswith('-') =>
        println(f"error: unknown option {optname}"); throw Fail("")
    | imgname :: rest =>
        images = imgname :: images; parse_args(rest)
    | _ => {}
}

parse_args(Sys.arguments())
images = images.rev()

if classifier_kind == ClassifierAuto {
    val mname_upcase = Filename.basename(mname).toupper()
    if mname_upcase.contains("EFFICIENTNET") {
        classifier_kind = ClassifierEfficientNet
    } else if mname_upcase.contains("GOOGLENET") ||
              mname_upcase.contains("INCEPTION") ||
              mname_upcase.contains("ALEXNET") ||
              mname_upcase.contains("SQUEEZENET") {
        classifier_kind = ClassifierAlexNet
    } else {
        classifier_kind = ClassifierResnet
    }
}

val preprocess_params = match classifier_kind {
    | ClassifierEfficientNet => preprocess_params_efficientnet
    | ClassifierAlexNet => preprocess_params_alexnet
    | _ => preprocess_params_resnet
}

if labels == [] {labels = NN.Labels.ImageNet}

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
val top_k = 5
val lname = "output"

type nn_output_t = (string, NN.Ast.nntensor_t)
if ntasks > 0 {
    *model.ntasks = ntasks
}

*model.profile = profile
*model.trace = trace
if trace || detailed_profile {
    *model.detailed_profile = true
}
*model.use_fp16 = use_fp16
*model.use_jit = use_jit
*model.jit_ctx = *jit_ctx

val preprocess_params =
    if use_fp16 {preprocess_params.{elemtype=Type_F16}}
    else {preprocess_params}
println(f"network input type: {preprocess_params.elemtype}")

for imgname@i <- images {
    println(f"processing '{imgname}' ...")
    val img = Image.Decoder.imread_rgb(imgname)
    val inp = NN.Preprocess.image_to_tensor(img, preprocess_params, ntasks)
    var outputs: nn_output_t [] = []
    var best_profile = model.perf_profile_time.copy()
    val (gmean, mintime) = Sys.timeit(
        fun () {
            if detailed_profile || trace {println("----------------------------")}
            outputs =
            try NN.Inference.run(model, [("", inp)], outputs=[]) catch {
            | NN.Ast.NNError msg => println(f"exception NNError('{msg}') occured"); []
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
        println(f"output #{i}: name='{out.0}', shape={out.1.shape}: top-{top_k}:")
        val sorted_k = NN.Inference.top_k(out.1, top_k)
        val top_k = min(top_k, sorted_k.size().1)
        for j <- 0:top_k {
            val (label, prob) = sorted_k[0, j]
            val label_str = if labels != [] {labels[label]} else {f"class_{label}"}
            println(f"\t{j+1}. label={label_str} ({label}), prob={prob}")
        }
    }
}
