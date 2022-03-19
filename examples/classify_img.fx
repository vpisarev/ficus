import Json, Sys, LexerUtils as Lxu
//import OpenCV as cv
import DL.Ast, DL.FromOnnx, DL.BufferAllocator

var mname = "", lname = ""
var images: string list = []

fun parse_args(args: string list)
{
    | "-labels" :: lname_ :: rest =>
        lname = lname_; parse_args(rest)
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

/*val net = cv.readNet(mname)
val fface = cv.makeFontFace("sans")
val fontSize = 20

for imgname@i <- images {
    val img = cv.imread(imgname)
    val inp = cv.blobFromImage(img, size=(224, 224),
            mean=(104.00698793, 116.66876762, 122.67891434),
            swapRB=false, crop=false)
    val probs = net.forward(inp)
    val (_, _, _, n) = size(probs)
    val tprobs = [|for i <- 0:n {(probs[0, 0, 0, i], i)}|]
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

println(f"starting reading model '{mname}'")
val model =
    try DL.FromOnnx.read(mname)
    catch {
    | DL.FromOnnx.OnnxConvertError(msg) =>
        println(f"error: {msg}"); throw Fail("")
    | Fail(msg) =>
        println(f"error: {msg}"); throw Fail("")
    }
println(f"dumping model '{mname}'")
val model = DL.BufferAllocator.assign_buffers(model)
println(model)
