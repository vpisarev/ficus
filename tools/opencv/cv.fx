/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/* The first attempt to wrap some OpenCV functionality */

import Sys

pragma "c++", "clib:opencv_core", "clib:opencv_imgproc", "clib:opencv_videoio", "clib:opencv_highgui", "clib:opencv_imgcodecs"

@ccode {
#include <limits.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"
#include "opencv2/highgui.hpp"
#include "opencv2/imgcodecs.hpp"
#include "opencv2/videoio.hpp"
}

exception OpenCVError : string

type uint8x3 = (uint8*3)
type uint8x4 = (uint8*4)
type uint16x3 = (uint16*3)
type uint16x4 = (uint16*4)

type depth_t =
    | DEPTH_U8 | DEPTH_S8 | DEPTH_U16 | DEPTH_S16 | DEPTH_U32 | DEPTH_S32
    | DEPTH_U64 | DEPTH_S64 | DEPTH_FP16 | DEPTH_BF16 | DEPTH_FP32 | DEPTH_FP64

type elemtype_t = (depth_t, int)

fun elemtype(_: 't [+]) = (elemdepth(0:>'t), 1)
fun elemtype(_: ('t*2) [+]) = (elemdepth(0:>'t), 2)
fun elemtype(_: ('t*3) [+]) = (elemdepth(0:>'t), 3)
fun elemtype(_: ('t*4) [+]) = (elemdepth(0:>'t), 4)
fun elemtype(_: ('t*5) [+]) = (elemdepth(0:>'t), 5)
fun elemtype(_: ('t*6) [+]) = (elemdepth(0:>'t), 6)
fun elemtype(_: ('t*7) [+]) = (elemdepth(0:>'t), 7)
fun elemtype(_: ('t*8) [+]) = (elemdepth(0:>'t), 8)
fun elemtype(_: ('t*9) [+]) = (elemdepth(0:>'t), 9)
fun elemtype(_: ('t*10) [+]) = (elemdepth(0:>'t), 10)

fun elemdepth(_: uint8) = DEPTH_U8
fun elemdepth(_: int8) = DEPTH_S8
fun elemdepth(_: uint16) = DEPTH_U16
fun elemdepth(_: int16) = DEPTH_S16
fun elemdepth(_: int32) = DEPTH_S32
fun elemdepth(_: float) = DEPTH_FP32
fun elemdepth(_: double) = DEPTH_FP64

type anyarr_t = (uint8 [,], depth_t, int)

// basically, this is violation of the type system; use with care
@nothrow fun reinterpret(x: 'from [+]): 'to [+] = @ccode
{
    fx_copy_arr(x, fx_result);
}
fun anyarray(x: 't [+]): anyarr_t
{
    val (depth, channels) = elemtype(x)
    ((reinterpret(x): uint8 [,]), depth, channels)
}

@ccode {

enum {
    _FX_DEPTH_U8 = 1,
    _FX_DEPTH_S8,
    _FX_DEPTH_U16,
    _FX_DEPTH_S16,
    _FX_DEPTH_U32,
    _FX_DEPTH_S32,
    _FX_DEPTH_U64,
    _FX_DEPTH_S64,
    _FX_DEPTH_FP16,
    _FX_DEPTH_BF16,
    _FX_DEPTH_FP32,
    _FX_DEPTH_FP64
};

FX_EXTERN_C int _fx_M2cvFM16make_OpenCVErrorE1S(fx_str_t* arg0, fx_exn_t* fx_result);
typedef struct _fx_anyarr_t
{
    fx_arr_t arr;
    int depth;
    int_ channels;
} _fx_anyarr_t;

static int fx_ocv_err(const char* msg)
{
    fx_exn_t exn0 = {};
    int fx_status = 0;
    fx_str_t wmsg;

    fx_status = fx_cstr2str(msg, -1, &wmsg);
    if(fx_status >= 0) {
        FX_CALL(_fx_M2cvFM16make_OpenCVErrorE1S(&wmsg, &exn0), _fx_cleanup);
        FX_THROW(&exn0, true, _fx_cleanup);
    } else {
        FX_UPDATE_BT();
    }
_fx_cleanup: ;
    return fx_status;
}

#define FX_OCV_STR(a) #a

#define FX_OCV_TRY_CATCH(try_stmt) \
    try { \
        if (fx_status >= 0) { \
            try_stmt \
        } \
    } catch (const std::exception& e) { \
        fx_status = fx_ocv_err(e.what()); \
    } catch (...) { \
        fx_status = fx_ocv_err("unknown exception at " __FILE__ ":" FX_OCV_STR(__LINE__)); \
    }

static int depth2cv(int depth)
{
    static const int cvdepth_tab[] = {
        CV_8U, CV_8S, CV_16U, CV_16S, -1, CV_32S, -1, -1, CV_16F, -1, CV_32F, CV_64F
    };
    return depth < 1 || depth > 12 ? -1 : cvdepth_tab[depth-1];
}

static int cvt_to(const _fx_anyarr_t* src, cv::Mat& dst)
{
    int cvdepth = depth2cv(src->depth);
    int channels = (int)src->channels;
    int i, ndims = src->arr.ndims;
    int size[FX_MAX_DIMS];
    size_t step[FX_MAX_DIMS];

    if(!src->arr.data) {
        dst = cv::Mat();
        return FX_OK;
    }

    if(cvdepth < 0)
        FX_FAST_THROW_RET(FX_EXN_BadArgError);

    for(i = 0; i < ndims; i++) {
        if (src->arr.dim[i].size > INT_MAX || src->arr.dim[i].step <= 0)
            FX_FAST_THROW_RET(FX_EXN_SizeError);
        size[i] = (int)src->arr.dim[i].size;
        step[i] = (size_t)src->arr.dim[i].step;
    }

    dst = cv::Mat(ndims, size, CV_MAKETYPE(cvdepth, channels),
                  (void*)src->arr.data, step);
    return FX_OK;
}

// [TODO] need to implement Ficus-based cv::MatAllocator,
// so that we don't need to physically copy the output arrays
static int cvt_from(const cv::Mat& src, int dstdims, int dstdepth, int_ dstchannels, fx_arr_t* dst)
{
    const int cvdepth_tab[] = {CV_8U, CV_8S, CV_16U, CV_16S, CV_32S, CV_16F, -1, CV_32F, CV_64F};
    int i, ndims = src.dims;
    int_ size[FX_MAX_DIMS];
    int status;
    cv::Mat cvdst;

    if(!src.data) {
        memset(dst, 0, sizeof(*dst));
        return FX_OK;
    }

    if( src.dims > FX_MAX_DIMS ||
        depth2cv(dstdepth) != src.depth() ||
        dstchannels != src.channels() ||
        dstdims != src.dims)
        FX_FAST_THROW_RET(FX_EXN_TypeMismatchError);

    for(i = 0; i < ndims; i++)
        size[i] = src.size.p[i];
    status = fx_make_arr(ndims, size, src.elemSize(), 0, 0, 0, dst);
    if(status < 0)
        return status;
    {
    _fx_anyarr_t tmpsrc = {*dst, dstdepth, dstchannels};
    status = cvt_to(&tmpsrc, cvdst);
    }
    if(status < 0)
        return status;
    src.copyTo(cvdst);
    if((char*)cvdst.data != dst->data)
        FX_FAST_THROW_RET(FX_EXN_BadArgError);
    return FX_OK;
}

static int cvt_to(const fx_str_t* src, std::string& dst)
{
    fx_cstr_t c_src;
    int fx_status = fx_str2cstr(src, &c_src, 0, 0);
    if(fx_status >= 0) {
        dst.assign(c_src.data);
        fx_free_cstr(&c_src);
    }
    return fx_status;
}

}

fun imread(filename: string): uint8x3 [,] = @ccode
{
    std::string c_filename;
    int fx_status = cvt_to(filename, c_filename);
    FX_OCV_TRY_CATCH(
        cv::Mat dst = cv::imread(c_filename, 1);
        fx_status = cvt_from(dst, 2, _FX_DEPTH_U8, 3, fx_result);
    )
    return fx_status;
}

fun imwrite_(filename: string, img: anyarr_t): void = @ccode
{
    std::string c_filename;
    cv::Mat c_img;
    int fx_status = cvt_to(filename, c_filename);
    if (fx_status >= 0)
        fx_status = cvt_to(img, c_img);
    FX_OCV_TRY_CATCH(
        cv::imwrite(c_filename, c_img);
    )
    return fx_status;
}

fun imwrite(filename: string, img: 't [,]) = imwrite_(filename, anyarray(img))

@private fun imshow_(window: string, img: anyarr_t): void = @ccode
{
    std::string c_window;
    cv::Mat c_img;
    int fx_status = cvt_to(window, c_window);
    if(fx_status >= 0)
        fx_status = cvt_to((_fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::imshow(c_window, c_img);
    )
    return fx_status;
}

fun imshow(window: string, img: 't [,]) = imshow_(window, anyarray(img))

fun waitKey(delay: int) {
    @nothrow fun waitKey_(delay: int): int = @ccode
    {
        return cv::waitKey((int)delay);
    }
    var delay_ = delay
    if delay_ < 0 {throw OpenCVError("delay must be non-negative")}
    waitKey_(delay)
}

@ccode
{

typedef struct _fx_ocv_trackbar_callb_t
{
    int (*fp)(int_, void*);
    fx_fcv_t* fcv;
} _fx_ocv_trackbar_callb_t;

static void _fx_ocv_trackbar_callb(int pos, void* userdata)
{
    _fx_ocv_trackbar_callb_t* callb = (_fx_ocv_trackbar_callb_t*)userdata;
    callb->fp(pos, callb->fcv);
}

}

fun createTrackbar(trackbarname: string, window: string,
                   value: int32 ref, count: int,
                   onchange: (int->void)?): void = @ccode
{
    std::string c_trackbarname, c_window;
    int fx_status = cvt_to(trackbarname, c_trackbarname);
    if (fx_status >= 0)
        fx_status = cvt_to(window, c_window);
    FX_OCV_TRY_CATCH(
        _fx_ocv_trackbar_callb_t* callb = 0;
        if (onchange->tag > 1) {
            callb = (_fx_ocv_trackbar_callb_t*)fx_malloc(sizeof(*callb));
            if(!callb)
                FX_FAST_THROW_RET(FX_EXN_OutOfMemError);
            callb->fp = onchange->u.Some.fp;
            callb->fcv = onchange->u.Some.fcv;
            if(callb->fcv)
                FX_INCREF(callb->fcv->rc);
        }
        cv::createTrackbar(
            c_trackbarname, c_window,
            &value->data, (int)count,
            callb ? _fx_ocv_trackbar_callb : 0,
            callb);
    )
    return fx_status;
}

class VideoCapture = { cap: cptr }

@ccode
{
static void _fx_ocv_free_cap(void* ptr)
{
    delete (cv::VideoCapture*)ptr;
}
}

fun captureFromCamera(id: int)
{
    fun captureFromCamera_(id: int): cptr =
    @ccode {
        cv::VideoCapture* cap = 0;
        int fx_status = 0;
        FX_OCV_TRY_CATCH(
            cap = new cv::VideoCapture((int)id);
            if(cap && cap->isOpened()) {
                fx_status = fx_make_cptr(cap, _fx_ocv_free_cap, fx_result);
            } else {
                delete cap;
                fx_status = fx_ocv_err("cannot open the camera");
            }
        )
        return fx_status;
    }
    VideoCapture {cap=captureFromCamera_(id)}
}

fun captureFromFile(filename: string)
{
    fun captureFromFile_(filename: string): cptr =
    @ccode {
        cv::VideoCapture* cap = 0;
        std::string c_filename;
        int fx_status = cvt_to(filename, c_filename);
        FX_OCV_TRY_CATCH(
            cap = new cv::VideoCapture(c_filename);
            if(cap && cap->isOpened()) {
                fx_status = fx_make_cptr(cap, _fx_ocv_free_cap, fx_result);
            } else {
                delete cap;
                fx_status = fx_ocv_err("cannot open the video file");
            }
        )
        return fx_status;
    }
    VideoCapture {cap=captureFromFile_(filename)}
}

fun VideoCapture.read(): uint8x3 [,] =
@ccode
{
    memset(fx_result, 0, sizeof(*fx_result));
    cv::VideoCapture* cap;
    cv::Mat frame;
    int fx_status = 0;
    if (!self->cap || !self->cap->ptr)
        return fx_ocv_err("video stream is not initialized");
    cap = (cv::VideoCapture*)self->cap->ptr;
    if (!cap->isOpened())
        return fx_ocv_err("video stream is not open");
    FX_OCV_TRY_CATCH(
        *cap >> frame;
        fx_status = cvt_from(frame, 2, _FX_DEPTH_U8, 3, fx_result);
    )
    return fx_status;
}

fun Canny(img: uint8 [,], ~threshold1: int, ~threshold2: int,
          ~apertureSize: int=3, ~L2gradient: bool=false): uint8 [,]
{
    fun Canny_(img: uint8 [,], threshold1: int, threshold2: int,
            apertureSize: int, L2gradient: bool): uint8 [,] =
    @ccode {
        memset(fx_result, 0, sizeof(*fx_result));
        cv::Mat c_img, c_edges;
        _fx_anyarr_t img_ = {*img, _FX_DEPTH_U8, 1};
        int fx_status = cvt_to(&img_, c_img);
        FX_OCV_TRY_CATCH(
            cv::Canny(c_img, c_edges, threshold1, threshold2, apertureSize, L2gradient);
            fx_status = cvt_from(c_edges, 2, _FX_DEPTH_U8, 1, fx_result);
        )
        return fx_status;
    }
    Canny_(img, threshold1, threshold2, apertureSize, L2gradient)
}

fun GaussianBlur_(img: anyarr_t, ksize: (int, int), sigma: double, sigmaY: double, borderType: int): uint8 [,] =
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_img, c_dst;
    int fx_status = cvt_to((_fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::GaussianBlur(c_img, c_dst, cv::Size((int)ksize->t0, (int)ksize->t1), sigma, sigmaY, borderType);
        fx_status = cvt_from(c_dst, 2, img->t1.tag, img->t2, fx_result);
    )
    return fx_status;
}

val BORDER_DEFAULT: int = @ccode {cv::BORDER_DEFAULT}
val BORDER_REFLECT_101: int = @ccode {cv::BORDER_REFLECT_101}
val BORDER_REPLICATE: int = @ccode {cv::BORDER_REPLICATE}

fun GaussianBlur(img: 't [,], ksize: (int, int), ~sigma: double, ~sigmaY: double=0.,
                 ~borderType: int=4): 't [,]
{
    (reinterpret(GaussianBlur_(anyarray(img), ksize, sigma, sigmaY, borderType)) : 't [,])
}

fun bgr2gray(img: uint8x3 [,]) =
[| for (b, g, r) <- img {uint8((b*1868 + g*9617 + r*4899 + 8192) >> 14)} |]

val imgname = match Sys.arguments() {x :: _ => x | _ => "lena.jpg"}
val a = imread(imgname)
imshow("test", a)
val pos = ref 0i32
createTrackbar("radius", "test", pos, 10, Some(fun (pos) {
    val k = pos*2+1
    imshow("test", GaussianBlur(a, (k, k), sigma=pos*0.5))
}))
ignore(waitKey(0))

/*val cap = captureFromFile("/Users/vpisarev/work/opencv_extra/testdata/highgui/video/big_buck_bunny.mp4")
while true {
    val a = cap.read()
    if a.empty() {break}
    val blurred = GaussianBlur(bgr2gray(a), (17, 17), sigma=5.)
    //imshow("frame", Canny(bgr2gray(a), threshold1=0, threshold2=30))
    imshow("frame", Canny(blurred, threshold1=0, threshold2=30))
    if waitKey(30) > 0 {break}
}
*/
