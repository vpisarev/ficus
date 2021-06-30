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

// basically, this is violation of the type system; this function is intended
// to be followed immediately by the conversion of anyimage_t to
// immediately
fun anyarray(x: 't [,]): anyarr_t
{
    val (depth, channels) = elemtype(x)
    @nothrow fun reinterpret_arr(x: 't [,]): uint8 [,] = @ccode
    {
        fx_copy_arr(x, fx_result);
    }
    (reinterpret_arr(x), depth, channels)
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
        FX_FAST_THROW_RET(FX_EXN_BadArgError);

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

}

fun imread(filename: string): uint8x3 [,] = @ccode
{
    fx_cstr_t c_filename={0};
    int fx_status = fx_str2cstr(filename, &c_filename, 0, 0);
    FX_OCV_TRY_CATCH(
        cv::Mat dst = cv::imread(c_filename.data, 1);
        fx_status = cvt_from(dst, 2, _FX_DEPTH_U8, 3, fx_result);
    )
    fx_free_cstr(&c_filename);
    return fx_status;
}

fun imshow_(window: string, img: anyarr_t): void = @ccode
{
    fx_cstr_t c_window={0};
    cv::Mat c_img;
    int fx_status;
    fx_status = fx_str2cstr(window, &c_window, 0, 0);
    if(fx_status >= 0)
        fx_status = cvt_to((_fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::imshow(c_window.data, c_img);
    )
    fx_free_cstr(&c_window);
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

val imgname = match Sys.arguments() {x :: _ => x | _ => "lena.jpg"}
val a = imread(imgname)
imshow("test", a)
ignore(waitKey(0))
