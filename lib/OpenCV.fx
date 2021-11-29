/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Prototypes of OpenCV wrappers in Ficus
*/

import Sys

pragma "c++", "clib:opencv_core", "clib:opencv_imgproc", "clib:opencv_videoio",
    "clib:opencv_highgui", "clib:opencv_imgcodecs", "clib:opencv_dnn"

@ccode {
#include <limits.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"
#include "opencv2/highgui.hpp"
#include "opencv2/imgcodecs.hpp"
#include "opencv2/videoio.hpp"
#include "opencv2/dnn.hpp"
}

exception OpenCVError : string

type uint8x3 = (uint8*3)
type uint8x4 = (uint8*4)
type uint16x3 = (uint16*3)
type uint16x4 = (uint16*4)
type intx2 = (int*2)
type intx3 = (int*3)
type intx4 = (int*4)
type intx5 = (int*5)
type intx6 = (int*6)
type int32x2 = (int32*2)
type int32x3 = (int32*3)
type int32x4 = (int32*4)
type floatx2 = (float*2)
type floatx3 = (float*3)
type floatx4 = (float*4)
type floatx5 = (float*5)
type floatx6 = (float*6)
type doublex2 = (double*2)
type doublex3 = (double*3)
type doublex4 = (double*4)
type doublex5 = (double*5)
type doublex6 = (double*6)

type depth_t =
    | DEPTH_U8 | DEPTH_S8 | DEPTH_U16 | DEPTH_S16 | DEPTH_U32 | DEPTH_S32
    | DEPTH_U64 | DEPTH_S64 | DEPTH_FP16 | DEPTH_BF16 | DEPTH_FP32 | DEPTH_FP64

type elemtype_t = (depth_t, int)

fun elemdepth(_: uint8) = DEPTH_U8
fun elemdepth(_: int8) = DEPTH_S8
fun elemdepth(_: uint16) = DEPTH_U16
fun elemdepth(_: int16) = DEPTH_S16
fun elemdepth(_: int32) = DEPTH_S32
fun elemdepth(_: float) = DEPTH_FP32
fun elemdepth(_: double) = DEPTH_FP64

fun elemtype(_: 't) = (elemdepth(0:>'t), 1)
fun elemtype(_: ('t*2)) = (elemdepth(0:>'t), 2)
fun elemtype(_: ('t*3)) = (elemdepth(0:>'t), 3)
fun elemtype(_: ('t*4)) = (elemdepth(0:>'t), 4)
fun elemtype(_: ('t*5)) = (elemdepth(0:>'t), 5)
fun elemtype(_: ('t*6)) = (elemdepth(0:>'t), 6)
fun elemtype(_: ('t*7)) = (elemdepth(0:>'t), 7)
fun elemtype(_: ('t*8)) = (elemdepth(0:>'t), 8)
fun elemtype(_: ('t*9)) = (elemdepth(0:>'t), 9)
fun elemtype(_: ('t*10)) = (elemdepth(0:>'t), 10)

fun arrelemtype(_: 't [+]) = elemtype(0:>'t)

type anyarr_t = (uint8 [,], depth_t, int)

// basically, this is violation of the type system; use with care
@nothrow fun reinterpret(x: 'from [+]): 'to [+]
@ccode {
    fx_copy_arr(x, fx_result);
}
fun anyarray(x: 't [+]): anyarr_t
{
    val (depth, channels) = arrelemtype(x)
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

FX_EXTERN_C int _fx_M6OpenCVFM16make_OpenCVErrorE1S(fx_str_t* arg0, fx_exn_t* fx_result);
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
        FX_CALL(_fx_M6OpenCVFM16make_OpenCVErrorE1S(&wmsg, &exn0), _fx_cleanup);
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
    if (ndims == 1) {
        step[1] = step[0];
        size[1] = size[0];
        step[0] *= size[0];
        size[0] = 1;
        ndims = 2;
    }

    dst = cv::Mat(ndims, size, CV_MAKETYPE(cvdepth, channels),
                  (void*)src->arr.data, step);
    return FX_OK;
}

namespace cv
{
#ifndef CV_MAX_DIM
#define CV_MAX_DIM 32
#endif

class FXArrAllocator : public MatAllocator
{
public:
    FXArrAllocator() { stdAllocator = Mat::getStdAllocator(); }
    ~FXArrAllocator() {}

    UMatData* allocate(fx_arr_t* arr, int dims, const int* sizes, int type, size_t* step) const
    {
        UMatData* u = new UMatData(this);
        u->data = u->origdata = (uchar*)arr->data;
        for( int i = 0; i < dims; i++ )
            step[i] = arr->dim[i].step;
        u->size = sizes[0]*step[0];
        u->userdata = arr;
        return u;
    }

    UMatData* allocate(int dims, const int* sizes, int type, void* data, size_t* step, AccessFlag flags, UMatUsageFlags usageFlags) const CV_OVERRIDE
    {
        if( data != 0 )
            return stdAllocator->allocate(dims, sizes, type, data, step, flags, usageFlags);
        size_t esz = CV_ELEM_SIZE(type);
        int_ sz[CV_MAX_DIM];
        int fx_status;
        for( int i = 0; i < dims; i++ )
            sz[i] = sizes[i];
        fx_arr_t* arr = (fx_arr_t*)fx_malloc(sizeof(*arr));
        if(!arr)
            CV_Error_(Error::StsError, ("The ficus array of type=%d, ndims=%d can not be created", type, dims));
        memset(arr, 0, sizeof(*arr));
        fx_status = fx_make_arr(dims, sz, esz, 0, 0, 0, arr);
        if (fx_status < 0)
            CV_Error_(Error::StsError, ("The ficus array of type=%d, ndims=%d can not be created", type, dims));
        return allocate(arr, dims, sizes, type, step);
    }

    bool allocate(UMatData* u, AccessFlag accessFlags, UMatUsageFlags usageFlags) const CV_OVERRIDE
    {
        return stdAllocator->allocate(u, accessFlags, usageFlags);
    }

    void deallocate(UMatData* u) const CV_OVERRIDE
    {
        if(!u)
            return;
        CV_Assert(u->urefcount >= 0);
        CV_Assert(u->refcount >= 0);
        if(u->refcount == 0 && u->userdata != 0)
        {
            fx_arr_t* arr = (fx_arr_t*)u->userdata;
            fx_free_arr(arr);
            fx_free(arr);
            delete u;
        }
    }

    const MatAllocator* stdAllocator;
};
}

static cv::FXArrAllocator g_fxarrAllocator;

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
        (dstdims != src.dims && (dstdims != 1 || src.dims != 2 || src.rows != 1)))
        FX_FAST_THROW_RET(FX_EXN_TypeMismatchError);

    if (src.allocator == &g_fxarrAllocator && src.u->userdata != 0) {
        fx_copy_arr((fx_arr_t*)src.u->userdata, dst);
    } else {
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
    }
    if (dstdims == 1 && src.dims == 2) {
        dst->ndims = 1;
        dst->dim[0].size = dst->dim[1].size;
        dst->dim[0].step = dst->dim[1].step;
    }
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

static cv::Size cvt_size(const int_* sz)
{
    return cv::Size((int)sz[0], (int)sz[1]);
}

static cv::Size2f cvt_size(const float* sz)
{
    return cv::Size2f(sz[0], sz[1]);
}

static cv::Point cvt_point(const int_* sz)
{
    return cv::Point((int)sz[0], (int)sz[1]);
}

static cv::Point2f cvt_point(const float* sz)
{
    return cv::Point2f(sz[0], sz[1]);
}

static cv::Rect cvt_rect(const int_* r)
{
    return cv::Rect((int)r[0], (int)r[1], (int)r[2], (int)r[3]);
}

static void cvt_rect2intx4(const cv::Rect& r, int_* dst)
{
    dst[0] = r.x;
    dst[1] = r.y;
    dst[2] = r.width;
    dst[3] = r.height;
}

static cv::Scalar cvt_scalar(const double* sc)
{
    return cv::Scalar(sc[0], sc[1], sc[2], sc[3]);
}

}

///////////////////////////////////// core /////////////////////////////////

val BORDER_DEFAULT: int = @ccode {cv::BORDER_DEFAULT}
val BORDER_REFLECT_101: int = @ccode {cv::BORDER_REFLECT_101}
val BORDER_REPLICATE: int = @ccode {cv::BORDER_REPLICATE}

val ZEROS=(0.,0.,0.,0.)

val DECOMP_LU: int = @ccode {cv::DECOMP_LU}
val DECOMP_SVD: int = @ccode {cv::DECOMP_SVD}
val DECOMP_EIG: int = @ccode {cv::DECOMP_EIG}
val DECOMP_CHOLESKY: int = @ccode {cv::DECOMP_CHOLESKY}
val DECOMP_QR: int = @ccode {cv::DECOMP_QR}
val DECOMP_NORMAL: int = @ccode {cv::DECOMP_NORMAL}

@pure @nothrow fun borderInterpolate(p: int, len: int, borderType: int): int
@ccode { return cv::borderInterpolate((int)p, (int)len, (int)borderType); }

@private fun copyMakeBorder_(src: anyarr_t, top: int, bottom: int, left: int, right: int,
                   borderType: int, borderValue: doublex4): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((_fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::copyMakeBorder(c_src, c_dst, (int)top, (int)bottom, (int)left, (int)right,
                           (int)borderType, cvt_scalar(&borderValue->t0));
        fx_status = cvt_from(c_dst, 2, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun copyMakeBorder(src: 't [,], ~top: int, ~bottom: int, ~left: int=0, ~right: int=0,
                   ~borderType: int, ~borderValue: doublex4=ZEROS) =
    (reinterpret(copyMakeBorder_(anyarray(src), top, bottom,
        left, right, borderType, borderValue)) : 't [,])

@private fun PSNR_(src1: anyarr_t, src2: anyarr_t, R: double): double
@ccode
{
    cv::Mat c_src1, c_src2;
    int fx_status = cvt_to((_fx_anyarr_t*)src1, c_src1);
    if (fx_status >= 0)
        fx_status = cvt_to((_fx_anyarr_t*)src1, c_src1);
    FX_OCV_TRY_CATCH(
        *fx_result = cv::PSNR(c_src1, c_src2, R);
    )
    return fx_status;
}

fun PSNR(src1: 't [,], src2: 't [,], ~R: double=255): double =
    PSNR_(anyarray(src1), anyarray(src2), R)

/*fun batchDistance(src1: 't [,], src2: 't [,], ~normType: int=NORM_L2,
                  ~K: int=0, ~update: int=0, ~crosscheck: bool=false): (float [,], int [])
fun reduce(src: 't [,], s0: 's, ~dim: int, ~rtype: int): 's []*/
type rotatecode_t = ROTATE_90_CLOCKWISE | ROTATE_180 | ROTATE_90_COUTERCLOCKWISE
@private fun rotate_(src: anyarr_t, rotateCode: rotatecode_t): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::rotate(c_src, c_dst, (int)(rotateCode->tag-1));
        fx_status = cvt_from(c_dst, 2, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun rotate(src: 't [,], rotateCode: rotatecode_t): 't [,] =
    (reinterpret(rotate_(anyarray(src), rotateCode)) : 't [,])

//fun repeat(src: 't [,], ~ny: int, ~nx: int): 't [,]
//fun checkRange(src: 't [+], ~quiet: bool=true, ~minVal: double=-DBL_MAX, ~maxVal: double=DBL_MAX): (bool, intx4)
@private fun patchNans_(arr: anyarr_t, v: double): void
@ccode {
    cv::Mat c_arr;
    int fx_status = cvt_to((const _fx_anyarr_t*)arr, c_arr);
    FX_OCV_TRY_CATCH(
        cv::patchNans(c_arr, v);
    )
    return fx_status;
}
fun patchNans(arr: 't [+], ~v: double=0) = patchNans_(anyarray(arr), v)

//fun gemm(src1: 't [,], src2: 't [,], src3: 't [,], ~alpha: double=1, ~beta: double=0, ~flags: int=0): 't [,]
//fun mulTransposed(src1: 't [,], ~aTa: bool, ~delta: 't [,] = [], ~scale: double=1): 't [,]
@private fun transform_(src: anyarr_t, m: anyarr_t): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_m, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)m, c_m);
    FX_OCV_TRY_CATCH(
        cv::transform(c_src, c_dst, c_m);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun transform(src: 't [], m: 'k [,]): 't [] =
    (reinterpret(transform_(anyarray(src), anyarray(m))): 't [])
fun transform(src: 't [,], m: 'k [,]): 't [,] =
    (reinterpret(transform_(anyarray(src), anyarray(m))): 't [,])

@private fun perspectiveTransform_(src: anyarr_t, m: anyarr_t): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_m, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((_fx_anyarr_t*)m, c_m);
    FX_OCV_TRY_CATCH(
        cv::perspectiveTransform(c_src, c_dst, c_m);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun perspectiveTransform(src: 't [], m: 'k [,]): 't [] =
    (reintrpret(perspectiveTransform_(anyarray(src), anyarray(m))): 't [])
fun perspectiveTransform(src: 't [,], m: 'k [,]): 't [,] =
    (reintrpret(perspectiveTransform_(anyarray(src), anyarray(m))): 't [,])

fun solveCubic(coeffs: double []): double []
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    _fx_anyarr_t coeffs_ = {*coeffs, _FX_DEPTH_FP64, 1};
    cv::Mat c_coeffs, c_roots;
    c_roots.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to(&coeffs_, c_coeffs);
    FX_OCV_TRY_CATCH(
        cv::solveCubic(c_coeffs, c_roots);
        fx_status = cvt_from(c_roots, 1, _FX_DEPTH_FP64, 1, fx_result);
    )
    return fx_status;
}

fun solvePoly(coeffs: double [], ~maxIters: int=300): doublex2 []
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    _fx_anyarr_t coeffs_ = {*coeffs, _FX_DEPTH_FP64, 1};
    cv::Mat c_coeffs, c_roots;
    c_roots.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to(&coeffs_, c_coeffs);
    FX_OCV_TRY_CATCH(
        cv::solvePoly(c_coeffs, c_roots);
        fx_status = cvt_from(c_roots, 1, _FX_DEPTH_FP64, 2, fx_result);
    )
    return fx_status;
}

@private fun eigen_(src: anyarr_t): (bool, uint8 [], uint8 [,])
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_evals, c_evecs;
    c_evals.allocator = &g_fxarrAllocator;
    c_evecs.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        fx_result->t0 = cv::eigen(c_src, c_evals, c_evecs);
        fx_status = cvt_from(c_evals, 1, src->t1.tag, 1, &fx_result->t1);
        if (fx_status >= 0)
            fx_status = cvt_from(c_evecs, 2, src->t1.tag, 1, &fx_result->t2);
    )
    return fx_status;
}

fun eigen(src: 't [,]): (bool, 't [], 't [,])
{
    val (f, values, vectors) = eigen_(anyarray(src))
    (f, (reintrpret(values): 't []), (reinterpet(vectors): 't [,]))
}

@private fun eigenNonSymmetric_(src: anyarr_t): (uint8 [], uint8 [,])
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_evals, c_evecs;
    c_evals.allocator = &g_fxarrAllocator;
    c_evecs.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::eigenNonSymmetric(c_src, c_evals, c_evecs);
        fx_status = cvt_from(c_evals, 1, src->t1.tag, 1, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(c_evecs, 2, src->t1.tag, 1, &fx_result->t1);
    )
    return fx_status;
}

fun eigenNonSymmetric(src: 't [,]): ('t [], 't [,])
{
    val (evals, evecs) = eigenNonSymmtric_(anyarray(src))
    ((reinterpret(evals): 't []), (reinterpet(evecs): 't [,]))
}

@private fun PCACompute_(data: anyarr_t, mean: anyarr_t, maxComponents: int): (uint8 [], uint8 [,])
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_data, c_mean, c_evals, c_evecs;
    c_evals.allocator = &g_fxarrAllocator;
    c_evecs.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)data, c_data);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)mean, c_mean);
    FX_OCV_TRY_CATCH(
        cv::PCACompute(c_data, c_mean, c_evecs, c_evals, (int)maxComponents);
        fx_status = cvt_from(c_evals, 1, data->t1.tag, 1, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(c_evecs, 2, data->t1.tag, 1, &fx_result->t1);
    )
    return fx_status;
}

fun PCACompute(data: 't [,], mean: 't [], ~maxComponents: int=0): ('t [], 't [,])
{
    val (evals, evecs) = PCACompute_(anyarray(data), anyarray(mean))
    (reinterpret(evals): 't [], reinterpet(evecs): 't [,])
}

@private fun PCAProject_(data: anyarr_t, mean: anyarr_t, eigenvectors: anyarr_t): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_data, c_mean, c_evecs, c_proj;
    c_proj.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)data, c_data);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)mean, c_mean);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)eigenvectors, c_evecs);
    FX_OCV_TRY_CATCH(
        cv::PCAProject(c_data, c_mean, c_evecs, c_proj);
        fx_status = cvt_from(c_proj, 2, data->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun PCAProject(data: 't [,], mean: 't [], eigenvectors: 't [,]): 't [,]
{
    val proj = PCAProject_(anyarray(data), anyarray(mean), anyarray(eigenvectors))
    (reinterpret(proj): 't [,])
}

@private fun PCABackProject_(data: anyarr_t, mean: anyarr_t, eigenvectors: anyarr_t): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_data, c_mean, c_evecs, c_bproj;
    c_bproj.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)data, c_data);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)mean, c_mean);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)eigenvectors, c_evecs);
    FX_OCV_TRY_CATCH(
        cv::PCABackProject(c_data, c_mean, c_evecs, c_bproj);
        fx_status = cvt_from(c_bproj, 2, data->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun PCABackProject(data: 't [,], mean: 't [], eigenvectors: 't [,]): 't [,]
{
    val bproj = PCABackProject_(anyarray(data), anyarray(mean), anyarray(eigenvectors))
    (reinterpret(bproj): 't [,])
}

@private fun SVDecomp_(src: anyarr_t, flags: int): (uint8 [,], uint8 [], uint8 [,])
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_w, c_u, c_vt;
    c_w.allocator = &g_fxarrAllocator;
    c_u.allocator = &g_fxarrAllocator;
    c_vt.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::SVDecomp(c_src, c_w, c_u, c_vt, (int)flags);
        fx_status = cvt_from(c_u, 2, src->t1.tag, 1, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(c_w, 1, src->t1.tag, 1, &fx_result->t1);
        if (fx_status >= 0)
            fx_status = cvt_from(c_vt, 2, src->t1.tag, 1, &fx_result->t2);
    )
    return fx_status;
}

fun SVDecomp(src: 't [,], ~flags: int=0): ('t [,], 't [], 't [,])
{
    val (u, w, vt) = SVDecomp_(anyarray(src), flags)
    ((reinterpet(u): 't [,]), (reinterpet(w): 't []), (reinterpet(vt): 't [,]))
}

@private fun SVDecompValues_(src: anyarr_t, ~flags: int=0): uint8 []
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_w;
    c_w.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::SVDecomp(c_src, c_w, noArray(), noArray(), (int)flags);
        fx_status = cvt_from(c_w, 1, src->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun SVDecompValues(src: 't [,], ~flags: int=0): 't [] =
    (reinterpet(SVDecompValues_(anyarray(src), flags)) : 't [])

@private fun SVBackSubst2D_(u: anyarr_t, w: anyarr_t, vt: anyarr_t, rhs: anyarr_t): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_w, c_u, c_vt, c_rhs, c_result;
    c_result.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)u, c_u);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)w, c_w);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)vt, c_vt);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)rhs, c_rhs);
    FX_OCV_TRY_CATCH(
        cv::SVBackSubst(c_w, c_u, c_vt, c_rhs, c_result);
        fx_status = cvt_from(c_result, 2, w->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun SVBackSubst(u: 't [,], w: 't [], vt: 't [,], rhs: 't [,]): 't [,] =
    (reinterpret(SVBackSubst2D_(anyarray(u), anyarray(w), anyarray(vt), anyarray(rhs))) : 't [,])

@private fun SVBackSubst1D_(u: anyarr_t, w: anyarr_t, vt: anyarr_t, rhs: anyarr_t): uint8 []
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_w, c_u, c_vt, c_rhs, c_result;
    c_result.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)u, c_u);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)w, c_w);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)vt, c_vt);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)rhs, c_rhs);
    FX_OCV_TRY_CATCH(
        cv::SVBackSubst(c_w, c_u, c_vt, c_rhs, c_result);
        fx_status = cvt_from(c_result, 1, w->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun SVBackSubst(u: 't [,], w: 't [], vt: 't [,], rhs: 't []): 't [] =
    (reinterpret(SVBackSubst1D_(anyarray(u), anyarray(w), anyarray(vt), anyarray(rhs))) : 't [])

val DFT_INVERSE=1
val DFT_SCALE=2
val DFT_ROWS=4

@private fun dft_(src: anyarr_t, flags: int): uint8 []
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        int flags_ = (int)flags | ((flags & cv::DFT_INVERSE) ? (cv::DFT_REAL_OUTPUT+cv::DFT_SCALE) : 0);
        cv::dft(c_src, c_dst, (int)flags);
        CV_Assert(c_dst.type() == c_src.type());
        fx_status = cvt_from(c_dst, 1, src->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun dft(src: 't [], ~flags: int=0): 't [] =
    (reinterpret(dft_(anyarray(src), flags)) : 't [])

@private fun dft_(src: anyarr_t, flags: int, nonzeroRows: int): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        int flags_ = (int)flags | ((flags & cv::DFT_INVERSE) ? (cv::DFT_REAL_OUTPUT+cv::DFT_SCALE) : 0);
        cv::dft(c_src, c_dst, (int)flags, (int)nonzeroRows);
        CV_Assert(c_dst.type() == c_src.type());
        fx_status = cvt_from(c_dst, 2, src->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun dft(src: 't [,], ~flags: int=0, ~nonzeroRows: int=0): 't [,] =
    (reinterpret(dft_(anyarray(src), flags, nonzeroRows)) : 't [])

fun idft(src: 't [], ~flags: int=0): 't [] =
    (reinterpret(dft_(anyarray(src), flags^DFT_INVERSE)) : 't [])

fun idft(src: 't [,], ~flags: int=0, ~nonzeroRows: int=0): 't [,] =
    (reinterpret(dft_(anyarray(src), flags^DFT_INVERSE, nonzeroRows)) : 't [,])

@private fun mulSpectrums1D_(a: anyarr_t, b: anyarr_t, flags: int, conjB: bool): uint8 []
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_a, c_b, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)a, c_a);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)a, c_a);
    FX_OCV_TRY_CATCH(
        cv::mulSpectrums(c_a, c_b, c_dst, (int)flags, conjB);
        fx_status = cvt_from(c_dst, 1, a->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun mulSpectrums(a: 't [], b: 't [], ~flags: int, ~conjB: bool=false): 't [] =
    (reinterpret(mulSpectrums1D_(anyarray(a), anyarray(b), flags, conjB)) : 't [])

@private fun mulSpectrums2D_(a: anyarr_t, b: anyarr_t, flags: int, conjB: bool): uint8 []
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_a, c_b, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)a, c_a);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)a, c_a);
    FX_OCV_TRY_CATCH(
        cv::mulSpectrums(c_a, c_b, c_dst, (int)flags, conjB);
        fx_status = cvt_from(c_dst, 1, a->t1.tag, 1, fx_result);
    )
    return fx_status;
}

fun mulSpectrums(a: 't [,], b: 't [,], ~flags: int, ~conjB: bool=false): 't [,] =
    (reinterpret(mulSpectrums2D_(anyarray(a), anyarray(b), flags, conjB)) : 't [,])

fun getOptimalDFTSize(vecsize: int): int
@ccode {
    int fx_status = 0;
    FX_OCV_TRY_CATCH(
        *fx_result = cv::getOptimalDFTSize((int)vecsize);
    )
    return fx_status;
}

@nothrow fun getRNGSeed(): uint64
@ccode {
    return theRNG().state;
}

@nothrow fun setRNGSeed(seed: uint64): void
@ccode {
    theRNG() = RNG(seed);
}

val RNG_UNIFORM : int = @ccode {cv::RNG::UNIFORM}
val RNG_NORMAL : int = @ccode {cv::RNG::NORMAL}

@private fun rand_(ndims: int, sz: (int*5), dc: (depth_t, int), param1: double, param2: double, dist: int): uint8 [,]
@ccode {
    int sz_[] = {(int)sz->t0, (int)sz->t1, (int)sz->t2, (int)sz->t3, (int)sz->t4};
    cv::Mat c_arr;
    c_arr.allocator = &g_fxarrAllocator;
    c_arr.create((int)ndims, sz_, CV_MAKETYPE(depth2cv(dc->t0.tag), (int)dc->t1));
    int fx_status = CV_OK;
    FX_OCV_TRY_CATCH(
        if (dist == cv::RNG::UNIFORM)
            cv::randu(c_arr, param1, param2);
        else
            cv::randn(c_arr, param1, param2);
        fx_status = cvt_from(c_arr, ndims, dc->t0.tag, dc->t1, fx_result);
    )
    return fx_status;
}

fun randu(size: int, ~low: 't, ~high: 't): 't [] =
    (reinterpret(rand_(1, (size, 0, 0, 0, 0), double(low), double(high), RNG_UNIFORM)) : 't [])

fun randu(size: (int, int), ~low: 't, ~high: 't): 't [,] =
    (reinterpret(rand_(2, (size.0, size.1, 0, 0, 0), double(low), double(high), RNG_UNIFORM)) : 't [,])

fun randu(size: (int, int, int), ~low: 't, ~high: 't): 't [,,] =
    (reinterpret(rand_(3, (size.0, size.1, size.2, 0, 0), double(low), double(high), RNG_UNIFORM)) : 't [,,])

fun randn(size: int, ~mean: 't, ~stddev: 't): 't [] =
    (reinterpret(rand_(1, (size, 0, 0, 0, 0), double(low), double(high), RNG_NORMAL)) : 't [])

fun randu(size: (int, int), ~low: 't, ~high: 't): 't [,] =
    (reinterpret(rand_(2, (size.0, size.1, 0, 0, 0), double(low), double(high), RNG_NORMAL)) : 't [,])

fun randu(size: (int, int, int), ~low: 't, ~high: 't): 't [,,] =
    (reinterpret(rand_(3, (size.0, size.1, size.2, 0, 0), double(low), double(high), RNG_NORMAL)) : 't [,,])

@private fun randShuffle_(arr: anyarr_t, iterFactor: double): void
@ccode {
    cv::Mat c_arr;
    int fx_status = cvt_to((const _fx_anyarr_t*)arr, c_arr);
    FX_OCV_TRY_CATCH(
        cv::randShuffle(c_arr, iterFactor);
    )
    return fx_status;
}

fun randShuffle(arr: 't [], ~iterFactor: double = 1.): void =
    randShuffle(anyarray(arr), iterFactor)

@private fun kmeans_(data: anyarr_t, K: int, flags: int, maxIters: int, epsilon: double,
    attempts: int, centers: anyarr_t, labels0: anyarr_t): (double, int32 [])
@ccode {
    cv::Mat c_data, c_centers, c_labels0, c_labels;
    c_labels.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)data, c_data);
    int flags = cv::KMEANS_PP_CENTERS;
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)centers, c_centers);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)labels0, c_labels0);
    FX_OCV_TRY_CATCH(
        if (!c_labels0.empty()) {
            c_labels0.copyTo(c_labels);
            flags = cv::KMEANS_USE_INITIAL_LABELS;
        }
        fx_result->t0 = cv::kmeans(c_data, (int)K, c_labels,
            cv::TermCriteria(cv::TermCriteria::COUNT + cv::TermCriteria::EPS,
                             (int)maxIters, epsilon),
            (int)attempts, flags, c_centers);
        fx_status = cvt_from(c_labels, 1, _FX_DEPTH_S32, 1, &fx_result->t1);
    )
    return fx_status;
}

fun kmeans(data: float [,], K: int, ~flags: int, ~maxIters: int, ~epsilon: double=0.,
           ~attempts: int=1, ~centers: float [,]=[], ~labels0: int32 []=[]): (double, int32 []) =
    kmeans_(anyarray(data), K, flags, maxIters, epsilon, attempts,
            anyarray(centers), anyarray(labels0))

//////////////////////////////////// imgproc ///////////////////////////////

type box_t =
{
    center: floatx2;
    size: floatx2;
    angle: float
}

val DIST_LABEL_CCOMP = 0
val DIST_LABEL_PIXEL = 1

val DIST_USER    = -1
val DIST_L1      = 1
val DIST_L2      = 2
val DIST_C       = 3
val DIST_L12     = 4
val DIST_FAIR    = 5
val DIST_WELSCH  = 6
val DIST_HUBER   = 7

@ccode {

static cv::RotatedRect cvt_box2rr(const float* box)
{
    return cv::RotatedRect(cv::Point2f(box[0], box[1]), cv::Size2f(box[2], box[3]), box[4]);
}

static void cvt_rr2box(const cv::RotatedRect& rrect, float* box)
{
    box[0] = rrect.center.x;
    box[1] = rrect.center.y;
    box[2] = rrect.size.width;
    box[3] = rrect.size.height;
    box[4] = rrect.angle;
}

}

fun getGaussianKernel(ksize: int, sigma: double): float []
@ccode {
    cv::Mat c_kernel;
    int fx_status = FX_OK;
    FX_OCV_TRY_CATCH(
        c_kernel = cv::getGaussianKernel(ksize, sigma, CV_32F);
        fx_status = cvt_from(c_kernel, 1, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

@private fun getGaborKernel_(ksize: intx2, sigma: double, theta: double, lambda: double,
                             gamma: double, psi: double): float [,]
@ccode {
    cv::Mat c_kernel;
    int fx_status = FX_OK;
    FX_OCV_TRY_CATCH(
        c_kernel = cv::getGaborKernel(cvt_size(&ksize->t0),
                                      sigma, theta, lambda, gamma, psi, CV_32F);
        fx_status = cvt_from(c_kernel, 2, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

fun getGaborKernel(ksize: intx2, ~sigma: double, ~theta: double, ~lambda: double,
                   ~gamma: double, ~psi: double): float [,] =
    getGaborKernel_(ksize, sigma, theta, lambda, gamma, psi)

val MORPH_RECT: int = @ccode {cv::MORPH_RECT}
val MORPH_CROSS: int = @ccode {cv::MORPH_CROSS}
val MORPH_ELLIPSE: int = @ccode {cv::MORPH_ELLIPSE}

@private fun getStructuringElement_(shape: int, ksize: intx2, anchor: intx2): uint8 [,]
@ccode {
    cv::Mat c_kernel;
    int fx_status = FX_OK;
    FX_OCV_TRY_CATCH(
        c_kernel = cv::getStructuringElement((int)shape,
                        cvt_size(&ksize->t0), cvt_point(&anchor->t0));
        fx_status = cvt_from(c_kernel, 2, _FX_DEPTH_U8, 1, fx_result);
    )
    return fx_status;
}

fun getStructuringElement(shape: int, ksize: intx2, ~anchor: intx2 = (-1, -1)): uint8 [,] =
    getStructuringElement_(shape, ksize, anchor)

@private fun medianBlur_(src: anyarr_t, ksize: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::medianBlur(c_src, c_dst, (int)ksize);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun medianBlur(src: 't [,], ksize: int): 't [,] =
    (reinterpret(medianBlur_(anyarray(src), ksize)) : 't [,])

@private fun GaussianBlur_(src: anyarr_t, ksize: intx2, sigma: double,
                 sigmaY: double, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::GaussianBlur(c_src, c_dst, cvt_size(&ksize->t0), sigma, sigmaY, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun GaussianBlur(src: 't [,], ksize: intx2, ~sigma: double,
                 ~sigmaY: double=0., ~borderType: int=BORDER_DEFAULT): 't [,] =
    (reinterpret(GaussianBlur_(anyarray(src), ksize, sigma, sigmaY, borderType)) : 't [,])

@private fun bilateralFilter_(src: anyarr_t, d: int, sigmaColor: double,
                    sigmaSpace: double, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::bilateralFilter(c_src, c_dst, (int)d, sigmaColor, sigmaSpace, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun bilateralFilter(src: 't [,], d: int, ~sigmaColor: double,
                    ~sigmaSpace: double, ~borderType: int=BORDER_DEFAULT): 't [,] =
    (reinterpret(bilteralFilter_(anyarray(src), d, sigmaColor, sigmaSpace, borderType)) : 't [,])

@private fun blur_(src: anyarr_t, ksize: intx2, anchor: intx2, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::blur(c_src, c_dst, cvt_size(&ksize->t0), cvt_point(&anchor->t0), (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun blur(src: 't [,], ksize: intx2, ~anchor: intx2=(-1, -1),
         ~borderType: int=BORDER_DEFAULT): 't [,] =
    (reinterpret(blur_(anyarray(src), ksize, anchor, borderType)) : 't [,])

@private fun filter2D_(src: anyarr_t, kernel: anyarr_t, anchor: intx2,
                    delta: double, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_kernel, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)kernel, c_kernel);
    FX_OCV_TRY_CATCH(
        cv::filter2D(c_src, c_dst, c_src.depth(), c_kernel, cvt_point(&anchor->t0), delta, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun filter2D(src: 't [,], kernel: 'k [,], ~anchor: intx2=(-1, -1),
             ~delta: double=0., ~borderType: int=BORDER_DEFAULT): 't [,] =
    (reinterpret(filter2D_(anyarray(src), anyarray(kernel), anchor, delta, borderType)) : 't [,])

@private fun sepFilter2D_(src: anyarr_t, kernelX: anyarr_t, kernelY: anyarr_t,
                          anchor: intx2, delta: double, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_kernelX, c_kernelY, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)kernelX, c_kernelX);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)kernelY, c_kernelY);
    FX_OCV_TRY_CATCH(
        cv::filter2D(c_src, c_dst, c_src.depth(), c_kernelX, c_kernelY,
                     cvt_point(&anchor->t0), delta, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun sepFilter2D(src: 't [,], kernelX: 'k [,], kernelY: 'k [,], ~anchor: intx2=(-1, -1),
                ~delta: double=0., ~borderType: int=BORDER_DEFAULT): 't [,] =
    (reinterpret(sepFilter2D_(anyarray(src), anyarray(kernelX),
            anyarray(kernelY), anchor, delta, borderType)) : 't [,])

@private fun Sobel_(src: anyarr_t, dx: int, dy: int, ksize: int, scale: double,
                    delta: double, borderType: int): float [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::Sobel(c_src, c_dst, CV_32F, (int)dx, (int)dy, (int)ksize, scale, delta, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, _FX_DEPTH_FP32, src->t2, fx_result);
    )
    return fx_status;
}

fun Sobel(src: 't [,], dx: int, dy: int, ~ksize: int=3, ~scale: double=1.,
          ~delta: double=0., ~borderType: int=BORDER_DEFAULT): float [,] =
    Sobel_(anyarray(src), dx, dy, ksize, scale, delta, borderType)

@private fun spatialGradient_(src: anyarr_t, ksize: int,
                    borderType: int): (int16 [,], int16 [,])
@ccode {
    cv::Mat c_src, c_dx, c_dy;
    c_dx.allocator = &g_fxarrAllocator;
    c_dy.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::spatialGradient(c_src, c_dx, c_dy, (int)ksize, (int)borderType);
        fx_status = cvt_from(c_dx, src->t0.ndims, _FX_DEPTH_S16, src->t2, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(c_dy, src->t0.ndims, _FX_DEPTH_S16, src->t2, &fx_result->t1);
    )
    return fx_status;
}

fun spatialGradient(src: uint8 [,], ~ksize: int=3,
                    ~borderType: int=BORDER_DEFAULT): (int16 [,], int16 [,]) =
    spatialGradient_(anyarray(src), ksize, borderType)

@private fun Laplacian_(src: anyarr_t, ksize: int, scale: double, delta: double,
                        borderType: int): float [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::Laplacian(c_src, c_dst, CV_32F, (int)ksize, scale, delta, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, _FX_DEPTH_FP32, src->t2, fx_result);
    )
    return fx_status;
}

fun Laplacian(src: 't [,], ~ksize: int = 1, ~scale: double = 1, ~delta: double = 0,
              ~borderType: int=BORDER_DEFAULT): float [,] =
    Laplacian_(anyarray(src), ksize, scale, delta, borderType)

@private fun Canny_(src: anyarr_t, threshold1: double, threshold2: double,
                   ksize: int, L2gradient: bool): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::Canny(c_src, c_dst, threshold1, threshold2, (int)ksize, L2gradient);
        fx_status = cvt_from(c_dst, src->t0.ndims, _FX_DEPTH_U8, src->t2, fx_result);
    )
    return fx_status;
}

fun Canny(src: uint8 [,], threshold1: double, threshold2: double,
          ~ksize: int=3, ~L2gradient: bool = false): uint8 [,] =
    Canny_(anyarray(src), threshold1, threshold2, ksize, L2gradient)

@private fun goodFeaturesToTrack_(src: anyarr_t, maxCorners: int, qualityLevel: double,
                        minDistance: double, mask: anyarr_t,
                        blockSize: int, gradientSize: int,
                        useHarrisDetector: bool,
                        k: double): floatx2 []
@ccode {
    cv::Mat c_src, c_mask, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)mask, c_mask);
    FX_OCV_TRY_CATCH(
        cv::goodFeaturesToTrack(c_src, c_dst, (int)maxCorners, qualityLevel, minDistance,
                                c_mask, (int)blockSize, (int)gradientSize, useHarrisDetector, k);
        fx_status = cvt_from(c_dst, 1, _FX_DEPTH_FP32, 2, fx_result);
    )
    return fx_status;
}

fun goodFeaturesToTrack(src: uint8 [,], maxCorners: int, ~qualityLevel: double,
                        ~minDistance: double, ~mask: uint8 [,]=[],
                        ~blockSize: int=3, ~gradientSize: int=3,
                        ~useHarrisDetector: bool=false,
                        ~k: double=0.04): floatx2 [] =
    goodFeaturesToTrack_(anyarray(src), maxCorners, qualityLevel, minDistance, anyarray(mask),
                         blockSize, gradientSize, useHarrisDetector, k)

@private fun HoughLines_(src: anyarr_t, rho: double, theta: double, threshold: int,
               srn: double, stn: double, minTheta: double, maxTheta: double): floatx2 []
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::HoughLines(c_src, c_dst, rho, theta, threshold, srn, stn, minTheta, maxTheta);
        fx_status = cvt_from(c_dst, 1, _FX_DEPTH_FP32, 2, fx_result);
    )
    return fx_status;
}

fun HoughLines(src: uint8 [,], ~rho: double, ~theta: double, ~threshold: int,
               ~srn: double=0., ~stn: double=0.,
               ~minTheta: double=0., ~maxTheta: double=M_PI): floatx2 [] =
    HoughLines_(anyarray(src), rho, theta, threshold, srn, stn, minTheta, maxTheta)

@private fun HoughLinesP_(src: anyarr_t, rho: double, theta: double, threshold: int,
                          minLineLength: int, maxLineGap: int): int32x4 []
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::HoughLinesP(c_src, c_dst, rho, theta, threshold, minLineLength, maxLineGap);
        fx_status = cvt_from(c_dst, 1, _FX_DEPTH_S32, 4, fx_result);
    )
    return fx_status;
}

fun HoughLinesP(src: uint8 [,], ~rho: double, ~theta: double, ~threshold: int,
               ~minLineLength: int=0, ~maxLineGap: int=0): int32x4 [] =
    HoughLinesP_(anyarray(src), rho, theta, threshold, minLineLength, maxLineGap)

@private fun HoughCircles_(src: anyarr_t, method: int,
                           dp: double, minDist: double,
                           param1: double, param2: double,
                           minRadius: int, maxRadius: int): floatx3 []
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::HoughCircles(c_src, c_dst, (int)method, dp, minDist,
            param1, param2, (int)minRadius, (int)maxRadius);
        fx_status = cvt_from(c_dst, 1, _FX_DEPTH_FP32, 3, fx_result);
    )
    return fx_status;
}

fun HoughCircles(src: uint8 [,], ~method: int=4, ~dp: double, ~minDist: double,
                 ~param1: double=100., ~param2: double=100.,
                 ~minRadius: int=0, ~maxRadius: int=0): floatx3 [] =
    HoughCircles_(anyarray(src), method, dp, minDist, param1, param2, minRadius, maxRadius)

@private fun erode_(src: anyarr_t, kernel: anyarr_t, anchor: intx2,
                    iterations: int, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_kernel, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)kernel, c_kernel);
    FX_OCV_TRY_CATCH(
        cv::erode(c_src, c_dst, c_kernel, cvt_point(&anchor->t0), (int)iterations, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun erode(src: 't [,], kernel: 'k [,], ~anchor: intx2=(-1, -1),
          ~iterations: int=1, ~borderType: int = BORDER_CONSTANT): 't [,] =
    (reinterpret(erode_(anyarray(src), anyarray(kernel), anchor, iterations, borderType)) : 't [,])

@private fun dilate_(src: anyarr_t, kernel: anyarr_t, anchor: intx2,
                     iterations: int, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_kernel, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)kernel, c_kernel);
    FX_OCV_TRY_CATCH(
        cv::erode(c_src, c_dst, c_kernel, cvt_point(&anchor->t0), (int)iterations, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun dilate(src: 't [,], kernel: 'k [,], ~anchor: intx2=(-1, -1),
           ~iterations: int=1, ~borderType: int = BORDER_CONSTANT): 't [,] =
    (reinterpret(dilate_(anyarray(src), anyarray(kernel), anchor, iterations, borderType)) : 't [,])

@private fun morphologyEx_(src: anyarr_t, kernel: anyarr_t, anchor: intx2, op: int,
                           iterations: int, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_kernel, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)kernel, c_kernel);
    FX_OCV_TRY_CATCH(
        cv::morphologyEx(c_src, c_dst, (int)op, c_kernel,
                         cvt_point(&anchor->t0), (int)iterations, (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun morphologyEx(src: 't [,], kernel: 'k [,], ~op: int,
                 ~anchor: intx2=(-1, -1),
                 ~iterations: int=1, ~borderType: int = BORDER_CONSTANT): 't [,] =
    (reinterpret(morphologyEx_(anyarray(src), anyarray(kernel), anchor, op,
                 iterations, borderType)) : 't [,])

val INTER_LINEAR: int = @ccode {cv::INTER_LINEAR}
val INTER_CUBIC: int = @ccode {cv::INTER_CUBIC}
val INTER_NEAREST: int = @ccode {cv::INTER_NEAREST}
val INTER_LANCZOS4: int = @ccode {cv::INTER_LANCZOS4}
val INTER_AREA: int = @ccode {cv::INTER_AREA}
val INTER_LINEAR_EXACT: int = @ccode {cv::INTER_LINEAR_EXACT}
val INTER_NEAREST_EXACT: int = @ccode {cv::INTER_LINEAR_EXACT}
val WARP_FILL_OUTLIERS: int = @ccode {cv::WARP_FILL_OUTLIERS}
val WARP_INVERSE_MAP: int = @ccode {cv::WARP_INVERSE_MAP}

@private fun resize_(src: anyarr_t, dsize: intx2, fx: double, fy: double,
           interpolation: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::resize(c_src, c_dst, cvt_size(&dsize->t0), fx, fy, (int)interpolation);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun resize(src: 't [,], dsize: intx2, ~fx: double=0, ~fy: double=0,
           ~interpolation: int = INTER_LINEAR ): 't [,] =
    (reinterpret(resize_(anyarray(src), dsize, fx, fy, interpolation)) : 't [,])

@private fun warpAffine_(src: anyarr_t, M: anyarr_t, dsize: intx2,
               interpolation: int, borderType: int,
               borderValue: doublex4): uint8 [,]
@ccode {
    cv::Mat c_src, c_M, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)M, c_M);
    FX_OCV_TRY_CATCH(
        cv::warpAffine(c_src, c_dst, c_M, cvt_size(&dsize->t0),
            (int)interpolation, (int)borderType, cvt_scalar(&borderValue->t0));
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun warpAffine(src: 't [,], M: 'k [,], dsize: intx2,
               ~interpolation: int = INTER_LINEAR,
               ~borderType: int = BORDER_CONSTANT,
               ~borderValue: doublex4 = (0., 0., 0., 0.)): 't [,] =
    (reinterpret(warpAffine_(anyarray(src), anyarray(M), dsize,
        interpolation, borderType, borderValue)) : 't [,])

@private fun warpPerspective_(src: anyarr_t, M: anyarr_t, dsize: intx2,
               interpolation: int, borderType: int,
               borderValue: doublex4): uint8 [,]
@ccode {
    cv::Mat c_src, c_M, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)M, c_M);
    FX_OCV_TRY_CATCH(
        cv::warpPerspective(c_src, c_dst, c_M, cvt_size(&dsize->t0),
            (int)interpolation, (int)borderType, cvt_scalar(&borderValue->t0));
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun warpPerspective(src: 't [,], M: 'k [,], dsize: intx2,
               ~interpolation: int = INTER_LINEAR,
               ~borderType: int = BORDER_CONSTANT,
               ~borderValue: doublex4 = (0., 0., 0., 0.)): 't [,] =
    (reinterpret(warpPerspective_(anyarray(src), anyarray(M), dsize,
        interpolation, borderType, borderValue)) : 't [,])

@private fun remap_(src: anyarr_t, map1: anyarr_t, map2: anyarr_t,
                   iterpolation: int, borderType: int,
                   borderValue: doublex4): uint8 [,]
@ccode {
    cv::Mat c_src, c_map1, c_map2, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)map1, c_map1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)map2, c_map2);
    FX_OCV_TRY_CATCH(
        cv::remap(c_src, c_dst, c_map1, c_map2,
            (int)interpolation, (int)borderType, cvt_scalar(&borderValue->t0));
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun remap(src: 't [,], map1: 'k [,], map2: 'k [,],
          ~iterpolation: int = INTER_LINEAR,
          ~borderType: int = BORDER_CONSTANT,
          ~borderValue: doublex4 = (0., 0., 0., 0.)): 't [,] =
    (reinterpret(remap_(anyarray(src), anyarray(map1), anyarray(map2),
            interpolation, borderType, borderValue)) : 't [,])

fun getRotationMatrix2D(center: floatx2, angle: double, scale: double): double [,]
@ccode {
    cv::Mat c_mtx;
    int fx_status = FX_OK;
    FX_OCV_TRY_CATCH(
        c_mtx = cv::getRotationMatrix2D(cvt_point(&center->t0), angle, scale);
        fx_status = cvt_from(c_mtx, 2, _FX_DEPTH_FP64, 1, fx_result);
    )
    return fx_status;
}

fun getAffineTransform(src: floatx2 [], dst: floatx2 []): double [,]
@ccode {
    cv::Mat c_mtx;
    FX_OCV_TRY_CATCH(
        CV_Assert(src->dim[0].size == 3 && src->dim[0].size == dst->dim[0].size);
        c_mtx = cv::getAffineTransform((cv::Point2f*)src->data, (cv::Point2f*)dst->data);
        fx_status = cvt_from(c_mtx, 2, _FX_DEPTH_FP64, 1, fx_result);
    )
    return fx_status;
}

fun invertAffineTransform(M: double [,]): double [,]
@ccode {
    _fx_anyarr_t src = {*M, _FX_DEPTH_FP64, 1};
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to(&src, c_src);

    FX_OCV_TRY_CATCH(
        cv::invertAffineTransform(c_src, c_dst);
        fx_status = cvt_from(c_dst, 2, _FX_DEPTH_FP64, 1, fx_result);
    )
    return fx_status;
}

@private fun getPerspectiveTransform_(src: anyarr_t, dst: anyarr_t, method: int): double [,]
@ccode {
    cv::Mat c_src, c_dst, c_mtx;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)dst, c_dst);

    FX_OCV_TRY_CATCH(
        c_mtx = cv::getPerspectiveTransform(c_src, c_dst, (int)method);
        fx_status = cvt_from(c_mtx, 2, _FX_DEPTH_FP64, 1, fx_result);
    )
    return fx_status;
}

fun getPerspectiveTransform(src: floatx2 [], dst: floatx2 [],
                            ~solveMethod: int=DECOMP_LU): double [,] =
    getPerspectiveTransform_(anyarray(src), anyarray(dst), solveMethod)

@private fun getRectSubPix_(src: anyarr_t, patchSize: intx2, center: floatx2): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::getRectSubPix(c_src, cvt_size(&patchSize->t0), cvt_point(&center->t0), c_src.type());
        fx_status = cvt_from(c_dst, 2, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}
fun getRectSubPix(src: 't [,], patchSize: intx2, center: floatx2): 't [,] =
    (reinterpret(getRectSubPix_(anyarray(src), patchSize, center)) : 't [,])

@private fun logPolar_(src: anyarr_t, center: floatx2, M: double, flags: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::logPolar(c_src, c_dst, cvt_point(&center->t0), M, flags);
        fx_status = cvt_from(c_dst, 2, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun logPolar(src: 't [,], ~center: floatx2, ~M: double, ~flags: int): 't [,] =
    (reinterpret(logPolar_(anyarray(src), center, M, flags)) : 't [,])

@private fun linearPolar_(src: anyarr_t, center: floatx2, maxRadius: double, flags: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::logPolar(c_src, c_dst, cvt_point(&center->t0), maxRadius, flags);
        fx_status = cvt_from(c_dst, 2, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun linearPolar(src: 't [,], ~center: floatx2, ~maxRadius: double, ~flags: int): 't [,] =
    (reinterpret(linearPolar_(anyarray(src), center, maxRadius, flags)) : 't [,])

@private fun integral_(src: anyarr_t, sd: depth_t): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    int sdepth = depth2cv(sd->tag);

    FX_OCV_TRY_CATCH(
        cv::integral(c_src, c_dst, sdepth);
        fx_status = cvt_from(c_dst, src->t0.ndims, sd->tag, src->t2, fx_result);
    )
    return fx_status;
}

fun integral(src: 't [,], s0: 's): 's [,] =
    (reinterpret(integral_(anyarray(src), elemtype(s0).0)) : 's [,])

@private fun integral2_(src: anyarr_t, sd: depth_t, sqd: depth_t): (uint8 [,], uint8 [,])
@ccode {
    cv::Mat c_src, c_dst, c_sqdst;
    c_dst.allocator = &g_fxarrAllocator;
    c_sqdst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    int sdepth = depth2cv(sd->tag);
    int sqdepth = depth2cv(sqd->tag);

    FX_OCV_TRY_CATCH(
        cv::integral2(c_src, c_dst, c_sqdst, sdepth, sqdepth);
        fx_status = cvt_from(c_dst, src->t0.ndims, sd->tag, src->t2, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(c_sqdst, src->t0.ndims, sqd->tag, src->t2, &fx_result->t1);
    )
    return fx_status;
}

fun integral2(src: 't [,], s0: 's, sq0: 'sq): ('s [,], 'sq [,])
{
    val (s, sq) = integral2_(anyarray(src), elemtype(s0).0, elemtype(sq0).0)
    (reinterpret(s) : 's [,], reinterpret(sq): 'sq [,])
}

@private fun phaseCorrelate_(src1: anyarr_t, src2: anyarr_t, window: anyarr_t): (doublex2, double)
@ccode {
    cv::Mat c_src1, c_src2, c_window;
    int fx_status = cvt_to((const _fx_anyarr_t*)src1, c_src1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)src2, c_src2);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)window, c_window);

    FX_OCV_TRY_CATCH(
        cv::Point2d pt = cv::phaseCorrelate(c_src1, c_src2, c_window, &fx_result->t1);
        fx_result->t0.t0 = pt.x;
        fx_result->t0.t1 = pt.y;
    )
    return fx_status;
}

fun phaseCorrelate(src1: 't [,], src2: 't [,], window: 'k [,]): (doublex2, double) =
    phaseCorrelate_(anyarray(src1), anyarray(src2), anyarray(window))

fun createHanningWindow(winSize: intx2): float [,]
@ccode {
    cv::Mat c_window;
    c_window.allocator = &g_fxarrAllocator;
    int fx_status = FX_OK;

    FX_OCV_TRY_CATCH(
        cv::createHanningWindow(c_dst, cvt_size(&winSize->t0), CV_32F);
        fx_status = cvt_from(c_dst, 2, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

@private fun divSpectrums_(src1: anyarr_t, src2: anyarr_t, flags: int, conj: bool): uint8 [,]
@ccode {
    cv::Mat c_src1, c_src2, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src1, c_src1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)src2, c_src2);

    FX_OCV_TRY_CATCH(
        cv::divSpectrums(c_src1, c_src2, c_dst, (int)flags, conj);
        fx_status = cvt_from(c_dst, src1->t0.ndims, src1->t1.tag, src1->t2, fx_result);
    )
    return fx_status;
}

fun divSpectrums(src1: 't [,], src2: 't [,], flags: int, ~conj: bool=false): 't [,] =
    (reinterpret(divSpectrums_(anyarray(src1), anyarray(src2), flags, conj)) : 't [,])

val THRESH_BINARY: int = @ccode {cv::THRESH_BINARY}
val THRESH_BINARY_INV: int = @ccode {cv::THRESH_BINARY}
val THRESH_TRUNC: int = @ccode {cv::THRESH_BINARY}
val THRESH_TOZERO: int = @ccode {cv::THRESH_BINARY}
val THRESH_TOZERO_INV: int = @ccode {cv::THRESH_BINARY}
val THRESH_OTSU: int = @ccode {cv::THRESH_BINARY}
val THRESH_TRIANGLE: int = @ccode {cv::THRESH_BINARY}

@private fun threshold_(src: anyarr_t, thresh: double, maxVal: double, op: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::threshold(c_src, c_dst, thresh, maxVal, (int)type);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun threshold(src: 't [,], ~threshold: double, ~maxVal: double, ~op: int): 't [,] =
    (reinterpret(threshold_(anyarray(src), threshold, maxVal, op)) : 't [,])

@private fun adaptiveThreshold_(src: anyarr_t, maxVal: double,
                      adaptiveMethod: int, thresholdType: int,
                      blockSize: int, C: double): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::adaptiveThreshold(c_src, c_dst, maxVal, (int)adaptiveMethod,
            (int)thresholdType, (int)blockSize, C);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun adaptiveThreshold(src: 't [,], maxVal: double,
                      ~adaptiveMethod: int, ~thresholdType: int,
                      ~blockSize: int, ~C: double): 't [,] =
    (reinterpret(adaptiveThreshold_(anyarray(src), maxVal, thresholdType, blockSize, C)) : 't [,])

@private fun pyrDown_(src: anyarr_t, dsize: intx2, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::pyrDown(c_src, c_dst, cvt_size(&dsize->t0), (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun pyrDown(src: 't [,], ~dsize: intx2=(0, 0), ~borderType: int = BORDER_DEFAULT ): 't [,] =
    (reinterpret(pyrDown_(anyarray(src), dsize, borderType)) : 't [,])

@private fun pyrUp_(src: anyarr_t, dsize: intx2, borderType: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::pyrUp(c_src, c_dst, cvt_size(&dsize->t0), (int)borderType);
        fx_status = cvt_from(c_dst, src->t0.ndims, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun pyrUp(src: 't [,], ~dsize: intx2=(0, 0), ~borderType: int = BORDER_DEFAULT ): 't [,] =
    (reinterpret(pyrUp_(anyarray(src), dsize, borderType)) : 't [,])

@private fun calcHist_(ndims: int, src: anyarr_t, mask: anyarr_t,
                       channels: intx5, hsize: intx5, ranges: float [][],
                       uniform: bool): float []
@ccode {
    int c_channels[] = {(int)channels->t0, (int)channels->t1,
        (int)channels->t2, (int)channels->t3, (int)channels->t4};
    int c_hsize[] = {(int)hsize->t0, (int)hsize->t1,
        (int)hsize->t2, (int)hsize->t3, (int)hsize->t4};
    float* c_ranges[FX_MAX_DIMS] = {0, 0, 0, 0, 0};
    cv::Mat c_src, c_mask, c_hist;
    c_hist.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)mask, c_mask);
    FX_OCV_TRY_CATCH(
        if (ranges->data) {
            CV_Assert(ndims == ranges->dim[0].size);
            for(int_ i = 0; i < ndims; i++)
                c_ranges[i] = FX_PTR_1D(float, *FX_PTR_1D(fx_arr_t, *ranges, i), 0);
        }
        cv::calcHist(&c_src, 1, c_channels, c_mask, c_hist, (int)ndims, c_hsize,
                    ranges->data ? (const float**)c_ranges : 0, uniform, false);
        fx_status = cvt_from(c_hist, (int)ndims, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

fun calcHist(src: 't [+], hsize: int, ~mask: 'm [+],
            ~channel: int=0, ~ranges: float []=[],
            ~uniform: bool=true): float [] =
    (reinterpret(calcHist_(1, anyarray(src), anyarray(mask),
        (channel, 0, 0, 0, 0), (hsize, 0, 0, 0, 0),
        [|ranges|], uniform)) : float [])

fun calcHist(src: ('t*2) [+], hsize: intx2, ~mask: 'm [+],
            ~channels: intx2 = (0, 1), ~ranges: float [][]=[],
            ~uniform: bool=true): float [,] =
    (reinterpret(calcHist_(2, anyarray(src), anyarray(mask),
        (channels.0, channels.1, 0, 0, 0), (hsize.0, hsize.1, 0, 0, 0),
        ranges, uniform)) : float [,])

fun calcHist(src: ('t*3) [+], hsize: intx3, ~mask: 't [+],
            ~channels: intx3 = (0, 1, 2), ~ranges: float [][]=[],
            ~uniform: bool=true): float [,,] =
    (reinterpret(calcHist_(3, anyarray(src), anyarray(mask),
        (channels.0, channels.1, channels.2, 0, 0),
        (hsize.0, hsize.1, hsize.2, 0, 0),
        ranges, uniform)) : float [,,])

@private fun calcBackProject_(src: anyarr_t, hist: anyarr_t,
                              channels: intx5, ranges: float [][],
                              scale: double, uniform: bool): float []
@ccode {
    int ndims = hist->t0.ndims;
    int c_channels[] = {(int)channels->t0, (int)channels->t1,
        (int)channels->t2, (int)channels->t3, (int)channels->t4};
    float* c_ranges[FX_MAX_DIMS] = {0, 0, 0, 0, 0};
    cv::Mat c_src, c_hist, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)hist, c_hist);
    FX_OCV_TRY_CATCH(
        if (ranges->data) {
            CV_Assert(ndims == (int)ranges->dim[0].size);
            for(int i = 0; i < ndims; i++)
                c_ranges[i] = FX_PTR_1D(float, *FX_PTR_1D(fx_arr_t, *ranges, i), 0);
        }
        cv::calcBackProject(&c_src, 1, c_channels, c_hist, c_dst,
                ranges->data ? (const float**)c_ranges : 0, uniform);
        fx_status = cvt_from(c_dst, ndims, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

fun calcBackProject(src: 't [,], hist: float [],
                    ~channel: int=0, ~ranges: float []=[],
                    ~scale: double=1, ~uniform: bool=true): float [,] =
    (reinterpret(calcBackProject_(anyarray(src), anyarray(hist),
        (channel, 0, 0, 0, 0), (hsize, 0, 0, 0, 0),
        [|ranges|], uniform)) : float [])

fun calcBackProject(src: 't [,], hist: float [,],
                    ~channels: intx2=(0, 1), ~ranges: float [][]=[],
                    ~scale: double=1, ~uniform: bool=true): float [,] =
    (reinterpret(calcBackProject_(anyarray(src), anyarray(hist),
        (channels.0, channels.1, 0, 0, 0), ranges, uniform)) : float [,])

fun calcBackProject(src: 't [,], hist: float [,,],
                    ~channels: intx3=(0, 1, 2), ~ranges: float [][]=[],
                    ~scale: double=1, ~uniform: bool=true): float [,] =
    (reinterpret(calcBackProject_(anyarray(src), anyarray(hist),
        (channels.0, channels.1, channels.2, 0, 0), ranges, uniform)) : float [,,])

@private fun compareHist_(h1: anyarr_t, h2: anyarr_t, method: int): double
@ccode {
    cv::Mat c_h1, c_h2;
    int fx_status = cvt_to((const _fx_anyarr_t*)h1, c_h1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)h2, c_h2);
    FX_OCV_TRY_CATCH(
        *fx_result = cv::compareHist(c_h1, c_h2, (int)method);
    )
    return fx_status;
}

fun compareHist(h1: float [+], h2: float [+], method: int): double =
    compareHist_(anyarray(h1), anyarray(h2), method)

fun equalizeHist(src: uint8 [,]): uint8 [,]
@ccode {
    _fx_anyarr_t src_ = {*src, _FX_DEPTH_U8, 1};
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to(&src_, c_src);
    FX_OCV_TRY_CATCH(
        cv::equalizeHist(c_src, c_dst);
        fx_status = cvt_from(c_dst, 2, _FX_DEPTH_8U, src_.t2, fx_result);
    )
    return fx_status;
}

fun watershed(src: uint8x3 [,], markers: int32 [,]): void
@ccode {
    _fx_anyarr_t src_ = {*src, _FX_DEPTH_U8, 3},
                 markers_ = {*markers, _FX_DEPTH_S32, 1};
    cv::Mat c_src, c_markers;
    int fx_status = cvt_to(&src_, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to(&markers_, c_markers);
    FX_OCV_TRY_CATCH(
        cv::watershed(c_src, c_markers);
    )
    return fx_status;
}

val GC_INIT_WITH_RECT  = 0
val GC_INIT_WITH_MASK  = 1
val GC_EVAL            = 2
val GC_EVAL_FREEZE_MODEL = 3

@private fun grabCut_(src: anyarr_t, mask: anyarr_t, rect: intx4,
                      bgdModel: anyarr_t, fgdModel: anyarr_t,
                      iterations: int, mode: int): (double [], double [])
@ccode {
    cv::Mat c_src, c_mask, c_bgdModel, c_fgdModel;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)mask, c_mask);
    if (fx_status >= 0) {
        fx_status = cvt_to((const _fx_anyarr_t*)bgdModel, c_bgdModel);
        if(c_bgdModel.empty())
            c_bgdModel.allocator = &g_fxarrAllocator;
    }
    if (fx_status >= 0) {
        fx_status = cvt_to((const _fx_anyarr_t*)fgdModel, c_fgdModel);
        if(c_fgdModel.empty())
            c_fgdModel.allocator = &g_fxarrAllocator;
    }
    FX_OCV_TRY_CATCH(
        cv::grabCut(c_src, c_mask, cvt_rect(rect), c_bgdModel, c_fgdModel,
                    (int)iterations, (int)mode);
        fx_status = cvt_from(c_bgdModel, 1, _FX_DEPTH_FP64, 1, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(c_fgdModel, 1, _FX_DEPTH_FP64, 1, &fx_result->t1);
    )
    return fx_status;
}

fun grabCut(src: uint8x3 [,], mask: uint8 [,], rect: intx4,
            ~bgdModel: double []=[], ~fgdModel: double []=[],
            ~iterations: int=1, ~mode: int=GC_EVAL): (double [], double []) =
    grabCut_(anyarray(src), anyarray(mask), rect,
             anyarray(bgdModel), anyarray(fgdModel),
             iterations, mode)

@private fun distanceTransform_(src: anyarr_t, distanceType: int,
    maskSize: int, labelType: int, needLabels: bool): (float [,], int32 [,])
@ccode {
    cv::Mat c_src, c_dist, c_labels;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    c_dist.allocator = &g_fxarrAllocator;
    c_labels.allocator = &g_fxarrAllocator;
    FX_OCV_TRY_CATCH(
        if (needLabels)
            cv::distanceTransform(c_src, c_dist, c_labels, (int)distanceType, (int)maskSize, (int)labelType);
        else
            cv::distanceTransform(c_src, c_dist, (int)distanceType, (int)maskSize);
        fx_status = cvt_from(c_dist, 2, _FX_DEPTH_FP32, 1, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(c_labels, 2, _FX_DEPTH_S32, 1, &fx_result->t1);
    )
    return fx_status;
}

fun distanceTransformWithLabels(src: uint8 [,], ~distanceType: int,
        ~maskSize: int=0, ~labelType: int=DIST_LABEL_CCOMP): (float [,], int32 [,]) =
    distanceTransform_(anyarray(src), distanceType, maskSize, labelType, true)

fun distanceTransform(src: uint8 [,], ~distanceType: int, ~maskSize: int=0): float [,] =
    distanceTransform_(anyarray(src), distanceType, maskSize, 0, false).0

@private fun floodFill_(img: anyarr_t, seed: intx2, newVal: doublex4,
              mask: anyarr_t, loDiff: doublex4, upDiff: doublex4, flags: int): (intx4, int)
@ccode {
    cv::Mat c_src, c_mask;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)mask, c_mask);
    FX_OCV_TRY_CATCH(
        bool haveMask = mask->t0.data != 0;
        cv::Rect r;
        if (!haveMask)
            fx_result->t1 = cv::floodFill(c_src, cvt_point(&seed->t0), cvt_scalar(&newVal->t0),
                          &r, cvt_scalar(&loDiff->t0), cvt_scalar(&upDiff->t0), (int)flags);
        else
            fx_result->t1 = cv::floodFill(c_src, c_mask, cvt_point(&seed->t0), cvt_scalar(&newVal->t0),
                          &r, cvt_scalar(&loDiff->t0), cvt_scalar(&upDiff->t0), (int)flags);
        cvt_rect2intx4(r, &fx_result->t0.t0);
    )
    return fx_status;
}

fun floodFill(img: 't [,], seed: intx2, newVal: doublex4,
              ~mask: uint8 [,]=[],
              ~loDiff: doublex4=(0., 0., 0., 0.),
              ~upDiff: doublex4=(0., 0., 0., 0.),
              ~flags: int=4): (intx4, int) =
    floodFill(anyarray(img), seed, newVal, anyarray(mask), lodiff, upDiff, flags)

@private fun blendLinear_(src1: anyarr_t, src2: anyarr_t, w1: anyarr_t, w2: anyarr_t): uint8 [,]
@ccode {
    cv::Mat c_src1, c_src2, c_w1, c_w2, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src1, c_src1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)src2, c_src2);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)w1, c_w1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)w2, c_w2);

    FX_OCV_TRY_CATCH(
        cv::blendLinear(c_src1, c_src2, c_w1, c_w2, c_dst);
        fx_status = cvt_from(c_dst, src1->t0.ndims, src1->t1.tag, src1->t2, fx_result);
    )
    return fx_status;
}

fun blendLinear(src1: 't [,], src2: 't [,], w1: 'w [,], w2: 'w [,]): 't [,] =
    (reinterpret(blendLinear_(anyarray(src1), anyarray(src2), anyarray(w1), anyarray(w2))) : 't [,])

val COLOR_BGR2BGRA: int = @ccode {cv::COLOR_BGR2BGRA}
val COLOR_RGB2RGBA: int = @ccode {cv::COLOR_RGB2RGBA}

val COLOR_BGRA2BGR: int = @ccode {cv::COLOR_BGRA2BGR}
val COLOR_RGBA2RGB: int = @ccode {cv::COLOR_RGBA2RGB}

val COLOR_BGR2RGBA: int = @ccode {cv::COLOR_BGR2RGBA}
val COLOR_RGB2BGRA: int = @ccode {cv::COLOR_RGB2BGRA}

val COLOR_RGBA2BGR: int = @ccode {cv::COLOR_RGBA2BGR}
val COLOR_BGRA2RGB: int = @ccode {cv::COLOR_BGRA2RGB}

val COLOR_BGR2RGB: int = @ccode {cv::COLOR_BGR2RGB}
val COLOR_RGB2BGR: int = @ccode {cv::COLOR_RGB2BGR}

val COLOR_BGRA2RGBA: int = @ccode {cv::COLOR_BGRA2RGBA}
val COLOR_RGBA2BGRA: int = @ccode {cv::COLOR_RGBA2BGRA}

val COLOR_BGR2GRAY: int = @ccode {cv::COLOR_BGR2GRAY}
val COLOR_RGB2GRAY: int = @ccode {cv::COLOR_RGB2GRAY}
val COLOR_GRAY2BGR: int = @ccode {cv::COLOR_GRAY2BGR}
val COLOR_GRAY2RGB: int = @ccode {cv::COLOR_GRAY2RGB}
val COLOR_GRAY2BGRA: int = @ccode {cv::COLOR_GRAY2BGRA}
val COLOR_GRAY2RGBA: int = @ccode {cv::COLOR_GRAY2RGBA}
val COLOR_BGRA2GRAY: int = @ccode {cv::COLOR_BGRA2GRAY}
val COLOR_RGBA2GRAY: int = @ccode {cv::COLOR_RGBA2GRAY}

val COLOR_BGR2BGR565: int = @ccode {cv::COLOR_BGR2BGR565}
val COLOR_RGB2BGR565: int = @ccode {cv::COLOR_RGB2BGR565}
val COLOR_BGR5652BGR: int = @ccode {cv::COLOR_BGR5652BGR}
val COLOR_BGR5652RGB: int = @ccode {cv::COLOR_BGR5652RGB}
val COLOR_BGRA2BGR565: int = @ccode {cv::COLOR_BGRA2BGR565}
val COLOR_RGBA2BGR565: int = @ccode {cv::COLOR_RGBA2BGR565}
val COLOR_BGR5652BGRA: int = @ccode {cv::COLOR_BGR5652BGRA}
val COLOR_BGR5652RGBA: int = @ccode {cv::COLOR_BGR5652RGBA}

val COLOR_GRAY2BGR565: int = @ccode {cv::COLOR_GRAY2BGR565}
val COLOR_BGR5652GRAY: int = @ccode {cv::COLOR_BGR5652GRAY}

val COLOR_BGR2BGR555: int = @ccode {cv::COLOR_BGR2BGR555}
val COLOR_RGB2BGR555: int = @ccode {cv::COLOR_RGB2BGR555}
val COLOR_BGR5552BGR: int = @ccode {cv::COLOR_BGR5552BGR}
val COLOR_BGR5552RGB: int = @ccode {cv::COLOR_BGR5552RGB}
val COLOR_BGRA2BGR555: int = @ccode {cv::COLOR_BGRA2BGR555}
val COLOR_RGBA2BGR555: int = @ccode {cv::COLOR_RGBA2BGR555}
val COLOR_BGR5552BGRA: int = @ccode {cv::COLOR_BGR5552BGRA}
val COLOR_BGR5552RGBA: int = @ccode {cv::COLOR_BGR5552RGBA}

val COLOR_GRAY2BGR555: int = @ccode {cv::COLOR_GRAY2BGR555}
val COLOR_BGR5552GRAY: int = @ccode {cv::COLOR_BGR5552GRAY}

val COLOR_BGR2XYZ: int = @ccode {cv::COLOR_BGR2XYZ}
val COLOR_RGB2XYZ: int = @ccode {cv::COLOR_RGB2XYZ}
val COLOR_XYZ2BGR: int = @ccode {cv::COLOR_XYZ2BGR}
val COLOR_XYZ2RGB: int = @ccode {cv::COLOR_XYZ2RGB}

val COLOR_BGR2YCrCb: int = @ccode {cv::COLOR_BGR2YCrCb}
val COLOR_RGB2YCrCb: int = @ccode {cv::COLOR_RGB2YCrCb}
val COLOR_YCrCb2BGR: int = @ccode {cv::COLOR_YCrCb2BGR}
val COLOR_YCrCb2RGB: int = @ccode {cv::COLOR_YCrCb2RGB}

val COLOR_BGR2HSV: int = @ccode {cv::COLOR_BGR2HSV}
val COLOR_RGB2HSV: int = @ccode {cv::COLOR_RGB2HSV}

val COLOR_BGR2Lab: int = @ccode {cv::COLOR_BGR2Lab}
val COLOR_RGB2Lab: int = @ccode {cv::COLOR_RGB2Lab}

val COLOR_BGR2Luv: int = @ccode {cv::COLOR_BGR2Luv}
val COLOR_RGB2Luv: int = @ccode {cv::COLOR_RGB2Luv}
val COLOR_BGR2HLS: int = @ccode {cv::COLOR_BGR2HLS}
val COLOR_RGB2HLS: int = @ccode {cv::COLOR_RGB2HLS}

val COLOR_HSV2BGR: int = @ccode {cv::COLOR_HSV2BGR}
val COLOR_HSV2RGB: int = @ccode {cv::COLOR_HSV2RGB}

val COLOR_Lab2BGR: int = @ccode {cv::COLOR_Lab2BGR}
val COLOR_Lab2RGB: int = @ccode {cv::COLOR_Lab2RGB}
val COLOR_Luv2BGR: int = @ccode {cv::COLOR_Luv2BGR}
val COLOR_Luv2RGB: int = @ccode {cv::COLOR_Luv2RGB}
val COLOR_HLS2BGR: int = @ccode {cv::COLOR_HLS2BGR}
val COLOR_HLS2RGB: int = @ccode {cv::COLOR_HLS2RGB}

val COLOR_BGR2HSV_FULL: int = @ccode {cv::COLOR_BGR2HSV_FULL}
val COLOR_RGB2HSV_FULL: int = @ccode {cv::COLOR_RGB2HSV_FULL}
val COLOR_BGR2HLS_FULL: int = @ccode {cv::COLOR_BGR2HLS_FULL}
val COLOR_RGB2HLS_FULL: int = @ccode {cv::COLOR_RGB2HLS_FULL}

val COLOR_HSV2BGR_FULL: int = @ccode {cv::COLOR_HSV2BGR_FULL}
val COLOR_HSV2RGB_FULL: int = @ccode {cv::COLOR_HSV2RGB_FULL}
val COLOR_HLS2BGR_FULL: int = @ccode {cv::COLOR_HLS2BGR_FULL}
val COLOR_HLS2RGB_FULL: int = @ccode {cv::COLOR_HLS2RGB_FULL}

val COLOR_LBGR2Lab: int = @ccode {cv::COLOR_LBGR2Lab}
val COLOR_LRGB2Lab: int = @ccode {cv::COLOR_LRGB2Lab}
val COLOR_LBGR2Luv: int = @ccode {cv::COLOR_LBGR2Luv}
val COLOR_LRGB2Luv: int = @ccode {cv::COLOR_LRGB2Luv}

val COLOR_Lab2LBGR: int = @ccode {cv::COLOR_Lab2LBGR}
val COLOR_Lab2LRGB: int = @ccode {cv::COLOR_Lab2LRGB}
val COLOR_Luv2LBGR: int = @ccode {cv::COLOR_Luv2LBGR}
val COLOR_Luv2LRGB: int = @ccode {cv::COLOR_Luv2LRGB}

val COLOR_BGR2YUV: int = @ccode {cv::COLOR_BGR2YUV}
val COLOR_RGB2YUV: int = @ccode {cv::COLOR_RGB2YUV}
val COLOR_YUV2BGR: int = @ccode {cv::COLOR_YUV2BGR}
val COLOR_YUV2RGB: int = @ccode {cv::COLOR_YUV2RGB}

//! YUV 4:2:0 family to RGB
val COLOR_YUV2RGB_NV12: int = @ccode {cv::COLOR_YUV2RGB_NV12}
val COLOR_YUV2BGR_NV12: int = @ccode {cv::COLOR_YUV2BGR_NV12}
val COLOR_YUV2RGB_NV21: int = @ccode {cv::COLOR_YUV2RGB_NV21}
val COLOR_YUV2BGR_NV21: int = @ccode {cv::COLOR_YUV2BGR_NV21}
val COLOR_YUV420sp2RGB: int = @ccode {cv::COLOR_YUV420sp2RGB}
val COLOR_YUV420sp2BGR: int = @ccode {cv::COLOR_YUV420sp2BGR}

val COLOR_YUV2RGBA_NV12: int = @ccode {cv::COLOR_YUV2RGBA_NV12}
val COLOR_YUV2BGRA_NV12: int = @ccode {cv::COLOR_YUV2BGRA_NV12}
val COLOR_YUV2RGBA_NV21: int = @ccode {cv::COLOR_YUV2RGBA_NV21}
val COLOR_YUV2BGRA_NV21: int = @ccode {cv::COLOR_YUV2BGRA_NV21}
val COLOR_YUV420sp2RGBA: int = @ccode {cv::COLOR_YUV420sp2RGBA}
val COLOR_YUV420sp2BGRA: int = @ccode {cv::COLOR_YUV420sp2BGRA}

val COLOR_YUV2RGB_YV12: int = @ccode {cv::COLOR_YUV2RGB_YV12}
val COLOR_YUV2BGR_YV12: int = @ccode {cv::COLOR_YUV2BGR_YV12}
val COLOR_YUV2RGB_IYUV: int = @ccode {cv::COLOR_YUV2RGB_IYUV}
val COLOR_YUV2BGR_IYUV: int = @ccode {cv::COLOR_YUV2BGR_IYUV}
val COLOR_YUV2RGB_I420: int = @ccode {cv::COLOR_YUV2RGB_I420}
val COLOR_YUV2BGR_I420: int = @ccode {cv::COLOR_YUV2BGR_I420}
val COLOR_YUV420p2RGB: int = @ccode {cv::COLOR_YUV420p2RGB}
val COLOR_YUV420p2BGR: int = @ccode {cv::COLOR_YUV420p2BGR}

val COLOR_YUV2RGBA_YV12: int = @ccode {cv::COLOR_YUV2RGBA_YV12}
val COLOR_YUV2BGRA_YV12: int = @ccode {cv::COLOR_YUV2BGRA_YV12}
val COLOR_YUV2RGBA_IYUV: int = @ccode {cv::COLOR_YUV2RGBA_IYUV}
val COLOR_YUV2BGRA_IYUV: int = @ccode {cv::COLOR_YUV2BGRA_IYUV}
val COLOR_YUV2RGBA_I420: int = @ccode {cv::COLOR_YUV2RGBA_I420}
val COLOR_YUV2BGRA_I420: int = @ccode {cv::COLOR_YUV2BGRA_I420}
val COLOR_YUV420p2RGBA: int = @ccode {cv::COLOR_YUV420p2RGBA}
val COLOR_YUV420p2BGRA: int = @ccode {cv::COLOR_YUV420p2BGRA}

val COLOR_YUV2GRAY_420: int = @ccode {cv::COLOR_YUV2GRAY_420}
val COLOR_YUV2GRAY_NV21: int = @ccode {cv::COLOR_YUV2GRAY_NV21}
val COLOR_YUV2GRAY_NV12: int = @ccode {cv::COLOR_YUV2GRAY_NV12}
val COLOR_YUV2GRAY_YV12: int = @ccode {cv::COLOR_YUV2GRAY_YV12}
val COLOR_YUV2GRAY_IYUV: int = @ccode {cv::COLOR_YUV2GRAY_IYUV}
val COLOR_YUV2GRAY_I420: int = @ccode {cv::COLOR_YUV2GRAY_I420}
val COLOR_YUV420sp2GRAY: int = @ccode {cv::COLOR_YUV420sp2GRAY}
val COLOR_YUV420p2GRAY: int = @ccode {cv::COLOR_YUV420p2GRAY}

//! YUV 4:2:2 family to RGB
val COLOR_YUV2RGB_UYVY: int = @ccode {cv::COLOR_YUV2RGB_UYVY}
val COLOR_YUV2BGR_UYVY: int = @ccode {cv::COLOR_YUV2BGR_UYVY}
//COLOR_YUV2RGB_VYUY = 109,
//COLOR_YUV2BGR_VYUY = 110,
val COLOR_YUV2RGB_Y422: int = @ccode {cv::COLOR_YUV2RGB_Y422}
val COLOR_YUV2BGR_Y422: int = @ccode {cv::COLOR_YUV2BGR_Y422}
val COLOR_YUV2RGB_UYNV: int = @ccode {cv::COLOR_YUV2RGB_UYNV}
val COLOR_YUV2BGR_UYNV: int = @ccode {cv::COLOR_YUV2BGR_UYNV}

val COLOR_YUV2RGBA_UYVY: int = @ccode {cv::COLOR_YUV2RGBA_UYVY}
val COLOR_YUV2BGRA_UYVY: int = @ccode {cv::COLOR_YUV2BGRA_UYVY}
//COLOR_YUV2RGBA_VYUY = 113,
//COLOR_YUV2BGRA_VYUY = 114,
val COLOR_YUV2RGBA_Y422: int = @ccode {cv::COLOR_YUV2RGBA_Y422}
val COLOR_YUV2BGRA_Y422: int = @ccode {cv::COLOR_YUV2BGRA_Y422}
val COLOR_YUV2RGBA_UYNV: int = @ccode {cv::COLOR_YUV2RGBA_UYNV}
val COLOR_YUV2BGRA_UYNV: int = @ccode {cv::COLOR_YUV2BGRA_UYNV}

val COLOR_YUV2RGB_YUY2: int = @ccode {cv::COLOR_YUV2RGB_YUY2}
val COLOR_YUV2BGR_YUY2: int = @ccode {cv::COLOR_YUV2BGR_YUY2}
val COLOR_YUV2RGB_YVYU: int = @ccode {cv::COLOR_YUV2RGB_YVYU}
val COLOR_YUV2BGR_YVYU: int = @ccode {cv::COLOR_YUV2BGR_YVYU}
val COLOR_YUV2RGB_YUYV: int = @ccode {cv::COLOR_YUV2RGB_YUYV}
val COLOR_YUV2BGR_YUYV: int = @ccode {cv::COLOR_YUV2BGR_YUYV}
val COLOR_YUV2RGB_YUNV: int = @ccode {cv::COLOR_YUV2RGB_YUNV}
val COLOR_YUV2BGR_YUNV: int = @ccode {cv::COLOR_YUV2BGR_YUNV}

val COLOR_YUV2RGBA_YUY2: int = @ccode {cv::COLOR_YUV2RGBA_YUY2}
val COLOR_YUV2BGRA_YUY2: int = @ccode {cv::COLOR_YUV2BGRA_YUY2}
val COLOR_YUV2RGBA_YVYU: int = @ccode {cv::COLOR_YUV2RGBA_YVYU}
val COLOR_YUV2BGRA_YVYU: int = @ccode {cv::COLOR_YUV2BGRA_YVYU}
val COLOR_YUV2RGBA_YUYV: int = @ccode {cv::COLOR_YUV2RGBA_YUYV}
val COLOR_YUV2BGRA_YUYV: int = @ccode {cv::COLOR_YUV2BGRA_YUYV}
val COLOR_YUV2RGBA_YUNV: int = @ccode {cv::COLOR_YUV2RGBA_YUNV}
val COLOR_YUV2BGRA_YUNV: int = @ccode {cv::COLOR_YUV2BGRA_YUNV}

val COLOR_YUV2GRAY_UYVY: int = @ccode {cv::COLOR_YUV2GRAY_UYVY}
val COLOR_YUV2GRAY_YUY2: int = @ccode {cv::COLOR_YUV2GRAY_YUY2}
//CV_YUV2GRAY_VYUY    = CV_YUV2GRAY_UYVY,
val COLOR_YUV2GRAY_Y422: int = @ccode {cv::COLOR_YUV2GRAY_Y422}
val COLOR_YUV2GRAY_UYNV: int = @ccode {cv::COLOR_YUV2GRAY_UYNV}
val COLOR_YUV2GRAY_YVYU: int = @ccode {cv::COLOR_YUV2GRAY_YVYU}
val COLOR_YUV2GRAY_YUYV: int = @ccode {cv::COLOR_YUV2GRAY_YUYV}
val COLOR_YUV2GRAY_YUNV: int = @ccode {cv::COLOR_YUV2GRAY_YUNV}

//! alpha premultiplication
val COLOR_RGBA2mRGBA: int = @ccode {cv::COLOR_RGBA2mRGBA}
val COLOR_mRGBA2RGBA: int = @ccode {cv::COLOR_mRGBA2RGBA}

//! RGB to YUV 4:2:0 family
val COLOR_RGB2YUV_I420: int = @ccode {cv::COLOR_RGB2YUV_I420}
val COLOR_BGR2YUV_I420: int = @ccode {cv::COLOR_BGR2YUV_I420}
val COLOR_RGB2YUV_IYUV: int = @ccode {cv::COLOR_RGB2YUV_IYUV}
val COLOR_BGR2YUV_IYUV: int = @ccode {cv::COLOR_BGR2YUV_IYUV}

val COLOR_RGBA2YUV_I420: int = @ccode {cv::COLOR_RGBA2YUV_I420}
val COLOR_BGRA2YUV_I420: int = @ccode {cv::COLOR_BGRA2YUV_I420}
val COLOR_RGBA2YUV_IYUV: int = @ccode {cv::COLOR_RGBA2YUV_IYUV}
val COLOR_BGRA2YUV_IYUV: int = @ccode {cv::COLOR_BGRA2YUV_IYUV}
val COLOR_RGB2YUV_YV12: int = @ccode {cv::COLOR_RGB2YUV_YV12}
val COLOR_BGR2YUV_YV12: int = @ccode {cv::COLOR_BGR2YUV_YV12}
val COLOR_RGBA2YUV_YV12: int = @ccode {cv::COLOR_RGBA2YUV_YV12}
val COLOR_BGRA2YUV_YV12: int = @ccode {cv::COLOR_BGRA2YUV_YV12}

//! Demosaicing
val COLOR_BayerBG2BGR: int = @ccode {cv::COLOR_BayerBG2BGR}
val COLOR_BayerGB2BGR: int = @ccode {cv::COLOR_BayerGB2BGR}
val COLOR_BayerRG2BGR: int = @ccode {cv::COLOR_BayerRG2BGR}
val COLOR_BayerGR2BGR: int = @ccode {cv::COLOR_BayerGR2BGR}

val COLOR_BayerBG2RGB: int = @ccode {cv::COLOR_BayerBG2RGB}
val COLOR_BayerGB2RGB: int = @ccode {cv::COLOR_BayerGB2RGB}
val COLOR_BayerRG2RGB: int = @ccode {cv::COLOR_BayerRG2RGB}
val COLOR_BayerGR2RGB: int = @ccode {cv::COLOR_BayerGR2RGB}

val COLOR_BayerBG2GRAY: int = @ccode {cv::COLOR_BayerBG2GRAY}
val COLOR_BayerGB2GRAY: int = @ccode {cv::COLOR_BayerGB2GRAY}
val COLOR_BayerRG2GRAY: int = @ccode {cv::COLOR_BayerRG2GRAY}
val COLOR_BayerGR2GRAY: int = @ccode {cv::COLOR_BayerGR2GRAY}

//! Demosaicing using Variable Number of Gradients
val COLOR_BayerBG2BGR_VNG: int = @ccode {cv::COLOR_BayerBG2BGR_VNG}
val COLOR_BayerGB2BGR_VNG: int = @ccode {cv::COLOR_BayerGB2BGR_VNG}
val COLOR_BayerRG2BGR_VNG: int = @ccode {cv::COLOR_BayerRG2BGR_VNG}
val COLOR_BayerGR2BGR_VNG: int = @ccode {cv::COLOR_BayerGR2BGR_VNG}

val COLOR_BayerBG2RGB_VNG: int = @ccode {cv::COLOR_BayerBG2RGB_VNG}
val COLOR_BayerGB2RGB_VNG: int = @ccode {cv::COLOR_BayerGB2RGB_VNG}
val COLOR_BayerRG2RGB_VNG: int = @ccode {cv::COLOR_BayerRG2RGB_VNG}
val COLOR_BayerGR2RGB_VNG: int = @ccode {cv::COLOR_BayerGR2RGB_VNG}

//! Edge-Aware Demosaicing
val COLOR_BayerBG2BGR_EA: int = @ccode {cv::COLOR_BayerBG2BGR_EA}
val COLOR_BayerGB2BGR_EA: int = @ccode {cv::COLOR_BayerGB2BGR_EA}
val COLOR_BayerRG2BGR_EA: int = @ccode {cv::COLOR_BayerRG2BGR_EA}
val COLOR_BayerGR2BGR_EA: int = @ccode {cv::COLOR_BayerGR2BGR_EA}

val COLOR_BayerBG2RGB_EA: int = @ccode {cv::COLOR_BayerBG2RGB_EA}
val COLOR_BayerGB2RGB_EA: int = @ccode {cv::COLOR_BayerGB2RGB_EA}
val COLOR_BayerRG2RGB_EA: int = @ccode {cv::COLOR_BayerRG2RGB_EA}
val COLOR_BayerGR2RGB_EA: int = @ccode {cv::COLOR_BayerGR2RGB_EA}

//! Demosaicing with alpha channel
val COLOR_BayerBG2BGRA: int = @ccode {cv::COLOR_BayerBG2BGRA}
val COLOR_BayerGB2BGRA: int = @ccode {cv::COLOR_BayerGB2BGRA}
val COLOR_BayerRG2BGRA: int = @ccode {cv::COLOR_BayerRG2BGRA}
val COLOR_BayerGR2BGRA: int = @ccode {cv::COLOR_BayerGR2BGRA}

val COLOR_BayerBG2RGBA: int = @ccode {cv::COLOR_BayerBG2RGBA}
val COLOR_BayerGB2RGBA: int = @ccode {cv::COLOR_BayerGB2RGBA}
val COLOR_BayerRG2RGBA: int = @ccode {cv::COLOR_BayerRG2RGBA}
val COLOR_BayerGR2RGBA: int = @ccode {cv::COLOR_BayerGR2RGBA}

val COLOR_COLORCVT_MAX: int = @ccode {cv::COLOR_COLORCVT_MAX}

@private fun cvtColor_(src: anyarr_t, code: int, dstcn: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::cvtColor(c_src, c_dst, (int)code, (int)dstcn);
        fx_status = cvt_from(c_dst, src1->t0.ndims, src1->t1.tag, dstcn, fx_result);
    )
    return fx_status;
}

fun cvtColorToGray(src: ('t...) [,], code: int): 't [,] =
    (reinterpret(cvtColor_(anyarray(src), code, 1)) : 't [,])
fun cvtGrayToColor(src: 't [,], code: int): ('t*3) [,] =
    (reinterpret(cvtColor_(anyarray(src), code, 3)) : ('t*3) [,])
fun cvtGrayToColorAlpha(src: 't [,], code: int): ('t*3) [,] =
    (reinterpret(cvtColor_(anyarray(src), code, 4)) : ('t*4) [,])
fun cvtColor(src: ('t ...) [,], code: int): ('t*3) [,] =
    (reinterpret(cvtColor_(anyarray(src), code, 3)) : ('t*3) [,])
fun cvtColorAlpha(src: ('t ...) [,], code: int): ('t*4) [,] =
    (reinterpret(cvtColor_(anyarray(src), code, 4)) : ('t*4) [,])

@private fun cvtColorTwoPlane_(src1: anyarr_t, src2: anyarr_t, code: int, dstcn: int): uint8 [,]
@ccode {
    cv::Mat c_src1, c_src2, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src1, c_src1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)src2, c_src2);

    FX_OCV_TRY_CATCH(
        cv::cvtColorTwoPlane(c_src1, c_src2, c_dst, (int)code);
        fx_status = cvt_from(c_dst, src1->t0.ndims, src1->t1.tag, dstcn, fx_result);
    )
    return fx_status;
}

fun cvtColorTwoPlane(src1: 't [,], src2: ('t*2) [,], code: int): ('t*3) [,] =
    (reinterpret(cvtColorTwoPlane_(anyarray(src1), anyarray(src2), code, 3)) : ('t*3) [,])
fun cvtColorTwoPlaneAlpha(src1: 't [,], src2: ('t*2) [,], code: int): ('t*4) [,] =
    (reinterpret(cvtColorTwoPlane_(anyarray(src1), anyarray(src2), code, 4)) : ('t*4) [,])

@private fun demosaic_(src: anyarr_t, code: int, dstcn: int): uint8 [,]
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::demosaic(c_src, c_dst, (int)code, (int)dstcn);
        fx_status = cvt_from(c_dst, src1->t0.ndims, src1->t1.tag, dstcn, fx_result);
    )
    return fx_status;
}

fun demosaic(src: 't [,], code: int): ('t*3) [,] =
    (reinterpret(demosaic_(anyarray(src), code, 3)) : ('t*3) [,])
fun demosaicAlpha(src: 't [,], code: int): ('t*4) [,] =
    (reinterpret(demosaic_(anyarray(src), code, 4)) : ('t*4) [,])

@private fun moments_(src: anyarr_t, binaryImage:bool): (double*10)
@ccode {
    cv::Mat c_src;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::Moments m = cv::moments(c_src, binaryImage);
        CV_Assert(sizeof(m) == sizeof(*fx_result));
        memcpy(&m, fx_result, sizeof(*fx_result));
    )
    return fx_status;
}

fun moments(src: 't [,], ~binaryImage:bool=false): (double*10) =
    moments_(anyarray(src), binaryImage)

fun HuMoments(moments: (double*10)): (double*7)
@ccode {
    int fx_status = FX_OK;
    FX_OCV_TRY_CATCH(
        cv::HuMoments((cv::Moments*)&moments, &fx_result->t0);
    )
    return fx_status;
}

val TM_SQDIFF: int = @ccode {cv::TM_SQDIFF}
val TM_SQDIFF_NORMED: int = @ccode {cv::TM_SQDIFF_NORMED}
val TM_CCORR: int = @ccode {cv::TM_CCORR}
val TM_CCORR_NORMED: int = @ccode {cv::TM_CCORR_NORMED}
val TM_CCOEFF: int = @ccode {cv::TM_CCOEFF}
val TM_CCOEFF_NORMED: int = @ccode {cv::TM_CCOEFF_NORMED}

@private fun matchTemplate_(image: anyarr_t, templ: anyarr_t,
                            method: int, mask: anyarr_t): float [,]
@ccode {
    cv::Mat c_src, c_templ, c_mask, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)templ, c_templ);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)mask, c_mask);

    FX_OCV_TRY_CATCH(
        cv::matchTemplate(c_src, c_templ, c_dst, (int)method, c_mask);
        fx_status = cvt_from(c_dst, 2, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

fun matchTemplate(image: 't [,], templ: 't [,], method: int, ~mask: uint8 [,]=[]): float [,] =
    matchTemplate_(anyarray(image), anyarray(templ), method, anyarray(mask))

fun connectedComponents_(src: uint8 [,], connectivity: int): int32 [,]
@ccode {
    _fx_anyarr_t src_ = {*src, _FX_DEPTH_U8, 1};
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to(&src_, c_src);

    FX_OCV_TRY_CATCH(
        cv::connectedComponents(c_src, c_dst, (int)connectivity);
        fx_status = cvt_from(c_dst, 2, _FX_DEPTH_S32, 1, fx_result);
    )
    return fx_status;
}

fun connectedComponentsWithStats(src: uint8 [,], connectivity: int):
    (int32 [,], int32 [,], double [,])
@ccode {
    _fx_anyarr_t src_ = {*src, _FX_DEPTH_U8, 1};
    cv::Mat c_src, c_labels, c_stats, c_centroids;
    c_labels.allocator = &g_fxarrAllocator;
    c_stats.allocator = &g_fxarrAllocator;
    c_centroids.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to(&src_, c_src);

    FX_OCV_TRY_CATCH(
        cv::connectedComponentsWithStats(c_src, c_labels, c_stats, c_centroids, (int)connectivity);
        fx_status = cvt_from(c_labels, 2, _FX_DEPTH_S32, 1, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(c_stats, 2, _FX_DEPTH_S32, 1, &fx_result->t1);
        if (fx_status >= 0)
            fx_status = cvt_from(c_centroids, 2, _FX_DEPTH_FP64, 1, &fx_result->t2);
    )
    return fx_status;
}

// [TODO]
//fun findContours(src: uint8 [,], mode: int, method: int, ~offset:intx2=(0,0)): (int32x2 [], intx2 [], intx4 [])

@private fun approxPolyDP_(curve: anyarr_t, epsilon: double, closed: bool): uint8 []
@ccode {
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)curve, c_src);

    FX_OCV_TRY_CATCH(
        cv::approxPolyDP(c_src, c_dst, epsilon, closed);
        fx_status = cvt_from(c_dst, 1, curve->t1.tag, curve->t2, fx_result);
    )
    return fx_status;
}

fun approxPolyDP(curve: 't [], epsilon: double, ~closed: bool): 't [] =
    (reinterpret(approxPolyDP_(anyarray(curve), epsilon, closed)) : 't [])

@private fun arcLength_(curve: anyarr_t, closed: bool): double
@ccode {
    cv::Mat c_src;
    int fx_status = cvt_to((const _fx_anyarr_t*)curve, c_src);

    FX_OCV_TRY_CATCH(
        *fx_result = cv::arcLength(c_src, closed);
    )
    return fx_status;
}

fun arcLength(curve: 't [], ~closed: bool): double =
    arcLength_(anyarray(curve), closed)

@private fun contourArea_(curve: anyarr_t, oriented: bool): double
@ccode {
    cv::Mat c_src;
    int fx_status = cvt_to((const _fx_anyarr_t*)curve, c_src);

    FX_OCV_TRY_CATCH(
        *fx_result = cv::contourArea(c_src, oriented);
    )
    return fx_status;
}

fun contourArea(src: 't [], ~oriented: bool=false): double =
    contourArea_(anyarray(curve), oriented)

@private fun boundingRect_(src: anyarr_t): intx4
@ccode {
    cv::Mat c_src;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);

    FX_OCV_TRY_CATCH(
        cv::Rect r = cv::boundingRect(c_src);
        cvt_rect2intx4(r, &fx_result->t0);
    )
    return fx_status;
}

fun boundingRect(src: 't [+]): intx4 = boundingRect_(anyarray(src))

@private fun minAreaRect_(points: anyarr_t): box_t
@ccode {
    cv::Mat c_src;
    int fx_status = cvt_to((const _fx_anyarr_t*)points, c_src);

    FX_OCV_TRY_CATCH(
        cv::RotatedRect rr = cv::minAreaRect(c_src);
        cvt_rr2box(rr, fx_result);
    )
    return fx_status;
}

fun minAreaRect(points: 't []): box_t = minAreaRect_(anyarray(points))

fun boxPoints(box: box_t): floatx2 []
@ccode {
    cv::Mat c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = FX_OK;
    cv::RotatedRect rr = cvt_box2rr(box);

    FX_OCV_TRY_CATCH(
        cv::boxPoints(rr, c_dst);
        fx_status = cvt_from(c_dst, 1, _FX_DEPTH_FP32, 2, fx_result);
    )
    return fx_status;
}

@private fun minEnclosingCircle_(points: anyarr_t): (floatx2, float)
@ccode {
    cv::Mat c_src;
    int fx_status = cvt_to((const _fx_anyarr_t*)points, c_src);

    FX_OCV_TRY_CATCH(
        cv::Point2f center;
        minEnclosingCircle(c_src, center, fx_result->t1);
        fx_result->t0.t0 = center.x;
        fx_result->t0.t1 = center.y;
    )
    return fx_status;
}

fun minEnclosingCircle(points: 't []): (floatx2, float) =
    minEnclosingCircle_(anyarray(points))

@private fun matchShapes_(contour1: anyarr_t, contour2: anyarr_t,
                          method: int, parameter: double): double
@ccode {
    cv::Mat c_src1, c_src2;
    int fx_status = cvt_to((const _fx_anyarr_t*)contour1, c_src1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)contour2, c_src2);

    FX_OCV_TRY_CATCH(
        *fx_result = cv::matchShapes(c_src1, c_src2, (int)method, parameter);
    )
    return fx_status;
}

fun matchShapes(contour1: 't [], contour2: 't [], ~method: int, ~parameter: double): double =
    matchShapes_(anyarray(contour1), anyarray(contour2), method, parameter)

@private fun convexHull_(points: anyarr_t, clockwise:bool, returnPoints: bool): uint8 []
@ccode {
    cv::Mat c_points, c_hull;
    c_hull.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)points, c_points);
    FX_OCV_TRY_CATCH(
        cv::convexHull(c_points, c_hull, clockwise, returnPoints);
        if (returnPoints)
            fx_status = cvt_from(c_hull, 1, points->t1.tag, points->t2, fx_result);
        else
            fx_status = cvt_from(c_hull, 1, _FX_DEPTH_S32, 1, fx_result);
    )
    return fx_status;
}

fun convexHull(points: 't [], ~clockwise:bool=false): 't [] =
    (reinterpret(convexHull_(anyarray(points), clockwise, true)) : 't [])

fun convexHullIdx(points: 't [], ~clockwise:bool=false): 't [] =
    (reinterpret(convexHull_(anyarray(points), clockwise, false)) : int32 [])

@private fun isContourConvex_(contour: anyarr_t): bool
@ccode {
    cv::Mat c_contour;
    int fx_status = cvt_to((const _fx_anyarr_t*)contour, c_contour);
    FX_OCV_TRY_CATCH(
        *fx_result = cv::isContourConvex(c_contour);
    )
    return fx_status;
}

fun isContourConvex(contour: 't []): bool = isContourConvex_(anyarray(contour))

@private fun intersectConvexConvex_(p1: anyarr_t, p2: anyarr_t, handleNested: bool): uint8 []
@ccode {
    cv::Mat c_p1, c_p2, c_intersect;
    c_intersect.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)p1, c_p1);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)p2, c_p2);
    FX_OCV_TRY_CATCH(
        cv::intersectConvexConvex(c_p1, c_p2, c_intersect, handleNested);
        fx_status = cvt_from(c_intersect, 1, p1->t1.tag, c_p1->t2, fx_result);
    )
    return fx_status;
}

fun intersectConvexConvex(p1: 't [], p2: 't [], ~handleNested: bool=true): 't [] =
    (reinterpret(intersectConvexConvex_(anyarray(p1), anyarray(p2), handleNested)) : 't [])

@private fun fitEllipse_(points: anyarr_t): box_t
@ccode {
    cv::Mat c_points, c_ellipse;
    c_ellipse.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)points, c_points);
    FX_OCV_TRY_CATCH(
        cv::RotatedRect rr = cv::fitEllipse(c_points);
        cvt_rr2box(rr, fx_result);
    )
    return fx_status;
}

fun fitEllipse(points: 't []): box_t = fitEllipse_(anyarray(points))

@private fun fitEllipseAMS_(points: anyarr_t): box_t
@ccode {
    cv::Mat c_points, c_ellipse;
    c_ellipse.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)points, c_points);
    FX_OCV_TRY_CATCH(
        cv::RotatedRect rr = cv::fitEllipseAMS(c_points);
        cvt_rr2box(rr, fx_result);
    )
    return fx_status;
}

fun fitEllipseAMS(points: 't []): box_t = fitEllipseAMS_(anyarray(points))

@private fun fitEllipseDirect_(points: anyarr_t): box_t
@ccode {
    cv::Mat c_points, c_ellipse;
    c_ellipse.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)points, c_points);
    FX_OCV_TRY_CATCH(
        cv::RotatedRect rr = cv::fitEllipseDirect(c_points);
        cvt_rr2box(rr, fx_result);
    )
    return fx_status;
}

fun fitEllipseDirect(points: 't []): box_t = fitEllipseDirect_(anyarray(points))

@private fun fitLine_(points: anyarr_t, distType: int,
                      param: double, reps: double, aeps: double): float []
@ccode {
    cv::Mat c_points, c_line;
    c_line.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)points, c_points);
    FX_OCV_TRY_CATCH(
        cv::fitLine(c_points, c_line, (int)distType, param, reps, aeps);
        fx_status = cvt_from(c_line, 1, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

fun fitLine(points: 't [], ~distType: int, ~param: double, ~reps: double, ~aeps: double): float [] =
    fitLine_(anyarray(points), distType, param, reps, aeps)

@private fun pointPolygonTest_(contour: anyarr_t, pt: floatx2, measureDist: bool): double
@ccode {
    cv::Mat c_contour;
    int fx_status = cvt_to((const _fx_anyarr_t*)contour, c_contour);
    FX_OCV_TRY_CATCH(
        *fx_result = cv::pointPolygonTest(c_contour, cvt_point(&pt->t0), measureDist);
    )
    return fx_status;
}

fun pointPolygonTest(contour: 't [], pt: floatx2, ~measureDist: bool): double =
    pointPolygonTest_(anyarray(contour), pt, measureDist)

fun rotatedRectangleIntersection(rrect1: box_t, rrect2: box_t): (int, floatx2 [])
@ccode {
    cv::Mat c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    cv::RotatedRect rr1 = cvt_box2rr(rrect1), rr2 = cvt_box2rr(rrect2);
    FX_OCV_TRY_CATCH(
        *fx_result->t0 = cv::rotatedRectangleIntersection(rr1, rr2, c_dst);
        fx_status = cvt_from(c_dst, 1, _FX_DEPTH_FP32, 2, &fx_result->t1);
    )
    return fx_status;
}

@private fun applyColorMap_(src: anyarr_t, colormap: int, usermap: anyarr_t): uint8x3 [,]
@ccode {
    cv::Mat c_src, c_usermap, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)src, c_src);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)usermap, c_usermap);
    FX_OCV_TRY_CATCH(
        bool have_usermap = usermap->t0.data != 0;
        if (!have_usermap)
            cv::applyColorMap(c_src, c_dst, (int)colormap);
        else
            cv::applyColorMap(c_src, c_dst, c_usermap);
        fx_status = cvt_from(c_dst, src->t0.ndims, _FX_DEPTH_U8, 3, fx_result);
    )
    return fx_status;
}

fun applyColorMap(src: 't [,], ~colormap: int, ~usermap: uint8x3 []=[]): uint8x3 [,] =
    applyColorMap_(anyarray(src), colormap, anyarray(usermap))

val FILLED  = -1
val LINE_4  = 4
val LINE_8  = 8
val LINE_AA = 16

fun RGB(r: 't, g: 't, b: 't): doublex4 = (double(b), double(g), double(r), 0.)
fun RGBA(r: 't, g: 't, b: 't, a: 't): doublex4 = (double(b), double(g), double(r), double(a))
fun GRAY(g: 't): doublex4 = (double(g), double(g), double(g), 0.)

@private fun line_(img: anyarr_t, pt1: intx2, pt2: intx2, color: doublex4,
        thickness: int, lineType: int, shift: int): void
@ccode {
    cv::Mat c_img;
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::line(c_img, cvt_point(&pt1->t0), cvt_point(&pt2->t0), cvt_scalar(&color->t0),
                 (int)thickness, (int)lineType, (int)shift);
    )
    return fx_status;
}

fun line(img: 't [,], pt1: intx2, pt2: intx2, color: doublex4,
        ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void =
    line_(anyarray(img), pt1, pt2, color, thickness, linetype, shift)

@private fun arrowedLine_(img: anyarr_t, pt1: intx2, pt2: intx2, color: doublex4,
                thickness: int, lineType: int, shift: int,
                tipLength: double): void
@ccode {
    cv::Mat c_img;
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::arrowedLine(c_img, cvt_point(&pt1->t0), cvt_point(&pt2->t0), cvt_scalar(&color->t0),
                 (int)thickness, (int)lineType, (int)shift, tipLength);
    )
    return fx_status;
}

fun arrowedLine(img: 't [,], pt1: intx2, pt2: intx2, color: doublex4,
                ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0,
                ~tipLength: double=0.1): void =
    allowedLine_(anyarray(img), pt1, pt2, color, thickness, lineType, shift, tipLength)

@private fun rectangle_(img: anyarr_t, pt1: intx2, pt2: intx2, color: doublex4,
                        thickness: int, lineType: int, shift: int): void
@ccode {
    cv::Mat c_img;
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::rectangle(c_img, cvt_point(&pt1->t0), cvt_point(&pt2->t0), cvt_scalar(&color->t0),
                      (int)thickness, (int)lineType, (int)shift);
    )
    return fx_status;
}

fun rectangle(img: 't [,], pt1: intx2, pt2: intx2, color: doublex4,
              ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void =
    rectangle_(anyarray(img), pt1, pt2, color, thickness, lineType, shift)

fun rectangle(img: 't [,], rect: intx4, color: doublex4,
              ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void
{
    val delta = 1 << shift
    rectangle_(anyarray(img), (rect.0, rect.1),
        (rect.0 + rect.2 - delta, rect.1 + rect.3 - delta),
        color, thickness, lineType, shift)
}

@private fun circle_(img: anyarr_t, center: intx2, radius: int, color: doublex4,
                     thickness: int, lineType: int, shift: int): void
@ccode {
    cv::Mat c_img;
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::circle(c_img, cvt_point(&center->t0), (int)radius, cvt_scalar(&color->t0),
                      (int)thickness, (int)lineType, (int)shift);
    )
    return fx_status;
}

fun circle(img: 't [,], center: intx2, radius: int, color: doublex4,
            ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void =
    circle_(anyarray(img), center, radius, color, thickness, lineType, shift)

@private fun ellipse_(img: anyarr_t, center: intx2, axes: intx2,
            angle: double, startAngle: double, endAngle: double,
            color: doublex4, thickness: int, lineType: int, shift: int): void
@ccode {
    cv::Mat c_img;
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::ellipse(c_img, cvt_point(&center->t0), cvt_size(&axes->t0),
                    angle, startangle, endAngle, cvt_scalar(&color->t0),
                    (int)thickness, (int)lineType, (int)shift);
    )
    return fx_status;
}

fun ellipse(img: 't [,], center: intx2, axes: intx2, color: doublex4,
            ~angle: double=0, ~startAngle: double=0, ~endAngle: double=360,
            ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void =
    ellipse_(anyarray(img), center, axes, angle, startAngle, endAngle,
            color, thickness, lineType, shift)

@private fun ellipse_(img: anyarr_t, box: box_t, color: doublex4,
            thickness: int, lineType: int): void
@ccode {
    cv::Mat c_img;
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::RotatedRect rr = cvt_box2rr(box);
        cv::ellipse(c_img, rr, cvt_scalar(&color->t0),
                    (int)thickness, (int)lineType);
    )
    return fx_status;
}

fun ellipse(img: 't [,], box: box_t, color: doublex4,
            ~thickness: int=1, ~lineType: int=LINE_8): void =
    ellipse_(anyarray(img), box, color, thickness, lineType)

val MARKER_CROSS: int = @ccode {cv::MARKER_CROSS}
val MARKER_TILTED_CROSS: int = @ccode {cv::MARKER_TILTED_CROSS}
val MARKER_STAR: int = @ccode {cv::MARKER_STAR}
val MARKER_DIAMOND: int = @ccode {cv::MARKER_DIAMOND}
val MARKER_SQUARE: int = @ccode {cv::MARKER_SQUARE}
val MARKER_TRIANGLE_UP: int = @ccode {cv::MARKER_TRIANGLE_UP}
val MARKER_TRIANGLE_DOWN: int = @ccode {cv::MARKER_TRIANGLE_DOWN}

@private fun drawMarker_(img: anyarr_t, pos: intx2, color: doublex4,
               markerType: int, markerSize: int,
               thickness: int, lineType: int): void
@ccode {
    cv::Mat c_img;
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    FX_OCV_TRY_CATCH(
        cv::drawMarker(c_img, cvt_point(&pos->t0), cvt_scalar(&color->t0),
                    (int)markerType, (int)markerSize,
                    (int)thickness, (int)lineType);
    )
    return fx_status;
}

fun drawMarker(img: 't [,], pos: intx2, color: doublex4,
               ~markerType: int=MARKER_CROSS, ~markerSize: int=20,
               ~thickness: int=1, ~lineType: int=LINE_8): void =
    drawMaker_(anyarray(img), pos, color, markerType, markerSize, thickness, lineType)

@private fun fillConvexPoly(img: anyarr_t, points: anyarr_t, color: doublex4,
                            lineType: int, shift: int): void
@ccode {
    cv::Mat c_img, c_points;
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)points, c_points);
    FX_OCV_TRY_CATCH(
        cv::fillConvexPoly(c_img, c_points, cvt_scalar(&color->t0),
                        (int)lineType, (int)shift);
    )
    return fx_status;
}

fun fillConvexPoly(img: 't [,], points: int32x2 [], color: doublex4,
                   ~lineType: int = LINE_8, ~shift: int=0): void =
    fillConvexPoly_(anyarray(img), anyarray(points), color, lineType, shift)

@private fun fillPoly_(img: anyarr_t, points: int32x2 [][], color: doublex4,
             lineType: int, shift: int, offset: intx2): void
@ccode {
    cv::Mat c_img;
    int i, ncontours = (int)points->dim[0].size;
    std::vector<cv::Point*> pts(ncontours + 1);
    std::vector<int> npts(ncontours + 1);
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    for(i = 0; i < ncontours; i++) {
        fx_arr_t* contour_i = FX_PTR_1D(fx_arr_t, points, i);
        pts[i] = FX_PTR_1D(cv::Point, contour_i, 0);
        npts[i] = (int)contour_i->dim[0].size;
    }
    FX_OCV_TRY_CATCH(
        cv::fillPoly(c_img, (const cv::Point**)&pts[0], &npts[0], ncontours,
            cvt_scalar(&color->t0), (int)lineType, (int)shift, cvt_point(&offset->t0));
    )
    return fx_status;
}

fun fillPoly(img: 't [,], points: int32x2 [][], color: doublex4,
             ~lineType: int = LINE_8, ~shift: int=0, ~offset: intx2=(0,0)): void =
    fillPoly_(anyarray(img), points, color, linetype, shift, offset)

@private fun polylines_(img: anyarr_t, points: int32x2 [][], color: doublex4,
    isClosed: bool, thickness: int, lineType: int, shift: int, offset: intx2): void
@ccode {
    cv::Mat c_img;
    int i, ncontours = (int)points->dim[0].size;
    std::vector<cv::Point*> pts(ncontours + 1);
    std::vector<int> npts(ncontours + 1);
    int fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    for(i = 0; i < ncontours; i++) {
        fx_arr_t* contour_i = FX_PTR_1D(fx_arr_t, points, i);
        pts[i] = FX_PTR_1D(cv::Point, contour_i, 0);
        npts[i] = (int)contour_i->dim[0].size;
    }
    FX_OCV_TRY_CATCH(
        cv::polylines(c_img, (const cv::Point**)&pts[0], &npts[0], ncontours,
            isClosed, cvt_scalar(&color->t0), (int)thickness,
            (int)lineType, (int)shift, cvt_point(&offset->t0));
    )
    return fx_status;
    InputOutputArray img, const Point* const* pts, const int* npts,
                          int ncontours, bool isClosed, const Scalar& color,
                          int thickness = 1, int lineType = LINE_8, int shift = 0 );
}

fun polylines(img: 't [,], points: int32x2 [][], color: doublex4,
    ~isClosed: bool=true, ~thickness: int=1, ~lineType: int = LINE_8,
    ~shift: int=0, ~offset: intx2=(0,0)): void =
    polylines_(anyarray(img), points, color, isClosed, thickness, lineType, shift, offset)

// [TODO]
//fun drawContours(img: 't [,], contours: (int32x2 [], intx2 [], intx4 []),
//                 ~contourIdx: int, ~color: doublex4,
//                 ~thickness: int=1, ~lineType: int=LINE_8,
//                  ~maxLevel: int=1000000, ~offset: intx2=(0,0)): void

//fun clipLine(imgSize: intx2, pt1: intx2, pt2: intx2): (bool, intx2, intx2)
//fun ellipse2Poly(center: floatx2, axes: floatx2, ~angle: int=0,
//                ~arcStart: int=0, ~arcEnd: int=360,
//                ~delta: int=1): int []

class FontFace
{
    fface: cptr;
}

@ccode {
static void _fx_ocv_free_fface(void* ptr) { delete (cv::FontFace*)ptr; }
}

fun makeFontFace(fontNameOrPath: string): FontFace
{
    fun makeFontFace_(fontPathOrName: string): cptr =
    @ccode {
        cv::FontFace* fface = 0;
        std::string c_fontname;
        int fx_status = cvt_to(fontPathOrName, c_fontname);
        FX_OCV_TRY_CATCH(
            fface = new cv::FontFace(c_fontname);
            if(fface && !fface->getName().empty()) {
                fx_status = fx_make_cptr(fface, _fx_ocv_free_fface, fx_result);
            } else {
                delete fface;
                fx_status = fx_ocv_err("cannot load font");
            }
        )
        return fx_status;
    }
    FontFace {fface=makeFontFace_(fontNameOrPath)}
}

fun FontFace.setInstance(params: int []): bool
@ccode {
    int i, nparams = (int)params->dim[0].size;
    int fx_status = FX_OK;
    std::vector<int> params_(nparams);
    if (!self->fface || !self->fface->ptr)
        return fx_ocv_err("font is not initialized");
    for(i = 0; i < nparams; i++)
        params_[i] = (int)*FX_PTR_1D(int_, params, i);
    FX_OCV_TRY_CATCH(
        *fx_result = ((cv::FontFace*)self->fface->ptr)->setInstance(params_);
    )
    return fx_status;
}

@private fun FontFace_getInstance_(fface: cptr): int32 []
@ccode {
    int fx_status = FX_OK;
    std::vector<int> params_;
    if (!fface || !fface->ptr)
        return fx_ocv_err("font is not initialized");
    FX_OCV_TRY_CATCH(
        bool ok = ((cv::FontFace*)self->fface->ptr)->getInstance(params_);
        if (ok) {
            cv::Mat m_params(params_, false);
            fx_status = cvt_from(m_params, 1, _FX_DEPTH_S32, 1, fx_result);
        }
    )
    return fx_status;
}

fun FontFace.getInstance(): int [] = int(FontFace_getInstance_(self.fface))

val PUT_TEXT_ALIGN_LEFT: int = 0 //@ccode {cv::PUT_TEXT_ALIGN_LEFT}
val PUT_TEXT_ALIGN_CENTER: int = 1 //@ccode {cv::PUT_TEXT_ALIGN_CENTER}
val PUT_TEXT_ALIGN_RIGHT: int = 2 //@ccode {cv::PUT_TEXT_ALIGN_RIGHT}
val PUT_TEXT_ORIGIN_TL: int = 0 //@ccode {cv::PUT_TEXT_ORIGIN_TL}
val PUT_TEXT_ORIGIN_BL: int = 32 //@ccode {cv::PUT_TEXT_ORIGIN_BL}
val PUT_TEXT_WRAP: int = 128 //@ccode {cv::PUT_TEXT_WRAP}

@private fun putText_(img: anyarr_t, text: string, org: intx2, color: doublex4,
            fontFace: FontFace, size: int, weight: int,
            flags: int, wrap: intx2): void
@ccode {
    cv::Mat c_img;
    std::string c_text;
    int fx_status;
    if (!fontFace->fface || !fontFace->fface->ptr)
        return fx_ocv_err("font is not initialized");
    fx_status = cvt_to((const _fx_anyarr_t*)img, c_img);
    if (fx_status >= 0)
        fx_status = cvt_to(text, c_text);
    FX_OCV_TRY_CATCH(
        cv::putText(c_img, c_text, cvt_point(&org->t0),
                cvt_scalar(&color->t0), *(cv::FontFace*)(fontFace->fface->ptr),
                (int)size, (int)weight, (cv::PutTextFlags)flags,
                cv::Range((int)wrap->t0, (int)wrap->t1));
    )
    return fx_status;
}

fun putText(img: 't [,], text: string, org: intx2, color: doublex4,
            ~fontFace: FontFace, ~size: int, ~weight: int=0,
            ~flags: int=PUT_TEXT_ALIGN_LEFT, ~wrap: intx2=(0,0)): void =
    putText_(anyarray(img), text, org, color, fontFace, size, weight, flags, wrap)

@private fun getTextSize_(imgSize: intx2, text: string, org: intx2,
    fontFace: FontFace, size: int, weight: int,
    flags: int, wrap: intx2): intx4
@ccode {
    std::string c_text;
    int fx_status = cvt_to(text, c_text);
    if (!fontFace->fface || !fontFace->fface->ptr)
        return fx_ocv_err("font is not initialized");
    FX_OCV_TRY_CATCH(
        cv::Rect r = cv::getTextSize(cvt_size(&imgSize->t0), c_text, cvt_point(&org->t0),
                *(cv::FontFace*)(fontFace->fface->ptr),
                (int)size, (int)weight, (cv::PutTextFlags)flags,
                cv::Range((int)wrap->t0, (int)wrap->t1));
        cvt_rect2intx4(r, &fx_result->t0);
    )
    return fx_status;
}

fun getTextSize(imgSize: intx2, text: string, org: intx2,
                ~fontFace: FontFace, ~size: int, ~weight: int=0,
                ~flags: int=PUT_TEXT_ALIGN_LEFT, ~wrap: intx2=(0,0) ): intx4 =
    getTextSize_(imgSize, text, org, fontFace, size, weight, flags, wrap)

//fun lineIterator(pt1: intx2, pt2: intx2, ~connectivity: int, ~leftToRight: bool=false): int32x2 []

///////////////////////////// imgcodecs ////////////////////////////

@ccode {

static int cvt_to(const fx_arr_t* params, std::vector<int>& cvparams)
{
    int fx_status = 0;
    if (params->data != 0) {
        int_ i, nparams = params->dim[0].size;
        if(params->ndims != 1 || params->dim[0].step != sizeof(int_))
            return fx_ocv_err("invalid 'params' array, should be 1D array of int's");
        cvparams.resize((size_t)nparams);
        for(i = 0; i < nparams; i++)
            cvparams[i] = (int)((int_*)params->data)[i];
    }
    return FX_OK;
}

}

val IMWRITE_JPEG_QUALITY: int = @ccode { cv::IMWRITE_JPEG_QUALITY }
val IMWRITE_JPEG_PROGRESSIVE: int = @ccode { cv::IMWRITE_JPEG_PROGRESSIVE }
val IMWRITE_JPEG_OPTIMIZE: int = @ccode { cv::IMWRITE_JPEG_OPTIMIZE }
val IMWRITE_JPEG_RST_INTERVAL: int = @ccode { cv::IMWRITE_JPEG_RST_INTERVAL }
val IMWRITE_JPEG_LUMA_QUALITY: int = @ccode { cv::IMWRITE_JPEG_LUMA_QUALITY }
val IMWRITE_JPEG_CHROMA_QUALITY: int = @ccode { cv::IMWRITE_JPEG_CHROMA_QUALITY }
val IMWRITE_PNG_COMPRESSION: int = @ccode { cv::IMWRITE_PNG_COMPRESSION }
val IMWRITE_PNG_STRATEGY: int = @ccode { cv::IMWRITE_PNG_STRATEGY }
val IMWRITE_PNG_BILEVEL: int = @ccode { cv::IMWRITE_PNG_BILEVEL }
val IMWRITE_PXM_BINARY: int = @ccode { cv::IMWRITE_PXM_BINARY }
val IMWRITE_EXR_TYPE: int = @ccode { cv::IMWRITE_EXR_TYPE }
val IMWRITE_EXR_COMPRESSION: int = @ccode { cv::IMWRITE_EXR_COMPRESSION }
val IMWRITE_WEBP_QUALITY: int = @ccode { cv::IMWRITE_WEBP_QUALITY }
val IMWRITE_PAM_TUPLETYPE: int = @ccode { cv::IMWRITE_PAM_TUPLETYPE }
val IMWRITE_TIFF_RESUNIT: int = @ccode { cv::IMWRITE_TIFF_RESUNIT }
val IMWRITE_TIFF_XDPI: int = @ccode { cv::IMWRITE_TIFF_XDPI }
val IMWRITE_TIFF_YDPI: int = @ccode { cv::IMWRITE_TIFF_YDPI }
val IMWRITE_TIFF_COMPRESSION: int = @ccode { cv::IMWRITE_TIFF_COMPRESSION }
val IMWRITE_JPEG2000_COMPRESSION_X1000: int = @ccode { cv::IMWRITE_JPEG2000_COMPRESSION_X1000 }

val IMWRITE_EXR_TYPE_HALF: int = @ccode { cv::IMWRITE_EXR_TYPE_HALF }
val IMWRITE_EXR_TYPE_FLOAT: int = @ccode { cv::IMWRITE_EXR_TYPE_FLOAT }

val IMWRITE_EXR_COMPRESSION_NO: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_NO }
val IMWRITE_EXR_COMPRESSION_RLE: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_RLE }
val IMWRITE_EXR_COMPRESSION_ZIPS: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_ZIPS }
val IMWRITE_EXR_COMPRESSION_ZIP: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_ZIP }
val IMWRITE_EXR_COMPRESSION_PIZ: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_PIZ }
val IMWRITE_EXR_COMPRESSION_PXR24: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_PXR24 }
val IMWRITE_EXR_COMPRESSION_B44: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_B44 }
val IMWRITE_EXR_COMPRESSION_B44A: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_B44A }
val IMWRITE_EXR_COMPRESSION_DWAA: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_DWAA }
val IMWRITE_EXR_COMPRESSION_DWAB: int = @ccode { cv::IMWRITE_EXR_COMPRESSION_DWAB }

val IMWRITE_PNG_STRATEGY_DEFAULT: int = @ccode { cv::IMWRITE_PNG_STRATEGY_DEFAULT }
val IMWRITE_PNG_STRATEGY_FILTERED: int = @ccode { cv::IMWRITE_PNG_STRATEGY_FILTERED }
val IMWRITE_PNG_STRATEGY_HUFFMAN_ONLY: int = @ccode { cv::IMWRITE_PNG_STRATEGY_HUFFMAN_ONLY }
val IMWRITE_PNG_STRATEGY_RLE: int = @ccode { cv::IMWRITE_PNG_STRATEGY_RLE }
val IMWRITE_PNG_STRATEGY_FIXED: int = @ccode { cv::IMWRITE_PNG_STRATEGY_FIXED }

val IMWRITE_PAM_FORMAT_NULL: int = @ccode { cv::IMWRITE_PAM_FORMAT_NULL }
val IMWRITE_PAM_FORMAT_BLACKANDWHITE: int = @ccode { cv::IMWRITE_PAM_FORMAT_BLACKANDWHITE }
val IMWRITE_PAM_FORMAT_GRAYSCALE: int = @ccode { cv::IMWRITE_PAM_FORMAT_GRAYSCALE }
val IMWRITE_PAM_FORMAT_GRAYSCALE_ALPHA: int = @ccode { cv::IMWRITE_PAM_FORMAT_GRAYSCALE_ALPHA }
val IMWRITE_PAM_FORMAT_RGB: int = @ccode { cv::IMWRITE_PAM_FORMAT_RGB }
val IMWRITE_PAM_FORMAT_RGB_ALPHA: int = @ccode { cv::IMWRITE_PAM_FORMAT_RGB_ALPHA }

fun imread(filename: string): uint8x3 [,]
@ccode {
    std::string c_filename;
    int fx_status = cvt_to(filename, c_filename);
    FX_OCV_TRY_CATCH(
        cv::Mat dst = cv::imread(c_filename, 1);
        fx_status = cvt_from(dst, 2, _FX_DEPTH_U8, 3, fx_result);
    )
    return fx_status;
}

fun imread_gray(filename: string): uint8 [,]
@ccode {
    std::string c_filename;
    int fx_status = cvt_to(filename, c_filename);
    FX_OCV_TRY_CATCH(
        cv::Mat dst = cv::imread(c_filename, 0);
        fx_status = cvt_from(dst, 2, _FX_DEPTH_U8, 1, fx_result);
    )
    return fx_status;
}

@private fun imwrite_(filename: string, img: anyarr_t, params: int []): void
@ccode {
    std::string c_filename;
    cv::Mat c_img;
    std::vector<int> c_params;
    int fx_status = cvt_to(filename, c_filename);
    if (fx_status >= 0)
        fx_status = cvt_to(img, c_img);
    if (fx_status >= 0)
        fx_status = cvt_to(params, c_params);
    FX_OCV_TRY_CATCH(
        cv::imwrite(c_filename, c_img, c_params);
    )
    return fx_status;
}

fun imwrite(filename: string, img: 't [,], ~params: int []=[]) =
    imwrite_(filename, anyarray(img), params)

fun imdecode(buf: uint8 []): uint8x3 [,]
@ccode {
    cv::Mat c_buf;
    int fx_status = cvt_to(filename, c_filename);
    if (fx_status >= 0)
        fx_status = cvt_to(buf, c_buf);
    FX_OCV_TRY_CATCH(
        cv::Mat dst = cv::imdecode(c_buf, 1);
        fx_status = cvt_from(dst, 2, _FX_DEPTH_U8, 3, fx_result);
    )
    return fx_status;
}

fun imdecode(buf: uint8 []): uint8 [,]
@ccode {
    cv::Mat c_buf;
    int fx_status = cvt_to(filename, c_filename);
    if (fx_status >= 0)
        fx_status = cvt_to(buf, c_buf);
    FX_OCV_TRY_CATCH(
        cv::Mat dst = cv::imdecode(c_buf, 0);
        fx_status = cvt_from(dst, 2, _FX_DEPTH_U8, 1, fx_result);
    )
    return fx_status;
}

@private fun imencode_(ext: string, img: anyarr_t, params: int []): uint8 []
@ccode {
    std::string c_filename;
    cv::Mat c_img;
    std::vector<int> c_params;
    std::vector<uchar> dstvec;
    int fx_status = cvt_to(filename, c_filename);
    if (fx_status >= 0)
        fx_status = cvt_to(img, c_img);
    if (fx_status >= 0)
        fx_status = cvt_to(params, c_params);
    FX_OCV_TRY_CATCH(
        cv::imencode(c_filename, c_img, dstvec, c_params);
        cv::Mat dst(dstvec, false);
        fx_status = cvt_from(dst, 1, _FX_DEPTH_U8, 1, fx_result);
    )
    return fx_status;
}

fun imencode(ext: string, img: 't [,], ~params: int []=[]): uint8 [] =
    imencode_(ext, anyarray(img), params)

fun haveImageReader(filename: string): bool
@ccode {
    std::string c_filename;
    *fx_result = false;
    int fx_status = cvt_to(filename, c_filename);
    FX_OCV_TRY_CATCH(
        *fx_result = cv::haveImageReader(c_filename);
    )
    return fx_status;
}

fun haveImageWriter(filename: string): bool
@ccode {
    std::string c_filename;
    *fx_result = false;
    int fx_status = cvt_to(filename, c_filename);
    FX_OCV_TRY_CATCH(
        *fx_result = cv::haveImageWriter(c_filename);
    )
    return fx_status;
}

///////////////////////////// videoio ///////////////////////////////

val CAP_ANY: int = @ccode { cv::CAP_ANY }
val CAP_VFW: int = @ccode { cv::CAP_VFW }
val CAP_V4L: int = @ccode { cv::CAP_V4L }
val CAP_V4L2: int = @ccode { cv::CAP_V4L2 }
val CAP_FIREWIRE: int = @ccode { cv::CAP_FIREWIRE }
val CAP_FIREWARE: int = @ccode { cv::CAP_FIREWARE }
val CAP_IEEE1394: int = @ccode { cv::CAP_IEEE1394 }
val CAP_DC1394: int = @ccode { cv::CAP_DC1394 }
val CAP_CMU1394: int = @ccode { cv::CAP_CMU1394 }
val CAP_QT: int = @ccode { cv::CAP_QT }
val CAP_UNICAP: int = @ccode { cv::CAP_UNICAP }
val CAP_DSHOW: int = @ccode { cv::CAP_DSHOW }
val CAP_PVAPI: int = @ccode { cv::CAP_PVAPI }
val CAP_OPENNI: int = @ccode { cv::CAP_OPENNI }
val CAP_OPENNI_ASUS: int = @ccode { cv::CAP_OPENNI_ASUS }
val CAP_ANDROID: int = @ccode { cv::CAP_ANDROID }
val CAP_XIAPI: int = @ccode { cv::CAP_XIAPI }
val CAP_AVFOUNDATION: int = @ccode { cv::CAP_AVFOUNDATION }
val CAP_GIGANETIX: int = @ccode { cv::CAP_GIGANETIX }
val CAP_MSMF: int = @ccode { cv::CAP_MSMF }
val CAP_WINRT: int = @ccode { cv::CAP_WINRT }
val CAP_INTELPERC: int = @ccode { cv::CAP_INTELPERC }
val CAP_REALSENSE: int = @ccode { cv::CAP_REALSENSE }
val CAP_OPENNI2: int = @ccode { cv::CAP_OPENNI2 }
val CAP_OPENNI2_ASUS: int = @ccode { cv::CAP_OPENNI2_ASUS }
val CAP_OPENNI2_ASTRA: int = @ccode { cv::CAP_OPENNI2_ASTRA }
val CAP_GPHOTO2: int = @ccode { cv::CAP_GPHOTO2 }
val CAP_GSTREAMER: int = @ccode { cv::CAP_GSTREAMER }
val CAP_FFMPEG: int = @ccode { cv::CAP_FFMPEG }
val CAP_IMAGES: int = @ccode { cv::CAP_IMAGES }
val CAP_ARAVIS: int = @ccode { cv::CAP_ARAVIS }
val CAP_OPENCV_MJPEG: int = @ccode { cv::CAP_OPENCV_MJPEG }
val CAP_INTEL_MFX: int = @ccode { cv::CAP_INTEL_MFX }
val CAP_XINE: int = @ccode { cv::CAP_XINE }
val CAP_UEYE: int = @ccode { cv::CAP_UEYE }

val CAP_PROP_POS_MSEC: int = @ccode { cv::CAP_PROP_POS_MSEC }
val CAP_PROP_POS_FRAMES: int = @ccode { cv::CAP_PROP_POS_FRAMES }
val CAP_PROP_POS_AVI_RATIO: int = @ccode { cv::CAP_PROP_POS_AVI_RATIO }
val CAP_PROP_FRAME_WIDTH: int = @ccode { cv::CAP_PROP_FRAME_WIDTH }
val CAP_PROP_FRAME_HEIGHT: int = @ccode { cv::CAP_PROP_FRAME_HEIGHT }
val CAP_PROP_FPS: int = @ccode { cv::CAP_PROP_FPS }
val CAP_PROP_FOURCC: int = @ccode { cv::CAP_PROP_FOURCC }
val CAP_PROP_FRAME_COUNT: int = @ccode { cv::CAP_PROP_FRAME_COUNT }
val CAP_PROP_FORMAT: int = @ccode { cv::CAP_PROP_FORMAT }
val CAP_PROP_MODE: int = @ccode { cv::CAP_PROP_MODE }
val CAP_PROP_BRIGHTNESS: int = @ccode { cv::CAP_PROP_BRIGHTNESS }
val CAP_PROP_CONTRAST: int = @ccode { cv::CAP_PROP_CONTRAST }
val CAP_PROP_SATURATION: int = @ccode { cv::CAP_PROP_SATURATION }
val CAP_PROP_HUE: int = @ccode { cv::CAP_PROP_HUE }
val CAP_PROP_GAIN: int = @ccode { cv::CAP_PROP_GAIN }
val CAP_PROP_EXPOSURE: int = @ccode { cv::CAP_PROP_EXPOSURE }
val CAP_PROP_CONVERT_RGB: int = @ccode { cv::CAP_PROP_CONVERT_RGB }
val CAP_PROP_WHITE_BALANCE_BLUE_U: int = @ccode { cv::CAP_PROP_WHITE_BALANCE_BLUE_U }
val CAP_PROP_RECTIFICATION: int = @ccode { cv::CAP_PROP_RECTIFICATION }
val CAP_PROP_MONOCHROME: int = @ccode { cv::CAP_PROP_MONOCHROME }
val CAP_PROP_SHARPNESS: int = @ccode { cv::CAP_PROP_SHARPNESS }
val CAP_PROP_AUTO_EXPOSURE: int = @ccode { cv::CAP_PROP_AUTO_EXPOSURE }
val CAP_PROP_GAMMA: int = @ccode { cv::CAP_PROP_GAMMA }
val CAP_PROP_TEMPERATURE: int = @ccode { cv::CAP_PROP_TEMPERATURE }
val CAP_PROP_TRIGGER: int = @ccode { cv::CAP_PROP_TRIGGER }
val CAP_PROP_TRIGGER_DELAY: int = @ccode { cv::CAP_PROP_TRIGGER_DELAY }
val CAP_PROP_WHITE_BALANCE_RED_V: int = @ccode { cv::CAP_PROP_WHITE_BALANCE_RED_V }
val CAP_PROP_ZOOM: int = @ccode { cv::CAP_PROP_ZOOM }
val CAP_PROP_FOCUS: int = @ccode { cv::CAP_PROP_FOCUS }
val CAP_PROP_GUID: int = @ccode { cv::CAP_PROP_GUID }
val CAP_PROP_ISO_SPEED: int = @ccode { cv::CAP_PROP_ISO_SPEED }
val CAP_PROP_BACKLIGHT: int = @ccode { cv::CAP_PROP_BACKLIGHT }
val CAP_PROP_PAN: int = @ccode { cv::CAP_PROP_PAN }
val CAP_PROP_TILT: int = @ccode { cv::CAP_PROP_TILT }
val CAP_PROP_ROLL: int = @ccode { cv::CAP_PROP_ROLL }
val CAP_PROP_IRIS: int = @ccode { cv::CAP_PROP_IRIS }
val CAP_PROP_SETTINGS: int = @ccode { cv::CAP_PROP_SETTINGS }
val CAP_PROP_BUFFERSIZE: int = @ccode { cv::CAP_PROP_BUFFERSIZE }
val CAP_PROP_AUTOFOCUS: int = @ccode { cv::CAP_PROP_AUTOFOCUS }
val CAP_PROP_SAR_NUM: int = @ccode { cv::CAP_PROP_SAR_NUM }
val CAP_PROP_SAR_DEN: int = @ccode { cv::CAP_PROP_SAR_DEN }
val CAP_PROP_BACKEND: int = @ccode { cv::CAP_PROP_BACKEND }
val CAP_PROP_CHANNEL: int = @ccode { cv::CAP_PROP_CHANNEL }
val CAP_PROP_AUTO_WB: int = @ccode { cv::CAP_PROP_AUTO_WB }
val CAP_PROP_WB_TEMPERATURE: int = @ccode { cv::CAP_PROP_WB_TEMPERATURE }
val CAP_PROP_CODEC_PIXEL_FORMAT: int = @ccode { cv::CAP_PROP_CODEC_PIXEL_FORMAT }
val CAP_PROP_BITRATE: int = @ccode { cv::CAP_PROP_BITRATE }
val CAP_PROP_ORIENTATION_META: int = @ccode { cv::CAP_PROP_ORIENTATION_META }
val CAP_PROP_ORIENTATION_AUTO: int = @ccode { cv::CAP_PROP_ORIENTATION_AUTO }
val CAP_PROP_HW_ACCELERATION: int = @ccode { cv::CAP_PROP_HW_ACCELERATION }
val CAP_PROP_HW_DEVICE: int = @ccode { cv::CAP_PROP_HW_DEVICE }
val CAP_PROP_HW_ACCELERATION_USE_OPENCL: int = @ccode { cv::CAP_PROP_HW_ACCELERATION_USE_OPENCL }

val VIDEOWRITER_PROP_QUALITY: int = @ccode { cv::VIDEOWRITER_PROP_QUALITY }
val VIDEOWRITER_PROP_FRAMEBYTES: int = @ccode { cv::VIDEOWRITER_PROP_FRAMEBYTES }
val VIDEOWRITER_PROP_NSTRIPES: int = @ccode { cv::VIDEOWRITER_PROP_NSTRIPES }
val VIDEOWRITER_PROP_IS_COLOR: int = @ccode { cv::VIDEOWRITER_PROP_IS_COLOR }
val VIDEOWRITER_PROP_DEPTH: int = @ccode { cv::VIDEOWRITER_PROP_DEPTH }
val VIDEOWRITER_PROP_HW_ACCELERATION: int = @ccode { cv::VIDEOWRITER_PROP_HW_ACCELERATION }
val VIDEOWRITER_PROP_HW_DEVICE: int = @ccode { cv::VIDEOWRITER_PROP_HW_DEVICE }
val VIDEOWRITER_PROP_HW_ACCELERATION_USE_OPENCL: int = @ccode { cv::VIDEOWRITER_PROP_HW_ACCELERATION_USE_OPENCL }

val VIDEO_ACCELERATION_NONE: int = @ccode { cv::VIDEO_ACCELERATION_NONE }
val VIDEO_ACCELERATION_ANY: int = @ccode { cv::VIDEO_ACCELERATION_ANY }
val VIDEO_ACCELERATION_D3D11: int = @ccode { cv::VIDEO_ACCELERATION_D3D11 }
val VIDEO_ACCELERATION_VAAPI: int = @ccode { cv::VIDEO_ACCELERATION_VAAPI }
val VIDEO_ACCELERATION_MFX: int = @ccode { cv::VIDEO_ACCELERATION_MFX }
val CAP_PROP_DC1394_OFF: int = @ccode { cv::CAP_PROP_DC1394_OFF }
val CAP_PROP_DC1394_MODE_MANUAL: int = @ccode { cv::CAP_PROP_DC1394_MODE_MANUAL }
val CAP_PROP_DC1394_MODE_AUTO: int = @ccode { cv::CAP_PROP_DC1394_MODE_AUTO }
val CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO: int = @ccode { cv::CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO }
val CAP_PROP_DC1394_MAX: int = @ccode { cv::CAP_PROP_DC1394_MAX }
val CAP_OPENNI_DEPTH_GENERATOR: int = @ccode { cv::CAP_OPENNI_DEPTH_GENERATOR }
val CAP_OPENNI_IMAGE_GENERATOR: int = @ccode { cv::CAP_OPENNI_IMAGE_GENERATOR }
val CAP_OPENNI_IR_GENERATOR: int = @ccode { cv::CAP_OPENNI_IR_GENERATOR }
val CAP_OPENNI_GENERATORS_MASK: int = @ccode { cv::CAP_OPENNI_GENERATORS_MASK }
val CAP_PROP_OPENNI_OUTPUT_MODE: int = @ccode { cv::CAP_PROP_OPENNI_OUTPUT_MODE }
val CAP_PROP_OPENNI_FRAME_MAX_DEPTH: int = @ccode { cv::CAP_PROP_OPENNI_FRAME_MAX_DEPTH }
val CAP_PROP_OPENNI_BASELINE: int = @ccode { cv::CAP_PROP_OPENNI_BASELINE }
val CAP_PROP_OPENNI_FOCAL_LENGTH: int = @ccode { cv::CAP_PROP_OPENNI_FOCAL_LENGTH }
val CAP_PROP_OPENNI_REGISTRATION: int = @ccode { cv::CAP_PROP_OPENNI_REGISTRATION }
val CAP_PROP_OPENNI_REGISTRATION_ON: int = @ccode { cv::CAP_PROP_OPENNI_REGISTRATION_ON }
val CAP_PROP_OPENNI_APPROX_FRAME_SYNC: int = @ccode { cv::CAP_PROP_OPENNI_APPROX_FRAME_SYNC }
val CAP_PROP_OPENNI_MAX_BUFFER_SIZE: int = @ccode { cv::CAP_PROP_OPENNI_MAX_BUFFER_SIZE }
val CAP_PROP_OPENNI_CIRCLE_BUFFER: int = @ccode { cv::CAP_PROP_OPENNI_CIRCLE_BUFFER }
val CAP_PROP_OPENNI_MAX_TIME_DURATION: int = @ccode { cv::CAP_PROP_OPENNI_MAX_TIME_DURATION }
val CAP_PROP_OPENNI_GENERATOR_PRESENT: int = @ccode { cv::CAP_PROP_OPENNI_GENERATOR_PRESENT }
val CAP_PROP_OPENNI2_SYNC: int = @ccode { cv::CAP_PROP_OPENNI2_SYNC }
val CAP_PROP_OPENNI2_MIRROR: int = @ccode { cv::CAP_PROP_OPENNI2_MIRROR }
val CAP_OPENNI_IMAGE_GENERATOR_PRESENT: int = @ccode { cv::CAP_OPENNI_IMAGE_GENERATOR_PRESENT }
val CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE: int = @ccode { cv::CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE }
val CAP_OPENNI_DEPTH_GENERATOR_PRESENT: int = @ccode { cv::CAP_OPENNI_DEPTH_GENERATOR_PRESENT }
val CAP_OPENNI_DEPTH_GENERATOR_BASELINE: int = @ccode { cv::CAP_OPENNI_DEPTH_GENERATOR_BASELINE }
val CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH: int = @ccode { cv::CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH }
val CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION: int = @ccode { cv::CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION }
val CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON: int = @ccode { cv::CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON }
val CAP_OPENNI_IR_GENERATOR_PRESENT: int = @ccode { cv::CAP_OPENNI_IR_GENERATOR_PRESENT }

val CAP_OPENNI_DEPTH_MAP: int = @ccode { cv::CAP_OPENNI_DEPTH_MAP }
val CAP_OPENNI_POINT_CLOUD_MAP: int = @ccode { cv::CAP_OPENNI_POINT_CLOUD_MAP }
val CAP_OPENNI_DISPARITY_MAP: int = @ccode { cv::CAP_OPENNI_DISPARITY_MAP }
val CAP_OPENNI_DISPARITY_MAP_32F: int = @ccode { cv::CAP_OPENNI_DISPARITY_MAP_32F }
val CAP_OPENNI_VALID_DEPTH_MASK: int = @ccode { cv::CAP_OPENNI_VALID_DEPTH_MASK }
val CAP_OPENNI_BGR_IMAGE: int = @ccode { cv::CAP_OPENNI_BGR_IMAGE }
val CAP_OPENNI_GRAY_IMAGE: int = @ccode { cv::CAP_OPENNI_GRAY_IMAGE }
val CAP_OPENNI_IR_IMAGE: int = @ccode { cv::CAP_OPENNI_IR_IMAGE }

val CAP_OPENNI_VGA_30HZ: int = @ccode { cv::CAP_OPENNI_VGA_30HZ }
val CAP_OPENNI_SXGA_15HZ: int = @ccode { cv::CAP_OPENNI_SXGA_15HZ }
val CAP_OPENNI_SXGA_30HZ: int = @ccode { cv::CAP_OPENNI_SXGA_30HZ }
val CAP_OPENNI_QVGA_30HZ: int = @ccode { cv::CAP_OPENNI_QVGA_30HZ }
val CAP_OPENNI_QVGA_60HZ: int = @ccode { cv::CAP_OPENNI_QVGA_60HZ }
val CAP_PROP_GSTREAMER_QUEUE_LENGTH: int = @ccode { cv::CAP_PROP_GSTREAMER_QUEUE_LENGTH }

val CAP_PROP_PVAPI_MULTICASTIP: int = @ccode { cv::CAP_PROP_PVAPI_MULTICASTIP }
val CAP_PROP_PVAPI_FRAMESTARTTRIGGERMODE: int = @ccode { cv::CAP_PROP_PVAPI_FRAMESTARTTRIGGERMODE }
val CAP_PROP_PVAPI_DECIMATIONHORIZONTAL: int = @ccode { cv::CAP_PROP_PVAPI_DECIMATIONHORIZONTAL }
val CAP_PROP_PVAPI_DECIMATIONVERTICAL: int = @ccode { cv::CAP_PROP_PVAPI_DECIMATIONVERTICAL }
val CAP_PROP_PVAPI_BINNINGX: int = @ccode { cv::CAP_PROP_PVAPI_BINNINGX }
val CAP_PROP_PVAPI_BINNINGY: int = @ccode { cv::CAP_PROP_PVAPI_BINNINGY }
val CAP_PROP_PVAPI_PIXELFORMAT: int = @ccode { cv::CAP_PROP_PVAPI_PIXELFORMAT }

val CAP_PVAPI_FSTRIGMODE_FREERUN: int = @ccode { cv::CAP_PVAPI_FSTRIGMODE_FREERUN }
val CAP_PVAPI_FSTRIGMODE_SYNCIN1: int = @ccode { cv::CAP_PVAPI_FSTRIGMODE_SYNCIN1 }
val CAP_PVAPI_FSTRIGMODE_SYNCIN2: int = @ccode { cv::CAP_PVAPI_FSTRIGMODE_SYNCIN2 }
val CAP_PVAPI_FSTRIGMODE_FIXEDRATE: int = @ccode { cv::CAP_PVAPI_FSTRIGMODE_FIXEDRATE }
val CAP_PVAPI_FSTRIGMODE_SOFTWARE: int = @ccode { cv::CAP_PVAPI_FSTRIGMODE_SOFTWARE }

val CAP_PVAPI_DECIMATION_OFF: int = @ccode { cv::CAP_PVAPI_DECIMATION_OFF }
val CAP_PVAPI_DECIMATION_2OUTOF4: int = @ccode { cv::CAP_PVAPI_DECIMATION_2OUTOF4 }
val CAP_PVAPI_DECIMATION_2OUTOF8: int = @ccode { cv::CAP_PVAPI_DECIMATION_2OUTOF8 }
val CAP_PVAPI_DECIMATION_2OUTOF16: int = @ccode { cv::CAP_PVAPI_DECIMATION_2OUTOF16 }

val CAP_PVAPI_PIXELFORMAT_MONO8: int = @ccode { cv::CAP_PVAPI_PIXELFORMAT_MONO8 }
val CAP_PVAPI_PIXELFORMAT_MONO16: int = @ccode { cv::CAP_PVAPI_PIXELFORMAT_MONO16 }
val CAP_PVAPI_PIXELFORMAT_BAYER8: int = @ccode { cv::CAP_PVAPI_PIXELFORMAT_BAYER8 }
val CAP_PVAPI_PIXELFORMAT_BAYER16: int = @ccode { cv::CAP_PVAPI_PIXELFORMAT_BAYER16 }
val CAP_PVAPI_PIXELFORMAT_RGB24: int = @ccode { cv::CAP_PVAPI_PIXELFORMAT_RGB24 }
val CAP_PVAPI_PIXELFORMAT_BGR24: int = @ccode { cv::CAP_PVAPI_PIXELFORMAT_BGR24 }
val CAP_PVAPI_PIXELFORMAT_RGBA32: int = @ccode { cv::CAP_PVAPI_PIXELFORMAT_RGBA32 }
val CAP_PVAPI_PIXELFORMAT_BGRA32: int = @ccode { cv::CAP_PVAPI_PIXELFORMAT_BGRA32 }

val CAP_PROP_XI_DOWNSAMPLING: int = @ccode { cv::CAP_PROP_XI_DOWNSAMPLING }
val CAP_PROP_XI_DATA_FORMAT: int = @ccode { cv::CAP_PROP_XI_DATA_FORMAT }
val CAP_PROP_XI_OFFSET_X: int = @ccode { cv::CAP_PROP_XI_OFFSET_X }
val CAP_PROP_XI_OFFSET_Y: int = @ccode { cv::CAP_PROP_XI_OFFSET_Y }
val CAP_PROP_XI_TRG_SOURCE: int = @ccode { cv::CAP_PROP_XI_TRG_SOURCE }
val CAP_PROP_XI_TRG_SOFTWARE: int = @ccode { cv::CAP_PROP_XI_TRG_SOFTWARE }
val CAP_PROP_XI_GPI_SELECTOR: int = @ccode { cv::CAP_PROP_XI_GPI_SELECTOR }
val CAP_PROP_XI_GPI_MODE: int = @ccode { cv::CAP_PROP_XI_GPI_MODE }
val CAP_PROP_XI_GPI_LEVEL: int = @ccode { cv::CAP_PROP_XI_GPI_LEVEL }
val CAP_PROP_XI_GPO_SELECTOR: int = @ccode { cv::CAP_PROP_XI_GPO_SELECTOR }
val CAP_PROP_XI_GPO_MODE: int = @ccode { cv::CAP_PROP_XI_GPO_MODE }
val CAP_PROP_XI_LED_SELECTOR: int = @ccode { cv::CAP_PROP_XI_LED_SELECTOR }
val CAP_PROP_XI_LED_MODE: int = @ccode { cv::CAP_PROP_XI_LED_MODE }
val CAP_PROP_XI_MANUAL_WB: int = @ccode { cv::CAP_PROP_XI_MANUAL_WB }
val CAP_PROP_XI_AUTO_WB: int = @ccode { cv::CAP_PROP_XI_AUTO_WB }
val CAP_PROP_XI_AEAG: int = @ccode { cv::CAP_PROP_XI_AEAG }
val CAP_PROP_XI_EXP_PRIORITY: int = @ccode { cv::CAP_PROP_XI_EXP_PRIORITY }
val CAP_PROP_XI_AE_MAX_LIMIT: int = @ccode { cv::CAP_PROP_XI_AE_MAX_LIMIT }
val CAP_PROP_XI_AG_MAX_LIMIT: int = @ccode { cv::CAP_PROP_XI_AG_MAX_LIMIT }
val CAP_PROP_XI_AEAG_LEVEL: int = @ccode { cv::CAP_PROP_XI_AEAG_LEVEL }
val CAP_PROP_XI_TIMEOUT: int = @ccode { cv::CAP_PROP_XI_TIMEOUT }
val CAP_PROP_XI_EXPOSURE: int = @ccode { cv::CAP_PROP_XI_EXPOSURE }
val CAP_PROP_XI_EXPOSURE_BURST_COUNT: int = @ccode { cv::CAP_PROP_XI_EXPOSURE_BURST_COUNT }
val CAP_PROP_XI_GAIN_SELECTOR: int = @ccode { cv::CAP_PROP_XI_GAIN_SELECTOR }
val CAP_PROP_XI_GAIN: int = @ccode { cv::CAP_PROP_XI_GAIN }
val CAP_PROP_XI_DOWNSAMPLING_TYPE: int = @ccode { cv::CAP_PROP_XI_DOWNSAMPLING_TYPE }
val CAP_PROP_XI_BINNING_SELECTOR: int = @ccode { cv::CAP_PROP_XI_BINNING_SELECTOR }
val CAP_PROP_XI_BINNING_VERTICAL: int = @ccode { cv::CAP_PROP_XI_BINNING_VERTICAL }
val CAP_PROP_XI_BINNING_HORIZONTAL: int = @ccode { cv::CAP_PROP_XI_BINNING_HORIZONTAL }
val CAP_PROP_XI_BINNING_PATTERN: int = @ccode { cv::CAP_PROP_XI_BINNING_PATTERN }
val CAP_PROP_XI_DECIMATION_SELECTOR: int = @ccode { cv::CAP_PROP_XI_DECIMATION_SELECTOR }
val CAP_PROP_XI_DECIMATION_VERTICAL: int = @ccode { cv::CAP_PROP_XI_DECIMATION_VERTICAL }
val CAP_PROP_XI_DECIMATION_HORIZONTAL: int = @ccode { cv::CAP_PROP_XI_DECIMATION_HORIZONTAL }
val CAP_PROP_XI_DECIMATION_PATTERN: int = @ccode { cv::CAP_PROP_XI_DECIMATION_PATTERN }
val CAP_PROP_XI_TEST_PATTERN_GENERATOR_SELECTOR: int = @ccode { cv::CAP_PROP_XI_TEST_PATTERN_GENERATOR_SELECTOR }
val CAP_PROP_XI_TEST_PATTERN: int = @ccode { cv::CAP_PROP_XI_TEST_PATTERN }
val CAP_PROP_XI_IMAGE_DATA_FORMAT: int = @ccode { cv::CAP_PROP_XI_IMAGE_DATA_FORMAT }
val CAP_PROP_XI_SHUTTER_TYPE: int = @ccode { cv::CAP_PROP_XI_SHUTTER_TYPE }
val CAP_PROP_XI_SENSOR_TAPS: int = @ccode { cv::CAP_PROP_XI_SENSOR_TAPS }
val CAP_PROP_XI_AEAG_ROI_OFFSET_X: int = @ccode { cv::CAP_PROP_XI_AEAG_ROI_OFFSET_X }
val CAP_PROP_XI_AEAG_ROI_OFFSET_Y: int = @ccode { cv::CAP_PROP_XI_AEAG_ROI_OFFSET_Y }
val CAP_PROP_XI_AEAG_ROI_WIDTH: int = @ccode { cv::CAP_PROP_XI_AEAG_ROI_WIDTH }
val CAP_PROP_XI_AEAG_ROI_HEIGHT: int = @ccode { cv::CAP_PROP_XI_AEAG_ROI_HEIGHT }
val CAP_PROP_XI_BPC: int = @ccode { cv::CAP_PROP_XI_BPC }
val CAP_PROP_XI_WB_KR: int = @ccode { cv::CAP_PROP_XI_WB_KR }
val CAP_PROP_XI_WB_KG: int = @ccode { cv::CAP_PROP_XI_WB_KG }
val CAP_PROP_XI_WB_KB: int = @ccode { cv::CAP_PROP_XI_WB_KB }
val CAP_PROP_XI_WIDTH: int = @ccode { cv::CAP_PROP_XI_WIDTH }
val CAP_PROP_XI_HEIGHT: int = @ccode { cv::CAP_PROP_XI_HEIGHT }
val CAP_PROP_XI_REGION_SELECTOR: int = @ccode { cv::CAP_PROP_XI_REGION_SELECTOR }
val CAP_PROP_XI_REGION_MODE: int = @ccode { cv::CAP_PROP_XI_REGION_MODE }
val CAP_PROP_XI_LIMIT_BANDWIDTH: int = @ccode { cv::CAP_PROP_XI_LIMIT_BANDWIDTH }
val CAP_PROP_XI_SENSOR_DATA_BIT_DEPTH: int = @ccode { cv::CAP_PROP_XI_SENSOR_DATA_BIT_DEPTH }
val CAP_PROP_XI_OUTPUT_DATA_BIT_DEPTH: int = @ccode { cv::CAP_PROP_XI_OUTPUT_DATA_BIT_DEPTH }
val CAP_PROP_XI_IMAGE_DATA_BIT_DEPTH: int = @ccode { cv::CAP_PROP_XI_IMAGE_DATA_BIT_DEPTH }
val CAP_PROP_XI_OUTPUT_DATA_PACKING: int = @ccode { cv::CAP_PROP_XI_OUTPUT_DATA_PACKING }
val CAP_PROP_XI_OUTPUT_DATA_PACKING_TYPE: int = @ccode { cv::CAP_PROP_XI_OUTPUT_DATA_PACKING_TYPE }
val CAP_PROP_XI_IS_COOLED: int = @ccode { cv::CAP_PROP_XI_IS_COOLED }
val CAP_PROP_XI_COOLING: int = @ccode { cv::CAP_PROP_XI_COOLING }
val CAP_PROP_XI_TARGET_TEMP: int = @ccode { cv::CAP_PROP_XI_TARGET_TEMP }
val CAP_PROP_XI_CHIP_TEMP: int = @ccode { cv::CAP_PROP_XI_CHIP_TEMP }
val CAP_PROP_XI_HOUS_TEMP: int = @ccode { cv::CAP_PROP_XI_HOUS_TEMP }
val CAP_PROP_XI_HOUS_BACK_SIDE_TEMP: int = @ccode { cv::CAP_PROP_XI_HOUS_BACK_SIDE_TEMP }
val CAP_PROP_XI_SENSOR_BOARD_TEMP: int = @ccode { cv::CAP_PROP_XI_SENSOR_BOARD_TEMP }
val CAP_PROP_XI_CMS: int = @ccode { cv::CAP_PROP_XI_CMS }
val CAP_PROP_XI_APPLY_CMS: int = @ccode { cv::CAP_PROP_XI_APPLY_CMS }
val CAP_PROP_XI_IMAGE_IS_COLOR: int = @ccode { cv::CAP_PROP_XI_IMAGE_IS_COLOR }
val CAP_PROP_XI_COLOR_FILTER_ARRAY: int = @ccode { cv::CAP_PROP_XI_COLOR_FILTER_ARRAY }
val CAP_PROP_XI_GAMMAY: int = @ccode { cv::CAP_PROP_XI_GAMMAY }
val CAP_PROP_XI_GAMMAC: int = @ccode { cv::CAP_PROP_XI_GAMMAC }
val CAP_PROP_XI_SHARPNESS: int = @ccode { cv::CAP_PROP_XI_SHARPNESS }
val CAP_PROP_XI_CC_MATRIX_00: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_00 }
val CAP_PROP_XI_CC_MATRIX_01: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_01 }
val CAP_PROP_XI_CC_MATRIX_02: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_02 }
val CAP_PROP_XI_CC_MATRIX_03: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_03 }
val CAP_PROP_XI_CC_MATRIX_10: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_10 }
val CAP_PROP_XI_CC_MATRIX_11: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_11 }
val CAP_PROP_XI_CC_MATRIX_12: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_12 }
val CAP_PROP_XI_CC_MATRIX_13: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_13 }
val CAP_PROP_XI_CC_MATRIX_20: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_20 }
val CAP_PROP_XI_CC_MATRIX_21: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_21 }
val CAP_PROP_XI_CC_MATRIX_22: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_22 }
val CAP_PROP_XI_CC_MATRIX_23: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_23 }
val CAP_PROP_XI_CC_MATRIX_30: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_30 }
val CAP_PROP_XI_CC_MATRIX_31: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_31 }
val CAP_PROP_XI_CC_MATRIX_32: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_32 }
val CAP_PROP_XI_CC_MATRIX_33: int = @ccode { cv::CAP_PROP_XI_CC_MATRIX_33 }
val CAP_PROP_XI_DEFAULT_CC_MATRIX: int = @ccode { cv::CAP_PROP_XI_DEFAULT_CC_MATRIX }
val CAP_PROP_XI_TRG_SELECTOR: int = @ccode { cv::CAP_PROP_XI_TRG_SELECTOR }
val CAP_PROP_XI_ACQ_FRAME_BURST_COUNT: int = @ccode { cv::CAP_PROP_XI_ACQ_FRAME_BURST_COUNT }
val CAP_PROP_XI_DEBOUNCE_EN: int = @ccode { cv::CAP_PROP_XI_DEBOUNCE_EN }
val CAP_PROP_XI_DEBOUNCE_T0: int = @ccode { cv::CAP_PROP_XI_DEBOUNCE_T0 }
val CAP_PROP_XI_DEBOUNCE_T1: int = @ccode { cv::CAP_PROP_XI_DEBOUNCE_T1 }
val CAP_PROP_XI_DEBOUNCE_POL: int = @ccode { cv::CAP_PROP_XI_DEBOUNCE_POL }
val CAP_PROP_XI_LENS_MODE: int = @ccode { cv::CAP_PROP_XI_LENS_MODE }
val CAP_PROP_XI_LENS_APERTURE_VALUE: int = @ccode { cv::CAP_PROP_XI_LENS_APERTURE_VALUE }
val CAP_PROP_XI_LENS_FOCUS_MOVEMENT_VALUE: int = @ccode { cv::CAP_PROP_XI_LENS_FOCUS_MOVEMENT_VALUE }
val CAP_PROP_XI_LENS_FOCUS_MOVE: int = @ccode { cv::CAP_PROP_XI_LENS_FOCUS_MOVE }
val CAP_PROP_XI_LENS_FOCUS_DISTANCE: int = @ccode { cv::CAP_PROP_XI_LENS_FOCUS_DISTANCE }
val CAP_PROP_XI_LENS_FOCAL_LENGTH: int = @ccode { cv::CAP_PROP_XI_LENS_FOCAL_LENGTH }
val CAP_PROP_XI_LENS_FEATURE_SELECTOR: int = @ccode { cv::CAP_PROP_XI_LENS_FEATURE_SELECTOR }
val CAP_PROP_XI_LENS_FEATURE: int = @ccode { cv::CAP_PROP_XI_LENS_FEATURE }
val CAP_PROP_XI_DEVICE_MODEL_ID: int = @ccode { cv::CAP_PROP_XI_DEVICE_MODEL_ID }
val CAP_PROP_XI_DEVICE_SN: int = @ccode { cv::CAP_PROP_XI_DEVICE_SN }
val CAP_PROP_XI_IMAGE_DATA_FORMAT_RGB32_ALPHA: int = @ccode { cv::CAP_PROP_XI_IMAGE_DATA_FORMAT_RGB32_ALPHA }
val CAP_PROP_XI_IMAGE_PAYLOAD_SIZE: int = @ccode { cv::CAP_PROP_XI_IMAGE_PAYLOAD_SIZE }
val CAP_PROP_XI_TRANSPORT_PIXEL_FORMAT: int = @ccode { cv::CAP_PROP_XI_TRANSPORT_PIXEL_FORMAT }
val CAP_PROP_XI_SENSOR_CLOCK_FREQ_HZ: int = @ccode { cv::CAP_PROP_XI_SENSOR_CLOCK_FREQ_HZ }
val CAP_PROP_XI_SENSOR_CLOCK_FREQ_INDEX: int = @ccode { cv::CAP_PROP_XI_SENSOR_CLOCK_FREQ_INDEX }
val CAP_PROP_XI_SENSOR_OUTPUT_CHANNEL_COUNT: int = @ccode { cv::CAP_PROP_XI_SENSOR_OUTPUT_CHANNEL_COUNT }
val CAP_PROP_XI_FRAMERATE: int = @ccode { cv::CAP_PROP_XI_FRAMERATE }
val CAP_PROP_XI_COUNTER_SELECTOR: int = @ccode { cv::CAP_PROP_XI_COUNTER_SELECTOR }
val CAP_PROP_XI_COUNTER_VALUE: int = @ccode { cv::CAP_PROP_XI_COUNTER_VALUE }
val CAP_PROP_XI_ACQ_TIMING_MODE: int = @ccode { cv::CAP_PROP_XI_ACQ_TIMING_MODE }
val CAP_PROP_XI_AVAILABLE_BANDWIDTH: int = @ccode { cv::CAP_PROP_XI_AVAILABLE_BANDWIDTH }
val CAP_PROP_XI_BUFFER_POLICY: int = @ccode { cv::CAP_PROP_XI_BUFFER_POLICY }
val CAP_PROP_XI_LUT_EN: int = @ccode { cv::CAP_PROP_XI_LUT_EN }
val CAP_PROP_XI_LUT_INDEX: int = @ccode { cv::CAP_PROP_XI_LUT_INDEX }
val CAP_PROP_XI_LUT_VALUE: int = @ccode { cv::CAP_PROP_XI_LUT_VALUE }
val CAP_PROP_XI_TRG_DELAY: int = @ccode { cv::CAP_PROP_XI_TRG_DELAY }
val CAP_PROP_XI_TS_RST_MODE: int = @ccode { cv::CAP_PROP_XI_TS_RST_MODE }
val CAP_PROP_XI_TS_RST_SOURCE: int = @ccode { cv::CAP_PROP_XI_TS_RST_SOURCE }
val CAP_PROP_XI_IS_DEVICE_EXIST: int = @ccode { cv::CAP_PROP_XI_IS_DEVICE_EXIST }
val CAP_PROP_XI_ACQ_BUFFER_SIZE: int = @ccode { cv::CAP_PROP_XI_ACQ_BUFFER_SIZE }
val CAP_PROP_XI_ACQ_BUFFER_SIZE_UNIT: int = @ccode { cv::CAP_PROP_XI_ACQ_BUFFER_SIZE_UNIT }
val CAP_PROP_XI_ACQ_TRANSPORT_BUFFER_SIZE: int = @ccode { cv::CAP_PROP_XI_ACQ_TRANSPORT_BUFFER_SIZE }
val CAP_PROP_XI_BUFFERS_QUEUE_SIZE: int = @ccode { cv::CAP_PROP_XI_BUFFERS_QUEUE_SIZE }
val CAP_PROP_XI_ACQ_TRANSPORT_BUFFER_COMMIT: int = @ccode { cv::CAP_PROP_XI_ACQ_TRANSPORT_BUFFER_COMMIT }
val CAP_PROP_XI_RECENT_FRAME: int = @ccode { cv::CAP_PROP_XI_RECENT_FRAME }
val CAP_PROP_XI_DEVICE_RESET: int = @ccode { cv::CAP_PROP_XI_DEVICE_RESET }
val CAP_PROP_XI_COLUMN_FPN_CORRECTION: int = @ccode { cv::CAP_PROP_XI_COLUMN_FPN_CORRECTION }
val CAP_PROP_XI_ROW_FPN_CORRECTION: int = @ccode { cv::CAP_PROP_XI_ROW_FPN_CORRECTION }
val CAP_PROP_XI_SENSOR_MODE: int = @ccode { cv::CAP_PROP_XI_SENSOR_MODE }
val CAP_PROP_XI_HDR: int = @ccode { cv::CAP_PROP_XI_HDR }
val CAP_PROP_XI_HDR_KNEEPOINT_COUNT: int = @ccode { cv::CAP_PROP_XI_HDR_KNEEPOINT_COUNT }
val CAP_PROP_XI_HDR_T1: int = @ccode { cv::CAP_PROP_XI_HDR_T1 }
val CAP_PROP_XI_HDR_T2: int = @ccode { cv::CAP_PROP_XI_HDR_T2 }
val CAP_PROP_XI_KNEEPOINT1: int = @ccode { cv::CAP_PROP_XI_KNEEPOINT1 }
val CAP_PROP_XI_KNEEPOINT2: int = @ccode { cv::CAP_PROP_XI_KNEEPOINT2 }
val CAP_PROP_XI_IMAGE_BLACK_LEVEL: int = @ccode { cv::CAP_PROP_XI_IMAGE_BLACK_LEVEL }
val CAP_PROP_XI_HW_REVISION: int = @ccode { cv::CAP_PROP_XI_HW_REVISION }
val CAP_PROP_XI_DEBUG_LEVEL: int = @ccode { cv::CAP_PROP_XI_DEBUG_LEVEL }
val CAP_PROP_XI_AUTO_BANDWIDTH_CALCULATION: int = @ccode { cv::CAP_PROP_XI_AUTO_BANDWIDTH_CALCULATION }
val CAP_PROP_XI_FFS_FILE_ID: int = @ccode { cv::CAP_PROP_XI_FFS_FILE_ID }
val CAP_PROP_XI_FFS_FILE_SIZE: int = @ccode { cv::CAP_PROP_XI_FFS_FILE_SIZE }
val CAP_PROP_XI_FREE_FFS_SIZE: int = @ccode { cv::CAP_PROP_XI_FREE_FFS_SIZE }
val CAP_PROP_XI_USED_FFS_SIZE: int = @ccode { cv::CAP_PROP_XI_USED_FFS_SIZE }
val CAP_PROP_XI_FFS_ACCESS_KEY: int = @ccode { cv::CAP_PROP_XI_FFS_ACCESS_KEY }
val CAP_PROP_XI_SENSOR_FEATURE_SELECTOR: int = @ccode { cv::CAP_PROP_XI_SENSOR_FEATURE_SELECTOR }
val CAP_PROP_XI_SENSOR_FEATURE_VALUE: int = @ccode { cv::CAP_PROP_XI_SENSOR_FEATURE_VALUE }

val CAP_PROP_ARAVIS_AUTOTRIGGER: int = @ccode { cv::CAP_PROP_ARAVIS_AUTOTRIGGER }
val CAP_PROP_IOS_DEVICE_FOCUS: int = @ccode { cv::CAP_PROP_IOS_DEVICE_FOCUS }
val CAP_PROP_IOS_DEVICE_EXPOSURE: int = @ccode { cv::CAP_PROP_IOS_DEVICE_EXPOSURE }
val CAP_PROP_IOS_DEVICE_FLASH: int = @ccode { cv::CAP_PROP_IOS_DEVICE_FLASH }
val CAP_PROP_IOS_DEVICE_WHITEBALANCE: int = @ccode { cv::CAP_PROP_IOS_DEVICE_WHITEBALANCE }
val CAP_PROP_IOS_DEVICE_TORCH: int = @ccode { cv::CAP_PROP_IOS_DEVICE_TORCH }

val CAP_PROP_GIGA_FRAME_OFFSET_X: int = @ccode { cv::CAP_PROP_GIGA_FRAME_OFFSET_X }
val CAP_PROP_GIGA_FRAME_OFFSET_Y: int = @ccode { cv::CAP_PROP_GIGA_FRAME_OFFSET_Y }
val CAP_PROP_GIGA_FRAME_WIDTH_MAX: int = @ccode { cv::CAP_PROP_GIGA_FRAME_WIDTH_MAX }
val CAP_PROP_GIGA_FRAME_HEIGH_MAX: int = @ccode { cv::CAP_PROP_GIGA_FRAME_HEIGH_MAX }
val CAP_PROP_GIGA_FRAME_SENS_WIDTH: int = @ccode { cv::CAP_PROP_GIGA_FRAME_SENS_WIDTH }
val CAP_PROP_GIGA_FRAME_SENS_HEIGH: int = @ccode { cv::CAP_PROP_GIGA_FRAME_SENS_HEIGH }

val CAP_PROP_INTELPERC_PROFILE_COUNT: int = @ccode { cv::CAP_PROP_INTELPERC_PROFILE_COUNT }
val CAP_PROP_INTELPERC_PROFILE_IDX: int = @ccode { cv::CAP_PROP_INTELPERC_PROFILE_IDX }
val CAP_PROP_INTELPERC_DEPTH_LOW_CONFIDENCE_VALUE: int = @ccode { cv::CAP_PROP_INTELPERC_DEPTH_LOW_CONFIDENCE_VALUE }
val CAP_PROP_INTELPERC_DEPTH_SATURATION_VALUE: int = @ccode { cv::CAP_PROP_INTELPERC_DEPTH_SATURATION_VALUE }
val CAP_PROP_INTELPERC_DEPTH_CONFIDENCE_THRESHOLD: int = @ccode { cv::CAP_PROP_INTELPERC_DEPTH_CONFIDENCE_THRESHOLD }
val CAP_PROP_INTELPERC_DEPTH_FOCAL_LENGTH_HORZ: int = @ccode { cv::CAP_PROP_INTELPERC_DEPTH_FOCAL_LENGTH_HORZ }
val CAP_PROP_INTELPERC_DEPTH_FOCAL_LENGTH_VERT: int = @ccode { cv::CAP_PROP_INTELPERC_DEPTH_FOCAL_LENGTH_VERT }

val CAP_INTELPERC_DEPTH_GENERATOR: int = @ccode { cv::CAP_INTELPERC_DEPTH_GENERATOR }
val CAP_INTELPERC_IMAGE_GENERATOR: int = @ccode { cv::CAP_INTELPERC_IMAGE_GENERATOR }
val CAP_INTELPERC_IR_GENERATOR: int = @ccode { cv::CAP_INTELPERC_IR_GENERATOR }
val CAP_INTELPERC_GENERATORS_MASK: int = @ccode { cv::CAP_INTELPERC_GENERATORS_MASK }

val CAP_INTELPERC_DEPTH_MAP: int = @ccode { cv::CAP_INTELPERC_DEPTH_MAP }
val CAP_INTELPERC_UVDEPTH_MAP: int = @ccode { cv::CAP_INTELPERC_UVDEPTH_MAP }
val CAP_INTELPERC_IR_MAP: int = @ccode { cv::CAP_INTELPERC_IR_MAP }
val CAP_INTELPERC_IMAGE: int = @ccode { cv::CAP_INTELPERC_IMAGE }

val CAP_PROP_GPHOTO2_PREVIEW: int = @ccode { cv::CAP_PROP_GPHOTO2_PREVIEW }
val CAP_PROP_GPHOTO2_WIDGET_ENUMERATE: int = @ccode { cv::CAP_PROP_GPHOTO2_WIDGET_ENUMERATE }
val CAP_PROP_GPHOTO2_RELOAD_CONFIG: int = @ccode { cv::CAP_PROP_GPHOTO2_RELOAD_CONFIG }
val CAP_PROP_GPHOTO2_RELOAD_ON_CHANGE: int = @ccode { cv::CAP_PROP_GPHOTO2_RELOAD_ON_CHANGE }
val CAP_PROP_GPHOTO2_COLLECT_MSGS: int = @ccode { cv::CAP_PROP_GPHOTO2_COLLECT_MSGS }
val CAP_PROP_GPHOTO2_FLUSH_MSGS: int = @ccode { cv::CAP_PROP_GPHOTO2_FLUSH_MSGS }
val CAP_PROP_SPEED: int = @ccode { cv::CAP_PROP_SPEED }
val CAP_PROP_APERTURE: int = @ccode { cv::CAP_PROP_APERTURE }
val CAP_PROP_EXPOSUREPROGRAM: int = @ccode { cv::CAP_PROP_EXPOSUREPROGRAM }
val CAP_PROP_VIEWFINDER: int = @ccode { cv::CAP_PROP_VIEWFINDER }

val CAP_PROP_IMAGES_BASE: int = @ccode { cv::CAP_PROP_IMAGES_BASE }
val CAP_PROP_IMAGES_LAST: int = @ccode { cv::CAP_PROP_IMAGES_LAST }

@ccode
{
static void _fx_ocv_free_cap(void* ptr) { delete (cv::VideoCapture*)ptr; }
static void _fx_ocv_free_writer(void* ptr) { delete (cv::VideoWriter*)ptr; }
}

class VideoCapture = { cap: cptr }
class VideoWriter = { writer: cptr }

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

fun VideoCapture.close(): void
@ccode {
    if (self->cap && self->cap->ptr) {
        cv::VideoCapture* cap = (cv::VideoCapture*)self->cap->ptr;
        cap->release();
    }
    return FX_OK;
}

fun VideoCapture.read(): uint8x3 [,] =
@ccode
{
    memset(fx_result, 0, sizeof(*fx_result));
    cv::VideoCapture* cap;
    cv::Mat frame;
    frame.allocator = &g_fxarrAllocator;
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

fun VideoCapture.set(propId: int, value: double): bool =
@ccode
{
    int fx_status = 0;
    if (!self->cap || !self->cap->ptr)
        return fx_ocv_err("video stream is not initialized");
    *fx_result = false;
    FX_OCV_TRY_CATCH(
        cv::VideoCapture* cap = (cv::VideoCapture*)self->cap->ptr;
        *fx_result = cap->set((int)propId, value);
    )
    return fx_status;
}

fun VideoCapture.get(propId: int): double =
@ccode
{
    int fx_status = 0;
    if (!self->cap || !self->cap->ptr)
        return fx_ocv_err("video stream is not initialized");
    *fx_result = 0.;
    FX_OCV_TRY_CATCH(
        cv::VideoCapture* cap = (cv::VideoCapture*)self->cap->ptr;
        *fx_result = cap->get((int)propId);
    )
    return fx_status;
}

fun VideoCapture.getBackendName(): string =
@ccode
{
    std::string name;
    int fx_status = 0;
    if (!self->cap || !self->cap->ptr)
        return fx_ocv_err("video stream is not initialized");
    FX_OCV_TRY_CATCH(
        cv::VideoCapture* cap = (cv::VideoCapture*)self->cap->ptr;
        name = cap->getBackendName();
    )
    if (fx_status >= 0)
        fx_status = fx_cstr2str(name.c_str(), -1, fx_result);
    return fx_status;
}

fun FOURCC(c0: char, c1: char, c2: char, c3: char): int =
    ((ord(c3)*256 + ord(c2))*256 + ord(c1))*256 + ord(c0)

fun FOURCC(s: string): int
{
    assert(s.length() == 4)
    ((ord(s[3])*256 + ord(s[2]))*256 + ord(s[1]))*256 + ord(s[0])
}

fun openVideoWriter(filename: string, ~fourcc: int, ~fps: double,
                    ~frameSize: intx2, ~isColor: bool=true,
                    ~params: int [] = [])
{
    fun openVideoWriter_(filename: string, fourcc: int, fps: double,
                         frameSize: intx2, isColor: bool, params: int []): cptr =
    @ccode {
        cv::VideoWriter* writer = 0;
        std::string c_filename;
        std::vector<int> c_params;
        int fx_status = cvt_to(filename, c_filename);
        if (fx_status >= 0)
            fx_status = cvt_to(params, c_params);
        FX_OCV_TRY_CATCH(
            writer = new cv::VideoWriter(c_filename, (int)fourcc, fps,
                cvt_size(&frameSize->t0), isColor, c_params);
            if(writer && writer->isOpened()) {
                fx_status = fx_make_cptr(cap, _fx_ocv_free_writer, fx_result);
            } else {
                delete writer;
                fx_status = fx_ocv_err("cannot create the video writer");
            }
        )
        return fx_status;
    }
    VideoWriter {
        writer=openVideoWriter_(filename, fourcc,
                fps, frameSize, isColor, params)
    }
}

fun VideoWriter.close(): void
@ccode {
    if (self->writer && self->writer->ptr) {
        cv::VideoWriter* writer = (cv::VideoWriter*)self->writer->ptr;
        writer->release();
    }
    return FX_OK;
}

@private fun VideoWriter.write_(img: anyarr_t): void
@ccode {
    cv::Mat c_img;
    int fx_status = cvt_to(img, c_img);
    if (!self->writer || !self->writer->ptr)
        return fx_ocv_err("video writer is not initialized");
    FX_OCV_TRY_CATCH(
        cv::VideoWriter* writer = (cv::VideoWriter*)self->writer->ptr;
        writer->write(c_img);
    )
    return FX_OK;
}

fun VideoWriter.write(img: 't [,]) = self.write_(anyarray(img))

//////////////////////////////// highgui ///////////////////////////////

@private fun imshow_(window: string, img: anyarr_t): void
@ccode {
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
    @nothrow fun waitKey_(delay: int): int =
    @ccode {
        return cv::waitKey((int)delay);
    }
    if delay < 0 {throw OpenCVError("delay must be non-negative")}
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
                   onchange: (int->void)?): void
@ccode {
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

//////////////////////////////// dnn ///////////////////////////////////

class Net { net: cptr }

@ccode {
static void _fx_ocv_free_net(void* ptr) { delete (cv::dnn::Net*)ptr; }
static void _fx_ocv_free_model(void* ptr) { delete (cv::dnn::Model*)ptr; }
}

val DNN_TARGET_CPU: int = 0 //@ccode {cv::dnn::DNN_TARGET_CPU}
val DNN_TARGET_OPENCL: int = 1 //@ccode {cv::dnn::DNN_TARGET_OPENCL}
val DNN_TARGET_OPENCL_FP16: int = 2 //@ccode {cv::dnn::DNN_TARGET_OPENCL_FP16}
val DNN_TARGET_MYRIAD: int = 3 //@ccode {cv::dnn::DNN_TARGET_MYRIAD}
val DNN_TARGET_VULKAN: int = 4 //@ccode {cv::dnn::DNN_TARGET_VULKAN}
val DNN_TARGET_FPGA: int = 5 //@ccode {cv::dnn::DNN_TARGET_FPGA}
val DNN_TARGET_CUDA: int = 6 //@ccode {cv::dnn::DNN_TARGET_CUDA}
val DNN_TARGET_CUDA_FP16: int = 7 //@ccode {cv::dnn::DNN_TARGET_CUDA_FP16}
val DNN_TARGET_HDDL: int = 8 //@ccode {cv::dnn::DNN_TARGET_HDDL}

val DNN_BACKEND_DEFAULT: int = 0 //@ccode {cv::dnn::DNN_BACKEND_DEFAULT}
val DNN_BACKEND_HALIDE: int = 1 //@ccode {cv::dnn::DNN_BACKEND_HALIDE}
val DNN_BACKEND_INFERENCE_ENGINE: int = 2 //@ccode {cv::dnn::DNN_BACKEND_INFERENCE_ENGINE}
val DNN_BACKEND_OPENCV: int = 3 //@ccode {cv::dnn::DNN_BACKEND_OPENCV}
val DNN_BACKEND_VKCOM: int = 4 //@ccode {cv::dnn::DNN_BACKEND_VKCOM}
val DNN_BACKEND_CUDA: int = 5 //@ccode {cv::dnn::DNN_BACKEND_CUDA}
val DNN_BACKEND_WEBNN: int = 6 //@ccode {cv::dnn::DNN_BACKEND_WEBNN}

@nothrow fun Net.empty(): bool
@ccode {
    return self->net == 0 || self->net->ptr == 0 ||
           ((cv::dnn::Net*)self->net->ptr)->empty();
}

fun Net.dump(): string
{
    fun dump_(net: cptr): string =
    @ccode {
        std::string result;
        int fx_status = FX_OK;
        FX_OCV_TRY_CATCH(
            (cv::dnn::Net*)(net->ptr)->dump(result);
            if (!result.empty())
                fx_status = fx_cstr2str(&result[0], (int_)result.size(), fx_result);
        )
        return fx_status;
    }
    if (self.empty()) {throw OpenCVError("the network is not properly initialized")}
    dump_(self.net)
}

fun Net.forward(): float [,,,]
{
    fun forward_(net: cptr): float [,,,] =
    @ccode {
        int fx_status = FX_OK;
        FX_OCV_TRY_CATCH(
            cv::Mat c_out = (cv::dnn::Net*)(net->ptr)->forward();
            fx_status = cvt_from(c_out, 4, _FX_DEPTH_FP32, 1, fx_result);
        )
        return fx_status;
    }
    if (self.empty()) {throw OpenCVError("the network is not properly initialized")}
    forward_(self.net)
}

fun Net.setPreferableBackend(backendId: int): void
{
    fun setPreferableBackend_(net: cptr, backendId: int): void
    @ccode {
        int fx_status = FX_OK;
        FX_OCV_TRY_CATCH(
            ((cv::dnn::Net*)(net->ptr))->setPreferableBackend((cv::dnn::Backend)backendId);
        )
        return fx_status;
    }
    if (self.empty()) {throw OpenCVError("the network is not properly initialized")}
    setPreferableBackend_(self.net, backendId)
}

fun Net.setPreferableTarget(targetId: int): void
{
    fun setPreferableTarget_(net: cptr, targetId: int): void =
    @ccode {
        int fx_status = FX_OK;
        FX_OCV_TRY_CATCH(
            ((cv::dnn::Net*)(net->ptr))->setPreferableTarget((cv::dnn::Target)targetId);
        )
        return fx_status;
    }
    if (self.empty()) {throw OpenCVError("the network is not properly initialized")}
    setPreferableTarget_(self.net, targetId)
}

fun Net.setInput(blob: 't [+], ~name: string="", ~scaleFactor: double=1.,
                    ~mean: doublex4=(0.,0.,0.,0.)): void
{
    fun setInput_(net: cptr, blob: anyarr_t, name: string,
                  scaleFactor: double, mean: doublex4): void
    @ccode {
        cv::Mat c_blob;
        std::string c_name;
        int fx_status = cvt_to((const _fx_anyarr_t*)blob, c_blob);
        if (fx_status >= 0)
            fx_status = cvt_to(name, c_name);
        FX_OCV_TRY_CATCH(
            ((cv::dnn::Net*)(net->ptr))->setInput(c_blob, name, scaleFactor, cvt_scalar(&mean->t0));
        )
        return fx_status;
    }
    if (self.empty()) {throw OpenCVError("the network is not properly initialized")}
    setInput_(self.net, blob, name, scaleFactor, mean)
}

fun readNet(model: string, ~config: string="", ~framework: string=""): Net
{
    fun readNet_(model: string, config: string, framework: string): cptr
    @ccode {
        std::string c_model, c_config, c_framework;
        int fx_status = cvt_to(model, c_model);
        if (fx_status >= 0)
            fx_status = cvt_to(config, c_config);
        if (fx_status >= 0)
            fx_status = cvt_to(framework, c_framework);
        FX_OCV_TRY_CATCH(
            cv::dnn::Net* net = new cv::dnn::readNet(c_model, c_config, c_framework);
            if (!net || net->empty()) {
                delete net;
                fx_status = fx_ocv_err("cannot load the network");
            }
            else
                fx_status = fx_make_cptr(net, _fx_ocv_free_net, fx_result);
        )
        return fx_status;
    }
    val net = readNet_(model, config, framework)
    if net == null {throw OpenCVError("cannot load the network")}
    Net {net=net}
}

@private fun blobFromImages_(images: anyarr_t [], scaleFactor: double, size: intx2,
                             mean: doublex4, swapRB: bool, crop: bool): float [,,,]
@ccode {
    int_ i, nimages = images->dim[0].size;
    cv::Mat c_blob;
    c_blob.allocator = &g_fxarrAllocator;
    std::vector<cv::Mat> c_images(nimages);
    int fx_status = FX_OK;
    for(i = 0; i < nimages; i++)
        if (fx_status >= 0)
            fx_status = cvt_to(FX_PTR_1D(fx_anyarr_t, images, i), c_images[i]);
    FX_OCV_TRY_CATCH(
        cv::dnn::blobFromImages(c_images, c_blob, scaleFactor, cvt_size(&size->t0),
                                cvt_scalar(&mean->t0), swapRB, crop);
        fx_status = cvt_from(c_blob, 4, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

fun blobFromImages(images: 't[,][], ~scaleFactor: double=1., ~size: intx2=(0, 0),
                    ~mean: doublex4=(0., 0., 0., 0.), ~swapRB: bool=false,
                    ~crop: bool=false): float [,,,] =
    blobFromImages_([| for i <- images {anyarray(i)} |],
                scaleFactor, size, mean, swapRB, crop)

fun blobFromImage(image: 't[,], ~scaleFactor: double=1., ~size: intx2=(0, 0),
                    ~mean: doublex4=(0., 0., 0., 0.), ~swapRB: bool=false,
                    ~crop: bool=false): float [,,,] =
    blobFromImages_([|anyarray(image)|], scaleFactor, size,
                   mean, swapRB, crop)

@ccode {
static int _fx_ocv_model_nonempty(fx_cptr_t model)
{
    if (!model || !model->ptr || ((cv::dnn::Model*)(model->ptr))->getNetwork_().empty())
        return fx_ocv_err("the model is not initialized");
    return FX_OK;
}
}

@private fun Model_assertNonEmpty(model: cptr): void
@ccode { return _fx_ocv_model_nonempty(model); }

@private fun Model_setPreferableBackend_(model: cptr, backendId: int): void
@ccode {
    int fx_status = _fx_ocv_model_nonempty(model);
    FX_OCV_TRY_CATCH(
        ((cv::dnn::Model*)(model->ptr))->setPreferableBackend((cv::dnn::Backend)backendId);
    )
    return fx_status;
}

@private fun Model_setPreferableTarget_(model: cptr, targetId: int): void
@ccode {
    int fx_status = _fx_ocv_model_nonempty(model);
    FX_OCV_TRY_CATCH(
        ((cv::dnn::Model*)(model->ptr))->setPreferableTarget((cv::dnn::Target)targetId);
    )
    return fx_status;
}

@private fun Model_setInputMean(model: cptr, mean: doublex4): void
@ccode {
    int fx_status = _fx_ocv_model_nonempty(model);
    FX_OCV_TRY_CATCH(
        ((cv::dnn::Model*)(model->ptr))->setInputMean(cvt_scalar(&mean->t0));
    )
    return fx_status;
}

@private fun Model_setInputScale(model: cptr, scale: double): void
@ccode {
    int fx_status = _fx_ocv_model_nonempty(model);
    FX_OCV_TRY_CATCH(
        ((cv::dnn::Model*)(model->ptr))->setInputScale(scale);
    )
    return fx_status;
}

@private fun Model_setInputSize(model: cptr, size: intx2): void
@ccode {
    int fx_status = _fx_ocv_model_nonempty(model);
    FX_OCV_TRY_CATCH(
        ((cv::dnn::Model*)(model->ptr))->setInputSize(cvt_size(&size->t0));
    )
    return fx_status;
}

@private fun Model_setInputSwapRB(model: cptr, swapRB: bool): void
@ccode {
    int fx_status = _fx_ocv_model_nonempty(model);
    FX_OCV_TRY_CATCH(
        ((cv::dnn::Model*)(model->ptr))->setInputSwapRB(swapRB);
    )
    return fx_status;
}

@private fun Model_setInputCrop(model: cptr, crop: bool): void
@ccode {
    int fx_status = _fx_ocv_model_nonempty(model);
    FX_OCV_TRY_CATCH(
        ((cv::dnn::Model*)(model->ptr))->setInputCrop(crop);
    )
    return fx_status;
}

class DetectionModel { model: cptr }

fun readDetectionModel(modelname: string, ~config: string=""): DetectionModel
{
    fun readModel_(modelname: string, config: string): cptr
    @ccode {
        std::string c_modelname, c_config;
        int fx_status = cvt_to(modelname, c_modelname);
        if (fx_status >= 0)
            fx_status = cvt_to(config, c_config);
        FX_OCV_TRY_CATCH(
            cv::dnn::DetectionModel* model = new cv::dnn::DetectionModel(c_modelname, c_config);
            if (!model || model->getNetwork_().empty()) {
                delete model;
                fx_status = fx_ocv_err("cannot load the model");
            }
            else
                fx_status = fx_make_cptr(model, _fx_ocv_free_model, fx_result);
        )
        return fx_status;
    }
    val model = readModel_(modelname, config)
    if model == null {throw OpenCVError("cannot load the model")}
    DetectionModel {model=model}
}

fun DetectionModel.setPreferableBackend(backendId: int) =
    Model_setPreferableBackend_(self.model, backendId)
fun DetectionModel.setPreferableTarget(targetId: int) =
    Model_setPreferableTarget_(self.model, targetId)

fun DetectionModel.setInputMean(mean: doublex4): void =
    Model_setInputMean(self.model, mean)
fun DetectionModel.setInputScale(scale: double): void =
    Model_setInputScale(self.model, scale)
fun DetectionModel.setInputSize(size: intx2): void =
    Model_setInputSize(self.model, size)
fun DetectionModel.setInputSwapRB(swapRB: bool): void =
    Model_setInputSwapRB(self.model, swapRB)
fun DetectionModel.setInputCrop(crop: bool): void =
    Model_setInputCrop(self.model, crop)

@private fun detect_(model: cptr, frame: anyarr_t, confThreshold: double, nmsThreshold: double):
    (int32 [], float [], int32x4 [])
@ccode {
    cv::Mat c_frame;
    std::vector<int> c_labels;
    std::vector<float> c_conf;
    std::vector<cv::Rect> c_boxes;
    int fx_status = _fx_ocv_model_nonempty(model);
    if (fx_status >= 0)
        fx_status = cvt_to((const _fx_anyarr_t*)frame, c_frame);
    FX_OCV_TRY_CATCH(
        ((cv::dnn::DetectionModel*)(model->ptr))->detect(c_frame,
            c_labels, c_conf, c_boxes, (float)confThreshold, (float)nmsThreshold);
        fx_status = cvt_from(cv::Mat(c_labels, false), 1, _FX_DEPTH_S32, 1, &fx_result->t0);
        if (fx_status >= 0)
            fx_status = cvt_from(cv::Mat(c_conf, false), 1, _FX_DEPTH_FP32, 1, &fx_result->t1);
        if (fx_status >= 0)
            fx_status = cvt_from(cv::Mat(c_boxes, false), 1, _FX_DEPTH_S32, 4, &fx_result->t2);
    )
    return fx_status;
}

fun DetectionModel.detect(frame: 't [,], ~confThreshold: double=0.5,
                        ~nmsThreshold: double=0.): (int32 [], float [], int32x4 [])
{
    detect_(self.model, anyarray(frame), confThreshold, nmsThreshold)
}
