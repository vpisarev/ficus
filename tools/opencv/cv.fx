/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Prototypes of OpenCV wrappers in Ficus
*/

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
type int32x4 = (int32x4)
type intx2 = (int*2)
type intx3 = (int*3)
type intx4 = (int*4)
type intx5 = (int*5)
type intx6 = (int*6)
type float2 = (float*2)
type float3 = (float*3)
type float4 = (float*4)
type float5 = (float*5)
type float6 = (float*6)
type double2 = (double*2)
type double3 = (double*3)
type double4 = (double*4)
type double5 = (double*5)
type double6 = (double*6)

type depth_t =
    | DEPTH_U8 | DEPTH_S8 | DEPTH_U16 | DEPTH_S16 | DEPTH_U32 | DEPTH_S32
    | DEPTH_U64 | DEPTH_S64 | DEPTH_FP16 | DEPTH_BF16 | DEPTH_FP32 | DEPTH_FP64

type elemtype_t = (depth_t, int)

fun arrelemtype(_: 't [+]) = elemtype(0:>'t)

fun elemtype(_: 't) = (elemdepth(0:>'t), 2)
fun elemtype(_: ('t*2)) = (elemdepth(0:>'t), 2)
fun elemtype(_: ('t*3)) = (elemdepth(0:>'t), 3)
fun elemtype(_: ('t*4)) = (elemdepth(0:>'t), 4)
fun elemtype(_: ('t*5)) = (elemdepth(0:>'t), 5)
fun elemtype(_: ('t*6)) = (elemdepth(0:>'t), 6)
fun elemtype(_: ('t*7)) = (elemdepth(0:>'t), 7)
fun elemtype(_: ('t*8)) = (elemdepth(0:>'t), 8)
fun elemtype(_: ('t*9)) = (elemdepth(0:>'t), 9)
fun elemtype(_: ('t*10)) = (elemdepth(0:>'t), 10)

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
        (dstdims != src.dims && (dstdims != 1 || src.dims != 2 || src.rows != 1))
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

static cv::Size cvt_Size(const _fx_TA2i* sz)
{
    return cv::Size((int)sz->t0, (int)sz->t1);
}

static cv::Point cvt_Point(const _fx_TA2i* sz)
{
    return cv::Point((int)sz->t0, (int)sz->t1);
}

static cv::Rect cvt_Rect(const _fx_TA4i* r)
{
    return cv::Rect((int)r->t0, (int)r->t1, (int)r->t2, (int)r->t3);
}

static cv::Scalar cvt_Scalar(const _fx_TA4d* sc)
{
    return cv::Scalar(sc->t0, sc->t1, sc->t2, sc->t3);
}

}

///////////////////////////////////// core /////////////////////////////////

val BORDER_DEFAULT: int = @ccode {cv::BORDER_DEFAULT}
val BORDER_REFLECT_101: int = @ccode {cv::BORDER_REFLECT_101}
val BORDER_REPLICATE: int = @ccode {cv::BORDER_REPLICATE}

val ZEROS=(0.,0.,0.,0.)

@pure @nothrow fun borderInterpolate(p: int, len: int, borderType: int): int = @ccode
{ return cv::borderInterpolate((int)p, (int)len, (int)borderType); }

@private fun copyMakeBorder_(src: anyarr_t, top: int, bottom: int, left: int, right: int,
                   borderType: int, borderValue: double4): uint8 [,]
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    cv::Mat c_src, c_dst;
    c_dst.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((_fx_anyarr_t*)src, c_src);
    FX_OCV_TRY_CATCH(
        cv::copyMakeBorder(c_src, c_dst, (int)top, (int)bottom, (int)left, (int)right,
                           (int)borderType, cvt_Scalar(borderValue));
        fx_status = cvt_from(c_dst, 2, src->t1.tag, src->t2, fx_result);
    )
    return fx_status;
}

fun copyMakeBorder(src: 't [,], ~top: int, ~bottom: int, ~left: int=0, ~right: int=0,
                   ~borderType: int, ~borderValue: double4=ZEROS) =
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
@private fun transform_(src: anyarr_t, m: anyarr_t): uint8 [,] =
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
    (reintrpret(transform_(anyarray(src), anyarray(m)): 't [])
fun transform(src: 't [,], m: 'k [,]): 't [,] =
    (reintrpret(transform_(anyarray(src), anyarray(m)): 't [,])

@private fun perspectiveTransform_(src: anyarr_t, m: anyarr_t): uint8 [,] =
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
    (reintrpret(perspectiveTransform_(anyarray(src), anyarray(m)): 't [])
fun perspectiveTransform(src: 't [,], m: 'k [,]): 't [,] =
    (reintrpret(perspectiveTransform_(anyarray(src), anyarray(m)): 't [,])

fun solveCubic(coeffs: double []): double [] =
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    _fx_anyarr_t coeffs_ = {*coeffs, _FX_DEPTH_FP64, 1};
    cv::Mat c_coeffs, c_roots;
    c_roots.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)&coeffs_, c_coeffs);
    FX_OCV_TRY_CATCH(
        cv::solveCubic(c_coeffs, c_roots);
        fx_status = cvt_from(c_roots, 1, _FX_DEPTH_FP64, 1, fx_result);
    )
    return fx_status;
}

fun solvePoly(coeffs: double [], ~maxIters: int=300): double2 [] =
@ccode {
    memset(fx_result, 0, sizeof(*fx_result));
    _fx_anyarr_t coeffs_ = {*coeffs, _FX_DEPTH_FP64, 1};
    cv::Mat c_coeffs, c_roots;
    c_roots.allocator = &g_fxarrAllocator;
    int fx_status = cvt_to((const _fx_anyarr_t*)&coeffs_, c_coeffs);
    FX_OCV_TRY_CATCH(
        cv::solvePoly(c_coeffs, c_roots);
        fx_status = cvt_from(c_roots, 1, _FX_DEPTH_FP64, 2, fx_result);
    )
    return fx_status;
}

@private fun eigen_(src: anyarr_t): (bool, uint8 [], uint8 [,]) =
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

@private fun eigenNonSymmetric_(src: anyarr_t): (uint8 [], uint8 [,]) =
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

@private fun PCACompute_(data: anyarr_t, mean: anyarr_t, maxComponents: int): (uint8 [], uint8 [,]) =
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

@private fun PCAProject_(data: anyarr_t, mean: anyarr_t, eigenvectors: anyarr_t): uint8 [,] =
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

@private fun PCABackProject_(data: anyarr_t, mean: anyarr_t, eigenvectors: anyarr_t): uint8 [,] =
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

@private fun SVDecomp(src: anyarr_t, flags: int): (uint8 [,], uint8 [], uint8 [,]) =
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

@private fun SVDecompValues(src: anyarr_t, ~flags: int=0): uint8 [] =
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

@private fun SVBackSubst2D_(u: anyarr_t, w: anyarr_t, vt: anyarr_t, rhs: anyarr_t): uint8 [,] =
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

@private fun SVBackSubst1D_(u: anyarr_t, w: anyarr_t, vt: anyarr_t, rhs: anyarr_t): uint8 [] =
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

@private fun dft(src: anyarr_t, flags: int): uint8 [] =
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

@private fun dft(src: anyarr_t, flags: int, nonzeroRows: int): uint8 [,] =
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

@private fun mulSpectrums1D_(a: anyarr_t, b: anyarr_t, flags: int, conjB: bool): uint8 [] =
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

fun mulSpectrums(a: 't [], b: 't [], ~flags: int, conjB: bool=false): 't [] =
    (reinterpret(mulSpectrums1D_(anyarray(a), anyarray(b), flags, conjB)) : 't [])

@private fun mulSpectrums2D_(a: anyarr_t, b: anyarr_t, flags: int, conjB: bool): uint8 [] =
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

fun mulSpectrums(a: 't [,], b: 't [,], ~flags: int, conjB: bool=false): 't [,] =
    (reinterpret(mulSpectrums2D_(anyarray(a), anyarray(b), flags, conjB)) : 't [,])

fun getOptimalDFTSize(vecsize: int): int =
@ccode {
    int fx_status = 0;
    FX_OCV_TRY_CATCH(
        *fx_result = cv::getOptimalDFTSize((int)vecsize);
    )
    return fx_status;
}

@nothrow fun getRNGSeed(): uint64 =
@ccode {
    return theRNG().state;
}

@nothrow fun setRNGSeed(seed: uint64): void =
@ccode {
    theRNG() = RNG(seed);
}

val RNG_UNIFORM : int = @ccode {cv::RNG::UNIFORM}
val RNG_NORMAL : int = @ccode {cv::RNG::NORMAL}

@private fun rand_(ndims: int, sz: (int*5), dc: (depth_t, int), param1: double, param2: double, dist: int): uint8 [,] =
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
    (reinterpret(rand_(1, (size, 0, 0, 0, 0), double(low), double(high), RNG_UNIFORM) : 't [])

fun randu(size: (int, int), ~low: 't, ~high: 't): 't [,] =
    (reinterpret(rand_(2, (size.0, size.1, 0, 0, 0), double(low), double(high), RNG_UNIFORM) : 't [,])

fun randu(size: (int, int, int), ~low: 't, ~high: 't): 't [,,] =
    (reinterpret(rand_(3, (size.0, size.1, size.2, 0, 0), double(low), double(high), RNG_UNIFORM) : 't [,,])

fun randn(size: int, ~mean: 't, ~stddev: 't): 't [] =
    (reinterpret(rand_(1, (size, 0, 0, 0, 0), double(low), double(high), RNG_NORMAL) : 't [])

fun randu(size: (int, int), ~low: 't, ~high: 't): 't [,] =
    (reinterpret(rand_(2, (size.0, size.1, 0, 0, 0), double(low), double(high), RNG_NORMAL) : 't [,])

fun randu(size: (int, int, int), ~low: 't, ~high: 't): 't [,,] =
    (reinterpret(rand_(3, (size.0, size.1, size.2, 0, 0), double(low), double(high), RNG_NORMAL) : 't [,,])

@private fun randShuffle(arr: anyarr_t, iterFactor: double): void =
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
    attempts: int, centers: anyarr_t, labels0: anyarr_t): (double, int []) =
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

fun kmeans(data: float [,], K: int, ~flags: int, ~maxIters: int, ~epsilon: double=0,
           ~attempts: int=1, ~centers: float [,]=[], ~labels0: int []=[]): (double, int []) =
    kmeans_(anyarr(data), K, flags, maxIters, epsilon, attempts,
            anyarray(centers), anyarray(labels0))

//////////////////////////////////// imgproc ///////////////////////////////

type box_t =
{
    center: float2;
    size: float2;
    angle: float
}

fun getGaussianKernel(ksize: int, sigma: double): float [] =
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
                             gamma: double, psi: double): float [,] =
@ccode {
    cv::Mat c_kernel;
    int fx_status = FX_OK;
    FX_OCV_TRY_CATCH(
        c_kernel = cv::getGaborKernel(cv::Size((int)ksize->t0, (int)ksize->t1),
                                      sigma, theta, lambda, gamma, psi, CV_32F);
        fx_status = cvt_from(c_kernel, 2, _FX_DEPTH_FP32, 1, fx_result);
    )
    return fx_status;
}

fun getGaborKernel(ksize: intx2, ~sigma: double, ~theta: double, ~lambda: double,
                   ~gamma: double, ~psi: double): float [,] =
    getGaborKernel_(ksize, sigma, theta, lambda, gamma, psi)

fun getStructuringElement_(ksize: intx2, ~sigma: double, ~theta: double, ~lambda: double,
                          ~gamma: double, ~psi: double = M_PI*0.5): uint8 [,] =
@ccode {
    cv::Mat c_kernel;
    int fx_status = FX_OK;
    FX_OCV_TRY_CATCH(
        c_kernel = cv::getStructuringElement(cv::Size((int)ksize->t0, (int)ksize->t1),

        fx_status = cvt_from(c_kernel, 2, _FX_DEPTH_U8, 1, fx_result);
    )
    return fx_status;
}

fun getStructuringElement(shape: int, ksize: intx2, ~anchor: intx2 = (-1, -1)): bool [,]

@private fun medianBlur_(src: 't [,], ksize: int): 't [,] =
@ccode {

}

fun GaussianBlur(src: 't [,], ksize: intx2, ~sigma: double,
                 ~sigmaY: double=0., ~borderType: int=BORDER_DEFAULT): 't [,] =
@ccode {

}

fun bilateralFilter(src: 't [,], d: int, ~sigmaColor: double,
                    ~sigmaSpace: double, ~borderType: int=BORDER_DEFAULT): 't [,] =

fun blur(src: 't [,], ksize: intx2, ~anchor: intx2=(-1, -1),
         ~borderType: int=BORDER_DEFAULT): 't [,]
fun filter2D(src: 't [,], kernel: 'k [,], ~anchor: intx2=(-1, -1),
             ~delta: double=0., ~borderType: int=BORDER_DEFAULT): 't [,]
fun sepFilter2D(src: 't [,], kernelX: 'k [,], kernelY: 'k [,], ~anchor: intx2=(-1, -1),
                ~delta: double=0., ~borderType: int=BORDER_DEFAULT): 't [,]
fun Sobel(src: 't [,], dx: int, dy: int, ~ksize: int=3, ~scale: double=1.,
          ~delta: double=0., ~borderType: int=BORDER_DEFAULT): float [,]
fun spatialGradient(src: uint8 [,], ~ksize: int=3,
                    ~borderType: int=BORDER_DEFAULT): (int16 [,], int16 [,])
fun Laplacian(src: 't [,], ~ksize: int = 1, ~scale: double = 1, ~delta: double = 0,
              ~borderType: int=BORDER_DEFAULT): float [,]
fun Canny(src: uint8 [,], threshold1: double, threshold2: double,
          ~ksize: int=3, ~L2gradient: bool = false): uint8 [,]
fun goodFeaturesToTrack(src: uint8 [,], maxCorners: int, ~qualityLevel: double,
                        ~minDistance: double, ~mask: uint8 [,]=[],
                        ~blockSize: int=3, ~gradientSize: int=3,
                        ~useHarrisDetector: bool=false,
                        ~k: double=0.04): float2 []
fun HoughLines(src: uint8 [,], ~rho: double, ~theta: double, ~threshold: int,
               ~srn: double=0, ~stn: double=0,
               ~min_theta: double=0, ~max_theta: double=M_PI): float2 []
fun HoughLinesP(src: uint8 [,], ~rho: double, ~theta: double, ~threshold: int,
               ~minLineLength: int=0, ~maxLineGap: int=0): int32x4 []
fun HoughCircles(src: uint8 [,], ~method: int, ~dp: double, ~minDist: double,
                 ~param1: double=100, ~param2: double=100,
                 ~minRadius: int=0, ~maxRadius: int=0): float3 []
fun erode(src: 't [,], kernel: 'k [,], ~anchor: intx2=(-1, -1),
          ~iterations: int=1, ~borderType: int = BORDER_CONSTANT): 't [,]
fun dilate(src: 't [,], kernel: 'k [,], ~anchor: intx2=(-1, -1),
          ~iterations: int=1, ~borderType: int = BORDER_CONSTANT): 't [,]
fun morphologyEx(src: 't [,], kernel: 'k [,], ~op: int,
                 ~anchor: intx2=(-1, -1),
                 ~iterations: int=1, ~borderType: int = BORDER_CONSTANT): 't [,]
fun resize(src: 't [,], dsize: intx2, ~fx: double=0, ~fy: double=0,
           ~interpolation: int = INTER_LINEAR ): 't [,]
fun warpAffine(src: 't [,], M: 'k [,], dsize: intx2,
               ~interpolation: int = INTER_LINEAR,
               ~borderType: int = BORDER_CONSTANT,
               ~borderValue: double4 = (0., 0., 0., 0.)): 't [,]
fun warpPerspective(src: 't [,], M: 'k [,], dsize: intx2,
                    ~iterpolation: int = INTER_LINEAR,
                    ~borderType: int = BORDER_CONSTANT,
                    ~borderValue: double4 = (0., 0., 0., 0.)): 't [,]
fun remap(src: 't [,], map1: 'k [,], map2: 'k [,],
          ~iterpolation: int = INTER_LINEAR,
          ~borderType: int = BORDER_CONSTANT,
          ~borderValue: double4 = (0., 0., 0., 0.)): 't [,]
fun getRotationMatrix2D(center: float2, angle: double, scale: double): float [,]
fun getAffineTransform(src: float2 [], dst: float2 []): float [,]
fun invertAffineTransform(M: float [,]): float [,]
fun getPerspectiveTransform(src: float2 [], dst: float2 []): float [,]
fun getRectSubPix(src: 't [,], patchSize: intx2, center: float2): 't [,]
fun logPolar(src: 't [,], center: float2, M: double, flags: int): 't [,]
fun linearPolar(src: 't [,], center: float2, maxRadius: double, flags: int): 't [,]
fun integral(src: 't [,], s0: 's): 's [,]
fun integral2(src: 't [,], s0: 's, sq0: 'sq): ('s [,], 'sq [,])
fun phaseCorrelate(src1: 't [,], src2: 't [,], window: 'k [,]): double3
fun createHanningWindow(winSize: intx2, type: int): float [,]
fun divSpectrums(a: 't [,], b: 't [,], flags: int, ~conj: bool=false): 't [,]
fun threshold(src: 't [,], thresh: double, maxval: double, type: int): 't [,]
fun adaptiveThreshold(src: 't [,], maxval: double,
                      ~adaptiveMethod: int, ~thresholdType: int,
                      ~blockSize: int, ~C: double): 't [,]

fun pyrDown(src: 't [,], ~dsize: (int, int)=(0, 0), ~borderType: int = BORDER_DEFAULT ): 't [,]
fun pyrUp(src: 't [,], ~dsize: (int, int)=(0, 0), ~borderType: int = BORDER_DEFAULT ): 't [,]
fun calcHist(src: 't [+], hsize: int, ~ranges: float []=[],
            ~uniform: bool=true, ~accumulate: bool=false): float []
fun calcHist(src: ('t*2) [+], hsize: intx2, ~ranges: float [][]=[],
            ~uniform: bool=true, ~accumulate: bool=false): float [,]
fun calcHist(src: ('t*3) [+], hsize: intx3, ~ranges: float [][]=[],
            ~uniform: bool=true, ~accumulate: bool=false): float [,,]
fun calcBackProject(src: 't [,], hist: float [],
                    ~channel: int=0, ~ranges: float []=[],
                    ~scale: double=1, ~uniform: bool=true): float [,]
fun calcBackProject(src: 't [,], hist: float [,],
                    ~channels: intx2=(0, 1), ~ranges: float [][]=[],
                    ~scale: double=1, ~uniform: bool=true): float [,]
fun calcBackProject(src: 't [,], hist: float [,,],
                    ~channels: intx3=(0, 1, 2), ~ranges: float [][]=[],
                    ~scale: double=1, ~uniform: bool=true): float [,]
fun compareHist(h1: float [+], h2: float [+], method: int): double
fun equalizeHist(src: uint8 [,]): uint8 [,]
fun watershed(src: uint8x3 [,], markers: int32 [,]): void
fun grabCut(src: uint8x3 [,], mask: uint8 [,], rect: intx4,
            ~bgdModel: double []=[], ~fgdModel: double []=[],
            ~iterations: int=1, ~mode: int=GC_EVAL): (double [], double [])

fun distanceTransformWithLabels(src: uint8 [,], ~distanceType: int,
                                ~maskSize: int=0, ~labelType: int=DIST_LABEL_CCOMP): (float [,], int32 [,])
fun distanceTransform(src: uint8 [,], ~distanceType: int, ~maskSize: int=0): float [,]
fun floodFill(img: 't [,], seed: intx2, newVal: double4,
              ~mask: uint8 [,]=[],
              ~loDiff: double4=(0., 0., 0., 0.),
              ~upDiff: double4=(0., 0., 0., 0.),
              ~flags: int=4): intx4
fun blendLinear(src1: 't [,], src2: 't [,], w1: float [,], w2: float [,]): 't [,]
fun cvtColor(src: ('t ...) [,], code: int): ('t*3) [,]
fun cvtColorAlpha(src: ('t ...) [,], code: int): ('t*4) [,]
fun cvtColorTwoPlane(src1: 't [,], src2: ('t, 't) [,], code: int): ('t*3) [,]
fun cvtColorTwoPlaneAlpha(src1: 't [,], src2: ('t, 't) [,], code: int): ('t*4) [,]
fun demosaic(src: 't [,], code: int): ('t*3) [,]
fun demosaicAlpha(src: 't [,], code: int): ('t*4) [,]
fun moments(src: 't [,], ~binaryImage:bool=false): (double*10)
fun HuMoments(moments: (double*10)): (double*7)
fun matchTemplate(image: 't [,], templ: 't [,], method: int, ~mask: uint8 [,]=[]): float [,]
fun connectedComponents(src: uint8 [,], connectivity: int): int32 [,]
fun connectedComponentsWithStats(src: uint8 [,], connectivity: int): (int32 [,], int32 [,], double [,])
fun findContours(src: uint8 [,], mode: int, method: int, ~offset:intx2=(0,0)): (int32x2 [], intx2 [], intx4 [])
fun approxPolyDP(curve: 't [], epsilon: double, ~closed: bool): 't []
fun arcLength(curve: 't [], ~closed: bool): double
fun boundingRect(src: 't [+]): intx4
fun contourArea(src: 't [], ~oriented: bool=false): double
fun minAreaRect(points: 't []): box_t
fun boxPoints(box: box_t): (float2*4)
fun minEnclosingCircle(points: 't []): float3
fun minEnclosingTriangle(points: 't []): ((float2*3), double)
fun matchShapes(contour1: 't [], contour2: 't [], method: int, parameter: double): double
fun convexHull(points: 't [], ~clockwise:bool=false): 't []
fun convexHullIdx(points: 't [], ~clockwise:bool=false): int []
fun isContourConvex(contour: 't []): bool
fun intersectConvexConvex(p1: 't [], p2: 't [], ~handleNested: bool=true): 't []
fun fitEllipse(points: 't []): box_t
fun fitEllipseAMS(points: 't []): box_t
fun fitEllipseDirect(points: 't []): box_t
fun fitLine(points: 't [], ~distType: int, ~param: double, ~reps: double, ~aeps: double): float []
fun pointPolygonTest(contour: 't [], pt: float2, ~measureDist: bool): double
fun rotatedRectangleIntersection(rect1: box_t, rect2: box_t): (int, float [])
fun applyColorMap(src: 't [,], ~colormap: int, ~usermap: uint8x3 []=[]): uint8x3 [,]

fun RGB(r: 't, g: 't, b: 't): double4 = (double(b), double(g), double(r), 0.)
fun RGBA(r: 't, g: 't, b: 't, a: 't): double4 = (double(b), double(g), double(r), double(a))
fun GRAY(g: 't): double4 = (double(g), double(g), double(g), 0.)

fun line(img: 't [,], pt1: intx2, pt2: intx2, ~color: doublex4,
        ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void
fun arrowedLine(img: 't [,], pt1: intx2, pt2: intx2, ~color: double4,
                ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0,
                ~tipLength: double=0.1): void
fun rectangle(img: 't [,], pt1: intx2, pt2: intx2, ~color: double4,
              ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void
fun rectangle(img: 't [,], rect: intx4, ~color: double4,
              ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void
fun circle(img: 't [,], center: intx2, radius: int, ~color: double4,
            ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void
fun ellipse(img: 't [,], center: intx2, axes: intx2, ~color: double4,
            ~angle: double=0, ~startAngle: double=0, ~endAngle: double=360,
            ~thickness: int=1, ~lineType: int=LINE_8, ~shift: int=0): void
fun ellipse(img: 't [,], box: box_t, ~color: double4,
            ~thickness: int=1, ~lineType: int=LINE_8): void
fun drawMarker(img: 't [,], pos: intx2, ~color: double4,
               ~markerType: int=MARKER_CROSS, ~markerSize: int=20,
               ~thickness: int=1, ~lineType: int=LINE_8): void
fun fillConvexPoly(img: 't [,], points: int32x2 [], ~color: double4,
                   ~lineType: int = LINE_8, ~shift: int=0): void
fun fillPoly(img: 't [,], points: int32x2 [][], ~color: double4,
             ~lineType: int = LINE_8, ~shift: int=0, ~offset: intx2=(0,0)): void
fun polylines(img: 't [,], points: int32x2 [][], ~color: double4,
             ~thickness: int=1, ~lineType: int = LINE_8, ~shift: int=0, ~offset: intx2=(0,0)): void
fun drawContours(img: 't [,], contours: (int32x2 [], intx2 [], intx4 []),
                 ~contourIdx: int, ~color: double4,
                 ~thickness: int=1, ~lineType: int=LINE_8,
                 ~maxLevel: int=1000000, ~offset: intx2=(0,0)): void
fun clipLine(imgSize: intx2, pt1: intx2, pt2: intx2): (bool, intx2, intx2)
fun ellipse2Poly(center: float2, axes: float2, ~angle: int=0,
                ~arcStart: int=0, ~arcEnd: int=360,
                ~delta: int=1): int []
class FontFace
{
    fface: cptr;
}

fun makeFontFace(nameOrPath: string): FontFace
fun FontFace.setInstance(params: int []): void
fun FontFace.getInstance(): int []
fun putText(img: 't [,], text: string, ~org: intx2, ~color: double4,
            ~fontface: FontFace, ~size: int, ~weight: int=0,
            ~flags: int=PUT_TEXT_ALIGN_LEFT, ~wrap: int2=(0,0)): void
fun getTextSize(imgSize: intx2, text: string, ~org: intx2,
                ~fontface: FontFace, ~size: int, ~weight: int=0,
                ~flags: int=PUT_TEXT_ALIGN_LEFT, ~wrap: intx2=(0,0) ): intx4

fun lineIterator(pt1: intx2, pt2: intx2, ~connectivity: int, ~leftToRight: bool=false): int32x2 []

///////////////////////////// imgcodecs ////////////////////////////

@ccode {

static int cvt_to(const fx_arr_t* params, vector<int>& cvparams)
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

fun imread_gray(filename: string): uint8 [,] = @ccode
{
    std::string c_filename;
    int fx_status = cvt_to(filename, c_filename);
    FX_OCV_TRY_CATCH(
        cv::Mat dst = cv::imread(c_filename, 0);
        fx_status = cvt_from(dst, 2, _FX_DEPTH_U8, 1, fx_result);
    )
    return fx_status;
}

@private fun imwrite_(filename: string, img: anyarr_t, params: int []): void = @ccode
{
    std::string c_filename;
    cv::Mat c_img;
    vector<int> c_params;
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

fun imdecode(buf: uint8 []): uint8x3 [,] = @ccode
{
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

fun imdecode(buf: uint8 []): uint8 [,] = @ccode
{
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

@private fun imencode(ext: string, img: anyarr_t, params: int []): uint8 [] = @ccode
{
    std::string c_filename;
    cv::Mat c_img;
    vector<int> c_params;
    vector<uchar> dstvec;
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

fun haveImageReader(filename: string): bool = @ccode
{
    std::string c_filename;
    *fx_result = false;
    int fx_status = cvt_to(filename, c_filename);
    FX_OCV_TRY_CATCH(
        *fx_result = cv::haveImageReader(c_filename);
    )
    return fx_status;
}

fun haveImageWriter(filename: string): bool = @ccode
{
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
val VIDEOWRITER_PROP_HW_ACCELERATION_USE_OPENCL = @ccode { cv::VIDEOWRITER_PROP_HW_ACCELERATION_USE_OPENCL }

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

fun VideoCapture.close(): void =
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
                    ~params: int [] = []) =
{
    fun openVideoWriter_(filename: string, fourcc: int, fps: double,
                         frameSize: intx2, isColor: bool, params: int32 []) = @ccode
    {
        cv::VideoWriter* writer = 0;
        std::string c_filename;
        std::vector<int> c_params;
        int fx_status = cvt_to(filename, c_filename);
        if (fx_status >= 0)
            fx_status = cvt_to(params, c_params);
        FX_OCV_TRY_CATCH(
            writer = new cv::VideoWriter(c_filename, (int)fourcc, fps,
                cvt_Size(frameSize), isColor, c_params);
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

fun VideoWriter.close(): void =
@ccode {
    if (self->writer && self->writer->ptr) {
        cv::VideoWriter* writer = (cv::VideoWriter*)self->writer->ptr;
        writer->release();
    }
    return FX_OK;
}

@private fun VideoWriter.write_(img: anyarr_t): void =
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
