/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

/*
    Prototypes of OpenCV wrappers in Ficus
*/

///////////////////////////////////// core /////////////////////////////////

val ZEROS=(0.,0.,0.,0.)

fun borderInterpolate(p: int, len: int, borderType: int): int
fun copyMakeBorder(src: 't [,], ~top: int, ~bottom: int, ~left: int=0, ~right: int=0,
                   ~borderType: int, ~borderValue: double4=ZEROS)

fun PSNR(src1: 't [,], src2: 't [,], ~R: double=255): double
fun batchDistance(src1: 't [,], src2: 't [,], ~normType: int=NORM_L2,
                  ~K: int=0, ~update: int=0, ~crosscheck: bool=false): (float [,], int [])
fun reduce(src: 't [,], s0: 's, ~dim: int, ~rtype: int): 's []
type rotatecode_t = ROTATE_90_CLOCKWISE | ROTATE_180 | ROTATE_90_COUTERCLOCKWISE
fun rotate(src: 't [,], rotateCode: rotatecode_t): 't [,]

fun repeat(src: 't [,], ~ny: int, ~nx: int): 't [,]
fun checkRange(src: 't [+], ~quiet: bool=true, ~minVal: double=-DBL_MAX, ~maxVal: double=DBL_MAX): (bool, intx4)
fun patchNans(arr: 't [+], ~v: double=0)
fun gemm(src1: 't [,], src2: 't [,], src3: 't [,], ~alpha: double=1, ~beta: double=0, ~flags: int=0): 't [,]
fun mulTransposed(src1: 't [,], ~aTa: bool, ~delta: 't [,] = [], ~scale: double=1): 't [,]
fun transform(src: 't [,], m: 'k [,]): 't [,]
fun perspectiveTransform(src: 't [,], m: 'k [,]): 't [,]
fun solveCubic(coeffs: double []): double []
fun solvePoly(coeffs: double [], ~maxIters: int=300): double2 []
fun eigen(src: 't [,]): (bool, 't [], 't [,])
fun eigenNonSymmetric(src: 't [,]): ('t [], 't [,])
fun PCACompute(data: 't [,], mean: 't [], ~maxComponents: int=0): ('t [], 't [,])
fun PCAProject(data: 't [,], mean: 't [], eigenvectors: 't [,]): 't [,]
fun PCABackProject(data: 't [,], mean: 't [], eigenvectors: 't [,]): 't [,]
fun SVDecomp(src: 't [,], ~flags: int=0): ('t [,], 't [], 't [,])
fun SVDecompValues(src: 't [,], ~flags: int=0): 't []
fun SVBackSubst(u: 't [,], w: 't [], vt: 't [,], rhs: 't [,]): 't [,]
fun SVBackSubst(u: 't [,], w: 't [], vt: 't [,], rhs: 't []): 't []
fun dft(src: ('t*2) [], ~flags: int=0, ~nonzeroRows: int=0): ('t*2) []
fun dft(src: ('t*2) [,], ~flags: int=0, ~nonzeroRows: int=0): ('t*2) [,]
fun realdft(src: 't [], ~flags: int=0, ~nonzeroRows: int=0): 't []
fun realdft(src: 't [], ~flags: int=0, ~nonzeroRows: int=0): 't [,]
fun idft(src: ('t*2) [], ~flags: int=0, ~nonzeroRows: int=0): ('t*2) []
fun idft(src: ('t*2) [,], ~flags: int=0, ~nonzeroRows: int=0): ('t*2) [,]
fun ccsidft(src: 't [], ~flags: int=0, ~nonzeroRows: int=0): 't []
fun ccsidft(src: 't [], ~flags: int=0, ~nonzeroRows: int=0): 't [,]
fun dct(src: 't [], ~flags: int=0): 't []
fun idct(src: 't [], ~flags: int=0): 't []
fun mulSpectrums(a: 't [], b: 't [], ~flags: int, conjB: bool=false): 't []
fun mulSpectrums(a: 't [,], b: 't [,], ~flags: int, conjB: bool=false): 't [,]
fun getOptimalDFTSize(vecsize: int): int
fun getRNGSeed(): int
fun setRNGSeed(seed: int): void
fun randu(size: int, ~low: 't, ~high: 't): 't []
fun randu(size: (int, int), ~low: 't, ~high: 't): 't [,]
fun randn(size: int, ~mean: 't, ~stddev: 't): 't []
fun randn(size: (int, int), ~mean: 't, ~stddev: 't): 't [,]
fun randShuffle(arr: 't [], ~iterFactor: double = 1.): void
fun kmeans(data: float [,], K: int, ~flags: int, ~maxIters: int, ~epsilon: double=0,
           ~attempts: int=1, ~centers: float [,]=[], ~labels0: int []=[]): (double, int [])

//////////////////////////////////// imgproc ///////////////////////////////

type box_t =
{
    center: float2;
    size: float2;
    angle: float
}

fun getGaussianKernel(ksize: int, sigma: double): float [,]
fun getGaborKernel(ksize: intx2, ~sigma: double, ~theta: double, ~lambda: double,
                   ~gamma: double, ~psi: double = M_PI*0.5): float [,]

fun getStructuringElement(shape: int, ksize: intx2, ~anchor: intx2 = (-1, -1)): bool [,]

fun medianBlur(src: 't [,], ksize: int): 't [,]
fun GaussianBlur(src: 't [,], ksize: intx2, ~sigma: double,
                 ~sigmaY: double=0., ~borderType: int=BORDER_DEFAULT): 't [,]
fun bilateralFilter(src: 't [,], d: int, ~sigmaColor: double,
                    ~sigmaSpace: double, ~borderType: int=BORDER_DEFAULT): 't [,]
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

fun imwrite_(filename: string, img: anyarr_t, params: int []): void = @ccode
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

fun imencode(ext: string, img: 't [,], ~params: int []=[]): uint8 []
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

class VideoWriter
{
    writer: cptr;
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

    }
}

fun VideoWriter.close(): void = @ccode
@ccode {
    if (self->writer && self->writer->ptr) {
        cv::VideoWriter* writer = (cv::VideoWriter*)self->writer->ptr;
        writer->release();
    }
    return FX_OK;
}

@private fun VideoWriter.write_()

fun VideoWriter.write(img: 't [,]) = self.write_(anyarray(img))
