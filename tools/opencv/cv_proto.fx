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
    cptr fface;
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

fun imread(filename: string): uint8x3 [,]
fun imread(filename: string, pix0: 'pix): 'pix [,]
fun imwrite(filename: string, img: 't [,], ~params: int []=[]): void
fun imdecode(buf: uint8 []): uint8x3 [,]
fun imdecode(buf: uint8 [], pix0: 'pix): 'pix [,]
fun imencode(ext: string, img: 't [,], ~params: int []=[]): uint8 []
fun haveImageReader(filename: string): bool
fun haveImageWriter(filename: string): bool

//! @} imgcodecs

} // cv

#endif //OPENCV_IMGCODECS_HPP
