/*
The example uses some OpenCV 5 API, so please download or clone OpenCV 5.x pre from
https://github.com/opencv/opencv/tree/5.x.

Example build instructions for Linux, Windows+WSL, macOS and other Unix-like OSes
(assuming that ficus and opencv projects reside in ~/work directory):

cd ~/work
git clone git@github.com:opencv/opencv.git
cd opencv
git checkout 5.x
mkdir ../build/ocv5
cd ../build/ocv5
# configure and build opencv 5.x
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=~/work/build/ocv5/install ~/work/opencv .
make -j8
cd ~/work/ficus
# compile objdetect example
bin/ficus -cflags "-I$HOME/work/build/ocv5/install/include/opencv5/" \
          -clibs "-L$HOME/work/build/ocv5/install/lib" examples/objdetect.fx
# make sure the dynamic library loader can locate the just built .so/.dylib files.
# on macOS use DYLD_LIBRARY_PATH instead of LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$HOME/work/build/ocv5/install/lib:$LD_LIBRARY_PATH
ldconfig # may be needed on Linux
# download opencv_face_detector* from
# https://github.com/spmallick/learnopencv/tree/master/AgeGender ...

# finally, run the example. You need a webcam to be attached to your computer.
__fxbuild__/objdetect/objdetect
*/

pragma "c++"
import OpenCV as cv, Sys, Filename
val size0 = 300

// take the model from
val path = match Sys.arguments() { path :: _ => path | _ => "." }
val modelname = Filename.concat(path, "opencv_face_detector.pbtxt")
val configname = Filename.concat(path, "opencv_face_detector_uint8.pb")
if !Filename.exists(modelname) || !Filename.exists(configname) {
   println(f"\nThe face detector model {modelname}\n\
           and/or weights {configname} cannot be located.\n\n\
           Please, download them from\n\
           https://github.com/spmallick/learnopencv/tree/master/AgeGender\n\
           and pass the directory there you put them as the command-line parameter.\n")
   throw Fail("cannot load model")
}

val net = cv.readDetectionModel(modelname, config=configname)
net.setPreferableTarget(cv.DNN_TARGET_CPU)
net.setInputMean((104., 177., 123., 0.))
net.setInputScale(1.)
net.setInputSize((size0, size0))
val cap = cv.captureFromCamera(0)
val fface = cv.makeFontFace("sans")
val fontSize = 20
while true {
   val frame = cap.read()
   if frame == [] {break}
   val (fh, fw) = size(frame)
   val (_, confidences, boxes) = net.detect(frame, confThreshold=0.5)
   for confidence <- confidences, (x, y, w, h) <- boxes {
        val x0 = x + 0
        val y0 = y + 0
        val x1 = x0 + w - 1
        val y1 = y0 + h - 1
        cv.rectangle(frame, (x0, y0), (x1, y1), cv.RGB(100, 255, 100), thickness=5)
        val label = f"face: {round(confidence*100)}%"
        val tbox = cv.getTextSize((fw, fh), label, (0, 0), fontFace=fface, size=fontSize)
        cv.rectangle(frame, (x0, y0), (x0+tbox.2, y0+tbox.3), cv.RGB(100,255,100), thickness=-1)
        cv.putText(frame, label, (x0, y0+tbox.3), cv.RGB(0, 0, 0), fontFace=fface, size=fontSize)
   }
   cv.imshow("detections", frame)
   if cv.waitKey(30) >= 0 {break}
}
