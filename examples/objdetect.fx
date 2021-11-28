pragma "c++"
import OpenCV as cv
val size0 = 300
// take the model from https://github.com/spmallick/learnopencv/tree/master/AgeGender
val net = cv.readDetectionModel("opencv_face_detector.pbtxt", config="opencv_face_detector_uint8.pb")
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
