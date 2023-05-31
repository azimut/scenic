(in-package #:scenic)

(defclass camera-audio (raycast nepal::listener)
  ())

;;(al:distance-model :exponent-distance)
(defmethod (setf rot) :before (new-value (camera camera-audio))
  (when (not (q:= new-value (rot camera)))
    (setf (nepal::rot camera) new-value)))

(defmethod (setf pos) :before (new-value (camera camera-audio))
  (when (not (v3:= new-value (pos camera)))
    (setf (nepal::pos camera) new-value)
    (setf (from camera) new-value)))

(defclass camera-audio-perspective (perspective camera-audio)
  ())
(defclass camera-audio-defered (defered camera-audio)
  ())
