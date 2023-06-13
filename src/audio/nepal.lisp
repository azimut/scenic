(in-package #:scenic)

(defclass camera-audio (raycast nepal::listener)
  ())

(defmethod update :before ((camera camera-audio) dt)
  (nepal::update camera dt))

;;(al:distance-model :exponent-distance)
(defmethod (setf rot) :before (new-value (camera camera-audio))
  (when (not (q:= new-value (rot camera)))
    (setf (nepal::rot camera) new-value)))

(defmethod (setf pos) :before (new-value (camera camera-audio))
  (when (not (v3:= new-value (pos camera)))
    (setf (nepal::pos camera) new-value)
    (setf (from camera) new-value)))

(defclass camera-audio-ode         (camera-audio physic-camera) ())

(defmethod update ((camera camera-audio-ode) dt)
  (human-move-ode camera 1f0)
  (human-rot .5 dt camera))

(defclass camera-audio-defered     (camera-audio defered)       ())
(defclass camera-audio-perspective (camera-audio perspective)   ())
