(in-package #:scenic)

(defclass scene-ibl (scene ibl)
  ())

(defun make-scene-ibl ()
  (make-instance 'scene-ibl))

(defmethod pos ((scene scene-ibl))
  (pos (capture scene)))

(defmethod (setf pos) (new-value (scene scene-ibl))
  (setf (pos (capture scene)) new-value))
(defmethod (setf pos) :after (_ (scene scene-ibl))
  (setf (drawp (prefilter  scene)) T
        (drawp (irradiance scene)) T))

(defmethod draw :before ((scene scene-ibl) camera dt)
  (draw scene (capture    scene) dt)
  (draw scene (prefilter  scene) dt)
  (draw scene (irradiance scene) dt))

(defmethod upload ((scene scene-ibl))
  (dolist (m (materials *state*))
    (upload m))
  (dolist (l (lights scene))
    (upload l))
  (upload (capture    scene))
  (upload (prefilter  scene))
  (upload (irradiance scene)))
