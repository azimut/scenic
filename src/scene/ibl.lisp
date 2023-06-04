(in-package #:scenic)

(defclass scene-ibl (scene ibl)
  ())

(defun make-scene-ibl (&rest args)
  (apply #'make-instance 'scene-ibl args))

(defmethod (setf color) :after (_ (scene scene-ibl))
  (setf (drawp (irradiance scene)) T
        (drawp (prefilter  scene)) T
        (drawp (capture    scene)) T))

(defmethod draw :before ((scene scene-ibl) (camera defered) dt)
  (draw scene (capture    scene) dt)
  (draw scene (prefilter  scene) dt)
  (draw scene (irradiance scene) dt))

(defmethod draw :before ((scene scene-ibl) (camera perspective) dt)
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
