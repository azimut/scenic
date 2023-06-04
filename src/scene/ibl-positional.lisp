(in-package #:scenic)

(defclass scene-ibl-positional (scene-ibl) ())

(defun make-scene-ibl-positional (&rest args)
  (apply #'make-instance 'scene-ibl-positional args))

(defmethod pos ((scene scene-ibl-positional))
  (pos (capture scene)))

(defmethod (setf pos) (new-value (scene scene-ibl-positional))
  (setf (pos (capture scene)) new-value))
(defmethod (setf pos) :after (_ (scene scene-ibl-positional))
  (setf (drawp (prefilter  scene)) T
        (drawp (irradiance scene)) T))

;; NOTE: this assumes that IBL depends on the environment
;;       not only the cubemap
(defmethod handle ((e movement) (scene scene-ibl-positional))
  (setf (drawp (prefilter  scene)) T (uploadp (prefilter  scene)) T
        (drawp (irradiance scene)) T (uploadp (irradiance scene)) T
        (drawp (capture    scene)) T (uploadp (capture    scene)) T))
