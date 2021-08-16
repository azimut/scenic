(in-package #:scenic)

(defclass light ()
  ((color :initarg :color :accessor color :documentation "light color")
   (fbo :reader fbo :documentation "light camera fbo")
   (idx :reader idx :documentation "light index on texture")
   (ubo :reader ubo :documentation "reference to scene ubo with light data"))
  (:default-initargs
   :color (v! 1 1 1)
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:documentation "base class for all lights"))

(defun upload-transform (light)
  "uploads to the UBO the light matrix 4x4"
  (with-slots (ubo idx) light
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (setf (aref-c (lightspace (aref-c c 0)) idx)
            (world->clip light)))))

(defmethod (setf pos) :after (val (obj light))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (positions (aref-c c 0)) (idx obj)) val)))
(defmethod (setf color) :after (val (obj light))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (colors (aref-c c 0)) (idx obj)) val)))

(defmethod init-light ((obj light) idx ubo tex)
  (setf (slot-value obj 'idx) idx)
  (setf (slot-value obj 'ubo) ubo)
  (with-gpu-array-as-c-array (c (ubo-data ubo))
    (setf (aref-c (positions (aref-c c 0)) idx) (pos   obj))
    (setf (aref-c (colors    (aref-c c 0)) idx) (color obj))))

(defmethod draw ((obj scene) (light light) time)
  (let ((fbo (fbo light)))
    (with-setf (cull-face) :front
      (with-fbo-bound (fbo :attachment-for-size :d)
        (clear-fbo fbo :d)
        (dolist (a (actors obj))
          (draw a light time))))))

