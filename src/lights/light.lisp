(in-package #:scenic)

(defclass light ()
  ((fbo :reader fbo :documentation "light camera fbo")
   (idx :reader idx :documentation "light index on texture")
   (ubo :reader ubo :documentation "reference to scene ubo with light data")
   (color :initarg :color :accessor color :documentation "light color")
   (drawp :initarg :drawp :accessor drawp :documentation "if TRUE would draw the scene from the light POV")
   (uploadp :initarg :uploadp :accessor uploadp :documentation "if TRUE upload the information to the GPU"))
  (:default-initargs
   :drawp T
   :uploadp T
   :color (v! 1 1 1))
  (:documentation "base class for all lights"))

(defmethod (setf pos)   :after (_ (obj light)) (setf (drawp obj) T (uploadp obj) T))
(defmethod (setf color) :after (_ (obj light)) (setf (uploadp obj) T))

(defmethod upload :around ((obj light))
  (when (uploadp obj)
    (call-next-method)
    (setf (uploadp obj) NIL)))

(defmethod init-light ((obj light) idx ubo tex)
  "ran by LIGHTS container object, when we know the IDX"
  (setf (slot-value obj 'idx) idx)
  (setf (slot-value obj 'ubo) ubo)
  (upload obj))

