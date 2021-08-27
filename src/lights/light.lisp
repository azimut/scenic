(in-package #:scenic)

(defclass light ()
  ((fbo :reader fbo :documentation "light camera fbo")
   (idx :reader idx :documentation "light index on texture, for the type")
   (ubo :reader ubo :documentation "reference to scene ubo with light data")
   (color   :initarg :color   :accessor color   :documentation "light color")
   (uploadp :initarg :uploadp :accessor uploadp :documentation "if TRUE, upload the information to the GPU"))
  (:default-initargs
   :uploadp T
   :color (v! 1 1 1))
  (:documentation "base class for all lights"))

(defmethod (setf pos)   :after (_ (obj light)) (setf (uploadp obj) T))
(defmethod (setf color) :after (_ (obj light)) (setf (uploadp obj) T))

(defmethod upload :around ((light light))
  (when (uploadp light)
    (call-next-method)
    (setf (uploadp light) NIL)))

(defmethod draw :around ((actor actor) (light light) _)
  (when (shadowp actor)
    (print "shadop")
    (call-next-method)))
