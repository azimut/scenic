(in-package #:scenic)

(defclass physic-cone (derived)
  ((radius :initarg :radius)
   (height :initarg :height))
  (:default-initargs
   :radius 0.5f0
   :height 1.0f0))

(defmethod initialize-instance :before ((obj physic-cone) &key radius height)
  (setf (slot-value obj 'buf) (cone radius height)))



