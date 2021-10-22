(in-package #:scenic)

(defclass physic-newcyl (derived)
  ((radius :initarg :radius)
   (height :initarg :height))
  (:default-initargs
   :radius 0.5f0
   :height 1.0f0))

(defmethod initialize-instance :before ((obj physic-newcyl) &key radius height)
  (with-slots (buf) obj
    (setf buf (cylinder radius height))))

(defmethod initialize-instance :after ((obj physic-newcyl) &key pos rot)
  (ode-update-pos obj pos)
  (ode-update-rot obj rot))
