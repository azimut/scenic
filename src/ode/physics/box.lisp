(in-package #:scenic)

(defclass physic-box (physic)
  ((x :initarg :x)
   (y :initarg :y)
   (z :initarg :z))
  (:default-initargs
   :x 1f0 :y 1f0 :z 1f0))

(defmethod initialize-instance :after ((obj physic-box) &key space x y z body mass density immovablep buf)
  (unless buf
    (setf (slot-value obj 'buf) (box x y z)))
  (with-slots (geom) obj
    (setf geom (%ode:create-box space x y z))
    (unless immovablep
      (%ode:mass-set-box  mass density x y z)
      (%ode:body-set-mass body mass)
      (%ode:geom-set-body geom body))))
