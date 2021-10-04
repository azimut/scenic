(in-package #:scenic)

(defclass physic-box (physic)
  ((x :initform 1f0 :initarg :x)
   (y :initform 1f0 :initarg :y)
   (z :initform 1f0 :initarg :z)))

(defmethod initialize-instance :after ((obj physic-box) &key)
  (with-slots (mass body geom density immovablep x y z pos rot) obj
    (setf geom (%ode:create-box *space* x y z))
    (unless immovablep
      (cffi-c-ref:c-let ((m %ode:mass :from mass))
        (%ode:mass-set-box (m &) density x y z)
        (%ode:body-set-mass body (m &))
        (%ode:geom-set-body geom body)))
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))


