(in-package #:scenic)

(defclass physic-sphere (physic)
  ((radius  :initarg :radius))
  (:default-initargs
   :radius .5f0))

(defmethod initialize-instance :after ((obj physic-sphere) &key space density radius immovablep mass body buf)
  (unless buf
    (setf (slot-value obj 'buf) (sphere radius)))
  (with-slots (geom) obj
    (setf geom (%ode:create-sphere space radius))
    (unless immovablep
      (cffi-c-ref:c-let ((m %ode:mass :from mass))
        (%ode:mass-set-sphere (m &) density radius)
        (%ode:body-set-mass body (m &))
        (%ode:geom-set-body geom body)))))
