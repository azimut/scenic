(in-package #:scenic)

(defclass physic-sphere (physic)
  ((radius  :initarg :radius))
  (:default-initargs
   :radius .5f0))

(defmethod initialize-instance :after ((obj physic-sphere) &key space)
  (with-slots (mass body geom density radius pos rot immovablep) obj
    (setf geom (%ode:create-sphere space radius))
    (unless immovablep
      (cffi-c-ref:c-let ((m %ode:mass :from mass))
        (%ode:mass-set-sphere (m &) density radius)
        (%ode:body-set-mass body (m &))
        (%ode:geom-set-body geom body)))
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))
