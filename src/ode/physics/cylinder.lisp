(in-package #:scenic)

;;; ?? NOT WORKING mass is weird

(defclass physic-cylinder (physic)
  ((density :initarg :density)
   (radius  :initarg :radius)
   (height  :initarg :height))
  (:default-initargs
   :density 1f0
   :radius .5f0
   :height 1f0))

(defmethod initialize-instance :after ((obj physic-cylinder) &key space)
  (with-slots (mass body geom density radius height) obj
    (setf geom (%ode:create-cylinder space radius height))
    (cffi-c-ref:c-let ((m %ode:mass :from mass))
      (%ode:mass-set-cylinder (m &) density 1 radius height)
      ;;(%ode:mass-set-sphere (m &) density radius)
      (%ode:body-set-mass body (m &))
      (%ode:geom-set-body geom body))))
