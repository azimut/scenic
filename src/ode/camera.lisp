(in-package #:scenic)

(defclass physic-camera (pers)
  ((body    :reader body    :initarg :body)
   (geom    :reader geom    :initarg :geom)
   (mass    :reader mass    :initarg :mass); FIXME: Leaking?
   (space   :reader space   :initarg :space)
   (radius  :reader radius  :initarg :radius)
   (density :reader density :initarg :density))
  (:default-initargs
   :radius .5f0
   :density 1f0
   :body (%ode:body-create *world*)
   :mass (cffi:foreign-alloc '%ode:mass)
   :space (space (current-scene))))

(defmethod free ((obj physic-camera))
  (%ode:body-destroy (body obj))
  (%ode:geom-destroy (geom obj)))

(defmethod initialize-instance :after ((obj physic-camera) &key space radius density mass body pos)
  (with-slots (geom) obj
    (setf geom (%ode:create-sphere space radius))
    (cffi-c-ref:c-let ((m %ode:mass :from mass))
      (%ode:mass-set-sphere (m &) density radius)
      (%ode:body-set-mass body (m &))
      (%ode:geom-set-body geom body)))
  (setf (pos obj) pos))

(defmethod (setf pos) :after (new-value (obj physic-camera))
  (%ode:body-set-position (body obj) (x new-value) (y new-value) (z new-value)))

;; TODO: helper to swap/toggle cameras
