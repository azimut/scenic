(in-package #:scenic)

(defclass physic-camera (defered)
  ((body    :reader body    :initarg :body)
   (geom    :reader geom    :initarg :geom)
   (mass    :reader mass    :initarg :mass); FIXME: Leaking?
   (space   :reader space   :initarg :space)
   (radius  :reader radius  :initarg :radius)
   (len     :reader len     :initarg :len)
   (density :reader density :initarg :density)
   (orot    :reader orot    :initarg :orot))
  (:default-initargs
   :orot (cffi:foreign-alloc '%ode:real :count 4)
   :len .7f0
   :radius 0.3f0
   :density 50f0
   :body (%ode:body-create *world*)
   :mass (cffi:foreign-alloc '%ode:mass)
   :space (space (current-scene))))

(defun make-physic-camera (&rest args)
  (apply #'make-instance 'physic-camera args))

(defmethod free ((obj physic-camera))
  (%ode:body-destroy (body obj))
  (%ode:geom-destroy (geom obj)))

(defmethod initialize-instance :after ((obj physic-camera) &key space radius density mass body pos len)
  (%ode:body-set-max-angular-speed body 0f0)
  (%ode:mass-set-capsule mass density 3 radius len)
  (%ode:body-set-mass body mass)
  (with-slots (geom) obj
    (setf geom (%ode:create-capsule space radius len))
    (%ode:geom-set-body geom body))
  (ode-update-pos obj pos))

(defmethod (setf pos) :after (new-pos (obj physic-camera))
  ;;(%ode:body-enable (body obj))
  ;;(%ode:body-set-position (body obj) (x new-pos) (y new-pos) (z new-pos))
  )

;; TODO: helper to swap/toggle cameras
