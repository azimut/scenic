(in-package #:scenic)

(defvar *body-to-actor* (list)
  "associative list between body pointer and lisp object")

(defclass physic (untextured)
  ((body       :initarg :body :reader body :documentation "body pointer")
   (mass       :initarg :mass :reader mass :documentation "mass pointer")
   (geom       :initarg :geom :reader geom :documentation "geometry pointer") ;; RM?
   (orot       :initarg :orot              :documentation "ODE rotation pointer")
   (density    :initarg :density)
   (space      :initarg :space)
   (immovablep :initarg :immovablep :reader immovablep
               :documentation "ode immovable object (aka without body or mass) but that still interacts with other objects"))
  (:default-initargs
   :body (%ode:body-create *world*)
   :mass (cffi:foreign-alloc '%ode:mass)
   :orot (cffi:foreign-alloc '%ode:real :count 4)
   :density 1.0f0
   :space (space (current-scene))
   :immovablep NIL))

;; FIXME: mass is leaking?
(defmethod free :after ((object physic))
  (with-slots (body geom orot) object
    (%ode:body-destroy body)
    (%ode:geom-destroy geom)
    (cffi:foreign-free orot)
    (alexandria:removef *body-to-actor* body
                        :key #'car :test #'sb-sys:sap=)))

(defmethod initialize-instance :around ((obj physic) &key pos rot)
  (call-next-method)
  (ode-update-pos obj pos)
  (ode-update-rot obj rot))

(defmethod initialize-instance :after ((obj physic) &key)
  (push (list (slot-value obj 'body) obj) *body-to-actor*))

(defun pointer-to-actor (pointer)
  (serapeum:assocadr pointer *body-to-actor* :test #'sb-sys:sap=))

(defun update-ode-rot (orot qrot)
  "set the ODE rotation to the QROT rtg-math quaternion"
  (declare (type rtg-math.types:quaternion qrot))
  (cffi-c-ref:c-let ((ode-rot %ode:real :from orot))
    (setf (ode-rot 0) (x qrot))
    (setf (ode-rot 1) (y qrot))
    (setf (ode-rot 2) (z qrot))
    (setf (ode-rot 3) (w qrot))))

(defun ode-update-rot (physic q)
  "Ideally only called once at initialization"
  (declare (type rtg-math.types:quaternion q))
  (with-slots (geom orot) physic
    (update-ode-rot orot q)
    (%ode:geom-set-quaternion geom orot)))

(defun ode-update-pos (physic v)
  "Ideally only called once at initialization"
  (declare (type rtg-math.types:vec3 v))
  (%ode:geom-set-position (slot-value physic 'geom) (x v) (y v) (z v)))

(defmethod handle :around ((e tick) (obj physic))
  (when (and *world*
             (not (immovablep obj))
             (not (zerop (%ode:body-is-enabled (slot-value obj 'body)))))
    (call-next-method)))
(defmethod handle :after ((e tick) (obj physic))
  (with-slots (pos rot orot geom) obj
    (issue (current-scene) 'movement)
    (setf pos (ode-geom-get-position geom))
    (setf rot (ode-geom-get-quaternion2 orot geom))))
