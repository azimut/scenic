(in-package #:scenic)

(defclass physic-camera (defered)
  ((hit-floor-p   :reader hit-floor-p   :initform nil)
   (floor-geom    :reader floor-geom    :initarg :floor-geom)
   (floor-contact :reader floor-contact :initarg :floor-contact)
   (running-p     :accessor running-p     :initform nil)
   (body          :reader body          :initarg :body)
   (geom          :reader geom)
   (mass          :reader mass          :initarg :mass); FIXME: Leaking?
   (space         :reader space         :initarg :space)
   (radius        :reader radius        :initarg :radius)
   (len           :reader len           :initarg :len)
   (density       :reader density       :initarg :density)
   (orot          :reader orot          :initarg :orot))
  (:default-initargs
   :body (%ode:body-create *world*)
   :orot (cffi:foreign-alloc '%ode:real :count 4)
   :mass (cffi:foreign-alloc '%ode:mass)
   :space (space (current-scene))
   :floor-geom (floor-geom (current-scene))
   :floor-contact (cffi:foreign-alloc '%ode:contact)
   :len 0.7f0
   :radius 0.3f0
   :density 1f0))

(defun make-physic-camera (&rest args)
  (apply #'make-instance 'physic-camera args))

(defmethod free ((obj physic-camera))
  (%ode:body-destroy (body obj))
  (%ode:geom-destroy (geom obj)))

(defmethod initialize-instance
    :before ((obj physic-camera) &key len radius density)
  (check-type density single-float)
  (check-type radius single-float)
  (check-type len single-float))
(defmethod initialize-instance
    :after ((obj physic-camera) &key space mass body pos len radius density)
  (%ode:mass-set-capsule mass density 3 radius len)
  (%ode:body-set-mass body mass)
  (with-slots (geom) obj
    (setf geom (%ode:create-capsule space radius len))
    (%ode:geom-set-body geom body))
  (ode-update-pos obj pos)
  ;; NOTE: I think this is uprighting the capsule. Movement is clumsy otherwise.
  (cffi-c-ref:c-with ((m %ode:matrix3 :clear t))
    (%ode:r-from-axis-and-angle (m &) 1f0 0f0 0f0 (radians 90f0))
    (%ode:body-set-rotation body (m &)))
  (%ode:body-set-max-angular-speed body 0f0))

(defmethod handle :after ((e tick) (camera physic-camera))
  (setf (slot-value camera 'hit-floor-p)
        (plusp (%ode:collide (floor-geom camera)
                             (geom camera)
                             1
                             (floor-contact camera)
                             (cffi:foreign-type-size '%ode:contact)))))

#+nil
(defmethod (setf pos) :after (new-pos (obj physic-camera))
  ;;(%ode:body-enable (body obj))
  ;;(%ode:body-set-position (body obj) (x new-pos) (y new-pos) (z new-pos))
  )

;; TODO: helper to swap/toggle cameras
