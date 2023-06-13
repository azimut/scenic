(in-package #:scenic)

(defclass physic-camera (defered)
  ((hit-floor-p   :accessor hit-floor-p :initform nil)
   (floor-geom    :reader   floor-geom)
   (floor-contact :reader   floor-contact)
   (running-p     :accessor running-p   :initform nil)
   (body          :reader body)
   (geom          :reader geom)
   (mass          :reader mass)         ; FIXME: Leaking?
   (space         :reader space)
   (radius        :reader radius        :initarg :radius)
   (len           :reader len           :initarg :len)
   (density       :reader density       :initarg :density)
   (orot          :reader orot))
  (:default-initargs
   :len 0.7f0
   :radius 0.3f0
   :density 1f0))

(defun make-physic-camera (&rest args)
  (apply #'make-instance 'physic-camera args))

(defmethod free :after ((obj physic-camera))
  (%ode:body-destroy (body obj))
  (%ode:geom-destroy (geom obj)))

(defmethod initialize-instance
    :before ((obj physic-camera) &key len radius density)
  (check-type density single-float)
  (check-type radius single-float)
  (check-type len single-float))
(defmethod initialize-instance
    :after ((obj physic-camera) &key pos len radius density)
  (with-slots (body orot mass space floor-geom floor-contact geom) obj
    (setf floor-geom (floor-geom (current-scene)))
    (setf floor-contact (cffi:foreign-alloc '%ode:contact))
    (setf body (%ode:body-create *world*))
    (setf orot (cffi:foreign-alloc '%ode:real :count 4))
    (setf mass (cffi:foreign-alloc '%ode:mass))
    (setf space (space (current-scene)))

    (%ode:mass-set-capsule mass density 3 radius len)
    (%ode:body-set-mass body mass)
    (setf geom (%ode:create-capsule space radius len))
    (%ode:geom-set-category-bits geom (getf *ode-bits* :camera))
    (%ode:geom-set-collide-bits geom (logand #xffffffff (lognot (getf *ode-bits* :ray))))
    (%ode:geom-set-body geom body)

    (ode-update-pos obj pos)
    ;; NOTE: I think this is uprighting the capsule. Movement is clumsy otherwise.
    (cffi-c-ref:c-with ((m %ode:matrix3 :clear t))
      (%ode:r-from-axis-and-angle (m &) 1f0 0f0 0f0 (radians 90f0))
      (%ode:body-set-rotation body (m &)))
    (%ode:body-set-max-angular-speed body 0f0)))

(defmethod handle :after ((e tick) (camera physic-camera))
  (setf (hit-floor-p camera)
        (plusp (%ode:collide (geom camera)
                             (floor-geom camera); or (space camera)
                             1
                             (floor-contact camera)
                             (cffi:foreign-type-size '%ode:contact)))))

(defmethod update ((camera physic-camera) dt)
  (human-move-ode camera 1f0)
  (human-rot .5 dt camera))

#+nil
(defmethod (setf pos) :after (new-pos (obj physic-camera))
  ;;(%ode:body-enable (body obj))
  ;;(%ode:body-set-position (body obj) (x new-pos) (y new-pos) (z new-pos))
  )

;; TODO: helper to swap/toggle cameras
