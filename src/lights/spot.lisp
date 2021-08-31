(in-package #:scenic)

(defclass spot (pers light)
  ((linear       :initarg :linear       :accessor linear       :documentation "linear factor of spot light decay")
   (quadratic    :initarg :quadratic    :accessor quadratic    :documentation "quadratic factor of spot light decay")
   (cutoff       :initarg :cutoff       :accessor cutoff       :documentation "cutoff factor of spot light cone")
   (outer-cutoff :initarg :outer-cutoff :accessor outer-cutoff :documentation "outer-cutoff factor of spot light cone")
   (drawp        :initarg :drawp        :accessor drawp
                 :documentation "if TRUE would draw the scene from the light POV"))
  (:default-initargs
   :drawp T
   :cutoff (radians 12.5)
   :outer-cutoff (radians 17.5)
   :linear 0.14
   :quadratic 0.07
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:documentation "simple spot light"))

(defstruct-g (spot-light-data :layout :std-140)
  (positions    (:vec3  2) :accessor positions)
  (direction    (:vec3  2))
  (lightspace   (:mat4  2) :accessor lightspace)
  (colors       (:vec3  2) :accessor colors)
  (linear       (:float 2))
  (quadratic    (:float 2))
  (cutoff       (:float 2))
  (outer-cutoff (:float 2)))

(defmethod (setf pos)          :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf cutoff)       :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf outer-cutoff) :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf linear)       :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf quadratic)    :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf fs)           :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf far)          :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf fov)          :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf near)         :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))
(defmethod (setf rot)          :after (_ (obj spot)) (setf (uploadp obj) T (drawp obj) T))

(defmethod print-object ((obj spot) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (pos near far) obj
      (format stream "(~a ~a ~a) NEAR:~a FAR:~a" (x pos) (y pos) (z pos) near far))))

(defmethod upload ((obj spot))
  (with-slots (pos rot color ubo idx linear quadratic cutoff outer-cutoff) obj
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (let ((e (aref-c c 0)))
        (setf (aref-c (lightspace e) idx) (world->clip obj))
        (setf (aref-c (positions  e) idx) pos)
        (setf (aref-c (colors     e) idx) color)
        (setf (aref-c (spot-light-data-quadratic    e) idx) quadratic)
        (setf (aref-c (spot-light-data-linear       e) idx) linear)
        (setf (aref-c (spot-light-data-cutoff       e) idx) cutoff)
        (setf (aref-c (spot-light-data-outer-cutoff e) idx) outer-cutoff)
        (setf (aref-c (spot-light-data-direction    e) idx) (q:to-direction rot))))))

(defmethod initialize-instance :after ((obj spot) &key)
  (setf (slot-value obj 'ubo) (spot-ubo *state*)))

(defmethod init-light ((obj spot) idx)
  (log4cl:log-info "IDX:~a" idx)
  (setf (slot-value obj 'idx) idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref (spot-tex *state*) :layer idx)))))

(defmethod paint (scene actor (camera spot) time)
  (with-slots (buf scale) actor
    (map-g #'simplest-3d-pipe buf
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale)))

(defmethod draw :around ((obj scene) (light spot) _)
  (when (drawp light)
    (call-next-method)
    (setf (drawp light) NIL)))

(defmethod draw ((scene scene) (light spot) time)
  ;;(with-setf (cull-face) :front)
  (let ((fbo (fbo light)))
    (with-fbo-bound (fbo :attachment-for-size :d)
      (clear-fbo fbo :d)
      (dolist (a (actors scene))
        (paint scene a light time)))))

(defun make-spot (&rest args)
  (apply #'make-instance 'spot args))

(defun spot-p (obj) (typep obj 'spot))
