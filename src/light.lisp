(in-package #:scenic)

(defclass light ()
  ((tex   :reader  tex    :allocation :class :documentation "common shadow textures array")
   (sam   :reader  sam    :allocation :class :documentation "common shadow samplers array")
   (dim   :reader  dim    :allocation :class :documentation "shadow dimensions NxN"        :initarg :dim)
   (ubo   :reader  ubo    :allocation :class :documentation "light ubo")
   (color :initarg :color :accessor   color  :documentation "light color"))
  (:default-initargs
   :color (v! 1 1 1)
   :dim 1024)
  (:documentation "base class for lights"))

(defstruct-g (light-data :layout :std-140)
  (positions (:vec3 5))
  (rotations (:vec4 5))
  (colors    (:vec3 5)))

(defmethod initialize-instance :after ((obj light) &key dim)
  (with-slots (tex sam ubo) obj
    (unless (slot-boundp obj 'tex)
      (setf tex (make-texture NIL :dimensions `(,dim ,dim) :layer-count 5 :element-type :depth-component24)))
    (unless (slot-boundp obj 'sam)
      (setf sam (sample tex :wrap :clamp-to-border :minify-filter :nearest :magnify-filter :nearest)))
    (unless (slot-boundp obj 'ubo)
      (setf ubo (make-ubo NIL 'light-data)))))

(defclass directional (orth light)
  ((fbo :reader fbo
        :documentation "light camera fbo")
   (idx :initarg :idx
        :reader idx
        :initform (error ":idx must be specified")
        :documentation "light index on texture"))
  (:default-initargs
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:documentation "simple directional light"))

(defmethod initialize-instance :before ((obj directional) &key idx)
  (check-type idx (integer 0 4)))

(defmethod initialize-instance :after ((obj directional) &key idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref (slot-value obj 'tex) :layer idx))))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (light-data-positions (aref-c c 0)) idx) (pos   obj))
    (setf (aref-c (light-data-rotations (aref-c c 0)) idx) (rot   obj))
    (setf (aref-c (light-data-colors    (aref-c c 0)) idx) (color obj))))

(defmethod (setf pos) :after (val (obj directional))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (light-data-positions (aref-c c 0)) (idx obj)) val)))
(defmethod (setf rot) :after (val (obj directional))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (light-data-rotations (aref-c c 0)) (idx obj)) val)))
(defmethod (setf color) :after (val (obj directional))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (light-data-colors (aref-c c 0)) (idx obj)) val)))

(defun make-directional (&rest args &key (idx 0) &allow-other-keys)
  (apply #'make-instance 'directional :idx idx args))

(defmethod draw :around ((actor scene) (camera light) time)
  (let ((fbo (fbo camera)))
    (with-fbo-bound (fbo :attachment-for-size :d)
      (clear-fbo fbo :d)
      (call-next-method))))

(defmethod draw (actor (camera directional) time)
  "Simple pass to draw actor from light's POV"
  (with-slots (buf scale color) actor
    (map-g #'flat-3d-pipe buf
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale
           :color color
           :time time)))
