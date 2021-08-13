(in-package #:scenic)

(defclass lights ()
  ((collection :reader collection :documentation "list of lights"                    :initarg :collection)
   (dim        :reader dim        :documentation "shadow dimensions NxN"             :initarg :dim)
   (tex        :reader tex        :documentation "common shadow textures array")
   (sam        :reader sam        :documentation "common shadow samplers array")
   (ubo        :reader ubo        :documentation "common light ubo, with light data"))
  (:default-initargs
   :dim 1024)
  (:documentation "light resources for the scene"))

(defstruct-g (light-data :layout :std-140)
  (positions (:vec3 5))
  (rotations (:vec4 5))
  (colors    (:vec3 5))
  (size       :int))

(defmethod free ((obj lights))
  (with-slots (tex ubo collection) obj
    (free tex)
    (free ubo)
    (mapc #'free collection)))

(defmethod initialize-instance :before ((obj light) &key dim collection)
  (check-type collection list)
  (check-type dim (integer 256 4096)))

(defmethod initialize-instance :after ((obj light) &key dim collection)
  (with-slots (tex sam ubo) obj
    (setf tex (make-texture NIL :dimensions `(,dim ,dim) :layer-count 5 :element-type :depth-component24))
    (setf sam (sample tex :wrap :clamp-to-border :minify-filter :nearest :magnify-filter :nearest))
    (setf ubo (make-ubo NIL 'light-data))
    (with-gpu-array-as-c-array (c (ubo-data ubo))
      (setf (light-data-size (aref-c c 0)) (length collection)))
    (init-collection obj)))

(defun init-collection (lights)
  "takes care of calling each individual light initialization, once we know their IDX position on the collection"
  (let ((idx 0))
    (dolist (light (collection lights))
      (init-light light idx (ubo lights))
      (incf idx))))

(defgeneric init-light (obj idx ubo))

(defun make-lights (&rest args)
  (apply #'make-instance 'lights args))

(defclass directional (orth)
  ((fbo :reader fbo
        :documentation "light camera fbo")
   (idx :reader idx
        :documentation "light index on texture")
   (ubo :reader ubo
        :documentation "reference to scene ubo with light data")
   (color :initarg :color :accessor color :documentation "light color"))
  (:default-initargs
   :color (v! 1 1 1)
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:documentation "simple directional light"))

(defmethod init-light ((obj directional) idx ubo)
  (setf (slot-value obj 'idx) idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref (slot-value ubo 'tex) :layer idx))))
  (setf (slot-value obj 'ubo) ubo)
  (with-gpu-array-as-c-array (c (ubo-data (ubo ubo)))
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

(defun make-directional (&rest args)
  (apply #'make-instance 'directional args))

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
