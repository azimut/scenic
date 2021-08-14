(in-package #:scenic)

(defclass lights ()
  ((point-lights :reader point-lights :documentation "list of point lights"         :initarg :point-lights)
   (dir-lights   :reader dir-lights   :documentation "list of directional lights"   :initarg :dir-lights)
   (dim          :reader dim          :documentation "shadow dimensions NxN"        :initarg :dim)
   (tex          :reader tex          :documentation "common shadow textures array")
   (sam          :reader sam          :documentation "common shadow samplers array")
   (dir-ubo      :reader dir-ubo      :documentation "common directional lights ubo")
   (point-ubo    :reader point-ubo    :documentation "common point lights ubo"))
  (:default-initargs
   :dim 1024)
  (:documentation "light resources for the scene"))

(defmethod print-object ((obj lights) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "DIM:~a D: ~a P: ~a"
            (slot-value obj 'dim)
            (length (slot-value obj 'dir-lights))
            (length (slot-value obj 'point-lights)))))

(defstruct-g (dir-light-data :layout :std-140)
  (positions (:vec3 5) :accessor positions)
  (colors    (:vec3 5) :accessor colors)
  (size       :uint    :accessor size))

(defstruct-g (point-light-data :layout :std-140)
  (positions (:vec3 5) :accessor positions)
  (colors    (:vec3 5) :accessor colors)
  (linear    (:float 5))
  (quadratic (:float 5))
  (size       :uint    :accessor size))

(defmethod free ((obj lights))
  (with-slots (tex dir-ubo point-ubo dir-lights point-lights) obj
    (free tex)
    (free dir-ubo)
    (free point-ubo)
    (mapc #'free dir-lights)
    (mapc #'free point-lights)))

(defmethod initialize-instance :before ((obj lights) &key dim dir-lights point-lights)
  (check-type dir-lights list)
  (check-type point-lights list)
  (check-type dim (integer 256 4096)))

(defmethod initialize-instance :after ((obj lights) &key dim dir-lights point-lights)
  (with-slots (tex sam dir-ubo point-ubo) obj
    (setf tex       (make-texture NIL :dimensions `(,dim ,dim) :layer-count 10 :element-type :depth-component24))
    (setf sam       (sample tex :wrap :clamp-to-border :minify-filter :nearest :magnify-filter :nearest))
    (setf dir-ubo   (make-ubo NIL 'dir-light-data))
    (setf point-ubo (make-ubo NIL 'point-light-data))
    (with-gpu-array-as-c-array (c (ubo-data dir-ubo))
      (setf (size (aref-c c 0)) (length dir-lights)))
    (with-gpu-array-as-c-array (c (ubo-data point-ubo))
      (setf (size (aref-c c 0)) (length point-lights)))
    (init-collection dir-lights   dir-ubo   tex)
    (init-collection point-lights point-ubo tex)))

(defun init-collection (lights ubo tex)
  "takes care of calling each individual light initialization, once we know their IDX"
  (let ((idx 0))
    (dolist (light lights)
      (init-light light idx ubo tex)
      (incf idx))))

(defgeneric init-light (obj idx ubo tex))

(defun make-lights (&rest args)
  (apply #'make-instance 'lights args))

(defclass light ()
  ((color :initarg :color :accessor color :documentation "light color")
   (fbo :reader fbo :documentation "light camera fbo")
   (idx :reader idx :documentation "light index on texture")
   (ubo :reader ubo :documentation "reference to scene ubo with light data"))
  (:default-initargs
   :color (v! 1 1 1)
   :fs (v2! 10)
   :near  1f0
   :far 100f0)
  (:documentation "base class for all lights"))

(defmethod (setf pos) :after (val (obj light))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (positions (aref-c c 0)) (idx obj)) val)))
(defmethod (setf color) :after (val (obj light))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (colors (aref-c c 0)) (idx obj)) val)))

(defmethod init-light ((obj light) idx ubo tex)
  (setf (slot-value obj 'idx) idx)
  (setf (slot-value obj 'ubo) ubo)
  (with-gpu-array-as-c-array (c (ubo-data ubo))
    (setf (aref-c (positions (aref-c c 0)) idx) (pos   obj))
    (setf (aref-c (colors    (aref-c c 0)) idx) (color obj))))

(defclass directional (orth light)
  ()
  (:documentation "simple directional light"))

(defmethod print-object ((obj directional) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "POS: ~a" (slot-value obj 'pos))))

(defun make-directional (&rest args)
  (apply #'make-instance 'directional args))

(defmethod init-light :after ((obj directional) idx ubo tex)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer idx)))))

(defclass point (orth light)
  ((linear    :initarg :linear
              :accessor linear
              :documentation "linear factor of pointlight decay")
   (quadratic :initarg :quadratic
              :accessor quadratic
              :documentation "quadratic factor of pointlight decay"))
  (:default-initargs
   :linear 0.14
   :quadratic 0.07)
  (:documentation "simple pointlight light"))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "POS: ~a LINEAR: ~a QUADRATIC: ~a"
            (slot-value obj 'pos)
            (slot-value obj 'linear)
            (slot-value obj 'quadratic))))

(defmethod (setf linear) :after (val (obj point))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-linear (aref-c c 0)) (idx obj)) val)))
(defmethod (setf quadratic) :after (val (obj point))
  (with-gpu-array-as-c-array (c (ubo-data (ubo obj)))
    (setf (aref-c (point-light-data-quadratic (aref-c c 0)) (idx obj)) val)))

(defun make-point (&rest args)
  (apply #'make-instance 'point args))

(defmethod init-light :after ((obj directional) idx ubo tex)
  (let ((offset 5));; HACK: we offset this much to index on the scene lights
    (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref tex :layer (+ offset idx)))))))

(defmethod draw :around ((actor scene) (camera directional) time)
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
