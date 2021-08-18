(in-package #:scenic)

(defclass lights ()
  ((dir-lights   :reader dir-lights   :documentation "list of directional lights"   :initarg :dir-lights)
   (dir-tex      :reader dir-tex      :documentation "common shadow textures array")
   (dir-sam      :reader dir-sam      :documentation "common shadow samplers array")
   (dir-ubo      :reader dir-ubo      :documentation "common directional lights ubo")
   (point-lights :reader point-lights :documentation "list of point lights"         :initarg :point-lights)
   (point-tex    :reader point-tex    :documentation "common shadow textures array")
   (point-sam    :reader point-sam    :documentation "common shadow samplers array")
   (point-ubo    :reader point-ubo    :documentation "common point lights ubo")
   (dim          :reader dim          :documentation "shadow dimensions NxN"        :initarg :dim))
  (:default-initargs
   :dim 1024
   :dir-lights ()
   :point-lights ())
  (:documentation "light resources for the scene"))

(defmethod print-object ((obj lights) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "DIM:~a D: ~a P: ~a"
            (slot-value obj 'dim)
            (length (slot-value obj 'dir-lights))
            (length (slot-value obj 'point-lights)))))

(defstruct-g (dir-light-data :layout :std-140)
  (positions  (:vec3 3) :accessor positions)
  (lightspace (:mat4 3) :accessor lightspace)
  (colors     (:vec3 3) :accessor colors)
  (size        :uint    :accessor size))

(defstruct-g (shadow-projections :layout :std-140)
  (mats (:mat4 6)))

(defstruct-g (point-light-data :layout :std-140)
  (positions   (:vec3 5) :accessor positions)
  (lightspace  (:mat4 5) :accessor lightspace); 1 world->clip matrix for the lights
  (shadowspace (shadow-projections 6) :accessor shadowspace); 6 projections matrices
  (colors      (:vec3 5) :accessor colors)
  (linear      (:float 5))
  (quadratic   (:float 5))
  (far         (:float 5)); FIXME: move to a camera?
  (size         :uint    :accessor size))

(defmethod free ((obj lights))
  (with-slots (dir-tex dir-ubo point-tex point-ubo dir-lights point-lights) obj
    (free dir-tex)
    (free dir-ubo)
    (free point-tex)
    (free point-ubo)
    (mapc #'free dir-lights)
    (mapc #'free point-lights)))

(defmethod initialize-instance :before ((obj lights) &key dim dir-lights point-lights)
  (check-type dir-lights list)
  (check-type point-lights list)
  (check-type dim (integer 256 4096)))

(defmethod initialize-instance :after ((obj lights) &key dim dir-lights point-lights)
  (with-slots (dir-tex dir-sam dir-ubo point-ubo point-tex point-sam) obj
    ;; DIRECTIONAL
    (setf dir-tex   (make-texture NIL :dimensions `(,dim ,dim) :layer-count 3 :element-type :depth-component24))
    (setf dir-sam   (sample dir-tex :wrap :clamp-to-border :minify-filter :nearest :magnify-filter :nearest))
    (setf (cepl.samplers::border-color dir-sam) (v! 1 1 1 1))
    (setf dir-ubo   (make-ubo NIL 'dir-light-data))
    (with-gpu-array-as-c-array (c (ubo-data dir-ubo))   (setf (size (aref-c c 0)) (length dir-lights)))
    (init-collection dir-lights   dir-ubo   dir-tex)
    ;; POINT
    (setf point-tex (make-texture nil :dimensions `(,dim ,dim) :element-type :depth-component24 :cubes t :layer-count 5))
    (setf point-sam (sample point-tex :wrap :clamp-to-edge :minify-filter :nearest :magnify-filter :nearest))
    (setf point-ubo (make-ubo NIL 'point-light-data))
    (with-gpu-array-as-c-array (c (ubo-data point-ubo)) (setf (size (aref-c c 0)) (length point-lights)))
    (init-collection point-lights point-ubo point-tex)))

(defun init-collection (lights ubo tex)
  "takes care of calling each individual light initialization, once we know their IDX"
  (let ((idx 0))
    (dolist (light lights)
      (init-light light idx ubo tex)
      (incf idx))))

(defgeneric init-light (obj idx ubo tex))

(defmethod draw ((obj scene) (lights lights) time)
  (dolist (i (point-lights lights)) (draw obj i time))
  (dolist (i (dir-lights   lights)) (draw obj i time)))

(defun make-lights (&rest args)
  (apply #'make-instance 'lights args))
