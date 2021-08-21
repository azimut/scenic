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
    (with-slots (dim dir-lights point-lights) obj
      (format stream "DIM:~a D: ~a P: ~a" dim (length dir-lights) (length point-lights)))))

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
    (setf dir-ubo   (make-ubo NIL 'dir-light-data))
    (setf (cepl.samplers::border-color dir-sam) (v! 1 1 1 1))
    (with-gpu-array-as-c-array (c (ubo-data dir-ubo))   (setf (size (aref-c c 0)) (length dir-lights)))
    (init-collection dir-lights   dir-ubo   dir-tex)
    ;; POINT
    (setf point-tex (make-texture nil :dimensions `(,dim ,dim) :element-type :depth-component24 :cubes t :layer-count 5))
    (setf point-sam (sample point-tex :wrap :clamp-to-edge :minify-filter :nearest :magnify-filter :nearest))
    (setf point-ubo (make-ubo NIL 'point-light-data))
    (with-gpu-array-as-c-array (c (ubo-data point-ubo)) (setf (size (aref-c c 0)) (length point-lights)))
    (init-collection point-lights point-ubo point-tex)))

(defgeneric init-light (obj ubo tex))

(defun init-collection (lights ubo tex)
  "takes care of calling each individual light initialization, once we know their IDX"
  (dolist (light lights)
    (init-light light ubo tex)))

(defmethod upload ((obj lights))
  (dolist (i (point-lights obj)) (upload i))
  (dolist (i (dir-lights   obj)) (upload i)))

(defmethod draw ((obj scene) (lights lights) time)
  ;; FIXME: hack, until I can clean a single cube in a cubemaparray, OR I implement an even system?
  (when-let* ((pointlights (point-lights lights))
              (first       (first pointlights))
              (drawp       (drawp first)))
    (when drawp
      (clear-fbo (fbo first) :d)
      (dolist (i pointlights)
        (draw obj i time))
      (setf (drawp first) NIL)))
  (dolist (i (dir-lights   lights)) (draw obj i time)))

(defun make-lights (&rest args)
  (apply #'make-instance 'lights args))
