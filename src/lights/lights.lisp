(in-package #:scenic)

(defclass lights ()
  ((dim :reader dim :documentation "shadow dimensions NxN" :initarg :dim))
  (:default-initargs
   :dim 1024)
  (:documentation "light resources for the scene"))

(defclass spotlights (lights)
  ((spot-tex :reader spot-tex :documentation "common shadow textures array")
   (spot-sam :reader spot-sam :documentation "common shadow samplers array")
   (spot-ubo :reader spot-ubo :documentation "common directional lights ubo"))
  (:documentation "state global spotlights resources"))

(defclass dirlights (lights)
  ((dir-tex :reader dir-tex :documentation "common shadow textures array")
   (dir-sam :reader dir-sam :documentation "common shadow samplers array")
   (dir-ubo :reader dir-ubo :documentation "common directional lights ubo"))
  (:documentation "state global dirlights resources"))

(defclass pointlights (lights)
  ((point-tex :reader point-tex :documentation "common shadow textures array")
   (point-sam :reader point-sam :documentation "common shadow samplers array")
   (point-ubo :reader point-ubo :documentation "common point lights ubo"))
  (:documentation "state global pointlight resources"))

(defmethod free :after ((obj spotlights))
  (with-slots (spot-tex spot-ubo) obj
    (free spot-tex)
    (free spot-ubo)))

(defmethod free :after ((obj pointlights))
  (with-slots (point-tex point-ubo) obj
    (free point-tex)
    (free point-ubo)))

(defmethod free :after ((obj dirlights))
  (with-slots (dir-tex dir-ubo) obj
    (free dir-tex)
    (free dir-ubo)))

(defmethod initialize-instance :before ((obj lights) &key dim)
  (check-type dim (integer 256 4096)))

(defmethod initialize-instance :after ((obj spotlights) &key dim)
  (with-slots (spot-tex spot-sam spot-ubo) obj
    (setf spot-tex (make-texture NIL :dimensions `(,dim ,dim) :layer-count 2 :element-type :depth-component24))
    (setf spot-sam (sample spot-tex :wrap :clamp-to-border :minify-filter :nearest :magnify-filter :nearest))
    (setf spot-ubo (make-ubo NIL 'spot-light-data))
    (setf (cepl.samplers::border-color spot-sam) (v! 1 1 1 1))))

(defmethod initialize-instance :after ((obj pointlights) &key dim)
  (with-slots (point-ubo point-tex point-sam) obj
    (setf point-tex (make-texture nil :dimensions `(,dim ,dim)  :layer-count 4 :element-type :depth-component24 :cubes t))
    (setf point-sam (sample point-tex :wrap :clamp-to-edge :minify-filter :nearest :magnify-filter :nearest))
    (setf point-ubo (make-ubo NIL 'point-light-data))))

(defmethod initialize-instance :after ((obj dirlights) &key dim)
  (with-slots (dir-tex dir-sam dir-ubo) obj
    (setf dir-tex (make-texture NIL :dimensions `(,dim ,dim) :layer-count 2 :element-type :depth-component24))
    (setf dir-sam (sample dir-tex :wrap :clamp-to-border :minify-filter :nearest :magnify-filter :nearest))
    (setf dir-ubo (make-ubo NIL 'dir-light-data))
    (setf (cepl.samplers::border-color dir-sam) (v! 1 1 1 1))))

#+nil
(defun init-collection (lights ubo tex)
  "takes care of calling each individual light initialization, once we know their IDX"
  (dolist (light lights)
    (init-light light ubo tex)))

(defmethod upload ((obj lights))
  ;; (dolist (i (point-lights obj)) (upload i))
  ;; (dolist (i (dir-lights   obj)) (upload i))
  ;; (dolist (i (spot-lights   obj)) (upload i))
  )

(defmethod draw ((obj scene) (lights lights) time)
  ;; FIXME: hack, until I can clean a single cube in a cubemaparray, OR I implement an even system?
  ;; (let* ((pointlights (point-lights lights))
  ;;        (first       (first pointlights))
  ;;        (drawp       (drawp first)))
  ;;   (when drawp
  ;;     (clear-fbo (fbo first) :d)
  ;;     (dolist (i pointlights)
  ;;       (draw obj i time))
  ;;     (setf (drawp first) NIL)))
  ;; (dolist (i (dir-lights   lights)) (draw obj i time))
  ;; (dolist (i (spot-lights   lights)) (draw obj i time))
  )
