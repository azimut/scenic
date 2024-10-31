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
