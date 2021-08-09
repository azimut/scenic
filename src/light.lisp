(in-package #:scenic)

(defvar *number-of-lights* 5)

(defclass light ()
  ((tex  :reader  tex
         :allocation :class
         :documentation "common shadow textures array")
   (sam  :reader  sam
         :allocation :class
         :documentation "common shadow samplers array")
   (dim  :initarg :dim
         :reader     dim-changed
         :allocation    :class
         :documentation "shadow dimensions NxN"))
  (:default-initargs
   :dim 1024)
  (:documentation "base class for lights"))

(defmethod initialize-instance :after ((obj light) &key dim)
  (unless (slot-boundp obj 'tex)
    (setf (slot-value obj 'tex)  (make-texture nil :dimensions (list dim dim)
                                                   :layer-count *number-of-lights*
                                                   :element-type :depth-component24))
    (setf (slot-value obj 'sam)  (sample (slot-value obj 'tex)
                                         :wrap           :clamp-to-border
                                         :minify-filter  :nearest
                                         :magnify-filter :nearest))))

(defclass directional (orth light)
  ((fbo :reader fbo-changed
        :documentation "light camera fbo")
   (idx :initarg :idx
        :initform (error ":idx must be specified")
        :documentation "light index on texture"))
  (:default-initargs
   :fs (v2! 10)
   :near  1f0
   :far 100f0
   :rot nil)
  (:documentation "simple directional light"))

(defmethod initialize-instance :after ((obj directional) &key idx)
  (setf (slot-value obj 'fbo) (make-fbo `(:d ,(texref (slot-value obj 'tex) :layer idx)))))

(defun make-directional (&rest args &key (idx 0) &allow-other-keys)
  (apply #'make-instance 'directional :idx idx args))

(defmethod draw (actor (camera directional) time)
  "Simple pass to draw actor from light's POV"
  (with-fbo-bound ((fbo camera) :attachment-for-size :d)
    (with-slots (buf scale color) actor
      (map-g #'flat-3d-frag buf
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip (projection camera)
             :scale scale
             :color color
             :time time))))
