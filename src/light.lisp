(in-package #:scenic)

(defvar *number-of-lights* 5)

(defclass light ()
  ((tex :reader tex
        :allocation :class
        :documentation "common shadow textures")
   (sam :reader sam
        :allocation :class
        :documentation "common shadow samplers")
   (dim :initarg :dim
        :reader dim-changed
        :allocation :class
        :documentation "shadow dimensions NxN"))
  (:default-initargs
   :dim 1024)
  (:documentation "base class for lights"))

(defmethod initialize-instance :after ((obj light) &key dim)
  (unless (slot-boundp obj 'tex)
    (setf (slot-value obj 'tex)
          (make-texture nil :dimensions (list dim dim)
                            :layer-count *number-of-lights*
                            :element-type :r8)))
  (unless (slot-boundp obj 'sam)
    (setf (slot-value obj 'sam) (sample (slot-value obj 'tex)))))

(defclass directional (orth light)
  ()
  (:default-initargs
   :fs (v2! 10)
   :near  1f0
   :far 100f0
   :rot nil)
  (:documentation "simple directional light"))

(defun make-directional (&rest args)
  (apply #'make-instance 'directional args))

;;(defmethod update ((actor )))
(defmethod draw (actor (camera directional) time)
  "Simple pass to draw actor from light's POV"
  (with-slots (bs scale color) actor
    (map-g #'flat-3d-frag bs
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale
           :color color
           :time time)))
