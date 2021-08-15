(in-package #:scenic)

(defclass actor ()
  ((pos   :initarg :pos   :accessor pos   :documentation "3d position")
   (rot   :initarg :rot   :accessor rot   :documentation "3d rotation")
   (buf   :initarg :buf   :accessor buf   :documentation "buffer stream")
   (color :initarg :color :accessor color :documentation "base color")
   (scale :initarg :scale :accessor scale :documentation "vextex fudge scale"))
  (:default-initargs
   :color (v! 1 1 1)
   :pos (v! 0 0 0 0)
   :rot (q:identity)
   :scale 1f0
   :buf (box))
  (:documentation "base object"))

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4      rot))))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor dt))
(defmethod update (actor dt))

(defmethod update ((actor actor) dt)
  ;; (setf (pos actor) (v! 0 0 0))
  ;;(setf (rot actor) (q:from-axis-angle (v! 0 1 0) (radians 0)))
  )

(defun-g actor-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                     &uniform
                     (time        :float)
                     (color       :vec3)
                     (shadows     :sampler-2d-array)
                     (dirlights   dir-light-data   :ubo)
                     (pointlights point-light-data :ubo))
  (let ((final-color (v! 0 0 0))
        (shadow      (texture shadows (v! uv 0))))
    (dotimes (i (size dirlights))
      (incf final-color (dir-light-apply color
                                         (aref (colors dirlights) i)
                                         (aref (positions dirlights) i)
                                         frag-pos
                                         frag-norm)))
    (dotimes (i (size pointlights))
      (with-slots (colors positions linear quadratic) pointlights
        (incf final-color (point-light-apply color
                                             (aref colors i)
                                             (aref positions i)
                                             frag-pos
                                             frag-norm
                                             (aref linear i)
                                             (aref quadratic i)))))
    (v! final-color 1)))

(defpipeline-g actor-pipe ()
  :vertex (vert g-pnt)
  :fragment (actor-frag :vec2 :vec3 :vec3))

(let ((stepper (make-stepper (seconds 5) (seconds 5))))
  (defmethod update ((actor actor) dt)
    (when (funcall stepper)
      (setf (rot actor)
            (q:* (q:from-axis-angle (v! 1 0 0) (radians (random 90)))
                 (q:from-axis-angle (v! 0 0 1) (radians (random 90))))))))

(defmethod draw ((actor actor) (camera renderable) time)
  (let ((scene (current-scene)))
    (with-slots (buf scale color) actor
      (map-g #'actor-pipe buf
             :shadows (dir-sam (lights scene))
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip (projection camera)
             :scale scale
             :color color
             :dirlights (dir-ubo (lights scene))
             :pointlights (point-ubo (lights scene))
             :time time))))
