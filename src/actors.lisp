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
                     &uniform (time :float) (color :vec3) (lights light-data :ubo))
  (let ((final-color (v! 0 0 0)))
    (dotimes (i (light-data-size lights))
      (setf final-color (dir-light-apply color
                                         (aref (light-data-colors lights) i)
                                         (aref (light-data-positions lights) i)
                                         frag-pos
                                         frag-norm)))
    (v! final-color 1)))

(defpipeline-g actor-pipe ()
  :vertex (vert g-pnt)
  :fragment (actor-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor actor) (camera renderable) time)
  (with-slots (buf scale color) actor
    (map-g #'actor-pipe buf
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale
           :color color
           :lights (ubo (lights (current-scene)))
           :time time)))
