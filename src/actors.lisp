(in-package #:scenic)

(defclass actor ()
  ((pos :initarg :pos :accessor pos :documentation "3d position")
   (rot :initarg :rot :accessor rot :documentation "3d rotation")
   (buf :initarg :buf :accessor buf :documentation "buffer stream"))
  (:default-initargs
   :pos (v! 0 0 0 0)
   :rot (q:identity)
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

(defmethod draw ((actor actor) (camera renderable) time)
  (with-fbo-bound ((fbo camera))
    (with-slots (fbo buf scale color) actor
      (map-g #'flag-3d-frag buf
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip (projection camera)
             :scale scale
             :color color
             :time time))))
