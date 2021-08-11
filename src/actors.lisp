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
  (setf (pos actor) (v! 0 0 0))
  (setf (rot actor) (q:from-axis-angle (v! 0 1 0) (radians 11)))
  )

(defmethod draw ((actor actor) (camera renderable) time)
  (let ((bar (fbo camera)))
    (with-fbo-bound (bar)
      (clear bar)
      (with-slots (buf scale color) actor
        (map-g #'flat-3d-pipe buf
               :model-world (model->world actor)
               :world-view (world->view camera)
               :view-clip (projection camera)
               :scale 1f0 ; FIXME
               :color (v! 0 1 0)
               :time 1f0)))))
