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

(defmethod free (object) t)

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4      rot))))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor dt))
(defmethod update (actor dt))
