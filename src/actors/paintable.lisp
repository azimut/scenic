(in-package #:scenic)

(defclass paintable ()
  ((paintp :initarg :paintp :accessor paintp))
  (:default-initargs
   :paintp t))

(defmethod initialize-instance :before ((obj paintable) &key paintp)
  (check-type paintp boolean))

(defmethod (setf paintp) :before (new-value (obj paintable))
  (check-type new-value boolean))
