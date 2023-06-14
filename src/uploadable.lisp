(in-package #:scenic)

(defclass uploadable ()
  ((uploadp :accessor uploadp :initform T))
  (:documentation "to avoid uploading outside lisp, unless there is a reason"))

(defmethod upload (obj))
(defmethod upload :around ((obj uploadable))
  (when (uploadp obj)
    (call-next-method)
    (setf (uploadp obj) NIL)))
