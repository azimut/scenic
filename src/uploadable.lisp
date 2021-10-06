(in-package #:scenic)

(defclass uploadable ()
  ((uploadp :reader uploadp :initform T))
  (:documentation "to avoid uploading unless there is a reason"))

(defmethod upload :around ((obj uploadable))
  (when (uploadp obj)
    (call-next-method)
    (setf (uploadp obj) NIL)))
