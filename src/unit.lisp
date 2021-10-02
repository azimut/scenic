(in-package #:scenic)

(defclass unit (listener)
  ()
  (:documentation "doc"))

(defmethod initialize-instance :after ((obj unit) &key)
  (add-listener obj ))
