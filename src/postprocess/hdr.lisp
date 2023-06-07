(in-package #:scenic)

(defclass hdr (postprocess)
  ((exposure :initarg :exposure
             :accessor exposure
             :documentation "camera exposure"))
  (:default-initargs
   :exposure 1f0)
  (:documentation "abstract class"))
