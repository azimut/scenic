(in-package #:scenic)

(defclass occluder ()
  ((occludesp :initarg :occludesp
              :accessor occludesp))
  (:default-initargs
   :occludesp t)
  (:documentation "occludes audio emmisions"))

(defmethod initialize-instance :before ((obj occluder) &key occludesp)
  (check-type occludesp boolean))

(defmethod (setf occludesp) :before (new-value (obj occluder))
  (check-type new-value boolean))
