(in-package #:scenic)

(defclass screen ()
  ((prev :accessor prev :allocation :class :documentation "previous screen capture")
   (next :accessor next :allocation :class :documentation "next screen capture")))

(defmethod initialize-instance :after ((obj screen) &key)
  (log4cl:log-info "making screen...")
  (flet ((make-renderable ()
           (make-instance
            'renderable
            :dim  (viewport-dimensions (current-viewport))
            :sample-opts '((:wrap :clamp-to-edge))
            :texture-opts `((0 :element-type :rgba16f)))))
    (setf (slot-value obj 'prev) (make-renderable))
    (setf (slot-value obj 'next) (make-renderable))))

(defmethod free :after ((obj screen))
  (free (prev obj))
  (free (next obj)))

(defmethod handle :after ((e resize) (obj screen))
  (setf (dim (prev obj)) (list (width e) (height e))
        (dim (next obj)) (list (width e) (height e))))
