(in-package #:scenic)

(defclass screen (listener)
  ((prev :accessor prev :allocation :class :documentation "previous screen capture")
   (next :accessor next :allocation :class :documentation "next screen capture")))

(defmethod initialize-instance :after ((obj screen) &key)
  (log4cl:log-info "making screen...")
  (flet ((make-renderable ()
           (make-instance
            'renderable
            :dim  (viewport-dimensions (current-viewport))
            :sample-opts '((:wrap :clamp-to-edge))
            :texture-opts `((0 :element-type :rgba32f)))))
    (setf (slot-value obj 'prev) (make-renderable))
    (setf (slot-value obj 'next) (make-renderable))))

(defmethod handle :after ((e resize) (obj screen))
  (setf (dim (prev *state*)) (list (width e) (height e))
        (dim (next *state*)) (list (width e) (height e))))
