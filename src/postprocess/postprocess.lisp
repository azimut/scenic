(in-package #:scenic)

(defclass postprocess ()
  ((bs :reader bs
       :allocation :class
       :documentation "buffer stream for single stage pipelines")))

(defmethod initialize-instance :after ((obj postprocess) &key)
  (unless (slot-boundp obj 'bs)
    (setf (slot-value obj 'bs) (make-buffer-stream nil :primitive :points))))

