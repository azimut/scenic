(in-package #:scenic)

(defclass postprocess ()
  ((bs :reader bs
       :allocation :class
       :documentation "buffer stream for single stage pipelines")))

(defmethod initialize-instance :after ((obj postprocess) &key)
  (unless (slot-boundp obj 'bs)
    (setf (slot-value obj 'bs) (make-buffer-stream nil :primitive :points))))

(defclass simple (postprocess)
  ((exposure :initarg :exposure
             :initform (error ":exposure must be specified")
             :accessor exposure
             :documentation "camera exposure")))

(defun make-simple-postprocess (&key (exposure 1f0))
  (make-instance 'simple :exposure exposure))

(defun-g frag-2d ((uv :vec2) &uniform (sam :sampler-2d) (exposure :float))
  (let* ((final-color (s~ (texture sam uv) :xyz))
         (ldr (tone-map-acesfilm final-color exposure))
         (luma (rgb->luma-bt601 ldr)))
    (v! ldr luma)
    final-color));; FIXME

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))

(defmethod draw :around ((obj simple) camera time)
  (with-setf* ((depth-test-function) nil
               (depth-mask)          nil
               (cull-face)           nil)
    (call-next-method)))

(defmethod draw ((obj simple) camera time)
  (with-slots (exposure bs) obj
    (map-g #'generic-2d-pipe bs
           :sam (first (sam camera))
           :exposure exposure)))
