(in-package #:scenic)

(defclass postprocess ()
  ((bs :reader bs
       :allocation :class
       :documentation "buffer stream for single stage pipelines")))

(defmethod initialize-instance :after ((obj postprocess) &key)
  (unless (slot-boundp obj 'bs)
    (setf (slot-value obj 'bs) (make-buffer-stream nil :primitive :points))))

(defgeneric post (obj))

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
    (v! ldr luma)))

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))

(defmethod post :around ((obj simple))
  (with-setf* ((depth-test-function) nil
               (depth-mask)          nil
               (cull-face)           nil)
    (call-next-method)))

(defmethod post ((obj simple))
  (let ((camera (current-camera)))
    (with-slots (exposure bs) obj
      (map-g #'generic-2d-pipe bs
             :sam  (nth 0 (sam camera))
             :exposure exposure))))
