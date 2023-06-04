(in-package #:scenic)

(defclass postprocess (listener)
  ((bs :reader bs
       :allocation :class
       :documentation "buffer stream for single stage pipelines")))

(defmethod initialize-instance :after ((obj postprocess) &key)
  (unless (slot-boundp obj 'bs)
    (setf (slot-value obj 'bs)
          (make-buffer-stream nil :primitive :points))))

(defun-g pass-frag ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g pass-pipe (:points)
  :fragment (pass-frag :vec2))

(defmethod blit
    :around (scene (postprocess list) camera time)
  (with-setf* ((depth-test-function) NIL
               (depth-mask)          NIL
               (cull-face)           NIL)
    (call-next-method)))

(defmethod blit (scene (postprocess list) (camera renderable) time)
  (with-slots (prev bs) *state*
    (map-g-into (fbo prev) #'pass-pipe bs :sam (first (sam camera)))))

(defmethod blit
    :after (scene (postprocess list) camera time)
  (dolist (post (butlast postprocess))
    (with-fbo-bound ((fbo (next *state*)))
      (blit scene post camera time))
    (rotatef (prev *state*) (next *state*)))
  (blit scene (alexandria:lastcar postprocess) camera time))
