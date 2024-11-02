(in-package #:scenic)

(defclass fxaa3 (postprocess)
  ((aliasing-removal
    :accessor fxaa3-aliasing-removal
    :initarg :aliasing-removal
    :documentation "Amount of sub pixel aliasing removal. Choose the amount of sub-pixel aliasing removal. This can effect sharpness.")
   (min-contrast
    :accessor fxaa3-min-contrast
    :initarg :min-contrast
    :documentation "Minimum local contrast required to apply. The minimum amount of local contrast required to apply algorithm.")
   (min-threshold
    :accessor fxaa3-min-threshold
    :initarg :min-threshold
    :documentation "Edge threshold min. Trims the algorithm from processing darks.")
   (settings
    :reader fxaa3-settings
    :documentation "vec3 sent as an uniform"))
  (:default-initargs
   :aliasing-removal 0.75
   :min-contrast     0.166
   :min-threshold    0.0833))

(defmethod initialize-instance :before ((obj fxaa3) &key aliasing-removal min-contrast min-threshold)
  (check-type aliasing-removal (float 0      1))      ; off    - default - softer
  (check-type min-contrast     (float 0.063  0.333))  ; slower - default - faster
  (check-type min-threshold    (float 0.0312 0.0833))); slower -           faster (default)

(defmethod (setf fxaa3-aliasing-removal) :before (new-value (obj fxaa3))
  (check-type new-value (float 0 1)))
(defmethod (setf fxaa3-min-contrast) :before (new-value (obj fxaa3))
  (check-type new-value (float 0.063 0.333)))
(defmethod (setf fxaa3-min-threshold) :before (new-value (obj fxaa3))
  (check-type new-value (float 0.0312 0.0833)))

(defmethod (setf fxaa3-aliasing-removal) :after (new-value (obj fxaa3))
  (setf (x (slot-value obj 'settings)) new-value))
(defmethod (setf fxaa3-min-contrast) :after (new-value (obj fxaa3))
  (setf (y (slot-value obj 'settings)) new-value))
(defmethod (setf fxaa3-min-threshold) :after (new-value (obj fxaa3))
  (setf (z (slot-value obj 'settings)) new-value))

(defmethod initialize-instance :after ((obj fxaa3) &key aliasing-removal min-contrast min-threshold)
  (setf (slot-value obj 'settings)
        (v! aliasing-removal min-contrast min-threshold)))

(defun-g fxaa3-frag
    ((uv           :vec2)
     &uniform
     (sam          :sampler-2d)
     (one-over-res :vec2)
     (settings     :vec3))
  (nineveh.anti-aliasing:fxaa3
   uv sam one-over-res (x settings) (y settings) (z settings)))

(defpipeline-g fxaa3-pipe (:points)
  :fragment (fxaa3-frag :vec2))

(defmethod blit (scene (postprocess fxaa3) camera time)
  (let ((sam (first (sam (prev *state*)))))
    (destructuring-bind (width height)
        (texture-base-dimensions (sampler-texture sam))
      (map-g #'fxaa3-pipe (bs postprocess)
             :settings (fxaa3-settings postprocess)
             :sam (first (sam (prev *state*)))
             :one-over-res (v! (/ 1 width) (/ 1 height))))))
