(in-package #:scenic)

;; References:
;; - https://github.com/McNopper/OpenGL/blob/master/Example28/shader/ssao.frag.glsl
;; - http://ogldev.atspace.co.uk/www/tutorial46/tutorial46.html
;; - https://learnopengl.com/Advanced-Lighting/SSAO
;; - https://www.youtube.com/watch?v=7fLV5ezO64w
;;   "AO is the area where ambient light gets ocludded"

;; TODO
;; - I am gonna need to add a fake "ambient light" to my render for non-ibl scenes
;; - There might be a way to save me from doing an extra map-g

(defclass ssao-postprocess (postprocess renderable-screen ssao)
  ()
  (:default-initargs
   :texture-opts '((0 :element-type :r8))
   :sample-opts '((:wrap :clamp-to-edge))))

(defun-g ssao-frag
    ((uv :vec2)
     &uniform
     (kernel                      :int)
     (radius                      :float)
     (kernel-effect               :float)
     ;;
     (tex-noise                   :sampler-2d)
     (random-kernel random-kernel :ubo)
     ;;
     (g-normal                    :sampler-2d)
     (g-depth                     :sampler-2d)
     ;;
     (res                         :vec2)
     (view-clip                   :mat4))
  (let* ((pos-view (get-view-pos uv g-depth view-clip))
         (normal-view
           (normalize
            (1- (* 2 (s~ (texture g-normal uv) :xyz)))))
         (random-vector
           (normalize
            (1- (* 2 (s~ (texture tex-noise (* (/ res 4) uv)) :xyz)))))
         (tangent-view
           (normalize
            (- random-vector (* (dot random-vector normal-view)
                                normal-view))))
         (bitangent-view (cross normal-view tangent-view))
         (kernel-matrix  (mat3 tangent-view bitangent-view normal-view))
         (oclussion 0f0))
    (with-slots (random-v3) random-kernel
      (dotimes (i kernel)
        (let* ((sample-vector-view (* kernel-matrix (aref random-v3 (int i))))
               (sample-point-view  (+ pos-view (* radius (v! sample-vector-view 0))))
               (sample-point-ndc   (* view-clip sample-point-view))
               (sample-point-ndc   (/ sample-point-ndc
                                      (w sample-point-ndc)))
               (sample-point-uv    (+ .5 (* .5 (s~ sample-point-ndc :xy))))
               (z-scene-ndc        (1- (* 2 (x (texture g-depth sample-point-uv)))))
               (delta (- (z sample-point-ndc) z-scene-ndc)))
          (if (and (> delta .0001)
                   (< delta .005))
              (incf oclussion 1f0)))))
    #+nil
    (v! (vec3 (- 1 (/ oclussion (1- (* kernel-effect kernel)))))
        1)
    (- 1 (/ oclussion (1- (* kernel-effect kernel))))))

(defpipeline-g ssao-pipe (:points)
  :fragment (ssao-frag :vec2))

(defun-g ssao-apply-frag ((uv :vec2) &uniform (scene :sampler-2d) (ssao :sampler-2d))
  (let ((ao    (x (texture ssao uv)))
        (color (texture scene uv)))
    (v! (* (s~ color :xyz) ao)
        (w color))))

(defpipeline-g ssao-apply-pipe (:points)
  :fragment (ssao-apply-frag :vec2))

(defmethod blit (scene (postprocess ssao-postprocess) (camera defered) time)
  (with-slots (kernel-effect kernel-radius kernel-number
               noise-sam noise-kernel
               fbo sam)
      postprocess
    (with-fbo-bound (fbo)
      (with-setf* ((depth-test-function) #'always
                   (depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1))
        (map-g #'ssao-pipe (bs *state*)
               :g-normal (third (sam camera))
               :g-depth  (fifth (sam camera))
               :kernel kernel-number
               :kernel-effect kernel-effect
               :radius kernel-radius
               :tex-noise noise-sam
               :random-kernel noise-kernel
               :res (res postprocess)
               :view-clip (projection camera))))

    (map-g #'ssao-apply-pipe (bs *state*)
           :scene (first (sam (prev *state*)))
           :ssao (first sam))))
