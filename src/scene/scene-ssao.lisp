(in-package #:scenic)

;; TODO: this is awkward...but oh well
(defclass scene-ssao (scene ssao)
  (ssao-render
   ssao-blur))

(defmethod initialize-instance :after ((obj scene-ssao) &key)
  (with-slots (ssao-render ssao-blur) obj
    (setf ssao-render (make-instance
                       'renderable-screen
                       :texture-opts '((0 :element-type :r8))
                       :sample-opts '((:minify-filter  :nearest
                                       :magnify-filter :nearest))))
    (setf ssao-blur   (make-instance
                       'renderable-screen
                       :texture-opts '((0 :element-type :r8))
                       :sample-opts '((:minify-filter  :nearest
                                       :magnify-filter :nearest))))
    (add-listener ssao-render obj)
    (add-listener ssao-blur   obj)))

(defun make-scene-ssao (&rest args)
  (apply #'make-instance 'scene-ssao args))

(defmethod free :after ((obj scene-ssao))
  (with-slots (ssao-render ssao-blur) obj
    (free ssao-render)
    (free ssao-blur)))

(defun-g get-view-pos ((uv         :vec2)
                       (g-depth    :sampler-2d)
                       (world-view :mat4))
  (let* ((x (1- (* 2 (x uv))))
         (y (1- (* 2 (y uv))))
         (z (1- (* 2 (x (texture g-depth uv)))))
         (pos-proj (v! x y z 1))
         (pos-view (* (inverse world-view) pos-proj))
         (pos-view (/ pos-view (w pos-view))))
    pos-view))

(defun-g ssao-frag
    ((uv                 :vec2)
     &uniform
     (res                :vec2)
     (view-clip          :mat4)
     (g-normal           :sampler-2d)
     (g-depth            :sampler-2d)
     (random-kernel      random-kernel :ubo)
     (ssao-tex-noise     :sampler-2d)
     (ssao-kernel-radius :float)
     (ssao-kernel        :int)
     (ssao-kernel-effect :float))
  (let* ((normal (s~ (texture g-normal uv) :xyz))
         (pos-view (get-view-pos uv g-depth view-clip))
         (normal-view
           (normalize
            (1- (* 2 normal))))
         (random-vector
           (normalize
            (1- (* 2 (s~ (texture ssao-tex-noise (* (/ res 4) uv)) :xyz)))))
         (tangent-view
           (normalize
            (- random-vector (* (dot random-vector normal-view)
                                normal-view))))
         (bitangent-view (cross normal-view tangent-view))
         (kernel-matrix  (mat3 tangent-view bitangent-view normal-view))
         (oclussion 0f0))
    (with-slots (random-v3) random-kernel
      (dotimes (i ssao-kernel)
        (let* ((sample-vector-view (* kernel-matrix (aref random-v3 (int i))))
               (sample-point-view  (+ pos-view (* ssao-kernel-radius (v! sample-vector-view 0))))
               (sample-point-ndc   (* view-clip sample-point-view))
               (sample-point-ndc   (/ sample-point-ndc
                                      (w sample-point-ndc)))
               (sample-point-uv    (+ .5 (* .5 (s~ sample-point-ndc :xy))))
               (z-scene-ndc        (1- (* 2 (x (texture g-depth sample-point-uv)))))
               (delta (- (z sample-point-ndc) z-scene-ndc)))
          (if (and (> delta .0001)
                   (< delta .005))
              (incf oclussion 1f0)))))
    (- 1 (/ oclussion (1- (* ssao-kernel-effect ssao-kernel))))))

(defpipeline-g ssao-pipe (:points)
  :fragment (ssao-frag :vec2))

(defun-g blur-frag ((uv :vec2)
                    &uniform
                    (ssao-pass :sampler-2d))
  (let* ((texel-size (/ 1f0 (s~ (texture-size ssao-pass 0) :xy)))
         (result 0f0))
    (for (x -2) (< x 2) (++ x)
         (for (y -2) (< y 2) (++ y)
              (let ((offset (* texel-size (v! x y))))
                (incf result (x (texture ssao-pass (+ uv offset)))))))
    (/ result (* 4 4))))

(defpipeline-g blur-pipe (:points)
  :fragment (blur-frag :vec2))
