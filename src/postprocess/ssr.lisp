(in-package #:scenic)

(defclass ssr (postprocess renderable)
  ((out :reader out))
  (:default-initargs
   :texture-opts '((0 :element-type :rgba32f))
   :sample-opts  '((:wrap :clamp-to-edge)))
  (:documentation "screen space reflections, compute shader based"))

(defmethod initialize-instance :after ((obj ssr) &key)
  (with-slots (tex out) obj
    (setf out (sample (first tex)))
    (setf (%cepl.types::%sampler-imagine out) t)))

(defmethod handle ((e resize) (ssr ssr))
  (setf (dim ssr) (list (width e) (height e)))
  (with-slots (tex out) ssr
    (setf out (sample (first tex)))
    (setf (%cepl.types::%sampler-imagine out) t)))

(defun-g read-depth ((uv    :vec2)
                     (depth :sampler-2d))
  (1- (* (x (texture-lod depth uv 0)))))

(defun-g binary-search ((dir   :vec3)
                        (coord :vec3)
                        (proj  :mat4)
                        (samd  :sampler-2d))
  (let* ((projected-coord (vec4 0))
         (coord coord))
    (dotimes (i 5) ; kNumBinarySearchSteps
      (setf projected-coord
            (* proj (v! coord 1)))
      (setf (s~ projected-coord :xyz)
            (/ (s~ projected-coord :xyz)
               (w projected-coord)))
      (setf (s~ projected-coord :xy)
            (+ .5 (* .5 (s~ projected-coord :xy))))
      (setf dir (* .5 dir))
      (let ((depth (1- (* 2 (x (texture samd (s~ projected-coord :xy)))))))
        (if (< (z projected-coord) depth)
            (incf coord dir)
            (decf coord dir))))
    (setf projected-coord (* proj (v! coord 1)))
    (setf (s~ projected-coord :xy)
          (/ (s~ projected-coord :xy) (w projected-coord)))
    (setf (s~ projected-coord :xy)
          (+ .5 (* .5 (s~ projected-coord :xy))))
    (s~ projected-coord :xy)
    #+nil
    (values
     dir
     coord)))

(defun-g ray-march ((dir  :vec3)
                    (pos  :vec3)
                    (proj :mat4)
                    (samd :sampler-2d))
  (let* ((k-ray-step      .1)
         (ray-step        (* dir k-ray-step))
         (ray-sample      pos)
         (projected-coord (vec4 0))
         (z-val           0f0))
    (dotimes (i 32) ; kMaxSteps
      (incf ray-sample ray-step)
      (setf projected-coord
            (* proj (v! ray-sample 1)))
      (setf (s~ projected-coord :xyz)
            (/ (s~ projected-coord :xyz)
               (w projected-coord)))
      (setf (s~ projected-coord :xy)
            (+ .5 (* .5 (s~ projected-coord :xy))))
      (setf z-val
            (read-depth (s~ projected-coord :xy) samd))
      (if (> (z projected-coord) z-val)
          ;;(multiple-value-bind (ret newdir newcoord))
          ;;(binary-search ray-step ray-sample proj samd)
          ;;(setf ray-step   newdir)
          ;;(setf ray-sample newcoord)
          (return (v! (binary-search ray-step ray-sample proj samd)
                      1)))))
  (vec3 0f0))

(defun-g ssr-compute (&uniform (world-view  :mat4)
                               ;;(model-world :mat4)
                               (view-clip   :mat4)
                               (iview-clip  :mat4)
                               (iworld-view :mat4)
                               (metal-map   :sampler-2d)
                               (albedo      :sampler-2d)
                               (samd        :sampler-2d)
                               (samp        :sampler-2d)
                               (samn        :sampler-2d)
                               (ssr-sam     :image-2d))
  (declare (local-size :x 16 :y 16 :z 1))
  (let* ((k-min-ray-step .1)
         (size (texture-size samd 0))
         (tex-coord (/ (v! (int (x gl-global-invocation-id))
                           (int (y gl-global-invocation-id)))
                       (v! (- (x size) 1)
                           (- (y size) 1))))
         ;; Reconstruct view space position
         ;;(view-pos (s~ (get-view-pos tex-coord samd world-view) :xyz))
         #+nil
         (view-pos (view-position-from-depth
                    tex-coord
                    (x (texture samd tex-coord))
                    iview-clip
                    ))
         ;;#+nil
         (view-pos
           (s~
            (* world-view
               (v! (s~ (texture samp tex-coord) :xyz) 1))
            :xyz))
         #+nil
         (view-pos (* (m4:to-mat3 world-view)
                      (s~ (texture samp tex-coord) :xyz)))
         ;; Get normal from G-Buffer in View Space
         (world-normal (normalize (s~ (texture samn tex-coord) :xyz)))
         (view-normal  (normalize (* (m4:to-mat3 world-view) world-normal)))
         ;; Retrieve metalness and roughness values
         (metallic     (x (texture metal-map tex-coord)))
         (view-dir     (normalize view-pos))
         (reflection   (normalize (reflect view-dir view-normal)))
         (camera-facing-refl-attenuation
           (- 1 (smoothstep .25 .5 (dot (- view-dir) reflection)))))
    ;;#+nil
    (if (or (<= camera-facing-refl-attenuation 0f0)
            (< metallic .1))
        (image-store ssr-sam
                     (ivec2 (int (x gl-global-invocation-id))
                            (int (y gl-global-invocation-id)))
                     (vec4 0f0))
        (let* ((wp        (s~ (* iworld-view (v! view-pos 1f0)) :xyz))
               (hit-pos   view-pos)
               (ray       (* reflection (max k-min-ray-step (- (z view-pos)))))
               (hit-coord (ray-march ray hit-pos view-clip samd))
               (tex-coord (smoothstep (vec2 .2)
                                      (vec2 .6)
                                      (abs (- (v! .5 .5) (s~ hit-coord :xy)))))
               (screen-edge-factor
                 (clamp (- 1 (+ (x tex-coord) (y tex-coord))) 0f0 1f0))
               (uv-sampling-attenuation
                 (* (smoothstep (vec2 0.05) (vec2 0.1) (s~ hit-coord :xy))
                    (- (vec2 1f0)
                       (smoothstep (vec2 0.95) (vec2 1f0) (s~ hit-coord :xy)))))
               (uv-sampling-attenuation
                 (v! (* (x uv-sampling-attenuation)
                        (y uv-sampling-attenuation))
                     (y uv-sampling-attenuation)))
               (total-attenuation (* camera-facing-refl-attenuation
                                     screen-edge-factor
                                     (z hit-coord))))
          (image-store ssr-sam
                       (ivec2 (int (x gl-global-invocation-id))
                              (int (y gl-global-invocation-id)))
                       (v! (s~ hit-coord :xy)
                           #+nil
                           (v! (x hit-coord)
                               (- 1 (y hit-coord)))
                           total-attenuation
                           1f0))))
    (values)))

(defpipeline-g ssr-pipe ()
  :compute ssr-compute)

(defun-g ssr-apply-frag ((uv          :vec2)
                         &uniform
                         (color       :vec3)
                         (scene-sam   :sampler-2d)
                         (ssr-sam     :sampler-2d))
  (let* ((ssr (s~ (texture ssr-sam uv) :xyz))
         (reflection (s~ (texture scene-sam (s~ ssr :xy)) :xyz))
         (albedo color))
    (v! (mix albedo reflection (* .5 (z ssr)))
        1)))

(defpipeline-g ssr-apply-pipe (:points)
  :fragment (ssr-apply-frag :vec2))

(defmethod blit (scene (ssr ssr) (camera defered) _)
  ;;#+nil
  (when (drawp ssr)
    (let ((compute (destructuring-bind (x y) (dim ssr)
                     (make-compute-space (floor (/ x 16)) (floor (/ y 16))))))
      (destructuring-bind (s1 s2 s3 s4 sd) (sam camera)
        (map-g #'ssr-pipe compute
               :world-view  (world->view camera)
               :iworld-view (m4:inverse (world->view camera))
               :view-clip (projection  camera)
               :iview-clip (m4:inverse (projection camera))
               :albedo s1
               :samp s2                 ; pos
               :samn s3                 ; normal
               :metal-map s4
               :samd sd                 ; depth
               :ssr-sam (out ssr)))))
  ;;#+nil
  (map-g #'ssr-apply-pipe (bs *state*)
         :scene-sam (first (sam (next *state*)))
         :ssr-sam (first (sam ssr))
         :color (v! .5 .5 .5))
  ;;(wait-on-gpu-fence (make-gpu-fence))
  )
