(in-package #:scenic)

(defun-g defered-ssao-frag ((uv :vec2)
                            &uniform
                            (sample1      :sampler-2d)
                            (sample2      :sampler-2d)
                            (sample3      :sampler-2d)
                            (sample4      :sampler-2d)
                            ;; Lighting
                            (ssao-blur    :sampler-2d)
                            (fakeambient  :float)
                            (cam-pos      :vec3)
                            (scene        scene-data          :ubo)
                            ;; Lights
                            (dirlights    dir-light-data      :ubo)
                            (pointlights  point-light-data    :ubo)
                            (spotlights   spot-light-data     :ubo)
                            ;; Shadows
                            (dirshadows   :sampler-2d-array)
                            (spotshadows  :sampler-2d-array)
                            (pointshadows :sampler-cube-array))
  (let* ((color1    (texture sample1 uv))
         (color2    (texture sample2 uv))
         (color3    (texture sample3 uv))
         (color4    (texture sample4 uv))
         (color     (s~ color1 :xyz))
         (frag-pos  (s~ color2 :xyz))
         (frag-norm (s~ color3 :xyz))
         (roughness (w color1))
         (ao        (* (w color2)
                       (x (texture ssao-blur uv))))
         (specular  (w color3))
         (metallic  (x color4))
         (emissive  (y color4))
         (final-color (vec3 0))
         (ambient     (vec3 0)))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions lightspace fudge)
          dirlights
        (incf ambient (* fakeambient color))
        (incf final-color
              (* (pbr-direct-lum (aref positions i) frag-pos cam-pos frag-norm
                                 roughness
                                 metallic
                                 color
                                 specular
                                 (aref colors i))
                 (shadow-factor dirshadows
                                (* (aref lightspace i) (v! frag-pos 1))
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf ambient (* fakeambient color
                         (point-light-attenuation
                          (aref linear i) (aref quadratic i) (aref positions i) frag-pos)))
        (incf final-color
              (* (pbr-point-lum (aref positions i) frag-pos cam-pos
                                frag-norm
                                roughness
                                metallic
                                color
                                specular
                                (aref linear i) (aref quadratic i) (aref colors i))
                 (shadow-factor pointshadows
                                frag-pos
                                (aref positions i) ;;?
                                (aref far i)
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction lightspace fudge)
          spotlights
        (incf ambient (* fakeambient color
                         (point-light-attenuation
                          (aref linear i) (aref quadratic i) (aref positions i) frag-pos)))
        (incf final-color
              (* (pbr-spot-lum (aref positions i) frag-pos cam-pos frag-norm
                               roughness
                               metallic
                               color
                               specular
                               (aref colors       i)
                               (aref direction    i)
                               (aref cutoff       i)
                               (aref outer-cutoff i)
                               (aref linear       i)
                               (aref quadratic    i))
                 (shadow-factor spotshadows
                                (* (aref lightspace i) (v! frag-pos 1))
                                (aref fudge i)
                                i)))))
    (v! (+ (* ambient ao)
           final-color)
        ;; TODO: this alpha is to blend the possible cubemap
        (- 1 (step (y color) 0f0)))))

(defpipeline-g defer-ssao-pipe (:points)
  :fragment (defered-ssao-frag :vec2))

(defmethod blit ((scene scene-ssao) (postprocess list) (camera defered) time)
  (destructuring-bind (_s1 _s2 g-normal _s4 g-depth) (sam camera)
    (declare (ignore _s1 _s2 _s4))
    (with-slots (ssao-render ssao-blur
                 kernel-number kernel-effect kernel-radius kernel
                 noise-kernel noise-sam tex-noise)
        scene
      (with-setf* ((depth-test-function) #'always
                   (depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1))
        (map-g-into (fbo ssao-render) #'ssao-pipe (bs *state*)
                    :ssao-kernel-effect kernel-effect
                    :ssao-kernel-radius kernel-radius
                    :ssao-kernel kernel-number
                    :random-kernel noise-kernel
                    :ssao-tex-noise noise-sam
                    :res (res camera)
                    :view-clip (projection camera)
                    :g-normal g-normal
                    :g-depth g-depth)
        (map-g-into (fbo ssao-blur) #'blur-pipe (bs *state*)
                    :ssao-pass (first (sam ssao-render))))))
  (destructuring-bind (s1 s2 s3 s4 _) (sam camera)
    (declare (ignore _))
    (with-slots (prev bs) *state*
      (with-slots (kernel-number kernel-effect kernel-radius
                   noise-kernel noise-sam ssao-blur)
          scene
        (with-fbo-bound ((fbo prev))
          (clear-fbo (fbo prev)) ;; needed for scenes with no envmap
          (with-blending (blend camera)
            (alexandria:when-let
                ((actor (find-if #'cube-p (actors scene))))
              (paint scene camera actor time))
            (map-g #'defer-ssao-pipe bs
                   :fakeambient (fakeambient camera)
                   :cam-pos (pos camera)
                   :scene (ubo scene)
                   ;; Samples
                   :sample1 s1
                   :sample2 s2
                   :sample3 s3
                   :sample4 s4
                   :ssao-blur (first (sam ssao-blur))
                   ;; Lights
                   :dirlights (dir-ubo *state*)
                   :spotlights (spot-ubo *state*)
                   :pointlights (point-ubo *state*)
                   ;; Shadows
                   :dirshadows (dir-sam *state*)
                   :spotshadows (spot-sam *state*)
                   :pointshadows (point-sam *state*))))))))
