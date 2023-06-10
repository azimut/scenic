(in-package #:scenic)

(defun-g defered-ibl-frag ((uv :vec2)
                           &uniform
                           (sample1      :sampler-2d)
                           (sample2      :sampler-2d)
                           (sample3      :sampler-2d)
                           (sample4      :sampler-2d)
                           (brdf         :sampler-2d)
                           (prefilter    :sampler-cube)
                           (irradiance   :sampler-cube)
                           (dirlights    dir-light-data      :ubo)
                           (pointlights  point-light-data    :ubo)
                           (spotlights   spot-light-data     :ubo)
                           (scene        scene-data          :ubo)
                           (cam-pos      :vec3)
                           (time         :float)
                           (dirshadows   :sampler-2d-array)
                           (spotshadows  :sampler-2d-array)
                           (pointshadows :sampler-cube-array)
                           (exposure     :float))
  (let* ((color1    (texture sample1 uv))
         (color2    (texture sample2 uv))
         (color3    (texture sample3 uv))
         (color4    (texture sample4 uv))
         (color     (s~ color1 :xyz))
         (frag-pos  (s~ color2 :xyz))
         (frag-norm (s~ color3 :xyz))
         (roughness (w color1))
         (ao        (w color2))
         (specular  (w color3))
         (metallic  (x color4))
         (emissive  (y color4))
         (final-color (v! 0 0 0))
         #+nil
         (ambient (ambient-ibl (normalize (- cam-pos frag-pos))
                               frag-norm
                               irradiance
                               roughness
                               metallic
                               color
                               ao))
         ;;#+nil
         (ambient (ambient-ibl (normalize (- cam-pos frag-pos))
                               frag-norm
                               brdf
                               prefilter
                               irradiance
                               roughness
                               metallic
                               color
                               ao)))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions lightspace fudge)
          dirlights
        (incf final-color
              (* (pbr-direct-lum (aref positions i) frag-pos cam-pos frag-norm
                                 roughness
                                 metallic
                                 color
                                 specular
                                 (aref colors i))
                 (shadow-factor dirshadows (* (aref lightspace i) (v! frag-pos 1)) (aref fudge i) i)))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
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
                                (aref positions i)
                                (aref far i)
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction lightspace fudge)
          spotlights
        (incf final-color
              (* (pbr-spot-lum (aref positions i) frag-pos cam-pos
                               frag-norm
                               roughness
                               metallic
                               color
                               specular
                               (aref colors i)
                               (aref direction i)
                               (aref cutoff i)
                               (aref outer-cutoff i)
                               (aref linear i)
                               (aref quadratic i))
                 (shadow-factor spotshadows
                                (* (aref lightspace i) (v! frag-pos 1))
                                (aref fudge i)
                                i)))))
    (v! (+ ambient
           final-color)
        ;; TODO: this alpha is to blend the possible cubemap
        (- 1 (step (y color) 0f0)))))

(defpipeline-g defer-ibl-pipe (:points)
  :fragment (defered-ibl-frag :vec2))

(defmethod blit ((scene scene-ibl) (postprocess list) (camera defered) time)
  (destructuring-bind (s1 s2 s3 s4 _) (sam camera)
    (declare (ignore _))
    (with-blending (blend camera)
      (with-slots (prev bs) *state*
        (with-fbo-bound ((fbo prev))
          (clear-fbo (fbo prev)) ;; needed for scenes with no envmap
          (alexandria:when-let
              ((actor (find-if #'cube-p (actors scene))))
            (paint scene camera actor time))
          (map-g #'defer-ibl-pipe bs
                 :sample1 s1
                 :sample2 s2
                 :sample3 s3
                 :sample4 s4
                 :cam-pos (pos (current-camera))
                 :scene (ubo scene)
                 :time time
                 ;; IBL
                 :brdf (brdf-sam *state*)
                 :prefilter (first (sam (prefilter scene)))
                 :irradiance (first (sam (irradiance scene)))
                 ;; Shadows
                 :dirshadows (dir-sam *state*)
                 :spotshadows (spot-sam *state*)
                 :pointshadows (point-sam *state*)
                 ;; Lights
                 :dirlights (dir-ubo *state*)
                 :spotlights (spot-ubo *state*)
                 :pointlights (point-ubo *state*)))))))
