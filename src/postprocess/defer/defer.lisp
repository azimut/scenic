(in-package #:scenic)

(defun-g point-light-attenuation ((linear    :float)
                                  (quadratic :float)
                                  (light-pos :vec3)
                                  (frag-pos  :vec3))
  "Can be usefult to apply distance attenuation to the ambient of a pointlight"
  (let ((distance (length (- light-pos frag-pos))))
    (/ 1f0 (+ 1f0
              (* linear distance)
              (* quadratic distance)))))

(defun-g defered-frag ((uv :vec2)
                       &uniform
                       (sample1      :sampler-2d)
                       (sample2      :sampler-2d)
                       (sample3      :sampler-2d)
                       (sample4      :sampler-2d)
                       (dirlights    dir-light-data      :ubo)
                       (pointlights  point-light-data    :ubo)
                       (spotlights   spot-light-data     :ubo)
                       (scene        scene-data          :ubo)
                       (cam-pos      :vec3)
                       (fakeambient  :float)
                       (dirshadows   :sampler-2d-array)
                       (spotshadows  :sampler-2d-array)
                       (pointshadows :sampler-cube-array))
  (let* ((color1    (texture sample1 uv))
         (color2    (texture sample2 uv))
         (color3    (texture sample3 uv))
         (color4    (texture sample4 uv))
         (roughness (w color1))
         (ao        (w color2))
         (specular  (w color3))
         (metallic  (x color4))
         (emissive  (y color4))
         (color     (s~ color1 :xyz))
         (frag-pos  (s~ color2 :xyz))
         (frag-norm (s~ color3 :xyz))
         (final-color (vec3 0))
         (ambient     (vec3 0))
         (ambient-mix .25))
    (incf final-color (* emissive color))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions lightspace fudge)
          dirlights
        (incf ambient (* fakeambient (mix color (aref colors i) ambient-mix)))
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
        (incf ambient (* fakeambient (mix color (aref colors i) ambient-mix)
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
        (incf ambient (* fakeambient (mix color (aref colors i) ambient-mix)
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
    (v! (+ final-color (* ambient ao)) 1)))

(defpipeline-g defer-pipe (:points)
  :fragment (defered-frag :vec2))

(defmethod blit ((scene scene) (postprocess list) (camera defered) time)
  "takes defered camera samplers and blits into PREV"
  (declare (ignore postprocess))
  (destructuring-bind (s1 s2 s3 s4 _) (sam camera)
    (declare (ignore _))
    (with-slots (prev bs) *state*
      (with-fbo-bound ((fbo prev))
        (clear-fbo (fbo prev));; needed for scenes with no envmap
        (with-blending (blend camera)
          (alexandria:when-let
              ((actor (find-if #'cube-p (actors scene))))
            (paint scene camera actor time))
          (map-g #'defer-pipe bs
                 :fakeambient (fakeambient camera)
                 :cam-pos (pos camera)
                 :scene (ubo scene)
                 ;; Samples
                 :sample1 s1
                 :sample2 s2
                 :sample3 s3
                 :sample4 s4
                 ;; Lights
                 :dirlights (dir-ubo *state*)
                 :spotlights (spot-ubo *state*)
                 :pointlights (point-ubo *state*)
                 ;; Shadows
                 :dirshadows (dir-sam *state*)
                 :spotshadows (spot-sam *state*)
                 :pointshadows (point-sam *state*)))))))
