(in-package #:scenic)

(defun-g textured-forward-frag
    ((uv              :vec2)
     (frag-norm       :vec3)
     (frag-pos        :vec3)
     (tbn             :mat3)
     (dir-pos        (:vec4 2))
     (spot-pos       (:vec4 2))
     (tan-dir-pos    (:vec3 2))
     (tan-spot-pos   (:vec3 2))
     (tan-point-pos  (:vec3 4))
     (tan-cam-pos     :vec3)
     (tan-frag-pos    :vec3)
     &uniform
     (material     :int)
     (materials    pbr-material        :ubo)
     (scene        scene-data          :ubo)
     (cam-pos      :vec3)
     (time         :float)
     (dispscale    :float)
     ;; Samplers
     (albedo    :sampler-2d)
     (roughmap  :sampler-2d)
     (normalmap :sampler-2d)
     (specmap   :sampler-2d)
     (aomap     :sampler-2d)
     (dispmap   :sampler-2d)
     ;; Lights
     (dirlights    dir-light-data      :ubo)
     (pointlights  point-light-data    :ubo)
     (spotlights   spot-light-data     :ubo)
     ;; Shadows
     (dirshadows   :sampler-2d-array)
     (spotshadows  :sampler-2d-array)
     (pointshadows :sampler-cube-array))
  (let* ((uv          (parallax-mapping
                       uv
                       (normalize (- tan-cam-pos tan-frag-pos))
                       dispmap
                       dispscale))
         (color       (pow (s~ (texture albedo uv) :xyz) (vec3 2.2)))
         (roughness   (x (texture roughmap uv)))
         (ao          (x (texture aomap    uv)))
         (spec        (x (texture specmap  uv)))
         (fakeambient (aref (pbr-material-fakeambient materials) material))
         (metallic    (aref (pbr-material-metallic    materials) material))
         (normal      (norm-from-map normalmap uv tbn))
         ;;(normal      (norm-from-map normalmap uv frag-pos frag-norm))
         ;;(normal      frag-norm)
         (ambient     (vec3 0))
         (ambient-mix .25)
         (final-color (v! 0 0 0)))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions fudge)
          dirlights
        (incf ambient (* fakeambient (mix color (aref colors i) ambient-mix)))
        (incf final-color
              (* (pbr-direct-lum (aref positions i) frag-pos cam-pos normal
                                 roughness
                                 metallic
                                 color
                                 spec
                                 (aref colors i))
                 (shadow-factor dirshadows (aref dir-pos i) (aref fudge i) i)))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf ambient (* fakeambient (mix color (aref colors i) ambient-mix)
                         (point-light-attenuation
                          (aref linear i)
                          (aref quadratic i)
                          (aref positions i)
                          frag-pos)))
        (incf final-color
              (* (pbr-point-lum (aref positions i) frag-pos cam-pos normal
                                roughness
                                metallic
                                color
                                spec
                                (aref linear    i)
                                (aref quadratic i)
                                (aref colors    i))
                 (shadow-factor pointshadows
                                frag-pos
                                (aref positions i)
                                (aref far i)
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic cutoff outer-cutoff direction fudge)
          spotlights
        (incf ambient (* fakeambient (mix color (aref colors i) ambient-mix)
                         (point-light-attenuation
                          (aref linear i)
                          (aref quadratic i)
                          (aref positions i)
                          frag-pos)))
        (incf final-color
              (* (pbr-spot-lum (aref positions i) frag-pos cam-pos normal
                               roughness
                               metallic
                               color
                               spec
                               (aref colors       i)
                               (aref direction    i)
                               (aref cutoff       i)
                               (aref outer-cutoff i)
                               (aref linear       i)
                               (aref quadratic    i))
                 (shadow-factor spotshadows
                                (aref spot-pos i)
                                (aref fudge    i)
                                i)))))
    (v! (+ final-color (* ambient ao)) 1)))

(defpipeline-g textured-forward-pipe ()
  (vert-with-tbdata g-pnt tb-data)
  (textured-forward-frag
   :vec2 :vec3 :vec3
   :mat3
   (:vec4 2) (:vec4 2)
   (:vec3 2) (:vec3 2) (:vec3 4)
   :vec3 :vec3))

(defmethod paint (scene (camera forward) (actor textured-pbr) time)
  (with-slots (buf material scale uv-repeat dispscale
               albedo normal aomap roughmap specmap dispmap)
      actor
    (map-g #'textured-forward-pipe buf
           :dispscale dispscale
           :uv-repeat uv-repeat
           :scale scale
           :scene (ubo scene)
           :cam-pos (pos camera)
           ;; Material
           :material material
           :materials (materials-ubo *state*)
           ;; Shadows
           :dirshadows (dir-sam *state*)
           :spotshadows (spot-sam *state*)
           :pointshadows (point-sam *state*)
           ;; Lights
           :dirlights (dir-ubo *state*)
           :spotlights (spot-ubo *state*)
           :pointlights (point-ubo *state*)
           ;; Samplers
           :albedo albedo
           :aomap aomap
           :dispmap dispmap
           :normalmap normal
           :roughmap roughmap
           :specmap specmap
           ;; Matrices
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera))))
