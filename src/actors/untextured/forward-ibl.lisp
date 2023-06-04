(in-package #:scenic)

(defun-g untextured-ibl-frag ((uv         :vec2)
                              (frag-norm  :vec3)
                              (frag-pos   :vec3)
                              (dir-pos   (:vec4 2))
                              (spot-pos  (:vec4 2))
                              &uniform
                              (brdf         :sampler-2d)
                              (prefilter    :sampler-cube)
                              (irradiance   :sampler-cube)
                              (material     :int)
                              (materials    pbr-material        :ubo)
                              (dirlights    dir-light-data      :ubo)
                              (pointlights  point-light-data    :ubo)
                              (spotlights   spot-light-data     :ubo)
                              (scene        scene-data          :ubo)
                              (cam-pos      :vec3)
                              (time         :float)
                              (color        :vec3)
                              (dirshadows   :sampler-2d-array)
                              (spotshadows  :sampler-2d-array)
                              (pointshadows :sampler-cube-array))
  (let ((final-color (v! 0 0 0))
        ;;#+nil
        (ambient (ambient-ibl (normalize (- cam-pos frag-pos))
                              frag-norm
                              irradiance
                              (aref (pbr-material-roughness materials) material)
                              (aref (pbr-material-metallic materials) material)
                              color
                              .1f0))
        #+nil
        (ambient (ambient-ibl (normalize (- cam-pos frag-pos))
                              frag-norm
                              brdf
                              prefilter
                              irradiance
                              (aref (pbr-material-roughness materials) material)
                              (aref (pbr-material-metallic materials) material)
                              color
                              1f0)))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions fudge)
          dirlights
        (incf final-color
              (* (pbr-direct-lum (aref positions i) frag-pos cam-pos frag-norm
                                 (aref (pbr-material-roughness materials) material)
                                 (aref (pbr-material-metallic materials) material)
                                 color
                                 (aref (pbr-material-specular materials) material)
                                 (aref colors i))
                 (shadow-factor dirshadows (aref dir-pos i) (aref fudge i) i)))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf final-color
              (* (pbr-point-lum (aref positions i) frag-pos cam-pos
                                frag-norm
                                (aref (pbr-material-roughness materials) material)
                                (aref (pbr-material-metallic materials) material)
                                color
                                (aref (pbr-material-specular materials) material)
                                (aref linear i) (aref quadratic i) (aref colors i))
                 (shadow-factor pointshadows
                                frag-pos
                                (aref positions i)
                                (aref far i)
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic cutoff outer-cutoff direction fudge)
          spotlights
        (incf final-color
              (* (pbr-spot-lum (aref positions i) frag-pos cam-pos
                               frag-norm
                               (aref (pbr-material-roughness materials) material)
                               (aref (pbr-material-metallic materials) material)
                               color
                               (aref (pbr-material-specular materials) material)
                               (aref colors i)
                               (aref direction i)
                               (aref cutoff i)
                               (aref outer-cutoff i)
                               (aref linear i)
                               (aref quadratic i))
                 (shadow-factor spotshadows (aref spot-pos i) (aref fudge i) i)))))
    (v! (+ final-color ambient) 1)))

(defpipeline-g untextured-ibl-pipe ()
  :vertex (untextured-vert g-pnt)
  :fragment (untextured-ibl-frag :vec2 :vec3 :vec3 (:vec4 2) (:vec4 2)))

(defmethod paint ((scene scene-ibl) (actor untextured) (camera renderable) time)
  (with-slots (buf scale color material) actor
    (map-g #'untextured-ibl-pipe buf
           :time time
           :scene (ubo scene)
           :cam-pos (pos camera)
           :scale scale
           :color color
           :material material
           :materials (materials-ubo *state*)
           ;; IBL
           :brdf (brdf-sam *state*)
           :prefilter (first (sam (prefilter scene)))
           :irradiance (first (sam (irradiance scene)))
           ;; Matrices
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           ;; Shadows
           :dirshadows (dir-sam *state*)
           :pointshadows (point-sam *state*)
           :spotshadows (spot-sam *state*)
           ;; Lights
           :dirlights (dir-ubo *state*)
           :pointlights (point-ubo *state*)
           :spotlights (spot-ubo *state*))))
