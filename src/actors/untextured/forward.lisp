(in-package #:scenic)

(defun-g untextured-vert ((vert g-pnt) &uniform
                          (model-world :mat4)
                          (world-view  :mat4)
                          (view-clip   :mat4)
                          (scale       :float)
                          (scene       scene-data      :ubo)
                          (dirlights   dir-light-data  :ubo)
                          (spotlights  spot-light-data :ubo))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos))
         (dir-pos    (vector (v! 0 0 0 0) (v! 0 0 0 0)))
         (spot-pos   (vector (v! 0 0 0 0) (v! 0 0 0 0))))
    (dotimes (i (scene-data-ndir scene))
      (setf (aref dir-pos i)
            (* (aref (lightspace dirlights) i) world-pos)))
    (dotimes (i (scene-data-nspot scene))
      (setf (aref spot-pos i)
            (* (aref (lightspace spotlights) i) world-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz)
            dir-pos
            spot-pos)))

(defun-g untextured-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                          (dir-pos   (:vec4 2))
                          (spot-pos  (:vec4 2))
                          &uniform
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
  (let ((final-color (v! 0 0 0)))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions fudge) dirlights
        (incf final-color
              (* (pbr-direct-lum (aref positions i) frag-pos cam-pos frag-norm
                                 (aref (pbr-material-roughness materials) material)
                                 (aref (pbr-material-metallic materials) material)
                                 color
                                 (aref (pbr-material-specular materials) material)
                                 (aref colors i))
                 (shadow-factor dirshadows (aref dir-pos i) (aref fudge i) i)))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge) pointlights
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
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction fudge) spotlights
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
    (v! final-color 1)))

(defpipeline-g untextured-pipe ()
  :vertex (untextured-vert g-pnt)
  :fragment (untextured-frag :vec2 :vec3 :vec3 (:vec4 2) (:vec4 2)))

(defmethod paint (scene (actor untextured) (camera renderable) time)
  (with-slots (buf scale color material) actor
    (map-g #'untextured-pipe buf
           :scene (ubo scene)
           :cam-pos (pos camera)
           :dirshadows (dir-sam *state*)
           :pointshadows (point-sam *state*)
           :spotshadows (spot-sam *state*)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale
           :color color
           :material material
           :materials (materials-ubo *state*)
           :dirlights (dir-ubo *state*)
           :pointlights (point-ubo *state*)
           :spotlights (spot-ubo *state*)
           :time time)))
