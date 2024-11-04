(in-package #:scenic)

(defun-g untextured-defered-vert ((vert g-pnt) &uniform
                                  (model-world :mat4)
                                  (world-view  :mat4)
                                  (view-clip   :mat4)
                                  (scale       :float))
  (let* ((pos        (* scale (pos vert)))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos))
         (tex        (tex vert))
         (norm       (norm vert))
         (world-norm (* (m4:to-mat3 model-world) norm)))
    (values clip-pos tex world-norm (s~ world-pos :xyz))))

(defun-g untextured-defered-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                                  &uniform
                                  (color     :vec3)
                                  (material  :int)
                                  (materials pbr-material :ubo))
  (with-slots (roughness specular metallic emissive aocclusion) materials
    (let ((ao        (aref aocclusion material))
          (roughness (aref roughness  material))
          (specular  (aref specular   material))
          (metallic  (aref metallic   material))
          (emissive  (aref emissive   material)))
      (values (v! color     roughness)
              (v! frag-pos  ao)
              (v! frag-norm specular)
              (v! metallic
                  emissive)))))

(defpipeline-g untextured-defered-pipe ()
  :vertex (untextured-defered-vert g-pnt)
  :fragment (untextured-defered-frag :vec2 :vec3 :vec3))

(defmethod paint (scene (camera defered) (actor untextured) time)
  (with-slots (buf scale color material) actor
    (map-g #'untextured-defered-pipe buf
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale
           :color color
           :material material
           :materials (materials-ubo *state*))))
