(in-package #:scenic)

(defun-g untextured-defered-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                                  &uniform
                                  (color     :vec3)
                                  (material  :int)
                                  (materials pbr-material :ubo))
  (with-slots (roughness specular metallic emissive) materials
    (let ((ao 1f0))
      (values (v! color (aref roughness material))
              (v! frag-pos ao)
              (v! frag-norm (aref specular material))
              (v! (aref metallic material) (aref emissive material))))))

(defpipeline-g untextured-defered-pipe ()
  :vertex (vert g-pnt)
  :fragment (untextured-defered-frag :vec2 :vec3 :vec3))

(defmethod paint (scene (actor untextured) (camera defered) time)
  (with-slots (buf scale color material) actor
    (map-g #'untextured-defered-pipe buf
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale
           :color color
           :material material
           :materials (materials-ubo *state*))))


