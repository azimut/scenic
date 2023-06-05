(in-package #:scenic)

(defun-g textured-frag ((uv          :vec2)
                        (frag-normal :vec3)
                        (frag-pos    :vec3)
                        (tbn         :mat3)
                        (tcam-pos    :vec3)
                        (tfrag-pos   :vec3)
                        &uniform
                        (color      :vec3)
                        (aomap      :sampler-2d)
                        (albedo     :sampler-2d)
                        (specmap    :sampler-2d)
                        (roughmap   :sampler-2d)
                        (normal-map :sampler-2d))
  (let* (;;(color (* color (vec3 (x (pow (s~ (texture albedo uv) :xyz) (vec3 2.2))))))
         (color     (pow (s~ (texture albedo uv) :xyz) (vec3 2.2)))
         (roughness (x (texture roughmap uv)))
         (normal    (norm-from-map normal-map uv tbn))
         ;;(normal    (norm-from-map normal-map uv frag-pos frag-normal))
         ;;(normal frag-normal)
         (metallic  .04f0)
         ;; (metallic  0f0)
         (emissive  0f0)
         (ao        (x (texture aomap    uv)))
         (spec      (x (texture specmap  uv))))
    (values (v! color     roughness)
            (v! frag-pos  ao)
            (v! normal    spec)
            (v! metallic  emissive))))

(defpipeline-g textured-pipe ()
  (vert-with-tbdata-defer g-pnt tb-data)
  (textured-frag :vec2 :vec3 :vec3 :mat3 :vec3 :vec3))

(defmethod paint (scene (camera defered) (actor textured) time)
  (with-slots (buf scale uv-repeat specmap roughmap albedo normal aomap color) actor
    (map-g #'textured-pipe buf
           :color color
           :roughmap roughmap
           :specmap specmap
           :aomap aomap
           :albedo albedo
           :normal-map normal
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :uv-repeat uv-repeat
           :scale scale)))
