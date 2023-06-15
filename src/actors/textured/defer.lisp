(in-package #:scenic)

(defun-g textured-frag ((uv          :vec2)
                        (frag-normal :vec3)
                        (frag-pos    :vec3)
                        (tbn         :mat3)
                        (tcam-pos    :vec3)
                        (tfrag-pos   :vec3)
                        &uniform
                        (cam-pos    :vec3)
                        (color      :vec3)
                        (dispscale  :float)
                        (material   :int)
                        (materials  pbr-material :ubo)
                        (aomap      :sampler-2d)
                        (albedo     :sampler-2d)
                        (dispmap    :sampler-2d)
                        (specmap    :sampler-2d)
                        (roughmap   :sampler-2d)
                        (normal-map :sampler-2d))
  (let* ((metallic  (aref (pbr-material-metallic materials) material))
         (emissive  0f0)
         ;;#+nil
         (uv        (parallax-mapping
                     uv
                     (normalize (- tcam-pos tfrag-pos))
                     dispmap
                     dispscale))
         ;;(color (* color (vec3 (x (pow (s~ (texture albedo uv) :xyz) (vec3 2.2))))))
         (color     (pow (s~ (texture albedo uv) :xyz) (vec3 2.2)))
         ;;(normal    (norm-from-map normal-map uv frag-pos frag-normal))
         ;;(normal frag-normal)
         (normal    (norm-from-map normal-map uv tbn))
         (roughness (x (texture roughmap uv)))
         (ao        (x (texture aomap    uv)))
         (spec      (x (texture specmap  uv))))
    (values (v! color     roughness)
            (v! frag-pos  ao)
            (v! normal    spec)
            (v! metallic  emissive))))

(defpipeline-g textured-pipe ()
  (vert-with-tbdata-defer g-pnt tb-data)
  (textured-frag :vec2 :vec3 :vec3 :mat3 :vec3 :vec3))

(defmethod paint (scene (camera defered) (actor textured-pbr) time)
  (with-slots (buf scale uv-repeat color dispscale material
               albedo aomap dispmap normal roughmap specmap)
      actor
    (map-g #'textured-pipe buf
           :color color
           :uv-repeat uv-repeat
           :scale scale
           :cam-pos (pos camera)
           :dispscale dispscale
           ;; Material
           :material material
           :materials (materials-ubo *state*)
           ;; Samplers
           :albedo albedo
           :aomap aomap
           :dispmap dispmap
           :normal-map normal
           :roughmap roughmap
           :specmap specmap
           ;; Matrices
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera))))
