(in-package #:scenic)

(defclass textured-albedo-only (textured assimp-thing-with-bones albedoed)
  ())

(defun-g textured-albedo-only-frag ((uv          :vec2)
                                    (frag-normal :vec3)
                                    (frag-pos    :vec3)
                                    (tbn         :mat3)
                                    (tcam-pos    :vec3)
                                    (tfrag-pos   :vec3)
                                    &uniform
                                    (cam-pos    :vec3)
                                    (color      :vec3)
                                    (material   :int)
                                    (materials  pbr-material :ubo)
                                    (albedo     :sampler-2d))
  (let* ((metallic  (aref (pbr-material-metallic materials) material))
         (emissive  (aref (pbr-material-emissive materials) material))
         ;;(color (* color (vec3 (x (pow (s~ (texture albedo uv) :xyz) (vec3 2.2))))))
         (color     (pow (s~ (texture albedo uv) :xyz) (vec3 2.2)))
         (normal frag-normal)
         (roughness (aref (pbr-material-roughness  materials) material))
         (ao        (aref (pbr-material-aocclusion materials) material))
         (spec      (aref (pbr-material-specular   materials) material)))
    (values (v! color     roughness)
            (v! frag-pos  ao)
            (v! normal    spec)
            (v! metallic  emissive))))

(defpipeline-g textured-albedo-only-bone-pipe ()
  (vert-with-tbdata-defer-bones g-pnt tb-data assimp-bones)
  (textured-albedo-only-frag :vec2 :vec3 :vec3 :mat3 :vec3 :vec3))

(defmethod paint (scene (camera defered) (actor textured-albedo-only) time)
  (with-slots (buf scale albedo color bones material) actor
    (map-g #'textured-albedo-only-bone-pipe buf
           :color color
           :offsets bones
           :scale scale
           ;; Sampler
           :albedo albedo
           ;; Matrices
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           ;; Material
           :material material
           :materials (materials-ubo *state*))))

;; ----------------------------------------

(defclass textured-asn (textured assimp-thing-with-bones albedoed speced normaled)
  ())

(defun-g textured-asn-frag
    ((uv          :vec2)
     (frag-normal :vec3)
     (frag-pos    :vec3)
     (tbn         :mat3)
     (tcam-pos    :vec3)
     (tfrag-pos   :vec3)
     &uniform
     (cam-pos    :vec3)
     (color      :vec3)
     (material   :int)
     (materials  pbr-material :ubo)
     (albedo     :sampler-2d)
     (specmap    :sampler-2d)
     (normal-map :sampler-2d))
  (let* ((metallic  (aref (pbr-material-metallic materials) material))
         (emissive  (aref (pbr-material-emissive materials) material))
         ;;(color (* color (vec3 (x (pow (s~ (texture albedo uv) :xyz) (vec3 2.2))))))
         (color     (pow (s~ (texture albedo uv) :xyz) (vec3 2.2)))
         ;;(normal     frag-normal)
         ;;(normal    (norm-from-map normal-map uv tbn))
         (normal    (norm-from-map normal-map uv frag-pos frag-normal))
         (roughness (aref (pbr-material-roughness  materials) material))
         (ao        (aref (pbr-material-aocclusion materials) material))
         (spec      (x (texture specmap uv))))
    (values (v! color     roughness)
            (v! frag-pos  ao)
            (v! normal    spec)
            (v! metallic  emissive))))

(defpipeline-g textured-asn-bones-pipe ()
  (vert-with-tbdata-defer-bones g-pnt tb-data assimp-bones)
  (textured-asn-frag :vec2 :vec3 :vec3 :mat3 :vec3 :vec3))

(defmethod paint (scene (camera defered) (actor textured-asn) time)
  (with-slots (buf scale albedo color bones material specmap normal) actor
    (map-g #'textured-asn-bones-pipe buf
           :color color
           :offsets bones
           :scale scale
           ;; Sampler
           :albedo albedo
           :specmap specmap
           :normal-map normal
           ;; Matrices
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           ;; Material
           :material material
           :materials (materials-ubo *state*))))
