(in-package #:scenic)

(defclass textured (actor)
  ((albedo     :initarg :albedo)
   (normal     :initarg :normal)
   (roughmap   :initarg :roughmap)
   (specmap    :initarg :specmap)
   (aomap      :initarg :aomap)
   (uv-repeat  :initarg :uv-repeat))
  (:default-initargs
   :aomap    (get-tex "static/2k_wall/8/white_plaster_02_ao_2k.png"    NIL T :r8)
   :albedo   (get-tex "static/2k_wall/8/white_plaster_02_diff_2k.png"  NIL T :rgb8)
   :normal   (get-tex "static/2k_wall/8/white_plaster_02_nor_2k.png"   NIL T :rgb8)
   :specmap  (get-tex "static/2k_wall/8/white_plaster_02_spec_2k.png"  NIL T :r8)
   :roughmap (get-tex "static/2k_wall/8/white_plaster_02_rough_2k.png" NIL T :r8)
   :uv-repeat (v! 1 1)
   :buf (box 1f0 1f0 1f0 t)))

(defmethod initialize-instance :before ((obj textured) &key albedo normal roughmap aomap specmap)
  (check-type roughmap %cepl.types:sampler)
  (check-type albedo %cepl.types:sampler)
  (check-type normal %cepl.types:sampler)
  (check-type aomap %cepl.types:sampler)
  (check-type specmap %cepl.types:sampler))

(defun make-textured (&rest args)
  (apply #'make-instance 'textured args))

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
  (let (;;(color (* color (vec3 (x (pow (s~ (texture albedo uv) :xyz) (vec3 2.2))))))
        (color     (pow (s~ (texture albedo uv) :xyz) (vec3 2.2)))
        (normal    (norm-from-map normal-map uv tbn))
        (roughness (x (texture roughmap uv)))
        (ao        (x (texture aomap uv)))
        (spec (x (texture specmap uv))))
    (values (v! color roughness)
            (v! frag-pos ao)
            (v! normal spec)
            (v! .01 0))))

(defpipeline-g textured-pipe ()
  (vert-with-tbdata-defer g-pnt tb-data)
  (textured-frag :vec2 :vec3 :vec3 :mat3 :vec3 :vec3))

(defmethod paint (scene (actor textured) (camera defered) time)
  (with-slots (buf scale uv-repeat albedo normal aomap color) actor
    (map-g #'textured-pipe buf
           :color color
           :aomap aomap
           :albedo albedo
           :normal-map normal
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :uv-repeat uv-repeat
           :scale scale)))

