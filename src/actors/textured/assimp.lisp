(in-package #:scenic)

(defpipeline-g textured-bone-pipe ()
  (vert-with-tbdata-defer-bones g-pnt tb-data assimp-bones)
  (textured-frag :vec2 :vec3 :vec3 :mat3 :vec3 :vec3))

(defmethod paint (scene (camera defered) (actor assimp-thing-with-bones) time)
  (with-slots (buf scale albedo normal specmap aomap color bones) actor
    (map-g #'textured-bone-pipe buf
           :specmap specmap
           :color color
           :aomap aomap
           :albedo albedo
           :offsets bones
           :normal-map normal
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           :scale scale)))
