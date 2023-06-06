(in-package #:scenic)

(defmethod paint (scene (camera capture) actor time)
  (with-slots (buf color) actor
    (map-g #'capture-pipe buf
           :world        (model->world actor)
           :color         color
           :scene        (ubo scene)
           :projections  (ubo camera)
           ;; Lights
           :dirlights    (dir-ubo *state*)
           :spotlights   (spot-ubo *state*)
           :pointlights  (point-ubo *state*)
           ;; Shadows
           :pointshadows (point-sam *state*))))
