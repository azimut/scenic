(in-package #:scenic)

(defmethod draw ((scene scene) (camera capture) time)
  (dolist (actor (actors scene))
    (with-slots (buf color) actor
      (map-g #'capture-pipe buf
             :color         color
             :world        (model->world actor)
             :scene        (ubo scene)
             :dirlights    (dir-ubo *state*)
             :spotlights   (spot-ubo *state*)
             :pointlights  (point-ubo *state*)
             :pointshadows (point-sam *state*)
             :projections  (ubo camera)))))

(defmethod draw :around ((scene scene-ibl) (camera capture) dt)
  (when (drawp camera)
    (call-next-method)
    (setf (drawp camera) NIL)))

(defmethod draw ((scene scene-ibl) (camera capture) time)
  (alexandria:when-let ((actor (find-if #'cube-p (actors scene))))
    (with-setf* ((depth-test-function) #'<=
                 (cull-face) :front
                 (depth-mask) NIL)
      (with-slots (buf) actor
        (map-g #'capture-cube-pipe buf
               :world (m4:identity)      ;; ??
               :projections (ubo camera) ;; ??
               :cube-sam (sam actor))))))
