(in-package #:scenic)

(defmethod draw ((scene scene) (camera capture) time)
  (log4cl:log-info camera)
  (dolist (actor (actors scene))
    (paint scene camera actor time)))

(defmethod draw
    :around ((scene scene-ibl) (camera capture) dt)
  (when (drawp camera)
    (log4cl:log-info camera)
    (call-next-method)
    (setf (drawp camera) NIL)))

(defmethod draw ((scene scene-ibl) (camera capture) time)
  (alexandria:when-let ((actor (find-if #'cube-p (actors scene))))
    (paint scene camera actor time)))
