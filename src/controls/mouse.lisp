(in-package #:scenic)

(defun mouse-spin (camera)
  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:* (q:from-axis-angle *vec3-right* (- (y move)))
                        (q:from-axis-angle *vec3-up*    (- (x move)))))))))))
