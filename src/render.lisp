(in-package #:scenic)

;;--------------------------------------------------
;; 3D - g-pnt mesh without tangents

(defun-g vert ((vert g-pnt) &uniform
               (model-world :mat4)
               (world-view  :mat4)
               (view-clip   :mat4)
               (scale       :float))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz))))

