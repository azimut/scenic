(in-package #:scenic)

(defun-g indirect-diffuse-light ((wpos        :vec3)
                                 (normal      :vec3)
                                 (voxel-light :sampler-3d)
                                 (albedo      :vec3))
  (let* ((acc   (vec3 0))
         ;; Rotate cone around the normal
         (guide (if (= 1 (abs (dot normal (vec3 0 1 0))))
                    (vec3 0 0 1)
                    (vec3 0 1 0)))
         ;; Find a tangent and bitangent
         (right (normalize (- guide (* normal (dot normal guide)))))
         (up    (cross right normal))
         (cone-directions
           (vector (v! 0.0 1.0 0.0)
                   (v! 0.0 0.5 0.866025)
                   (v! 0.823639 0.5 0.267617)
                   (v! 0.509037 0.5 -0.7006629)
                   (v! -0.50937 0.5 -0.7006629)
                   (v! -0.823639 0.5 0.267617)))
         (cone-weights
           (vector #.(/ +PI+ 4.0f0)
                   #.(/ (* 3.0f0 +PI+) 20.0f0)
                   #.(/ (* 3.0f0 +PI+) 20.0f0)
                   #.(/ (* 3.0f0 +PI+) 20.0f0)
                   #.(/ (* 3.0f0 +PI+) 20.0f0)
                   #.(/ (* 3.0f0 +PI+) 20.0f0))))
    (dotimes (i 6)
      (let ((cone-direction
              (normalize
               (+ normal; ?
                  (* (x (aref cone-directions i)) right)
                  (* (z (aref cone-directions i)) up))))
            (start-clip-pos
              (+ wpos ; ?
                 (/ 1f0 64f0)
                 (* normal              ; ?
                    "0.1953125"
                    ))))

        (incf acc
              (* (aref cone-weights i)
                 (trace-diffuse-voxel-cone start-clip-pos
                                           cone-direction
                                           voxel-light)))))
    acc)
  #+nil
  (let* ((isqrt2                  #.(/ (sqrt 2f0) 2f0)) ; 0.7071
         (diffuse-indirect-factor .52)
         ;; Angle mix (1.0f => orthogonal direction,
         ;;            0.0f => direction of normal).
         (angle-mix               0.5)
         (w                       (v! 1 1 1)) ; cone weights
         ;; Find a base for the side cones with the normal as one
         ;; of its base vectors.
         (ortho                   (normalize (orthogonal normal)))
         (ortho2                  (normalize (cross ortho normal)))
         ;; Find base vectors for the corner cones too.
         (corner                  (* 0.5 (+ ortho ortho2)))
         (corner2                 (* 0.5 (- ortho ortho2)))
         ;; Find start position of trace (start with a bit of offset).
         (voxel-size              #.(/ 1f0 64f0))
         (n-offset                (* normal (+ 1 (* 4 isqrt2)) voxel-size))
         (c-origin                (+ wpos n-offset))
         ;; Accumulate indirect diffuse light.
         (acc                     (vec3 0))
         ;; We offset forward in normal direction, and backward in cone direction.
         ;; Backward in cone direction improves GI, and forward direction removes
         ;; artifacts.
         (cone-offset             "-0.01"))
    ;; Trace front cone
    (incf acc (* (x w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset normal))
                                                 normal
                                                 voxel-light)))
    ;; Trace 4 side cones.
    (incf acc (* (y w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset ortho))
                                                 (mix normal ortho angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset ortho))
                                                 (mix normal (- ortho) angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset ortho2))
                                                 (mix normal ortho2 angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset ortho2))
                                                 (mix normal (- ortho2) angle-mix)
                                                 voxel-light)))
    ;; Trace 4 corner cones.
    (incf acc (* (z w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset corner))
                                                 (mix normal corner angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset corner))
                                                 (mix normal (- corner) angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset corner2))
                                                 (mix normal corner2 angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset corner2))
                                                 (mix normal (- corner2) angle-mix)
                                                 voxel-light)))
    ;; Return result.
    (* acc diffuse-indirect-factor (+ albedo 0.001))
    ;;(* (+ albedo .0001) (/ acc 9))
    ))
