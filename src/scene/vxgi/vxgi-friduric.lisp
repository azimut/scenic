(in-package #:scenic)

(defun-g trace-diffuse-voxel-cone ((from        :vec3)
                                   (direction   :vec3)
                                   (voxel-light :sampler-3d))
  "Traces a diffuse voxel cone."
  (let* ((voxel-size #.(/ 1f0 64f0))
         (mipmap-hardcap 5.4)
         (max-dist
           ;;(distance (abs from) (v3! -1))
           #.(sqrt 2)
           ;;#.(sqrt 3)
           )             ;F 1.414213=(sqrt 2);A 1.73205080757=(sqrt 3)
         ;;
         (aperture
           "0.325"
           ;;#.(max 0.1f0 (tan (* (radians 22f0) 0.5)))
           ;;".55785173935"
           ;;#.(clamp (* 3.14159265359 .5 .75 (max 0f0 .8)) 0.00174533102 3.14159265359)
           )        ;F=.325;A=.55785173935=(tan 22.5); AKA CONE_SPREAD
         ;;
         ;; Controls bleeding from close surfaces.
         ;; Low values look rather bad if using shadow cone tracing.
         ;; Might be a better choice to use shadow maps and lower this value.
         (dist
           ;;#.(* 1.5 (/ 1 64));; Shimmer
           "0.1953125"
           ;;(* 1 0.04)
           ;;(* 3.5f0 voxel-size)
           )        ; F .1953125 ; A 0.04 * voxelgiOffset("1"*100/100)
         (diam (* dist aperture))
         (direction (normalize direction))
         (acc (vec4 0f0)))
    ;; Trace
    (while (and (< dist max-dist)
                (< (w acc) 1f0))
           (let* ((c (scale-and-bias (+ from (* direction dist))))
                  (l (1+ (/ diam voxel-size)))
                  (level (log2 l))
                  (ll (* (1+ level) (1+ level)))
                  (voxel (texture-lod voxel-light
                                      c
                                      (min mipmap-hardcap level))))
             (incf acc
                   (* 0.075 ll voxel (pow (- 1f0 (w voxel)) 2f0)))
             (incf dist
                   (* ll voxel-size 2))))
    (v! (pow (* 2 (s~ acc :xyz))
             (vec3 1.5))
        (w acc))))

;; From Friduric
(defun-g orthogonal ((u :vec3))
  "Returns a vector that is orthogonal to u."
  (let ((u (normalize u))
        (v (v! "0.99146" "0.11664" "0.05832")))
    (if (> (abs (dot u v))
           "0.99999")
        (cross u (v! 0 1 0))
        (cross u v))))

;; From Armory (used instead of orthogonal)
(defun-g tangential ((n :vec3))
  (let ((t1 (cross n (v! 0 0 1)))
        (t2 (cross n (v! 0 1 0))))
    (if (> (length t1) (length t2))
        (normalize t1)
        (normalize t2))))

(defun-g indirect-diffuse-light ((wpos        :vec3)
                                 (normal      :vec3)
                                 (voxel-light :sampler-3d)
                                 (albedo      :vec3))
  (let* ((isqrt2                  #.(/ (sqrt 2) 2)) ; 0.7071
         (diffuse-indirect-factor 0.52)
         ;; Angle mix (1.0f => orthogonal direction,
         ;;            0.0f => direction of normal).
         (angle-mix               0.5)
         (w                       (vec3 1)) ; cone weights
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
         (acc                     (vec4 0))
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
    (v! (* diffuse-indirect-factor
           (s~ acc :xyz)
           (+ albedo 0.001))
        (w acc))
    ;;(* (+ albedo .0001) (/ acc 9))
    ))
;;----------------------------------------

(defun-g trace-specular-voxel-cone ((from        :vec3)
                                    (normal      :vec3)
                                    (direction   :vec3)
                                    (spec        :float)
                                    (voxel-light :sampler-3d))
  (let* ((max-distance (distance (abs from) (vec3 -1f0)))
         (voxel-size (/ (x (texture-size voxel-light 0))))
         (step      voxel-size)
         (mipmap-hardcap 5.4)
         (offset    (* 8 voxel-size))
         (from      (+ from (* normal offset)))
         (acc       (vec4 0f0))
         (dist      offset))
    (while (and (< dist max-distance)
                (< (w acc) 1f0))
           (let ((c (+ from (* dist direction))))
             #+nil
             (if (not (inside-cube-p c))
                 (break))
             (setf c (scale-and-bias c))
             (let* ((level (* .1 spec (log2 (+ 1f0 (/ dist voxel-size)))))
                    (voxel (texture-lod voxel-light c (min level mipmap-hardcap)))
                    (f (- 1 (w acc))))
               (incf (s~ acc :xyz) (* .25 (+ spec) (s~ voxel :xyz) (w voxel) f))
               (incf (w acc)       (* .25 (w voxel) f))
               (incf dist          (* step (+ 1f0 (* .125 level)))))))
    (* (pow (+ 1 spec) .8)
       (s~ acc :xyz))))
