(in-package #:scenic)

(defun-g trace-specular-voxel-cone
    ((from        :vec3)
     (normal      :vec3)
     (direction   :vec3)
     (spec        :float)
     (voxel-light :sampler-3d))
  (let* ((aperture   #.(max 0.1 (tan (radians 5f0)))) ;; !!! parameter 5°
         (voxel-resolution (x (texture-size voxel-light 0)))
         (voxel-size (/ voxel-resolution))
         (distance   (* 8 voxel-size));; aka offset ;; !!! parameter 8
         (from       (+ from (* distance normal)))
         (acc-color  (vec3 0))
         (acc-occlusion 0f0)
         (sampling-factor 1f0);; !! parameter 0-1
         (distance-max  2f0))
    (while (and (<= distance distance-max)
                (< acc-occlusion 1f0))
           (let* ((cone-voxelgrid-pos
                    (scale-and-bias (+ from (* direction distance))))
                  (diameter (* 2 aperture distance))
                  (mipmap-level (log2 (* diameter voxel-resolution)))
                  (voxel-sample (texture-lod voxel-light
                                             cone-voxelgrid-pos
                                             (min mipmap-level 6))))
             ;; front to back composition
             (incf acc-color     (* (- 1f0 acc-occlusion) (s~ voxel-sample :xyz)))
             (incf acc-occlusion (* (- 1f0 acc-occlusion) (w voxel-sample)))
             (incf distance (* diameter sampling-factor))))
    acc-color))

(defun-g indirect-specular-light ((frag-pos       :vec3)
                                  (voxe-pos       :vec3)
                                  (frag-nor       :vec3)
                                  (cam-pos        :vec3)
                                  (spec           :float)
                                  (voxel-light    :sampler-3d)
                                  (albedo         :vec3)
                                  (metallic       :float))
  (let* ((direction  (normalize (- frag-pos cam-pos)))
         (reflection (normalize (reflect direction frag-nor))))
    (* metallic
       albedo
       (trace-specular-voxel-cone voxe-pos
                                  frag-nor
                                  reflection
                                  spec
                                  voxel-light))))

(defun-g trace-cone
    ((from           :vec3)
     (normal         :vec3)
     (direction      :vec3)
     (voxel-light    :sampler-3d)
     (aperture-angle :float))
  (let* ((voxel-resolution (x (texture-size voxel-light 0)))
         (voxel-size (/ voxel-resolution))
         (aperture (max 0.1 (tan (radians aperture-angle)))) ;; !!! parameter 5°
         (distance   (* 8 voxel-size)) ;; aka offset ;; !!! parameter 8
         (sampling-factor 1f0);; !! parameter 0-1
         (distance-max  2f0);; !! parameter
         (acc-color (vec3 0))
         (acc-occlusion 0f0))
    (while (and (<= distance distance-max)
                (< acc-occlusion 1f0))
           (let* ((cone-voxelgrid-pos (scale-and-bias (+ from (* direction distance))))
                  (diameter (* 2 aperture distance))
                  (mipmap-level (log2 (* diameter voxel-resolution)))
                  (voxel-sample (texture-lod voxel-light
                                             cone-voxelgrid-pos
                                             (min mipmap-level 6))));; !! param 6
             ;; front to back composition
             (incf acc-color     (* (- 1 acc-occlusion) (s~ voxel-sample :xyz)))
             (incf acc-occlusion (* (- 1 acc-occlusion) (w voxel-sample)))
             (incf distance (* diameter sampling-factor))))
    (v! acc-color (min acc-occlusion 1f0))))

(defun-g indirect-diffuse-light ((vpos        :vec3)
                                 (normal      :vec3)
                                 (voxel-light :sampler-3d)
                                 (albedo      :vec3))
  (let* ((acc-color (vec4 0))
         ;; rotate cone around the normal
         (guide     (if (= 1f0 (abs (dot normal (v! 0 1 0))))
                        (v! 0 0 1)
                        (v! 0 1 0)))
         ;; find a tangent and a bitangent
         (right      (normalize
                      (- guide (* (dot normal guide) normal))))
         (up         (cross right normal))
         (directions (vector (v!  0.0      1.0  0.0)
                             (v!  0.0      0.5  0.866025)
                             (v!  0.823639 0.5  0.267617)
                             (v!  0.509037 0.5 -0.7006629)
                             (v! -0.50937  0.5 -0.7006629)
                             (v! -0.823639 0.5  0.267617)))
         (weights    (vector #.(/ +PI+ 4)
                             #.(* 3 (/ +PI+ 20))
                             #.(* 3 (/ +PI+ 20))
                             #.(* 3 (/ +PI+ 20))
                             #.(* 3 (/ +PI+ 20))
                             #.(* 3 (/ +PI+ 20)))))
    (dotimes (i 6)
      (let ((cone-direction
              (normalize
               (+ (* right (x (aref directions i)))
                  (* up    (z (aref directions i)))
                  normal)))
            (start-clip-pos
              (+ vpos (* normal
                         (* 8 (/ 1 64f0))
                         ;;(/ .81 64)
                         ))))
        (incf acc-color (* (aref weights i)
                           (trace-cone start-clip-pos
                                       normal
                                       cone-direction
                                       voxel-light
                                       60f0)))))
    (v! (* (s~ acc-color :xyz)
           albedo)
        (clamp (w acc-color) 0 1))))
