(in-package #:scenic)
;; ?
;; From "pushing pixels" don't remember why it's needed
(defun-g treat-uvs ((uv :vec2))
  (v! (x uv) (- 1.0 (y uv))))

(defun-g vert-with-tbdata ((vert g-pnt) (tb tb-data) &uniform
                           (model-world :mat4)
                           (world-view  :mat4)
                           (view-clip   :mat4)
                           (scale       :float)
                           (uv-repeat   :vec2)
                           (cam-pos     :vec3)
                           (scene       scene-data       :ubo)
                           (dirlights   dir-light-data   :ubo)
                           (pointlights point-light-data :ubo)
                           (spotlights  spot-light-data  :ubo))
  (let* ((pos       (* scale       (pos vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         (uv        (* uv-repeat (tex vert)))
         (normal-m3 (transpose (inverse (m4:to-mat3 model-world))))
         (norm      (norm vert))
         (norm      (* (m4:to-mat3 model-world)
                       norm))
         (t0  (normalize (* normal-m3 (tb-data-tangent tb))))
         (n0  (normalize (* normal-m3 (norm vert))))
         (t0  (normalize (- t0 (* (dot t0 n0) n0))))
         (b0  (cross n0 t0))
         (tbn (mat3 t0 b0 n0))
         (dir-pos       (vector (v! 0 0 0 0) (v! 0 0 0 0)))
         (spot-pos      (vector (v! 0 0 0 0) (v! 0 0 0 0)))
         (tan-dir-pos   (vector (v! 0 0 0) (v! 0 0 0)))
         (tan-spot-pos  (vector (v! 0 0 0) (v! 0 0 0)))
         (tan-point-pos (vector (v! 0 0 0) (v! 0 0 0) (v! 0 0 0) (v! 0 0 0))))
    (dotimes (i (scene-data-ndir scene))
      (setf (aref dir-pos i) (*  (aref (lightspace dirlights) i) world-pos)))
    (dotimes (i (scene-data-nspot scene))
      (setf (aref spot-pos i) (*  (aref (lightspace spotlights) i) world-pos)))
    (dotimes (i (scene-data-ndir scene))
      (setf (aref tan-dir-pos i) (* tbn (aref (positions dirlights) i))))
    (dotimes (i (scene-data-nspot scene))
      (setf (aref tan-spot-pos i) (* tbn (aref (positions spotlights) i))))
    (dotimes (i (scene-data-npoint scene))
      (setf (aref tan-point-pos i) (* tbn (aref (positions pointlights) i))))
    (values clip-pos (treat-uvs uv) norm (s~ world-pos :xyz)
            tbn
            dir-pos spot-pos
            tan-dir-pos tan-spot-pos tan-point-pos
            (* tbn cam-pos)
            (* tbn (s~ world-pos :xyz)))))




;; Defer

(defun-g vert-with-tbdata-defer ((vert g-pnt) (tb tb-data) &uniform
                                 (model-world :mat4)
                                 (world-view  :mat4)
                                 (view-clip   :mat4)
                                 (cam-pos     :vec3)
                                 (uv-repeat   :vec2)
                                 (scale       :float))
  (let* ((pos       (* scale       (pos vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         (uv        (* uv-repeat (tex vert)))
         (normal-m3 (transpose (inverse (m4:to-mat3 model-world))))
         (norm      (norm vert))
         (norm      (* (m4:to-mat3 model-world)
                       norm))
         (t0  (normalize (* normal-m3 (tb-data-tangent tb))))
         (n0  (normalize (* normal-m3 (norm vert))))
         (t0  (normalize (- t0 (* (dot t0 n0) n0))))
         (b0  (cross n0 t0))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos (treat-uvs uv) norm (s~ world-pos :xyz)
            tbn
            (* tbn cam-pos)
            (* tbn (s~ world-pos :xyz)))))



;;--------------------------------------------------
;; https://learnopengl.com/Advanced-Lighting/Normal-Mapping
;; "Pushing pixels" code

(defun-g norm-from-map ((normal-map :sampler-2d)
                        (uv         :vec2))
  (let* ((normal (s~ (texture normal-map uv) :xyz))
         (normal (normalize (- (* 2f0 normal) 1f0))))
    normal))

(defun-g norm-from-map-flipped ((normal-map :sampler-2d)
                                (uv         :vec2))
  "Sometimes y component is wrong on the normal map."
  (let* ((normal (s~ (texture normal-map uv) :xyz))
         (normal (v! (x normal) (- (y normal)) (z normal)))
         (normal (normalize (1- (* 2 normal)))))
    normal))

;; If there is no parallax mapping, we need to convert normals to tangent space

(defun-g norm-from-map ((normal-map :sampler-2d)
                        (uv         :vec2)
                        (tbn        :mat3))
  (* tbn (norm-from-map normal-map uv)))

(defun-g norm-from-map-flipped ((normal-map :sampler-2d)
                                (uv         :vec2)
                                (tbn        :mat3))
  "Sometimes y component is wrong on the normal map."
  (* tbn (norm-from-map-flipped normal-map uv)))

;; https://github.com/JoeyDeVries/LearnOpenGL/blob/master/src/6.pbr/1.2.lighting_textured/1.2.pbr.fs
;; "Easy trick to get tangent-normals to world-space to keep PBR code simplified.
;;  Don't worry if you don't get what's going on; you generally want to do normal
;;  mapping the usual way for performance anways; I do plan make a note of this
;;  technique somewhere later in the normal mapping tutorial."
;; NT: To get the tangent normal to world-space IN the fragment shader
(defun-g norm-from-map ((normal-map :sampler-2d)
                        (uv :vec2)
                        (world-pos :vec3)
                        (normal :vec3))
  (let* ((tangent-normal (+ -1 (* 2 (s~ (texture normal-map uv) :xyz))))
         (q1  (d-fdx world-pos))
         (q2  (d-fdy world-pos))
         (st1 (d-fdx uv))
         (st2 (d-fdy uv))
         (n0  (normalize normal))
         (t0  (normalize (- (* q1 (y st2))
                            (* q1 (y st1)))))
         (b0  (- (normalize (cross n0 t0))))
         (tbn (mat3 t0 b0 n0))
         (result (normalize (* tbn tangent-normal))))
    result))

(defun-g norm-from-map-flipped ((normal-map :sampler-2d)
                                (uv :vec2)
                                (world-pos :vec3)
                                (normal :vec3))
  (let* ((tangent-normal (+ -1 (* 2 (s~ (texture normal-map uv) :xyz))))
         (q1  (d-fdx world-pos))
         (q2  (d-fdy world-pos))
         (st1 (d-fdx uv))
         (st2 (d-fdy uv))
         (n0  (normalize normal))
         (t0  (normalize (- (* q1 (y st2))
                            (* q1 (y st1)))))
         (b0  (- (normalize (cross n0 t0))))
         (tbn (mat3 t0 b0 n0))
         (result (normalize (* tbn tangent-normal))))
    (v! (x result)
        (- (y result))
        (z normal))))


