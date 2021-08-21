(in-package #:scenic)

(defun-g spec-phong ((light-dir :vec3) (view-dir :vec3) (normal :vec3) (shininess :float))
  (let* ((reflect-dir (reflect (- light-dir) normal))
         (spec-angle  (max (dot view-dir reflect-dir) 0))
         (spec        (pow spec-angle shininess)))
    spec))

;; https://learnopengl.com/Advanced-Lighting/Advanced-Lighting
;; https://en.wikipedia.org/wiki/Blinn%E2%80%93Phong_shading_model
(defun-g spec-blinn ((light-dir :vec3) (view-dir :vec3) (normal :vec3) (shininess :float))
  (let* ((half-dir   (normalize (+ light-dir view-dir)))
         (spec-angle (max (dot half-dir normal) 0))
         (spec       (pow spec-angle shininess)))
    spec))

;; oren-nayar
;; https://github.com/glslify/glsl-diffuse-oren-nayar
;; - is roughness a value in radians?
(defun-g diffuse-oren-nayar ((light-dir :vec3)
                             (view-dir  :vec3)
                             (normal    :vec3)
                             (roughness :float)
                             (intensity :float))
  (let* ((l-dot-v (dot light-dir view-dir))
         (n-dot-l (dot light-dir normal))
         (n-dot-v (dot normal view-dir))
         (s       (- l-dot-v (* n-dot-l n-dot-v)))
         (tt      (mix 1 (max n-dot-l n-dot-v) (step 0 s)))
         (sigma2  (* roughness roughness))
         (a       (1+ (* sigma2 (+ (/ intensity (+ sigma2 .13))
                                   (/ .5 (+ sigma2 .33))))))
         (b       (* .45 (/ sigma2 (+ sigma2 .09)))))
    (* intensity (max 0 n-dot-l) (/ (+ a (/ (* b s) tt)) +pi+))))
