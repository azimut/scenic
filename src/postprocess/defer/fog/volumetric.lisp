(in-package #:scenic)

;; Reference:
;; https://github.com/Unity-Technologies/VolumetricLighting/
;; >>>>~/projects/cl/incandescent/raymarching/volumetric-light.lisp

(defclass volumetric-fog (postprocess)
  ())

(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-int v-int) v-int :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:int :int) :int :pure t)

(defun-g ihash ((n :int))
  (let ((n (pow (<< n (int 13)) n)))
    (logand (int (+ (* n (+ (* n n 15731) 7879221))
                    1376312589))
            2147483647)))

(defun-g fog ((depth :float)
              (uv    :vec2)
              (sam3d :sampler-3d))
  (let* ((z (* depth (/ 10f0 100f0)))
         (z (/ (- z  (/ .1 100f0))
               (- 1  (/ .1 100f0)))))
    (if (< z 0f0)
        (v! 1 1 1 1)
        (let* ((uvw (v! (x uv) (y uv) z))
               (uvw (v! (+ (s~ uvw :xy)
                           ;;#+nil
                           (* (cell-noise (ivec2 (int (round (* (x uvw) 341)))
                                                 (int (round (* (y uvw) 192)))))
                              .8
                              (/ 1f0 32f0)))
                        (z uvw)))
               #+nil
               (uvw (v! (+ (* (cell-noise (ivec2 (int (round (* (x uv)
                                                                341)))
                                                 (int (round (* (y uv)
                                                                192)))))
                              .8
                              (/ 32f0))
                           uv)
                        z)))
          (texture sam3d uvw)
          ;;(v! uvw 1)
          ))))
