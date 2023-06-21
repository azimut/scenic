(in-package #:scenic)

;; References:
;; - https://github.com/McNopper/OpenGL/blob/master/Example28/shader/ssao.frag.glsl
;; - http://ogldev.atspace.co.uk/www/tutorial46/tutorial46.html
;; - https://learnopengl.com/Advanced-Lighting/SSAO
;; - https://www.youtube.com/watch?v=7fLV5ezO64w
;;   "AO is the area where ambient light gets ocludded"

(defstruct-g (random-kernel :layout :std-140)
  (random-v3 (:vec3 64)))

(defclass ssao ()
  ((noise-kernel  :reader   ssao-noise-kernel)
   (noise-tex     :reader   ssao-noise-tex)
   (noise-sam     :reader   ssao-noise-sam)
   (kernel-number :accessor ssao-kernel-number :initarg :kernel-number)
   (kernel-effect :accessor ssao-kernel-effect :initarg :kernel-effect)
   (kernel-radius :accessor ssao-kernel-radius :initarg :kernel-radius))
  (:default-initargs
   :kernel-radius 0.5; Default: 0.5
   :kernel-effect 1f0; Default: 1.0?
   :kernel-number 10); Default: 64
  (:documentation "Screen Space Ambien Oclussion"))

(defmethod (setf ssao-kernel-number) :before (new-value (obj ssao))
  (check-type new-value fixnum))
(defmethod (setf ssao-kernel-effect) :before (new-value (obj ssao))
  (check-type new-value single-float))
(defmethod (setf ssao-kernel-radius) :before (new-value (obj ssao))
  (check-type new-value single-float))

(defmethod initialize-instance :after ((obj ssao) &key)
  (with-slots (noise-tex noise-sam noise-kernel) obj
    (setf noise-tex (make-texture (generate-rotation-kernel)
                                  :element-type :rgb16f))
    (setf noise-sam (sample noise-tex
                            :minify-filter :nearest
                            :magnify-filter :nearest))
    (setf noise-kernel (make-ubo (list (generate-sample-kernel))
                                 'random-kernel))))

(defmethod free :after ((obj ssao))
  (free (ssao-noise-kernel obj))
  (free (ssao-noise-tex obj)))

(defun generate-rotation-kernel ()
  "runs once, goes to a TEXTURE, 4x4x3"
  (loop :repeat 4
        :collect
        (loop :repeat 4
              :collect
              (v! (1- (* 2 (random 1f0)))
                  (1- (* 2 (random 1f0)))
                  0f0))))

(defun generate-sample-kernel ()
  "runs once, goes to an UBO, 64x3"
  (loop :for i :below 64
        :collect
        (let* ((sample (v! (1- (* 2 (random 1f0)))
                           (1- (* 2 (random 1f0)))
                           (random 1f0)))
               (sample (v3:normalize sample))
               (sample (v3:*s sample (random 1f0)))
               (scale (/ i 64f0))
               (scale (lerp .1 1f0 (* scale scale))))
          (v3:*s sample scale))))
