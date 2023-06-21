(in-package #:scenic)

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

(defun-g get-view-pos ((uv         :vec2)
                       (g-depth    :sampler-2d)
                       (world-view :mat4))
  (let* ((x (1- (* 2 (x uv))))
         (y (1- (* 2 (y uv))))
         (z (1- (* 2 (x (texture g-depth uv)))))
         (pos-proj (v! x y z 1))
         (pos-view (* (inverse world-view) pos-proj))
         (pos-view (/ pos-view (w pos-view))))
    pos-view))

(defun-g ssao-calculate
    ((uv                 :vec2)
     (res                :vec2)
     (normal             :vec3)
     (view-clip          :mat4)
     (g-depth            :sampler-2d)
     (random-v3         (:vec3 64))
     (ssao-tex-noise     :sampler-2d)
     (ssao-radius        :float)
     (ssao-kernel        :int)
     (ssao-kernel-effect :float))
  (let* ((pos-view (get-view-pos uv g-depth view-clip))
         (normal-view
           (normalize
            (1- (* 2 normal))))
         (random-vector
           (normalize
            (1- (* 2 (s~ (texture ssao-tex-noise (* (/ res 4) uv)) :xyz)))))
         (tangent-view
           (normalize
            (- random-vector (* (dot random-vector normal-view)
                                normal-view))))
         (bitangent-view (cross normal-view tangent-view))
         (kernel-matrix  (mat3 tangent-view bitangent-view normal-view))
         (oclussion 0f0))
    (dotimes (i ssao-kernel)
      (let* ((sample-vector-view (* kernel-matrix (aref random-v3 (int i))))
             (sample-point-view  (+ pos-view (* ssao-radius (v! sample-vector-view 0))))
             (sample-point-ndc   (* view-clip sample-point-view))
             (sample-point-ndc   (/ sample-point-ndc
                                    (w sample-point-ndc)))
             (sample-point-uv    (+ .5 (* .5 (s~ sample-point-ndc :xy))))
             (z-scene-ndc        (1- (* 2 (x (texture g-depth sample-point-uv)))))
             (delta (- (z sample-point-ndc) z-scene-ndc)))
        (if (and (> delta .0001)
                 (< delta .005))
            (incf oclussion 1f0))))
    (- 1 (/ oclussion (1- (* ssao-kernel-effect ssao-kernel))))))
