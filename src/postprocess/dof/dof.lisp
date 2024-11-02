(in-package #:scenic)

;; References:
;; - https://catlikecoding.com/unity/tutorials/advanced-rendering/depth-of-field/
;;
;; TODO:
;; - postprocess pass to do a tent-filter on the halfed
;;   fbo looks good enough for me in the low res i work
;; - Is really awkward use "renderable" with some textures being half the size

(defclass dof (postprocess)
  ((radius          :accessor dof-radius
                    :initarg :radius
                    :documentation "resolution of the bokeh effect, aka strength")
   (distance        :accessor dof-distance
                    :initarg :distance
                    :documentation "focus distance between the camera and the focus plane where everything is perfectly sharp")
   (range           :accessor dof-range
                    :initarg :range
                    :documentation "focus range")
   (kernel          :accessor dof-kernel
                    :documentation "?")
   (target          :accessor dof-target)
   (render-coc      :reader dof-render-coc)
   (render-coc-half :reader dof-render-coc-half)
   (render-bokeh    :reader dof-render-bokeh))
  (:default-initargs
   :radius    4f0 ; 1.0 -  10
   :distance 10f0 ; 0.1 - 100
   :range     3f0); 0.1 -  10
  (:documentation "Depth of Field"))

;; Other kernels at:
;; https://github.com/Unity-Technologies/PostProcessing/blob/v2/PostProcessing/Shaders/Builtins/DiskKernels.hlsl
(defvar *dof-kernel*
  (list (v!  0           0)
        (v!  0.54545456  0)
        (v!  0.16855472  0.5187581)
        (v! -0.44128203  0.3206101)
        (v! -0.44128197 -0.3206102)
        (v!  0.1685548  -0.5187581)
        (v!  1           0)
        (v!  0.809017    0.58778524)
        (v!  0.30901697  0.95105654)
        (v! -0.30901703  0.9510565)
        (v! -0.80901706  0.5877852)
        (v! -1           0)
        (v! -0.80901694 -0.58778536)
        (v! -0.30901664 -0.9510566)
        (v!  0.30901712 -0.9510565)
        (v!  0.80901694 -0.5877853)))

(defmethod initialize-instance :after ((obj dof) &key)
  (with-slots (kernel render-bokeh render-coc render-coc-half) obj
    (setf render-bokeh
          (make-instance
           'renderable
           :downscale 0.5
           :texture-opts '((0 :element-type :rgba16f))
           :sample-opts '((:wrap :clamp-to-edge))))
    (setf render-coc
          (make-instance
           'renderable
           :texture-opts '((0 :element-type :r16f))
           :sample-opts '((:wrap :clamp-to-edge))))
    (setf render-coc-half
          (make-instance
           'renderable
           :downscale 0.5
           :texture-opts '((0 :element-type :rgba16f))
           :sample-opts '((:wrap :clamp-to-edge))))
    (setf kernel
          (make-c-array
           *dof-kernel*
           :dimensions 16
           :element-type :vec2))))

;; Holds the hardcoded values for the kernel on the frag shader
(defstruct-g sample-kernel
  (k (:vec2 16)))

(defmethod free :after ((obj dof))
  (free (dof-render-coc-half obj))
  (free (dof-render-bokeh obj))
  (free (dof-render-coc obj))
  (free (dof-kernel obj)))

(defmethod (setf dof-radius) :before (new-value (obj dof))
  (check-type new-value single-float))
(defmethod (setf dof-distance) :before (new-value (obj dof))
  (check-type new-value single-float))
(defmethod (setf dof-range) :before (new-value (obj dof))
  (check-type new-value single-float))
(defmethod (setf dof-target) :before (new-value (obj dof))
  (check-type new-value rtg-math.types:vec3))

(defmethod (setf dof-target) :after (new-value (obj dof))
  (setf (slot-value obj 'distance) ; FIXME: what happens when the camera moves?
        (v3:distance new-value (pos (current-camera)))))

(defmethod handle ((e resize) (obj dof))
  (setf (dim (dof-render-coc-half obj)) (list (width e) (height e)))
  (setf (dim (dof-render-bokeh    obj)) (list (width e) (height e)))
  (setf (dim (dof-render-coc      obj)) (list (width e) (height e))))
