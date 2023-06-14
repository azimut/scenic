(in-package #:scenic)

;; References:
;; - https://catlikecoding.com/unity/tutorials/advanced-rendering/depth-of-field/
;;
;; TODO:
;; - postprocess pass to do a tent-filter on the halfed
;;   fbo looks good enough for me in the low res i work
;; - Is really awkward use "renderable" with some textures being half the size

(defclass dof (postprocess renderable)
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
           :downscale 0.5f0
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
           :downscale 0.5f0
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

;;--------------------------------------------------
;; 1 - Circle of confusion - CoC
;; "determines the strength of the bokeh effect per point"

;; http://www.voidcn.com/article/p-nvhpdsyj-yy.html
(defun-g linear-eye-depth ((d :float) (n :float) (f :float))
  (let ((zz (/ (/ (- 1 (/ f n)) 2) f))
        (zw (/ (/ (+ 1 (/ f n)) 2) f)))
    (/ 1 (+ (* zz d) zw))))

(defun-g coc-frag ((uv :vec2)
                   &uniform
                   (near           :float)
                   (far            :float)
                   (samd           :sampler-2d)
                   (bokeh-radius   :float)
                   (focus-distance :float)
                   (focus-range    :float))
  (let* ((depth (x (texture samd uv)))
         (depth (linear-eye-depth depth near far))
         (coc   (/ (- depth focus-distance)
                   focus-range))
         (coc   (* (clamp coc -1 1)
                   bokeh-radius)))
    coc))

(defpipeline-g coc-pipe (:points)
  :fragment (coc-frag :vec2))

;;--------------------------------------------------
;; 2 - Circle of confusion - CoC

(defun-g coc-weight ((c :vec3))
  (/ 1f0 (+ 1f0 (max (max (x c) (y c)) (z c)))))

(defun-g coc-prefilter-frag ((uv :vec2) &uniform
                             (sam        :sampler-2d)
                             (coc-sam    :sampler-2d))
  (let* ((texel-size (/ 1f0 (texture-size sam 0)))
         (o       (* (s~ texel-size :xyxy)
                     (v! -.5 -.5 .5 .5)))
         (s0      (s~ (texture sam (+ uv (s~ o :xy))) :xyz))
         (s1      (s~ (texture sam (+ uv (s~ o :zy))) :xyz))
         (s2      (s~ (texture sam (+ uv (s~ o :xw))) :xyz))
         (s3      (s~ (texture sam (+ uv (s~ o :zw))) :xyz))
         (w0      (coc-weight s0))
         (w1      (coc-weight s1))
         (w2      (coc-weight s2))
         (w3      (coc-weight s3))
         (color   (/ (+ (* s0 w0)
                        (* s1 w1)
                        (* s2 w2)
                        (* s3 w3))
                     (max (+ w0 w1 w2 s3) .00001))) ;; s3?
         (coc0    (x (texture coc-sam (+ uv (s~ o :xy)))))
         (coc1    (x (texture coc-sam (+ uv (s~ o :zy)))))
         (coc2    (x (texture coc-sam (+ uv (s~ o :xw)))))
         (coc3    (x (texture coc-sam (+ uv (s~ o :zw)))))
         ;;
         (coc-min (min (min (min coc0 coc1) coc2) coc3))
         (coc-max (max (max (max coc0 coc1) coc2) coc3))
         (coc     (if (>= coc-max (- coc-min))
                      coc-max
                      coc-min)))
    (v! color coc)
    ;;(v4! coc)
    ))

(defpipeline-g coc-prefilter-pipe (:points)
  :fragment (coc-prefilter-frag :vec2))


;;--------------------------------------------------
;; 3 - Bokeh - DOF
;; TODO: precompute the radius per sample on the kernel

(defun-g bokeh-weight ((coc :float) (radius :float))
  (saturate (/ (+ 2f0 (- coc radius)) 2f0)))

(defun-g bokeh-frag ((uv :vec2)
                     &uniform
                     (dof-kernel   (:vec2 16))
                     (bokeh-radius :float)
                     (scene        :sampler-2d))
  (let ((texel-size (/ 1f0 (texture-size scene 0)))
        (coc      (w (texture scene uv)))
        (bg-color (v! 0 0 0))
        (fg-color (v! 0 0 0))
        (bg-weight 0f0)
        (fg-weight 0f0))
    (dotimes (k 16)
      (let* ((o (* (aref dof-kernel k)
                   bokeh-radius))
             (radius (length o))
             (o (* o texel-size))
             (s (texture scene (+ uv o)))
             (bgw (bokeh-weight (max 0 (min (w s) coc)) radius))
             (fgw (bokeh-weight (- (w s))               radius)))
        (incf bg-color  (* (s~ s :xyz) bgw))
        (incf bg-weight bgw)
        (incf fg-color  (* (s~ s :xyz) fgw))
        (incf fg-weight fgw)))
    (setf bg-color (* bg-color (/ 1f0 (if (= bg-weight 0) 1f0 bg-weight))))
    (setf fg-color (* fg-color (/ 1f0 (if (= fg-weight 0) 1f0 fg-weight))))
    (let* ((bgfg  (min 1 (/ (* fg-weight "3.14159265359") 16)))
           (color (mix bg-color fg-color bgfg)))
      (v! color bgfg))))

(defpipeline-g bokeh-pipe (:points)
  :fragment (bokeh-frag :vec2))

;; ------------------------------
;; 4

(defun-g bokeh-postfilter-frag ((uv :vec2)
                                &uniform
                                (sam :sampler-2d)
                                (texel-size :vec2))
  (let* ((o (* (s~ texel-size :xyxy)
               (v! -.5 -.5 .5 .5)))
         (s (+ (texture sam (+ uv (s~ o :xy)))
               (texture sam (+ uv (s~ o :zy)))
               (texture sam (+ uv (s~ o :xw)))
               (texture sam (+ uv (s~ o :zw))))))
    (* s .25)))

(defpipeline-g bokeh-postfilter-pipe (:points)
  :fragment (bokeh-postfilter-frag :vec2))

;;--------------------------------------------------
;; 5
(defun-g dof-combine-frag ((uv        :vec2)
                           &uniform
                           (sam       :sampler-2d)
                           (coc-sam   :sampler-2d)
                           (coc-h-sam :sampler-2d)) ;; dof-sam
  (let* ((source (texture sam uv))
         (coc    (x (texture coc-sam uv)))
         (dof    (texture coc-h-sam uv))
         (dof-strength (smoothstep .1 1 (abs coc)))
         (color  (mix (s~ source :xyz)
                      (s~ dof    :xyz)
                      (- (+ dof-strength (w dof))
                         (* dof-strength (w dof))))))
    (v! color (w source))))

(defpipeline-g dof-combine-pipe (:points)
  :fragment (dof-combine-frag :vec2))

;;--------------------------------------------------

(defmethod blit (scene (postprocess dof) (camera defered) time)
  (with-slots (radius distance range kernel bs
               render-bokeh render-coc render-coc-half)
      postprocess
    (with-setf* ((depth-test-function) #'always
                 (cull-face)  nil
                 (depth-mask) nil)
      (map-g-into (fbo render-coc) #'coc-pipe bs
                  :near (near camera)
                  :far (far camera)
                  :samd (fifth (sam camera))
                  :bokeh-radius   radius
                  :focus-distance distance
                  :focus-range    range)
      (map-g-into (fbo render-coc-half) #'coc-prefilter-pipe bs
                  :sam           (first (sam (prev *state*)))
                  :coc-sam       (first (sam render-coc)))
      (map-g-into (fbo render-bokeh) #'bokeh-pipe bs
                  :scene         (first (sam render-coc-half))
                  :dof-kernel     kernel
                  :bokeh-radius   radius)
      (map-g-into (fbo render-coc-half) #'bokeh-postfilter-pipe bs
                  :sam           (first (sam render-bokeh))))
    (map-g #'dof-combine-pipe bs
           :sam            (first (sam (prev *state*)))
           :coc-sam        (first (sam render-coc))
           :coc-h-sam      (first (sam render-coc-half)))))

(defmethod handle ((e resize) (obj dof))
  (setf (dim (dof-render-coc-half obj)) (list (width e) (height e)))
  (setf (dim (dof-render-bokeh    obj)) (list (width e) (height e)))
  (setf (dim (dof-render-coc      obj)) (list (width e) (height e))))
