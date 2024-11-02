(in-package #:scenic)

;;--------------------------------------------------
;; 1 - Circle of confusion - CoC
;; "determines the strength of the bokeh effect per point"

;; http://www.voidcn.com/article/p-nvhpdsyj-yy.html
(defun-g linear-eye-depth ((d :float) (n :float) (f :float))
  (let ((zz (/ (/ (- 1 (/ f n)) 2) f))
        (zw (/ (/ (+ 1 (/ f n)) 2) f)))
    (/ 1 (+ (* zz d) zw))))

(defun-g coc-frag
    ((uv             :vec2)
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
