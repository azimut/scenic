(in-package #:scenic)

;;--------------------------------------------------
;; 2D - Brightness Threshold (from catlikecoding)

(defun-g bloom-prefilter ((color :vec3) (threshold :float))
  (let* ((brightness (max (x color) (max (y color) (z color))))
         (contribution (/ (max 0 (- brightness threshold))
                          (max brightness 0.00001))))
    (* color contribution)))

(defun-g bloom-prefilter ((color :vec3) (threshold :float) (soft :float))
  (let* ((brightness (max (x color) (max (y color) (z color))))
         (knee       (* threshold soft))
         (soft       (+ (- brightness threshold) knee))
         (soft       (clamp soft 0 (* 2 knee)))
         (soft       (/ (* soft soft) (+ (* 4 knee) 0.00001)))
         (contribution (/ (max soft (- brightness threshold))
                          (max brightness 0.00001))))
    (* color contribution)))

(defun-g bloom-threshold-frag
    ((uv        :vec2)
     &uniform
     (intensity :float)
     (threshold :float)
     (soft      :float)
     (sam       :sampler-2d))
  (let ((color (s~ (texture sam uv) :xyz)))
    (bloom-prefilter color
                     threshold
                     soft))
  );; TODO: add uniform

(defpipeline-g bloom-threshold-pipe (:points)
  :fragment (bloom-threshold-frag :vec2))

;;--------------------------------------------------
;; 2D - Blur - For upscaling/downscaling

(defun-g sample-box
    ((uv    :vec2)
     (delta :float)
     (sam   :sampler-2d)
     (x     :float)
     (y     :float))
  (let* ((texture-size (texture-size sam 0))
         (o (* (v! x y x y)
               (v! (- delta) (- delta) delta delta)))
         (s (+ (texture sam (+ uv (s~ o :xy)))
               (texture sam (+ uv (s~ o :zy)))
               (texture sam (+ uv (s~ o :xw)))
               (texture sam (+ uv (s~ o :zw))))))
    (* s .25)))

(defun-g bloom-blur-frag
    ((uv    :vec2)
     &uniform
     (delta :float)
     (sam   :sampler-2d)
     (x     :float)
     (y     :float))
  (let ((color (sample-box uv delta sam x y)))
    color))

(defpipeline-g bloom-blur-pipe (:points)
  :fragment (bloom-blur-frag :vec2))

;;----------------------------------------
;; 2D - Apply Bloom

(defun-g bloom-frag
    ((uv        :vec2)
     &uniform
     (light-sam :sampler-2d)
     (sam       :sampler-2d)
     (delta     :float)
     (x         :float)
     (y         :float))
  (let* ((c (texture light-sam uv))
         (c (v! (+ (s~ (sample-box uv delta sam x y) :xyz)
                   (s~ c :xyz))
                (w c))))
    c))

(defpipeline-g bloom-pipe (:points)
  :fragment (bloom-frag :vec2))

;;--------------------------------------------------
(defun-g add-textures-frag
    ((uv   :vec2)
     &uniform
     (sam1 :sampler-2d)
     (sam2 :sampler-2d))
  (+ (texture sam1 uv)
     (texture sam2 uv)))

(defpipeline-g add-textures-pipe (:points)
  :fragment (add-textures-frag :vec2))
