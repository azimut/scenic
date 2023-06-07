(in-package #:scenic)

(defclass hdr-acesfilm (hdr)
  ())

;; https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
(defun-g linear-to-srgb ((c :vec3))
  (let ((igamma #.(/ 1f0 2.2f0)))
    (pow c (vec3 igamma))))

(defun-g tone-map-acesfilm ((x :vec3) (exposure :float))
  (let ((x (* exposure x))
        (a 2.51)
        (b 0.03)
        (c 2.43)
        (d 0.59)
        (e 0.14))
    (linear-to-srgb
     (clamp (/ (* x (+ b (* a x)))
               (+ e (* x (+ d (* c x)))))
            0f0 1f0))))

(defun-g hdr-acesfilm-frag ((uv :vec2) &uniform (sam :sampler-2d) (exposure :float))
  (let* ((final-color (s~ (texture sam uv) :xyz))
         (ldr  (tone-map-acesfilm final-color exposure))
         (luma (rgb->luma-bt601 ldr)))
    (v! ldr luma)))

(defpipeline-g hdr-acesfilm-pipe (:points)
  :fragment (hdr-acesfilm-frag :vec2))

(defmethod blit (scene (postprocess hdr-acesfilm) (camera renderable) time)
  (with-slots (prev bs) *state*
    (with-slots (exposure) postprocess
      (map-g #'hdr-acesfilm-pipe bs
             :sam (first (sam prev))
             :exposure exposure))))
