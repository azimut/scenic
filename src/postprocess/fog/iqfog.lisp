(in-package #:scenic)

;; 2010 - http://iquilezles.org/www/articles/fog/fog.htm

;; "For example, the color of the fog can tell us about the strengh of the
;; sun. Even more, if we make the color of the fog not constant but
;; orientation dependant we can introduce an extra level of realism to
;; the image. For example, we can change the typical bluish fog color to
;; something yellowish when the view vector aligns with the sun
;; direction. This gives a very natural light dispersion effect. One
;; would argue that sucha an effect shouldn't be called fog but
;; scattering, and I agree, but in the end of the day one simply has to
;; modify a bit the fog equation to get the effect done."

(defclass iqfog (postprocess)
  ((density :initarg :density :accessor iqfog-density)
   (color1  :initarg :color1  :accessor iqfog-color1)
   (color2  :initarg :color2  :accessor iqfog-color2))
  (:default-initargs
   :color1 (v! 0.5 0.6 0.7) ;; blueish   - night color (v! 0.08 0.08 0.16)
   :color2 (v! 1.0 0.9 0.7) ;; yellowish - night color (v! 0.31 0.31 0.47)
   :density 1f0))

(defmethod initialize-instance :before ((obj iqfog) &key density color1 color2)
  (check-type color1 rtg-math.types:vec3)
  (check-type color2 rtg-math.types:vec3)
  (check-type density single-float))
(defmethod (setf iqfog-density) :before (new-value (obj iqfog))
  (check-type new-value single-float))

(defun-g iqfog-frag ((uv       :vec2)
                     &uniform
                     (color1   :vec3)
                     (color2   :vec3)
                     (sam-pos  :sampler-2d)
                     (sam      :sampler-2d)
                     (density  :float)
                     (cam-pos  :vec3)
                     (sun-pos  :vec3))
  (let* ((color    (texture sam uv))
         (color3   (s~ color :xyz))
         (frag-pos (s~ (texture sam-pos uv) :xyz))
         (distance (length (- cam-pos frag-pos)))
         (ray-dir  (normalize (- cam-pos frag-pos)))
         (sun-dir  (normalize (- sun-pos frag-pos)))
         (sun-amount (max (dot ray-dir sun-dir) 0f0))
         (fog-color  (mix color1 color2 (pow sun-amount 8f0))))
    (v!
     (mix color3 fog-color (- 1f0 (exp (- (* distance density)))))
     ;; NOTE: Alternative with more distance params,
     ;;       divided into "scattering" and "inscattering".
     #+nil
     (+ (* color3    (- 1f0 (v! (exp (- (* distance .4)))
                                (exp (- (* distance .4)))
                                (exp (- (* distance .4))))))
        (* fog-color (- 1f0 (v! (exp (- (* distance .2)))
                                (exp (- (* distance .2)))
                                (exp (- (* distance .2)))))) )
     (w color))))

(defpipeline-g iqfog-pipe (:points)
  :fragment (iqfog-frag :vec2))

(defmethod blit (scene (postprocess iqfog) (camera defered) time)
  (with-slots (bs density color1 color2) postprocess
    (map-g #'iqfog-pipe bs
           :sam (first (sam (prev *state*)))
           :sam-pos (second (sam camera))
           :color1 color1
           :color2 color2
           :density density
           :cam-pos (pos camera)
           :sun-pos (pos (first (lights scene))))))
