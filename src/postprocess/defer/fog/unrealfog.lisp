(in-package #:scenic)

;; References
;;
;; https://web.archive.org/web/20150518141248/http://michael-david-palmer.com/fisa/UDK-2010-07/Engine/Shaders/HeightFogCommon.usf
;; https://docs.unrealengine.com/en-us/Engine/Actors/FogEffects/HeightFog
;;
;; Calculates fogging from exponential height fog,
;; returns fog color in rgb, fog factor in a.
;; Fog Height Falloff: Height density factor, controls how the density
;;   increases as height decreases. Smaller values make the
;;   transition larger.
;; x - FogDensity *
;;     exp2(-FogHeightFalloff *
;;          (CameraWorldPosition.z - FogHeight))
;; y - FogHeightFalloff
;; z - CosTerminatorAngle

(defclass unrealfog (postprocess)
  ((density          :initarg :density          :accessor unrealfog-density)
   (falloff          :initarg :falloff          :accessor unrealfog-falloff)
   (height           :initarg :height           :accessor unrealfog-height)
   (terminator-angle :initarg :terminator-angle :accessor unrealfog-terminator-angle))
  (:default-initargs
   :density 0.05
   :falloff 0.5
   :height  3f0
   :terminator-angle 10f0)
  (:documentation "aka exponential height fog"))

(defmethod initialize-instance
    :before ((obj unrealfog) &key density falloff height terminator-angle)
  (check-type density          single-float)
  (check-type falloff          single-float)
  (check-type height           single-float)
  (check-type terminator-angle single-float))

(defmethod print-object ((obj unrealfog) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (with-slots (density falloff height terminator-angle) obj
      (format stream "DFHA: ~a ~a ~a ~a"
              density falloff height terminator-angle))))

(defun-g unrealfog-frag ((uv         :vec2)
                         &uniform
                         (world-clip :mat4)
                         (sam        :sampler-2d)
                         (sam-pos    :sampler-2d)
                         (cam-pos    :vec3)
                         (fog-params :vec3)
                         (light-pos  :vec3))
  (let* ((color           (texture sam uv))
         (color3          (s~ color :xyz))
         (frag-pos        (s~ (texture sam-pos uv) :xyz))
         ;; FIXME: Unreal uses "InCameraPosition", instead of cam-pos
         (cam-to-receiver (- frag-pos cam-pos))
         (line-integral   (* (x fog-params) (length cam-to-receiver)))
         (line-integral
           (if (> (abs (z cam-to-receiver)) .0001)
               (* line-integral
                  (/ (- 1f0 (exp2 (- (* (y fog-params) (z cam-to-receiver)))))
                     (* (y fog-params) (z cam-to-receiver))))
               line-integral))
         ;; 1 in the direction of the light vector, -1 in the opposite direction
         (cos-light-angle (dot (normalize (- light-pos frag-pos))
                               (normalize cam-to-receiver)))
         (fog-color
           (if (< cos-light-angle (z fog-params))
               (mix (v! .5 .6 .7)
                    (* .5 (+ (v! .5 .6 .7) (v! .1 .1 .1)))
                    (vec3 (saturate (/ (1+ cos-light-angle)
                                       (1+ (z fog-params))))))
               (let ((alpha (saturate (/ (- cos-light-angle (z fog-params))
                                         (- 1 (z fog-params))))))
                 (mix (* .5 (+ (v! .5 .6 .7) (v! .1 .1 .1)))
                      (v! .1 .1 .1)
                      (vec3 (* alpha alpha))))))
         (fog-factor (max (saturate (exp2 (- line-integral))) 0f0)))
    (v! (mix color3
             (* fog-color (- 1f0 fog-factor))
             (- 1f0 fog-factor))
        (w color))
    ;;(v! (* fog-color (- 1 fog-factor)) fog-factor)
    ))

(defpipeline-g unrealfog-pipe (:points)
  :fragment (unrealfog-frag :vec2))

;; FIXME: assumes the first light is the sun
(defmethod blit (scene (postprocess unrealfog) (camera defered) time)
  (with-slots (bs falloff terminator-angle density height) postprocess
    (map-g #'unrealfog-pipe bs
           :light-pos  (pos (first (lights scene)))
           :sam-pos    (second (sam camera))
           :cam-pos    (pos camera)
           :sam        (first  (sam (prev *state*)))
           :world-clip (world->clip camera)
           :fog-params
           (v! (* density
                  (expt 2 (* (- falloff)
                             (- (z (pos camera)) height))))
               falloff
               terminator-angle))))
