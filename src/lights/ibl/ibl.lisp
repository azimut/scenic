(in-package #:scenic)

(defclass ibl ()
  ((irradiance :reader irradiance)
   (prefilter  :reader prefilter)
   (capture    :reader capture))
  (:documentation "inheritable object, with all the information needed to IBL"))

(defmethod initialize-instance :after ((obj ibl) &key)
  (with-slots (irradiance prefilter capture) obj
    (setf irradiance (make-irradiance)
          prefilter  (make-prefilter)
          capture    (make-capture))))

(defmethod free :after ((obj ibl))
  (free (irradiance obj))
  (free (prefilter obj))
  (free (capture obj)))

(defun-g fresnel-schlick-roughness ((cos-theta :float)
                                    (f0        :vec3)
                                    (roughness :float))
  (+ f0
     (* (- (max (vec3 (- 1 roughness))
                f0)
           f0)
        (pow (- 1 cos-theta) 5f0))))

(defun-g ambient-ibl ((v              :vec3)
                      (n              :vec3)
                      (irradiance-map :sampler-cube)
                      (roughness      :float)
                      (metallic       :float)
                      (color          :vec3)
                      (ao             :float))
  (let* ((f0 (vec3 0.04))
         (f0 (mix f0 color metallic))
         (ks (fresnel-schlick-roughness (max (dot n v) 0) f0 roughness))
         (kd (- 1 ks))
         (irradiance (s~ (texture irradiance-map n) :xyz))
         (diffuse (* irradiance color))
         (ambient (* kd diffuse ao)))
    ambient))

(defun-g ambient-ibl ((v          :vec3)
                      (n          :vec3)
                      (brdf       :sampler-2d)
                      (prefilter  :sampler-cube)
                      (irradiance :sampler-cube)
                      (roughness  :float)
                      (metallic   :float)
                      (color      :vec3)
                      (ao         :float))
  (let* ((f0 (vec3 0.04))
         (f0 (mix f0 color metallic))
         (f  (fresnel-schlick-roughness (max (dot n v) 0) f0 roughness))
         ;; Diffuse
         (ks f)
         (kd (* (- 1 ks)
                (- 1 metallic)))
         (irradiance (s~ (texture irradiance n) :xyz))
         (diffuse    (* irradiance color))
         ;; Specular
         ;;(r (reflect (- v) n)); unflipped
         (r (reflect v n)); flipped version
         (prefiltered-color
           (s~ (texture-lod prefilter r (* roughness 4f0))
               :xyz))
         (env-brdf
           (s~ (texture brdf (v! (max (dot n v) 0) roughness))
               :xy))
         (specular (* prefiltered-color
                      (+ (* f (x env-brdf))
                         (y env-brdf))))
         ;; AO just diffuse...
         (ambient (+ specular (* kd diffuse ao))))
    ambient))
