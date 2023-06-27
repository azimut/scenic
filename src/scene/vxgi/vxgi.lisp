(in-package #:scenic)

;; References:
;; - 2013 https://simonstechblog.blogspot.com/2013/01/implementing-voxel-cone-tracing.html
;; - 2016 https://github.com/Friduric/voxel-cone-tracing/
;;        https://vimeo.com/212749785
;; - 2020 https://github.com/mcela/vxgi/

(defclass vxgi ()
  (voxel-fbo
   voxel-light
   voxel-sam
   voxel-zam
   (voxel-diffuse    :accessor voxel-diffuse
                     :initarg  :diffuse
                     :documentation "power of indirect diffuse lighting")
   (voxel-specular   :accessor voxel-specular
                     :initarg  :specular
                     :documentation "power of indirect specular lighting")
   (voxel-scale      :reader   voxel-scale)
   (voxel-bounds-min :accessor voxel-bounds-min :initarg :bounds-min)
   (voxel-bounds-max :accessor voxel-bounds-max :initarg :bounds-max))
  (:default-initargs
   :diffuse 1f0
   :specular 1f0
   :bounds-min (v! -1 -1 -1)
   :bounds-max (v! +1 +1 +1))
  (:documentation "voxel global illumination"))

(defun calculate-voxel-scale (bmin bmax &optional (offset 0.1))
  "(v! 0.95 0.95 0.95) ; when -1,+1 and offset 0.1
   (v! 1 1 1)          ; when -1,+1 and offset 0"
  (v! (/ (- 2 offset) (abs (- (x bmax) (x bmin))))
      (/ (- 2 offset) (abs (- (y bmax) (y bmin))))
      (/ (- 2 offset) (abs (- (z bmax) (z bmin))))))

(defmethod initialize-instance :before ((obj vxgi) &key bounds-min bounds-max diffuse specular)
  (check-type bounds-min rtg-math.types:vec3)
  (check-type bounds-max rtg-math.types:vec3)
  (check-type diffuse single-float)
  (check-type specular single-float)
  (assert (and (> (x bounds-max) (x bounds-min))
               (> (y bounds-max) (y bounds-min))
               (> (z bounds-max) (z bounds-min)))))

(defmethod initialize-instance :after ((obj vxgi) &key bounds-min bounds-max)
  (setf (slot-value obj 'voxel-scale)
        (calculate-voxel-scale bounds-min bounds-max))
  (with-slots (voxel-fbo voxel-light voxel-sam voxel-zam) obj
    (setf voxel-fbo (make-fbo `(:d :dimensions (64 64))))
    (setf voxel-light (make-texture
                       nil
                       :dimensions '(64 64 64)
                       :mipmap 7
                       :element-type :rgba16f))
    (setf voxel-sam (sample voxel-light :magnify-filter :nearest
                                        :wrap :clamp-to-border))
    (setf voxel-zam (sample voxel-light :magnify-filter :nearest
                                        :wrap :clamp-to-border))
    (setf (cepl.samplers::border-color voxel-sam) (v! 0 0 0 1))
    (setf (cepl.samplers::border-color voxel-zam) (v! 0 0 0 1))
    (setf (%cepl.types::%sampler-imagine voxel-sam) t)))

(defmethod (setf voxel-diffuse) :before (new-value (obj vxgi))
  (check-type new-value single-float))
(defmethod (setf voxel-specular) :before (new-value (obj vxgi))
  (check-type new-value single-float))
(defmethod (setf voxel-bounds-max) :before (new-value (obj vxgi))
  (check-type new-value rtg-math.types:vec3))
(defmethod (setf voxel-bounds-min) :before (new-value (obj vxgi))
  (check-type new-value rtg-math.types:vec3))

(defmethod (setf voxel-bounds-max) :after (new-value (obj vxgi))
  (with-slots (voxel-bounds-min) obj
    (setf (slot-value obj 'voxel-scale)
          (calculate-voxel-scale voxel-bounds-min new-value))))
(defmethod (setf voxel-bounds-min) :after (new-value (obj vxgi))
  (with-slots (voxel-bounds-max) obj
    (setf (slot-value obj 'voxel-scale)
          (calculate-voxel-scale new-value voxel-bounds-max))))

(defmethod free :after ((obj vxgi))
  (with-slots (voxel-fbo voxel-light) obj
    (free voxel-fbo)
    (free voxel-light)))

(defclass scene-vxgi (scene vxgi)
  ()
  (:documentation "scene with global illumination"))

(defun-g defered-vxgi-frag ((uv :vec2)
                            &uniform
                            (specular-power :float)
                            (diffuse-power  :float)
                            (sample1        :sampler-2d)
                            (sample2        :sampler-2d)
                            (sample3        :sampler-2d)
                            (sample4        :sampler-2d)
                            (dirlights      dir-light-data      :ubo)
                            (pointlights    point-light-data    :ubo)
                            (spotlights     spot-light-data     :ubo)
                            (scene          scene-data          :ubo)
                            (voxel-scale    :vec3)
                            (cam-pos        :vec3)
                            (voxel-light    :sampler-3d)
                            (dirshadows     :sampler-2d-array)
                            (spotshadows    :sampler-2d-array)
                            (pointshadows   :sampler-cube-array))
  (let* ((color1    (texture sample1 uv))
         (color2    (texture sample2 uv))
         (color3    (texture sample3 uv))
         (color4    (texture sample4 uv))
         (roughness (w color1))
         (ao        (w color2))
         (specular  (w color3))
         (metallic  (x color4))
         (emissive  (y color4))
         (color     (s~ color1 :xyz))
         (frag-pos  (s~ color2 :xyz))
         (frag-norm (s~ color3 :xyz))
         (voxel-pos (* frag-pos voxel-scale))
         (final-color (v! 0 0 0))
         (indirect-raw (indirect-diffuse-light voxel-pos
                                               frag-norm
                                               voxel-light
                                               color))
         (ao       (* ao (w indirect-raw)))
         (indirect (+ (* diffuse-power
                         (s~ indirect-raw :xyz))
                      (* specular-power
                         (indirect-specular-light frag-pos
                                                  voxel-pos
                                                  frag-norm
                                                  cam-pos
                                                  specular
                                                  voxel-light
                                                  color
                                                  metallic)))))
    (dotimes (i (scene-data-ndir scene))
      (with-slots (colors positions lightspace fudge)
          dirlights
        (incf final-color
              (* (pbr-direct-lum (aref positions i) frag-pos cam-pos frag-norm
                                 roughness
                                 metallic
                                 color
                                 specular
                                 (aref colors i))
                 (shadow-factor dirshadows (* (aref lightspace i) (v! frag-pos 1))
                                (aref fudge i) i)))))
    (dotimes (i (scene-data-npoint scene))
      (with-slots (colors positions linear quadratic far fudge)
          pointlights
        (incf final-color
              (* (pbr-point-lum (aref positions i) frag-pos cam-pos
                                frag-norm
                                roughness
                                metallic
                                color
                                specular
                                (aref linear i) (aref quadratic i) (aref colors i))
                 (shadow-factor pointshadows
                                frag-pos
                                (aref positions i) ;;?
                                (aref far i)
                                (aref fudge i)
                                i)))))
    (dotimes (i (scene-data-nspot scene))
      (with-slots (colors positions linear quadratic far cutoff outer-cutoff direction lightspace fudge)
          spotlights
        (incf final-color
              (* (pbr-spot-lum (aref positions i) frag-pos cam-pos frag-norm
                               roughness
                               metallic
                               color
                               specular
                               (aref colors       i)
                               (aref direction    i)
                               (aref cutoff       i)
                               (aref outer-cutoff i)
                               (aref linear       i)
                               (aref quadratic    i))
                 (shadow-factor spotshadows
                                (* (aref lightspace i) (v! frag-pos 1))
                                (aref fudge i)
                                i)))))
    (v! (+ (* ao indirect)
           ;;indirect
           final-color
           ;;(vec3 ao)
           ;;(vec3 (w indirect-raw))
           )
        ;; TODO: this alpha is to blend the possible cubemap
        (- 1 (step (y color) 0f0)))))

(defpipeline-g defered-vxgi-pipe (:points)
  :fragment (defered-vxgi-frag :vec2))

(defmethod blit ((scene scene-vxgi) (postprocess list) (camera defered) time)
  (destructuring-bind (s1 s2 s3 s4 _) (sam camera)
    (declare (ignore _))
    (with-slots (prev bs) *state*
      (with-fbo-bound ((fbo prev))
        (clear-fbo (fbo prev)) ;; needed for scenes with no envmap
        (with-blending (blend camera)
          (alexandria:when-let
              ((actor (find-if #'cube-p (actors scene))))
            (paint scene camera actor time))
          (map-g #'defered-vxgi-pipe bs
                 :cam-pos (pos camera)
                 :scene (ubo scene)
                 :diffuse-power (voxel-diffuse scene)
                 :specular-power (voxel-specular scene)
                 ;; Samples
                 :voxel-scale (voxel-scale scene)
                 :voxel-light (slot-value scene 'voxel-sam)
                 :sample1 s1
                 :sample2 s2
                 :sample3 s3
                 :sample4 s4
                 ;; Lights
                 :dirlights (dir-ubo *state*)
                 :spotlights (spot-ubo *state*)
                 :pointlights (point-ubo *state*)
                 ;; Shadows
                 :dirshadows (dir-sam *state*)
                 :spotshadows (spot-sam *state*)
                 :pointshadows (point-sam *state*)))))))
